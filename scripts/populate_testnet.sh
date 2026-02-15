#!/bin/bash

# BJJ Belt System - Testnet Data Population Script
# This script populates the testnet with sample profiles and promotions for testing/demo purposes
#
# Created profiles:
#   - 2 Organizations (academies)
#   - 1 Grand Master (Red belt)
#   - 2 Masters (Black belts)
#   - 4 Students (various belt levels)
#
# Promotion scenarios:
#   - Master promotes student White -> Blue
#   - Master promotes student Blue -> Purple
#   - Different master promotes different student White -> Blue
#   - Grand Master promotes master Black -> Black1
#
# Membership scenarios:
#   - John at Gracie Barra: create history, add second interval, accept interval
#   - Maria at Alliance: create history, add second interval, accept interval
#   - Carlos at Gracie Barra: create history, add interval, accept
#   - Emma at Alliance: create history, add interval, accept
#   - John at Alliance (second org): create history only
#   - Master Ricardo at Gracie Barra (instructor): create history

set -e # Exit on any error
set -o pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m' # No Color

# Function to print colored output
print_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1" >&2
}

print_section() {
    echo ""
    echo -e "${MAGENTA}════════════════════════════════════════════════════════════════${NC}"
    echo -e "${MAGENTA}  $1${NC}"
    echo -e "${MAGENTA}════════════════════════════════════════════════════════════════${NC}"
    echo ""
}

print_subsection() {
    echo -e "${CYAN}────────────────────────────────────────${NC}"
    echo -e "${CYAN}  $1${NC}"
    echo -e "${CYAN}────────────────────────────────────────${NC}"
}

# Resolve repo root (script may be run from anywhere)
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

print_info "SCRIPT_DIR: $SCRIPT_DIR"
print_info "REPO_ROOT: $REPO_ROOT"

# Admin CLI command
ADMIN="cabal run admin --"

# Helper function to run admin command and extract profile/promotion ID
# Suppresses verbose output, only shows errors on failure
run_admin_cmd() {
    local output
    local exit_code=0
    output=$(cd "$REPO_ROOT" && $ADMIN "$@" 2>&1) || exit_code=$?
    
    # Check for exit code failure
    if [ "$exit_code" -ne 0 ]; then
        print_error "Command failed (exit code $exit_code): $ADMIN $*"
        echo "$output" >&2
        exit 1
    fi
    
    # Check for known error messages in output (CLI doesn't always use exit codes)
    if echo "$output" | grep -q "Please run 'deploy-reference-scripts' first"; then
        print_error "Reference scripts not properly deployed. Run: $ADMIN deploy-reference-scripts"
        echo "$output" >&2
        exit 1
    fi
    
    if echo "$output" | grep -q "No transaction building context found"; then
        print_error "No transaction building context. Run: $ADMIN deploy-reference-scripts"
        echo "$output" >&2
        exit 1
    fi
    
    # Extract the last non-empty line (the ID)
    # Profile/Rank IDs look like: "abc123def456.tokenname"
    local last_line
    last_line=$(echo "$output" | grep -v "^$" | tail -n 1)
    
    # Validate it looks like an asset ID (contains a dot)
    if ! echo "$last_line" | grep -q '\.'; then
        print_error "Unexpected output from command. Expected asset ID (format: hexhash.tokenname), got:"
        echo "Last line: '$last_line'" >&2
        echo "" >&2
        echo "Full output:" >&2
        echo "$output" >&2
        exit 1
    fi
    
    echo "$last_line"
}

# For commands that don't return an ID (like accept-promotion)
run_admin_cmd_no_output() {
    local output
    local exit_code=0
    output=$(cd "$REPO_ROOT" && $ADMIN "$@" 2>&1) || exit_code=$?
    
    # Check for exit code failure
    if [ "$exit_code" -ne 0 ]; then
        print_error "Command failed (exit code $exit_code): $ADMIN $*"
        echo "$output" >&2
        exit 1
    fi
    
    # Check for known error messages in output
    if echo "$output" | grep -q "Please run 'deploy-reference-scripts' first"; then
        print_error "Reference scripts not properly deployed. Run: $ADMIN deploy-reference-scripts"
        echo "$output" >&2
        exit 1
    fi
    
    if echo "$output" | grep -q "No transaction building context found"; then
        print_error "No transaction building context. Run: $ADMIN deploy-reference-scripts"
        echo "$output" >&2
        exit 1
    fi
}

# Pre-flight checks for required files
if [ ! -f "$REPO_ROOT/config/config_atlas.json" ]; then
    print_error "Missing $REPO_ROOT/config/config_atlas.json"
    exit 1
fi

if [ ! -f "$REPO_ROOT/operation.prv" ]; then
    print_error "Missing $REPO_ROOT/operation.prv (signing key)."
    exit 1
fi

print_section "BJJ Belt System - Testnet Data Population"
print_info "This script populates the testnet with sample profiles and promotions"
print_info "for testing and demonstration purposes."
print_info ""
print_info "The deploy-reference-scripts command now includes the full oracle and validators flow:"
print_info "  1. Deploy OracleValidator, ProfilesValidator, RanksValidator, MembershipsValidator as reference scripts"
print_info "  2. Mint oracle NFT and lock initial OracleParams at oracle validator"
print_info "  3. Compile MintingPolicy with oracle NFT and deploy as reference script"

# ============================================================================
# STEP 1: Deploy reference scripts (if not already deployed)
# ============================================================================
print_section "Step 1: Deploy Reference Scripts (includes Oracle)"

if [ ! -f "$REPO_ROOT/config/config_bjj_validators.json" ]; then
    print_info "Deploying reference scripts and oracle..."
    # Use direct command for deploy (it outputs informational messages we shouldn't treat as errors)
    deploy_output=""
    deploy_exit=0
    deploy_output=$(cd "$REPO_ROOT" && $ADMIN deploy-reference-scripts 2>&1) || deploy_exit=$?
    
    if [ "$deploy_exit" -ne 0 ]; then
        print_error "Failed to deploy reference scripts (exit code $deploy_exit)"
        echo "$deploy_output" >&2
        exit 1
    fi
    
    # Verify the config file was created
    if [ ! -f "$REPO_ROOT/config/config_bjj_validators.json" ]; then
        print_error "deploy-reference-scripts completed but config_bjj_validators.json was not created"
        echo "$deploy_output" >&2
        exit 1
    fi
    
    print_success "Reference scripts and oracle deployed successfully"
else
    print_info "Reference scripts config found (config_bjj_validators.json exists)"
    print_info "If you see deployment errors below, delete config_bjj_validators.json and re-run this script."
fi

# ============================================================================
# STEP 1b: Query Oracle (verify deployment)
# ============================================================================
print_subsection "Verify Oracle Deployment"
print_info "Querying oracle parameters..."
oracle_output=""
oracle_exit=0
oracle_output=$(cd "$REPO_ROOT" && $ADMIN query-oracle 2>&1) || oracle_exit=$?

if [ "$oracle_exit" -ne 0 ]; then
    print_warning "Could not query oracle (exit code $oracle_exit). Continuing anyway..."
else
    print_success "Oracle is accessible"
    echo "$oracle_output" | grep -E "(Paused|Min Output|Fee Config)" | while read -r line; do
        print_info "  $line"
    done
fi

# ============================================================================
# STEP 1c: Test Oracle Admin Actions
# ============================================================================
print_subsection "Test Oracle Admin Actions"

print_info "Setting fee configuration..."
run_admin_cmd_no_output set-fees \
    --fee-address "addr_test1qz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwq2ytjqp" \
    --profile-fee 2000000 \
    --promotion-fee 3000000 \
    --membership-fee 1500000
print_success "Fee configuration set"

print_info "Querying oracle after fee update..."
oracle_output2=""
oracle_exit2=0
oracle_output2=$(cd "$REPO_ROOT" && $ADMIN query-oracle 2>&1) || oracle_exit2=$?
if [ "$oracle_exit2" -eq 0 ]; then
    echo "$oracle_output2" | grep -E "(Paused|Min Output|Fee Config|Fee Address|Profile Creation Fee|Promotion Fee|Membership Fee)" | while read -r line; do
        print_info "  $line"
    done
fi

print_info "Clearing fee configuration (running without fees for testnet population)..."
run_admin_cmd_no_output set-fees --clear-fees
print_success "Fees cleared for testnet population"

# ============================================================================
# Timestamps for profile creation and promotions
# Using realistic dates with proper time-in-belt requirements
# ============================================================================

# Base timestamps (in milliseconds)
# Grand Master got Red belt in 2010
GRANDMASTER_CREATION_TIME=1262304000000  # 2010-01-01
# Masters got Black belt in 2015
MASTER1_CREATION_TIME=1420070400000      # 2015-01-01
MASTER2_CREATION_TIME=1425168000000      # 2015-03-01
# Students started at various times
STUDENT1_CREATION_TIME=1609459200000     # 2021-01-01
STUDENT2_CREATION_TIME=1612137600000     # 2021-02-01
STUDENT3_CREATION_TIME=1614556800000     # 2021-03-01
STUDENT4_CREATION_TIME=1617235200000     # 2021-04-01
# Organizations created in 2020
ORG1_CREATION_TIME=1577836800000         # 2020-01-01
ORG2_CREATION_TIME=1580515200000         # 2020-02-01

# Promotion timestamps (with proper time-in-belt requirements)
# Student1 White->Blue after 18 months (2022-07-01)
STUDENT1_BLUE_TIME=1656633600000
# Student1 Blue->Purple after 24 months (2024-07-01)
STUDENT1_PURPLE_TIME=1719792000000
# Student2 White->Blue after 18 months (2022-08-01)
STUDENT2_BLUE_TIME=1659312000000
# Master1 Black->Black1 after 36 months (2018-01-01)
MASTER1_BLACK1_TIME=1514764800000

# ============================================================================
# STEP 2: Create Organizations
# ============================================================================
print_section "Step 2: Create Organizations"

print_info "Creating Gracie Barra Academy..."
ORG1_ID=$(run_admin_cmd init-profile \
    --name "Gracie Barra Academy" \
    --description "One of the largest BJJ organizations in the world. Founded by Master Carlos Gracie Jr., dedicated to spreading Jiu-Jitsu for everyone." \
    --image-uri "https://example.com/gracie-barra-logo.png" \
    --organization \
    --posix "$ORG1_CREATION_TIME" \
    --output-id)
print_success "Gracie Barra Academy created: $ORG1_ID"

print_info "Creating Alliance Jiu-Jitsu..."
ORG2_ID=$(run_admin_cmd init-profile \
    --name "Alliance Jiu-Jitsu" \
    --description "Premier BJJ academy known for producing world champions. Committed to excellence in martial arts training." \
    --image-uri "https://example.com/alliance-logo.png" \
    --organization \
    --posix "$ORG2_CREATION_TIME" \
    --output-id)
print_success "Alliance Jiu-Jitsu created: $ORG2_ID"

# ============================================================================
# STEP 3: Create Grand Master (Red Belt)
# ============================================================================
print_section "Step 3: Create Grand Master"

print_info "Creating Grand Master Helio..."
GRANDMASTER_ID=$(run_admin_cmd create-profile-with-rank \
    --name "Grand Master Helio" \
    --description "9th degree Red Belt. Pioneer of Brazilian Jiu-Jitsu. Dedicated his life to perfecting the gentle art and spreading its philosophy worldwide." \
    --image-uri "https://example.com/grandmaster-helio.jpg" \
    --practitioner \
    --posix "$GRANDMASTER_CREATION_TIME" \
    --belt Red \
    --output-id)
print_success "Grand Master created: $GRANDMASTER_ID"

# ============================================================================
# STEP 4: Create Masters (Black Belts)
# ============================================================================
print_section "Step 4: Create Masters"

print_info "Creating Master Ricardo Silva..."
MASTER1_ID=$(run_admin_cmd create-profile-with-rank \
    --name "Master Ricardo Silva" \
    --description "3rd degree Black Belt. Head instructor at Gracie Barra Academy. Specializes in competition training and has produced multiple world champions." \
    --image-uri "https://example.com/master-ricardo.jpg" \
    --practitioner \
    --posix "$MASTER1_CREATION_TIME" \
    --belt Black \
    --output-id)
print_success "Master Ricardo created: $MASTER1_ID"

print_info "Creating Master Ana Santos..."
MASTER2_ID=$(run_admin_cmd create-profile-with-rank \
    --name "Master Ana Santos" \
    --description "2nd degree Black Belt. Head instructor at Alliance Academy. Pioneer in women's BJJ and advocate for inclusive training." \
    --image-uri "https://example.com/master-ana.jpg" \
    --practitioner \
    --posix "$MASTER2_CREATION_TIME" \
    --belt Black \
    --output-id)
print_success "Master Ana created: $MASTER2_ID"

# ============================================================================
# STEP 5: Create Students
# ============================================================================
print_section "Step 5: Create Students"

print_info "Creating Student John Martinez..."
STUDENT1_ID=$(run_admin_cmd init-profile \
    --name "John Martinez" \
    --description "Dedicated practitioner training under Master Ricardo. Focused on competition and self-defense techniques." \
    --image-uri "ipfs://QmReBRNMe7tBr6WbA89uwnHHW7f7Zoe8wY2mzVpA8STdAk" \
    --practitioner \
    --posix "$STUDENT1_CREATION_TIME" \
    --output-id)
print_success "Student John created (White belt): $STUDENT1_ID"

print_info "Creating Student Maria Garcia..."
STUDENT2_ID=$(run_admin_cmd init-profile \
    --name "Maria Garcia" \
    --description "Passionate about BJJ. Training for self-improvement and community. Dreams of becoming an instructor." \
    --image-uri "ipfs://QmYwAPJzv5CZsnA625s3Xf2nemtYgPpHdWEz79ojWnPbdG" \
    --practitioner \
    --posix "$STUDENT2_CREATION_TIME" \
    --output-id)
print_success "Student Maria created (White belt): $STUDENT2_ID"

print_info "Creating Student Carlos Oliveira..."
STUDENT3_ID=$(run_admin_cmd init-profile \
    --name "Carlos Oliveira" \
    --description "Former wrestler transitioning to BJJ. Bringing grappling experience and eager to learn submission techniques." \
    --image-uri "ipfs://QmZ4tDuvesekSs4qM5ZBKpXiZGun7S2CYtEZRB3DYXkjGx" \
    --practitioner \
    --posix "$STUDENT3_CREATION_TIME" \
    --output-id)
print_success "Student Carlos created (White belt): $STUDENT3_ID"

print_info "Creating Student Emma Thompson..."
STUDENT4_ID=$(run_admin_cmd init-profile \
    --name "Emma Thompson" \
    --description "Started BJJ for fitness, fell in love with the art. Active competitor in local tournaments." \
    --image-uri "ipfs://QmPZ9gcCEpqKTo6aq61g2nXGUhM4iCL3ewB6LDXZCtioEB" \
    --practitioner \
    --posix "$STUDENT4_CREATION_TIME" \
    --output-id)
print_success "Student Emma created (White belt): $STUDENT4_ID"

# ============================================================================
# STEP 6: Promotions
# ============================================================================
print_section "Step 6: Create and Accept Promotions"

# --- Promotion 1: Master Ricardo promotes John to Blue Belt ---
print_subsection "Promotion 1: John White -> Blue"
print_info "Master Ricardo promoting John to Blue belt..."

PROMO1_ID=$(run_admin_cmd promote-profile \
    --promoted-profile-id "$STUDENT1_ID" \
    --promoted-by-profile-id "$MASTER1_ID" \
    --posix "$STUDENT1_BLUE_TIME" \
    --belt Blue \
    --output-id)
print_success "Blue belt promotion created: $PROMO1_ID"

print_info "John accepting Blue belt promotion..."
run_admin_cmd_no_output accept-promotion --asset-class "$PROMO1_ID"
print_success "John is now a Blue belt!"

# --- Promotion 2: Master Ricardo promotes John to Purple Belt ---
print_subsection "Promotion 2: John Blue -> Purple"
print_info "Master Ricardo promoting John to Purple belt..."

PROMO2_ID=$(run_admin_cmd promote-profile \
    --promoted-profile-id "$STUDENT1_ID" \
    --promoted-by-profile-id "$MASTER1_ID" \
    --posix "$STUDENT1_PURPLE_TIME" \
    --belt Purple \
    --output-id)
print_success "Purple belt promotion created: $PROMO2_ID"

print_info "John accepting Purple belt promotion..."
run_admin_cmd_no_output accept-promotion --asset-class "$PROMO2_ID"
print_success "John is now a Purple belt!"

# --- Promotion 3: Master Ana promotes Maria to Blue Belt ---
print_subsection "Promotion 3: Maria White -> Blue"
print_info "Master Ana promoting Maria to Blue belt..."

PROMO3_ID=$(run_admin_cmd promote-profile \
    --promoted-profile-id "$STUDENT2_ID" \
    --promoted-by-profile-id "$MASTER2_ID" \
    --posix "$STUDENT2_BLUE_TIME" \
    --belt Blue \
    --output-id)
print_success "Blue belt promotion created: $PROMO3_ID"

print_info "Maria accepting Blue belt promotion..."
run_admin_cmd_no_output accept-promotion --asset-class "$PROMO3_ID"
print_success "Maria is now a Blue belt!"

# --- Promotion 4: Grand Master promotes Master Ricardo to Black1 ---
print_subsection "Promotion 4: Master Ricardo Black -> Black1"
print_info "Grand Master promoting Master Ricardo to 1st Degree Black belt..."

PROMO4_ID=$(run_admin_cmd promote-profile \
    --promoted-profile-id "$MASTER1_ID" \
    --promoted-by-profile-id "$GRANDMASTER_ID" \
    --posix "$MASTER1_BLACK1_TIME" \
    --belt Black1 \
    --output-id)
print_success "Black1 promotion created: $PROMO4_ID"

print_info "Master Ricardo accepting 1st Degree Black belt promotion..."
run_admin_cmd_no_output accept-promotion --asset-class "$PROMO4_ID"
print_success "Master Ricardo is now a 1st Degree Black belt!"

# ============================================================================
# STEP 6b: Memberships (practitioner at organization)
# ============================================================================
print_section "Step 6b: Create Memberships"

# John at Gracie Barra: create history (2021-01-01 to 2022-01-01), add second interval (2022-01-01 to 2023-01-01), accept second interval
MEMBERSHIP_JOHN_START=1609459200000   # 2021-01-01
MEMBERSHIP_JOHN_FIRST_END=1640995200000   # 2022-01-01
MEMBERSHIP_JOHN_SECOND_END=1672531200000  # 2023-01-01

print_subsection "Membership 1: John at Gracie Barra Academy"
print_info "Creating membership history (John at Gracie Barra)..."
MEMBERSHIP_JOHN_ID=$(run_admin_cmd create-membership-history \
    --org-profile-id "$ORG1_ID" \
    --practitioner-profile-id "$STUDENT1_ID" \
    --posix "$MEMBERSHIP_JOHN_START" \
    --end-posix "$MEMBERSHIP_JOHN_FIRST_END" \
    --output-id)
print_success "Membership history created: $MEMBERSHIP_JOHN_ID"

print_info "Adding second membership interval (2022-01-01 to 2023-01-01)..."
INTERVAL_JOHN_ID=$(run_admin_cmd add-membership-interval \
    --org-profile-id "$ORG1_ID" \
    --membership-node-id "$MEMBERSHIP_JOHN_ID" \
    --posix "$MEMBERSHIP_JOHN_FIRST_END" \
    --end-posix "$MEMBERSHIP_JOHN_SECOND_END" \
    --output-id)
print_success "Second interval created: $INTERVAL_JOHN_ID"

print_info "John accepting membership interval..."
run_admin_cmd_no_output accept-membership-interval --interval-id "$INTERVAL_JOHN_ID"
print_success "John's membership interval accepted!"

# Maria at Alliance: create history, add second interval, accept
print_subsection "Membership 2: Maria at Alliance Jiu-Jitsu"
print_info "Creating membership history (Maria at Alliance)..."
MEMBERSHIP_MARIA_ID=$(run_admin_cmd create-membership-history \
    --org-profile-id "$ORG2_ID" \
    --practitioner-profile-id "$STUDENT2_ID" \
    --posix "$MEMBERSHIP_JOHN_START" \
    --end-posix "$MEMBERSHIP_JOHN_FIRST_END" \
    --output-id)
print_success "Membership history created: $MEMBERSHIP_MARIA_ID"

print_info "Adding second membership interval for Maria..."
INTERVAL_MARIA_ID=$(run_admin_cmd add-membership-interval \
    --org-profile-id "$ORG2_ID" \
    --membership-node-id "$MEMBERSHIP_MARIA_ID" \
    --posix "$MEMBERSHIP_JOHN_FIRST_END" \
    --end-posix "$MEMBERSHIP_JOHN_SECOND_END" \
    --output-id)
print_success "Second interval created: $INTERVAL_MARIA_ID"

print_info "Maria accepting membership interval..."
run_admin_cmd_no_output accept-membership-interval --interval-id "$INTERVAL_MARIA_ID"
print_success "Maria's membership interval accepted!"

# Carlos at Gracie Barra: create history, add interval, accept
print_subsection "Membership 3: Carlos at Gracie Barra Academy"
print_info "Creating membership history (Carlos at Gracie Barra)..."
MEMBERSHIP_CARLOS_ID=$(run_admin_cmd create-membership-history \
    --org-profile-id "$ORG1_ID" \
    --practitioner-profile-id "$STUDENT3_ID" \
    --posix "$MEMBERSHIP_JOHN_START" \
    --end-posix "$MEMBERSHIP_JOHN_FIRST_END" \
    --output-id)
print_success "Membership history created: $MEMBERSHIP_CARLOS_ID"

print_info "Adding second interval for Carlos..."
INTERVAL_CARLOS_ID=$(run_admin_cmd add-membership-interval \
    --org-profile-id "$ORG1_ID" \
    --membership-node-id "$MEMBERSHIP_CARLOS_ID" \
    --posix "$MEMBERSHIP_JOHN_FIRST_END" \
    --end-posix "$MEMBERSHIP_JOHN_SECOND_END" \
    --output-id)
print_success "Second interval created: $INTERVAL_CARLOS_ID"

print_info "Carlos accepting membership interval..."
run_admin_cmd_no_output accept-membership-interval --interval-id "$INTERVAL_CARLOS_ID"
print_success "Carlos's membership interval accepted!"

# Emma at Alliance: create history, add interval, accept
print_subsection "Membership 4: Emma at Alliance Jiu-Jitsu"
print_info "Creating membership history (Emma at Alliance)..."
MEMBERSHIP_EMMA_ID=$(run_admin_cmd create-membership-history \
    --org-profile-id "$ORG2_ID" \
    --practitioner-profile-id "$STUDENT4_ID" \
    --posix "$MEMBERSHIP_JOHN_START" \
    --end-posix "$MEMBERSHIP_JOHN_FIRST_END" \
    --output-id)
print_success "Membership history created: $MEMBERSHIP_EMMA_ID"

print_info "Adding second interval for Emma..."
INTERVAL_EMMA_ID=$(run_admin_cmd add-membership-interval \
    --org-profile-id "$ORG2_ID" \
    --membership-node-id "$MEMBERSHIP_EMMA_ID" \
    --posix "$MEMBERSHIP_JOHN_FIRST_END" \
    --end-posix "$MEMBERSHIP_JOHN_SECOND_END" \
    --output-id)
print_success "Second interval created: $INTERVAL_EMMA_ID"

print_info "Emma accepting membership interval..."
run_admin_cmd_no_output accept-membership-interval --interval-id "$INTERVAL_EMMA_ID"
print_success "Emma's membership interval accepted!"

# John at Alliance (same practitioner, second organization)
print_subsection "Membership 5: John at Alliance Jiu-Jitsu (second org)"
print_info "Creating membership history (John at Alliance)..."
MEMBERSHIP_JOHN_ALLIANCE_ID=$(run_admin_cmd create-membership-history \
    --org-profile-id "$ORG2_ID" \
    --practitioner-profile-id "$STUDENT1_ID" \
    --posix "$MEMBERSHIP_JOHN_FIRST_END" \
    --output-id)
print_success "Membership history created: $MEMBERSHIP_JOHN_ALLIANCE_ID"

# Master Ricardo at Gracie Barra (instructor membership)
print_subsection "Membership 6: Master Ricardo at Gracie Barra (instructor)"
print_info "Creating membership history (Master Ricardo at Gracie Barra)..."
MEMBERSHIP_MASTER_RICARDO_ID=$(run_admin_cmd create-membership-history \
    --org-profile-id "$ORG1_ID" \
    --practitioner-profile-id "$MASTER1_ID" \
    --posix "$MASTER1_CREATION_TIME" \
    --output-id)
print_success "Membership history created: $MEMBERSHIP_MASTER_RICARDO_ID"

# ============================================================================
# STEP 7: Dust Cleanup (Permissionless Maintenance)
# ============================================================================
print_section "Step 7: Dust Cleanup"

print_info "Running cleanup-dust to sweep any dust UTxOs from validator addresses..."
cleanup_output=""
cleanup_exit=0
cleanup_output=$(cd "$REPO_ROOT" && $ADMIN cleanup-dust 2>&1) || cleanup_exit=$?
if [ "$cleanup_exit" -eq 0 ]; then
    print_success "Dust cleanup completed"
else
    # NoDustFound is expected on a clean testnet — not an error
    if echo "$cleanup_output" | grep -q "No dust UTxOs found"; then
        print_info "No dust UTxOs found at validator addresses (clean state)"
    else
        print_warning "Dust cleanup returned exit code $cleanup_exit"
    fi
fi

# ============================================================================
# Summary
# ============================================================================
print_section "Testnet Population Complete!"

echo ""
echo -e "${GREEN}Organizations:${NC}"
echo -e "  • Gracie Barra Academy: ${CYAN}$ORG1_ID${NC}"
echo -e "  • Alliance Jiu-Jitsu:   ${CYAN}$ORG2_ID${NC}"
echo ""
echo -e "${GREEN}Grand Master:${NC}"
echo -e "  • Grand Master Helio (Red): ${CYAN}$GRANDMASTER_ID${NC}"
echo ""
echo -e "${GREEN}Masters:${NC}"
echo -e "  • Master Ricardo Silva (Black1): ${CYAN}$MASTER1_ID${NC}"
echo -e "  • Master Ana Santos (Black):     ${CYAN}$MASTER2_ID${NC}"
echo ""
echo -e "${GREEN}Students:${NC}"
echo -e "  • John Martinez (Purple): ${CYAN}$STUDENT1_ID${NC}"
echo -e "  • Maria Garcia (Blue):    ${CYAN}$STUDENT2_ID${NC}"
echo -e "  • Carlos Oliveira (White): ${CYAN}$STUDENT3_ID${NC}"
echo -e "  • Emma Thompson (White):   ${CYAN}$STUDENT4_ID${NC}"
echo ""
echo -e "${GREEN}Promotions Created:${NC}"
echo -e "  1. John: White -> Blue -> Purple"
echo -e "  2. Maria: White -> Blue"
echo -e "  3. Master Ricardo: Black -> Black1"
echo ""
echo -e "${GREEN}Memberships:${NC}"
echo -e "  • John at Gracie Barra:       history ${CYAN}$MEMBERSHIP_JOHN_ID${NC}, interval accepted ${CYAN}$INTERVAL_JOHN_ID${NC}"
echo -e "  • Maria at Alliance:          history ${CYAN}$MEMBERSHIP_MARIA_ID${NC}, interval accepted ${CYAN}$INTERVAL_MARIA_ID${NC}"
echo -e "  • Carlos at Gracie Barra:     history ${CYAN}$MEMBERSHIP_CARLOS_ID${NC}, interval accepted ${CYAN}$INTERVAL_CARLOS_ID${NC}"
echo -e "  • Emma at Alliance:           history ${CYAN}$MEMBERSHIP_EMMA_ID${NC}, interval accepted ${CYAN}$INTERVAL_EMMA_ID${NC}"
echo -e "  • John at Alliance (2nd org):  history ${CYAN}$MEMBERSHIP_JOHN_ALLIANCE_ID${NC}"
echo -e "  • Master Ricardo at Gracie B: history ${CYAN}$MEMBERSHIP_MASTER_RICARDO_ID${NC}"
echo ""

print_success "Testnet successfully populated with sample BJJ data!"
print_info "You can now query these profiles and promotions via the Query API."
