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

# ============================================================================
# STEP 1: Deploy reference scripts (if not already deployed)
# ============================================================================
print_section "Step 1: Deploy Reference Scripts"

if [ ! -f "$REPO_ROOT/config/config_bjj_validators.json" ]; then
    print_info "Deploying reference scripts..."
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
    
    print_success "Reference scripts deployed successfully"
else
    print_info "Reference scripts config found (config_bjj_validators.json exists)"
    print_info "If you see deployment errors below, delete config_bjj_validators.json and re-run this script."
fi

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

print_success "Testnet successfully populated with sample BJJ data!"
print_info "You can now query these profiles and promotions via the Query API."
