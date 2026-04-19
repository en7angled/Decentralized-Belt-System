#!/bin/bash

# BJJ Belt System - Testnet Data Population Script
# This script populates the testnet with sample profiles and promotions for testing/demo purposes
#
# Created profiles:
#   - 3 Organizations (academies)
#   - 1 Grand Master (Red belt)
#   - 3 Masters (Black belts)
#   - 6 Students (various belt levels)
#
# Promotion scenarios (accepted):
#   - Professor Diego promotes Lucas: White -> Blue (accepted)
#   - Professor Diego promotes Lucas: Blue -> Purple (accepted)
#   - Professor Leticia promotes Sofia: White -> Blue (accepted)
#   - Grand Master promotes Professor Diego: Black -> Black1 (accepted)
#
# Promotion scenarios (pending - NOT accepted):
#   - Professor Thiago promotes Isabela: White -> Blue (pending)
#   - Professor Leticia promotes Pedro: White -> Blue (pending)
#   - Professor Diego promotes Rafael: White -> Blue (pending)
#
# Membership scenarios:
#   - Lucas at Checkmat: create history, add second interval, accept interval
#   - Sofia at Atos: create history, add second interval, accept interval
#   - Rafael at Checkmat: create history, add interval, accept
#   - Isabela at Nova Uniao: create history, add interval, accept
#   - Lucas at Atos (second org): create history only
#   - Professor Diego at Checkmat (instructor): create history
#
# Achievement scenarios:
#   - Lucas: Gold Medal (awarded by Checkmat, accepted)
#   - Sofia: Seminar Certificate (awarded by Atos, accepted)
#   - Rafael: Training Camp (awarded by Professor Diego, pending)

set -e # Exit on any error
set -o pipefail

# Parse arguments
FORCE_REDEPLOY=false
for arg in "$@"; do
    case "$arg" in
        --force-redeploy)
            FORCE_REDEPLOY=true
            ;;
        *)
            echo "Unknown argument: $arg" >&2
            echo "Usage: $0 [--force-redeploy]" >&2
            exit 1
            ;;
    esac
done

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

# Metadata length limits (enforced for profiles and achievements)
NAME_MAX=128
DESC_MAX=1024
IMAGE_MAX=256

# Truncate string to max length (byte length in bash)
truncate_str() {
    local max=$1
    local s="$2"
    if [ "${#s}" -le "$max" ]; then
        printf '%s' "$s"
    else
        printf '%s' "${s:0:$max}"
    fi
}

# Resolve repo root (script may be run from anywhere)
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

print_info "SCRIPT_DIR: $SCRIPT_DIR"
print_info "REPO_ROOT: $REPO_ROOT"

# Admin CLI command
ADMIN="cabal run admin --"

# Helper function to run admin command and extract profile/promotion ID
# Prints full admin stdout/stderr, then parses output for errors and the returned ID
run_admin_cmd() {
    local output
    local exit_code=0
    output=$(cd "$REPO_ROOT" && $ADMIN "$@" 2>&1) || exit_code=$?
    
    # Show admin command output as-is (to stderr so ID extraction still works when captured)
    echo "$output" >&2
    
    # Check for exit code failure
    if [ "$exit_code" -ne 0 ]; then
        print_error "Command failed (exit code $exit_code): $ADMIN $*"
        exit 1
    fi
    
    # Check for known error messages in output (CLI doesn't always use exit codes)
    if echo "$output" | grep -q "Please run 'deploy-reference-scripts' first"; then
        print_error "Reference scripts not properly deployed. Run: $ADMIN deploy-reference-scripts"
        exit 1
    fi
    
    if echo "$output" | grep -q "No transaction building context found"; then
        print_error "No transaction building context. Run: $ADMIN deploy-reference-scripts"
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
# Prints full admin stdout/stderr, then parses output for errors
run_admin_cmd_no_output() {
    local output
    local exit_code=0
    output=$(cd "$REPO_ROOT" && $ADMIN "$@" 2>&1) || exit_code=$?
    
    # Show admin command output as-is
    echo "$output"
    
    # Check for exit code failure
    if [ "$exit_code" -ne 0 ]; then
        print_error "Command failed (exit code $exit_code): $ADMIN $*"
        exit 1
    fi
    
    # Check for known error messages in output
    if echo "$output" | grep -q "Please run 'deploy-reference-scripts' first"; then
        print_error "Reference scripts not properly deployed. Run: $ADMIN deploy-reference-scripts"
        exit 1
    fi
    
    if echo "$output" | grep -q "No transaction building context found"; then
        print_error "No transaction building context. Run: $ADMIN deploy-reference-scripts"
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
print_info "  1. Deploy OracleValidator, ProfilesValidator, RanksValidator, MembershipsValidator, AchievementsValidator as reference scripts"
print_info "  2. Mint oracle NFT and lock initial OracleParams at oracle validator"
print_info "  3. Compile MintingPolicy with oracle NFT and deploy as reference script"

# ============================================================================
# STEP 1: Deploy reference scripts (if not already deployed)
# ============================================================================
print_section "Step 1: Deploy Reference Scripts (includes Oracle)"

if [ "$FORCE_REDEPLOY" = true ] && [ -f "$REPO_ROOT/config/config_bjj_validators.json" ]; then
    print_warning "Force redeploy requested — removing existing config_bjj_validators.json"
    rm "$REPO_ROOT/config/config_bjj_validators.json"
fi

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
    print_info "If you see deployment errors below, re-run with --force-redeploy"
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
    --membership-history-fee 1500000 \
    --membership-interval-fee 1500000 \
    --achievement-fee 1000000
print_success "Fee configuration set"

print_info "Querying oracle after fee update..."
oracle_output2=""
oracle_exit2=0
oracle_output2=$(cd "$REPO_ROOT" && $ADMIN query-oracle 2>&1) || oracle_exit2=$?
if [ "$oracle_exit2" -eq 0 ]; then
    echo "$oracle_output2" | grep -E "(Paused|Min Output|Fee Config|Fee Address|Profile Creation Fee|Promotion Fee|Membership History Fee|Membership Interval Fee|Achievement Fee)" | while read -r line; do
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
MASTER1_CREATION_TIME=1420070400000      # 2015-01-01 (Diego)
MASTER2_CREATION_TIME=1425168000000      # 2015-03-01 (Leticia)
MASTER3_CREATION_TIME=1430438400000      # 2015-05-01 (Thiago)
# Students started at various times
STUDENT1_CREATION_TIME=1609459200000     # 2021-01-01 (Lucas)
STUDENT2_CREATION_TIME=1612137600000     # 2021-02-01 (Sofia)
STUDENT3_CREATION_TIME=1614556800000     # 2021-03-01 (Rafael)
STUDENT4_CREATION_TIME=1617235200000     # 2021-04-01 (Isabela)
STUDENT5_CREATION_TIME=1619827200000     # 2021-05-01 (Pedro)
STUDENT6_CREATION_TIME=1622505600000     # 2021-06-01 (Camila)
# Organizations created in 2020
ORG1_CREATION_TIME=1577836800000         # 2020-01-01
ORG2_CREATION_TIME=1580515200000         # 2020-02-01
ORG3_CREATION_TIME=1583020800000         # 2020-03-01

# Promotion timestamps (with proper time-in-belt requirements)
# Lucas White->Blue after 18 months (2022-07-01)
STUDENT1_BLUE_TIME=1656633600000
# Lucas Blue->Purple after 24 months (2024-07-01)
STUDENT1_PURPLE_TIME=1719792000000
# Sofia White->Blue after 18 months (2022-08-01)
STUDENT2_BLUE_TIME=1659312000000
# Master Diego Black->Black1 after 36 months (2018-01-01)
MASTER1_BLACK1_TIME=1514764800000
# Pending promotion timestamps
# Isabela White->Blue after 18 months (2022-10-01)
STUDENT4_BLUE_TIME=1664582400000
# Pedro White->Blue after 18 months (2022-11-01)
STUDENT5_BLUE_TIME=1667260800000
# Rafael White->Blue after 18 months (2022-09-01)
STUDENT3_BLUE_TIME=1661990400000

# ============================================================================
# STEP 2: Create Organizations
# ============================================================================
print_section "Step 2: Create Organizations"

print_info "Creating Checkmat International..."
ORG1_ID=$(run_admin_cmd init-profile \
    --name "$(truncate_str $NAME_MAX "Checkmat International")" \
    --description "$(truncate_str $DESC_MAX "World-renowned BJJ team founded in 2008. Known for producing top-level competitors and fostering technical excellence across gi and no-gi.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://ui-avatars.com/api/?name=Checkmat+International&size=256&background=1a1a2e&color=e94560&bold=true&format=png")" \
    --organization \
    --posix "$ORG1_CREATION_TIME" --output-id)
print_success "Checkmat International created: $ORG1_ID"

print_info "Creating Atos Jiu-Jitsu HQ..."
ORG2_ID=$(run_admin_cmd init-profile \
    --name "$(truncate_str $NAME_MAX "Atos Jiu-Jitsu HQ")" \
    --description "$(truncate_str $DESC_MAX "Elite competition academy based in San Diego. Home to multiple ADCC and World Championship medalists pushing the boundaries of modern grappling.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://ui-avatars.com/api/?name=Atos+JJ&size=256&background=0f3460&color=e94560&bold=true&format=png")" \
    --organization \
    --posix "$ORG2_CREATION_TIME" --output-id)
print_success "Atos Jiu-Jitsu HQ created: $ORG2_ID"

print_info "Creating Nova Uniao Fight Team..."
ORG3_ID=$(run_admin_cmd init-profile \
    --name "$(truncate_str $NAME_MAX "Nova Uniao Fight Team")" \
    --description "$(truncate_str $DESC_MAX "Historic Brazilian fight team established in 1995. Produced legendary fighters across BJJ and MMA with a focus on aggressive guard play.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://ui-avatars.com/api/?name=Nova+Uniao&size=256&background=533483&color=e94560&bold=true&format=png")" \
    --organization \
    --posix "$ORG3_CREATION_TIME" --output-id)
print_success "Nova Uniao Fight Team created: $ORG3_ID"

# ============================================================================
# STEP 3: Create Grand Master (Red Belt)
# ============================================================================
print_section "Step 3: Create Grand Master"

print_info "Creating Grand Master Osvaldo Moizinho..."
GRANDMASTER_ID=$(run_admin_cmd create-profile-with-rank \
    --name "$(truncate_str $NAME_MAX "Grand Master Osvaldo Moizinho")" \
    --description "$(truncate_str $DESC_MAX "9th degree Red Belt. Over 60 years dedicated to Brazilian Jiu-Jitsu instruction. Mentor to hundreds of black belts and guardian of traditional techniques.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://robohash.org/osvaldo-moizinho?size=256x256&set=set5")" \
    --practitioner \
    --posix "$GRANDMASTER_CREATION_TIME" \
    --belt Red \
    --output-id)
print_success "Grand Master created: $GRANDMASTER_ID"

# ============================================================================
# STEP 4: Create Masters (Black Belts)
# ============================================================================
print_section "Step 4: Create Masters"

print_info "Creating Professor Diego Faria..."
MASTER1_ID=$(run_admin_cmd create-profile-with-rank \
    --name "$(truncate_str $NAME_MAX "Professor Diego Faria")" \
    --description "$(truncate_str $DESC_MAX "4th degree Black Belt. Head instructor at Checkmat International. Renowned for his pressure passing system and competition coaching methodology.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://robohash.org/diego-faria?size=256x256&set=set5")" \
    --practitioner \
    --posix "$MASTER1_CREATION_TIME" \
    --belt Black \
    --output-id)
print_success "Professor Diego created: $MASTER1_ID"

print_info "Creating Professor Leticia Ribeiro..."
MASTER2_ID=$(run_admin_cmd create-profile-with-rank \
    --name "$(truncate_str $NAME_MAX "Professor Leticia Ribeiro")" \
    --description "$(truncate_str $DESC_MAX "3rd degree Black Belt. Lead instructor at Atos Jiu-Jitsu. Multiple-time world champion and pioneer in women's competitive jiu-jitsu.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://robohash.org/leticia-ribeiro?size=256x256&set=set5")" \
    --practitioner \
    --posix "$MASTER2_CREATION_TIME" \
    --belt Black \
    --output-id)
print_success "Professor Leticia created: $MASTER2_ID"

print_info "Creating Professor Thiago Barros..."
MASTER3_ID=$(run_admin_cmd create-profile-with-rank \
    --name "$(truncate_str $NAME_MAX "Professor Thiago Barros")" \
    --description "$(truncate_str $DESC_MAX "2nd degree Black Belt. Head coach at Nova Uniao. Specialist in dynamic guard play and submission chains from bottom position.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://robohash.org/thiago-barros?size=256x256&set=set5")" \
    --practitioner \
    --posix "$MASTER3_CREATION_TIME" \
    --belt Black \
    --output-id)
print_success "Professor Thiago created: $MASTER3_ID"

# ============================================================================
# STEP 5: Create Students
# ============================================================================
print_section "Step 5: Create Students"

print_info "Creating Student Lucas Ferreira..."
STUDENT1_ID=$(run_admin_cmd init-profile \
    --name "$(truncate_str $NAME_MAX "Lucas Ferreira")" \
    --description "$(truncate_str $DESC_MAX "Competition-focused athlete training at Checkmat. Former judo practitioner bringing takedown expertise to his jiu-jitsu game.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://robohash.org/lucas-ferreira?size=256x256&set=set5")" \
    --practitioner \
    --posix "$STUDENT1_CREATION_TIME" \
    --output-id)
print_success "Student Lucas created (White belt): $STUDENT1_ID"

print_info "Creating Student Sofia Andrade..."
STUDENT2_ID=$(run_admin_cmd init-profile \
    --name "$(truncate_str $NAME_MAX "Sofia Andrade")" \
    --description "$(truncate_str $DESC_MAX "Dedicated competitor at Atos. Known for her berimbolo entries and creative inversions in tournament settings.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://robohash.org/sofia-andrade?size=256x256&set=set5")" \
    --practitioner \
    --posix "$STUDENT2_CREATION_TIME" \
    --output-id)
print_success "Student Sofia created (White belt): $STUDENT2_ID"

print_info "Creating Student Rafael Nunes..."
STUDENT3_ID=$(run_admin_cmd init-profile \
    --name "$(truncate_str $NAME_MAX "Rafael Nunes")" \
    --description "$(truncate_str $DESC_MAX "Hobbyist practitioner at Checkmat. Software engineer by day, training four times per week with a focus on self-defense applications.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://robohash.org/rafael-nunes?size=256x256&set=set5")" \
    --practitioner \
    --posix "$STUDENT3_CREATION_TIME" \
    --output-id)
print_success "Student Rafael created (White belt): $STUDENT3_ID"

print_info "Creating Student Isabela Costa..."
STUDENT4_ID=$(run_admin_cmd init-profile \
    --name "$(truncate_str $NAME_MAX "Isabela Costa")" \
    --description "$(truncate_str $DESC_MAX "Committed white belt at Nova Uniao. Former swimmer applying her athleticism and discipline to developing a well-rounded guard game.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://robohash.org/isabela-costa?size=256x256&set=set5")" \
    --practitioner \
    --posix "$STUDENT4_CREATION_TIME" \
    --output-id)
print_success "Student Isabela created (White belt): $STUDENT4_ID"

print_info "Creating Student Pedro Almeida..."
STUDENT5_ID=$(run_admin_cmd init-profile \
    --name "$(truncate_str $NAME_MAX "Pedro Almeida")" \
    --description "$(truncate_str $DESC_MAX "Enthusiastic beginner at Atos. Training consistently for eight months with a natural talent for scrambles and escapes.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://robohash.org/pedro-almeida?size=256x256&set=set5")" \
    --practitioner \
    --posix "$STUDENT5_CREATION_TIME" \
    --output-id)
print_success "Student Pedro created (White belt): $STUDENT5_ID"

print_info "Creating Student Camila Rocha..."
STUDENT6_ID=$(run_admin_cmd init-profile \
    --name "$(truncate_str $NAME_MAX "Camila Rocha")" \
    --description "$(truncate_str $DESC_MAX "Aspiring competitor at Nova Uniao. Cross-trains with the MMA team to sharpen her takedowns and top pressure game.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://robohash.org/camila-rocha?size=256x256&set=set5")" \
    --practitioner \
    --posix "$STUDENT6_CREATION_TIME" \
    --output-id)
print_success "Student Camila created (White belt): $STUDENT6_ID"

# ============================================================================
# STEP 6: Promotions
# ============================================================================
print_section "Step 6: Create Promotions (Accepted and Pending)"

# --- Promotion 1: Professor Diego promotes Lucas to Blue Belt ---
print_subsection "Promotion 1: Lucas White -> Blue"
print_info "Professor Diego promoting Lucas to Blue belt..."

PROMO1_ID=$(run_admin_cmd promote-profile \
    --promoted-profile-id "$STUDENT1_ID" \
    --promoted-by-profile-id "$MASTER1_ID" \
    --posix "$STUDENT1_BLUE_TIME" \
    --belt Blue \
    --output-id)
print_success "Blue belt promotion created: $PROMO1_ID"

print_info "Lucas accepting Blue belt promotion..."
run_admin_cmd_no_output accept-promotion --asset-class "$PROMO1_ID"
print_success "Lucas is now a Blue belt!"

# --- Promotion 2: Professor Diego promotes Lucas to Purple Belt ---
print_subsection "Promotion 2: Lucas Blue -> Purple"
print_info "Professor Diego promoting Lucas to Purple belt..."

PROMO2_ID=$(run_admin_cmd promote-profile \
    --promoted-profile-id "$STUDENT1_ID" \
    --promoted-by-profile-id "$MASTER1_ID" \
    --posix "$STUDENT1_PURPLE_TIME" \
    --belt Purple \
    --output-id)
print_success "Purple belt promotion created: $PROMO2_ID"

print_info "Lucas accepting Purple belt promotion..."
run_admin_cmd_no_output accept-promotion --asset-class "$PROMO2_ID"
print_success "Lucas is now a Purple belt!"

# --- Promotion 3: Professor Leticia promotes Sofia to Blue Belt ---
print_subsection "Promotion 3: Sofia White -> Blue"
print_info "Professor Leticia promoting Sofia to Blue belt..."

PROMO3_ID=$(run_admin_cmd promote-profile \
    --promoted-profile-id "$STUDENT2_ID" \
    --promoted-by-profile-id "$MASTER2_ID" \
    --posix "$STUDENT2_BLUE_TIME" \
    --belt Blue \
    --output-id)
print_success "Blue belt promotion created: $PROMO3_ID"

print_info "Sofia accepting Blue belt promotion..."
run_admin_cmd_no_output accept-promotion --asset-class "$PROMO3_ID"
print_success "Sofia is now a Blue belt!"

# --- Promotion 4: Grand Master promotes Professor Diego to Black1 ---
print_subsection "Promotion 4: Professor Diego Black -> Black1"
print_info "Grand Master promoting Professor Diego to 1st Degree Black belt..."

PROMO4_ID=$(run_admin_cmd promote-profile \
    --promoted-profile-id "$MASTER1_ID" \
    --promoted-by-profile-id "$GRANDMASTER_ID" \
    --posix "$MASTER1_BLACK1_TIME" \
    --belt Black1 \
    --output-id)
print_success "Black1 promotion created: $PROMO4_ID"

print_info "Professor Diego accepting 1st Degree Black belt promotion..."
run_admin_cmd_no_output accept-promotion --asset-class "$PROMO4_ID"
print_success "Professor Diego is now a 1st Degree Black belt!"

# --- Pending Promotion 5: Professor Thiago promotes Isabela to Blue Belt (NOT accepted) ---
print_subsection "Promotion 5 (PENDING): Isabela White -> Blue"
print_info "Professor Thiago promoting Isabela to Blue belt (will remain pending)..."

PROMO5_ID=$(run_admin_cmd promote-profile \
    --promoted-profile-id "$STUDENT4_ID" \
    --promoted-by-profile-id "$MASTER3_ID" \
    --posix "$STUDENT4_BLUE_TIME" \
    --belt Blue \
    --output-id)
print_success "Pending Blue belt promotion created: $PROMO5_ID"
print_warning "Isabela has NOT accepted this promotion (pending on-chain)"

# --- Pending Promotion 6: Professor Leticia promotes Pedro to Blue Belt (NOT accepted) ---
print_subsection "Promotion 6 (PENDING): Pedro White -> Blue"
print_info "Professor Leticia promoting Pedro to Blue belt (will remain pending)..."

PROMO6_ID=$(run_admin_cmd promote-profile \
    --promoted-profile-id "$STUDENT5_ID" \
    --promoted-by-profile-id "$MASTER2_ID" \
    --posix "$STUDENT5_BLUE_TIME" \
    --belt Blue \
    --output-id)
print_success "Pending Blue belt promotion created: $PROMO6_ID"
print_warning "Pedro has NOT accepted this promotion (pending on-chain)"

# --- Pending Promotion 7: Professor Diego promotes Rafael to Blue Belt (NOT accepted) ---
print_subsection "Promotion 7 (PENDING): Rafael White -> Blue"
print_info "Professor Diego promoting Rafael to Blue belt (will remain pending)..."

PROMO7_ID=$(run_admin_cmd promote-profile \
    --promoted-profile-id "$STUDENT3_ID" \
    --promoted-by-profile-id "$MASTER1_ID" \
    --posix "$STUDENT3_BLUE_TIME" \
    --belt Blue \
    --output-id)
print_success "Pending Blue belt promotion created: $PROMO7_ID"
print_warning "Rafael has NOT accepted this promotion (pending on-chain)"

# ============================================================================
# STEP 6b: Memberships (practitioner at organization)
# ============================================================================
print_section "Step 6b: Create Memberships"

# Lucas at Checkmat: create history (2021-01-01 to 2022-01-01), add second interval (2022-01-01 to 2023-01-01), accept second interval
MEMBERSHIP_START=1609459200000        # 2021-01-01
MEMBERSHIP_FIRST_END=1640995200000    # 2022-01-01
MEMBERSHIP_SECOND_END=1672531200000   # 2023-01-01

print_subsection "Membership 1: Lucas at Checkmat International"
print_info "Creating membership history (Lucas at Checkmat)..."
MEMBERSHIP_LUCAS_ID=$(run_admin_cmd create-membership-history \
    --org-profile-id "$ORG1_ID" \
    --practitioner-profile-id "$STUDENT1_ID" \
    --posix "$MEMBERSHIP_START" \
    --end-posix "$MEMBERSHIP_FIRST_END" \
    --output-id)
print_success "Membership history created: $MEMBERSHIP_LUCAS_ID"

# Protocol requires the current (first) interval to be accepted before adding the next one.
print_info "Lucas accepting first membership interval (required before adding second)..."
FIRST_INTERVAL_LUCAS_ID=$(run_admin_cmd get-first-interval-id --membership-node-id "$MEMBERSHIP_LUCAS_ID")
run_admin_cmd_no_output accept-membership-interval --interval-id "$FIRST_INTERVAL_LUCAS_ID"
print_success "First interval accepted!"

print_info "Adding second membership interval (2022-01-01 to 2023-01-01)..."
INTERVAL_LUCAS_ID=$(run_admin_cmd add-membership-interval \
    --org-profile-id "$ORG1_ID" \
    --membership-node-id "$MEMBERSHIP_LUCAS_ID" \
    --posix "$MEMBERSHIP_FIRST_END" \
    --end-posix "$MEMBERSHIP_SECOND_END" \
    --output-id)
print_success "Second interval created: $INTERVAL_LUCAS_ID"

print_info "Lucas accepting second membership interval..."
run_admin_cmd_no_output accept-membership-interval --interval-id "$INTERVAL_LUCAS_ID"
print_success "Lucas's second membership interval accepted!"

# Sofia at Atos: create history, add second interval, accept
print_subsection "Membership 2: Sofia at Atos Jiu-Jitsu HQ"
print_info "Creating membership history (Sofia at Atos)..."
MEMBERSHIP_SOFIA_ID=$(run_admin_cmd create-membership-history \
    --org-profile-id "$ORG2_ID" \
    --practitioner-profile-id "$STUDENT2_ID" \
    --posix "$MEMBERSHIP_START" \
    --end-posix "$MEMBERSHIP_FIRST_END" \
    --output-id)
print_success "Membership history created: $MEMBERSHIP_SOFIA_ID"

print_info "Sofia accepting first membership interval..."
FIRST_INTERVAL_SOFIA_ID=$(run_admin_cmd get-first-interval-id --membership-node-id "$MEMBERSHIP_SOFIA_ID")
run_admin_cmd_no_output accept-membership-interval --interval-id "$FIRST_INTERVAL_SOFIA_ID"
print_success "First interval accepted!"

print_info "Adding second membership interval for Sofia..."
INTERVAL_SOFIA_ID=$(run_admin_cmd add-membership-interval \
    --org-profile-id "$ORG2_ID" \
    --membership-node-id "$MEMBERSHIP_SOFIA_ID" \
    --posix "$MEMBERSHIP_FIRST_END" \
    --end-posix "$MEMBERSHIP_SECOND_END" \
    --output-id)
print_success "Second interval created: $INTERVAL_SOFIA_ID"

print_info "Sofia accepting membership interval..."
run_admin_cmd_no_output accept-membership-interval --interval-id "$INTERVAL_SOFIA_ID"
print_success "Sofia's membership interval accepted!"

# Rafael at Checkmat: create history, add interval, accept
print_subsection "Membership 3: Rafael at Checkmat International"
print_info "Creating membership history (Rafael at Checkmat)..."
MEMBERSHIP_RAFAEL_ID=$(run_admin_cmd create-membership-history \
    --org-profile-id "$ORG1_ID" \
    --practitioner-profile-id "$STUDENT3_ID" \
    --posix "$MEMBERSHIP_START" \
    --end-posix "$MEMBERSHIP_FIRST_END" \
    --output-id)
print_success "Membership history created: $MEMBERSHIP_RAFAEL_ID"

print_info "Rafael accepting first membership interval..."
FIRST_INTERVAL_RAFAEL_ID=$(run_admin_cmd get-first-interval-id --membership-node-id "$MEMBERSHIP_RAFAEL_ID")
run_admin_cmd_no_output accept-membership-interval --interval-id "$FIRST_INTERVAL_RAFAEL_ID"
print_success "First interval accepted!"

print_info "Adding second interval for Rafael..."
INTERVAL_RAFAEL_ID=$(run_admin_cmd add-membership-interval \
    --org-profile-id "$ORG1_ID" \
    --membership-node-id "$MEMBERSHIP_RAFAEL_ID" \
    --posix "$MEMBERSHIP_FIRST_END" \
    --end-posix "$MEMBERSHIP_SECOND_END" \
    --output-id)
print_success "Second interval created: $INTERVAL_RAFAEL_ID"

print_info "Rafael accepting membership interval..."
run_admin_cmd_no_output accept-membership-interval --interval-id "$INTERVAL_RAFAEL_ID"
print_success "Rafael's membership interval accepted!"

# Isabela at Nova Uniao: create history, add interval, accept
print_subsection "Membership 4: Isabela at Nova Uniao Fight Team"
print_info "Creating membership history (Isabela at Nova Uniao)..."
MEMBERSHIP_ISABELA_ID=$(run_admin_cmd create-membership-history \
    --org-profile-id "$ORG3_ID" \
    --practitioner-profile-id "$STUDENT4_ID" \
    --posix "$MEMBERSHIP_START" \
    --end-posix "$MEMBERSHIP_FIRST_END" \
    --output-id)
print_success "Membership history created: $MEMBERSHIP_ISABELA_ID"

print_info "Isabela accepting first membership interval..."
FIRST_INTERVAL_ISABELA_ID=$(run_admin_cmd get-first-interval-id --membership-node-id "$MEMBERSHIP_ISABELA_ID")
run_admin_cmd_no_output accept-membership-interval --interval-id "$FIRST_INTERVAL_ISABELA_ID"
print_success "First interval accepted!"

print_info "Adding second interval for Isabela..."
INTERVAL_ISABELA_ID=$(run_admin_cmd add-membership-interval \
    --org-profile-id "$ORG3_ID" \
    --membership-node-id "$MEMBERSHIP_ISABELA_ID" \
    --posix "$MEMBERSHIP_FIRST_END" \
    --end-posix "$MEMBERSHIP_SECOND_END" \
    --output-id)
print_success "Second interval created: $INTERVAL_ISABELA_ID"

print_info "Isabela accepting membership interval..."
run_admin_cmd_no_output accept-membership-interval --interval-id "$INTERVAL_ISABELA_ID"
print_success "Isabela's membership interval accepted!"

# Lucas at Atos (same practitioner, second organization)
print_subsection "Membership 5: Lucas at Atos Jiu-Jitsu HQ (second org)"
print_info "Creating membership history (Lucas at Atos)..."
MEMBERSHIP_LUCAS_ATOS_ID=$(run_admin_cmd create-membership-history \
    --org-profile-id "$ORG2_ID" \
    --practitioner-profile-id "$STUDENT1_ID" \
    --posix "$MEMBERSHIP_FIRST_END" \
    --output-id)
print_success "Membership history created: $MEMBERSHIP_LUCAS_ATOS_ID"

# Professor Diego at Checkmat (instructor membership)
print_subsection "Membership 6: Professor Diego at Checkmat (instructor)"
print_info "Creating membership history (Professor Diego at Checkmat)..."
MEMBERSHIP_DIEGO_ID=$(run_admin_cmd create-membership-history \
    --org-profile-id "$ORG1_ID" \
    --practitioner-profile-id "$MASTER1_ID" \
    --posix "$MASTER1_CREATION_TIME" \
    --output-id)
print_success "Membership history created: $MEMBERSHIP_DIEGO_ID"

# ============================================================================
# STEP 7: Achievements
# ============================================================================
print_section "Step 7: Award and Accept Achievements"

# Timestamps for achievements (in the past)
ACHIEVEMENT1_TIME=1672531200000  # 2023-01-01 (Lucas wins gold medal at tournament)
ACHIEVEMENT2_TIME=1675209600000  # 2023-02-01 (Sofia completes seminar)
ACHIEVEMENT3_TIME=1677628800000  # 2023-03-01 (Rafael attends camp)

# --- Achievement 1: Checkmat awards Lucas a tournament gold medal ---
print_subsection "Achievement 1: Lucas - Pan American Gold Medal"
print_info "Checkmat awarding gold medal to Lucas..."
ACHIEVEMENT1_ID=$(run_admin_cmd award-achievement \
    --awarded-to-profile-id "$STUDENT1_ID" \
    --awarded-by-profile-id "$ORG1_ID" \
    --name "$(truncate_str $NAME_MAX "Gold Medal - Pan American Championship")" \
    --description "$(truncate_str $DESC_MAX "Won gold medal in the purple belt division at the 2023 Pan American BJJ Championship. Submitted all opponents.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://ui-avatars.com/api/?name=Gold+Medal&size=256&background=ffd700&color=000&bold=true&format=png")" \
    --posix "$ACHIEVEMENT1_TIME" \
    --output-id)
print_success "Achievement awarded: $ACHIEVEMENT1_ID"

print_info "Lucas accepting gold medal achievement..."
run_admin_cmd_no_output accept-achievement --achievement-id "$ACHIEVEMENT1_ID"
print_success "Lucas accepted gold medal achievement!"

# --- Achievement 2: Atos awards Sofia a seminar certificate ---
print_subsection "Achievement 2: Sofia - Guard Retention Seminar"
print_info "Atos awarding seminar certificate to Sofia..."
ACHIEVEMENT2_ID=$(run_admin_cmd award-achievement \
    --awarded-to-profile-id "$STUDENT2_ID" \
    --awarded-by-profile-id "$ORG2_ID" \
    --name "$(truncate_str $NAME_MAX "Advanced Guard Retention Seminar")" \
    --description "$(truncate_str $DESC_MAX "Completed advanced guard retention seminar with visiting world champion. 8 hours of intensive drilling and sparring.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://ui-avatars.com/api/?name=Seminar&size=256&background=4a90d9&color=fff&bold=true&format=png")" \
    --posix "$ACHIEVEMENT2_TIME" \
    --output-id)
print_success "Achievement awarded: $ACHIEVEMENT2_ID"

print_info "Sofia accepting seminar achievement..."
run_admin_cmd_no_output accept-achievement --achievement-id "$ACHIEVEMENT2_ID"
print_success "Sofia accepted seminar achievement!"

# --- Achievement 3: Professor Diego awards Rafael a camp completion ---
print_subsection "Achievement 3: Rafael - Fundamentals Training Camp"
print_info "Professor Diego awarding training camp completion to Rafael..."
ACHIEVEMENT3_ID=$(run_admin_cmd award-achievement \
    --awarded-to-profile-id "$STUDENT3_ID" \
    --awarded-by-profile-id "$MASTER1_ID" \
    --name "$(truncate_str $NAME_MAX "Fundamentals Training Camp 2023")" \
    --description "$(truncate_str $DESC_MAX "Completed the intensive 2-week fundamentals training camp. Demonstrated excellent technique progression and dedication.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://ui-avatars.com/api/?name=Camp&size=256&background=27ae60&color=fff&bold=true&format=png")" \
    --posix "$ACHIEVEMENT3_TIME" \
    --output-id)
print_success "Achievement awarded: $ACHIEVEMENT3_ID"
# Note: Rafael has NOT accepted this achievement yet (pending state)

# ============================================================================
# STEP 8: Dust Cleanup (Permissionless Maintenance)
# ============================================================================
print_section "Step 8: Dust Cleanup"

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
echo -e "  • Checkmat International:  ${CYAN}$ORG1_ID${NC}"
echo -e "  • Atos Jiu-Jitsu HQ:      ${CYAN}$ORG2_ID${NC}"
echo -e "  • Nova Uniao Fight Team:   ${CYAN}$ORG3_ID${NC}"
echo ""
echo -e "${GREEN}Grand Master:${NC}"
echo -e "  • Grand Master Osvaldo Moizinho (Red): ${CYAN}$GRANDMASTER_ID${NC}"
echo ""
echo -e "${GREEN}Masters:${NC}"
echo -e "  • Professor Diego Faria (Black1):    ${CYAN}$MASTER1_ID${NC}"
echo -e "  • Professor Leticia Ribeiro (Black):  ${CYAN}$MASTER2_ID${NC}"
echo -e "  • Professor Thiago Barros (Black):    ${CYAN}$MASTER3_ID${NC}"
echo ""
echo -e "${GREEN}Students:${NC}"
echo -e "  • Lucas Ferreira (Purple):  ${CYAN}$STUDENT1_ID${NC}"
echo -e "  • Sofia Andrade (Blue):     ${CYAN}$STUDENT2_ID${NC}"
echo -e "  • Rafael Nunes (White):     ${CYAN}$STUDENT3_ID${NC}"
echo -e "  • Isabela Costa (White):    ${CYAN}$STUDENT4_ID${NC}"
echo -e "  • Pedro Almeida (White):    ${CYAN}$STUDENT5_ID${NC}"
echo -e "  • Camila Rocha (White):     ${CYAN}$STUDENT6_ID${NC}"
echo ""
echo -e "${GREEN}Accepted Promotions:${NC}"
echo -e "  1. Lucas: White -> Blue -> Purple"
echo -e "  2. Sofia: White -> Blue"
echo -e "  3. Professor Diego: Black -> Black1"
echo ""
echo -e "${YELLOW}Pending Promotions (NOT accepted):${NC}"
echo -e "  4. Isabela: White -> Blue (pending): ${CYAN}$PROMO5_ID${NC}"
echo -e "  5. Pedro: White -> Blue (pending):   ${CYAN}$PROMO6_ID${NC}"
echo -e "  6. Rafael: White -> Blue (pending):  ${CYAN}$PROMO7_ID${NC}"
echo ""
echo -e "${GREEN}Memberships:${NC}"
echo -e "  • Lucas at Checkmat:          history ${CYAN}$MEMBERSHIP_LUCAS_ID${NC}, interval accepted ${CYAN}$INTERVAL_LUCAS_ID${NC}"
echo -e "  • Sofia at Atos:              history ${CYAN}$MEMBERSHIP_SOFIA_ID${NC}, interval accepted ${CYAN}$INTERVAL_SOFIA_ID${NC}"
echo -e "  • Rafael at Checkmat:         history ${CYAN}$MEMBERSHIP_RAFAEL_ID${NC}, interval accepted ${CYAN}$INTERVAL_RAFAEL_ID${NC}"
echo -e "  • Isabela at Nova Uniao:      history ${CYAN}$MEMBERSHIP_ISABELA_ID${NC}, interval accepted ${CYAN}$INTERVAL_ISABELA_ID${NC}"
echo -e "  • Lucas at Atos (2nd org):    history ${CYAN}$MEMBERSHIP_LUCAS_ATOS_ID${NC}"
echo -e "  • Prof. Diego at Checkmat:    history ${CYAN}$MEMBERSHIP_DIEGO_ID${NC}"
echo ""
echo -e "${GREEN}Achievements:${NC}"
echo -e "  • Lucas: Gold Medal (accepted):        ${CYAN}$ACHIEVEMENT1_ID${NC}"
echo -e "  • Sofia: Seminar Cert (accepted):       ${CYAN}$ACHIEVEMENT2_ID${NC}"
echo -e "  • Rafael: Training Camp (pending):      ${CYAN}$ACHIEVEMENT3_ID${NC}"
echo ""

print_success "Testnet successfully populated with sample BJJ data!"
print_info "You can now query these profiles and promotions via the Query API."
