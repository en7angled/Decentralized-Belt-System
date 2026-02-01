#!/bin/bash

# BJJ Belt System - Simple Black Promotes White to Blue Test Script
# This script reproduces the core part of blackPromotesWhiteToBlue test from UnitTests.hs

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

print_section "BJJ Belt System - Black Promotes White to Blue Test"
print_info "This script reproduces the core part of blackPromotesWhiteToBlue test from UnitTests.hs"

# ============================================================================
# Step 1: Deploy reference scripts (if not already deployed)
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
# ============================================================================

STUDENT_PROFILE_CREATION_TIME=1666819105000
MASTER_PROFILE_CREATION_TIME=1752441505000
BLUE_PROMOTION_TIME=1752623616000

print_info "Student profile creation time: $STUDENT_PROFILE_CREATION_TIME"
print_info "Master profile creation time: $MASTER_PROFILE_CREATION_TIME"
print_info "Blue promotion time: $BLUE_PROMOTION_TIME"

# ============================================================================
# Step 2: Create master profile with Black belt
# ============================================================================
print_section "Step 2: Create Master Profile"

print_info "Creating master profile with Black belt..."
MASTER_PROFILE_ID=$(run_admin_cmd create-profile-with-rank \
    --name "Master" \
    --description "Master is a master" \
    --image-uri "https://github.com/en7angled/Decentralized-Belt-System/blob/main/out/puml/CARDANO-BJJ-BANNER.jpeg?raw=true" \
    --practitioner \
    --posix "$MASTER_PROFILE_CREATION_TIME" \
    --belt Black \
    --output-id)
print_success "Master profile created: $MASTER_PROFILE_ID"

# ============================================================================
# Step 3: Create student profile with White belt (initially)
# ============================================================================
print_section "Step 3: Create Student Profile"

print_info "Creating student profile (White belt)..."
STUDENT_PROFILE_ID=$(run_admin_cmd init-profile \
    --name "John Doe" \
    --description "John Doe is a student" \
    --image-uri "ipfs://QmReBRNMe7tBr6WbA89uwnHHW7f7Zoe8wY2mzVpA8STdAk" \
    --practitioner \
    --posix "$STUDENT_PROFILE_CREATION_TIME" \
    --output-id)
print_success "Student profile created: $STUDENT_PROFILE_ID"

# ============================================================================
# Step 4: Promote student from White to Blue belt
# ============================================================================
print_section "Step 4: Create Promotion"

print_info "Master promoting student from White to Blue belt..."
BLUE_PROMOTION_ID=$(run_admin_cmd promote-profile \
    --promoted-profile-id "$STUDENT_PROFILE_ID" \
    --promoted-by-profile-id "$MASTER_PROFILE_ID" \
    --posix "$BLUE_PROMOTION_TIME" \
    --belt Blue \
    --output-id)
print_success "Blue belt promotion created: $BLUE_PROMOTION_ID"

# ============================================================================
# Step 5: Accept the Blue belt promotion
# ============================================================================
print_section "Step 5: Accept Promotion"

print_info "Student accepting Blue belt promotion..."
run_admin_cmd_no_output accept-promotion --asset-class "$BLUE_PROMOTION_ID"
print_success "Blue belt promotion accepted!"

# ============================================================================
# Summary
# ============================================================================
print_section "Test Completed Successfully!"

echo ""
echo -e "${GREEN}Results:${NC}"
echo -e "  - Master profile (Black belt): ${CYAN}$MASTER_PROFILE_ID${NC}"
echo -e "  - Student profile:             ${CYAN}$STUDENT_PROFILE_ID${NC}"
echo -e "  - Blue belt promotion:         ${CYAN}$BLUE_PROMOTION_ID${NC}"
echo ""
echo -e "${GREEN}Progression:${NC} Student White belt -> Blue belt"
echo ""

print_success "Black Promotes White to Blue test completed successfully!"
