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
    echo -e "${RED}[ERROR]${NC} $1"
}

# Resolve repo root (script may be run from anywhere)
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

echo "SCRIPT_DIR: $SCRIPT_DIR"
echo "REPO_ROOT: $REPO_ROOT"

# Determine how to run the admin CLI

ADMIN="cabal run admin --"


# Pre-flight checks for required files
if [ ! -f "$REPO_ROOT/config/config_atlas.json" ]; then
    print_error "Missing $REPO_ROOT/config/config_atlas.json"
    exit 1
fi

if [ ! -f "$REPO_ROOT/operation.prv" ]; then
    print_error "Missing $REPO_ROOT/operation.prv (signing key)."
    exit 1
fi

print_info "Starting BJJ Belt System - Simple Black Promotes White to Blue Test"
print_info "This script reproduces the core part of blackPromotesWhiteToBlue test from UnitTests.hs"

# Step 1: Deploy reference scripts (if not already deployed)
print_info "Step 1: Deploying reference scripts..."
if [ ! -f "$REPO_ROOT/config/config_bjj_validators.json" ]; then
    (cd "$REPO_ROOT" && $ADMIN deploy-reference-scripts)
    print_success "Reference scripts deployed successfully"
else
    print_info "Reference scripts already deployed (config_bjj_validators.json exists)"
fi

# Get current timestamp for creation dates
STUDENT_PROFILE_CREATION_TIME=1666819105000
MASTER_PROFILE_CREATION_TIME=1752441505000
BLUE_PROMOTION_TIME=1752623616000

print_info "Student profile creation time: $STUDENT_PROFILE_CREATION_TIME"
print_info "Master profile creation time: $MASTER_PROFILE_CREATION_TIME"

# Step 2: Create master profile with Black belt
print_info "Step 2: Creating master profile with Black belt..."
MASTER_PROFILE_ID=$(cd "$REPO_ROOT" && $ADMIN create-profile-with-rank \
    --name "Master" \
    --description "Master is a master" \
    --image-uri "https://github.com/en7angled/Decentralized-Belt-System/blob/main/out/puml/CARDANO-BJJ-BANNER.jpeg?raw=true" \
    --practitioner \
    --posix "$MASTER_PROFILE_CREATION_TIME" \
    --belt Black \
    --output-id | tee /dev/tty | tail -n 1)

print_success "Master profile created with ID: $MASTER_PROFILE_ID"

# Step 3: Create student profile with White belt (initially)
print_info "Step 3: Creating student profile with White belt..."
STUDENT_PROFILE_ID=$(cd "$REPO_ROOT" && $ADMIN init-profile \
    --name "John Doe" \
    --description "John Doe is a student" \
    --image-uri "ipfs://QmReBRNMe7tBr6WbA89uwnHHW7f7Zoe8wY2mzVpA8STdAk" \
    --practitioner \
    --posix "$STUDENT_PROFILE_CREATION_TIME" \
    --output-id | tee /dev/tty | tail -n 1)

print_success "Student profile created with ID: $STUDENT_PROFILE_ID"

# Step 4: Promote student from White to Blue belt
print_info "Step 4: Promoting student from White to Blue belt..."
BLUE_PROMOTION_ID=$(cd "$REPO_ROOT" && $ADMIN promote-profile \
    --promoted-profile-id "$STUDENT_PROFILE_ID" \
    --promoted-by-profile-id "$MASTER_PROFILE_ID" \
    --posix "$BLUE_PROMOTION_TIME" \
    --belt Blue \
    --output-id | tee /dev/tty | tail -n 1)

print_success "Blue belt promotion created with ID: $BLUE_PROMOTION_ID"

# Step 5: Accept the Blue belt promotion
print_info "Step 5: Accepting Blue belt promotion..."
(cd "$REPO_ROOT" && $ADMIN accept-promotion --asset-class "$BLUE_PROMOTION_ID")
print_success "Blue belt promotion accepted"

print_success "Test completed successfully!"
print_info "Summary:"
print_info "  - Master profile (Black belt): $MASTER_PROFILE_ID"
print_info "  - Student profile: $STUDENT_PROFILE_ID"
print_info "  - Blue belt promotion: $BLUE_PROMOTION_ID"
print_info "  - Student progressed from White → Blue belt"

print_success "🎉  Black Promotes White to Blue test completed successfully!  🎉 "
