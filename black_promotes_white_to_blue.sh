#!/bin/bash

# BJJ Belt System - Black Promotes White to Blue Test Script
# This script reproduces the blackPromotesWhiteToBlue test from UnitTests.hs

set -e  # Exit on any error

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

# Function to get current POSIX timestamp in milliseconds
get_posix_time() {
    # Get current time in milliseconds (POSIX timestamp * 1000)
    echo $(( $(date +%s) * 1000 ))
}

# Function to add months to POSIX timestamp in milliseconds (approximate)
add_months_to_posix() {
    local timestamp=$1
    local months=$2
    # 30 days * 24 hours * 60 minutes * 60 seconds * 1000 milliseconds = 2592000000 milliseconds per month
    local milliseconds_per_month=2592000000
    local total_milliseconds=$((months * milliseconds_per_month))
    echo $((timestamp + total_milliseconds))
}

# Check if the BJJ CLI is available
if ! command -v admin &> /dev/null; then
    print_error "BJJ CLI (admin) not found. Please make sure it's built and available in PATH."
    exit 1
fi

print_info "Starting BJJ Belt System - Black Promotes White to Blue Test"
print_info "This script reproduces the blackPromotesWhiteToBlue test from UnitTests.hs"

# Step 1: Deploy reference scripts (if not already deployed)
print_info "Step 1: Deploying reference scripts..."
if [ ! -f "config_bjj_validators.json" ]; then
    admin deploy-reference-scripts
    print_success "Reference scripts deployed successfully"
else
    print_info "Reference scripts already deployed (config_bjj_validators.json exists)"
fi

# Get current timestamp for creation dates
CREATION_TIME=$(get_posix_time)
print_info "Using creation time: $CREATION_TIME"

# Step 2: Create master profile with Black belt
print_info "Step 2: Creating master profile with Black belt..."
MASTER_PROFILE_ID=$(admin create-profile-with-rank \
    --name "Master" \
    --description "Master is a master" \
    --image-uri "ipfs://Qmb3JXJHQxuReSUaH6rXAoP5oX9NRs6JmnRFGTj2RVhGwe" \
    --practitioner \
    --posix "$CREATION_TIME" \
    --belt black \
    --output-id)

print_success "Master profile created with ID: $MASTER_PROFILE_ID"

# Step 3: Create student profile with White belt (initially)
print_info "Step 3: Creating student profile with White belt..."
STUDENT_PROFILE_ID=$(admin init-profile \
    --name "John Doe" \
    --description "John Doe is a student" \
    --image-uri "ipfs://QmReBRNMe7tBr6WbA89uwnHHW7f7Zoe8wY2mzVpA8STdAk" \
    --practitioner \
    --posix "$CREATION_TIME" \
    --output-id)

print_success "Student profile created with ID: $STUDENT_PROFILE_ID"

# Wait a bit for blockchain confirmation
print_info "Waiting for blockchain confirmation..."
sleep 5

# Step 4: Promote student from White to Blue belt
print_info "Step 4: Promoting student from White to Blue belt..."
BLUE_PROMOTION_TIME=$(get_posix_time)
BLUE_PROMOTION_ID=$(admin promote-profile \
    --promoted-profile-id "$STUDENT_PROFILE_ID" \
    --promoted-by-profile-id "$MASTER_PROFILE_ID" \
    --posix "$BLUE_PROMOTION_TIME" \
    --belt blue \
    --output-id)

print_success "Blue belt promotion created with ID: $BLUE_PROMOTION_ID"

# Step 5: Accept the Blue belt promotion
print_info "Step 5: Accepting Blue belt promotion..."
admin accept-promotion --asset-class "$BLUE_PROMOTION_ID"
print_success "Blue belt promotion accepted"

# Wait a bit for blockchain confirmation
print_info "Waiting for blockchain confirmation..."
sleep 5

# Step 6: Promote student from Blue to Purple belt
print_info "Step 6: Promoting student from Blue to Purple belt..."
# Calculate time for Purple belt (minimum 18 months after Blue)
PURPLE_PROMOTION_TIME=$(add_months_to_posix "$BLUE_PROMOTION_TIME" 18)
PURPLE_PROMOTION_ID=$(admin promote-profile \
    --promoted-profile-id "$STUDENT_PROFILE_ID" \
    --promoted-by-profile-id "$MASTER_PROFILE_ID" \
    --posix "$PURPLE_PROMOTION_TIME" \
    --belt purple \
    --output-id)

print_success "Purple belt promotion created with ID: $PURPLE_PROMOTION_ID"

# Step 7: Accept the Purple belt promotion
print_info "Step 7: Accepting Purple belt promotion..."
admin accept-promotion --asset-class "$PURPLE_PROMOTION_ID"
print_success "Purple belt promotion accepted"

print_success "Test completed successfully!"
print_info "Summary:"
print_info "  - Master profile (Black belt): $MASTER_PROFILE_ID"
print_info "  - Student profile: $STUDENT_PROFILE_ID"
print_info "  - Blue belt promotion: $BLUE_PROMOTION_ID"
print_info "  - Purple belt promotion: $PURPLE_PROMOTION_ID"
print_info "  - Student progressed from White → Blue → Purple belt"

echo ""
print_success "🎉 Black Promotes White to Blue test completed successfully!" 