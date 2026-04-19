#!/bin/bash

# BJJ Belt System - Recent Activity Population Script
# This script populates the testnet with recent (last 12 months) activity for testing/demo purposes.
# It is self-contained: creates its own profiles and then generates recent promotions,
# achievements, and membership activity dated within April 2025 - March 2026.
#
# Created profiles:
#   - 2 Organizations: Alliance BJJ Academy, Gracie Barra Academy
#   - 2 Masters: Professor Ana Silva (Alliance), Professor Carlos Machado (Gracie Barra)
#   - 7 Students (created 2020-2024, various starting belts)
#
# Recent promotions (accepted):
#   - Professor Ana promotes Matheus: White -> Blue (June 2025, accepted)
#   - Professor Ana promotes Gabriela: Blue -> Purple (September 2025, accepted)
#   - Professor Ana promotes Matheus: Blue -> Purple (January 2026, accepted)
#   - Professor Ana promotes Fernanda: White -> Blue (February 2026, accepted)
#
# Recent promotions (pending):
#   - Professor Ana promotes Bruno: White -> Blue (March 2026, pending)
#
# Recent promotions (superseded):
#   - Professor Carlos promotes Matheus: White -> Blue (July 2025, superseded — he is Purple)
#   - Professor Carlos promotes Gabriela: White -> Blue (August 2025, superseded — she is Purple)
#   - Professor Carlos promotes Carolina: White -> Blue (April 2025, superseded — she is Purple)
#
# Recent memberships:
#   - Matheus at Alliance: history (2022 to 2025), renewed (2025 to 2026), accepted
#   - Gabriela at Alliance: history (2023 to 2025), renewed (2025 to 2026), accepted
#   - Fernanda at Alliance: new history (2025 to 2026), accepted
#   - Bruno at Alliance: new history (2025 to 2026), pending
#   - Professor Ana at Alliance: instructor membership (2016 to present)
#   - Lucas at Gracie Barra: history (2021 to 2026), accepted
#   - Carolina at Gracie Barra: history (2020 to 2025), renewed (2025 to 2026), accepted
#   - Rafael at Alliance: new history (2024 to 2026), accepted
#
# Recent achievements (accepted):
#   - Matheus: Silver Medal at IBJJF World Championship (May 2025)
#   - Gabriela: Gold Medal at Pan American Championship (July 2025)
#   - Matheus: Outstanding Student of the Year (December 2025)
#   - Fernanda: Fundamentals Course Completion (November 2025)
#
# Recent achievements (pending):
#   - Bruno: 100 Classes Milestone (March 2026)

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
run_admin_cmd() {
    local output
    local exit_code=0
    output=$(cd "$REPO_ROOT" && $ADMIN "$@" 2>&1) || exit_code=$?

    echo "$output" >&2

    if [ "$exit_code" -ne 0 ]; then
        print_error "Command failed (exit code $exit_code): $ADMIN $*"
        exit 1
    fi

    if echo "$output" | grep -q "Please run 'deploy-reference-scripts' first"; then
        print_error "Reference scripts not properly deployed. Run: $ADMIN deploy-reference-scripts"
        exit 1
    fi

    if echo "$output" | grep -q "No transaction building context found"; then
        print_error "No transaction building context. Run: $ADMIN deploy-reference-scripts"
        exit 1
    fi

    local last_line
    last_line=$(echo "$output" | grep -v "^$" | tail -n 1)

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

# For commands that don't return an ID
run_admin_cmd_no_output() {
    local output
    local exit_code=0
    output=$(cd "$REPO_ROOT" && $ADMIN "$@" 2>&1) || exit_code=$?

    echo "$output"

    if [ "$exit_code" -ne 0 ]; then
        print_error "Command failed (exit code $exit_code): $ADMIN $*"
        exit 1
    fi

    if echo "$output" | grep -q "Please run 'deploy-reference-scripts' first"; then
        print_error "Reference scripts not properly deployed. Run: $ADMIN deploy-reference-scripts"
        exit 1
    fi

    if echo "$output" | grep -q "No transaction building context found"; then
        print_error "No transaction building context. Run: $ADMIN deploy-reference-scripts"
        exit 1
    fi
}

# Pre-flight checks
if [ ! -f "$REPO_ROOT/config/config_atlas.json" ]; then
    print_error "Missing $REPO_ROOT/config/config_atlas.json"
    exit 1
fi

if [ ! -f "$REPO_ROOT/operation.prv" ]; then
    print_error "Missing $REPO_ROOT/operation.prv (signing key)."
    exit 1
fi

if [ ! -f "$REPO_ROOT/config/config_bjj_validators.json" ]; then
    print_error "Missing $REPO_ROOT/config/config_bjj_validators.json"
    print_error "Run scripts/populate_testnet.sh first to deploy reference scripts and oracle."
    exit 1
fi

print_section "BJJ Belt System - Recent Activity Population"
print_info "This script adds recent activity (last 12 months) to the testnet."
print_info "Reference scripts and oracle must already be deployed."

# ============================================================================
# Timestamps (POSIX milliseconds)
# ============================================================================

# Historical profile creation dates
ANA_CREATION_TIME=1451606400000         # 2016-01-01 (Professor Ana - Black belt)
MATHEUS_CREATION_TIME=1643673600000     # 2022-02-01
GABRIELA_CREATION_TIME=1654041600000    # 2022-06-01
FERNANDA_CREATION_TIME=1672531200000    # 2023-01-01
BRUNO_CREATION_TIME=1680307200000       # 2023-04-01
ALLIANCE_CREATION_TIME=1577836800000    # 2020-01-01
GRACIE_BARRA_CREATION_TIME=1546300800000 # 2019-01-01
LUCAS_CREATION_TIME=1612137600000       # 2021-02-01
CAROLINA_CREATION_TIME=1580515200000    # 2020-02-01
RAFAEL_CREATION_TIME=1704067200000      # 2024-01-01

# Matheus promotion timestamps (meets time-in-belt requirements)
# White -> Blue after 18+ months (June 2025)
MATHEUS_BLUE_TIME=1748736000000         # 2025-06-01
# Blue -> Purple after 7+ months in Blue (January 2026)
MATHEUS_PURPLE_TIME=1735689600000       # 2026-01-01

# Gabriela promotion timestamps
# (Gabriela starts as Blue belt via create-profile-with-rank, so she has Blue from 2022-06-01)
# Blue -> Purple after 36+ months (September 2025)
GABRIELA_PURPLE_TIME=1756684800000      # 2025-09-01

# Fernanda promotion timestamps
# White -> Blue after 24+ months (February 2026)
FERNANDA_BLUE_TIME=1738368000000        # 2026-02-01

# Bruno promotion timestamps
# White -> Blue after 23+ months (March 2026, pending)
BRUNO_BLUE_TIME=1740787200000           # 2026-03-01

# Professor Carlos (second instructor at Gracie Barra)
CARLOS_CREATION_TIME=1483228800000      # 2017-01-01

# Superseded promotion timestamps
# Carlos also promoted these practitioners to Blue, but they accepted Ana's promotions
# instead and advanced to Purple — making Carlos's Blue proposals superseded.
CARLOS_MATHEUS_BLUE_TIME=1751328000000  # 2025-07-01 (Matheus accepted Ana's Blue, now Purple)
CARLOS_GABRIELA_BLUE_TIME=1753920000000 # 2025-08-01 (Gabriela accepted Ana's Purple, now Purple)
CARLOS_CAROLINA_BLUE_TIME=1743465600000 # 2025-04-01 (Carolina already Purple via rank)

# Membership timestamps
MEMBERSHIP_OLD_START=1643673600000      # 2022-02-01
MEMBERSHIP_OLD_END=1748736000000        # 2025-06-01
MEMBERSHIP_RENEWAL_END=1780272000000    # 2026-06-01
MEMBERSHIP_NEW_START=1748736000000      # 2025-06-01
MEMBERSHIP_NEW_END=1780272000000        # 2026-06-01

# New member membership timestamps
LUCAS_MEMBERSHIP_START=1612137600000    # 2021-02-01
LUCAS_MEMBERSHIP_END=1780272000000      # 2026-06-01
CAROLINA_MEMBERSHIP_START=1580515200000 # 2020-02-01
CAROLINA_MEMBERSHIP_OLD_END=1748736000000 # 2025-06-01
RAFAEL_MEMBERSHIP_START=1719792000000   # 2024-07-01
RAFAEL_MEMBERSHIP_END=1780272000000     # 2026-06-01

# Achievement timestamps (all within last 12 months)
ACHIEVEMENT_MATHEUS_SILVER_TIME=1746057600000   # 2025-05-01
ACHIEVEMENT_GABRIELA_GOLD_TIME=1751328000000    # 2025-07-01
ACHIEVEMENT_FERNANDA_COURSE_TIME=1730419200000  # 2025-11-01
ACHIEVEMENT_MATHEUS_STUDENT_TIME=1733011200000  # 2025-12-01
ACHIEVEMENT_BRUNO_CLASSES_TIME=1740787200000    # 2026-03-01

# ============================================================================
# STEP 1: Create Organization
# ============================================================================
print_section "Step 1: Create Organization"

print_info "Creating Alliance BJJ Academy..."
ALLIANCE_ID=$(run_admin_cmd init-profile \
    --name "$(truncate_str $NAME_MAX "Alliance BJJ Academy")" \
    --description "$(truncate_str $DESC_MAX "One of the largest BJJ teams globally. Founded in 1993, Alliance has won more World Championship team trophies than any other academy. Known for technical precision and competition dominance.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://ui-avatars.com/api/?name=Alliance+BJJ&size=256&background=003366&color=ffd700&bold=true&format=png")" \
    --organization \
    --posix "$ALLIANCE_CREATION_TIME" --output-id)
print_success "Alliance BJJ Academy created: $ALLIANCE_ID"

print_info "Creating Gracie Barra Academy..."
GRACIE_BARRA_ID=$(run_admin_cmd init-profile \
    --name "$(truncate_str $NAME_MAX "Gracie Barra Academy")" \
    --description "$(truncate_str $DESC_MAX "One of the world's largest BJJ organizations. Founded by Carlos Gracie Jr. in 1986, Gracie Barra has over 800 schools worldwide. Committed to making jiu-jitsu accessible through structured curriculum and community.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://ui-avatars.com/api/?name=Gracie+Barra&size=256&background=cc0000&color=ffffff&bold=true&format=png")" \
    --organization \
    --posix "$GRACIE_BARRA_CREATION_TIME" --output-id)
print_success "Gracie Barra Academy created: $GRACIE_BARRA_ID"

# ============================================================================
# STEP 2: Create Master
# ============================================================================
print_section "Step 2: Create Masters"

print_info "Creating Professor Ana Silva..."
ANA_ID=$(run_admin_cmd create-profile-with-rank \
    --name "$(truncate_str $NAME_MAX "Professor Ana Silva")" \
    --description "$(truncate_str $DESC_MAX "3rd degree Black Belt. Head instructor at Alliance BJJ Academy. Multiple-time world champion in gi and no-gi. Known for her innovative leg lock system and dedication to developing the next generation of competitors.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://robohash.org/ana-silva?size=256x256&set=set5")" \
    --practitioner \
    --posix "$ANA_CREATION_TIME" \
    --belt Black \
    --output-id)
print_success "Professor Ana Silva created: $ANA_ID"

print_info "Creating Professor Carlos Machado..."
CARLOS_ID=$(run_admin_cmd create-profile-with-rank \
    --name "$(truncate_str $NAME_MAX "Professor Carlos Machado")" \
    --description "$(truncate_str $DESC_MAX "2nd degree Black Belt. Lead instructor at Gracie Barra Academy. Renowned for his half-guard system and competition coaching. Has produced multiple Pan American and European champions.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://robohash.org/carlos-machado?size=256x256&set=set5")" \
    --practitioner \
    --posix "$CARLOS_CREATION_TIME" \
    --belt Black \
    --output-id)
print_success "Professor Carlos Machado created: $CARLOS_ID"

# ============================================================================
# STEP 3: Create Students
# ============================================================================
print_section "Step 3: Create Students"

print_info "Creating Student Matheus Oliveira..."
MATHEUS_ID=$(run_admin_cmd init-profile \
    --name "$(truncate_str $NAME_MAX "Matheus Oliveira")" \
    --description "$(truncate_str $DESC_MAX "Dedicated competitor at Alliance BJJ. Former wrestler who transitioned to jiu-jitsu with explosive takedowns and relentless top pressure. Competes regularly at IBJJF events.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://robohash.org/matheus-oliveira?size=256x256&set=set5")" \
    --practitioner \
    --posix "$MATHEUS_CREATION_TIME" \
    --output-id)
print_success "Student Matheus created (White belt): $MATHEUS_ID"

print_info "Creating Student Gabriela Mendes..."
GABRIELA_ID=$(run_admin_cmd create-profile-with-rank \
    --name "$(truncate_str $NAME_MAX "Gabriela Mendes")" \
    --description "$(truncate_str $DESC_MAX "Technical blue belt at Alliance BJJ. Background in judo gives her excellent grip fighting and throws. Focused on developing a modern guard game to complement her top game.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://robohash.org/gabriela-mendes?size=256x256&set=set5")" \
    --practitioner \
    --posix "$GABRIELA_CREATION_TIME" \
    --belt Blue \
    --output-id)
print_success "Student Gabriela created (Blue belt): $GABRIELA_ID"

print_info "Creating Student Fernanda Lima..."
FERNANDA_ID=$(run_admin_cmd init-profile \
    --name "$(truncate_str $NAME_MAX "Fernanda Lima")" \
    --description "$(truncate_str $DESC_MAX "Committed white belt at Alliance BJJ. Yoga instructor who brings flexibility and body awareness to her guard game. Training five days a week with rapid improvement.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://robohash.org/fernanda-lima?size=256x256&set=set5")" \
    --practitioner \
    --posix "$FERNANDA_CREATION_TIME" \
    --output-id)
print_success "Student Fernanda created (White belt): $FERNANDA_ID"

print_info "Creating Student Bruno Santos..."
BRUNO_ID=$(run_admin_cmd init-profile \
    --name "$(truncate_str $NAME_MAX "Bruno Santos")" \
    --description "$(truncate_str $DESC_MAX "Enthusiastic white belt at Alliance BJJ. Crossfit athlete who started training to improve his ground game. Shows natural aptitude for scrambles and passing.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://robohash.org/bruno-santos?size=256x256&set=set5")" \
    --practitioner \
    --posix "$BRUNO_CREATION_TIME" \
    --output-id)
print_success "Student Bruno created (White belt): $BRUNO_ID"

print_info "Creating Student Lucas Ferreira..."
LUCAS_ID=$(run_admin_cmd create-profile-with-rank \
    --name "$(truncate_str $NAME_MAX "Lucas Ferreira")" \
    --description "$(truncate_str $DESC_MAX "Experienced blue belt at Gracie Barra. Passionate about the self-defense aspects of BJJ. Trains 4 days a week and has been competing at local tournaments since 2022.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://robohash.org/lucas-ferreira?size=256x256&set=set5")" \
    --practitioner \
    --posix "$LUCAS_CREATION_TIME" \
    --belt Blue \
    --output-id)
print_success "Student Lucas created (Blue belt): $LUCAS_ID"

print_info "Creating Student Carolina Rocha..."
CAROLINA_ID=$(run_admin_cmd create-profile-with-rank \
    --name "$(truncate_str $NAME_MAX "Carolina Rocha")" \
    --description "$(truncate_str $DESC_MAX "Dedicated purple belt at Gracie Barra. Former competitive swimmer who found her passion in jiu-jitsu. Known for her creative guard game and strong mental toughness in competition.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://robohash.org/carolina-rocha?size=256x256&set=set5")" \
    --practitioner \
    --posix "$CAROLINA_CREATION_TIME" \
    --belt Purple \
    --output-id)
print_success "Student Carolina created (Purple belt): $CAROLINA_ID"

print_info "Creating Student Rafael Costa..."
RAFAEL_ID=$(run_admin_cmd init-profile \
    --name "$(truncate_str $NAME_MAX "Rafael Costa")" \
    --description "$(truncate_str $DESC_MAX "Enthusiastic white belt at Alliance BJJ. Software engineer who started training to build discipline and stay active. Shows strong analytical approach to learning techniques.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://robohash.org/rafael-costa?size=256x256&set=set5")" \
    --practitioner \
    --posix "$RAFAEL_CREATION_TIME" \
    --output-id)
print_success "Student Rafael created (White belt): $RAFAEL_ID"

# ============================================================================
# STEP 4: Promotions (Recent - Last 12 Months)
# ============================================================================
print_section "Step 4: Recent Promotions"

# --- Promotion 1: Matheus White -> Blue (June 2025, accepted) ---
print_subsection "Promotion 1: Matheus White -> Blue (June 2025)"
print_info "Professor Ana promoting Matheus to Blue belt..."

PROMO_MATHEUS_BLUE_ID=$(run_admin_cmd promote-profile \
    --promoted-profile-id "$MATHEUS_ID" \
    --promoted-by-profile-id "$ANA_ID" \
    --posix "$MATHEUS_BLUE_TIME" \
    --belt Blue \
    --output-id)
print_success "Blue belt promotion created: $PROMO_MATHEUS_BLUE_ID"

print_info "Matheus accepting Blue belt promotion..."
run_admin_cmd_no_output accept-promotion --asset-class "$PROMO_MATHEUS_BLUE_ID"
print_success "Matheus is now a Blue belt!"

# --- Promotion 2: Gabriela Blue -> Purple (September 2025, accepted) ---
print_subsection "Promotion 2: Gabriela Blue -> Purple (September 2025)"
print_info "Professor Ana promoting Gabriela to Purple belt..."

PROMO_GABRIELA_PURPLE_ID=$(run_admin_cmd promote-profile \
    --promoted-profile-id "$GABRIELA_ID" \
    --promoted-by-profile-id "$ANA_ID" \
    --posix "$GABRIELA_PURPLE_TIME" \
    --belt Purple \
    --output-id)
print_success "Purple belt promotion created: $PROMO_GABRIELA_PURPLE_ID"

print_info "Gabriela accepting Purple belt promotion..."
run_admin_cmd_no_output accept-promotion --asset-class "$PROMO_GABRIELA_PURPLE_ID"
print_success "Gabriela is now a Purple belt!"

# --- Promotion 3: Matheus Blue -> Purple (January 2026, accepted) ---
print_subsection "Promotion 3: Matheus Blue -> Purple (January 2026)"
print_info "Professor Ana promoting Matheus to Purple belt..."

PROMO_MATHEUS_PURPLE_ID=$(run_admin_cmd promote-profile \
    --promoted-profile-id "$MATHEUS_ID" \
    --promoted-by-profile-id "$ANA_ID" \
    --posix "$MATHEUS_PURPLE_TIME" \
    --belt Purple \
    --output-id)
print_success "Purple belt promotion created: $PROMO_MATHEUS_PURPLE_ID"

print_info "Matheus accepting Purple belt promotion..."
run_admin_cmd_no_output accept-promotion --asset-class "$PROMO_MATHEUS_PURPLE_ID"
print_success "Matheus is now a Purple belt!"

# --- Promotion 4: Fernanda White -> Blue (February 2026, accepted) ---
print_subsection "Promotion 4: Fernanda White -> Blue (February 2026)"
print_info "Professor Ana promoting Fernanda to Blue belt..."

PROMO_FERNANDA_BLUE_ID=$(run_admin_cmd promote-profile \
    --promoted-profile-id "$FERNANDA_ID" \
    --promoted-by-profile-id "$ANA_ID" \
    --posix "$FERNANDA_BLUE_TIME" \
    --belt Blue \
    --output-id)
print_success "Blue belt promotion created: $PROMO_FERNANDA_BLUE_ID"

print_info "Fernanda accepting Blue belt promotion..."
run_admin_cmd_no_output accept-promotion --asset-class "$PROMO_FERNANDA_BLUE_ID"
print_success "Fernanda is now a Blue belt!"

# --- Promotion 5: Bruno White -> Blue (March 2026, PENDING) ---
print_subsection "Promotion 5 (PENDING): Bruno White -> Blue (March 2026)"
print_info "Professor Ana promoting Bruno to Blue belt (will remain pending)..."

PROMO_BRUNO_BLUE_ID=$(run_admin_cmd promote-profile \
    --promoted-profile-id "$BRUNO_ID" \
    --promoted-by-profile-id "$ANA_ID" \
    --posix "$BRUNO_BLUE_TIME" \
    --belt Blue \
    --output-id)
print_success "Pending Blue belt promotion created: $PROMO_BRUNO_BLUE_ID"
print_warning "Bruno has NOT accepted this promotion (pending on-chain)"

# --- Promotion 6: Carlos promotes Matheus to Blue (July 2025, SUPERSEDED) ---
# Matheus accepted Ana's Blue promotion and later advanced to Purple.
# Carlos's unaccepted Blue proposal is now superseded (current Purple > proposed Blue).
print_subsection "Promotion 6 (SUPERSEDED): Carlos promotes Matheus to Blue (July 2025)"
print_info "Professor Carlos also promoting Matheus to Blue belt..."

PROMO_CARLOS_MATHEUS_BLUE_ID=$(run_admin_cmd promote-profile \
    --promoted-profile-id "$MATHEUS_ID" \
    --promoted-by-profile-id "$CARLOS_ID" \
    --posix "$CARLOS_MATHEUS_BLUE_TIME" \
    --belt Blue \
    --output-id)
print_success "Superseded Blue belt promotion created: $PROMO_CARLOS_MATHEUS_BLUE_ID"
print_warning "Matheus never accepted Carlos's promotion (superseded — he is already Purple)"

# --- Promotion 7: Carlos promotes Gabriela to Blue (August 2025, SUPERSEDED) ---
# Gabriela accepted Ana's Purple promotion, so Carlos's Blue proposal is superseded.
print_subsection "Promotion 7 (SUPERSEDED): Carlos promotes Gabriela to Blue (August 2025)"
print_info "Professor Carlos promoting Gabriela to Blue belt..."

PROMO_CARLOS_GABRIELA_BLUE_ID=$(run_admin_cmd promote-profile \
    --promoted-profile-id "$GABRIELA_ID" \
    --promoted-by-profile-id "$CARLOS_ID" \
    --posix "$CARLOS_GABRIELA_BLUE_TIME" \
    --belt Blue \
    --output-id)
print_success "Superseded Blue belt promotion created: $PROMO_CARLOS_GABRIELA_BLUE_ID"
print_warning "Gabriela never accepted Carlos's promotion (superseded — she is already Purple)"

# --- Promotion 8: Carlos promotes Carolina to Blue (April 2025, SUPERSEDED) ---
# Carolina holds Purple via create-profile-with-rank, so a Blue proposal is superseded.
print_subsection "Promotion 8 (SUPERSEDED): Carlos promotes Carolina to Blue (April 2025)"
print_info "Professor Carlos promoting Carolina to Blue belt..."

PROMO_CARLOS_CAROLINA_BLUE_ID=$(run_admin_cmd promote-profile \
    --promoted-profile-id "$CAROLINA_ID" \
    --promoted-by-profile-id "$CARLOS_ID" \
    --posix "$CARLOS_CAROLINA_BLUE_TIME" \
    --belt Blue \
    --output-id)
print_success "Superseded Blue belt promotion created: $PROMO_CARLOS_CAROLINA_BLUE_ID"
print_warning "Carolina never accepted Carlos's promotion (superseded — she is already Purple)"

# ============================================================================
# STEP 5: Memberships (Recent Activity)
# ============================================================================
print_section "Step 5: Recent Membership Activity"

# --- Professor Ana at Alliance (instructor, long-running) ---
print_subsection "Membership 1: Professor Ana at Alliance (instructor)"
print_info "Creating membership history (Professor Ana at Alliance)..."
MEMBERSHIP_ANA_ID=$(run_admin_cmd create-membership-history \
    --org-profile-id "$ALLIANCE_ID" \
    --practitioner-profile-id "$ANA_ID" \
    --posix "$ANA_CREATION_TIME" \
    --output-id)
print_success "Membership history created: $MEMBERSHIP_ANA_ID"

# --- Matheus at Alliance (existing member, renewed recently) ---
print_subsection "Membership 2: Matheus at Alliance (renewed 2025)"
print_info "Creating membership history (Matheus at Alliance, 2022-2025)..."
MEMBERSHIP_MATHEUS_ID=$(run_admin_cmd create-membership-history \
    --org-profile-id "$ALLIANCE_ID" \
    --practitioner-profile-id "$MATHEUS_ID" \
    --posix "$MEMBERSHIP_OLD_START" \
    --end-posix "$MEMBERSHIP_OLD_END" \
    --output-id)
print_success "Membership history created: $MEMBERSHIP_MATHEUS_ID"

print_info "Matheus accepting first membership interval..."
FIRST_INTERVAL_MATHEUS_ID=$(run_admin_cmd get-first-interval-id --membership-node-id "$MEMBERSHIP_MATHEUS_ID")
run_admin_cmd_no_output accept-membership-interval --interval-id "$FIRST_INTERVAL_MATHEUS_ID"
print_success "First interval accepted!"

print_info "Adding renewal interval (2025-06 to 2026-06)..."
INTERVAL_MATHEUS_RENEWAL_ID=$(run_admin_cmd add-membership-interval \
    --org-profile-id "$ALLIANCE_ID" \
    --membership-node-id "$MEMBERSHIP_MATHEUS_ID" \
    --posix "$MEMBERSHIP_OLD_END" \
    --end-posix "$MEMBERSHIP_RENEWAL_END" \
    --output-id)
print_success "Renewal interval created: $INTERVAL_MATHEUS_RENEWAL_ID"

print_info "Matheus accepting renewal..."
run_admin_cmd_no_output accept-membership-interval --interval-id "$INTERVAL_MATHEUS_RENEWAL_ID"
print_success "Matheus's membership renewed through June 2026!"

# --- Gabriela at Alliance (existing member, renewed recently) ---
print_subsection "Membership 3: Gabriela at Alliance (renewed 2025)"
GABRIELA_OLD_START=1654041600000   # 2022-06-01
GABRIELA_OLD_END=1748736000000     # 2025-06-01

print_info "Creating membership history (Gabriela at Alliance, 2022-2025)..."
MEMBERSHIP_GABRIELA_ID=$(run_admin_cmd create-membership-history \
    --org-profile-id "$ALLIANCE_ID" \
    --practitioner-profile-id "$GABRIELA_ID" \
    --posix "$GABRIELA_OLD_START" \
    --end-posix "$GABRIELA_OLD_END" \
    --output-id)
print_success "Membership history created: $MEMBERSHIP_GABRIELA_ID"

print_info "Gabriela accepting first membership interval..."
FIRST_INTERVAL_GABRIELA_ID=$(run_admin_cmd get-first-interval-id --membership-node-id "$MEMBERSHIP_GABRIELA_ID")
run_admin_cmd_no_output accept-membership-interval --interval-id "$FIRST_INTERVAL_GABRIELA_ID"
print_success "First interval accepted!"

print_info "Adding renewal interval (2025-06 to 2026-06)..."
INTERVAL_GABRIELA_RENEWAL_ID=$(run_admin_cmd add-membership-interval \
    --org-profile-id "$ALLIANCE_ID" \
    --membership-node-id "$MEMBERSHIP_GABRIELA_ID" \
    --posix "$GABRIELA_OLD_END" \
    --end-posix "$MEMBERSHIP_RENEWAL_END" \
    --output-id)
print_success "Renewal interval created: $INTERVAL_GABRIELA_RENEWAL_ID"

print_info "Gabriela accepting renewal..."
run_admin_cmd_no_output accept-membership-interval --interval-id "$INTERVAL_GABRIELA_RENEWAL_ID"
print_success "Gabriela's membership renewed through June 2026!"

# --- Fernanda at Alliance (new member, joined 2025) ---
print_subsection "Membership 4: Fernanda at Alliance (new member 2025)"
print_info "Creating membership history (Fernanda at Alliance, 2025-2026)..."
MEMBERSHIP_FERNANDA_ID=$(run_admin_cmd create-membership-history \
    --org-profile-id "$ALLIANCE_ID" \
    --practitioner-profile-id "$FERNANDA_ID" \
    --posix "$MEMBERSHIP_NEW_START" \
    --end-posix "$MEMBERSHIP_NEW_END" \
    --output-id)
print_success "Membership history created: $MEMBERSHIP_FERNANDA_ID"

print_info "Fernanda accepting membership interval..."
FIRST_INTERVAL_FERNANDA_ID=$(run_admin_cmd get-first-interval-id --membership-node-id "$MEMBERSHIP_FERNANDA_ID")
run_admin_cmd_no_output accept-membership-interval --interval-id "$FIRST_INTERVAL_FERNANDA_ID"
print_success "Fernanda's membership accepted!"

# --- Bruno at Alliance (new member, joined 2025, pending acceptance) ---
print_subsection "Membership 5: Bruno at Alliance (new member 2025)"
print_info "Creating membership history (Bruno at Alliance, 2025-2026)..."
MEMBERSHIP_BRUNO_ID=$(run_admin_cmd create-membership-history \
    --org-profile-id "$ALLIANCE_ID" \
    --practitioner-profile-id "$BRUNO_ID" \
    --posix "$MEMBERSHIP_NEW_START" \
    --end-posix "$MEMBERSHIP_NEW_END" \
    --output-id)
print_success "Membership history created: $MEMBERSHIP_BRUNO_ID"
print_warning "Bruno has NOT accepted this membership (pending on-chain)"

# --- Lucas at Gracie Barra (long-running member, accepted) ---
print_subsection "Membership 6: Lucas at Gracie Barra (active since 2021)"
print_info "Creating membership history (Lucas at Gracie Barra, 2021-2026)..."
MEMBERSHIP_LUCAS_ID=$(run_admin_cmd create-membership-history \
    --org-profile-id "$GRACIE_BARRA_ID" \
    --practitioner-profile-id "$LUCAS_ID" \
    --posix "$LUCAS_MEMBERSHIP_START" \
    --end-posix "$LUCAS_MEMBERSHIP_END" \
    --output-id)
print_success "Membership history created: $MEMBERSHIP_LUCAS_ID"

print_info "Lucas accepting membership interval..."
FIRST_INTERVAL_LUCAS_ID=$(run_admin_cmd get-first-interval-id --membership-node-id "$MEMBERSHIP_LUCAS_ID")
run_admin_cmd_no_output accept-membership-interval --interval-id "$FIRST_INTERVAL_LUCAS_ID"
print_success "Lucas's membership accepted!"

# --- Carolina at Gracie Barra (existing member, renewed recently) ---
print_subsection "Membership 7: Carolina at Gracie Barra (renewed 2025)"
print_info "Creating membership history (Carolina at Gracie Barra, 2020-2025)..."
MEMBERSHIP_CAROLINA_ID=$(run_admin_cmd create-membership-history \
    --org-profile-id "$GRACIE_BARRA_ID" \
    --practitioner-profile-id "$CAROLINA_ID" \
    --posix "$CAROLINA_MEMBERSHIP_START" \
    --end-posix "$CAROLINA_MEMBERSHIP_OLD_END" \
    --output-id)
print_success "Membership history created: $MEMBERSHIP_CAROLINA_ID"

print_info "Carolina accepting first membership interval..."
FIRST_INTERVAL_CAROLINA_ID=$(run_admin_cmd get-first-interval-id --membership-node-id "$MEMBERSHIP_CAROLINA_ID")
run_admin_cmd_no_output accept-membership-interval --interval-id "$FIRST_INTERVAL_CAROLINA_ID"
print_success "First interval accepted!"

print_info "Adding renewal interval (2025-06 to 2026-06)..."
INTERVAL_CAROLINA_RENEWAL_ID=$(run_admin_cmd add-membership-interval \
    --org-profile-id "$GRACIE_BARRA_ID" \
    --membership-node-id "$MEMBERSHIP_CAROLINA_ID" \
    --posix "$CAROLINA_MEMBERSHIP_OLD_END" \
    --end-posix "$MEMBERSHIP_RENEWAL_END" \
    --output-id)
print_success "Renewal interval created: $INTERVAL_CAROLINA_RENEWAL_ID"

print_info "Carolina accepting renewal..."
run_admin_cmd_no_output accept-membership-interval --interval-id "$INTERVAL_CAROLINA_RENEWAL_ID"
print_success "Carolina's membership renewed through June 2026!"

# --- Rafael at Alliance (new member, accepted) ---
print_subsection "Membership 8: Rafael at Alliance (active since 2024)"
print_info "Creating membership history (Rafael at Alliance, 2024-2026)..."
MEMBERSHIP_RAFAEL_ID=$(run_admin_cmd create-membership-history \
    --org-profile-id "$ALLIANCE_ID" \
    --practitioner-profile-id "$RAFAEL_ID" \
    --posix "$RAFAEL_MEMBERSHIP_START" \
    --end-posix "$RAFAEL_MEMBERSHIP_END" \
    --output-id)
print_success "Membership history created: $MEMBERSHIP_RAFAEL_ID"

print_info "Rafael accepting membership interval..."
FIRST_INTERVAL_RAFAEL_ID=$(run_admin_cmd get-first-interval-id --membership-node-id "$MEMBERSHIP_RAFAEL_ID")
run_admin_cmd_no_output accept-membership-interval --interval-id "$FIRST_INTERVAL_RAFAEL_ID"
print_success "Rafael's membership accepted!"

# ============================================================================
# STEP 6: Achievements (Recent - Last 12 Months)
# ============================================================================
print_section "Step 6: Recent Achievements"

# --- Achievement 1: Matheus - IBJJF Silver Medal (May 2025) ---
print_subsection "Achievement 1: Matheus - IBJJF World Championship Silver (May 2025)"
print_info "Alliance awarding IBJJF silver medal to Matheus..."
ACHIEVEMENT_MATHEUS_SILVER_ID=$(run_admin_cmd award-achievement \
    --awarded-to-profile-id "$MATHEUS_ID" \
    --awarded-by-profile-id "$ALLIANCE_ID" \
    --name "$(truncate_str $NAME_MAX "Silver Medal - IBJJF World Championship 2025")" \
    --description "$(truncate_str $DESC_MAX "Won silver medal in the blue belt middleweight division at the 2025 IBJJF World Jiu-Jitsu Championship. Fought through a tough bracket of 32 competitors with 4 submissions before losing a close final by advantages.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://ui-avatars.com/api/?name=Silver+Medal&size=256&background=c0c0c0&color=000&bold=true&format=png")" \
    --posix "$ACHIEVEMENT_MATHEUS_SILVER_TIME" \
    --output-id)
print_success "Achievement awarded: $ACHIEVEMENT_MATHEUS_SILVER_ID"

print_info "Matheus accepting silver medal achievement..."
run_admin_cmd_no_output accept-achievement --achievement-id "$ACHIEVEMENT_MATHEUS_SILVER_ID"
print_success "Matheus accepted IBJJF silver medal!"

# --- Achievement 2: Gabriela - Pan American Gold Medal (July 2025) ---
print_subsection "Achievement 2: Gabriela - Pan American Gold (July 2025)"
print_info "Alliance awarding Pan Am gold medal to Gabriela..."
ACHIEVEMENT_GABRIELA_GOLD_ID=$(run_admin_cmd award-achievement \
    --awarded-to-profile-id "$GABRIELA_ID" \
    --awarded-by-profile-id "$ALLIANCE_ID" \
    --name "$(truncate_str $NAME_MAX "Gold Medal - Pan American Championship 2025")" \
    --description "$(truncate_str $DESC_MAX "Won gold medal at the 2025 Pan American BJJ Championship in the purple belt featherweight division. Dominant performance with three submissions in four matches including an armbar in the final.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://ui-avatars.com/api/?name=Gold+Pan+Am&size=256&background=ffd700&color=000&bold=true&format=png")" \
    --posix "$ACHIEVEMENT_GABRIELA_GOLD_TIME" \
    --output-id)
print_success "Achievement awarded: $ACHIEVEMENT_GABRIELA_GOLD_ID"

print_info "Gabriela accepting gold medal achievement..."
run_admin_cmd_no_output accept-achievement --achievement-id "$ACHIEVEMENT_GABRIELA_GOLD_ID"
print_success "Gabriela accepted Pan Am gold medal!"

# --- Achievement 3: Fernanda - Fundamentals Course Completion (November 2025) ---
print_subsection "Achievement 3: Fernanda - Fundamentals Course (November 2025)"
print_info "Professor Ana awarding fundamentals completion to Fernanda..."
ACHIEVEMENT_FERNANDA_COURSE_ID=$(run_admin_cmd award-achievement \
    --awarded-to-profile-id "$FERNANDA_ID" \
    --awarded-by-profile-id "$ANA_ID" \
    --name "$(truncate_str $NAME_MAX "Fundamentals Program Completion")" \
    --description "$(truncate_str $DESC_MAX "Successfully completed the Alliance BJJ 6-month fundamentals program. Demonstrated proficiency in all core positions, basic submissions, and self-defense techniques. Passed the written and practical examination.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://ui-avatars.com/api/?name=Fundamentals&size=256&background=2ecc71&color=fff&bold=true&format=png")" \
    --posix "$ACHIEVEMENT_FERNANDA_COURSE_TIME" \
    --output-id)
print_success "Achievement awarded: $ACHIEVEMENT_FERNANDA_COURSE_ID"

print_info "Fernanda accepting fundamentals completion..."
run_admin_cmd_no_output accept-achievement --achievement-id "$ACHIEVEMENT_FERNANDA_COURSE_ID"
print_success "Fernanda accepted fundamentals completion!"

# --- Achievement 4: Matheus - Outstanding Student of the Year (December 2025) ---
print_subsection "Achievement 4: Matheus - Outstanding Student 2025 (December 2025)"
print_info "Alliance awarding Outstanding Student to Matheus..."
ACHIEVEMENT_MATHEUS_STUDENT_ID=$(run_admin_cmd award-achievement \
    --awarded-to-profile-id "$MATHEUS_ID" \
    --awarded-by-profile-id "$ALLIANCE_ID" \
    --name "$(truncate_str $NAME_MAX "Outstanding Student of the Year 2025")" \
    --description "$(truncate_str $DESC_MAX "Recognized as Alliance BJJ Academy's Outstanding Student for 2025. Demonstrated exceptional dedication with over 300 training sessions, consistent competition performance, and mentoring newer students.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://ui-avatars.com/api/?name=Student+Award&size=256&background=9b59b6&color=fff&bold=true&format=png")" \
    --posix "$ACHIEVEMENT_MATHEUS_STUDENT_TIME" \
    --output-id)
print_success "Achievement awarded: $ACHIEVEMENT_MATHEUS_STUDENT_ID"

print_info "Matheus accepting Outstanding Student award..."
run_admin_cmd_no_output accept-achievement --achievement-id "$ACHIEVEMENT_MATHEUS_STUDENT_ID"
print_success "Matheus accepted Outstanding Student award!"

# --- Achievement 5: Bruno - 100 Classes Milestone (March 2026, PENDING) ---
print_subsection "Achievement 5 (PENDING): Bruno - 100 Classes Milestone (March 2026)"
print_info "Alliance awarding 100 Classes milestone to Bruno..."
ACHIEVEMENT_BRUNO_CLASSES_ID=$(run_admin_cmd award-achievement \
    --awarded-to-profile-id "$BRUNO_ID" \
    --awarded-by-profile-id "$ALLIANCE_ID" \
    --name "$(truncate_str $NAME_MAX "100 Classes Milestone")" \
    --description "$(truncate_str $DESC_MAX "Reached the milestone of 100 training sessions at Alliance BJJ Academy. Consistent attendance and steady progress from beginner to advanced fundamentals level.")" \
    --image-uri "$(truncate_str $IMAGE_MAX "https://ui-avatars.com/api/?name=100+Classes&size=256&background=e67e22&color=fff&bold=true&format=png")" \
    --posix "$ACHIEVEMENT_BRUNO_CLASSES_TIME" \
    --output-id)
print_success "Achievement awarded: $ACHIEVEMENT_BRUNO_CLASSES_ID"
print_warning "Bruno has NOT accepted this achievement (pending on-chain)"

# ============================================================================
# STEP 7: Dust Cleanup
# ============================================================================
print_section "Step 7: Dust Cleanup"

print_info "Running cleanup-dust to sweep any dust UTxOs from validator addresses..."
cleanup_output=""
cleanup_exit=0
cleanup_output=$(cd "$REPO_ROOT" && $ADMIN cleanup-dust 2>&1) || cleanup_exit=$?
if [ "$cleanup_exit" -eq 0 ]; then
    print_success "Dust cleanup completed"
else
    if echo "$cleanup_output" | grep -q "No dust UTxOs found"; then
        print_info "No dust UTxOs found at validator addresses (clean state)"
    else
        print_warning "Dust cleanup returned exit code $cleanup_exit"
    fi
fi

# ============================================================================
# Summary
# ============================================================================
print_section "Recent Activity Population Complete!"

echo ""
echo -e "${GREEN}Organizations:${NC}"
echo -e "  • Alliance BJJ Academy:  ${CYAN}$ALLIANCE_ID${NC}"
echo -e "  • Gracie Barra Academy:  ${CYAN}$GRACIE_BARRA_ID${NC}"
echo ""
echo -e "${GREEN}Masters:${NC}"
echo -e "  • Professor Ana Silva (Black):    ${CYAN}$ANA_ID${NC}"
echo -e "  • Professor Carlos Machado (Black): ${CYAN}$CARLOS_ID${NC}"
echo ""
echo -e "${GREEN}Students:${NC}"
echo -e "  • Matheus Oliveira (Purple):  ${CYAN}$MATHEUS_ID${NC}"
echo -e "  • Gabriela Mendes (Purple):   ${CYAN}$GABRIELA_ID${NC}"
echo -e "  • Fernanda Lima (Blue):       ${CYAN}$FERNANDA_ID${NC}"
echo -e "  • Bruno Santos (White):       ${CYAN}$BRUNO_ID${NC}"
echo -e "  • Lucas Ferreira (Blue):      ${CYAN}$LUCAS_ID${NC}"
echo -e "  • Carolina Rocha (Purple):    ${CYAN}$CAROLINA_ID${NC}"
echo -e "  • Rafael Costa (White):       ${CYAN}$RAFAEL_ID${NC}"
echo ""
echo -e "${GREEN}Accepted Promotions (recent):${NC}"
echo -e "  1. Matheus: White -> Blue (June 2025)"
echo -e "  2. Gabriela: Blue -> Purple (September 2025)"
echo -e "  3. Matheus: Blue -> Purple (January 2026)"
echo -e "  4. Fernanda: White -> Blue (February 2026)"
echo ""
echo -e "${YELLOW}Pending Promotions:${NC}"
echo -e "  5. Bruno: White -> Blue (March 2026, pending): ${CYAN}$PROMO_BRUNO_BLUE_ID${NC}"
echo ""
echo -e "${RED}Superseded Promotions:${NC}"
echo -e "  6. Carlos promotes Matheus to Blue (July 2025):    ${CYAN}$PROMO_CARLOS_MATHEUS_BLUE_ID${NC}"
echo -e "  7. Carlos promotes Gabriela to Blue (August 2025): ${CYAN}$PROMO_CARLOS_GABRIELA_BLUE_ID${NC}"
echo -e "  8. Carlos promotes Carolina to Blue (April 2025):  ${CYAN}$PROMO_CARLOS_CAROLINA_BLUE_ID${NC}"
echo ""
echo -e "${GREEN}Memberships:${NC}"
echo -e "  • Professor Ana at Alliance:  history ${CYAN}$MEMBERSHIP_ANA_ID${NC}"
echo -e "  • Matheus at Alliance:        history ${CYAN}$MEMBERSHIP_MATHEUS_ID${NC}, renewed ${CYAN}$INTERVAL_MATHEUS_RENEWAL_ID${NC}"
echo -e "  • Gabriela at Alliance:       history ${CYAN}$MEMBERSHIP_GABRIELA_ID${NC}, renewed ${CYAN}$INTERVAL_GABRIELA_RENEWAL_ID${NC}"
echo -e "  • Fernanda at Alliance:       history ${CYAN}$MEMBERSHIP_FERNANDA_ID${NC} (accepted)"
echo -e "  • Bruno at Alliance:          history ${CYAN}$MEMBERSHIP_BRUNO_ID${NC} (pending)"
echo -e "  • Lucas at Gracie Barra:      history ${CYAN}$MEMBERSHIP_LUCAS_ID${NC} (accepted)"
echo -e "  • Carolina at Gracie Barra:   history ${CYAN}$MEMBERSHIP_CAROLINA_ID${NC}, renewed ${CYAN}$INTERVAL_CAROLINA_RENEWAL_ID${NC}"
echo -e "  • Rafael at Alliance:         history ${CYAN}$MEMBERSHIP_RAFAEL_ID${NC} (accepted)"
echo ""
echo -e "${GREEN}Accepted Achievements (recent):${NC}"
echo -e "  • Matheus: IBJJF Silver Medal (May 2025):      ${CYAN}$ACHIEVEMENT_MATHEUS_SILVER_ID${NC}"
echo -e "  • Gabriela: Pan Am Gold Medal (July 2025):      ${CYAN}$ACHIEVEMENT_GABRIELA_GOLD_ID${NC}"
echo -e "  • Fernanda: Fundamentals Course (Nov 2025):     ${CYAN}$ACHIEVEMENT_FERNANDA_COURSE_ID${NC}"
echo -e "  • Matheus: Outstanding Student (Dec 2025):      ${CYAN}$ACHIEVEMENT_MATHEUS_STUDENT_ID${NC}"
echo ""
echo -e "${YELLOW}Pending Achievements:${NC}"
echo -e "  • Bruno: 100 Classes Milestone (Mar 2026):      ${CYAN}$ACHIEVEMENT_BRUNO_CLASSES_ID${NC}"
echo ""

print_success "Recent activity successfully added to testnet!"
print_info "You can now query these profiles, promotions, memberships, and achievements via the Query API."
