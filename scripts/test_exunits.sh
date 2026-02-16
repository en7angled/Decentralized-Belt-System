#!/usr/bin/env bash
# Script to run cabal test and extract exUnits (memory and steps) for each interaction
# Displays results in a formatted table with totals

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
BOLD='\033[1m'
NC='\033[0m' # No Color

# Run cabal test and capture output (do not exit on failure so we always print script sizes)
echo -e "${CYAN}Running cabal test...${NC}"
TEST_OUTPUT=$(cabal test 2>&1) || true

# Check if tests passed
if echo "$TEST_OUTPUT" | grep -q "All.*tests passed\|Test suite.*PASS"; then
    echo -e "${GREEN}Tests passed!${NC}\n"
else
    if echo "$TEST_OUTPUT" | grep -q "FAIL"; then
        echo -e "${RED}Some tests failed!${NC}\n"
    fi
fi

# Format numbers with thousand separators
format_number() {
    printf "%'d" "$1" 2>/dev/null || echo "$1"
}

# Create temp files to store parsed data
TEMP_FILE=$(mktemp)
SIZE_FILE=$(mktemp)
trap "rm -f $TEMP_FILE $SIZE_FILE" EXIT

# ── Script Sizes ──────────────────────────────────────────────────────────────
# Parse SCRIPT_SIZES block from test output
# Maximum reference script size: 16,384 bytes (Cardano max transaction size)
MAX_SCRIPT_SIZE=16384

echo "$TEST_OUTPUT" | perl -e '
use strict;
use warnings;
local $/;
my $text = <STDIN>;
$text =~ s/\e\[[0-9;]*m//g;  # Remove ANSI codes

while ($text =~ /SCRIPT_SIZES:\s*(.*?)(?=\n\S|\z)/sg) {
    my $block = $1;
    while ($block =~ /^\s*(\w+):\s*(\d+)\s*bytes/mg) {
        print "$1|$2\n";
    }
}
' | sort -u > "$SIZE_FILE"

if [ -s "$SIZE_FILE" ]; then
    echo -e "${BOLD}${CYAN}Deployed Script Sizes:${NC}"
    printf "${YELLOW}+-%-24s-+-%-12s-+-%-16s-+${NC}\n" "------------------------" "------------" "----------------"
    printf "${YELLOW}|${NC} ${CYAN}%-24s${NC} ${YELLOW}|${NC} ${CYAN}%-12s${NC} ${YELLOW}|${NC} ${CYAN}%-16s${NC} ${YELLOW}|${NC}\n" "Script" "Size (bytes)" "% of Max (16 KB)"
    printf "${YELLOW}+-%-24s-+-%-12s-+-%-16s-+${NC}\n" "------------------------" "------------" "----------------"

    TOTAL_SIZE=0
    while IFS='|' read -r name size; do
        size_formatted=$(format_number "$size")
        pct=$(awk "BEGIN {printf \"%.1f\", ($size / $MAX_SCRIPT_SIZE) * 100}")
        TOTAL_SIZE=$((TOTAL_SIZE + size))

        # Color code based on size percentage
        if (( $(echo "$pct > 80" | bc -l) )); then
            pct_color="${RED}"
        elif (( $(echo "$pct > 50" | bc -l) )); then
            pct_color="${YELLOW}"
        else
            pct_color="${GREEN}"
        fi

        printf "${YELLOW}|${NC} %-24s ${YELLOW}|${NC} %12s ${YELLOW}|${NC} ${pct_color}%15s%%${NC} ${YELLOW}|${NC}\n" "$name" "$size_formatted" "$pct"
    done < "$SIZE_FILE"

    printf "${YELLOW}+-%-24s-+-%-12s-+-%-16s-+${NC}\n" "------------------------" "------------" "----------------"
    total_formatted=$(format_number "$TOTAL_SIZE")
    printf "${YELLOW}|${NC} ${GREEN}%-24s${NC} ${YELLOW}|${NC} %12s ${YELLOW}|${NC} %16s ${YELLOW}|${NC}\n" "TOTAL" "$total_formatted" ""
    printf "${YELLOW}+-%-24s-+-%-12s-+-%-16s-+${NC}\n" "------------------------" "------------" "----------------"
    echo ""
fi

# Parse the output to extract interactions and their exUnits
# Use perl for robust multi-line regex matching
echo "$TEST_OUTPUT" | perl -e '
use strict;
use warnings;

# Slurp all input
local $/;
my $text = <STDIN>;

# Remove ANSI escape codes
$text =~ s/\e\[[0-9;]*m//g;

# Find all INTERACTION blocks followed by TX BUDGET
my @results;
my $total_mem = 0;
my $total_steps = 0;
my $total_fee = 0;
my $count = 0;

# Split by INTERACTION markers (matches "INTERACTION:", "ADMIN INTERACTION:", "PROTOCOL INTERACTION:")
my @blocks = split(/(?:ADMIN |PROTOCOL )?INTERACTION:/, $text);
shift @blocks;  # Remove text before first INTERACTION

foreach my $block (@blocks) {
    $count++;
    
    # Extract action category and specific action from the interaction description
    my $category = "Unknown";
    my $action = "Unknown";
    if ($block =~ /ProfileAction/) {
        $category = "Profile";
    } elsif ($block =~ /AdminAction/) {
        $category = "Admin";
    } elsif ($block =~ /ProtocolAction/) {
        $category = "Protocol";
    }

    if ($block =~ /CreateProfileWithRankAction/) {
        $action = "CreateProfileWithRank";
    } elsif ($block =~ /InitProfileAction/) {
        $action = "InitProfile";
    } elsif ($block =~ /PromoteProfileAction/) {
        $action = "PromoteProfile";
    } elsif ($block =~ /AcceptPromotionAction/) {
        $action = "AcceptPromotion";
    } elsif ($block =~ /UpdateProfileAction/) {
        $action = "UpdateProfile";
    } elsif ($block =~ /CreateMembershipHistoryAction/) {
        $action = "CreateMembershipHistory";
    } elsif ($block =~ /AddMembershipIntervalAction/) {
        $action = "AddMembershipInterval";
    } elsif ($block =~ /AcceptMembershipIntervalAction/) {
        $action = "AcceptMembershipInterval";
    } elsif ($block =~ /UpdateEndDateAction/) {
        $action = "UpdateEndDate";
    } elsif ($block =~ /CleanupDustAction/) {
        $action = "CleanupDust";
    } elsif ($block =~ /PauseProtocolAction/) {
        $action = "PauseProtocol";
    } elsif ($block =~ /UnpauseProtocolAction/) {
        $action = "UnpauseProtocol";
    } elsif ($block =~ /SetFeesAction/) {
        $action = "SetFees";
    }
    
    # Find the TX BUDGET section - ends at TxScriptValidity (end of GYTxBody show output)
    # Using TxScriptValidity as boundary prevents capturing exUnitsMem/Steps values
    # from DebugPlutusFailure error messages that may appear later in the same block
    # (e.g. from mustFail tests whose errors are logged before the next INTERACTION)
    my $budget_section = "";
    if ($block =~ /TX BUDGET:(.*?)TxScriptValidity/s) {
        $budget_section = $1;
    }
    
    # Extract fee from TX BUDGET section (Fee: NNNN lovelace)
    my $fee = 0;
    if ($budget_section =~ /Fee:\s*(\d+)\s*lovelace/) {
        $fee = $1;
    }
    
    # Extract ALL exUnits pairs from this budget section
    my @mem_vals = ($budget_section =~ /exUnitsMem\x27\s*=\s*(\d+)/g);
    my @steps_vals = ($budget_section =~ /exUnitsSteps\x27\s*=\s*(\d+)/g);
    
    my $mem_sum = 0;
    my $steps_sum = 0;
    my $script_count = scalar @mem_vals;
    
    foreach my $m (@mem_vals) {
        $mem_sum += $m;
    }
    foreach my $s (@steps_vals) {
        $steps_sum += $s;
    }
    
    if ($script_count > 0) {
        print "$count|$category|$action|$mem_sum|$steps_sum|$script_count|$fee\n";
        $total_mem += $mem_sum;
        $total_steps += $steps_sum;
        $total_fee += $fee;
    }
}

print "TOTAL|||$total_mem|$total_steps||$total_fee\n";
' > "$TEMP_FILE"

# Mainnet limits: 14,000,000 memory units, 10,000,000,000 CPU steps per transaction
MAX_MEM=14000000
MAX_STEPS=10000000000

# Print the table header
printf "${YELLOW}+-------+----------+--------------------------+---------+--------------------+-----------------------+---------------+-----------------+${NC}\n"
printf "${YELLOW}|${NC} ${CYAN}%5s${NC} ${YELLOW}|${NC} ${CYAN}%-8s${NC} ${YELLOW}|${NC} ${CYAN}%-24s${NC} ${YELLOW}|${NC} ${CYAN}%7s${NC} ${YELLOW}|${NC} ${CYAN}%18s${NC} ${YELLOW}|${NC} ${CYAN}%21s${NC} ${YELLOW}|${NC} ${CYAN}%13s${NC} ${YELLOW}|${NC} ${CYAN}%15s${NC} ${YELLOW}|${NC}\n" "#" "Type" "Action" "Scripts" "exUnitsMem" "exUnitsSteps" "Fee (ADA)" "% of Limit"
printf "${YELLOW}+-------+----------+--------------------------+---------+--------------------+-----------------------+---------------+-----------------+${NC}\n"

# Read and format the data
while IFS='|' read -r num category action mem steps script_count fee; do
    if [ "$num" = "TOTAL" ]; then
        printf "${YELLOW}+-------+----------+--------------------------+---------+--------------------+-----------------------+---------------+-----------------+${NC}\n"
        mem_formatted=$(format_number "$mem")
        steps_formatted=$(format_number "$steps")
        fee_ada=$(awk "BEGIN {printf \"%.4f\", $fee / 1000000}")
        printf "${YELLOW}|${NC} ${GREEN}%-5s${NC} ${YELLOW}|${NC} %8s ${YELLOW}|${NC} %-24s ${YELLOW}|${NC} %7s ${YELLOW}|${NC} %18s ${YELLOW}|${NC} %21s ${YELLOW}|${NC} %13s ${YELLOW}|${NC} %15s ${YELLOW}|${NC}\n" "TOTAL" "" "" "" "$mem_formatted" "$steps_formatted" "$fee_ada" ""
        printf "${YELLOW}+-------+----------+--------------------------+---------+--------------------+-----------------------+---------------+-----------------+${NC}\n"
    else
        # Truncate action name if too long
        if [ ${#action} -gt 24 ]; then
            action="${action:0:21}..."
        fi
        mem_formatted=$(format_number "$mem")
        steps_formatted=$(format_number "$steps")
        fee_ada=$(awk "BEGIN {printf \"%.4f\", $fee / 1000000}")
        
        # Calculate percentage of budget used
        mem_pct=$(awk "BEGIN {printf \"%.1f\", ($mem / $MAX_MEM) * 100}")
        steps_pct=$(awk "BEGIN {printf \"%.1f\", ($steps / $MAX_STEPS) * 100}")
        
        # Color code based on budget usage
        if (( $(echo "$mem_pct > 80" | bc -l) )) || (( $(echo "$steps_pct > 80" | bc -l) )); then
            pct_color="${RED}"
        elif (( $(echo "$mem_pct > 50" | bc -l) )) || (( $(echo "$steps_pct > 50" | bc -l) )); then
            pct_color="${YELLOW}"
        else
            pct_color="${GREEN}"
        fi
        
        pct_display="${mem_pct}%/${steps_pct}%"
        printf "${YELLOW}|${NC} %5s ${YELLOW}|${NC} %-8s ${YELLOW}|${NC} %-24s ${YELLOW}|${NC} %7s ${YELLOW}|${NC} %18s ${YELLOW}|${NC} %21s ${YELLOW}|${NC} %13s ${YELLOW}|${NC} ${pct_color}%15s${NC} ${YELLOW}|${NC}\n" "$num" "$category" "$action" "$script_count" "$mem_formatted" "$steps_formatted" "$fee_ada" "$pct_display"
    fi
done < "$TEMP_FILE"

echo ""

# Generate summary by action type
echo -e "${BOLD}${CYAN}Summary by Action:${NC}"
printf "${YELLOW}+----------+--------------------------+-------+--------------------+-----------------------+---------------+-----------------+${NC}\n"
printf "${YELLOW}|${NC} ${CYAN}%-8s${NC} ${YELLOW}|${NC} ${CYAN}%-24s${NC} ${YELLOW}|${NC} ${CYAN}%5s${NC} ${YELLOW}|${NC} ${CYAN}%18s${NC} ${YELLOW}|${NC} ${CYAN}%21s${NC} ${YELLOW}|${NC} ${CYAN}%13s${NC} ${YELLOW}|${NC} ${CYAN}%15s${NC} ${YELLOW}|${NC}\n" "Type" "Action" "Count" "Avg Memory" "Avg Steps" "Avg Fee" "Avg % Limit"
printf "${YELLOW}+----------+--------------------------+-------+--------------------+-----------------------+---------------+-----------------+${NC}\n"

# Aggregate by category + action type
awk -F'|' '
BEGIN { }
$1 != "TOTAL" && NF >= 7 {
    cat = $2
    action = $3
    mem = $4
    steps = $5
    fee = $7
    key = cat "|" action
    count[key]++
    total_mem[key] += mem
    total_steps[key] += steps
    total_fee[key] += fee
}
END {
    for (key in count) {
        avg_mem = int(total_mem[key] / count[key])
        avg_steps = int(total_steps[key] / count[key])
        avg_fee = int(total_fee[key] / count[key])
        print key "|" count[key] "|" avg_mem "|" avg_steps "|" avg_fee
    }
}' "$TEMP_FILE" | sort -t'|' -k1,2 | while IFS='|' read -r cat action cnt avg_mem avg_steps avg_fee; do
    avg_mem_formatted=$(format_number "$avg_mem")
    avg_steps_formatted=$(format_number "$avg_steps")
    avg_fee_ada=$(awk "BEGIN {printf \"%.4f\", $avg_fee / 1000000}")
    mem_pct=$(awk "BEGIN {printf \"%.1f\", ($avg_mem / $MAX_MEM) * 100}")
    steps_pct=$(awk "BEGIN {printf \"%.1f\", ($avg_steps / $MAX_STEPS) * 100}")
    
    if (( $(echo "$mem_pct > 80" | bc -l) )) || (( $(echo "$steps_pct > 80" | bc -l) )); then
        pct_color="${RED}"
    elif (( $(echo "$mem_pct > 50" | bc -l) )) || (( $(echo "$steps_pct > 50" | bc -l) )); then
        pct_color="${YELLOW}"
    else
        pct_color="${GREEN}"
    fi
    
    printf "${YELLOW}|${NC} %-8s ${YELLOW}|${NC} %-24s ${YELLOW}|${NC} %5s ${YELLOW}|${NC} %18s ${YELLOW}|${NC} %21s ${YELLOW}|${NC} %13s ${YELLOW}|${NC} ${pct_color}%15s${NC} ${YELLOW}|${NC}\n" \
        "$cat" "$action" "$cnt" "$avg_mem_formatted" "$avg_steps_formatted" "$avg_fee_ada" "${mem_pct}%/${steps_pct}%"
done

printf "${YELLOW}+----------+--------------------------+-------+--------------------+-----------------------+---------------+-----------------+${NC}\n"

echo ""
echo -e "${CYAN}Budget limits per transaction:${NC}"
echo -e "  Memory: $(format_number $MAX_MEM) units"
echo -e "  Steps:  $(format_number $MAX_STEPS) units"
echo -e "  Script: $(format_number $MAX_SCRIPT_SIZE) bytes (max tx size)"
echo ""
echo -e "${CYAN}Legend:${NC} % of Limit = Memory%/Steps% (exUnits) or Size% (scripts)"
echo -e "  ${GREEN}Green${NC}  = Under 50% of limit"
echo -e "  ${YELLOW}Yellow${NC} = 50-80% of limit"
echo -e "  ${RED}Red${NC}    = Over 80% of limit"
