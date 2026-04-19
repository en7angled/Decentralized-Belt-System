# Write Unit Tests

## Overview

Create unit and property tests for the Decentralized Belt System using Tasty as the test framework, with HUnit for unit tests and QuickCheck for property-based tests.

## Steps

1. **Determine Test Location**
    - Unit tests go in `src/test/UnitTests/<Topic>.hs`
    - Property tests go in `src/test/BJJPropertyTests.hs` or a new `<Topic>PropertyTests.hs`
    - Register new test modules in `src/test/TestRuns.hs` and the `.cabal` file

2. **Write Unit Tests (HUnit / Tasty)**
    - Use `testCase` for individual test cases and `testGroup` for grouping
    - Follow the existing pattern: build test actions/interactions → run against test context → assert outcomes
    - Test both success paths and expected failures (e.g., invalid promotions, wrong belt transitions)
    - For on-chain validation: test that valid redeemers pass and invalid ones fail with expected trace codes

3. **Write Property Tests (QuickCheck)**
    - Use `testProperty` with `Arbitrary` instances for domain types
    - Test invariants: e.g., belt ordering is transitive, ID derivation is deterministic, rank transitions are valid
    - Use custom generators (like `genBeltNotMax`) when domain constraints exclude certain values
    - Ensure generators produce well-distributed values across the domain

4. **Test Interactions & Tx Building**
    - Test the full interaction pipeline: Action → `interactionToTxSkeleton` → validate skeleton shape
    - Verify correct UTxO lookups and datum construction
    - Test error cases: missing UTxOs, invalid state transitions, unauthorized actions

5. **ExUnit & Script Size Testing**
    - For on-chain changes, run `scripts/test_exunits.sh` to check execution unit budgets
    - Ensure new validators/minting policies stay within Cardano protocol limits

6. **Verification**
    - Run `cabal test` to confirm all tests pass
    - Run `scripts/test_exunits.sh` if on-chain code changed

## Write Unit Tests Checklist

- [ ] Test module created in `src/test/UnitTests/<Topic>.hs` or property test file
- [ ] Module registered in `TestRuns.hs` and `.cabal`
- [ ] Unit tests cover success and failure paths
- [ ] Property tests verify domain invariants with appropriate generators
- [ ] Interaction/tx-building pipeline tested where applicable
- [ ] ExUnit budgets checked for on-chain changes (`scripts/test_exunits.sh`)
- [ ] `cabal test` passes
