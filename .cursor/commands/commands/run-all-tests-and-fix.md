# Run All Tests and Fix Failures

## Overview

Execute the full test suite for the Decentralized Belt System and systematically fix any failures.

## Steps

1. **Run the Test Suite**
    - Run `cabal test` to execute all unit and property tests
    - Capture the full output — Tasty reports which test groups and cases failed
    - If on-chain code changed, also run `scripts/test_exunits.sh` for execution unit budget checks

2. **Analyze Failures**
    - Read the Tasty failure output: it shows the test name, expected vs actual values, and assertion messages
    - Categorize failures:
      - **Type errors / won't compile**: fix imports, type signatures, missing instances first
      - **Assertion failures**: check if the test or the implementation is wrong
      - **Property test failures**: examine the counterexample QuickCheck provides; check generator constraints
      - **ExUnit budget exceeded**: optimize Plutus scripts (add `INLINEABLE` pragmas, reduce datum size)
    - Check if failures relate to recent changes (`git diff`)

3. **Fix Systematically**
    - Fix compilation errors first — nothing else runs until `cabal build all` succeeds
    - Fix unit test failures one at a time, re-running `cabal test` after each fix
    - For property test failures: either fix the implementation or tighten the generator (e.g., exclude invalid domain values)
    - Respect library layering when making fixes — don't pull off-chain deps into `onchain-lib`

4. **Verify**
    - Run `cabal test` — all tests must pass
    - Run `scripts/test_exunits.sh` if on-chain code was touched
    - Run `cabal build all` to confirm no regressions

## Test Recovery Checklist

- [ ] `cabal test` executed and output captured
- [ ] `scripts/test_exunits.sh` run (if on-chain changes)
- [ ] Failures categorized (compile, assertion, property, exunit)
- [ ] Compilation errors fixed first
- [ ] Unit test failures resolved
- [ ] Property test failures resolved (implementation or generator fix)
- [ ] Final `cabal test` passes cleanly
- [ ] Final `cabal build all` succeeds
