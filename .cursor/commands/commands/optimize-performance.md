# Optimize Performance

## Overview

Analyze and optimize performance in the Decentralized Belt System, covering Plutus on-chain script efficiency, Haskell off-chain code, and database query performance.

## Steps

1. **On-Chain Script Optimization (Plutus)**
    - Add `{-# INLINEABLE #-}` pragmas on all validator/minting-policy helper functions
    - Minimize datum size — smaller datums reduce execution units and transaction fees
    - Avoid list operations that traverse the full UTxO set; prefer direct lookups
    - Use `BuiltinByteString` over `Text` in on-chain code
    - Reduce pattern matching depth — flatten nested case expressions
    - Run `scripts/test_exunits.sh` to measure execution unit budgets before and after

2. **Off-Chain Haskell Optimization**
    - Add strictness annotations (`!`) on record fields to avoid space leaks
    - Use `foldl'` (strict) instead of `foldl` (lazy)
    - Prefer `Text` over `String`, `Map` over association lists
    - Avoid repeated UTxO lookups — bind once, reuse the result
    - Use `BangPatterns` for strict bindings in hot paths
    - Profile with GHC's `-prof` flag and `+RTS -p` for heap analysis

3. **Database & Query Optimization**
    - Review chain-sync projection queries for missing indexes
    - Check query-api endpoints for N+1 query patterns
    - Ensure batch operations use single queries, not loops
    - Review `Storage.hs` for unnecessary full-table scans

4. **Transaction Building Efficiency**
    - Minimize the number of UTxO lookups per transaction
    - Batch related operations into single transactions where possible
    - Optimize `interactionToTxSkeleton` to avoid redundant datum serialization
    - Check that reference scripts are used instead of inline scripts where possible

5. **Measurement & Verification**
    - Run `scripts/test_exunits.sh` — compare before/after execution units
    - Run `cabal test` — ensure optimizations don't break behavior
    - Document any significant budget savings in the PR description

## Optimize Performance Checklist

- [ ] `{-# INLINEABLE #-}` pragmas on on-chain helpers
- [ ] Datum sizes minimized
- [ ] Strictness annotations on record fields
- [ ] `foldl'` used instead of `foldl`
- [ ] `Text` preferred over `String`
- [ ] Database queries reviewed for indexes and N+1 patterns
- [ ] UTxO lookups minimized in transaction building
- [ ] `scripts/test_exunits.sh` shows acceptable execution units
- [ ] `cabal test` passes
