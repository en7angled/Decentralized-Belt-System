# Code Review

## Overview

Perform a thorough code review of changes in the Decentralized Belt System, verifying functionality, Haskell style, library layering, and Cardano-specific correctness.

## Steps

1. **Understand the Change**
    - Read the PR description and related issues/CR docs for context
    - Identify which layers are affected (onchain-lib, offchain-lib, webapi-lib, executables)
    - Note the phase order: were changes made in the correct sequence?

2. **Validate Functionality**
    - Confirm the code delivers the intended behavior
    - Check error handling uses `TxBuildingException`, not raw strings
    - Verify on-chain validation logic: all redeemer branches covered, no silent pass-through
    - Test edge cases mentally: empty inputs, missing UTxOs, unauthorized callers

3. **Check Library Layering**
    - `onchain-lib` must not import Aeson, Servant, Swagger, or any off-chain dependency
    - `offchain-lib` imports only from `onchain-lib` (project libs)
    - `webapi-lib` has no project-library dependencies
    - Executables depend on `offchain-lib` + `webapi-lib`

4. **Assess Haskell Style**
    - PascalCase types, camelCase functions, `mk` smart constructors, `is`/`has`/`check` predicates
    - Max 120 characters per line, 2-space indent, no tabs
    - Haddock (`-- |`) on exports, `-- ^` on non-obvious record fields
    - `deriving-aeson` with `StripPrefix` + `CamelToSnake` for JSON
    - No partial functions (`head`, `tail`, `fromJust`) — use total alternatives
    - No redundant record field prefixes

5. **Review Type-Level Correctness**
    - New types follow the hierarchy: onchain → domain → transfer (API DTOs)
    - Conversions between layers are explicit, not implicit coercions
    - Records used for 4+ fields; newtypes for type-safe wrappers
    - Deriving strategies are explicit (`stock`, `newtype`, `anyclass`)

6. **Check Tests & Versioning**
    - New behavior has corresponding tests in `src/test/UnitTests/`
    - Property tests cover new invariants where applicable
    - API version bumped in `RestAPI.hs` if endpoints changed
    - Blueprint version bumped in `Blueprint.hs` if on-chain types changed
    - CHANGELOG.md updated

## Review Checklist

### Functionality
- [ ] Intended behavior works and matches requirements
- [ ] Error handling uses `TxBuildingException` with proper HTTP mapping
- [ ] On-chain validation covers all redeemer branches

### Architecture
- [ ] Library layering respected (onchain-lib has no off-chain deps)
- [ ] Phase order followed for new features
- [ ] Type hierarchy maintained (onchain → domain → transfer)

### Haskell Style
- [ ] Naming conventions followed
- [ ] Line length ≤ 120, 2-space indent
- [ ] No partial functions
- [ ] Haddock on exports and non-obvious fields
- [ ] `deriving-aeson` with correct modifiers for JSON

### Tests & Versioning
- [ ] New tests cover the change
- [ ] API/blueprint versions bumped if needed
- [ ] CHANGELOG.md updated
