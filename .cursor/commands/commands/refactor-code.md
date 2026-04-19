# Refactor Code

## Overview

Refactor the selected Haskell code to improve its quality while maintaining the same functionality. Follow the project's layering rules and Haskell style conventions from CLAUDE.md.

## Steps

1. **Type-Level Improvements**
    - Replace stringly-typed values with newtypes or sum types
    - Use records for functions with 4+ arguments of similar types
    - Ensure smart constructors (`mk` prefix) enforce invariants
    - Prefer `deriving` strategies over hand-written instances
    - Use `deriving-aeson` with `StripPrefix` + `CamelToSnake` for JSON

2. **Code Quality**
    - Extract reusable functions; eliminate duplication
    - Improve naming: PascalCase types, camelCase functions, `is`/`has`/`check` predicates
    - Remove redundant record field prefixes
    - Simplify complex logic — reduce nesting, prefer pattern matching and guards over nested `case`/`if`
    - Replace partial functions (`head`, `tail`, `fromJust`) with total alternatives

3. **Performance & Strictness**
    - Add strictness annotations (`!`) on record fields where appropriate
    - Prefer `Text` over `String`, `Map` over association lists
    - Reduce unnecessary intermediate data structures (list fusion, `foldl'` over `foldl`)
    - Avoid repeated lookups — bind once, reuse

4. **Maintainability**
    - Add Haddock (`-- |`) on module exports and public functions
    - Add `-- ^` on record fields where the meaning isn't obvious
    - Keep functions under ~30 lines; extract helpers for readability
    - Respect the library layering: `onchain-lib` must not import off-chain deps
    - Ensure error types use `TxBuildingException`, not raw strings

5. **Verification**
    - Run `cabal build all` to confirm the refactored code compiles
    - Run `cabal test` to confirm no regressions

## Refactor Code Checklist

- [ ] Replaced stringly-typed values with proper types where applicable
- [ ] Eliminated code duplication and extracted reusable functions
- [ ] Improved naming to match project conventions
- [ ] Simplified complex logic (pattern matching, guards, fewer nesting levels)
- [ ] Removed partial functions in favor of total alternatives
- [ ] Added strictness annotations where appropriate
- [ ] Added Haddock documentation on exports and non-obvious fields
- [ ] Verified library layering is respected
- [ ] `cabal build all` succeeds
- [ ] `cabal test` passes
