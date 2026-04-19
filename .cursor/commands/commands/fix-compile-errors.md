# Fix Compile Errors

## Overview

Analyze and fix GHC compilation errors in the Decentralized Belt System. The project uses GHC 9.6 via Nix/direnv and builds with Cabal.

## Steps

1. **Run the Build**
    - Run `cabal build all` to surface all compilation errors
    - Read errors carefully — GHC reports the file, line, column, expected type, and actual type

2. **Diagnose Common GHC Errors**
    - **Type mismatches**: Check if a function signature changed upstream (e.g., onchain type renamed). Trace the type through the layers: onchain → domain → tx building → API
    - **Missing imports**: Add the import. Check which library exports the symbol (respect layering — `onchain-lib` symbols come from `Onchain.*` modules)
    - **Missing instances**: Add `deriving` clauses or write manual instances. For JSON, use `deriving-aeson` with `StripPrefix` + `CamelToSnake`
    - **Ambiguous type variables**: Add type annotations or use `TypeApplications`
    - **Non-exhaustive patterns**: Add missing constructors — never add a wildcard `_` catch-all unless justified
    - **Redundant constraints**: Remove unused typeclass constraints from signatures
    - **Module not found**: Check the `.cabal` file — ensure the module is listed under `exposed-modules` or `other-modules`

3. **Fix in Dependency Order**
    - Fix `onchain-lib` errors first (innermost layer)
    - Then `chainsync-lib` and `offchain-lib`
    - Then `webapi-lib`
    - Finally executables (`admin`, `interaction-api`, `query-api`, `chainsync-service`)
    - Re-run `cabal build all` after each layer to confirm progress

4. **Verify**
    - `cabal build all` succeeds with no errors or warnings-as-errors
    - `cabal test` passes (compilation fix may have changed behavior)

## Fix Compile Errors Checklist

- [ ] Ran `cabal build all` and captured all errors
- [ ] Fixed errors in dependency order (onchain-lib → offchain-lib → webapi-lib → executables)
- [ ] Type mismatches resolved by tracing through the type hierarchy
- [ ] Missing imports/modules added (respecting library layering)
- [ ] Missing instances derived or implemented
- [ ] Non-exhaustive patterns completed (no blind wildcards)
- [ ] `.cabal` file updated if new modules were added
- [ ] `cabal build all` succeeds
- [ ] `cabal test` passes
