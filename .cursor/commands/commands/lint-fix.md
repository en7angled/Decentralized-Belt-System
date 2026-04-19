# Lint and Fix Code

## Overview

Analyze Haskell source files for style issues and apply fixes according to the project's coding standards (CLAUDE.md).

## Steps

1. **Identify Style Issues**
    - Lines exceeding 120 characters
    - Tabs instead of 2-space indentation
    - Missing blank lines between top-level declarations
    - Unused imports and variables (GHC `-Wunused-imports`, `-Wunused-binds`)
    - Redundant brackets, `$`, or `do` blocks

2. **Naming Convention Violations**
    - Types/constructors not in PascalCase
    - Functions/fields not in camelCase
    - Smart constructors missing `mk` prefix
    - Predicates missing `is`/`has`/`check` prefix
    - Redundant record field prefixes (e.g., `membershipHistoryPractitionerId` → `practitionerId`)

3. **HLint Suggestions**
    - Run `hlint` on changed files (if available in the Nix shell)
    - Apply suggestions: eta-reduction, redundant `return`, `<$>` instead of `fmap`, etc.
    - Ignore suggestions that reduce readability in complex Plutus/GY code

4. **JSON & Deriving Style**
    - Ensure `deriving-aeson` uses `StripPrefix` + `CamelToSnake` for snake_case API fields
    - Prefer `deriving` strategies (`stock`, `newtype`, `anyclass`) over hand-written instances
    - Use `DerivingStrategies` extension explicitly

5. **Apply Fixes**
    - Fix issues directly in the source files
    - Re-run `cabal build all` to confirm fixes don't break compilation

## Lint and Fix Checklist

- [ ] Lines within 120-character limit
- [ ] 2-space indentation, no tabs
- [ ] Blank lines between top-level declarations
- [ ] Unused imports and variables removed
- [ ] Naming conventions followed (PascalCase types, camelCase functions)
- [ ] No redundant record field prefixes
- [ ] HLint suggestions reviewed and applied where appropriate
- [ ] `deriving-aeson` with `StripPrefix` + `CamelToSnake` for JSON
- [ ] `cabal build all` succeeds
