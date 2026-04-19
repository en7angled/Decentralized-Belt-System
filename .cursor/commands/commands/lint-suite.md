# Fix Lint Issues

## Overview

Run Haskell linting tools across the codebase, apply fixes, and ensure the code meets the project's formatting and style standards.

## Steps

1. **Run Linters**
    - Run `hlint src/` to get suggestions (if available in the Nix shell)
    - Check GHC warnings: `cabal build all` with default warning flags
    - Review `-Wunused-imports`, `-Wunused-binds`, `-Wincomplete-patterns` warnings

2. **Resolve Findings**
    - Apply HLint suggestions that improve clarity (eta-reduce, use `<$>`, remove redundant `do`)
    - Skip suggestions that obscure intent in complex Plutus validator code
    - Fix all GHC warnings — especially incomplete pattern matches and unused imports
    - Ensure naming matches conventions: PascalCase types, camelCase functions, `mk` constructors

3. **Verify Style Compliance**
    - Max 120 characters per line
    - 2-space indentation, no tabs
    - One blank line between top-level declarations
    - Haddock (`-- |`) on module exports; `-- ^` on non-obvious record fields
    - `deriving-aeson` with `StripPrefix` + `CamelToSnake` for JSON instances

4. **Final Verification**
    - Re-run `cabal build all` — no warnings or errors
    - Re-run `hlint src/` — no remaining actionable suggestions
    - Stage changes with clear commit messages

## Lint Checklist

- [ ] HLint executed on `src/`
- [ ] GHC warnings reviewed and fixed
- [ ] Incomplete pattern matches resolved
- [ ] Unused imports and bindings removed
- [ ] Style conventions verified (line length, indentation, naming)
- [ ] `cabal build all` clean
- [ ] Changes staged and ready for commit
