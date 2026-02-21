# Git Push (sync with origin)

## Overview

Push current branch to origin and sync with remote updates.

## Steps

1. **Fetch and rebase onto latest main (optional but recommended)**
    - `git fetch origin`
    - `git rebase origin/main || git rebase --abort` (if not on main, rebase your feature branch onto latest main)
2. **Push current branch**
    - `git push -u origin HEAD`
3. **If push rejected due to remote updates**
    - Rebase and push: `git pull --rebase && git push`

## Notes

- Prefer `rebase` over `merge` for a linear history.
- If you need to force push after a rebase: you need to ask the user if they want to force push: `git push --force-with-lease`.
- Force-push and tag/release steps below should only be run with user confirmation when using this command for "push and release".

## Tags and releases

Use **Commitizen (cz)** for version bumps, then tag and release.

1. **Bump version with Commitizen**
   - `cz bump` (interactive) or `cz bump --increment PATCH` (or `MINOR` / `MAJOR`)
   - This updates `.cz.toml` and `CHANGELOG.md`. Then set the same version in [Decentralized-Belt-System.cabal](Decentralized-Belt-System.cabal) (see [versioning rule](.cursor/rules/workflow/versioning.mdc)).
   - Commit: `git add .cz.toml CHANGELOG.md Decentralized-Belt-System.cabal && git commit -m "chore: bump version to X.Y.Z.W"`
2. **Create annotated tag** (version from cz; no `v` prefix to match `tag_format = "$version"`):
   - `git tag -a 0.3.1.8 -m "Release 0.3.1.8: <short summary from CHANGELOG>"`
3. **Push branch and tag**
   - `git push -u origin HEAD`
   - `git push origin 0.3.1.8` (or `git push origin --tags`)
4. **GitHub Release**
   - Repo → Releases → "Draft a new release"; choose the new tag; paste the relevant CHANGELOG section as the description; publish.
