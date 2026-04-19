# Onboard New Developer

## Overview

Get a new developer set up and productive on the Decentralized Belt System — a Cardano-based BJJ belt/rank management system built in Haskell.

## Steps

1. **Environment Setup**
    - Install Nix (multi-user) — the project uses a Nix flake for GHC 9.6, Cabal, and HLS
    - Install direnv and run `direnv allow` in the repo root to load the dev shell
    - Verify: `ghc --version` (should show 9.6.x), `cabal --version`
    - Install recommended VS Code extensions: Haskell (HLS), direnv

2. **Build & Test**
    - Run `cabal build all` to compile all four libraries and four executables
    - Run `cabal test` to confirm the test suite passes
    - Run `cabal repl exe:admin` to open a REPL for interactive exploration

3. **Project Architecture**
    - Read `CLAUDE.md` for the full architecture overview
    - Understand the four libraries and their layering: `onchain-lib` → `offchain-lib` → `webapi-lib` (+ `chainsync-lib`)
    - Understand the three executables: `interaction-api` (8082), `query-api` (8083), `chainsync-service` (8084) + `admin` CLI
    - Review the three conceptual flows: type hierarchy, interactions → transactions, onchain events → projections

4. **Key Files to Read First**
    - `src/lib/onchain-lib/Onchain/Protocol/Types.hs` — on-chain data types
    - `src/lib/offchain-lib/DomainTypes/Core/Types.hs` — domain types
    - `src/lib/offchain-lib/DomainTypes/Core/Actions.hs` — domain actions
    - `src/lib/offchain-lib/TxBuilding/Interactions.hs` — how actions become transactions
    - `src/exe/interaction-api/RestAPI.hs` — API endpoint definitions

5. **Configuration**
    - `config/config_atlas.json` — Cardano provider config
    - `config/config_bjj_validators.json` — deployed validator references
    - `.env` — local overrides (never committed). See README §6.4 for variables
    - `operation.prv` — admin signing key (never committed)

6. **First Task**
    - Pick a small issue or read through a recent PR to understand the change flow
    - Follow the phase order when implementing: onchain → validator → domain → tx building → API → tests
    - Run `cabal build all && cabal test` before submitting a PR

## Onboarding Checklist

- [ ] Nix installed and `direnv allow` run
- [ ] `ghc --version` shows 9.6.x
- [ ] `cabal build all` succeeds
- [ ] `cabal test` passes
- [ ] CLAUDE.md and architecture overview read
- [ ] Key source files reviewed
- [ ] Configuration files understood (.env, config/, operation.prv)
- [ ] First PR submitted
