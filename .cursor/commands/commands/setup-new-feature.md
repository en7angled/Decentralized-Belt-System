# Setup New Feature

## Overview

Systematically set up a new feature in the Decentralized Belt System, following the mandatory phase order from on-chain types through deployment.

## Steps

1. **Define Requirements**
    - Clarify feature scope, acceptance criteria, and which layers are affected
    - Determine if the feature requires new on-chain types, validators, minting policy changes, or is off-chain only
    - Check `docs/` for existing change requests (CR-*.md) or create a new one

2. **Create Feature Branch**
    - Branch from `main`
    - Run `direnv allow` and `cabal build all` to confirm a clean baseline

3. **Follow the Phase Order**
    - Implement in this strict sequence (skip phases that don't apply):
      1. **On-chain types** — `Onchain/Protocol/Types.hs`, `Onchain/Protocol/Id.hs`
      2. **Validator** — `Onchain/Validators/<Name>Validator.hs`, update `Onchain/Protocol.hs`
      3. **Minting policy** — `Onchain/Validators/MintingPolicy.hs` (new redeemers/fee types)
      4. **Domain types** — `DomainTypes/Core/Types.hs`, `DomainTypes/Core/Actions.hs`
      5. **Tx building** — `TxBuilding/Operations.hs`, `Lookups.hs`, `Interactions.hs`, `Conversions.hs`
      6. **API** — `RestAPI.hs`, `ServiceHandlers.hs`, `ServiceRequests.hs`
      7. **Chain-sync** — `Ingestion.hs`, `Storage.hs` (if new projections needed)
      8. **Admin CLI** — `src/exe/admin/Main.hs`
      9. **Tests** — `UnitTests/<Topic>.hs`, `TestRuns.hs`
      10. **Deployment** — Blueprint (`admin write-blueprint`), config updates, version bumps

4. **Respect Library Layering**
    - `onchain-lib` must never import off-chain deps (no Aeson, Servant, Swagger)
    - `offchain-lib` imports from `onchain-lib` only
    - `webapi-lib` has no project-library dependencies
    - Executables depend on `offchain-lib` + `webapi-lib`

5. **Version & Documentation**
    - Bump API version in `RestAPI.hs` (Swagger) if endpoints change
    - Bump blueprint version in `Blueprint.hs` if on-chain types change
    - Update CHANGELOG.md
    - Use `cz bump` for app version, then sync `.cabal` version manually

## Setup New Feature Checklist

- [ ] Requirements documented (CR doc if needed)
- [ ] Feature branch created from `main`
- [ ] Phase order followed (on-chain → validator → minting → domain → tx → API → sync → admin → tests → deploy)
- [ ] Library layering respected
- [ ] Versions bumped where applicable (API, blueprint, app)
- [ ] CHANGELOG.md updated
- [ ] `cabal build all` succeeds
- [ ] `cabal test` passes
