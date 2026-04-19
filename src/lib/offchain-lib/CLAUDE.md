# Offchain Library Rules

Domain types, tx building pipeline, storage/ingestion, and Atlas conventions.

## Pipeline

Interactions → Transactions via Operations. Do not bypass this flow.

1. `interactionToTxSkeleton` (Interactions): dispatches `Interaction` (action + user addresses) to Operations
2. Operations build `GYTxSkeleton` by composing **Skeletons** (ref inputs, validity, CIP-68) and **Lookups** (UTxO/datum queries)
3. `interactionToTxBody` (Transactions): runs skeleton through builder to produce tx

Output order in skeleton `mconcat` must match on-chain redeemer indices. Document output layout in comments.

## Atlas Conventions (GYTxSkeleton)

- **Specify-what-you-want**: Declare inputs, outputs, ref inputs, validity, signatories. Framework handles change, min ADA, collateral, balancing. No manual change/min-ADA logic.
- **Monad hierarchy** (least to most capability): `GYTxQueryMonad` → `GYTxUserQueryMonad` → `GYTxBuilderMonad` → `GYTxMonad` → `GYTxGameMonad`. Use the least capable that suffices.
- **Skeleton helpers**: `mustHaveOutput`, `mustHaveInput`, `mustHaveRefInput`, `mustBeSignedBy`, `isInvalidBefore`/`isInvalidAfter`, `mustMint`. Compose with `mconcat`.
- **Scripts**: `scriptFromPlutus` → `GYScript v`. Reference: `GYBuildPlutusScriptReference refScript script`. Inlined: `GYBuildPlutusScriptInlined script`.
- **Lookups**: `utxoDatum'` for one UTxO's datum. `utxosDatums` when filtering invalid datums at an address where anyone can create UTxOs.

## Context

- **DeployedScriptsContext**: Validator refs + oracle. `MonadReader` in Operations/Lookups/Interactions.
- **TxBuildingContext**: Deployed scripts + provider. For `runTx`, `interactionToTxBody`, `runBJJActionWithPK`.
- New validator → new field in Context, JSON config, `deployReferenceScripts`, and `config_bjj_validators.json` after deploy.
- **OracleNFTPolicy is NOT in DeployedScriptsContext** — inlined in oracle-mint tx only.

## Onchain Functions Used Offchain

Do NOT call onchain functions directly (they can `traceError`). Use safe wrappers in `TxBuilding.SafeOnchainLogic`:
- Each has a dedicated `TxBuildingException` constructor
- Wrappers pre-validate and throw `GYApplicationException`
- Example: `safeGetCurrentRankId` instead of `Onchain.getCurrentRankId`; `safeIntToBelt` instead of `intToBelt`

## Storage, Ingestion & Chain-Sync

- **Flow**: Chain-sync matches → `projectChainEvent` (Ingestion, maps datum to `ChainEventProjection`) → `putMatchAndProjections` (Storage, dispatches to `put*Projection`)
- **New projection**: Extend `ChainEventProjection`, add case in `projectChainEvent`, add Persistent entity + `put*` in Storage, **update `rollbackTo`**, **add table to `chainSyncTableNames`** (for `wipeChainSyncTables`)
- **Idempotence**: Use `upsertByUnique` with `Unique*` constraint
- **Backfill**: When projection B depends on A and A can arrive after B, implement backfill logic

## New Concept Checklist

When adding a new validator, token kind, projection, or API surface:

**On-chain**: datum type (`Types.hs`, stable indices, `HasBlueprintDefinition`) → ID derivation (`Id.hs`) → redeemer/validator → minting redeemer (exact mint, oracle pause gate, fee check) → `INLINEABLE` pragmas → cabal `exposed-modules`

**Off-chain**: domain type (`Core/Types.hs`, JSON/Swagger) → action (`Core/Actions.hs`) → conversions (`Conversions.hs`) → lookups (`Lookups.hs`) → errors (`Exceptions.hs`) → operation (`Operations.hs`) → interaction mapping (`Interactions.hs`) → validator accessor/context/deployment if new validator → output index consistency

**Storage**: event type (`Ingestion.hs`) → Persistent entity + put function (`Storage.hs`) → `putMatchAndProjections` → `rollbackTo` → `chainSyncTableNames`

**API/CLI**: query endpoints → admin command → Swagger metadata

**Tests/docs**: unit tests → `onchain-architecture.md` → blueprint regenerated → test scripts updated → test runner in `GYTxGameMonad`

### Common Pitfalls

- Output index mismatch: `mconcat` order must match redeemer indices
- Missing exact mint check: every minting redeemer needs `mintValueMinted == expectedTokens`
- Forgetting oracle reference input: use `getOracleRefInputSkeleton`
- Constructor index collision: stable, unique, never reordered
- Missing rollback/wipe handling for new projections
- Off-chain deps in onchain-lib: forbidden
- Missing `INLINEABLE` on cross-module onchain functions
- Forgetting ProtocolParams: update `ProtocolParams` and redeploy minting policy if new validator hash needed
- Not checking opPaused: check pause gate if concept involves minting
- Missing CurrencySymbol validation: validate referenced AssetClasses share protocol CurrencySymbol via `hasCurrencySymbol`
- Datum type confusion: use wrapper sum type for multiple datum types at same validator (e.g. `MembershipDatum`)
- Incomplete skeleton: include all required ref inputs (e.g. oracle), validity range when contract checks time, `mustBeSignedBy` when contract expects signatories
- Inconsistent JSON naming: use `deriving-aeson` with `StripPrefix` + `CamelToSnake` for API types
