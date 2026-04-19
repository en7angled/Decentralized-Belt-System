# Developer Guide — Adding New Concepts

*Audience: engineers extending the repo with new domain concepts. This doc is a pure workflow + checklist; deep material lives elsewhere.*

## Where to find what

| You need                           | Go to                                                                                                                 |
| ---------------------------------- | --------------------------------------------------------------------------------------------------------------------- |
| Repo overview, how to run services | [`../README.md`](../README.md)                                                                                        |
| Library layering + file-change matrix | [`../CLAUDE.md`](../CLAUDE.md) §Architecture                                                                       |
| On-chain patterns, security rules  | [`../src/lib/onchain-lib/CLAUDE.md`](../src/lib/onchain-lib/CLAUDE.md)                                                |
| Off-chain tx-building pipeline     | [`../src/lib/offchain-lib/CLAUDE.md`](../src/lib/offchain-lib/CLAUDE.md)                                              |
| Executables, API conventions       | [`../src/exe/CLAUDE.md`](../src/exe/CLAUDE.md)                                                                        |
| Validator internals (what each does) | [`onchain-architecture.md`](onchain-architecture.md)                                                                |
| Coding style (naming, JSON, INLINEABLE) | [`haskell-style-guide.md`](haskell-style-guide.md)                                                               |
| On-chain error codes               | [`onchain-trace-codes.md`](onchain-trace-codes.md)                                                                    |
| REST endpoints (current)           | [`generated/swagger/`](generated/swagger/) — auto-generated                                                           |
| CIP-57 blueprint (current)         | [`generated/bjj-belt-system-blueprint.json`](generated/bjj-belt-system-blueprint.json) — auto-generated               |

This guide stitches those together into a **single sequence of phases** you follow when adding a new concept. For any phase, the linked CLAUDE.md is the authoritative detail.

---

## The rule: follow the layer order

Concepts flow **inner → outer**:

1. On-chain types (`onchain-lib`)
2. On-chain validator / minting redeemer (`onchain-lib`)
3. Off-chain domain types + actions (`offchain-lib`)
4. Off-chain tx building (`offchain-lib`)
5. Storage & ingestion (`offchain-lib`)
6. API surface (`interaction-api` is usually unchanged; `query-api` gets endpoints)
7. Admin CLI command (if admin-operable)
8. MCP tool (if the new concept should surface to agents)
9. Tests
10. Deployment / config

Do not jump layers. Each phase compiles; each phase is testable.

---

## Phase-by-phase

### Phase 1 — On-chain types

**`Onchain/Protocol/Types.hs`**
- Define the datum type. Records for 4+ fields, camelCase.
- `makeIsDataSchemaIndexed ''MyType [('MyType, 0)]`. **Constructor indices are stable — never reorder existing ones.**
- `deriving anyclass (HasBlueprintDefinition)` for CIP-57.
- If multiple datum kinds live at the same validator, wrap in a sum type (e.g. `MembershipDatum`) and list which redeemers each kind accepts.

**`Onchain/Protocol/Id.hs`**
- Add a deterministic ID derivation function. `blake2b_224` for 28-byte names. Use `generateRefAndUserTN` for CIP-68 pairs.

**`.cabal`** — expose the module under `onchain-lib`.

### Phase 2 — Validator (if the concept needs one)

**`Onchain/Validators/<Name>Validator.hs`**
- Typed lambda under ~80 lines; extract per-redeemer logic into named helpers.
- Pattern-match `ScriptContext txInfo (Redeemer bredeemer) scriptInfo` with `SpendingScript spendingTxOutRef mdatum`.
- `Cleanup` redeemer on every state validator: permissionless spend only when datum absent/unparseable; must fail when datum is valid. Trace code `<Prefix>0`.
- Output checks by **index**, not search. Use `checkTxOutAtIndexWithDatumMinValueAndAddress` for continuing outputs (balancer may add lovelace).
- `{-# INLINEABLE #-}` before every type signature on cross-module functions.
- Trace codes: two characters (`traceIfFalse "X1" condition`). Add your module's prefix to [`onchain-trace-codes.md`](onchain-trace-codes.md) if new.

Also update:
- **`Onchain/Protocol.hs`** — re-export.
- **`TxBuilding/Validators.hs`** — compiled script accessor.
- **`TxBuilding/Context.hs`** — field on `DeployedScriptsContext`.
- **`TxBuilding/Transactions.hs`** — add to `deployReferenceScripts`.
- **`config/config_bjj_validators.json`** — new field for the deployed ref-script.
- **`.cabal`** — expose the validator module.

### Phase 3 — Minting-policy redeemer

**`Onchain/Validators/MintingPolicy.hs`**
- Add redeemer constructor + handler in `mintingPolicyLambda`.
- Validate **exact mint**: `mintValueMinted == expectedValue`. Never allow extra token names.
- Include the oracle as a reference input. Check `opPaused`. Call `checkFee` with the right selector (`fcProfileCreationFee`, `fcPromotionFee`, …).
- Min lovelace comes from oracle `opMinUTxOValue` only — no fixed constants.
- One-off tokens: require spending a seed `TxOutRef`; derive the token name from it.
- Authorization: promotions require master user NFT; membership ops require org user NFT; achievements require awarder user NFT. Use `deriveUserFromRefAC`.
- Validate referenced AssetClasses share protocol `CurrencySymbol` via `hasCurrencySymbol`.

### Phase 4 — Off-chain domain types

**`DomainTypes/Core/Types.hs`** — domain type with JSON/Swagger instances. `deriving-aeson` with `StripPrefix` + `CamelToSnake`.

**`DomainTypes/Core/Actions.hs`** — add constructor to `ProfileActionType` (or the relevant action type).

### Phase 5 — Off-chain tx building

Order matters — each step depends on the previous.

| Step | File                             | What                                                                |
| ---- | -------------------------------- | ------------------------------------------------------------------- |
| 1    | `TxBuilding/Conversions.hs`      | Domain ↔ on-chain conversions.                                      |
| 2    | `TxBuilding/Utils.hs`            | Datum parsing helpers.                                              |
| 3    | `TxBuilding/Lookups.hs`          | UTxO + state lookups.                                               |
| 4    | `TxBuilding/SafeOnchainLogic.hs` | Safe wrappers (throw `TxBuildingException`) around onchain helpers that can `traceError`. Do **not** call onchain helpers directly from off-chain code. |
| 5    | `TxBuilding/Exceptions.hs`       | Error constructors. Map to HTTP via `txBuildingExceptionToHttpStatus`. |
| 6    | `TxBuilding/Operations.hs`       | Build the `GYTxSkeleton`. Output order in `mconcat` **must match** on-chain redeemer indices — document the layout. |
| 7    | `TxBuilding/Interactions.hs`     | Map the new action to the operation.                                |

Do not bypass the `Interaction → Operation → GYTxSkeleton → GYTxBody` flow.

### Phase 6 — Storage & ingestion

**`Ingestion.hs`** — extend `ChainEventProjection`, add a case to `projectChainEvent`.

**`Storage.hs`**
- Persistent entity + `put<Concept>Projection` function.
- Use `upsertByUnique` with a `Unique*` constraint for idempotence.
- Extend `putMatchAndProjections` to dispatch to it.
- **Update `rollbackTo`** — every new table needs a `deleteWhere` branch.
- **Add the new table name to `chainSyncTableNames`** — required by `wipeChainSyncTables`.
- If projection B depends on A and A can arrive after B (chain event order is not guaranteed), add backfill logic.

### Phase 7 — API

**`src/exe/interaction-api/`** — usually **no changes**. `/build-tx` accepts any `ActionType`.

**`src/exe/query-api/`**
- New routes in `RestAPI.hs`, with Servant `Summary` + `Description`.
- Handlers in `Query/Projected.hs` (projection-backed) or `Query/Live.hs` (chain-live). Use `withBackend` so RestAPI stays thin.
- Response DTOs in `offchain-lib/DomainTypes/Transfer/QueryResponses.hs`.
- Filter types + `*FromParams` helpers in `DomainTypes/Transfer/Filters.hs`.
- Sort enums in `DomainTypes/Transfer/OrderBy.hs`.
- For aggregate/page endpoints, extend `Query/Aggregates.hs`, `Query/Pages.hs`.
- Add `ToSchema` for new types so Swagger stays accurate.

### Phase 8 — Admin CLI

**`src/exe/admin/Main.hs`** — add a `Command` constructor, an optparse-applicative `command "kebab-name"` parser, and a branch in `executeCommand`. Share parsers (`assetClassParser`, `posixTimeParser`, etc.) rather than rolling your own.

### Phase 9 — MCP tool (if agent-visible)

**`src/lib/mcp-server-lib/MCPServer/Tools/<Domain>.hs`**
- Add the `ToolHandler` with a clear `Description` — this is what the LLM sees.
- Include in the module's `tools :: AppCtx -> [ToolHandler]` export. Gate write tools on `enableWriteTx ctx` so `tools/list` reflects deployed capability.
- If new domain types need MCP schemas, add orphans in `MCPServer/Orphans.hs`.
- Wire the module into `allTools` in `MCPServer.Server` if it's a new module.

### Phase 10 — Tests, deployment, docs

**Tests** — `src/test/UnitTests/<Topic>.hs` (and wire into `UnitTests.hs`). Property tests where the domain pins a rule (see `BJJPropertyTests.hs`). Use `GYTxGameMonad` for integration tests.

**Deployment**
- Run `cabal run admin -- deploy-reference-scripts` (or equivalent) to push the new validator's ref-script.
- Update `config/config_bjj_validators.json`.
- Run `cabal run admin -- write-blueprint` — commits regenerated [`generated/bjj-belt-system-blueprint.json`](generated/bjj-belt-system-blueprint.json).
- Start interaction-api and query-api once — regenerates [`generated/swagger/`](generated/swagger/).

**Docs**
- Update [`onchain-architecture.md`](onchain-architecture.md) — this is the authoritative reference.
- Add trace-code prefix to [`onchain-trace-codes.md`](onchain-trace-codes.md) if new.
- If the concept has a security implication, add a section to [`onchain-security-audit.md`](onchain-security-audit.md).

---

## Consistency checklist

Tick off when adding any new concept.

### On-chain
- [ ] Datum type in `Protocol/Types.hs` with stable indices + `HasBlueprintDefinition`
- [ ] ID derivation in `Protocol/Id.hs`
- [ ] Validator under `Onchain/Validators/` with `Cleanup` redeemer
- [ ] Minting redeemer + exact-mint check + oracle pause gate + fee check
- [ ] Output checks by **index**, min-value for continuing outputs
- [ ] `{-# INLINEABLE #-}` on every cross-module function
- [ ] Trace codes unique; prefix listed in `onchain-trace-codes.md`
- [ ] `.cabal` `exposed-modules` updated

### Off-chain
- [ ] Domain type with JSON/Swagger (`StripPrefix` + `CamelToSnake`)
- [ ] Action constructor in `DomainTypes/Core/Actions.hs`
- [ ] Conversions + Lookups + SafeOnchainLogic wrapper + Exceptions + Operation + Interaction mapping
- [ ] Output order in `mconcat` matches on-chain redeemer indices (documented)
- [ ] New validator → Context field, `Validators.hs` accessor, `deployReferenceScripts` updated

### Storage
- [ ] Event variant in `ChainEventProjection`
- [ ] Persistent entity + `put*` + unique constraint
- [ ] `putMatchAndProjections` dispatches
- [ ] `rollbackTo` deletes the new table
- [ ] Table name added to `chainSyncTableNames`
- [ ] Backfill logic if cross-projection ordering matters

### API / CLI / MCP
- [ ] Query routes with `Summary` + `Description` + `ToSchema`
- [ ] Filter/sort/response types in `DomainTypes/Transfer/…`
- [ ] Admin CLI command if admin-operable
- [ ] MCP tool in relevant `Tools/<Domain>.hs`, gated if write

### Tests & docs
- [ ] Unit tests (happy path + error cases)
- [ ] Property tests if a domain rule exists
- [ ] `onchain-architecture.md` updated
- [ ] Blueprint regenerated (`admin write-blueprint`)
- [ ] Swagger regenerated (start the APIs once)

---

## Common pitfalls

| Pitfall                                | Fix                                                                                                                          |
| -------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| Output index mismatch                  | `mconcat` order in the skeleton must match on-chain redeemer indices. Add a layout comment.                                  |
| Missing exact-mint check               | Every minting redeemer needs `mintValueMinted == expected`. Without it, attackers mint arbitrary tokens.                     |
| Forgetting the oracle reference input  | Every minting tx must include the oracle as a reference input. Use `getOracleRefInputSkeleton`.                              |
| Constructor index reordering           | `makeIsDataSchemaIndexed` indices are stable forever. Append, never reorder.                                                 |
| Missing rollback / wipe handling       | New projections need both a `deleteWhere` in `rollbackTo` and an entry in `chainSyncTableNames`.                             |
| Off-chain deps leaking into on-chain   | `onchain-lib` must not import Aeson, Servant, Swagger. Define Plutus instances here; off-chain instances in `offchain-lib`.  |
| Missing `INLINEABLE`                   | PlutusTx compilation fails silently without it on cross-module functions.                                                    |
| `ProtocolParams` not updated           | If a new validator hash must be known to another script, update `ProtocolParams` and redeploy the minting policy.            |
| `opPaused` not checked                 | Any concept that mints must honour the pause gate.                                                                           |
| Missing `CurrencySymbol` validation    | Referenced AssetClasses must share the protocol's `CurrencySymbol`. Use `hasCurrencySymbol`.                                 |
| Datum-type confusion                   | Multiple datum types at one validator → wrap in a sum type and reject incompatible `(datum, redeemer)` pairs.                |
| Calling onchain helpers off-chain      | They can `traceError`. Use `TxBuilding/SafeOnchainLogic.hs` wrappers that raise `TxBuildingException`.                       |
| Inconsistent JSON names                | `deriving-aeson` with `StripPrefix` + `CamelToSnake` is the single rule for API types.                                       |

---

## After the work

Before you call it done:

1. `cabal build all` — green.
2. `cabal test` — green. `scripts/test_exunits.sh` if the change touches on-chain code (exUnits + script size report).
3. Regenerate the blueprint and swagger (see Phase 10).
4. Grep the docs for any stale path or count you invalidated (validator count, port, module map). The [`onchain-architecture.md`](onchain-architecture.md) Overview and this guide's cross-references are the common drift points.
