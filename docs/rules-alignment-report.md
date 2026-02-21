# Rules vs src Alignment Report

Generated from a systematic pass over `src/` against `.cursor/rules/`. Scope: all rule files whose globs match `src/**/*` (project-architecture, executables, onchain-rules, offchain-rules, plutus-rules, plutus-security, testing-and-config, docs-and-api, storage-ingestion).

---

## Summary

| Rule                     | Scope                        | Issues found                                 |
| ------------------------ | ---------------------------- | -------------------------------------------- |
| project-architecture.mdc | src/**/*                     | 0 (onchain purity verified)                  |
| executables.mdc          | src/exe/**/*                 | 0 (fixed: startup uses `die`)                |
| onchain-rules.mdc        | src/lib/onchain-lib/**/*.hs  | 0 (fixed: OracleValidator uses output-index) |
| offchain-rules.mdc       | src/lib/offchain-lib/**/*.hs | 0 (fixed: BJJ `IsString` removed)            |
| plutus-rules.mdc         | src/lib/onchain-lib/**/*.hs  | 0 (fixed: output-index; K0 documented)       |
| plutus-security.mdc      | src/lib/onchain-lib/**/*.hs  | 0 (fixed: OracleValidator output-index)      |
| testing-and-config.mdc   | src/test/**/*                | 0                                            |
| docs-and-api.mdc         | src/exe/*-api/**/*           | 0                                            |
| storage-ingestion.mdc    | offchain-lib, chain-sync     | 0                                            |

**Total distinct inconsistencies: 0** (all six prior inconsistencies have been fixed).

---

## By Rule

### project-architecture.mdc

- **On-chain purity**: Grep for `Aeson|Servant|Swagger` in `src/lib/onchain-lib/` found no matches. **PASS**.
- **Library layout / file reference**: Not fully audited (would require Cabal and cross-file feature checks). No automated inconsistency reported.

---

### executables.mdc

**Fixed:** All four executables now use `die` (System.Exit) instead of `error` when required config is missing.

**Other checks**: Admin has `Command` sum type, `hsubparser`, `executeCommand` with `Either ProviderCtx TxBuildingContext`. Both API servers write Swagger JSON on startup (`BL8.writeFile "docs/swagger/..."). CORS (`WebAPI.CORS.setupCors`) and auth (`getBasicAuthFromEnv`, `basicAuthServerContext`) are used. Chain-sync has probe server, sync loop, and `rollbackTo` on Ahead. **PASS** for structure.

---

### onchain-rules.mdc

**Fixed:** OracleValidator now uses redeemer `OracleUpdate outputIdx` and `checkTxOutAtIndexWithDatumValueAndAddress` for an O(1) output check.

**Other checks**: Types in Protocol/Types.hs and Id.hs; minting uses `mintValueMinted == expectedValue` (exact mint); Cleanup redeemers present; validators use `mkUntypedLambda`; business logic in BJJ/Protocol.Core. **PASS** for these.

---

### offchain-rules.mdc

**Fixed:** Removed `IsString BJJBelt` instance (and thus the `error "Invalid belt"` path). String-to-belt parsing is only via `parseBelt` and `FromHttpApiData.parseQueryParam`.

**Other checks**: Pipeline (Interactions → Operations → Transactions) is followed; `interactionToTxBody` uses `interactionToTxSkeleton` in ReaderT. TxBuilding code uses `TxBuildingException` and `txBuildingExceptionToHttpStatus`; Lookups throw `GYApplicationException` with domain errors. Transfer/Core types use `StripPrefix` + `CamelToSnake`. **PASS** for these.

---

### plutus-rules.mdc

**Fixed:** OracleValidator uses output-index check (see onchain-rules). Lookup.hs trace "K0" is now documented with inline comments on both lines.

**Other checks**: INLINEABLE is placed before the type signature on cross-module onchain functions. Validators use `mkUntypedLambda`. No magic numbers in validator logic (named constants from Onchain.Utils). **PASS** for these.

---

### plutus-security.mdc

**Fixed:** OracleValidator now uses bounded output-index check (see onchain-rules).

**Other checks**: Minting policies use full mint check (`mintValueMinted == expectedValue`). Datum is validated (Cleanup paths and redeemer paths). No further security inconsistencies identified in the sampled code.

---

### testing-and-config.mdc

- **Tests**: Unit tests in `UnitTests.hs` with topic modules (Achievement, Cleanup, Membership, Oracle, Promotion); integration in `TestRuns.hs`; property tests in `BJJPropertyTests.hs`. **PASS**.

---

### docs-and-api.mdc

- **Swagger**: Both API servers write Swagger JSON on startup to `docs/swagger/`. **PASS**.
- **ToSchema**: Query API types (e.g. `SortOrder`, `ProfilesOrderBy`, etc.) have `ToSchema` in `src/exe/query-api/Types.hs`. **PASS**.

---

### storage-ingestion.mdc

- **ChainEventProjection / projectChainEvent**: Defined in `Ingestion.hs`; covers Rank, Profile, Promotion, MembershipHistory, MembershipInterval, Achievement and NoEvent. **PASS**.
- **putMatchAndProjections / put*Projection**: All event kinds have corresponding `put*Projection` in `Storage.hs` and are used in `putMatchAndProjections`. **PASS**.
- **rollbackTo**: `Storage.hs` implements `rollbackTo` and deletes all projection tables (Profile, Rank, Promotion, MembershipHistory, MembershipInterval, Achievement) by slot/header. **PASS**.
- **Idempotence**: Projections use `upsertByUnique` with appropriate `Unique*` constraints. **PASS**.

---

## By Area (optional)

| Area                         | Inconsistencies |
| ---------------------------- | --------------- |
| **src/lib/onchain-lib/**     | (none)          |
| **src/lib/offchain-lib/**    | (none)          |
| **src/exe/admin/**           | (none)          |
| **src/exe/interaction-api/** | (none)          |
| **src/exe/query-api/**       | (none)          |
| **src/exe/chain-sync/**      | (none)          |
| **src/test/**                | (none)          |

---

## Recommendations

1. **OracleValidator**: Replace `any (…) txInfoOutputs` with an output-index check (e.g. redeemer carries the expected output index; use `checkTxOutAtIndexWithDatumValueAndAddress` or equivalent).
2. **Lookup.hs**: Add a short comment documenting trace code "K0" (e.g. wrong datum type for list node / interval).
3. **DomainTypes/Core/BJJ.hs**: Replace `error "Invalid belt"` with a parse result or a domain exception that can be mapped to HTTP (e.g. via TxBuildingException or a dedicated type).
4. **Startup config**: Consider handling missing config without `error` (e.g. explicit exit with message, or document that `error` is intentional for fail-fast startup).

---

## Fixes applied (2025-02-21)

All six inconsistencies from the original report have been addressed:

1. **OracleValidator** — Introduced `OracleRedeemer (OracleUpdate Integer)`; validator now uses `checkTxOutAtIndexWithDatumValueAndAddress` at the redeemer-supplied output index instead of O(n) `any` over `txInfoOutputs`. Offchain `updateOracleTX` passes `OracleUpdate 0`. Blueprint updated to reference `OracleRedeemer`.
2. **Lookup.hs** — Inline comments added for trace code "K0" on both lines (expected ListNodeDatum / IntervalDatum).
3. **DomainTypes/Core/BJJ.hs** — Removed `IsString BJJBelt` instance (and the `error "Invalid belt"` path). Parsing is via `parseBelt` and `FromHttpApiData.parseQueryParam` only.
4. **Startup config** — In query-api, interaction-api, chain-sync, and admin `Main.hs`, replaced `error "..."` on missing config with `die "..."` (System.Exit).
