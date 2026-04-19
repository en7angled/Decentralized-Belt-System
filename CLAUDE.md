# Decentralized Belt System

Cardano-based BJJ belt/rank management system. Five libraries with strict layering; four HTTP servers + admin CLI.

## Environment

- **Toolchain**: Nix + direnv. Run `direnv allow` before building (loads GHC 9.6, Cabal, HLS via devx flake).
- **Build**: `cabal build all` from repo root. No Stack, no `package.yaml`.
- **Test**: `cabal test`. For exUnits/script-size reporting: `scripts/test_exunits.sh`.
- **REPL**: `cabal repl` (e.g. `cabal repl exe:admin`).
- **Clean**: `cabal clean`.
- **Config**: `config/config_atlas.json` (provider), `config/config_bjj_validators.json` (deployed validators), `operation.prv` (admin key, never commit).
- **`.env`**: Local overrides sourced by direnv. Never committed. See README §6.4 for variables.

## Architecture

**Library layering** (inner → outer): `onchain-lib` + `chainsync-lib` → `offchain-lib` → `webapi-lib`. `mcp-server-lib` sits outermost (depends on `offchain-lib`, `onchain-lib`, `webapi-lib`) and is consumed only by the `mcp-server` executable. Dependencies only point inward. `webapi-lib` has no project-library dependencies (HTTP, auth, CORS only). `onchain-lib` must never import off-chain deps (no Aeson, Servant, Swagger).

**Executables**: `admin` (CLI), `interaction-api` (8082), `query-api` (8083), `chainsync-service` (8084), `mcp-server` (8085). Sources in `src/exe/<name>/`. Executables depend on `offchain-lib` + `webapi-lib`; `chainsync-service` also depends on `chainsync-lib`; `mcp-server` is a thin shim over `mcp-server-lib`.

**Three conceptual flows**:
1. **Type hierarchy**: Transfer (API DTOs) → Domain (`Core/Types.hs`, `Core/Actions.hs`) → Onchain (`Protocol/Types.hs`, `Id.hs`). Conversions bridge onchain↔domain.
2. **Interactions → Transactions**: Interaction → `interactionToTxSkeleton` → Operations (Skeletons + Lookups) → `GYTxSkeleton` → `interactionToTxBody` → tx. Do not bypass.
3. **Onchain events → projections**: Chain-sync matches → `projectChainEvent` (Ingestion) → `putMatchAndProjections` (Storage) → DB rows.

**File reference** — where to change what (paths under `src/lib/<lib>/` or `src/exe/<exe>/`):

| Change                    | Files                                                                                                                                                                                                                             |
| ------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| New on-chain type         | `Onchain/Protocol/Types.hs`, `Onchain/Protocol/Id.hs`, `.cabal`                                                                                                                                                                   |
| New validator             | `Onchain/Validators/<Name>Validator.hs`, `Onchain/Protocol.hs`, `TxBuilding/Validators.hs`, `TxBuilding/Context.hs`, `TxBuilding/Transactions.hs`, `.cabal`                                                                       |
| New minting redeemer      | `Onchain/Validators/MintingPolicy.hs`                                                                                                                                                                                             |
| New off-chain domain type | `DomainTypes/Core/Types.hs`, `DomainTypes/Core/Actions.hs`                                                                                                                                                                        |
| New tx operation          | `TxBuilding/Operations.hs`, `TxBuilding/Lookups.hs`, `TxBuilding/Interactions.hs`, `TxBuilding/Conversions.hs`                                                                                                                    |
| New error type            | `TxBuilding/Exceptions.hs`                                                                                                                                                                                                        |
| New query endpoint        | `query-api/RestAPI.hs`, `Query/Projected.hs` or `Query/Live.hs`; extend `Query/Common.hs` for shared helpers; response DTOs in `offchain-lib/DomainTypes/Transfer/QueryResponses.hs`, filter types + `*FromParams` in `Transfer/Filters.hs`, sort enums in `Transfer/OrderBy.hs`; for aggregate/page endpoints see `Query/Aggregates.hs`, `Query/Pages.hs` |
| New projection            | `Storage.hs`, `Ingestion.hs`                                                                                                                                                                                                      |
| New admin command         | `src/exe/admin/Main.hs`                                                                                                                                                                                                           |
| New fee type              | `Onchain/Protocol/Types.hs` (FeeConfig), `TxBuilding/Operations.hs`, `Onchain/Validators/MintingPolicy.hs`                                                                                                                        |
| Updating oracle format    | `Onchain/Protocol/Types.hs` (requires migration/redeployment)                                                                                                                                                                     |
| New test                  | `UnitTests.hs`, `TestRuns.hs`, `UnitTests/<Topic>.hs`                                                                                                                                                                             |

## Haskell Style

- **Naming**: PascalCase types, camelCase functions/fields. `mk` smart constructors, `unsafe` prefix, `is`/`has`/`check` predicates. `derive`/`generate` for ID derivation. Avoid redundant record field prefixes (e.g. use `practitionerId` not `membershipHistoryPractitionerId`).
- **Formatting**: Max 120 chars, 2-space indent, no tabs. One blank line between top-level declarations.
- **Haddock**: Module-level documentation required; prefer `-- |` on exports in new code and library surfaces; `-- ^` on record fields where helpful.
- **Types**: Records for 4+ fields. Small record/newtype for 4+ similar-typed args.
- **JSON**: `deriving-aeson` with `StripPrefix` + `CamelToSnake` for snake_case API fields.
- **Errors**: Use `TxBuildingException`; map to HTTP via `txBuildingExceptionToHttpStatus`. No raw strings.

## Workflow

- **After implementing a plan**: Run `cabal build all` then `cabal test` before considering work done.
- **Config loading**: `decodeConfigEnvOrFile "ENV_VAR" defaultPath`. Ports: `getPortFromEnvOrDefault`.
- **Swagger**: Auto-generated on API startup to `docs/generated/swagger/`. Do not edit by hand.
- **Blueprint**: Generated by `admin write-blueprint`. Do not edit by hand.
- **Version bumps**: Use `cz bump`, then sync `.cabal` version manually. Tags: no `v` prefix.
- **API/blueprint versions**: Independent from app version. Bump in `RestAPI.hs` (Swagger) or `Blueprint.hs`.
- **Scripts** (`scripts/`): Maintained tooling. Update when changing admin CLI, config, or test flows.
- **New concept**: Follow phase order: onchain types → validator → minting → domain types → tx building → API → chain-sync → admin → tests → deployment. See `src/lib/offchain-lib/CLAUDE.md` for full checklist.
