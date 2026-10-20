# F1 design note — Track 2: new repo layout + extraction manifest

Target: the new governance-reputation repo (working name **Palmares**), built by extracting from
`Decentralized-Belt-System` (MIT, freezing) into a new Apache-2.0 repo.

Verified against the real files on disk (`/Users/mg/Projects/CardanoProjects/Decentralized-Belt-System`)
and against primary upstream sources, **2026-10-19**. Every external fact carries its URL inline.
**(decision)** = my call. **(BLOCKER)** = genuine blocker.

---

## 0. Decisions that change the spec before anything is built

These are laziness-rule call-outs: the spec asks for things that already exist upstream or that no
longer have a consumer. Read this section first — it removes one service, one fetcher, ~1200 lines of
extracted code and a whole dependency from the plan.

| # | Spec item | Verdict | Why (one line) |
|---|---|---|---|
| 0.1 | §4.1 **Metadata fetcher** (own fetch + blake2b verify + status + retries + cache) | **DO NOT BUILD** | db-sync already does all of it — see §0.1 below. |
| 0.2 | §4.1 **"Serviciul nostru Haskell devine stratul de proiecții"** as a separate executable | **DO NOT BUILD as a service** | governance projections = versioned SQL views in schema `palmares`, applied by query-api at startup; db-sync owns the cursor, the rollback and the raw log for this path. |
| 0.3 | §4.1 **optional mini-Ogmios type-follower** | **DO NOT BUILD, ever** | Ogmios v7 is the thing that broke Atlas; re-adding it for activity-feed latency is the worst trade in the spec. Poll db-sync. |
| 0.4 | Belt System **live-vs-projected dual query backend** | **DO NOT EXTRACT** | there is no live on-chain source for a DRep's vote history (it lives in tx bodies, not UTxOs). Kills `Query/Live.hs` (657 lines), `RestAPI/Common.hs`, the `liveprojection` flag and the `multiset` dep. |
| 0.5 | §2 **Affiliation "cu intervale"** implemented as a linked list | **DO NOT EXTRACT `LinkedList.hs` / `MembershipsValidator.hs`** | an affiliation with a start and end date is an Attestation with two date fields, not a 400-line list with a head pointer. |
| 0.6 | Image upload path (`IPFS.hs`, `WebAPI/ImageType.hs`, `kubo` compose service) | **DO NOT EXTRACT** | profiles carry an image **URI** the user supplies; anchors are already content-addressed. Kills `servant-multipart` + the IPFS container. |
| 0.7 | `Query/Rules.hs` (formulas as a Haskell endpoint) | **DO NOT EXTRACT** | `docs/metrics.md` served as a static file (and as an MCP resource at F3) satisfies S2 better — a reviewer can read it without GHC. |
| 0.8 | Any composite reputation score | already a non-goal (§10) | restated because it is the first thing that will be requested. |

### 0.1 Why the metadata fetcher must not be built (verified in db-sync source + schema)

db-sync's `off_chain_vote_data` table doc, verbatim:

> "Stores off-chain voting anchor data with validation status. … Only data with hash matches are stored
> here; hash mismatches are stored in `off_chain_vote_fetch_error` for retry. The `is_valid` column
> indicates the parsing status: TRUE … conforms to CIP-100 schema … FALSE: Hash matches and content is
> valid JSON BUT does not conform to CIP-100 … NULL: Hash matches but content is not valid JSON at all."

Source: <https://github.com/IntersectMBO/cardano-db-sync/blob/master/doc/schema.md> (`off_chain_vote_data`,
`off_chain_vote_fetch_error` with `fetch_error` + `retry_count`, plus `off_chain_vote_gov_action_data`,
`off_chain_vote_drep_data`, `off_chain_vote_author`, `off_chain_vote_reference`,
`off_chain_vote_external_update`).

Spec F1 check 7 wants a verdict per anchor in `valid | hash_mismatch | unreachable | oversized`. Mapping,
no new code:

| Our verdict | db-sync source |
|---|---|
| `valid` | row in `off_chain_vote_data` with `is_valid = TRUE` |
| `valid_non_cip100` | row in `off_chain_vote_data` with `is_valid = FALSE` |
| `malformed_json` | row in `off_chain_vote_data` with `is_valid IS NULL` |
| `hash_mismatch` / `unreachable` / `oversized` | row in `off_chain_vote_fetch_error`, discriminated on `fetch_error` text |

Verified in `cardano-db-sync/src/Cardano/DbSync/OffChain/Http.hs` @13.7.2.1, so the discriminators are known
constructors rather than guesswork: `OCFErrHashMismatch` (L138), `OCFErrDataTooLong` (L208) at a **3 MB**
cap (`httpGetBytes manager request 3000000 3000000`, L114), `OCFErrTimeout url "Response"|"Connection"`
(L258-259), `OCFErrJsonDecodeFail`, `OCFErrBadContentTypeHtml`, plus `OCFErrNoIpfsGateway` /
`OCFErrIpfsGatewayFailures` — db-sync resolves `ipfs://` anchors itself through the `ipfs_gateway` config key
(default `["https://ipfs.io/ipfs"]`, `Config/Types.hs:456`), which also covers the spec's IPFS-anchored
dispute reasons.

**Two documented holes in "db-sync already does all of it" — write both into the ADR:**
1. **Constitution anchors are never fetched.** `queryNewVoteWorkQueueDataStmt`
   (`cardano-db/src/Cardano/Db/Statement/OffChain.hs:536-551`) has `AND va.type != 'constitution'`. Every
   other `voting_anchor.type` (`gov_action`, `drep`, `other`, `vote`, `committee_dereg`) **is** fetched — in
   particular `vote`, which is what spec §4.2's rationale-rate metric needs. If a constitution anchor ever
   has to be displayed, that one is ours to fetch; it is one document per era, not a fetcher service.
2. **Duplicate child rows.** db-sync 13.7.2.1's own release notes list under Known issues: "duplicate
   off-chain vote metadata child rows https://github.com/IntersectMBO/cardano-db-sync/pull/2137". So
   `sql/020_anchors.sql` must `DISTINCT`/aggregate over `off_chain_vote_author`,
   `off_chain_vote_reference` and `off_chain_vote_external_update` rather than counting rows.

Also confirmed: the fetch thread is gated on **both** flags — `when (ioOffChainVoteData iopts && ioGov
iopts)` (`DbSync/OffChain.hs:271`) — and `only_governance` + `offchain_vote_data: enable` satisfies both.

**(decision)** The one catch: the `only_governance` preset sets `offchain_vote_data: disable`, and
`doc/configuration.md` claims "A preset overrides all other properties". **That doc statement is wrong.**
The source parses the preset as a *base* and lets sibling keys override it per-key:

```haskell
-- cardano-db-sync/src/Cardano/DbSync/Config/Types.hs
instance FromJSON SyncInsertConfig where
  parseJSON = Aeson.withObject "SyncInsertConfig" $ \obj -> do
    preset <- obj .:? "preset"
    baseOptions <- case preset of
      Just OnlyGovInsertPreset -> pure onlyGovInsertOptions
      ...
    options <- parseOverrides obj baseOptions     -- <= sibling keys override the preset
```
Source: <https://github.com/IntersectMBO/cardano-db-sync/blob/master/cardano-db-sync/src/Cardano/DbSync/Config/Types.hs>
(`FromJSON SyncInsertConfig`, `parseOverrides`, `onlyGovInsertOptions`).

So the config is **`{"preset": "only_governance", "offchain_vote_data": "enable"}`** — see §1.5 for the
exact file. One key, zero code.

Also verified in source, so the plan can stop worrying about it: `shelley: disable` (which the preset
sets) does **not** drop vote delegations — `delegation_vote` inserts are gated on `ioGov`, not `ioShelley`:

```haskell
DelegVote drep       -> when (ioGov iopts) $ insertDelegationVote syncEnv network txId idx cred drep
DelegStakeVote _ drep -> ... when (ioGov iopts) $ insertDelegationVote syncEnv network txId idx cred drep
```
Source: <https://github.com/IntersectMBO/cardano-db-sync/blob/master/cardano-db-sync/src/Cardano/DbSync/Era/Universal/Insert/Certificate.hs>
(≈ lines 148–157).

Exact `only_governance` expansion, from the same `Types.hs`, so the plan knows precisely what is empty:

```haskell
onlyGovInsertOptions = disableAllInsertOptions
  { sioLedger = LedgerEnable, sioGovernance = GovernanceConfig True, sioPoolStats = PoolStatsConfig True }
-- disableAllInsertOptions: tx_cbor off, tx_out DISABLE, shelley DISABLE, rewards off, multi_asset off,
-- metadata off, plutus off, offchain_pool_data off, offchain_vote_data off, json_type text, epoch on
```

Consequences to write into the ADR: `tx_out`/`tx_in`/`ma_tx_out` are **empty** and `tx.fee` is **0** under
this preset — so db-sync can never answer anything about our attestation NFTs or their datums. That is
exactly why the Kupo pipeline stays for the attestation protocol (spec §4.1, confirmed correct).

---

## 1. New-repo file tree

### 1.0 Package shape

**(decision) One cabal package (`palmares.cabal`) with four internal libraries + four executables + one
test-suite — the Belt System's exact shape.** Not a multi-package project.

Why: cabal's named internal libraries are already separate components with their own `build-depends`, so
the layering doctrine (`onchain-lib` must never see Aeson) is enforced by the build graph either way.
Multi-package would buy per-library versions and sdists, which nothing needs, and would cost one glob in
`Dockerfile.base` (`COPY *.cabal cabal.project cabal.project.freeze ./`) turning into six explicit COPYs.
Smallest thing that works.

### 1.1 Tree

```
palmares/
├── cabal.project                     # pins (§1.4) — carried from Belt System, verified
├── cabal.project.freeze              # generated by `cabal freeze` after first successful solve; committed
├── palmares.cabal                    # §1.3
├── LICENSE                           # Apache-2.0 full text
├── NOTICE                            # Apache-2.0 §4(d) attribution (§3.3)
├── licenses/
│   └── LICENSE.decentralized-belt-system.MIT   # verbatim MIT text + 2025 Marius Georgescu copyright
├── README.md                         # §1.6 skeleton
├── CLAUDE.md                         # root doctrine; adapted from Belt System root CLAUDE.md
│                                     # (CORRECTED: the Belt System has FIVE CLAUDE.md files — root,
│                                     #  src/exe/, src/lib/offchain-lib/, src/lib/onchain-lib/, src/test/.
│                                     #  All five are ported, and onchain-lib's MUST be edited: it encodes
│                                     #  the old `checkFee … selector (FeeConfig -> Integer)` contract that
│                                     #  fix #1 deletes, plus `Onchain.LinkedList`, `Onchain.BJJ` /
│                                     #  `Protocol.Core` business logic and `unsafeGetRank` /
│                                     #  `unsafeGetMembershipInterval`, all of which §0.5 and §2.2 drop.)
├── .envrc                            # devx flake pin, copied as-is bar one env var (§1.7)
├── .env.example                      # §1.8
├── .gitignore                        # Belt System's, minus the PureScript block (§1.9)
├── .dockerignore                     # copy as-is
├── .justfile                         # copy as-is (allow/reload/repl/link-hls/gc)
├── hie.yaml                          # `cradle: {cabal: }` — copy as-is
├── libblst.pc                        # copy as-is; Dockerfile.base COPYs it
├── Dockerfile.base                   # copy as-is, s/Decentralized-Belt-System/palmares/ in WORKDIR
├── Dockerfile.runtime-base           # copy as-is
├── Dockerfile.admin                  # new, same 2-stage shape
├── Dockerfile.query-api              # copy + rename
├── Dockerfile.interaction-api        # copy + rename
├── Dockerfile.attest-sync            # copy + rename (was Dockerfile.chainsync)
├── docker-compose.yml                # §1.10 — dev/audit stack
├── .github/workflows/
│   ├── ci.yml                        # NEW, runs-on: ubuntu-latest (§6.3)
│   └── images.yml                    # NEW, runs-on: self-hosted, matrix over 4 services (§6.3)
├── config/
│   ├── config_atlas.example.json         # Atlas core config; real file gitignored (carries the provider token)
│   ├── config_validators.preprod.json    # CORRECTED: COMMITTED, not gitignored — see §1.9
│   ├── node/                             # CORRECTED: the node needs its own config + genesis files
│   │   ├── preprod-config.json           # cardano-node config, from IOG book.play.dev.cardano.org
│   │   ├── preprod-topology.json
│   │   ├── preprod-byron-genesis.json
│   │   ├── preprod-shelley-genesis.json
│   │   ├── preprod-alonzo-genesis.json
│   │   └── preprod-conway-genesis.json
│   └── db-sync/
│       └── preprod-db-sync-config.json   # §1.5 — logging block + only_governance + offchain_vote_data
├── sql/                              # the governance "projections" (§0.2). Applied in lexical order.
│   ├── 000_schema.sql                #   CREATE SCHEMA IF NOT EXISTS palmares / palmares_attest
│   ├── 010_gov_views.sql             #   drep, gov_action, vote, vote_delegation, drep_epoch_power views
│   ├── 020_anchors.sql               #   anchor + verdict view over off_chain_vote_data/_fetch_error
│   ├── 030_drep_metrics.sql          #   the 6 v1 metrics, one view each, formula in a SQL comment
│   └── 040_indexes.sql               #   indexes on db-sync columns our views filter on
├── docs/
│   ├── adr/
│   │   ├── 0001-governance-data-substrate.md   # F0 exit criterion (spec §9 F0)
│   │   └── 0002-write-path-provider.md         # Maestro vs local-node (§7 risk)
│   ├── metrics.md                    # the published formulas (S2); mirrors sql/030 comments
│   ├── onchain-architecture.md
│   ├── onchain-trace-codes.md
│   ├── deployment/
│   └── generated/                    # swagger + CIP-57 blueprint; gitignored except .gitkeep
├── scripts/
│   ├── build-images.sh               # copy + rename — the manual escape hatch when CI is down
│   ├── reconcile_koios.sh            # NEW — F1 check 6 (§1.11)
│   ├── smoke_attest_flow.sh          # NEW — F1 check 11, modelled on test_black_promotes_white_to_blue.sh
│   └── test_exunits.sh               # copy as-is
└── src/
    ├── lib/onchain-lib/Palmares/Onchain/        # PlutusTx only. No Aeson/Servant/Swagger. Ever.
    ├── lib/chainsync-lib/Palmares/ChainSync/    # Kupo HTTP client + Kupo→Atlas decoding. No domain types.
    ├── lib/offchain-lib/Palmares/               # domain types, TxBuilding/*, Storage, Ingestion
    ├── lib/webapi-lib/Palmares/WebAPI/          # HTTP/auth/CORS/probe only. No project-library deps.
    ├── exe/admin/                               # CLI: deploy, blueprint, oracle ops, dispute arbitration
    ├── exe/query-api/                           # :8083 public read API + Swagger + SQL view bootstrap
    ├── exe/interaction-api/                     # :8082 build-tx / submit-tx
    ├── exe/attest-sync/                         # :8084 Kupo → raw log → attestation projections
    └── test/                                    # CLB emulator suite + in-memory SQLite storage tests
```

Deliberately absent in F0/F1: `src/lib/mcp-server-lib/`, `src/exe/mcp-server/`, `src/test-mcp-integration/`
— F3 (spec §9 F1 non-goals). Created when F3 starts, from the manifest rows in §2.6.

**(decision)** Dockerfiles stay at the repo root, not in `deploy/`. `.dockerignore` already excludes
`Dockerfile*` and the workflows reference them by root path; moving them is churn for zero gain.

### 1.2 Database layout

**(decision) One Postgres database — db-sync's own (`cexplorer`) — with two added schemas.**

| Schema | Owner | Contents | Lifecycle |
|---|---|---|---|
| `public` | cardano-db-sync | all db-sync tables; **read-only to us** | db-sync migrations |
| `palmares` | our `sql/*.sql` | governance views (§0.2) | dropped + recreated at every query-api start — see the DDL rule below |
| `palmares_attest` | `Palmares.Storage` (persistent) | `chain_cursor`, `chain_sync_config`, `onchain_match_event`, `profile_projection`, `attestation_projection`, `dispute_projection` | persistent migrations + the schema-version-gated wipe |

Why one database: a Postgres view cannot cross databases, so the governance views must live where
db-sync's tables live. Putting the attestation projections in the same database too means **one
connection pool and one `PG_CONN_STR`** everywhere, instead of two of each. `wipeChainSyncTablesRaw`
becomes `DROP TABLE IF EXISTS palmares_attest.<t> CASCADE` — scoped, so a chain-sync schema bump can
never touch db-sync's ~16 GB. Set `search_path` per pool rather than qualifying every query.

**(CORRECTED) The view DDL must be `DROP SCHEMA … CASCADE`, not `CREATE OR REPLACE VIEW`.** PostgreSQL:
"The new query must generate the same columns that were generated by the existing view query (that is, the
same column names in the same order and with the same data types), but it may add additional columns to the
end of the list" (<https://www.postgresql.org/docs/17/sql-createview.html>). So `CREATE OR REPLACE VIEW` is
*not* idempotent across a shape change — the first time `sql/010` drops or retypes a column, query-api
fails to boot. `sql/000_schema.sql` therefore starts with:
```sql
DROP SCHEMA IF EXISTS palmares CASCADE;
CREATE SCHEMA palmares;
```
and `sql/010`–`040` use plain `CREATE VIEW` / `CREATE INDEX`. The whole file set runs in one transaction,
so a failure leaves the previous schema in place.

**(CORRECTED) `palmares_attest` is created by attest-sync, not by query-api's `sql/000`.** attest-sync is
the process that runs the `persistent` migrations, and in the compose/k8s topology it does not depend on
query-api having started. If the schema does not exist when `runMigrations` fires, `persistent` creates its
tables in whatever `search_path` resolves to — i.e. db-sync's `public`. So: attest-sync executes
`CREATE SCHEMA IF NOT EXISTS palmares_attest` and sets `search_path` **before** `runMigrations`
(`src/exe/attest-sync/Main.hs`, immediately before the schema probe); `sql/000_schema.sql` creates only
`palmares`.

Two caveats for the runbook:
- A db-sync volume wipe takes our schemas with it. Recovery = re-run attest-sync from origin (idempotent by
  design) + restart query-api (recreates views). Nothing is lost that is not re-derivable.
- **db-sync migrations `CASCADE`-drop dependent views.** `schema/migration-2-0049-20260604.sql` begins
  `DROP TABLE IF EXISTS "epoch" CASCADE;`. Any `palmares` view over a db-sync table that a future migration
  drops is silently removed. That is survivable because query-api recreates the views at boot — but it is
  the reason the `palmares` views must stay **plain `VIEW`s, never `MATERIALIZED VIEW`s** over db-sync
  tables (a CASCADE-dropped materialized view loses its data with no error), and the reason "restart
  query-api after any db-sync upgrade" belongs in the runbook.

### 1.3 `palmares.cabal` (real dependency lists)

Derived from the Belt System's per-component `build-depends`, minus what §0 drops. Deltas from the Belt
System are flagged `-- DELTA`.

```cabal
cabal-version: 3.0
name:          palmares
version:       0.1.0
synopsis:
  Cardano governance reputation and track-record registry: an independently derived explorer over
  cardano-db-sync, plus a countersigned on-chain attestation protocol.
homepage:      https://github.com/<owner>/palmares
license:       Apache-2.0
license-file:  LICENSE
author:        Marius Georgescu
maintainer:    georgescumarius@live.com
category:      Web3
build-type:    Simple

extra-source-files:
  NOTICE
  licenses/LICENSE.decentralized-belt-system.MIT
  sql/*.sql                      -- DELTA: embedded via file-embed by query-api's view bootstrap

common common-options
  ghc-options:      -Wall -Wunused-packages
  default-language: GHC2021
  build-depends:    base

common plinth-ghc-options
  -- copy the Belt System block byte-for-byte; it encodes the plutus-tx-plugin contract:
  --   -fno-ignore-interface-pragmas -fno-omit-interface-pragmas -fno-full-laziness -fno-spec-constr
  --   -fno-specialise -fno-strictness -fno-unbox-strict-fields -fno-unbox-small-strict-fields
  --   -fconstraint-solver-iterations=0 -fobject-code
  --   -fplugin-opt PlutusTx.Plugin:defer-errors
  --   -fplugin-opt PlutusTx.Plugin:target-version=1.1.0
  --   -fplugin-opt PlutusTx.Plugin:remove-trace -fplugin-opt PlutusTx.Plugin:optimize
  default-language:   GHC2021
  default-extensions: TemplateHaskell

library onchain-lib
  import:           common-options, plinth-ghc-options
  default-language: Haskell2010
  hs-source-dirs:   src/lib/onchain-lib
  exposed-modules:
    Palmares.Onchain.Blueprint
    Palmares.Onchain.CIP68
    Palmares.Onchain.Protocol
    Palmares.Onchain.Protocol.Id
    Palmares.Onchain.Protocol.Lookup
    Palmares.Onchain.Protocol.Types
    Palmares.Onchain.Utils
    Palmares.Onchain.Validators.AttestationsValidator
    Palmares.Onchain.Validators.DisputesValidator
    Palmares.Onchain.Validators.MintingPolicy
    Palmares.Onchain.Validators.OracleNFTPolicy
    Palmares.Onchain.Validators.OracleValidator
    Palmares.Onchain.Validators.ProfilesValidator
  if !(impl(ghcjs) || os(ghcjs))
    build-depends: plutus-tx-plugin
  build-depends:    bytestring, containers
  build-depends:    plutus-core, plutus-ledger-api, plutus-tx
  default-extensions: -- copy the Belt System's 28-entry list verbatim (measured: BangPatterns …
                      -- ViewPatterns, Decentralized-Belt-System.cabal L97-125)

library chainsync-lib
  import:           common-options
  default-language: Haskell2010
  hs-source-dirs:   src/lib/chainsync-lib
  exposed-modules:  Palmares.ChainSync.KupoAtlas, Palmares.ChainSync.KupoClient
  build-depends:
    aeson, base16-bytestring, cardano-api, containers, extra, http-client, http-client-tls,
    persistent, servant, servant-client, text
  build-depends:    atlas-cardano
  default-extensions: DataKinds, DeriveGeneric, NamedFieldPuns, OverloadedStrings, TemplateHaskell, TypeOperators

library offchain-lib
  import:           common-options
  default-language: Haskell2010
  hs-source-dirs:   src/lib/offchain-lib
  exposed-modules:
    Palmares.Constants
    Palmares.DomainTypes.Core.Actions
    Palmares.DomainTypes.Core.Types
    Palmares.DomainTypes.Transfer.Filters
    Palmares.DomainTypes.Transfer.OrderBy
    Palmares.DomainTypes.Transfer.QueryResponses
    Palmares.DomainTypes.Transfer.Types
    Palmares.Ingestion
    Palmares.Storage
    Palmares.TxBuilding.Context
    Palmares.TxBuilding.Conversions
    Palmares.TxBuilding.Exceptions
    Palmares.TxBuilding.Interactions
    Palmares.TxBuilding.Lookups
    Palmares.TxBuilding.Operations
    Palmares.TxBuilding.Skeletons
    Palmares.TxBuilding.Transactions
    Palmares.TxBuilding.Utils
    Palmares.TxBuilding.Validators
    Palmares.Utils
  -- DELTA: no DomainTypes.Core.BJJ, no DomainTypes.Rules.Promotion, no TxBuilding.SafeOnchainLogic
  build-depends:
    aeson, bytestring, chainsync-lib, containers, deriving-aeson, extra, lens, mtl, onchain-lib,
    persistent, servant-server, swagger2, text, time
  build-depends:    atlas-cardano, cardano-api, plutus-ledger-api, plutus-tx

library webapi-lib
  import:           common-options
  default-language: Haskell2010
  hs-source-dirs:   src/lib/webapi-lib
  exposed-modules:
    Palmares.WebAPI.Auth, Palmares.WebAPI.CORS, Palmares.WebAPI.Errors,
    Palmares.WebAPI.ServiceProbe, Palmares.WebAPI.Utils
  -- DELTA: no WebAPI.ImageType (§0.6)
  build-depends:    aeson, bytestring, http-types, lens, memory, servant-server, swagger2, text, time, wai, wai-cors
  default-extensions: DataKinds, NamedFieldPuns, OverloadedStrings, TypeOperators

executable admin
  import:           common-options
  main-is:          Main.hs
  hs-source-dirs:   src/exe/admin
  build-depends:    offchain-lib, onchain-lib
  build-depends:    aeson, aeson-pretty, bytestring, mtl, optparse-applicative, text
  build-depends:    atlas-cardano, plutus-ledger-api

executable query-api
  import:           common-options
  main-is:          Main.hs
  hs-source-dirs:   src/exe/query-api
  ghc-options:      -threaded -rtsopts -with-rtsopts=-T -with-rtsopts=-N -O2
  other-modules:
    Query.Aggregates, Query.Common, Query.Gov, Query.Pages, Query.Projected, Query.ServiceStatus,
    Query.Views, QueryAppMonad, RestAPI
  -- DELTA: no Query.Live, no Query.Rules, no Query.ExplorerMonthly, no RestAPI.Common (§0.4, §0.7)
  -- NEW: Query.Gov (raw SQL over schema `palmares`), Query.Views (file-embed + apply sql/*.sql at boot)
  build-depends:    offchain-lib, onchain-lib, webapi-lib
  build-depends:
    aeson, aeson-pretty, bytestring ^>=0.11.5, containers, esqueleto, file-embed, lens ^>=5.2.3,
    monad-logger, mtl, persistent, persistent-postgresql, servant-options, servant-server,
    servant-swagger, servant-swagger-ui, swagger2, text, time, wai, warp
  -- DELTA: dropped `multiset` (only Query.Live used it); added `file-embed` for sql/*.sql
  build-depends:    atlas-cardano, plutus-ledger-api

executable interaction-api
  import:           common-options
  main-is:          Main.hs
  hs-source-dirs:   src/exe/interaction-api
  ghc-options:      -threaded -rtsopts -with-rtsopts=-T -with-rtsopts=-N -O2
  other-modules:    InteractionAppMonad, RestAPI, ServiceHandlers, ServiceRequests
  -- DELTA: no IPFS (§0.6)
  build-depends:    offchain-lib, webapi-lib
  build-depends:
    aeson, aeson-pretty, bytestring ^>=0.11.5, deriving-aeson, http-types ^>=0.12.4, lens ^>=5.2.3,
    mtl, servant-options, servant-server, servant-swagger, servant-swagger-ui, swagger2, text, time,
    wai, wai-extra, warp
  -- DELTA: dropped servant-multipart, http-client, http-client-tls (all IPFS-only)
  build-depends:    atlas-cardano

executable attest-sync
  import:           common-options
  main-is:          Main.hs
  hs-source-dirs:   src/exe/attest-sync
  ghc-options:      -threaded -rtsopts -with-rtsopts=-T -with-rtsopts=-N -O2
  other-modules:    ChainsyncAPI, ChainSyncLogic, ChainSyncServer
  build-depends:    chainsync-lib, offchain-lib, webapi-lib
  build-depends:
    bytestring, deriving-aeson, extra, monad-logger, persistent, persistent-postgresql,
    servant-server, swagger2, text, time, warp
  build-depends:    atlas-cardano

test-suite test
  import:           common-options
  type:             exitcode-stdio-1.0
  hs-source-dirs:   src/test
  main-is:          Main.hs
  other-modules:
    PropertyTests, Test.Fixtures, Test.Helpers, TestRuns, UnitTests, UnitTests.ApiHardening,
    UnitTests.Attestation, UnitTests.ChainSyncReplay, UnitTests.Cleanup, UnitTests.Conversions,
    UnitTests.Dispute, UnitTests.Oracle, UnitTests.Profile, UnitTests.StorageRoundtrip
  build-depends:    chainsync-lib, offchain-lib, onchain-lib, webapi-lib
  build-depends:
    aeson, bytestring, containers, extra, hedgehog, http-types, mtl, persistent, persistent-sqlite,
    servant-server, tasty, tasty-hedgehog, tasty-hunit, wai, wai-cors, warp
  -- DELTA: dropped mcp, mcp-server-lib, http-client, http-client-tls, servant, servant-client (F3)
  build-depends:    atlas-cardano, plutus-ledger-api, plutus-tx
```

### 1.4 `cabal.project` — exact pins, with the verification behind each

**(decision) Carry the Belt System's `cabal.project` forward verbatim except for the comments.** Do not
bump a single pin during extraction — extraction already changes ~20k lines; a dependency bump on top of
that makes a build break unattributable. Bumps are a separate, post-F0 workstream.

```cabal
packages: ./

repository cardano-haskell-packages
  url: https://chap.intersectmbo.org/
  secure: True
  root-keys:
    3e0cce471cf09815f930210f7827266fd09045445d65923e6d0238a6cd15126f
    443abb7fb497a134c343faf52f0b659bd7999bc06b7f63fa76dc99d631f9bea1
    a86a1f6ce86c449c46666bda44268677abf29b5b2d2eb5ec7af903ec2f117a82
    bcec67e8e99cabfa7764d75ad9b158d72bfacf70ca1d0ec8bc6b4406d1bf8413
    c00aae8461a256275598500ea0e187588c35a5d5d7454fb57eac18d9edb86a56
    d4a35cd3121aa00d18544bb0ac01c3e1691d618f462c46129271bccf39f7e8ee

tests: true

-- repeated for hackage.nix's parser
index-state: 2026-02-20T00:00:00Z
index-state:
  , hackage.haskell.org 2026-02-20T00:00:00Z
  , cardano-haskell-packages 2025-04-11T16:42:25Z

test-show-details: direct

package cardano-crypto-praos
  flags: -external-libsodium-vrf
package cardano-node
  flags: -systemd
package postgresql-libpq
  flags: +use-pkg-config

-- Atlas. Pinned PAST the v0.14.1 tag (2025-05-21) to the commit the Belt System proved out.
-- bf0d2d1 = "Merge PR #472 … allow utxos query on empty addresses list for Maestro provider", 2025-08-27.
source-repository-package
  type: git
  location: https://github.com/geniusyield/atlas
  tag: bf0d2d13db3583f5b399344ef9893afc1ccbf335
  --sha256: sha256-4jh/I3xK9F4Gn9HDwoICesyfbbjTFwPkHS5iQe8p14Y=

-- Maestro Haskell SDK. NOT optional and NOT temporary: `maestro-sdk` has no Hackage release and no CHaP
-- release, and upstream is ARCHIVED. (The repo the Belt System's own TODO names is
-- github.com/maestro-org/haskell-sdk, which now redirects to maestro-org/maestro-cardano-haskell-sdk:
-- "archived": true, last push 2026-04-30 — use that one date, not two.) Atlas's Maestro
-- provider needs it, so this fork pin is load-bearing. See docs/adr/0002-write-path-provider.md.
source-repository-package
  type: git
  location: https://github.com/sourabhxyz/haskell-sdk
  tag: 1e84ccb43aa0f56dd2776e64a9a7341441fc623e
  --sha256: sha256-O9WSVdqZfKYHt5IBCF3+nknswNl/VH8/FTmTW/iAgb8=

-- CLB emulator (test harness). This commit IS the repo HEAD (2025-06-04); nothing newer exists.
source-repository-package
  type: git
  location: https://github.com/mlabs-haskell/clb
  tag: 867ff70b4d3086f728733f25509692cda41ea0ef
  --sha256: sha256-goNGkysHX5ips1q5KKJi6hzSsSVf6f3aD9XFzL1QU08=
  subdir:
    clb

-- cardano-addresses. Still required: IntersectMBO/cardano-addresses#294 ("Release new version 4.0.0") is
-- closed, but cardano-addresses-4.0.0 is NOT on CHaP (404; CHaP tops out at 3.12.0 as of 2026-10-19).
source-repository-package
  type: git
  location: https://github.com/IntersectMBO/cardano-addresses
  tag: d611632fc3d616d5b4038a70f864bf2613c125d0
  --sha256: sha256-vQ2XB95kw05IZuRkyK4cPQtaKZ1bZAoLtN9GrOOwQvM=
```

Verification table for each pin:

| Pin | Status 2026-10-19 | Source |
|---|---|---|
| Atlas `bf0d2d1` | commit dated 2025-08-27. Latest release still **v0.14.1 (2025-05-21)** → 17 months no release. `main` last pushed **2026-02-03**, 52 open issues, not archived. Newer-than-pin commits exist (Hydra provider, exported provider exception types) — bump candidate, not now. | `api.github.com/repos/geniusyield/atlas` + `/releases` |
| maestro fork `1e84ccb` | commit 2025-05-20 ("add `IsTimestamped`/`HasCursor`"). Fork last pushed 2025-11-04. **Upstream `maestro-org/maestro-cardano-haskell-sdk` is archived**, last commit 2026-03-12. `maestro-sdk` not on Hackage, not on CHaP. | `api.github.com/repos/{sourabhxyz,maestro-org}/...`; `hackage.haskell.org/package/maestro-sdk` → 404; `chap.intersectmbo.org/package/maestro-sdk-1.7.0|1.7.1|1.8.0` → 404 |
| clb `867ff70` | **is HEAD** (2025-06-04) | `api.github.com/repos/mlabs-haskell/clb/commits` |
| cardano-addresses `d611632` | required; 4.0.0 absent from CHaP | `chap.intersectmbo.org/package/cardano-addresses-4.0.0` → 404, `-3.12.0` → 200 |
| devx `d316c039` (in `.envrc`) | pinned flake rev dated 2025-02-27; devx `main` has moved to 2026-04-23. Pinned flake = reproducible → **copy as-is, do not bump.** | `api.github.com/repos/input-output-hk/devx/commits` |
| `benz0li/ghc-musl:9.6.6` | exists, **multi-arch amd64 + arm64**, last updated 2025-02-17 → the ARM minirack can build it. | `hub.docker.com/v2/repositories/benz0li/ghc-musl/tags/9.6.6` |

`cabal.project.local` is gitignored; document the Plu-Stan/HLS content in README (it is `ignore-project: False`
+ `program-options ghc-options: -fwrite-ide-info -hiedir=.hie`).

### 1.5 `config/db-sync/preprod-db-sync-config.json` (the one key that matters)

**(CORRECTED 2026-10-19 — do not use a hand-written minimal config.)** The db-sync config file is NOT a
six-key object. `SyncPreConfig`'s second field is `pcLoggingConfig :: Logging.Representation`, parsed from
the *same* top-level object (`parseJSON (Object o)` in `parseGenSyncNodeConfig`), and
`instance FromJSON Representation = implicit_fill_representation <$> genericParseJSON defaultOptions`, whose
non-`Maybe` fields `minSeverity`, `setupScribes`, `defaultScribes`, `setupBackends`, `defaultBackends` and
`options` are **required**. A minimal object fails to parse at startup.
Sources: `cardano-db-sync/src/Cardano/DbSync/Config/Types.hs` @13.7.2.1 L440–456 (`parseGenSyncNodeConfig`);
`iohk-monitoring/src/Cardano/BM/Data/Configuration.lhs` L94–113.

Procedure: **start from db-sync's own shipped file**
<https://github.com/IntersectMBO/cardano-db-sync/blob/13.7.2.1/config/preprod-config.yaml> (converted to
JSON or kept as YAML — both are accepted), then change exactly two things:

```jsonc
{
  // ... the whole logging block from the shipped preprod-config.yaml, verbatim ...
  "NetworkName": "preprod",
  // The CARDANO-NODE config, path relative to THIS file. NOT this file.
  "NodeConfigFile": "../node/preprod-config.json",
  "EnableLogMetrics": false,
  "EnableLogging": true,
  "insert_options": {
    "preset": "only_governance",
    "offchain_vote_data": "enable"
  }
}
```

Two further corrections to the earlier draft of this section:
- `"NodeConfigFile": "preprod-config.json"` was **self-referential** — it pointed the node-config key at
  the db-sync config file itself. db-sync's own comment: "The config file for the node we are connecting
  to. If this is not the correct config, it will likely lead to db-sync throwing up weird error messages
  from the consensus layer. The path to the node config file is relative to this config file."
- `"RequiresNetworkMagic": "RequiresMagic"` is **not a db-sync config key**. `parseGenSyncNodeConfig` never
  reads it (`dncRequiresNetworkMagic` is derived from the *node* config). Drop it — a stray key is silently
  ignored and misleads the next reader.

`offchain_vote_data: enable` is what kills the custom metadata fetcher (§0.1). `ledger` is already
`enable` inside the preset — do not restate it; restating invites someone to "simplify" it to `disable`
and silently empty `drep_distr`.

db-sync version: **13.7.2.1** is the newest *release* (published 2026-06-17;
<https://github.com/IntersectMBO/cardano-db-sync/releases/latest>). A `13.7.3.0` git tag and ghcr image
exist with no GitHub release — ignore them.

**(BLOCKER, CORRECTED) The official db-sync image is amd64-only; the minirack is ARM64-only.**
`ghcr.io/intersectmbo/cardano-db-sync:13.7.2.1`'s image config reads `"architecture": "amd64"` (single
manifest, not an index), and the release notes say "Platforms: Linux 64-bit". `apps-k8s-configs/CLAUDE.md`
says, verbatim: "**ARM64-only workloads.** Cluster is Raspberry Pi. If you pick an image, verify it has an
`arm64` manifest." So spec §8 / F1 check 1 ("db-sync in minirack") cannot be met with the official image.
Decision for the plan: **use `ghcr.io/blinklabs-io/cardano-db-sync`, whose newest 13.7.x tag is `13.7.1.0`
and which publishes a real `manifest.list` with `{amd64, arm64}`.** 13.7.1.0 carries everything §0.1
depends on — verified at tag `13.7.1.0`: `onlyGovInsertOptions`, `parseOverrides obj baseOptions`,
`offchain_vote_data` override key, and `ledger_backend` defaulting to `LedgerBackendInMemory`. So the ARM
pin is **13.7.1.0, not 13.7.2.1**; the only thing given up is the 13.7.2.1 `epoch`-table redesign, which
our governance views do not need. (Alternative, more work: build db-sync arm64 ourselves.)

Keep the default `InMemory` ledger backend — db-sync's 13.7.2.1 known issues list the LSM backend as
crashing "due to how `ghc` handles system threads. This only happens on resource-constrained devices such
as the Raspberry Pi". Node: **11.1.2** is fine (11.1.3 landed 2026-09-29). Note that
`ghcr.io/intersectmbo/cardano-node:11.1.2` *is* multi-arch (`11.1.2-amd64` + `11.1.2-arm64` + a list), and
the minirack's existing preprod node already runs `ghcr.io/blinklabs-io/cardano-node:11.1.2`
(`apps-k8s-configs/chains/cardano/preprod/workloads/statefulset-cardano-preprod.yaml:81`) — match the repo's
existing choice rather than introducing a second publisher.

### 1.6 `README.md` skeleton (headings only — the plan fills them)

```
# Palmares — Cardano governance reputation & track-record registry
1. What this is / what it is not            (lift §1 + §10 of the spec; the non-goals go ABOVE the fold)
2. Reproduce our numbers yourself           (the S2 claim, as a runnable recipe: node + db-sync + this repo)
3. Architecture in one diagram              (spec §5 data flow)
4. Quick start (dev/audit)                  (docker-compose up; what each service is; what needs a key)
5. Build from source                        (§6 of this note, verbatim)
6. Configuration                            (every env var, with "required / default / fail-closed")
7. The metrics, and their formulas          (link docs/metrics.md; "no composite score, by design")
8. Self-hosting: what is autonomous, what is not  (read path 100% own node; write path needs a provider key)
9. On-chain protocol                        (link docs/onchain-architecture.md + the CIP-57 blueprint)
10. Contributing / licence / attribution    (Apache-2.0; derived from Decentralized-Belt-System, MIT)
```

### 1.7 `.envrc`

Copy the Belt System's byte-for-byte with exactly two substitutions:
`BJJ_ENV_LOADED` → `PALMARES_ENV_LOADED`, and the final `echo` string. Everything else is load-bearing
macOS/Nix plumbing that took real debugging: `GC_DONT_GC=1` on Darwin, the nix-direnv 3.0.6 `source_url`
with its sha256, `CABAL_DIR="$PROJECT_ROOT/.cabal"`, the `xz.dev` pkgconfig `PATH_add`, the
`PKG_CONFIG_PATH_FOR_TARGET` merge, and the `libblst` → `libblst-any` `.pc` alias generation
(`cardano-crypto-class` pkgconfig-depends on `libblst-any`, nixpkgs only ships `libblst.pc`). Do not
"clean this up".

Flake pin to keep: `use flake "github:input-output-hk/devx?rev=d316c0394de3c72abd2d79d1b55a3b6abc9d0f0c#ghc96-iog-full"`.

### 1.8 `.env.example`

```sh
# ---- required by every HTTP server (fail-closed: servers `die` if unset) ----
BASIC_USER=changeme          # interaction-api + admin only; query-api is PUBLIC (spec §4.2)
BASIC_PASS=changeme
CORS_ALLOWED_ORIGINS=        # comma-separated allowlist; empty = deny all cross-origin

# ---- Postgres (ONE pool; db-sync's database, see §1.2) ----
PG_CONN_STR=host=postgres user=postgres password=postgres dbname=cexplorer port=5432

# ---- attest-sync (Kupo path only) ----
KUPO_URL=http://localhost:1442
BATCH_SIZE=1000000
FETCH_BATCH_SIZE=100000
ROLLBACK_MARGIN=2160

# ---- Atlas + deployed validators (JSON blob or file path) ----
ATLAS_CORE_CONFIG=
DEPLOYED_VALIDATORS_CONFIG=
# MAESTRO_API_KEY is read by Atlas from the core config, never from here. Never commit a real one.

# ---- query-api ----
PORT=8083
PALMARES_VIEW_REFRESH_SECONDS=300   # only consulted once a view becomes MATERIALIZED (§0.2)
PALMARES_MAX_DBSYNC_LAG_SECONDS=120 # /ready fails above this (spec F1 check 1)
```

### 1.9 `.gitignore`

The Belt System's file minus the PureScript block (`.psci_modules`, `.spago`, `bower_components`,
`output`, `.psci`), plus:
```
cabal.project.local
config/config_atlas.json
docs/generated/*
!docs/generated/.gitkeep
*.skey
*.prv
db/
.direnv
.env
```
Keep `*.prv` and `*.skey` — the Belt System's one real near-miss was a committed provider token, and
`config/config_atlas.json` being ignored is the mitigation. The new repo is **public from commit #1**
(spec §7), so verify with `git log -p | grep -iE 'maestro|api[_-]?key|bearer'` before the first push.

**(CORRECTED) do NOT gitignore the deployed-validators config.** An earlier draft of this section ignored
`config/config_validators.json`. That is wrong on two counts: (a) the Belt System *commits* its equivalent —
`git ls-files config/` returns `config_atlas.example.json` and `config_bjj_validators.json`, i.e. only the
Atlas provider config (which carries the Maestro token) is untracked; (b) the deployed-validators JSON is
script hashes + reference-script `TxOutRef`s, contains no secret, and is exactly what a third party needs to
verify our preprod deployment — spec S2/§7 reproducibility. Commit it as
`config/config_validators.preprod.json` and let `DEPLOYED_VALIDATORS_CONFIG` override it. (Keep the Belt
System's lesson in mind: its committed copy went stale against a redeploy — so `admin deploy-reference-scripts`
must rewrite this file in the same commit as the redeploy, and `/protocol-status` should surface the hashes.)

### 1.10 `docker-compose.yml` (dev/audit stack)

Shape copied from the Belt System's, with these changes: `postgres:17-alpine` (spec §8), **no `ipfs`
service** (§0.6), `chainsync` → `attest-sync`, plus the two new containers the governance path needs.

```yaml
services:
  postgres:
    image: postgres:17-alpine
    environment: [POSTGRES_DB=cexplorer, POSTGRES_USER=postgres, POSTGRES_PASSWORD=postgres]
    volumes: [pgdata:/var/lib/postgresql/data]
    ports: ["5432:5432"]
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U postgres -d cexplorer || exit 1"]
      interval: 30s
      timeout: 5s
      retries: 10
      start_period: 10s
    restart: unless-stopped

  # CORRECTED: node and db-sync get DIFFERENT config files and DIFFERENT mounts.
  cardano-node:
    image: ghcr.io/blinklabs-io/cardano-node:11.1.2        # multi-arch; matches apps-k8s-configs
    command: ["run", "--config", "/config/preprod-config.json", "--topology", "/config/preprod-topology.json",
              "--database-path", "/data/db", "--socket-path", "/ipc/node.socket"]
    volumes: [nodedb:/data, nodeipc:/ipc, ./config/node:/config:ro]
    restart: unless-stopped
    # ponytail: bootstrap the DB with Mithril out-of-band (the July 2026 procedure), not in compose.

  db-sync:
    # CORRECTED: ghcr.io/intersectmbo/cardano-db-sync:13.7.2.1 is amd64-ONLY (image config
    # "architecture":"amd64"; release notes "Platforms: Linux 64-bit"). The minirack is ARM64-only.
    # blinklabs publishes a {amd64,arm64} manifest list; its newest 13.7.x is 13.7.1.0. See §1.5.
    image: ghcr.io/blinklabs-io/cardano-db-sync:13.7.1.0
    environment:
      - NETWORK=                                   # intentionally EMPTY: we pass an explicit config
      - POSTGRES_HOST=postgres
      - POSTGRES_PORT=5432
      - POSTGRES_DB=cexplorer
      - POSTGRES_USER=postgres
      - POSTGRES_PASSWORD=postgres
      - DISABLE_LEDGER=N                           # must stay N or drep_distr empties (§0.1)
    # Mount the WHOLE config/ tree so NodeConfigFile "../node/preprod-config.json" resolves.
    command: ["--config", "/config/db-sync/preprod-db-sync-config.json", "--socket-path", "/ipc/node.socket"]
    # CORRECTED state-dir mount: with NETWORK empty the entrypoint runs
    #   --state-dir ${services.cardano-db-sync.stateDir}, which defaults to /var/lib/cexplorer
    #   (nix/nixos/cardano-db-sync-service.nix:148). /var/lib/cdbsync was wrong and would have put the
    #   ledger state — the expensive part — on the container's ephemeral layer.
    volumes:
      - dbsyncdata:/var/lib/cexplorer
      - nodeipc:/ipc
      - ./config:/config:ro
    depends_on:
      postgres: {condition: service_healthy}
    restart: unless-stopped

  attest-sync:
    image: <owner>/palmares-attest-sync:dev
    environment: [KUPO_URL=${KUPO_URL}, PG_CONN_STR=..., ATLAS_CORE_CONFIG=..., DEPLOYED_VALIDATORS_CONFIG=...]
    ports: ["8084:8084"]
    depends_on: {postgres: {condition: service_healthy}}
    healthcheck: {test: ["CMD-SHELL", "curl -fsS http://localhost:8084/ready || exit 1"], interval: 10s, retries: 12000}
    restart: unless-stopped

  query-api:
    image: <owner>/palmares-query-api:dev
    environment: [PORT=8083, PG_CONN_STR=..., CORS_ALLOWED_ORIGINS=${CORS_ALLOWED_ORIGINS}]
    ports: ["8083:8083"]
    depends_on: {postgres: {condition: service_healthy}, attest-sync: {condition: service_healthy}}
    healthcheck: {test: ["CMD-SHELL", "curl -fsS http://localhost:8083/health || exit 1"], interval: 30s, retries: 10}
    restart: unless-stopped

  interaction-api:
    image: <owner>/palmares-interaction-api:dev
    environment: [PORT=8082, BASIC_USER=${BASIC_USER}, BASIC_PASS=${BASIC_PASS},
                  CORS_ALLOWED_ORIGINS=${CORS_ALLOWED_ORIGINS}, ATLAS_CORE_CONFIG=..., DEPLOYED_VALIDATORS_CONFIG=...]
    ports: ["8082:8082"]
    depends_on: {attest-sync: {condition: service_healthy}}
    healthcheck: {test: ["CMD-SHELL", "curl -fsS http://localhost:8082/health || exit 1"], interval: 30s, retries: 10}
    restart: unless-stopped

volumes: {pgdata: , nodedb: , nodeipc: , dbsyncdata: }
```

### 1.11 `scripts/reconcile_koios.sh` — verified endpoints

Probed live against `https://preprod.koios.rest/api/v1/` on 2026-10-19:

| Endpoint | HTTP | Note |
|---|---|---|
| `proposal_list` | 200 | gov-action parity (F1 check 6) |
| `vote_list` | 200 | vote parity (F1 check 6) |
| `drep_list` | 200 | DRep enumeration |
| `drep_epoch_summary` | 200 | epoch aggregates (F1 check 5) |
| `drep_voting_power_history` | 504 | exists but times out on an unfiltered GET — **always pass `_drep_id`**, never scan |
| `drep_info` | 404 on GET | it is a **POST** endpoint (body carries the id list) |

So the reconciliation script is per-DRep and per-action, never a full table scan. Base URL for mainnet at
F2: `https://api.koios.rest/api/v1/`.

---

## 2. Extraction manifest

Verdict vocabulary: **as-is** (byte-identical bar the module header/namespace) · **rename** (as-is + new
module name) · **strip** (delete BJJ-specific members, keep the rest untouched) · **rewrite** (keep the
file's structure and the hard-won comments; replace the payload) · **skip** (do not take).

Line counts and "BJJ hits" (case-insensitive matches of `bjj|belt|rank|promotion|practitioner|membership|achievement|dojo|lineage`)
are measured, not estimated. All source paths are relative to
`/Users/mg/Projects/CardanoProjects/Decentralized-Belt-System/`.

### 2.1 `webapi-lib` — the spec says "integral"; it is 5 of 6

| Source | Lines / BJJ | Destination | Verdict |
|---|---|---|---|
| `src/lib/webapi-lib/WebAPI/Auth.hs` | 62 / 0 | `src/lib/webapi-lib/Palmares/WebAPI/Auth.hs` | **as-is** — constant-time `constEq`, never decodes attacker bytes, `die`s with no creds. Still needed by interaction-api even though query-api is public. |
| `WebAPI/CORS.hs` | 68 / 0 | `Palmares/WebAPI/CORS.hs` | **as-is** — fail-closed allowlist |
| `WebAPI/Errors.hs` | 35 / 0 | `Palmares/WebAPI/Errors.hs` | **as-is** — `mkServantErr` maps unknown statuses to 500, not 400 |
| `WebAPI/ServiceProbe.hs` | 63 / 0 | `Palmares/WebAPI/ServiceProbe.hs` | **as-is** |
| `WebAPI/Utils.hs` | 47 / 2 | `Palmares/WebAPI/Utils.hs` | **strip** — keep `getPortFromEnvOrDefault` + `escapeLikePattern` verbatim; in `addSharedSwaggerDescriptions` replace the `BJJBelt`/`ProfileType` entries with `GYPubKeyHash`, `DRepId`, `GovActionId`, `AttestationType`. |
| `WebAPI/ImageType.hs` | 24 / 0 | — | **skip** — only consumer was the IPFS upload path (§0.6) |

### 2.2 `onchain-lib`

| Source (`src/lib/onchain-lib/`) | Lines / BJJ | Destination | Verdict + one-line why |
|---|---|---|---|
| `Onchain/CIP68.hs` | 178 / 0 | `Palmares/Onchain/CIP68.hs` | **as-is** — CIP-68 datum + CIP-67 ref/user token derivation. The single most reusable on-chain file. |
| `Onchain/Utils.hs` | 141 / 0 | `Palmares/Onchain/Utils.hs` | **as-is** — `mkUntypedLambda`, the index-based output checks, `hasCurrencySymbol`, `nameFromTxOutRef` |
| `Onchain/Validators/OracleNFTPolicy.hs` | 60 / 0 | `Palmares/Onchain/Validators/OracleNFTPolicy.hs` | **as-is** — one-shot NFT from a seed TxOutRef |
| `Onchain/Validators/OracleValidator.hs` | 92 / 1 | `Palmares/Onchain/Validators/OracleValidator.hs` | **as-is** (lambda unchanged!) — fix #2 lands entirely in `Protocol/Types.hs`; see §4.2 |
| `Onchain/Validators/AchievementsValidator.hs` | 112 / 34 | `Palmares/Onchain/Validators/AttestationsValidator.hs` | **rename + strip** — the two-phase `Accept` + permissionless `Cleanup` pattern IS the attestation protocol. Highest-value on-chain extraction. |
| `Onchain/Validators/ProfilesValidator.hs` | 203 / 44 | `Palmares/Onchain/Validators/ProfilesValidator.hs` | **strip** — keep the CIP-68 metadata-update + `Cleanup` redeemers; drop the `currentRank` field and its checks |
| `Onchain/Protocol/Types.hs` | 257 / 77 | `Palmares/Onchain/Protocol/Types.hs` | **rewrite** — keep `ProtocolParams`/`FeeConfig`/`OracleParams`/`OracleAdminAction`/`applyOracleAdminAction` and the `makeIsDataSchemaIndexed` stable-index discipline; replace every BJJ payload type. Fixes #1 + #2 land here. |
| `Onchain/Protocol/Lookup.hs` | 131 / 20 | `Palmares/Onchain/Protocol/Lookup.hs` | **strip** — keep `readOracleParams`, `checkFee`, `unsafeGetProfile`; drop `unsafeGetRank`, `unsafeGetListNodeDatumAndValue`, `unsafeGetMembershipInterval`. Fix #1 adds `feeFor` here. |
| `Onchain/Protocol/Id.hs` | 62 / 34 | `Palmares/Onchain/Protocol/Id.hs` | **rewrite** — keep the `blake2b_224` 28-byte-name derivation pattern; new `deriveAttestationId`, `deriveDisputeId` |
| `Onchain/Protocol.hs` | 17 / 0 | `Palmares/Onchain/Protocol.hs` | **rewrite** — pure re-export barrel; the list changes |
| `Onchain/Blueprint.hs` | 233 / 61 | `Palmares/Onchain/Blueprint.hs` | **strip** — the CIP-57 preamble + `writeBlueprint` assembly is reusable; the validator entries change |
| `Onchain/Validators/MintingPolicy.hs` | 454 / 165 | `Palmares/Onchain/Validators/MintingPolicy.hs` | **rewrite** — keep the structure exactly: global `opPaused` gate, `checkFee` per redeemer, exact-mint `mintValueMinted == expected`, seed-TxOutRef one-shot names, `hasCurrencySymbol`. New redeemers: `ClaimProfile`, `IssueAttestation`, `OpenDispute`. Fixes #1 + #4 land here. |
| `Onchain/Validators/DisputesValidator.hs` | — | new | **new file** — modelled on `AttestationsValidator`; `Resolve` (admin signs) + `Withdraw` (deposit back) + `Cleanup` |
| `Onchain/LinkedList.hs` | 106 / 0 | — | **skip** — §0.5; its only consumer was the membership list. 106 lines sitting in a frozen repo if ordering is ever needed. |
| `Onchain/Protocol/Core.hs` | 360 / 183 | — | **skip** — BJJ eligibility engine + list ops; nothing governance-shaped survives |
| `Onchain/BJJ.hs` | 245 / 78 | — | **skip** — belt hierarchy |
| `Onchain/Validators/RanksValidator.hs` | 133 / 41 | — | **skip** — belt-chain lineage, no analogue |
| `Onchain/Validators/MembershipsValidator.hs` | 301 / 90 | — | **skip** — §0.5 |

### 2.3 `chainsync-lib` — 100% as-is

| Source | Lines / BJJ | Destination | Verdict |
|---|---|---|---|
| `src/lib/chainsync-lib/KupoClient.hs` | 335 / 0 | `src/lib/chainsync-lib/Palmares/ChainSync/KupoClient.hs` | **as-is** |
| `src/lib/chainsync-lib/KupoAtlas.hs` | 123 / 0 | `Palmares/ChainSync/KupoAtlas.hs` | **as-is** — incl. the separatorless-asset-key fallback (`toGYAssetClass`), a real Kupo quirk |

### 2.4 `offchain-lib`

| Source (`src/lib/offchain-lib/`) | Lines / BJJ | Destination | Verdict + why |
|---|---|---|---|
| `TxBuilding/Skeletons.hs` | 175 / 0 | `Palmares/TxBuilding/Skeletons.hs` | **as-is** — the generic Atlas skeleton vocabulary. Keep `safeEraTime = 25920` and `txIsValidForSafeEra`; fix #4 builds on them. |
| `Utils.hs` | 83 / 0 | `Palmares/Utils.hs` | **as-is** |
| `TxBuilding/Transactions.hs` | 199 / 9 | `Palmares/TxBuilding/Transactions.hs` | **as-is** + rename `runBJJActionWithPK` → `runActionWithPK` |
| `TxBuilding/Context.hs` | 150 / 12 | `Palmares/TxBuilding/Context.hs` | **strip** — `DeployedScriptsContext` fields become minting policy / profiles / attestations / disputes / oracle + `oracleNFTAssetClass`. `ProviderCtx`, `runQuery`, `runTx`, `runTx'`, `collateralToRunParam` untouched. |
| `TxBuilding/Utils.hs` | 171 / 24 | `Palmares/TxBuilding/Utils.hs` | **strip** — keep `extractNFTAssetClass`, `getInlineDatumAndValue(OrThrow)`, `pPOSIXTimeFromGYSlot`, `gySlotFromPOSIXTime`, `txOutRefToV3Plutus`, `tnFromGYAssetClass`; drop the rank/membership datum extractors. Fix #3's guard is added here. |
| `TxBuilding/Exceptions.hs` | 162 / 73 | `Palmares/TxBuilding/Exceptions.hs` | **strip** — `txBuildingExceptionToHttpStatus` and its insufficient-funds→400 mapping (Belt System commit `12cee09`) carry over verbatim; constructors change |
| `TxBuilding/Validators.hs` | 210 / 48 | `Palmares/TxBuilding/Validators.hs` | **strip** — compiled-script accessors + `*Size` reporting + `compileMintingPolicy`/`mkProtocolParams` |
| `TxBuilding/Interactions.hs` | 132 / 18 | `Palmares/TxBuilding/Interactions.hs` | **strip** — the `Interaction → interactionToTxSkeleton` dispatch shape is the pipeline contract |
| `Storage.hs` | 511 / 129 | `Palmares/Storage.hs` | **rewrite the persist block, keep the machinery** — keep verbatim: `PersistField Integer` (bigint, the F-02 fix), `PersistField GYTime` (timestamptz + legacy-text fallback), `upsertByUnique`, `ChainCursor`, `ChainSyncConfig`, `OnchainMatchEvent` + `putKupoMatch`, `readSchemaProbe`, `wipeChainSyncTablesRaw`, `chainSyncTableNames`, `replayOrder`, `rollbackTo`. Replace 6 projection entities with 3. Drop `resolveOrganizationForInterval` + `backfillIntervalOrganizationsForHistory` (they existed only for the history/interval split). Reset `currentSchemaVersion = 1`. Fix #5 lands here (as a *removal*). |
| `Ingestion.hs` | 77 / 28 | `Palmares/Ingestion.hs` | **rewrite** — same address-dispatch shape, 3 branches. **Fix #3 lands here.** |
| `TxBuilding/Lookups.hs` | 498 / 163 | `Palmares/TxBuilding/Lookups.hs` | **rewrite** — keep `getUTxOWithNFT`, `getUTxOWithTokenAtAddresses`, `queryOracleParams`, `getAllParsedDatumsAtValidator`. **Fix #3's second site is `getAllParsedDatumsAtValidator`.** |
| `TxBuilding/Operations.hs` | 1117 / 278 | `Palmares/TxBuilding/Operations.hs` | **rewrite** — take verbatim: `getFeeSkeleton`, `getOracleFeeAndValiditySkeleton`, `applyAdminAction`, `adminActionToOracleAdminAction` (≈ lines 55–110 and 1060–1080). Rewrite the 6 BJJ ops into 5. **Fixes #1 + #4 land here.** |
| `TxBuilding/Conversions.hs` | 220 / 85 | `Palmares/TxBuilding/Conversions.hs` | **rewrite** — the onchain↔domain bridge; shape survives, types do not |
| `Constants.hs` | 38 / 8 | `Palmares/Constants.hs` | **strip** — paths rename; keep `defaultRollbackMargin = 2160` and `appVersion` |
| `DomainTypes/Core/Types.hs` | 365 / 182 | `Palmares/DomainTypes/Core/Types.hs` | **rewrite** — keep the `deriving-aeson` StripPrefix+CamelToSnake + `ToSchema` conventions |
| `DomainTypes/Core/Actions.hs` | 160 / 20 | `Palmares/DomainTypes/Core/Actions.hs` | **rewrite** — keep `AdminActionType` and the action-sum shape; add `SetAdminAction` (fix #2) |
| `DomainTypes/Transfer/Types.hs` | 216 / 82 | `Palmares/DomainTypes/Transfer/Types.hs` | **rewrite** |
| `DomainTypes/Transfer/Filters.hs` | 163 / 82 | `Palmares/DomainTypes/Transfer/Filters.hs` | **rewrite** — keep the `*FromParams` convention |
| `DomainTypes/Transfer/OrderBy.hs` | 334 / 103 | `Palmares/DomainTypes/Transfer/OrderBy.hs` | **rewrite** — keep `SortOrder` + the enum/`FromHttpApiData`/`ToParamSchema` triple |
| `DomainTypes/Transfer/QueryResponses.hs` | 639 / 285 | `Palmares/DomainTypes/Transfer/QueryResponses.hs` | **rewrite** — all new DTOs |
| `TxBuilding/SafeOnchainLogic.hs` | 119 / 78 | — | **skip** — it wraps `Onchain.BJJ`/`Protocol.Core`, neither of which we take. The *pattern* ("never call a `traceError`-ing on-chain function off-chain; wrap it with a dedicated exception") carries forward in `CLAUDE.md`, as doctrine, not code. |
| `DomainTypes/Core/BJJ.hs` | 47 / 13 | — | **skip** |
| `DomainTypes/Rules/Promotion.hs` | 199 / 96 | — | **skip** |

### 2.5 Executables

| Source | Lines / BJJ | Destination | Verdict + why |
|---|---|---|---|
| `src/exe/query-api/QueryAppMonad.hs` | 112 / 0 | `src/exe/query-api/QueryAppMonad.hs` | **strip** (was mislabelled as-is: dropping `liveProjection` is an edit, and it also `import`s nothing skipped) — both error-handling shapes (`runWithQueryErrorHandling` for bare `TxBuildingException`, `…Wrapped` for the `GYApplicationException`-wrapped case) are hard-won; their comments explain exactly why one cannot replace the other. Drop only the `liveProjection` field (§0.4). |
| `src/exe/query-api/Query/Common.hs` | 51 / 4 | `Query/Common.hs` | **strip** — keep `normalizeLimitOffset` (default page 100, clamp [1,500] — the F-17 pagination fix), `normalizeOrder`, `applyLimits`, `applyFilterOrderLimit`; drop `beltIsMasterCapable` |
| `src/exe/query-api/Main.hs` | 60 / 4 | `Main.hs` | **strip** — add the `sql/*.sql` bootstrap call |
| `src/exe/query-api/RestAPI.hs` | 738 / 182 | `RestAPI.hs` | **rewrite** — keep the `swagger :<|> probe :<|> public` composition and the `Summary`/`Description` discipline. **No `BasicAuth` on the read routes** (spec §4.2). |
| `src/exe/query-api/Query/Projected.hs` | 1527 / 567 | `Query/Projected.hs` | **rewrite** — attestation/profile/dispute queries (esqueleto over `palmares_attest`) |
| `src/exe/query-api/Query/Aggregates.hs` | 395 / 156 | `Query/Aggregates.hs` | **rewrite** — network dashboard. Fix #5's read-side conversion lands here. |
| `src/exe/query-api/Query/Pages.hs` | 434 / 158 | `Query/Pages.hs` | **rewrite** — the one-screen-one-request composite pattern is the point; keep it |
| `src/exe/query-api/Query/ServiceStatus.hs` | 82 / 7 | `Query/ServiceStatus.hs` | **strip** + extend — needs a *second* lag source: `MAX(block.time)` in db-sync vs wall clock (F1 check 1) alongside the Kupo cursor lag |
| — | — | `Query/Gov.hs` | **new** — `rawSql` over schema `palmares`. The governance read path. |
| — | — | `Query/Views.hs` | **new**, ~40 lines — `file-embed` the `sql/*.sql` files, apply them in lexical order at startup inside one transaction |
| `src/exe/query-api/RestAPI/Common.hs` | 10 / 0 | — | **skip** — §0.4 |
| `src/exe/query-api/Query/Live.hs` | 657 / 286 | — | **skip** — §0.4 |
| `src/exe/query-api/Query/Rules.hs` | 90 / 39 | — | **skip** — §0.7 |
| `src/exe/query-api/Query/ExplorerMonthly.hs` | 49 / 15 | — | **skip** — BJJ-specific monthly stat |
| `src/exe/interaction-api/InteractionAppMonad.hs` | 164 / 0 | `src/exe/interaction-api/InteractionAppMonad.hs` | **strip** — CORRECTED from as-is: line 31 is `import IPFS (IPFSConfig)` and the `IPFSConfig` field must go with §0.6 |
| `src/exe/interaction-api/Main.hs` | 50 / 4 | `Main.hs` | **strip** |
| `src/exe/interaction-api/RestAPI.hs` | 314 / 33 | `RestAPI.hs` | **strip** — drop the multipart image route |
| `src/exe/interaction-api/ServiceHandlers.hs` | 161 / 31 | `ServiceHandlers.hs` | **strip** — drop the image handler |
| `src/exe/interaction-api/ServiceRequests.hs` | 266 / 48 | `ServiceRequests.hs` | **rewrite** — keep the used-addresses + change-address + optional-collateral request shape exactly (it is the CIP-30 contract the FE depends on) |
| `src/exe/interaction-api/IPFS.hs` | 80 / 0 | — | **skip** — §0.6 |
| `src/exe/chain-sync/Main.hs` | 145 / 0 | `src/exe/attest-sync/Main.hs` | **as-is** — the startup order (schema probe → wipe → migrate → cursor → fork probe → loop) is correct and subtle |
| `src/exe/chain-sync/ChainSyncLogic.hs` | 122 / 0 | `ChainSyncLogic.hs` | **as-is** — `evaluateChainSyncState` 4-state machine |
| `src/exe/chain-sync/ChainSyncServer.hs` | 17 / 0 | `ChainSyncServer.hs` | **as-is** |
| `src/exe/chain-sync/ChainsyncAPI.hs` | 69 / 0 | `ChainsyncAPI.hs` | **as-is** |
| `src/exe/admin/Main.hs` | 777 / 182 | `src/exe/admin/Main.hs` | **rewrite** — keep the `Either ProviderCtx TxBuildingContext` context split, the `Command` sum + `hsubparser` + `executeCommand` shape, and the shared parsers (`assetClassParser` with its JSON `maybeReader`, `posixTimeParser`, `outputIdParser`). Keep `deploy-reference-scripts` + `write-blueprint` nearly verbatim. New commands: `rotate-admin` (fix #2), `resolve-dispute`. |

### 2.6 `mcp-server-lib` — skip in F0/F1, take at F3

All **18** modules (corrected from "14"; the table below lists 18, and `find src/lib/mcp-server-lib -name
'*.hs' | wc -l` = 18, 2,831 lines): **skip for now** (spec §9 F1 non-goals). When F3 starts, the split is
already clear from the BJJ-hit counts:

| F3 verdict | Modules |
|---|---|
| **as-is** (plumbing, 0–1 BJJ hits) | `MCPServer/App.hs` (73), `Readiness.hs` (118), `Tools/Common.hs` (167), `Api/Interaction.hs` (35), `Tools/Transactions.hs` (158) |
| **strip** | `Server.hs` (154/8), `Schema.hs` (181/26), `Resources.hs` (211/36), `Clients.hs` (228/51), `Api/Query.hs` (207/50), `Orphans.hs` (140/49), `Tools/Activity.hs` (88/13), `Tools/Search.hs` (67/3), `Tools/Docs.hs` (40/5), `Tools/Profiles.hs` (170/29) |
| **rewrite** | `Tools/Promotions.hs` (355/124), `Tools/Memberships.hs` (244/93), `Tools/Achievements.hs` (195/52) → the governance tool set |

### 2.7 Test harness

| Source (`src/test/`) | Lines / BJJ | Destination | Verdict + why |
|---|---|---|---|
| `UnitTests/StorageRoundtrip.hs` | 73 / 0 | `src/test/UnitTests/StorageRoundtrip.hs` | **as-is** — it only touches `OnchainMatchEvent`/`putKupoMatch`/`rollbackTo`/`runMigrations`, all of which survive. In-memory SQLite, `runSqlite ":memory:"`, zero fixtures. The cheapest high-value test in the repo. |
| `Test/Helpers.hs` | 22 / 0 | `Test/Helpers.hs` | **as-is** |
| `UnitTests/ApiHardening.hs` | 100 / 0 | `UnitTests/ApiHardening.hs` | **strip** — delete the `imageTests` group (4 cases + 1 import + 1 list entry, §0.6); `authTests`/`corsTests`/`escapeTests`/`errorTests` as-is |
| `TestRuns.hs` | 480 / 74 | `TestRuns.hs` | **rewrite** — keep `deployReferenceScriptRun`, `mintTestOracleNFT`, `sendDustToValidator` (fix #3's test needs it!), the `asUser … buildTxBody >>= signAndSubmitConfirmed` runner shape, and the `malicious*` negative-runner convention. `deployBJJValidators` → `deployValidators`. |
| `UnitTests/Achievement.hs` | 545 / 196 | `UnitTests/Attestation.hs` | **rename + rewrite** — the two-phase pending→accept coverage, incl. the double-accept negative that exposed a real off-chain crash (memory: Stream F). Highest-value test file. Fix #4's tests go here. |
| `UnitTests/Oracle.hs` | 286 / 27 | `UnitTests/Oracle.hs` | **strip + extend** — pause/unpause/fees/minUTxO + the malicious-oracle-NFT-mint negatives carry over. Fixes #1 + #2 tests go here. |
| `UnitTests/ChainSyncReplay.hs` | 226 / 68 | `UnitTests/ChainSyncReplay.hs` | **rewrite** — keep the replay-order + idempotence + rollback shape. Fixes #3 + #5 tests go here. |
| `UnitTests/Cleanup.hs` | 175 / 38 | `UnitTests/Cleanup.hs` | **strip** — permissionless `Cleanup` coverage survives per-validator |
| `UnitTests/Conversions.hs` | 33 / 1 | `UnitTests/Conversions.hs` | **strip** |
| `UnitTests.hs` | 32 / 7 | `UnitTests.hs` | **strip** — test-tree assembly |
| `Main.hs` | 12 / 2 | `Main.hs` | **strip** |
| `Test/Fixtures.hs` | 136 / 26 | `Test/Fixtures.hs` | **rewrite** |
| `BJJPropertyTests.hs` | 571 / 210 | `PropertyTests.hs` | **skip the file, port the pattern** — ~60 lines: one hedgehog generator over the new datum type + one `property` asserting off-chain predicate ≡ on-chain predicate (the "mirror-and-pin" doctrine). The other 500 lines are belt-progression algebra. |
| `UnitTests/Membership.hs` | 1307 / 416 | — | **skip** |
| `UnitTests/Promotion.hs` | 572 / 166 | — | **skip** |
| `UnitTests/MCPTools.hs` | 356 / 49 | — | **skip** (F3) |
| `src/test-mcp-integration/*` | 364 / 59 | — | **skip** (F3) |

### 2.8 Extraction totals

**(CORRECTED 2026-10-19 — recomputed by `wc -l` over the exact rows of §2.1–§2.7; the earlier numbers did
not reconcile with the manifest.)**

| Verdict | Files | Lines |
|---|---|---|
| **as-is** | 19 | 2,062 |
| **rename / strip** | 24 | 2,935 (edit, don't retype) |
| **rewrite** (structure kept, payload new) | 25 | 10,614 source → ≈ 4,500–5,500 new |
| **skip** (F1 scope) | 20 | 5,590 never touched |
| **skip** (`mcp-server-lib`, deferred to F3, §2.6) | 18 | 2,831 |

(as-is/strip split after the two reclassifications in §2.5: `QueryAppMonad.hs` 112 and
`InteractionAppMonad.hs` 164 move from as-is to strip.)

Reading: about 5,000 lines arrive needing little or no thought; about **10,600** lines are *templates* whose
comments and shape are the real asset; 5,590 lines of BJJ domain stay behind in F1 (8,421 counting the F3
MCP modules). The F0 estimate should be sized on "**25** rewrite files", not on total line count.

---

## 3. Extraction mechanism

### 3.1 Decision

**(decision) Plain copy, in one commit, with a scripted rename. Not `git subtree`. Not a shared package.**

| Option | Verdict |
|---|---|
| `git subtree` | **No.** Subtree's only advantage is merging changes both ways. The Belt System freezes (spec §8), so there is nothing to pull; and 48 of the 70 extracted files get renamed or rewritten in the very next commit, which destroys the shared history subtree exists to preserve. You would pay the complexity and get nothing. |
| shared cabal package / submodule / `source-repository-package` → Belt System | **No.** It makes an Apache-2.0 public repo structurally depend on a frozen MIT repo at a git commit — exactly the coupling the extraction exists to remove — and `onchain-lib` has to change anyway (fixes #1, #2, #4). |
| **plain copy** | **Yes.** One `git log` entry that says what came from where, a `NOTICE` that says it legally, and from then on a repo with no upstream. |

### 3.2 Exact commands

```bash
set -euo pipefail
SRC=/Users/mg/Projects/CardanoProjects/Decentralized-Belt-System
DST=/Users/mg/Projects/CardanoProjects/palmares
BELT_SHA=$(git -C "$SRC" rev-parse HEAD)     # record it; it goes in the commit message

mkdir -p "$DST" && cd "$DST" && git init -b main

# 1. Scaffolding first, so the repo compiles nothing but is legally complete from commit #1.
curl -sL https://www.apache.org/licenses/LICENSE-2.0.txt -o LICENSE
cp "$SRC/LICENSE" licenses/LICENSE.decentralized-belt-system.MIT   # mkdir -p licenses first
# write NOTICE (§3.3), README.md, .gitignore, cabal.project (§1.4), palmares.cabal (§1.3)
for f in .envrc .dockerignore .justfile hie.yaml libblst.pc Dockerfile.base Dockerfile.runtime-base; do
  cp "$SRC/$f" .
done
sed -i '' 's/BJJ_ENV_LOADED/PALMARES_ENV_LOADED/; s/Decentralized Belt System environment/Palmares environment/' .envrc
sed -i '' 's#/Decentralized-Belt-System#/palmares#g' Dockerfile.base Dockerfile.runtime-base
git add -A && git commit -m "chore: bootstrap Apache-2.0 repo with pinned toolchain"

# 2. Copy only the files the manifest keeps (§2). Explicit list; NEVER `cp -r src`.
#    Put the kept paths in a file so the manifest and the command cannot drift.
mkdir -p src/lib/{onchain-lib,offchain-lib,chainsync-lib,webapi-lib} src/exe/{admin,query-api,interaction-api,attest-sync} src/test
while read -r rel; do
  mkdir -p "$(dirname "$rel")" && cp "$SRC/$rel" "$rel"
done < extraction-keep.txt            # generated from §2: every row whose verdict is not `skip`

# 3. Namespace: move library modules under Palmares/ and rewrite imports mechanically.
for lib in onchain-lib offchain-lib chainsync-lib webapi-lib; do
  mkdir -p "src/lib/$lib/Palmares"
  (cd "src/lib/$lib" && for e in *; do [ "$e" = Palmares ] || mv "$e" Palmares/; done)
done
mv src/lib/chainsync-lib/Palmares/Kupo*.hs src/lib/chainsync-lib/Palmares/ChainSync/   # mkdir -p first
grep -rl . --include='*.hs' src | xargs sed -i '' -E \
  -e 's/^module (Onchain|WebAPI|TxBuilding|DomainTypes)\./module Palmares.\1./' \
  -e 's/^(import +(qualified +)?)(Onchain|WebAPI|TxBuilding|DomainTypes)\./\1Palmares.\3./' \
  -e 's/^module (Storage|Ingestion|Constants|Utils)$/module Palmares.\1/' \
  -e 's/^(import +(qualified +)?)(Storage|Ingestion|Constants|Utils)\b/\1Palmares.\3/' \
  -e 's/\b(KupoClient|KupoAtlas)\b/Palmares.ChainSync.\1/g'
git add -A
git commit -m "feat: extract reusable libraries from Decentralized-Belt-System

Source: Decentralized-Belt-System @ ${BELT_SHA} (MIT, Copyright (c) 2025 Marius Georgescu).
Relicensed to Apache-2.0 by the copyright holder; the MIT notice is retained in
licenses/LICENSE.decentralized-belt-system.MIT per NOTICE.
File-by-file provenance and verdicts: docs/superpowers/design/f1-library-extraction.md §2."

# 4. Only now do the strips/rewrites, each as its own commit, so `git log` shows what changed and why.
```

Three rules for step 3/4 that make the difference between a clean extraction and a swamp:
1. **One commit per manifest verdict class**, in order: as-is → rename → strip → rewrite. A reviewer can
   then `git show` the as-is commit and verify it is byte-identical to the Belt System.
2. **Never `cp -r src`.** The explicit `extraction-keep.txt` is what makes the `skip` column real.
3. **`cabal build all` must pass after the as-is + rename + strip commits** — i.e. the *strip* commit is
   part of the green gate, not after it. **(CORRECTED: the original rule said "after step 3, as-is +
   rename only", which is impossible.)** Measured: these kept files `import` modules the manifest skips, so
   a verbatim copy cannot compile until the strips land:

   | Kept file (verdict) | Imports (skipped) |
   |---|---|
   | `src/exe/interaction-api/InteractionAppMonad.hs` (strip) | `IPFS (IPFSConfig)` — L31 |
   | `src/exe/interaction-api/Main.hs` (strip) | `IPFS (initIPFSConfig)` — L12 |
   | `src/exe/interaction-api/ServiceHandlers.hs` (strip) | `IPFS`, `WebAPI.ImageType` — L19, L27 |
   | `src/test/UnitTests/ApiHardening.hs` (strip) | `WebAPI.ImageType` — L21 |
   | `src/exe/query-api/Query/Common.hs` (strip) | `DomainTypes.Core.BJJ` — L6 |
   | `src/lib/onchain-lib/Onchain/Protocol.hs` (rewrite) | `Onchain.Protocol.Core` — L14 |
   | `src/lib/onchain-lib/Onchain/Protocol/Types.hs` (rewrite) | `Onchain.LinkedList` — L8 |
   | `src/lib/onchain-lib/Onchain/Validators/MintingPolicy.hs` (rewrite) | `Onchain.BJJ` — L25 |
   | `src/lib/offchain-lib/TxBuilding/{Lookups,Conversions,Operations}.hs` (rewrite) | `TxBuilding.SafeOnchainLogic` |
   | `src/exe/query-api/{RestAPI,Query/Pages,Query/Aggregates}.hs` (rewrite) | `Query.Live`, `Query.Rules`, `Query.ExplorerMonthly`, `RestAPI.Common` |

   Practical consequence for the plan: the green gate lands **after** the strip commit, and the
   *rewrite*-class files must at minimum have their skipped imports and the declarations that use them
   removed in that same strip commit (a "make it compile with the payload stubbed out" pass) before the real
   rewrites begin. Otherwise there is no checkpoint between "copied" and "finished".

### 3.3 Attribution — exact text

Both repos are the same copyright holder, so there is no permission problem: the owner can relicense his
own code. What is still *required* is retaining MIT's notice for the code that was published under MIT
(and it is the right thing to do regardless of whose code it is).

`LICENSE` = the verbatim Apache-2.0 text from <https://www.apache.org/licenses/LICENSE-2.0.txt>.

`NOTICE` (Apache-2.0 §4(d) — downstream redistributors must carry this forward):

```
Palmares
Copyright 2026 Marius Georgescu

This product includes software developed as part of the Decentralized Belt System
(https://github.com/<owner>/Decentralized-Belt-System), Copyright (c) 2025 Marius Georgescu,
originally released under the MIT License. Portions of src/lib/ and src/exe/ are derived
from that work and are redistributed here under the Apache License, Version 2.0 by the
copyright holder. The original MIT license text is retained in
licenses/LICENSE.decentralized-belt-system.MIT.

This product includes third-party software under its own licenses; see the dependency
pins in cabal.project and the generated license report.
```

`licenses/LICENSE.decentralized-belt-system.MIT` = the Belt System's `LICENSE` file, byte-for-byte.

Per-file headers: **(decision) do not add them.** `NOTICE` + the extraction commit + §2 of this document
give better provenance than 70 copied header blocks that will rot.

Two things to check before the first public push (spec §7, "fără secrete în istorie"):
```bash
git log -p --all | grep -inE 'maestro|api[_-]?key|bearer |BASIC_PASS=[^$]' || echo CLEAN
test ! -e config/config_atlas.json && test ! -e operation.prv && echo "no secret files staged"
```

---

## 4. Where the five inherited fixes land

### 4.1 Fix #1 — fee-selector mismatch

**Where it is, exactly.** `src/lib/offchain-lib/TxBuilding/Operations.hs:611`, in `addMembershipIntervalTX`:

```haskell
(oracleRefSkeleton, oracleParams, feeSkeleton, isInvalidBeforeNow) <-
  getOracleFeeAndValiditySkeleton OnchainTypes.fcMembershipHistoryFee   -- WRONG selector
```
while `src/lib/onchain-lib/Onchain/Validators/MintingPolicy.hs:162`, for the matching redeemer, checks:
```haskell
NewMembershipInterval … -> checkFee oracle fcMembershipIntervalFee txInfoOutputs
```
Impact: the moment an admin sets `fcMembershipIntervalFee /= fcMembershipHistoryFee`, every
`addMembershipInterval` transaction is built with the wrong fee output and dies on-chain with `K2`. Not a
money loss — a **total outage of one action, triggerable by a legitimate admin config change**. It
survived 155 tests because every test sets all fees equal (or to zero).

**Root cause, not symptom.** `checkFee :: OracleParams -> (FeeConfig -> Integer) -> [TxOut] -> Bool` takes
the selector as a *free function argument*, passed independently by on-chain and off-chain code. Two lists
that must agree, with nothing checking that they do. Fixing the one call site leaves the defect class
alive for every future redeemer.

**New design (one guard, both callers).** Delete the selector argument. A single `INLINEABLE` function in
`Palmares.Onchain.Protocol.Lookup`, keyed on the redeemer, is the only place a fee field is ever named:

```haskell
-- Palmares.Onchain.Protocol.Types
data MintAction = MintClaimProfile | MintIssueAttestation | MintOpenDispute
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
  deriving anyclass (HasBlueprintDefinition)
makeIsDataSchemaIndexed ''MintAction
  [('MintClaimProfile, 0), ('MintIssueAttestation, 1), ('MintOpenDispute, 2)]

data FeeConfig = FeeConfig
  { fcFeeAddress      :: Address
  , fcProfileClaimFee :: Integer
  , fcAttestationFee  :: Integer
  , fcDisputeDeposit  :: Integer
  }

-- Palmares.Onchain.Protocol.Lookup — the ONLY place a FeeConfig field is selected, on or off chain.
{-# INLINEABLE feeFor #-}
feeFor :: MintAction -> FeeConfig -> Integer
feeFor MintClaimProfile     = fcProfileClaimFee
feeFor MintIssueAttestation = fcAttestationFee
feeFor MintOpenDispute      = fcDisputeDeposit

{-# INLINEABLE checkFee #-}
checkFee :: OracleParams -> MintAction -> [TxOut] -> Bool   -- selector arg GONE
```
Off-chain, `getOracleFeeAndValiditySkeleton` takes a `MintAction`, not a selector — and it already has one
in hand, because it is building that exact redeemer. The mismatch becomes **unrepresentable**.

**The test.** `src/test/UnitTests/Oracle.hs`:
`"each mint action pays its own fee when the three fees differ"` — a CLB test that sets
`FeeConfig{ fcProfileClaimFee = 1_000_000, fcAttestationFee = 3_000_000, fcDisputeDeposit = 7_000_000 }`
(**three distinct values — equal values hide the bug**), then runs each of the three actions and asserts
each succeeds, with `withWalletBalancesCheckSimple [feeWallet := valueFromLovelace n]` for the matching
`n`. Under the Belt System's two-list shape with a swapped selector, the mis-paired action fails `K2`;
under `feeFor` it cannot. A hedgehog property `feeFor a cfg == offchainFeeFor a cfg` would be tautological
once `feeFor` is the single source — **do not write it**; the balance assertion is the real test.

### 4.2 Fix #2 — oracle admin key cannot be rotated

**Where it is.** `src/lib/onchain-lib/Onchain/Protocol/Types.hs:84–101`:
`OracleAdminAction = OraclePause | OracleUnpause | OracleSetFees (Maybe FeeConfig) | OracleSetMinUTxOValue Integer`.
No `SetAdmin`. `opAdminPkh` is commented `-- | Admin public key hash (can rotate itself)` — which is
false; nothing can change it. The admin key is permanent for the life of the deployment.

**The fix lands in exactly ONE file on-chain and three off-chain plus the CLI. `oracleLambda` does not
change.** (Corrected from "two files on-chain": the list below names only `Protocol/Types.hs`, and
`OracleValidator.hs` — verified at 92 lines, trace codes `O0`–`O4`, `O1` = admin signature — is untouched.
Note the compiled oracle *script hash* still changes, because `applyOracleAdminAction` is `INLINEABLE` and
gets inlined into `oracleLambda`; irrelevant for a fresh deployment, but say so in the ADR.)

```haskell
-- Palmares.Onchain.Protocol.Types — index 4, appended. 0..3 are frozen forever.
data OracleAdminAction
  = OraclePause
  | OracleUnpause
  | OracleSetFees (Maybe FeeConfig)
  | OracleSetMinUTxOValue Integer
  | OracleSetAdmin PubKeyHash              -- NEW
makeIsDataSchemaIndexed ''OracleAdminAction
  [ ('OraclePause, 0), ('OracleUnpause, 1), ('OracleSetFees, 2)
  , ('OracleSetMinUTxOValue, 3), ('OracleSetAdmin, 4) ]

{-# INLINEABLE applyOracleAdminAction #-}
applyOracleAdminAction (OracleSetAdmin pkh) params = params {opAdminPkh = pkh}   -- NEW clause
```

`oracleLambda` already does the right thing and needs no edit — it checks
`elem (opAdminPkh currentParams) txInfoSignatories` (the **old** admin signs) and then
`checkTxOutAtIndexWithDatumMinValueAndAddress … (applyOracleAdminAction action currentParams) …`. Rotation
is correct by construction. That is the payoff of the existing "redeemer carries the action, validator
recomputes the expected datum" design — **say so in the ADR**, because it is the reason this fix is three
lines instead of a validator rewrite.

Off-chain mirror (the "keep on-chain and off-chain update logic in sync" rule in `onchain-lib/CLAUDE.md`):
- `Palmares/DomainTypes/Core/Actions.hs` — `AdminActionType` gains `SetAdminAction GYPubKeyHash`
- `Palmares/TxBuilding/Operations.hs` — one clause each in `adminActionToOracleAdminAction` and
  `applyAdminAction`
- `src/exe/admin/Main.hs` — `rotate-admin --new-admin <pkh>` with a typed confirmation prompt

**(decision) The new admin does NOT co-sign.** Requiring it would make rotating to a cold or
hardware-held key impossible — which is the main reason anyone rotates. A wrong-pkh rotation is an
operator error recoverable only by the new holder; that is inherent to key rotation, not a validator
defect, and an on-chain check costs ExUnits on every oracle update for a non-exploitable mistake (house
doctrine: "no ExUnits for non-exploitable defense"). Mitigate in the CLI: echo the decoded bech32 and
require the operator to retype the last 8 hex chars.

**The test.** `src/test/UnitTests/Oracle.hs`:
`"admin rotation: old admin rotates out, new admin takes over, old admin is locked out"`
```
1. adminInteraction w1 (SetAdminAction (pkh w2))   -- succeeds
2. adminInteraction w2 PauseAction                 -- succeeds: new admin has authority
3. mustFail $ adminInteraction w1 UnpauseAction     -- fails with O1: old admin has none
```
Step 3 is the assertion. The test cannot even be *written* against the old behaviour (`SetAdminAction`
does not exist) — which is the honest version of "fails on the old code".

### 4.3 Fix #3 — projection poisoning

**Where it is — two sites, one root cause.**

1. `src/lib/offchain-lib/Ingestion.hs:35–75`. `projectChainEvent` classifies a UTxO purely by
   `amAddress == addressFromScriptHash nid <xValidator>` plus a successful datum parse, then reads the
   entity id **out of the datum**. Four of the five branches never look at `amValue`:
   ```haskell
   add | add == addressFromScriptHash nid ranksValidatorHashGY ->
     case rankFromGYOutDatum amDatum of
       Just onchainRank -> …                       -- id comes from the datum; value unchecked
   ```
   Only the `IntervalDatum` branch is safe — it calls `extractNFTAssetClass amValue` (line 64). So the
   correct pattern already exists in the file; it was applied once out of five times.
2. `src/lib/offchain-lib/TxBuilding/Lookups.hs:134–138`. `getAllParsedDatumsAtValidator` throws the value
   away before the caller can check it:
   ```haskell
   allDatums <- fmap snd <$> utxosAtAddressesWithDatums [addr]
   return $ mapMaybe parser (catMaybes allDatums)
   ```
   Four call sites (lines 141, 154, 353, 468) are therefore all poisonable.

   **(CORRECTED) This second site becomes dead code, so DELETE it instead of guarding it.** Measured: every
   consumer of those four wrappers (`getAllOnchainValidRanks` L141, `getAllOnchainProfiles` L154,
   `getAllMembershipDatums` L353, `getAllAchievements` L468 — via `getAllRanks`/`getAllPromotions`/
   `getAllProfiles`/`getAllAchievements`) lives in `src/exe/query-api/Query/Live.hs`, which §0.4 skips.
   Nothing in `interaction-api`, `admin`, `chain-sync` or the test suite calls them
   (`grep -rn 'getAll\(Profiles\|Ranks\|Promotions\|MembershipDatums\|Achievements\)' src/` returns only
   `Query/Live.hs` and `Query/Projected.hs`, and the latter uses its own `*Projected` DB queries). So
   `getAllParsedDatumsAtValidator` + its four wrappers are deleted with `Query/Live.hs`, and
   **fix #3 has exactly one implementation site: `Ingestion.hs`.** The shared `authenticateStateUTxO` guard
   still belongs in `TxBuilding/Utils.hs` (Ingestion already imports from there for `extractNFTAssetClass`).
   The plan therefore loses the "update four call sites" task — and nothing may reintroduce a
   whole-address-scan helper without the guard.

Exploit (free, permissionless, no minting): pay 2 ADA to the validator address with a hand-crafted inline
datum claiming any id, any issuer, any date. It lands in `attestation_projection` and in the API.

**The fix: one guard in the shared helper, both callers routed through it.**

```haskell
-- Palmares.TxBuilding.Utils
-- | A protocol state UTxO is authentic iff its value carries exactly one of the NFT named by
-- (or derived from) its own datum, under OUR minting policy. Address + parseable datum is not enough:
-- anyone can pay a forged datum to a script address.
authenticateStateUTxO :: GYMintingPolicyId -> GYAssetClass -> GYValue -> Bool
authenticateStateUTxO ourPolicy ac v =
  case ac of
    GYToken pid _ -> pid == ourPolicy && valueAssetClass v ac == 1
    GYLovelace    -> False
```

- `Ingestion.projectChainEvent` passes `amValue` on **every** branch and returns `NoEvent am` unless
  `authenticateStateUTxO ourPolicy (idOf datum) amValue`. The raw match is still written to
  `onchain_match_event` (so the log stays a faithful record of the chain); only the projection rejects.
- `getAllParsedDatumsAtValidator` gains an id projector and filters on the value:
  ```haskell
  getAllAuthenticStateAtValidator ::
    (GYTxQueryMonad m) => GYNetworkId -> GYMintingPolicyId -> GYScriptHash ->
    (GYDatum -> Maybe a) -> (a -> GYAssetClass) -> m [a]
  ```
  All four call sites updated. The old name is deleted, not deprecated, so no caller can take the unsafe
  path by accident.
- Free second guard: the policy-id check rejects a forged NFT minted under someone else's policy, which a
  bare `valueAssetClass v ac == 1` would accept.

**The tests.** `src/test/UnitTests/ChainSyncReplay.hs` — `sendDustToValidator` already exists in
`TestRuns.hs`, so both cases are cheap:
- `"a well-formed datum with no NFT in the value is not projected"` — pay 2 ADA + a *valid* attestation
  datum naming a real id to the attestations validator address; run the projection; assert
  `attestation_projection` count is **unchanged** and `onchain_match_event` count is **+1**. (Two
  assertions, because "the raw log still records it" is part of the contract.)
- `"a datum naming id X in a UTxO holding NFT Y is not projected"` — the subtler case; same assertions.
- Positive control: the genuine `IssueAttestation` tx **is** projected.

Both negative cases produce a phantom row on the current Belt System code.

### 4.4 Fix #4 — dates in datums not bounded by the tx validity range

**Where it is.** `MintingPolicy.hs` takes `achievementDate` / `startDate` / `endDate` as redeemer
arguments and writes them into the datum with no comparison against `txInfoValidRange`. Off-chain,
`getOracleFeeAndValiditySkeleton` returns an `isInvalidBeforeNow` that several operations then **discard**
(`_isInvalidBeforeNow` at `Operations.hs:312`). So an issuer can mint an attestation dated 1970 or 2099.

**The fix in the new design — and the distinction that matters.** There are two kinds of date, and they
must be treated differently. Getting this backwards is the likeliest plan-author error:

| Field | Meaning | Bound by validity range? |
|---|---|---|
| `attIssuedAt :: POSIXTime` | *when this attestation was made* — a timestamp | **YES.** It must be provably the tx's own time. |
| `affFrom`, `affTo` (Affiliation) | *the period being asserted about* — a historical claim | **NO.** "X was a member of Y in 2023" is a legitimate claim in a 2026 tx. Bounding it would make history unattestable. |

```haskell
-- Palmares.Onchain.Utils
-- | A datum timestamp is sound iff it lies inside a validity interval that is FINITE ON BOTH SIDES.
-- The finiteness check is the load-bearing half: with an open upper bound, `member` is satisfied by
-- any future timestamp, so an unbounded range is as good as no check at all.
{-# INLINEABLE checkTimestampInFiniteRange #-}
checkTimestampInFiniteRange :: POSIXTime -> POSIXTimeRange -> Bool
checkTimestampInFiniteRange t r =
  case (ivFrom r, ivTo r) of
    (LowerBound (Finite lo) _, UpperBound (Finite hi) _) -> lo <= t && t <= hi
    _                                                    -> False
```
Call it in the minting policy for `MintIssueAttestation` and `MintClaimProfile`:
```haskell
traceIfFalse "A5" (checkTimestampInFiniteRange attIssuedAt txInfoValidRange)
```
Off-chain: every operation that mints a timestamped datum uses `txIsValidForSafeEra now` from
`Palmares.TxBuilding.Skeletons` (already extracted as-is, `safeEraTime = 25920` ≈ 7h) and sets
`attIssuedAt = now`. No operation may discard the validity skeleton — enforce it by making
`getOracleFeeAndValiditySkeleton` return a record whose validity field has no `_`-prefixed use anywhere
(`-Wall -Wunused-*` is already on, so a genuinely unused one would be caught).

**The tests.** `src/test/UnitTests/Attestation.hs`, three cases:
- `"issuedAt outside the validity range is rejected"` — build `IssueAttestation` with validity
  `[now, now+7h]` and `attIssuedAt = now + 10h` → `mustFail` (`A5`).
- `"an unbounded validity range is rejected"` — same tx with only `isInvalidBefore now` (no upper bound)
  → `mustFail` (`A5`). **This is the case a naive `Interval.member` fix passes and must not.**
- `"issuedAt inside a finite range is accepted"` — positive control.
- Plus `"an Affiliation attestation may assert a period before the tx"` — `affFrom = now - 2 years`
  succeeds. This locks in the distinction so a later "tighten the date checks" commit cannot break
  historical claims.

### 4.5 Fix #5 — activity timestamps use DB `insertedAt` instead of chain time

**Where it is.** Every `put*Projection` in `src/lib/offchain-lib/Storage.hs` does
`now <- liftIO getCurrentTime` and stores it as `insertedAt` (lines 340, 357, 374, 397, 446, 464), and
the activity feed in `Query/Aggregates.hs` / `Query/Pages.hs` orders and `since`-filters on it. The chain
time was always recoverable — `createdAtSlot` is right there in the same row — it was just never used.

The sharp consequence: `rollbackTo` wipes the projection tables and replays the raw log, so **every
rollback rewrites every `insertedAt`** and the whole activity feed silently renumbers. A re-sync reorders
history.

**The fix: do not store a timestamp at all. Convert slot → time on read.**

Rejected alternative (what the obvious fix would be): add a `createdAt UTCTime` column computed at
projection time. That needs a `GYSlotConfig` threaded into `projectAndStore`, which runs inside
`SqlPersistT m` with no provider in scope — a new argument through five functions, a new field in the
sync env, and a second copy of a value that must agree with the slot. Two columns that must agree is the
same defect class as fix #1.

What to do instead:
1. **Keep `createdAtSlot` as the only time in the projection tables.** It is already there, it is already
   the chain's own clock, and it is immune to replay.
2. **`insertedAt` stays** (it is genuinely useful ingest telemetry) but is **never serialized** — it
   appears in no `QueryResponses` DTO and in no `SELECT` list of a public endpoint.
3. **query-api converts on read.** `QueryAppContext` already holds `providerContext :: ProviderCtx`
   (`QueryAppMonad.hs:34`). **(CORRECTED) `slotToBeginTime` is monadic, not pure** —
   `slotToBeginTime :: GYTxQueryMonad f => GYSlot -> f GYTime`, defined as
   `flip slotToBeginTimePure x <$> slotConfig` (Atlas @`bf0d2d1`, `src/GeniusYield/TxBuilder/Class.hs:555`).
   So a handler must not call it per row inside `runQuery`. Pattern: fetch the slot config **once per
   request** (`cfg <- liftIO $ runQuery providerCtx slotConfig`) and map rows with the pure
   `slotToBeginTimePure cfg`. Same for the `since` translation: `enclosingSlotFromTimePure cfg`
   (`enclosingSlotFromTime'` at `Class.hs:573` is the monadic, throwing variant). The API's `created_at` is
   derived, never stored.
4. **The activity feed orders by `created_at_slot`**, and a `since :: UTCTime` query param is translated
   time → slot (`enclosingSlotFromTimePure cfg`, see above) **before** the SQL, so the index on
   `created_at_slot` is used and no provider round-trip happens per row.

Zero schema columns added, zero new arguments, and the two values cannot drift because there is only one.

**The test.** `src/test/UnitTests/ChainSyncReplay.hs`:
`"the activity feed is replay-stable: created_at follows the chain slot, not ingest time"`
```
1. store raw matches for slots [S1, S2, S3] with valid datums; project.
2. snapshot the API-shaped activity feed (list of (id, created_at)).
3. assert created_at_i == slotToBeginTime S_i for each.
4. sleep 1s; rollbackTo (S1 - 1); re-project from the raw log.
5. assert the feed snapshot is BYTE-IDENTICAL to step 2.
```
Step 5 is the assertion that fails on the Belt System: `insertedAt` is re-stamped on replay, so after a
rollback the feed's ordering and displayed times both change. Step 3 alone would pass on a half-fix that
stores a chain-derived `createdAt` at projection time; only step 5 catches the drift class.

### 4.6 Fix summary

| Fix | Primary file(s) in the extracted code | Nature of the fix | Test file | The assertion that fails on the old behaviour |
|---|---|---|---|---|
| #1 fee selector | `Onchain/Protocol/{Types,Lookup}.hs` + `TxBuilding/Operations.hs` | delete the selector argument; `feeFor` keyed on the redeemer | `UnitTests/Oracle.hs` | each action's fee output, with **three distinct** fee values |
| #2 admin rotation | `Onchain/Protocol/Types.hs` (+3 off-chain files, +CLI) | `OracleSetAdmin` at index 4; `oracleLambda` unchanged | `UnitTests/Oracle.hs` | old admin's next update fails with `O1` |
| #3 projection poisoning | `Ingestion.hs` + `TxBuilding/Utils.hs` (the `Lookups.hs` helper is **deleted with `Query/Live.hs`**, not guarded — §4.3) | one `authenticateStateUTxO` guard | `UnitTests/ChainSyncReplay.hs` | projection count unchanged **and** raw-log count +1 |
| #4 unbounded dates | `Onchain/Utils.hs` + `MintingPolicy.hs` + `TxBuilding/Operations.hs` | timestamp ∈ **finite** validity range; historical claims exempt | `UnitTests/Attestation.hs` | an **unbounded** validity range is rejected |
| #5 chain time | `Storage.hs` (a removal) + `Query/Aggregates.hs` | convert slot→time on read; never store a wall clock | `UnitTests/ChainSyncReplay.hs` | the feed is byte-identical after rollback + replay |

---

## 5. Naming and the rename footprint

### 5.1 Names

| Thing | Value |
|---|---|
| Repo / cabal package | `palmares` (file `palmares.cabal`) |
| Module namespace | **`Palmares.*`** — every module, no exceptions |
| Internal libraries | `onchain-lib`, `offchain-lib`, `chainsync-lib`, `webapi-lib` (+ `mcp-server-lib` at F3) — **unchanged from the Belt System**: already generic, and both `CLAUDE.md` files reference them by these names |
| Executables | `admin`, `query-api` (:8083), `interaction-api` (:8082), `attest-sync` (:8084) (+ `mcp-server` :8085 at F3) |
| Docker images | `<owner>/palmares-{query-api,interaction-api,attest-sync,admin}`; builder `palmares-builder:9.6.6`, runtime `palmares-runtime-base:3.20` |
| Postgres schemas | `palmares` (governance views), `palmares_attest` (Kupo projections) — §1.2 |
| New env vars | `PALMARES_*` prefix. Existing ones (`PORT`, `PG_CONN_STR`, `KUPO_URL`, `BASIC_USER`/`BASIC_PASS`, `CORS_ALLOWED_ORIGINS`, `ATLAS_CORE_CONFIG`, `DEPLOYED_VALIDATORS_CONFIG`, `BATCH_SIZE`, `FETCH_BATCH_SIZE`, `ROLLBACK_MARGIN`) keep their names, so the existing k8s/Flux topology is reused with no edits |
| Config files | `config/config_atlas.json`, `config/config_validators.json` — **not** `config_palmares_validators.json` (see rule 3 below) |
| MCP resource URIs (F3) | `gov://metrics/*`, `gov://glossary`, `gov://cip1694` — a **neutral scheme**, not `palmares://` |

**`Palmares.*` vs `GovRegistry.*`: choose `Palmares.*`.** A product name is one `sed` away from any other
product name; a *descriptive* namespace survives a rename but misdescribes the project the moment scope
shifts (the Belt System's `Onchain.BJJ` is exactly that trap). Pick the proper noun, and make the rename
mechanical — which is what §5.2 is for.

### 5.2 Rename footprint — three commands, and the three rules that keep it at three

```bash
# 1. every textual occurrence
grep -rl -E 'Palmares|palmares' --include='*.hs' --include='*.cabal' --include='*.md' \
     --include='*.yml' --include='*.yaml' --include='*.sh' --include='*.json' --include='*.sql' . \
  | xargs sed -i '' 's/Palmares/NewName/g; s/palmares/newname/g'
# 2. the namespace directories
find src -type d -name Palmares | while read -r d; do git mv "$d" "$(dirname "$d")/NewName"; done
# 3. the cabal file
git mv palmares.cabal newname.cabal && cabal build all && cabal test
```

This stays at three commands only if these rules hold from commit #1. They are cheap now and expensive
later — the Belt System broke all three (`mkBJJApp`, `runBJJActionWithPK`, `BJJ_ENV_LOADED`,
`bjj://rules/*`, `config_bjj_validators.json`, `deployBJJValidators`, `bjjInteraction`):

1. **No identifier contains the project name.** `runActionWithPK`, not `runPalmaresActionWithPK`.
   `mkWebApp`, not `mkPalmaresApp`. Enforce in review; it is a one-word grep.
2. **One namespace segment.** `Palmares.X.Y`, never `Palmares.Palmares.*` and never a second
   project-named segment deeper in.
3. **No project name in a filename or a config key.** `config_validators.json`, not
   `config_palmares_validators.json`. `PALMARES_` is the one permitted prefix, and only for env vars,
   where a namespace prefix is genuinely needed to avoid collisions.
4. **No project name in a public URI path or an MCP scheme.** `gov://metrics/participation-rate` keeps
   working after a rename; `palmares://…` breaks every client.

Residual after the three commands: the Docker Hub repository names and the GitHub repo name (manual, one
each), and `NOTICE` (one line). Nothing in the build.

---

## 6. Build verification

### 6.1 The command sequence

```bash
cd /path/to/palmares
direnv allow                       # first run: downloads the devx GHC 9.6.6 closure
cabal update                       # fetches hackage + CHaP at the pinned index-states
cabal build all                    # must be green before any rewrite commit (§3.2 rule 3)
cabal test --test-show-details=direct
cabal freeze && git add cabal.project.freeze && git commit -m "chore: freeze dependency plan"
```
Per-component iteration (what you actually run all day):
```bash
cabal build onchain-lib            # Plinth only — the slow one
cabal build lib:offchain-lib
cabal build exe:query-api
cabal test                         # CLB emulator + in-memory SQLite; no network, no Postgres needed
cabal run exe:admin -- write-blueprint     # CIP-57 blueprint regen (F1 check 8)
scripts/test_exunits.sh            # ExUnits + script sizes from the test log
```

### 6.2 Realistic cold-build time

The Belt System's `cabal.project.freeze` carries **551 version constraints** (707 lines). That plan is
dominated by `cardano-api`, `cardano-ledger-*`, `plutus-core`, `plutus-ledger-api` and `plutus-tx-plugin`.

| Scenario | Expectation |
|---|---|
| `direnv allow` first run (devx closure, ~8–12 GB) | 20–40 min on a decent connection |
| `cabal build all` cold, Apple-silicon laptop | **40–75 min** |
| `cabal build all` cold, 4-core GitHub-hosted runner | **2–3 h** (see §6.3) |
| `cabal build all` cold, ARM minirack | don't — use it to run images, not to build them |
| incremental, `offchain-lib`/exe change | seconds to ~2 min |
| incremental, **`onchain-lib`** change | **2–4 min** — the Plinth flags (`-fobject-code -fno-full-laziness -fno-specialise …`) disable most of GHC's caching on purpose. Budget for it; do not "optimize" the flags. |

### 6.3 CI must change — and how

**Current state.** All four Belt System workflows (`chainsync.yml`, `interaction-api.yml`,
`mcp-server.yml`, `query-api.yml`) have `runs-on: self-hosted` on **both** jobs (`test` at line 32,
`build-and-push` at line 49). The runner is the minirack. Per the owner's own notes the runner was offline
in July 2026 and backend images had to be built by hand. So today: **the minirack being down blocks every
PR check.** For a public, community, contributor-facing repo that is disqualifying — an outside
contributor's PR would simply never get a green tick.

**(decision) Split by runner, collapse four workflows into two.**

`.github/workflows/ci.yml` — never touches the minirack.

**(CORRECTED 2026-10-19) do NOT use `container: benz0li/ghc-musl:9.6.6`.** That image is Alpine/musl
(`Dockerfile.base` installs with `apk`), and the Actions runner injects its own **glibc-linked** `node20` to
execute JavaScript actions inside the job container. `actions/checkout@v4` and `actions/cache@v4` are
JavaScript actions, so they fail in a musl container (`actions/checkout#151`, `actions/runner#1637`; the
`setup-node` maintainers state the Node binaries "are built against glibc … we don't support alternative C
libraries like musl, which is used in Alpine Linux" — `actions/setup-node#1293`). On arm64 the runner refuses
outright ("JavaScript Actions in Alpine containers are only supported on x64 Linux runners").

The lazy and correct shape is the one the Belt System already proves out — checkout and Docker run on the
**host**, the build runs *inside* `docker run`. Only `runs-on` changes:

```yaml
name: CI
on:
  push: {branches: [main]}
  pull_request: {branches: [main]}
  workflow_dispatch:
concurrency: {group: "ci-${{ github.ref }}", cancel-in-progress: true}
jobs:
  build-and-test:
    runs-on: ubuntu-latest                    # host has glibc + docker; no `container:` key
    timeout-minutes: 350                      # first (uncached) run is the expensive one; GH max is 360
    steps:
      - uses: actions/checkout@v4
      - uses: docker/setup-buildx-action@v3
      - name: Build the builder image (deps layer is cached by the registry)
        uses: docker/build-push-action@v6
        with:
          context: .
          file: Dockerfile.base
          tags: palmares-builder:9.6.6
          load: true
          cache-from: type=registry,ref=${{ secrets.DOCKERHUB_USERNAME }}/palmares-builder:cache
          cache-to: type=registry,ref=${{ secrets.DOCKERHUB_USERNAME }}/palmares-builder:cache,mode=max
      - name: Build and test
        run: |
          docker run --rm -v "$PWD:/palmares" -w /palmares palmares-builder:9.6.6 \
            bash -lc "cabal update && cabal build all && cabal test --test-show-details=direct"
```
`Dockerfile.base` already ends with `COPY *.cabal cabal.project cabal.project.freeze ./` + `cabal update` +
`cabal build --only-dependencies --enable-tests`, so the 551-constraint dependency build **is** the cached
Docker layer — there is nothing left for `actions/cache` to do, and the cache key is the freeze file by
construction. (`cache-to` needs the Docker Hub secret; without it, drop both cache lines and accept the cold
build, or publish `palmares-builder:9.6.6` once from a laptop and `FROM` it.)
`.github/workflows/images.yml` — self-hosted, but only for releases:
```yaml
name: Images
on:
  push: {tags: ['*.*.*']}
  workflow_dispatch: {}
jobs:
  build-and-push:
    runs-on: self-hosted
    strategy:
      matrix: {service: [query-api, interaction-api, attest-sync, admin]}
    steps:
      - uses: actions/checkout@v4
      - run: docker build -t palmares-builder:9.6.6 -f Dockerfile.base .
      - run: docker build -t palmares-runtime-base:3.20 -f Dockerfile.runtime-base .
      - uses: docker/login-action@v3
        with: {username: "${{ secrets.DOCKERHUB_USERNAME }}", password: "${{ secrets.DOCKERHUB_TOKEN }}"}
      - run: |
          SHA=${GITHUB_SHA::7}
          IMG=${{ secrets.DOCKERHUB_USERNAME }}/palmares-${{ matrix.service }}
          docker build -t "$IMG:$SHA" -f Dockerfile.${{ matrix.service }} .
          docker tag "$IMG:$SHA" "$IMG:${GITHUB_REF_NAME}"
          docker push "$IMG:$SHA" && docker push "$IMG:${GITHUB_REF_NAME}"
```

Why this shape:
- **A PR never depends on the minirack.** Public repos get free GitHub-hosted minutes, so there is no cost
  argument for keeping `test` on `self-hosted`.
- **A dead runner blocks releases, not contributions** — and `scripts/build-images.sh` is already the
  documented manual path (the July 2026 workaround, now a first-class escape hatch rather than a
  surprise).
- **Four near-identical workflows become one matrix job.** The Belt System's four files differ only in the
  `paths:` filter and the image name; that duplication is why a CI change had to be made four times.

**Honest caveat.** A 551-package Cardano Haskell dependency build on a 4-core hosted runner is marginal
against GitHub's 6-hour job limit on the *first, uncached* run. Two mitigations, in order: (a) the registry
build cache above makes it a once-per-pin-change cost; (b) if the cold run still times out, build
`palmares-builder:9.6.6` once from the minirack or a laptop, push it to Docker Hub, and change
`Dockerfile.base`'s first stage to `FROM <owner>/palmares-builder:9.6.6` — CI then compiles only our ~5k
lines. Decide after measuring the first run; do not pre-build the fallback.

---

## 7. Open questions and blockers

1. **(BLOCKER, write path only) The Maestro Haskell SDK is abandonware.**
   `maestro-org/maestro-cardano-haskell-sdk` is **archived** (last commit 2026-03-12, archived state
   confirmed 2026-10-19); `maestro-sdk` is on **neither Hackage nor CHaP**; the Belt System depends on a
   personal fork (`sourabhxyz/haskell-sdk`, last push 2025-11-04). The read path (db-sync + Kupo + API +
   UI) is unaffected; the **write path (build-tx) is**, and spec §7 makes Maestro the write-path provider.
   Mitigation to decide at plan time, as `docs/adr/0002-write-path-provider.md`: Atlas ships a
   **local-node provider** (`src/GeniusYield/Providers/Node.hs`) and we already run both a node and Kupo.
   Node + Kupo would remove the commercial dependency entirely and make the "self-hostable" claim true for
   the write path too. Needs one spike: does Atlas's local-node provider carry Conway governance
   certificates and `VotingProcedures` through `GYTxSkeleton`? (It queries the node directly, so it should.)
   Sources: `api.github.com/repos/maestro-org/maestro-cardano-haskell-sdk` (`"archived": true`);
   `hackage.haskell.org/package/maestro-sdk` → 404; `chap.intersectmbo.org/package/maestro-sdk-*` → 404.
2. **(correction to the spec, not a blocker) Atlas has no db-sync provider.**
   `src/GeniusYield/Providers/` contains exactly: `Blockfrost`, `CacheLocal`, `CacheMempool`,
   `CachedQueryUTxOs`, `Common`, `GCP`, `Hydra`, `Kupo`, `LiteChainIndex`, `Maestro`, `Node`, `Ogmios`,
   `Sentry`. Spec §7 and §13 assume a db-sync provider exists ("de evaluat … providerii local-node sau
   db-sync ai Atlas"). Half of that is real: **local-node yes, db-sync no.** Delete the db-sync-provider
   hope from the plan and pursue local-node (item 1).
   Source: `api.github.com/repos/geniusyield/atlas/contents/src/GeniusYield/Providers`.
3. **Atlas staleness is worse than the spec records.** Spec §13 says "5 luni fără release". Measured:
   last release **v0.14.1, 2025-05-21 → 17 months**; `main` last pushed **2026-02-03 → 8.5 months**; 52
   open issues; not archived. Not an F1 blocker (the pinned commit builds), but the escape hatch ("build
   the few tx types directly on `cardano-api`") deserves to be a written ADR with a named list of the
   transactions involved, not a sentence in a risk table.
4. **Could not determine: preprod `only_governance` disk and RAM, first-hand.** Spec §8's "≈16 GB
   Postgres + ≈12 GB node DB, ~3.5 GB RAM" is not stated in db-sync's own docs and I did not find a
   primary source for the `only_governance` preset specifically. db-sync 13.7.2.1's notes do confirm the
   negative half (the LSM backend "crashes on resource-constrained devices such as the Raspberry Pi"), so
   **keep the default InMemory backend** and treat the sizing as an F1-check-1 measurement, not a given.
   Measure `pg_database_size` after the first full preprod sync and write the real number into the ADR.
5. **Script-DRep profile claim is still unspecified** (spec §4.3 defers it to the builder). Key-DReps
   prove control with an extra signatory (`mustBeSignedBy`), which is one line. Script-DReps cannot sign,
   so the claim needs a different proof (spend a UTxO at the DRep script address? require a witness from
   the script?). This is on-chain design, it blocks `MintClaimProfile`, and it should be settled in the
   F1 plan rather than discovered during implementation.
6. **Not a blocker, but decide before writing `sql/030_drep_metrics.sql`:** spec §4.2's participation rate
   needs "actions in which the DRep was *eligible* to vote". The exact definition (does an action proposed
   and ratified inside a single epoch count? does an expired action count?) is a published-formula decision,
   not a SQL detail. Write the definition in `docs/metrics.md` first, then the SQL.
   **(CORRECTED) the inputs are NOT all in `drep_registration` + `gov_action_proposal`.** Verified against
   `doc/schema.md` @13.7.2.1: neither table carries an epoch. `drep_registration` has only
   `(tx_id, cert_index, deposit, voting_anchor_id, drep_hash_id)`; `gov_action_proposal` has `expiration`,
   `ratified_epoch`, `enacted_epoch`, `dropped_epoch`, `expired_epoch` but **no proposed/submitted epoch**.
   Both the registration epoch and the proposal epoch come from `tx` → `block.epoch_no` (and
   `block.time`/`block.slot_no` for latency). `tx` and `block` are populated under `only_governance` (the
   governance cert inserts FK to `tx_id`), but `tx.fee` is 0 and `tx_out`/`tx_in`/`ma_tx_out` are empty —
   verified in `Era/Universal/Insert/Tx.hs:90-91` (`(True, _, _) -> pure ([], 0, …)` when `disInOut`).
   Second correction: 13.7.2.1 turned `epoch` into a **view** over `epoch_finalized` + `epoch_current`
   (`schema/migration-2-0049-20260604.sql`), and its own notes say to query `epoch_finalized` directly when
   the in-progress epoch is not needed — so prefer `epoch_finalized`. `epoch_finalized` is **not** in
   `doc/schema.md` yet; read it from the migration file.
   (If the ARM pin lands on 13.7.1.0 per §1.5, `epoch` is still a plain table there — write the metric views
   against `block.epoch_no`, which is stable across both, and never against `epoch`.)

7. **(BLOCKER, infrastructure) The official db-sync image does not run on the minirack.** See §1.5: amd64
   only, cluster is ARM64-only. Decision taken there (blinklabs 13.7.1.0), but the plan must carry the
   version delta and re-verify `drep_distr` population on 13.7.1.0 as part of F1 check 1.

8. **(BLOCKER, CI) `container: benz0li/ghc-musl` + JavaScript actions do not work.** See §6.3 for the
   verified cause and the replacement workflow.

---

## Fact-check corrections

Adversarial re-verification of this note, 2026-10-19. Every row below was opened at its primary source, not
taken from the note. Everything **not** listed here was checked and **held** — in particular: all 70+ file
paths and `wc -l` line counts in §2 (exact), all BJJ-hit counts spot-checked (`OracleValidator` 1,
`Protocol/Types` 77, `Storage` 129, `WebAPI/Utils` 2, `Operations` 278, `AchievementsValidator` 34 — exact),
`Operations.hs:611` vs `MintingPolicy.hs:162` (fix #1 real, and `UnitTests/Oracle.hs` really does set both
fees to 1 500 000), `Ingestion.hs` 4-of-5 unguarded branches with `extractNFTAssetClass amValue` at L64,
`Lookups.hs:134-138` + call sites 141/154/353/468, `Storage.hs` `getCurrentTime` at 340/358/374/397/446/464,
`_isInvalidBeforeNow` at `Operations.hs:312`, `safeEraTime = 25920`, `multiset` used only by `Query/Live.hs`,
`O1` = oracle admin signature, all four workflows `runs-on: self-hosted` at lines 32 and 49, 551 freeze
constraints / 707 lines, the `.envrc` plumbing, CORS fail-closed-on-empty, `normalizeLimitOffset` 100/[1,500],
`QueryAppMonad.hs:34` `providerContext`, and every upstream pin (Atlas `bf0d2d1` = PR #472 @2025-08-27, latest
release v0.14.1 @2025-05-21, `main` @2026-02-03, 52 open issues; maestro fork `1e84ccb` @2025-05-20, fork last
push 2025-11-04, `maestro-sdk` 404 on Hackage **and** CHaP; clb `867ff70` **is** `master` HEAD @2025-06-04;
`cardano-addresses-4.0.0` 404 on CHaP while `-3.12.0` resolves, issue #294 closed 2025-03-03; devx
`d316c039` @2025-02-27 vs `main` @2026-04-23; `benz0li/ghc-musl:9.6.6` = `{amd64, arm64}`, updated
2025-02-17). The db-sync source claims all hold **at the 13.7.2.1 tag, not just master**: `parseOverrides obj
baseOptions` (Types.hs:489-510) really does make a preset a base that sibling keys override — so
`doc/configuration.md`'s "Preset is an aggregate setting that overrides all other properties" (L79) really is
wrong; `onlyGovInsertOptions` is exactly `disableAllInsertOptions { sioLedger = LedgerEnable, sioGovernance =
GovernanceConfig True, sioPoolStats = PoolStatsConfig True }`; `delegation_vote` inserts are gated on `ioGov`,
not `ioShelley` (`Certificate.hs:150-157`); `tx.fee = 0` under `tx_out: disable` (`Insert/Tx.hs:90-91`); and
all of `off_chain_vote_data.is_valid` (tri-state), `off_chain_vote_fetch_error.{fetch_error,retry_count}`,
`drep_distr`, `gov_action_proposal.{ratified,enacted,dropped,expired}_epoch`, `voting_procedure`,
`delegation_vote`, `drep_registration`, `committee`, `treasury_withdrawal`, `voting_anchor` exist in
`doc/schema.md` @13.7.2.1. Koios preprod re-probed live: `proposal_list`/`vote_list`/`drep_list`/
`drep_epoch_summary` → 200, `drep_voting_power_history` unfiltered → fails but **200 with `_drep_id`**,
`drep_info` GET → 404 (POST only).

### Blockers introduced by this note (now fixed above)

| # | Was | Now | Primary source |
|---|---|---|---|
| F1 | §1.5's db-sync config was a 6-key JSON with `NodeConfigFile: "preprod-config.json"` (pointing at itself) and a `RequiresNetworkMagic` key | Start from db-sync's shipped `config/preprod-config.yaml`; `NodeConfigFile` = the **node** config; drop `RequiresNetworkMagic`. The minimal object cannot parse: the logging `Representation` is read from the same object and its non-`Maybe` fields are required | `Config/Types.hs:440-456` @13.7.2.1; `iohk-monitoring .../Data/Configuration.lhs:94-113`; db-sync `config/preprod-config.yaml` |
| F2 | `ghcr.io/intersectmbo/cardano-db-sync:13.7.2.1` in compose, on an ARM cluster | `ghcr.io/blinklabs-io/cardano-db-sync:13.7.1.0` (real `{amd64,arm64}` manifest list); version pin drops to 13.7.1.0 for ARM | ghcr image config `"architecture":"amd64"`; 13.7.2.1 notes "Platforms: Linux 64-bit"; `apps-k8s-configs/CLAUDE.md` "ARM64-only workloads" |
| F3 | `ci.yml` with `container: benz0li/ghc-musl:9.6.6` + `actions/checkout@v4` + `actions/cache@v4` | `runs-on: ubuntu-latest`, no `container:`; build inside `docker run` exactly as the Belt System's own workflows do; the deps cache is `Dockerfile.base`'s layer | `actions/setup-node#1293` ("built against glibc … we don't support … musl"); `actions/checkout#151`; `actions/runner#1637`; `Dockerfile.base` uses `apk` |
| F4 | §3.2 rule 3: "`cabal build all` must pass after step 3 (as-is + rename only)" | Green gate moves to **after the strip commit**; 10+ kept files import skipped modules (table in §3.2) | `grep -rn 'import .*\(Onchain\.BJJ\|…\)' src/` |

### Other corrections made in place

| § | Correction |
|---|---|
| 0.1 | Added the two real holes: `va.type != 'constitution'` is excluded from the fetch queue (`Statement/OffChain.hs:536-551`), and 13.7.2.1 ships a known "duplicate off-chain vote metadata child rows" bug (PR #2137) so the anchor view must de-duplicate. Added the verified 3 MB cap and the error constructors, and the `ipfs_gateway` default. |
| 1.1 | Added `config/node/` (node config + 4 genesis files) — absent before, so neither the node nor db-sync could start. Added the four **other** `CLAUDE.md` files (`src/exe/`, `src/lib/offchain-lib/`, `src/lib/onchain-lib/`, `src/test/`), and flagged that `src/lib/onchain-lib/CLAUDE.md` documents the very `checkFee` selector contract fix #1 deletes. |
| 1.2 | `CREATE OR REPLACE VIEW` is **not** idempotent across a column-shape change (PostgreSQL CREATE VIEW docs) → `DROP SCHEMA … CASCADE` in `sql/000`. `palmares_attest` must be created by **attest-sync** before `runMigrations`, not by query-api, or `persistent` migrates into db-sync's `public`. Added the `DROP TABLE … CASCADE` hazard from `migration-2-0049` → never `MATERIALIZED VIEW` over db-sync tables. |
| 1.3 | onchain-lib's `default-extensions` list is **28** entries, not 26. |
| 1.4 | Maestro upstream: one repo (`haskell-sdk` → `maestro-cardano-haskell-sdk`), archived, last push 2026-04-30 — the note gave two different dates. |
| 1.9 | Stopped gitignoring the deployed-validators config: the Belt System commits its equivalent (`git ls-files config/`), it holds no secret, and spec S2/§7 reproducibility needs it. Only `config/config_atlas.json` stays ignored. |
| 1.10 | Node and db-sync now get different config files and different mounts; db-sync state volume moved from `/var/lib/cdbsync` to `/var/lib/cexplorer` (the entrypoint's actual `--state-dir`, `nix/nixos/cardano-db-sync-service.nix:148`) — as written the ledger state would have been lost on every container recreate. |
| 2.5 | `InteractionAppMonad.hs` reclassified as-is → **strip** (`import IPFS (IPFSConfig)`, L31). `QueryAppMonad.hs` likewise (dropping `liveProjection` is an edit). |
| 2.6 | `mcp-server-lib` has **18** modules, not 14 (the table already listed 18). |
| 2.8 | Totals recomputed from the manifest rows: as-is 19/2,062 · strip 24/2,935 · **rewrite 25/10,614** · skip-F1 20/5,590 · skip-F3 18/2,831. The old table (22/2,350, 24/2,900, 24/9,400, 25/7,700) understated the rewrite work by ~1,200 lines and one file, which is what F0 is sized on. |
| 4.2 | "two files on-chain" → **one** (`Protocol/Types.hs`); added that the oracle script *hash* still changes because `applyOracleAdminAction` is inlined. |
| 4.3 | Fix #3's "second site" is dead code: every caller of `getAllParsedDatumsAtValidator`'s wrappers lives in the skipped `Query/Live.hs`. Delete the helper instead of guarding it → fix #3 has **one** site, `Ingestion.hs`. Removes a plan task. |
| 4.5 | `slotToBeginTime` is `GYTxQueryMonad f => GYSlot -> f GYTime` (Atlas `TxBuilder/Class.hs:555`), not pure: fetch `slotConfig` once per request and use `slotToBeginTimePure` / `enclosingSlotFromTimePure` per row. |
| 7.6 | Participation-rate inputs need `tx` → `block.epoch_no` joins; neither `drep_registration` nor `gov_action_proposal` carries an epoch, and `gov_action_proposal` has no *proposed* epoch at all. Prefer `block.epoch_no` over `epoch`, which 13.7.2.1 turned into a view. |

### Still-open gaps the plan must close (not fixed here)

- **The spec contradicts itself and this note on the indexer, in text a plan author will read as normative.**
  §4.1 was revised to cardano-db-sync, but §9 F0's exit criterion still says "un ADR care fixează sursa de
  datelor (**Ogmios follower**)" and §9 F1's GOAL still says "indexer de guvernanță (**follower Ogmios** →
  raw log → proiecții Postgres)". This note's §0.2/§0.3 kill the follower but never flag that §9 is stale.
  Fix the spec text, or the F1 task list will contain a service nobody decided to build.
- **Spec F1 checks 3 and 4** ("re-running the projections from zero produces identical tables", "after a
  rollback the projections rebuild consistently") were written for a projection *service*. With governance as
  plain views they are satisfied by construction — say so explicitly, or someone will build a determinism
  harness for a `SELECT`.
- **Preprod `only_governance` disk/RAM** is still unmeasured (the note's U1 stands), and it now has a second
  unknown: nobody has run blinklabs' arm64 db-sync build with `only_governance` + ledger ON on a Pi-class
  node. That is the real content of F1 check 1.
