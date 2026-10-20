# F1 — cross-track synthesis

Input: the five fact-checked design notes in this directory, plus the approved spec
(`docs/superpowers/specs/2026-10-19-gov-registry-spec.md`). Date 2026-10-19.

This note resolves what the five tracks could not see about each other. It does **not** re-litigate
anything a track already decided and had fact-checked — it only fixes collisions, picks one name per
concept, orders the work, and cuts what F1 does not need.

**Track labels are inconsistent across the notes** (the metric note calls infra "Track 1" and the API
note "Track 2"; the API note calls itself "Track 4"; the db-sync note also calls itself "Track 1").
Canonical labels used below, by filename:

| Label | File |
|---|---|
| **T-DATA** | `f1-dbsync-data-layer.md` |
| **T-EXTRACT** | `f1-library-extraction.md` |
| **T-METRIC** | `f1-metric-formulas.md` |
| **T-API** | `f1-api-contract.md` |
| **T-INFRA** | `f1-preprod-infra.md` |

Nothing below needs new research. Two facts were re-checked on disk because two tracks disagreed:

- `chains/cardano/preprod/workloads/statefulset-cardano-preprod.yaml` **exists** and runs the preprod
  node (T-DATA and T-INFRA are right; **T-API §8.3b is wrong** — it looked only under `apps/`).
- Flux root convention: chain-infra roots live in `clusters/minirack/chains/cardano-*.yaml`, app roots
  directly in `clusters/minirack/<app>/`. Both of T-INFRA's roots are therefore placeable without
  breaking the pattern (see §2, D-26).

---

## 1. Canonical names

One row per concept that drifted. **Use the "Canonical" column verbatim**; everything else is an alias
to be deleted on sight.

### 1.1 Database objects

| Concept | Seen as | **Canonical** |
|---|---|---|
| Governance schema | `gov` (T-DATA) · `palmares` (T-EXTRACT, T-METRIC) · `registry` (T-INFRA) | **`palmares`** |
| Attestation schema | `palmares_attest` (T-EXTRACT) | **`palmares_attest`** |
| Database | `cexplorer` (all) | **`cexplorer`** — one database, two added schemas, one `PG_CONN_STR` |
| db-sync's schema | `public` | **`public`** — read-only to us, except the `palmares_idx_*` indexes |
| View naming | `gov.v_*` / `gov.mv_*` (T-DATA) · bare names (T-METRIC) | **bare, schema-qualified**: `palmares.<name>`. No `v_`/`mv_` prefix — the schema already says whose it is. |
| Our indexes on db-sync tables | `gov_idx_*` (T-DATA) · `idx_delegation_vote_drep` (T-METRIC — collides with upstream's `idx_*`) | **`palmares_idx_*`** |
| Postgres roles | `dbsync`/`gov_owner`/`gov_api` (T-DATA) · bare `postgres` superuser (T-INFRA) | **`dbsync`** (db-sync's own, owns `public`) · **`palmares_owner`** (owns `palmares` + `palmares_attest`, `SELECT` on `public.*`, refreshes the matview) · **`palmares_ro`** (query-api runtime: `USAGE`+`SELECT` only) |

### 1.2 The view set (final)

Fourteen views and exactly one materialized view. Derived from T-DATA §3.3 and T-METRIC §0.3, merged.

| Canonical object | Replaces | Note |
|---|---|---|
| `palmares.sync_state` | T-DATA `v_sync_state` | tip by `ORDER BY block.id DESC LIMIT 1`; `now() AT TIME ZONE 'UTC' - b.time` |
| `palmares.drep` | `v_drep` | status from `drep_registration`, never from a `drep_hash` row's existence |
| `palmares.drep_cert` | `v_drep_cert` | `drep_update_log` of spec §6 |
| `palmares.drep_interval` | T-METRIC `drep_interval` | **carries `from_slot`** (T-METRIC §5.3 correction) |
| `palmares.action_window` | T-METRIC `action_window` + part of `v_gov_action` | carries `last_votable_epoch` **and** `drep_votable` |
| `palmares.gov_action` | `v_gov_action` | joins `action_window` + `anchor` + `off_chain_vote_gov_action_data` |
| `palmares.vote` | `v_vote` **+** `v_vote_latest` **+** T-METRIC `drep_vote` | **all three roles**, with `seq_first`, `seq_last`, `vote_rows`, and **both** `voter_hot_hash` and `voter_cold_hash` for CC (§2 D-24) |
| `palmares.vote_tally` | `v_vote_tally` | computed over `vote WHERE seq_last = 1` |
| `palmares.anchor` | `v_anchor` | the single verdict `CASE` (§2 D-10) |
| `palmares.vote_delegation` | `v_vote_delegation` | |
| `palmares.live_delegation` | `v_live_delegation` | latest delegation minus a newer `stake_deregistration` |
| `palmares.drep_epoch_delegators` | *(the gap both T-DATA and T-INFRA flagged)* | per-epoch delegator count; **a view in F1**, matview only if measured slow (§3 cut list) |
| `palmares.drep_epoch_power` | `v_drep_epoch_power` **+** `v_drep_epoch_power_all` | ONE view, `target_kind ∈ {drep, always_abstain, always_no_confidence}`, **no filter** — consumers filter |
| `palmares.committee_member` | `v_committee_member` | `DISTINCT`/max-id over `epoch_state` (#2155); subsumes spec §6's `committee_vote_auth` via `committee_registration` — **say so, do not add a view** |
| `palmares.treasury_withdrawal` | `v_treasury_withdrawal` | the `reward_rest` "paid" column is labelled **`paid_heuristic`** |
| `palmares.thresholds` | `v_thresholds` | `epoch_param`, governance columns only |
| `palmares.activity` | `v_activity` | **must include the five epoch-boundary event kinds** (§2 D-19) |
| `palmares.drep_metrics` | T-DATA `mv_drep_metrics` | **the only MATERIALIZED view**, `REFRESH … CONCURRENTLY` |
| `palmares.eligible_actions(drep_hash_id bigint, as_of_epoch int)` | T-METRIC's copy-pasted `elig` CTE | **SQL function**, not a text include (§2 D-15) |

Deleted outright: `gov.v_vote_latest`, `gov.v_drep_epoch_power_all`, `metrics/_common.sql`'s
concatenation mechanism, and all five of T-API §7.4's projection tables (§2 D-1).

### 1.3 Code, files, services

| Concept | Seen as | **Canonical** |
|---|---|---|
| Repo / package / module namespace | `palmares` / `Palmares.*` (all) | **`palmares`**, **`Palmares.*`** — rename rules in T-EXTRACT §5.2 stand |
| Governance data library | new `src/lib/govdata-lib/` with `GovData.{Schema,Query,Ids,Refresh}` (T-DATA) | **no new library.** Four libraries, as T-EXTRACT §1.0 |
| CIP-129 encoder/decoder | `GovData.Ids` (T-DATA) | **`Palmares.DomainTypes.Core.Ids`** in `offchain-lib`; `bech32 ==1.1.8`, already in the freeze |
| Governance read module | `Query/Governance.hs` (T-DATA) · `Query/Gov.hs` (T-EXTRACT) | **`src/exe/query-api/Query/Gov.hs`** |
| View bootstrap | `GovData.Schema` (T-DATA) · `Query/Views.hs` (T-EXTRACT) | **`Query/Views.hs`** |
| Matview refresh | `GovData.Refresh` (T-DATA) · `deployment-palmares-projector` (T-INFRA) | **`Query/Refresh.hs`** — one thread inside query-api |
| Metric SQL runner | `Query/Metrics.hs` (T-METRIC) | **`Query/Metrics.hs`** + **`Metrics/Catalog.hs`** |
| Kupo attestation service | `chain-sync` (Belt) · `attest-sync` (T-EXTRACT) | **`attest-sync`** (:8084) |
| Haskell executables | 4 (T-EXTRACT) | **`admin`, `query-api` (8083), `interaction-api` (8082), `attest-sync` (8084)** |
| F1 container images | {query-api, interaction-api, attest-sync, admin} (T-EXTRACT) · {projector, query-api, interaction-api, bff, frontend} (T-INFRA) | **`query-api`, `interaction-api`, `attest-sync`, `admin`, `bff`, `frontend`** — **no `projector`** |
| Image registry | Docker Hub (T-EXTRACT) · GHCR (T-INFRA) | **GHCR**, `ghcr.io/<owner>/palmares-<svc>` — kills `DOCKERHUB_*` |
| Dev stack file | `docker-compose.yml` (T-EXTRACT) · `compose.yaml` (T-INFRA) | **`compose.yaml`** (profiles + `working_dir` fix) |
| Koios reconcile | `scripts/reconcile_koios.sh` (T-DATA, T-EXTRACT) · `scripts/reconcile-koios.sh` (T-INFRA) | **`scripts/reconcile_koios.sh`**, thin wrapper over **`admin reconcile-koios`** |
| Public hostname | `gov.cardano.vip` (T-INFRA) | **`gov.cardano.vip`** |
| Status endpoint | `/protocol-status` (T-DATA §5.3, Belt name) · `/status` (T-API §0.4) | **`GET /status`** — one endpoint |
| Ready-lag env var | `PALMARES_MAX_DBSYNC_LAG_SECONDS=120` (T-EXTRACT) · `READY_MAX_LAG_SECONDS=180` (T-API) | **`PALMARES_READY_MAX_LAG_SECONDS=300`** + **`PALMARES_AMBER_LAG_SECONDS=120`** (§2 D-7) |
| db-sync image | agreed by all three | **`ghcr.io/blinklabs-io/cardano-db-sync:13.7.1.0@sha256:2a341eb32f15a3e65435f4123c3186f467bf2ed9d3ea2eac449592b5a54eb950`** |
| db-sync metrics path | `/metrics` (T-INFRA first draft) | **`/`** — probe *and* ServiceMonitor (T-INFRA C2) |
| db-sync lag alert | `*_node_slot_height` | **`cardano_db_sync_node_block_height - cardano_db_sync_db_block_height`** |

### 1.4 Metric and DTO field names

T-METRIC/T-API names are canonical. T-DATA's `mv_drep_metrics` column names are **aliases to delete**:

| T-DATA column | **Canonical** |
|---|---|
| `eligible_closed_actions` | `eligible_actions` |
| `voted_closed_actions` | `votes_cast` |
| `votes_with_anchor` / `votes_resolved_rationale` / `votes_inline_rationale` | `rationale_absent` / `rationale_verified` / `rationale_inline` (five grades, §2 D-3) |
| `avg_vote_latency_seconds` | `vote_latency_median_hours`, `vote_latency_p90_hours`, `vote_latency_sample` |
| `latest_power_lovelace` / `latest_power_epoch` | `voting_power` / `voting_power_epoch` |
| `live_delegator_count` | `delegator_count` |
| `epochs_with_power` | *(cut — nothing reads it)* |

Anchor verdict enum, canonical wire values (merge of T-API §2.4 and T-DATA §6.3):
**`valid | invalid_schema | hash_mismatch | oversized | unreachable | inline | not_fetched`**,
plus a separate `cip100_valid :: Maybe Bool`. `inline` is T-DATA's find (`data:` URIs exist on live
preprod votes) and T-API/T-INFRA both missed it.

---

## 2. Contradictions, each with its resolution

### Architecture

**D-1 — Projection tables: five, or none?**
T-API §7.4 specifies five of our own projection tables (`drep_projection`, `drep_epoch_delegators`,
`gov_action_projection`, `vote_projection`, `activity_projection`). T-DATA D1, T-EXTRACT §0.2 and
T-METRIC §1.4 all say views only. T-INFRA sizes and deploys a `projector` Deployment to fill them.
**Resolution: no governance projection tables. Views + one matview.** T-API's four justifications die
individually:
- *trigram search text* → we may create indexes on `public.*` through the owner-run index job
  (T-DATA §3.4 already does), so `palmares_idx_ocvdd_given_name_trgm` /
  `palmares_idx_ocvgad_title_trgm` (GIN, `gin_trgm_ops`) answer it. Add
  `CREATE EXTENSION IF NOT EXISTS pg_trgm` to that job.
- *precomputed metrics* → that is exactly `palmares.drep_metrics`, the one matview.
- *precomputed vote latency / power-at-cast* → a join in `palmares.vote`; the "hot path" claim is
  unmeasured. Named ceiling: if `/votes` p95 > 500 ms, add the columns to the matview.
- *cursor components* → computable deterministically **in** `palmares.activity`; "stored as columns,
  not recomputed at read time" is satisfied by a view (D-19).
**Consequence: T-INFRA's `deployment-palmares-projector`, `Dockerfile.projector` and the projector
image are CUT.** The matview refresh is a thread in query-api (`Query/Refresh.hs`). One fewer image,
Deployment, probe and GitOps resource.

**D-2 — DRep eligibility: `drep_distr` or `drep_registration` intervals?**
T-DATA D7 chose `drep_distr` per-epoch membership, explicitly rejecting reconstructed certificate
intervals. T-METRIC §1.1 uses `drep_interval` from `drep_registration` plus a `drep_votable`
protocol-phase test. **Resolution: T-METRIC wins.** Three reasons, all from the notes' own measured
data: (a) `drep_distr` is an epoch-boundary snapshot, which forced T-DATA into a `UNION` with voted
pairs and still mishandles register-and-vote inside one epoch; (b) `drep_distr` has no notion of the
Conway bootstrap phase or the role×type matrix, so it inflates every pre-Plomin denominator;
(c) **101 of 294 registered preprod DReps have no `drep_distr` row at all**, so T-DATA's denominator
would be null for a third of the list. `drep_distr` keeps its two real jobs: the voting-power series,
and the Koios R3/R4 cross-check. Keep `palmares_idx_drep_distr_epoch` anyway — the power series uses it.

**D-3 — `rationale_rate` denominator: vote rows or distinct actions?**
T-DATA §3.5 divides by `count(*)` over vote rows; T-METRIC §1.2 / T-API by distinct eligible actions
voted, final vote only. **Resolution: T-METRIC.** One preprod (DRep, action) pair has **20** vote rows.
**But T-DATA's `inline` arm must be folded in**: a `data:` rationale is a real rationale and db-sync
refuses to fetch it. Canonical: five grades — `verified | inline | unverified | pending | absent` —
numerator `verified + inline`, and the five sum to `votes_cast`. (This replaces T-METRIC's four.)

**D-4 — `shelley.enable`: true or false?**
T-DATA D4, T-METRIC §0.1 and T-API §7.1 require `true`; T-INFRA §1.3 accepts `false` and publishes
delegator counts as a documented upper bound. **Resolution: `shelley: {enable: true}`.** It is not a
close call: F1 check 12 requires delegator churn and retention, three tracks' SQL reads
`stake_deregistration`, and **T-INFRA itself says the flip is a full re-sync** — so it has to be in the
config before the first sync starts or the 24–48 h is paid twice. Costs `delegation`,
`stake_registration/_deregistration`, `withdrawal`, `pool_update/_retire`.
Two corrections that ride along: `param_proposal` is populated **regardless** of this flag
(T-METRIC's fact-check), so it is **not** a valid verification probe — probe
`stake_deregistration` and `pool_update`. And T-INFRA's Postgres estimate moves from 6–10 GB to
**10–18 GB** (still well inside a 50 Gi PVC).

**D-5 — the db-sync `insert_options` block.** Four different blocks were proposed. T-API §7.1's
hand-written 11-key block is **actively wrong** (no `preset` ⇒ base is `def` ⇒ `tx_metadata` and
`reward` come back on, and `rewards` is unsettable from JSON). T-DATA restates nine preset keys
"so an upstream change cannot silently alter us" — a liability, since a restated key that drifts from
the preset is itself a silent misconfiguration, and the preset is pinned by the image digest anyway.
**Resolution — exactly this, and nothing else:**
```yaml
insert_options:
  preset: "only_governance"
  offchain_vote_data: "enable"
  shelley:
    enable: true
```
plus top-level `ipfs_gateway: ["https://ipfs.io/ipfs", "https://cloudflare-ipfs.com/ipfs"]`,
`NodeConfigFile: /node-configs/config.json`, and **no** `ledger_backend`, **no** `disable_epoch`
(does not exist at 13.7.1.0), **no** `RequiresNetworkMagic` (not a db-sync key), **no**
`remove_jsonb_from_schema` (already the default). Start from db-sync's shipped
`config/preprod-config.yaml` **at tag 13.7.1.0** (not 13.7.2.1 — that is not the image we run) so the
required logging `Representation` block is present verbatim.

**D-6 — db-sync pod topology.** T-DATA §2.5 chose a sidecar in the node's StatefulSet; T-INFRA §2.2–2.3
chose a separate Deployment reading a hostPath `ipc` + `configs`, pinned to `minirack-pi5-1`.
**Resolution: T-INFRA.** T-DATA's own fact-check demolished the sidecar rationale (it does not avoid
the node restart, it makes it recur on every db-sync change and drags `ogmios` and `hydra-node` with
it). T-INFRA also pins Postgres to the same node, which closes the cross-node write-path question
T-DATA left open. Cost: one pod restart plus a V2LSM snapshot reload (seconds to minutes, **not** the
"~26 s" figure, which was a post-conversion measurement).

**D-14 — `govdata-lib`.** T-DATA proposes a fifth library; T-EXTRACT's package shape has four.
**Resolution: four libraries.** See §1.3 for where each `GovData.*` module lands.

**D-15 — DDL mechanism: four were proposed, and two of them are mutually impossible.**
T-DATA wants Haskell DDL at startup *and* a one-shot Job for indexes *and* mentions `--schema-dir`;
T-EXTRACT wants `sql/*.sql` applied by query-api in **one transaction**. Nobody noticed that
`CREATE INDEX CONCURRENTLY` **cannot run inside a transaction block** — so T-DATA's index list and
T-EXTRACT's single-transaction bootstrap are incompatible as written. **Resolution: two mechanisms,
split by privilege and by transactionality:**
1. `sql/000_schema.sql` … `sql/030_metrics.sql` — applied by **query-api at boot** as
   `palmares_owner`, in **one transaction**, beginning
   `DROP SCHEMA IF EXISTS palmares CASCADE; CREATE SCHEMA palmares;` then plain `CREATE VIEW` /
   `CREATE MATERIALIZED VIEW` / `CREATE FUNCTION`. This also dissolves T-DATA's matview-dependency
   hazard (§3.2) for free: the matview is dropped and recreated with the schema every boot.
   **Fail-closed: `die` on failure** (Stream C doctrine; closes T-EXTRACT's open item).
2. `sql/040_indexes.sql` — applied by a **one-shot k8s Job** (`admin apply-indexes`, image
   `palmares-admin`) as the `public` table owner, **outside** any transaction, all
   `CONCURRENTLY IF NOT EXISTS`, all `palmares_idx_*`, preceded by
   `CREATE EXTENSION IF NOT EXISTS pg_trgm`. **Do not also use `--schema-dir`.**
`palmares_attest` is created by **attest-sync** immediately before `runMigrations`, with `search_path`
set — never by query-api, or `persistent` migrates into db-sync's `public`.

**D-26 — where the k8s manifests live.** T-INFRA's `ci.yml` validates `deploy/k8s/{base,preprod}` in
the new repo, while all its manifests are written for `apps-k8s-configs`. **Resolution: manifests live
only in `apps-k8s-configs`** (Flux reads nothing else); the new repo ships `compose.yaml` and no
`deploy/k8s/`. The `manifests` CI job is therefore **cut** from the new repo — `apps-k8s-configs`'s own
`validate.yaml` already runs kustomize + kubeconform + the plaintext-Secret guard.
Flux roots, matching the repo's own convention (verified on disk):
`clusters/minirack/chains/cardano-preprod-dbsync.yaml` (chain infra, sibling of `cardano-preprod.yaml`)
and `clusters/minirack/palmares/palmares-preprod.yaml` (app). Manifest dirs
`chains/cardano/preprod-dbsync/` and `apps/palmares/{base,preprod}/`.

### Data and correctness

**D-10 — anchor verdict: SQL `CASE` or Haskell classifier?** T-DATA §6.3 and T-API §7.3 put it in one
SQL `CASE`; T-INFRA §1.4.1 specifies a ~30-line Haskell `Palmares.Anchor.classifyFetchError`.
**Resolution: the SQL `CASE`, in `palmares.anchor`.** It is one place, it is already joined by every
reader, and a Haskell classifier would require shipping raw `fetch_error` text to Haskell on every row.
The golden test (`UnitTests/Anchor.hs`, seeded rows) is **mandatory**, pinning the exact strings both
tracks sourced: `LIKE 'Hash mismatch%'` (the only arm without the `"Error Offchain Voting Anchor: "`
prefix) and `LIKE '%Size error, fetching metadata from%'`. Default arm is `unreachable` — never a
false accusation. All reads of `off_chain_vote_{author,reference,external_update}` carry `DISTINCT`
(upstream PR #2137), with a `ponytail:` comment naming the version that removes it.

**D-19 — the activity feed cannot produce five of its own event types.** T-API §1.6/§2.1 defines 18
event types including `gov_action_ratified/enacted/expired/dropped` and
`treasury_withdrawal_enacted`, with a corrected cursor (`sort_slot` = first slot of the epoch,
`tx_index = -1`, `item_index = gov_action_proposal.id`) and nullable `ChainPoint.slot`/`.tx_hash`.
T-DATA's `v_activity` is a `UNION ALL` of five **tx-borne** kinds only and can produce none of them.
**Resolution: `palmares.activity` must `UNION ALL` the five epoch-boundary kinds**, with
`time = block.time of MIN(block.id) WHERE epoch_no = e` and T-API's cursor rule. Non-negotiable:
without it the cursor is not total and the feed can skip or repeat.

> **D-19 CORRECTION (2026-10-19, from `f1-yaci-store-evaluation.md` §4.1 C1 — supersedes the event list
> above).** `dropped_epoch` is **not** an independent fourth outcome. db-sync's `doc/schema.md` line 910 is
> stale per its own maintainer (issue #1932); CHANGELOG 13.6.0.1: *"Enacted gov_action_proposal are no
> longer marked as dropped"*. On enactment it stays NULL; on **expiry it is set to `expired_epoch + 1`**.
> Our own measured preprod data agrees (`f1-dbsync-data-layer.md` line 866: 102 expired actions **all**
> carry `dropped_epoch`, plus **6 dropped-only** rows).
> Therefore: emit `gov_action_dropped` **only** for the genuine pruned-sibling case —
> `dropped_epoch IS NOT NULL AND ratified_epoch IS NULL AND enacted_epoch IS NULL AND expired_epoch IS NULL`
> (6 rows on preprod today). Emitting it for expired actions duplicates the expiry event one epoch later,
> which is a feed defect, not a feature. The outcome enum exposed in the API is
> `ratified | enacted | expired | dropped | open`, with `dropped` carrying that same restricted predicate.
> **No SQL change to `close_epoch`**: `LEAST(ratified, expired, dropped)` still picks the smaller
> `expired_epoch`, so it remains correct. The five epoch-boundary kinds stay five — `gov_action_dropped`
> narrows its predicate, it is not removed.

**D-24 — CC votes are cast by hot keys.** T-DATA's `v_vote` resolves the voter to one
`(role, credential_raw, has_script)` triple, which for `committee_voter` is the **hot** key; T-API §1.3
proved `committee_member.committee_hash_id` is the **cold** credential and that a member's history
splits across rotations. **Resolution: `palmares.vote` carries both `voter_hot_hash` and
`voter_cold_hash`**, the latter resolved by `LATERAL` over `committee_registration (cold_key_id,
hot_key_id)` as of the vote's tx. `VoteRecord.voter_id` for CC is the cold CIP-129 `cc_cold1…`.
`committee_hash` has no `view` column, so all four `cc_hot`/`cc_cold` bech32 forms are computed by us
in `Palmares.DomainTypes.Core.Ids`.

**D-20 — pseudo-DReps in the Koios parity rows.** T-DATA corrected R3/R4 to a matched actor set via a
second view; T-METRIC excludes them from every denominator but requires them in the dashboard's
abstain figure. **Resolution: one view, `palmares.drep_epoch_power`, with `target_kind` and no
filter.** `/dreps` and every rate filter `target_kind = 'drep'`; the dashboard reads
`always_abstain_lovelace` / `always_no_confidence_lovelace` off the same view; R4 compares
`target_kind IN (…)` totals against Koios's unfiltered sum. `drep_always_abstain` is 46 % of the
epoch-319 total — getting this wrong fails R4 by 46 % on day one.

**D-25 — the (role, gov-action-type) → threshold column table.** T-API §2.6 authored it, including
`pvtpp_security_group` (which a `pvt_%` prefix scan misses) and `threshold: null` where no threshold
exists; T-DATA's `v_thresholds` is just `epoch_param` restricted; T-METRIC §3.3 owns the SecurityGroup
`param_proposal` test. All three are compatible. **Resolution: T-API's static table is a named
deliverable with its own test**, consumed by `palmares.vote_tally`; `_normal` is always chosen for
committee thresholds in F1 (because `epoch_state.no_confidence_id` is documented "remains NULL"), and
the DTO echoes `threshold_param`.

**D-17 — `age_days` nullability.** T-API types it `Maybe Int` on `DRepSummary` and `Int` on
`DRepMetrics` — internally inconsistent. T-METRIC defines it as a sum of registered intervals.
**Resolution: `Maybe Int` in both**; null exactly when `registered_at` is null (reserved DReps).

**D-18 — `/activity` total.** T-API sets `Page.total = -1` as a magic value.
**Resolution: `pageTotal :: Maybe Int`**, `null` on `/activity`. Same cost, no magic number.

### Process and infrastructure

**D-7 — `/ready` lag thresholds: 120 vs 180 vs 300 s.** **Resolution: `/ready` red at 300 s** (T-DATA's
corrected figure, from the real ~22.7 s mean block interval), **UI amber at 120 s**, and spec check A1
("< 120 s sustained") is asserted from `/status`'s `lag_seconds` over a soak window — not from the
probe. Two env vars, canonical names in §1.3. Also `/ready` keeps T-DATA's checks 3 and 4
(`drep_distr_epoch >= epoch_no - 1`, `meta.network_name` match) — check 3 is the cheap guard against
the one misconfiguration that silently empties `drep_distr`.

**D-8 — CI runner and shape.** T-EXTRACT: `ubuntu-latest` + `docker run palmares-builder`, Docker Hub,
`images.yml` on `self-hosted`. T-INFRA: `ubuntu-24.04-arm` + `haskell-actions/setup@v2`, GHCR, no
self-hosted runner. **Resolution: T-INFRA, with T-EXTRACT's shape as the named fallback.** The cluster
is arm64-only, so the images must be arm64; T-EXTRACT's `images.yml` builds on `self-hosted` with no
`--platform` at all, which is the bug T-INFRA's native-arm64 runner fixes. T-EXTRACT's finding still
governs the fallback: **never `container: benz0li/ghc-musl`** (musl + glibc `node20` ⇒ JS actions fail,
and on arm64 the runner refuses outright) — the fallback is `docker run` from the host.
Three workflows: `ci.yml` (blocks), `images.yml` (post-merge, GHCR, `platforms: linux/arm64`),
`reconcile.yml` (nightly, warns only). **No self-hosted runner anywhere.**

**D-9 — `/metrics/formulas` vs "don't extract `Query/Rules.hs`".** T-EXTRACT §0.7 cuts the formulas
endpoint in favour of a static `docs/metrics.md`; T-API §1.1 and T-METRIC §7 make a `file-embed`ed
catalog the single source of truth with the **SQL itself** inside it. **Resolution: T-METRIC/T-API.**
T-EXTRACT's objection was to `Query/Rules.hs` (hand-written Haskell formula prose), which stays
skipped. The catalog is one embedded file plus one handler, and it inverts the drift risk:
`docs/metrics.md` becomes **generated** by `admin write-metrics-doc`, CI failing if it differs.
Mechanics as T-METRIC corrected them: `makeRelativeToProject … >>= embedFile`, **one `embedFile` per
`.sql`** (never `embedDir`), and `extra-source-files: resources/metrics/*.sql` +
`resources/metric-formulas.json`.

**D-11 — CSV.** T-API adds `cassava`, `Palmares.WebAPI.CSV` (with the formula-injection guard) and
`Header "Accept" Text` on nine endpoints; T-EXTRACT's `palmares.cabal` has neither.
**Resolution: accept T-API** — quoting and injection are a trust boundary, not a laziness target.
Add `cassava` to `webapi-lib`'s `build-depends` and `Palmares.WebAPI.CSV` to its `exposed-modules`
(webapi-lib still has no project-library deps). CSV **not** on `/drep/{id}/power`.

**D-12 — "the cluster has no preprod node".** T-API §8.3b asserts `apps-k8s-configs` has no preprod
`cardano-node` and no db-sync workload. **It is wrong about the node** (verified:
`chains/cardano/preprod/workloads/statefulset-cardano-preprod.yaml`; T-API looked only under `apps/`).
It is right that there is no db-sync workload and that the only Postgres is a 2 Gi PVC.
**Consequences: the Mithril bootstrap task is OFF the F1 critical path** (T-DATA §9 and T-INFRA 0.1
agree), and the provisioning work is db-sync + its own Postgres, nothing more.

**D-13 — Postgres sizing and PVC.** T-INFRA 50 Gi / 6–10 GB; T-DATA 100 Gi; T-API calls it a BLOCKER
whose fallback deletes `anchor.status` from four DTOs. **Resolution: 50 Gi** (T-DATA's 100 Gi was
copied from the node's PVC), estimate **10–18 GB** with D-4's flip, and it is a **measurement task,
not a blocker**. T-API's fallback is rejected: dropping F1 check 7 is the most expensive possible
response. The ordered fallbacks are (1) `DELETE FROM epoch_stake WHERE epoch_no < tip-2` as a CronJob
*after* the `validateEpochStake` test exists, (2) `ledger_backend: lsm` accepting its three known bugs,
(3) db-sync off-cluster. Note `local-path` has `allowVolumeExpansion: false` — **size it once**.

**D-22 — the spec contradicts itself and every track.** Spec §9 F0's exit criterion still reads "un ADR
care fixează sursa de date (**Ogmios follower**)" and §9 F1's GOAL still reads "indexer de guvernanță
(**follower Ogmios** → raw log → proiecții Postgres)". All five tracks kill the follower.
**Resolution: editing spec §9 F0 and §9 F1 is a plan task**, done before the task list is written, or
the plan will contain a service nobody decided to build.

**D-23 — F1 checks 3 and 4 were written for a projection service.** With governance as views they hold
by construction. **Resolution: restate them in the plan** — check 3 = run the catalog SQL twice against
the same db-sync and hash the output; check 4 = db-sync's own rollback plus the existing attestation
replay test. **Do not build a determinism harness for a `SELECT`.**

---

## 3. Ordering and dependencies

Hard constraints the task order must respect. `→` means "must complete before".

**Phase 0 — decisions and the two early experiments (day 1)**

1. **Owner decisions, all five at once**: project name · `shelley: enable: true` accepted (D-4) ·
   db-sync as a separate Deployment on a hostPath socket, accepting one node restart (D-6) ·
   our schemas co-located in db-sync's `cexplorer` with role separation (D-15) · db-sync pinned at
   13.7.1.0 (blinklabs, arm64). → everything.
2. **Experiment A (the riskiest unknown, §5)** — start it on day 1, off-cluster, in the background.
3. **Experiment B (15 min)** — `ubuntu-24.04-arm` + `haskell-actions/setup@v2` + GHC 9.6.6.
   → decides `ci.yml`'s shape; if it fails, the fallback needs a published builder image, which is a
   day of work nobody should discover in week 3.
4. Edit spec §9 (D-22) and restate checks 3/4 (D-23). → the task list itself.

**Phase 1 — repo (parallel with Experiment A)**

5. Bootstrap: Apache-2.0 + `NOTICE` + `licenses/` + pins (`cabal.project` verbatim, no bumps) +
   `Dockerfile.base` + all **five** `CLAUDE.md` files, with `onchain-lib`'s edited to delete the
   `checkFee` selector contract that fix #1 removes.
6. Extraction, in this commit order: **as-is → rename → strip → GREEN GATE (`cabal build all`) →
   rewrites.** The gate is after **strip**, not after rename: 10+ kept files import skipped modules,
   so the strip commit must also remove those imports and the declarations using them.
7. `Palmares.DomainTypes.Core.Ids` (CIP-129) + its golden test. → every DTO carrying an id, and
   → the Koios reconcile (which joins on CIP-129 ids).

**Phase 2 — the substrate (strictly sequential; this is the critical path)**

8. hostPath edit to `statefulset-cardano-preprod.yaml` + `cardano.io/node-config-stamp` bump
   → 9.
9. Postgres 17 Deployment + **50 Gi** PVC, pinned to `minirack-pi5-1` → 10.
10. db-sync Deployment: `NETWORK: ""` (present and empty), `workingDir: /var/lib/cexplorer`,
    probe and ServiceMonitor on `path: /`, config per D-5 **including `shelley: enable: true`**
    → 11. *(A shelley flip after this point costs a full re-sync.)*
11. **From-genesis preprod sync, measured.** This *is* F1 check 1. → 12, 13, and every parity row.
    **Why from genesis is a CORRECTNESS requirement, not just a completeness one** (added 2026-10-19 from
    `f1-yaci-store-evaluation.md` §4.1 C2): db-sync issue #1975 (open since 2025-05) records two *identical*
    mainnet instances disagreeing on one DRep's epoch-538 voting power by ~740×, and the 13.6.0.4 Known
    Issue names **preprod** explicitly — pre-upgrade `drep_distribution` values "are not well defined and
    could be slightly different between different executions". The documented remedy is to delete the
    ledger-state files and let the ledger rules recompute, which is exactly what a from-genesis sync on
    13.7.1.0 does. A snapshot-restored or upgraded db-sync would inherit ill-defined values in the one
    column the whole product rests on.
12. `sql/040_indexes.sql` via the admin Job (`CONCURRENTLY`, `pg_trgm`). **Cannot usefully run before
    11**: db-sync's own stage-4 indexes do not exist until it is within 30 min of tip, so any query
    benchmarked earlier is measuring the wrong plan. → any performance claim.
13. Measure `pg_total_relation_size` per table + peak RSS → ADR `0001-governance-data-substrate.md`.

**Phase 3 — the read path**

14. Roles + grants (`palmares_owner`, `palmares_ro`) → 15.
15. `sql/000`–`sql/020` (schema, views, `palmares.eligible_actions`) → 16.
16. `sql/030_metrics.sql` + `resources/metrics/*.sql` + `resources/metric-formulas.json` +
    `Metrics/Catalog.hs` → 17 and → the DTOs' `formula_ids`.
17. The epoch-320 fixture test (`test/fixtures/preprod-epoch-320-metrics.json`), **with the cursor
    block pinned by hash + slot + `block.time`** — `age_days` moves inside an epoch, which is how the
    original `1463` went unnoticed.
18. DTOs + `Query/Gov.hs` + `RestAPI.hs` + `Query/Views.hs` + `Query/Refresh.hs`.
19. **`query-api --write-swagger-only`** — a real new task, not a house pattern (the Belt System writes
    Swagger unconditionally at startup and then serves forever). → the Swagger CI guard and
    → the frontend's `gen:api`.
20. Frontend, generated from `docs/generated/swagger/query-swagger-api.json`.

**Phase 4 — attestations (parallel with Phase 3 after step 6)**

21. On-chain fixes #1 (`feeFor`), #2 (`OracleSetAdmin` at index 4), #4
    (`checkTimestampInFiniteRange`) → validators → `write-blueprint` → `deploy-reference-scripts`
    → commit `config/config_validators.preprod.json` **in the same commit as the redeploy**.
22. attest-sync (creates `palmares_attest`, sets `search_path`, migrates) + fix #3
    (`authenticateStateUTxO`, **one** site: `Ingestion.hs`) + fix #5 (slot→time on read).
23. `scripts/smoke_attest_flow.sh` → F1 check 11.
24. **Script-DRep claim design.** Blocks `MintClaimProfile` and therefore F1 check 11. **Must be
    settled in the plan**, not discovered at implementation time — it is the one genuinely
    unspecified piece of on-chain design left.

**Phase 5 — parity (last)**

25. `admin reconcile-koios` + `scripts/reconcile_koios.sh` + `reconcile-exceptions.yaml`. Needs 11,
    12, 15, 16, 18 and a caught-up sync; R10's freshness guard aborts the run otherwise.

---

## 4. Cut list

**Cut from F1 outright**

| Cut | Where it came from | Why |
|---|---|---|
| `deployment-palmares-projector`, `Dockerfile.projector`, the projector image | T-INFRA §1.2, §5.2 | no projection service exists (D-1) |
| The five governance projection tables | T-API §7.4 | views + 1 matview + 2 trigram indexes (D-1) |
| `govdata-lib` | T-DATA §3.6 | four libraries (D-14) |
| `gov.v_vote_latest` as a separate view | T-DATA §3.3 | `palmares.vote WHERE seq_last = 1` |
| `gov.v_drep_epoch_power_all` | T-DATA §8.2 R4 | one view with `target_kind` (D-20) |
| `metrics/_common.sql` + its concatenation loader | T-METRIC §0.3, §7 | views + one SQL function (D-15) |
| `Palmares.Anchor.classifyFetchError` | T-INFRA §1.4.1 | one SQL `CASE` (D-10) |
| T-DATA's nine restated `insert_options` keys · `disable_epoch` · `RequiresNetworkMagic` · T-API's whole hand-written block | T-DATA §2.2, T-EXTRACT §1.5, T-API §7.1 | D-5 |
| Docker Hub, `DOCKERHUB_*` secrets, `runs-on: self-hosted` | T-EXTRACT §6.3 | GHCR + arm64 hosted (D-8) |
| The `manifests` CI job in the new repo, `deploy/k8s/**` | T-INFRA §5.1 | `apps-k8s-configs` already validates (D-26) |
| `/protocol-status` as its own endpoint | T-DATA §5.3 | folded into `/status` (D-9 naming) |
| `EpochPowerPoint.rank` | T-API §2.5 | a window function over all of `drep_distr` per epoch, on a Pi, that no F1 check asks for |
| `q` on `/spos` · `/treasury-withdrawal/{id}` · `/committees` · per-actor vote endpoints · `/export/*` · `ETag` · `openapi3` migration | T-API §0, §1.5 | already cut by T-API; restated so they do not reappear |
| `LinkedList.hs`, `MembershipsValidator.hs`, `Protocol/Core.hs`, `Onchain/BJJ.hs`, `RanksValidator.hs`, `Query/Live.hs`, `Query/Rules.hs`, `Query/ExplorerMonthly.hs`, `RestAPI/Common.hs`, `IPFS.hs`, `WebAPI/ImageType.hs`, `SafeOnchainLogic.hs`, the `multiset` and `servant-multipart` deps, the `kubo` container | T-EXTRACT §0 | already cut; restated |
| `getAllParsedDatumsAtValidator` + its four wrappers | T-EXTRACT §4.3 | deleted with `Query/Live.hs`; fix #3 has **one** site |
| A determinism harness for F1 check 3 | implied by spec §9 | views are deterministic by construction (D-23) |
| Our own metadata fetcher, the epoch-snapshot scheme, the mini-Ogmios follower | spec §4.1 | all five tracks agree; the follower verdict is "never", on laziness grounds |

**Deferred (named, with the trigger)**

| Deferred | Trigger to revisit |
|---|---|
| `mcp-server-lib` (18 modules) + `mcp-server` | **F3**, per spec §9 F1 non-goals |
| Mithril bootstrap Job | only to rebuild the node, or for a contributor's local stack (§3 of T-INFRA) |
| `palmares.drep_epoch_delegators` as a **matview** | `/drep/{id}` or `/drep/{id}/power` p95 > 500 ms on preprod |
| Latency/power columns on `palmares.drep_metrics` | `/votes` p95 > 500 ms |
| `DELETE FROM epoch_stake` CronJob | only after the `validateEpochStake` test exists, and only if disk is tight |
| `ledger_backend: lsm` | only if measured peak RSS does not fit pi5-1, accepting the three known Pi bugs |
| A supplementary anchor re-fetcher | > ~20 % of anchors at `not_fetched` 48 h after the sync completes — **measure in F1, build in F2** |
| `offchain_pool_data: enable` (SPO tickers) | F2, if the SPO pages feel bare |
| Atlas local-node provider replacing Maestro | the spike in ADR `0002-write-path-provider.md`; Maestro stays for F1's write path, and **Atlas has no db-sync provider** — delete that hope from the plan |
| **Yaci-Store as an optional second derivation (cross-check), never as the substrate** | triggers T6/T7 in `f1-yaci-store-evaluation.md` §8. **Rejected as substrate** (evaluated 2026-10-19): its `drep_dist` depends on the UTxO store via account→stake-address-balance→snapshot, so it must index the whole UTxO set to produce per-epoch DRep power — *heavier* than db-sync's `only_governance`, which gets it from the Haskell ledger replay with `tx_out` disabled; `GovActionStatus` has only `{ACTIVE, RATIFIED, EXPIRED}` (no ENACTED/DROPPED, open issue #1194); it stores `anchor_url`/`anchor_hash` only, with **no off-chain fetching or hash verification**, so our deleted fetcher would come back; and its per-epoch range partitioning makes one unbounded `drep_dist` query need ~3,160 locks (open issue #1200) while its baseline migration opens with `drop table if exists drep_dist cascade`, which would silently drop our views. **The "not worth the language hop for a Haskell shop" argument is deleted — it runs backwards** (we consume either substrate as an unmodified container over SQL; Spring Boot logs + `jcmd` are easier to debug than katip + GHC RTS flags). Also: Yaci-Store is **not** a valid fallback in the D-13 disk chain, being structurally heavier; the honest fallback for "does not fit the Pi" is the x86_64 box already budgeted for F2. |
| Dependency bumps (Atlas, devx, db-sync 13.7.2.1 arm64) | post-F0 workstream; **no pin moves during extraction** |

---

## 5. The riskiest unknown, and the cheapest experiment

**The risk.** Not the Pi's RAM. It is: *does db-sync 13.7.1.0 with `preset: only_governance` +
`shelley: enable: true` + `offchain_vote_data: enable` + `ledger: enable` complete a from-genesis
preprod sync at all — and in what wall-clock time, at what on-disk size, at what peak RSS?*

Why it dominates everything else:

- **No published figure exists** for any preset on any architecture. T-INFRA's 24–48 h is a derived
  band; T-DATA and T-EXTRACT both list the size as "could not determine". Upstream documents RAM only
  for *mainnet* (`~16 GB` InMemory / `~2–3 GB` LSM).
- **Nobody has run blinklabs' arm64 build of db-sync with these options on a Pi-class node**, next to
  a cardano-node that already requests 3 Gi and limits 8 Gi, plus ogmios and hydra-node.
- **The config is frozen by the sync.** D-4's `shelley` flip after the fact is a full re-sync. So is
  a `preset` correction. The first sync is effectively one shot.
- **Everything is downstream.** Steps 12–13 and 25 of §3 are blocked on it; four of the five tracks'
  SQL cannot be benchmarked (db-sync's stage-4 indexes do not exist mid-sync); F1 checks 1–7 and 12
  cannot be signed. If it does not fit, the spec's §4.1 substrate decision reopens — and that is the
  decision the whole design rests on.

**Experiment A — the cheapest thing that settles it, and it starts on day 1.**

Do **not** start on the Pi. Run the **identical image, config and volume shape** from `compose.yaml` on
the owner's Apple-silicon Mac mini (or any 4-core/12 GB box with 120 GB free), pointed at a preprod
node — the cluster already runs **two** preprod nodes, so one can serve the socket, or compose can
bring up its own. Let it run 48 h unattended and record:

| Measurement | Why it transfers |
|---|---|
| wall-clock to tip, and the per-epoch timings from db-sync's own log | bounds the Pi by the single-core ratio (~3–5×) — turns a 2× guess into a 1.3× one |
| `pg_total_relation_size` per table, top 20 | **hardware-independent** — this is the PVC answer, exactly |
| peak RSS of the db-sync process | the only figure that needs the arm64 Pi re-run, and it is now a confirmation |
| `count(*)` on `stake_deregistration`, `pool_update`, `drep_distr`, `off_chain_vote_data`, `off_chain_vote_fetch_error` | proves the `shelley` flip took, proves `drep_distr` is populated for every Conway epoch (F1 check 1), and gives the anchor-coverage number T-API §8.5 defers |

Cost: one evening of setup, **zero cluster churn**, and it runs in parallel with the entire extraction
track. The Pi run afterwards is a confirmation, not a discovery — and if the numbers are bad, the
substrate decision reopens in week 1 instead of week 6.

**Experiment B — 15 minutes, same day.** `ubuntu-24.04-arm` + `haskell-actions/setup@v2` +
`ghc-version: 9.6.6` → `ghc --version`. ghcup publishes an aarch64 9.6.6 bindist, so it should work,
but the account's only proven Haskell CI run is x86_64. The answer decides `ci.yml`'s whole shape, and
the fallback (publish `palmares-builder:9.6.6` to GHCR, `docker run` it) is a day of work best not
discovered late.

---

## 6. Readiness verdict per track

| Track | Verdict | What is still open |
|---|---|---|
| **T-DATA** | **Ready**, after D-1/D-2/D-3/D-5/D-6/D-14/D-15/D-20 are applied | Nothing blocking. Its `v_*` naming, `drep_distr` eligibility, row-based rationale denominator, nine restated config keys, fifth library and sidecar topology are all superseded here. The `CONCURRENTLY`-in-a-transaction collision (D-15) was its only unflagged defect. |
| **T-EXTRACT** | **Ready** | Its CI shape yields to T-INFRA's (D-8) and its `/metrics/formulas` cut is reversed (D-9); add `cassava` + `Palmares.WebAPI.CSV` (D-11) and the `palmares.cabal` modules in §1.3. Sizing for F0 is **25 rewrite files / 10,614 lines**. Open: the script-DRep claim (§3 step 24) — its own blocker, and the only one that needs on-chain design. |
| **T-METRIC** | **Ready**, and it wins every formula collision | Its five-grade rationale split needs T-DATA's `inline` arm added (D-3); `_common.sql` becomes `palmares.eligible_actions` (D-15). Open and owner-facing: `W = 10` epochs for retention; `InfoAction` in the participation denominator (recommend keeping both as specified). Still unmeasured: the `epoch_stake ↔ drep_distr` reconciliation tolerance — Experiment A answers it. |
| **T-API** | **Ready with the most edits** | `/metrics/formulas`, the nullable `ChainPoint`, the cold↔hot CC resolution, the threshold table, the restored `oversized` verdict and the `Header "Accept"` CSV mechanism all stand. Rejected: the five projection tables (D-1), the hand-written `insert_options` (D-5), the 180 s `/ready` (D-7), `Page.total = -1` (D-18), and §8.3b's "no preprod node" (D-12). `--write-swagger-only` is a real task (§3 step 19). |
| **T-INFRA** | **Ready**, and it wins every infrastructure collision | Its preset-over-hand-written config, `path: /` metrics, block-height alert, `workingDir`, arm64 CI and GHCR decisions all stand. Must change: add `shelley: enable: true` (D-4) · drop the projector (D-1) · drop the `manifests` CI job and `deploy/k8s/**` (D-26) · estimate 10–18 GB, PVC stays 50 Gi (D-13) · schema `palmares`, not `registry`. Open: re-read `minirack-m4`'s live taint; whether db-sync tolerates the node config's `LedgerDB: V2LSM` block (5-minute test, fallback already written). |

**Overall: ready for planning**, with the five Phase-0 owner decisions taken and Experiments A and B
started on day 1. No track is blocked on research. The two genuine unknowns left are both measurements
(Experiment A) and one on-chain design question (the script-DRep claim), and all three are schedulable
inside F1 rather than ahead of it.
