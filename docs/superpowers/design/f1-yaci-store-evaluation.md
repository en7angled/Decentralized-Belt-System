# F1 — Yaci-Store vs cardano-db-sync as the governance-data substrate

Re-examination of the §4.1 substrate decision, requested by the owner. Date 2026-10-19.
Every claim below is either quoted from a primary source with its URL, or explicitly marked
**unverified** / **no published figure**. No numbers are invented.

Versions examined: **Yaci-Store 2.0.7** (stable, released 2026-10-11) and **3.0.0-beta5**; schema read
from the Flyway DDL at tag `v2.0.7`. **cardano-db-sync 13.7.1.0** (the pinned image) and 13.7.2.1.

---

## 1. Verdict

**Keep cardano-db-sync. Demote Yaci-Store from "solid plan B" to "rejected as the substrate, retained
as an optional cross-check".** This is a stronger rejection than the synthesis currently records, and
it is *not* for the reason the synthesis gives.

The current justification — *"not worth the language hop for a Haskell shop"* — is the **weakest**
argument available and should be deleted. We consume the substrate over SQL, as a pinned container we
never build. Java costs us almost nothing (§6). Two much harder facts decide it:

1. **The modular-stores argument, which was the only thing that could have overturned this, fails on
   inspection.** Per-epoch DRep voting power in Yaci-Store is produced by the `ledger-state` profile,
   whose own shipped config turns on the Account aggregate and the default RDBMS UTxO storage, and
   whose stake snapshot reads `stake_address_balance` — i.e. **you must index the UTxO set to get
   `drep_dist`**. db-sync's `only_governance` + `ledger: enable` gets `drep_distr` from the Haskell
   ledger replay with `tx_out` *disabled*. The advertised modularity does not reach the one table we
   care most about. Yaci-Store is therefore the **heavier** substrate for our workload, not the
   lighter one. (§2 R2, §3)

2. **Two requirements are verifiably absent, not merely unverified.** Yaci-Store has no off-chain
   anchor fetch/verify/failure-record subsystem at all (only `anchor_url` + `anchor_hash`, the raw
   on-chain values), and its proposal lifecycle enum is `{ACTIVE, RATIFIED, EXPIRED}` — no `ENACTED`,
   no `DROPPED`. Switching would rebuild the fetcher we deliberately deleted *and* lose two of the
   four lifecycle states the spec names. (§2 R5, R9)

What Yaci-Store does **not** do is resolve our riskiest unknown. There is no published from-genesis
preprod figure for it either — wall-clock, disk, or RSS — and the only resource datapoint that exists
is a mainnet instance on a **31 GB RAM host**. Switching trades a measured-in-one-week unknown for the
same unknown plus two rebuilt components. (§3)

**Where Yaci-Store genuinely wins, stated plainly:** multi-arch arm64 images are published as a matter
of course (so is blinklabs' db-sync, so this is parity, not an edge); its governance schema has been
*byte-stable* across 2.x (one migration file, unchanged between `v2.0.7` and `main`) where db-sync
ships 80 migration files; and it publishes per-epoch, per-DRep mismatch reports against db-sync, which
is more public correctness accounting than db-sync does about itself. That last point is the basis of
the hybrid option in §7 — and the hybrid is worth more than the substrate swap.

---

## 2. Requirement-by-requirement

Legend: **verified** = read in a primary source (DDL, enum, source file, release note);
**unverified** = plausible but not confirmed; **absent** = confirmed not to exist.

| # | What Palmares needs | cardano-db-sync 13.7.x | Yaci-Store 2.0.7 |
|---|---|---|---|
| R1 | DRep register / update / retire certs with anchors | **verified** — `drep_registration(deposit, voting_anchor_id, drep_hash_id)`, one table, sign of `deposit` discriminates ([schema.md](https://github.com/IntersectMBO/cardano-db-sync/blob/13.7.2.1/doc/schema.md)) | **verified** — `drep_registration(type, deposit, drep_hash, drep_id, anchor_url, anchor_hash, epoch, slot, block, block_time)` ([`V0_1100_1__init.sql`](https://github.com/bloxbean/yaci-store/blob/v2.0.7/stores/governance/src/main/resources/db/store/postgresql/V0_1100_1__init.sql)). Carries an explicit `type` instead of deposit-sign inference — mildly nicer. Open gap: DRep updates omit `deposit` ([#1072](https://github.com/bloxbean/yaci-store/issues/1072)) |
| R2 | **Per-epoch DRep voting power, every past Conway epoch, from genesis** | **verified** — `drep_distr(hash_id, amount, epoch_no, active_until)`, requires `ledger: enable`, no `tx_out` needed | **verified that the table exists and is historical**: `drep_dist(drep_hash, drep_type, drep_id, amount, epoch, active_until, expiry, update_datetime)`, PK `(drep_hash, drep_type, epoch)`, `partition by range (epoch)` ([`V0_1400_1__init.sql`](https://github.com/bloxbean/yaci-store/blob/v2.0.7/aggregates/governance-aggr/src/main/resources/db/store/postgresql/V0_1400_1__init.sql)); mismatch reports cover **mainnet epochs 507–610**, proving historical derivation ([report](https://store.yaci.xyz/docs/v2/ledger-state-mismatches/2-0-0/mainnet/drep-distribution-detail)). **But it costs the UTxO set** — see §3. Also needs from-genesis: mid-chain start via `store.cardano.sync-start-slot` is supported but yields no history ([tutorial](https://store.yaci.xyz/docs/v2/tutorials/governance-watch)) |
| R3 | Vote delegations incl. `always_abstain` / `always_no_confidence` | **verified** — `delegation_vote` → `drep_hash` rows with `raw IS NULL`, `view IN ('drep_always_abstain','drep_always_no_confidence')` | **verified** — `delegation_vote(address, drep_hash, drep_id, drep_type, credential, cred_type, …)`; `drep_type` carries the pseudo-targets, and the parity script compares `ABSTAIN` / `NO_CONFIDENCE` aggregates ([`scripts/compare/README.md`](https://github.com/bloxbean/yaci-store/blob/main/scripts/compare/README.md)). Bonus: `address` is already bech32. Open: stale delegations survive reward-account deregistration ([#1148](https://github.com/bloxbean/yaci-store/issues/1148)) |
| R4 | All votes, three roles; CC hot→cold resolvable | **verified** — `voting_procedure(voter_role, committee_voter, drep_voter, pool_voter, vote)` + `committee_registration(cold_key_id, hot_key_id)` | **verified** — `voting_procedure(tx_hash, idx, tx_index, voter_type, voter_hash, gov_action_tx_hash, gov_action_index, vote, anchor_url, anchor_hash, …)` + `committee_registration(cold_key, hot_key, cred_type)`. One voter column instead of three, discriminated by `voter_type` — simpler. Known defect: `getVotesBySPO`/`getVotesByCommittee` return duplicate rows per (voter, proposal) because the `max(SLOT)` join was not migrated to a ranked query ([#1069](https://github.com/bloxbean/yaci-store/issues/1069)) — our own `seq_last` ranking would sidestep it |
| R5 | **Gov actions with lifecycle epochs: ratified / enacted / expired / dropped** | **verified, with a correction** — four columns `ratified_epoch, enacted_epoch, dropped_epoch, expired_epoch`, `UPDATE`d at epoch boundaries. See §4.1: `dropped_epoch` is **not** an independent outcome | **ENACTED and DROPPED absent.** `public enum GovActionStatus { ACTIVE, RATIFIED, EXPIRED }` ([`GovActionStatus.java` @v2.0.7](https://github.com/bloxbean/yaci-store/blob/v2.0.7/components/common/src/main/java/com/bloxbean/cardano/yaci/store/common/domain/GovActionStatus.java)). `gov_action_proposal_status(gov_action_tx_hash, gov_action_index, type, status, voting_stats jsonb, epoch)` gives a per-epoch status row, so first-ratified epoch is derivable — but enactment is not represented, and dropped proposals **show as ACTIVE** ([#1194](https://github.com/bloxbean/yaci-store/issues/1194), open 2026-09-29). Their own parity script compares only `ACTIVE/RATIFIED/EXPIRED`, "derived from DB Sync epoch columns" |
| R6 | Ratification thresholds per epoch | **verified** — `epoch_param` with the 10 `dvt_*`, 5 `pvt_*`, `pvtpp_security_group`, `committee_min_size`, `gov_action_lifetime`, `drep_activity` as **typed columns** | **present but untyped** — `epoch_param(epoch, params jsonb, cost_model_hash, slot, block, block_time)` ([`V0_500_1__init.sql`](https://github.com/bloxbean/yaci-store/blob/v2.0.7/stores/epoch/src/main/resources/db/store/postgresql/V0_500_1__init.sql)). One `jsonb` blob. **Our D-25 static (role, action-type) → threshold-column table does not transfer**; every threshold read becomes a JSON path with no column-level schema guarantee |
| R7 | Committee composition over time | **verified** — `committee`, `committee_member`, `committee_hash`, `epoch_state`, `committee_registration`, `committee_de_registration` | **verified** — `committee(gov_action_tx_hash, gov_action_index, threshold_numerator, threshold_denominator, epoch)`, `committee_member(hash, cred_type, start_epoch, expired_epoch, epoch)`, `committee_registration`, `committee_deregistration`, plus `committee_state(epoch, state)`. `committee_member` carries `start_epoch`/`expired_epoch` directly, which is **better** than db-sync's `epoch_state` join (and dodges db-sync's duplicate-row bug [#2155](https://github.com/IntersectMBO/cardano-db-sync/issues/2155)) |
| R8 | Treasury withdrawals | **verified** — `treasury_withdrawal` + `reward_rest(type='treasury')` for the heuristic "paid" column | **verified table exists** — `LocalTreasuryWithdrawalEntity` + `treasury_withdrawal` in the governance DDL; `reward_rest` exists and is parity-compared. "Actually paid" linkage **unverified** |
| R9 | **Off-chain anchor fetch + blake2b-256 verify + recorded failure reasons** | **verified** — `off_chain_vote_data(hash, json, is_valid, …)`, `off_chain_vote_gov_action_data` (CIP-108), `off_chain_vote_drep_data` (CIP-119), `off_chain_vote_author/reference/external_update` (CIP-100), `off_chain_vote_fetch_error(fetch_error, fetch_time, retry_count)`. This is why our fetcher is deleted | **ABSENT.** The governance DDL stores `anchor_url` + `anchor_hash` only — the raw on-chain values. There is no off-chain content table, no fetch-error table, no CIP-100/108/119 parse. A repo-wide code search for off-chain fetching returns only CIP-26 *token* metadata (`assets-ext`) and an external dApp-registry fetcher in the 3.x MCP server — nothing for governance anchors. The plugin API exposes `drep.anchorUrl` / `drep.anchorHash` for *you* to fetch ([plugin API](https://store.yaci.xyz/docs/v2/plugins/plugin-api-guide)) |
| R10 | Stake deregistration events (churn/retention) | **verified** — `stake_deregistration`, via our `shelley: enable: true` override (D-4) | **verified** — `stake_registration` table in the staking store, referenced by [#1148](https://github.com/bloxbean/yaci-store/issues/1148). Enabled by default (all stores except ledger-state aggregates are on by default) |
| R11 | **A SQL surface to build 14 views + 1 matview + 1 function on** | **verified** — one Postgres database `cexplorer`, schema `public`, published [`doc/schema.md`](https://github.com/IntersectMBO/cardano-db-sync/blob/13.7.2.1/doc/schema.md) generated from the Haskell schema. Our entire read path assumes it | **verified SQL-first, with one architectural hazard.** The governance tutorial drives `psql` directly: `SET search_path TO yaci_store;` then `SELECT` over `gov_action_proposal`/`voting_procedure`. A "Stores Schema" doc section exists. **The hazard:** `reward`, `epoch_stake` and `drep_dist` are range-partitioned **one partition per epoch**, and any query on them *without an epoch predicate* must lock every partition and partition index — ~3,160 locks for one unbounded `drep_dist` query against 6,400 available slots on preview ([#1200](https://github.com/bloxbean/yaci-store/issues/1200), open 2026-09-30). Our `palmares.drep_epoch_power` is unbounded **by design** (the whole power series). Second hazard: `V0_1400_1__init.sql` opens with `drop table if exists drep_dist cascade;` — a baseline re-run silently drops dependent views |

**Scorecard: 6 of 11 at parity or better for Yaci-Store (R1, R3, R4, R7, R8, R10 — three of them
strictly nicer); 1 materially worse (R6); 2 hard failures (R5, R9); 1 architectural conflict with our
read path (R11); 1 that looks equal and is not (R2 — see §3).**

---

## 3. Resources and sync cost

This is the dimension the owner flagged as decisive. **Neither substrate has a published
from-genesis preprod figure on Pi-class arm64.** Yaci-Store does not rescue us here.

| Dimension | cardano-db-sync 13.7.1.0, `only_governance` + ledger + shelley + offchain_vote | Yaci-Store 2.0.7, `ledger-state` profile |
|---|---|---|
| **Can the expensive stores be skipped?** | **Yes, partially.** `tx_out: {value: disable}` ⇒ no `tx_out`, `ma_tx_out`, `collateral_tx_out`. `drep_distr` still populated, because the Haskell ledger replay computes stake distribution in memory from the ledger state, not from persisted rows. `epoch_stake` is populated unconditionally and is unavoidable (synthesis §2.4) | **No, not for what we need.** [`config/application-ledger-state.properties` @v2.0.7](https://github.com/bloxbean/yaci-store/blob/v2.0.7/config/application-ledger-state.properties) ships: `store.account.enabled=true`, `store.account.balance-aggregation-enabled=true`, `store.account.stake-address-balance-enabled=true`, `store.extensions.utxo-storage-type=default`, `store.adapot.enabled=true`, `store.governance-aggr.enabled=true`. `aggregates/adapot/.../snapshot/StakeSnapshotService.java` reads `stake_address_balance`, and the Account aggregate "depends on the `utxo` store and the event published by utxo store" ([design](https://store.yaci.xyz/docs/v2/introduction/design)). **`drep_dist` ⟸ stake snapshot ⟸ `stake_address_balance` ⟸ account aggregation ⟸ UTxO store.** The modular story does not reach R2 |
| **Peak RSS of the indexer** | **No published preprod figure.** Upstream documents mainnet only: ~16 GB InMemory / ~2–3 GB LSM. The spec's "~3.5 GB RAM db-sync with the classic ledger backend" is a derived estimate, not a measurement | **No published figure at all.** No JVM heap guidance in the docs. Requires Java 21. The only real datapoint: a working mainnet instance (beta5, `ledger-state` + analytics + mcp + blockfrost) on a host with **31 GB RAM** ([#1251](https://github.com/bloxbean/yaci-store/issues/1251)) — not a preprod figure and not a minimal profile |
| **Wall-clock from genesis, preprod** | **No published figure.** T-INFRA's 24–48 h is a derived band (synthesis §5) | **No published figure.** The only timing number anywhere is from the 2.0.0-**beta1** docs: ledger-state calculation ~**20–40 min per epoch transition on mainnet**. Preprod has ~320 epochs; naive multiplication is not a figure and is not offered as one |
| **On-disk size, preprod** | **No published figure** — this is precisely what Experiment A measures (hardware-independent `pg_total_relation_size`) | **No published figure**, and structurally larger: UTxO + `stake_address_balance` + account aggregation are mandatory (above), on top of the same `epoch_stake` both pay |
| **Postgres tuning required** | Standard. Our 50 Gi PVC + `postgres:17-alpine` plan unchanged | **Non-default and undocumented in the shipped template.** `shm_size` 2g→4g minimum, "6g or more" recommended ([docker docs](https://store.yaci.xyz/docs/v2/getting-started/installation/docker)); a working mainnet instance uses `shm_size: 8g`, `shared_buffers=4GB`, `effective_cache_size=8GB` ([#1251](https://github.com/bloxbean/yaci-store/issues/1251)). Plus `max_locks_per_transaction` must be raised for R11 |
| **arm64 container image** | **Yes** — `ghcr.io/blinklabs-io/cardano-db-sync:13.7.1.0` is an OCI index with `linux/amd64` + `linux/arm64` (verified in the design note by digest). Official `ghcr.io/intersectmbo/…` is amd64-only | **Yes** — `bloxbean/yaci-store:2.0.7` on Docker Hub is multi-arch `linux/amd64` + `linux/arm64` (verified via the Docker Hub v2 tags API; same for 2.0.4–2.0.6 and 3.0.0-beta5). **Parity. Not an advantage.** |
| **Operational blast radius on a Pi** | One process, one C-heap. Known hazard: `ledger_backend: lsm` crashes on resource-constrained devices "such as the Raspberry Pi" (13.7.2.1 release notes) — which is why we run InMemory | JVM + a second JVM-side aggregation workload + mandatory UTxO indexing, next to a `cardano-node` already requesting 3 Gi / limiting 8 Gi, plus ogmios and hydra-node. Nothing published about this combination on arm64 Pi-class hardware |

**Conclusion for §3:** the one argument that could have beaten "we already decided" — *materially cheaper
from-genesis cost on small hardware* — is **false**. For our specific read set, Yaci-Store is the
heavier substrate, and it is heavier in the dimension (disk) that Experiment A was designed to measure.
Experiment A remains the right experiment and remains on the critical path either way.

---

## 4. Correctness and trust

### 4.1 Two factual corrections to the existing design notes

Both are about **db-sync**, found while building the even-handed comparison. Neither changes the
verdict; both change text we have already written.

**Correction 1 — `dropped_epoch` is not an independent fourth outcome.**
`f1-dbsync-data-layer.md` §1.1 lists the four lifecycle columns as the source for
"ratified / enacted / expired / dropped", and T-API §1.6 defines a `gov_action_dropped` activity event
(synthesis D-19 makes those five epoch-boundary events non-negotiable). On
[db-sync #1932](https://github.com/IntersectMBO/cardano-db-sync/issues/1932) (open since 2025-01-20),
maintainer `kderme` states the schema doc is **outdated** and cites CHANGELOG 13.6.0.1: *"Enacted
gov_action_proposal are no longer marked as dropped"*. The reporter documents that for **expired**
proposals `dropped_epoch = expired_epoch + 1`. A follow-up on 2026-08-07 notes `doc/schema.md` line 910
still carries the stale text and remains unanswered.

Consequences, concretely:
- On enactment, `dropped_epoch` stays NULL. A distinct "dropped" outcome bucket will be **empty or a
  one-epoch-later duplicate of expiry** — not a real fourth state.
- `close_epoch(A) = LEAST(ratified_epoch, expired_epoch, dropped_epoch)` is **still correct** —
  `LEAST` picks the smaller `expired_epoch`. **No SQL change needed.**
- The **outcome enum and the `gov_action_dropped` event type must be restated** before the plan is
  written, or we ship an event that never fires and a status that double-counts expiry.
- Honest framing for the product: the "dropped as a pruned sibling/descendant" case — the one
  `doc/schema.md` promises — is **unverified on 13.7.x** and the upstream question is open. Note it as
  a known limitation rather than claiming four outcomes.

**Correction 2 — db-sync's own `drep_distr` has an open nondeterminism report, and it names preprod.**
[db-sync #1975](https://github.com/IntersectMBO/cardano-db-sync/issues/1975) (open since 2025-05-12):
two supposedly identical mainnet instances disagreed on one DRep's epoch-538 voting power —
**1,476,287,731 vs 1,093,348,347,589 lovelace (~740×)**. The maintainer pointed at the 13.6.0.4 Known
Issue: *"For networks that were already in Protocol Version 10 (e.g. Preview and PreProd) before the
upgrade to this release, the values of drep_distribution before the upgrade are not well defined and
could be slightly different between different executions… it is advised to delete all ledger state
files and let the ledger rules recompute."* The reporter's instance was mainnet and already on
13.6.0.4; last comment 2025-06-12, unresolved.

Consequences:
- **Our from-genesis sync on 13.7.1.0 with fresh ledger-state files is exactly the remedy that Known
  Issue prescribes.** The plan is right, but the *reason* should be written down — "from genesis"
  currently reads as a completeness requirement, when it is also a correctness requirement.
- This measurably weakens "db-sync is the canonical implementation, therefore the numbers are right"
  into "db-sync is canonical, and still carries an open nondeterminism report on precisely our hardest
  column". For a product whose pitch is *verifiable, re-derivable numbers*, that is an argument **for**
  a second derivation (§7), not for a substrate swap.

### 4.2 Is Yaci-Store's independent ledger math trustworthy?

**It is an independent reimplementation that is calibrated against db-sync, not independent of it.**
That cuts both ways and the honest reading is mixed:

*In its favour.* Bloxbean publishes per-epoch, per-DRep mismatch reports against db-sync and ships the
comparison harness in-repo ([`scripts/compare/`](https://github.com/bloxbean/yaci-store/tree/main/scripts/compare):
`compare_drep_amount.py`, `compare_drep_active_until.py`, `compare_gov_action_proposal_status.py`,
`compare_adapot.py`, `compare_epoch_stake.py`, `compare_reward_rest.py`). This is **more public
correctness accounting than db-sync does about itself** — db-sync has no equivalent artefact. The
2.x docs claim ADAPot and proposal status match on all three networks, verified through **mainnet
epoch 637 / preprod 295 / preview 1329**, and that 2.0.1 resolved all previously known DRep
distribution and DRep expiry mismatches ([overview](https://store.yaci.xyz/docs/v2/getting-started/overview)).
There is also real published documentation of the derivation itself — e.g.
[`drep-expiry.md`](https://github.com/bloxbean/yaci-store/blob/main/aggregates/governance-aggr/drep-expiry.md)
spells out `expiry = lastActivityEpoch + dRepActivity + dormantCount [+ v9Bonus]`, dormant-epoch
handling and the V9 bonus. db-sync documents nothing comparable; you read the Haskell.

*Against it.* The published mismatch pages are **not marked resolved** and still sit under "Known
Issues", footer *"Last updated on September 30, 2026"* — the mainnet DRep distribution page lists 23
`Amount mismatch (vs DB Sync)` entries across epochs 508–610 including an Auto-Abstain entry spanning
515–610, and the 2.0.0 overview lists further preview and preprod DRep-expiry mismatches. The
getting-started page says resolved; the detail pages do not say resolved. **We could not determine
which is current.** Alongside that: `drep_dist` semantics are offset from db-sync's
(`epoch_stake` compares at `epoch − 2`), SPO voting stake excludes active proposal deposits
([#1147](https://github.com/bloxbean/yaci-store/issues/1147)), `AlwaysNoConfidence` stake was
double-counted in the NoConfidence case ([#823](https://github.com/bloxbean/yaci-store/issues/823)),
and block/epoch totals diverge from db-sync for invalid Plutus transactions — on preprod,
**150 of 150** affected completed epochs mismatch on `total_fees`
([#1163](https://github.com/bloxbean/yaci-store/issues/1163),
[#1164](https://github.com/bloxbean/yaci-store/issues/1164)).

**Credibility verdict.** Because Bloxbean uses db-sync as the oracle, "two independent derivations"
is weaker than it sounds — Yaci-Store agreeing with db-sync is partly a design goal, not only evidence.
It is still worth having, because the open items are on *different* columns in each implementation.
But as the *primary* substrate for a registry whose pitch is re-derivability, the canonical Haskell
ledger replay is the defensible choice, and the one an auditor will accept without argument.

### 4.3 Incident profile, told straight

**Yaci-Store, October 2026.** Yaci 0.4.6 read a one-byte CBOR transaction count as a **signed** byte.
Mainnet block **14,053,493** (slot 200,136,161, 2026-10-11 07:07 UTC) therefore decoded as **zero
transactions, with no error**, and was stored empty — all **137** transactions lost. With
`ledger-state` enabled, sync then halts at block 14,053,494 with `Utxos not found for all ids`;
**without** it, sync continues silently with the block's transactions missing. The fix
([yaci#211](https://github.com/bloxbean/yaci/pull/211)) shipped as 2.0.7
([PR #1247](https://github.com/bloxbean/yaci-store/pull/1247)) and 3.0.0-beta5
([PR #1248](https://github.com/bloxbean/yaci-store/pull/1248)), backported to `main` on 2026-10-13
([PR #1256](https://github.com/bloxbean/yaci-store/pull/1256)). **Upgrading alone does not repair
stored data** — a repair script rewinds one block. Detection: `SELECT count(*) FROM transaction WHERE
block = 14053493;` < 137. Preprod and preview unaffected.

Read fairly, this is **two different stories**:
- *Bad:* a silent-corruption class of bug in the CBOR layer, in a hand-rolled decoder, reaching
  production, requiring a manual repair. db-sync does not have this class of failure because it reuses
  the node's own ledger/CBOR libraries. That asymmetry is structural, not luck, and it recurs every
  time a new era or a new CBOR edge case lands.
- *Good:* detected the same day, root-caused to one line, fixed, backported to two branches, shipped
  with a tested repair script and a one-query detection test, all within ~48 hours. That is a better
  incident response than most of this ecosystem manages.

**db-sync, for comparison** (72 open issues, 318 stars, 47 watchers, last push 2026-10-16; releases
13.7.0.2 → 13.7.2.1 between March and June 2026): the open-issue profile is quieter but hits closer to
home — #1975 `drep_distr` nondeterminism (§4.1), #1932 `dropped_epoch` semantics (§4.1), #1848
incorrect bech32 prefix for script-based DReps (already neutralised by our D-6 decision to compute
CIP-129 ourselves), #2155 duplicate `epoch_state` rows after a cross-epoch rollback (already handled by
our `DISTINCT`/max-id view), #2151 `only_utxo` populating governance tables — a preset/doc mismatch
that reinforces the design note's rule *"the source wins, the docs are unreliable"*. db-sync's
documentation is demonstrably stale in at least three places we have now verified.

**Yaci-Store project health** (355 open issues, 49 stars, 6 watchers, last push 2026-10-19; 2.0.3 →
2.0.7 between 2026-09-23 and 2026-10-11): far more open issues and far higher release cadence than
db-sync. Both readings are available — more active development, or more churn under a public API. For a
solo maintainer pinning a digest, high cadence mostly means more upgrade decisions to decline.

**Production users.** Yaci-Store's flagship is the Cardano Foundation's
[cardano-rosetta-java](https://github.com/cardano-foundation/cardano-rosetta-java) (Mesh/Rosetta API,
built on Yaci-Store for a lower resource footprint); its README additionally self-reports FluidTokens
Aquarium Node, CF Cardano Ballot, CF Reeve, CF AdaHandle Resolver, CF Token Metadata Registry,
AdaMatic, and Nexus. BloxBean develops it with direct Cardano Foundation support. **Note what is
*not* in that list: no governance explorer, no DRep registry, no voting-power product.** db-sync's
governance surface is what Koios, GovTool and the block explorers run on — and the spec's own precedent
argument (*"GovTool itself is an API over db-sync"*) still holds. For R2/R5 specifically, db-sync's
governance tables have orders of magnitude more adversarial production exposure.

---

## 5. The language argument, adversarially

The owner is right to attack this, and the attack lands.

**What Java actually costs us: almost nothing.** We consume one pinned container over SQL and never
build it. db-sync is equally opaque — nobody on this project is going to patch Haskell ledger-replay
internals either. The relevant question is not "which language can we read" but "which container can
we *operate*", and on that axis:

| Risk | Java / Yaci-Store | Haskell / db-sync | Honest delta |
|---|---|---|---|
| Debugging a stuck sync | Spring Boot logs, JVM thread dumps, `jcmd`, JFR. Mainstream, well-documented, and an LLM can read a stack trace fine | katip logs, GHC RTS flags, `-s` stats. Narrower tooling, fewer answers on the internet | **Java is easier.** This argument runs the *wrong* way in the synthesis |
| Reading its logs | Plain Spring Boot | Fine | Wash |
| Patching a bug upstream | Java + Maven + Spring. A contributor could plausibly do this | Haskell + cabal + CHaP + the ledger libraries. We *can* build Haskell (that is the whole project) | **Haskell is easier for us** — but only here, and only if we would ever actually do it. We would not |
| Trusting its ledger math | A Java reimplementation, calibrated against db-sync | The canonical implementation | **Decisive, and it is not about language** — it is about *which* implementation is canonical. §4.2 |
| Tuning memory | JVM heap + Spring + Postgres, undocumented for our profile | One RSS number, documented for mainnet | Mild edge to db-sync, mostly because the figure exists |

**Verdict on §6:** delete *"not worth the language hop for a Haskell shop"* from the synthesis. It is
the wrong reason and it is partly backwards. The real reasons are R5, R9, R6, R11 and §3 — all of
which survive the owner's attack, where the language argument does not.

---

## 6. Cost of switching now vs cost of discovering later

**Switching now (reversing in week 1):**

| Work | Size |
|---|---|
| Rebuild the off-chain anchor fetcher (R9): fetch, blake2b-256 verify, CIP-100/108/119 parse, typed failure rows, retry/backoff, IPFS gateways, `data:` URIs | **The largest single item.** It is a whole component that synthesis D-3 / spec check 7 deliberately deleted, and it is a *correctness-visible* component — the anchor verdict enum is in four DTOs |
| Replace the 4-state lifecycle with 3 (R5), or re-derive enacted/dropped ourselves from `gov_action_proposal_status` history + the constitution/committee/param tables | Spec check for lifecycle outcomes becomes partly unmeetable; the `gov_action_dropped` and `gov_action_enacted` activity events (D-19, "non-negotiable") lose their source |
| Rewrite the threshold table (R6) as JSON paths; D-25's static (role, action-type) → column table dies | Medium, and loses its test |
| Rework `palmares.drep_epoch_power` and `palmares.activity` to carry mandatory epoch predicates, to stay under `max_locks_per_transaction` (R11, #1200) | Medium, and it is a **permanent constraint on the read path**, not a one-time fix. Our deliberate "no projection tables" choice is what makes this bite |
| Re-plan Postgres sizing upward for the mandatory UTxO + account aggregation (§3) | Unknown, and the 50 Gi PVC on `local-path` **cannot be expanded** (`allowVolumeExpansion: false`, D-13) — this is the one that could force a re-provision |
| Rewrite the Koios reconcile joins (CIP-129 ids, `epoch − 2` offset) | Small |
| Re-do T-DATA's entire schema mapping, T-METRIC's SQL, T-API's DTO sources | **All five design tracks**, and §2 of the synthesis is a map of collisions between them resolved *against db-sync's schema* |
| Rediscover the resource envelope from zero (§3) | Experiment A must be rerun against a substrate with no published figures at all |

Net: this is **not** the cheap reversal the framing assumes. It is cheap only in the sense that nothing
is built yet — but five fact-checked design notes are written against db-sync's schema, and Yaci-Store
would void the three hardest resolutions in the synthesis (D-1 views-not-tables, D-3 the anchor
verdict, D-25 the thresholds).

**Discovering in six weeks that we should have switched:** the scenario is Experiment A returning a
figure that does not fit — db-sync not completing, not fitting the 50 Gi PVC, or exceeding pi5-1's
RAM. The existing plan already handles this correctly: Experiment A runs **off-cluster on day 1**, and
the synthesis already lists the ordered fallbacks (`epoch_stake` pruning CronJob → `ledger_backend:
lsm` → db-sync off-cluster) before the substrate decision reopens. **Yaci-Store is not in that
fallback chain and should not be** — it is structurally heavier on disk (§3), so a db-sync disk failure
is not a reason to reach for it. The honest fallback for "does not fit the Pi" is **x86_64 hardware**,
which the spec already budgets for F2.

**Therefore: the asymmetry the owner worried about does not exist here.** Reversing in six weeks is
expensive, but Yaci-Store would not be the thing we reverse *to*. The expensive-late scenario is a
hardware scenario, and Experiment A already de-risks it in week 1.

---

## 7. The hybrid: Yaci-Store as the cross-check

**This is the one place Yaci-Store is genuinely attractive, and it is worth saying so plainly.**

Koios runs on db-sync. So our R3/R4 Koios parity rows compare db-sync against db-sync — the spec
already concedes this (*"Koios rulează tot pe db-sync, deci e o verificare gratuită în CI, nu o
sursă"*). A Yaci-Store instance would be the **first genuinely independent derivation** in the stack,
and for a registry selling verifiable numbers that is a real claim: *"our per-epoch DRep voting power
agrees with a second, independently implemented indexer."* §4.1's db-sync #1975 makes the case
stronger, not weaker — the canonical implementation has an open nondeterminism report on exactly the
column a cross-check would cover.

Bloxbean has even done the work for us: `scripts/compare/compare_drep_amount.py` and
`compare_drep_active_until.py` already compare `drep_distr ↔ drep_dist` epoch by epoch, and
`compare_gov_action_proposal_status.py` compares proposal status. We would not write the comparator.

**But not in F1, and the reason is the same §3 finding.** The cross-check requires a *second*
from-genesis preprod sync, with the UTxO store and account aggregation mandatory, on a cluster whose
riskiest unknown is whether **one** such sync fits. Running it to validate the first one doubles the
exact cost we have not yet measured. And §4.2 discounts the credibility claim anyway: Bloxbean
calibrates against db-sync, so agreement is partly a design goal.

**Recommendation:** record it as a **deferred item with a named trigger**, in the synthesis §4 deferred
table, alongside the existing entries:

| Deferred | Trigger to revisit |
|---|---|
| Yaci-Store as an independent cross-check of `drep_epoch_power` and proposal status | F2, **and** only after a) db-sync's preprod figures are measured and comfortable, b) a host exists that is not pi5-1, c) the F1 Koios parity run has actually surfaced a divergence the db-sync-only stack cannot explain. Reuse `scripts/compare/compare_drep_amount.py` rather than writing a comparator |

A cheaper interim that costs nothing: when F1's Koios reconcile finds an unexplained divergence in
`drep_epoch_power`, check it against Bloxbean's **published** mismatch reports before assuming the bug
is ours. That is a one-line runbook entry, not a deployment.

---

## 8. What would make us switch later — monitorable triggers

Only triggers the project can actually observe. If none of these fires, do not revisit.

| # | Trigger | How we would notice | What it would change |
|---|---|---|---|
| T1 | **db-sync drops or breaks `only_governance` + `ledger: enable`**, or stops publishing an arm64-capable build we can pin | A db-sync release note or the blinklabs image matrix, checked at the deferred "dependency bumps" workstream | Substrate reopens for real. This is the only trigger that forces it |
| T2 | **Yaci-Store ships governance anchor fetch + hash verify + typed failure rows** (R9) | A `off_chain_*`-equivalent table appearing in the governance Flyway DDL, or a release note. Watch `stores/governance/src/main/resources/db/store/postgresql/` | Removes the largest switching cost. Still leaves R5, R6, R11 |
| T3 | **`GovActionStatus` gains `ENACTED` and `DROPPED`** (R5) — i.e. [#1194](https://github.com/bloxbean/yaci-store/issues/1194) closes with the enum extended | The enum file, one `gh api` call. Watch #1194 | Removes the second hard failure |
| T4 | **[#1200](https://github.com/bloxbean/yaci-store/issues/1200) closes** (partition-lock exhaustion on unbounded `drep_dist` queries) | Issue state. Watch #1200 | Makes our view-based read path viable on Yaci-Store at all. Until then, R11 is a blocker for *our specific architecture*, independent of everything else |
| T5 | **Experiment A says db-sync does not fit, and the published Yaci-Store figures say it would** | Experiment A's own numbers vs a *published* Yaci-Store preprod from-genesis figure, which does not exist today | Would reopen §3. Note the asymmetry: we need Yaci-Store figures to *exist* before this can ever fire. Today it cannot |
| T6 | **Mainnet (F2) on x86_64 makes the UTxO cost irrelevant** and we want the independent cross-check | The F2 hardware decision | Enables §7's hybrid, as an *addition*, never a replacement |
| T7 | **A Palmares number is publicly disputed and db-sync cannot settle it** | A GitHub issue or community challenge on our own numbers; or db-sync #1975 reproducing on our preprod | Strongest real-world case for §7's hybrid. Cheaper first step: check Bloxbean's published mismatch reports |

Not triggers, and should be ignored if raised: Yaci-Store's release cadence, its Java-ness, its star
count, or a new "modular" marketing claim. The decision rests on R5, R9, R6, R11 and the UTxO
dependency in §3 — nothing else.

---

## 9. Edits this note requires elsewhere

| File | Edit |
|---|---|
| `f1-synthesis.md` §4 (cut/deferred tables) | Change Yaci-Store's status from *"solid plan B, not worth the language hop for a Haskell shop"* to *"rejected as substrate (R5, R9, R6, R11, mandatory UTxO indexing); deferred as an optional cross-check with the T6/T7 trigger"*. **Delete the language argument** — §5 shows it runs backwards |
| `f1-synthesis.md` §1.4 + T-API §1.6/§2.1 | Restate the gov-action outcome enum and the `gov_action_dropped` activity event per §4.1 Correction 1: on enactment `dropped_epoch` stays NULL; on expiry it is `expired_epoch + 1`. `close_epoch`'s `LEAST` is unaffected — **no SQL change** |
| `f1-dbsync-data-layer.md` §1.1 (`gov_action_proposal` row) | Add the §4.1 Correction 1 caveat; db-sync's `doc/schema.md` line 910 is stale per its own maintainer |
| `f1-synthesis.md` §3 step 11 / spec §9 F1 check 1 | Add the §4.1 Correction 2 rationale: from-genesis with fresh ledger-state files is the remedy named in db-sync's 13.6.0.4 Known Issue for ill-defined pre-upgrade `drep_distribution` on preprod. It is a **correctness** requirement, not only a completeness one |
| ADR `0001-governance-data-substrate.md` | Cite this note as the Yaci-Store rejection record, with the R5/R9/R6/R11 + UTxO-dependency reasons and the T1–T7 triggers |

---

## 10. What could not be determined

- **Yaci-Store's current DRep-distribution parity status.** The 2.x getting-started page says 2.0.1
  resolved all previously known DRep distribution and expiry mismatches; the detail pages still list 23
  mainnet mismatch entries (epochs 508–610) under "Known Issues", footer dated 2026-09-30, with no
  resolution marker and no stated root cause or magnitude. The two pages cannot both be current.
- **Any from-genesis preprod figure for Yaci-Store** — wall-clock, on-disk size, or JVM heap. None
  published. The only resource datapoint is a 31 GB-RAM mainnet host running four extra modules.
- **Whether Yaci-Store's `ledger-state` profile can be trimmed below its shipped
  `application-ledger-state.properties`.** The dependency chain `drep_dist ⟸ StakeSnapshotService ⟸
  stake_address_balance ⟸ account aggregation ⟸ utxo store` is established from source and the design
  doc, but we did not test whether a hand-trimmed profile (e.g. `store.utxo.enabled=false` with
  stake-address balances only) starts and produces correct `drep_dist`. **If someone wants to reopen
  this, that experiment is the only thing worth running** — and it would still leave R5, R9, R6, R11.
- **Whether db-sync's "dropped as a pruned sibling/descendant" case is populated at all on 13.7.x.**
  The upstream question (#1932, 2026-08-07) is unanswered. Measurable on our own preprod sync once
  Experiment A completes: `SELECT count(*) FROM gov_action_proposal WHERE dropped_epoch IS NOT NULL
  AND expired_epoch IS NULL AND enacted_epoch IS NULL;`
- **Yaci-Store's treasury-withdrawal "actually paid" linkage** (R8) — the table exists; the join back
  to disbursement was not traced.
