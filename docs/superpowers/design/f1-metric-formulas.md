# F1 — Metric formulas (Track 3)

Design note for the plan author. Every metric the registry publishes, made computable and
unambiguous: definition, pinned denominator, exact db-sync columns, every edge case with a
decision, a worked example with real preprod numbers, exact SQL, and the anti-drift publishing
mechanism.

**Status:** researched and decided 2026-10-19 against live preprod data (Koios preprod, epoch 320)
and primary sources (cardano-ledger `master`, cardano-db-sync `master` schema doc, CIP-1694).
Sources are inline. `**(decision)**` = my call. `**(BLOCKER)**` = needs the owner or another track.

**Adversarially fact-checked 2026-10-19** against db-sync 13.7.2.1 schema **and source**,
cardano-ledger `master`, and freshly re-pulled Koios preprod data. Thirteen claims were corrected in
place; **read §10 first** — it is the diff, and three of the corrections change numbers that CI will
gate on (`age_days`, `vote_latency_p90_hours`, the network-level counts).

**Coordinates with:**
- `docs/superpowers/design/f1-preprod-infra.md` (Track 1) — db-sync config. **One delta required, §0.1.**
- `docs/superpowers/design/f1-api-contract.md` (Track 2) — wire field names and `/metrics/formulas`.
  I keep every field name Track 2 fixed and **correct three of its formula one-liners** (§0.2).

---

## 0. Cross-track deltas (read these first — they change other tracks' work)

### 0.1 Track 1 must flip `shelley.enable` to `true` **(decision)** **(BLOCKER if refused)**

Track 1's `insert_options` (f1-preprod-infra.md §1.3) sets `shelley: { enable: false }` and accepts the
gap because "no acceptance check needs stake delegation or pool registration metadata". Two v1 metrics
do need what that flag disables:

| Metric | Needs | Why `shelley.enable: false` breaks it |
|---|---|---|
| `delegator_count`, `delegator_retention_rate` | `stake_registration`, `stake_deregistration` | De-registering a stake credential **removes its vote delegation in the ledger**, but `delegation_vote` is an append-only tx log and keeps the row forever. Without the de-registration table every dead stake key counts as a live delegator — the count only ever goes up. Track 2's own `delegator_retention_rate_v1` already lists `stake_deregistration` as an input (f1-api-contract.md §2.5). Gate verified in source: `insertConwayDelegCert` wraps `ConwayRegCert`/`ConwayUnRegCert` in `when (ioShelley iopts)` ([Insert/Certificate.hs:132-138, 13.7.2.1](https://github.com/IntersectMBO/cardano-db-sync/blob/13.7.2.1/cardano-db-sync/src/Cardano/DbSync/Era/Universal/Insert/Certificate.hs)). |
| `spo_participation_rate` (and the SPO column of the gov-action tally) | `pool_update`, `pool_retire` | §5.8 derives the SPO's *registration interval* from `pool_update`…`pool_retire`. Both come from `insertPoolCert`, which is `when (ioShelley iopts)` ([Insert/Certificate.hs:73,83](https://github.com/IntersectMBO/cardano-db-sync/blob/13.7.2.1/cardano-db-sync/src/Cardano/DbSync/Era/Universal/Insert/Certificate.hs)). With no pool rows there is no SPO denominator at all. **Not** `param_proposal` — see the correction below. |

**Corrected 2026-10-19 (fact-check):** `param_proposal` is **not** affected by `shelley.enable`.
For a Conway `ParameterChange` action, `insertGovActionProposal` calls `insertParamProposal`
unconditionally, inside the `when (ioGov iopts)` block
([Insert/GovAction.hs:78-85](https://github.com/IntersectMBO/cardano-db-sync/blob/13.7.2.1/cardano-db-sync/src/Cardano/DbSync/Era/Universal/Insert/GovAction.hs)
called from [Insert/Tx.hs:198-200](https://github.com/IntersectMBO/cardano-db-sync/blob/13.7.2.1/cardano-db-sync/src/Cardano/DbSync/Era/Universal/Insert/Tx.hs)).
The `when (ioShelley iopts)` guard on `insertParamProposal` at Insert/Tx.hs:181-183 covers only the
**legacy pre-Conway** `txParamProposal` update proposals. So SPO SecurityGroup eligibility (§3.3) is
computable under `only_governance` as shipped, and the former conditional **(BLOCKER)** here is void.

Cost of the flip: `stake_registration`, `stake_deregistration`, `delegation`, `withdrawal`,
`pool_update`, `pool_retire` get populated (`param_proposal` already was). None of them is a large
table — the space hogs (`tx_out`, `ma_tx_out`, `multi_asset`, `tx_cbor`, `plutus`) stay disabled. The
delta is one line in Track 1's hand-written option set (Track 1 §0.4 already refuses `preset:`,
because a preset "overrides all other properties" — so this edit lands where it is read).

```yaml
  shelley:
    enable: true     # Track 3: stake_registration/_deregistration (delegator churn)
                     # + pool_update/pool_retire (SPO registration interval, §5.8).
                     # See f1-metric-formulas.md §0.1.
```

**Verify at first sync** (both must return > 0):
```sql
SELECT count(*) FROM stake_deregistration;             -- 0 ⇒ the flip did not take
SELECT count(*) FROM pool_update;                      -- 0 ⇒ the flip did not take
SELECT count(*) FROM param_proposal pp                 -- expect 16 on preprod @ epoch 320
  JOIN gov_action_proposal g ON g.param_proposal = pp.id;   -- > 0 even WITHOUT the flip
```
If the owner refuses the flip, `delegator_retention_rate` and `spo_participation_rate` report `null`
with a documented reason. `rationale_rate` is unaffected by `shelley`, but **is** gated by
`offchain_vote_data: "enable"` — already decided in Track 1 §0.4/§1.3, do not regress it.

### 0.2 Corrections to Track 2's formula one-liners

| `formula_id` | Track 2 wrote | Correction (this note) |
|---|---|---|
| `rationale_rate_v1` | `votes_with_anchor_status_valid / votes_cast`, validity from `off_chain_vote_data.is_valid` | Denominator must be **distinct actions voted**, not vote rows — preprod has one (DRep, action) pair with **20** vote rows (§3.2 re-votes). And `is_valid` is the **CIP-100 schema** verdict, not the hash verdict: hash mismatches never reach `off_chain_vote_data` at all. Validity = *a row exists in `off_chain_vote_data`*. See §1.2. |
| `participation_rate_v1` | eligible = action whose `[proposed_at, resolution]` window intersects the DRep's registered interval | `resolution` is underspecified and gets the dependency-dropped case wrong by one epoch. Pinned as `last_votable_epoch` in §1.1. Two further pins Track 2 omitted: **still-open actions are excluded from the denominator**, and **during the Conway bootstrap phase DReps could only vote on `InfoAction`** — without that, every pre-Plomin DRep's denominator is inflated. |
| `age_days_v1` | `(chain_tip_time - registered_at.time) / 86400` | Counts the time a retired DRep was *not* registered. Redefined as the **sum of registered intervals** in §1.3. Field name unchanged. |

Everything else from Track 2 (`participation_rate`, `rationale_rate`, `age_days`,
`vote_latency_median_hours`, `delegator_retention_rate`, `votes_by_type`, `voting_power` +
`voting_power_epoch`, `delegator_count`, `eligible_actions`, `votes_cast`, `formula_ids`,
`GovActionType` keeping db-sync's `NewCommittee` → wire `new_committee`, `DRepReserved` status,
`always_abstain_lovelace`) stands unchanged.

### 0.3 Shared vocabulary — three SQL CTEs every metric reuses

Write these **once**, in `metrics/_common.sql`, and `include` them from every metric query (the
catalog loader concatenates, §7). Do not re-derive them per metric; that is how the numbers drift.

**(decision)** They live in our own Postgres schema `palmares`, not in db-sync's `public`. db-sync
already owns `drep_hash`, `drep_distr`, `drep_registration`; an unqualified view called `drep_vote`
or `drep_interval` in `public` is a name collision waiting for the next db-sync schema migration.
`CREATE SCHEMA IF NOT EXISTS palmares;` once, then `SET search_path = palmares, public;` in the
read role. The §1.4 indexes stay on db-sync's own tables (an index cannot be namespaced away).

```sql
-- (1) action_window: the authoritative voting window of every governance action.
-- Ledger rule: a vote is valid while  currentEpoch <= gasExpiresAfter  AND the action is still in
-- the proposals set. Source: cardano-ledger Conway/Rules/Gov.hs, checkVotesAreNotForExpiredActions
-- https://github.com/IntersectMBO/cardano-ledger/blob/master/eras/conway/impl/src/Cardano/Ledger/Conway/Rules/Gov.hs
CREATE OR REPLACE VIEW palmares.action_window AS
SELECT g.id                AS gov_action_id,
       g.type              AS action_type,
       b.epoch_no          AS proposed_epoch,
       b.slot_no           AS proposed_slot,
       b.time              AS proposed_at,
       b.proto_major       AS proposed_proto_major,   -- display/audit only, NOT the phase test (§3.4)
       g.expiration,
       LEAST( g.expiration,
              COALESCE(g.ratified_epoch,     g.expiration),
              COALESCE(g.expired_epoch,      g.expiration),
              COALESCE(g.dropped_epoch - 1,  g.expiration) ) AS last_votable_epoch,
       -- the Conway bootstrap test, over the whole window (§3.4). Written ONCE, here.
       ( g.type = 'InfoAction'
         OR EXISTS (SELECT 1 FROM epoch_param ep
                    WHERE ep.epoch_no BETWEEN b.epoch_no AND
                          LEAST( g.expiration,
                                 COALESCE(g.ratified_epoch,    g.expiration),
                                 COALESCE(g.expired_epoch,     g.expiration),
                                 COALESCE(g.dropped_epoch - 1, g.expiration) )
                      AND ep.protocol_major <> 9) )        AS drep_votable,
       g.param_proposal
FROM gov_action_proposal g
JOIN tx    ON tx.id = g.tx_id
JOIN block b ON b.id = tx.block_id;
```
`last_votable_epoch` leaves `ratified_epoch` **un-decremented** on purpose, unlike `dropped_epoch`.
Verified empirically on preprod (DRep `vote_list` ∪ `proposal_list`, 2026-10-19): on 3 of the 9 voted
ratified actions the last DRep vote landed in the *same* epoch as `ratified_epoch` (228/228, 251/251,
293/293), and on all 4 voted dropped-only actions the last vote landed at `dropped_epoch − 1`
(251/252, 294/296, 304/305, 312/313). For expired actions no vote was ever observed later than
`expired_epoch − 1`, so `= expiration` is permissive but never wrong
(`checkVotesAreNotForExpiredActions`: `curEpoch <= gasExpiresAfter`).
`dropped_epoch - 1` is not cosmetic. Measured on all 131 preprod proposals (Koios
`proposal_list`, 2026-10-19): `expired_epoch = expiration` in 103/103 cases,
`dropped_epoch = expired_epoch + 1` in 102/102 cases, and enacted actions carry
`enacted_epoch = ratified_epoch + 1` with `dropped_epoch` and `expired_epoch` **NULL** — which
contradicts the schema doc's claim that "a proposal is dropped when it's expired or enacted"
([schema.md `gov_action_proposal`](https://github.com/IntersectMBO/cardano-db-sync/blob/master/doc/schema.md)).
The remaining 6 rows have `dropped_epoch` **only** (no expiry, no enactment): actions invalidated
because a competing action was enacted. For those the action left the set at the `dropped_epoch`
boundary, so the last epoch a vote could land in is `dropped_epoch - 1`.

```sql
-- (2) drep_interval: the epochs in which each DRep was registered.
-- drep_registration.deposit: >0 register, <0 retire, NULL update.
-- https://github.com/IntersectMBO/cardano-db-sync/blob/master/doc/schema.md  (drep_registration)
CREATE OR REPLACE VIEW palmares.drep_interval AS
WITH ev AS (
  SELECT r.drep_hash_id, b.epoch_no, b.slot_no, b.time, r.cert_index,
         CASE WHEN r.deposit > 0 THEN 'register' WHEN r.deposit < 0 THEN 'retire' END AS kind
  FROM drep_registration r
  JOIN tx    ON tx.id = r.tx_id
  JOIN block b ON b.id = tx.block_id
  WHERE r.deposit IS NOT NULL                       -- updates never open or close an interval
), seq AS (
  SELECT ev.*,
         LEAD(epoch_no) OVER w AS next_epoch,
         LEAD(kind)     OVER w AS next_kind
  FROM ev WINDOW w AS (PARTITION BY drep_hash_id ORDER BY slot_no, cert_index)
)
SELECT drep_hash_id,
       epoch_no AS from_epoch,
       slot_no  AS from_slot,      -- needed by §5.3: epoch alone cannot order intra-epoch events
       time     AS from_time,
       CASE WHEN next_kind = 'retire' THEN next_epoch END AS to_epoch   -- NULL ⇒ still registered
FROM seq
WHERE kind = 'register';
```

```sql
-- (3) drep_vote: one canonical row per (DRep, action), plus the re-vote audit trail.
-- A DRep may vote repeatedly on the same action; the ledger keeps the LAST vote
-- (Gov.hs: isVoteReplaced / GovRemovedVotes replacedVotes). db-sync keeps every row.
CREATE OR REPLACE VIEW palmares.drep_vote AS
SELECT v.drep_voter          AS drep_hash_id,
       v.gov_action_proposal_id AS gov_action_id,
       v.vote, v.voting_anchor_id,
       b.epoch_no AS vote_epoch, b.slot_no AS vote_slot, b.time AS cast_at,
       tx.hash    AS vote_tx_hash,
       ROW_NUMBER() OVER w        AS seq_first,   -- 1 = first vote  → latency
       ROW_NUMBER() OVER (PARTITION BY v.drep_voter, v.gov_action_proposal_id
                          ORDER BY b.slot_no DESC, tx.block_index DESC, v.index DESC) AS seq_last,
       COUNT(*)    OVER (PARTITION BY v.drep_voter, v.gov_action_proposal_id) AS vote_rows
FROM voting_procedure v
JOIN tx    ON tx.id = v.tx_id
JOIN block b ON b.id = tx.block_id
WHERE v.voter_role = 'DRep'
WINDOW w AS (PARTITION BY v.drep_voter, v.gov_action_proposal_id
             ORDER BY b.slot_no, tx.block_index, v.index);
```

**(decision)** `seq_last = 1` is the *verdict* (matches the ledger). `seq_first = 1` is the *latency*
(when the DRep first committed in public). `vote_rows > 1` is surfaced in the vote list as
`revote_count` so the re-votes are visible rather than silently collapsed.

---

## 1. Definitions and pinned formulas

Every metric is computed **as of a pinned epoch** — the projection cursor's epoch, exposed on every
response as `as_of_epoch` **(decision)**. Without it the numbers are not reproducible: the
denominator grows every epoch.

### 1.1 `participation_rate`

> Of the governance actions this DRep was eligible to vote on and whose voting window has closed,
> the fraction it actually voted on.

```
participation_rate(d, E) = |A_voted(d, E)| / |A_eligible(d, E)|          null if |A_eligible| = 0

A_eligible(d, E) = { a ∈ actions :
      drep_votable(a)                                  -- phase + type rule, §3.3
   ∧  a.last_votable_epoch < E                         -- window already closed  (§3.6)
   ∧  ∃ i ∈ drep_interval(d) :                         -- registration overlaps the window
          i.from_epoch <= a.last_votable_epoch
       ∧ (i.to_epoch IS NULL OR i.to_epoch >= a.proposed_epoch) }

A_voted(d, E)    = { a ∈ A_eligible(d, E) : ∃ a drep_vote row for (d, a) }   -- DISTINCT actions
```

Denominator pinned: **distinct governance actions**, not votes, not vote rows, not epochs.
`eligible_actions` and `votes_cast` are both published next to the rate so the fraction is auditable
(Track 2 already has both fields).

### 1.2 `rationale_rate`

> Of the actions this DRep voted on, the fraction where its final vote carried a metadata anchor
> whose content was retrieved and whose blake2b-256 hash matched.

```
rationale_rate(d, E) = |{ a ∈ A_voted : hash_verified(final_vote(d,a).voting_anchor_id) }|
                       / |A_voted(d, E)|                              null if |A_voted| = 0

hash_verified(anchor_id) = EXISTS (SELECT 1 FROM off_chain_vote_data WHERE voting_anchor_id = anchor_id)
```

**(decision)** Validity = *a row in `off_chain_vote_data`*, not `is_valid = true`. db-sync stores a row
there **only when the hash matched**; mismatches and unreachable URLs go to
`off_chain_vote_fetch_error`. `is_valid` then grades the *body*: `TRUE` = valid JSON conforming to
CIP-100, `FALSE` = valid JSON that does not conform, `NULL` = not JSON at all
([schema.md `off_chain_vote_data`](https://github.com/IntersectMBO/cardano-db-sync/blob/master/doc/schema.md)).
Using `is_valid` as the headline test would (a) punish an honest rationale for a schema nit, and
(b) be indistinguishable from "db-sync has not fetched it yet". The on-chain commitment is the
`(url, hash)` pair; hash-verified retrievable content is the reproducible fact. Schema conformance
ships as a separate, non-headline counter.

Published alongside, so the rate is auditable **(decision)** — four counters that always sum to
`votes_cast`:

| field | meaning |
|---|---|
| `rationale_verified` | numerator: anchor present, hash matched |
| `rationale_unverified` | anchor present, row in `off_chain_vote_fetch_error` (mismatch or unreachable) |
| `rationale_pending` | anchor present, neither table has a row yet (db-sync has not fetched it) |
| `rationale_absent` | `voting_procedure.voting_anchor_id IS NULL` — no rationale was offered |

`rationale_pending > 0` means the number is provisional; the UI shows "N of M verified" rather than a
bare percentage. No extra flag field.

### 1.3 `age_days` (tenure)

> Days this DRep has been registered, chain time, gaps excluded.

```
age_days(d, E) = ( Σ_{i ∈ drep_interval(d)} ( min(i.end_time, as_of_time) − i.from_time ) ) / 86400
     where i.end_time = the block time of the retire cert, or as_of_time if i.to_epoch IS NULL
```
`as_of_time` = `block.time` of the projection cursor's block — chain time, never `now()` and never
`insertedAt` (inherited fix #5). A DRep that registered, retired for 50 epochs and re-registered does
not get credit for the gap. The gap itself is visible in `/drep/{id}` → `updates[]`, so no extra field.

### 1.4 `delegator_count` and `delegator_retention_rate`

> `delegator_count` = stake credentials whose live vote-delegation targets this DRep, at epoch `E`.
> `delegator_retention_rate` = of the delegators it had 10 epochs ago, the fraction still delegating
> to it now.

```
D(d, e) = { s ∈ stake_address :
      latest_vote_delegation(s, e).drep_hash_id = d          -- latest as of the end of epoch e
   ∧  stake_credential_registered(s, e) }                    -- de-registration kills the delegation

delegator_count(d, E)           = |D(d, E)|
delegator_retention_rate(d, E)  = |D(d, E) ∩ D(d, E−W)| / |D(d, E−W)|      null if |D(d, E−W)| = 0
delegators_joined(d, E)         = |D(d, E) \ D(d, E−W)|
delegators_left(d, E)           = |D(d, E−W) \ D(d, E)|
```

**(decision)** `W = 10` epochs (50 days on both preprod and mainnet — one real governance cycle, and
longer than `govActionLifetime` on both networks — 7 epochs on preprod, 6 on mainnet, both verified — so a churn figure spans at least one full action). It is a
constant in `metrics/_common.sql`, published in the catalog as `retention_window_epochs: 10`, and
echoed on the response so a reader never has to guess the window.

**(decision)** No `drep_epoch_delegator` projection table in v1. The interval SQL in §5.4 answers both
numbers on demand off two indexes. `ponytail:` ceiling — if the `/drep/{id}` p95 exceeds 500 ms on
preprod, promote the two CTEs to a Postgres **materialized view** refreshed at the end of each
projection run; that is a `CREATE MATERIALIZED VIEW` plus one `REFRESH`, not Haskell.

Required indexes (ours, on db-sync's DB — created by our migration, not db-sync's):
```sql
CREATE INDEX IF NOT EXISTS idx_delegation_vote_drep ON delegation_vote (drep_hash_id);
CREATE INDEX IF NOT EXISTS idx_delegation_vote_addr ON delegation_vote (addr_id);
CREATE INDEX IF NOT EXISTS idx_voting_procedure_drep ON voting_procedure (drep_voter)
  WHERE voter_role = 'DRep';
```

### 1.5 `vote_latency_median_hours`

> Median hours between an action being proposed on chain and this DRep's **first** vote on it.

```
latency_hours(d, a) = ( first_vote(d, a).cast_at − a.proposed_at ) / 3600
vote_latency_median_hours(d, E) = median over a ∈ A_voted(d, E)        null if |A_voted| = 0
vote_latency_p90_hours(d, E)    = 90th percentile, same sample
vote_latency_sample(d, E)       = |A_voted(d, E)|
```
**(decision)** Measured in hours from the **proposal block time**, not from the start of the epoch and
not as a fraction of the window. Both numbers come from `percentile_cont` in one pass, and the sample
size ships with them, because a median over 2 votes is not a median. First vote, not final vote —
latency is about when the DRep showed up.

### 1.6 `votes_by_type` (distribution by action type)

> For each of the 7 Conway action types: how many this DRep was eligible for, how many it voted on,
> and the Yes/No/Abstain split of the final votes.

```
per action type t:  eligible_actions = |{a ∈ A_eligible : a.action_type = t}|
                    votes_cast       = |{a ∈ A_voted    : a.action_type = t}|
                    participation_rate = votes_cast / eligible_actions     null if eligible = 0
                    yes / no / abstain = counts of final_vote.vote over those actions
```
**(decision)** All 7 types are always emitted, including rows with `eligible_actions = 0` and
`participation_rate: null`. Omitting them would make a DRep look like it has no NoConfidence record
when the truth is that preprod only ever had one such action. The type-level rates are what carries
the nuance the headline rate flattens — e.g. the §4 fixture reads 14% overall but 38% on InfoActions
and 7.6% on TreasuryWithdrawals, because preprod is 79 treasury-withdrawal test actions.

### 1.7 Cheap additions I judge essential **(decision)**

Each is one column or one `COALESCE`, and each prevents a reader from drawing a wrong conclusion
from the metrics above.

| field | formula | why it has to be there |
|---|---|---|
| `as_of_epoch` | projection cursor's `block.epoch_no` | Without it no number is reproducible. Non-negotiable for spec S2. |
| `is_active`, `active_until_epoch` | `drep_distr.active_until >= as_of_epoch`; `active_until` | An **inactive** DRep's stake does not count toward the active voting stake in ratification, so its `voting_power` is real-but-inert. Publishing power without this is the single most misleading thing the page could do. Free: one column already in `drep_distr`. |
| `last_vote_at`, `last_vote_epoch` | `max(cast_at)` over `drep_vote` | "Has this DRep voted at all this year" answers the delegator's actual question faster than any rate. One `max()`. |
| `open_actions_unvoted` | count of actions where `drep_votable ∧ last_votable_epoch >= as_of_epoch ∧ no vote yet` | The actions deliberately excluded from the participation denominator (§3.6). Showing the count stops "why is 14% not 16%", and it is exactly the input F3's triage tool needs. |
| `revote_count` (per vote row, not per DRep) | `vote_rows` from `drep_vote` | Makes the collapse in §0.3 visible instead of silent. |

---

## 2. db-sync columns each metric reads

Verified against
[cardano-db-sync `master` doc/schema.md](https://github.com/IntersectMBO/cardano-db-sync/blob/master/doc/schema.md)
and
[cardano-db/src/Cardano/Db/Schema/Core/GovernanceAndVoting.hs](https://github.com/IntersectMBO/cardano-db-sync/blob/master/cardano-db/src/Cardano/Db/Schema/Core/GovernanceAndVoting.hs)
on 2026-10-19.

| Metric | Tables → columns |
|---|---|
| `participation_rate` | `gov_action_proposal` (`id`, `type`, `expiration`, `ratified_epoch`, `enacted_epoch`, `expired_epoch`, `dropped_epoch`, `param_proposal`, `tx_id`) · `tx` (`id`, `block_id`, `block_index`) · `block` (`epoch_no`, `slot_no`, `time`, `proto_major`) · `voting_procedure` (`gov_action_proposal_id`, `voter_role`, `drep_voter`, `index`, `tx_id`) · `drep_registration` (`drep_hash_id`, `deposit`, `cert_index`, `tx_id`) |
| `rationale_rate` | `voting_procedure.voting_anchor_id` · `voting_anchor` (`id`, `url`, `data_hash`, `type`) · `off_chain_vote_data` (`voting_anchor_id`, `is_valid`, `language`) · `off_chain_vote_fetch_error` (`voting_anchor_id`, `fetch_error`, `retry_count`) |
| `age_days` | `drep_registration` + `tx` + `block.time` |
| `delegator_count`, `delegator_retention_rate` | `delegation_vote` (`addr_id`, `drep_hash_id`, `cert_index`, `tx_id`) · `stake_registration` (`addr_id`, `epoch_no`, `cert_index`, `tx_id`) · `stake_deregistration` (same) · `stake_address` (`id`, `view`) |
| `vote_latency_median_hours` | `block.time` of the vote's tx and of the proposal's tx |
| `votes_by_type` | `gov_action_proposal.type` + `voting_procedure.vote` |
| `voting_power`, `voting_power_epoch` | `drep_distr` (`hash_id`, `amount`, `epoch_no`, `active_until`) |
| `always_abstain_lovelace`, `always_no_confidence_lovelace` | `drep_distr.amount` joined on `drep_hash.view IN ('drep_always_abstain','drep_always_no_confidence')` |
| SPO eligibility (SecurityGroup test) | `param_proposal` (11 columns, §3.3) |
| CC eligibility | `committee_member` (`committee_hash_id`, `expiration_epoch`) · `committee_registration` · `committee_de_registration` |

**Columns that do not exist** — do not plan against them:
- there is **no** `drep_epoch_summary`, no `drep_delegator_count`, no vote-count column anywhere.
  `drep_distr` is explicitly "a single entry per DRep and doesn't show every delegator" (schema.md).
- `voting_procedure` has **no** `block_id` and **no** timestamp. Chain time is only reachable through
  `tx → block`. Every metric that mentions time carries that join. Inherited fix #5 is structural here:
  there is no `insertedAt` to lazily reach for.
- `voting_procedure.invalid` is "currently not implemented and always stays null" (schema.md). Never
  filter on it; it will silently drop nothing today and everything the day it is implemented.
- db-sync uses **`NewCommittee`**, not CIP-1694's `UpdateCommittee`, in the `govactiontype` enum.

---

## 3. Edge cases, each with a decision

### 3.1 Eligibility per role per action type — the authoritative matrix

Derived from `cardano-ledger` `master`, not from CIP-1694 prose:
[`Conway/Governance/Internal.hs`](https://github.com/IntersectMBO/cardano-ledger/blob/master/eras/conway/impl/src/Cardano/Ledger/Conway/Governance/Internal.hs)
(`votingDRepThresholdInternal`, `votingStakePoolThresholdInternal`, `votingCommitteeThresholdInternal`,
`isVotingAllowed`) and
[`Conway/Rules/Gov.hs`](https://github.com/IntersectMBO/cardano-ledger/blob/master/eras/conway/impl/src/Cardano/Ledger/Conway/Rules/Gov.hs)
(`checkVotersAreValid` → `DisallowedVoters`). `isVotingAllowed` is false exactly when the role's
threshold is `NoVotingAllowed`; `InfoAction` yields `NoVotingThreshold`, which is *allowed but
unratifiable*.

| db-sync `type` | CIP-1694 name | DRep | SPO | CC |
|---|---|---|:--:|:--:|:--:|
| `ParameterChange` | Parameter change | ✅ | **only if the update touches the SecurityGroup** | ✅ |
| `HardForkInitiation` | Hard-fork initiation | ✅ | ✅ | ✅ |
| `TreasuryWithdrawals` | Treasury withdrawal | ✅ | ❌ | ✅ |
| `NoConfidence` | Motion of no confidence | ✅ | ✅ | ❌ |
| `NewCommittee` | Update committee/threshold | ✅ | ✅ | ❌ |
| `NewConstitution` | New constitution | ✅ | ❌ | ✅ |
| `InfoAction` | Info action | ✅ | ✅ | ✅ |

**(decision)** DRep denominator post-bootstrap = **all 7 types**. SPO and CC denominators use their
columns above. A ❌ cell never enters that role's denominator — a treasury withdrawal is not an SPO's
missed vote.

### 3.2 Re-votes: the dominant edge case on preprod

A DRep may vote on the same action any number of times; the ledger keeps the last
(`Gov.hs`: `isVoteReplaced`, `GovRemovedVotes replacedVotes`), db-sync keeps all rows. Measured on
preprod 2026-10-19 (Koios `vote_list?voter_role=eq.DRep`, **816** rows — the complete set, verified
via `Prefer: count=exact`): **69 (DRep, action) pairs have more than one row, and one pair has 20**.
A naive `COUNT(*)` participation rate still reads >100% for the worst offender.

**(decision)** Every count is `COUNT(DISTINCT gov_action_proposal_id)`. Verdict = `seq_last = 1`.
Latency = `seq_first = 1`. `revote_count` published per vote.

### 3.3 SPO on `ParameterChange`: the SecurityGroup test

The SecurityGroup members, from
[`Conway/PParams.hs`](https://github.com/IntersectMBO/cardano-ledger/blob/master/eras/conway/impl/src/Cardano/Ledger/Conway/PParams.hs)
(every `THKD ('PPGroups _ 'SecurityGroup)` field), mapped to `param_proposal` columns:

| ledger field | `param_proposal` column |
|---|---|
| `cppTxFeePerByte` | `min_fee_a` |
| `cppTxFeeFixed` | `min_fee_b` |
| `cppMaxBBSize` | `max_block_size` |
| `cppMaxTxSize` | `max_tx_size` |
| `cppMaxBHSize` | `max_bh_size` |
| `cppCoinsPerUTxOByte` | `coins_per_utxo_size` |
| `cppMaxBlockExUnits` | `max_block_ex_mem`, `max_block_ex_steps` |
| `cppMaxValSize` | `max_val_size` |
| `cppGovActionDeposit` | `gov_action_deposit` |
| `cppMinFeeRefScriptCostPerByte` | `min_fee_ref_script_cost_per_byte` |

```sql
-- spo_votable(a)
CASE a.action_type
  WHEN 'TreasuryWithdrawals' THEN FALSE
  WHEN 'NewConstitution'     THEN FALSE
  WHEN 'ParameterChange'     THEN EXISTS (
        SELECT 1 FROM param_proposal p WHERE p.id = a.param_proposal AND (
             p.min_fee_a IS NOT NULL OR p.min_fee_b IS NOT NULL
          OR p.max_block_size IS NOT NULL OR p.max_tx_size IS NOT NULL OR p.max_bh_size IS NOT NULL
          OR p.coins_per_utxo_size IS NOT NULL
          OR p.max_block_ex_mem IS NOT NULL OR p.max_block_ex_steps IS NOT NULL
          OR p.max_val_size IS NOT NULL OR p.gov_action_deposit IS NOT NULL
          OR p.min_fee_ref_script_cost_per_byte IS NOT NULL))
  ELSE TRUE
END
```
Depends on §0.1. If `param_proposal` is empty, every ParameterChange is unclassifiable for SPOs.

### 3.4 The Conway bootstrap phase — **this silently breaks denominators**

Confirmed, with preprod's own boundary dates:

- **The rule.** During the bootstrap phase, only `ParameterChange`, `HardForkInitiation` and
  `InfoAction` could be *proposed* (`Gov.hs`: `isBootstrapAction`, `checkBootstrapProposal` →
  `DisallowedProposalDuringBootstrap`), and **a DRep could only vote on `InfoAction`**:
  ```haskell
  checkBootstrapVotes pp votes
    | hardforkConwayBootstrapPhase (pp ^. ppProtocolVersionL) =
        checkDisallowedVotes votes DisallowedVotesDuringBootstrap $ \gas -> \case
          DRepVoter {} | gasAction gas == InfoAction -> True
          DRepVoter {} -> False
          _ -> isBootstrapAction $ gasAction gas
  ```
  ([Gov.hs](https://github.com/IntersectMBO/cardano-ledger/blob/master/eras/conway/impl/src/Cardano/Ledger/Conway/Rules/Gov.hs))
  CIP-1694's own §Bootstrapping Phase says the same in prose and that the phase "ends when the
  Constitutional Committee and SPOs ratify a subsequent hard fork, enabling the remaining governance
  actions and DRep participation"
  ([CIP-1694](https://github.com/cardano-foundation/CIPs/blob/master/CIP-1694/README.md)).
  `hardforkConwayBootstrapPhase pv = pvMajor pv == natVersion @9` — protocol major **9** only, strict
  equality ([Conway/Era.hs:256-257](https://github.com/IntersectMBO/cardano-ledger/blob/master/eras/conway/impl/src/Cardano/Ledger/Conway/Era.hs)).
- **Preprod crosses the boundary.** From `epoch_params` on Koios preprod (2026-10-19):
  PV8 → **PV9 at epoch 163** (2024-08-24, Chang), **PV10 at epoch 181** (2024-11-22, Plomin),
  **PV11 at epoch 294** (2026-06-10). So **preprod bootstrap = epochs 163–180**, and it contains
  **6 proposals** — 3 `ParameterChange`, 2 `InfoAction`, 1 `HardForkInitiation` — exactly the three
  permitted types, which independently confirms the rule against live data.
- **PV11 (van Rossem) changes nothing here.** It is an intra-Conway protocol bump; the preprod
  enactment date (2026-06-10 00:00 UTC) matches epoch 294 exactly
  ([Intersect](https://intersectmbo.org/news/cardano-upgrade-van-rossem-hard-fork)). No new action
  types, no voter-role changes. The matrix in §3.1 holds for PV10 and PV11 alike.

**(decision, corrected 2026-10-19 by fact-check)** The phase test must be evaluated **over the
action's whole voting window, not at its proposal block**. `checkBootstrapVotes pp votes` is run by
the GOV rule of the **voting** transaction against the pparams in force *then*
([Gov.hs `govTransition`, the `runTest $ checkBootstrapVotes pp knownVotes` line](https://github.com/IntersectMBO/cardano-ledger/blob/master/eras/conway/impl/src/Cardano/Ledger/Conway/Rules/Gov.hs)),
so a PV9-proposed action whose window reaches into PV10 **was** DRep-votable from the hardfork epoch
on. Reading only the proposal's own `block.proto_major` would wrongly drop it from the denominator.

```sql
-- drep_votable(a)   -- epoch_param is populated whenever ledger: "enable" (it is)
a.action_type = 'InfoAction'
OR EXISTS (SELECT 1 FROM epoch_param ep
           WHERE ep.epoch_no BETWEEN a.proposed_epoch AND a.last_votable_epoch
             AND ep.protocol_major <> 9)
```
`<> 9` mirrors the ledger's strict `pvMajor pv == natVersion @9`; a pre-Conway (PV ≤ 8) epoch can
never appear in a gov action's window, so it cannot leak in. `block.proto_major` stays on
`action_window` for display and for the per-action audit trail, but it is **not** the eligibility test.

Preprod does not expose the difference — verified: all 6 bootstrap-era proposals have
`last_votable_epoch ≤ 180` (170, 172, 179, 179, 184 ← `InfoAction`, 180), so the proposal-block test
and the window test give the same 114. **Mainnet does expose it** (Plomin, PV10, landed mid-window for
actions proposed in the epochs before it), which is why the "formulas are network-independent" claim
in §9 only holds with the window form above.

Without any phase test, every DRep registered before Plomin carries 4 extra preprod actions (and, on
mainnet, most of the PV9 ParameterChange/HardFork set) in its denominator for votes it was forbidden
to cast.

### 3.5 Retired, re-registered, and mid-window DReps

| case | decision |
|---|---|
| Retired and re-registered | Eligibility is the **union of registered intervals** (`drep_interval`, one row per register event). Votes cast in a since-ended interval still count — they happened on chain. `age_days` sums the intervals (§1.3). |
| Registered mid-window | Eligible, if `from_epoch <= last_votable_epoch`. The DRep could have voted; it had less time, and `vote_latency_*` is where that shows. Rejected the stricter `from_epoch <= proposed_epoch`: it would silently credit a DRep for actions it ignored while registered. |
| Retired mid-window | Eligible, if `to_epoch >= proposed_epoch`. It was a DRep while the action was live. |
| Retired, and the ledger wiped its votes | **Counts as voted.** When a tx contains `UnRegDRepTxCert`, the ledger deletes that DRep's votes from every live proposal (`Gov.hs`: `cleanupProposalVotes`/`unregisteredDReps`) while db-sync keeps the `voting_procedure` rows. This registry publishes *track record*, not ratification arithmetic: the vote was cast and signed on chain. Flagged per vote as `vote_voided_by_retirement: true` so the discrepancy against GovTool's tallies is explainable, not mysterious. |
| Inactive DRep (`active_until < as_of_epoch`) | **Still eligible, still ranked.** Inactivity does not forbid voting — voting is what re-activates it. Reported as `is_active: false` + `active_until_epoch`, never folded into the rate. |
| Never-registered `drep_hash` row | `drep_hash` rows exist for credentials that merely appear in a delegation; "the existance of an entry doesn't mean the DRep is registered" (schema.md). `/dreps` lists only rows with at least one `drep_interval`. |

### 3.6 Action lifecycle: expired / enacted / dropped / open

| case | decision |
|---|---|
| Expired | **In the denominator.** The DRep had the full 7 epochs. |
| Ratified / enacted | **In the denominator**, with `last_votable_epoch = ratified_epoch` — ratification closes voting earlier than `expiration` (preprod: ratified at proposed+1…+3, enacted the epoch after). |
| Dropped because a dependency was enacted | **In the denominator**, with `last_votable_epoch = dropped_epoch − 1`. 6 of 131 preprod actions; windows of 2–5 epochs. Rejected a minimum-window exclusion rule: it would be a judgment call, and the window length is already visible per action. |
| **Still open** (`last_votable_epoch >= as_of_epoch`) | **Excluded from the denominator** — a DRep cannot be marked absent from a vote that is still running. Surfaced as `open_actions_unvoted`. This is the one judgment I am making in the DRep's favour, and it is the fair one; it also removes the perverse incentive to rush a vote before the metric refreshes. |
| `InfoAction` | **In the denominator.** Info actions can never be ratified (`NoVotingThreshold` → `SNothing`, "no voting threshold prevents ratification", Internal.hs) so they always run the full window — which makes them the cheapest participation to score. Kept anyway: on mainnet the budget info actions are among the most consequential votes, and excluding a whole action type would be exactly the kind of opaque judgment the spec forbids. The per-type breakdown (§1.6) is what exposes an InfoAction-only voter. |

### 3.7 The always-abstain / always-no-confidence pseudo-DReps

**They are real rows in db-sync.** `drep_hash.raw` is nullable
([GovernanceAndVoting.hs](https://github.com/IntersectMBO/cardano-db-sync/blob/master/cardano-db/src/Cardano/Db/Schema/Core/GovernanceAndVoting.hs):
`drepHashRaw :: !(Maybe ByteString)`), and the two reserved rows carry `raw IS NULL` with
`view IN ('drep_always_abstain','drep_always_no_confidence')`. They do get `drep_distr` rows —
measured on preprod epoch 319:

| reserved DRep | `drep_distr.amount` (epoch 319) |
|---|---|
| `drep_always_abstain` | 431,863,790,957,959 lovelace (431.86 M ADA) |
| `drep_always_no_confidence` | 325,390,800,383 lovelace (0.33 M ADA) |
| *all DRep rows, total* | 937,803,900,128,796 lovelace (937.80 M ADA) |

They have no `drep_registration`, no `delegation_vote` target of their own, no votes, and no anchor.

**(decision)**
1. **Not ranked, not listed as DReps.** `/dreps` and every leaderboard add `WHERE dh.raw IS NOT NULL`.
   Equivalently: no `drep_interval` row ⇒ no metrics. Every rate for them is `null`, never `0`.
2. **They must appear in the dashboard's abstain figure** — straight into Track 2's
   `always_abstain_lovelace` / `always_no_confidence_lovelace`, which is the spec's own headline fact
   (§1, "9.75 billion ADA parked on always-abstain").
3. **A profile page exists but is a redirect-grade stub:** `/drep/drep_always_abstain` returns
   `status: "reserved"` (Track 2's `DRepReserved`), the power series, and a one-line explanation —
   no metrics object. Deep links from the dashboard must not 404.
4. They are excluded from every denominator and from `SUM(amount)` whenever that sum is labelled
   "delegated to DReps".

### 3.8 Zero denominators and ties

**(decision)** Every rate is `Maybe Double` and serialises to JSON `null` when its denominator is 0;
the UI renders **"n/a"**, never "0%". Track 2 already typed `participation_rate` and `rationale_rate`
as `Maybe Double`; the same applies to `delegator_retention_rate`, `vote_latency_median_hours`,
`vote_latency_p90_hours`, and every per-type `participation_rate`. A DRep registered in the current
epoch has `eligible_actions = 0` and `participation_rate: null` — that is the normal, correct state
for a new DRep, not a failure.

Ordering, since `null` sorts arbitrarily: `ORDER BY <rate> DESC NULLS LAST, drep_hash_id ASC`
**(decision)** — nulls always last regardless of sort direction, and `drep_hash_id` breaks ties so
pagination is stable. This matters: 101 of the 294 registered preprod DReps have no `drep_distr` row.

### 3.9 `voting_power`: absent row means zero, not unknown

Measured on preprod: of 294 registered DReps, 193 have a `drep_distr` row for epoch 319 and 101 do
not. Sampling 20 of the missing via Koios `drep_info`: **all 20 report `amount = 0`**, and they are a
mix of `active: true` and `active: false` — while DReps *present* in the distribution include 19
inactive ones with non-zero amounts.

**(decision)** Two conclusions, both load-bearing:
1. A missing `drep_distr` row means **zero delegated stake**: `COALESCE(dd.amount, 0)` for any epoch at
   or after the DRep's first distribution epoch, and `null` before it.
2. `drep_distr.amount` is **delegated stake, not ratification-counted stake** — inactive DReps keep
   their amount. So `voting_power` must always ship with `is_active` (§1.7), and the dashboard's
   "active voting stake" is `SUM(amount) WHERE active_until >= epoch_no AND raw IS NOT NULL`, a
   different number from `SUM(amount)`.
3. The series starts the epoch **after** registration: the §4 fixture registered in epoch 173 and its
   first `drep_distr` row is epoch 174 (then unbroken, 147 rows to epoch 320, zero gaps). So
   `voting_power` for the registration epoch itself is `null`, not 0.

---

## 4. Worked example — real preprod numbers, the implementer's fixture

**DRep** `drep1y2qn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qmwj3xg`
(hex `81378d3ea89b68f0fc09944b1f93f2feb851318f196f98ba4029d8a8`, "Bitcows", key-credential).
Chosen because it spans the whole history (registered epoch 173, during the bootstrap phase), has
16 votes across 4 action types with **no re-votes**, has a CIP-119 anchor added later, and has 13
live delegators. Source: Koios preprod `drep_info` / `drep_updates` / `vote_list` / `proposal_list` /
`drep_voting_power_history` / `drep_delegators`, all read 2026-10-19 at **epoch 320**.

Registration history (`drep_updates`):

| action | chain time | epoch | deposit | anchor |
|---|---|---|---|---|
| `registered` (tx `bf0feec1…783bcc`) | 2024-10-16T20:38:10Z | **173** | 500000000 | — |
| `updated` (tx `2a30256e…4bb4e8`) | 2025-04-10T14:38:38Z | 208 | NULL | CIP-119 `…Bitcows%20preprod%20drep.jsonld`, hash `32bc0d81…2d94a` |

Network denominators at epoch 320: **131** proposals total · **119** settled
(`last_votable_epoch < 320`) · **115** settled *and* DRep-votable (the 4 excluded are the bootstrap-era
3 × `ParameterChange` + 1 × `HardForkInitiation`) · **12** still open.

### Expected values

| field | expected | derivation |
|---|---|---|
| `as_of_epoch` | `320` | |
| `age_days` | **`732`** ±1 | one interval, 2024-10-16T20:38:10Z → epoch-320 cursor time. **Corrected 2026-10-19**: the earlier `1463` was double-counted. Preprod epoch 173 starts 2024-10-13 and epoch 320 starts 2026-10-18 (Koios `epoch_info`, 5-day epochs), so the span is ~731–736 days, never 1463. The fixture must therefore **pin the cursor block** (hash + slot + `block.time`), not just `as_of_epoch = 320` — `age_days` is the one metric that moves inside an epoch. |
| `eligible_actions` | **`114`** | 115 settled-and-DRep-votable, minus 1 whose `last_votable_epoch < 173` |
| `votes_cast` | **`16`** | distinct actions; 16 vote rows, 0 re-votes |
| `participation_rate` | **`0.140351`** (14.04 %) | 16 / 114 |
| `rationale_verified` | **`2`** | 2 of its 16 final votes carry an anchor |
| `rationale_rate` | **`0.125`** | 2 / 16 |
| `vote_latency_median_hours` | **`158.04`** | median of first-vote latencies (6.59 days). Re-verified 2026-10-19 from the 16 (vote `block_time` − proposal `block_time`) deltas. |
| `vote_latency_p90_hours` | **`543.16`** | **Corrected 2026-10-19**: `percentile_cont(0.9)` over 16 sorted values interpolates at index 13.5 → `512.35 + 0.5 × (573.98 − 512.35) = 543.165`. The old `512.35` is the `percentile_disc` answer and contradicts §5.5's own `percentile_cont`. Full sorted sample (h): 1.29, 1.34, 50.27, 53.07, 75.32, 76.01, 151.30, 155.68, 160.41, 242.83, 304.53, 385.73, 459.36, 512.35, 573.98, 630.08. |
| `vote_latency_sample` | `16` | |
| `open_actions_unvoted` | **`12`** | all 12 open actions, none voted yet |
| `voting_power` / `voting_power_epoch` | **`3923709956713`** / `320` | `drep_distr.amount`, epoch 320 |
| `delegator_count` | **`13`** | cross-check: Koios `drep_info.live_delegator_count` = 13 |
| `is_active` / `active_until_epoch` | `true` / **`333`** | `drep_distr.active_until` |
| verdicts (final vote) | `yes 7`, `no 8`, `abstain 1` | |

### `votes_by_type`

| action type | `eligible_actions` | `votes_cast` | yes | no | abstain | `participation_rate` |
|---|---:|---:|---:|---:|---:|---|
| `parameter_change` | 11 | 4 | 2 | 1 | 1 | `0.3636` |
| `hard_fork_initiation` | 2 | 1 | 1 | 0 | 0 | `0.5000` |
| `treasury_withdrawals` | 79 | 6 | 1 | 5 | 0 | `0.0759` |
| `no_confidence` | 1 | 0 | 0 | 0 | 0 | `0.0000` |
| `new_committee` | 7 | 0 | 0 | 0 | 0 | `0.0000` |
| `new_constitution` | 1 | 0 | 0 | 0 | 0 | `0.0000` |
| `info_action` | 13 | 5 | 3 | 2 | 0 | `0.3846` |
| **Σ** | **114** | **16** | 7 | 8 | 1 | |

The Σ row is the test that matters: per-type `eligible_actions` must sum to the headline
`eligible_actions`, and per-type `votes_cast` to the headline `votes_cast`. Assert it.

### Zero-denominator fixture

Any DRep whose `from_epoch = 320`: `eligible_actions = 0`, `participation_rate = null`,
`rationale_rate = null`, `vote_latency_median_hours = null`, `delegator_retention_rate = null`,
`age_days` small-but-positive, and it must still render. Pick one at sync time with:
```sql
SELECT drep_hash_id FROM drep_interval WHERE from_epoch >= (SELECT max(epoch_no) FROM block) LIMIT 1;
```

### Pseudo-DRep fixture

`drep_always_abstain` at epoch 319 → `always_abstain_lovelace = 431863790957959`;
`drep_always_no_confidence` → `325390800383`; and it must be **absent** from `/dreps`.

### Network-level fixtures (preprod, epoch 320)

**Corrected 2026-10-19 (fact-check).** The first version of this table came from a 2-page Koios
`vote_list` pull, which silently stops at 2000 rows. Re-measured with per-role filters and
`Prefer: count=exact` (`content-range` header is the authority):

| figure | value |
|---|---|
| distinct DRep voters, all time | **511** (vs **294** currently registered — retired DReps keep their record) |
| DRep vote rows / re-voted pairs / worst pair | **816** / **69** / 20 rows |
| DRep votes carrying an anchor | 65 of 816 = **7.96 %** |
| SPO vote rows / distinct SPO voters / with anchor | **90** / **45** / 0 |
| CC vote rows | **3,243,802** (Koios preprod `vote_list?voter_role=eq.ConstitutionalCommittee`, count=exact). The old `1141` was the truncated slice. This figure is 3 orders of magnitude off, so **do not** put it in the fixture from Koios — re-measure CC on our own db-sync and treat preprod's CC traffic as test spam, not a model of mainnet. |
| proposal lifecycle | enacted 11 · ratified 11 · expired 103 · dropped 108 (6 dropped-only) · open 12 — re-verified, all 131 rows |
| `govActionLifetime` | 7 epochs on preprod (`expiration − proposed_epoch = 7` on all 131). **Network-specific**: mainnet's is 6. |

Lesson for the fixture: every Koios count in this note must be taken from `content-range` with
`Prefer: count=exact`, never from `len(rows)` of a paged fetch. The per-DRep figures above
(16 votes, 7/8/1, 2 anchors, the per-type split) were re-verified against the **complete** 816-row
DRep vote list and are unchanged.

### How the fixture goes into CI **(decision)**

Checked into the new repo as `test/fixtures/preprod-epoch-320-metrics.json` with the table above,
plus the minimal db-sync rows needed to reproduce it. The parity test (F1 check 6) loads it, runs the
**catalog SQL** (§7) against a seeded Postgres, and asserts every expected value. A metric whose
formula changes must change this file in the same commit — that is the regression gate on S2.

---

## 5. Exact SQL per metric

Assumes the three views from §0.3. Parameters: `:drep` (`drep_hash.id`), `:as_of_epoch`.
Response field names are Track 2's.

### 5.1 `participation_rate`, `eligible_actions`, `votes_cast`, `open_actions_unvoted`
```sql
WITH elig AS (
  SELECT a.gov_action_id, a.action_type
  FROM action_window a
  JOIN drep_interval i ON i.drep_hash_id = :drep
   AND i.from_epoch <= a.last_votable_epoch
   AND (i.to_epoch IS NULL OR i.to_epoch >= a.proposed_epoch)
  WHERE a.last_votable_epoch < :as_of_epoch
    AND a.drep_votable                               -- §3.4, computed once in action_window
  GROUP BY a.gov_action_id, a.action_type          -- GROUP BY: a DRep may have several intervals
), voted AS (
  SELECT DISTINCT v.gov_action_id
  FROM drep_vote v JOIN elig e ON e.gov_action_id = v.gov_action_id
  WHERE v.drep_hash_id = :drep
), open_unvoted AS (
  SELECT a.gov_action_id
  FROM action_window a
  WHERE a.last_votable_epoch >= :as_of_epoch
    AND a.drep_votable                               -- §3.4, computed once in action_window
    AND NOT EXISTS (SELECT 1 FROM drep_vote v
                    WHERE v.drep_hash_id = :drep AND v.gov_action_id = a.gov_action_id)
)
SELECT (SELECT count(*) FROM elig)         AS eligible_actions,
       (SELECT count(*) FROM voted)        AS votes_cast,
       CASE WHEN (SELECT count(*) FROM elig) = 0 THEN NULL
            ELSE (SELECT count(*) FROM voted)::float8 / (SELECT count(*) FROM elig) END
                                            AS participation_rate,
       (SELECT count(*) FROM open_unvoted) AS open_actions_unvoted;
```

### 5.2 `rationale_rate` and its four counters
**Corrected 2026-10-19 (fact-check):** `final` must be restricted to `A_voted`, i.e. to the actions
in §5.1's `elig` CTE. The earlier version took every action the DRep ever voted on, so a vote on a
**still-open** action entered the rationale denominator while §5.1's `votes_cast` excluded it — which
makes the "four counters always sum to `votes_cast`" invariant false and the two rates
non-comparable. (It happens to coincide on the §4 fixture, whose 12 open actions are all unvoted.)
The same correction applies to §5.5 (`f`) and §5.6 (`final`): **every** per-DRep sample derives from
`elig`, never from `drep_vote` alone. `elig` is one CTE — `include` it, do not retype it.

```sql
WITH elig AS ( /* §5.1 elig, verbatim */ ),
     final AS (                       -- one row per ELIGIBLE action this DRep voted on
  SELECT v.gov_action_id, v.voting_anchor_id
  FROM drep_vote v
  JOIN elig e ON e.gov_action_id = v.gov_action_id
  WHERE v.drep_hash_id = :drep AND v.seq_last = 1
), graded AS (
  SELECT f.gov_action_id,
         CASE WHEN f.voting_anchor_id IS NULL                 THEN 'absent'
              WHEN d.voting_anchor_id IS NOT NULL             THEN 'verified'
              WHEN e.voting_anchor_id IS NOT NULL             THEN 'unverified'
              ELSE 'pending' END AS grade,
         d.is_valid AS cip100_valid
  FROM final f
  LEFT JOIN off_chain_vote_data       d ON d.voting_anchor_id = f.voting_anchor_id
  LEFT JOIN off_chain_vote_fetch_error e ON e.voting_anchor_id = f.voting_anchor_id
)
SELECT count(*) FILTER (WHERE grade = 'verified')               AS rationale_verified,
       count(*) FILTER (WHERE grade = 'unverified')             AS rationale_unverified,
       count(*) FILTER (WHERE grade = 'pending')                AS rationale_pending,
       count(*) FILTER (WHERE grade = 'absent')                 AS rationale_absent,
       count(*) FILTER (WHERE cip100_valid IS TRUE)             AS rationale_cip100_valid,
       CASE WHEN count(*) = 0 THEN NULL
            ELSE count(*) FILTER (WHERE grade = 'verified')::float8 / count(*) END
                                                                AS rationale_rate
FROM graded;
```

### 5.3 `age_days`
```sql
SELECT GREATEST(0, SUM(EXTRACT(EPOCH FROM (
         LEAST(COALESCE(r.retire_time, c.cursor_time), c.cursor_time) - i.from_time)))) / 86400
         AS age_days
FROM drep_interval i
CROSS JOIN (SELECT max(b.time) AS cursor_time FROM block b WHERE b.epoch_no <= :as_of_epoch) c
LEFT JOIN LATERAL (
  SELECT b2.time AS retire_time
  FROM drep_registration r2 JOIN tx t2 ON t2.id = r2.tx_id JOIN block b2 ON b2.id = t2.block_id
  WHERE r2.drep_hash_id = i.drep_hash_id AND r2.deposit < 0 AND b2.slot_no > i.from_slot
  ORDER BY b2.slot_no LIMIT 1) r ON TRUE
WHERE i.drep_hash_id = :drep;
```
**Corrected 2026-10-19 (fact-check):** the retire lookup is bounded by `b2.slot_no > i.from_slot`,
not `b2.epoch_no >= i.from_epoch`. The epoch form picks up a retirement that happened *earlier in the
same epoch* than the registration that opens the interval (register → retire → re-register inside one
5-day epoch is legal and cheap), producing a negative term that `GREATEST(0, …)` then hides.

### 5.4 `delegator_count`, `delegator_retention_rate`, `delegators_joined/left`
```sql
-- live vote-delegation intervals per stake address, intersected with credential registration
WITH vd AS (
  SELECT dv.addr_id, dv.drep_hash_id, b.epoch_no AS from_epoch,
         LEAD(b.epoch_no) OVER w AS to_epoch
  FROM delegation_vote dv
  JOIN tx    ON tx.id = dv.tx_id
  JOIN block b ON b.id = tx.block_id
  WINDOW w AS (PARTITION BY dv.addr_id ORDER BY b.slot_no, tx.block_index, dv.cert_index)
), live AS (                         -- credential de-registered ⇒ the delegation is gone
  SELECT v.addr_id, v.drep_hash_id, v.from_epoch,
         LEAST(COALESCE(v.to_epoch, 2147483647),
               COALESCE((SELECT min(sd.epoch_no) FROM stake_deregistration sd
                         WHERE sd.addr_id = v.addr_id AND sd.epoch_no >= v.from_epoch),
                        2147483647)) AS to_epoch
  FROM vd v
), at_epoch AS (
  -- :w is retention_window_epochs, bound from the catalog (do NOT inline 10 — §1.4 says it is one
  -- constant; two literals here was the drift this section exists to prevent)
  SELECT e.e, l.addr_id
  FROM (VALUES (:as_of_epoch), (:as_of_epoch - :w)) e(e)
  JOIN live l ON l.drep_hash_id = :drep AND l.from_epoch <= e.e AND l.to_epoch > e.e
)
SELECT count(*) FILTER (WHERE e = :as_of_epoch)                       AS delegator_count,
       count(*) FILTER (WHERE e = :as_of_epoch - :w)                  AS delegator_count_prev,
       (SELECT count(*) FROM (SELECT addr_id FROM at_epoch WHERE e = :as_of_epoch
                              INTERSECT
                              SELECT addr_id FROM at_epoch WHERE e = :as_of_epoch - :w) x)
                                                                      AS delegators_retained,
       (SELECT count(*) FROM (SELECT addr_id FROM at_epoch WHERE e = :as_of_epoch
                              EXCEPT
                              SELECT addr_id FROM at_epoch WHERE e = :as_of_epoch - :w) x)
                                                                      AS delegators_joined,
       (SELECT count(*) FROM (SELECT addr_id FROM at_epoch WHERE e = :as_of_epoch - :w
                              EXCEPT
                              SELECT addr_id FROM at_epoch WHERE e = :as_of_epoch) x)
                                                                      AS delegators_left
FROM at_epoch;
-- delegator_retention_rate = delegators_retained / NULLIF(delegator_count_prev, 0)   (NULL ⇒ "n/a")
```
`2147483647` stands in for "no end" so `LEAST`/range tests stay in `word31type`'s domain. Reconcile
once at sync time: `SUM(epoch_stake.amount)` over `D(d, E)` should track `drep_distr.amount` within a
documented tolerance — that is the honest check that the de-registration rule is being applied.

### 5.5 `vote_latency_median_hours`, `vote_latency_p90_hours`
```sql
WITH elig AS ( /* §5.1 elig, verbatim — the sample is A_voted, not "every vote" */ ),
     f AS (
  SELECT EXTRACT(EPOCH FROM (v.cast_at - a.proposed_at)) / 3600.0 AS h
  FROM drep_vote v
  JOIN elig e          ON e.gov_action_id = v.gov_action_id
  JOIN action_window a ON a.gov_action_id = v.gov_action_id
  WHERE v.drep_hash_id = :drep AND v.seq_first = 1
)
SELECT percentile_cont(0.5) WITHIN GROUP (ORDER BY h) AS vote_latency_median_hours,
       percentile_cont(0.9) WITHIN GROUP (ORDER BY h) AS vote_latency_p90_hours,
       count(*)                                       AS vote_latency_sample
FROM f;    -- percentile_cont over an empty set is NULL — the "n/a" case needs no special casing
```

### 5.6 `votes_by_type`
```sql
WITH elig AS ( /* §5.1 elig */ ),
     final AS (SELECT v.gov_action_id, v.vote FROM drep_vote v
               WHERE v.drep_hash_id = :drep AND v.seq_last = 1)
SELECT t.action_type,
       count(e.gov_action_id)                                   AS eligible_actions,
       count(f.gov_action_id)                                   AS votes_cast,
       count(*) FILTER (WHERE f.vote = 'Yes')                   AS yes,
       count(*) FILTER (WHERE f.vote = 'No')                     AS no,
       count(*) FILTER (WHERE f.vote = 'Abstain')                AS abstain,
       CASE WHEN count(e.gov_action_id) = 0 THEN NULL
            ELSE count(f.gov_action_id)::float8 / count(e.gov_action_id) END AS participation_rate
FROM (VALUES ('ParameterChange'),('HardForkInitiation'),('TreasuryWithdrawals'),('NoConfidence'),
             ('NewCommittee'),('NewConstitution'),('InfoAction')) t(action_type)
LEFT JOIN elig  e ON e.action_type = t.action_type
LEFT JOIN final f ON f.gov_action_id = e.gov_action_id
GROUP BY t.action_type;
```
The `VALUES` list is what guarantees all 7 rows appear even at zero (§1.6).

### 5.7 `voting_power`, dashboard abstain split, active voting stake
```sql
-- per-DRep series. drep_distr covers the lovelace half of the spec's drep_epoch_power (§6);
-- its delegator-count half is NOT in drep_distr ("doesn't show every delegator", schema.md) and is
-- answered on demand by §5.4. So: no projection table, but the two are not the same table.
SELECT dd.epoch_no AS voting_power_epoch, dd.amount AS voting_power, dd.active_until
FROM drep_distr dd WHERE dd.hash_id = :drep ORDER BY dd.epoch_no;

-- dashboard, one pass
SELECT SUM(dd.amount) FILTER (WHERE dh.view = 'drep_always_abstain')        AS always_abstain_lovelace,
       SUM(dd.amount) FILTER (WHERE dh.view = 'drep_always_no_confidence')  AS always_no_confidence_lovelace,
       SUM(dd.amount) FILTER (WHERE dh.raw IS NOT NULL)                     AS drep_delegated_lovelace,
       SUM(dd.amount) FILTER (WHERE dh.raw IS NOT NULL
                                AND dd.active_until >= dd.epoch_no)         AS active_voting_stake_lovelace,
       count(*)       FILTER (WHERE dh.raw IS NOT NULL AND dd.amount > 0)   AS dreps_with_power
FROM drep_distr dd JOIN drep_hash dh ON dh.id = dd.hash_id
WHERE dd.epoch_no = :as_of_epoch;
```

### 5.8 SPO and CC participation (the minimal equivalent pages)
Same shape as §5.1, swapping the role predicate and the registration interval:
- SPO: `voting_procedure.voter_role = 'SPO'`, `pool_voter`; denominator uses `spo_votable` (§3.3) and
  the pool's `pool_update`…`pool_retire` interval. **(decision)** For v1 the SPO interval is simply
  "the pool has at least one `pool_update` at or before `last_votable_epoch` and no `pool_retire`
  that took effect before `proposed_epoch`" — `pool_retire.retiring_epoch` is the effective epoch.
- CC: `voter_role = 'ConstitutionalCommittee'`, `committee_voter`; a member is eligible while
  `as_of_epoch <= committee_member.expiration_epoch` and it has not resigned
  (`committee_de_registration`), matching `isActive` in `votingCommitteeThresholdInternal`.
  **Added 2026-10-19 (fact-check):** from protocol major **11** on, an *unelected* committee member
  may not vote at all —
  `hardforkConwayDisallowUnelectedCommitteeFromVoting pv = pvMajor pv > natVersion @10`
  ([Conway/Era.hs:261-262](https://github.com/IntersectMBO/cardano-ledger/blob/master/eras/conway/impl/src/Cardano/Ledger/Conway/Era.hs)).
  Preprod is on PV11 since epoch 294, so the CC denominator must additionally require membership in
  the *enacted* committee (`committee_member.committee_id` = the enacted `committee`) for actions
  whose window lies in PV11+.
  Note the CC denominator excludes `NoConfidence` and `NewCommittee` entirely (§3.1) — 2 of
  131 preprod actions.

---

## 6. What NOT to compute in v1

This section protects the product's central claim ("zero opaque scores", S5). Each line is a metric
someone will ask for; each is refused for a reason that fits on the line.

| Not built | Why |
|---|---|
| Any **composite / weighted reputation score** | The weights are the opinion; publishing a formula for an opinion does not make it a fact. Spec §10 forbids it. |
| **"Alignment with the majority"**, "voted with the winners" | Rewards conformity and is trivially gamed by voting late; also undefined for the 103 expired preprod actions that never had a majority. |
| **Ratification prediction / "will it pass"** | Needs the SPO default-vote rule (reward account delegated to always-abstain / always-no-confidence, HFI and bootstrap exceptions) from `Conway/Rules/Ratify.hs`; already cut by Track 2 (§0.7) after two such bugs in yaci-store. v1 publishes cast-votes tallies with an explicit `basis` field and reads the outcome from db-sync. |
| **Rationale quality / sentiment / topic** of anchor text | Off-chain text judged by a model is not reproducible and not a fact. We publish the hash verdict and the text; readers judge. |
| **"Independence" / cartel / voting-bloc detection** (vote correlation between DReps) | Correlation is cheap to compute and impossible to interpret honestly — two DReps agreeing on an uncontroversial InfoAction are not a bloc. |
| **Delegator-weighted participation** (participation × voting power) | A composite with a hidden weight. Ship `participation_rate` and `voting_power` side by side and let the reader multiply. |
| **Responsiveness to delegators**, forum/X activity, off-chain presence | Not on chain, not derivable from db-sync, not reproducible. The spec's non-goal "no forum/discussions". |
| **Attendance "streaks"** | A streak is just participation with a fragile, arbitrary reset rule. |
| **A per-DRep "activity score"** from `drep_distr.active_until` | `is_active` is a boolean the ledger already computes; dressing it up as a score adds nothing. |
| **Live (intra-epoch) voting power** | db-sync tracks per-epoch active power only ([db-sync #1951](https://github.com/IntersectMBO/cardano-db-sync/issues/1951)). Already cut by Track 2; every power number carries `voting_power_epoch`. |
| **Participation excluding a "noisy" action type** (e.g. dropping preprod's 79 treasury test actions) | The moment we choose which actions count, the number is a judgment. `votes_by_type` gives the reader the same insight without us picking. |
| **Normalising / z-scoring rates across DReps**, percentile badges | Requires a cohort definition (which DReps? which epochs?) that is itself an opinion. Sorting a column is enough. |

---

## 7. Publishing the formulas so docs cannot drift from code

The spec wants the formulas in the repo, as an MCP resource, and visible in the UI next to each
number (S2). Track 2 already fixed the delivery mechanism — `GET /metrics/formulas` serving one
`file-embed`ed JSON at `resources/metric-formulas.json` (f1-api-contract.md §1.2). **(decision)** Reuse
it unchanged and make it the *single source of truth* by moving one more thing into it: **the SQL**.

```
resources/
  metric-formulas.json        -- the catalog: id, name, prose, denominator, inputs, field, sql_file
  metrics/
    _common.sql               -- the three views of §0.3 + retention_window_epochs
    participation_rate_v1.sql
    rationale_rate_v1.sql
    age_days_v1.sql
    delegator_retention_rate_v1.sql
    vote_latency_median_hours_v1.sql
    votes_by_type_v1.sql
```

```haskell
-- Metrics/Catalog.hs
-- file-embed IS already a dependency (Decentralized-Belt-System.cabal:511), and the house pattern
-- is makeRelativeToProject — see src/lib/mcp-server-lib/MCPServer/Resources.hs:121,132. A bare
-- embedFile resolves against the compile-time CWD and breaks in the Docker build.
import Data.FileEmbed (embedFile, makeRelativeToProject)

catalogJson :: ByteString
catalogJson = $(makeRelativeToProject "resources/metric-formulas.json" >>= embedFile)

metricSql :: [(FilePath, ByteString)]      -- one embedFile per metric, listed explicitly
metricSql =
  [ ("_common.sql",            $(makeRelativeToProject "resources/metrics/_common.sql" >>= embedFile))
  , ("participation_rate_v1.sql", $(makeRelativeToProject "resources/metrics/participation_rate_v1.sql" >>= embedFile))
  -- … one line per .sql file
  ]
```
**Corrected 2026-10-19 (fact-check), two repo-sourced constraints:**
1. **`makeRelativeToProject` is mandatory**, not optional — the only two `file-embed` call sites in
   this codebase both use it, and the `.cabal` carries a comment documenting the exact failure when
   an embedded file is not shipped: *"Docker builds fail with `…/resources/annex-3.md: openFile: does
   not exist`"* (`Decentralized-Belt-System.cabal:14-22`).
2. **Every `.sql` must be listed in `extra-source-files`** (same comment), or `cabal sdist` / the
   Docker build omits it. `cabal-version: 3.0` supports globs there, so one line does it:
   `resources/metrics/*.sql`. Prefer explicit `embedFile` per metric over `embedDir`: `embedDir`
   registers a dependency on the files it *found*, so **adding** a new `.sql` does not invalidate the
   module and CI can pass against a stale embed — which defeats the whole anti-drift point of §7.

One catalog entry:
```json
{
  "formula_id": "participation_rate_v1",
  "name": "Participation rate",
  "field": "participation_rate",
  "expression": "|actions_voted| / |actions_eligible|",
  "denominator": "distinct governance actions whose voting window closed before as_of_epoch, of a type the role may vote on in the action's protocol-version phase, overlapping a registered interval of the actor",
  "null_when": "eligible_actions = 0",
  "inputs": ["gov_action_proposal", "voting_procedure", "drep_registration", "block.proto_major"],
  "edge_cases": [
    "Conway bootstrap phase (block.proto_major <= 9): DReps could only vote on InfoAction",
    "re-votes collapsed to distinct actions; verdict = last vote, latency = first vote",
    "still-open actions excluded from the denominator",
    "votes voided by DRep retirement still count as cast"
  ],
  "sql_file": "participation_rate_v1.sql",
  "spec": "docs/design/f1-metric-formulas.md#11-participation_rate",
  "version": 1
}
```

Why this cannot drift **(decision)**, in four mechanical consequences:

1. **The code executes the published SQL.** `Query/Metrics.hs` looks the statement up by
   `formula_id` from the embedded `metrics/*.sql` and runs it. There is no second copy of the
   arithmetic to fall out of step — if you change the number, you changed the published formula.
   This is the one place where going through a lookup table beats inlining the query.
2. **`GET /metrics/formulas`** serves the catalog **with the SQL text inlined** (`sql` field, resolved
   from `sql_file` at startup). The UI fetches it once and renders expression + denominator + SQL in
   the info popover beside every number. One fetch, no per-metric endpoint.
3. **MCP resource `gov://metrics/formulas`** (and `gov://metrics/{formula_id}`) returns the same bytes
   from the same embedded file — not a hand-written copy. F3 adds the resource, not the content.
4. **CI holds it together** with three tests, all cheap:
   - every `formula_id` referenced by a DTO's `formula_ids` exists in the catalog, and every catalog
     entry's `field` exists on some response DTO (reflection over the Swagger schema);
   - every `sql_file` resolves and parses (`PREPARE` it against the test DB);
   - the §4 fixture reproduces, running **the catalog SQL** against seeded db-sync rows — so a formula
     change that alters a published number fails CI until the fixture is updated in the same commit.

The repo copy is the same file: `docs/metrics.md` is **generated** from the catalog by
`admin write-metrics-doc` (the existing `write-blueprint` pattern), and CI fails if the generated file
differs from the committed one. **(decision)** No hand-written formula prose anywhere else in the
repo, in Swagger descriptions, or in the UI — all three read the catalog.

---

## 8. Open questions for the owner

1. **§0.1 — `shelley.enable: true`.** Costs a slightly larger db-sync; buys honest delegator churn
   (`stake_deregistration`) and an SPO registration interval (`pool_update`/`pool_retire`). I
   recommend the flip. If the owner would rather keep the DB minimal, then
   `delegator_retention_rate` and `spo_participation_rate` must be dropped from v1 (report `null`
   with a documented reason), not approximated. Note the flip is **not** needed for SecurityGroup
   classification — `param_proposal` is populated either way (fact-check, §0.1).
2. **`W = 10` epochs for retention** (§1.4) is a product choice as much as a technical one. 10 epochs
   = 50 days. A reader might expect "since last epoch" or "all time". Easy to change; it is one
   constant in `_common.sql` and one field in the catalog.
3. **`InfoAction` in the participation denominator** (§3.6). I keep it, because excluding a whole
   action type is exactly the opaque judgment the spec forbids — but on preprod it is the type our
   example DRep scores highest on (38 %), and on mainnet info actions are both the most and the least
   consequential votes depending on the action. The per-type table makes either choice legible; say
   the word if the headline should exclude them.

## 9. What I could not determine

- ~~Whether `param_proposal` is populated for Conway gov actions under `shelley.enable: false`.~~
  **Resolved 2026-10-19 by reading db-sync 13.7.2.1 source: yes, it is.** `insertGovActionProposal`
  calls `insertParamProposal` inside `when (ioGov iopts)`
  ([Insert/GovAction.hs:78-85](https://github.com/IntersectMBO/cardano-db-sync/blob/13.7.2.1/cardano-db-sync/src/Cardano/DbSync/Era/Universal/Insert/GovAction.hs));
  the `ioShelley` guard at Insert/Tx.hs:181-183 only covers legacy pre-Conway param updates. See §0.1.
- **The exact tolerance for the `epoch_stake` ↔ `drep_distr` delegator-stake reconciliation** (§5.4).
  Needs the real DB; it belongs in Track 1's parity script, with the measured number documented.
- **Whether preprod's one `expired_epoch`-without-`dropped_epoch` row is a transient state** (the
  action expired in the current epoch and will be dropped next epoch) **or a db-sync quirk.** The
  `last_votable_epoch` formula is correct either way, since `expired_epoch` already bounds it.
- **Mainnet's magnitudes.** Every fixture here is preprod epoch 320. Mainnet denominators, the
  always-abstain figure and the re-vote frequency will all differ. The formulas are
  network-independent **only in the corrected form**: the bootstrap test must scan `epoch_param`
  across the action's window (§3.4), and `govActionLifetime` is 6 on mainnet vs 7 on preprod (§1.4).
  With those two in place F2 needs new fixture numbers, not new formulas.

---

## 10. Fact-check corrections

Adversarial re-verification of every external claim in this note against primary sources
(cardano-db-sync 13.7.2.1 `doc/schema.md`, `doc/configuration.md` **and source**; cardano-ledger
`master` Conway impl; Koios preprod with `Prefer: count=exact`). What changed:

| § | Was | Now | Primary source |
|---|---|---|---|
| §0.1 | `shelley.enable: false` empties `param_proposal`, so SPO SecurityGroup eligibility is uncomputable (conditional **BLOCKER**) | **False.** Conway `ParameterChange` actions write `param_proposal` inside `when (ioGov iopts)`. The flip is still needed, but for `stake_deregistration` (churn) and `pool_update`/`pool_retire` (§5.8 SPO interval). Blocker void. | `Insert/GovAction.hs:78-85`, `Insert/Tx.hs:181-183,198-200`, `Insert/Certificate.hs:73,83,132-138` @ 13.7.2.1 |
| §0.3, §3.4, §5.1 | bootstrap test = `action_window.proposed_proto_major <= 9` | window test: `type = 'InfoAction' OR EXISTS (epoch_param in [proposed_epoch, last_votable_epoch] with protocol_major <> 9)`, computed once as `action_window.drep_votable`. `checkBootstrapVotes` runs against the **voting** tx's pparams, and `hardforkConwayBootstrapPhase` is `== 9`, not `<= 9`. Same answer on preprod (all 6 bootstrap-era actions close by epoch 180 — verified), wrong on mainnet. | `Conway/Era.hs:256-257`, `Conway/Rules/Gov.hs` (`govTransition` → `checkBootstrapVotes`) |
| §0.3 | views created unqualified | created in schema `palmares`; `drep_vote`/`drep_interval` in `public` collide with db-sync's own `drep_*` namespace | — |
| §0.3 | `last_votable_epoch` asserted | **confirmed empirically**: votes land at `epoch = ratified_epoch` (3 cases), at `dropped_epoch − 1` (4 of 4 voted dropped-only), never past `expired_epoch − 1` | Koios preprod `vote_list` ∪ `proposal_list` |
| §3.2 | 807 DRep vote rows, 67 re-voted pairs | **816** rows, **69** pairs (worst pair still 20) | Koios `vote_list?voter_role=eq.DRep`, `content-range: 0-0/816` |
| §4 | `age_days` = `1463` | **`732`** ±1. 1463 is ~2× the real span: epoch 173 starts 2024-10-13, epoch 320 starts 2026-10-18. Fixture must pin the cursor **block**, not just the epoch. | Koios `epoch_info` |
| §4 | `vote_latency_p90_hours` = `512.35` | **`543.16`**. `percentile_cont(0.9)` over 16 points interpolates at index 13.5 between 512.35 and 573.98; `512.35` is `percentile_disc` and contradicts §5.5's own SQL. | recomputed from the 16 real (vote − proposal) `block_time` deltas |
| §4 | network table: 505 voters / 807 rows / SPO 52,18 / CC 1141,232,29 | **511 / 816 / SPO 90,45 / CC 3,243,802**. The old numbers came from a 2-page (2000-row) Koios pull, which is its page cap. | Koios `Prefer: count=exact` per role |
| §5.2 | `final` = every action the DRep ever voted on | `final` joins §5.1 `elig`. Otherwise a vote on a still-open action is in the rationale denominator but not in `votes_cast`, and "the four counters sum to `votes_cast`" is false. Same fix in §5.5 and §5.6. | internal coherence vs §1.2 |
| §5.3 | retire lookup bounded by `b2.epoch_no >= i.from_epoch` | bounded by `b2.slot_no > i.from_slot`; `drep_interval` now exposes `from_slot`. Epoch-granular ordering mis-handles register→retire→re-register inside one epoch. | internal |
| §5.4 | `:as_of_epoch - 10` inlined 4× | `:as_of_epoch - :w`, bound from the catalog's `retention_window_epochs` | internal vs §1.4 |
| §5.7 | "`drep_distr` **is** the spec's `drep_epoch_power`" | it is the lovelace half only; the per-epoch delegator count is not in it and comes from §5.4 | schema.md `drep_distr` |
| §5.8 | CC eligibility = `expiration_epoch` + not resigned | plus: from PV11 (preprod epoch 294+) unelected CC members cannot vote | `Conway/Era.hs:261-262` |
| §7 | `$(embedFile "…")`, `$(embedDir "resources/metrics")` | `makeRelativeToProject … >>= embedFile`, one per file, plus `extra-source-files: resources/metrics/*.sql`. Bare `embedFile` breaks the Docker build (the `.cabal` documents that exact error); `embedDir` lets a newly added `.sql` pass CI against a stale embed. | `MCPServer/Resources.hs:121,132`; `Decentralized-Belt-System.cabal:14-22,511` |
| §8.1, §9 | "could not determine whether `param_proposal` is populated" | resolved from source; item struck | as §0.1 |
| §9 | "the formulas are network-independent by construction" | true only with the corrected window-based bootstrap test and a per-network `govActionLifetime` | §3.4, §1.4 |

**Verified correct, no change needed** (listed so nobody re-checks them): every db-sync table and
column name used in §2 and §5, including `drep_distr.active_until`, `block.proto_major`,
`voting_procedure.index`/`drep_voter`/`pool_voter`/`committee_voter`, the absence of
`voting_procedure.block_id`, `voting_procedure.invalid` being permanently null, and all 11
`param_proposal` SecurityGroup columns; the `off_chain_vote_data` semantics (schema.md says verbatim
"Only data with hash matches are stored here; hash mismatches are stored in
off_chain_vote_fetch_error", and grades `is_valid` TRUE/FALSE/NULL exactly as §1.2 describes) — so
§1.2's central decision is right; the 10 ledger SecurityGroup fields (`Conway/PParams.hs`, every
`THKD ('PPGroups _ 'SecurityGroup)`); the full role×type matrix (`Governance/Internal.hs`
`votingStakePoolThresholdInternal` → `NewConstitution`/`TreasuryWithdrawals` = `NoVotingAllowed`,
`ParameterChange` only via `isSecurityRelevant`; `votingCommitteeThresholdInternal` →
`NoConfidence`/`UpdateCommittee` = `NoVotingAllowed`; DRep all 7; `isVotingAllowed`);
`checkBootstrapVotes` quoted verbatim; `checkVotesAreNotForExpiredActions` = `curEpoch <= gasExpiresAfter`;
the `only_governance` preset really does set `shelley.enable: false`; the whole denominator chain
131 → 119 settled → 115 DRep-votable → **114** eligible and every cell of the §4 `votes_by_type`
table (79/13/11/7/2/1/1 eligible, 6/5/4/0/0/0/1 → 16 cast, yes 7 / no 8 / abstain 1), reproduced
independently; the preprod PV timeline (PV9 @ 163, PV10 @ 181, PV11 @ 294) and the 6 bootstrap-era
proposals; `govActionLifetime = 7` on all 131; 294 registered DReps, 193 with a `drep_distr` row at
epoch 319, 101 without, and 20 of 20 sampled missing DReps reporting `amount = 0` with a mix of
active/inactive; `always_abstain` 431,863,790,957,959 and `always_no_confidence` 325,390,800,383 and
the 937,803,900,128,796 total; the fixture DRep's `voting_power` 3,923,709,956,713,
`active_until` 333, 13 live delegators, registration 2024-10-16T20:38:10Z in epoch 173, update
2025-04-10T14:38:38Z with hash `32bc0d81…2d94a`, and 147 unbroken `drep_distr` rows for epochs
174–320; `vote_latency_median_hours` 158.04.
