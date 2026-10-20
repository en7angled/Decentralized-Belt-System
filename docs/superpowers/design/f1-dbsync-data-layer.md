# F1 — Track 1 design: the governance data layer (cardano-db-sync → our query layer)

Input spec: `docs/superpowers/specs/2026-10-19-gov-registry-spec.md` (§4.1, §4.2, §6, §9 F1 A1–A7).
Date: 2026-10-19. Verified against cardano-db-sync **13.7.2.1** (latest GitHub *release*; `13.7.3.0`
exists as a git tag + docker tag but has no release notes or CHANGELOG entry yet).

Everything below is either (a) quoted from a primary source with its URL, or (b) marked
**(decision)** / **(BLOCKER)**. Where the published docs and the Haskell source disagree, **the source
wins** and the disagreement is called out — this happened three times and two of them change the config.

---

## 0. Headline decisions (read this, then the detail)

| # | Decision | One-line why |
|---|---|---|
| D1 | **No projection tables, no raw log, no replay for the governance layer.** Serve F1 from read-only SQL **views** over db-sync's `public` schema, in our own `gov` schema. | A view is a pure function of db-sync's rows, so the A3 determinism check and the A4 rollback check are satisfied *by construction*; there is no pipeline to be wrong. |
| D2 | **Exactly one materialized view**: `gov.mv_drep_metrics`. Refreshed `CONCURRENTLY` on a tick. | `/dreps` must sort/filter by participation; nothing else needs persistence. |
| D3 | **Delete our own metadata fetcher.** db-sync's off-chain vote fetcher already does fetch + blake2b-256 verify + CIP-100/108/119 parse + typed failure rows + exponential retry. | Whole component removed. We add ~5 lines of SQL for `data:` URIs, which db-sync refuses (see §6). |
| D4 | Config = `preset: "only_governance"` **plus three explicit overrides**: `offchain_vote_data: "enable"`, `shelley: {enable: true}`, `ipfs_gateway` (top-level). | The preset leaves off-chain metadata off and disables stake de-registration (breaks the delegator-count parity check). **Corrected:** `disable_epoch` is *not* a fourth override — the key does not exist at all in **13.7.1.0**, the version D5 actually runs (`parseOverrides` has no `disable_epoch`/`sioEpoch`/`EpochConfig`: verified in `Config/Types.hs` at both tags). It is a 13.7.2.1 key. Keeping the line is harmless (unknown keys are ignored) but it buys nothing — on 13.7.1.0 `public.epoch` is the **legacy cached table**, whose rollup-corruption bugs are exactly what the 13.7.2.1 `epoch_finalized` redesign removed. Treat `public.epoch` as suspect on this image: derive epoch start/end from `block` (`min/max(time)` per `epoch_no`) rather than from `epoch`. |
| D5 | **Image: `ghcr.io/blinklabs-io/cardano-db-sync:13.7.1.0`** (multi-arch, has `linux/arm64`). | The official `ghcr.io/intersectmbo/cardano-db-sync` image is **amd64-only** and the minirack is ARM64-only. See **(BLOCKER-1)**. |
| D6 | **Canonical actor IDs are CIP-129**, computed by us in Haskell from `drep_hash.raw` + `has_script`. db-sync's `drep_hash.view` is **CIP-105** and does not match Koios/GovTool/wallets. | Without this the whole §9 F1 check 6 reconciliation compares different ID spaces. |
| D7 | **DRep eligibility for the participation metric comes from `drep_distr`, not from reconstructed certificate intervals.** | It is the ledger's own per-epoch answer to "was this a DRep in epoch E". One set-based join, zero N+1. (Corrected: the shipped `(hash_id, epoch_no)` index does **not** serve this join — the predicate is on `epoch_no` alone, so we add `gov_idx_drep_distr_epoch`; see §3.4.) |
| D8 | We add our own indexes to db-sync's governance tables, via a one-shot admin SQL job, all named `gov_idx_*`. | db-sync 13.7.2.1 creates **two** indexes on the entire governance schema (§3.4). Every join we need is unindexed. |
| D9 | Readiness/freshness reads `block` by `ORDER BY id DESC LIMIT 1`. `/ready` red at **lag > 300 s**; UI amber at **> 120 s**. A1's "< 120 s sustained" **is** achievable and stays as written. | `block.id` is the PK and this is db-sync's own documented tip query. **Corrected:** preprod's mean block interval is **~22.7 s** (epoch 319: 19 054 blocks / 432 000 s), not 62 s — the earlier figure divided the *in-progress* epoch 320's partial block count by a full epoch. At 22.7 s, `P(gap > 120 s) ≈ 0.5 %` and `P(gap > 300 s) ≈ 2e-6`, so 900 s was 40 block-intervals of blindness for no reason. |
| D10 | Rollback detection reuses the **existing `ChainCursor` shape** (`slotNo` + `headerHash`) from `src/lib/offchain-lib/Storage.hs`. On mismatch we `REFRESH` the one matview. That is the entire rollback story. | Reuse over new code; nothing else we hold is derived state. |

---

## 1. Verified db-sync 13.7.x governance schema

Primary source for every table/column below:
`https://github.com/IntersectMBO/cardano-db-sync/blob/13.7.2.1/doc/schema.md`
(auto-generated from `cardano-db/src/Cardano/Db/Schema/BaseSchema.hs` — the doc header says so).

### 1.1 Concept → table.column mapping

| Our concept | db-sync table.column | Notes (verified) |
|---|---|---|
| DRep credential | `drep_hash(id, raw hash28type NULL, view, has_script)` | `raw IS NULL` for the two pseudo-DReps. `UNIQUE(raw, has_script)`. Existence ≠ registered. |
| always-abstain / always-no-confidence | `drep_hash.view = 'drep_always_abstain'` / `'drep_always_no_confidence'`, `raw IS NULL` | Exact string constants from `cardano-db/src/Cardano/Db/Types.hs:569-573` (`hardcodedAlwaysAbstain`). |
| DRep register / update / retire | `drep_registration(id, tx_id, cert_index, deposit, voting_anchor_id, drep_hash_id)` | **One table for all three.** `deposit > 0` → register, `deposit < 0` → retire, `deposit IS NULL` → update. Latest row per DRep = its state (doc says so verbatim). |
| DRep metadata anchor | `drep_registration.voting_anchor_id` → `voting_anchor` | |
| Metadata anchor (all kinds) | `voting_anchor(id, block_id, data_hash blob, url varchar, type anchorType)` | `type ∈ {gov_action, drep, other, vote, committee_dereg, constitution}`. `UNIQUE(data_hash, url, type)`. `block_id` exists *only* to make rollback possible (doc says so). |
| Fetched + validated anchor content | `off_chain_vote_data(id, voting_anchor_id, hash, language, comment, json jsonb, bytes, warning, is_valid)` | Only **hash-matching** content lands here. `is_valid`: TRUE = valid JSON + CIP-100-conformant; FALSE = valid JSON, not conformant; **NULL = not JSON at all** (and then `json` holds an error message). `UNIQUE(voting_anchor_id, hash)`. |
| CIP-108 proposal metadata | `off_chain_vote_gov_action_data(off_chain_vote_data_id, title, abstract, motivation, rationale)` | |
| CIP-119 DRep metadata | `off_chain_vote_drep_data(off_chain_vote_data_id, payment_address, given_name, objectives, motivations, qualifications, image_url, image_hash)` | `given_name` is the only mandatory field. |
| CIP-100 authors / refs / updates | `off_chain_vote_author`, `off_chain_vote_reference`, `off_chain_vote_external_update` | `off_chain_vote_author.warning` carries witness-verification complaints. |
| Anchor fetch failure | `off_chain_vote_fetch_error(voting_anchor_id, fetch_error, fetch_time, retry_count)` | **Hash mismatches live here, not in `off_chain_vote_data`.** `UNIQUE(voting_anchor_id, retry_count)`. |
| Vote delegation (incl. always-*) | `delegation_vote(id, addr_id, cert_index, drep_hash_id, tx_id, redeemer_id)` | always-abstain/no-confidence are ordinary rows pointing at the two `raw IS NULL` `drep_hash` rows. |
| Delegating stake address | `delegation_vote.addr_id` → `stake_address(id, hash_raw, view, script_hash)` | `stake_address` **is** populated under `only_governance`: `insertDelegationVote` calls `queryOrInsertStakeAddress` unconditionally (`Certificate.hs:419`). |
| Per-epoch DRep voting power | `drep_distr(id, hash_id, amount, epoch_no, active_until)` | **Requires `ledger: enable`.** Indexed `(hash_id, epoch_no)`. One row per DRep per epoch; no per-delegator breakdown (doc says so). |
| Gov action proposal | `gov_action_proposal(id, tx_id, index, prev_gov_action_proposal, deposit, return_address, expiration, voting_anchor_id, type govactiontype, description jsonb, param_proposal, ratified_epoch, enacted_epoch, dropped_epoch, expired_epoch)` | `type ∈ {ParameterChange, HardForkInitiation, TreasuryWithdrawals, NoConfidence, NewCommittee, NewConstitution, InfoAction}`. `expiration` needs the ledger. **CAVEAT (2026-10-19, `f1-yaci-store-evaluation.md` §4.1 C1): `dropped_epoch` is not an independent outcome — upstream `doc/schema.md` line 910 is stale per maintainer (issue #1932). On enactment it stays NULL; on expiry it is `expired_epoch + 1`. Only `dropped_epoch IS NOT NULL` with ratified/enacted/expired all NULL is a genuine "dropped" (pruned sibling) — 6 such rows on preprod. `close_epoch = LEAST(...)` is unaffected.** |
| Gov action lifecycle | the four `*_epoch` columns above | **These are `UPDATE`d in place at epoch boundaries** (`GovAction.hs:419-455`: `updateRatified`, `updateExpired`, `updateDropped`, `insertUpdateEnacted`). Consequence in §3.3 and §4. |
| Vote | `voting_procedure(id, tx_id, index, gov_action_proposal_id, voter_role voterrole, committee_voter, drep_voter, pool_voter, vote vote, voting_anchor_id, invalid)` | `voter_role ∈ {ConstitutionalCommittee, DRep, SPO}`; `vote ∈ {Yes, No, Abstain}`. `committee_voter` is `INT8 → committee_hash.id` (the SQL and the Haskell record agree; `schema.md`'s column order differs from the DDL — use named columns). `invalid` is documented as **always NULL / not implemented**: do not rely on it. |
| Vote rationale anchor | `voting_procedure.voting_anchor_id` → `voting_anchor` (`type='vote'`) | Nullable: votes may carry no anchor. |
| Committee credentials | `committee_hash(id, raw hash28type, has_script)` | `UNIQUE(raw, has_script)`. **No `view` column** — we compute CIP-129 `cc_hot`/`cc_cold` ourselves. |
| CC hot-key authorisation | `committee_registration(tx_id, cert_index, cold_key_id, hot_key_id)` | |
| CC resignation | `committee_de_registration(tx_id, cert_index, cold_key_id, voting_anchor_id)` | Carries an anchor (the resignation rationale). |
| Committee (as proposed/current) | `committee(id, gov_action_proposal_id NULL, quorum_numerator, quorum_denominator)` | `gov_action_proposal_id IS NULL` ⇒ the **genesis** committee. |
| Committee members | `committee_member(id, committee_id, committee_hash_id, expiration_epoch)` | Indexed on `committee_id`. |
| Current committee / constitution per epoch | `epoch_state(id, committee_id, no_confidence_id, constitution_id, epoch_no)` | **`no_confidence_id` is documented as "remains NULL"** — do not use. 13.7.3.0 adds `UNIQUE(epoch_no)`; on 13.7.1.0/13.7.2.1 a rollback across an epoch boundary can leave **duplicate rows** (issue #2155) ⇒ our view must `DISTINCT`/pick max `id`. |
| Constitution | `constitution(gov_action_proposal_id, voting_anchor_id, script_hash)` | |
| Treasury withdrawal (proposed) | `treasury_withdrawal(gov_action_proposal_id, stake_address_id, amount)` | Many rows may share one proposal. |
| Treasury withdrawal (actually paid) + proposal deposit refunds | `reward_rest(addr_id, type rewardtype, amount, earned_epoch, spendable_epoch)`, `type ∈ {reserves, treasury, proposal_refund}` | Inserted under `ioGov` — verified: the `LedgerGovInfo` branch is guarded by `when (ioGov iopts)` and calls `insertProposalRefunds` + `insertRewardRests` (`cardano-db-sync/src/Cardano/DbSync/Era/Universal/Insert/LedgerEvent.hs:131-146` — note the path, not `DbSync/LedgerEvent.hs`), and `insertRewardRests` (`Era/Universal/Epoch.hs:287`) has no `ioRewards` gate ⇒ **available under `only_governance`**. Caveat: `reward_rest` carries no link back to the gov action, so the "paid" LEFT JOIN in `v_treasury_withdrawal` must match on `(stake_address_id, spendable_epoch)` and is **ambiguous when one address receives two withdrawals in one epoch** — label that column "paid (heuristic)" or drop it. This is how we show "enacted, and the money moved". |
| Voting thresholds per epoch | `epoch_param(epoch_no, dvt_*, pvt_*, pvtpp_security_group, committee_min_size, committee_max_term_length, gov_action_lifetime, gov_action_deposit, drep_deposit, drep_activity, …)` | Needs the ledger. All 10 `dvt_*` + 5 pool thresholds are columns. This is the source for "progress against threshold". |
| Chain time (every timestamp we show) | `block(id, hash, epoch_no, slot_no, epoch_slot_no, block_no, time timestamp, …)` + `tx(id, hash, block_id, block_index, invalid_before, invalid_hereafter, …)` | `block.time` is **the** chain clock. This is the fix for inherited defect #5 (`insertedAt`). |
| tx-proof (R1 pattern) | `tx.hash` + `block.block_no`/`slot_no`/`time` | |
| Era / network bookkeeping | `meta(start_time, network_name, version)` | Single row. Use `network_name` as a fail-closed guard (refuse to serve if it is not the configured network). |
| db-sync's own sync state | `schema_version(stage_one, stage_two, stage_three)`, `extra_migrations(token, description)`, `epoch_sync_time(no, seconds, state)`, `reverse_index` | There is **no** "tip"/"progress" table. The tip is `block ORDER BY id DESC LIMIT 1`. `reverse_index` is a rollback-optimisation internal — do not read it. |

### 1.2 Concepts db-sync does **not** give us

| Missing | Consequence / our answer |
|---|---|
| **CIP-129 bech32 IDs** for DRep, CC hot, CC cold, gov action | `drep_hash.view` is CIP-105 (`serialiseDrepToBech32 = serialiseToBech32 "drep"` over the raw 28 bytes — `cardano-db-sync/src/Cardano/DbSync/Util/Bech32.hs`). `committee_hash` has no view at all. `gov_action_proposal` has no bech32 id. **We compute all of them** (§1.3). |
| **Delegator *count* per DRep per epoch** | `drep_distr` has `amount` only. We derive it from `delegation_vote` (+ `stake_deregistration`, which is why D4 turns `shelley` on). |
| **Any index on the governance joins** | §3.4 (D8). |
| A machine-readable rollback event | §4 (D10). |
| `data:` URI anchor content | db-sync refuses non-http(s) URLs. §6. |
| `voting_procedure.invalid` | Documented as never populated. |
| `epoch_state.no_confidence_id` | Documented as always NULL. |

### 1.3 CIP-129 encoding — exact values (verified empirically against Koios preprod **and** against the CIP)

Source: `https://github.com/cardano-foundation/CIPs/blob/master/CIP-0129/README.md`.
Header byte = `(key-type << 4) | credential-type`; credential-type `0b0010` = key hash, `0b0011` = script hash.

| Identifier | HRP | header (key hash) | header (script hash) |
|---|---|---|---|
| CC hot | `cc_hot` | `0x02` | `0x03` |
| CC cold | `cc_cold` | `0x12` | `0x13` |
| DRep | `drep` | `0x22` | `0x23` |
| gov action | `gov_action` | *(no header)* — 32-byte tx id ‖ 1-byte index, 33 bytes total | |

Decoded live from Koios preprod to confirm: `drep1ygpuetne…` → 29 bytes, header `0x22`, tail = the
`hex` field (`has_script=false`); `drep1yvqy2gstt…` → header `0x23` (`has_script=true`);
`cc_hot1qddl5lv…` → `0x03`; `cc_cold1zds4k4q…` → `0x13`;
`gov_action1qnergwfc…` → 33 bytes = `04f2…3f44` ‖ `00` (matches `proposal_tx_hash` + `proposal_index=0`).

**(decision)** Implement in `GovData.Ids` using the **`bech32` package, version 1.1.8, which is already
in `cabal.project.freeze`** (`any.bech32 ==1.1.8`) — so this is a `build-depends` line, not a new
dependency. Export, with Haddock on each:

```haskell
-- | CIP-129 DRep id, e.g. @drep1y…@. 'Bool' is drep_hash.has_script.
drepIdCip129   :: Bool -> ByteString -> Text
-- | CIP-105 DRep id, e.g. @drep1…@ — what db-sync puts in drep_hash.view. Kept only for cross-links.
drepIdCip105   :: ByteString -> Text
ccHotIdCip129  :: Bool -> ByteString -> Text
ccColdIdCip129 :: Bool -> ByteString -> Text
-- | CIP-129 gov action id from (tx.hash, gov_action_proposal.index).
govActionIdCip129 :: ByteString -> Word8 -> Text
-- | Parse any of the above back to (header, 28-byte hash) / (txid, index).
parseDrepIdAny :: Text -> Either Text (Bool, ByteString)  -- accepts CIP-129 and CIP-105
```

`parseDrepIdAny` accepting both forms is deliberate: users paste CIP-105 ids from older tools.

**One runnable check** (house rule: non-trivial logic leaves one check behind) — a golden test with the
four decoded vectors above, asserting round-trip and the exact header byte. Put it in
`UnitTests/GovIds.hs`.

---

## 2. The db-sync configuration

### 2.1 What `only_governance` actually is

The **source** is authoritative and differs from `doc/configuration.md`
(`cardano-db-sync/src/Cardano/DbSync/Config/Types.hs:879-907`):

```haskell
onlyGovInsertOptions = disableAllInsertOptions
  { sioLedger     = LedgerEnable
  , sioGovernance = GovernanceConfig True
  , sioPoolStats  = PoolStatsConfig True
  }
-- disableAllInsertOptions: txCBOR off, txOut DISABLE, ledger DISABLE, shelley DISABLE,
--   rewards False, multiAsset off, metadata off, plutus off, offchainPool off,
--   offchainVote off, jsonType Text, removeJsonb False, sioEpoch = EpochConfig True
```

Three facts the published `doc/configuration.md` "Only Governance" block **omits**, each of which
changes our config:

1. `sioEpoch = EpochConfig True` (the newtype field is `isEpochDisabled`, and
   `isEpochEnabled = not . isEpochDisabled`, `Config/Types.hs:220-224, 436-437`) ⇒ **the `epoch` view
   returns zero rows under this preset.** (`doc/configuration.md#disable-epoch` confirms the semantics:
   "When `disable_epoch` is `true` both views return no rows and `SELECT * FROM epoch` is empty.")
   **13.7.2.1 ONLY** — `sioEpoch`/`EpochConfig`/`disable_epoch` do not exist in 13.7.1.0, so on D5's
   image this paragraph does not apply and `public.epoch` is the pre-redesign cached table (see D4).
2. `sioRewards = RewardsConfig False`, and **`rewards` is not settable from the config file at all** —
   `parseOverrides` has `<*> pure (sioRewards baseOptions)` (`Config/Types.hs:499` at 13.7.2.1,
   `:484` at 13.7.1.0 — verified at both tags), i.e. the field is
   serialised on output but never read on input. It can only come from the preset. It only filters reward
   *ledger events* (`Ledger/State.hs:264,327`) and does **not** disable `epoch_stake` (see §2.4), so this
   costs us nothing — but nobody should waste time trying to override it.
3. `offchain_vote_data` is off, and has defaulted to off **since 13.7.0.1** for every preset including
   `full` (`doc/configuration.md`: "Starting from version 13.7.0.1, this defaults to `disable`").

And one more doc error worth knowing before writing any config: `doc/configuration.md#preset` claims
*"Preset is an aggregate setting that overrides all other properties."* **It is the reverse.**
`instance FromJSON SyncInsertConfig` (`Config/Types.hs:480-490`) reads `preset` into `baseOptions` and
then calls `parseOverrides obj baseOptions`, where every key is `obj .:? "key" .!= <preset value>`.
So **explicit keys override the preset**, which is what makes D4 possible at all. Verified in source;
do not trust the doc sentence.

### 2.2 The exact config we run **(decision)**

`config/db-sync-preprod-gov.yaml` — take `https://github.com/IntersectMBO/cardano-db-sync/blob/13.7.2.1/config/preprod-config.yaml`
verbatim (logging/katip blocks unchanged, `NodeConfigFile` repointed at our node's ConfigMap mount) and
replace the `insert_options` block with exactly this:

```yaml
NetworkName: preprod
NodeConfigFile: /configs/config.json        # the preprod node ConfigMap already mounted for cardano-node
EnableLogging: true
EnableLogMetrics: false
PrometheusPort: 8080

ipfs_gateway:
  - https://ipfs.io/ipfs
  - https://cloudflare-ipfs.com/ipfs

insert_options:
  preset: "only_governance"
  # --- the four overrides (preset is applied first, then these keys) ---
  offchain_vote_data: "enable"   # D3: this IS our metadata fetcher
  shelley:
    enable: true                 # D4: need stake_(de)registration for an honest delegator count
  # NOTE (fact-check): `disable_epoch` was removed from this block. The key only exists from
  # 13.7.2.1; on 13.7.1.0 (D5's image) `parseOverrides` has no such field and the key is
  # silently ignored. Do not depend on `public.epoch` on this image (see D4).
  # --- made explicit so a preset change upstream cannot silently alter us ---
  ledger: "enable"               # MANDATORY: ledger: disable empties drep_distr and nulls expiration
  governance: "enable"
  tx_cbor: "disable"
  tx_out:
    value: "disable"
  multi_asset:
    enable: false
  metadata:
    enable: false
  plutus:
    enable: false
  pool_stat: "enable"
  offchain_pool_data: "disable"
  json_type: "text"
```

Notes on this block, each verified in source:

- `ipfs_gateway` is a **top-level** key, not inside `insert_options` (`Config/Types.hs:456`:
  `o .:? "ipfs_gateway" .!= ["https://ipfs.io/ipfs"]`). Getting this wrong silently keeps the
  single-gateway default rather than erroring.
- `ledger_backend` is omitted deliberately: it defaults to `LedgerBackendInMemory`
  (`Config/Types.hs:458`). **Do not set `"ledger_backend": "lsm"`** — the 13.7.2.1 release notes list
  three open LSM bugs, one of which is *"db-sync could crash due to how ghc handles system threads. This
  only happens on resource-constrained devices such as the Raspberry Pi."*
- `rewards` is intentionally absent: it cannot be set from the file (§2.1 note 2).

CLI (`doc/manual-rollbacks.md`, `doc/command-line-options.md`):

```
cardano-db-sync \
  --config      /config/db-sync-preprod-gov.yaml \
  --socket-path /ipc/node.socket \
  --state-dir   /var/lib/cexplorer/ledger-state \
  --schema-dir  /schema
```

`--state-dir` is **required** because the ledger is enabled; omitting it with `ledger: disable` is an
error, and here the reverse applies.

### 2.3 What each disabled section costs us

| Disabled | Empty / wrong | Does F1 care? |
|---|---|---|
| `tx_out: {value: disable}` | `tx_out`, `ma_tx_out`, `collateral_tx_out` empty; **`tx.fee` is a wrong `0`**; `redeemer.script_hash` NULL | No. We never show tx fees. **If we ever want to show the fee of a vote tx, this is why it reads 0** — write it down in the API docs. |
| `multi_asset: false` | `multi_asset`, `ma_tx_mint` empty | No — our own attestation NFTs come through Kupo, a separate pipeline. |
| `metadata: false` | `tx_metadata` empty | No. Governance carries no tx metadata. |
| `plutus: false` | `script`, `datum`, `redeemer`, `redeemer_data`, `extra_key_witness` empty | **Partly.** A script-DRep's or script-CC-member's script is then not in `script`. We only ever need its *hash*, which we have (`drep_hash.raw` + `has_script`). The §4.3 script-DRep claim is an on-chain concern, not a db-sync one. Accept. |
| `offchain_pool_data: disable` | `off_chain_pool_data` empty — no SPO tickers/names | **Yes, cosmetically.** The SPO-as-voter pages will show pool hashes, not tickers. **(decision)** accept in F1; flip to `enable` in F2 if the SPO pages feel bare. It costs one HTTP fetcher's worth of traffic, nothing structural. |
| `tx_cbor: disable` | `tx_cbor` empty | No. |
| `rewards: false` (from the preset) | reward ledger events dropped ⇒ `reward` stays empty | No — pool rewards are out of scope. `reward_rest` (treasury payouts, proposal refunds) is **unaffected**, it comes from `ioGov`. |
| `shelley: enable` **overridden on** | *adds* `stake_registration`, `stake_deregistration`, `delegation`, `withdrawal`, `param_proposal`, `pool_update/owner/retire/relay` | This is the cost of D4. `delegation` (pool delegations) is the big one on preprod. Measured size: see §7.4. |

### 2.4 `epoch_stake` — the one cost we cannot configure away

`insertStakeSlice` is called **unconditionally** from the block path whenever the ledger is enabled
(`Era/Universal/Block.hs:138`; the `ioShelley` argument inside `insertEpochStake` is an
insert-if-missing cache flag, not a gate). There is no `epoch_stake` switch in `SyncInsertOptions`
(`Config/Types.hs:190-207` lists every option; there is none).

⇒ Under `only_governance` with `ledger: enable`, **`epoch_stake` is populated for every epoch since
Shelley** and will be the largest table in our database by a wide margin, while being 100% unused by F1.

**(decision)** Accept it in F1 and *measure* (§7.4). If preprod disk gets tight, the escape hatch is a
`DELETE FROM epoch_stake WHERE epoch_no < <tip-2>` CronJob — but **do not ship that without first
checking `Era/Universal/Validate.hs:validateEpochStake` behaviour with `--abort-on-panic` unset**, since
db-sync reads that table back for validation. Flag, not blocker.

### 2.5 Image tag **(decision, and see BLOCKER-1)**

```
image: ghcr.io/blinklabs-io/cardano-db-sync:13.7.1.0@sha256:2a341eb32f15a3e65435f4123c3186f467bf2ed9d3ea2eac449592b5a54eb950
```

Verified by querying the ghcr manifest API: this tag is an OCI index with **`linux/amd64` and
`linux/arm64`**. It is the same publisher as the `cardano-node` image this cluster already runs
(`ghcr.io/blinklabs-io/cardano-node:11.1.2@sha256:3620624c…` in
`apps-k8s-configs/chains/cardano/preprod/workloads/statefulset-cardano-preprod.yaml:81`), so the
provenance pattern is already accepted here. Image docs:
`https://github.com/blinklabs-io/docker-cardano-db-sync` ("attempts to keep interface compatibility with
`inputoutput/cardano-db-sync`"; state volume `/var/lib/cexplorer`, node socket `/node-ipc`).

Postgres: `postgres:17-alpine` (official image, multi-arch, matches the spec's §8 pin). The cluster
already proves `postgres:16-alpine` on ARM64 (`apps/bjj-belts/base/workloads/deployment-bjjbelts-postgres.yaml`)
— that is the fallback if 17 misbehaves. **A dedicated StatefulSet, not the existing BJJ Postgres**:
db-sync is a heavy writer and must not share a Pi's page cache with the API database.

Node: the preprod `cardano-node` **already exists in the cluster** at 11.1.2 on backend `V2LSM`
(`apps-k8s-configs/CLAUDE.md:11`: "All three Cardano networks run cardano-node 11.1.2 on backend
`V2LSM`" — the inline comment at `statefulset-cardano-preprod.yaml:80` still says `V2InMemory` and is
stale), with `ogmios` v6.14.0 and a `hydra-node` 2.2.0 sidecar sharing `/ipc/node.socket` via an
`emptyDir` (`statefulset-cardano-preprod.yaml:108-203`). **(decision)** db-sync joins that StatefulSet
as an **additional container in the same pod** so it can read the existing `ipc` emptyDir, rather than
converting `ipc` to a hostPath (which restarts the node and is exactly the pattern the mainnet BP note
in `apps-k8s-configs/CLAUDE.md` warns about). Its ledger-state + Postgres data get their own PVCs.

**Three costs of the sidecar choice that the plan must price in (fact-check):**

1. Adding the container **rolls the StatefulSet**, and so does *every later edit* to db-sync's own
   container or config — restarting `cardano-node`, `ogmios` **and** `hydra-node` (which holds Hydra
   head state) each time. The stated reason for preferring the sidecar was "avoid restarting the node";
   the sidecar does not avoid it, it only moves it from once to every-change. If db-sync config churn
   is expected, a hostPath `ipc` + separate pod is the cheaper shape — the preprod node has no Hydra
   head in production use, so the one-time restart is the smaller bill. **Owner call.**
2. The node's PVC is `storageClassName: local-path`, 100 Gi (`:258-261`), so the pod is pinned to
   whatever node its PV landed on, and db-sync's two new PVCs land on that **same** node. Verify free
   space on that node before provisioning another 100 Gi there.
3. `cardano-node` already requests 3 Gi / limits 8 Gi (`:110-115`). A dedicated Postgres StatefulSet
   (also `local-path`) will schedule on a **different** node, so db-sync's write path crosses the
   cluster network. That is fine for correctness and is a measurable cost in B2's timed sync — but it
   contradicts "must not share a Pi's page cache" only if Postgres actually lands elsewhere; pin it
   deliberately either way instead of leaving it to the scheduler.
No new node, no Mithril bootstrap — **A1's "nod preprod (bootstrap Mithril)" is already done**; the plan
should say so and drop that work item.

---

## 3. Projection design — the core decision

### 3.1 The choice

Options were: (a) query db-sync directly from the Query API, (b) SQL views, (c) materialize our own
projection tables.

**(decision) (b) for everything, with exactly one materialized view. (c) is rejected.**

Determinism argument (A3: "re-running the projections from scratch over the same db-sync produces
identical tables → verify: hash of the table dump, identical across two runs"):
a view has no state, so there is nothing to re-run and nothing to diverge. A3 collapses to hashing the
*output of a query*, which is trivially stable. With (c) we would have to write, test and operate the
very replay machinery the spec itself scopes to the attestation pipeline only (§6: "Bookkeeping (only
the attestation pipeline)").

Rollback argument (A4): db-sync's rollback deletes rows and NULLs the lifecycle epochs **inside the
same transaction** as the rollback. A view observes the post-rollback state immediately, with zero code.
With (c), every rollback needs a detect → wipe → replay cycle over a raw log we would have to keep —
and our raw log would be a *second* copy of db-sync's tables, which is the opposite of
"anyone can re-derive this by running cardano-node + db-sync + this repo".

Laziness argument: this deletes a whole subsystem (governance raw-event log, cursor, idempotent
upserts, replay, schema-version wipe) from the F1 scope.

Cost we accept: read latency depends on Postgres plan quality. That is what D8 (our own indexes) and
D2 (the one matview) buy back.

### 3.2 Physical layout **(decision)**

- One database, db-sync's own (`cexplorer`). Views must join `public.*`, so no FDW, no dblink, no
  second database.
- Schema `gov`, owned by role `gov_owner`. All our objects live there; we never create, alter or drop
  anything in `public` **except** the `gov_idx_*` indexes of §3.4.
- Roles:
  - `dbsync` — db-sync's own role, unchanged. Owns `public`.
  - `gov_owner` — owns schema `gov`; `GRANT SELECT ON ALL TABLES IN SCHEMA public`; runs the matview refresh.
  - `gov_api` — the Query API's runtime role: `USAGE` on `gov`, `SELECT` on `gov.*` and `public.*`, **nothing else**. The public read API never holds a role that can write.
- DDL is applied idempotently at service startup from `GovData.Schema` (`CREATE SCHEMA IF NOT EXISTS`,
  `CREATE OR REPLACE VIEW`, `CREATE … IF NOT EXISTS`), guarded by a `gov.schema_version` singleton row
  — **with one hazard (fact-check):** `gov.mv_drep_metrics` selects from `gov.v_live_delegation`, so
  Postgres records a dependency. `CREATE OR REPLACE VIEW` then cannot add, reorder or retype that
  view's columns, and dropping it needs `CASCADE`, which would silently drop the matview. Either inline
  the live-delegation subquery into the matview, or make the startup path explicitly
  `DROP MATERIALIZED VIEW IF EXISTS … ; CREATE OR REPLACE VIEW … ; CREATE MATERIALIZED VIEW …` when
  `gov.schema_version` is behind. Pick one in the plan; do not discover it on the first schema bump.
  in the same shape as the existing `ChainSyncConfig.schemaVersion`
  (`src/lib/offchain-lib/Storage.hs:102`). No `file-embed`, no `--schema-dir`, no new dependency.
- **Every read happens in a single `REPEATABLE READ` transaction.** db-sync declares `-- noreference` on
  the governance FKs (`cardano-db/src/Cardano/Db/Schema/Core/GovernanceAndVoting.hs:196-205`), i.e. there
  are **no foreign-key constraints** on `voting_procedure`, `gov_action_proposal`, `delegation_vote`,
  `drep_registration`. Reading a composite page across several statements while a rollback is committing
  can therefore observe dangling ids. One snapshot removes the whole class of bug.

### 3.3 The view set

Names are final. `v_` = view, `mv_` = materialized view.

| Object | What it is | Serves |
|---|---|---|
| `gov.v_sync_state` | 1 row: tip block/slot/epoch/time, lag seconds, `max(drep_distr.epoch_no)`, `schema_version.stage_two`, `meta.network_name` | `/ready`, freshness indicator (§5) |
| `gov.v_drep_cert` | `drep_registration` ⋈ `tx` ⋈ `block`, with `cert_kind ∈ {register, update, retire}` and chain time | `drep_update_log` in spec §6 |
| `gov.v_drep` | one row per real DRep: CIP-129 id (joined in Haskell — see note), `has_script`, current status, current anchor + verdict, first/last cert slot & time | `/dreps`, `/drep/{id}` |
| `gov.v_anchor` | `voting_anchor` ⋈ `off_chain_vote_data` ⋈ `off_chain_vote_fetch_error`, collapsed to **one verdict column** (§6.3) | everywhere an anchor is shown |
| `gov.v_gov_action` | `gov_action_proposal` ⋈ `tx` ⋈ `block` ⋈ `v_anchor` ⋈ `off_chain_vote_gov_action_data`, + `proposed_epoch`, `close_epoch`, `outcome`, `withdrawal_total` | `/gov-actions`, `/gov-action/{id}` |
| `gov.v_vote` | `voting_procedure` ⋈ `tx` ⋈ `block` ⋈ `v_anchor`, with the voter resolved to one `(role, credential_raw, has_script)` triple | vote history for every actor |
| `gov.v_vote_latest` | **(fact-check addition)** one row per `(gov_action_proposal_id, voter)` — the vote with the greatest `(block.id, voting_procedure.index)`. Re-votes are real on preprod (up to 5 rows for one DRep on one action) and the ledger counts only the last one. | every tally, every "how did X vote on Y" |
| `gov.v_vote_tally` | per `(gov_action_proposal_id, voter_role, vote)` counts over **`v_vote_latest`, not `v_vote`** — plus DRep-weighted lovelace from `drep_distr` at the action's close epoch | aggregated per-group progress with thresholds |
| `gov.v_vote_delegation` | `delegation_vote` ⋈ `tx` ⋈ `block` ⋈ `stake_address`, + `target_kind ∈ {drep, always_abstain, always_no_confidence}` | delegation discovery |
| `gov.v_live_delegation` | latest delegation per stake address, minus addresses whose latest `stake_deregistration` is newer than their latest `delegation_vote` | delegator counts, abstain-parked-stake dashboard |
| `gov.v_drep_epoch_power` | `drep_distr` ⋈ `drep_hash`, excluding `raw IS NULL` | per-epoch voting power |
| `gov.v_committee_member` | `committee_member` ⋈ `committee_hash` ⋈ `committee` ⋈ `epoch_state`, + `committee_registration`/`committee_de_registration` status | committee pages |
| `gov.v_treasury_withdrawal` | `treasury_withdrawal` ⋈ `stake_address` ⋈ `v_gov_action`, LEFT JOIN `reward_rest` (`type='treasury'`) for "paid" | vendor/treasury pages |
| `gov.v_thresholds` | `epoch_param` restricted to the governance columns | threshold display |
| `gov.v_activity` | `UNION ALL` of (vote, delegation, drep cert, proposal, cc cert) with `(slot_no, block_time, kind, subject)` | `/activity?since=` |
| `gov.mv_drep_metrics` | **the only materialized object** — §3.5 | `/dreps` sort/filter, `/drep/{id}` metrics |

Note on CIP-129 in views: bech32 in PL/pgSQL is not worth writing. Views expose `(raw bytea,
has_script bool)`; the Haskell layer encodes on the way out and decodes on the way in. Filtering by id
therefore passes `(raw, has_script)` as parameters, which is exactly what the indexes want.

### 3.4 Indexes we add **(decision, D8)**

Verified by downloading all 80 files in `schema/` at tag 13.7.2.1 and grepping every `CREATE INDEX`
(80 in total). db-sync creates **two** indexes on the entire governance schema:

```
CREATE INDEX IF NOT EXISTS idx_drep_distr_hash_id_epoch_no ON drep_distr(hash_id, epoch_no);  -- schema/migration-4-0009-20250223.sql
CREATE INDEX IF NOT EXISTS idx_committee_member_committee_id ON committee_member (committee_id); -- schema/migration-3-0004-20240603.sql
```

Nothing else. **Two corrections to the earlier draft:**

- `idx_drep_distr_hash_id_epoch_no` lives in a `migration-4-*` file, i.e. **stage 4**, which
  `doc/schema-management.md:18-22` says "is executed when db-sync has reached 30 mins before the tip of
  the chain". It therefore **does not exist during the initial from-genesis sync**. Any plan step that
  benchmarks a governance query mid-sync must create its own indexes first.
- Stage-3 and stage-4 files are explicitly user-extensible ("These files can be modified or extended by
  users", same doc). Mounting an extra `schema/migration-4-9000-*.sql` through `--schema-dir` is a
  supported alternative to the one-shot Job below; the Job is kept because it is version-independent and
  does not fork db-sync's schema dir. Either is fine — pick one, do not do both. Unique constraints exist on `drep_hash(raw, has_script)`, `voting_anchor(data_hash, url,
type)`, `off_chain_vote_data(voting_anchor_id, hash)`, `off_chain_vote_fetch_error(voting_anchor_id,
retry_count)` — none of which help our joins.

Apply as a one-shot k8s Job running as the table owner (indexes on `public.*` need table ownership),
SQL committed at `sql/gov-indexes.sql`, all `CONCURRENTLY` + `IF NOT EXISTS`, all prefixed `gov_idx_`
so they can never collide with a future `idx_*` from upstream:

```sql
CREATE INDEX CONCURRENTLY IF NOT EXISTS gov_idx_voting_procedure_gap     ON public.voting_procedure (gov_action_proposal_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS gov_idx_voting_procedure_drep    ON public.voting_procedure (drep_voter)  WHERE drep_voter  IS NOT NULL;
CREATE INDEX CONCURRENTLY IF NOT EXISTS gov_idx_voting_procedure_pool    ON public.voting_procedure (pool_voter)  WHERE pool_voter  IS NOT NULL;
CREATE INDEX CONCURRENTLY IF NOT EXISTS gov_idx_voting_procedure_cc      ON public.voting_procedure (committee_voter) WHERE committee_voter IS NOT NULL;
CREATE INDEX CONCURRENTLY IF NOT EXISTS gov_idx_voting_procedure_tx      ON public.voting_procedure (tx_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS gov_idx_drep_registration_drep   ON public.drep_registration (drep_hash_id, tx_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS gov_idx_delegation_vote_drep     ON public.delegation_vote (drep_hash_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS gov_idx_delegation_vote_addr     ON public.delegation_vote (addr_id, tx_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS gov_idx_gov_action_proposal_tx   ON public.gov_action_proposal (tx_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS gov_idx_gov_action_prev          ON public.gov_action_proposal (prev_gov_action_proposal) WHERE prev_gov_action_proposal IS NOT NULL;
CREATE INDEX CONCURRENTLY IF NOT EXISTS gov_idx_ocvd_anchor              ON public.off_chain_vote_data (voting_anchor_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS gov_idx_ocvfe_anchor             ON public.off_chain_vote_fetch_error (voting_anchor_id, retry_count DESC);
CREATE INDEX CONCURRENTLY IF NOT EXISTS gov_idx_treasury_withdrawal_gap  ON public.treasury_withdrawal (gov_action_proposal_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS gov_idx_committee_reg_cold       ON public.committee_registration (cold_key_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS gov_idx_committee_dereg_cold     ON public.committee_de_registration (cold_key_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS gov_idx_stake_dereg_addr         ON public.stake_deregistration (addr_id, tx_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS gov_idx_reward_rest_type         ON public.reward_rest (type, spendable_epoch);
CREATE INDEX CONCURRENTLY IF NOT EXISTS gov_idx_epoch_state_epoch        ON public.epoch_state (epoch_no);
-- fact-check addition: the eligibility join in §3.5 filters drep_distr by epoch_no ALONE, which the
-- shipped (hash_id, epoch_no) index cannot drive (wrong leading column), and which does not exist at
-- all until stage 4 runs. This is the index D7 actually needs.
CREATE INDEX CONCURRENTLY IF NOT EXISTS gov_idx_drep_distr_epoch         ON public.drep_distr (epoch_no, hash_id);
```

These are additive and survive db-sync migrations (its migrations only add). They must be **re-applied
after any `--rollback-to-slot` + resync**: nothing drops them, but note it in the runbook anyway.

### 3.5 The window metric without N+1 — `gov.mv_drep_metrics`

The hard metric is **participation rate = actions voted on / actions the DRep was eligible to vote on**.
The naive shape is one query per DRep. The set-based shape, in one statement:

**Eligibility (decision, D7).** A DRep `D` is eligible for action `A` iff `A`'s voting window is
*closed* and `D` has a `drep_distr` row in any epoch in `[A.proposed_epoch, A.close_epoch]`. We use
`drep_distr` rather than reconstructing registration intervals from `drep_registration` because
(i) it is the **ledger's own** per-epoch answer to "was this a DRep in epoch E", which is exactly the
reproducibility property the spec demands, and (ii) `(hash_id, epoch_no)` is the one index db-sync
ships, so the join is index-driven.

Definitions, all published alongside the metric:

- `proposed_epoch(A)` = `block.epoch_no` of `A`'s proposing tx.
- `close_epoch(A)` = `LEAST(A.ratified_epoch, A.expired_epoch, A.dropped_epoch)`. Postgres `LEAST`
  ignores NULLs. `enacted_epoch` is deliberately excluded — enactment happens *after* ratification and
  is not a voting deadline.
- **Open actions are excluded from the denominator.** A DRep is not penalised for not having voted on an
  action that is still live.
- The denominator is `eligible_pairs ∪ voted_pairs`. The union is not cosmetic: `drep_distr` snapshots
  at the epoch boundary, so a DRep that registers *and* votes inside epoch E has no `drep_distr` row for
  E. Without the union, participation could exceed 1.
- No action-type filter: under CIP-1694 DReps vote on all seven types, including `InfoAction`.

```sql
CREATE MATERIALIZED VIEW gov.mv_drep_metrics AS
WITH action AS (
  SELECT gap.id                                                          AS gap_id,
         b.epoch_no                                                      AS proposed_epoch,
         b.time                                                          AS proposed_time,
         LEAST(gap.ratified_epoch, gap.expired_epoch, gap.dropped_epoch) AS close_epoch
  FROM public.gov_action_proposal gap
  JOIN public.tx    t ON t.id = gap.tx_id
  JOIN public.block b ON b.id = t.block_id
),
closed AS (SELECT * FROM action WHERE close_epoch IS NOT NULL),
dvote AS (                                        -- every DRep vote ever, with chain time
  SELECT vp.drep_voter        AS drep_hash_id,
         vp.gov_action_proposal_id AS gap_id,
         vp.vote,
         vp.voting_anchor_id,
         b.time               AS vote_time
  FROM public.voting_procedure vp
  JOIN public.tx    t ON t.id = vp.tx_id
  JOIN public.block b ON b.id = t.block_id
  WHERE vp.voter_role = 'DRep' AND vp.drep_voter IS NOT NULL
),
eligible AS (
  SELECT DISTINCT dd.hash_id AS drep_hash_id, c.gap_id
  FROM closed c
  JOIN public.drep_distr dd ON dd.epoch_no BETWEEN c.proposed_epoch AND c.close_epoch
  JOIN public.drep_hash  dh ON dh.id = dd.hash_id AND dh.raw IS NOT NULL   -- drop always-abstain/NC
  UNION
  SELECT v.drep_hash_id, v.gap_id
  FROM dvote v JOIN closed c USING (gap_id)
),
participation AS (
  -- NB (fact-check): count(DISTINCT ...) is MANDATORY. A DRep may vote repeatedly on the same action
  -- and db-sync keeps every voting_procedure row; measured on preprod 2026-10-19, 75 of the first
  -- 1000 (voter, proposal) pairs from Koios /vote_list have 2-5 rows. With plain count(*) the LEFT
  -- JOIN fans out and inflates BOTH counts, skewing participation_rate towards 1.
  SELECT e.drep_hash_id,
         count(DISTINCT e.gap_id)        AS eligible_closed_actions,
         count(DISTINCT v.gap_id)        AS voted_closed_actions
  FROM eligible e
  LEFT JOIN dvote v USING (drep_hash_id, gap_id)
  GROUP BY e.drep_hash_id
),
rationale AS (
  SELECT v.drep_hash_id,
         count(*)                                                              AS total_votes,
         count(*) FILTER (WHERE v.voting_anchor_id IS NOT NULL)                AS votes_with_anchor,
         count(*) FILTER (WHERE va.url LIKE 'data:%')                          AS votes_inline_rationale,
         count(*) FILTER (WHERE EXISTS (SELECT 1 FROM public.off_chain_vote_data o
                                        WHERE o.voting_anchor_id = v.voting_anchor_id))
                                                                               AS votes_resolved_rationale,
         min(v.vote_time)                                                      AS first_vote_time,
         max(v.vote_time)                                                      AS last_vote_time,
         avg(EXTRACT(EPOCH FROM (v.vote_time - a.proposed_time)))::bigint      AS avg_vote_latency_seconds
  FROM dvote v
  JOIN action a ON a.gap_id = v.gap_id
  LEFT JOIN public.voting_anchor va ON va.id = v.voting_anchor_id
  GROUP BY v.drep_hash_id
),
power AS (
  SELECT hash_id AS drep_hash_id,
         max(epoch_no)                                        AS latest_power_epoch,
         (array_agg(amount ORDER BY epoch_no DESC))[1]         AS latest_power_lovelace,
         count(*)                                             AS epochs_with_power
  FROM public.drep_distr GROUP BY hash_id
),
deleg AS (
  SELECT drep_hash_id, count(*) AS live_delegator_count
  FROM gov.v_live_delegation GROUP BY drep_hash_id
)
SELECT dh.id                                           AS drep_hash_id,
       dh.raw                                          AS drep_raw,
       dh.has_script,
       coalesce(p.eligible_closed_actions, 0)          AS eligible_closed_actions,
       coalesce(p.voted_closed_actions, 0)             AS voted_closed_actions,
       CASE WHEN coalesce(p.eligible_closed_actions,0) = 0 THEN NULL
            ELSE round(p.voted_closed_actions::numeric / p.eligible_closed_actions, 6) END
                                                       AS participation_rate,
       coalesce(r.total_votes, 0)                      AS total_votes,
       coalesce(r.votes_with_anchor, 0)                AS votes_with_anchor,
       coalesce(r.votes_inline_rationale, 0)           AS votes_inline_rationale,
       coalesce(r.votes_resolved_rationale, 0)         AS votes_resolved_rationale,
       CASE WHEN coalesce(r.total_votes,0) = 0 THEN NULL
            ELSE round((coalesce(r.votes_resolved_rationale,0) + coalesce(r.votes_inline_rationale,0))::numeric
                       / r.total_votes, 6) END          AS rationale_rate,
       r.avg_vote_latency_seconds,
       r.first_vote_time, r.last_vote_time,
       pw.latest_power_epoch, pw.latest_power_lovelace, pw.epochs_with_power,
       coalesce(d.live_delegator_count, 0)             AS live_delegator_count
FROM public.drep_hash dh
LEFT JOIN participation p  ON p.drep_hash_id  = dh.id
LEFT JOIN rationale     r  ON r.drep_hash_id  = dh.id
LEFT JOIN power         pw ON pw.drep_hash_id = dh.id
LEFT JOIN deleg         d  ON d.drep_hash_id  = dh.id
WHERE dh.raw IS NOT NULL;

CREATE UNIQUE INDEX gov_idx_mv_drep_metrics_pk ON gov.mv_drep_metrics (drep_hash_id);  -- required by CONCURRENTLY
CREATE INDEX gov_idx_mv_drep_metrics_part      ON gov.mv_drep_metrics (participation_rate DESC NULLS LAST);
CREATE INDEX gov_idx_mv_drep_metrics_power     ON gov.mv_drep_metrics (latest_power_lovelace DESC NULLS LAST);
CREATE INDEX gov_idx_mv_drep_metrics_raw       ON gov.mv_drep_metrics (drep_raw, has_script);
```

Cost shape: the `eligible` CTE is one hash/merge join producing `|closed actions| × |DReps live in that
window|` pairs. Preprod today: 131 actions × ≤195 DReps ⇒ **≤ 26 k pairs**. Mainnet projection is low
millions — still one set-based pass, which is the whole point.

**Refresh (decision).** `REFRESH MATERIALIZED VIEW CONCURRENTLY gov.mv_drep_metrics` driven by
`GovData.Refresh`, on the cheapest trigger that is correct:

- **every 60 s if the tip advanced**, i.e. `v_sync_state.block_no` changed since the last refresh; and
- **unconditionally once** when `v_sync_state.epoch_no` changes (the lifecycle `*_epoch` columns and
  `drep_distr` only move at epoch boundaries, and that is when the metric actually changes); and
- **unconditionally once** on rollback detection (§4).

`CONCURRENTLY` keeps readers unblocked and needs the unique index above. The refresh is idempotent, so a
missed tick is self-healing — no cursor, no catch-up logic.

### 3.6 Query API surface (reuse)

New module `src/exe/query-api/Query/Governance.hs`, sibling to the existing `Query/Projected.hs` /
`Query/Live.hs` split, reusing `Query/Common.hs` verbatim: `normalizeLimitOffset` (default 100, clamp
[1,500]), `normalizeOrder`, `applyFilterOrderLimit`. Response DTOs go in
`DomainTypes/Transfer/QueryResponses.hs`, filters + `*FromParams` in `Transfer/Filters.hs`, sort enums in
`Transfer/OrderBy.hs` — the table in the root `CLAUDE.md` already prescribes this and it needs no change.
Composite screens stay in `Query/Pages.hs`; `/pages/*` is what keeps the React frontend from doing N+1.

Access: the whole governance read path uses the `gov_api` role through its own connection pool, separate
from the attestation pool. New library: `src/lib/govdata-lib/` with `GovData.Schema`, `GovData.Query`,
`GovData.Ids`, `GovData.Refresh`. It depends on `persistent-postgresql` + `esqueleto` (already in
`Decentralized-Belt-System.cabal`) and uses `rawSql` for the view reads — Persistent entities are the
wrong tool for views we do not own and never migrate.

---

## 4. Rollback and consistency

### 4.1 What db-sync does on a rollback — verified

Source: `doc/manual-rollbacks.md` at 13.7.2.1, plus `cardano-db/src/Cardano/Db/Statement/Base.hs`
and `cardano-db/src/Cardano/Db/Statement/Rollback.hs`.

**db-sync deletes rows. There is no reversible/append-only log.** `deleteTablesAfterBlockId` issues real
`DELETE`s. The governance tables explicitly rolled back are `gov_action_proposal`, `voting_procedure`,
`committee_registration`, `committee_de_registration`, `drep_registration`, `treasury_withdrawal`,
`param_proposal`, `voting_anchor`, `off_chain_vote_data`, `off_chain_vote_fetch_error` and the other
off-chain vote child tables, plus `delegation_vote` under "Stake and Delegation Tables"
(`doc/manual-rollbacks.md`).

Three mechanisms beyond plain deletes, all in `Base.hs:687-845`:

1. **Epoch-scoped deletes**: `DELETE FROM drep_distr WHERE epoch_no > $1` (plus `reward_rest`,
   `pool_stat`, `epoch_param`). Log line in the source: *"Rollback - Using epoch N for deletion
   (DrepDistr: epoch_no > N)"*.
2. **Un-doing in-place updates**: `setNullEnactedStmt`, `setNullRatifiedStmt`, `setNullDroppedStmt`,
   `setNullExpiredStmt` set `gov_action_proposal.{enacted,ratified,dropped,expired}_epoch` back to NULL
   for epochs beyond the rollback point. **This is the mechanism that makes lifecycle columns mutable,
   and it is why a "max id seen" watermark is not sufficient to detect staleness.**
3. **FK-nulling** for `committee` and `constitution` (`prepareQueryThenNull @SCG.Committee
   "gov_action_proposal_id" …`), while `treasury_withdrawal` rows are deleted outright.

**Effectively append-only (never rolled back):** `drep_hash`, `committee_hash`, `stake_address`,
`pool_hash`, `multi_asset`. These are credential caches; an entry's existence means "seen once", never
"currently registered". Our `v_drep` must therefore derive status from `drep_registration`, never from
the presence of a `drep_hash` row — the doc says this outright: *"The existance of an entry doesn't mean
the DRep is registered."*

`epoch_finalized` (the table behind the `epoch` view, new in 13.7.2.1 — **it does not exist on D5's
13.7.1.0 image**, where `epoch` is still the legacy cached table; `schema.md` is byte-identical between
the two tags and documents neither) rolls back via
`DELETE FROM epoch_finalized WHERE no > $rollback_epoch`. Source for the redesign: the 13.7.2.1 release
notes ("the `epoch` interface is now a view over an append-only `epoch_finalized` table and a live
`epoch_current` view") plus `doc/configuration.md#disable-epoch`. We depend on neither.

### 4.2 What that means for us

Nothing, for the views — they are recomputed on read. The only derived state we hold is
`gov.mv_drep_metrics`, and `REFRESH` rebuilds it from scratch. So:

**(decision)** The whole governance rollback story is: *detect → refresh the one matview → re-publish
freshness*. No wipe, no replay, no raw log, no orphan rows possible (a view cannot have orphans).

### 4.3 Rollback detection **(decision, D10)**

Reuse the existing cursor shape from `src/lib/offchain-lib/Storage.hs:89-96` — `ChainCursor { slotNo,
headerHash }` — as a `gov.cursor` singleton row with the same two columns. Each refresh tick:

```sql
-- 1. read our watermark
SELECT slot_no, header_hash FROM gov.cursor WHERE singleton;
-- 2. ask db-sync whether that block is still on its chain
SELECT 1 FROM public.block WHERE slot_no = $1 AND hash = decode($2,'hex');
```

Empty result ⇒ **db-sync rolled back past our watermark**. Also treat "tip epoch < our stored tip epoch"
as a rollback (covers a `--rollback-to-slot` operator action). Action in both cases: refresh the matview,
re-read the tip, store the new `(slot_no, hash)`.

Because the lifecycle columns mutate in place without any row being added, we **cannot** use "max(block.id)
unchanged ⇒ nothing changed" as a staleness test. The epoch-change trigger in §3.5 is what covers it.

`/ready` must additionally report a rollback that is still being processed: db-sync rollbacks hold a long
transaction, so our `REPEATABLE READ` reads stay consistent throughout and nothing special is needed —
but a rollback larger than 10 000 blocks can take minutes (`doc/manual-rollbacks.md`: "Large rollbacks
(> 10000 blocks): May take longer"), during which the tip appears stale and `/ready` will correctly go red.

### 4.4 Attestation pipeline — unchanged

Our own policy's UTxOs keep the existing Kupo chain-sync: `ChainCursor`, `OnchainMatchEvent` raw log,
ordered replay, `rollbackTo` wiping every projection table before replay, `chainSyncTableNames` for the
schema-version wipe (`src/lib/offchain-lib/Storage.hs`, `src/lib/offchain-lib/CLAUDE.md`). Zero new
technology on that path, exactly as spec §4.1 says. The two pipelines share a Postgres *server* at most,
never a schema.

---

## 5. Readiness and freshness

### 5.1 The view

```sql
CREATE OR REPLACE VIEW gov.v_sync_state AS
SELECT b.block_no,
       b.slot_no,
       b.epoch_no,
       b.hash                                                   AS tip_hash,
       b.time                                                   AS tip_time,
       -- block.time is `timestamp` WITHOUT time zone (schema.md: "The block time (UTCTime)").
       -- `now()` is timestamptz, so `now() - b.time` is resolved via the SESSION TimeZone and is
       -- wrong by the offset on any non-UTC session. Pin it explicitly. (fact-check)
       (now() AT TIME ZONE 'UTC' - b.time)                      AS lag_interval,
       EXTRACT(EPOCH FROM (now() AT TIME ZONE 'UTC' - b.time))::bigint AS lag_seconds,
       (SELECT max(epoch_no) FROM public.drep_distr)            AS drep_distr_epoch,
       (SELECT max(stage_two) FROM public.schema_version)        AS dbsync_schema_stage_two,
       (SELECT network_name FROM public.meta LIMIT 1)            AS network_name
FROM public.block b
ORDER BY b.id DESC
LIMIT 1;
```

`ORDER BY b.id DESC LIMIT 1` — this is the exact shape db-sync's own doc uses to find the tip
(`doc/manual-rollbacks.md`, "Verifying Rollback Success"), and `block.id` is the primary key.
**Correction to the earlier rationale:** `block.time` is *not* unindexed —
`schema/migration-4-0002-20200810.sql` creates `idx_block_time ON block(time)`. The reason to prefer
`ORDER BY id` is therefore not a missing index but that stage-4 indexes only exist once db-sync is
within 30 min of tip (`doc/schema-management.md:18`), i.e. exactly not during the sync when the probe
matters most. The official "how far behind" one-liner
`select now() - max(time) as behind_by from block;` (`doc/interesting-queries.md:112`) is the same
quantity but seq-scans; keep it in the runbook for humans, not in the probe.

### 5.2 Thresholds **(decision, D9)**

`/ready` on query-api returns 503 unless **all** of:

1. Postgres reachable (reuse `verifyProjectionDbConnection` — it lives in
   `src/exe/query-api/QueryAppMonad.hs:96`, not in `Query/ServiceStatus.hs`; `ServiceStatus.hs:32` is
   `getReadyProbe`, which merely calls it).
2. `lag_seconds <= 300`.
3. `drep_distr_epoch >= epoch_no - 1`.
4. `network_name = <configured network>`.

Why 300 s **(corrected — the earlier 900 s rested on a bad measurement)**: the first draft read
6 920 blocks out of the *in-progress* epoch 320 and divided by a full 5-day epoch, giving a phantom
~62 s interval. The real figure, from the last **completed** epoch, is **~22.7 s**: Koios
`epoch_info?_epoch_no=319` returns `blk_count = 19 054` over `end_time - start_time = 432 000 s`
(re-measured 2026-10-19; epoch 320 was at 7 017 blocks and still open). Modelling gaps as exponential
with mean 22.7 s: `P(gap > 120 s) ≈ 0.5 %`, `P(gap > 300 s) ≈ 2e-6`. So **300 s is the probe, 120 s is
the UI's amber band**, and A1's "< 120 s sustained" stands as written — do **not** weaken it to a soak
p50. 900 s would have been 40 block-intervals of undetected staleness.

Check 3 is the cheap, specific guard against the single worst misconfiguration this design has: someone
setting `ledger: disable`, which silently empties `drep_distr` and NULLs `gov_action_proposal.expiration`
while every other table keeps filling.

### 5.3 Freshness indicator for the explorer

`GET /protocol-status` (existing pattern) returns `v_sync_state` verbatim plus a
`freshness ∈ {fresh, lagging, stale}` derived as `lag_seconds <= 120 | <= 300 | > 300`, and
`matview_refreshed_at` from `gov.cursor`. The UI shows the **chain time of the tip**, never
`now()` — this is the inherited-defect-#5 discipline applied to the explorer chrome as well.

---

## 6. Metadata anchors — the laziness call

### 6.1 What db-sync already does (verified in source, not inferred)

`cardano-db-sync/src/Cardano/DbSync/OffChain/Http.hs` at 13.7.2.1:

- **Hash verification**: `Crypto.digest (Proxy :: Proxy Crypto.Blake2b_256) bs`, compared to the anchor's
  `data_hash`; mismatch ⇒ `OCFErrHashMismatch` (lines 133-138). This is precisely the
  "verificare blake2b-256" the spec asked us to build.
- **Size limit**: vote metadata is fetched with `httpGetBytes manager request 3000000 3000000` ⇒ a
  **3 MB** read-and-max cap, `OCFErrDataTooLong` beyond it (line 114).
- **Content-type policing**: accepts `application/json`, `application/ld+json`, `text/plain`,
  `*/octet-stream`, `application/binary`, and tolerates `text/html` only if the body starts with `{`;
  otherwise `OCFErrBadContentTypeHtml` / `OCFErrBadContentType`.
- **IPFS**: `ipfs://` is rewritten through the configurable `ipfs_gateway` list (default
  `["https://ipfs.io/ipfs"]`, `Config/Types.hs:456`), trying each in turn; all-failed ⇒
  `OCFErrIpfsGatewayFailures`.
- **SSRF hardening**: HTTPS required in production, private/loopback/link-local blocked, DNS resolved
  immediately before the request; relaxable only with the `--allow-private-offchain-urls` test flag
  (`doc/configuration.md#offchain-pool-data`, `#bypassing-the-url-restriction-for-local-cluster-testing`).
- **CIP-100/108/119 parsing into typed tables**, with the three-state `is_valid` semantics quoted in §1.1.
- **Author witness checking**: `off_chain_vote_author(witness_algorithm, public_key, signature, warning)`.
- **Exponential retry, capped at 24 h**: `OffChain/FetchQueue.hs:retryAgain` —
  `min(86400, 30 + 2^n * 60)` seconds, and `n >= 5` ⇒ flat 24 h. `off_chain_vote_fetch_error.retry_count`
  and `.fetch_time` are persisted.

That is the entire spec §4.1 "Metadata fetcher" bullet — fetch with timeout and size limit, blake2b-256
verification, content + status storage, exponential retry, permanently re-derivable cache — already
implemented, already audited by the ecosystem, already rolled back correctly.

### 6.2 Decision

**(decision, D3) Delete the metadata fetcher from the F1 scope.** We set `offchain_vote_data: "enable"`
and read db-sync's tables. This removes an HTTP client, an SSRF allow/deny policy, a retry scheduler, a
content cache, a hash verifier and their tests from the project.

What we give up, stated honestly:

| Gap | Severity | Our answer |
|---|---|---|
| **`data:` URIs are rejected.** `parseOffChainUrl` requires an `http(s)://` prefix (line 227) ⇒ `OCFErrUrlParseFail`, so the anchor lands in `off_chain_vote_fetch_error`. **Preprod has real `data:` rationales** — e.g. `meta_url: "data:text/plain;utf8,Approve parameter change action on preprod testnet"` on a live vote. | Would wrongly depress every rationale-rate metric. | **5 lines of SQL, no fetcher**: the content *is* the URL. `v_anchor` classifies `url LIKE 'data:%'` as verdict `inline` and exposes the payload after the first comma; `mv_drep_metrics.votes_inline_rationale` counts them (§3.5). Hash verification is vacuous for a `data:` URI. |
| No `fetched_at` on **successful** fetches (`off_chain_vote_data` has no timestamp column; only the error table has `fetch_time`). | Cosmetic. | Report "resolved" without a timestamp; show `fetch_time`/`retry_count` only for failures. |
| Hash-mismatch verdicts are text in `off_chain_vote_fetch_error.fetch_error`, not an enum. | **Was Medium, not Low** — the first draft of §6.3 matched constructor names that are never written, so every failure would have read `unreachable`. | `v_anchor` pattern-matches the actual `show` output (§6.3, corrected). The golden test is **required**, not optional: assert the three live preprod strings (`Hash mismatch when fetching…`, `…Size error, fetching metadata…`, `…URL parse error for…`). |
| 13.7.1.0/13.7.2.1 have a known bug: **duplicate off-chain vote metadata child rows** (`off_chain_vote_author`, `_reference`, `_external_update`) — release notes "Known issues", fixed by PR #2137 in the unreleased line. | Medium: duplicated authors/references in the UI. | `SELECT DISTINCT` in the views that read those three tables. Remove the `DISTINCT` after upgrading past 13.7.2.1 — and leave a `ponytail:` comment saying exactly that. |

### 6.3 The one verdict column

Spec §4.1 wants `valid | hash_mismatch | unreachable | oversized`. `gov.v_anchor` produces exactly that
plus two extras the data demands:

```sql
CREATE OR REPLACE VIEW gov.v_anchor AS
SELECT va.id                                     AS voting_anchor_id,
       va.url, va.data_hash, va.type             AS anchor_type,
       CASE
         WHEN va.url LIKE 'data:%'                      THEN 'inline'
         WHEN d.id IS NOT NULL AND d.is_valid           THEN 'valid'
         WHEN d.id IS NOT NULL AND NOT d.is_valid       THEN 'valid_hash_bad_schema'
         WHEN d.id IS NOT NULL                          THEN 'valid_hash_not_json'   -- is_valid IS NULL
         -- fact-check: the persisted text is `show err` on OffChainFetchError
         -- (OffChain.hs:407 `offChainVoteFetchErrorFetchError = show err`), i.e. the human-readable
         -- rendering in Types.hs:244-283 -- NOT the constructor name. The earlier patterns
         -- '%HashMismatch%' / '%DataTooLong%' can never match, which would have classified every
         -- failure as 'unreachable' and broken F1 check 7.
         --   OCFErrHashMismatch -> "Hash mismatch when fetching metadata from <url>. Expected ..."
         --   OCFErrDataTooLong  -> "Error Offchain Voting Anchor: Size error, fetching metadata from
         --                          <url> exceeded 512 bytes."   (the "512 bytes" is an upstream
         --                          copy-paste bug; the real vote cap is 3 MB -- do not parse the number)
         WHEN e.fetch_error LIKE 'Hash mismatch%'       THEN 'hash_mismatch'
         WHEN e.fetch_error LIKE '%Size error%'         THEN 'oversized'
         WHEN e.id IS NOT NULL                          THEN 'unreachable'
         ELSE                                                'pending'
       END                                       AS verdict,
       d.language, d.comment, d.warning, d.json,
       e.fetch_error, e.fetch_time, e.retry_count
FROM public.voting_anchor va
LEFT JOIN public.off_chain_vote_data d ON d.voting_anchor_id = va.id
LEFT JOIN LATERAL (
  SELECT * FROM public.off_chain_vote_fetch_error x
  WHERE x.voting_anchor_id = va.id ORDER BY x.retry_count DESC LIMIT 1
) e ON true;
```

`pending` (anchor known, neither success nor failure row yet) is not in the spec's list but is a real
state — db-sync queues fetches and flushes them every half hour or every 10 000 blocks
(`Era/Universal/Block.hs:140`). The API must distinguish "not fetched yet" from "unreachable", or F1
check 7 ("every anchor has a verdict") is unsatisfiable during the first hours of a sync.

---

## 7. Preprod reality check

All figures measured live against `https://preprod.koios.rest/api/v1` on 2026-10-19. Koios runs on
db-sync, so these are the same numbers our instance will produce.

### 7.1 Volumes

| Entity | Preprod today | Source |
|---|---|---|
| Tip | epoch **320**, slot 136 754 769, block 5 311 567, era Conway | `/tip` |
| `drep_hash` rows (incl. never-registered) | **802** | `/drep_list`, `Content-Range: 0-0/802` |
| **Registered** DReps | **294** | `/drep_list?registered=eq.true` ⇒ `/294` |
| Rows in `drep_voting_power_history` for epoch 319 | **195** = **193 real DReps + the 2 pseudo-DReps** | `/drep_voting_power_history?_epoch_no=319` ⇒ `/195`; the response contains `drep_always_abstain` (431 863 790 957 959 lovelace, **46 % of the 937 803 900 128 796 total**) and `drep_always_no_confidence` (325 390 800 383). **This is why R3/R4 below had to be corrected.** |
| Gov action proposals, all time | **131** | `/proposal_list` ⇒ `/131` |
| Constitutional Committee | 2-of-3 quorum, members expiring epoch 372, all script credentials | `/committee_info` |
| First Conway epoch (proto_major 9) | **163** — *corrected*, was stated as 162 | `/epoch_params?_epoch_no=N`: 161→8, 162→**8**, 163→**9** |
| Conway epochs to date | 163 → 320 = **158** | |
| `drep_distr` rows, estimated | 158 epochs × 31→195 rows ≈ **20–25 k** | growth sampled at epochs 180/200/240/280/310/319 = 31/51/123/149/186/195 |
| Mean block interval | **~22.7 s** — *corrected*, was stated as ~62 s | `/epoch_info?_epoch_no=319` (last **completed** epoch): `blk_count = 19 054`, `end_time - start_time = 432 000 s`. The 62 s figure came from dividing the open epoch 320's partial `blk_count` by a full epoch. |
| Gov action lifecycle shape | 11 ratified (= 11 enacted), 102 expired (all also `dropped_epoch`), 6 dropped-only, **11 still open** | `/proposal_list`; no row has both `ratified_epoch` and `dropped_epoch` ⇒ `LEAST(ratified, expired, dropped)` is well-defined on real data |
| Re-votes | **common** — 75 of the first 1000 `(voter, proposal)` pairs have 2–5 rows | `/vote_list?limit=1000`; `/proposal_votes` returns only the **latest** vote per voter, `/drep_votes` returns the **full** history |

**The explorer will not look empty**: 294 registered DReps and 131 actions is more than enough for every
page, every filter and every chart in F1 check 13.

### 7.2 Acceptance sample sizing

F1 check 6 asks for ≥ 20 DReps and ≥ 10 gov actions reconciled. Given 294 / 131, **(decision)** sample
far above the floor, because the comparison is cheap and a 7 % sample is not evidence:

- **All 294 registered DReps** for the identity + status + voting-power comparison (one Koios call per
  100 ids via `POST /drep_info`; 3 calls total).
- **All 131 gov actions** for the lifecycle + tally comparison (`/proposal_list` is one call;
  `/proposal_votes` is 131 calls — fine for a nightly job).
- **30 DReps** for the full vote-history comparison (`/drep_votes`, one call each), chosen
  deterministically as the 30 with the most votes so the sample is the interesting tail, plus a
  fixed-seed random 10 for coverage.

### 7.3 Known preprod data shapes our code must survive

- A proposal whose `meta_url` is `ipfs://bafkrei…` (needs the IPFS gateway).
- A vote whose `meta_url` is `data:text/plain;utf8,…` (§6.2).
- `drep_info` returning `active: false` with `expires_epoch_no: 219` while tip is 320 — i.e. **registered
  but inactive** DReps with non-zero `amount` (830 773 382 067 lovelace in the sample). Our status model
  needs both axes, `registration_status` and `active`, not one enum.
- Script DReps (`has_script: true`) are the majority of the 802 `drep_hash` rows and mostly
  `registered: false` — do not assume a `drep_hash` row implies anything.

### 7.4 Sizing — **unverified, must be measured**

**Corrected.** The claim "no upstream RAM figure exists" was wrong:
`doc/configuration.md#ledger-backend` (13.7.2.1) states plainly — *"InMemory (default): Keeps the full
UTxO set in memory. Faster but uses significantly more RAM (~16GB on mainnet at tip)"* and *"LSM: …
reducing RAM usage to ~2-3GB"*. There is still no **preprod** figure and no per-preset disk table, and
`doc/installing.md`/`doc/Readme.md` do state no requirements — but the RAM axis is documented, and it
cuts against §2.2's blanket "do not set `ledger_backend: lsm`": LSM is upstream's *only* documented
answer for memory-constrained hosts, which a Pi is. Revised position: start on InMemory (preprod's UTxO
set is a small fraction of mainnet's), measure peak RSS, and keep `"ledger_backend": "lsm"` as the
named fallback accepting the three known LSM bugs — rather than treating LSM as forbidden.
Combined with §2.4
(`epoch_stake` is unavoidably populated) and §2.3 (`shelley: enable` adds `delegation`), the plan should
treat sizing as a **measurement task in F0**, not a known quantity:

```sql
-- run after the first full sync; this is the number that decides the PVC size
SELECT relname, pg_size_pretty(pg_total_relation_size(c.oid)) AS total
FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace
WHERE n.nspname = 'public' AND c.relkind = 'r'
ORDER BY pg_total_relation_size(c.oid) DESC LIMIT 20;
```

Provision the PVC at **100 Gi** initially (the existing preprod node PVC is already 100 Gi) and resize
down once measured. Over-provisioning a thin-provisioned PVC is free; running out mid-sync is not.

---

## 8. Koios cross-check

### 8.1 Endpoints and fields (all verified live on preprod)

Base: `https://preprod.koios.rest/api/v1`. PostgREST semantics: `Prefer: count=exact` + `Range: 0-0`
gives an exact count in `Content-Range` without fetching rows; `limit`/`offset` paginate.

| Koios endpoint | Params that actually work | Fields we compare | Our source |
|---|---|---|---|
| `GET /tip` | — | `epoch_no`, `abs_slot`, `block_no`, `block_time` | `gov.v_sync_state` |
| `GET /drep_list` | `registered=eq.true` | `drep_id` (CIP-129), `hex`, `has_script`, `registered` | `gov.v_drep` |
| `POST /drep_info` | `{"_drep_ids": [...]}` | `drep_status`, `deposit`, `active`, `expires_epoch_no`, `amount`, `live_delegator_count` | `gov.v_drep` + `mv_drep_metrics` |
| `GET /drep_voting_power_history` | **`_epoch_no` is required** (or `_drep_id`); bare call returns empty | `drep_id`, `epoch_no`, `amount` | `gov.v_drep_epoch_power` |
| `GET /proposal_list` | — | `proposal_id` (CIP-129), `proposal_tx_hash`, `proposal_index`, `proposal_type`, `deposit`, `return_address`, `proposed_epoch`, `ratified_epoch`, `enacted_epoch`, `dropped_epoch`, `expired_epoch`, `expiration`, `meta_url`, `meta_hash`, `meta_is_valid`, `withdrawal[]` | `gov.v_gov_action` + `v_treasury_withdrawal` |
| `GET /proposal_votes` | `_proposal_id` | `voter_role`, `voter_id`, `voter_hex`, `voter_has_script`, `vote`, `meta_url`, `meta_hash`, `block_time` | `gov.v_vote` |
| `GET /drep_votes` | `_drep_id` (CIP-129) | `proposal_id`, `proposal_tx_hash`, `proposal_index`, `vote_tx_hash`, `vote`, `block_time` | `gov.v_vote` |
| `GET /vote_list` | — | same as `proposal_votes` + `epoch_no`, `proposal_type` | `gov.v_vote` |
| `GET /committee_info` | — | `quorum_numerator/denominator`, `members[].{cc_hot_id, cc_cold_id, cc_hot_hex, cc_cold_hex, status, expiration_epoch, *_has_script}` | `gov.v_committee_member` |
| `GET /epoch_info` | `_epoch_no` | `start_time`, `end_time`, `blk_count`, `tx_count` | `public.epoch` (needs `disable_epoch: false` — D4) |

**Every Koios id is CIP-129** (`drep1y…`, `cc_hot1q…`, `cc_cold1z…`, `gov_action1…`), which is why D6 is
not optional: db-sync's `drep_hash.view` is CIP-105 and would compare as 100 % divergent.

### 8.2 The CI job **(decision)**

`scripts/reconcile_koios.sh` + `src/exe/admin` subcommand `reconcile-koios --network preprod
--sample-dreps 30 --out report.json`, run nightly in CI and on demand. It compares **our own public API**
(not raw SQL) against Koios, so the test covers the whole read path including CIP-129 encoding. Exact
comparisons, each either exact-equal or an explained exception:

| # | Comparison | Pass condition |
|---|---|---|
| R1 | Registered-DRep set: our `/dreps?status=registered` ids vs `/drep_list?registered=eq.true` | **Set equality** on CIP-129 ids. Zero symmetric difference. |
| R2 | Per-DRep status (all 294) | `registration_status`, `has_script`, `deposit`, `expires_epoch_no` exactly equal to `drep_info`. |
| R3 | Voting power, for the last 5 completed epochs, all DReps | **Exact lovelace equality** per `(drep_id, epoch_no)` vs `drep_voting_power_history?_epoch_no=E`. Not a tolerance — both read the same `drep_distr`. **Corrected:** Koios's response includes `drep_always_abstain` and `drep_always_no_confidence`, while `gov.v_drep_epoch_power` filters `raw IS NOT NULL`. Compare on the **intersection of real DReps** and assert the two pseudo rows separately against our own `always_*` aggregates. |
| R4 | A5 (spec): Σ voting power at epoch E vs total delegated | `abs(Σ our_amount − Σ koios_amount) = 0` **over the same actor set**. **Corrected:** as written this was guaranteed to fail by ~46 % — `drep_always_abstain` alone is 431.9 T of the 937.8 T total at epoch 319. Either include the two pseudo-DReps in our sum (preferred: keep them in a `v_drep_epoch_power_all` with a `target_kind` column) or subtract them from the Koios side. State which in the report. |
| R5 | Gov action set + lifecycle (all 131) | For every action: `proposal_type`, `proposed_epoch`, `expiration`, and all four of `ratified/enacted/dropped/expired_epoch` exactly equal to `proposal_list`. |
| R6 | Per-action vote tally (all 131) | Our `(voter_role, vote) → count` map equals the map built from `proposal_votes`. **Corrected:** this must be computed from `gov.v_vote_latest`, not `gov.v_vote` — Koios `/proposal_votes` returns one row per voter (verified: 3 rows for `gov_action197dpq8x…`, where `/vote_list` shows 4 votes from one DRep on it), and the ledger likewise counts only the last vote. Tallying raw `voting_procedure` rows both fails R6 and publishes wrong numbers. |
| R7 | Per-DRep vote history (30 sampled) | Set of `(gov_action_id_cip129, vote, vote_tx_hash)` exactly equal to `drep_votes`. This one **is** the full history including superseded votes (verified: 11 rows for one DRep, 5 of them on a single action) ⇒ compare against `gov.v_vote`, *not* `v_vote_latest`. |
| R8 | Committee | Member set by `cc_cold_id`, their `cc_hot_id`, `status`, `expiration_epoch`, and the quorum fraction equal to `committee_info`. |
| R9 | Treasury withdrawals | For every action with `withdrawal[]` non-empty: `(stake_address, amount)` multiset equality. |
| R10 | Freshness guard | Abort the whole run (neither pass nor fail) if `abs(our tip epoch − Koios tip epoch) > 1` or `our lag_seconds > 300`. Comparing across a boundary produces fake divergences. |

Output: `report.json` with `{check, expected, actual, status}` per row, plus a markdown summary.
CI fails on any `status: "divergent"` that is not listed in a committed `reconcile-exceptions.yaml`
(each exception carries a reason, a date and an owner). That file is how spec A6's "explained
divergences are documented" becomes a machine-checked artefact rather than a paragraph.

**Koios is a check, never a source** — assert that in the job's own header comment, because a future
contributor *will* be tempted to fall back to it when our sync is behind.

Rate limits: the free Koios tier throttles unauthenticated calls. The full run is ~300 requests; add a
250 ms delay between calls and honour `Retry-After`. If that becomes painful, a free Koios API token goes
in the CI secret store — never in the repo (the Maestro-token lesson).

---

## 9. What should NOT be built (laziness callouts on the spec)

| Spec item | Verdict |
|---|---|
| §4.1 "Metadata fetcher" (own fetch/hash/retry/cache) | **Delete.** db-sync does all of it (§6). Keep only the `data:` URI handling, which is SQL, not a fetcher. |
| §4.1 optional "mini Ogmios follower for low-latency type events" | **Do not build in F1** — but **not for the reason first given**. Preprod's mean block interval is ~22.7 s, not ~62 s, so a 60 s tick is ~3 blocks behind, not sub-block. The real argument is plain laziness: a whole second chain-sync pipeline to shave ≤60 s off an activity feed nobody has complained about. If it ever matters, drop the tick to 15 s first — that is a config change, not a pipeline. |
| §6 "`chain_cursor`, `raw_event_log`, `schema_version`" for the governance layer | **Already excluded by the spec, and this design confirms it.** The only bookkeeping the governance layer needs is a 2-column `gov.cursor` for rollback detection (§4.3). |
| §6 projection tables `drep`, `vote`, `gov_action`, `vote_delegation`, `drep_epoch_power`, `committee_member`, `treasury_withdrawal`, `anchor_content` | **Do not materialize any of them.** Each is a view (§3.3). `anchor_content` in particular is db-sync's `off_chain_vote_data` and must not be copied. |
| §4.1 "projection poisoning fix (#3): verify the datum-named token is in the UTxO value" | **Correct, and it belongs only to the attestation pipeline.** db-sync-derived projections have no datums and no NFTs; the fix has no meaning there. The spec already says this; the plan should not create a task for it on the governance side. |
| §7 "evaluate Atlas's local-node or db-sync providers to drop Maestro" | **Correction: Atlas 0.14.1 has no db-sync provider.** Its providers are Blockfrost, Kupo, Maestro, Node, Ogmios (`src/GeniusYield/Providers/` at tag `v0.14.1`). The `Node` provider does give `nodeSubmitTx`, `nodeGetParameters`, `nodeGetDRepState(s)`, `nodeProposals`, `nodeCommitteeMembersState` — but its UTxO query module is self-documented as *"Inefficient, for testing purposes only"* (`Providers/Node/Query.hs:3`). So local-node can replace Maestro for *submit + params + live governance state*, not for UTxO queries. Keep Maestro for the write path in F1 and drop this from the evaluation backlog. |
| §8 "nod preprod (bootstrap Mithril)" as F1 work | **Already done.** A preprod `cardano-node` 11.1.2 StatefulSet with a shared `/ipc/node.socket` exists in `apps-k8s-configs/chains/cardano/preprod/`. db-sync attaches to it (§2.5). Drop the Mithril bootstrap task. |
| `/dreps` composite scoring | Stays out, per §10 non-goals. `mv_drep_metrics` deliberately exposes **numerator and denominator** for every rate, never a blended score — so anyone can recompute, and the §1 S2 reproducibility criterion is met by the API shape itself. |

---

## 10. Blockers and open questions

### (BLOCKER-1) — resolved with a named workaround, but the owner must accept it

The official `ghcr.io/intersectmbo/cardano-db-sync` image is **amd64-only** at every 13.7.x tag.
Verified by resolving the manifests: `13.7.2.1` and `13.7.3.0` are single `application/vnd.docker.
distribution.manifest.v2+json` manifests whose config blobs report `"architecture": "amd64"`. The only
arm64 artefact in that registry is `feature_aarch64-release-bin` (a multi-arch index built from the
unreleased `feature/aarch64-release-bin` branch). The CHANGELOG's `## Unreleased` section lists
*"Add static binaries and docker images for aarch64-linux"* — i.e. official ARM64 images land in the next
release, not in 13.7.2.1.

The minirack is **ARM64-only** (7 nodes: 4× Pi 5, rockpi-4a, 2 VMs on an Apple-Silicon Mac mini), and
`apps-k8s-configs/CLAUDE.md` carries the standing rule *"ARM64-only workloads… If you pick an image,
verify it has an arm64 manifest."* There is no x86 escape hatch in this cluster.

Three options, ranked:

1. **(recommended) `ghcr.io/blinklabs-io/cardano-db-sync:13.7.1.0`** — verified multi-arch index with
   `linux/arm64`, same publisher as the `cardano-node` image this cluster already runs. Cost: 13.7.1.0,
   not 13.7.2.1. What we give up is the `epoch`-view redesign and the `pool_relay.port` fix — neither
   touches governance. What we must handle: the `epoch_state` duplicate-rows bug (#2155, fixed only in
   the unreleased line) ⇒ `DISTINCT` in `v_committee_member` (already in §3.3), and the duplicate
   off-chain-vote child rows (§6.2).
2. Build our own arm64 image from the 13.7.2.1 source. The repo supports `aarch64-linux` via nix
   (branch `feature/aarch64-linux`, merged into the unreleased line). Cost: a cross-build pipeline and
   an image we maintain, on a cluster whose CI runner is currently offline.
3. Wait for 13.7.3.0's official arm64 images. Cost: unknown date; blocks F1.

**Needs an owner decision only between 1 and 2.** Recommendation: 1, and revisit when 13.7.3.0 ships
official arm64 images.

### (BLOCKER-2) — a genuine unknown that the plan must schedule, not guess

**Will db-sync with `ledger: enable` actually sync preprod from genesis on a Pi 5, and how big does it
get?** The spec's 16 GB / 3.5 GB RAM figures have **no upstream source** (§7.4), `epoch_stake` is
unavoidably populated (§2.4), and `shelley: enable` adds `delegation` (§2.3). On top of that, the
release notes flag three LSM-backend bugs that *specifically* hit resource-constrained devices
("db-sync could crash due to how ghc handles system threads. This only happens on resource-constrained
devices such as the Raspberry Pi") — we are not using LSM (`ledger_backend` defaults to `InMemory`), but
it shows the shape of the risk.

Mitigation, not resolution: make the first F0 task a **timed, measured from-genesis sync** on the real
hardware with the §7.4 size query at the end, and a decision gate. **Corrected gate:** the relevant
budget is not `minirack-m4` (14 Gi — that VM hosts the *mainnet* relay + mempool-feed + submit-api,
`apps-k8s-configs/CLAUDE.md:4`). Because db-sync is a sidecar in the preprod node's pod, and that pod
is pinned by its `local-path` PV, the budget is **whatever node holds the preprod node's PV** — a Pi 5
at 16 Gi, already committed to `cardano-node` (requests 3 Gi, limits 8 Gi) plus ogmios and hydra-node.
Measure RSS there, with that co-tenancy. If it does not fit: LSM backend first (§7.4), off-cluster
second.

### Open question (needs owner, not research)

**Postgres co-location.** §3.2 requires our views to live in db-sync's own database. That means the
public Query API holds a connection — read-only, as `gov_api` — into the database db-sync writes. The
alternative (a second database + logical replication or FDW) costs a replication pipeline and reintroduces
staleness we just designed away. Recommendation: accept the co-location, with the role separation of
§3.2 as the control. Flagging it because it is an architectural commitment, not a reversible detail.

### Could not determine

- Whether `13.7.3.0` is a real release (git tag + docker tag exist; no GitHub release, no CHANGELOG
  heading). Treat 13.7.2.1 as latest-released until Intersect publishes notes.
- Actual on-disk size and peak RSS of `only_governance` + `shelley: enable` + ledger on preprod — no
  upstream figure exists and no instance has been run. See BLOCKER-2.
- Whether deleting old `epoch_stake` rows (§2.4) is safe against `validateEpochStake`. Requires a live
  instance to test; do not ship the CronJob before that test exists.

---

## Fact-check corrections

Adversarial re-verification against primary sources (db-sync `schema.md` / `configuration.md` /
`schema-management.md` / `manual-rollbacks.md` and the Haskell source at tags **13.7.2.1 and
13.7.1.0**; all 80 files of `schema/`; GitHub releases; the ghcr manifest + config-blob API;
CIP-0129; live Koios preprod; and the owner's three repos on disk). What **held up**: every
table/column name in §1.1, every unique constraint, `voting_procedure.invalid` never populated,
`epoch_state.no_confidence_id` always NULL, `parseOverrides` letting explicit keys beat the preset,
`rewards` unsettable, `ipfs_gateway` as a top-level key (present at *both* tags), `ledger_backend`
defaulting to InMemory, `insertStakeSlice` unconditional (`Era/Universal/Block.hs:138`), the off-chain
flush gate (`:140`), the whole of §6.1 (blake2b-256, 3 MB cap, content-type list, `data:` rejection at
`Http.hs:227`, `min(86400, 30 + 2^n·60)` retry), `-- noreference` on the governance FKs
(`GovernanceAndVoting.hs:196-205`), `setNull{Enacted,Ratified,Dropped,Expired}Stmt`
(`Statement/Base.hs:832-835`) and the `DrepDistr: epoch_no > N` log line (`:798`), the two shipped
governance indexes, every CIP-129 header byte and HRP, Atlas v0.14.1's provider list and its
"Inefficient, for testing purposes only" node UTxO query, the blinklabs `13.7.1.0` digest
`sha256:2a341eb3…` as a manifest list with `linux/arm64`, intersectmbo being a single-arch
`"architecture":"amd64"` manifest at 13.7.1.0/13.7.2.1/13.7.3.0, 13.7.2.1 as the latest GitHub
release, issue #2155 and PR #2137, and the preprod counts 802 / 294 / 131 / epoch 320 / CC 2-of-3 all
script creds.

Changed:

| # | Where | Was | Now |
|---|---|---|---|
| 1 | §3.5 `participation` | `count(*)` / `count(v.gap_id)` over a one-to-many LEFT JOIN | `count(DISTINCT …)`. Re-votes are real on preprod (75 of the first 1000 `(voter, proposal)` pairs have 2–5 rows), so the fan-out inflated both counts and skewed `participation_rate` toward 1. |
| 2 | §3.3, §8.2 R6 | tallies over `voting_procedure` rows | new `gov.v_vote_latest` (latest vote per voter per action) feeds every tally. Koios `/proposal_votes` is latest-only (3 rows where `/vote_list` shows 4 from one DRep) and the ledger counts only the last vote. R7 keeps full history (`/drep_votes` returns 11 rows, 5 on one action). |
| 3 | §6.3 `v_anchor` | `LIKE '%HashMismatch%'` / `'%DataTooLong%'` | `LIKE 'Hash mismatch%'` / `'%Size error%'`. `OffChain.hs:407` persists `show err`, and `Types.hs:244-283` renders prose, not constructor names — the old patterns never match, so every failure would have read `unreachable` and F1 check 7 would have been vacuous. |
| 4 | D9, §5.2, §5.3, §7.1, §8.2 R10, §9 | "~62 s mean block interval", `/ready` at 900 s, A1 "unachievable as written" | **~22.7 s** (`epoch_info?_epoch_no=319`: 19 054 blocks / 432 000 s). The 62 s came from the *open* epoch 320's partial `blk_count`. `/ready` red at 300 s, amber 120 s, and **A1's "<120 s sustained" stands**. |
| 5 | §8.2 R3/R4 | exact equality of Σ voting power vs Koios | Koios `drep_voting_power_history` includes `drep_always_abstain` (431.9 T = 46 % of the epoch-319 total) and `drep_always_no_confidence`, which `v_drep_epoch_power` filters out. Compare on a matched actor set. |
| 6 | D4, §2.1, §2.2, §4.1 | `disable_epoch: false` as a required override | The key, `sioEpoch` and `EpochConfig` **do not exist in 13.7.1.0**, the image D5 chooses. Removed from the config block; `public.epoch` on that image is the legacy cached table (the thing 13.7.2.1 replaced), so derive epoch times from `block`. |
| 7 | D7, §3.4 | "(hash_id, epoch_no) is the one index db-sync ships, so the join is index-driven" | The eligibility join filters `epoch_no` alone — wrong leading column. Added `gov_idx_drep_distr_epoch ON drep_distr (epoch_no, hash_id)`. Also: that shipped index is a **stage-4** object, created only once db-sync is within 30 min of tip (`doc/schema-management.md:18`), so it is absent for the whole initial sync. |
| 8 | §5.1 | "`block.time` is unindexed" | `schema/migration-4-0002-20200810.sql` creates `idx_block_time ON block(time)`. `ORDER BY id` is still right, for the stage-4 reason instead. Also pinned `now() AT TIME ZONE 'UTC' - b.time`: `block.time` is `timestamp` without zone, so the original expression was session-TimeZone-dependent. |
| 9 | §7.4, BLOCKER-2 | "the spec's RAM figures have no upstream source" | `doc/configuration.md#ledger-backend` states "~16GB on mainnet at tip" for InMemory and "~2-3GB" for LSM. That also softens §2.2's blanket ban on LSM — it is upstream's documented answer for memory-constrained hosts. |
| 10 | BLOCKER-2 gate | "if peak RSS exceeds what `minirack-m4` (14 Gi) can give" | `minirack-m4` hosts the *mainnet* relay. The sidecar runs wherever the preprod node's `local-path` PV lives — a 16 Gi Pi 5 already running cardano-node (3 Gi req / 8 Gi limit) + ogmios + hydra-node. |
| 11 | §2.5 | sidecar "rather than converting ipc to a hostPath (which restarts the node)" | The sidecar does not avoid the restart, it makes it recur on every db-sync pod-spec change (taking ogmios and hydra-node with it), and its PVCs are pinned to one node by `local-path`. Three costs now priced in; owner call. |
| 12 | §7.1 | first Conway epoch 162; "195 DReps with power" | `/epoch_params`: 162 → proto 8, **163** → proto 9. The 195 rows are 193 real DReps + 2 pseudo. |
| 13 | §1.1 `reward_rest` | `LedgerEvent.hs:133-150` | `Era/Universal/Insert/LedgerEvent.hs:131-146` (the path in the note 404s). Claim itself verified. Added: `reward_rest` has no gov-action FK, so the "paid" join is a `(stake_address_id, spendable_epoch)` heuristic. |
| 14 | §3.2 | DDL at startup via `CREATE OR REPLACE VIEW` | `mv_drep_metrics` depends on `v_live_delegation`, so that view's columns become frozen and `DROP … CASCADE` would take the matview with it. Decide: inline the subquery, or drop-and-recreate under the `schema_version` guard. |
| 15 | §2.1 note 2, §5.2 | `Config/Types.hs:497`; `verifyProjectionDbConnection` in `Query/ServiceStatus.hs:32` | `:499` (13.7.2.1) / `:484` (13.7.1.0); the function is in `src/exe/query-api/QueryAppMonad.hs:96` — `ServiceStatus.hs:32` is `getReadyProbe`. |
| 16 | §2.5 | preprod node "on backend `V2LSM`" | Correct per `apps-k8s-configs/CLAUDE.md:11`; the inline comment at `statefulset-cardano-preprod.yaml:80` claiming `V2InMemory` is stale. Noted so nobody "fixes" the design from the manifest comment. |

Still open, for the plan author:

- **Per-epoch delegator count.** Spec §6 models `drep_epoch_power (drep, epoch, lovelace, nr. delegatori)`
  and §1.2 says we derive counts from `delegation_vote` + `stake_deregistration`, but no view in §3.3
  does it — `v_live_delegation` is latest-only. Either add `v_drep_epoch_delegators` (latest delegation
  per stake address as of each epoch's last block) or cut the per-epoch count from the spec. Decide; do
  not leave it to implementation time.
- **`committee_vote_auth`** from spec §6 is never mentioned. `v_committee_member` probably covers it via
  `committee_registration`; say so explicitly or add the view.
- `govActionIdCip129 :: ByteString -> Word8 -> Text` — `gov_action_proposal.index` is `integer (64)`.
  Fine on real data, but the encoder needs an explicit bounds check rather than a silent truncation.
