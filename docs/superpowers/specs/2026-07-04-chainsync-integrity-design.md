# Chain-Sync Integrity — Design Spec

**Date:** 2026-07-04
**Branch:** `fix/chainsync-integrity`
**Sub-project B** of the Fable Review (`FableReview-0407.md`).

## Goal

Fix the interconnected chain-sync data-integrity cluster so that projections in Postgres
are a faithful, rollback-safe reflection of on-chain state. Concretely, resolve findings
F-02, F-03, F-04, F-05, F-06, F-07, F-08, F-10, plus a pinning test (F-33), plus two
co-located correctness fixes (F-31, F-42).

## Success criteria

1. `cabal build all` and `cabal test` pass.
2. A new test (`ChainSyncReplay`) drives the **real** `projectChainEvent` on synthetic fixtures and
   the **real** shared replay-ordering function (§3), asserting that replaying the surviving prefix
   reproduces expected projection state, including promotion-deletion semantics via the successor
   `RankEvent` and same-block `(txIndex, outputIndex)` ordering. Scope is honest: the test pins the
   *classify* and *ordering* halves (the shared pure code); the `SqlPersistT` *apply* half
   (upsert/delete against Postgres) is out of scope for the pure test and deferred to a future
   integration test (§7).
3. Slot columns are stored as `bigint` (numeric comparison), verified by inspecting the generated
   `migrateAll` output / column types after a clean migration.
4. On an **existing pre-fix DB**, the first startup of this branch wipes + re-syncs successfully
   (no migration crash): the schema-version check and DROP happen **before** `runMigration migrateAll`
   (§1). Verified against the real persistent/persistent-postgresql migration behavior.
5. `ROLLBACK_MARGIN` is clamped to `max 1`; margin `0` is rejected as a no-op (would infinite-loop).
6. Manual reasoning trace (documented in the plan) shows each of the 8 sync-loop / rollback scenarios
   produces correct state.

## Verification record

This spec was adversarially stress-tested against the real codebase and pinned library sources
(persistent 2.15.1.0, persistent-postgresql 2.13.6.2, atlas-cardano 0.14.1) by a 6-dimension
multi-agent review with independent skeptic verification (2026-07-04). It found and this revision
fixes: a **blocker** (migration ordering — `runMigration` ran before the wipe and crashed on the
un-`USING` varchar→INT8 cast; one skeptic reproduced `SQLSTATE 42804` on a live Postgres), a
**major** (`ROLLBACK_MARGIN=0` infinite loop), a **major** (pure test pinned a re-implemented model,
not shipped code), and minors (snippet sort key, Atlas function name, missing test dependency,
imprecise replay note). The core replay model, the `PersistField Integer` instance, and the
F-03/F-06 fixes were verified sound as designed.

## Core principle

Adopt an explicit model:

- **`OnchainMatchEvent` = append-only log; the source of truth cache.**
- **Projection tables = pure derived state, fully reconstructable** by replaying the surviving
  raw-match log through the existing projection logic.

Every fix below follows from this model. The DB remains a disposable cache: it can always be
rebuilt from Kupo (full re-sync) or from the retained raw-match log (rollback replay).

---

## 1. Schema + migration (F-02, F-05)

### F-02 — slots stored as text

`Storage.hs:43` currently uses `derivePersistFieldJSON "Integer"`, which persists every `Integer`
column (all slot columns: `ChainCursor.slotNo`, `OnchainMatchEvent.createdSlot`, every
`createdAtSlot`, `intervalNumber`) as a JSON **varchar**. SQL `>` / `ORDER BY` on those columns is
therefore lexicographic: `"99999999" > "100000000"` is true. Preview slots crossed 100,000,000
(~Jan 2026), so `rollbackTo`'s `deleteWhere [slot >. X]` already deletes the wrong rows.

**Fix:** replace `derivePersistFieldJSON "Integer"` with a hand-written orphan instance mapping to
`PersistInt64` (bigint):

```haskell
instance PersistField Integer where
  toPersistValue i
    | i >= toInteger (minBound :: Int64) && i <= toInteger (maxBound :: Int64) =
        PersistInt64 (fromInteger i)
    | otherwise = error "Integer out of Int64 range for persistence"  -- unreachable for slots
  fromPersistValue (PersistInt64 n) = Right (toInteger n)
  fromPersistValue x = Left ("Expected PersistInt64 for Integer, got: " <> Text.pack (show x))

instance PersistFieldSql Integer where
  sqlType _ = SqlInt64
```

- Single point of change; no call-site changes (call sites keep passing `Integer`).
- Cardano slots fit comfortably in `Int64` (max ~10^9); the overflow guard is defensive only.
- The file already carries `{-# OPTIONS_GHC -Wno-orphans #-}` and an orphan `PersistField GYTime`,
  so this is consistent with existing style.
- persistent deliberately ships **no** `PersistField Integer`, so there is no instance clash.
- The `error` in `toPersistValue` is the one place we retain a raw error; it is unreachable for
  slot/interval-number values and documented as such. (Alternative considered: `Either`-less
  `PersistField` cannot signal failure on write, so a clamp-or-error is the only option; error is
  correct because silently clamping a slot would corrupt ordering.)

### F-05 — raw-match unique key collapses multiple matches per block

`OnchainMatchEvent`'s unique key is `(createdSlot, createdHeader)` (`Storage.hs:96`); `putKupoMatch`
`replace`s on conflict (`Storage.hs:259-264`). Two matches in the same block (the common case: one
`CreateProfile` tx produces matched outputs at both the profiles and ranks validators) collide and
overwrite each other. The raw-match store — the replay source for F-07 — is therefore lossy.

**Fix:** extend the entity and its unique key. Both new fields already exist in `KupoMatch`
(`transaction_id`, `output_index`, `KupoClient.hs:99,101`):

```
OnchainMatchEvent
    createdSlot        Integer
    createdHeader      Text
    createdTxId        Text
    createdTxIndex     Int       -- block-relative tx order; used for replay sort (§3)
    createdOutputIndex Int
    kupoMatch          KupoMatch
    UniqueKupoMatch createdSlot createdHeader createdTxId createdOutputIndex
    deriving Show
```

`putKupoMatch` populates the new columns from `transaction_id (km)` / `transaction_index (km)` /
`output_index (km)` and upserts by the 4-field key. `createdTxIndex` is not part of the unique key
(the key is identity: slot+header+txId+outputIndex); it exists so replay can sort by true chain order
`(slot, txIndex, outputIndex)` in SQL without decoding every `kupoMatch` blob.

### Migration strategy — wipe BEFORE migrate, gated by a raw schema-version probe

Both changes above alter column types / unique constraints. **The wipe must run before
`runMigration migrateAll`, not after.** persistent's auto-migration classifies a `varchar → INT8`
type change and a `ADD COLUMN NOT NULL` (no default) as *safe* and executes them via `rawExecute`;
on a populated pre-fix DB the varchar→INT8 `ALTER COLUMN ... TYPE INT8` (emitted with no `USING`
clause) is rejected by Postgres (`SQLSTATE 42804`), and the `ADD COLUMN`s fail with `SQLSTATE 23502`
— both crash startup at `Main.hs:52` *before* any post-migration wipe check could run. This was
confirmed against persistent-postgresql 2.13.6.2 (`findAlters`/`modType`/`showAlter`) and reproduced
on a live Postgres. **So the original "migrate first, then wipe" ordering is fatal and must be
inverted.**

New startup sequence in `chain-sync/Main.hs` (before `runMigration`):

1. Create the pool.
2. **Raw, schema-tolerant probe** (not via a persistent entity — the entity schema does not match the
   old DB). Using `rawSql` / `rawExecute`:
   - Does table `chain_sync_config` exist? (`information_schema.tables`). If **no** → fresh install,
     skip to step 4.
   - Does column `schema_version` exist? (`information_schema.columns`). If **no** → pre-fix DB
     → `storedVersion = 1`.
   - Else read `SELECT schema_version, policy_hex_text FROM chain_sync_config LIMIT 1`.
3. **Wipe decision:** if `storedVersion < currentSchemaVersion` **OR** stored policy ≠ current policy:
   `DROP TABLE IF EXISTS <each chainSyncTableName> CASCADE` via raw SQL (do **not** call
   `migrateAll` on the old tables).
4. `runMigration migrateAll` — now runs against fresh/empty (or unchanged) tables, so it only
   `CREATE`s, never `ALTER`s incompatible columns.
5. Store `(policyHexText, currentSchemaVersion)` via the normal persistent upsert.

- Add `schemaVersion Int` to the `ChainSyncConfig` singleton **with `default=1`** (so that even if a
  future non-wipe path ever adds it via `migrateAll` on a populated table, the `ADD COLUMN` carries
  `DEFAULT 1` and backfills existing rows instead of failing `23502`). The steady-state value written
  by step 5 is `currentSchemaVersion`.
- Add `currentSchemaVersion :: Int = 2` (1 = implicit pre-fix schema).
- Refactor: extract a `wipeChainSyncTablesRaw` (the `DROP TABLE ... CASCADE` loop **without** the
  trailing `runMigrations`, since migrate now happens in step 4). The existing `wipeChainSyncTables`
  (drop + migrate) may stay for the runtime policy-change path, or be re-expressed in terms of the
  new startup flow — decided in the plan.

**Deploy note:** the first deploy of this branch wipes and re-syncs the chain-sync DB. Query-api
serves empty/partial results during the re-sync window (minutes on preview). This is the sanctioned
pattern for this project (offchain state is fully reconstructable). No manual DB surgery, and — with
the pre-migration wipe — no migration crash on the existing preview DB (whose slots already crossed
100M).

---

## 2. Startup / first-run (F-04)

`findCheckpoint` (`ChainSyncLogic.hs:60-75`) walks *forward* from the cursor slot in `batchSize`
(default 100,000,000) steps until Kupo returns a checkpoint, and `Main.hs:83-84` then pins the
cursor to that result. On an empty DB this jumps the cursor near the tip and skips all history;
same after `wipeChainSyncTables`. Kupo's `GET /checkpoints/{slot}` without `?strict` returns the
largest checkpoint ≤ the requested slot, which is why the 100M probe lands near tip.

**Fix:**

- Remove the startup `findCheckpoint` + `updateLocalTip startingCheckPoint` dance.
- Empty cursor → keep origin `(0, "")`. The first `Behind` fetch uses `created_after = 0`, which
  returns everything Kupo has indexed (Kupo is already bounded by its `--since` configuration, so
  there is no wasted scan over pre-deployment slots — Kupo returns matches, not empty ranges).
- Non-empty cursor → use it as-is (do not overwrite on restart). The stored cursor already carries a
  real header from the last `updateLocalTip`.
- `findCheckpoint` has no remaining callers → remove it (and its `runKupoCheckpointBySlot` import if
  now unused).

**Header sentinel:** on a fresh start the cursor header is `""` until the first successful
fetch + `updateLocalTip` (which stores a real header). `evaluateChainSyncState` only compares
headers when slots are **equal** (the `UpToDateButDifferentBlockHash` case); while the cursor is at
`(0,"")` and behind the tip, only the slot comparison runs, so the sentinel header is never
misinterpreted. After the first cycle the header is real.

---

## 3. `rollbackTo` rewrite (F-07)

Current `rollbackTo` (`Storage.hs:408-428`) deletes rows `> slot` per table and rows at `slot` with
a mismatching header. Because projections are destructive upserts whose `createdAtSlot` is bumped to
the latest event's slot, an entity created at S0 and mutated at S2 is deleted entirely by a rollback
to S1 (S0 < S1 < S2) and never re-created (Kupo re-sends only matches created after the rollback
point). `deletePromotionProjection` deletions are likewise unrecoverable.

**Fix:** make rollback a replay from the retained raw-match log.

```haskell
rollbackTo :: (MonadIO m) => GYNetworkId -> Integer -> SqlPersistT m ()
rollbackTo networkId slot = do
  -- 1. Discard raw matches strictly beyond the rollback slot (numeric compare; F-02 fixed).
  deleteWhere [OnchainMatchEventCreatedSlot >. slot]
  -- 2. Wipe all derived projection tables.
  deleteWhere ([] :: [Filter ProfileProjection])
  deleteWhere ([] :: [Filter RankProjection])
  deleteWhere ([] :: [Filter PromotionProjection])
  deleteWhere ([] :: [Filter MembershipHistoryProjection])
  deleteWhere ([] :: [Filter MembershipIntervalProjection])
  deleteWhere ([] :: [Filter AchievementProjection])
  -- 3. Replay surviving raw matches in TRUE chain order (slot, txIndex, outputIndex).
  surviving <-
    selectList []
      [ Asc OnchainMatchEventCreatedSlot
      , Asc OnchainMatchEventCreatedTxIndex     -- block-relative tx order, NOT txId (lexicographic hash)
      , Asc OnchainMatchEventCreatedOutputIndex ]
  forM_ surviving (projectAndStore networkId . onchainMatchEventKupoMatch . entityVal)
```

**No header parameter.** `rollbackTo` deletes purely by `createdSlot > slot`; it does not special-case
same-slot-different-header rows. This is safe **only because every caller passes a slot strictly below
any possible orphan** — guaranteed by the F-08 `rollbackMargin ≥ 1` clamp (§4). If the margin were 0,
a same-slot orphan raw match would survive and replay into a phantom projection; hence the clamp is
`max 1`, not `max 0`. The single-argument (slot-only) signature is deliberate given this invariant.

Refactor `putMatchAndProjections` into two reusable pieces:

- `putKupoMatch km` — store the raw match (unchanged except new key columns).
- `projectAndStore networkId km` — the `kupoMatchToAtlasMatch` + `projectChainEvent` + `put*Projection`
  case analysis (currently the body of `putMatchAndProjections` after `putKupoMatch`).

Then:
- Normal path: `putMatchAndProjections = putKupoMatch >> projectAndStore`.
- Replay path: `projectAndStore` only (raw already stored).

**Ordering note.** Replay must be in true chain order so upserts land the latest state and
`deletePromotionProjection` / backfill fire correctly. Chain order within a block is
`(transaction_index, output_index)`, not `transaction_id`. The raw entity currently stores
`createdTxId`/`createdOutputIndex`; the sort must use the block-relative **transaction_index**. Two
options: (a) also persist `createdTxIndex Int` and sort `(slot, txIndex, outputIndex)`; (b) sort in
Haskell after `selectList` using `transaction_index`/`output_index` read from the decoded `KupoMatch`.
**Decision: (a)** — persist `createdTxIndex` and sort in SQL, so ordering is explicit and not
dependent on decoding every row. (This supersedes sorting by `createdTxId`.)

**How deletions are reproduced (precise mechanism).** Promotion removal is driven by the **successor
`RankEvent` match**, not by spent-status. During incremental sync we only ever fetch forward
(`created_after`/`created_before` advance monotonically), so a promotion output first stored while
unspent keeps `spent_at = Nothing` in `OnchainMatchEvent` even after it is later spent. On replay,
that stored promotion match still emits a `PromotionEvent` and re-creates the projection; it is the
**separately stored rank match** (the accept tx re-locks a rank at the same NFT id) that fires
`deletePromotionProjection` when reached in chain order. This is why replay order
`(slot, txIndex, outputIndex)` is load-bearing: the rank match provably sorts after the promotion
match (later block, or greater `transaction_index` within a block, since the accept tx spends the
promotion UTxO). Other accept flows (interval/achievement/profile) spend-and-recreate at the same NFT
id and rely on destructive upsert, so latest-in-chain-order wins. Interval↔org backfill re-runs as
histories are replayed. **Implementation caveat:** do NOT "optimize" replay by skipping stored
matches whose `spent_at` is set — a match's stored `spent_at` is generally stale (never refreshed
after first store), and skipping it would drop the create-event that later matches depend on.

Spent-status is only relevant for *inertness of already-spent outputs seen during a fresh full
re-sync* (Kupo may return a match already carrying `spent_at`), where `projectChainEvent` returns
`NoEvent` (`Ingestion.hs:33`).

**Kupo pruning tolerance.** The model tolerates a Kupo instance run with `--prune-utxo` (which drops
spent outputs after ~2160 blocks and makes `?spent` empty): deletions are reproduced by successor
create-matches (rank / updated state), not by spent markers, and the local `OnchainMatchEvent` log —
not Kupo — is the rollback-replay source, so it is independent of Kupo pruning.

**Cost.** Full projection rebuild on every rollback. Rollbacks are rare and this protocol's match
volume is small, so full rebuild is chosen for provable correctness over a targeted rebuild.
Documented as a future optimization if match volume grows.

---

## 4. Sync loop (F-03, F-08, F-10)

### F-03 — `Behind` pins cursor to a fresher tip than it fetched

`Main.hs:122-124` fetches up to `blockchainTip`, then reads a *fresh* `blockchainTip'` and
`updateLocalTip blockchainTip'`. Matches in `(blockchainTip, blockchainTip']` are skipped forever.

**Fix:** `updateLocalTip pool blockchainTip` — the bound actually fetched. Drop the re-fetch.

### F-08 — fork handling (minimal + safety margin)

`UpToDateButDifferentBlockHash` (`Main.hs:133-136`) currently only updates the tip, leaving phantom
rows from the orphaned block. `Ahead` (`Main.hs:125-131`) rolls back only to the reported tip, so a
deeper Kupo rollback leaves stale rows below it.

**Fix (chosen approach: minimal correct + configurable margin):**

- `UpToDateButDifferentBlockHash`: `rollbackTo networkId (max 0 (slot - rollbackMargin))`, then set the
  cursor to `(rollbackSlot, "")`; the next `Behind` cycle re-fetches and heals. The orphan at `slot`
  is removed because `slot > slot - rollbackMargin` (requires `rollbackMargin ≥ 1`).
- `Ahead`: `rollbackTo networkId (max 0 (blockchainSlot - rollbackMargin))`, then set the cursor
  **explicitly** to `(rollbackSlot, "")` (same as the hash-mismatch case — spelled out so no
  implementer writes a real/stale header here); re-sync forward heals up to `rollbackMargin` depth.
  Kupo is canonical, upserts are idempotent.
- `rollbackMargin :: Integer` from env `ROLLBACK_MARGIN` (default `2160`), **clamped to `max 1`**.

**Why `max 1`, not `max 0`.** A margin of `0` makes the rollback target equal `slot`, so the cursor
becomes `(slot, "")`; since Kupo headers are never empty and `evaluateChainSyncState` maps
equal-slot/different-header to `UpToDateButDifferentBlockHash`, the loop would roll back to the same
slot forever (a hot spin that wipes+replays all projections every iteration on a live chain, and a
true infinite loop on a stalled tip). Clamping the *margin* to `≥ 1` guarantees `rollbackSlot < slot`,
so the next state is `Behind` (slot-only comparison, header irrelevant) and heals in ≤ 2 iterations.
Margin `0` is documented as **not** a valid no-op. (The rollback *slot* is still additionally
`max 0`-clamped so slot 0 is the floor near genesis.)

`rollbackMargin` must exceed realistic rollback depth; 2160 slots (~12h on preview at ~1 block/20s)
is generous. Trade-off: each divergence re-processes up to `rollbackMargin` slots. Divergences are
rare, so this is acceptable. A full common-ancestor anchor search (comparing stored headers against
Kupo `/checkpoints`) is the strictly-correct alternative and is documented as a future enhancement.

### F-10 — `fetchingMatches` retry duplicates work and grows the stack

`ChainSyncLogic.hs:121-132`: the trailing `fetchingMatches ... endInterval end` runs unconditionally
after the `case`, so the `Left` retry (which already re-covers the range) is followed by a second
full pass, and the retry call is non-tail.

**Fix:** move the continuation into the `Right` branch; make `Left` a plain tail retry. Note the
`[start, endInterval]` / `[endInterval, end]` windows overlap by exactly one slot (`endInterval`)
because Kupo `created_after`/`created_before` are both **inclusive** — this is duplicate-*safe*, not
duplicate-*free*: every write is an idempotent `upsertByUnique` (raw match on the 4-field key,
projections on their entity ids), so the one-slot overlap is harmless. The wording is "duplicate-safe
(idempotent), overlaps by 1 slot by design," not "non-duplicating."

```haskell
fetchingMatches ... start end batchSize
  | end <= start = putStrLn "No more matches to fetch"
  | otherwise = do
      let endInterval = min end (start + batchSize)
      eMatches <- runKupoMatches ... (Just start) (Just endInterval) ...
      case eMatches of
        Left err -> do
          putStrLn ("Kupo client error: " <> show err); threadDelay 10_000_000
          fetchingMatches ... start end batchSize            -- tail retry, same range
        Right matches -> do
          applyMatches networkId pool matches
          now <- getCurrentTime
          modifyMVar_ metricsVar $ \m -> pure m {smLocalTip = endInterval, smLastSyncTime = now}
          fetchingMatches ... endInterval end batchSize       -- continuation only on success
```

---

## 5. Value conversion (F-06)

`toGYAssetClass` (`KupoAtlas.hs:118-121`) calls `error` on asset keys without a `.` separator. Kupo
renders an empty-asset-name token as a bare 56-char policy id (no dot), and `parseAssetClassWithSep
'.'` requires the separator. The `error` sits in a lazy record field, escapes the `Either` in
`putMatchAndProjections`, and crash-loops the sync on the offending batch. Any holder of a protocol
token can trigger it by co-locating an empty-name token.

**Fix:** thread `Either` through the conversion.

```haskell
toGYAssetClass :: Text -> Either String GYAssetClass
toGYAssetClass t = case parseAssetClassWithSep '.' t of
  Right ac -> Right ac
  Left _   -> parseAssetClassWithoutSep t   -- bare 56-char policyId -> GYToken policyId ""

kupoValueToGYValue :: KupoValue -> Either String GYValue
kupoValueToGYValue KupoValue{coins, assets} = do
  gyAssets <- traverse (\(k,v) -> (,v) <$> toGYAssetClass k) (Map.toList assets)
  pure $ valueFromList ((GYLovelace, coins) : gyAssets)
```

Resolved: the empty-token-name case uses **`parseAssetClassWithoutSep :: Text -> Either String
GYAssetClass`** (atlas-cardano 0.14.1, `GeniusYield.Types.Value`), already in scope via the existing
`import GeniusYield.Types` (`KupoAtlas.hs:15`) — no new import. It parses a bare 56-char policy id to
`GYToken policyId ""`, which is representable and keys distinctly from `GYLovelace` in `valueFromList`
(no collision). The current `map (Data.Bifunctor.first toGYAssetClass)` (`KupoAtlas.hs:114`) becomes
the `traverse` form above, since `first` cannot thread `Either`.

`kupoMatchToAtlasMatch` binds `amValue <- kupoValueToGYValue value` (was a pure field). Conversion
errors now surface as `Left`, already handled by `putMatchAndProjections` / `projectAndStore`
(log + skip).

*Edge note:* in Atlas the empty **string** `""` (and `"lovelace"`) parses to `GYLovelace`, but this
is unreachable here — Kupo `assets` map keys are always ≥ 56 hex chars (`policy[.asset]`); lovelace
is carried separately in `coins`. So the fallback only ever receives a real policy id, never `""`.

---

## 6. Co-located correctness fixes (F-31, F-42)

Both live in `chain-sync/Main.hs`, which is being rewritten; near-zero risk.

- **F-31** — `smDbReady` / `smMigrationsComplete` are set `False` and never updated
  (`Main.hs:93-94`), so `/health` reports them false forever. Set `smMigrationsComplete = True` after
  `runMigrations`, and `smDbReady = True` after the first successful pool use.
- **F-42** — `read` on `BATCH_SIZE` / `FETCH_BATCH_SIZE` (`Main.hs:62,65`) crashes on non-numeric
  input. Use `readMaybe` with the existing default fallback (consistent with `getPortFromEnvOrDefault`).

**Deferred (stream C, security):** F-13 (DSN logged with password, `Main.hs:105`) — noted, not
changed here to keep this sub-project scoped to integrity.

---

## 7. Test (F-33) — pin real classify + real ordering; be honest about the rest

New test module `src/test/UnitTests/ChainSyncReplay.hs`, wired into `UnitTests.hs`. The test suite
must gain a **`chainsync-lib` build-dependency** (it owns `AtlasMatch`/`KupoAtlas` and
`KupoMatch`/`KupoClient` needed for fixtures; `offchain-lib` depending on it does not re-export them).

To make the test pin **shipped code** rather than a re-implemented model, the implementation extracts
one **pure shared function** used by both `rollbackTo` and the test:

```haskell
-- pure; the single source of replay order. rollbackTo and the test both call this.
replayOrder :: [OnchainMatchEvent] -> [OnchainMatchEvent]   -- sort by (slot, txIndex, outputIndex)
```

**What the test pins (real code):**
1. **Classify** — drive the *real* `projectChainEvent` on synthetic `AtlasMatch` fixtures. It is
   `MonadError GYTxMonadException`-polymorphic and depends only on `GYNetworkId` + pure validator
   hashes, so it runs purely via `runExceptT` at `Either GYTxMonadException` — no DB, no provider.
   This pins the shipped classify logic (profile/rank/promotion/interval/achievement/NoEvent) at no
   extra cost.
2. **Ordering** — assert `replayOrder` yields true chain order on a fixture with:
   - two matches in the same block, different `(txIndex, outputIndex)` (both present, correct order);
   - slots `99_999_999` and `100_000_000` (numeric order — pins F-02's *intent*).
3. **Deletion sequencing** — a profile-create → promotion → rank-confirms fixture: after classifying
   in `replayOrder`, assert the derived event sequence places the `RankEvent` after the
   `PromotionEvent` (so `deletePromotionProjection` fires after the promotion is created).

**Honest limitations (do not overstate):**
- The `SqlPersistT` *apply* half (`upsertByUnique`, `deletePromotionProjection`, backfill, the wipe
  table list) cannot run in a pure test — there is no in-memory persistent backend in the dependency
  set (only `persistent-postgresql`). So the test does **not** catch: a wrong `UniqueKupoMatch` key, a
  projection table omitted from the `rollbackTo` wipe list, or a wrong SQL `Asc` clause that diverges
  from `replayOrder`. Those are covered by the build + a future Postgres integration test.
- F-05 (same-block collision) is pinned only at the *ordering/dedup intent* level; the real 4-field
  `UniqueKupoMatch` collision behavior needs the integration test.
- F-02 end-to-end (lexicographic vs numeric SQL comparison) is only caught by the Postgres integration
  test; here the build confirms the `bigint` column type and `replayOrder` confirms numeric intent.

A follow-up integration test (add `persistent-sqlite` and run the real `rollbackTo` against
`:memory:`, or a docker-compose Postgres round-trip) is noted as the way to pin the apply half; it is
out of scope for this sub-project per the chosen "pure replay-fold" decision.

---

## 8. Files touched

| File | Changes |
|------|---------|
| `src/lib/offchain-lib/Storage.hs` | `PersistField Integer`→Int64; `OnchainMatchEvent` (+createdTxId,+createdTxIndex,+createdOutputIndex, 4-field key); `ChainSyncConfig` (+`schemaVersion Int default=1`); `rollbackTo` rewrite (slot-only arg, wipe+replay); pure `replayOrder`; split `putMatchAndProjections` → `putKupoMatch` + `projectAndStore`; `wipeChainSyncTablesRaw` (drop-only) |
| `src/lib/chainsync-lib/KupoAtlas.hs` | `toGYAssetClass` → `Either` w/ `parseAssetClassWithoutSep` fallback; `kupoValueToGYValue` → `Either` (`traverse`); strict bind `amValue <-` in `kupoMatchToAtlasMatch` |
| `src/exe/chain-sync/Main.hs` | **pre-migration** raw schema-version+policy probe → raw DROP → then `migrateAll` (F-02/F-05 blocker fix); remove `findCheckpoint` dance (F-04); `Behind` tip (F-03); `Ahead` + hash-mismatch margin rollback w/ explicit `(rollbackSlot,"")` cursor (F-08); `ROLLBACK_MARGIN` clamp `max 1`; `readMaybe` (F-42); health flags (F-31) |
| `src/exe/chain-sync/ChainSyncLogic.hs` | `fetchingMatches` restructure (F-10); remove `findCheckpoint`; `rollbackTo` call now takes `networkId`; wire margin |
| `src/exe/chain-sync/Constants.hs` (or equivalent) | `currentSchemaVersion = 2`, `defaultRollbackMargin = 2160` |
| `Decentralized-Belt-System.cabal` | add `chainsync-lib` to the `test-suite` `build-depends` |
| `src/test/UnitTests/ChainSyncReplay.hs` (new) + `UnitTests.hs` | test driving real `projectChainEvent` + real `replayOrder` + wiring |

## 9. Out of scope

Streams A (secrets), C (API hardening, incl. deferred F-13), D (offchain correctness), E (on-chain
hardening), F (extended test coverage), G (cleanup) — each its own spec → plan → implementation cycle.

## 10. Rollout / verification

1. `cabal build all` — type-checks, confirms `bigint` schema.
2. `cabal test` — `ChainSyncReplay` passes; existing suite green.
3. Manual scenario trace (documented in the plan) for all 4 `evaluateChainSyncState` branches +
   first-run + deep-fork + same-block multi-match + create-then-update-then-rollback.
4. Deploy note called out: first run of this branch wipes + re-syncs the chain-sync DB.
