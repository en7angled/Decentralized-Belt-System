# Chain-Sync Integrity Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Make the chain-sync Postgres projections a faithful, rollback-safe reflection of on-chain state by fixing the interconnected integrity cluster (F-02/03/04/05/06/07/08/10 + test F-33 + co-located F-31/F-42).

**Architecture:** Adopt an explicit model — `OnchainMatchEvent` is an append-only raw-match log (the source-of-truth cache); projection tables are pure derived state, rebuilt by replaying the surviving log through the existing projection logic. Schema changes deploy via wipe-**before**-migrate + full re-sync (the DB is a disposable cache). Fork recovery uses a bounded rollback margin.

**Tech Stack:** Haskell (GHC 9.6, Cabal), `persistent` + `persistent-postgresql`, Atlas (GeniusYield), Kupo indexer client (Servant), `tasty` + `tasty-hunit`.

**Design spec:** `docs/superpowers/specs/2026-07-04-chainsync-integrity-design.md` (adversarially verified).

## Global Constraints

- Build: `cabal build all` from repo root (Nix + direnv env; run `direnv allow` first). Test: `cabal test`.
- Haskell style: max 120 chars, 2-space indent, no tabs; Haddock `-- |` on new exports; one blank line between top-level decls.
- No raw `error`/string exceptions in new code — **except** the one documented, unreachable overflow guard in `PersistField Integer` (§Task 1).
- `currentSchemaVersion = 2` (1 = implicit pre-fix schema). `defaultRollbackMargin = 2160`. `ROLLBACK_MARGIN` is clamped to `max 1` (never 0).
- Replay order is the single pure function `replayOrder`; `rollbackTo` and tests both use it. Sort key: `(createdSlot, createdTxIndex, createdOutputIndex)` — never `createdTxId`.
- Deploy note (must land in the PR description): the first deploy of this branch **wipes + re-syncs** the chain-sync DB. No manual DB surgery.
- Branch: `fix/chainsync-integrity` (already created). Commit after every task.

---

## File Structure

| File | Responsibility | Tasks |
|------|----------------|-------|
| `src/lib/offchain-lib/Storage.hs` | schema, `PersistField Integer`, raw-match log, projections, `rollbackTo`, `replayOrder`, schema-version helpers | 1,2,3,4,5,6 |
| `src/lib/chainsync-lib/KupoAtlas.hs` | Kupo→Atlas conversion; `Either`-safe value decoding | 7 |
| `src/exe/chain-sync/Main.hs` | startup (pre-migration wipe, origin cursor), sync loop, fork recovery, health metrics | 4,8,9,10,11 |
| `src/exe/chain-sync/ChainSyncLogic.hs` | tip comparison, batch fetch, `fetchingMatches` | 8,9 |
| `src/exe/chain-sync/Constants.hs` | `defaultRollbackMargin` | 10 |
| `Decentralized-Belt-System.cabal` | add `chainsync-lib` + `UnitTests.ChainSyncReplay` to test-suite | 5 |
| `src/test/UnitTests/ChainSyncReplay.hs` (new) | pure tests: `replayOrder` ordering, `Either` value conversion | 5,7 |
| `src/test/UnitTests.hs` | wire `chainSyncReplayTests` into the suite | 5 |

**Dependency order:** 1 → 2 → 3 → 4 → 5 → 6 → 7 → 8 → 9 → 10 → 11. Each task leaves the tree building.

---

### Task 1: `PersistField Integer` → bigint (F-02)

**Files:**
- Modify: `src/lib/offchain-lib/Storage.hs` (line 43 region + imports)

**Interfaces:**
- Consumes: nothing new.
- Produces: `instance PersistField Integer` / `instance PersistFieldSql Integer` mapping to `PersistInt64`/`SqlInt64`; all `Integer` persist columns become `bigint`.

- [ ] **Step 1: Add `Int64` import**

In the import block of `Storage.hs`, add:

```haskell
import Data.Int (Int64)
```

- [ ] **Step 2: Replace the JSON-text Integer instance with a bigint instance**

Delete this line (Storage.hs:43):

```haskell
derivePersistFieldJSON "Integer"
```

Add, immediately after the `derivePersistFieldJSON "GYAssetClass"` line, this instance (the file already has `{-# OPTIONS_GHC -Wno-orphans #-}` and an orphan `PersistField GYTime`, so this is consistent):

```haskell
-- | Persist 'Integer' columns (chain slots, interval numbers) as PostgreSQL @bigint@ so SQL
-- comparisons and @ORDER BY@ are numeric, not lexicographic. Cardano slots are ~10^9, far under
-- 'Int64' range; the overflow guard is defensive and unreachable for real slot/interval values.
instance PersistField Integer where
  toPersistValue i
    | i >= toInteger (minBound :: Int64) && i <= toInteger (maxBound :: Int64) =
        PersistInt64 (fromInteger i)
    | otherwise = error "Storage: Integer out of Int64 range for bigint persistence"
  fromPersistValue (PersistInt64 n) = Right (toInteger n)
  fromPersistValue x = Left ("Expected PersistInt64 for Integer, got: " <> Text.pack (show x))

instance PersistFieldSql Integer where
  sqlType _ = SqlInt64
```

- [ ] **Step 3: Build**

Run: `cabal build all`
Expected: PASS (compiles; `mkPersist` now generates `INT8` columns for every `Integer` field).

- [ ] **Step 4: Commit**

```bash
git add src/lib/offchain-lib/Storage.hs
git commit -m "fix(storage): persist Integer slot columns as bigint, not JSON text (F-02)"
```

---

### Task 2: `OnchainMatchEvent` raw-match key + columns (F-05)

**Files:**
- Modify: `src/lib/offchain-lib/Storage.hs` (`OnchainMatchEvent` entity + `putKupoMatch`)

**Interfaces:**
- Consumes: `KupoMatch` fields `transaction_id :: Text`, `transaction_index :: Int`, `output_index :: Int` (from `KupoClient`).
- Produces: `OnchainMatchEvent` with fields `createdTxId`, `createdTxIndex`, `createdOutputIndex` and 4-field `UniqueKupoMatch createdSlot createdHeader createdTxId createdOutputIndex`; accessors `onchainMatchEventCreatedTxIndex` / `onchainMatchEventCreatedOutputIndex` used by `replayOrder` (Task 5).

- [ ] **Step 1: Extend the entity**

Replace the `OnchainMatchEvent` block in the `persistLowerCase` quasi-quote (Storage.hs:92-97) with:

```
OnchainMatchEvent
    createdSlot        Integer
    createdHeader      Text
    createdTxId        Text
    createdTxIndex     Int
    createdOutputIndex Int
    kupoMatch          KupoMatch
    UniqueKupoMatch createdSlot createdHeader createdTxId createdOutputIndex
    deriving Show
```

- [ ] **Step 2: Populate the new columns and the 4-field key in `putKupoMatch`**

Replace `putKupoMatch` (Storage.hs:259-264) with:

```haskell
-- | Store a raw Kupo match event, upserting by (slot, header, txId, outputIndex) — the on-chain
-- identity of the output, so multiple matches in the same block no longer overwrite each other.
putKupoMatch :: (MonadIO m) => KupoMatch -> SqlPersistT m ()
putKupoMatch km = do
  let cSlot = slot_no (created_at km)
      cHash = header_hash (created_at km)
      ev = OnchainMatchEvent cSlot cHash (transaction_id km) (transaction_index km) (output_index km) km
  upsertByUnique
    ( \e ->
        UniqueKupoMatch
          (onchainMatchEventCreatedSlot e)
          (onchainMatchEventCreatedHeader e)
          (onchainMatchEventCreatedTxId e)
          (onchainMatchEventCreatedOutputIndex e)
    )
    ev
```

- [ ] **Step 3: Build**

Run: `cabal build all`
Expected: PASS.

- [ ] **Step 4: Commit**

```bash
git add src/lib/offchain-lib/Storage.hs
git commit -m "fix(storage): key raw matches by slot+header+txId+outputIndex (F-05)"
```

---

### Task 3: `ChainSyncConfig` schema version (migration gate)

**Files:**
- Modify: `src/lib/offchain-lib/Storage.hs` (`ChainSyncConfig` entity, `currentSchemaVersion`, `putStoredPolicyHexText`)

**Interfaces:**
- Produces: `currentSchemaVersion :: Int`; `ChainSyncConfig` with `schemaVersion Int default=1`; `putStoredPolicyHexText` writes `currentSchemaVersion`.

- [ ] **Step 1: Add the schema-version constant**

After `runMigrations` (Storage.hs:177-178), add:

```haskell
-- | Bump when the chain-sync DB schema changes incompatibly; startup wipes + re-syncs on a mismatch.
currentSchemaVersion :: Int
currentSchemaVersion = 2
```

- [ ] **Step 2: Add the column (with a SQL default so a future non-wipe ADD COLUMN cannot fail)**

Replace the `ChainSyncConfig` block (Storage.hs:85-89) with:

```
ChainSyncConfig
    singleton        Bool
    policyHexText    Text
    schemaVersion    Int default=1
    UniqueChainSyncConfig singleton
    deriving Show
```

- [ ] **Step 3: Write the current version alongside the policy**

Replace `putStoredPolicyHexText` (Storage.hs:205-207) with:

```haskell
-- | Upsert the singleton config row with the given policy hex and the current schema version.
putStoredPolicyHexText :: (MonadIO m) => Text -> SqlPersistT m ()
putStoredPolicyHexText policyHexText =
  upsertByUnique (const (UniqueChainSyncConfig True)) (ChainSyncConfig True policyHexText currentSchemaVersion)
```

- [ ] **Step 4: Build**

Run: `cabal build all`
Expected: PASS.

- [ ] **Step 5: Commit**

```bash
git add src/lib/offchain-lib/Storage.hs
git commit -m "feat(storage): add ChainSyncConfig.schemaVersion + currentSchemaVersion gate"
```

---

### Task 4: Pre-migration wipe (blocker fix — migrate must not ALTER old tables)

**Files:**
- Modify: `src/lib/offchain-lib/Storage.hs` (add `readSchemaProbe`, `wipeChainSyncTablesRaw`)
- Modify: `src/exe/chain-sync/Main.hs` (startup restructure)

**Interfaces:**
- Consumes: `currentSchemaVersion` (Task 3), `chainSyncTableNames`, `putStoredPolicyHexText`.
- Produces: `readSchemaProbe :: (MonadIO m) => SqlPersistT m (Maybe (Int, Text))`; `wipeChainSyncTablesRaw :: (MonadIO m) => SqlPersistT m ()`.

- [ ] **Step 1: Add the raw, schema-tolerant probe and a drop-only wipe to `Storage.hs`**

Add after `wipeChainSyncTables` (Storage.hs:226-230):

```haskell
-- | Drop all chain-sync tables WITHOUT re-migrating (migration happens separately, after this).
wipeChainSyncTablesRaw :: (MonadIO m) => SqlPersistT m ()
wipeChainSyncTablesRaw =
  forM_ chainSyncTableNames $ \tableName ->
    rawExecute ("DROP TABLE IF EXISTS " <> tableName <> " CASCADE") []

-- | Read (schemaVersion, policyHex) from an existing @chain_sync_config@ via raw SQL, tolerant of the
-- table or the @schema_version@ column being absent (pre-fix DBs). Returns 'Nothing' if the table does
-- not exist (fresh install). A pre-fix table without the column reports version 1.
readSchemaProbe :: (MonadIO m) => SqlPersistT m (Maybe (Int, Text))
readSchemaProbe = do
  tbl <-
    rawSql
      "SELECT EXISTS (SELECT 1 FROM information_schema.tables WHERE table_name = 'chain_sync_config')"
      []
  case tbl of
    [Single True] -> do
      col <-
        rawSql
          "SELECT EXISTS (SELECT 1 FROM information_schema.columns WHERE table_name = 'chain_sync_config' AND column_name = 'schema_version')"
          []
      case col of
        [Single True] -> do
          rows <- rawSql "SELECT schema_version, policy_hex_text FROM chain_sync_config LIMIT 1" []
          pure $ case rows of
            [(Single v, Single p)] -> Just (v, p)
            _ -> Just (1, "")
        _ -> do
          rows <- rawSql "SELECT policy_hex_text FROM chain_sync_config LIMIT 1" []
          pure $ case rows of
            [Single p] -> Just (1, p)
            _ -> Just (1, "")
    _ -> pure Nothing
```

- [ ] **Step 2: Update the `Storage` import list in `Main.hs`**

Replace the `Storage` import (Main.hs:29) with:

```haskell
import Storage (currentSchemaVersion, putStoredPolicyHexText, readSchemaProbe, rollbackTo, runMigrations, wipeChainSyncTablesRaw)
```

- [ ] **Step 3: Restructure startup — load config, probe, wipe-if-needed, THEN migrate**

Three surgical edits (the existing config bindings and `batchSize`/`fetchBatchSize` bindings stay put):

**(a)** Delete the early migration line (Main.hs:52):

```haskell
  runSqlPool runMigrations pool
```

**(b)** Immediately AFTER the existing `policyHexText` binding (Main.hs:58), insert the probe → wipe → migrate → store block (config is already loaded just above it; do NOT re-declare it):

```haskell
  -- Probe the existing schema BEFORE migrating. Wipe (drop) the chain-sync tables if the schema
  -- version or minting policy changed, so migrateAll only CREATEs fresh tables and never runs an
  -- incompatible in-place ALTER (varchar->bigint / ADD COLUMN NOT NULL) that Postgres would reject.
  mProbe <- runSqlPool readSchemaProbe pool
  let needWipe = case mProbe of
        Nothing -> False
        Just (storedVersion, storedPolicy) -> storedVersion < currentSchemaVersion || storedPolicy /= policyHexText
  when needWipe $ do
    putStrLn "Schema version or policy changed; dropping chain-sync tables before migration."
    runSqlPool wipeChainSyncTablesRaw pool
  runSqlPool runMigrations pool
  runSqlPool (putStoredPolicyHexText policyHexText) pool
```

**(c)** Delete the old post-migration wipe block (Main.hs:67-80), i.e. from `stored <- runSqlPool getStoredPolicyHexText pool` through the final `runSqlPool (putStoredPolicyHexText policyHexText) pool` of the `case stored of …`:

```haskell
  stored <- runSqlPool getStoredPolicyHexText pool
  case stored of
    Nothing -> do
      liftIO $ putStrLn "First run: no stored policy; storing current policy."
      runSqlPool (putStoredPolicyHexText policyHexText) pool
    Just storedHex
      | storedHex == policyHexText ->
          liftIO $ putStrLn "Stored policy matches current policy; skipping wipe."
      | otherwise -> do
          liftIO $
            putStrLn $
              "Policy changed (stored: " <> T.unpack storedHex <> ", current: " <> T.unpack policyHexText <> "); wiping chain-sync tables and storing current policy."
          runSqlPool wipeChainSyncTables pool
          runSqlPool (putStoredPolicyHexText policyHexText) pool
```

- [ ] **Step 4: Build**

Run: `cabal build all`
Expected: PASS. (`when` comes from `Control.Monad.Extra`, already imported; `rawSql`/`Single`/`rawExecute` from `Database.Persist.Sql`, already imported in Storage.)

- [ ] **Step 5: Manual verification note**

Full end-to-end needs a populated pre-fix DB. Verify by reasoning: on an existing DB, `readSchemaProbe` returns `Just (1, policy)`; `1 < 2` ⇒ wipe drops tables ⇒ `runMigrations` then only CREATEs `bigint`/4-field-key tables. On a fresh DB, `readSchemaProbe` returns `Nothing` ⇒ no wipe ⇒ migrate creates fresh. Neither path ALTERs a populated table.

- [ ] **Step 6: Commit**

```bash
git add src/lib/offchain-lib/Storage.hs src/exe/chain-sync/Main.hs
git commit -m "fix(chainsync): wipe chain-sync tables before migrate, gated by schema version (blocker)"
```

---

### Task 5: `replayOrder` + pure test module (F-33 ordering)

**Files:**
- Modify: `src/lib/offchain-lib/Storage.hs` (add `replayOrder`)
- Create: `src/test/UnitTests/ChainSyncReplay.hs`
- Modify: `src/test/UnitTests.hs` (wire the group in)
- Modify: `Decentralized-Belt-System.cabal` (test-suite: add module + `chainsync-lib`)

**Interfaces:**
- Consumes: `OnchainMatchEvent` accessors (Task 2).
- Produces: `replayOrder :: [OnchainMatchEvent] -> [OnchainMatchEvent]`; `chainSyncReplayTests :: TestTree`.

- [ ] **Step 1: Write the failing test module**

Create `src/test/UnitTests/ChainSyncReplay.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- | Pure tests for the chain-sync replay/rollback ordering (F-02/F-05/F-07).
module UnitTests.ChainSyncReplay (chainSyncReplayTests) where

import Data.Map qualified as Map
import KupoClient (CreatedAt (..), KupoMatch (..), KupoValue (..))
import Storage
  ( OnchainMatchEvent (..),
    onchainMatchEventCreatedOutputIndex,
    onchainMatchEventCreatedSlot,
    onchainMatchEventCreatedTxIndex,
    replayOrder,
  )
import Test.Tasty
import Test.Tasty.HUnit

-- A raw match whose Kupo payload is irrelevant to ordering.
dummyMatch :: KupoMatch
dummyMatch =
  KupoMatch
    { transaction_index = 0,
      transaction_id = "tx",
      output_index = 0,
      address = "addr",
      value = KupoValue 0 Map.empty,
      datum_hash = Nothing,
      datum_type = Nothing,
      datum = Nothing,
      script_hash = Nothing,
      created_at = CreatedAt 0 "h",
      spent_at = Nothing
    }

ev :: Integer -> Int -> Int -> OnchainMatchEvent
ev slot txIx outIx = OnchainMatchEvent slot "hdr" "tx" txIx outIx dummyMatch

chainSyncReplayTests :: TestTree
chainSyncReplayTests =
  testGroup
    "Chain-sync replay ordering"
    [ testCase "sorts across the 8-digit/9-digit slot boundary numerically (F-02)" $
        map onchainMatchEventCreatedSlot (replayOrder [ev 100000000 0 0, ev 99999999 0 0])
          @?= [99999999, 100000000],
      testCase "same-block matches ordered by (txIndex, outputIndex); all retained (F-05/F-07)" $
        map (\e -> (onchainMatchEventCreatedTxIndex e, onchainMatchEventCreatedOutputIndex e))
          (replayOrder [ev 5 2 0, ev 5 1 3, ev 5 1 0])
          @?= [(1, 0), (1, 3), (2, 0)]
    ]
```

- [ ] **Step 2: Wire the module into the cabal test-suite**

In `Decentralized-Belt-System.cabal`, in the `test-suite test` stanza (starts line 430): add `UnitTests.ChainSyncReplay` to `other-modules`, and add `, chainsync-lib` to the first `build-depends` block (next to `offchain-lib`):

```
  other-modules:
    ...
    UnitTests.ChainSyncReplay
```

```
  build-depends:
    , base
    , chainsync-lib
    , mcp
    ...
```

- [ ] **Step 3: Wire the group into `UnitTests.hs`**

Add the import and list entry in `src/test/UnitTests.hs`:

```haskell
import UnitTests.ChainSyncReplay (chainSyncReplayTests)
```

Add `chainSyncReplayTests,` to the `unitTests` `testGroup` list.

- [ ] **Step 4: Run the test to verify it FAILS (replayOrder undefined)**

Run: `cabal test 2>&1 | tail -20`
Expected: FAIL — `Variable not in scope: replayOrder` (or module compile error).

- [ ] **Step 5: Implement `replayOrder` in `Storage.hs`**

Add near `rollbackTo` (Storage.hs, before line 406):

```haskell
-- | The single source of chain-replay order: sort raw matches by true chain order
-- @(slot, transaction_index, output_index)@. Used by 'rollbackTo' and by tests.
replayOrder :: [OnchainMatchEvent] -> [OnchainMatchEvent]
replayOrder =
  L.sortOn
    ( \e ->
        ( onchainMatchEventCreatedSlot e,
          onchainMatchEventCreatedTxIndex e,
          onchainMatchEventCreatedOutputIndex e
        )
    )
```

(`L` is the existing `import Data.List qualified as L`.)

- [ ] **Step 6: Run the test to verify it PASSES**

Run: `cabal test 2>&1 | tail -20`
Expected: PASS — "Chain-sync replay ordering" group green; full suite green.

- [ ] **Step 7: Commit**

```bash
git add src/lib/offchain-lib/Storage.hs src/test/UnitTests/ChainSyncReplay.hs src/test/UnitTests.hs Decentralized-Belt-System.cabal
git commit -m "test(chainsync): pin replay ordering (slot,txIndex,outputIndex); add replayOrder (F-33)"
```

---

### Task 6: Rewrite `rollbackTo` as wipe + replay (F-07)

**Files:**
- Modify: `src/lib/offchain-lib/Storage.hs` (`putMatchAndProjections` split, `rollbackTo`)
- Modify: `src/exe/chain-sync/Main.hs` (update the `Ahead` caller to the new signature — interim behavior)

**Interfaces:**
- Consumes: `replayOrder` (Task 5), `projectChainEvent`, all `put*Projection`.
- Produces: `projectAndStore :: (MonadIO m) => GYNetworkId -> KupoMatch -> SqlPersistT m ()`; `rollbackTo :: (MonadIO m) => GYNetworkId -> Integer -> SqlPersistT m ()` (header param dropped; safe because callers pass a slot strictly below any orphan — Task 10 guarantees `rollbackMargin ≥ 1`).

- [ ] **Step 1: Split `putMatchAndProjections` into `putKupoMatch` + `projectAndStore`**

Replace `putMatchAndProjections` (Storage.hs:233-256) with:

```haskell
-- | Store a raw Kupo match and its derived projections.
putMatchAndProjections :: (MonadIO m) => GYNetworkId -> KupoMatch -> SqlPersistT m ()
putMatchAndProjections networkId km = do
  putKupoMatch km
  projectAndStore networkId km

-- | Derive and store the projections for a raw match (does NOT re-store the raw match). Reused by
-- 'rollbackTo' to replay the surviving log. Conversion/projection failures are logged and skipped.
projectAndStore :: (MonadIO m) => GYNetworkId -> KupoMatch -> SqlPersistT m ()
projectAndStore networkId km =
  case kupoMatchToAtlasMatch km of
    Left convErr -> liftIO $ putStrLn ("Conversion error: " <> convErr)
    Right am -> do
      let slotNoInt = slot_no (created_at km)
          header = header_hash (created_at km)
      ev <- runExceptT (projectChainEvent networkId am)
      case ev of
        Left e -> liftIO $ putStrLn ("Projection error: " <> show e)
        Right proj -> case proj of
          RankEvent r -> do
            putRankProjection slotNoInt header r
            deletePromotionProjection (rankId r)
          ProfileEvent p -> putProfileProjection slotNoInt header p
          PromotionEvent pr -> putPromotionProjection slotNoInt header pr
          MembershipHistoryEvent mh -> putMembershipHistoryProjection slotNoInt header mh
          MembershipIntervalEvent mi -> do
            mOrg <- resolveOrganizationForInterval mi
            putMembershipIntervalProjection slotNoInt header mi mOrg
          AchievementEvent a -> putAchievementProjection slotNoInt header a
          NoEvent _ -> pure ()
```

- [ ] **Step 2: Rewrite `rollbackTo`**

Replace `rollbackTo` and its `where` block (Storage.hs:406-429) with:

```haskell
-- | Roll back to @slot@: discard raw matches beyond it, wipe the derived projection tables, and
-- rebuild them by replaying the surviving raw-match log in chain order. Correct because callers pass
-- a slot strictly below any orphan (guaranteed by the F-08 rollback margin >= 1).
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
  -- 3. Replay surviving raw matches in true chain order.
  surviving <- selectList [] []
  mapM_ (projectAndStore networkId . onchainMatchEventKupoMatch) (replayOrder (map entityVal surviving))
```

- [ ] **Step 3: Update the `Ahead` caller in `Main.hs` to the new signature (interim behavior)**

In `Main.hs`, in the `Ahead` branch, replace:

```haskell
        runSqlPool (rollbackTo (ck_slot_no blockchainTip) (ck_header_hash blockchainTip)) pool
```

with:

```haskell
        runSqlPool (rollbackTo networkId (ck_slot_no blockchainTip)) pool
```

(Task 10 refines this to the margin-based rollback. `networkId` is in scope.)

- [ ] **Step 4: Build**

Run: `cabal build all`
Expected: PASS. (`Filter`, `selectList`, `entityVal`, `deleteWhere` from `Database.Persist`, already imported.)

- [ ] **Step 5: Manual verification note**

DB-level behavior needs the deferred Postgres integration test. Reason: replay reconstructs deletions via the successor `RankEvent` match (never by stale `spent_at`); ordering by `replayOrder` (Task 5, tested) guarantees the rank match applies after the promotion match.

- [ ] **Step 6: Commit**

```bash
git add src/lib/offchain-lib/Storage.hs src/exe/chain-sync/Main.hs
git commit -m "fix(storage): rollbackTo rebuilds projections by replaying surviving raw log (F-07)"
```

---

### Task 7: `Either`-safe Kupo value conversion (F-06)

**Files:**
- Modify: `src/lib/chainsync-lib/KupoAtlas.hs` (`toGYAssetClass`, `kupoValueToGYValue`, `kupoMatchToAtlasMatch`)
- Modify: `src/test/UnitTests/ChainSyncReplay.hs` (add conversion tests)

**Interfaces:**
- Consumes: `parseAssetClassWithSep`, `parseAssetClassWithoutSep` (from `GeniusYield.Types`, already imported).
- Produces: `toGYAssetClass :: Text -> Either String GYAssetClass`; `kupoValueToGYValue :: KupoValue -> Either String GYValue`.

- [ ] **Step 1: Write the failing conversion tests**

In `src/test/UnitTests/ChainSyncReplay.hs`, add imports:

```haskell
import Data.Either (isLeft, isRight)
import KupoAtlas (toGYAssetClass)
```

Add this group and include it in `chainSyncReplayTests`'s list:

```haskell
valueConversionTests :: TestTree
valueConversionTests =
  testGroup
    "Kupo asset conversion (F-06)"
    [ testCase "dotted policyId.assetName parses" $
        isRight (toGYAssetClass "00000000000000000000000000000000000000000000000000000000.6162") @?= True,
      testCase "dotless empty-name policyId parses instead of crashing" $
        isRight (toGYAssetClass "00000000000000000000000000000000000000000000000000000000") @?= True,
      testCase "garbage returns Left, not a crash" $
        isLeft (toGYAssetClass "xyz") @?= True
    ]
```

Change the `chainSyncReplayTests` list to `[ ..., valueConversionTests ]`.

- [ ] **Step 2: Run to verify it FAILS (type mismatch: `toGYAssetClass` returns `GYAssetClass`, not `Either`)**

Run: `cabal test 2>&1 | tail -20`
Expected: FAIL — `isRight`/`isLeft` cannot apply to `GYAssetClass` (type error), or old `error`-based value crashes.

- [ ] **Step 3: Make the conversion total in `KupoAtlas.hs`**

Replace `kupoValueToGYValue` and `toGYAssetClass` (KupoAtlas.hs:110-121) with:

```haskell
-- | Convert a Kupo value (lovelace + native assets) into an Atlas 'GYValue', failing on any
-- unparseable asset key rather than crashing the sync loop.
kupoValueToGYValue :: KupoValue -> Either String GYValue
kupoValueToGYValue KupoValue {coins, assets} = do
  gyAssets <- traverse (\(k, v) -> (,) <$> toGYAssetClass k <*> pure v) (Map.toList assets)
  pure $ valueFromList ((GYLovelace, coins) : gyAssets)

-- | Parse a Kupo asset key into an Atlas 'GYAssetClass'. Kupo renders an empty-token-name asset as a
-- bare 56-char policy id (no separator), so fall back to the separatorless parser on failure.
toGYAssetClass :: Text -> Either String GYAssetClass
toGYAssetClass t = case parseAssetClassWithSep '.' t of
  Right assetClass -> Right assetClass
  Left _ -> parseAssetClassWithoutSep t
```

- [ ] **Step 4: Thread the `Either` through `kupoMatchToAtlasMatch`**

In `kupoMatchToAtlasMatch` (KupoAtlas.hs:82-108), add a bind in the `do` block (e.g. after the `createdSlot` bind) and use it in the record:

```haskell
  gyValue <- kupoValueToGYValue value
```

and change the record field from `amValue = kupoValueToGYValue value` to:

```haskell
        amValue = gyValue,
```

- [ ] **Step 5: Run the tests to verify they PASS**

Run: `cabal test 2>&1 | tail -20`
Expected: PASS — "Kupo asset conversion (F-06)" green; full suite green.

- [ ] **Step 6: Commit**

```bash
git add src/lib/chainsync-lib/KupoAtlas.hs src/test/UnitTests/ChainSyncReplay.hs
git commit -m "fix(chainsync): total Either-based Kupo asset conversion; no crash on empty-name tokens (F-06)"
```

---

### Task 8: Start from origin on empty cursor; drop `findCheckpoint` (F-04)

**Files:**
- Modify: `src/exe/chain-sync/Main.hs` (remove startup checkpoint dance + unused `batchSize`)
- Modify: `src/exe/chain-sync/ChainSyncLogic.hs` (remove `findCheckpoint`, its export, and the now-unused import)

**Interfaces:**
- Consumes: `getLocalTip` (returns `(0,"")` on empty cursor).
- Produces: startup no longer overwrites the cursor; empty DB syncs from slot 0.

- [ ] **Step 1: Remove the startup checkpoint dance in `Main.hs`**

Delete these two lines (Main.hs:83-84):

```haskell
  startingCheckPoint <- findCheckpoint kupoUrl batchSize (ck_slot_no initialTip)
  updateLocalTip pool startingCheckPoint
```

(`initialTip` remains, used by the metrics `smLocalTip = ck_slot_no initialTip`.)

- [ ] **Step 2: Remove the now-unused `BATCH_SIZE` binding**

Delete the `batchSize` binding (Main.hs:60-62); it was only consumed by `findCheckpoint`. Leave `fetchBatchSize` (still used).

- [ ] **Step 3: Remove `findCheckpoint` from `ChainSyncLogic.hs`**

- Delete `findCheckpoint` (ChainSyncLogic.hs:59-75).
- Remove `findCheckpoint,` from the module export list (ChainSyncLogic.hs:6-13).
- Remove `runKupoCheckpointBySlot` from the `KupoClient` import (ChainSyncLogic.hs:24), leaving `KupoClient (KupoCheckpoint (..), KupoMatch (..), runKupoCheckpointsList, runKupoMatches)`.

- [ ] **Step 4: Remove the `findCheckpoint` import from `Main.hs`**

`findCheckpoint` is imported via `import ChainSyncLogic` (whole module) — no explicit import line to edit. Confirm `Main.hs` no longer references `findCheckpoint`.

- [ ] **Step 5: Build**

Run: `cabal build all`
Expected: PASS, with no `-Wunused` warning for `batchSize` or `runKupoCheckpointBySlot`.

- [ ] **Step 6: Manual verification note**

On an empty cursor, `getLocalTip` returns `(0,"")`; `evaluateChainSyncState` yields `Behind` (slot-only comparison), so the first fetch pulls `created_after=0` (everything Kupo has). On restart with a populated cursor, it is used as-is.

- [ ] **Step 7: Commit**

```bash
git add src/exe/chain-sync/Main.hs src/exe/chain-sync/ChainSyncLogic.hs
git commit -m "fix(chainsync): sync from origin on empty cursor; remove forward-walk findCheckpoint (F-04)"
```

---

### Task 9: `Behind` cursor + `fetchingMatches` restructure (F-03, F-10)

**Files:**
- Modify: `src/exe/chain-sync/Main.hs` (`Behind` branch)
- Modify: `src/exe/chain-sync/ChainSyncLogic.hs` (`fetchingMatches`)

**Interfaces:**
- Produces: `Behind` writes the cursor to the bound actually fetched; `fetchingMatches` is tail-recursive with the continuation only on success.

- [ ] **Step 1: Fix the `Behind` cursor (F-03)**

In `Main.hs`, in the `Behind` branch, replace:

```haskell
        fetchingMatches metricsVar kupoUrl matchPattern policyHexText networkId pool (ck_slot_no localTip) (ck_slot_no blockchainTip) fetchBatchSize
        blockchainTip' <- getBlockchainTip kupoUrl
        updateLocalTip pool blockchainTip'
```

with:

```haskell
        fetchingMatches metricsVar kupoUrl matchPattern policyHexText networkId pool (ck_slot_no localTip) (ck_slot_no blockchainTip) fetchBatchSize
        updateLocalTip pool blockchainTip
```

- [ ] **Step 2: Restructure `fetchingMatches` (F-10)**

Replace `fetchingMatches` (ChainSyncLogic.hs:93-132) with:

```haskell
-- | Recursively fetch and project Kupo matches in batches from @start@ to @end@. The continuation
-- recurses only on success; on error it tail-retries the SAME window. Windows overlap by one slot
-- (Kupo bounds are inclusive) — duplicate-safe via idempotent upserts.
fetchingMatches :: MVar SyncMetrics -> String -> T.Text -> T.Text -> GYNetworkId -> ConnectionPool -> Integer -> Integer -> Integer -> IO ()
fetchingMatches metricsVar kupoUrl matchPattern policyHexText networkId pool start end batchSize
  | end <= start = putStrLn "No more matches to fetch"
  | otherwise = do
      let endInterval = if (start + batchSize) > end then end else start + batchSize
      putStrLn ("Fetching matches from " <> show start <> " to " <> show endInterval)
      eMatches <-
        runKupoMatches
          kupoUrl
          matchPattern
          (Just policyHexText)
          Nothing
          Nothing
          Nothing
          (Just start)
          (Just endInterval)
          Nothing
          Nothing
          (Just "oldest_first")
          False
          False
          True
      case eMatches of
        Left err -> do
          putStrLn ("Kupo client error: " <> show err)
          putStrLn "Retrying in 10 seconds"
          threadDelay 10000000
          fetchingMatches metricsVar kupoUrl matchPattern policyHexText networkId pool start end batchSize
        Right matches -> do
          applyMatches networkId pool matches
          now <- getCurrentTime
          modifyMVar_ metricsVar $ \m -> pure m {smLocalTip = endInterval, smLastSyncTime = now}
          fetchingMatches metricsVar kupoUrl matchPattern policyHexText networkId pool endInterval end batchSize
```

- [ ] **Step 3: Build**

Run: `cabal build all`
Expected: PASS.

- [ ] **Step 4: Manual verification note**

`Behind` now writes exactly the bound fetched (no `(blockchainTip, blockchainTip']` gap). `fetchingMatches` no longer double-processes after a retry, and each recursion is in tail position.

- [ ] **Step 5: Commit**

```bash
git add src/exe/chain-sync/Main.hs src/exe/chain-sync/ChainSyncLogic.hs
git commit -m "fix(chainsync): write fetched bound as cursor (F-03); tail-recursive fetch retry (F-10)"
```

---

### Task 10: Fork recovery with bounded rollback margin (F-08)

**Files:**
- Modify: `src/exe/chain-sync/Constants.hs` (add `defaultRollbackMargin`)
- Modify: `src/exe/chain-sync/Main.hs` (`ROLLBACK_MARGIN` read + clamp; `Ahead` and `UpToDateButDifferentBlockHash` branches)

**Interfaces:**
- Consumes: `rollbackTo networkId slot` (Task 6), `KupoCheckpoint` constructor.
- Produces: divergence heals within ≤2 iterations for any `rollbackMargin ≥ 1`.

- [ ] **Step 1: Add the default margin constant**

In `src/exe/chain-sync/Constants.hs`, add:

```haskell
-- | Default fork-recovery depth (slots) rolled back on chain divergence. Overridable via ROLLBACK_MARGIN.
defaultRollbackMargin :: Integer
defaultRollbackMargin = 2160
```

- [ ] **Step 2: Add `Text.Read` import to `Main.hs`**

Add:

```haskell
import Text.Read (readMaybe)
```

- [ ] **Step 3: Read and clamp `ROLLBACK_MARGIN`**

In `Main.hs`, near the `fetchBatchSize` binding, add:

```haskell
  rollbackMargin <- do
    mb <- lookupEnv "ROLLBACK_MARGIN"
    pure $ max 1 (maybe defaultRollbackMargin id (mb >>= readMaybe))
```

- [ ] **Step 4: Replace the `Ahead` branch**

In `Main.hs`, replace the whole `Ahead -> do …` branch with:

```haskell
      Ahead -> do
        liftIO $ putStrLn "Chain is ahead"
        let rollbackSlot = max 0 (ck_slot_no blockchainTip - rollbackMargin)
        runSqlPool (rollbackTo networkId rollbackSlot) pool
        updateLocalTip pool (KupoCheckpoint rollbackSlot "")
        liftIO $ putStrLn ("Rolled back to slot " <> show rollbackSlot <> "; will re-sync forward")
```

- [ ] **Step 5: Replace the `UpToDateButDifferentBlockHash` branch**

Replace the whole `UpToDateButDifferentBlockHash -> do …` branch with:

```haskell
      UpToDateButDifferentBlockHash -> do
        liftIO $ putStrLn "Same slot, different block hash; rolling back with margin"
        let rollbackSlot = max 0 (ck_slot_no blockchainTip - rollbackMargin)
        runSqlPool (rollbackTo networkId rollbackSlot) pool
        updateLocalTip pool (KupoCheckpoint rollbackSlot "")
```

- [ ] **Step 6: Build**

Run: `cabal build all`
Expected: PASS.

- [ ] **Step 7: Manual verification note**

`rollbackMargin ≥ 1` ⇒ `rollbackSlot < blockchainSlot` ⇒ next state is `Behind` (slot-only), which re-fetches and, via the F-03 fix, writes a real header ⇒ `UpToDate`. Margin `0` is impossible (clamped), so no equal-slot `""`-header infinite loop.

- [ ] **Step 8: Commit**

```bash
git add src/exe/chain-sync/Constants.hs src/exe/chain-sync/Main.hs
git commit -m "fix(chainsync): heal forks via bounded rollback margin, clamp >= 1 (F-08)"
```

---

### Task 11: Health metrics + robust env parsing (F-31, F-42)

**Files:**
- Modify: `src/exe/chain-sync/Main.hs`

**Interfaces:**
- Produces: `/health` reports `db_ready`/`migrations_complete` truthfully; `FETCH_BATCH_SIZE` no longer crashes on non-numeric input.

- [ ] **Step 1: Report migration/DB readiness (F-31)**

In `Main.hs`, in the `SyncMetrics` record passed to `newMVar` (Main.hs:88-96), change:

```haskell
          smDbReady = False,
          smMigrationsComplete = False,
```

to (migrations and the schema probe have already run against the pool by this point):

```haskell
          smDbReady = True,
          smMigrationsComplete = True,
```

- [ ] **Step 2: Parse `FETCH_BATCH_SIZE` with `readMaybe` (F-42)**

Replace the `fetchBatchSize` binding (Main.hs:63-65) with:

```haskell
  fetchBatchSize <- do
    mb <- lookupEnv "FETCH_BATCH_SIZE"
    pure $ maybe (10_000_000 :: Integer) id (mb >>= readMaybe)
```

(`readMaybe` imported in Task 10.)

- [ ] **Step 3: Build**

Run: `cabal build all`
Expected: PASS.

- [ ] **Step 4: Commit**

```bash
git add src/exe/chain-sync/Main.hs
git commit -m "fix(chainsync): report health readiness; readMaybe for FETCH_BATCH_SIZE (F-31, F-42)"
```

---

## Final verification

- [ ] `cabal build all` — clean.
- [ ] `cabal test` — full suite green, including "Chain-sync replay ordering" and "Kupo asset conversion (F-06)".
- [ ] Re-read `git log --oneline` on `fix/chainsync-integrity`: 11 focused commits.
- [ ] PR description carries the **deploy note**: first deploy wipes + re-syncs the chain-sync DB.

---

## Spec-coverage self-review

| Spec item | Task |
|-----------|------|
| F-02 slots as bigint | 1 |
| F-05 4-field raw-match key | 2 |
| Schema-version gate | 3 |
| Migration blocker (wipe before migrate) | 4 |
| F-07 rollback replay + `replayOrder` | 5, 6 |
| F-33 pure ordering + conversion tests | 5, 7 |
| F-06 Either value conversion | 7 |
| F-04 origin start, drop `findCheckpoint` | 8 |
| F-03 Behind cursor | 9 |
| F-10 `fetchingMatches` restructure | 9 |
| F-08 fork margin, clamp ≥ 1 | 10 |
| F-31 health metrics | 11 |
| F-42 `readMaybe` | 11 |

Deferred (documented, not in this plan): the Postgres integration test that would pin the `SqlPersistT`
apply half end-to-end (wrong Unique key, wipe-list omission, SQL cast). Noted in the spec §7 as a
follow-up.
