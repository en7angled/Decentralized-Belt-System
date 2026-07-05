{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | In-memory SQLite round-trip tests for the Storage layer — the coverage the
-- Fable review said would have caught F-02 and F-05 (F-33.7). Exercises the real
-- 'putKupoMatch' upsert and 'rollbackTo' deletion against a live (in-memory) DB,
-- with dummy Kupo payloads (raw-match identity is all these properties need):
--
--   * same-block matches with distinct (txId, outputIndex) are both retained (F-05);
--   * an identical match is stored once (upsertByUnique idempotence);
--   * rollbackTo deletes strictly-greater slots numerically, not lexicographically (F-02).
module UnitTests.StorageRoundtrip (storageRoundtripTests) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ReaderT)
import Data.Map qualified as Map
import Database.Persist (Filter, selectList)
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Sqlite (runSqlite)
import Data.Text (Text)
import GeniusYield.Types (GYNetworkId (GYTestnetPreview))
import KupoClient (CreatedAt (..), KupoMatch (..), KupoValue (..))
import Storage (OnchainMatchEvent, putKupoMatch, rollbackTo, runMigrations)
import Test.Tasty
import Test.Tasty.HUnit

-- | A raw match with an irrelevant Kupo payload; vary slot / txId / outputIndex.
mkMatch :: Integer -> Text -> Int -> KupoMatch
mkMatch slot txid outIx =
  KupoMatch
    { transaction_index = 0,
      transaction_id = txid,
      output_index = outIx,
      address = "addr",
      value = KupoValue 0 Map.empty,
      datum_hash = Nothing,
      datum_type = Nothing,
      datum = Nothing,
      script_hash = Nothing,
      created_at = CreatedAt slot "hdr",
      spent_at = Nothing
    }

countMatches :: (MonadIO m) => ReaderT SqlBackend m Int
countMatches = length <$> selectList ([] :: [Filter OnchainMatchEvent]) []

storageRoundtripTests :: TestTree
storageRoundtripTests =
  testGroup
    "Storage round-trip (in-memory SQLite)"
    [ testCase "same-block matches with distinct output index are both retained (F-05)" $
        runSqlite ":memory:" $ do
          runMigrations
          putKupoMatch (mkMatch 100 "tx1" 0)
          putKupoMatch (mkMatch 100 "tx1" 1) -- same slot/header/txId, different outputIndex
          n <- countMatches
          liftIO $ assertEqual "both same-block matches retained" 2 n,
      testCase "putKupoMatch is idempotent (upsertByUnique)" $
        runSqlite ":memory:" $ do
          runMigrations
          putKupoMatch (mkMatch 100 "tx1" 0)
          putKupoMatch (mkMatch 100 "tx1" 0) -- identical → upsert replaces, not duplicates
          n <- countMatches
          liftIO $ assertEqual "identical match stored once" 1 n,
      testCase "rollbackTo deletes strictly-greater slots numerically, not lexicographically (F-02)" $
        runSqlite ":memory:" $ do
          runMigrations
          putKupoMatch (mkMatch 99999999 "tx-old" 0) -- 8-digit slot
          putKupoMatch (mkMatch 100000000 "tx-new" 0) -- 9-digit slot; lexically "100000000" < "99999999"
          rollbackTo GYTestnetPreview 99999999 -- must remove only the 9-digit one
          n <- countMatches
          liftIO $ assertEqual "only slots > 99999999 removed (numeric compare)" 1 n
    ]
