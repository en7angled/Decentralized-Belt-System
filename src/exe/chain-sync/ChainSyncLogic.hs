{-# LANGUAGE OverloadedStrings #-}

-- | Core chain-sync logic: tip comparison, checkpoint discovery, and
-- batch match fetching against the Kupo indexer.
module ChainSyncLogic
  ( evaluateChainSyncState,
    updateLocalTip,
    getLocalTip,
    findCheckpoint,
    getBlockchainTip,
    fetchingMatches,
  )
where

import ChainsyncAPI (ChainSyncState (..), SyncMetrics (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import GeniusYield.Types (GYNetworkId)
import KupoClient (KupoCheckpoint (..), KupoMatch (..), runKupoCheckpointBySlot, runKupoCheckpointsList, runKupoMatches)
import Storage (ChainCursor (..), getCursorValue, putCursor, putMatchAndProjections)

-- | Compare local and blockchain tips to derive the current sync state.
evaluateChainSyncState :: KupoCheckpoint -> KupoCheckpoint -> ChainSyncState
evaluateChainSyncState localTip@(KupoCheckpoint localSlot localHeader) blockchainTip@(KupoCheckpoint blockchainSlot blockchainHeader)
  | localTip == blockchainTip = UpToDate
  | localSlot == blockchainSlot && localHeader /= blockchainHeader = UpToDateButDifferentBlockHash
  | localSlot < blockchainSlot = Behind (blockchainSlot - localSlot > 1200) -- if the difference is more than 1200 slots, we consider it way behind
  | localSlot > blockchainSlot = Ahead
  -- All cases are covered by the guards above; GHC needs a fallback for exhaustiveness.
  | otherwise = UpToDate

-- | Persist the given checkpoint as the new local tip in the database.
updateLocalTip :: ConnectionPool -> KupoCheckpoint -> IO ()
updateLocalTip pool tip = do
  runSqlPool
    ( do
        putCursor (ChainCursor True (ck_slot_no tip) (ck_header_hash tip) Nothing Nothing)
    )
    pool
  putStrLn $ "Local tip updated to: " <> show tip

-- | Read the last-persisted local tip from the database (slot 0 if none).
getLocalTip :: ConnectionPool -> IO KupoCheckpoint
getLocalTip pool = do
  runSqlPool
    ( do
        mCur <- getCursorValue
        case mCur of
          Just cur -> return (KupoCheckpoint (chainCursorSlotNo cur) (chainCursorHeaderHash cur))
          Nothing -> return (KupoCheckpoint 0 "")
    )
    pool

-- | Walk forward from a slot in increments of @stepSize@ until Kupo returns a checkpoint.
findCheckpoint :: String -> Integer -> Integer -> IO KupoCheckpoint
findCheckpoint kupoUrl stepSize curSlot = do
  eCk <- liftIO $ runKupoCheckpointBySlot kupoUrl curSlot
  case eCk of
    Left err -> do
      liftIO $ putStrLn ("Warning: checkpoint fetch failed: " <> show err)
      liftIO $ putStrLn "Retrying in 10 seconds"
      liftIO $ threadDelay 10000000
      findCheckpoint kupoUrl stepSize curSlot
    Right Nothing -> do
      liftIO $ putStrLn "No checkpoint found"
      liftIO $ putStrLn ("Checking next batch " <> show (curSlot + stepSize))
      findCheckpoint kupoUrl stepSize (curSlot + stepSize)
    Right (Just tip) -> do
      liftIO $ putStrLn ("Checkpoint found: " <> show (ck_slot_no tip))
      return tip

-- | Fetch the current blockchain tip from Kupo, retrying on failure.
getBlockchainTip :: String -> IO KupoCheckpoint
getBlockchainTip kupoUrl = do
  eCk <- liftIO $ runKupoCheckpointsList kupoUrl
  case eCk of
    Left err -> do
      liftIO $ putStrLn ("Kupo client error: " <> show err)
      liftIO $ putStrLn "Retrying in 10 seconds"
      liftIO $ threadDelay 10000000
      getBlockchainTip kupoUrl
    Right (ck : _) -> return ck
    Right [] -> do
      liftIO $ putStrLn "No checkpoints returned from Kupo"
      liftIO $ threadDelay 10000000
      getBlockchainTip kupoUrl

-- | Recursively fetch and project Kupo matches in batches from @start@ to @end@.
fetchingMatches :: MVar SyncMetrics -> String -> T.Text -> T.Text -> GYNetworkId -> ConnectionPool -> Integer -> Integer -> Integer -> IO ()
fetchingMatches metricsVar kupoUrl matchPattern policyHexText networkId pool start end batchSize =
  if end <= start
    then do
      liftIO $ putStrLn "No more matches to fetch"
    else do
      let startInterval = start
      let endInterval = if (start + batchSize) > end then end else start + batchSize
      liftIO $ putStrLn ("Fetching matches from " <> show start <> " to " <> show endInterval)
      eMatches <-
        liftIO $
          runKupoMatches
            kupoUrl
            matchPattern
            (Just policyHexText)
            Nothing
            Nothing
            Nothing
            (Just startInterval)
            (Just endInterval)
            Nothing
            Nothing
            (Just "oldest_first")
            False
            False
            True

      case eMatches of
        Left err -> do
          liftIO $ putStrLn ("Kupo client error: " <> show err)
          liftIO $ putStrLn "Retrying in 10 seconds"
          liftIO $ threadDelay 10000000
          fetchingMatches metricsVar kupoUrl matchPattern policyHexText networkId pool start end batchSize
        Right matches -> do
          applyMatches networkId pool matches
          now <- getCurrentTime
          modifyMVar_ metricsVar $ \m -> pure m {smLocalTip = endInterval, smLastSyncTime = now}

      fetchingMatches metricsVar kupoUrl matchPattern policyHexText networkId pool endInterval end batchSize

applyMatches :: GYNetworkId -> ConnectionPool -> [KupoMatch] -> IO ()
applyMatches _networkId _pool [] = return ()
applyMatches networkId pool matches = do
  liftIO $ putStrLn ("-----------------------------------------> Applying matches: " <> show (length matches))
  runSqlPool
    (forM_ matches (putMatchAndProjections networkId))
    pool
