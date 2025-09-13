{-# LANGUAGE OverloadedStrings #-}

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
import Database.Persist.Sqlite (runSqlite)
import GeniusYield.Types (GYNetworkId (..))
import KupoClient (KupoCheckpoint (..), KupoMatch (..), runKupoCheckpointBySlot, runKupoCheckpointsList, runKupoMatches)
import Storage (ChainCursor (..), getCursorValue, putCursor, putMatchAndProjections)

evaluateChainSyncState :: KupoCheckpoint -> KupoCheckpoint -> ChainSyncState
evaluateChainSyncState localTip@(KupoCheckpoint localSlot localHeader) blockchainTip@(KupoCheckpoint blockchainSlot blockchainHeader)
  | localTip == blockchainTip = UpToDate
  | localSlot == blockchainSlot && localHeader /= blockchainHeader = UpToDateButDifferentBlockHash
  | localSlot < blockchainSlot = Behind
  | localSlot > blockchainSlot = Ahead
  | otherwise = error "Impossible state in evaluateChainSyncState"

updateLocalTip :: T.Text -> KupoCheckpoint -> IO ()
updateLocalTip kupoDBPathText tip = do
  runSqlite kupoDBPathText $ do
    putCursor (ChainCursor True (ck_slot_no tip) (ck_header_hash tip) Nothing Nothing)

getLocalTip :: T.Text -> IO KupoCheckpoint
getLocalTip kupoDBPathText = do
  runSqlite kupoDBPathText $ do
    mCur <- getCursorValue
    case mCur of
      Just cur -> return (KupoCheckpoint (chainCursorSlotNo cur) (chainCursorHeaderHash cur))
      Nothing -> return (KupoCheckpoint 0 "")

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

getBlockchainTip :: String -> IO KupoCheckpoint
getBlockchainTip kupoUrl = do
  eCk <- liftIO $ runKupoCheckpointsList kupoUrl
  case eCk of
    Left err -> do
      liftIO $ putStrLn ("Kupo client error: " <> show err)
      liftIO $ putStrLn "Retrying in 10 seconds"
      liftIO $ threadDelay 10000000
      getBlockchainTip kupoUrl
    Right cks -> return (head cks)

fetchingMatches :: MVar SyncMetrics -> String -> T.Text -> T.Text -> T.Text -> Integer -> Integer -> Integer -> IO ()
fetchingMatches metricsVar kupoUrl matchPattern policyHexText kupoDBPathText start end batch_size =
  if end <= start
    then do
      liftIO $ putStrLn "No more matches to fetch"
    else do
      let startInterval = start
      let endInterval = if (start + batch_size) > end then end else start + batch_size
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
          fetchingMatches metricsVar kupoUrl matchPattern policyHexText kupoDBPathText start end batch_size
        Right matches -> do
          applyMatches GYTestnetPreview kupoDBPathText matches
          now <- getCurrentTime
          modifyMVar_ metricsVar $ \m -> pure m {smLastSyncTime = now}

      fetchingMatches metricsVar kupoUrl matchPattern policyHexText kupoDBPathText endInterval end batch_size

applyMatches :: GYNetworkId -> T.Text -> [KupoMatch] -> IO ()
applyMatches _networkId _kupoDBPathText [] = return ()
applyMatches networkId kupoDBPathText matches =
  runSqlite kupoDBPathText $ do
    liftIO $ putStrLn ("-----------------------------------------> Applying matches: " <> show (length matches))
    forM_ matches (putMatchAndProjections networkId)
