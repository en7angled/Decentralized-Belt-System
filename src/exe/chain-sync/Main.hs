{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import ChainSyncLogic
import ChainSyncServer (startProbeServer)
import ChainsyncAPI (ChainSyncState (..), SyncMetrics (..))
import Control.Concurrent.Extra
import Control.Monad.Extra
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Database.Persist.Sqlite (runSqlite)
import GeniusYield.Types (mintingPolicyCurrencySymbol)
import KupoClient (KupoCheckpoint (..))
import Storage
import System.Environment (lookupEnv)
import TxBuilding.Validators (mintingPolicyGY)

getPortFromEnv :: IO Int
getPortFromEnv = do
  eport <- lookupEnv "PORT"
  case eport of
    Nothing -> return 8084
    Just p -> return (read p)

defaultLookUpPath :: FilePath
defaultLookUpPath = "db/chainsync.sqlite"

defaultKupoUrl :: String
defaultKupoUrl = "http://localhost:1442"

main :: IO ()
main = do
  port <- getPortFromEnv
  kupoUrl <- liftIO $ fmap (fromMaybe defaultKupoUrl) (lookupEnv "KUPO_URL")
  kupoDBPath <- liftIO $ fmap (fromMaybe defaultLookUpPath) (lookupEnv "LOOKUP_PATH")

  let policyHexText =
        let cs = mintingPolicyCurrencySymbol mintingPolicyGY
         in T.pack $ show cs
  let matchPattern = policyHexText <> ".*"
  putStrLn "Starting chain-sync ..."
  putStrLn ("Base URL: " <> kupoUrl)
  putStrLn ("Pattern: " <> T.unpack matchPattern)
  putStrLn ("Lookup (projection) DB: " <> kupoDBPath)

  let kupoDBPathText = T.pack kupoDBPath

  runSqlite kupoDBPathText runMigrations

  batch_size <- do
    mb <- lookupEnv "BATCH_SIZE"
    pure $ maybe (10_000_000 :: Integer) read mb
  fetch_batch_size <- do
    mb <- lookupEnv "FETCH_BATCH_SIZE"
    pure $ maybe (10_000 :: Integer) read mb

  initialTip <- getLocalTip kupoDBPathText

  now0 <- getCurrentTime
  metricsVar <-
    newMVar
      SyncMetrics
        { smLocalTip = ck_slot_no initialTip,
          smBlockchainTip = ck_slot_no initialTip,
          smLastSyncTime = now0,
          smDbReady = False,
          smMigrationsComplete = False,
          smChainSyncState = UpToDate
        }

  -- Sync loop in background thread (includes initial checkpoint alignment)
  _ <- forkIO $ do
    initLocal <- getLocalTip kupoDBPathText
    firstCheckPoint <- findCheckpoint kupoUrl batch_size (ck_slot_no initLocal)
    updateLocalTip kupoDBPathText firstCheckPoint
    modifyMVar_ metricsVar $ \m -> pure m {smLocalTip = ck_slot_no firstCheckPoint, smBlockchainTip = ck_slot_no firstCheckPoint}

    forever $ do
      blockchainTip <- getBlockchainTip kupoUrl
      localTip <- getLocalTip kupoDBPathText
      let chainSyncState = evaluateChainSyncState localTip blockchainTip
      modifyMVar_ metricsVar $ \m -> pure m {smLocalTip = ck_slot_no localTip, smBlockchainTip = ck_slot_no blockchainTip, smChainSyncState = chainSyncState}
      liftIO $ putStrLn ("Local tip      : " <> show localTip)
      liftIO $ putStrLn ("Blockchain tip : " <> show blockchainTip)
      case chainSyncState of
        UpToDate -> do
          liftIO $ putStrLn "Chain is up to date"
          liftIO $ putStrLn "Sleeping for 10 seconds"
          liftIO $ threadDelay 10000000
        Behind -> do
          liftIO $ putStrLn "Chain is behind"
          liftIO $ putStrLn "Fetching matches"
          fetchingMatches metricsVar kupoUrl matchPattern policyHexText kupoDBPathText (ck_slot_no localTip) (ck_slot_no blockchainTip) fetch_batch_size
          blockchainTip' <- getBlockchainTip kupoUrl
          updateLocalTip kupoDBPathText blockchainTip'
        Ahead -> do
          liftIO $ putStrLn "Chain is ahead"
          liftIO $ putStrLn "Starting rollback"
          -- Rollback DB state to blockchain tip (retain rows up to tip with matching header)
          runSqlite kupoDBPathText $ rollbackTo (ck_slot_no blockchainTip) (ck_header_hash blockchainTip)
          -- Update the local tip cursor to match the blockchain tip
          updateLocalTip kupoDBPathText blockchainTip
          liftIO $ putStrLn "Rollback complete and local tip updated"
        UpToDateButDifferentBlockHash -> do
          liftIO $ putStrLn "Chain is on the same slot but different block hash"
          updateLocalTip kupoDBPathText blockchainTip
          liftIO $ putStrLn "Updated local tip with blockchain tip"

  -- Start probe server
  startProbeServer port metricsVar
