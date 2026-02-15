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
import Constants
import Control.Concurrent.Extra
import GeniusYield.GYConfig (GYCoreConfig (..))
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE


import Database.Persist.Postgresql (ConnectionString, createPostgresqlPool)
import Database.Persist.Sql (runSqlPool)
import KupoClient (KupoCheckpoint (..))
import Storage
import System.Environment (lookupEnv)
import Text.Printf
import TxBuilding.Context
import Utils (decodeConfigEnvOrFile)
import Data.Time
import WebAPI.Utils


defaultConnStr :: String
defaultConnStr = "host=localhost user=postgres password=postgres dbname=chainsync port=5432"

defaultKupoUrl :: String
defaultKupoUrl = "https://kupo16cdjk05emessgrpy45t.preview-v2.kupo-m1.demeter.run"

main :: IO ()
main = do
  port <- getPortFromEnvOrDefault 8084

  kupoUrl <- liftIO $ fmap (fromMaybe defaultKupoUrl) (lookupEnv "KUPO_URL")
  connStr <- liftIO $ fmap (fromMaybe defaultConnStr) (lookupEnv "PG_CONN_STR")

  let connBS :: ConnectionString = TE.encodeUtf8 (T.pack connStr)
  pool <- runStdoutLoggingT $ createPostgresqlPool connBS 16
  runSqlPool runMigrations pool


  initialTip <- getLocalTip pool

  now <- getCurrentTime
  metricsVar <-
    newMVar
      SyncMetrics
        { smLocalTip = ck_slot_no initialTip,
          smBlockchainTip = ck_slot_no initialTip,
          smLastSyncTime = now,
          smDbReady = False,
          smMigrationsComplete = False,
          smChainSyncState = Behind True -- by default we are way behind
        } 

 
  -- Start probe server
  void $ forkIO $ startProbeServer port metricsVar

  -- Chain sync loop

  atlasConfig <- fromMaybe (error "Atlas configuration failed") <$> decodeConfigEnvOrFile "ATLAS_CORE_CONFIG" defaultAtlasCoreConfig
  let networkId = cfgNetworkId atlasConfig

  deployedScriptsContext <- fromMaybe (error "Deployed validators configuration failed") <$> decodeConfigEnvOrFile @DeployedScriptsContext "DEPLOYED_VALIDATORS_CONFIG" defaultTxBuldingContextFile
  let mpHash = getMintingPolicyHash deployedScriptsContext

  let policyHexText = T.pack $ printf "%s" mpHash

  let matchPattern = policyHexText <> ".*"
  putStrLn "Starting chain-sync ..."
  putStrLn ("Base URL: " <> kupoUrl)
  putStrLn ("Pattern: " <> T.unpack matchPattern)
  putStrLn ("Postgres DSN: " <> connStr)

  batch_size <- do
    mb <- lookupEnv "BATCH_SIZE"
    pure $ maybe (100_000_000 :: Integer) read mb
  fetch_batch_size <- do
    mb <- lookupEnv "FETCH_BATCH_SIZE"
    pure $ maybe (10_000_000 :: Integer) read mb

  -- Sync loop in background thread (includes initial checkpoint alignment)

  initLocal <- getLocalTip pool
  startingCheckPoint <- findCheckpoint kupoUrl batch_size (ck_slot_no initLocal)
  updateLocalTip pool startingCheckPoint

  forever $ do
    blockchainTip <- getBlockchainTip kupoUrl
    localTip <- getLocalTip pool
    let chainSyncState = evaluateChainSyncState localTip blockchainTip
    modifyMVar_ metricsVar $ \m -> pure m {smLocalTip = ck_slot_no localTip, smBlockchainTip = ck_slot_no blockchainTip, smChainSyncState = chainSyncState}
    liftIO $ putStrLn ("Local tip      : " <> show (ck_slot_no localTip))
    liftIO $ putStrLn ("Blockchain tip : " <> show (ck_slot_no blockchainTip))
    case chainSyncState of
      UpToDate -> do
        liftIO $ putStrLn "Chain is up to date"
        liftIO $ putStrLn "Sleeping for 10 seconds"
        liftIO $ threadDelay 10000000
      Behind _isWayBehind -> do
        liftIO $ putStrLn "Chain is behind"
        liftIO $ putStrLn "Fetching matches"
        fetchingMatches metricsVar kupoUrl matchPattern policyHexText networkId pool (ck_slot_no localTip) (ck_slot_no blockchainTip) fetch_batch_size
        blockchainTip' <- getBlockchainTip kupoUrl
        updateLocalTip pool blockchainTip'
      Ahead -> do
        liftIO $ putStrLn "Chain is ahead"
        liftIO $ putStrLn "Starting rollback"
        -- Rollback DB state to blockchain tip (retain rows up to tip with matching header)
        runSqlPool (rollbackTo (ck_slot_no blockchainTip) (ck_header_hash blockchainTip)) pool
        -- Update the local tip cursor to match the blockchain tip
        updateLocalTip pool blockchainTip
        liftIO $ putStrLn "Rollback complete and local tip updated"
      UpToDateButDifferentBlockHash -> do
        liftIO $ putStrLn "Chain is on the same slot but different block hash"
        updateLocalTip pool blockchainTip
        liftIO $ putStrLn "Updated local tip with blockchain tip"
