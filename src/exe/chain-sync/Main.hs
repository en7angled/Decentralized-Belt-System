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
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time
import Database.Persist.Postgresql (ConnectionString, createPostgresqlPool)
import Database.Persist.Sql (runSqlPool)
import GeniusYield.GYConfig (GYCoreConfig (..))
import KupoClient (KupoCheckpoint (..))
import Storage
  ( currentSchemaVersion,
    putStoredPolicyHexText,
    readSchemaProbe,
    rollbackTo,
    runMigrations,
    wipeChainSyncTablesRaw,
  )
import System.Environment (lookupEnv)
import System.Exit (die)
import Text.Printf
import Text.Read (readMaybe)
import TxBuilding.Context
import Utils (decodeConfigEnvOrFile)
import WebAPI.Utils

defaultConnStr :: String
defaultConnStr = "host=localhost user=postgres password=postgres dbname=chainsync port=5432"

defaultKupoUrl :: String
defaultKupoUrl = "https://kupo16cdjk05emessgrpy45t.cardano-preview-v2.kupo-m1.dmtr.host:443/"

main :: IO ()
main = do
  port <- getPortFromEnvOrDefault 8084

  kupoUrl <- liftIO $ fmap (fromMaybe defaultKupoUrl) (lookupEnv "KUPO_URL")
  connStr <- liftIO $ fmap (fromMaybe defaultConnStr) (lookupEnv "PG_CONN_STR")

  let connBS :: ConnectionString = TE.encodeUtf8 (T.pack connStr)
  pool <- runStdoutLoggingT $ createPostgresqlPool connBS 16

  atlasConfig <- maybe (die "Atlas configuration failed") return =<< decodeConfigEnvOrFile "ATLAS_CORE_CONFIG" defaultAtlasCoreConfig
  let networkId = cfgNetworkId atlasConfig

  deployedScriptsContext <- maybe (die "Deployed validators configuration failed") return =<< decodeConfigEnvOrFile @DeployedScriptsContext "DEPLOYED_VALIDATORS_CONFIG" defaultTxBuildingContextFile
  let policyHexText = T.pack $ printf "%s" (getMintingPolicyHash deployedScriptsContext)

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

  fetchBatchSize <- do
    mb <- lookupEnv "FETCH_BATCH_SIZE"
    pure $ maybe (10_000_000 :: Integer) read mb

  rollbackMargin <- do
    mb <- lookupEnv "ROLLBACK_MARGIN"
    pure $ max 1 (maybe defaultRollbackMargin id (mb >>= readMaybe))

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

  let matchPattern = policyHexText <> ".*"
  putStrLn "Starting chain-sync ..."
  putStrLn ("Base URL: " <> kupoUrl)
  putStrLn ("Pattern: " <> T.unpack matchPattern)
  putStrLn ("Postgres DSN: " <> connStr)

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
        fetchingMatches metricsVar kupoUrl matchPattern policyHexText networkId pool (ck_slot_no localTip) (ck_slot_no blockchainTip) fetchBatchSize
        updateLocalTip pool blockchainTip
      Ahead -> do
        liftIO $ putStrLn "Chain is ahead"
        let rollbackSlot = max 0 (ck_slot_no blockchainTip - rollbackMargin)
        runSqlPool (rollbackTo networkId rollbackSlot) pool
        updateLocalTip pool (KupoCheckpoint rollbackSlot "")
        liftIO $ putStrLn ("Rolled back to slot " <> show rollbackSlot <> "; will re-sync forward")
      UpToDateButDifferentBlockHash -> do
        liftIO $ putStrLn "Same slot, different block hash; rolling back with margin"
        let rollbackSlot = max 0 (ck_slot_no blockchainTip - rollbackMargin)
        runSqlPool (rollbackTo networkId rollbackSlot) pool
        updateLocalTip pool (KupoCheckpoint rollbackSlot "")
