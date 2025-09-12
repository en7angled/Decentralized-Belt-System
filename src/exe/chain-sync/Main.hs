{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import qualified Constants
import Control.Concurrent.Extra
import Control.Monad (forM_, when)
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Either.Extra (rights)
import Data.Functor.Constant (Constant (Constant))
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist (Entity (..))
import Database.Persist.Sql
import Database.Persist.Sqlite (runSqlite)
import GeniusYield.GYConfig
import GeniusYield.Types (GYNetworkId (..), mintingPolicyCurrencySymbol)
import GeniusYield.Types.Logging (GYLogNamespace)
import GeniusYield.Types.Slot
import KupoAtlas (AtlasMatch (..), kupoMatchToAtlasMatch)
import KupoClient (CreatedAt (..), KupoCheckpoint (..), KupoMatch (..), runKupoCheckpointBySlot, runKupoCheckpointsList, runKupoMatches)
import Ingestion
import PlutusLedgerApi.V1.Value (unCurrencySymbol)
import PlutusTx.Builtins (fromBuiltin)
import Storage
import System.Environment (lookupEnv)
import TxBuilding.Context
import TxBuilding.Validators (mintingPolicyGY)
import Utils

defaultLookUpPath :: FilePath
defaultLookUpPath = "db/chainsync.sqlite"


defaultKupoUrl :: String
defaultKupoUrl = "http://localhost:1442"

main :: IO ()
main = do
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

  runSqlite kupoDBPathText $ do
    runMigrations

  batch_size <- do
    mb <- lookupEnv "BATCH_SIZE"
    pure $ maybe (10_000_000 :: Integer) read mb
  fetch_batch_size <- do
    mb <- lookupEnv "FETCH_BATCH_SIZE"
    pure $ maybe (10_000 :: Integer) read mb

  initialTip <- getLocalTip kupoDBPathText
  firstCheckPoint <- findCheckpoint kupoUrl batch_size (ck_slot_no initialTip)
  updateLocalTip kupoDBPathText firstCheckPoint

  forever $ do
    blockchainTip <- getBlockchainTip kupoUrl
    localTip <- getLocalTip kupoDBPathText
    let chainSyncState = evaluateChainSyncState localTip blockchainTip
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
        fetchingMatches kupoUrl matchPattern policyHexText kupoDBPathText (ck_slot_no localTip) (ck_slot_no blockchainTip) fetch_batch_size
        blockchainTip <- getBlockchainTip kupoUrl
        updateLocalTip kupoDBPathText blockchainTip
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

  return ()

moveSlotNo :: KupoCheckpoint -> Integer -> KupoCheckpoint
moveSlotNo (KupoCheckpoint slot _) batch_size = KupoCheckpoint (slot + batch_size) ""

data ChainSyncState = UpToDate | UpToDateButDifferentBlockHash | Behind | Ahead deriving (Show, Eq)

evaluateChainSyncState :: KupoCheckpoint -> KupoCheckpoint -> ChainSyncState
evaluateChainSyncState localTip@(KupoCheckpoint localSlot localHeader) blockchainTip@(KupoCheckpoint blockchainSlot blockchainHeader)
  | localTip == blockchainTip = UpToDate
  | localSlot < blockchainSlot = Behind
  | localSlot > blockchainSlot = Ahead
  | localSlot == blockchainSlot && localHeader /= blockchainHeader = UpToDateButDifferentBlockHash

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

fetchingMatches :: String -> T.Text -> T.Text -> T.Text -> Integer -> Integer -> Integer -> IO ()
fetchingMatches kupoUrl matchPattern policyHexText kupoDBPathText start end batch_size =
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
          fetchingMatches kupoUrl matchPattern policyHexText kupoDBPathText start end batch_size
        Right matches -> applyMatches GYTestnetPreview kupoDBPathText matches

      fetchingMatches kupoUrl matchPattern policyHexText kupoDBPathText endInterval end batch_size

applyMatches :: GYNetworkId -> T.Text -> [KupoMatch] -> IO ()
applyMatches networkId kupoDBPathText [] = return ()
applyMatches networkId kupoDBPathText matches =
  runSqlite kupoDBPathText $ do
    liftIO $ putStrLn ("-----------------------------------------> Applying matches: " <> show (length matches))
    -- process each match
    forM_ matches (putMatchAndProjections networkId)
