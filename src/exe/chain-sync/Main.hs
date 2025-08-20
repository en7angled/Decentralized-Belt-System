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

import Control.Concurrent.Extra
import Control.Monad (forM_, when)
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Either.Extra (rights)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Database.Persist (Entity (..))
import Database.Persist.Sqlite (runSqlite)
import GeniusYield.Types (GYNetworkId (..), mintingPolicyCurrencySymbol)
import Ingestion
import KupoAtlas (AtlasMatch (..), kupoMatchToAtlasMatch)
import KupoClient (CreatedAt (..), KupoCheckpoint (..), KupoMatch (..), runKupoCheckpointBySlot, runKupoMatches)
import PlutusLedgerApi.V1.Value (unCurrencySymbol)
import PlutusTx.Builtins (fromBuiltin)
import Storage
import System.Environment (lookupEnv)
import TxBuilding.Validators (mintingPolicyGY)

main :: IO ()
main = do
  kupoUrl <- fmap (fromMaybe "http://localhost:1442") (lookupEnv "KUPO_URL")

  let kupoDBPathText = T.pack "db/chainsync.sqlite"
  let policyHexText =
        let cs = mintingPolicyCurrencySymbol mintingPolicyGY
         in T.pack $ show cs

  let matchPattern = policyHexText <> ".*"
  putStrLn "Starting chain-sync ..."
  putStrLn ("Base URL: " <> kupoUrl)
  putStrLn ("Pattern: " <> T.unpack matchPattern)

  runSqlite kupoDBPathText $ do
    runMigrations

  countMvar <- liftIO $ newMVar (0 :: Integer)
  let batch_size = (10000000 :: Integer)

  forever $
    runSqlite kupoDBPathText $ do
      mCur <- getCursorValue
      let (curSlot, curHeader) = case mCur of
            Just cur -> (chainCursorSlotNo cur, chainCursorHeaderHash cur)
            Nothing -> (0, "")

      liftIO $ putStrLn "Fetching matches..."
      liftIO $ putStrLn ("Current cursor slot: " <> show curSlot)
      liftIO $ putStrLn ("Current cursor block hash: " <> show curHeader)

      count <- liftIO $ readMVar countMvar
      when (curSlot > count) $ do
        void $ liftIO $ swapMVar countMvar curSlot
        liftIO $ putStrLn ("Updated mvar cursor to slot " <> show curSlot)

      liftIO $ putStrLn "Validating checkpoint..."

      eCk <- liftIO $ runKupoCheckpointBySlot kupoUrl curSlot
      case eCk of
        Left err -> liftIO $ putStrLn ("Warning: checkpoint fetch failed: " <> show err)
        Right Nothing -> do
          liftIO $ putStrLn "No checkpoint found"
          void $ liftIO $ swapMVar countMvar (count + batch_size)
        Right (Just (KupoCheckpoint ck_slot ck_header_hash)) -> do
          liftIO $ putStrLn "Checkpoint found"
          void $ liftIO $ swapMVar countMvar ck_slot
          let same = ck_slot == ck_slot
          if same
            then do
              liftIO $ putStrLn "Tip of chain found, sleeping..."
              upsertCursor (ChainCursor True ck_slot ck_header_hash Nothing Nothing)
              liftIO $ threadDelay 10000000
            else do
              liftIO $ putStrLn "Checkpoint mismatch detected"
              liftIO $ putStrLn ("Checkpoint slot: " <> show ck_slot)
              liftIO $ putStrLn ("Checkpoint block hash: " <> show ck_header_hash)
              upsertCursor (ChainCursor True ck_slot ck_header_hash Nothing Nothing)
              liftIO $ putStrLn "Cusror updated with checkpoint"

      eMatches <-
        liftIO $
          runKupoMatches
            kupoUrl
            matchPattern
            (Just policyHexText)
            Nothing
            Nothing
            Nothing
            (Just count)
            (Just $ count + batch_size)
            Nothing
            Nothing
            (Just "oldest_first")
            False
            False
            True

      case eMatches of
        Left err -> liftIO $ putStrLn ("Kupo client error: " <> show err)
        Right matches -> do
          liftIO $ putStrLn ("Kupo returned matches: " <> show (length matches))
          -- process each match
          forM_ matches $ \km -> do
            -- store the raw atlas match row
            upsertKupoMatch km
            case kupoMatchToAtlasMatch km of
              Left convErr -> liftIO $ putStrLn ("Conversion error: " <> convErr)
              Right am -> do
                let slotNoInt = slot_no (created_at km)
                    header = header_hash (created_at km)
                    txIdTxt = transaction_id km
                    outIx = output_index km
                ev <- runExceptT (projectChainEvent GYTestnetPreview am)
                case ev of
                  Left e -> liftIO $ putStrLn ("Projection error: " <> show e)
                  Right proj -> case proj of
                    RankEvent r -> upsertRankProjection slotNoInt header r
                    ProfileEvent p -> upsertProfileProjection slotNoInt header p
                    PromotionEvent pr -> upsertPromotionProjection slotNoInt header pr
                    NoEvent _ -> pure ()

          -- update cursor to last match if any
          case reverse matches of
            (lastKm : _) -> do
              let slotNoInt = slot_no (created_at lastKm)
                  header = header_hash (created_at lastKm)
                  txIdTxt = transaction_id lastKm
                  outIx = output_index lastKm
              upsertCursor (ChainCursor True slotNoInt header (Just txIdTxt) (Just outIx))
              liftIO $ putStrLn ("Updated DB cursor to slot " <> show slotNoInt)
              liftIO $ swapMVar countMvar slotNoInt
              liftIO $ putStrLn ("Updated mvar cursor to slot " <> show slotNoInt)
            [] -> do
              liftIO $ swapMVar countMvar (count + batch_size)
              liftIO $ putStrLn ("Updated mvar cursor to slot " <> show (count + batch_size))
