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

import Control.Monad (forM_, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Except (runExceptT)
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
  let policyHexText =
        let cs = mintingPolicyCurrencySymbol mintingPolicyGY
            bytes :: BS.ByteString
            bytes = fromBuiltin (unCurrencySymbol cs)
         in TE.decodeUtf8 (B16.encode bytes)

  let matchPattern = policyHexText <> ".*"
  putStrLn "Starting chain-sync ..."
  putStrLn ("Base URL: " <> kupoUrl)
  putStrLn ("Pattern: " <> T.unpack matchPattern)
  putStrLn ("Policy ID: " <> T.unpack policyHexText)

  runSqlite "db/chainsync.sqlite" $ do
    runMigrations

    mCur <- getCursor
    let (startAfter, curSlot, curHeader) = case mCur of
          Just (Entity _ cur) -> (chainCursorSlotNo cur, chainCursorSlotNo cur, chainCursorHeaderHash cur)
          Nothing -> (0, 0, "")

    liftIO $ putStrLn ("Current cursor slot: " <> show curSlot)

    -- validate checkpoint
    when (curSlot > 0 && not (T.null curHeader)) $ do
      eCk <- liftIO $ runKupoCheckpointBySlot kupoUrl curSlot
      case eCk of
        Left err -> liftIO $ putStrLn ("Warning: checkpoint fetch failed: " <> show err)
        Right ck -> do
          let same = ck_header_hash ck == curHeader
          if same
            then pure ()
            else liftIO $ putStrLn "Checkpoint mismatch detected, rollback required (TODO)"

    eMatches <-
      liftIO $
        runKupoMatches
          kupoUrl
          matchPattern
          (Just policyHexText)
          Nothing
          Nothing
          Nothing
          (Just startAfter)
          Nothing
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
          insertKupoMatch km
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
                  RankEvent r -> insertRankProjectionRow slotNoInt header r
                  ProfileEvent p -> insertProfileProjectionRow slotNoInt header p
                  PromotionEvent pr -> insertPromotionProjectionRow slotNoInt header pr
                  NoEvent _ -> pure ()

        -- update cursor to last match if any
        case reverse matches of
          (lastKm : _) -> do
            let slotNoInt = slot_no (created_at lastKm)
                header = header_hash (created_at lastKm)
                txIdTxt = transaction_id lastKm
                outIx = output_index lastKm
            putCursor slotNoInt header (Just txIdTxt) (Just outIx)
            liftIO $ putStrLn ("Updated cursor to slot " <> show slotNoInt)
          [] -> liftIO $ putStrLn "No new matches; cursor unchanged"
