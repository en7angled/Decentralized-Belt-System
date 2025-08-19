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

import Control.Monad.IO.Class
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (Value)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.Either (fromRight)
import Data.Either.Extra
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import GeniusYield.Types (GYNetworkId (..), mintingPolicyCurrencySymbol)
import GeniusYield.Types.Address (addressFromScriptHash, addressToText)
import Ingestion
import KupoAtlas (AtlasMatch (..), kupoMatchToAtlasMatch)
import KupoClient (KupoMatch (..), runKupoCheckpointsList, runKupoMatches)
import PlutusLedgerApi.V1.Value (unCurrencySymbol)
import PlutusTx.Builtins (fromBuiltin)
import System.Environment (lookupEnv)
import TxBuilding.Validators
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
  putStrLn "Querying Kupo (address pattern) ..."
  putStrLn ("Base URL: " <> kupoUrl)
  putStrLn ("Pattern: " <> T.unpack matchPattern)
  putStrLn ("Policy ID: " <> T.unpack policyHexText)
  res <-
    runKupoMatches
      kupoUrl
      matchPattern
      (Just policyHexText) -- policy_id
      Nothing -- asset_name
      Nothing -- transaction_id
      Nothing -- output_index
      (Just "88864369")
      Nothing -- created_after/before
      Nothing
      Nothing -- spent_after/before
      (Just "oldest_first") -- order
      False
      False
      True -- spent, unspent, resolve_hashes
  case res of
    Left err -> putStrLn ("Kupo client error: " <> show err)
    Right matches -> do
      putStrLn ("Kupo returned matches: " <> show (length matches))
      let atlaMatches = kupoMatchToAtlasMatch <$> matches
      chainevents <- mapM (runExceptT . projectChainEvent GYTestnetPreview) $ rights atlaMatches
      mapM_ print $ rights chainevents

-- resC <- runKupoCheckpointsList kupoUrl
-- case resC of
--   Left err -> putStrLn ("Kupo client error: " <> show err)
--   Right checkpoints -> do
--     putStrLn ("Kupo returned checkpoints: " <> show (length checkpoints))
--     mapM_ (print) $  checkpoints
