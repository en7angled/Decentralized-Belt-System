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
import qualified Data.Aeson as Aeson
import Data.Aeson (Value)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Environment (lookupEnv)
import Data.Foldable (toList)
import GeniusYield.Types (GYNetworkId (..))
import GeniusYield.Types.Address (addressFromScriptHash, addressToText)
import TxBuilding.Validators
import qualified Data.ByteString.Base16 as B16
import PlutusLedgerApi.V1.Value (unCurrencySymbol)
import PlutusTx.Builtins (fromBuiltin)
import TxBuilding.Validators (mintingPolicyGY)
import GeniusYield.Types (mintingPolicyCurrencySymbol)
import qualified Data.Vector as V
import KupoClient (KupoMatch(..), runKupoMatches, runKupoCheckpointsList)
import KupoAltas (AtlasMatch(..), kupoMatchToAtlasMatch)

main :: IO ()
main = do
  kupoUrl <- fmap (fromMaybe "https://kupo16cdjk05emessgrpy45t.preview-v2.kupo-m1.demeter.run") (lookupEnv "KUPO_URL")
  let ranksAddrText = addressToText (addressFromScriptHash GYTestnetPreview ranksValidatorHashGY)
      policyHexText =
        let cs = mintingPolicyCurrencySymbol mintingPolicyGY
            bytes :: BS.ByteString
            bytes = fromBuiltin (unCurrencySymbol cs)
         in TE.decodeUtf8 (B16.encode bytes)

  let matchPattern = policyHexText <> ".*"
  putStrLn "Querying Kupo (address pattern) ..."
  putStrLn ("Base URL: " <> kupoUrl)
  putStrLn ("Pattern: " <> T.unpack matchPattern)
  putStrLn ("Policy ID: " <> T.unpack policyHexText)
  res <- runKupoMatches
           kupoUrl
           matchPattern
           (Just policyHexText) -- policy_id
           Nothing              -- asset_name
           Nothing              -- transaction_id
           Nothing              -- output_index
           (Just "88864369") Nothing      -- created_after/before
           Nothing Nothing      -- spent_after/before
           (Just "most_recent_first") -- order
           False False True     -- spent, unspent, resolve_hashes
  case res of
    Left err -> putStrLn ("Kupo client error: " <> show err)
    Right matches -> do
      putStrLn ("Kupo returned matches: " <> show (length matches))
      mapM_ (print) $  kupoMatchToAtlasMatch <$> matches

  resC <- runKupoCheckpointsList kupoUrl
  case resC of
    Left err -> putStrLn ("Kupo client error: " <> show err)
    Right checkpoints -> do
      putStrLn ("Kupo returned checkpoints: " <> show (length checkpoints))
      mapM_ (print) $  checkpoints
