{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module KupoClient

where

import Data.Aeson (FromJSON (..), Value, withObject, (.:), (.:?))
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API
import Servant.Client

-- | Value object from Kupo matches: coins and assets map policy.asset -> quantity
data KupoValue = KupoValue
  { coins :: Integer,
    assets :: Map.Map Text Integer
  }
  deriving (Show, Generic)

instance FromJSON KupoValue where
  parseJSON = withObject "KupoValue" $ \o ->
    KupoValue
      <$> o .: "coins"
      <*> o .: "assets"

-- | Point representation for created_at / spent_at
data CreatedAt = CreatedAt
  { slot_no :: Integer,
    header_hash :: Text
  }
  deriving (Show, Generic)

instance FromJSON CreatedAt where
  parseJSON = withObject "CreatedAt" $ \o ->
    CreatedAt
      <$> o .: "slot_no"
      <*> o .: "header_hash"

-- | Point representation for created_at / spent_at
data SpentAt = SpentAt
  { spent_slot_no :: Integer,
    spent_header_hash :: Text,
    spent_transaction_id :: Maybe Text,
    spent_input_index :: Maybe Integer,
    spent_redeemer :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON SpentAt where
  parseJSON = withObject "SpentAt" $ \o ->
    SpentAt
      <$> o .: "slot_no"
      <*> o .: "header_hash"
      <*> o .: "transaction_id"
      <*> o .: "input_index"
      <*> o .: "redeemer"

-- | Complete representation of a Kupo match entry as returned by the Matches API.
data KupoMatch = KupoMatch
  { transaction_index :: Int,
    transaction_id :: Text,
    output_index :: Int,
    address :: Text,
    value :: KupoValue,
    datum_hash :: Maybe Text,
    datum_type :: Maybe Text,
    datum :: Maybe Text,
    script_hash :: Maybe Text,
    created_at :: CreatedAt,
    spent_at :: Maybe SpentAt
  }
  deriving (Show, Generic)

instance FromJSON KupoMatch where
  parseJSON = withObject "KupoMatch" $ \o ->
    KupoMatch
      <$> o .: "transaction_index"
      <*> o .: "transaction_id"
      <*> o .: "output_index"
      <*> o .: "address"
      <*> o .: "value"
      <*> o .:? "datum_hash"
      <*> o .: "datum_type"
      <*> o .:? "datum"
      <*> o .:? "script_hash"
      <*> o .: "created_at"
      <*> o .:? "spent_at"

-- | Kupo Matches API: GET /v1/matches/{pattern}?resolve_datums
-- | Kupo Matches API with full query parameters as per docs
-- Reference: https://cardanosolutions.github.io/kupo/#tag/Matches
type MatchesAPI =
  "v1"
    :> "matches"
    :> Capture "pattern" Text
    :> QueryParam "policy_id" Text
    :> QueryParam "asset_name" Text
    :> QueryParam "transaction_id" Text
    :> QueryParam "output_index" Int
    :> QueryParam "created_after" Text
    :> QueryParam "created_before" Text
    :> QueryParam "spent_after" Text
    :> QueryParam "spent_before" Text
    :> QueryParam "order" Text
    :> QueryFlag "spent"
    :> QueryFlag "unspent"
    :> QueryFlag "resolve_hashes"
    :> Get '[JSON] [KupoMatch]

matchesClient ::
  Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Bool ->
  Bool ->
  Bool ->
  ClientM [KupoMatch]
matchesClient = client (Proxy :: Proxy MatchesAPI)

-- | High-level helper to call the API using a pre-built ClientEnv.
kupoMatches ::
  ClientEnv ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Bool ->
  Bool ->
  Bool ->
  IO (Either ClientError [KupoMatch])
kupoMatches env pattern polId assetName txId outIx cAfter cBefore sAfter sBefore order spent unspent resolveHashes =
  runClientM (matchesClient pattern polId assetName txId outIx cAfter cBefore sAfter sBefore order spent unspent resolveHashes) env

-- | Convenience: build a ClientEnv from a base URL string using TLS manager.
runKupoMatches ::
  String ->
  Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Maybe Text ->
  Bool ->
  Bool ->
  Bool ->
  IO (Either ClientError [KupoMatch])
runKupoMatches baseUrlStr pattern polId assetName txId outIx cAfter cBefore sAfter sBefore order spent unspent resolveHashes = do
  baseUrl <- parseKupoBaseUrl baseUrlStr
  manager <- newManager tlsManagerSettings
  let env = mkClientEnv manager baseUrl
  kupoMatches env pattern polId assetName txId outIx cAfter cBefore sAfter sBefore order spent unspent resolveHashes

parseKupoBaseUrl :: String -> IO BaseUrl
parseKupoBaseUrl url =
  case parseBaseUrl url of
    Left err -> fail ("Invalid KUPO_URL: " <> show err)
    Right bu -> pure bu
