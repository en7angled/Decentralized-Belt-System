{-# LANGUAGE OverloadedStrings #-}

module KupoAltas where

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as C
import qualified Data.Aeson as Aeson
import qualified Data.Bifunctor
import qualified Data.ByteString.Base16 as B16
import qualified Data.Csv as Csv
import Data.Either.Extra (maybeToEither)
import qualified Data.Map as Map
import Data.Maybe
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GeniusYield.Types
import GeniusYield.Types.Address
import GeniusYield.Types.Datum
import GeniusYield.Types.Script.ScriptHash (GYScriptHash)
import KupoClient

-- | Product type using Atlas (GeniusYield) types where applicable.
data AtlasMatch = AtlasMatch
  { amTransactionIndex :: Int,
    amTransactionId :: GYTxId,
    amOutputIndex :: Int,
    amAddress :: GYAddress,
    amValue :: GYValue,
    amDatum :: GYOutDatum,
    amScriptHash :: Maybe GYScriptHash,
    amCreatedAt :: GYSlot,
    amCreatedAtHeaderHash :: Text,
    amSpentAt :: Maybe GYSlot,
    amSpentAtHeaderHash :: Maybe Text,
    amSpentAtTransactionId :: Maybe GYTxId,
    amSpentAtInputIndex :: Maybe Integer,
    amSpentAtRedeemer :: Maybe GYRedeemer
  }
  deriving (Show)

decodeGYDatumHash :: Text -> Either String GYDatumHash
decodeGYDatumHash t = maybeToEither "Invalid datum hash" (datumHashFromHex (T.unpack t))

decodeGYScriptHash :: Text -> GYScriptHash
decodeGYScriptHash t = fromString (T.unpack t)

decodeGYDatum :: Text -> Either String GYDatum
decodeGYDatum t =
  case B16.decode (TE.encodeUtf8 t) of
    Left err -> Left ("Invalid hex datum: " <> show err)
    Right bytes ->
      case C.deserialiseFromCBOR C.AsHashableScriptData bytes of
        Left decErr -> Left ("Invalid CBOR datum: " <> show decErr)
        Right scriptData -> Right (datumFromApi' scriptData)

kupoDatumToGYDatum :: Maybe Text -> Maybe Text -> Maybe Text -> Either String GYOutDatum
kupoDatumToGYDatum datum datumHash datumType =
  case (datum, datumHash, datumType) of
    (Just d, _, Just "inline") -> case decodeGYDatum d of
      Left err -> Left err
      Right datum -> Right (GYOutDatumInline datum)
    (Nothing, Just dh, Just "hash") -> case decodeGYDatumHash dh of
      Left err -> Left err
      Right hash -> Right (GYOutDatumHash hash)
    _ -> Right GYOutDatumNone

kupoRedeemerToGYRedeemer :: Text -> Either String GYRedeemer
kupoRedeemerToGYRedeemer t =
  case B16.decode (TE.encodeUtf8 t) of
    Left err -> Left ("Invalid hex redeemer: " <> show err)
    Right bytes ->
      case C.deserialiseFromCBOR C.AsHashableScriptData bytes of
        Left decErr -> Left ("Invalid CBOR redeemer: " <> show decErr)
        Right scriptData -> Right (redeemerFromApi scriptData)

decodeTxId :: Text -> Either String GYTxId
decodeTxId t = maybeToEither "Invalid transaction ID" (txIdFromHex (T.unpack t))

kupoMatchToAtlasMatch :: KupoMatch -> Either String AtlasMatch
kupoMatchToAtlasMatch KupoMatch {transaction_index, transaction_id, output_index, address, value, datum, datum_hash, datum_type, script_hash, created_at, spent_at} = do
  txId <- decodeTxId transaction_id
  datum <- kupoDatumToGYDatum datum datum_hash datum_type
  address <- maybeToEither "Invalid address" (addressFromTextMaybe address)
  spentSlot <- mapM (maybeToEither "Invalid spent slot") (slotFromInteger . spent_slot_no <$> spent_at)
  createdSlot <- maybeToEither "Invalid created slot" (slotFromInteger $ slot_no created_at)
  amSpentAtTransactionId <- mapM decodeTxId (spent_transaction_id =<< spent_at)
  amSpentAtInputIndex <- mapM (maybeToEither "Invalid spent input index") (spent_input_index <$> spent_at)
  amSpentWithRedeemer <- mapM kupoRedeemerToGYRedeemer (spent_redeemer =<< spent_at)
  return $
    AtlasMatch
      { amTransactionIndex = transaction_index,
        amTransactionId = txId,
        amOutputIndex = output_index,
        amAddress = address,
        amValue = kupoValueToGYValue value,
        amDatum = datum,
        amScriptHash = decodeGYScriptHash <$> script_hash,
        amCreatedAt = createdSlot,
        amCreatedAtHeaderHash = header_hash created_at,
        amSpentAt = spentSlot,
        amSpentAtHeaderHash = spent_header_hash <$> spent_at,
        amSpentAtTransactionId = amSpentAtTransactionId,
        amSpentAtInputIndex = amSpentAtInputIndex,
        amSpentAtRedeemer = amSpentWithRedeemer
      }

kupoValueToGYValue :: KupoValue -> GYValue
kupoValueToGYValue KupoValue {coins, assets} =
  let listOfAssets = Map.toList assets
      listOfAssetsGY = (GYLovelace, coins) : map (Data.Bifunctor.first toGYAssetClass) listOfAssets
   in valueFromList listOfAssetsGY

toGYAssetClass :: Text -> GYAssetClass
toGYAssetClass assetName = case parseAssetClassWithSep '.' assetName of
  Left err -> error $ "Invalid asset name: " <> show err
  Right assetClass -> assetClass
