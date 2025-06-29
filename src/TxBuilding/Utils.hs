module TxBuilding.Utils where

import Cardano.Api (Key (getVerificationKey), castVerificationKey)
import Data.Aeson (decodeFileStrict)
import Data.Either.Extra
import Data.List (find)
import Data.Text qualified as T
import Data.Text.IO qualified
import GeniusYield.Types (GYUTxO, GYUTxOs, GYAddress, GYDatum, GYValue, GYNetworkId(..), GYSlot, Ada, GYOutDatum(..), GYAssetClass, GYExtendedPaymentSigningKey, fromValue, valueToPlutus, foldMapUTxOs, utxoValue, paymentKeyHash, GYValue, valueAssets, utxosToList, utxosFromList, utxoOutDatum)
import GeniusYield.Types.Key (extendedPaymentSigningKeyToApi, paymentVerificationKeyFromApi)
import GeniusYield.Types.Address (addressFromPaymentKeyHash)
import GeniusYield.Types.Time (timeToPlutus, timeFromPlutus)
import GeniusYield.Types.Datum (datumToPlutus')
import PlutusLedgerApi.V1.Interval qualified
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V3
import Onchain.CIP68 (CIP68Datum, MetadataFields(..), extra, metadata, mkCIP68Datum)
import Onchain.Types (Profile)
import TxBuilding.Exceptions (ProfileException(..))
import TxBuilding.Validators
import System.Directory.Extra
import PlutusTx.Builtins (emptyByteString)
import Control.Monad.Except (MonadError, throwError)
import Data.Text (Text)
import GeniusYield.Types.Slot (unsafeSlotFromInteger)
import GeniusYield.TxBuilder.Query.Class (GYTxUserQueryMonad, GYTxQueryMonad, utxosAtAddress)
import GeniusYield.TxBuilder.Errors (GYTxMonadException(GYApplicationException))
import GeniusYield.TxBuilder.Class (slotToBeginTime, enclosingSlotFromTime', scriptAddress)

------------------------------------------------------------------------------------------------

-- * Utilities

------------------------------------------------------------------------------------------------

getAdaBalance :: GYUTxOs -> Ada
getAdaBalance = fromValue . getValueBalance

getValueBalance :: GYUTxOs -> Value
getValueBalance = valueToPlutus . foldMapUTxOs utxoValue

addressFromPaymentSigningKey :: GYNetworkId -> GYExtendedPaymentSigningKey -> GYAddress
addressFromPaymentSigningKey nid extendedSkey =
  let
    vkey = Cardano.Api.getVerificationKey $ extendedPaymentSigningKeyToApi extendedSkey
    pub_key = paymentVerificationKeyFromApi (castVerificationKey vkey)
    payment_key_hash = paymentKeyHash pub_key
    address = addressFromPaymentKeyHash nid payment_key_hash
   in
    address

pPOSIXTimeFromSlotInteger :: (GYTxQueryMonad m) => Integer -> m POSIXTime
pPOSIXTimeFromSlotInteger = (timeToPlutus <$>) . slotToBeginTime . unsafeSlotFromInteger

pPOSIXTimeFromGYSlot :: (GYTxQueryMonad m) => GYSlot -> m POSIXTime
pPOSIXTimeFromGYSlot = (timeToPlutus <$>) . slotToBeginTime

gySlotFromPOSIXTime :: (GYTxQueryMonad m) => POSIXTime -> m GYSlot
gySlotFromPOSIXTime ptime = do
  enclosingSlotFromTime' (timeFromPlutus ptime)

veryFarPosixDate :: POSIXTime
veryFarPosixDate = POSIXTime 999999999999999999

showLink :: GYNetworkId -> T.Text -> T.Text -> T.Text
showLink nid txId hash = case nid of
  GYMainnet -> "https://cardanoscan.io/transaction/" <> txId <> "?tab=utxo"
  GYTestnetPreprod -> "https://preprod.cardanoscan.io/transaction/" <> txId <> "?tab=utxo"
  GYTestnetPreview -> "https://preview.cardanoscan.io/transaction/" <> txId <> "?tab=utxo"
  GYTestnetLegacy -> txId
  GYPrivnet _f -> txId

-- | Get UTxO with state token at validator address
getUTxOWithStateToken :: (GYTxUserQueryMonad m, MonadError GYTxMonadException m) => GYAssetClass -> GYAddress -> m GYUTxO
getUTxOWithStateToken stateTokenId validatorAddr = do
  utxos <- utxosAtAddress validatorAddr Nothing
  case findUTxOWithStateToken stateTokenId utxos of
    Just utxo -> return utxo
    Nothing -> throwError (GYApplicationException ProfileNotFound)

-- | Find UTxO with state token in a list of UTxOs
findUTxOWithStateToken :: GYAssetClass -> GYUTxOs -> Maybe GYUTxO
findUTxOWithStateToken stateTokenId utxos = do
  let utxoList = utxosToList utxos
  find (\utxo -> hasValidStateToken utxo) utxoList

-- | Get UTxO with state token at specific addresses
getUTxOWithStateTokenAtAddresses :: (GYTxUserQueryMonad m, MonadError GYTxMonadException m) => GYAssetClass -> [GYAddress] -> m GYUTxO
getUTxOWithStateTokenAtAddresses stateTokenId addrs = do
  utxosList <- mapM (\addr -> utxosAtAddress addr Nothing) addrs
  let allUtxos = foldMap utxosToList utxosList
  case findUTxOWithStateToken stateTokenId (utxosFromList allUtxos) of
    Just utxo -> return utxo
    Nothing -> throwError (GYApplicationException ProfileNotFound)

-- | Check if UTxO has valid state token
hasValidStateToken :: GYUTxO -> Bool
hasValidStateToken gyOut =
  let gyValue = utxoValue gyOut
      gyAssets = valueAssets gyValue
   in any (== stateTokenId) gyAssets
  where
    stateTokenId = undefined -- TODO: Implement when we have the actual state token

-- | Get inline datum and value from UTxO
getInlineDatumAndValue :: GYUTxO -> Maybe (GYDatum, GYValue)
getInlineDatumAndValue utxo = case utxoOutDatum utxo of
  GYOutDatumInline datum -> Just (datum, utxoValue utxo)
  _ -> Nothing

-- | Get profile state data and value from asset class
getProfileStateDataAndValue :: (GYTxUserQueryMonad m, MonadError GYTxMonadException m) => GYAssetClass -> m (Profile, Value)
getProfileStateDataAndValue profileRefAC = do
  profilesValidatorAddr <- scriptAddress profilesValidatorGY
  profileStateUTxO <- getUTxOWithStateToken profileRefAC profilesValidatorAddr
  case profileAndValueFromUTxO profileStateUTxO of
    Just (profile, value) -> return (profile, value)
    Nothing -> throwError (GYApplicationException InvalidProfileData)

-- | Extract profile and value from UTxO
profileAndValueFromUTxO :: GYUTxO -> Maybe (Profile, Value)
profileAndValueFromUTxO profileStateUTxO = do
  (gyDatum, gyValue) <- getInlineDatumAndValue profileStateUTxO
  cip68Datum <- profileDatumFromDatum gyDatum
  let pVal = valueToPlutus gyValue
  return (extra cip68Datum, pVal)

-- | Convert GY datum to profile datum
profileDatumFromDatum :: GYDatum -> Maybe (CIP68Datum Profile)
profileDatumFromDatum gyDatum = do
  let plutusDatum = datumToPlutus' gyDatum
  fromBuiltinData plutusDatum

-- | Get profile value and metadata from UTxO
profileValueAndMetadataFromUTxO :: GYUTxO -> Maybe (Profile, Value, MetadataFields)
profileValueAndMetadataFromUTxO profileStateUTxO = do
  (gyDatum, gyValue) <- getInlineDatumAndValue profileStateUTxO
  cip68Datum <- profileDatumFromDatum gyDatum
  let pVal = valueToPlutus gyValue
      -- TODO: Extract actual BuiltinByteString values from metadata
      meta1 = emptyByteString
      meta2 = emptyByteString
      meta3 = emptyByteString
  return (extra cip68Datum, pVal, Metadata222 meta1 meta2 meta3)

------------------------------------------------------------------------------------------------

-- * Profile Token Utils

------------------------------------------------------------------------------------------------

-- TODO: Add profile token utilities when minting policy is implemented
-- isProfileRefAC :: GYAssetClass -> Bool
-- isProfileRefAC (GYToken gyMP gyTN) =
--   (gyMP == mintingPolicyId profilesMintingPolicyGY)
--     && hasRefPrefix (tokenNameToPlutus gyTN)
-- isProfileRefAC _ = False

-- hasValidProfileRefToken :: GYUTxO -> Bool
-- hasValidProfileRefToken gyOut =
--   let gyValue = utxoValue gyOut
--       gyAssets = valueAssets gyValue
--    in any isProfileRefAC gyAssets 