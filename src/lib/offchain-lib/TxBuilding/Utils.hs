module TxBuilding.Utils where

import Cardano.Api (Key (getVerificationKey), castVerificationKey)
import Control.Monad.Except (throwError)
import Data.Text
import Data.Text.IO qualified
import GeniusYield.TxBuilder.Class (enclosingSlotFromTime', slotToBeginTime)
import GeniusYield.TxBuilder.Errors (GYTxMonadException (GYApplicationException))
import GeniusYield.TxBuilder.Query.Class (GYTxQueryMonad)
import GeniusYield.Types (GYAddress, GYAssetClass (..), GYDatum, GYExtendedPaymentSigningKey, GYNetworkId (..), GYOutDatum (..), GYPaymentKeyHash, GYSlot, GYTokenName, GYTxOutRef, GYUTxO, GYValue, paymentKeyHash, txOutRefToPlutus, utxoOutDatum, utxoValue, valueToPlutus)
import GeniusYield.Types.Address (addressFromPaymentKeyHash)
import GeniusYield.Types.Datum (datumToPlutus')
import GeniusYield.Types.Key (extendedPaymentSigningKeyToApi, paymentVerificationKeyFromApi)
import GeniusYield.Types.Slot (unsafeSlotFromInteger)
import GeniusYield.Types.Time (timeFromPlutus, timeToPlutus)
import GeniusYield.Types.Wallet
import Onchain.CIP68 (CIP68Datum)
import Onchain.Protocol qualified as Onchain
import PlutusLedgerApi.V1.Tx qualified as V1
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.Tx qualified as V3
import System.Directory.Extra
import TxBuilding.Exceptions (TxBuildingException (..))
import Utils

------------------------------------------------------------------------------------------------

-- * Utilities

------------------------------------------------------------------------------------------------

-- | Extract the payment key hash from an extended payment signing key.
pkhFromExtendedSkey :: GYExtendedPaymentSigningKey -> GYPaymentKeyHash
pkhFromExtendedSkey skey =
  let vkey = Cardano.Api.getVerificationKey $ extendedPaymentSigningKeyToApi skey
   in paymentKeyHash (paymentVerificationKeyFromApi (castVerificationKey vkey))

addressFromPaymentSigningKey :: GYNetworkId -> GYExtendedPaymentSigningKey -> GYAddress
addressFromPaymentSigningKey nid skey = addressFromPaymentKeyHash nid (pkhFromExtendedSkey skey)

pPOSIXTimeFromSlotInteger :: (GYTxQueryMonad m) => Integer -> m POSIXTime
pPOSIXTimeFromSlotInteger = (timeToPlutus <$>) . slotToBeginTime . unsafeSlotFromInteger

pPOSIXTimeFromGYSlot :: (GYTxQueryMonad m) => GYSlot -> m POSIXTime
pPOSIXTimeFromGYSlot = (timeToPlutus <$>) . slotToBeginTime

gySlotFromPOSIXTime :: (GYTxQueryMonad m) => POSIXTime -> m GYSlot
gySlotFromPOSIXTime ptime = do
  enclosingSlotFromTime' (timeFromPlutus ptime)

-- | Convert a GY TxOutRef to a Plutus V3 TxOutRef.
txOutRefToV3Plutus :: GYTxOutRef -> V3.TxOutRef
txOutRefToV3Plutus gyRef =
  let (V1.TxOutRef (V1.TxId bs) i) = txOutRefToPlutus gyRef
   in V3.TxOutRef (V3.TxId bs) i

------------------------------------------------------------------------------------------------

-- * Mnemonic Utils

------------------------------------------------------------------------------------------------

readMnemonicFile :: FilePath -> IO GYExtendedPaymentSigningKey
readMnemonicFile path = do
  putStrLn $ yellowColorString $ "Mnemonic phrase at " <> show path
  fileExist <- doesFileExist path
  if fileExist
    then do
      content <- Data.Text.IO.readFile path
      readMnemonic content
    else do
      error $ "File not found: " <> show path

readMnemonic :: Text -> IO GYExtendedPaymentSigningKey
readMnemonic content = do
  case walletKeysToExtendedPaymentSigningKey <$> walletKeysFromMnemonic (Data.Text.words content) of
    Left err -> do
      putStrLn $ yellowColorString $ "Error reading mnemonic: " <> err
      error err
    Right key -> return key

-- | Get inline datum and value from UTxO
getInlineDatumAndValue :: GYUTxO -> Maybe (GYDatum, GYValue)
getInlineDatumAndValue utxo = case utxoOutDatum utxo of
  GYOutDatumInline datum -> Just (datum, utxoValue utxo)
  _ -> Nothing

tnFromGYAssetClass :: (MonadError GYTxMonadException m) => GYAssetClass -> m GYTokenName
tnFromGYAssetClass (GYToken _ gyProfileRefTN) = return gyProfileRefTN
tnFromGYAssetClass _ = throwError (GYApplicationException InvalidAssetClass)

------------------------------------------------------------------------------------------------

-- * Specific Utils

------------------------------------------------------------------------------------------------

-- | Extract profile and value from UTxO
profileAndValueFromUTxO :: GYUTxO -> Maybe (CIP68Datum Onchain.OnchainProfile, Value)
profileAndValueFromUTxO profileStateUTxO = do
  (gyDatum, gyValue) <- getInlineDatumAndValue profileStateUTxO
  cip68Datum <- profileDatumFromDatum gyDatum
  let pVal = valueToPlutus gyValue
  return (cip68Datum, pVal)

-- | Convert GY datum to profile datum
profileDatumFromDatum :: GYDatum -> Maybe (CIP68Datum Onchain.OnchainProfile)
profileDatumFromDatum gyDatum = do
  let plutusDatum = datumToPlutus' gyDatum
  fromBuiltinData plutusDatum

rankAndValueFromUTxO :: GYUTxO -> Maybe (Onchain.OnchainRank, Value)
rankAndValueFromUTxO rankStateUTxO = do
  (gyDatum, gyValue) <- getInlineDatumAndValue rankStateUTxO
  rankDatum <- rankDatumFromDatum gyDatum
  let pVal = valueToPlutus gyValue
  return (rankDatum, pVal)

rankDatumFromDatum :: GYDatum -> Maybe Onchain.OnchainRank
rankDatumFromDatum gyDatum =
  fromBuiltinData (datumToPlutus' gyDatum)

rankFromGYOutDatum :: GYOutDatum -> Maybe Onchain.OnchainRank
rankFromGYOutDatum (GYOutDatumInline gyDatum) = rankDatumFromDatum gyDatum
rankFromGYOutDatum _ = Nothing

profileFromGYOutDatum :: GYOutDatum -> Maybe (CIP68Datum Onchain.OnchainProfile)
profileFromGYOutDatum (GYOutDatumInline gyDatum) = profileDatumFromDatum gyDatum
profileFromGYOutDatum _ = Nothing