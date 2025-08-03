module TxBuilding.Utils where

import Cardano.Api (Key (getVerificationKey), castVerificationKey)
import Control.Monad.Except (throwError)
import GeniusYield.TxBuilder.Class (enclosingSlotFromTime', slotToBeginTime)
import GeniusYield.TxBuilder.Errors (GYTxMonadException (GYApplicationException))
import GeniusYield.TxBuilder.Query.Class (GYTxQueryMonad)
import GeniusYield.Types (Ada, GYAddress, GYAssetClass (..), GYDatum, GYExtendedPaymentSigningKey, GYNetworkId (..), GYOutDatum (..), GYSlot, GYTokenName, GYUTxO, GYUTxOs, GYValue, foldMapUTxOs, fromValue, paymentKeyHash, utxoOutDatum, utxoValue, valueToPlutus)
import GeniusYield.Types.Address (addressFromPaymentKeyHash)
import GeniusYield.Types.Datum (datumToPlutus')
import GeniusYield.Types.Key (extendedPaymentSigningKeyToApi, paymentVerificationKeyFromApi)
import GeniusYield.Types.Slot (unsafeSlotFromInteger)
import GeniusYield.Types.Time (timeFromPlutus, timeToPlutus)
import Onchain.CIP68 (CIP68Datum)
import Onchain.Protocol qualified as Onchain
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V3
import TxBuilding.Exceptions (ProfileException (..))
import Data.Text
import Data.Aeson (FromJSON, decodeFileStrict)
import System.Directory.Extra
import Utils
import qualified Data.Text.IO
import GeniusYield.Types.Wallet


------------------------------------------------------------------------------------------------

-- * Utilities

------------------------------------------------------------------------------------------------

getAdaBalance :: GYUTxOs -> Ada
getAdaBalance = fromValue . getValueBalance

getValueBalance :: GYUTxOs -> Value
getValueBalance = valueToPlutus . foldMapUTxOs utxoValue

addressFromPaymentSigningKey :: GYNetworkId -> GYExtendedPaymentSigningKey -> GYAddress
addressFromPaymentSigningKey nid extendedSkey =
  let vkey = Cardano.Api.getVerificationKey $ extendedPaymentSigningKeyToApi extendedSkey
      pub_key = paymentVerificationKeyFromApi (castVerificationKey vkey)
      payment_key_hash = paymentKeyHash pub_key
      address = addressFromPaymentKeyHash nid payment_key_hash
   in address

pPOSIXTimeFromSlotInteger :: (GYTxQueryMonad m) => Integer -> m POSIXTime
pPOSIXTimeFromSlotInteger = (timeToPlutus <$>) . slotToBeginTime . unsafeSlotFromInteger

pPOSIXTimeFromGYSlot :: (GYTxQueryMonad m) => GYSlot -> m POSIXTime
pPOSIXTimeFromGYSlot = (timeToPlutus <$>) . slotToBeginTime

gySlotFromPOSIXTime :: (GYTxQueryMonad m) => POSIXTime -> m GYSlot
gySlotFromPOSIXTime ptime = do
  enclosingSlotFromTime' (timeFromPlutus ptime)



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

decodeConfigFile :: (FromJSON a) => FilePath -> IO (Maybe a)
decodeConfigFile path = do
  fileExist <- doesFileExist path
  if fileExist
    then decodeFileStrict path 
    else return Nothing
    

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

