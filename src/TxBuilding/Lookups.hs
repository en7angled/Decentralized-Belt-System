module TxBuilding.Lookups where

import GeniusYield.Imports hiding (fromMaybe)
import GeniusYield.TxBuilder
import GeniusYield.Types (GYAddress, GYAssetClass (GYToken), GYUTxO, GYUTxOs, filterUTxOs, utxoValue, utxosFromList, utxosToList, valueAssets)
import GeniusYield.Types.Value
import Onchain.Types qualified as Onchain
import PlutusLedgerApi.V1.Value
import TxBuilding.Exceptions (ProfileException (..))
import TxBuilding.Utils
import TxBuilding.Validators

------------------------------------------------------------------------------------------------

-- * OnChainProfileData Lookup Functions

------------------------------------------------------------------------------------------------

-- | Get UTxO with profile state token at validator address
getUTxOWithProfileStateToken :: (GYTxQueryMonad m) => GYAssetClass -> GYAddress -> m GYUTxO
getUTxOWithProfileStateToken profileRefAC validatorAddr = do
  utxos <- utxosAtAddress validatorAddr (Just profileRefAC)
  case utxosToList utxos of
    [utxo] -> return utxo
    [] -> throwError (GYApplicationException ProfileNotFound)
    _ -> throwError (GYApplicationException InvalidAssetClass)

-- | Get profile state data and value from asset class
getProfileStateDataAndValue :: (GYTxQueryMonad m) => GYAssetClass -> m (Onchain.Profile, Value)
getProfileStateDataAndValue profileRefAC = do
  profilesValidatorAddr <- scriptAddress profilesValidatorGY
  profileStateUTxO <- getUTxOWithProfileStateToken profileRefAC profilesValidatorAddr
  case profileAndValueFromUTxO profileStateUTxO of
    Just (profile, value) -> return (profile, value)
    Nothing -> throwError (GYApplicationException ProfileNotFound)

getUtxoWithTokenAtAddresses :: (GYTxQueryMonad m) => GYAssetClass -> [GYAddress] -> m GYUTxO
getUtxoWithTokenAtAddresses tokenId addrs = do
  utxos <- utxosAtAddresses addrs
  let utxosWithNFT = filterUTxOs (\utxo -> valueAssetPresent (utxoValue utxo) tokenId) utxos
  case utxosToList utxosWithNFT of
    [utxo] -> return utxo
    [] -> throwError (GYApplicationException ProfileNotFound)
    _ -> throwError (GYApplicationException InvalidAssetClass)