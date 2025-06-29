module TxBuilding.Lookups where

import GeniusYield.TxBuilder
import GeniusYield.Types (GYAddress, GYUTxO, filterUTxOs, utxoValue, utxosToList)
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
getUTxOWithNFT :: (GYTxQueryMonad m) => GYAssetClass -> GYAddress -> m GYUTxO
getUTxOWithNFT nftAC addr = do
  utxos <- utxosAtAddress addr (Just nftAC)
  case utxosToList utxos of
    [utxo] -> return utxo
    [] -> throwError (GYApplicationException ProfileNotFound)
    _ -> throwError (GYApplicationException InvalidAssetClass)

-- | Get profile state data and value from asset class
getProfileStateDataAndValue :: (GYTxQueryMonad m) => GYAssetClass -> m (Onchain.Profile, Value)
getProfileStateDataAndValue profileRefAC = do
  profilesValidatorAddr <- scriptAddress profilesValidatorGY
  profileStateUTxO <- getUTxOWithNFT profileRefAC profilesValidatorAddr
  case profileAndValueFromUTxO profileStateUTxO of
    Just (profile, value) -> return (profile, value)
    Nothing -> throwError (GYApplicationException ProfileNotFound)

getUtxoWithTokenAtAddresses :: (GYTxQueryMonad m) => GYAssetClass -> [GYAddress] -> m GYUTxO
getUtxoWithTokenAtAddresses nftAC addrs = do
  utxos <- utxosAtAddresses addrs
  let utxosWithNFT = filterUTxOs (\utxo -> valueAssetPresent (utxoValue utxo) nftAC) utxos
  case utxosToList utxosWithNFT of
    [utxo] -> return utxo
    [] -> throwError (GYApplicationException ProfileNotFound)
    _ -> throwError (GYApplicationException InvalidAssetClass)