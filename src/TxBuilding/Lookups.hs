module TxBuilding.Lookups where

import Control.Monad.Reader.Class
import GeniusYield.Imports hiding (fromMaybe)
import GeniusYield.TxBuilder
import GeniusYield.Types (GYAssetClass(GYToken), GYUTxO, GYUTxOs, GYAddress, utxosToList, utxosFromList, utxoValue, valueAssets)
import PlutusLedgerApi.V1.Value
import Onchain.Types (Profile)
import TxBuilding.Utils
import TxBuilding.Validators
import Control.Monad.Except (MonadError, throwError)
import Data.Text (Text)
import TxBuilding.Exceptions (ProfileException(..))

------------------------------------------------------------------------------------------------

-- * Profile Lookup Functions

------------------------------------------------------------------------------------------------

-- | Get UTxO with profile state token at validator address
getUTxOWithProfileStateToken :: (GYTxUserQueryMonad m, MonadError GYTxMonadException m) => GYAssetClass -> GYAddress -> m GYUTxO
getUTxOWithProfileStateToken profileRefAC validatorAddr = do
  utxos <- utxosAtAddress validatorAddr Nothing
  case findUTxOWithProfileStateToken profileRefAC utxos of
    Just utxo -> return utxo
    Nothing -> throwError (GYApplicationException ProfileNotFound)

-- | Find UTxO with profile state token in a list of UTxOs
findUTxOWithProfileStateToken :: GYAssetClass -> GYUTxOs -> Maybe GYUTxO
findUTxOWithProfileStateToken profileRefAC utxos = do
  let utxoList = utxosToList utxos
  find (\utxo -> hasValidProfileRefToken utxo) utxoList

-- | Get UTxO with profile state token at specific addresses
getUTxOWithProfileStateTokenAtAddresses :: (GYTxUserQueryMonad m, MonadError GYTxMonadException m) => GYAssetClass -> [GYAddress] -> m GYUTxO
getUTxOWithProfileStateTokenAtAddresses profileRefAC addrs = do
  utxosList <- mapM (\addr -> utxosAtAddress addr Nothing) addrs
  let allUtxos = foldMap utxosToList utxosList
  case findUTxOWithProfileStateToken profileRefAC (utxosFromList allUtxos) of
    Just utxo -> return utxo
    Nothing -> throwError (GYApplicationException ProfileNotFound)

-- | Get profile state data and value from asset class
getProfileStateDataAndValue :: (GYTxUserQueryMonad m) => GYAssetClass -> m (Profile, Value)
getProfileStateDataAndValue profileRefAC = do
  profilesValidatorAddr <- scriptAddress profilesValidatorGY
  profileStateUTxO <- getUTxOWithProfileStateToken profileRefAC profilesValidatorAddr
  case profileAndValueFromUTxO profileStateUTxO of
    Just (profile, value) -> return (profile, value)
    Nothing -> throwError (GYApplicationException ProfileNotFound)

------------------------------------------------------------------------------------------------

-- * Helper Functions

------------------------------------------------------------------------------------------------

-- | Check if UTxO has valid profile reference token
hasValidProfileRefToken :: GYUTxO -> Bool
hasValidProfileRefToken gyOut =
  let gyValue = utxoValue gyOut
      gyAssets = valueAssets gyValue
   in any isProfileRefAC gyAssets

-- | Check if asset class is a profile reference token
isProfileRefAC :: GYAssetClass -> Bool
isProfileRefAC (GYToken gyMP gyTN) =
  -- TODO: Update when minting policy is implemented
  -- (gyMP == mintingPolicyId profilesMintingPolicyGY)
  --   && hasRefPrefix (tokenNameToPlutus gyTN)
  False -- Placeholder until minting policy is implemented
isProfileRefAC _ = False

------------------------------------------------------------------------------------------------

-- * Custom Exceptions

------------------------------------------------------------------------------------------------

-- | Custom exception for profile not found
-- data ProfileException = ProfileNotFound
--   deriving stock (Show)

-- Remove or comment out the displayException method from the Exception instance for ProfileNotFound

-- instance Exception ProfileException where
--   displayException ProfileNotFound = "Profile not found" 