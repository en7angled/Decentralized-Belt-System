module TxBuilding.Operations where

import Control.Monad.Reader.Class
import GeniusYield.Imports hiding (fromMaybe)
import GeniusYield.TxBuilder
import GeniusYield.Types
import qualified PlutusLedgerApi.V1.Tx as V1
import qualified PlutusLedgerApi.V3.Tx as V3
import PlutusLedgerApi.V1.Value
import Onchain.CIP68 (CIP68Datum, MetadataFields, mkCIP68Datum)
import Onchain.ProfilesValidator (ProfilesRedeemer (..))
import Onchain.Types (Profile)
import TxBuilding.Skeletons
import TxBuilding.Validators
import TxBuilding.Context (ProfileTxBuildingContext(..))
import TxBuilding.Lookups (getProfileStateDataAndValue)
import GeniusYield.Types (GYAssetClass)

------------------------------------------------------------------------------------------------

-- * Profile Actions

------------------------------------------------------------------------------------------------

-- | Create Profile Transaction
createProfileTX ::
  (GYTxUserQueryMonad m) =>
  GYAddress ->
  MetadataFields ->
  Profile ->
  m (GYTxSkeleton 'PlutusV3)
createProfileTX recipient metadata profile = do
  seedTxOutRef <- someUTxOWithoutRefScript
  let isSpendingSeedUTxO = mustHaveInput (GYTxIn seedTxOutRef GYTxInWitnessKey)
  let (V1.TxOutRef (V1.TxId bs) i) = txOutRefToPlutus seedTxOutRef
  let seedTxOutRefPlutus = V3.TxOutRef (V3.TxId bs) i
  let createRedeemer = CreateProfile seedTxOutRefPlutus metadata profile
  let cip68Datum = mkCIP68Datum profile metadata
  isLockingProfileState <-
    txMustLockStateWithInlineDatumAndValue
      profilesValidatorGY
      cip68Datum
      (GeniusYield.Types.lovelaceValueOf 2000000) -- 2 ADA collateral
  return
    ( mconcat
        [ isLockingProfileState,
          isSpendingSeedUTxO
        ]
    )

-- | Update Profile Transaction
updateProfileTX ::
  (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m) =>
  GYAddress ->
  MetadataFields ->
  GYAssetClass ->
  m (GYTxSkeleton 'PlutusV3)
updateProfileTX recipient newMetadata profileRefAC = do
  profilesScriptRef <- asks profilesValidatorRef
  (profile, pValue) <- getProfileStateDataAndValue profileRefAC
  let updateRedeemer = UpdateProfile newMetadata
  spendsProfileRefNFT <- txMustSpendStateFromRefScriptWithRedeemer profilesScriptRef profileRefAC updateRedeemer profilesValidatorGY
  let newCip68Datum = mkCIP68Datum profile newMetadata
  isProfileStateUpdated <- txMustLockStateWithInlineDatumAndValue profilesValidatorGY newCip68Datum pValue
  return $
    mconcat
      [ spendsProfileRefNFT,
        isProfileStateUpdated
      ]

-- | Delete Profile Transaction
deleteProfileTX ::
  (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m) =>
  GYAddress ->
  GYAssetClass ->
  m (GYTxSkeleton 'PlutusV3)
deleteProfileTX recipient profileRefAC = do
  profilesScriptRef <- asks profilesValidatorRef
  (profile, pValue) <- getProfileStateDataAndValue profileRefAC
  let deleteRedeemer = DeleteProfile
  spendsProfileRefNFT <- txMustSpendStateFromRefScriptWithRedeemer profilesScriptRef profileRefAC deleteRedeemer profilesValidatorGY
  stakeValue <- valueFromPlutus' pValue
  isGettingStakeValue <- txIsPayingValueToAddress recipient stakeValue
  return $
    mconcat
      [ spendsProfileRefNFT,
        isGettingStakeValue
      ]

------------------------------------------------------------------------------------------------

-- * Helper Functions

------------------------------------------------------------------------------------------------

-- TODO: Implement these helper functions when we have the lookup infrastructure
-- getProfileStateDataAndValue :: (GYTxUserQueryMonad m) => AssetClass -> m (Profile, Value)
-- getProfileStateDataAndValue profileRefAC = do
--   profilesValidatorAddr <- scriptAddress profilesValidatorGY
--   profileStateUTxO <- getUTxOWithStateToken profileRefAC profilesValidatorAddr
--   case profileAndValueFromUTxO profileStateUTxO of
--     Just (profile, value) -> return (profile, value)
--     Nothing -> throwError (GYApplicationException ProfileNotFound)

-- TODO: Add context type for profile validator reference
-- data ProfileTxBuildingContext = ProfileTxBuildingContext
--   { profilesValidatorRef :: GYTxOutRef
--   }
--   deriving stock (Generic, Show)

-- TODO: Add reader monad instance
-- instance MonadReader ProfileTxBuildingContext m => MonadReader ProfileTxBuildingContext (GYTxUserQueryT m) where
--   ask = lift ask
--   local f = mapGYTxUserQueryT (local f) 