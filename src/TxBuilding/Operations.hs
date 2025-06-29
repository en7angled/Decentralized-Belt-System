module TxBuilding.Operations where

import Control.Monad.Reader.Class
import GeniusYield.TxBuilder
import GeniusYield.Types
import Onchain.CIP68 (MetadataFields, mkCIP68Datum)
import Onchain.ProfilesValidator (ProfilesRedeemer (..))
import Onchain.Types qualified as Onchain
import PlutusLedgerApi.V1.Tx qualified as V1
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.Tx qualified as V3
import TxBuilding.Context (ProfileTxBuildingContext (..))
import TxBuilding.Lookups (getProfileStateDataAndValue)
import TxBuilding.Skeletons
import TxBuilding.Validators

------------------------------------------------------------------------------------------------

-- * OnChainProfile Operations

------------------------------------------------------------------------------------------------

-- | Create OnChainProfile Transaction Skeleton
createProfileTX ::
  (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m) =>
  GYAddress ->
  MetadataFields ->
  Onchain.ProfileType ->
  m (GYTxSkeleton 'PlutusV3, GYAssetClass)
createProfileTX recipient metadata profileType = do
  profilesScriptRef <- asks profilesValidatorRef
  seedTxOutRef <- someUTxOWithoutRefScript
  let isSpendingSeedUTxO = mustHaveInput (GYTxIn seedTxOutRef GYTxInWitnessKey)
  let (V1.TxOutRef (V1.TxId bs) i) = txOutRefToPlutus seedTxOutRef
  let seedTxOutRefPlutus = V3.TxOutRef (V3.TxId bs) i
  let redeemer = CreateProfile seedTxOutRefPlutus metadata profileType
  let gyRedeemer = redeemerFromPlutus' . toBuiltinData $ redeemer
  let profilesMintingPolicy = mintingPolicyIdToCurrencySymbol $ mintingPolicyId profilesValidatorGY
  let ranksMintingPolicy = mintingPolicyIdToCurrencySymbol $ mintingPolicyId ranksValidatorGY
  let profileCIP68Datum = mkCIP68Datum (Onchain.mkProfile profilesMintingPolicy ranksMintingPolicy seedTxOutRefPlutus profileType) metadata
  (profileRefAC, profileUserAC) <- gyGenerateRefAndUserAC seedTxOutRef
  isMintingProfileCIP68UserAndRef <- txMustMintCIP68UserAndRef profilesScriptRef profilesValidatorGY gyRedeemer profileRefAC
  isLockingProfileState <-
    txMustLockStateWithInlineDatumAndValue
      profilesValidatorGY
      profileCIP68Datum
      (valueSingleton profileUserAC 1)
  isPayingProfileUserNFT <- txIsPayingValueToAddress recipient (valueSingleton profileUserAC 1)
  return
    ( mconcat
        [ isSpendingSeedUTxO,
          isMintingProfileCIP68UserAndRef,
          isLockingProfileState,
          isPayingProfileUserNFT
          ---- TODO: Mint rank NFT
          ---- TODO: Lock rank state at rank validator
        ],
      profileRefAC
    )

-- | Update OnChainProfile Transaction Skeleton
updateProfileTX ::
  (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m) =>
  GYAssetClass ->
  MetadataFields ->
  [GYAddress] ->
  m (GYTxSkeleton 'PlutusV3)
updateProfileTX profileRefAC newMetadata ownAddrs = do
  profilesScriptRef <- asks profilesValidatorRef
  (plutusProfile, plutusValue) <- getProfileStateDataAndValue profileRefAC
  let updateRedeemer = UpdateProfile newMetadata
  let gyRedeemer = redeemerFromPlutus' . toBuiltinData $ updateRedeemer
  spendsProfileRefNFT <- txMustSpendStateFromRefScriptWithRedeemer profilesScriptRef profileRefAC gyRedeemer profilesValidatorGY
  profileUserAC <- gyDeriveUserFromRefAC profileRefAC
  spendsProfileUserNFT <- txMustSpendFromAddress profileUserAC ownAddrs

  let newCip68Datum = mkCIP68Datum plutusProfile newMetadata
  gyProfileValue <- valueFromPlutus' plutusValue
  isProfileStateUpdated <- txMustLockStateWithInlineDatumAndValue profilesValidatorGY newCip68Datum gyProfileValue
  return $
    mconcat
      [ spendsProfileUserNFT,
        spendsProfileRefNFT,
        isProfileStateUpdated
      ]

-- | Delete OnChainProfile Transaction Skeleton
deleteProfileTX ::
  (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m) =>
  GYAssetClass ->
  GYAddress ->
  m (GYTxSkeleton 'PlutusV3)
deleteProfileTX profileRefAC recipient = do
  profilesScriptRef <- asks profilesValidatorRef
  (_plutusProfile, plutusValue) <- getProfileStateDataAndValue profileRefAC
  let redeemer = DeleteProfile
  let gyRedeemer = redeemerFromPlutus' . toBuiltinData $ redeemer
  spendsProfileRefNFT <- txMustSpendStateFromRefScriptWithRedeemer profilesScriptRef profileRefAC gyRedeemer profilesValidatorGY
  gyProfileValue <- valueFromPlutus' plutusValue
  isGettingProfileValue <- txIsPayingValueToAddress recipient gyProfileValue
  isBurningProfileRefNFT <- txMustBurnCIP68UserAndRef profilesScriptRef profilesValidatorGY gyRedeemer profileRefAC
  return $
    mconcat
      [ spendsProfileRefNFT,
        isGettingProfileValue,
        isBurningProfileRefNFT
      ]

------------------------------------------------------------------------------------------------

-- * Helper Functions

------------------------------------------------------------------------------------------------

-- TODO: Implement these helper functions when we have the lookup infrastructure
-- getProfileStateDataAndValue :: (GYTxUserQueryMonad m) => AssetClass -> m (OnChainProfile, Value)
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