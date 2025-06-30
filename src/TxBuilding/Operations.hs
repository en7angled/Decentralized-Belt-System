module TxBuilding.Operations where

import Control.Monad.Reader.Class
import GeniusYield.TxBuilder
import GeniusYield.Types
import Onchain.CIP68 (MetadataFields, mkCIP68Datum, updateCIP68DatumImage, ImageURI)
import Onchain.ProfilesValidator ( ProfilesRedeemer (..))
import Onchain.Types qualified as Onchain
import PlutusLedgerApi.V1.Tx qualified as V1
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.Tx qualified as V3
import TxBuilding.Context (ProfileTxBuildingContext (..))
import TxBuilding.Lookups (getProfileStateDataAndValue)
import TxBuilding.Skeletons
import TxBuilding.Utils
import TxBuilding.Validators
import Onchain.MintingPolicy
import Onchain.Types

------------------------------------------------------------------------------------------------

-- * OnChainProfile Operations

------------------------------------------------------------------------------------------------

-- | Create OnChainProfile Transaction Skeleton
createProfileTX ::
  (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m) =>
  GYAddress ->
  MetadataFields ->
  Onchain.ProfileType ->
  POSIXTime ->
  m (GYTxSkeleton 'PlutusV3, GYAssetClass)
createProfileTX recipient metadata profileType creationDate = do
  creationDateSlot <- gySlotFromPOSIXTime creationDate
  let isAfterCreationDate = isInvalidBefore creationDateSlot
  mintingPolicyRef <- asks profilesValidatorRef

  seedTxOutRef <- someUTxOWithoutRefScript
  let isSpendingSeedUTxO = mustHaveInput (GYTxIn seedTxOutRef GYTxInWitnessKey)
  let (V1.TxOutRef (V1.TxId bs) i) = txOutRefToPlutus seedTxOutRef
  let seedTxOutRefPlutus = V3.TxOutRef (V3.TxId bs) i

  (profileRefAC, profileUserAC) <- gyGenerateRefAndUserAC seedTxOutRef
  let profileId = assetClassToPlutus profileRefAC
  
  let plutusProfile = case profileType of
        Onchain.Practitioner -> fst $ mkPractitionerProfile profileId creationDate defaultProtocolParams
        Onchain.Organization ->  mkOrganizationProfile profileId defaultProtocolParams



  let plutusProfileCIP68Datum = mkCIP68Datum plutusProfile metadata

  let redeemer = CreateProfile seedTxOutRefPlutus metadata profileType creationDate
  let gyRedeemer = redeemerFromPlutus' . toBuiltinData $ redeemer

  isMintingProfileCIP68UserAndRef <- txMustMintCIP68UserAndRef mintingPolicyRef mintingPolicyGY gyRedeemer profileRefAC
  isLockingProfileState <-
    txMustLockStateWithInlineDatumAndValue
      profilesValidatorGY
      plutusProfileCIP68Datum
      (valueSingleton profileUserAC 1)
  isPayingProfileUserNFT <- txIsPayingValueToAddress recipient (valueSingleton profileUserAC 1)



  ifPractitionerMintAndLockRankState <- case profileType of
        Onchain.Organization -> return mempty
        Onchain.Practitioner -> do 
          let rankData = snd $ mkPractitionerProfile profileId creationDate defaultProtocolParams
          rankAC <-  assetClassFromPlutus' $ rankId rankData
          isMintingRank <- txMustMintWithMintRef True mintingPolicyRef mintingPolicyGY gyRedeemer rankAC
          isLockingRankState <-
            txMustLockStateWithInlineDatumAndValue
              ranksValidatorGY
              rankData
              (valueSingleton rankAC 1)
          return $ 
            mconcat [
                 isMintingRank,
                 isLockingRankState
                 ]

  return
    ( mconcat
        [ isSpendingSeedUTxO,
          isMintingProfileCIP68UserAndRef,
          isLockingProfileState,
          isPayingProfileUserNFT,
          isAfterCreationDate,
          ifPractitionerMintAndLockRankState
        ],
      profileRefAC
    )

-- | Update OnChainProfile Transaction Skeleton
updateProfileTX ::
  (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m) =>
  GYAssetClass ->
  ImageURI ->
  [GYAddress] ->
  m (GYTxSkeleton 'PlutusV3)
updateProfileTX profileRefAC newImageURI ownAddrs = do
  profilesScriptRef <- asks profilesValidatorRef
  (plutusProfileDatum, plutusValue) <- getProfileStateDataAndValue profileRefAC
  let updateRedeemer = UpdateProfileImage (assetClassToPlutus profileRefAC) newImageURI
  let gyRedeemer = redeemerFromPlutus' . toBuiltinData $ updateRedeemer
  spendsProfileRefNFT <- txMustSpendStateFromRefScriptWithRedeemer profilesScriptRef profileRefAC gyRedeemer profilesValidatorGY
  profileUserAC <- gyDeriveUserFromRefAC profileRefAC
  spendsProfileUserNFT <- txMustSpendFromAddress profileUserAC ownAddrs
  let newCip68Datum = updateCIP68DatumImage newImageURI plutusProfileDatum
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
  mintingPolicyRef <- asks profilesValidatorRef
  (_plutusProfileDatum, plutusValue) <- getProfileStateDataAndValue profileRefAC
  let spendRedeemer = DeleteProfile (assetClassToPlutus profileRefAC)
  let gySpendRedeemer = redeemerFromPlutus' . toBuiltinData $ spendRedeemer
  let burnRedeemer = BurnProfileId (assetClassToPlutus profileRefAC)
  let gyBurnRedeemer = redeemerFromPlutus' . toBuiltinData $ burnRedeemer
  spendsProfileRefNFT <- txMustSpendStateFromRefScriptWithRedeemer profilesScriptRef profileRefAC gySpendRedeemer profilesValidatorGY
  gyProfileValue <- valueFromPlutus' plutusValue
  isGettingProfileValue <- txIsPayingValueToAddress recipient gyProfileValue
  isBurningProfileRefAndUserNFTs <- txMustBurnCIP68UserAndRef mintingPolicyRef mintingPolicyGY gyBurnRedeemer profileRefAC
  return $
    mconcat
      [ spendsProfileRefNFT,
        isGettingProfileValue,
        isBurningProfileRefAndUserNFTs --- TODO: check redeemer
      ]

