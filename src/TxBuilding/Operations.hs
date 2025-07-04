module TxBuilding.Operations where

import Control.Monad.Reader.Class
import GeniusYield.TxBuilder
import GeniusYield.Types
import Onchain.CIP68 (ImageURI, MetadataFields, extra, mkCIP68Datum, updateCIP68DatumImage)
import Onchain.MintingPolicy
import Onchain.ProfilesValidator (ProfilesRedeemer (..))
import Onchain.Protocol
import qualified Onchain.Protocol as Onchain
import qualified PlutusLedgerApi.V1.Tx as V1
import PlutusLedgerApi.V3
import qualified PlutusLedgerApi.V3.Tx as V3
import TxBuilding.Context (ProfileTxBuildingContext (..))
import TxBuilding.Lookups (getProfileStateDataAndValue, getRankStateDataAndValue)
import TxBuilding.Skeletons
import TxBuilding.Utils
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
  POSIXTime ->
  m (GYTxSkeleton 'PlutusV3, GYAssetClass)
createProfileTX recipient metadata profileType creationDate = createProfileWithRankTX recipient metadata profileType creationDate 0

-- | Create OnChainProfile Transaction Skeleton
createProfileWithRankTX ::
  (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m) =>
  GYAddress ->
  MetadataFields ->
  Onchain.ProfileType ->
  POSIXTime ->
  Integer ->
  m (GYTxSkeleton 'PlutusV3, GYAssetClass)
createProfileWithRankTX recipient metadata profileType creationDate rankNumber = do
  creationDateSlot <- gySlotFromPOSIXTime creationDate
  let isAfterCreationDate = isInvalidBefore creationDateSlot

  mintingPolicyRef <- asks profilesValidatorRef
  seedTxOutRef <- someUTxOWithoutRefScript
  let isSpendingSeedUTxO = mustHaveInput (GYTxIn seedTxOutRef GYTxInWitnessKey)
  let (V1.TxOutRef (V1.TxId bs) i) = txOutRefToPlutus seedTxOutRef
  let seedTxOutRefPlutus = V3.TxOutRef (V3.TxId bs) i

  (gyProfileRefAC, gyProfileUserAC) <- gyGenerateRefAndUserAC seedTxOutRef
  let profileRefAC = assetClassToPlutus gyProfileRefAC

  let plutusProfile = case profileType of
        Onchain.Practitioner -> fst $ mkPractitionerProfile profileRefAC creationDate defaultProtocolParams rankNumber
        Onchain.Organization -> mkOrganizationProfile profileRefAC defaultProtocolParams
  let plutusProfileCIP68Datum = mkCIP68Datum plutusProfile metadata

  let redeemer = CreateProfile seedTxOutRefPlutus metadata profileType creationDate rankNumber
  let gyCreateProfileRedeemer = redeemerFromPlutus' . toBuiltinData $ redeemer

  isMintingProfileCIP68UserAndRef <- txMustMintCIP68UserAndRef mintingPolicyRef mintingPolicyGY gyCreateProfileRedeemer gyProfileRefAC
  isLockingProfileState <-
    txMustLockStateWithInlineDatumAndValue
      profilesValidatorGY
      plutusProfileCIP68Datum
      (valueSingleton gyProfileUserAC 1)
  isPayingProfileUserNFT <- txIsPayingValueToAddress recipient (valueSingleton gyProfileUserAC 1)

  ifPractitionerMintAndLockFirstRankState <- case profileType of
    Onchain.Organization -> return mempty
    Onchain.Practitioner -> do
      let rankData = snd $ mkPractitionerProfile profileRefAC creationDate defaultProtocolParams rankNumber
      gyRankAC <- assetClassFromPlutus' $ rankId rankData
      isMintingRank <- txMustMintWithMintRef True mintingPolicyRef mintingPolicyGY gyCreateProfileRedeemer gyRankAC
      isLockingRankState <-
        txMustLockStateWithInlineDatumAndValue
          ranksValidatorGY
          rankData
          (valueSingleton gyRankAC 1)
      return $
        mconcat
          [ isMintingRank,
            isLockingRankState
          ]

  return
    ( mconcat
        [ isSpendingSeedUTxO,
          isAfterCreationDate,
          isMintingProfileCIP68UserAndRef,
          isLockingProfileState,
          isPayingProfileUserNFT,
          ifPractitionerMintAndLockFirstRankState
        ],
      gyProfileRefAC
    )

-- | Update OnChainProfile Transaction Skeleton
updateProfileTX ::
  (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m) =>
  GYAssetClass ->
  ImageURI ->
  [GYAddress] ->
  m (GYTxSkeleton 'PlutusV3)
updateProfileTX gyProfileRefAC newImageURI ownAddrs = do
  profilesScriptRef <- asks profilesValidatorRef
  (plutusProfileDatum, plutusProfileValue) <- getProfileStateDataAndValue gyProfileRefAC
  let updateRedeemer = UpdateProfileImage (assetClassToPlutus gyProfileRefAC) newImageURI
  let gyRedeemer = redeemerFromPlutus' . toBuiltinData $ updateRedeemer
  spendsProfileRefNFT <- txMustSpendStateFromRefScriptWithRedeemer profilesScriptRef gyProfileRefAC gyRedeemer profilesValidatorGY
  gyProfileUserAC <- gyDeriveUserFromRefAC gyProfileRefAC
  spendsProfileUserNFT <- txMustSpendFromAddress gyProfileUserAC ownAddrs
  let newCip68Datum = updateCIP68DatumImage newImageURI plutusProfileDatum
  gyProfileValue <- valueFromPlutus' plutusProfileValue
  isLockingUpdatedProfileState <- txMustLockStateWithInlineDatumAndValue profilesValidatorGY newCip68Datum gyProfileValue
  return $
    mconcat
      [ spendsProfileUserNFT,
        spendsProfileRefNFT,
        isLockingUpdatedProfileState
      ]

-- | Delete OnChainProfile Transaction Skeleton
deleteProfileTX ::
  (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m) =>
  GYAssetClass ->
  GYAddress ->
  [GYAddress] ->
  m (GYTxSkeleton 'PlutusV3)
deleteProfileTX gyProfileRefAC recipient ownAddrs = do
  profilesScriptRef <- asks profilesValidatorRef
  mintingPolicyRef <- asks profilesValidatorRef
  (_plutusProfileDatum, plutusProfileValue) <- getProfileStateDataAndValue gyProfileRefAC
  let gySpendProfileRedeemer = redeemerFromPlutus' . toBuiltinData $ DeleteProfile (assetClassToPlutus gyProfileRefAC)
  let gyBurnProfileRedeemer = redeemerFromPlutus' . toBuiltinData $ BurnProfileId
  spendsProfileRefNFT <- txMustSpendStateFromRefScriptWithRedeemer profilesScriptRef gyProfileRefAC gySpendProfileRedeemer profilesValidatorGY
  gyProfileValue <- valueFromPlutus' plutusProfileValue
  isGettingProfileValue <- txIsPayingValueToAddress recipient gyProfileValue

  gyProfileUserAC <- gyDeriveUserFromRefAC gyProfileRefAC
  spendsProfileUserNFT <- txMustSpendFromAddress gyProfileUserAC ownAddrs

  isBurningProfileRefAndUserNFTs <- txMustBurnCIP68UserAndRef mintingPolicyRef mintingPolicyGY gyBurnProfileRedeemer gyProfileRefAC
  return $
    mconcat
      [ spendsProfileRefNFT,
        isGettingProfileValue,
        spendsProfileUserNFT,
        isBurningProfileRefAndUserNFTs
      ]

-- Promote ProfileId ProfileId POSIXTime Integer

promoteProfileTX ::
  (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m) =>
  GYAssetClass ->
  GYAssetClass ->
  POSIXTime ->
  Integer ->
  [GYAddress] ->
  m (GYTxSkeleton 'PlutusV3)
promoteProfileTX gyPromotedProfileId gyPromotedByProfileId achievementDate rankNumber ownAddrs = do
  mintingPolicyRef <- asks profilesValidatorRef

  gyMasterUserAC <- gyDeriveUserFromRefAC gyPromotedByProfileId
  spendsMasterProfileUserNFT <- txMustSpendFromAddress gyMasterUserAC ownAddrs

  let redeemer = Promote (assetClassToPlutus gyPromotedProfileId) (assetClassToPlutus gyPromotedByProfileId) achievementDate rankNumber
  let gyRedeemer = redeemerFromPlutus' . toBuiltinData $ redeemer
  gyPromotionRankAC <- assetClassFromPlutus' $ generateRankId (assetClassToPlutus gyPromotedProfileId) rankNumber
  isMintingPromotionRank <- txMustMintWithMintRef True mintingPolicyRef mintingPolicyGY gyRedeemer gyPromotionRankAC

  let pendingRankDatum = mkPendingRank (assetClassToPlutus gyPromotedProfileId) (assetClassToPlutus gyPromotedByProfileId) achievementDate rankNumber defaultProtocolParams
  isLockingPendingRankState <-
    txMustLockStateWithInlineDatumAndValue
      ranksValidatorGY
      pendingRankDatum
      (valueSingleton gyPromotionRankAC 1)

  return $
    mconcat
      [ spendsMasterProfileUserNFT,
        isMintingPromotionRank,
        isLockingPendingRankState
      ]

acceptPromotionTX ::
  (GYTxUserQueryMonad m, MonadReader ProfileTxBuildingContext m) =>
  GYAssetClass ->
  [GYAddress] ->
  m (GYTxSkeleton 'PlutusV3)
acceptPromotionTX gyPromotionId ownAddrs = do
  ranksValidatorRef <- asks ranksValidatorRef
  profilesScriptRef <- asks profilesValidatorRef

  (plutusPromotionRankDatum, plutusPromotionRankValue) <- getRankStateDataAndValue gyPromotionId

  let studentProfileRefAC = rankAchievedByProfileId plutusPromotionRankDatum
  gyStudentProfileRefAC <- assetClassFromPlutus' studentProfileRefAC
  let masterProfileRefAC = rankAwardedByProfileId plutusPromotionRankDatum
  gyMasterProfileRefAC <- assetClassFromPlutus' masterProfileRefAC

  gyStudentProfileUserAC <- gyDeriveUserFromRefAC gyStudentProfileRefAC
  spendsStudentProfileUserNFT <- txMustSpendFromAddress gyStudentProfileUserAC ownAddrs
  let gySpendProfileRedeemer = redeemerFromPlutus' . toBuiltinData $ AcceptPromotion (assetClassToPlutus gyPromotionId)
  spendsStudentProfileRefNFT <- txMustSpendStateFromRefScriptWithRedeemer profilesScriptRef gyStudentProfileRefAC gySpendProfileRedeemer profilesValidatorGY

  (plutusProfileDatum, plutusProfileValue) <- getProfileStateDataAndValue gyStudentProfileRefAC
  let plutusStudentProfile = extra plutusProfileDatum
  let studentCurrentRankId = Onchain.getCurrentRankId plutusStudentProfile
  gyStudentCurrentRankAC <- assetClassFromPlutus' studentCurrentRankId

  (plutusMasterProfileDatum, _plutusMasterProfileValue) <- getProfileStateDataAndValue gyMasterProfileRefAC
  let plutusMasterProfile = extra plutusMasterProfileDatum
  let masterCurrentRankId = Onchain.getCurrentRankId plutusMasterProfile
  gyMasterCurrentRankAC <- assetClassFromPlutus' masterCurrentRankId

  let (plutusStudentUpdatedProfileDatum, plutuStudentUpdatedRankDatum) = promoteProfile plutusProfileDatum plutusPromotionRankDatum

  gyProfileValue <- valueFromPlutus' plutusProfileValue
  isLockingUpdatedStudentProfile <- txMustLockStateWithInlineDatumAndValue profilesValidatorGY plutusStudentUpdatedProfileDatum gyProfileValue

  gyRankValue <- valueFromPlutus' plutusPromotionRankValue
  isLockingUpdatedRank <- txMustLockStateWithInlineDatumAndValue ranksValidatorGY plutuStudentUpdatedRankDatum gyRankValue

  spendsPromotionRank <- txMustSpendStateFromRefScriptWithRedeemer ranksValidatorRef gyPromotionId unitRedeemer ranksValidatorGY

  referencesMasterAndStudentProfilesAndRanks <- txMustHaveUTxOsAsRefInputs [gyMasterProfileRefAC, gyStudentProfileRefAC, gyStudentCurrentRankAC, gyMasterCurrentRankAC]

  return $
    mconcat
      [ spendsStudentProfileUserNFT,
        spendsStudentProfileRefNFT,
        isLockingUpdatedStudentProfile,
        spendsPromotionRank,
        isLockingUpdatedRank,
        referencesMasterAndStudentProfilesAndRanks
      ]
