module TxBuilding.Operations where

import Control.Monad.Reader.Class
import Data.Maybe
import GHC.Stack (HasCallStack)
import GeniusYield.TxBuilder
import GeniusYield.Types
import Onchain.BJJ (BJJBelt (White), beltToInt)
import Onchain.CIP68 (ImageURI, MetadataFields, extra, mkCIP68Datum, updateCIP68DatumImage)
import Onchain.MintingPolicy
import Onchain.ProfilesValidator (ProfilesRedeemer (..))
import Onchain.Protocol (OnchainRank (..), deriveMembershipHistoriesListId, generatePromotionRankId, initEmptyMembershipHistoriesList, mkOrganizationProfile, mkPendingRank, mkPractitionerProfile, promoteProfile)
import Onchain.Protocol qualified as Onchain
import PlutusLedgerApi.V1.Tx qualified as V1
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.Tx qualified as V3
import TxBuilding.Context (DeployedScriptsContext (..), getMintingPolicyRef, getProfilesValidatorRef, getRanksValidatorRef)
import TxBuilding.Lookups (getProfileStateDataAndValue, getRankStateDataAndValue)
import TxBuilding.Skeletons
import TxBuilding.Validators
import Onchain.RanksValidator (RanksRedeemer(..))

datumLovelaces :: Integer
datumLovelaces = 3500000

------------------------------------------------------------------------------------------------

-- * Verify If Deployed Scripts Are Ready

------------------------------------------------------------------------------------------------

verifyDeployedScriptsAreReady ::
  (GYTxQueryMonad m, MonadReader DeployedScriptsContext m) =>
  m Bool
verifyDeployedScriptsAreReady = do
  mpRef <- asks getMintingPolicyRef
  pvRef <- asks getProfilesValidatorRef
  rvRef <- asks getRanksValidatorRef

  mintingPolicyUTxO <- utxoAtTxOutRef mpRef
  let mintingPolicyHasRefScript = utxoRefScript =<< mintingPolicyUTxO

  profilesValidatorUTxO <- utxoAtTxOutRef pvRef
  let profilesValidatorHasRefScript = utxoRefScript =<< profilesValidatorUTxO

  ranksValidatorUTxO <- utxoAtTxOutRef rvRef
  let ranksValidatorHasRefScript = utxoRefScript =<< ranksValidatorUTxO

  return $ all isJust [mintingPolicyHasRefScript, profilesValidatorHasRefScript, ranksValidatorHasRefScript]

------------------------------------------------------------------------------------------------

-- * OnChainProfile Operations

------------------------------------------------------------------------------------------------

-- | Create OnChainProfile Transaction Skeleton
createProfileTX ::
  (GYTxUserQueryMonad m, MonadReader DeployedScriptsContext m) =>
  GYAddress ->
  MetadataFields ->
  Onchain.OnChainProfileType ->
  POSIXTime ->
  m (GYTxSkeleton 'PlutusV3, GYAssetClass)
createProfileTX recipient metadata profileType creationDate = createProfileWithRankTX recipient metadata profileType creationDate White

-- | Create OnChainProfile Transaction Skeleton
-- Output indices are tracked and passed in the redeemer for efficient on-chain validation.
createProfileWithRankTX ::
  (GYTxUserQueryMonad m, MonadReader DeployedScriptsContext m) =>
  GYAddress ->
  MetadataFields ->
  Onchain.OnChainProfileType ->
  POSIXTime ->
  BJJBelt ->
  m (GYTxSkeleton 'PlutusV3, GYAssetClass)
createProfileWithRankTX recipient metadata profileType creationDate belt = do
  now <- slotOfCurrentBlock
  let isInvalidBeforeNow = isInvalidBefore now

  mpRef <- asks getMintingPolicyRef
  seedTxOutRef <- someUTxOWithoutRefScript
  let isSpendingSeedUTxO = mustHaveInput (GYTxIn seedTxOutRef GYTxInWitnessKey)
  let (V1.TxOutRef (V1.TxId bs) i) = txOutRefToPlutus seedTxOutRef
  let seedTxOutRefPlutus = V3.TxOutRef (V3.TxId bs) i

  (gyProfileRefAC, gyProfileUserAC) <- gyGenerateRefAndUserAC seedTxOutRef
  gyLogDebug' "gyProfileRefAC: " $ show gyProfileRefAC

  let profileRefAC = assetClassToPlutus gyProfileRefAC

  let plutusProfile = case profileType of
        Onchain.Practitioner -> fst $ mkPractitionerProfile profileRefAC creationDate defaultProtocolParams (beltToInt belt)
        Onchain.Organization -> mkOrganizationProfile profileRefAC defaultProtocolParams
  let plutusProfileCIP68Datum = mkCIP68Datum plutusProfile metadata

  -- ============================================================
  -- Output index tracking (order must match skeleton mconcat order)
  -- ============================================================
  -- Output 0: Profile state locked at profilesValidator
  -- Output 1: User NFT payment to recipient
  -- Output 2: Rank state locked at ranksValidator (Practitioner only)
  let profileOutputIdx = 0 :: Integer
  let rankOrMembershipHistoriesRootOutputIdx = 2 :: Integer
  let redeemer = CreateProfile seedTxOutRefPlutus metadata profileType creationDate (beltToInt belt) profileOutputIdx rankOrMembershipHistoriesRootOutputIdx
  let gyCreateProfileRedeemer = redeemerFromPlutus' . toBuiltinData $ redeemer

  isMintingProfileCIP68UserAndRef <- txMustMintCIP68UserAndRef mpRef mintingPolicyGY gyCreateProfileRedeemer gyProfileRefAC

  -- Output 0: Profile state
  isLockingProfileState <-
    txMustLockStateWithInlineDatumAndValue
      profilesValidatorGY
      plutusProfileCIP68Datum
      (valueSingleton gyProfileRefAC 1 <> valueFromLovelace datumLovelaces)

  -- Output 1: User NFT payment
  isPayingProfileUserNFT <- txIsPayingValueToAddress recipient (valueSingleton gyProfileUserAC 1)

  -- Output 2: Rank state (Practitioner only) or Membership Histories Root NFT (Organization only)

  isLockingRankOrMembershipHistoriesRootState <- case profileType of
    Onchain.Organization -> do
      let membershipHistoriesRootDatum = initEmptyMembershipHistoriesList profileRefAC
      gyMembershipHistoriesRootAC <- assetClassFromPlutus' $ deriveMembershipHistoriesListId profileRefAC
      isMintingMembershipHistoriesRoot <- txMustMintWithMintRef True mpRef mintingPolicyGY gyCreateProfileRedeemer gyMembershipHistoriesRootAC
      isLockingMembershipHistoriesRootState <-
        txMustLockStateWithInlineDatumAndValue
          membershipsValidatorGY
          membershipHistoriesRootDatum
          (valueSingleton gyMembershipHistoriesRootAC 1 <> valueFromLovelace datumLovelaces)
      return $
        mconcat
          [ isMintingMembershipHistoriesRoot,
            isLockingMembershipHistoriesRootState -- Output 2: Membership Histories Root NFT
          ]
    Onchain.Practitioner -> do
      let rankData = snd $ mkPractitionerProfile profileRefAC creationDate defaultProtocolParams (beltToInt belt)
      gyRankAC <- assetClassFromPlutus' $ rankId rankData
      isMintingRank <- txMustMintWithMintRef True mpRef mintingPolicyGY gyCreateProfileRedeemer gyRankAC
      isLockingRankState <-
        txMustLockStateWithInlineDatumAndValue
          ranksValidatorGY
          rankData
          (valueSingleton gyRankAC 1 <> valueFromLovelace datumLovelaces)
      return $
        mconcat
          [ isMintingRank,
            isLockingRankState -- Output 2
          ]

  return
    ( mconcat
        [ isSpendingSeedUTxO, -- Input (no output index)
          isInvalidBeforeNow, -- Validity (no output index)
          isMintingProfileCIP68UserAndRef, -- Mint (no output index)
          isLockingProfileState, -- Output 0: Profile state
          isPayingProfileUserNFT, -- Output 1: User NFT payment
          isLockingRankOrMembershipHistoriesRootState -- Output 2: Rank state (if Practitioner) or Membership Histories Root NFT (if Organization)
        ],
      gyProfileRefAC
    )

-- | Update OnChainProfile Transaction Skeleton
-- Output indices are tracked and passed in the redeemer for efficient on-chain validation.
updateProfileTX ::
  (GYTxUserQueryMonad m, MonadReader DeployedScriptsContext m) =>
  GYAssetClass ->
  ImageURI ->
  [GYAddress] ->
  m (GYTxSkeleton 'PlutusV3)
updateProfileTX gyProfileRefAC newImageURI ownAddrs = do
  pvRef <- asks getProfilesValidatorRef

  -- ============================================================
  -- Output index tracking (order must match skeleton mconcat order)
  -- ============================================================
  -- Output 0: Updated profile state locked at profilesValidator
  let profileOutputIdx = 0 :: Integer

  (plutusProfileDatum, plutusProfileValue) <- getProfileStateDataAndValue gyProfileRefAC
  let updateRedeemer = UpdateProfileImage newImageURI profileOutputIdx
  let gyRedeemer = redeemerFromPlutus' . toBuiltinData $ updateRedeemer
  spendsProfileRefNFT <- txMustSpendStateFromRefScriptWithRedeemer pvRef gyProfileRefAC gyRedeemer profilesValidatorGY
  gyProfileUserAC <- gyDeriveUserFromRefAC gyProfileRefAC
  spendsProfileUserNFT <- txMustSpendFromAddress gyProfileUserAC ownAddrs
  let newCip68Datum = updateCIP68DatumImage newImageURI plutusProfileDatum
  gyProfileValue <- valueFromPlutus' plutusProfileValue

  -- Output 0: Updated profile state
  isLockingUpdatedProfileState <- txMustLockStateWithInlineDatumAndValue profilesValidatorGY newCip68Datum gyProfileValue
  return $
    mconcat
      [ spendsProfileUserNFT, -- Input (no output index)
        spendsProfileRefNFT, -- Input (no output index)
        isLockingUpdatedProfileState -- Output 0: Updated profile state
      ]

-- NOTE: deleteProfileTX is intentionally not implemented to preserve lineage integrity.
-- BJJ belt records are permanent historical facts that should not be erasable.

-- Promote TxOutRef ProfileId ProfileId POSIXTime Integer

-- | Promote a profile (award a new belt rank)
-- Output indices are tracked and passed in the redeemer for efficient on-chain validation.
promoteProfileTX ::
  (GYTxUserQueryMonad m, MonadReader DeployedScriptsContext m) =>
  GYAssetClass ->
  GYAssetClass ->
  POSIXTime ->
  BJJBelt ->
  [GYAddress] ->
  m (GYTxSkeleton 'PlutusV3, GYAssetClass)
promoteProfileTX gyPromotedProfileId gyPromotedByProfileId achievementDate belt ownAddrs = do
  mpRef <- asks getMintingPolicyRef

  -- Get a seed TxOutRef for uniqueness (similar to profile creation)
  seedTxOutRef <- someUTxOWithoutRefScript
  let isSpendingSeedUTxO = mustHaveInput (GYTxIn seedTxOutRef GYTxInWitnessKey)
  let (V1.TxOutRef (V1.TxId bs) i) = txOutRefToPlutus seedTxOutRef
  let seedTxOutRefPlutus = V3.TxOutRef (V3.TxId bs) i

  gyMasterUserAC <- gyDeriveUserFromRefAC gyPromotedByProfileId
  spendsMasterProfileUserNFT <- txMustSpendFromAddress gyMasterUserAC ownAddrs

  -- Look up student and master profiles to get their current rank IDs
  (plutusStudentProfileDatum, _) <- getProfileStateDataAndValue gyPromotedProfileId
  let plutusStudentProfile = extra plutusStudentProfileDatum
  let studentCurrentRankId = Onchain.getCurrentRankId plutusStudentProfile
  gyStudentCurrentRankAC <- assetClassFromPlutus' studentCurrentRankId

  (plutusMasterProfileDatum, _) <- getProfileStateDataAndValue gyPromotedByProfileId
  let plutusMasterProfile = extra plutusMasterProfileDatum
  let masterCurrentRankId = Onchain.getCurrentRankId plutusMasterProfile
  gyMasterCurrentRankAC <- assetClassFromPlutus' masterCurrentRankId

  -- ============================================================
  -- Output index tracking (order must match skeleton mconcat order)
  -- ============================================================
  -- Output 0: Pending rank state locked at ranksValidator
  let pendingRankOutputIdx = 0 :: Integer

  let redeemer = Promote seedTxOutRefPlutus (assetClassToPlutus gyPromotedProfileId) (assetClassToPlutus gyPromotedByProfileId) achievementDate (beltToInt belt) pendingRankOutputIdx
  let gyRedeemer = redeemerFromPlutus' . toBuiltinData $ redeemer
  gyPromotionRankAC <- assetClassFromPlutus' $ generatePromotionRankId seedTxOutRefPlutus (mintingPolicyCurrencySymbol mintingPolicyGY)
  isMintingPromotionRank <- txMustMintWithMintRef True mpRef mintingPolicyGY gyRedeemer gyPromotionRankAC

  let pendingRankDatum =
        mkPendingRank
          (assetClassToPlutus gyPromotionRankAC)
          (assetClassToPlutus gyPromotedProfileId)
          (assetClassToPlutus gyPromotedByProfileId)
          achievementDate
          (beltToInt belt)
          defaultProtocolParams

  -- Output 0: Pending rank state
  isLockingPendingRankState <-
    txMustLockStateWithInlineDatumAndValue
      ranksValidatorGY
      pendingRankDatum
      (valueSingleton gyPromotionRankAC 1 <> valueFromLovelace datumLovelaces)

  -- Add reference inputs for student and master profiles and their current ranks
  referencesProfilesAndRanks <-
    txMustHaveUTxOsAsRefInputs
      [ gyPromotedProfileId,
        gyPromotedByProfileId,
        gyStudentCurrentRankAC,
        gyMasterCurrentRankAC
      ]

  return
    ( mconcat
        [ isSpendingSeedUTxO, -- Input (no output index)
          spendsMasterProfileUserNFT, -- Input (no output index)
          isMintingPromotionRank, -- Mint (no output index)
          isLockingPendingRankState, -- Output 0: Pending rank state
          referencesProfilesAndRanks -- Reference inputs (no output index)
        ],
      gyPromotionRankAC
    )

-- | Accept a pending promotion
-- The ProfilesValidator validates:
-- 1. Student consents (spends their user NFT)
-- 2. Promotion is still valid (nextBelt > currentBelt, dates in order)
-- 3. Profile and rank outputs are correct
-- Output indices are tracked and passed in the redeemer for efficient on-chain validation.
acceptPromotionTX ::
  (HasCallStack, GYTxUserQueryMonad m, MonadReader DeployedScriptsContext m) =>
  GYAssetClass ->
  [GYAddress] ->
  m (GYTxSkeleton 'PlutusV3, GYAssetClass)
acceptPromotionTX gyPromotionId ownAddrs = do
  rvRef <- asks getRanksValidatorRef
  pvRef <- asks getProfilesValidatorRef

  (plutusPromotionRankDatum, plutusPromotionRankValue) <- getRankStateDataAndValue gyPromotionId

  let studentProfileRefAC = promotionAwardedTo plutusPromotionRankDatum
  gyStudentProfileRefAC <- assetClassFromPlutus' studentProfileRefAC

  gyStudentProfileUserAC <- gyDeriveUserFromRefAC gyStudentProfileRefAC
  spendsStudentProfileUserNFT <- txMustSpendFromAddress gyStudentProfileUserAC ownAddrs

  -- ============================================================
  -- Output index tracking (order must match skeleton mconcat order)
  -- ============================================================
  -- Output 0: Updated profile state locked at profilesValidator
  -- Output 1: Updated rank state locked at ranksValidator
  let profileOutputIdx = 0 :: Integer
  let rankOutputIdx = 1 :: Integer

  let gySpendProfileRedeemer = redeemerFromPlutus' . toBuiltinData $ AcceptPromotion (assetClassToPlutus gyPromotionId) profileOutputIdx rankOutputIdx
  spendsStudentProfileRefNFT <- txMustSpendStateFromRefScriptWithRedeemer pvRef gyStudentProfileRefAC gySpendProfileRedeemer profilesValidatorGY

  (plutusProfileDatum, plutusProfileValue) <- getProfileStateDataAndValue gyStudentProfileRefAC
  let plutusStudentProfile = extra plutusProfileDatum
  let studentCurrentRankId = Onchain.getCurrentRankId plutusStudentProfile
  gyStudentCurrentRankAC <- assetClassFromPlutus' studentCurrentRankId

  let (plutusStudentUpdatedProfileDatum, plutuStudentUpdatedRankDatum) = promoteProfile plutusProfileDatum plutusPromotionRankDatum

  gyProfileValue <- valueFromPlutus' plutusProfileValue
  -- Output 0: Updated profile state
  isLockingUpdatedStudentProfile <- txMustLockStateWithInlineDatumAndValue profilesValidatorGY plutusStudentUpdatedProfileDatum gyProfileValue

  gyRankValue <- valueFromPlutus' plutusPromotionRankValue
  -- Output 1: Updated rank state
  isLockingUpdatedRank <- txMustLockStateWithInlineDatumAndValue ranksValidatorGY plutuStudentUpdatedRankDatum gyRankValue

  let gySpendPromotionRedeemer = redeemerFromPlutus' . toBuiltinData $ PromotionAcceptance profileOutputIdx rankOutputIdx
  spendsPromotionRank <- txMustSpendStateFromRefScriptWithRedeemer rvRef gyPromotionId gySpendPromotionRedeemer ranksValidatorGY

  -- Reference the student's current rank for acceptance-time validation
  referencesCurrentRank <- txMustHaveUTxOsAsRefInputs [gyStudentCurrentRankAC]

  gyRankAC <- assetClassFromPlutus' $ rankId plutuStudentUpdatedRankDatum

  gyLogInfo' "plutusPromotionRankDatum" $ "plutusPromotionRankDatum" <> show plutusPromotionRankDatum
  gyLogInfo' "plutuStudentUpdatedRankDatum" $ "plutuStudentUpdatedRankDatum" <> show plutuStudentUpdatedRankDatum
  return
    ( mconcat
        [ spendsStudentProfileUserNFT, -- Input (no output index)
          spendsStudentProfileRefNFT, -- Input (no output index)
          isLockingUpdatedStudentProfile, -- Output 0: Updated profile state
          spendsPromotionRank, -- Input (no output index)
          isLockingUpdatedRank, -- Output 1: Updated rank state
          referencesCurrentRank -- Reference input (no output index)
        ],
      gyRankAC
    )
