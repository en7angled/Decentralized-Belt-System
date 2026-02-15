module TxBuilding.Operations where

import Control.Monad (unless, when)
import Control.Monad.Reader.Class
import Data.Maybe
import DomainTypes.Core.Actions (AdminActionType (..))
import GHC.Stack (HasCallStack)
import GeniusYield.TxBuilder
import GeniusYield.Types
import Onchain.BJJ (BJJBelt (White), beltToInt)
import Onchain.CIP68 (ImageURI, MetadataFields, extra, mkCIP68Datum, updateCIP68DatumImage)
import Onchain.MintingPolicy
import Onchain.ProfilesValidator (ProfilesRedeemer (..))
import Onchain.Protocol (OnchainRank (..), deriveMembershipHistoriesListId, derivePromotionRankId, initEmptyMembershipHistoriesList, mkOrganizationProfile, mkPromotion, mkPractitionerProfile, promoteProfile)
import Onchain.Protocol qualified as Onchain
import Onchain.Protocol.Types (FeeConfig (..), OracleParams (..))
import Onchain.RanksValidator (RanksRedeemer (..))
import PlutusLedgerApi.V3
import TxBuilding.Context (DeployedScriptsContext (..), getMintingPolicyRef, getOracleValidatorRef, getProfilesValidatorRef, getRanksValidatorRef)
import TxBuilding.Exceptions (TxBuildingException (..))
import TxBuilding.Lookups (getProfileStateDataAndValue, getRankStateDataAndValue, queryOracleParams)
import TxBuilding.Skeletons
import TxBuilding.Utils (txOutRefToV3Plutus)
import TxBuilding.Validators

-- | Get the compiled minting policy from the oracle NFT in context.
getMintingPolicyFromCtx :: DeployedScriptsContext -> GYScript 'PlutusV3
getMintingPolicyFromCtx ctx =
  compileMintingPolicy (assetClassToPlutus $ oracleNFTAssetClass ctx)

-- | Get the 'ProtocolParams' from the oracle NFT in context.
getProtocolParamsFromCtx :: DeployedScriptsContext -> Onchain.ProtocolParams
getProtocolParamsFromCtx ctx =
  mkProtocolParams (assetClassToPlutus $ oracleNFTAssetClass ctx)

-- | Build a skeleton component that adds the oracle UTxO as a reference input.
-- Also checks whether the protocol is paused and throws 'ProtocolPaused' if so.
-- Admin operations ('updateOracleTX') bypass this function and remain unaffected.
getOracleRefInputSkeleton ::
  (GYTxQueryMonad m, MonadReader DeployedScriptsContext m) =>
  m (GYTxSkeleton 'PlutusV3, OracleParams)
getOracleRefInputSkeleton = do
  (oracleParams, oracleRef, _oracleValue) <- queryOracleParams
  when (opPaused oracleParams) $
    throwError (GYApplicationException ProtocolPaused)
  return (mustHaveRefInput oracleRef, oracleParams)

-- | Build a skeleton component that pays a fee if fees are configured.
getFeeSkeleton ::
  (GYTxQueryMonad m) =>
  OracleParams ->
  (FeeConfig -> Integer) ->
  m (GYTxSkeleton 'PlutusV3)
getFeeSkeleton oracle feeSelector = case opFeeConfig oracle of
  Nothing -> return mempty
  Just feeConfig -> do
    let feeAmount = feeSelector feeConfig
    let plutusFeeAddr = fcFeeAddress feeConfig
    gyFeeAddr <- addressFromPlutus' plutusFeeAddr
    txIsPayingValueToAddress gyFeeAddr (valueFromLovelace feeAmount)

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
  ovRef <- asks getOracleValidatorRef

  mintingPolicyUTxO <- utxoAtTxOutRef mpRef
  let mintingPolicyHasRefScript = utxoRefScript =<< mintingPolicyUTxO

  profilesValidatorUTxO <- utxoAtTxOutRef pvRef
  let profilesValidatorHasRefScript = utxoRefScript =<< profilesValidatorUTxO

  ranksValidatorUTxO <- utxoAtTxOutRef rvRef
  let ranksValidatorHasRefScript = utxoRefScript =<< ranksValidatorUTxO

  oracleValidatorUTxO <- utxoAtTxOutRef ovRef
  let oracleValidatorHasRefScript = utxoRefScript =<< oracleValidatorUTxO

  return $ all isJust [mintingPolicyHasRefScript, profilesValidatorHasRefScript, ranksValidatorHasRefScript, oracleValidatorHasRefScript]

-- | Like 'verifyDeployedScriptsAreReady' but throws 'DeployedScriptsNotReady' when scripts are not ready.
ensureDeployedScriptsAreReady ::
  (GYTxQueryMonad m, MonadReader DeployedScriptsContext m, MonadError GYTxMonadException m) =>
  m ()
ensureDeployedScriptsAreReady = do
  ready <- verifyDeployedScriptsAreReady
  unless ready $ throwError (GYApplicationException DeployedScriptsNotReady)

------------------------------------------------------------------------------------------------

-- * OnChainProfile Operations

------------------------------------------------------------------------------------------------

-- | Create OnChainProfile Transaction Skeleton
createProfileTX ::
  (GYTxUserQueryMonad m, MonadReader DeployedScriptsContext m) =>
  GYAddress ->
  MetadataFields ->
  Onchain.OnchainProfileType ->
  POSIXTime ->
  m (GYTxSkeleton 'PlutusV3, GYAssetClass)
createProfileTX recipient metadata profileType creationDate = createProfileWithRankTX recipient metadata profileType creationDate White

-- | Create OnChainProfile Transaction Skeleton
-- Output indices are tracked and passed in the redeemer for efficient on-chain validation.
createProfileWithRankTX ::
  (GYTxUserQueryMonad m, MonadReader DeployedScriptsContext m) =>
  GYAddress ->
  MetadataFields ->
  Onchain.OnchainProfileType ->
  POSIXTime ->
  BJJBelt ->
  m (GYTxSkeleton 'PlutusV3, GYAssetClass)
createProfileWithRankTX recipient metadata profileType creationDate belt = do
  mpGY <- asks getMintingPolicyFromCtx
  pp <- asks getProtocolParamsFromCtx
  now <- slotOfCurrentBlock
  let isInvalidBeforeNow = isInvalidBefore now

  -- Oracle reference input + params (for minLovelace and fee)
  (oracleRefSkeleton, oracleParams) <- getOracleRefInputSkeleton
  let minLv = opMinOutputLovelace oracleParams
  feeSkeleton <- getFeeSkeleton oracleParams fcProfileCreationFee

  mpRef <- asks getMintingPolicyRef
  seedTxOutRef <- someUTxOWithoutRefScript
  let isSpendingSeedUTxO = mustHaveInput (GYTxIn seedTxOutRef GYTxInWitnessKey)
  let seedTxOutRefPlutus = txOutRefToV3Plutus seedTxOutRef

  (gyProfileRefAC, gyProfileUserAC) <- gyGenerateRefAndUserAC mpGY seedTxOutRef
  gyLogDebug' "gyProfileRefAC: " $ show gyProfileRefAC

  let profileRefAC = assetClassToPlutus gyProfileRefAC

  let plutusProfile = case profileType of
        Onchain.Practitioner -> fst $ mkPractitionerProfile profileRefAC creationDate pp (beltToInt belt)
        Onchain.Organization -> mkOrganizationProfile profileRefAC pp
  let plutusProfileCIP68Datum = mkCIP68Datum plutusProfile metadata

  -- ============================================================
  -- Output index tracking (order must match skeleton mconcat order)
  -- ============================================================
  -- Output 0: Profile state locked at profilesValidator
  -- Output 1: User NFT payment to recipient
  -- Output 2: Rank state locked at ranksValidator (Practitioner only)
  -- Output 3 (optional): Fee payment
  let profileOutputIdx = 0 :: Integer
  let rankOrMembershipHistoriesRootOutputIdx = 2 :: Integer
  let redeemer = CreateProfile seedTxOutRefPlutus metadata profileType creationDate (beltToInt belt) profileOutputIdx rankOrMembershipHistoriesRootOutputIdx
  let gyCreateProfileRedeemer = redeemerFromPlutusData redeemer

  isMintingProfileCIP68UserAndRef <- txMustMintCIP68UserAndRef mpRef mpGY gyCreateProfileRedeemer gyProfileRefAC

  -- Output 0: Profile state
  isLockingProfileState <-
    txMustLockStateWithInlineDatumAndValue
      profilesValidatorGY
      plutusProfileCIP68Datum
      (valueSingleton gyProfileRefAC 1 <> valueFromLovelace minLv)

  -- Output 1: User NFT payment
  isPayingProfileUserNFT <- txIsPayingValueToAddress recipient (valueSingleton gyProfileUserAC 1)

  -- Output 2: Rank state (Practitioner only) or Membership Histories Root NFT (Organization only)

  isLockingRankOrMembershipHistoriesRootState <- case profileType of
    Onchain.Organization -> do
      let membershipHistoriesRootDatum = initEmptyMembershipHistoriesList profileRefAC
      gyMembershipHistoriesRootAC <- assetClassFromPlutus' $ deriveMembershipHistoriesListId profileRefAC
      isMintingMembershipHistoriesRoot <- txMustMintWithMintRef True mpRef mpGY gyCreateProfileRedeemer gyMembershipHistoriesRootAC
      isLockingMembershipHistoriesRootState <-
        txMustLockStateWithInlineDatumAndValue
          membershipsValidatorGY
          membershipHistoriesRootDatum
          (valueSingleton gyMembershipHistoriesRootAC 1 <> valueFromLovelace minLv)
      return $
        mconcat
          [ isMintingMembershipHistoriesRoot,
            isLockingMembershipHistoriesRootState -- Output 2: Membership Histories Root NFT
          ]
    Onchain.Practitioner -> do
      let rankData = snd $ mkPractitionerProfile profileRefAC creationDate pp (beltToInt belt)
      gyRankAC <- assetClassFromPlutus' $ rankId rankData
      isMintingRank <- txMustMintWithMintRef True mpRef mpGY gyCreateProfileRedeemer gyRankAC
      isLockingRankState <-
        txMustLockStateWithInlineDatumAndValue
          ranksValidatorGY
          rankData
          (valueSingleton gyRankAC 1 <> valueFromLovelace minLv)
      return $
        mconcat
          [ isMintingRank,
            isLockingRankState -- Output 2
          ]

  return
    ( mconcat
        [ isSpendingSeedUTxO, -- Input (no output index)
          isInvalidBeforeNow, -- Validity (no output index)
          oracleRefSkeleton, -- Reference input: oracle UTxO
          isMintingProfileCIP68UserAndRef, -- Mint (no output index)
          isLockingProfileState, -- Output 0: Profile state
          isPayingProfileUserNFT, -- Output 1: User NFT payment
          isLockingRankOrMembershipHistoriesRootState, -- Output 2: Rank state (if Practitioner) or Membership Histories Root NFT (if Organization)
          feeSkeleton -- Output 3 (optional): Fee payment
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
  let gyRedeemer = redeemerFromPlutusData updateRedeemer
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
  mpGY <- asks getMintingPolicyFromCtx
  pp <- asks getProtocolParamsFromCtx

  -- Oracle reference input + params (for minLovelace and fee)
  (oracleRefSkeleton, oracleParams) <- getOracleRefInputSkeleton
  let minLv = opMinOutputLovelace oracleParams
  feeSkeleton <- getFeeSkeleton oracleParams fcPromotionFee

  mpRef <- asks getMintingPolicyRef

  -- Get a seed TxOutRef for uniqueness (similar to profile creation)
  seedTxOutRef <- someUTxOWithoutRefScript
  let isSpendingSeedUTxO = mustHaveInput (GYTxIn seedTxOutRef GYTxInWitnessKey)
  let seedTxOutRefPlutus = txOutRefToV3Plutus seedTxOutRef

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
  -- Output 1 (optional): Fee payment
  let pendingRankOutputIdx = 0 :: Integer

  let redeemer = Promote seedTxOutRefPlutus (assetClassToPlutus gyPromotedProfileId) (assetClassToPlutus gyPromotedByProfileId) achievementDate (beltToInt belt) pendingRankOutputIdx
  let gyRedeemer = redeemerFromPlutusData redeemer
  gyPromotionRankAC <- assetClassFromPlutus' $ derivePromotionRankId seedTxOutRefPlutus (mintingPolicyCurrencySymbol mpGY)
  isMintingPromotionRank <- txMustMintWithMintRef True mpRef mpGY gyRedeemer gyPromotionRankAC

  let pendingRankDatum =
        mkPromotion
          (assetClassToPlutus gyPromotionRankAC)
          (assetClassToPlutus gyPromotedProfileId)
          (assetClassToPlutus gyPromotedByProfileId)
          achievementDate
          (beltToInt belt)
          pp

  -- Output 0: Pending rank state
  isLockingPendingRankState <-
    txMustLockStateWithInlineDatumAndValue
      ranksValidatorGY
      pendingRankDatum
      (valueSingleton gyPromotionRankAC 1 <> valueFromLovelace minLv)

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
          oracleRefSkeleton, -- Reference input: oracle UTxO
          isMintingPromotionRank, -- Mint (no output index)
          isLockingPendingRankState, -- Output 0: Pending rank state
          referencesProfilesAndRanks, -- Reference inputs (no output index)
          feeSkeleton -- Output 1 (optional): Fee payment
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
  let rankOutputIdx = 1 :: Integer -- Used by RanksValidator redeemer (PV no longer needs this — R2 removed)

  let gySpendProfileRedeemer = redeemerFromPlutusData $ AcceptPromotion (assetClassToPlutus gyPromotionId) profileOutputIdx
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

  let gySpendPromotionRedeemer = redeemerFromPlutusData $ PromotionAcceptance profileOutputIdx rankOutputIdx
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

------------------------------------------------------------------------------------------------

-- * Oracle Management Operations

------------------------------------------------------------------------------------------------

-- | Build a transaction skeleton that updates oracle parameters based on an admin action.
-- Queries the current oracle params, applies the action delta, and builds spend + re-lock skeletons.
updateOracleTX ::
  (HasCallStack, GYTxQueryMonad m, MonadReader DeployedScriptsContext m) =>
  AdminActionType ->
  m (GYTxSkeleton 'PlutusV3)
updateOracleTX adminAction = do
  ovRef <- asks getOracleValidatorRef
  (currentParams, oracleRef, oracleValue) <- queryOracleParams
  let gyRedeemer = redeemerFromPlutusData ()

  -- Apply the action to derive the new oracle params
  let newParams = applyAdminAction adminAction currentParams

  -- Convert admin PKH from Plutus to GY for required signer
  gyAdminPkh <- pubKeyHashFromPlutus' (opAdminPkh currentParams)

  -- Spend the current oracle UTxO
  let currentDatum = datumFromPlutusData currentParams
  let spendOracle = txMustSpendFromRefScriptWithKnownDatum ovRef oracleRef currentDatum gyRedeemer oracleValidatorGY

  -- Re-lock oracle with new datum and same value
  lockOracle <- txMustLockStateWithInlineDatumAndValue oracleValidatorGY newParams oracleValue

  -- Require admin signature (on-chain validator checks txInfoSignatories)
  let requireAdminSig = mustBeSignedBy gyAdminPkh

  return $ mconcat [spendOracle, lockOracle, requireAdminSig]

-- | Apply an admin action delta to current oracle params to produce new params.
applyAdminAction :: AdminActionType -> OracleParams -> OracleParams
applyAdminAction PauseProtocolAction params = params {opPaused = True}
applyAdminAction UnpauseProtocolAction params = params {opPaused = False}
applyAdminAction (SetFeesAction mFeeConfig) params = params {opFeeConfig = mFeeConfig}
applyAdminAction (SetMinLovelaceAction minLv) params = params {opMinOutputLovelace = minLv}
