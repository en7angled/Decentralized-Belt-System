-- | Transaction skeleton building: profiles, ranks, memberships, achievements, oracle admin, dust cleanup.
module TxBuilding.Operations where

import Control.Monad (unless, when)
import Control.Monad.Reader.Class (MonadReader, ask, asks)
import Data.Maybe
import DomainTypes.Core.Actions (AdminActionType (..))
import GHC.Stack (HasCallStack)
import GeniusYield.TxBuilder
import GeniusYield.Types
import Onchain.BJJ (BJJBelt (White), beltToInt)
import Onchain.CIP68 (CIP68Datum (..), ImageURI, MetadataFields, extra, mkCIP68Datum, updateCIP68DatumImage)
import Onchain.LinkedList (NodeDatum (..))
import Onchain.Protocol qualified as Onchain
import Onchain.Protocol.Types qualified as OnchainTypes
import Onchain.Utils (protocolMinLovelace)
import Onchain.Validators.AchievementsValidator (AchievementsRedeemer (..))
import Onchain.Validators.MembershipsValidator (MembershipsRedeemer (..))
import Onchain.Validators.MintingPolicy
import Onchain.Validators.OracleValidator (OracleRedeemer (OracleUpdate))
import Onchain.Validators.ProfilesValidator (ProfilesRedeemer (AcceptPromotion, UpdateProfileImage))
import Onchain.Validators.ProfilesValidator qualified (ProfilesRedeemer (Cleanup))
import Onchain.Validators.RanksValidator (RanksRedeemer (PromotionAcceptance))
import Onchain.Validators.RanksValidator qualified (RanksRedeemer (Cleanup))
import PlutusLedgerApi.V3
import TxBuilding.Context (DeployedScriptsContext (..), getAchievementsValidatorRef, getMembershipsValidatorRef, getMintingPolicyFromCtx, getMintingPolicyRef, getOracleValidatorRef, getProfilesValidatorRef, getProtocolParamsFromCtx, getRanksValidatorRef)
import TxBuilding.Exceptions (AddMembershipIntervalReason (..), TxBuildingException (..))
import TxBuilding.Lookups (findInsertPointForNewMembership, getAchievementDatumAndValue, getDustUTxOs, getMembershipIntervalDatumAndValue, getMembershipListNodeDatumAndValue, getProfileStateDatumAndValue, getRankStateDatumAndValue, getUTxOWithTokenAtAddresses, queryOracleParams)
import TxBuilding.Skeletons
import TxBuilding.Utils (gySlotFromPOSIXTime, pPOSIXTimeFromGYSlot, txOutRefToV3Plutus)
import TxBuilding.Validators

-- | Build a skeleton component that adds the oracle UTxO as a reference input.
-- Also checks whether the protocol is paused and throws 'ProtocolPaused' if so.
-- Admin operations ('updateOracleTX') bypass this function and remain unaffected.
getOracleRefInputSkeleton ::
  (GYTxQueryMonad m, MonadReader DeployedScriptsContext m) =>
  m (GYTxSkeleton 'PlutusV3, OnchainTypes.OracleParams)
getOracleRefInputSkeleton = do
  (oracleParams, oracleRef, _oracleValue) <- queryOracleParams
  when (OnchainTypes.opPaused oracleParams) $
    throwError (GYApplicationException ProtocolPaused)
  return (mustHaveRefInput oracleRef, oracleParams)

-- | Build a skeleton component that pays a fee if fees are configured.
getFeeSkeleton ::
  (GYTxQueryMonad m) =>
  OnchainTypes.OracleParams ->
  (OnchainTypes.FeeConfig -> Integer) ->
  m (GYTxSkeleton 'PlutusV3)
getFeeSkeleton oracle feeSelector = case OnchainTypes.opFeeConfig oracle of
  Nothing -> return mempty
  Just feeConfig -> do
    let feeAmount = feeSelector feeConfig
    let plutusFeeAddr = OnchainTypes.fcFeeAddress feeConfig
    gyFeeAddr <- addressFromPlutus' plutusFeeAddr
    txMustPayValueToAddress gyFeeAddr (valueFromLovelace feeAmount)

-- | Oracle reference input, params, minLovelace, fee skeleton, and validity (invalid-before-now).
-- Use when building TXs that require oracle, fee, and a validity window.
getOracleFeeAndValiditySkeleton ::
  (GYTxQueryMonad m, MonadReader DeployedScriptsContext m) =>
  (OnchainTypes.FeeConfig -> Integer) ->
  m (GYTxSkeleton 'PlutusV3, OnchainTypes.OracleParams, Integer, GYTxSkeleton 'PlutusV3, GYTxSkeleton 'PlutusV3)
getOracleFeeAndValiditySkeleton feeSelector = do
  (oracleRefSkeleton, oracleParams) <- getOracleRefInputSkeleton
  let minLv = protocolMinLovelace
  feeSkeleton <- getFeeSkeleton oracleParams feeSelector
  now <- slotOfCurrentBlock
  let isValidForSafeEra = txIsValidForSafeEra now
  return (oracleRefSkeleton, oracleParams, minLv, feeSkeleton, isValidForSafeEra)

-- | Spend the User NFT for a profile (Ref AC) from the given addresses.
spendUserNFTForProfile ::
  (GYTxUserQueryMonad m, MonadReader DeployedScriptsContext m) =>
  GYAssetClass ->
  [GYAddress] ->
  m (GYTxSkeleton 'PlutusV3)
spendUserNFTForProfile profileRefAC ownAddrs = do
  gyUserAC <- gyDeriveUserFromRefAC profileRefAC
  txMustSpendFromAddress gyUserAC ownAddrs

------------------------------------------------------------------------------------------------

-- * Verify If Deployed Scripts Are Ready

------------------------------------------------------------------------------------------------

refGetters :: DeployedScriptsContext -> [GYTxOutRef]
refGetters ctx =
  [ getMintingPolicyRef ctx,
    getProfilesValidatorRef ctx,
    getRanksValidatorRef ctx,
    getMembershipsValidatorRef ctx,
    getAchievementsValidatorRef ctx,
    getOracleValidatorRef ctx
  ]

verifyDeployedScriptsAreReady ::
  (GYTxQueryMonad m, MonadReader DeployedScriptsContext m) =>
  m Bool
verifyDeployedScriptsAreReady = do
  ctx <- ask
  hasRefScripts <- mapM (fmap (utxoRefScript =<<) . utxoAtTxOutRef) (refGetters ctx)
  return $ all isJust hasRefScripts

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
  (oracleRefSkeleton, _oracleParams, minLv, feeSkeleton, isInvalidBeforeNow) <- getOracleFeeAndValiditySkeleton OnchainTypes.fcProfileCreationFee

  mpRef <- asks getMintingPolicyRef
  seedTxOutRef <- someUTxOWithoutRefScript
  let isSpendingSeedUTxO = mustHaveInput (GYTxIn seedTxOutRef GYTxInWitnessKey)
  let seedTxOutRefPlutus = txOutRefToV3Plutus seedTxOutRef

  (gyProfileRefAC, gyProfileUserAC) <- gyGenerateRefAndUserAC mpGY seedTxOutRef
  gyLogDebug' "gyProfileRefAC: " $ show gyProfileRefAC

  let profileRefAC = assetClassToPlutus gyProfileRefAC

  let plutusProfile = case profileType of
        Onchain.Practitioner -> fst $ Onchain.mkPractitionerProfile profileRefAC creationDate pp (beltToInt belt)
        Onchain.Organization -> Onchain.mkOrganizationProfile profileRefAC pp
  let plutusProfileCIP68Datum = mkCIP68Datum plutusProfile metadata []

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
  isPayingProfileUserNFT <- txMustPayValueToAddress recipient (valueSingleton gyProfileUserAC 1)

  -- Output 2: Rank state (Practitioner only) or Membership Histories Root NFT (Organization only)

  isLockingRankOrMembershipHistoriesRootState <- case profileType of
    Onchain.Organization -> do
      let membershipHistoriesRootDatum = Onchain.initEmptyMembershipHistoriesList profileRefAC
      gyMembershipHistoriesRootAC <- assetClassFromPlutus' $ Onchain.deriveMembershipHistoriesListId profileRefAC
      isMintingMembershipHistoriesRoot <- txMustMintWithMintRef True mpRef mpGY gyCreateProfileRedeemer gyMembershipHistoriesRootAC
      isLockingMembershipHistoriesRootState <-
        txMustLockStateWithInlineDatumAndValue
          membershipsValidatorGY
          (OnchainTypes.ListNodeDatum membershipHistoriesRootDatum) -- Must wrap in MembershipDatum for on-chain validation
          (valueSingleton gyMembershipHistoriesRootAC 1 <> valueFromLovelace minLv)
      return $
        mconcat
          [ isMintingMembershipHistoriesRoot,
            isLockingMembershipHistoriesRootState -- Output 2: Membership Histories Root NFT
          ]
    Onchain.Practitioner -> do
      let rankData = snd $ Onchain.mkPractitionerProfile profileRefAC creationDate pp (beltToInt belt)
      gyRankAC <- assetClassFromPlutus' $ Onchain.rankId rankData
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

  (plutusProfileDatum, plutusProfileValue) <- getProfileStateDatumAndValue gyProfileRefAC
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

  (oracleRefSkeleton, _oracleParams, minLv, feeSkeleton, _isInvalidBeforeNow) <- getOracleFeeAndValiditySkeleton OnchainTypes.fcPromotionFee

  mpRef <- asks getMintingPolicyRef

  -- Get a seed TxOutRef for uniqueness (similar to profile creation)
  seedTxOutRef <- someUTxOWithoutRefScript
  let isSpendingSeedUTxO = mustHaveInput (GYTxIn seedTxOutRef GYTxInWitnessKey)
  let seedTxOutRefPlutus = txOutRefToV3Plutus seedTxOutRef

  spendsMasterProfileUserNFT <- spendUserNFTForProfile gyPromotedByProfileId ownAddrs

  -- Look up student and master profiles to get their current rank IDs
  (plutusStudentProfileDatum, _) <- getProfileStateDatumAndValue gyPromotedProfileId
  let plutusStudentProfile = extra plutusStudentProfileDatum
  let studentCurrentRankId = Onchain.getCurrentRankId plutusStudentProfile
  gyStudentCurrentRankAC <- assetClassFromPlutus' studentCurrentRankId

  (plutusMasterProfileDatum, _) <- getProfileStateDatumAndValue gyPromotedByProfileId
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
  gyPromotionRankAC <- assetClassFromPlutus' $ Onchain.derivePromotionRankId seedTxOutRefPlutus (mintingPolicyCurrencySymbol mpGY)
  isMintingPromotionRank <- txMustMintWithMintRef True mpRef mpGY gyRedeemer gyPromotionRankAC

  let pendingRankDatum =
        Onchain.mkPromotion
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

  (plutusPromotionRankDatum, plutusPromotionRankValue) <- getRankStateDatumAndValue gyPromotionId

  let studentProfileRefAC = Onchain.promotionAwardedTo plutusPromotionRankDatum
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

  (plutusProfileDatum, plutusProfileValue) <- getProfileStateDatumAndValue gyStudentProfileRefAC
  let plutusStudentProfile = extra plutusProfileDatum
  let studentCurrentRankId = Onchain.getCurrentRankId plutusStudentProfile
  gyStudentCurrentRankAC <- assetClassFromPlutus' studentCurrentRankId

  let (plutusStudentUpdatedProfileDatum, plutusStudentUpdatedRankDatum) = Onchain.promoteProfile plutusProfileDatum plutusPromotionRankDatum

  gyProfileValue <- valueFromPlutus' plutusProfileValue
  -- Output 0: Updated profile state
  isLockingUpdatedStudentProfile <- txMustLockStateWithInlineDatumAndValue profilesValidatorGY plutusStudentUpdatedProfileDatum gyProfileValue

  gyRankValue <- valueFromPlutus' plutusPromotionRankValue
  -- Output 1: Updated rank state
  isLockingUpdatedRank <- txMustLockStateWithInlineDatumAndValue ranksValidatorGY plutusStudentUpdatedRankDatum gyRankValue

  let gySpendPromotionRedeemer = redeemerFromPlutusData $ PromotionAcceptance profileOutputIdx rankOutputIdx
  spendsPromotionRank <- txMustSpendStateFromRefScriptWithRedeemer rvRef gyPromotionId gySpendPromotionRedeemer ranksValidatorGY

  -- Reference the student's current rank for acceptance-time validation
  referencesCurrentRank <- txMustHaveUTxOsAsRefInputs [gyStudentCurrentRankAC]

  gyRankAC <- assetClassFromPlutus' $ Onchain.rankId plutusStudentUpdatedRankDatum

  gyLogInfo' "plutusPromotionRankDatum" $ "plutusPromotionRankDatum" <> show plutusPromotionRankDatum
  gyLogInfo' "plutusStudentUpdatedRankDatum" $ "plutusStudentUpdatedRankDatum" <> show plutusStudentUpdatedRankDatum
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

-- * Membership Operations

------------------------------------------------------------------------------------------------

-- | Create a new membership history for a practitioner at an organization.
-- This appends a new node to the organization's membership histories linked list.
-- Corresponds to 'NewMembershipHistory' minting + 'InsertNodeToMHList' spending.
--
-- Output index layout:
--   Output 0: Updated left node (membership histories list node)
--   Output 1: Inserted new node (membership histories list node)
--   Output 2 (from minting policy): First interval locked at memberships validator
--   Output 3 (optional): Fee payment
createMembershipHistoryTX ::
  (GYTxUserQueryMonad m, MonadReader DeployedScriptsContext m) =>
  -- | Organization profile Ref AC
  GYAssetClass ->
  -- | Practitioner profile Ref AC
  GYAssetClass ->
  -- | Start date
  POSIXTime ->
  -- | Optional end date
  Maybe POSIXTime ->
  -- | Organization's wallet addresses (to spend User NFT)
  [GYAddress] ->
  m (GYTxSkeleton 'PlutusV3, GYAssetClass)
createMembershipHistoryTX gyOrgProfileRefAC gyPractitionerProfileRefAC startDate mEndDate ownAddrs = do
  mpGY <- asks getMintingPolicyFromCtx
  mpRef <- asks getMintingPolicyRef
  mvRef <- asks getMembershipsValidatorRef
  (oracleRefSkeleton, _oracleParams, minLv, feeSkeleton, isInvalidBeforeNow) <- getOracleFeeAndValiditySkeleton OnchainTypes.fcMembershipHistoryFee

  spendsOrgUserNFT <- spendUserNFTForProfile gyOrgProfileRefAC ownAddrs

  -- List is sorted by node key (practitioner id). Find insert point: (left node to spend, optional right node to reference).
  let plutusOrgRefAC = assetClassToPlutus gyOrgProfileRefAC
  let plutusPractitionerRefAC = assetClassToPlutus gyPractitionerProfileRefAC
  (leftNodeAC, maybeRightNodeAC) <- findInsertPointForNewMembership gyOrgProfileRefAC gyPractitionerProfileRefAC
  (leftNode, leftNodeValue) <- getMembershipListNodeDatumAndValue leftNodeAC

  -- Create the new membership history and first interval
  let (newHistory, fstInterval) = Onchain.initMembershipHistory plutusPractitionerRefAC plutusOrgRefAC startDate mEndDate

  -- Build newNode and updatedLeftNode, and redeemer fields for append vs insert
  (newNode, updatedLeftNode, maybeRightNodeIdPlutus) <- case maybeRightNodeAC of
    Nothing -> do
      -- Append: new node is last; left's next becomes the new node's key
      let newNode' = Onchain.mkMembershipHistoriesListNode newHistory Nothing
      let updatedLeftNode' = Onchain.appendMembershipHistory (leftNode, newNode')
      return (newNode', updatedLeftNode', Nothing)
    Just rightNodeAC -> do
      -- Insert: new node goes between left and right; new node's next = right's key
      (rightNode, _) <- getMembershipListNodeDatumAndValue rightNodeAC
      let rightKey = nodeKey (OnchainTypes.nodeInfo rightNode)
      let newNode' = Onchain.mkMembershipHistoriesListNode newHistory rightKey
      let updatedLeftNode' = Onchain.insertMembershipHistoryInBetween (leftNode, rightNode, newNode')
      return (newNode', updatedLeftNode', Just (assetClassToPlutus rightNodeAC))

  -- Derive GY asset classes for the newly created tokens
  let historyId = Onchain.deriveMembershipHistoryIdFromHistory newHistory
      fstIntervalId = Onchain.deriveIntervalsHeadId newHistory
  gyMembershipHistoryAC <- assetClassFromPlutus' historyId
  gyFirstIntervalAC <- assetClassFromPlutus' fstIntervalId

  let updatedLeftNodeTxOutIdx = 0 :: Integer
  let insertedNodeTxOutIdx = 1 :: Integer
  let firstIntervalOutputIdx = 2 :: Integer

  let mvRedeemer =
        redeemerFromPlutusData $
          InsertNodeToMHList
            { maybeRightNodeId = maybeRightNodeIdPlutus,
              insertedMembershipHistory = newHistory,
              updatedLeftNodeTxOutIdx = updatedLeftNodeTxOutIdx,
              insertedNodeTxOutIdx = insertedNodeTxOutIdx
            }

  let mintRedeemer =
        redeemerFromPlutusData $
          NewMembershipHistory plutusOrgRefAC plutusPractitionerRefAC startDate mEndDate (assetClassToPlutus leftNodeAC) firstIntervalOutputIdx

  spendsLeftNode <- txMustSpendStateFromRefScriptWithRedeemer mvRef leftNodeAC mvRedeemer membershipsValidatorGY

  -- When inserting, the right node must be a reference input (validator reads it)
  refRightNodeSkeleton <- case maybeRightNodeAC of
    Nothing -> return mempty
    Just rightAC -> txMustHaveUTxOsAsRefInputs [rightAC]

  isMintingHistoryNFT <- txMustMintWithMintRef True mpRef mpGY mintRedeemer gyMembershipHistoryAC
  isMintingIntervalNFT <- txMustMintWithMintRef True mpRef mpGY mintRedeemer gyFirstIntervalAC

  gyLeftNodeValue <- valueFromPlutus' leftNodeValue
  isLockingUpdatedLeftNode <-
    txMustLockStateWithInlineDatumAndValue
      membershipsValidatorGY
      (OnchainTypes.ListNodeDatum updatedLeftNode)
      gyLeftNodeValue

  isLockingInsertedNode <-
    txMustLockStateWithInlineDatumAndValue
      membershipsValidatorGY
      (OnchainTypes.ListNodeDatum newNode)
      (valueSingleton gyMembershipHistoryAC 1 <> valueFromLovelace minLv)

  isLockingFirstInterval <-
    txMustLockStateWithInlineDatumAndValue
      membershipsValidatorGY
      (OnchainTypes.IntervalDatum fstInterval)
      (valueSingleton gyFirstIntervalAC 1 <> valueFromLovelace minLv)

  return
    ( mconcat
        [ spendsOrgUserNFT,
          spendsLeftNode,
          isInvalidBeforeNow,
          oracleRefSkeleton,
          refRightNodeSkeleton,
          isMintingHistoryNFT,
          isMintingIntervalNFT,
          isLockingUpdatedLeftNode,
          isLockingInsertedNode,
          isLockingFirstInterval,
          feeSkeleton
        ],
      gyMembershipHistoryAC
    )

-- | Add a new membership interval to an existing membership history.
-- Corresponds to 'NewMembershipInterval' minting + 'UpdateNodeInMHList' spending.
--
-- Output index layout:
--   Output 0: Updated membership history node
--   Output 1 (from minting policy): New interval locked at memberships validator
--   Output 2 (optional): Fee payment
addMembershipIntervalTX ::
  (GYTxUserQueryMonad m, MonadReader DeployedScriptsContext m) =>
  -- | Organization profile Ref AC
  GYAssetClass ->
  -- | Membership history node NFT AC
  GYAssetClass ->
  -- | Start date
  POSIXTime ->
  -- | Optional end date
  Maybe POSIXTime ->
  -- | Organization's wallet addresses (to spend User NFT)
  [GYAddress] ->
  m (GYTxSkeleton 'PlutusV3, GYAssetClass)
addMembershipIntervalTX gyOrgProfileRefAC gyMembershipNodeAC startDate mEndDate ownAddrs = do
  mpGY <- asks getMintingPolicyFromCtx
  mpRef <- asks getMintingPolicyRef
  mvRef <- asks getMembershipsValidatorRef
  (oracleRefSkeleton, _oracleParams, minLv, feeSkeleton, isInvalidBeforeNow) <- getOracleFeeAndValiditySkeleton OnchainTypes.fcMembershipHistoryFee

  spendsOrgUserNFT <- spendUserNFTForProfile gyOrgProfileRefAC ownAddrs

  -- Look up the membership history node
  (historyNode, historyNodeValue) <- getMembershipListNodeDatumAndValue gyMembershipNodeAC
  currentHistory <- case nodeData (OnchainTypes.nodeInfo historyNode) of
    Just h -> return h
    Nothing -> throwError (GYApplicationException MembershipRootNodeHasNoHistory)

  -- Look up the last interval (head of intervals chain) for reference input
  let lastIntervalAC = Onchain.deriveIntervalsHeadId currentHistory
  gyLastIntervalAC <- assetClassFromPlutus' lastIntervalAC
  (lastInterval, _lastIntervalValue) <- getMembershipIntervalDatumAndValue gyLastIntervalAC

  -- Pre-validate add-interval conditions (mirror on-chain validation) so we throw domain exceptions instead of Plutus traceError
  unless (OnchainTypes.membershipHistoryIntervalsHeadNumber currentHistory == OnchainTypes.membershipIntervalNumber lastInterval) $
    throwError (GYApplicationException (CannotAddMembershipInterval HeadNumberMismatch))
  case OnchainTypes.membershipIntervalEndDate lastInterval of
    Just lastEnd -> unless (startDate >= lastEnd) $ throwError (GYApplicationException (CannotAddMembershipInterval LastIntervalNotClosed))
    Nothing -> throwError (GYApplicationException (CannotAddMembershipInterval LastIntervalNotClosed))
  unless (OnchainTypes.membershipIntervalIsAck lastInterval) $
    throwError (GYApplicationException (CannotAddMembershipInterval LastIntervalNotAccepted))
  case mEndDate of
    Just ed -> unless (ed > startDate) $ throwError (GYApplicationException (CannotAddMembershipInterval InvalidNewIntervalEndDate))
    Nothing -> return ()

  -- Compute updated history and new interval using Protocol functions
  let (updatedHistory, newInterval) = Onchain.addMembershipIntervalToHistory currentHistory lastInterval startDate mEndDate
  let updatedHistoryNode = Onchain.updateNodeMembershipHistory historyNode updatedHistory

  -- Derive GY asset class for the new interval
  let historyId = Onchain.deriveMembershipHistoryIdFromHistory currentHistory
      newIntervalId = Onchain.deriveMembershipIntervalId historyId (OnchainTypes.membershipIntervalNumber newInterval)
  gyNewIntervalAC <- assetClassFromPlutus' newIntervalId

  -- ============================================================
  -- Output index tracking (order must match skeleton mconcat order)
  -- ============================================================
  -- Output 0: Updated membership history node
  -- Output 1 (from minting policy): New interval
  let updatedNodeTxOutIdx = 0 :: Integer
  let intervalOutputIdx = 1 :: Integer

  -- Build MembershipsValidator redeemer for spending the history node
  let mvRedeemer =
        redeemerFromPlutusData $
          UpdateNodeInMHList
            { lastIntervalId = lastIntervalAC,
              startDate = startDate,
              endDate = mEndDate,
              updatedNodeTxOutIdx = updatedNodeTxOutIdx
            }

  -- Build MintingPolicy redeemer
  let plutusOrgRefAC = assetClassToPlutus gyOrgProfileRefAC
  let mintRedeemer =
        redeemerFromPlutusData $
          NewMembershipInterval plutusOrgRefAC (assetClassToPlutus gyMembershipNodeAC) startDate mEndDate intervalOutputIdx

  -- Spend the membership history node from memberships validator
  spendsHistoryNode <- txMustSpendStateFromRefScriptWithRedeemer mvRef gyMembershipNodeAC mvRedeemer membershipsValidatorGY

  -- Add last interval as reference input (for validation)
  referencesLastInterval <- txMustHaveUTxOsAsRefInputs [gyLastIntervalAC]

  -- Mint new interval NFT
  isMintingIntervalNFT <- txMustMintWithMintRef True mpRef mpGY mintRedeemer gyNewIntervalAC

  gyHistoryNodeValue <- valueFromPlutus' historyNodeValue
  -- Output 0: Updated membership history node
  isLockingUpdatedHistoryNode <-
    txMustLockStateWithInlineDatumAndValue
      membershipsValidatorGY
      (OnchainTypes.ListNodeDatum updatedHistoryNode)
      gyHistoryNodeValue

  -- Output 1: New interval
  isLockingNewInterval <-
    txMustLockStateWithInlineDatumAndValue
      membershipsValidatorGY
      (OnchainTypes.IntervalDatum newInterval)
      (valueSingleton gyNewIntervalAC 1 <> valueFromLovelace minLv)

  return
    ( mconcat
        [ spendsOrgUserNFT, -- Input (no output index)
          spendsHistoryNode, -- Input (no output index)
          isInvalidBeforeNow, -- Validity (no output index)
          oracleRefSkeleton, -- Reference input: oracle UTxO
          referencesLastInterval, -- Reference input: last interval
          isMintingIntervalNFT, -- Mint (no output index)
          isLockingUpdatedHistoryNode, -- Output 0: Updated history node
          isLockingNewInterval, -- Output 1: New interval
          feeSkeleton -- Output 2 (optional): Fee payment
        ],
      gyNewIntervalAC
    )

-- | Accept a membership interval (practitioner acknowledges the membership).
-- Corresponds to 'AcceptInterval' spending (no minting).
--
-- Output index layout:
--   Output 0: Updated interval with isAck = True
acceptMembershipIntervalTX ::
  (HasCallStack, GYTxUserQueryMonad m, MonadReader DeployedScriptsContext m) =>
  -- | Interval NFT AC
  GYAssetClass ->
  -- | Practitioner's wallet addresses (to spend User NFT)
  [GYAddress] ->
  m (GYTxSkeleton 'PlutusV3)
acceptMembershipIntervalTX gyIntervalAC ownAddrs = do
  mvRef <- asks getMembershipsValidatorRef

  -- Look up the interval
  (interval, intervalValue) <- getMembershipIntervalDatumAndValue gyIntervalAC

  -- Spend practitioner User NFT
  let practitionerProfileRefAC = OnchainTypes.membershipIntervalPractitionerId interval
  gyPractitionerProfileRefAC <- assetClassFromPlutus' practitionerProfileRefAC
  gyPractitionerUserAC <- gyDeriveUserFromRefAC gyPractitionerProfileRefAC
  spendsPractitionerUserNFT <- txMustSpendFromAddress gyPractitionerUserAC ownAddrs

  -- Compute updated interval using Protocol functions
  let updatedInterval = Onchain.acceptMembershipInterval interval

  -- ============================================================
  -- Output index tracking (order must match skeleton mconcat order)
  -- ============================================================
  -- Output 0: Updated interval
  let updatedIntervalTxOutIdx = 0 :: Integer

  -- Build MembershipsValidator redeemer
  let mvRedeemer =
        redeemerFromPlutusData $
          AcceptInterval
            { updatedIntervalTxOutIdx = updatedIntervalTxOutIdx
            }

  -- Spend the interval from memberships validator
  spendsInterval <- txMustSpendStateFromRefScriptWithRedeemer mvRef gyIntervalAC mvRedeemer membershipsValidatorGY

  gyIntervalValue <- valueFromPlutus' intervalValue
  -- Output 0: Updated interval
  isLockingUpdatedInterval <-
    txMustLockStateWithInlineDatumAndValue
      membershipsValidatorGY
      (OnchainTypes.IntervalDatum updatedInterval)
      gyIntervalValue

  return $
    mconcat
      [ spendsPractitionerUserNFT, -- Input (no output index)
        spendsInterval, -- Input (no output index)
        isLockingUpdatedInterval -- Output 0: Updated interval
      ]

-- | Update the end date of a membership interval (org or practitioner).
-- Corresponds to 'UpdateEndDate' spending (no minting, no fee).
-- Caller must have either the organization User NFT or the practitioner User NFT.
--
-- Output index layout:
--   Output 0: Updated interval with new end date
--
-- When 'validityOverride' is 'Just' a skeleton, it is used as the tx validity
-- instead of 'validitySkeletonForNewEndDate'. Used only by tests (e.g. to trigger TD failure).
updateEndDateTX ::
  (HasCallStack, GYTxUserQueryMonad m, MonadReader DeployedScriptsContext m) =>
  -- | Interval NFT AC
  GYAssetClass ->
  -- | Membership history node NFT AC (reference input)
  GYAssetClass ->
  -- | New end date (must be in the future)
  GYTime ->
  -- | Org or practitioner wallet addresses (to spend User NFT)
  [GYAddress] ->
  -- | Optional validity override (test-only; Nothing = use validitySkeletonForNewEndDate)
  Maybe (GYTxSkeleton 'PlutusV3) ->
  m (GYTxSkeleton 'PlutusV3)
updateEndDateTX gyIntervalAC gyHistoryNodeAC newEndDateGY ownAddrs validityOverride = do
  mvRef <- asks getMembershipsValidatorRef

  (interval, intervalValue) <- getMembershipIntervalDatumAndValue gyIntervalAC
  (historyNode, _historyNodeValue) <- getMembershipListNodeDatumAndValue gyHistoryNodeAC

  history <- case nodeData (OnchainTypes.nodeInfo historyNode) of
    Just h -> return h
    Nothing -> throwError (GYApplicationException MembershipListNodeNotFound)
  gyOrgRef <- assetClassFromPlutus' (OnchainTypes.membershipHistoryOrganizationId history)
  orgUserAC <- gyDeriveUserFromRefAC gyOrgRef
  gyPractitionerRef <- assetClassFromPlutus' (OnchainTypes.membershipIntervalPractitionerId interval)
  practitionerUserAC <- gyDeriveUserFromRefAC gyPractitionerRef

  -- User must have either org or practitioner User NFT (validator enforces exactly one)
  hasOrg <- catchError (True <$ getUTxOWithTokenAtAddresses orgUserAC ownAddrs) (const $ return False)
  hasPractitioner <- catchError (True <$ getUTxOWithTokenAtAddresses practitionerUserAC ownAddrs) (const $ return False)
  spendUserNFT <- do
    case (hasOrg, hasPractitioner) of
      (True, False) -> txMustSpendFromAddress orgUserAC ownAddrs
      (False, True) -> txMustSpendFromAddress practitionerUserAC ownAddrs
      _ -> throwError (GYApplicationException ProfileNotFound) -- must have org or practitioner User NFT
  let newEndDate = timeToPlutus newEndDateGY
  let updatedInterval = Onchain.updateEndDateWithoutValidations interval newEndDate
  let updatedIntervalTxOutIdx = 0 :: Integer
  let mvRedeemer =
        redeemerFromPlutusData $
          UpdateEndDate
            { membershipHistoryNodeId = assetClassToPlutus gyHistoryNodeAC,
              newEndDate = newEndDate,
              updatedIntervalTxOutIdx = updatedIntervalTxOutIdx
            }

  spendsInterval <- txMustSpendStateFromRefScriptWithRedeemer mvRef gyIntervalAC mvRedeemer membershipsValidatorGY
  referencesHistoryNode <- txMustHaveUTxOsAsRefInputs [gyHistoryNodeAC]

  -- Tx validity: use validityOverride if provided, otherwise isValidBetween now validUntil with validUntil > newEndDate so the on-chain TD check enforces "within range".
  (now, validUntilSlot) <- validitySkeletonForNewEndDate
  let validitySkeleton = case validityOverride of
        Just override -> override
        Nothing -> isValidBetween now validUntilSlot

  gyIntervalValue <- valueFromPlutus' intervalValue
  isLockingUpdatedInterval <-
    txMustLockStateWithInlineDatumAndValue
      membershipsValidatorGY
      (OnchainTypes.IntervalDatum updatedInterval)
      gyIntervalValue

  return $
    mconcat
      [ spendUserNFT,
        spendsInterval,
        referencesHistoryNode,
        validitySkeleton,
        isLockingUpdatedInterval
      ]
  where
    validitySkeletonForNewEndDate :: (GYTxQueryMonad m) => m (GYSlot, GYSlot)
    validitySkeletonForNewEndDate = do
      now <- slotOfCurrentBlock
      nowPlutus <- pPOSIXTimeFromGYSlot now
      let newEndDatePlutus = timeToPlutus newEndDateGY
      let nowMs = getPOSIXTime nowPlutus
      let newEndDateMs = getPOSIXTime newEndDatePlutus
      let bufferMs = 1000 -- 1 second
      let safeEraMs = fromIntegral safeEraTime * 1000
      let validUntilMs = max (newEndDateMs + bufferMs) (nowMs + safeEraMs)
      validUntilSlot <- gySlotFromPOSIXTime (POSIXTime validUntilMs)
      return (now, validUntilSlot)

------------------------------------------------------------------------------------------------

-- * Achievement Operations

------------------------------------------------------------------------------------------------

-- | Award an achievement to a practitioner.
-- The awarder (awardedBy) spends their User NFT to authorize.
-- Corresponds to 'NewAchievement' minting.
--
-- Output index layout:
--   Output 0: Achievement NFT locked at achievementsValidator
--   Output 1 (optional): Fee payment
awardAchievementTX ::
  (GYTxUserQueryMonad m, MonadReader DeployedScriptsContext m) =>
  -- | Profile ID of the practitioner receiving the achievement
  GYAssetClass ->
  -- | Profile ID of the awarder (org or practitioner)
  GYAssetClass ->
  -- | CIP-68 metadata fields (name, description, image)
  MetadataFields ->
  -- | Additional metadata key-value pairs
  [(BuiltinByteString, BuiltinByteString)] ->
  -- | Achievement date
  POSIXTime ->
  -- | Awarder's wallet addresses (to spend User NFT)
  [GYAddress] ->
  m (GYTxSkeleton 'PlutusV3, GYAssetClass)
awardAchievementTX gyAwardedToProfileId gyAwardedByProfileId metadata otherMetadata achievementDate ownAddrs = do
  mpGY <- asks getMintingPolicyFromCtx
  mpRef <- asks getMintingPolicyRef
  (oracleRefSkeleton, _oracleParams, minLv, feeSkeleton, isInvalidBeforeNow) <- getOracleFeeAndValiditySkeleton OnchainTypes.fcAchievementFee

  -- Get a seed TxOutRef for uniqueness
  seedTxOutRef <- someUTxOWithoutRefScript
  let isSpendingSeedUTxO = mustHaveInput (GYTxIn seedTxOutRef GYTxInWitnessKey)
  let seedTxOutRefPlutus = txOutRefToV3Plutus seedTxOutRef

  spendsAwarderUserNFT <- spendUserNFTForProfile gyAwardedByProfileId ownAddrs

  -- ============================================================
  -- Output index tracking (order must match skeleton mconcat order)
  -- ============================================================
  -- Output 0: Achievement NFT locked at achievementsValidator
  -- Output 1 (optional): Fee payment
  let achievementOutputIdx = 0 :: Integer

  let redeemer =
        NewAchievement
          seedTxOutRefPlutus
          metadata
          otherMetadata
          (assetClassToPlutus gyAwardedToProfileId)
          (assetClassToPlutus gyAwardedByProfileId)
          achievementDate
          achievementOutputIdx
  let gyRedeemer = redeemerFromPlutusData redeemer

  -- Generate achievement NFT AC
  (gyAchievementRefAC, _gyAchievementUserAC) <- gyGenerateRefAndUserAC mpGY seedTxOutRef
  isMintingAchievementNFT <- txMustMintWithMintRef True mpRef mpGY gyRedeemer gyAchievementRefAC

  -- Build achievement datum
  let achievementInfo =
        OnchainTypes.OnchainAchievement
          { OnchainTypes.achievementId = assetClassToPlutus gyAchievementRefAC,
            OnchainTypes.achievementAwardedTo = assetClassToPlutus gyAwardedToProfileId,
            OnchainTypes.achievementAwardedBy = assetClassToPlutus gyAwardedByProfileId,
            OnchainTypes.achievementDate = achievementDate,
            OnchainTypes.achievementIsAccepted = False
          }
  let achievementDatum = mkCIP68Datum achievementInfo metadata otherMetadata

  -- Output 0: Achievement NFT
  isLockingAchievement <-
    txMustLockStateWithInlineDatumAndValue
      achievementsValidatorGY
      achievementDatum
      (valueSingleton gyAchievementRefAC 1 <> valueFromLovelace minLv)

  return
    ( mconcat
        [ isSpendingSeedUTxO, -- Input (no output index)
          spendsAwarderUserNFT, -- Input (no output index)
          isInvalidBeforeNow, -- Validity (no output index)
          oracleRefSkeleton, -- Reference input: oracle UTxO
          isMintingAchievementNFT, -- Mint (no output index)
          isLockingAchievement, -- Output 0: Achievement NFT
          feeSkeleton -- Output 1 (optional): Fee payment
        ],
      gyAchievementRefAC
    )

-- | Accept an achievement (practitioner consent).
-- Corresponds to 'AcceptAchievement' spending (no minting).
--
-- Output index layout:
--   Output 0: Updated achievement with isAccepted = True
acceptAchievementTX ::
  (HasCallStack, GYTxUserQueryMonad m, MonadReader DeployedScriptsContext m) =>
  -- | Achievement NFT AC
  GYAssetClass ->
  -- | Practitioner's wallet addresses (to spend User NFT)
  [GYAddress] ->
  m (GYTxSkeleton 'PlutusV3)
acceptAchievementTX gyAchievementAC ownAddrs = do
  avRef <- asks getAchievementsValidatorRef

  -- Look up the achievement
  (achievementDatum, achievementValue) <- getAchievementDatumAndValue gyAchievementAC

  -- Spend practitioner User NFT
  let practitionerProfileRefAC = OnchainTypes.achievementAwardedTo (extra achievementDatum)
  gyPractitionerProfileRefAC <- assetClassFromPlutus' practitionerProfileRefAC
  gyPractitionerUserAC <- gyDeriveUserFromRefAC gyPractitionerProfileRefAC
  spendsPractitionerUserNFT <- txMustSpendFromAddress gyPractitionerUserAC ownAddrs

  -- Compute updated achievement
  let updatedAchievement = Onchain.acceptAchievement (extra achievementDatum)
  let updatedAchievementDatum = achievementDatum {extra = updatedAchievement}

  -- ============================================================
  -- Output index tracking (order must match skeleton mconcat order)
  -- ============================================================
  -- Output 0: Updated achievement
  let achievementOutputIdx = 0 :: Integer

  -- Build AchievementsValidator redeemer
  let avRedeemer = redeemerFromPlutusData $ AcceptAchievement achievementOutputIdx

  -- Spend the achievement from achievements validator
  spendsAchievement <- txMustSpendStateFromRefScriptWithRedeemer avRef gyAchievementAC avRedeemer achievementsValidatorGY

  gyAchievementValue <- valueFromPlutus' achievementValue
  -- Output 0: Updated achievement
  isLockingUpdatedAchievement <-
    txMustLockStateWithInlineDatumAndValue
      achievementsValidatorGY
      updatedAchievementDatum
      gyAchievementValue

  return $
    mconcat
      [ spendsPractitionerUserNFT, -- Input (no output index)
        spendsAchievement, -- Input (no output index)
        isLockingUpdatedAchievement -- Output 0: Updated achievement
      ]

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
  let gyRedeemer = redeemerFromPlutusData (OracleUpdate 0)

  -- Apply the action to derive the new oracle params
  let newParams = applyAdminAction adminAction currentParams

  -- Convert admin PKH from Plutus to GY for required signer
  gyAdminPkh <- pubKeyHashFromPlutus' (OnchainTypes.opAdminPkh currentParams)

  -- Spend the current oracle UTxO
  let currentDatum = datumFromPlutusData currentParams
  let spendOracle = txMustSpendFromRefScriptWithKnownDatum ovRef oracleRef currentDatum gyRedeemer oracleValidatorGY

  -- Re-lock oracle with new datum and same value
  lockOracle <- txMustLockStateWithInlineDatumAndValue oracleValidatorGY newParams oracleValue

  -- Require admin signature (on-chain validator checks txInfoSignatories)
  let requireAdminSig = mustBeSignedBy gyAdminPkh

  return $ mconcat [spendOracle, lockOracle, requireAdminSig]

-- | Apply an admin action delta to current oracle params to produce new params.
applyAdminAction :: AdminActionType -> OnchainTypes.OracleParams -> OnchainTypes.OracleParams
applyAdminAction PauseProtocolAction params = params {OnchainTypes.opPaused = True}
applyAdminAction UnpauseProtocolAction params = params {OnchainTypes.opPaused = False}
applyAdminAction (SetFeesAction mFeeConfig) params = params {OnchainTypes.opFeeConfig = mFeeConfig}

------------------------------------------------------------------------------------------------

-- * Dust / Cleanup Operations

------------------------------------------------------------------------------------------------

-- | Build spend skeletons for dust UTxOs at one validator (for cleanup).
dustSpendsForValidatorSkeleton ::
  (GYTxUserQueryMonad m, MonadReader DeployedScriptsContext m) =>
  (DeployedScriptsContext -> GYTxOutRef) ->
  GYScript 'PlutusV3 ->
  GYRedeemer ->
  m [GYTxSkeleton 'PlutusV3]
dustSpendsForValidatorSkeleton getRef validatorGY cleanupRedeemer = do
  ref <- asks getRef
  addr <- scriptAddress validatorGY
  ctx <- ask
  let protocolCS = mintingPolicyCurrencySymbol (getMintingPolicyFromCtx ctx)
  dust <- getDustUTxOs addr protocolCS
  return $ map (\utxo -> txMustSpendUTxOFromRefScript ref utxo cleanupRedeemer validatorGY) dust

-- | Sweep dust UTxOs from all deployed validator addresses.
-- Returns the number of dust UTxOs found and the transaction skeleton.
-- Anyone can call this — the on-chain 'Cleanup' redeemer is permissionless.
-- The ADA locked in the dust UTxOs is returned to the caller.
cleanupDustTX ::
  (GYTxUserQueryMonad m, MonadReader DeployedScriptsContext m) =>
  m (GYTxSkeleton 'PlutusV3, Int)
cleanupDustTX = do
  configs <-
    sequence
      [ dustSpendsForValidatorSkeleton getProfilesValidatorRef profilesValidatorGY (redeemerFromPlutusData Onchain.Validators.ProfilesValidator.Cleanup),
        dustSpendsForValidatorSkeleton getRanksValidatorRef ranksValidatorGY (redeemerFromPlutusData Onchain.Validators.RanksValidator.Cleanup),
        dustSpendsForValidatorSkeleton getMembershipsValidatorRef membershipsValidatorGY (redeemerFromPlutusData Onchain.Validators.MembershipsValidator.Cleanup),
        dustSpendsForValidatorSkeleton getAchievementsValidatorRef achievementsValidatorGY (redeemerFromPlutusData Onchain.Validators.AchievementsValidator.Cleanup)
      ]
  let allSpends = concat configs
      dustCount = sum (map length configs)
  when (dustCount == 0) $
    throwError (GYApplicationException NoDustFound)
  return (mconcat allSpends, dustCount)
