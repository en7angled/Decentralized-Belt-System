-- Required for `makeLift`:
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:conservative-optimisation #-}

{-# HLINT ignore "Use &&" #-}

-- | Minting policy for the BJJ Belt protocol.
-- Controls token creation for profiles, ranks, promotions, and memberships.
module Onchain.Validators.MintingPolicy
  ( -- * Minting Redeemer
    MintingRedeemer (..),

    -- * Minting Policy
    mintingPolicyLambda,

    -- * Compilation
    mintingPolicyCompile,
  )
where

import GHC.Generics (Generic)
import Onchain.BJJ (BeltSnapshot (..), intToBelt, validatePromotion)
import Onchain.CIP68 (MetadataFields, deriveUserFromRefAC, generateRefAndUserTN, mkCIP68Datum, validateMetadataFields)
import Onchain.Protocol (addMembershipIntervalToHistory, deriveIntervalsHeadId, deriveMembershipHistoriesListId, deriveMembershipHistoryIdFromHistory, deriveMembershipIntervalId, derivePromotionRankId, getCurrentRankId, initEmptyMembershipHistoriesList, initMembershipHistory, mkOrganizationProfile, mkPractitionerProfile, mkPromotion, unsafeGetListNodeDatumAndValue, unsafeGetMembershipHistory, unsafeGetMembershipInterval, unsafeGetProfile, unsafeGetRank)
import Onchain.Protocol.Lookup (checkFee, readOracleParams)
import Onchain.Protocol.Types
import Onchain.Utils (hasCurrencySymbol)
import Onchain.Utils qualified as Utils
import PlutusCore.Builtin.Debug (plcVersion110)
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V1.Interval (before)
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.Contexts
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude
import Prelude qualified

-------------------------------------------------------------------------------

-- * Minting Redeemer

-------------------------------------------------------------------------------

-- | Custom redeemer :
-- NOTE: Profile deletion (burning) is intentionally not supported to preserve lineage integrity.
-- BJJ belt records are permanent historical facts that should not be erasable.
-- Output indices are passed in the redeemer for efficient O(1) lookup instead of O(n) search.
data MintingRedeemer
  = -- | CreateProfile seedTxOutRef metadata profileType creationDate rankNumber profileOutputIdx rankOrMembershipRootOutputIdx
    -- For Practitioner profiles, the last index points to the rank output.
    -- For Organization profiles, the last index points to the membership histories root output.
    CreateProfile TxOutRef MetadataFields OnchainProfileType POSIXTime Integer Integer Integer
  | -- | Promote seedTxOutRef promotedProfileId promoterProfileId achievementDate rankNumber pendingRankOutputIdx
    Promote TxOutRef ProfileId ProfileId POSIXTime Integer Integer
  | -- | NewMembershipHistory organizationProfileId practitionerId startDate endDate leftNodeId firstIntervalOutputIdx
    NewMembershipHistory ProfileId ProfileId POSIXTime (Maybe POSIXTime) MembershipHistoriesListNodeId Integer
  | -- | NewMembershipInterval organizationProfileId membershipNodeId startDate endDate intervalOutputIdx
    NewMembershipInterval ProfileId MembershipHistoriesListNodeId POSIXTime (Maybe POSIXTime) Integer
  | -- | NewAchievement seedTxOutRef metadata222 otherMetadata awardedTo awardedBy achievementDate achievementOutputIdx
    NewAchievement TxOutRef MetadataFields [(BuiltinByteString, BuiltinByteString)] ProfileId ProfileId POSIXTime Integer
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''MintingRedeemer [('CreateProfile, 0), ('Promote, 1), ('NewMembershipHistory, 2), ('NewMembershipInterval, 3), ('NewAchievement, 4)]

-------------------------------------------------------------------------------

-- | Precomputed minLv and validator addresses for the minting policy.
data MintingContext = MintingContext
  { ctxMinLv :: Value,
    ctxProfilesAddress :: Address,
    ctxRanksAddress :: Address,
    ctxMembershipsAddress :: Address,
    ctxAchievementsAddress :: Address
  }

-------------------------------------------------------------------------------

-- * Minting Policy

-------------------------------------------------------------------------------

{-# INLINEABLE mintingPolicyLambda #-}
mintingPolicyLambda :: ProtocolParams -> ScriptContext -> Bool
mintingPolicyLambda protocolParams (ScriptContext txInfo@TxInfo {..} (Redeemer bredeemer) scriptInfo) =
  let redeemer = unsafeFromBuiltinData @MintingRedeemer bredeemer
   in case scriptInfo of
        (MintingScript mintingPolicyCurrencySymbol) ->
          let oracle = readOracleParams (oracleToken protocolParams) txInfoReferenceInputs
              minLv = Utils.protocolMinLovelaceValue
              profilesValidatorAddress = V1.scriptHashAddress (profilesValidatorScriptHash protocolParams)
              ranksValidatorAddress = V1.scriptHashAddress (ranksValidatorScriptHash protocolParams)
              membershipsValidatorAddress = V1.scriptHashAddress (membershipsValidatorScriptHash protocolParams)
              achievementsValidatorAddress = V1.scriptHashAddress (achievementsValidatorScriptHash protocolParams)
              ctx =
                MintingContext
                  { ctxMinLv = minLv,
                    ctxProfilesAddress = profilesValidatorAddress,
                    ctxRanksAddress = ranksValidatorAddress,
                    ctxMembershipsAddress = membershipsValidatorAddress,
                    ctxAchievementsAddress = achievementsValidatorAddress
                  }
           in -- Global gate: protocol must not be paused
              traceIfFalse "M0" (not (opPaused oracle)) -- Protocol is paused (M0)
                && case redeemer of
                  CreateProfile seedTxOutRef metadata profileType creationDate rankNumber profileOutputIdx rankOrMembershipRootOutputIdx ->
                    checkFee oracle fcProfileCreationFee txInfoOutputs
                      && handleCreateProfile protocolParams txInfo mintingPolicyCurrencySymbol ctx seedTxOutRef metadata profileType creationDate rankNumber profileOutputIdx rankOrMembershipRootOutputIdx
                  Promote seedTxOutRef studentProfileId masterProfileId achievementDate nextRankNum pendingRankOutputIdx ->
                    checkFee oracle fcPromotionFee txInfoOutputs
                      && handlePromote protocolParams txInfo mintingPolicyCurrencySymbol ctx seedTxOutRef studentProfileId masterProfileId achievementDate nextRankNum pendingRankOutputIdx
                  NewMembershipHistory organizationProfileId practitionerId startDate endDate leftNodeId firstIntervalOutputIdx ->
                    checkFee oracle fcMembershipHistoryFee txInfoOutputs
                      && handleNewMembershipHistory txInfo mintingPolicyCurrencySymbol ctx organizationProfileId practitionerId startDate endDate leftNodeId firstIntervalOutputIdx
                  NewMembershipInterval organizationProfileId membershipNodeId startDate mEndDate intervalOutputIdx ->
                    checkFee oracle fcMembershipIntervalFee txInfoOutputs
                      && handleNewMembershipInterval txInfo mintingPolicyCurrencySymbol ctx organizationProfileId membershipNodeId startDate mEndDate intervalOutputIdx
                  NewAchievement seedTxOutRef metadata otherMetadata awardedTo awardedBy achievementDate achievementOutputIdx ->
                    checkFee oracle fcAchievementFee txInfoOutputs
                      && handleNewAchievement txInfo mintingPolicyCurrencySymbol ctx seedTxOutRef metadata otherMetadata awardedTo awardedBy achievementDate achievementOutputIdx
        _ -> traceError "M1" -- Invalid purpose (M1)

-------------------------------------------------------------------------------

-- * Per-Redeemer Handlers

-------------------------------------------------------------------------------

{-# INLINEABLE handleCreateProfile #-}
handleCreateProfile ::
  ProtocolParams ->
  TxInfo ->
  CurrencySymbol ->
  MintingContext ->
  TxOutRef ->
  MetadataFields ->
  OnchainProfileType ->
  POSIXTime ->
  Integer ->
  Integer ->
  Integer ->
  Bool
handleCreateProfile protocolParams TxInfo {..} mintingPolicyCurrencySymbol ctx seedTxOutRef metadata profileType creationDate rankNumber profileOutputIdx rankOrMembershipRootOutputIdx =
  -- NOTE: rankNumber validation handled by intToBelt (fails for values outside 0-14)
  let (profileRefTN, profileUserTN) = generateRefAndUserTN $ Utils.nameFromTxOutRef seedTxOutRef
      profileRefAssetClass = V1.AssetClass (mintingPolicyCurrencySymbol, profileRefTN)
      profileUserAssetClass = V1.AssetClass (mintingPolicyCurrencySymbol, profileUserTN)
      profileRefNFT = V1.assetClassValue profileRefAssetClass 1
      profileUserNFT = V1.assetClassValue profileUserAssetClass 1
      minLv = ctxMinLv ctx
      profilesValidatorAddress = ctxProfilesAddress ctx
   in traceIfFalse "M2" (creationDate `before` txInfoValidRange) -- Creation date before validity (M2)
        && traceIfFalse "M3" (Utils.inputsSpendTxOutRef seedTxOutRef txInfoInputs) -- Must spend seed TxOutRef (M3)
        && traceIfFalse "M4" (validateMetadataFields metadata) -- Metadata fields validation failed (M4)
        && case profileType of
          Practitioner ->
            let (profile, rankDatum) = mkPractitionerProfile profileRefAssetClass creationDate protocolParams rankNumber
                profileDatum = mkCIP68Datum profile metadata []
                rankAssetClass = rankId rankDatum
                rankNFT = V1.assetClassValue rankAssetClass 1
             in traceIfFalse
                  "M5" -- Practitioner mint/lock check failed (M5)
                  ( Utils.checkTxOutAtIndexWithDatumValueAndAddress profileOutputIdx profileDatum (profileRefNFT + minLv) profilesValidatorAddress txInfoOutputs
                      && mintValueMinted txInfoMint
                      == profileRefNFT
                      + profileUserNFT
                      + rankNFT
                      && Utils.checkTxOutAtIndexWithDatumValueAndAddress rankOrMembershipRootOutputIdx rankDatum (rankNFT + minLv) (ctxRanksAddress ctx) txInfoOutputs
                  )
          Organization ->
            let profile = mkOrganizationProfile profileRefAssetClass protocolParams
                profileDatum = mkCIP68Datum profile metadata []
                membershipHistoriesRootDatum = initEmptyMembershipHistoriesList profileRefAssetClass
                membershipHistoriesRootAssetClass = deriveMembershipHistoriesListId profileRefAssetClass
                membershipHistoriesRootNFT = V1.assetClassValue membershipHistoriesRootAssetClass 1
             in traceIfFalse
                  "M6" -- Organization mint/lock check failed (M6)
                  ( Utils.checkTxOutAtIndexWithDatumValueAndAddress profileOutputIdx profileDatum (profileRefNFT + minLv) profilesValidatorAddress txInfoOutputs
                      && Utils.checkTxOutAtIndexWithDatumValueAndAddress rankOrMembershipRootOutputIdx (ListNodeDatum membershipHistoriesRootDatum) (membershipHistoriesRootNFT + minLv) (ctxMembershipsAddress ctx) txInfoOutputs
                      && mintValueMinted txInfoMint
                      == profileRefNFT
                      + profileUserNFT
                      + membershipHistoriesRootNFT
                  )

{-# INLINEABLE handlePromote #-}
handlePromote ::
  ProtocolParams ->
  TxInfo ->
  CurrencySymbol ->
  MintingContext ->
  TxOutRef ->
  ProfileId ->
  ProfileId ->
  POSIXTime ->
  Integer ->
  Integer ->
  Bool
handlePromote protocolParams txInfo@TxInfo {..} mintingPolicyCurrencySymbol ctx seedTxOutRef studentProfileId masterProfileId achievementDate nextRankNum pendingRankOutputIdx =
  let profilesValidatorAddress = ctxProfilesAddress ctx
      ranksValidatorAddress = ctxRanksAddress ctx
      minLv = ctxMinLv ctx
      --- Validate student and master profiles are valid
      isStudentValid = hasCurrencySymbol studentProfileId mintingPolicyCurrencySymbol
      isMasterValid = hasCurrencySymbol masterProfileId mintingPolicyCurrencySymbol

      -- Generate unique promotion rank ID from seed
      pendingRankAssetClass = derivePromotionRankId seedTxOutRef mintingPolicyCurrencySymbol
      pendingRankNFT = V1.assetClassValue pendingRankAssetClass 1
      pendingRankDatum = mkPromotion pendingRankAssetClass studentProfileId masterProfileId achievementDate nextRankNum protocolParams

      -- Master must spend their user NFT to authorize promotion
      masterUserAC = deriveUserFromRefAC masterProfileId

      -- Get student and master profiles from reference inputs
      studentProfile = unsafeGetProfile studentProfileId profilesValidatorAddress txInfoReferenceInputs
      masterProfile = unsafeGetProfile masterProfileId profilesValidatorAddress txInfoReferenceInputs

      -- Get current ranks from reference inputs
      studentCurrentRankId = getCurrentRankId studentProfile
      masterCurrentRankId = getCurrentRankId masterProfile
      studentCurrentRank = unsafeGetRank studentCurrentRankId ranksValidatorAddress txInfoReferenceInputs
      masterRank = unsafeGetRank masterCurrentRankId ranksValidatorAddress txInfoReferenceInputs

      -- Validate the promotion using BJJ rules
      master = BeltSnapshot (intToBelt $ rankNumber masterRank) (rankAchievementDate masterRank)
      studentCurrent = BeltSnapshot (intToBelt $ rankNumber studentCurrentRank) (rankAchievementDate studentCurrentRank)
      studentNext = BeltSnapshot (intToBelt nextRankNum) achievementDate

      isPromotionValid = validatePromotion master studentCurrent studentNext
   in traceIfFalse "M7" (isStudentValid && isMasterValid) -- Profiles must have correct currency symbol (M7)
        && traceIfFalse "M8" (Utils.inputsSpendTxOutRef seedTxOutRef txInfoInputs) -- Must spend seed for uniqueness (M8)
        && traceIfFalse "M9" (V1.assetClassValueOf (valueSpent txInfo) masterUserAC == 1) -- Must spend master user NFT (M9)
        && traceIfFalse "Ma" (mintValueMinted txInfoMint == pendingRankNFT) -- Tx must mint JUST pending rank NFT (Ma)
        && traceIfFalse "Mb" (Utils.checkTxOutAtIndexWithDatumValueAndAddress pendingRankOutputIdx pendingRankDatum (pendingRankNFT + minLv) ranksValidatorAddress txInfoOutputs) -- Lock pending rank at RV (Mb)
        && traceIfFalse "Mc" isPromotionValid -- Must pass promotion validation (Mc)

{-# INLINEABLE handleNewMembershipHistory #-}
handleNewMembershipHistory ::
  TxInfo ->
  CurrencySymbol ->
  MintingContext ->
  ProfileId ->
  ProfileId ->
  POSIXTime ->
  Maybe POSIXTime ->
  MembershipHistoriesListNodeId ->
  Integer ->
  Bool
handleNewMembershipHistory txInfo@TxInfo {..} mintingPolicyCurrencySymbol ctx organizationProfileId practitionerId startDate endDate leftNodeId firstIntervalOutputIdx =
  let organizationUserAC = deriveUserFromRefAC organizationProfileId
      isOrganizationValid = hasCurrencySymbol organizationProfileId mintingPolicyCurrencySymbol
      isPractitionerValid = hasCurrencySymbol practitionerId mintingPolicyCurrencySymbol
      isLeftNodeIdValid = hasCurrencySymbol leftNodeId mintingPolicyCurrencySymbol
      membershipsValidatorAddress = ctxMembershipsAddress ctx
      minLv = ctxMinLv ctx

      (newHistory, fstInterval) = initMembershipHistory practitionerId organizationProfileId startDate endDate

      historyId = deriveMembershipHistoryIdFromHistory newHistory
      membershipHistoryNFT = V1.assetClassValue historyId 1
      fstIntervalId = deriveIntervalsHeadId newHistory
      fstIntervalNFT = V1.assetClassValue fstIntervalId 1
   in -- Must spend user NFT of the organization profile who is creating the membership history -- enforced here
      -- Must spend the UTxO of the left node of the membership history list (containing LeftNodeId NFT) from membershipsValidator address -- enforced here
      -- Must lock the updated left node with inline datum at membershipsValidator address (output idx) -- enforced by membershipsValidator
      -- Must mint the membership history NFT and the membership interval NFT -- enforced both here and in membershipsValidator
      -- Must lock the membership history NFT with inline datum at membershipsValidator address (output idx) -- enforced by membershipsValidator
      -- Must lock the membership interval NFT with inline datum at membershipsValidator address (output idx)
      traceIfFalse
        "Md" -- Membership history mint check failed (Md)
        ( (startDate `before` txInfoValidRange)
            && (isOrganizationValid && isPractitionerValid && isLeftNodeIdValid)
            && V1.assetClassValueOf (valueSpent txInfo) organizationUserAC
            == 1
            && Utils.hasTxInAtAddressWithNFT leftNodeId membershipsValidatorAddress txInfoInputs
            && mintValueMinted txInfoMint
            == (membershipHistoryNFT + fstIntervalNFT)
            && Utils.checkTxOutAtIndexWithDatumValueAndAddress firstIntervalOutputIdx (IntervalDatum fstInterval) (fstIntervalNFT + minLv) membershipsValidatorAddress txInfoOutputs
        )

{-# INLINEABLE handleNewMembershipInterval #-}
handleNewMembershipInterval ::
  TxInfo ->
  CurrencySymbol ->
  MintingContext ->
  ProfileId ->
  MembershipHistoriesListNodeId ->
  POSIXTime ->
  Maybe POSIXTime ->
  Integer ->
  Bool
handleNewMembershipInterval txInfo@TxInfo {..} mintingPolicyCurrencySymbol ctx organizationProfileId membershipNodeId startDate mEndDate intervalOutputIdx =
  let organizationUserAC = deriveUserFromRefAC organizationProfileId
      isOrganizationValid = hasCurrencySymbol organizationProfileId mintingPolicyCurrencySymbol
      isMembershipNodeIdValid = hasCurrencySymbol membershipNodeId mintingPolicyCurrencySymbol
      membershipsValidatorAddress = ctxMembershipsAddress ctx
      minLv = ctxMinLv ctx

      (_, membershipNodeDatum) = unsafeGetListNodeDatumAndValue membershipNodeId membershipsValidatorAddress txInfoInputs
      oldHistory = unsafeGetMembershipHistory membershipNodeDatum

      historyId = deriveMembershipHistoryIdFromHistory oldHistory
      lastIntervalId = deriveIntervalsHeadId oldHistory
      (_lastIntervalValue, lastIntervalDatum) = unsafeGetMembershipInterval lastIntervalId membershipsValidatorAddress txInfoReferenceInputs

      (_newHistory, newInterval) = addMembershipIntervalToHistory oldHistory lastIntervalDatum startDate mEndDate
      newIntervalId = deriveMembershipIntervalId historyId (membershipIntervalNumber newInterval)
      newIntervalNFT = V1.assetClassValue newIntervalId 1
   in -- Must spend user NFT of the organization profile who is creating the membership interval -- enforced here
      -- Must spend the UTxO of the updated membership history (with ListNodeDatum) -- enforced here
      -- Must lock the updated membership history with inline datum at membershipsValidator address (output idx) -- enforced by membershipsValidator
      -- Must reference the last interval from the membership validators with IntervalDatum -- enforced here
      -- Must mint the membership interval NFT -- enforced both here and in membershipsValidator
      -- Must lock the membership interval NFT with inline datum at membershipsValidator address (output idx) -- enforced here

      traceIfFalse
        "Me" -- Membership interval mint check failed (Me)
        ( (startDate `before` txInfoValidRange)
            && (isOrganizationValid && isMembershipNodeIdValid)
            && V1.assetClassValueOf (valueSpent txInfo) organizationUserAC
            == 1
            && Utils.hasTxInAtAddressWithNFT membershipNodeId membershipsValidatorAddress txInfoInputs
            && mintValueMinted txInfoMint
            == newIntervalNFT
            && Utils.checkTxOutAtIndexWithDatumValueAndAddress intervalOutputIdx (IntervalDatum newInterval) (newIntervalNFT + minLv) membershipsValidatorAddress txInfoOutputs
        )

{-# INLINEABLE handleNewAchievement #-}
handleNewAchievement ::
  TxInfo ->
  CurrencySymbol ->
  MintingContext ->
  TxOutRef ->
  MetadataFields ->
  [(BuiltinByteString, BuiltinByteString)] ->
  ProfileId ->
  ProfileId ->
  POSIXTime ->
  Integer ->
  Bool
handleNewAchievement txInfo@TxInfo {..} mintingPolicyCurrencySymbol ctx seedTxOutRef metadata otherMetdataEncoded awardedTo awardedBy achievementDate achievementOutputIdx =
  let isAwardedToValid = hasCurrencySymbol awardedTo mintingPolicyCurrencySymbol
      isAwardedByValid = hasCurrencySymbol awardedBy mintingPolicyCurrencySymbol
      minLv = ctxMinLv ctx
      achievementsValidatorAddress = ctxAchievementsAddress ctx

      (achievementRefTN, _) = generateRefAndUserTN (Utils.nameFromTxOutRef seedTxOutRef)
      achievementRefAssetClass = V1.AssetClass (mintingPolicyCurrencySymbol, achievementRefTN)
      achievementNFT = V1.assetClassValue achievementRefAssetClass 1
      achievementInfo =
        OnchainAchievement
          { achievementId = achievementRefAssetClass,
            achievementAwardedTo = awardedTo,
            achievementAwardedBy = awardedBy,
            achievementDate = achievementDate,
            achievementIsAccepted = False
          }

      achievementDatum = mkCIP68Datum achievementInfo metadata otherMetdataEncoded
   in traceIfFalse "Ml" (achievementDate `before` txInfoValidRange) -- Achievement date before validity (Ml)
        && traceIfFalse "Mm" (validateMetadataFields metadata) -- Metadata fields validation failed (Mm)
        && traceIfFalse "Mf" (V1.assetClassValueOf (valueSpent txInfo) (deriveUserFromRefAC awardedBy) == 1) -- Must spend awarded by user NFT (Mf)
        && traceIfFalse "Mg" (isAwardedToValid && isAwardedByValid) -- Profiles must have correct currency symbol (Mg)
        && traceIfFalse "Mh" (Utils.inputsSpendTxOutRef seedTxOutRef txInfoInputs) -- Must spend seed for uniqueness (Mh)
        && traceIfFalse "Mj" (mintValueMinted txInfoMint == achievementNFT) -- Tx must mint JUST achievement NFT (Mj)
        && traceIfFalse "Mk" (Utils.checkTxOutAtIndexWithDatumValueAndAddress achievementOutputIdx achievementDatum (achievementNFT + minLv) achievementsValidatorAddress txInfoOutputs) -- Lock achievement at AchievementsValidator address (Mk)

-------------------------------------------------------------------------------

-- * Compilation

-------------------------------------------------------------------------------

-- | Lose the types
{-# INLINEABLE mintingPolicyUntyped #-}
mintingPolicyUntyped :: ProtocolParams -> BuiltinData -> BuiltinUnit
mintingPolicyUntyped params =
  Utils.mkUntypedLambda (mintingPolicyLambda params)

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
mintingPolicyCompile :: ProtocolParams -> CompiledCode (BuiltinData -> BuiltinUnit)
mintingPolicyCompile params = $$(compile [||mintingPolicyUntyped||]) `unsafeApplyCode` liftCode plcVersion110 params
