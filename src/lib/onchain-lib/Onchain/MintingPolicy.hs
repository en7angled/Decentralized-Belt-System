-- Required for `makeLift`:
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use &&" #-}

-- | Minting policy for the BJJ Belt protocol.
-- Controls token creation for profiles, ranks, promotions, and memberships.
module Onchain.MintingPolicy
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
import Onchain.Protocol (MembershipDatum (..), MembershipHistoriesListNodeId, OnchainMembershipHistory (..), OnchainProfileType (..), OnchainRank (..), ProfileId, ProtocolParams, addMembershipIntervalToHistory, deriveMembershipHistoriesListId, derivePromotionRankId, getCurrentRankId, initEmptyMembershipHistoriesList, initMembershipHistory, membershipIntervalId, membershipsValidatorScriptHash, mkOrganizationProfile, mkPractitionerProfile, mkPromotion, oracleToken, profilesValidatorScriptHash, ranksValidatorScriptHash, unsafeGetListNodeDatumAndValue, unsafeGetMembershipHistory, unsafeGetMembershipInterval, unsafeGetProfile, unsafeGetRank)
import Onchain.Protocol.Types (FeeConfig (..), OracleParams (..))
import Onchain.Utils (checkFee, hasCurrencySymbol, readOracleParams)
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
  | NewMembershipHistory ProfileId ProfileId POSIXTime (Maybe POSIXTime) MembershipHistoriesListNodeId Integer
  | NewMembershipInterval ProfileId MembershipHistoriesListNodeId POSIXTime (Maybe POSIXTime) Integer
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''MintingRedeemer [('CreateProfile, 0), ('Promote, 1), ('NewMembershipHistory, 2), ('NewMembershipInterval, 3)]

-------------------------------------------------------------------------------

-- | Precomputed minLv and validator addresses for the minting policy.
data MintingContext = MintingContext
  { ctxMinLv :: Value,
    ctxProfilesAddress :: Address,
    ctxRanksAddress :: Address,
    ctxMembershipsAddress :: Address
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
              minLv = V1.lovelaceValue (V1.Lovelace (opMinOutputLovelace oracle))
              profilesValidatorAddress = V1.scriptHashAddress (profilesValidatorScriptHash protocolParams)
              ranksValidatorAddress = V1.scriptHashAddress (ranksValidatorScriptHash protocolParams)
              membershipsValidatorAddress = V1.scriptHashAddress (membershipsValidatorScriptHash protocolParams)
              ctx =
                MintingContext
                  { ctxMinLv = minLv,
                    ctxProfilesAddress = profilesValidatorAddress,
                    ctxRanksAddress = ranksValidatorAddress,
                    ctxMembershipsAddress = membershipsValidatorAddress
                  }
           in -- Global gate: protocol must not be paused
              traceIfFalse "0" (not (opPaused oracle)) -- Protocol is paused
                && case redeemer of
                  CreateProfile seedTxOutRef metadata profileType creationDate rankNumber profileOutputIdx rankOrMembershipRootOutputIdx ->
                    checkFee oracle fcProfileCreationFee txInfoOutputs
                      && handleCreateProfile protocolParams txInfo mintingPolicyCurrencySymbol ctx seedTxOutRef metadata profileType creationDate rankNumber profileOutputIdx rankOrMembershipRootOutputIdx
                  Promote seedTxOutRef studentProfileId masterProfileId achievementDate nextRankNum pendingRankOutputIdx ->
                    checkFee oracle fcPromotionFee txInfoOutputs
                      && handlePromote protocolParams txInfo mintingPolicyCurrencySymbol ctx seedTxOutRef studentProfileId masterProfileId achievementDate nextRankNum pendingRankOutputIdx
                  NewMembershipHistory organizationProfileId practitionerId startDate endDate leftNodeId firstIntervalOutputIdx ->
                    checkFee oracle fcMembershipFee txInfoOutputs
                      && handleNewMembershipHistory txInfo mintingPolicyCurrencySymbol ctx organizationProfileId practitionerId startDate endDate leftNodeId firstIntervalOutputIdx
                  NewMembershipInterval organizationProfileId membershipNodeId startDate mEndDate intervalOutputIdx ->
                    checkFee oracle fcMembershipFee txInfoOutputs
                      && handleNewMembershipInterval txInfo mintingPolicyCurrencySymbol ctx organizationProfileId membershipNodeId startDate mEndDate intervalOutputIdx
        _ -> traceError "1" -- Invalid purpose

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
   in traceIfFalse "2" (creationDate `before` txInfoValidRange) -- Creation date must be before the tx validity range
        && traceIfFalse "3" (any ((== seedTxOutRef) . txInInfoOutRef) txInfoInputs) -- Must spend seed TxOutRef
        && validateMetadataFields metadata
        && case profileType of
          Practitioner ->
            let (profile, rankDatum) = mkPractitionerProfile profileRefAssetClass creationDate protocolParams rankNumber
                profileDatum = mkCIP68Datum profile metadata
                rankAssetClass = rankId rankDatum
                rankNFT = V1.assetClassValue rankAssetClass 1
             in traceIfFalse "4" (Utils.checkTxOutAtIndexWithDatumValueAndAddress profileOutputIdx profileDatum (profileRefNFT + minLv) profilesValidatorAddress txInfoOutputs) -- Must lock profile at PV
                  && traceIfFalse "5" (mintValueMinted txInfoMint == (profileRefNFT + profileUserNFT + rankNFT)) -- Exact mint check
                  && traceIfFalse "6" (Utils.checkTxOutAtIndexWithDatumValueAndAddress rankOrMembershipRootOutputIdx rankDatum (rankNFT + minLv) (ctxRanksAddress ctx) txInfoOutputs) -- Must lock rank at RV
          Organization ->
            let profile = mkOrganizationProfile profileRefAssetClass protocolParams
                profileDatum = mkCIP68Datum profile metadata
                membershipHistoriesRootDatum = initEmptyMembershipHistoriesList profileRefAssetClass
                membershipHistoriesRootAssetClass = deriveMembershipHistoriesListId profileRefAssetClass
                membershipHistoriesRootNFT = V1.assetClassValue membershipHistoriesRootAssetClass 1
             in traceIfFalse "7" (Utils.checkTxOutAtIndexWithDatumValueAndAddress profileOutputIdx profileDatum (profileRefNFT + minLv) profilesValidatorAddress txInfoOutputs) -- Must lock profile at PV
                  && traceIfFalse "8" (Utils.checkTxOutAtIndexWithDatumValueAndAddress rankOrMembershipRootOutputIdx (ListNodeDatum membershipHistoriesRootDatum) (membershipHistoriesRootNFT + minLv) (ctxMembershipsAddress ctx) txInfoOutputs) -- Must lock root at MV
                  && traceIfFalse "9" (mintValueMinted txInfoMint == (profileRefNFT + profileUserNFT + membershipHistoriesRootNFT)) -- Exact mint check

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
   in traceIfFalse "A" (isStudentValid && isMasterValid) -- Profiles must have correct currency symbol
        && traceIfFalse "B" (any ((== seedTxOutRef) . txInInfoOutRef) txInfoInputs) -- Must spend seed TxOutRef for uniqueness
        && traceIfFalse "C" (V1.assetClassValueOf (valueSpent txInfo) masterUserAC == 1) -- Must spend user NFT of the profile who awards the promotion"
        && traceIfFalse "D" (mintValueMinted txInfoMint == pendingRankNFT) -- Tx must mint JUST the pending rank NFT
        && traceIfFalse "E" (Utils.checkTxOutAtIndexWithDatumValueAndAddress pendingRankOutputIdx pendingRankDatum (pendingRankNFT + minLv) ranksValidatorAddress txInfoOutputs) -- Must lock pending rank NFT with inline datum at ranksValidator address (output idx)
        && traceIfFalse "F" isPromotionValid -- Must pass promotion validation rules

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

      membershipHistoryNFT = V1.assetClassValue (membershipHistoryId newHistory) 1
      fstIntervalNFT = V1.assetClassValue (membershipIntervalId fstInterval) 1
   in -- Must spend user NFT of the organization profile who is creating the membership history -- enforced here
      -- Must spend the UTxO of the left node of the membership history list (containing LeftNodeId NFT) from membershipsValidator address -- enforced here
      -- Must lock the updated left node with inline datum at membershipsValidator address (output idx) -- enforced by membershipsValidator
      -- Must mint the membership history NFT and the membership interval NFT -- enforced both here and in membershipsValidator
      -- Must lock the membership history NFT with inline datum at membershipsValidator address (output idx) -- enforced by membershipsValidator
      -- Must lock the membership interval NFT with inline datum at membershipsValidator address (output idx)
      traceIfFalse "G" (startDate `before` txInfoValidRange) -- Start date before validity (can not be in the future)
        && traceIfFalse "H" (isOrganizationValid && isPractitionerValid && isLeftNodeIdValid) -- CS check
        && traceIfFalse "I" (V1.assetClassValueOf (valueSpent txInfo) organizationUserAC == 1) -- Must spend org User NFT
        && traceIfFalse "J" (Utils.hasTxInAtAddressWithNFT leftNodeId membershipsValidatorAddress txInfoInputs) -- Must spend left node
        && traceIfFalse "K" (mintValueMinted txInfoMint == (membershipHistoryNFT + fstIntervalNFT)) -- Exact mint check
        && traceIfFalse "L" (Utils.checkTxOutAtIndexWithDatumValueAndAddress firstIntervalOutputIdx (IntervalDatum fstInterval) (fstIntervalNFT + minLv) membershipsValidatorAddress txInfoOutputs) -- Lock interval at MV

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

      lastIntervalId = membershipHistoryIntervalsHeadId oldHistory
      (_lastIntervalValue, lastIntervalDatum) = unsafeGetMembershipInterval lastIntervalId membershipsValidatorAddress txInfoReferenceInputs

      (_newHistory, newInterval) = addMembershipIntervalToHistory oldHistory lastIntervalDatum startDate mEndDate
      newIntervalId = membershipIntervalId newInterval
      newIntervalNFT = V1.assetClassValue newIntervalId 1
   in -- Must spend user NFT of the organization profile who is creating the membership interval -- enforced here
      -- Must spend the UTxO of the updated membership history (with ListNodeDatum) -- enforced here
      -- Must lock the updated membership history with inline datum at membershipsValidator address (output idx) -- enforced by membershipsValidator
      -- Must reference the last interval from the membership validators with IntervalDatum -- enforced here
      -- Must mint the membership interval NFT -- enforced both here and in membershipsValidator
      -- Must lock the membership interval NFT with inline datum at membershipsValidator address (output idx) -- enforced here

      traceIfFalse "M" (startDate `before` txInfoValidRange) -- Start date before validity
        && traceIfFalse "N" (isOrganizationValid && isMembershipNodeIdValid) -- CS check
        && traceIfFalse "O" (V1.assetClassValueOf (valueSpent txInfo) organizationUserAC == 1) -- Must spend org User NFT
        && traceIfFalse "P" (Utils.hasTxInAtAddressWithNFT membershipNodeId membershipsValidatorAddress txInfoInputs) -- Must spend history node
        && traceIfFalse "Q" (mintValueMinted txInfoMint == newIntervalNFT) -- Exact mint check
        && traceIfFalse "R" (Utils.checkTxOutAtIndexWithDatumValueAndAddress intervalOutputIdx (IntervalDatum newInterval) (minLv + newIntervalNFT) membershipsValidatorAddress txInfoOutputs) -- Lock interval at MV

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
