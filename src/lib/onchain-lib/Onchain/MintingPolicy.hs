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
import Onchain.Protocol (MembershipDatum (..), MembershipHistoriesListNodeId, OnchainProfileType (..), OnchainMembershipHistory (..), OnchainRank (..), ProfileId, ProtocolParams, addMembershipIntervalToHistory, deriveMembershipHistoriesListId, derivePromotionRankId, getCurrentRankId, initEmptyMembershipHistoriesList, initMembershipHistory, membershipIntervalId, membershipsValidatorScriptHash, mkOrganizationProfile, mkPromotion, mkPractitionerProfile, oracleToken, profilesValidatorScriptHash, ranksValidatorScriptHash, unsafeGetListNodeDatumAndValue, unsafeGetMembershipHistory, unsafeGetMembershipInterval, unsafeGetProfile, unsafeGetRank)
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
           in -- Global gate: protocol must not be paused
              traceIfFalse "Protocol is paused" (not (opPaused oracle))
                && case redeemer of
                  CreateProfile seedTxOutRef metadata profileType creationDate rankNumber profileOutputIdx rankOrMembershipRootOutputIdx ->
                    checkFee oracle fcProfileCreationFee txInfoOutputs
                      && handleCreateProfile protocolParams txInfo mintingPolicyCurrencySymbol profilesValidatorAddress minLv seedTxOutRef metadata profileType creationDate rankNumber profileOutputIdx rankOrMembershipRootOutputIdx
                  Promote seedTxOutRef studentProfileId masterProfileId achievementDate nextRankNum pendingRankOutputIdx ->
                    checkFee oracle fcPromotionFee txInfoOutputs
                      && handlePromote protocolParams txInfo mintingPolicyCurrencySymbol profilesValidatorAddress minLv seedTxOutRef studentProfileId masterProfileId achievementDate nextRankNum pendingRankOutputIdx
                  NewMembershipHistory organizationProfileId practitionerId startDate endDate leftNodeId firstIntervalOutputIdx ->
                    checkFee oracle fcMembershipFee txInfoOutputs
                      && handleNewMembershipHistory protocolParams txInfo mintingPolicyCurrencySymbol minLv organizationProfileId practitionerId startDate endDate leftNodeId firstIntervalOutputIdx
                  NewMembershipInterval organizationProfileId membershipNodeId startDate mEndDate intervalOutputIdx ->
                    checkFee oracle fcMembershipFee txInfoOutputs
                      && handleNewMembershipInterval protocolParams txInfo mintingPolicyCurrencySymbol minLv organizationProfileId membershipNodeId startDate mEndDate intervalOutputIdx
        _ -> traceError "Invalid purpose"

-------------------------------------------------------------------------------

-- * Per-Redeemer Handlers

-------------------------------------------------------------------------------

{-# INLINEABLE handleCreateProfile #-}
handleCreateProfile ::
  ProtocolParams ->
  TxInfo ->
  CurrencySymbol ->
  Address ->
  Value ->
  TxOutRef ->
  MetadataFields ->
  OnchainProfileType ->
  POSIXTime ->
  Integer ->
  Integer ->
  Integer ->
  Bool
handleCreateProfile protocolParams TxInfo {..} mintingPolicyCurrencySymbol profilesValidatorAddress minLv seedTxOutRef metadata profileType creationDate rankNumber profileOutputIdx rankOrMembershipRootOutputIdx =
  -- NOTE: rankNumber validation handled by intToBelt (fails for values outside 0-14)
  let (profileRefTN, profileUserTN) = generateRefAndUserTN $ Utils.nameFromTxOutRef seedTxOutRef
      profileRefAssetClass = V1.AssetClass (mintingPolicyCurrencySymbol, profileRefTN)
      profileUserAssetClass = V1.AssetClass (mintingPolicyCurrencySymbol, profileUserTN)
      profileRefNFT = V1.assetClassValue profileRefAssetClass 1
      profileUserNFT = V1.assetClassValue profileUserAssetClass 1
   in and
        [ traceIfFalse "Creation date must be before the tx validity range"
            $ creationDate
            `before` txInfoValidRange,
          traceIfFalse "Must spend seed TxOutRef"
            $ any ((== seedTxOutRef) . txInInfoOutRef) txInfoInputs,
          -- Validate metadata field sizes to prevent oversized datums
          validateMetadataFields metadata
        ]
        && case profileType of
          Practitioner ->
            let (profile, rankDatum) = mkPractitionerProfile profileRefAssetClass creationDate protocolParams rankNumber
                profileDatum = mkCIP68Datum profile metadata
                rankAssetClass = rankId rankDatum
                rankNFT = V1.assetClassValue rankAssetClass 1
                ranksValidatorAddress = V1.scriptHashAddress (ranksValidatorScriptHash protocolParams)
             in and
                  [ traceIfFalse "Must lock profile Ref NFT with inline datum at profilesValidator address (output idx)"
                      $ Utils.checkTxOutAtIndexWithDatumValueAndAddress profileOutputIdx profileDatum (profileRefNFT + minLv) profilesValidatorAddress txInfoOutputs,
                    traceIfFalse "Tx must mint JUST OnchainProfile Ref and User NFTs and Rank NFT"
                      $ mintValueMinted txInfoMint -- protection against other-token-name attack vector
                      == (profileRefNFT + profileUserNFT + rankNFT),
                    traceIfFalse "Must lock rank NFT with inline datum at ranksValidator address (output idx)"
                      $ Utils.checkTxOutAtIndexWithDatumValueAndAddress rankOrMembershipRootOutputIdx rankDatum (rankNFT + minLv) ranksValidatorAddress txInfoOutputs
                  ]
          Organization ->
            let profile = mkOrganizationProfile profileRefAssetClass protocolParams
                profileDatum = mkCIP68Datum profile metadata
                membershipHistoriesRootDatum = initEmptyMembershipHistoriesList profileRefAssetClass
                membershipHistoriesRootAssetClass = deriveMembershipHistoriesListId profileRefAssetClass
                membershipHistoriesRootNFT = V1.assetClassValue membershipHistoriesRootAssetClass 1
                membershipsValidatorAddress = V1.scriptHashAddress (membershipsValidatorScriptHash protocolParams)
             in and
                  [ traceIfFalse "Must lock profile Ref NFT with inline datum at profilesValidator address (output idx)"
                      $ Utils.checkTxOutAtIndexWithDatumValueAndAddress profileOutputIdx profileDatum (profileRefNFT + minLv) profilesValidatorAddress txInfoOutputs,
                    traceIfFalse "Must lock membership histories root NFT with inline datum at membershipsValidator address (output idx)"
                      $ Utils.checkTxOutAtIndexWithDatumValueAndAddress rankOrMembershipRootOutputIdx (ListNodeDatum membershipHistoriesRootDatum) (membershipHistoriesRootNFT + minLv) membershipsValidatorAddress txInfoOutputs,
                    traceIfFalse "Tx must mint JUST Ref, User NFTs and Membership Histories Root NFT"
                      $ mintValueMinted txInfoMint -- protection against other-token-name attack vector
                      == (profileRefNFT + profileUserNFT + membershipHistoriesRootNFT)
                  ]

{-# INLINEABLE handlePromote #-}
handlePromote ::
  ProtocolParams ->
  TxInfo ->
  CurrencySymbol ->
  Address ->
  Value ->
  TxOutRef ->
  ProfileId ->
  ProfileId ->
  POSIXTime ->
  Integer ->
  Integer ->
  Bool
handlePromote protocolParams txInfo@TxInfo {..} mintingPolicyCurrencySymbol profilesValidatorAddress minLv seedTxOutRef studentProfileId masterProfileId achievementDate nextRankNum pendingRankOutputIdx =
  let ranksValidatorAddress = V1.scriptHashAddress (ranksValidatorScriptHash protocolParams)
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
   in and
        [ traceIfFalse
            "Profiles must have correct currency symbol"
            (isStudentValid && isMasterValid),
          traceIfFalse "Must spend seed TxOutRef for uniqueness"
            $ any ((== seedTxOutRef) . txInInfoOutRef) txInfoInputs,
          traceIfFalse "Must spend user NFT of the profile who awards the promotion"
            $ V1.assetClassValueOf (valueSpent txInfo) masterUserAC
            == 1,
          traceIfFalse "Tx must mint JUST the pending rank NFT"
            $ mintValueMinted txInfoMint
            == pendingRankNFT,
          traceIfFalse "Must lock pending rank NFT with inline datum at ranksValidator address (output idx)"
            $ Utils.checkTxOutAtIndexWithDatumValueAndAddress pendingRankOutputIdx pendingRankDatum (pendingRankNFT + minLv) ranksValidatorAddress txInfoOutputs,
          traceIfFalse
            "Must pass promotion validation rules"
            isPromotionValid
        ]

{-# INLINEABLE handleNewMembershipHistory #-}
handleNewMembershipHistory ::
  ProtocolParams ->
  TxInfo ->
  CurrencySymbol ->
  Value ->
  ProfileId ->
  ProfileId ->
  POSIXTime ->
  Maybe POSIXTime ->
  MembershipHistoriesListNodeId ->
  Integer ->
  Bool
handleNewMembershipHistory protocolParams txInfo@TxInfo {..} mintingPolicyCurrencySymbol minLv organizationProfileId practitionerId startDate endDate leftNodeId firstIntervalOutputIdx =
  let organizationUserAC = deriveUserFromRefAC organizationProfileId
      isOrganizationValid = hasCurrencySymbol organizationProfileId mintingPolicyCurrencySymbol
      isPractitionerValid = hasCurrencySymbol practitionerId mintingPolicyCurrencySymbol
      isLeftNodeIdValid = hasCurrencySymbol leftNodeId mintingPolicyCurrencySymbol
      membershipsValidatorAddress = V1.scriptHashAddress (membershipsValidatorScriptHash protocolParams)

      (newHistory, fstInterval) = initMembershipHistory practitionerId organizationProfileId startDate endDate

      membershipHistoryNFT = V1.assetClassValue (membershipHistoryId newHistory) 1
      fstIntervalNFT = V1.assetClassValue (membershipIntervalId fstInterval) 1
   in -- Must spend user NFT of the organization profile who is creating the membership history -- enforced here
      -- Must spend the UTxO of the left node of the membership history list (containing LeftNodeId NFT) from membershipsValidator address -- enforced here
      -- Must lock the updated left node with inline datum at membershipsValidator address (output idx) -- enforced by membershipsValidator
      -- Must mint the membership history NFT and the membership interval NFT -- enforced both here and in membershipsValidator
      -- Must lock the membership history NFT with inline datum at membershipsValidator address (output idx) -- enforced by membershipsValidator
      -- Must lock the membership interval NFT with inline datum at membershipsValidator address (output idx)
      and
        [ traceIfFalse "Start date must be before tx validity range"
            $ startDate
            `before` txInfoValidRange,
          traceIfFalse
            "Organization profile, practitioner and left node must have correct currency symbol"
            (isOrganizationValid && isPractitionerValid && isLeftNodeIdValid),
          traceIfFalse "Must spend user NFT of the organization profile who is creating the membership history"
            $ V1.assetClassValueOf (valueSpent txInfo) organizationUserAC
            == 1,
          traceIfFalse "Must spend left node of the membership history list from membershipsValidator address"
            $ Utils.hasTxInAtAddressWithNFT leftNodeId membershipsValidatorAddress txInfoInputs,
          traceIfFalse
            "Tx must mint JUST inserted node NFT and interval NFT"
            $ mintValueMinted
              txInfoMint -- protection against other-token-name attack vector
            == (membershipHistoryNFT + fstIntervalNFT),
          traceIfFalse "Must lock the first interval with inline datum at membershipsValidator address (output idx)"
            $ Utils.checkTxOutAtIndexWithDatumValueAndAddress firstIntervalOutputIdx (IntervalDatum fstInterval) (fstIntervalNFT + minLv) membershipsValidatorAddress txInfoOutputs
        ]

{-# INLINEABLE handleNewMembershipInterval #-}
handleNewMembershipInterval ::
  ProtocolParams ->
  TxInfo ->
  CurrencySymbol ->
  Value ->
  ProfileId ->
  MembershipHistoriesListNodeId ->
  POSIXTime ->
  Maybe POSIXTime ->
  Integer ->
  Bool
handleNewMembershipInterval protocolParams txInfo@TxInfo {..} mintingPolicyCurrencySymbol minLv organizationProfileId membershipNodeId startDate mEndDate intervalOutputIdx =
  let organizationUserAC = deriveUserFromRefAC organizationProfileId
      isOrganizationValid = hasCurrencySymbol organizationProfileId mintingPolicyCurrencySymbol
      isMembershipNodeIdValid = hasCurrencySymbol membershipNodeId mintingPolicyCurrencySymbol
      membershipsValidatorAddress = V1.scriptHashAddress (membershipsValidatorScriptHash protocolParams)

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

      and
        [ traceIfFalse "Start date must be before tx validity range"
            $ startDate
            `before` txInfoValidRange,
          traceIfFalse
            "Organization profile and membership node must have correct currency symbol"
            (isOrganizationValid && isMembershipNodeIdValid),
          traceIfFalse "Must spend user NFT of the organization profile who is creating the membership interval"
            $ V1.assetClassValueOf (valueSpent txInfo) organizationUserAC
            == 1,
          traceIfFalse "Must spend the UTxO of the updated membership history (with IntervalDatum) from membershipsValidator address"
            $ Utils.hasTxInAtAddressWithNFT membershipNodeId membershipsValidatorAddress txInfoInputs,
          traceIfFalse "Tx must mint JUST interval NFT"
            $ mintValueMinted txInfoMint -- protection against other-token-name attack vector
            == newIntervalNFT,
          traceIfFalse "Must lock interval with inline datum at membershipsValidator address (output idx)"
            $ Utils.checkTxOutAtIndexWithDatumValueAndAddress intervalOutputIdx (IntervalDatum newInterval) (minLv + newIntervalNFT) membershipsValidatorAddress txInfoOutputs
        ]

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
