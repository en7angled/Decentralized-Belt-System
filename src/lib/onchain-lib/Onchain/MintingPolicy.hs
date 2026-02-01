-- Required for `makeLift`:
---
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use &&" #-}

module Onchain.MintingPolicy where

import GHC.Generics (Generic)
import Onchain.BJJ (intToBelt, validatePromotion)
import Onchain.CIP68 (CIP68Datum, MetadataFields, deriveUserFromRefAC, generateRefAndUserTN, mkCIP68Datum, validateMetadataFields)
import Onchain.Protocol (OnChainProfileType (..), OnchainProfile (..), OnchainRank (..), ProfileId, ProtocolParams, generatePromotionRankId, getCurrentRankId, mkOrganizationProfile, mkPendingRank, mkPractitionerProfile, profilesValidatorScriptHash, ranksValidatorScriptHash, unsafeGetProfileDatumAndValue, unsafeGetRankDatumAndValue)
import Onchain.Utils hiding (hasTxOutWithInlineDatumAndValue)
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

-- | Custom redeemer :
-- NOTE: Profile deletion (burning) is intentionally not supported to preserve lineage integrity.
-- BJJ belt records are permanent historical facts that should not be erasable.
-- Output indices are passed in the redeemer for efficient O(1) lookup instead of O(n) search.
data MintingRedeemer
  = CreateProfile TxOutRef MetadataFields OnChainProfileType POSIXTime Integer Integer Integer
  -- ^ CreateProfile seedTxOutRef metadata profileType creationDate rankNumber profileOutputIdx rankOutputIdx
  -- For Organization profiles, rankOutputIdx is ignored (can be set to -1 or any value)
  | Promote TxOutRef ProfileId ProfileId POSIXTime Integer Integer
  -- ^ Promote seedTxOutRef promotedProfileId promoterProfileId achievementDate rankNumber pendingRankOutputIdx
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''MintingRedeemer [('CreateProfile, 0), ('Promote, 1)]

type ProfilesDatum = CIP68Datum OnchainProfile

--------------------------------------
-- Minting Policy
--------------------------------------

{-# INLINEABLE mintingPolicyLambda #-}
mintingPolicyLambda :: ProtocolParams -> ScriptContext -> Bool
mintingPolicyLambda protocolParams (ScriptContext txInfo@TxInfo {..} (Redeemer bredeemer) scriptInfo) =
  let redeemer = unsafeFromBuiltinData @MintingRedeemer bredeemer
      minValue = V1.lovelaceValue 3_500_000
   in case scriptInfo of
        (MintingScript mintingPolicyCurrencySymbol) ->
          let profilesValidatorAddress = V1.scriptHashAddress (profilesValidatorScriptHash protocolParams)
           in case redeemer of
                CreateProfile seedTxOutRef metadata profileType creationDate rankNumber profileOutputIdx rankOutputIdx ->
                  -- NOTE: rankNumber validation handled by intToBelt (fails for values outside 0-14)
                  let (profileRefTN, profileUserTN) = generateRefAndUserTN $ nameFromTxOutRef seedTxOutRef
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
                                      $ Utils.checkTxOutAtIndex profileOutputIdx profileDatum (profileRefNFT + minValue) profilesValidatorAddress txInfoOutputs,
                                    traceIfFalse "Tx must mint JUST OnchainProfile Ref and User NFTs and Rank NFT"
                                      $ mintValueMinted txInfoMint -- protection against other-token-name attack vector
                                      == (profileRefNFT + profileUserNFT + rankNFT),
                                    traceIfFalse "Must lock rank NFT with inline datum at ranksValidator address (output idx)"
                                      $ Utils.checkTxOutAtIndex rankOutputIdx rankDatum (rankNFT + minValue) ranksValidatorAddress txInfoOutputs
                                  ]
                          Organization ->
                            let profile = mkOrganizationProfile profileRefAssetClass protocolParams
                                profileDatum = mkCIP68Datum profile metadata
                             in and
                                  [ traceIfFalse "Must lock profile Ref NFT with inline datum at profilesValidator address (output idx)"
                                      $ Utils.checkTxOutAtIndex profileOutputIdx profileDatum (profileRefNFT + minValue) profilesValidatorAddress txInfoOutputs,
                                    traceIfFalse "Tx must mint JUST Ref and User NFTs"
                                      $ mintValueMinted txInfoMint -- protection against other-token-name attack vector
                                      == (profileRefNFT + profileUserNFT)
                                  ]
                (Promote seedTxOutRef studentProfileId masterProfileId achievementDate nextRankNum pendingRankOutputIdx) ->
                  let ranksValidatorAddress = V1.scriptHashAddress (ranksValidatorScriptHash protocolParams)

                      -- Generate unique promotion rank ID from seed
                      pendingRankAssetClass = generatePromotionRankId seedTxOutRef mintingPolicyCurrencySymbol
                      pendingRankNFT = V1.assetClassValue pendingRankAssetClass 1
                      pendingRankDatum = mkPendingRank seedTxOutRef mintingPolicyCurrencySymbol studentProfileId masterProfileId achievementDate nextRankNum protocolParams

                      -- Master must spend their user NFT to authorize promotion
                      masterUserAC = deriveUserFromRefAC masterProfileId

                      -- Get student and master profiles from reference inputs
                      (_, studentProfile) = unsafeGetProfileDatumAndValue studentProfileId profilesValidatorAddress txInfoReferenceInputs
                      (_, masterProfile) = unsafeGetProfileDatumAndValue masterProfileId profilesValidatorAddress txInfoReferenceInputs

                      -- Get current ranks from reference inputs
                      studentCurrentRankId = getCurrentRankId studentProfile
                      masterCurrentRankId = getCurrentRankId masterProfile
                      (_, studentCurrentRank) = unsafeGetRankDatumAndValue studentCurrentRankId ranksValidatorAddress txInfoReferenceInputs
                      (_, masterRank) = unsafeGetRankDatumAndValue masterCurrentRankId ranksValidatorAddress txInfoReferenceInputs

                      -- Validate the promotion using BJJ rules
                      masterBelt = intToBelt $ rankNumber masterRank
                      masterBeltDate = rankAchievementDate masterRank
                      studentCurrentBelt = intToBelt $ rankNumber studentCurrentRank
                      studentCurrentBeltDate = rankAchievementDate studentCurrentRank
                      studentNextBelt = intToBelt nextRankNum
                      studentNextBeltDate = achievementDate

                      isPromotionValid = validatePromotion masterBelt masterBeltDate studentCurrentBelt studentCurrentBeltDate studentNextBelt studentNextBeltDate
                   in and
                        [ traceIfFalse "Must spend seed TxOutRef for uniqueness"
                            $ any ((== seedTxOutRef) . txInInfoOutRef) txInfoInputs,
                          traceIfFalse "Must spend user NFT of the profile who awards the promotion"
                            $ V1.assetClassValueOf (valueSpent txInfo) masterUserAC
                            == 1,
                          traceIfFalse "Tx must mint JUST the pending rank NFT"
                            $ mintValueMinted txInfoMint
                            == pendingRankNFT,
                          traceIfFalse "Must lock pending rank NFT with inline datum at ranksValidator address (output idx)"
                            $ Utils.checkTxOutAtIndex pendingRankOutputIdx pendingRankDatum (pendingRankNFT + minValue) ranksValidatorAddress txInfoOutputs,
                          isPromotionValid
                        ]
        _ -> traceError "Invalid purpose"

-- | Lose the types
{-# INLINEABLE mintingPolicyUntyped #-}
mintingPolicyUntyped :: ProtocolParams -> BuiltinData -> BuiltinUnit
mintingPolicyUntyped params =
  mkUntypedLambda (mintingPolicyLambda params)

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
mintingPolicyCompile :: ProtocolParams -> CompiledCode (BuiltinData -> BuiltinUnit)
mintingPolicyCompile params = $$(compile [||mintingPolicyUntyped||]) `unsafeApplyCode` liftCode plcVersion110 params
