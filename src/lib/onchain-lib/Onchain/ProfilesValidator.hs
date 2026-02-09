-- Required for `makeLift`:
{-# LANGUAGE MultiParamTypeClasses #-}
-- Required for `makeLift`:
{-# LANGUAGE ScopedTypeVariables #-}
---
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:optimize #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

{-# HLINT ignore "Use &&" #-}

module Onchain.ProfilesValidator where

import GHC.Generics (Generic)
import Onchain.BJJ (intToBelt)
import Onchain.CIP68 (CIP68Datum (CIP68Datum), ImageURI, deriveUserFromRefAC, updateCIP68DatumImage, validateImageURI)
import Onchain.Protocol (OnchainProfile (..), OnchainRank (..), RankId, getCurrentRankId, hasCurrencySymbol, promoteProfile, protocolParams, ranksValidatorScriptHash, unsafeGetRankDatumAndValue)
import Onchain.Protocol qualified as Onchain
import Onchain.Utils qualified as Utils
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.Contexts
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude
import Prelude qualified

-- | Custom redeemer :
-- NOTE: Profile deletion is intentionally not supported to preserve lineage integrity.
-- BJJ belt records are permanent historical facts that should not be erasable.
-- Output indices are passed in the redeemer for efficient O(1) lookup instead of O(n) search.
data ProfilesRedeemer
  = -- | UpdateProfileImage profileId newImageURI profileOutputIdx
    UpdateProfileImage ImageURI Integer
  | -- | AcceptPromotion rankId profileOutputIdx rankOutputIdx
    AcceptPromotion RankId Integer Integer
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''ProfilesRedeemer [('UpdateProfileImage, 0), ('AcceptPromotion, 1)]

type ProfilesDatum = CIP68Datum Onchain.OnchainProfile

--------------------------------------
-- Profiles Validator
--------------------------------------

{-# INLINEABLE profilesLambda #-}
profilesLambda :: ScriptContext -> Bool
profilesLambda (ScriptContext txInfo@TxInfo {..} (Redeemer bredeemer) scriptInfo) =
  let redeemer = unsafeFromBuiltinData @ProfilesRedeemer bredeemer
   in case scriptInfo of
        (SpendingScript spendingTxOutRef mdatum) -> case mdatum of
          Nothing -> traceError "No datum"
          Just (Datum bdatum) -> case fromBuiltinData bdatum of
            Nothing -> traceError "Invalid datum"
            Just profileDatum@(CIP68Datum _metadata _version (profile :: OnchainProfile)) ->
              let ownInput = Utils.unsafeFindOwnInputByTxOutRef spendingTxOutRef txInfoInputs
                  ownValue = txOutValue ownInput
                  ownAddress = txOutAddress ownInput
                  profileRefAssetClass@(V1.AssetClass (mintingPolicyCurrencySymbol, _)) = profileId profile --  as in datum.
                  -- It is important to validate that the value of this UTxO contains an NFT of this asset class.
               in case redeemer of
                    (UpdateProfileImage newImageURI profileOutputIdx) ->
                      let newCip68Datum = updateCIP68DatumImage newImageURI profileDatum
                          profileUserAssetClass = deriveUserFromRefAC profileRefAssetClass
                       in and
                            [ traceIfFalse "Own value must contain profile Ref NFT"
                                $ V1.assetClassValueOf ownValue profileRefAssetClass
                                == 1,
                              traceIfFalse "Must spend profile User NFT"
                                $ V1.assetClassValueOf (valueSpent txInfo) profileUserAssetClass
                                == 1,
                              -- Validate image URI size to prevent oversized datums
                              validateImageURI newImageURI,
                              traceIfFalse "Must lock profile Ref NFT with inline updated datum at profilesValidator address (output idx)"
                                $ Utils.checkTxOutAtIndexWithDatumValueAndAddress profileOutputIdx newCip68Datum ownValue ownAddress txInfoOutputs -- Guarantees that tokens never leaves the validator.
                            ]
                    (AcceptPromotion promotionId profileOutputIdx rankOutputIdx) ->
                      let isValidPromotionId = hasCurrencySymbol promotionId mintingPolicyCurrencySymbol
                          ranksValidatorAddress = V1.scriptHashAddress $ ranksValidatorScriptHash $ protocolParams profile
                          (promotionValue, pendingRankDatum) = unsafeGetRankDatumAndValue promotionId ranksValidatorAddress txInfoInputs

                          -- Get current rank from reference inputs to validate promotion is still valid
                          currentRankId = getCurrentRankId profile
                          (_, currentRankDatum) = unsafeGetRankDatumAndValue currentRankId ranksValidatorAddress txInfoReferenceInputs

                          -- Validate the promotion is still valid given current state
                          currentBelt = intToBelt $ rankNumber currentRankDatum
                          currentBeltDate = rankAchievementDate currentRankDatum
                          nextBelt = intToBelt $ promotionRankNumber pendingRankDatum
                          nextBeltDate = promotionAchievementDate pendingRankDatum

                          isPromotionStillValid =
                            and
                              [ traceIfFalse
                                  "Promotion invalid - promotion ID must have correct currency symbol"
                                  isValidPromotionId,
                                traceIfFalse "Promotion invalid - already at or past this rank"
                                  $ nextBelt
                                  > currentBelt,
                                traceIfFalse "Promotion invalid - achievement date must be after current rank date"
                                  $ nextBeltDate
                                  > currentBeltDate
                              ]

                          (updatedProfileCIP68Datum, newRankDatum) = promoteProfile profileDatum pendingRankDatum
                       in -- NOTE: User NFT consent check is NOT needed here because:
                          -- 1. AcceptPromotion reads the promotion from txInfoInputs (line 84)
                          -- 2. This means the Promotion UTxO at RanksValidator MUST be spent
                          -- 3. RanksValidator always runs when Promotion UTxO is spent
                          -- 4. RanksValidator checks User NFT consent (deriveUserFromRefAC)
                          -- Therefore, RanksValidator guarantees user consent for this transaction.
                          and
                            [ traceIfFalse "Own value must contain profile Ref NFT"
                                $ V1.assetClassValueOf ownValue profileRefAssetClass
                                == 1,
                              isPromotionStillValid,
                              traceIfFalse "Must lock profile Ref NFT with inline updated datum at profilesValidator address (output idx)"
                                $ Utils.checkTxOutAtIndexWithDatumValueAndAddress profileOutputIdx updatedProfileCIP68Datum ownValue ownAddress txInfoOutputs, -- Guarantees that tokens never leaves the validator.
                              traceIfFalse "Must lock rank NFT with inline datum at ranksValidator address (output idx)"
                                $ Utils.checkTxOutAtIndexWithDatumValueAndAddress rankOutputIdx newRankDatum promotionValue ranksValidatorAddress txInfoOutputs
                            ]
        _ -> traceError "Invalid purpose"

-- | Lose the types
profilesUntyped :: BuiltinData -> BuiltinUnit
profilesUntyped = Utils.mkUntypedLambda profilesLambda

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
profilesCompile :: CompiledCode (BuiltinData -> BuiltinUnit)
profilesCompile = $$(compile [||profilesUntyped||])
