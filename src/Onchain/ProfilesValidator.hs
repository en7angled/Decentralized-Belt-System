-- Required for `makeLift`:
{-# LANGUAGE MultiParamTypeClasses #-}
-- Required for `makeLift`:
{-# LANGUAGE ScopedTypeVariables #-}
---
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-optimize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-remove-trace #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

{-# HLINT ignore "Use &&" #-}

module Onchain.ProfilesValidator where

import GHC.Generics (Generic)
import Onchain.CIP68 (CIP68Datum (CIP68Datum), deriveUserFromRefTN, updateCIP68DatumImage, ImageURI)
import Onchain.Types
import Onchain.Types qualified as Onchain
import Onchain.Utils
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.Contexts
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude
import Prelude qualified




-- | Custom redeemer :
data ProfilesRedeemer
  = UpdateProfileImage V1.AssetClass ImageURI
  | DeleteProfile V1.AssetClass
  | AcceptPromotion PromotionId
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''ProfilesRedeemer [('UpdateProfileImage, 0), ('DeleteProfile, 1), ('AcceptPromotion, 2)]

type ProfilesDatum = CIP68Datum Onchain.Profile



unsafeGetPromotionDatumAndValue :: V1.AssetClass -> Address -> [TxInInfo] -> (Value, Promotion)
unsafeGetPromotionDatumAndValue ac addr txins =
  let (v, b) = unsafeGetCurrentStateDatumAndValue ac addr txins
   in (v, unsafeFromBuiltinData b)
{-# INLINEABLE unsafeGetPromotionDatumAndValue #-}



--------------------------------------
-- Profiles Validator
--------------------------------------

{-# INLINEABLE profilesLambda #-}
profilesLambda :: ScriptContext -> Bool
profilesLambda  (ScriptContext txInfo@TxInfo {..} (Redeemer bredeemer) scriptInfo) =
  let redeemer = unsafeFromBuiltinData @ProfilesRedeemer bredeemer

   in case scriptInfo of
        (SpendingScript spendingTxOutRef mdatum) -> case mdatum of
          Nothing -> traceError "No datum"
          Just (Datum bdatum) -> case fromBuiltinData bdatum of
            Nothing -> traceError "Invalid datum"
            Just profileDatum@(CIP68Datum metadata version (profile :: Profile)) ->
              let ownInput = unsafeFindOwnInputByTxOutRef spendingTxOutRef txInfoInputs
                  ownValue = txOutValue ownInput
                  ownAddress = txOutAddress ownInput
               in case redeemer of
                    DeleteProfile profileRefAssetClass@(V1.AssetClass (profilesCurrencySymbol, profileRefTN)) ->
                      let profileUserAssetClass = V1.AssetClass (profilesCurrencySymbol, deriveUserFromRefTN profileRefTN)
                       in and
                            [ traceIfFalse "Must burn profile Ref NFT"
                                $ isBurningNFT profileRefAssetClass txInfoMint,
                            traceIfFalse "Must spend profile User NFT"
                                $ V1.assetClassValueOf (valueSpent txInfo) profileUserAssetClass == 1
                            ]
                    (UpdateProfileImage (V1.AssetClass (profilesCurrencySymbol, profileRefTN)) newImageURI) ->
                      let newCip68Datum = updateCIP68DatumImage newImageURI profileDatum -- !!! Open unbounded-datum vulnerability on metadata
                          profileUserAssetClass = V1.AssetClass (profilesCurrencySymbol, deriveUserFromRefTN profileRefTN)
                      
                       in and
                            [ traceIfFalse "Must spend profile User NFT"
                                $ V1.assetClassValueOf (valueSpent txInfo) profileUserAssetClass == 1,
                              traceIfFalse "Must lock profile Ref NFT with inline updated datum at profilesValidator address"
                                $ hasTxOutWithInlineDatumAndValue newCip68Datum ownValue ownAddress txInfoOutputs
                            ]

                    (AcceptPromotion promotionId) ->
                      let     
                          ranksValidatorAddress =  V1.scriptHashAddress $ ranksValidatorScriptHash $ protocolParams profile
                          (promotionValue, promotionDatum) = unsafeGetPromotionDatumAndValue promotionId ranksValidatorAddress txInfoInputs
                          
                          (updatedProfile, newRankDatum) =  promoteProfile profile promotionDatum
                          updatedCip68Datum = CIP68Datum metadata version updatedProfile 
                          profileUserAssetClass = promotionAwardedTo promotionDatum
                       in and
                            [ traceIfFalse "Must spend profile User NFT"
                                $ V1.assetClassValueOf (valueSpent txInfo) profileUserAssetClass == 1,
                              traceIfFalse "Must lock profile Ref NFT with inline updated datum at profilesValidator address"
                                $ hasTxOutWithInlineDatumAndValue updatedCip68Datum ownValue ownAddress txInfoOutputs,
                              traceIfFalse "Must lock rank NFT with inline datum at ranksValidator address"
                              $ hasTxOutWithInlineDatumAndValue newRankDatum promotionValue ranksValidatorAddress txInfoOutputs  
                            ]

        _ -> traceError "Invalid purpose"

-- | Lose the types
profilesUntyped :: BuiltinData -> BuiltinUnit
profilesUntyped  = mkUntypedLambda profilesLambda

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
profilesCompile ::  CompiledCode (BuiltinData -> BuiltinUnit)
profilesCompile  = $$(compile [||profilesUntyped||])

