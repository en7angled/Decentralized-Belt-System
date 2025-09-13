{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}


{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-optimize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-remove-trace #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}
{-# HLINT ignore "Use &&" #-}

module Onchain.RanksValidator where

import Onchain.Protocol (OnchainRank(..), getCurrentRankId, unsafeGetRankDatumAndValue, unsafeGetProfileDatumAndValue, profilesValidatorScriptHash, ranksValidatorScriptHash)
import Onchain.Utils
import PlutusLedgerApi.V3

import PlutusTx.Prelude

import PlutusTx
import qualified PlutusLedgerApi.V1 as V1
import Onchain.BJJ (validatePromotion, intToBelt)
import PlutusLedgerApi.V3.Contexts
import Onchain.CIP68 (deriveUserFromRefAC)



-----------------------------------------------------------------------------
-- Ranks Validator
-----------------------------------------------------------------------------

{-# INLINEABLE ranksLambda #-}
ranksLambda :: ScriptContext -> Bool
ranksLambda (ScriptContext txInfo@TxInfo{..} (Redeemer _) scriptInfo) =
  case scriptInfo of
        (SpendingScript _spendingTxOutRef mdatum) -> case mdatum of
          Nothing -> traceError "No datum"
          Just (Datum bdatum) -> case fromBuiltinData bdatum of
            Nothing -> traceError "Invalid datum"
            Just (promotionRank :: OnchainRank) ->
              let  
                  -- ownInput = unsafeFindOwnInputByTxOutRef spendingTxOutRef txInfoInputs
                  -- ownValue = txOutValue ownInput
                  -- ownAddress = txOutAddress ownInput

                  profilesValidatorAddr = V1.scriptHashAddress ( profilesValidatorScriptHash $ promotionProtocolParams promotionRank)
                  ranksValidatorAddr = V1.scriptHashAddress ( ranksValidatorScriptHash $ promotionProtocolParams promotionRank)
                  
                  studentProfileId = promotionAwardedTo promotionRank
                  masterProfileId = promotionAwardedBy promotionRank


                  -- Must have student and master profiles NFTs as reference inputs
                  -- Getting profiles based on the ids in the rank promotion datum
                  (_, studentProfile) = unsafeGetProfileDatumAndValue studentProfileId profilesValidatorAddr txInfoInputs
                  (_, masterProfile) = unsafeGetProfileDatumAndValue masterProfileId profilesValidatorAddr txInfoReferenceInputs

                   -- Must have student and master ranks NFTs as reference inputs
                   -- Getting ranks data based on the rank ids in the profiles datums  
                  studentCurrentRankId = getCurrentRankId studentProfile -- Fails if if profile is an organization
                  masterCurrentRankId = getCurrentRankId masterProfile -- Fails if if profile is an organization
                  (_, studentCurrentRank) = unsafeGetRankDatumAndValue studentCurrentRankId ranksValidatorAddr txInfoReferenceInputs
                  (_, masterRank) = unsafeGetRankDatumAndValue masterCurrentRankId ranksValidatorAddr txInfoReferenceInputs

                  -- Getting belts and dates from ranks

                  masterBelt =  intToBelt $ rankNumber masterRank
                  masterBeltDate = rankAchievementDate masterRank
                  studentCurrentBelt = intToBelt $ rankNumber studentCurrentRank
                  studentCurrentBeltDate = rankAchievementDate studentCurrentRank
                  studentNextBelt = intToBelt $ promotionRankNumber promotionRank
                  studentNextBeltDate = promotionAchievementDate promotionRank
                  
                  isPromotionValid = validatePromotion masterBelt masterBeltDate studentCurrentBelt studentCurrentBeltDate studentNextBelt studentNextBeltDate 
                  
                  profileUserAssetClass =  deriveUserFromRefAC studentProfileId
                in and 
                    [
                      traceIfFalse "Must spend profile User NFT"
                                  $ V1.assetClassValueOf (valueSpent txInfo) profileUserAssetClass
                                  == 1
                    , isPromotionValid
                    ]
        _ -> traceError "Invalid purpose"

-- | Lose the types
ranksUntyped :: BuiltinData -> BuiltinUnit
ranksUntyped = mkUntypedLambda ranksLambda

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
ranksCompile :: CompiledCode (BuiltinData -> BuiltinUnit)
ranksCompile = $$(compile [||ranksUntyped||])
