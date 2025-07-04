{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use &&" #-}



module Onchain.RanksValidator where

import Onchain.Protocol
import Onchain.Utils
import PlutusLedgerApi.V3

import PlutusTx.Prelude

import PlutusTx
import qualified PlutusLedgerApi.V1 as V1
import Onchain.BJJ (validatePromotion, intToBelt)
import PlutusLedgerApi.V3.Contexts



-----------------------------------------------------------------------------
-- Ranks Validator
-----------------------------------------------------------------------------

{-# INLINEABLE ranksLambda #-}
ranksLambda :: ScriptContext -> Bool
ranksLambda (ScriptContext txInfo@TxInfo{..} (Redeemer _) scriptInfo) =
  case scriptInfo of
        (SpendingScript spendingTxOutRef mdatum) -> case mdatum of
          Nothing -> traceError "No datum"
          Just (Datum bdatum) -> case fromBuiltinData bdatum of
            Nothing -> traceError "Invalid datum"
            Just (studentNextRank :: Rank) ->
              let ownInput = unsafeFindOwnInputByTxOutRef spendingTxOutRef txInfoInputs
                  -- ownValue = txOutValue ownInput
                  ownAddress = txOutAddress ownInput

                  profilesValidatorAddr = V1.scriptHashAddress ( profilesValidatorScriptHash $ pendingRankProtocolParams studentNextRank)
                  
                  -- Getting profiles based on the ids in the rank promotion datum
                  (_, studentProfile) = unsafeGetProfileDatumAndValue (rankAchievedByProfileId studentNextRank) profilesValidatorAddr txInfoReferenceInputs
                  (_, masterProfile) = unsafeGetProfileDatumAndValue (rankAwardedByProfileId studentNextRank) profilesValidatorAddr txInfoReferenceInputs

                   -- Getting ranks data based on the rank ids in the profiles datums  
                  studentCurrentRankId = getCurrentRank studentProfile -- Fails if if profile is an organization
                  masterCurrentRankId = getCurrentRank masterProfile -- Fails if if profile is an organization
                  (_, studentCurrentRank) = unsafeGetRankDatumAndValue studentCurrentRankId ownAddress txInfoReferenceInputs
                  (_, masterRank) = unsafeGetRankDatumAndValue masterCurrentRankId ownAddress txInfoReferenceInputs

                  -- Getting belts and dates from ranks

                  masterBelt =  intToBelt $ rankNumber masterRank
                  masterBeltDate = rankAchievementDate masterRank
                  studentCurrentBelt = intToBelt $ rankNumber studentCurrentRank
                  studentCurrentBeltDate = rankAchievementDate studentCurrentRank
                  studentNextBelt = intToBelt $ rankNumber studentNextRank
                  studentNextBeltDate = rankAchievementDate studentNextRank
                  
                  isPromotionValid = validatePromotion masterBelt masterBeltDate studentCurrentBelt studentCurrentBeltDate studentNextBelt studentNextBeltDate 
                  
                  profileUserAssetClass =  profileId studentProfile
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
