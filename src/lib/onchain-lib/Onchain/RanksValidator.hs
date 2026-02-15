{-# LANGUAGE MultiParamTypeClasses #-}
-- Required for `makeLift`:
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:optimize #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

{-# HLINT ignore "Use &&" #-}

-- | Ranks validator enforcing promotion acceptance and rank state transitions.
module Onchain.RanksValidator
  ( -- * Ranks Redeemer
    RanksRedeemer (..),

    -- * Ranks Validator
    ranksLambda,

    -- * Compilation
    ranksCompile,
  )
where

import Onchain.CIP68 (deriveUserFromRefAC)
import Onchain.Protocol (OnchainRank (..), profilesValidatorScriptHash, promoteProfile, unsafeGetProfileDatumAndValue)
import Onchain.Utils (mkUntypedLambda)
import Onchain.Utils qualified as Utils
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.Contexts
import GHC.Generics (Generic)
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude
import Prelude qualified

-------------------------------------------------------------------------------

-- * Ranks Redeemer

-------------------------------------------------------------------------------

-- This validator is simplified because promotion validation now happens
-- at mint time in the MintingPolicy.

data RanksRedeemer
  = -- | AcceptPromotion profileOutputIdx rankOutputIdx
    PromotionAcceptance Integer Integer
  | -- | Permissionless dust/griefing cleanup. Anyone can spend a UTxO at the
    -- validator address if its datum is absent or does not parse as a valid
    -- protocol datum. Legitimate protocol UTxOs (with valid datums) are rejected.
    Cleanup
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''RanksRedeemer [('PromotionAcceptance, 0), ('Cleanup, 1)]

-------------------------------------------------------------------------------

-- * Ranks Validator

-------------------------------------------------------------------------------

{-# INLINEABLE ranksLambda #-}
ranksLambda :: ScriptContext -> Bool
ranksLambda (ScriptContext txInfo@TxInfo {..} (Redeemer bredeemer) scriptInfo) =
  let redeemer = unsafeFromBuiltinData @RanksRedeemer bredeemer
   in case scriptInfo of
        (SpendingScript spendingTxOutRef mdatum) -> case redeemer of
          -- Permissionless cleanup: allow spending if datum is absent or unparseable.
          Cleanup -> case mdatum of
            Nothing -> True
            Just (Datum bd) -> case fromBuiltinData @OnchainRank bd of
              Nothing -> True
              Just _ -> traceError "3" -- Cannot cleanup valid datum
          _ -> case mdatum of
            Nothing -> traceError "4" -- No datum
            Just (Datum bdatum) -> case fromBuiltinData bdatum of
              Nothing -> traceError "5" -- Invalid datum
              Just (promotionRankDatum :: OnchainRank) ->
                case redeemer of
                  (PromotionAcceptance profileOutputIdx rankOutputIdx) ->
                    -- The promotion was already validated at mint time.
                    -- Ranks cannot be spent
                    -- We only need to verify the student consents by spending their user NFT.
                    let ownInput = Utils.unsafeFindOwnInputByTxOutRef spendingTxOutRef txInfoInputs
                        ownValue = txOutValue ownInput
                        ownAddress = txOutAddress ownInput

                        -- Getting profiles based on the ids in the rank promotion datum
                        (studentProfileValue, studentProfileDatum) = unsafeGetProfileDatumAndValue studentProfileId profilesValidatorAddress txInfoInputs
                        (updatedProfileCIP68Datum, newRank) = promoteProfile studentProfileDatum promotionRankDatum

                        profilesValidatorAddress = V1.scriptHashAddress $ profilesValidatorScriptHash $ promotionProtocolParams promotionRankDatum
                        studentProfileId = promotionAwardedTo promotionRankDatum -- Fails if trying to spend a rank instead of a promotion
                        profileUserAssetClass = deriveUserFromRefAC studentProfileId
                     in -- NOTE (R3 redundancy removed — see OnchainSecurityAudit.md):
                        -- The "Profile Ref NFT == 1" check was removed because
                        -- unsafeGetProfileDatumAndValue uses checkAndGetCurrentStateDatumAndValue,
                        -- which filters by `geq assetClassValue stateToken 1` and demands exactly
                        -- one matching UTxO. If the lookup succeeds, the NFT is guaranteed present.
                        -- Additionally, the MintingPolicy only ever mints exactly 1 of each profile
                        -- NFT, so >= 1 implies == 1. PV also independently checks this.
                        and
                          [ traceIfFalse "7" $ V1.assetClassValueOf (valueSpent txInfo) profileUserAssetClass == 1, -- Must spend User NFT
                            traceIfFalse "8" $ Utils.checkTxOutAtIndexWithDatumValueAndAddress profileOutputIdx updatedProfileCIP68Datum studentProfileValue profilesValidatorAddress txInfoOutputs, -- Lock profile at PV
                            traceIfFalse "9" $ Utils.checkTxOutAtIndexWithDatumValueAndAddress rankOutputIdx newRank ownValue ownAddress txInfoOutputs -- Lock rank at RV
                          ]
        _ -> traceError "6" -- Invalid script info

-------------------------------------------------------------------------------

-- * Compilation

-------------------------------------------------------------------------------

-- | Lose the types
ranksUntyped :: BuiltinData -> BuiltinUnit
ranksUntyped = mkUntypedLambda ranksLambda

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
ranksCompile :: CompiledCode (BuiltinData -> BuiltinUnit)
ranksCompile = $$(compile [||ranksUntyped||])
