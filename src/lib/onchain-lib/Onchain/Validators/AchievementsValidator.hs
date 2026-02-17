{-# LANGUAGE MultiParamTypeClasses #-}
-- Required for `makeLift`:
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:optimize #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

{-# HLINT ignore "Use &&" #-}

-- | Ranks validator enforcing promotion acceptance and rank state transitions.
module Onchain.Validators.AchievementsValidator where

import GHC.Generics (Generic)
import Onchain.CIP68
import Onchain.Protocol
import Onchain.Utils qualified as Utils
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.Contexts (valueSpent)
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude
import Prelude qualified

-------------------------------------------------------------------------------

-- * Ranks Redeemer

-------------------------------------------------------------------------------

-- This validator is simplified because promotion validation now happens
-- at mint time in the MintingPolicy.

data AchievementsRedeemer
  = -- | AcceptAchievement achievementOutputIdx
    AcceptAchievement Integer
  | Cleanup
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''AchievementsRedeemer [('AcceptAchievement, 0), ('Cleanup, 1)]

type AchievementsDatum = CIP68Datum OnchainAchievement

-------------------------------------------------------------------------------

{-# INLINEABLE achievementsLambda #-}
achievementsLambda :: ScriptContext -> Bool
achievementsLambda (ScriptContext txInfo@TxInfo {..} (Redeemer bredeemer) scriptInfo) =
  let redeemer = unsafeFromBuiltinData @AchievementsRedeemer bredeemer
   in case scriptInfo of
        (SpendingScript spendingTxOutRef mdatum) -> case redeemer of
          -- Permissionless cleanup: allow spending if datum is absent or unparseable.
          Cleanup -> case mdatum of
            Nothing -> True
            Just (Datum bd) -> case fromBuiltinData @AchievementsDatum bd of
              Nothing -> True
              Just _ -> traceError "A0" -- Cannot cleanup valid datum (A0)
          AcceptAchievement outputIdx -> case mdatum of
            Nothing -> traceError "A0" -- No datum (A0)
            Just (Datum bdatum) -> case fromBuiltinData bdatum of
              Nothing -> traceError "A0" -- Invalid datum (A0)
              Just (achievementDatum :: AchievementsDatum) ->
                let achievement = extra achievementDatum
                    ownInput = Utils.unsafeFindOwnInputByTxOutRef spendingTxOutRef txInfoInputs
                    ownValue = txOutValue ownInput
                    ownAddress = txOutAddress ownInput
                    updatedAchievementDatum = achievementDatum {extra = acceptAchievement achievement}
                    practitionerUserAC = deriveUserFromRefAC (achievementAwardedTo achievement)
                 in traceIfFalse
                      "A1"
                      $ and
                        [ Utils.checkTxOutAtIndexWithDatumValueAndAddress
                            outputIdx
                            updatedAchievementDatum
                            ownValue
                            ownAddress
                            txInfoOutputs, -- Lock achievement at AchievementsValidator address
                          V1.assetClassValueOf
                            (valueSpent txInfo)
                            practitionerUserAC
                            == 1 -- Must spend practitioner user NFT
                        ]
        _ -> traceError "A2" -- Invalid script info (A2)

-------------------------------------------------------------------------------

-- * Compilation

-------------------------------------------------------------------------------

-- | Lose the types
achievementsUntyped :: BuiltinData -> BuiltinUnit
achievementsUntyped = Utils.mkUntypedLambda achievementsLambda

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
achievementsCompile :: CompiledCode (BuiltinData -> BuiltinUnit)
achievementsCompile = $$(compile [||achievementsUntyped||])
