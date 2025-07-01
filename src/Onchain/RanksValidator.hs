{-# LANGUAGE NoImplicitPrelude #-}

module Onchain.RanksValidator where

import GHC.Generics (Generic)
import Onchain.Types
import PlutusTx.Blueprint
import PlutusTx.Prelude
import Prelude qualified
import PlutusLedgerApi.V3
import PlutusTx

newtype RanksRedeemer
  = AcceptPromotion RankId
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)


-- --------------------------------------
-- -- Ranks Validator
-- --------------------------------------

-- {-# INLINEABLE ranksLambda #-}
-- ranksLambda :: ScriptContext -> Bool
-- ranksLambda  (ScriptContext txInfo@TxInfo {..} (Redeemer bredeemer) scriptInfo) =
--   let redeemer = unsafeFromBuiltinData @RanksRedeemer bredeemer

--    in case scriptInfo of
--         (SpendingScript spendingTxOutRef mdatum) -> case mdatum of
--           Nothing -> traceError "No datum"
--           Just (Datum bdatum) -> case fromBuiltinData bdatum of
--             Nothing -> traceError "Invalid datum"
--             Just profileDatum@(CIP68Datum metadata version (profile :: Profile)) ->
--               let ownInput = unsafeFindOwnInputByTxOutRef spendingTxOutRef txInfoInputs
--                   ownValue = txOutValue ownInput
--                   ownAddress = txOutAddress ownInput
--                in True -- case redeemer of

--         _ -> traceError "Invalid purpose"

-- -- | Lose the types
-- ranksUntyped :: BuiltinData -> BuiltinUnit
-- ranksUntyped  = mkUntypedLambda ranksLambda

-- -- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
-- ranksCompile ::  CompiledCode (BuiltinData -> BuiltinUnit)
-- ranksCompile  = $$(compile [||ranksUntyped||])

