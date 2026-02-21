{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use &&" #-}
{-# HLINT ignore "Use newtype instead of data" #-}

-- | Oracle validator for the BJJ Belt protocol.
-- Holds mutable operational parameters ('OracleParams') in its datum.
-- Only the current admin (from the datum) can update the oracle.
-- The oracle UTxO must be returned to the same address with the same value,
-- ensuring the NFT remains locked and the oracle remains available.
module Onchain.Validators.OracleValidator
  ( -- * Redeemer
    OracleRedeemer (..),

    -- * Validator
    oracleLambda,

    -- * Compilation
    oracleCompile,
  )
where

import GHC.Generics (Generic)
import Onchain.Protocol.Types (OracleParams (..))
import Onchain.Utils qualified as Utils
import PlutusLedgerApi.V3
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude
import Prelude qualified

-------------------------------------------------------------------------------

-- * Oracle Redeemer

-------------------------------------------------------------------------------

-- | Redeemer for the oracle validator. Carries the output index where the
-- oracle UTxO must be returned (output-index check instead of O(n) search).
data OracleRedeemer = OracleUpdate Integer
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''OracleRedeemer [('OracleUpdate, 0)]

-------------------------------------------------------------------------------

-- * Oracle Validator

-------------------------------------------------------------------------------

-- | Oracle validator logic.
-- Validates that:
--   1. The current admin (from datum) signs the transaction
--   2. The oracle UTxO is returned at the given output index with same datum, value, and address
{-# INLINEABLE oracleLambda #-}
oracleLambda :: ScriptContext -> Bool
oracleLambda (ScriptContext TxInfo {..} (Redeemer bredeemer) scriptInfo) =
  case scriptInfo of
    SpendingScript spendingRef (Just (Datum bdatum)) ->
      case fromBuiltinData @OracleParams bdatum of
        Nothing -> traceError "O0" -- Invalid oracle datum (O0)
        Just currentParams ->
          case fromBuiltinData @OracleRedeemer bredeemer of
            Nothing -> traceError "O4" -- Invalid oracle redeemer (O4)
            Just (OracleUpdate oracleOutputIdx) ->
              let ownInput = Utils.unsafeFindOwnInputByTxOutRef spendingRef txInfoInputs
                  ownValue = txOutValue ownInput
                  ownAddr = txOutAddress ownInput
               in and
                    [ traceIfFalse "O1" $ elem (opAdminPkh currentParams) txInfoSignatories, -- Must be signed by admin (O1)
                      traceIfFalse "O2" $ Utils.checkTxOutAtIndexWithDatumValueAndAddress oracleOutputIdx currentParams ownValue ownAddr txInfoOutputs -- Must return oracle UTxO at index (O2)
                    ]
    _ -> traceError "O3" -- Invalid script purpose (O3)

-------------------------------------------------------------------------------

-- * Compilation

-------------------------------------------------------------------------------

-- | Lose the types.
{-# INLINEABLE oracleUntyped #-}
oracleUntyped :: BuiltinData -> BuiltinUnit
oracleUntyped = Utils.mkUntypedLambda oracleLambda

-- | Compile the oracle validator to a UPLC script.
oracleCompile :: CompiledCode (BuiltinData -> BuiltinUnit)
oracleCompile = $$(compile [||oracleUntyped||])
