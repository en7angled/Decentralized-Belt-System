{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use &&" #-}

-- | Oracle validator for the BJJ Belt protocol.
-- Holds mutable operational parameters ('OracleParams') in its datum.
-- Only the current admin (from the datum) can update the oracle.
-- The oracle UTxO must be returned to the same address with the same value,
-- ensuring the NFT remains locked and the oracle remains available.
module Onchain.Validators.OracleValidator
  ( -- * Validator
    oracleLambda,

    -- * Compilation
    oracleCompile,
  )
where

import Onchain.Protocol.Types (OracleParams (..))
import Onchain.Utils qualified as Utils
import PlutusLedgerApi.V3
import PlutusTx
import PlutusTx.Prelude

-------------------------------------------------------------------------------

-- * Oracle Validator

-------------------------------------------------------------------------------

-- | Oracle validator logic.
-- Validates that:
--   1. The current admin (from datum) signs the transaction
--   2. The oracle UTxO is returned to the same address with the same value
{-# INLINEABLE oracleLambda #-}
oracleLambda :: ScriptContext -> Bool
oracleLambda (ScriptContext TxInfo {..} _ scriptInfo) =
  case scriptInfo of
    SpendingScript spendingRef (Just (Datum bdatum)) ->
      case fromBuiltinData @OracleParams bdatum of
        Nothing -> traceError "O0" -- Invalid oracle datum (O0)
        Just currentParams ->
          let ownInput = Utils.unsafeFindOwnInputByTxOutRef spendingRef txInfoInputs
              ownValue = txOutValue ownInput
              ownAddr = txOutAddress ownInput
           in and
                [ traceIfFalse "O1" $ elem (opAdminPkh currentParams) txInfoSignatories, -- Must be signed by admin (O1)
                  traceIfFalse "O2" $ any (\TxOut {txOutValue = v, txOutAddress = a} -> a == ownAddr && v == ownValue) txInfoOutputs -- Must return oracle UTxO (O2)
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
