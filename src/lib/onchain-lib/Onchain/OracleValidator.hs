{-# LANGUAGE NoImplicitPrelude #-}

-- | Oracle validator for the BJJ Belt protocol.
-- Holds mutable operational parameters ('OracleParams') in its datum.
-- Only the current admin (from the datum) can update the oracle.
-- The oracle UTxO must be returned to the same address with the same value,
-- ensuring the NFT remains locked and the oracle remains available.
module Onchain.OracleValidator
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
        Nothing -> traceError "Invalid oracle datum"
        Just currentParams ->
          let ownInput = Utils.unsafeFindOwnInputByTxOutRef spendingRef txInfoInputs
              ownValue = txOutValue ownInput
              ownAddr = txOutAddress ownInput
           in and
                [ traceIfFalse "Must be signed by current admin"
                    $ elem (opAdminPkh currentParams) txInfoSignatories,
                  traceIfFalse "Must return oracle UTxO to same address with same value"
                    $ any
                      ( \TxOut {txOutValue = v, txOutAddress = a} ->
                          a == ownAddr && v == ownValue
                      )
                      txInfoOutputs
                ]
    _ -> traceError "Invalid script purpose"

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
