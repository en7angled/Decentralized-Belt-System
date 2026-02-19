{-# LANGUAGE NoImplicitPrelude #-}

-- | One-shot minting policy for the oracle NFT.
-- Parameterized by a 'TxOutRef' (seed UTxO) to guarantee that exactly one
-- oracle NFT can ever be minted. The seed must be spent in the transaction.
module Onchain.Validators.OracleNFTPolicy
  ( -- * Policy
    oracleNFTPolicyLambda,

    -- * Compilation
    oracleNFTPolicyCompile,
  )
where

import Onchain.Utils qualified as Utils
import PlutusCore.Builtin.Debug (plcVersion110)
import PlutusLedgerApi.V3
import PlutusTx
import PlutusTx.Prelude

-------------------------------------------------------------------------------

-- * Oracle NFT Policy

-------------------------------------------------------------------------------

-- | One-shot minting policy.
-- Validates that:
--   1. The seed 'TxOutRef' is consumed as an input (guarantees uniqueness)
{-# INLINEABLE oracleNFTPolicyLambda #-}
oracleNFTPolicyLambda :: TxOutRef -> ScriptContext -> Bool
oracleNFTPolicyLambda seedRef (ScriptContext TxInfo {..} _ scriptInfo) =
  case scriptInfo of
    MintingScript _ ->
      traceIfFalse "N0" -- Must spend seed UTxO (N0)
        $ any ((== seedRef) . txInInfoOutRef) txInfoInputs
    _ -> traceError "N1" -- Invalid script purpose (N1)

-------------------------------------------------------------------------------

-- * Compilation

-------------------------------------------------------------------------------

-- | Lose the types.
{-# INLINEABLE oracleNFTPolicyUntyped #-}
oracleNFTPolicyUntyped :: TxOutRef -> BuiltinData -> BuiltinUnit
oracleNFTPolicyUntyped seedRef =
  Utils.mkUntypedLambda (oracleNFTPolicyLambda seedRef)

-- | Compile the one-shot policy, parameterized by a seed 'TxOutRef'.
oracleNFTPolicyCompile :: TxOutRef -> CompiledCode (BuiltinData -> BuiltinUnit)
oracleNFTPolicyCompile seedRef =
  $$(compile [||oracleNFTPolicyUntyped||])
    `unsafeApplyCode` liftCode plcVersion110 seedRef
