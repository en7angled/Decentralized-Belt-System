{-# LANGUAGE NoImplicitPrelude #-}

-- | On-chain utility functions shared across validators.
-- Provides helpers for untyped lambda conversion, datum/value lookups,
-- output validation, oracle reading, fee checking, and the shared minimum
-- lovelace constant.
module Onchain.Utils
  ( -- * Shared Constants
    protocolMinLovelaceValue,
    protocolMinLovelace,

    -- * Helper Functions
    mkUntypedLambda,
    nameFromTxOutRef,

    -- * Oracle Helpers
    readOracleParams,
    checkFee,

    -- * Datum Helper Functions
    checkTxOutAtIndexWithDatumValueAndAddress,
    isTxOutAtIndexWithDatumValueAndAddress,
    hasTxInAtAddressWithNFT,
    isGivenInlineDatum,
    unsafeFindOwnInputByTxOutRef,

    -- * Validate and Get
    checkAndGetCurrentStateDatumAndValue,
    checkAndGetInlineDatum,

    -- * Asset Class Helpers
    hasCurrencySymbol,
  )
where

import Onchain.Protocol.Types (FeeConfig (..), OracleParams (..))
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V3
import PlutusTx.Builtins
import PlutusTx.List qualified
import PlutusTx.Prelude

-------------------------------------------------------------------------------

-- * Shared Constants

-------------------------------------------------------------------------------

protocolMinLovelace :: Integer
protocolMinLovelace = 3_500_000
{-# INLINEABLE protocolMinLovelace #-}

-- | Minimum lovelace value required for UTxO outputs.
{-# INLINEABLE protocolMinLovelaceValue #-}
protocolMinLovelaceValue :: Value
protocolMinLovelaceValue = V1.lovelaceValue (Lovelace protocolMinLovelace)

-------------------------------------------------------------------------------

-- * Helper Functions

-------------------------------------------------------------------------------

-- | Converts a typed validator lambda to the untyped form expected by Plutus.
{-# INLINEABLE mkUntypedLambda #-}
mkUntypedLambda ::
  (ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinUnit)
mkUntypedLambda f c = check $ f (parseData c "U0") -- Invalid context (U0)
  where
    parseData mdata message = case fromBuiltinData mdata of
      Just d -> d
      _ -> traceError message

{-# INLINEABLE nameFromTxOutRef #-}
nameFromTxOutRef :: TxOutRef -> BuiltinByteString
nameFromTxOutRef (TxOutRef (TxId txIdbs) txIdx) = blake2b_224 (txIdbs `appendByteString` integerToByteString BigEndian 0 txIdx)

-------------------------------------------------------------------------------

-- * Datum Helper Functions

-------------------------------------------------------------------------------

-- | Check that the output at a specific index has the expected datum, value, and address.
-- This is more efficient than searching through all outputs with hasTxOutWithInlineDatumAndValue.
{-# INLINEABLE checkTxOutAtIndexWithDatumValueAndAddress #-}
checkTxOutAtIndexWithDatumValueAndAddress :: (ToData a) => Integer -> a -> Value -> Address -> [TxOut] -> Bool
checkTxOutAtIndexWithDatumValueAndAddress idx datum value address outputs =
  isTxOutAtIndexWithDatumValueAndAddress datum value address (outputs !! idx)

{-# INLINEABLE isTxOutAtIndexWithDatumValueAndAddress #-}
isTxOutAtIndexWithDatumValueAndAddress :: (ToData a) => a -> Value -> Address -> TxOut -> Bool
isTxOutAtIndexWithDatumValueAndAddress datum value address TxOut {txOutValue, txOutAddress, txOutDatum} =
  (value == txOutValue) && (address == txOutAddress) && isGivenInlineDatum datum txOutDatum

{-# INLINEABLE hasTxInAtAddressWithNFT #-}
hasTxInAtAddressWithNFT :: AssetClass -> Address -> [TxInInfo] -> Bool
hasTxInAtAddressWithNFT ac address = any (\(TxInInfo _inOutRef (TxOut {txOutAddress, txOutValue})) -> (address == txOutAddress) && (V1.assetClassValueOf txOutValue ac == 1))

{-# INLINEABLE isGivenInlineDatum #-}
isGivenInlineDatum :: (ToData a) => a -> OutputDatum -> Bool
isGivenInlineDatum datum outdat = case outdat of
  OutputDatum da -> toBuiltinData datum == getDatum da
  _ -> False

{-# INLINEABLE unsafeFindOwnInputByTxOutRef #-}
unsafeFindOwnInputByTxOutRef :: TxOutRef -> [TxInInfo] -> TxOut
unsafeFindOwnInputByTxOutRef spendingTxOutRef txInfoInputs =
  let i = PlutusTx.List.find (\TxInInfo {txInInfoOutRef} -> txInInfoOutRef == spendingTxOutRef) txInfoInputs
   in case i of
        Just (TxInInfo _inOutRef inOut) -> inOut
        Nothing -> traceError "U1" -- Cannot find own input by TxOutRef (U1)

-------------------------------------------------------------------------------

-- * Validate and Get - Datum and Value Helper Functions

-------------------------------------------------------------------------------

{-# INLINEABLE checkAndGetCurrentStateDatumAndValue #-}
checkAndGetCurrentStateDatumAndValue :: V1.AssetClass -> Address -> [TxInInfo] -> (Value, BuiltinData)
checkAndGetCurrentStateDatumAndValue stateToken addr outs =
  case filter (\(TxInInfo _inOutRef (TxOut {txOutValue, txOutAddress})) -> (txOutValue `geq` V1.assetClassValue stateToken 1) && (addr == txOutAddress)) outs of
    [TxInInfo _inOutRef out] -> (txOutValue out, checkAndGetInlineDatum out)
    _ -> traceError "U2" -- Cannot find state NFT at expected address (U2)

{-# INLINEABLE checkAndGetInlineDatum #-}
checkAndGetInlineDatum :: TxOut -> BuiltinData
checkAndGetInlineDatum out = case txOutDatum out of
  OutputDatum da -> getDatum da
  _ -> traceError "U3" -- Invalid output: expected inline datum (U3)

-------------------------------------------------------------------------------

-- * Oracle Helpers

-------------------------------------------------------------------------------

-- | Read 'OracleParams' from the oracle UTxO identified by its NFT 'AssetClass'
-- in the transaction's reference inputs. Fails if the oracle UTxO is not found
-- or the datum cannot be decoded.
{-# INLINEABLE readOracleParams #-}
readOracleParams :: AssetClass -> [TxInInfo] -> OracleParams
readOracleParams oracleAC refInputs =
  case PlutusTx.List.find hasOracleNFT refInputs of
    Just (TxInInfo _ TxOut {txOutDatum}) -> case txOutDatum of
      OutputDatum (Datum bd) -> case fromBuiltinData @OracleParams bd of
        Just params -> params
        Nothing -> traceError "U4" -- Oracle read failed: cannot decode datum (U4)
      _ -> traceError "U4" -- Oracle read failed: must have inline datum (U4)
    Nothing -> traceError "U4" -- Oracle read failed: UTxO not found (U4)
  where
    hasOracleNFT (TxInInfo _ TxOut {txOutValue}) =
      V1.assetClassValueOf txOutValue oracleAC == 1

-- | Check that a fee payment is included in the transaction outputs.
-- If 'opFeeConfig' is 'Nothing', no fee is required and the check passes.
-- If 'opFeeConfig' is 'Just', validates that at least one output pays
-- >= the required fee (extracted via the selector) to 'fcFeeAddress'.
{-# INLINEABLE checkFee #-}
checkFee :: OracleParams -> (FeeConfig -> Integer) -> [TxOut] -> Bool
checkFee oracle feeSelector outputs = case opFeeConfig oracle of
  Nothing -> True
  Just feeConfig ->
    let requiredFee = feeSelector feeConfig
        feeAddr = fcFeeAddress feeConfig
        feeValue = V1.lovelaceValue (V1.Lovelace requiredFee)
     in traceIfFalse "U5" -- Must pay required fee (U5)
          $ any
            ( \TxOut {txOutValue = v, txOutAddress = a} ->
                a == feeAddr && v `geq` feeValue
            )
            outputs

-------------------------------------------------------------------------------

-- * Asset Class Helpers

-------------------------------------------------------------------------------

-- | Check whether an 'AssetClass' belongs to the given 'CurrencySymbol'.
{-# INLINEABLE hasCurrencySymbol #-}
hasCurrencySymbol :: AssetClass -> CurrencySymbol -> Bool
hasCurrencySymbol (AssetClass (cs, _)) cs' = cs == cs'
