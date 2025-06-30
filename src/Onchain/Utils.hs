{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use &&" #-}

module Onchain.Utils where

import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V3
import PlutusTx.Builtins (serialiseData)
import PlutusTx.List qualified
import PlutusTx.Prelude
import PlutusLedgerApi.V1.Value

--------------------------------------
--  Helper Functions
--------------------------------------

-- | Converts a typed lambda to untyped
{-# INLINEABLE mkUntypedLambda #-}
mkUntypedLambda ::
  (ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinUnit)
mkUntypedLambda f c = check $ f (parseData c "Invalid context")
  where
    parseData mdata message = case fromBuiltinData mdata of
      Just d -> d
      _ -> traceError message

-- TODO update with new builtins
{-# INLINEABLE integerToBs24 #-}
integerToBs24 :: Integer -> BuiltinByteString
integerToBs24 = dropByteString 1 . serialiseData . toBuiltinData -- Removing First Byte  (works for value > 24)

{-# INLINEABLE tokenNameFromTxOutRef #-}
tokenNameFromTxOutRef :: TxOutRef -> TokenName
tokenNameFromTxOutRef toref = TokenName (nameFromTxOutRef toref)

{-# INLINEABLE nameFromTxOutRef #-}
nameFromTxOutRef :: TxOutRef -> BuiltinByteString
nameFromTxOutRef (TxOutRef (TxId txIdbs) txIdx) = takeByteString 28 $ blake2b_256 (txIdbs <> (serialiseData . toBuiltinData) txIdx)

------------------------

-- ** Value Helper Functions

------------------------

{-# INLINEABLE isMintingNFT #-}
isMintingNFT :: V1.AssetClass -> MintValue -> Bool
isMintingNFT ac txInfoMint = V1.assetClassValueOf (mintValueMinted txInfoMint) ac == 1

{-# INLINEABLE isBurningNFT #-}
isBurningNFT :: V1.AssetClass -> MintValue -> Bool
isBurningNFT ac txInfoMint = V1.assetClassValueOf (mintValueBurned txInfoMint) ac == 1

------------------------

-- ** Datum Helper Functions

------------------------

{-# INLINEABLE hasTxOutWithInlineDatumAndValue #-}
hasTxOutWithInlineDatumAndValue :: (ToData a) => a -> Value -> Address -> [TxOut] -> Bool
hasTxOutWithInlineDatumAndValue datum value address = any (isTxOutWithInlineDatumAndValue datum value address)

{-# INLINEABLE isTxOutWithInlineDatumAndValue #-}
isTxOutWithInlineDatumAndValue :: (ToData a) => a -> Value -> Address -> TxOut -> Bool
isTxOutWithInlineDatumAndValue datum value address TxOut {txOutValue, txOutAddress, txOutDatum} =
  and
    [ value == txOutValue,
      address == txOutAddress,
      isGivenInlineDatum datum txOutDatum
    ]

{-# INLINEABLE isGivenInlineDatum #-}
isGivenInlineDatum :: (ToData a) => a -> OutputDatum -> Bool
isGivenInlineDatum datum outdat = case outdat of
  OutputDatum da -> toBuiltinData datum == getDatum da
  _ -> False

unsafeFindOwnInputByTxOutRef :: TxOutRef -> [TxInInfo] -> TxOut
unsafeFindOwnInputByTxOutRef spendingTxOutRef txInfoInputs =
  let i = PlutusTx.List.find (\TxInInfo {txInInfoOutRef} -> txInInfoOutRef == spendingTxOutRef) txInfoInputs
   in case i of
        Just (TxInInfo _inOutRef inOut) -> inOut
        Nothing -> traceError "Own input not found"
{-# INLINEABLE unsafeFindOwnInputByTxOutRef #-}


------------------------

-- ** Unsafe Datum Helper Functions

------------------------



unsafeGetCurrentStateDatumAndValue :: V1.AssetClass -> Address -> [TxInInfo] -> (Value, BuiltinData)
unsafeGetCurrentStateDatumAndValue stateToken addr outs =
  case filter (\(TxInInfo _inOutRef (TxOut {txOutValue, txOutAddress}) )-> ( txOutValue `geq` V1.assetClassValue stateToken 1) && (addr == txOutAddress) ) outs of
    [TxInInfo _inOutRef out] -> (txOutValue out, unsafeGetInlineDatum out)
    _ -> traceError "state nft not found"
{-# INLINEABLE unsafeGetCurrentStateDatumAndValue #-}


unsafeGetInlineDatum :: TxOut -> BuiltinData
unsafeGetInlineDatum out = case txOutDatum out of
  OutputDatum da -> getDatum da
  _ -> traceError "No inline datum"
{-# INLINEABLE unsafeGetInlineDatum #-}