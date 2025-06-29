{-# LANGUAGE NoImplicitPrelude #-}

module Onchain.Utils where

import PlutusLedgerApi.V3
import PlutusTx.Builtins (serialiseData)
import PlutusTx.Prelude

--------------------------------------
--  Helper Functions
--------------------------------------

-- | Converts a typed lambda to untyped
mkUntypedLambda ::
  (ScriptContext -> Bool) ->
  (BuiltinData -> BuiltinUnit)
mkUntypedLambda f c = check $ f (parseData c "Invalid context")
  where
    parseData mdata message = case fromBuiltinData mdata of
      Just d -> d
      _ -> traceError message
{-# INLINEABLE mkUntypedLambda #-}

-- TODO update with new builtins
integerToBs24 :: Integer -> BuiltinByteString
integerToBs24 = dropByteString 1 . serialiseData . toBuiltinData -- Removing First Byte  (works for value > 24)
{-# INLINEABLE integerToBs24 #-}

tokenNameFromTxOutRef :: TxOutRef -> TokenName
tokenNameFromTxOutRef toref = TokenName (nameFromTxOutRef toref)

nameFromTxOutRef :: TxOutRef -> BuiltinByteString
nameFromTxOutRef (TxOutRef (TxId txIdbs) txIdx) = takeByteString 28 $ blake2b_256 (txIdbs <> (serialiseData . toBuiltinData) txIdx)

{-# INLINEABLE tokenNameFromTxOutRef #-}