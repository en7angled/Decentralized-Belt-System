{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:optimize #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

{-# HLINT ignore "Use &&" #-}

module Onchain.RanksValidator where

import Onchain.CIP68 (deriveUserFromRefAC)
import Onchain.Protocol (OnchainRank (..))
import Onchain.Utils (mkUntypedLambda)
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.Contexts
import PlutusTx
import PlutusTx.Prelude

-----------------------------------------------------------------------------
-- Ranks Validator
--
-- This validator is simplified because promotion validation now happens
-- at mint time in the MintingPolicy. The RanksValidator only needs to
-- verify that the student consents to accepting the promotion by
-- spending their user NFT.
-----------------------------------------------------------------------------

{-# INLINEABLE ranksLambda #-}
ranksLambda :: ScriptContext -> Bool
ranksLambda (ScriptContext txInfo (Redeemer _) scriptInfo) =
  case scriptInfo of
    (SpendingScript _spendingTxOutRef mdatum) -> case mdatum of
      Nothing -> traceError "No datum"
      Just (Datum bdatum) -> case fromBuiltinData bdatum of
        Nothing -> traceError "Invalid datum"
        Just (promotionRank :: OnchainRank) ->
          -- The promotion was already validated at mint time.
          -- We only need to verify the student consents by spending their user NFT.
          let studentProfileId = promotionAwardedTo promotionRank
              profileUserAssetClass = deriveUserFromRefAC studentProfileId
           in traceIfFalse "Must spend profile User NFT to accept promotion"
                $ V1.assetClassValueOf (valueSpent txInfo) profileUserAssetClass
                == 1
    _ -> traceError "Invalid purpose"

-- | Lose the types
ranksUntyped :: BuiltinData -> BuiltinUnit
ranksUntyped = mkUntypedLambda ranksLambda

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
ranksCompile :: CompiledCode (BuiltinData -> BuiltinUnit)
ranksCompile = $$(compile [||ranksUntyped||])
