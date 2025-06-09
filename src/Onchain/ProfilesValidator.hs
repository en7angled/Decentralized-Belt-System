-- Required for `makeLift`:
{-# LANGUAGE MultiParamTypeClasses #-}
-- Required for `makeLift`:
{-# LANGUAGE ScopedTypeVariables #-}
---
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-optimize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-remove-trace #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

module Onchain.ProfilesValidator where

import GHC.Generics (Generic)
import Onchain.CIP68 (CIP68Datum, MetadataFields)
import Onchain.Types (Profile)
import Onchain.Utils
import PlutusCore.Builtin.Debug (plcVersion110)
import PlutusLedgerApi.V3
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude
import Prelude qualified

-- | Parameters :
newtype ProfilesParams = ProfilesParams ()
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

-- | Generate `Lift` instance for the custom parameter type with Template Haskell.
--  Allows argument value to be pre-compiled to UPLC, so the compiled parameterized script can be applied to it.
makeLift ''ProfilesParams

makeIsDataSchemaIndexed ''ProfilesParams [('ProfilesParams, 0)]

-- | Custom redeemer :
data ProfilesRedeemer
  = CreateProfile TxOutRef MetadataFields Profile
  | UpdateProfile MetadataFields
  | DeleteProfile
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''ProfilesRedeemer [('CreateProfile, 0), ('UpdateProfile, 1), ('DeleteProfile, 2)]

type ProfilesDatum = CIP68Datum Profile

--------------------------------------
-- Profiles Validator
--------------------------------------

{-# INLINEABLE profilesLambda #-}
profilesLambda :: ProfilesParams -> ScriptContext -> Bool
profilesLambda params (ScriptContext TxInfo {..} (Redeemer bredeemer) scriptInfo) =
  let redeemer = unsafeFromBuiltinData @ProfilesRedeemer bredeemer
   in case (scriptInfo, redeemer) of
        (MintingScript cs@(CurrencySymbol bshash), CreateProfile seedTxOutRef metadata profile) -> True
        (MintingScript cs@(CurrencySymbol bshash), DeleteProfile) -> True
        (SpendingScript txOutRef mdatum, UpdateProfile metadata) -> True
        (SpendingScript txOutRef mdatum, DeleteProfile) -> True
        _ -> False

-- | Lose the types
profilesUntyped :: ProfilesParams -> BuiltinData -> BuiltinUnit
profilesUntyped params =
  mkUntypedLambda (profilesLambda params)

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
profilesCompile :: ProfilesParams -> CompiledCode (BuiltinData -> BuiltinUnit)
profilesCompile params =
  $$(compile [||profilesUntyped||])
    `unsafeApplyCode` liftCode plcVersion110 params
