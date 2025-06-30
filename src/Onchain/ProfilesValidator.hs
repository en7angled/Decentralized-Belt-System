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
import Onchain.Types
import Onchain.Types qualified as Onchain
import Onchain.Utils
import PlutusCore.Builtin.Debug (plcVersion110)
import PlutusLedgerApi.V3
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude
import Prelude qualified

-- | Parameters :
newtype ProfilesParams = ProfilesParams
  { ranksValidatorScriptHash :: ScriptHash
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

-- | Generate `Lift` instance for the custom parameter type with Template Haskell.
--  Allows argument value to be pre-compiled to UPLC, so the compiled parameterized script can be applied to it.
makeLift ''ProfilesParams

makeIsDataSchemaIndexed ''ProfilesParams [('ProfilesParams, 0)]

-- | Custom redeemer :
data ProfilesRedeemer
  = CreateProfile TxOutRef MetadataFields Onchain.ProfileType POSIXTime
  | UpdateProfile MetadataFields
  | DeleteProfile
  | Promote ProfileId RankValue
  | AcceptPromotion PromotionId
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''ProfilesRedeemer [('CreateProfile, 0), ('UpdateProfile, 1), ('DeleteProfile, 2), ('Promote, 3), ('AcceptPromotion, 4)]

type ProfilesDatum = CIP68Datum Onchain.Profile

--------------------------------------
-- Profiles Validator
--------------------------------------

{-# INLINEABLE profilesLambda #-}
profilesLambda :: ProfilesParams -> ScriptContext -> Bool
profilesLambda params (ScriptContext TxInfo {..} (Redeemer bredeemer) scriptInfo) =
  let redeemer = unsafeFromBuiltinData @ProfilesRedeemer bredeemer
   in case (scriptInfo, redeemer) of
        (MintingScript cs@(CurrencySymbol bshash), CreateProfile seedTxOutRef metadata profile creationDate) -> do
          -- is after creation date
          -- is spending seedTxOutRef
          -- is minting profile Ref NFT with inline datum locked at profilesValidator address
          -- is minting profile User NFT
          -- is minting rank NFT with inline datum locked at ranksValidator address
          True
        (MintingScript cs@(CurrencySymbol bshash), DeleteProfile) ->
          -- Burns both Profile Ref and User NFTs
          True
        (SpendingScript txOutRef mdatum, DeleteProfile) ->
          -- Burns both Profile Ref and User NFTs
          True
        (SpendingScript txOutRef mdatum, UpdateProfile metadata) -> 
          -- is spending the corresponding profile user NFT
          -- is locking profile Ref NFT with inline updated datum with new metadata locked at profilesValidator address
          True
        (SpendingScript txOutRef mdatum, Promote profileId rankValue) -> 
          -- is minting promotion NFT
          -- is locking back the profile NFT with the same value and datum (unchanged)
          True
        (SpendingScript txOutRef mdatum, AcceptPromotion promotionId) -> 
          -- is burning promotion NFT
          -- is minting rank NFT based on the promotion datum
          -- is locking profile Ref NFT with inline updated datum with new rank locked at profilesValidator address
          True
        _ -> traceError "Invalid redeemer"

-- | Lose the types
profilesUntyped :: ProfilesParams -> BuiltinData -> BuiltinUnit
profilesUntyped params =
  mkUntypedLambda (profilesLambda params)

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
profilesCompile :: ProfilesParams -> CompiledCode (BuiltinData -> BuiltinUnit)
profilesCompile params =
  $$(compile [||profilesUntyped||])
    `unsafeApplyCode` liftCode plcVersion110 params
