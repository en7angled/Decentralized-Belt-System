-- Required for `makeLift`:
{-# LANGUAGE MultiParamTypeClasses #-}
-- Required for `makeLift`:
{-# LANGUAGE ScopedTypeVariables #-}
---
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-optimize #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:no-remove-trace #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

{-# HLINT ignore "Use &&" #-}

module Onchain.ProfilesValidator where

import GHC.Base (undefined)
import GHC.Generics (Generic)
import Onchain.CIP68 (CIP68Datum, MetadataFields, generateRefAndUserTN, mkCIP68Datum)
import Onchain.Types
import Onchain.Types qualified as Onchain
import Onchain.Utils
import PlutusCore.Builtin.Debug (plcVersion110)
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V1.Interval (before)
import PlutusLedgerApi.V3
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude
import Prelude qualified

-- | Parameters :
data ProfilesParams = ProfilesParams
  { ranksValidatorScriptHash :: ScriptHash,
    profilesCollateral :: Lovelace
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
profilesLambda ProfilesParams {..} (ScriptContext TxInfo {..} (Redeemer bredeemer) scriptInfo) =
  let redeemer = unsafeFromBuiltinData @ProfilesRedeemer bredeemer
   in case (scriptInfo, redeemer) of
        (MintingScript profilesCurrencySymbol@(CurrencySymbol bshash), CreateProfile seedTxOutRef metadata profileType creationDate) ->
          let baseId = nameFromTxOutRef seedTxOutRef
              (profileUserTN, profileRefTN) = generateRefAndUserTN baseId
              ranksCurrencySymbol = CurrencySymbol (getScriptHash ranksValidatorScriptHash)
              rankDatum = mkFirstRank profilesCurrencySymbol ranksCurrencySymbol seedTxOutRef creationDate
              profile = mkProfile profilesCurrencySymbol ranksCurrencySymbol seedTxOutRef profileType
              profileDatum = mkCIP68Datum profile metadata
              profileRefAssetClass = V1.AssetClass (profilesCurrencySymbol, profileRefTN)
              profileUserAssetClass = V1.AssetClass (profilesCurrencySymbol, profileUserTN)
              rankAssetClass = V1.AssetClass (ranksCurrencySymbol, TokenName baseId)
              minValue = V1.lovelaceValue profilesCollateral
              profileRefNFTValue = V1.assetClassValue profileRefAssetClass 1 + minValue
              rankNFTValue = V1.assetClassValue rankAssetClass 1 + minValue

              profilesValidatorAddress = V1.scriptHashAddress $ ScriptHash bshash
              ranksValidatorAddress = V1.scriptHashAddress $ ranksValidatorScriptHash
           in and
                [ traceIfFalse "Creation date must be before the tx validity range"
                    $ before creationDate txInfoValidRange,
                  traceIfFalse "Must spend seed TxOutRef"
                    $ any ((== seedTxOutRef) . txInInfoOutRef) txInfoInputs,
                  traceIfFalse "Must mint profile Ref NFT"
                    $ isMintingNFT profileRefAssetClass txInfoMint,
                  traceIfFalse "Must mint profile User NFT"
                    $ isMintingNFT profileUserAssetClass txInfoMint,
                  traceIfFalse "Must mint 1st Rank NFT"
                    $ isMintingNFT rankAssetClass txInfoMint,
                  traceIfFalse "Must lock profile Ref NFT with inline datum at profilesValidator address"
                    $ hasTxOutWithInlineDatumAndValue profileDatum profileRefNFTValue profilesValidatorAddress txInfoOutputs,
                  traceIfFalse "Must lock rank NFT with inline datum at ranksValidator address"
                    $ hasTxOutWithInlineDatumAndValue rankDatum rankNFTValue ranksValidatorAddress txInfoOutputs
                ]
        (MintingScript cs@(CurrencySymbol bshash), DeleteProfile) -> True
          -- and
          --   [ traceIfFalse "Must burn profile Ref NFT"
          --       $ isBurningNFT profileRefAssetClass txInfoMint,
          --     traceIfFalse "Must burn profile User NFT"
          --       $ isMintingNFT profileUserAssetClass txInfoMint
          --   ]
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
