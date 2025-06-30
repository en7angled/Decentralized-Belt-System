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

import GHC.Generics (Generic)
import Onchain.CIP68 (CIP68Datum (CIP68Datum), MetadataFields, deriveUserFromRefTN, generateRefAndUserTN, mkCIP68Datum, updateCIP68DatumImage)
import Onchain.Types
import Onchain.Types qualified as Onchain
import Onchain.Utils
import PlutusCore.Builtin.Debug (plcVersion110)
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V1.Interval (before)
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.Contexts
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

type ImageURI = BuiltinByteString

-- | Custom redeemer :
data ProfilesRedeemer
  = CreateProfile TxOutRef MetadataFields Onchain.ProfileType POSIXTime
  | UpdateProfileImage V1.AssetClass ImageURI
  | DeleteProfile V1.AssetClass
  | Promote ProfileId RankValue
  | AcceptPromotion PromotionId
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''ProfilesRedeemer [('CreateProfile, 0), ('UpdateProfileImage, 1), ('DeleteProfile, 2), ('Promote, 3), ('AcceptPromotion, 4)]

type ProfilesDatum = CIP68Datum Onchain.Profile

--------------------------------------
-- Profiles Validator
--------------------------------------

{-# INLINEABLE profilesLambda #-}
profilesLambda :: ProfilesParams -> ScriptContext -> Bool
profilesLambda ProfilesParams {..} context@(ScriptContext txInfo@TxInfo {..} (Redeemer bredeemer) scriptInfo) =
  let redeemer = unsafeFromBuiltinData @ProfilesRedeemer bredeemer
      minValue = V1.lovelaceValue profilesCollateral
   in case (scriptInfo, redeemer) of
        (MintingScript profilesCurrencySymbol@(CurrencySymbol bshash), mintingRedeemer) -> case mintingRedeemer of
          CreateProfile seedTxOutRef metadata profileType creationDate ->
            let baseId = nameFromTxOutRef seedTxOutRef
                (profileUserTN, profileRefTN) = generateRefAndUserTN baseId
                ranksCurrencySymbol = CurrencySymbol (getScriptHash ranksValidatorScriptHash)
                rankDatum = mkFirstRank profilesCurrencySymbol ranksCurrencySymbol seedTxOutRef creationDate
                profile = mkProfile profilesCurrencySymbol ranksCurrencySymbol seedTxOutRef profileType
                profileDatum = mkCIP68Datum profile metadata -- !!! Open unbounded-datum vulnerability on metadata
                profileRefAssetClass = V1.AssetClass (profilesCurrencySymbol, profileRefTN)
                profileUserAssetClass = V1.AssetClass (profilesCurrencySymbol, profileUserTN)
                rankAssetClass = V1.AssetClass (ranksCurrencySymbol, TokenName baseId)
                profileRefNFTValue = V1.assetClassValue profileRefAssetClass 1 + minValue
                rankNFTValue = V1.assetClassValue rankAssetClass 1 + minValue
                profilesValidatorAddress = V1.scriptHashAddress $ ScriptHash bshash
                ranksValidatorAddress = V1.scriptHashAddress ranksValidatorScriptHash
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
          (DeleteProfile _ac) ->
            let mintedValue = mintValueMinted txInfoMint
                mintedProfileNFTs = V1.currencySymbolValueOf mintedValue profilesCurrencySymbol
             in -- protection against other-token-name attack vector
                traceIfFalse "Tx must not mint other tokens of the same currency symbol" $ mintedProfileNFTs == 0
          _ -> traceError "Invalid redeemer for minting"
        (SpendingScript spendingTxOutRef mdatum, spendingRedeemer) -> case mdatum of
          Nothing -> traceError "No datum"
          Just (Datum bdatum) -> case fromBuiltinData bdatum of
            Nothing -> traceError "Invalid datum"
            Just profileDatum@(CIP68Datum _metadata _version profile@Profile {..}) ->
              let !ownInput = unsafeFindOwnInputByTxOutRef spendingTxOutRef txInfoInputs
                  profilesValidatorAddress = txOutAddress ownInput
               in case spendingRedeemer of
                    DeleteProfile profileRefAssetClass@(V1.AssetClass (profilesCurrencySymbol, profileRefTN)) ->
                      let profileUserAssetClass = V1.AssetClass (profilesCurrencySymbol, deriveUserFromRefTN profileRefTN)
                       in and
                            [ traceIfFalse "Must burn profile Ref NFT"
                                $ isBurningNFT profileRefAssetClass txInfoMint,
                              traceIfFalse "Must burn profile User NFT"
                                $ isBurningNFT profileUserAssetClass txInfoMint
                            ]
                    (UpdateProfileImage profileRefAssetClass@(V1.AssetClass (profilesCurrencySymbol, profileRefTN)) newImageURI) ->
                      let newCip68Datum = updateCIP68DatumImage newImageURI profileDatum -- !!! Open unbounded-datum vulnerability on metadata
                          profileUserAssetClass = V1.AssetClass (profilesCurrencySymbol, deriveUserFromRefTN profileRefTN)
                          profileRefNFTValue = V1.assetClassValue profileRefAssetClass 1 + minValue
                       in and
                            [ traceIfFalse "Must spend profile User NFT"
                                $ V1.assetClassValueOf (valueSpent txInfo) profileUserAssetClass
                                == 1,
                              traceIfFalse "Must lock profile Ref NFT with inline updated datum at profilesValidator address"
                                $ hasTxOutWithInlineDatumAndValue newCip68Datum profileRefNFTValue profilesValidatorAddress txInfoOutputs
                            ]
                    _ -> traceError "Invalid redeemer for spending"
        -- (SpendingScript txOutRef mdatum, Promote profileId rankValue) ->
        --   -- is minting promotion NFT
        --   -- is locking back the profile NFT with the same value and datum (unchanged)
        --   True
        -- (SpendingScript txOutRef mdatum, AcceptPromotion promotionId) ->
        --   -- is burning promotion NFT
        --   -- is minting rank NFT based on the promotion datum
        --   -- is locking profile Ref NFT with inline updated datum with new rank locked at profilesValidator address
        --   True
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
