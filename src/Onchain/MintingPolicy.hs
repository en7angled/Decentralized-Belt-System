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

module Onchain.MintingPolicy where

import GHC.Generics (Generic)
import Onchain.CIP68 (MetadataFields, generateRefAndUserTN, mkCIP68Datum, CIP68Datum)
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
import PlutusLedgerApi.V1 (isZero)
import PlutusLedgerApi.V3.Contexts





-- | Custom redeemer :
data MintingRedeemer
  = CreateProfile TxOutRef MetadataFields Onchain.ProfileType POSIXTime
  | Promote ProfileId [ProfileId] POSIXTime Integer
  | BurnProfileId V1.AssetClass
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''MintingRedeemer [('CreateProfile, 0), ('Promote, 1), ('BurnProfileId, 2)]

type ProfilesDatum = CIP68Datum Onchain.Profile



unsafeGetProfileDatumAndValue :: V1.AssetClass -> Address -> [TxInInfo] -> (Value, ProfilesDatum)
unsafeGetProfileDatumAndValue ac addr txins =
  let (v, b) = unsafeGetCurrentStateDatumAndValue ac addr txins
   in (v, unsafeFromBuiltinData b)
{-# INLINEABLE unsafeGetProfileDatumAndValue #-}


--------------------------------------
-- Minting Policy
--------------------------------------

{-# INLINEABLE mintingPolicyLambda #-}
mintingPolicyLambda :: ProtocolParams -> ScriptContext -> Bool
mintingPolicyLambda protocolParams@ProtocolParams {..} (ScriptContext txInfo@TxInfo {..} (Redeemer bredeemer) scriptInfo) =
  let redeemer = unsafeFromBuiltinData @MintingRedeemer bredeemer
      minValue = V1.lovelaceValue collateral

   in case scriptInfo of
        (MintingScript mintingPolicyCurrencySymbol@(CurrencySymbol bshash)) ->
          let profilesValidatorAddress = V1.scriptHashAddress $ ScriptHash bshash
          in
          case redeemer of
            CreateProfile seedTxOutRef metadata profileType creationDate ->
              let
                  (profileUserTN, profileRefTN) = generateRefAndUserTN $ nameFromTxOutRef seedTxOutRef
                  profileRefAssetClass = V1.AssetClass (mintingPolicyCurrencySymbol, profileRefTN)
                  profileUserAssetClass = V1.AssetClass (mintingPolicyCurrencySymbol, profileUserTN)
                  profileRefNFT = V1.assetClassValue profileRefAssetClass 1
                  profileUserNFT = V1.assetClassValue profileUserAssetClass 1


               in and
                    [ traceIfFalse "Creation date must be before the tx validity range"
                        $ before creationDate txInfoValidRange,
                      traceIfFalse "Must spend seed TxOutRef"
                        $ any ((== seedTxOutRef) . txInInfoOutRef) txInfoInputs,
                      traceIfFalse "Tx must mint JUST Ref and User NFTs" $ -- protection against other-token-name attack vector 
                         mintValueMinted txInfoMint == (profileRefNFT + profileUserNFT)
                        ]
               && case profileType of
                    Practitioner ->
                      let
                        (profile, rankDatum) = mkPractitionerProfile profileRefAssetClass creationDate protocolParams
                        profileDatum = mkCIP68Datum profile metadata -- !!! Open unbounded-datum vulnerability on metadata
                        rankAssetClass = rankId rankDatum
                        rankNFTValue = V1.assetClassValue rankAssetClass 1 + minValue
                        ranksValidatorAddress = V1.scriptHashAddress ranksValidatorScriptHash
                      in and
                          [
                            traceIfFalse "Must lock profile Ref NFT with inline datum at profilesValidator address"
                              $ hasTxOutWithInlineDatumAndValue profileDatum (profileRefNFT + minValue) profilesValidatorAddress txInfoOutputs,
                            traceIfFalse "Must mint 1st Rank NFT"
                              $ isMintingNFT rankAssetClass txInfoMint,
                            traceIfFalse "Must lock rank NFT with inline datum at ranksValidator address"
                              $ hasTxOutWithInlineDatumAndValue rankDatum rankNFTValue ranksValidatorAddress txInfoOutputs
                          ]
                    Organization ->
                      let
                        profile = mkOrganizationProfile profileRefAssetClass protocolParams
                        profileDatum = mkCIP68Datum profile metadata -- !!! Open unbounded-datum vulnerability on metadata
                      in
                       traceIfFalse "Must lock profile Ref NFT with inline datum at profilesValidator address"
                              $ hasTxOutWithInlineDatumAndValue profileDatum (profileRefNFT + minValue) profilesValidatorAddress txInfoOutputs
            
            (Promote profileId promotionAwardedBy promotionAchievementDate rankNumber) -> 
              let ranksValidatorAddress = V1.scriptHashAddress ranksValidatorScriptHash
                  promotionAssetClass = generateRankId profileId rankNumber
                  promotionNFT = V1.assetClassValue promotionAssetClass 1
                  promotionDatum = mkPendingRank profileId promotionAwardedBy promotionAchievementDate rankNumber 
              in
                and [
                  traceIfFalse "Must spend users NFTs of the awarded by profiles" $ 
                    all (\userProfileId -> V1.assetClassValueOf (valueSpent txInfo) userProfileId == 1) promotionAwardedBy,
                  traceIfFalse "Must lock promotion NFT with inline datum at ranksValidator address"
                      $ hasTxOutWithInlineDatumAndValue promotionDatum (promotionNFT + minValue) ranksValidatorAddress txInfoOutputs 
                ]
                            

            
            (BurnProfileId profileRefAssetClass) -> --- MUST ALLOW JUST FOR DELETE PROFILE (NOTHING ELSE SHOULD BE BURNED)
              let 
                  profileRefNFT = V1.assetClassValue profileRefAssetClass 1
               in 
                  and
                  [
                    traceIfFalse "Tx must spend profile Ref NFT" $ 
                         V1.assetClassValueOf (valueSpent txInfo) profileRefAssetClass == 1,
                    traceIfFalse "Tx must burn Ref NFT" $ -- protection against other burns
                         mintValueBurned txInfoMint == profileRefNFT,
                    traceIfFalse "Tx must not mint any other tokens" $ -- protection against other mints
                        isZero $ mintValueMinted txInfoMint 
                  ]
        _ -> traceError "Invalid purpose"

-- | Lose the types
mintingPolicyUntyped :: ProtocolParams -> BuiltinData -> BuiltinUnit
mintingPolicyUntyped params =
  mkUntypedLambda (mintingPolicyLambda params)

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
mintingPolicyCompile :: ProtocolParams -> CompiledCode (BuiltinData -> BuiltinUnit)
mintingPolicyCompile params =
  $$(compile [||mintingPolicyUntyped||])
    `unsafeApplyCode` liftCode plcVersion110 params
