-- Required for `makeLift`:
---
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use &&" #-}

module Onchain.MintingPolicy where

import GHC.Generics (Generic)
import Onchain.CIP68 (CIP68Datum, MetadataFields, deriveUserFromRefAC, generateRefAndUserTN, mkCIP68Datum)
import Onchain.Protocol (OnChainProfileType (..), OnchainProfile (..), OnchainRank (..), ProfileId, ProtocolParams, generateRankId, mkOrganizationProfile, mkPendingRank, mkPractitionerProfile, profilesValidatorScriptHash, ranksValidatorScriptHash)
import Onchain.Utils
import PlutusCore.Builtin.Debug (plcVersion110)
import PlutusLedgerApi.V1 (isZero)
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V1.Interval (before)
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.Contexts
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude
import Prelude qualified

-- | Custom redeemer :
data MintingRedeemer
  = CreateProfile TxOutRef MetadataFields OnChainProfileType POSIXTime Integer
  | Promote ProfileId ProfileId POSIXTime Integer
  | BurnProfileId
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''MintingRedeemer [('CreateProfile, 0), ('Promote, 1), ('BurnProfileId, 2)]

type ProfilesDatum = CIP68Datum OnchainProfile

--------------------------------------
-- Minting Policy
--------------------------------------

{-# INLINEABLE mintingPolicyLambda #-}
mintingPolicyLambda :: ProtocolParams -> ScriptContext -> Bool
mintingPolicyLambda protocolParams (ScriptContext txInfo@TxInfo {..} (Redeemer bredeemer) scriptInfo) =
  let redeemer = unsafeFromBuiltinData @MintingRedeemer bredeemer
      minValue = V1.lovelaceValue 3_500_000
   in case scriptInfo of
        (MintingScript mintingPolicyCurrencySymbol) ->
          let profilesValidatorAddress = V1.scriptHashAddress (profilesValidatorScriptHash protocolParams)
           in case redeemer of
                CreateProfile seedTxOutRef metadata profileType creationDate rankNumber ->
                  --- TODO: add restriction on rankNumber > 0c
                  let (profileRefTN, profileUserTN) = generateRefAndUserTN $ nameFromTxOutRef seedTxOutRef
                      profileRefAssetClass = V1.AssetClass (mintingPolicyCurrencySymbol, profileRefTN)
                      profileUserAssetClass = V1.AssetClass (mintingPolicyCurrencySymbol, profileUserTN)
                      profileRefNFT = V1.assetClassValue profileRefAssetClass 1
                      profileUserNFT = V1.assetClassValue profileUserAssetClass 1
                   in and
                        [ traceIfFalse "Creation date must be before the tx validity range"
                            $ creationDate
                            `before` txInfoValidRange,
                          traceIfFalse "Must spend seed TxOutRef"
                            $ any ((== seedTxOutRef) . txInInfoOutRef) txInfoInputs
                        ]
                        && case profileType of
                          Practitioner ->
                            let (profile, rankDatum) = mkPractitionerProfile profileRefAssetClass creationDate protocolParams rankNumber
                                profileDatum = mkCIP68Datum profile metadata -- !!! Open unbounded-datum vulnerability on metadata
                                rankAssetClass = rankId rankDatum
                                rankNFT = V1.assetClassValue rankAssetClass 1
                                ranksValidatorAddress = V1.scriptHashAddress (ranksValidatorScriptHash protocolParams)
                             in and
                                  [ traceIfFalse "Must lock profile Ref NFT with inline datum at profilesValidator address"
                                      $ hasTxOutWithInlineDatumAndValue profileDatum (profileRefNFT + minValue) profilesValidatorAddress txInfoOutputs,
                                    traceIfFalse "Tx must mint JUST  OnchainProfile Ref and User NFTs and Rank NFT"
                                      $ mintValueMinted txInfoMint -- protection against other-token-name attack vector  -- protection against other-token-name attack vector
                                      -- protection against other-token-name attack vector
                                      == (profileRefNFT + profileUserNFT + rankNFT),
                                    traceIfFalse "Must lock rank NFT with inline datum at ranksValidator address"
                                      $ hasTxOutWithInlineDatumAndValue rankDatum (rankNFT + minValue) ranksValidatorAddress txInfoOutputs
                                  ]
                          Organization ->
                            let profile = mkOrganizationProfile profileRefAssetClass protocolParams
                                profileDatum = mkCIP68Datum profile metadata -- !!! Open unbounded-datum vulnerability on metadata
                             in and
                                  [ traceIfFalse "Must lock profile Ref NFT with inline datum at profilesValidator address"
                                      $ hasTxOutWithInlineDatumAndValue profileDatum (profileRefNFT + minValue) profilesValidatorAddress txInfoOutputs,
                                    traceIfFalse "Tx must mint JUST Ref and User NFTs"
                                      $ mintValueMinted txInfoMint -- protection against other-token-name attack vector  -- protection against other-token-name attack vector
                                      -- protection against other-token-name attack vector
                                      == (profileRefNFT + profileUserNFT)
                                  ]
                (Promote profileId awardedByRef achievementDate rankNumber) ->
                  let ranksValidatorAddress = V1.scriptHashAddress (ranksValidatorScriptHash protocolParams)
                      pendingRankAssetClass = generateRankId profileId rankNumber
                      pendingRankNFT = V1.assetClassValue pendingRankAssetClass 1
                      pendingRankDatum = mkPendingRank profileId awardedByRef achievementDate rankNumber protocolParams
                      awardedByUser = deriveUserFromRefAC awardedByRef
                   in and
                        [ traceIfFalse "Must spend user NFT of the profile who awards the promotion"
                            $ V1.assetClassValueOf (valueSpent txInfo) awardedByUser
                            == 1,
                          traceIfFalse "Must lock pending rank NFT with inline datum at ranksValidator address"
                            $ hasTxOutWithInlineDatumAndValue pendingRankDatum (pendingRankNFT + minValue) ranksValidatorAddress txInfoOutputs
                        ]
                BurnProfileId ->
                  -- Anyone is free to burn
                  traceIfFalse "Tx must not mint any other tokens"
                    $ isZero -- protection against other mints -- protection against other mints
                    $ mintValueMinted txInfoMint
        _ -> traceError "Invalid purpose"

-- | Lose the types
{-# INLINEABLE mintingPolicyUntyped #-}
mintingPolicyUntyped :: ProtocolParams -> BuiltinData -> BuiltinUnit
mintingPolicyUntyped params =
  mkUntypedLambda (mintingPolicyLambda params)

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
mintingPolicyCompile :: ProtocolParams -> CompiledCode (BuiltinData -> BuiltinUnit)
mintingPolicyCompile params = $$(compile [||mintingPolicyUntyped||]) `unsafeApplyCode` liftCode plcVersion110 params
