-- Required for `makeLift`:
{-# LANGUAGE MultiParamTypeClasses #-}
-- Required for `makeLift`:
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:optimize #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}

{-# HLINT ignore "Use &&" #-}

-- | Profiles validator governing profile lifecycle, image updates,
-- and promotion acceptance.
module Onchain.ProfilesValidator
  ( -- * Profiles Redeemer
    ProfilesRedeemer (..),
    ProfilesDatum,

    -- * Profiles Validator
    profilesLambda,

    -- * Compilation
    profilesCompile,
  )
where

import GHC.Generics (Generic)
import Onchain.CIP68 (CIP68Datum (CIP68Datum), ImageURI, deriveUserFromRefAC, updateCIP68DatumImage, validateImageURI)
import Onchain.Protocol (OnchainProfile (..), OnchainRank (..), RankId, getCurrentRankId, promoteProfileDatum, protocolParams, ranksValidatorScriptHash, unsafeGetRankDatumAndValue)
import Onchain.Protocol qualified as Onchain
import Onchain.Utils (hasCurrencySymbol)
import Onchain.Utils qualified as Utils
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.Contexts
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude
import Prelude qualified

-------------------------------------------------------------------------------

-- * Profiles Redeemer

-------------------------------------------------------------------------------

-- | Custom redeemer :
-- NOTE: Profile deletion is intentionally not supported to preserve lineage integrity.
-- BJJ belt records are permanent historical facts that should not be erasable.
-- Output indices are passed in the redeemer for efficient O(1) lookup instead of O(n) search.
data ProfilesRedeemer
  = -- | UpdateProfileImage newImageURI profileOutputIdx
    UpdateProfileImage ImageURI Integer
  | -- | AcceptPromotion rankId profileOutputIdx
    -- NOTE: rankOutputIdx was removed (R2 redundancy — see OnchainSecurityAudit.md).
    -- RanksValidator has only one redeemer (PromotionAcceptance) and always validates
    -- the rank output, so PV's rank output check was genuinely redundant.
    AcceptPromotion RankId Integer
  | -- | Permissionless dust/griefing cleanup. Anyone can spend a UTxO at the
    -- validator address if its datum is absent or does not parse as a valid
    -- protocol datum. Legitimate protocol UTxOs (with valid datums) are rejected.
    Cleanup
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''ProfilesRedeemer [('UpdateProfileImage, 0), ('AcceptPromotion, 1), ('Cleanup, 2)]

type ProfilesDatum = CIP68Datum Onchain.OnchainProfile

-------------------------------------------------------------------------------

-- * Profiles Validator

-------------------------------------------------------------------------------

{-# INLINEABLE profilesLambda #-}
profilesLambda :: ScriptContext -> Bool
profilesLambda (ScriptContext txInfo@TxInfo {..} (Redeemer bredeemer) scriptInfo) =
  let redeemer = unsafeFromBuiltinData @ProfilesRedeemer bredeemer
   in case scriptInfo of
        (SpendingScript spendingTxOutRef mdatum) -> case redeemer of
          -- Permissionless cleanup: allow spending if datum is absent or unparseable.
          Cleanup -> case mdatum of
            Nothing -> True
            Just (Datum bd) -> case fromBuiltinData @(CIP68Datum OnchainProfile) bd of
              Nothing -> True
              Just _ -> traceError "3" -- Cannot cleanup valid datum
          _ -> case mdatum of
            Nothing -> traceError "4" -- No datum
            Just (Datum bdatum) -> case fromBuiltinData bdatum of
              Nothing -> traceError "5" -- Invalid datum
              Just profileDatum@(CIP68Datum _metadata _version (profile :: OnchainProfile)) ->
                let ownInput = Utils.unsafeFindOwnInputByTxOutRef spendingTxOutRef txInfoInputs
                    ownValue = txOutValue ownInput
                    ownAddress = txOutAddress ownInput
                    profileRefAssetClass@(V1.AssetClass (mintingPolicyCurrencySymbol, _)) = profileId profile --  as in datum.
                    -- It is important to validate that the value of this UTxO contains an NFT of this asset class.
                 in case redeemer of
                      (UpdateProfileImage newImageURI profileOutputIdx) ->
                        handleUpdateProfileImage txInfo ownValue ownAddress profileRefAssetClass profileDatum newImageURI profileOutputIdx
                      (AcceptPromotion promotionId profileOutputIdx) ->
                        handleAcceptPromotion txInfo ownValue ownAddress profileRefAssetClass mintingPolicyCurrencySymbol profile profileDatum promotionId profileOutputIdx
        _ -> traceError "6" -- Invalid purpose

-------------------------------------------------------------------------------

-- * Per-Redeemer Handlers

-------------------------------------------------------------------------------

{-# INLINEABLE handleUpdateProfileImage #-}
handleUpdateProfileImage ::
  TxInfo ->
  Value ->
  Address ->
  V1.AssetClass ->
  CIP68Datum OnchainProfile ->
  ImageURI ->
  Integer ->
  Bool
handleUpdateProfileImage txInfo@TxInfo {..} ownValue ownAddress profileRefAssetClass profileDatum newImageURI profileOutputIdx =
  let newCip68Datum = updateCIP68DatumImage newImageURI profileDatum
      profileUserAssetClass = deriveUserFromRefAC profileRefAssetClass
   in and
        [ traceIfFalse "7" $ V1.assetClassValueOf ownValue profileRefAssetClass == 1, -- Own value has Ref NFT
          traceIfFalse "8" $ V1.assetClassValueOf (valueSpent txInfo) profileUserAssetClass == 1, -- Must spend User NFT
          validateImageURI newImageURI,
          traceIfFalse "9" $ Utils.checkTxOutAtIndexWithDatumValueAndAddress profileOutputIdx newCip68Datum ownValue ownAddress txInfoOutputs -- Lock updated profile
        ]

{-# INLINEABLE handleAcceptPromotion #-}
handleAcceptPromotion ::
  TxInfo ->
  Value ->
  Address ->
  V1.AssetClass ->
  CurrencySymbol ->
  OnchainProfile ->
  CIP68Datum OnchainProfile ->
  RankId ->
  Integer ->
  Bool
handleAcceptPromotion TxInfo {..} ownValue ownAddress profileRefAssetClass mintingPolicyCurrencySymbol profile profileDatum promotionId profileOutputIdx =
  let isValidPromotionId = hasCurrencySymbol promotionId mintingPolicyCurrencySymbol
      ranksValidatorAddress = V1.scriptHashAddress $ ranksValidatorScriptHash $ protocolParams profile
      (_promotionValue, pendingRankDatum) = unsafeGetRankDatumAndValue promotionId ranksValidatorAddress txInfoInputs

      -- Get current rank from reference inputs to validate promotion is still valid
      currentRankId = getCurrentRankId profile
      (_, currentRankDatum) = unsafeGetRankDatumAndValue currentRankId ranksValidatorAddress txInfoReferenceInputs

      -- Validate the promotion is still valid given current state
      currentBelt = rankNumber currentRankDatum
      currentBeltDate = rankAchievementDate currentRankDatum
      nextBelt = promotionRankNumber pendingRankDatum
      nextBeltDate = promotionAchievementDate pendingRankDatum

      isPromotionStillValid =
        and
          [ traceIfFalse "A" isValidPromotionId, -- Promotion ID CS check
            traceIfFalse "B" $ nextBelt > currentBelt, -- Already at or past this rank
            traceIfFalse "C" $ nextBeltDate > currentBeltDate -- Date must be after current
          ]

      -- R4 optimization: use promoteProfileDatum instead of promoteProfile
      -- to avoid constructing the full 7-field Rank record that PV doesn't need.
      -- Rank output validation is handled by RanksValidator (R2).
      updatedProfileCIP68Datum = promoteProfileDatum profileDatum pendingRankDatum
   in -- NOTE: User NFT consent check is NOT needed here because:
      -- 1. AcceptPromotion reads the promotion from txInfoInputs
      -- 2. This means the Promotion UTxO at RanksValidator MUST be spent
      -- 3. RanksValidator always runs when Promotion UTxO is spent
      -- 4. RanksValidator checks User NFT consent (deriveUserFromRefAC)
      -- Therefore, RanksValidator guarantees user consent for this transaction.
      --
      -- NOTE (R2 redundancy removed — see OnchainSecurityAudit.md):
      -- The rank output check was removed from PV because RV has only one
      -- redeemer (PromotionAcceptance) and always validates the rank output.
      -- When PV forces the promotion to be in txInfoInputs, RV must run,
      -- and it has no alternative code path. Removing this also eliminates
      -- the rankOutputIdx parameter from the AcceptPromotion redeemer.
      and
        [ traceIfFalse "D" $ V1.assetClassValueOf ownValue profileRefAssetClass == 1, -- Own value has Ref NFT
          isPromotionStillValid,
          traceIfFalse "E" $ Utils.checkTxOutAtIndexWithDatumValueAndAddress profileOutputIdx updatedProfileCIP68Datum ownValue ownAddress txInfoOutputs -- Lock updated profile
        ]

-------------------------------------------------------------------------------

-- * Compilation

-------------------------------------------------------------------------------

-- | Lose the types
profilesUntyped :: BuiltinData -> BuiltinUnit
profilesUntyped = Utils.mkUntypedLambda profilesLambda

-- | Compile the untyped lambda to a UPLC script and splice back to Haskell.
profilesCompile :: CompiledCode (BuiltinData -> BuiltinUnit)
profilesCompile = $$(compile [||profilesUntyped||])
