{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module TestRuns where

import Control.Monad.Reader
import Data.Foldable.Extra
import DomainTypes.Core.Actions
import DomainTypes.Core.Types
import GHC.Stack
import GeniusYield.Test.Clb (sendSkeleton')
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder hiding (userAddresses)
import GeniusYield.TxBuilder.User qualified as User
import GeniusYield.Types
import Onchain.CIP68
import Onchain.ProfilesValidator (ProfilesRedeemer (..))
import Onchain.Protocol (OnchainProfile (..), OnchainRank (..), getCurrentRankId, promoteProfile)
import Onchain.Protocol qualified as Onchain
import PlutusLedgerApi.V3
import TxBuilding.Context
import TxBuilding.Interactions
import TxBuilding.Lookups
import TxBuilding.Skeletons
import TxBuilding.Validators
import Utils

------------------------------------------------------------------------------------------------

-- * Test Runs Helpers

------------------------------------------------------------------------------------------------

deployReferenceScriptRun :: (GYTxGameMonad m, HasCallStack) => GYScript PlutusV3 -> User -> User -> m (GYScriptHash, GYTxOutRef)
deployReferenceScriptRun validator fromWallet toWallet = asUser fromWallet $ do
  ref <- addRefScript (userChangeAddress toWallet) (validatorToScript validator)
  return (scriptHash validator, ref)

------------------------------------------------------------------------------------------------

-- * Test Runs

------------------------------------------------------------------------------------------------

deployBJJValidators :: (GYTxGameMonad m, HasCallStack) => User -> m DeployedScriptsContext
deployBJJValidators w = do
  (pVhash, refProfilesValidator) <- deployReferenceScriptRun profilesValidatorGY w w
  (rVhash, refRanksValidator) <- deployReferenceScriptRun ranksValidatorGY w w
  (mphash, refMintingPolicy) <- deployReferenceScriptRun mintingPolicyGY w w

  -- Log deployed script sizes for reporting
  gyLogInfo' ("SCRIPTSIZE" :: GYLogNamespace) $ cyanColorString $
    "SCRIPT_SIZES:\n" <>
    "  MintingPolicy: " <> show mintingPolicySize <> " bytes\n" <>
    "  ProfilesValidator: " <> show profilesValidatorSize <> " bytes\n" <>
    "  RanksValidator: " <> show ranksValidatorSize <> " bytes\n" <>
    "  MembershipsValidator: " <> show membershipsValidatorSize <> " bytes"

  return
    DeployedScriptsContext
      { profilesValidatorHashAndRef = (pVhash, refProfilesValidator),
        ranksValidatorHashAndRef = (rVhash, refRanksValidator),
        mintingPolicyHashAndRef = (mphash, refMintingPolicy)
      }

bjjInteraction :: (GYTxGameMonad m, HasCallStack) => DeployedScriptsContext -> User -> ProfileActionType -> Maybe GYAddress -> m (GYTxId, GYAssetClass)
bjjInteraction txBuildingContext user actionType mrecipient = asUser user $ do
  let interaction =
        Interaction
          { action = ProfileAction actionType,
            userAddresses = UserAddresses (toList $ User.userAddresses user) (User.userChangeAddress user) Nothing,
            recipient = mrecipient
          }
  (skeleton, gyAC) <- runReaderT (interactionToTxSkeleton interaction) txBuildingContext
  (gyTxBody, gyTxId) <- sendSkeleton' skeleton
  
  -- Log execution units / script budget information
  gyLogInfo' ("TESTLOG" :: GYLogNamespace) $ yellowColorString $ "INTERACTION: \n" <> show interaction
  logTxBudget gyTxBody
  
  return (gyTxId, gyAC)

-- | Log transaction budget information including fee and script execution units
logTxBudget :: (GYTxQueryMonad m) => GYTxBody -> m ()
logTxBudget txBody = do
  let fee = txBodyFee txBody
  gyLogInfo' ("EXUNITS" :: GYLogNamespace) $ cyanColorString $
    "TX BUDGET:\n" <>
    "  Fee: " <> show fee <> " lovelace\n" <>
    "  TxBody: " <> show txBody

------------------------------------------------------------------------------------------------

-- * QueryRuns

------------------------------------------------------------------------------------------------

logPractitionerProfileInformation :: (GYTxGameMonad m, HasCallStack) => User -> ProfileRefAC -> m ()
logPractitionerProfileInformation user profileRefAC = asUser user $ do
  profileInformation <- getPractiotionerInformation profileRefAC
  gyLogInfo' ("TESTLOG" :: GYLogNamespace) $ greenColorString $ "PRACTITIONER PROFILE INFORMATION: \n" <> show profileInformation
  return ()

getProfileAndRank :: (GYTxGameMonad m, HasCallStack) => GYAssetClass -> m ((CIP68Datum OnchainProfile, Value), (OnchainRank, Value))
getProfileAndRank profileRefAC = do
  (profile, profileValue) <- getProfileStateDataAndValue profileRefAC
  rankRefAC <- assetClassFromPlutus' $ getCurrentRankId $ extra profile
  (rank, rankValue) <- getRankStateDataAndValue rankRefAC
  return ((profile, profileValue), (rank, rankValue))

------------------------------------------------------------------------------------------------

-- * Malicious Transaction Builders (for vulnerability testing)

------------------------------------------------------------------------------------------------

-- | Malicious AcceptPromotion transaction that does NOT spend the student's User NFT
--
-- This tests the security of the AcceptPromotion flow:
-- - ProfilesValidator does NOT check User NFT (by design - avoids redundant check)
-- - RanksValidator DOES check User NFT consent (via deriveUserFromRefAC)
--
-- This malicious transaction will:
-- - PASS ProfilesValidator (no User NFT check - relies on RanksValidator)
-- - FAIL RanksValidator (correctly checks User AC is not spent)
--
-- The architecture intentionally delegates User NFT consent checking to RanksValidator
-- because AcceptPromotion MUST spend the Promotion UTxO, which triggers RanksValidator.
maliciousAcceptPromotionTX ::
  (HasCallStack, GYTxUserQueryMonad m, MonadReader DeployedScriptsContext m) =>
  GYAssetClass ->  -- ^ The promotion ID to accept
  m (GYTxSkeleton 'PlutusV3, GYAssetClass)
maliciousAcceptPromotionTX gyPromotionId = do
  rvRef <- asks getRanksValidatorRef
  pvRef <- asks getProfilesValidatorRef

  (plutusPromotionRankDatum, plutusPromotionRankValue) <- getRankStateDataAndValue gyPromotionId

  let studentProfileRefAC = promotionAwardedTo plutusPromotionRankDatum
  gyStudentProfileRefAC <- assetClassFromPlutus' studentProfileRefAC

  -- MALICIOUS: We intentionally DO NOT spend the student's User NFT
  -- This is what an attacker would do - they don't have the User NFT!
  -- 
  -- Normal code would do:
  --   gyStudentProfileUserAC <- gyDeriveUserFromRefAC gyStudentProfileRefAC
  --   spendsStudentProfileUserNFT <- txMustSpendFromAddress gyStudentProfileUserAC ownAddrs

  -- Output index tracking (order must match skeleton mconcat order)
  -- Output 0: Updated profile state locked at profilesValidator
  -- Output 1: Updated rank state locked at ranksValidator
  let profileOutputIdx = 0 :: Integer

  let gySpendProfileRedeemer = redeemerFromPlutus' . toBuiltinData $ AcceptPromotion (assetClassToPlutus gyPromotionId) profileOutputIdx
  spendsStudentProfileRefNFT <- txMustSpendStateFromRefScriptWithRedeemer pvRef gyStudentProfileRefAC gySpendProfileRedeemer profilesValidatorGY

  (plutusProfileDatum, plutusProfileValue) <- getProfileStateDataAndValue gyStudentProfileRefAC
  let plutusStudentProfile = extra plutusProfileDatum
  let studentCurrentRankId = Onchain.getCurrentRankId plutusStudentProfile
  gyStudentCurrentRankAC <- assetClassFromPlutus' studentCurrentRankId

  let (plutusStudentUpdatedProfileDatum, plutuStudentUpdatedRankDatum) = promoteProfile plutusProfileDatum plutusPromotionRankDatum

  gyProfileValue <- valueFromPlutus' plutusProfileValue
  -- Output 0: Updated profile state
  isLockingUpdatedStudentProfile <- txMustLockStateWithInlineDatumAndValue profilesValidatorGY plutusStudentUpdatedProfileDatum gyProfileValue

  gyRankValue <- valueFromPlutus' plutusPromotionRankValue
  -- Output 1: Updated rank state
  isLockingUpdatedRank <- txMustLockStateWithInlineDatumAndValue ranksValidatorGY plutuStudentUpdatedRankDatum gyRankValue

  spendsPromotionRank <- txMustSpendStateFromRefScriptWithRedeemer rvRef gyPromotionId unitRedeemer ranksValidatorGY

  -- Reference the student's current rank for acceptance-time validation
  referencesCurrentRank <- txMustHaveUTxOsAsRefInputs [gyStudentCurrentRankAC]

  gyRankAC <- assetClassFromPlutus' $ Onchain.rankId plutuStudentUpdatedRankDatum

  gyLogInfo' "MALICIOUS TX" "Building malicious AcceptPromotion WITHOUT student's User NFT..."
  gyLogInfo' "MALICIOUS TX" "This transaction should FAIL at RanksValidator (correct check)"
  gyLogInfo' "MALICIOUS TX" "If it succeeded, ProfilesValidator's bug would be exploitable!"

  return
    ( mconcat
        [ -- NOTICE: No spendsStudentProfileUserNFT here! This is the attack!
          spendsStudentProfileRefNFT,           -- Input (no output index)
          isLockingUpdatedStudentProfile,       -- Output 0: Updated profile state
          spendsPromotionRank,                  -- Input (no output index)
          isLockingUpdatedRank,                 -- Output 1: Updated rank state
          referencesCurrentRank                 -- Reference input (no output index)
        ],
      gyRankAC
    )

-- | Execute a malicious interaction that attempts to accept a promotion without consent
maliciousBjjAcceptPromotion :: 
  (GYTxGameMonad m, HasCallStack) => 
  DeployedScriptsContext -> 
  User ->  -- ^ The attacker (doesn't own student's User NFT)
  GYAssetClass ->  -- ^ The promotion to accept
  m (GYTxId, GYAssetClass)
maliciousBjjAcceptPromotion txBuildingContext attacker promotionId = asUser attacker $ do
  (skeleton, gyAC) <- runReaderT (maliciousAcceptPromotionTX promotionId) txBuildingContext
  gyLogInfo' ("MALICIOUS" :: GYLogNamespace) "Attempting malicious AcceptPromotion..."
  (gyTxBody, gyTxId) <- sendSkeleton' skeleton
  logTxBudget gyTxBody
  return (gyTxId, gyAC)
