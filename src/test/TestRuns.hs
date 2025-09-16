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
import Onchain.Protocol (OnchainProfile (..), OnchainRank (..), getCurrentRankId)
import PlutusLedgerApi.V3
import TxBuilding.Context
import TxBuilding.Interactions
import TxBuilding.Lookups
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
  (_, gyTxId) <- sendSkeleton' skeleton
  gyLogInfo' ("TESTLOG" :: GYLogNamespace) $ yellowColorString $ "INTERACTION: \n" <> show interaction
  return (gyTxId, gyAC)

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
