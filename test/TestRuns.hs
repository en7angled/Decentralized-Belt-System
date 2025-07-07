{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module TestRuns where

import Control.Monad.Reader
import Data.Foldable.Extra
import DomainTypes.Profile.Types (ProfileActionType)
import GHC.Stack
import GeniusYield.Test.Clb (sendSkeleton')
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder hiding (userAddresses)
import qualified GeniusYield.TxBuilder.User as User
import GeniusYield.Types
import Onchain.CIP68
import Onchain.Protocol (OnchainRank(..), OnchainProfile(..), getCurrentRankId)
import PlutusLedgerApi.V3
import TxBuilding.Context
import TxBuilding.Interactions
import TxBuilding.Lookups
import TxBuilding.Validators
import Utils

------------------------------------------------------------------------------------------------

-- * Test Runs Helpers

------------------------------------------------------------------------------------------------

deployReferenceScriptRun :: (GYTxGameMonad m, HasCallStack) => GYScript PlutusV3 -> User -> User -> m GYTxOutRef
deployReferenceScriptRun validator fromWallet toWallet = asUser fromWallet $ addRefScript (userChangeAddress toWallet) (validatorToScript validator)

------------------------------------------------------------------------------------------------

-- * Test Runs

------------------------------------------------------------------------------------------------

deployBJJValidators :: (GYTxGameMonad m, HasCallStack) => User -> m ProfileTxBuildingContext
deployBJJValidators w = do
  refProfilesValidator <- deployReferenceScriptRun profilesValidatorGY w w
  refRanksValidator <- deployReferenceScriptRun ranksValidatorGY w w
  refMintingPolicy <- deployReferenceScriptRun mintingPolicyGY w w
  return
    ProfileTxBuildingContext
      { profilesValidatorRef = refProfilesValidator,
        ranksValidatorRef = refRanksValidator,
        mintingPolicyRef = refMintingPolicy
      }

bjjInteraction :: (GYTxGameMonad m, HasCallStack) => ProfileTxBuildingContext -> User -> ProfileActionType -> Maybe GYAddress -> m (GYTxId, GYAssetClass)
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

logProfileState :: (GYTxGameMonad m, HasCallStack) => User -> GYAssetClass -> m (CIP68Datum OnchainProfile, Value)
logProfileState user profileRefAC = asUser user $ do
  (profile, value) <- getProfileStateDataAndValue profileRefAC
  gyLogInfo' ("TESTLOG" :: GYLogNamespace) $ greenColorString $ "PROFILE STATE: \n" <> show profile
  gyLogInfo' ("TESTLOG" :: GYLogNamespace) $ greenColorString $ "PROFILE VALUE: \n" <> show value
  return (profile, value)

logRankState :: (GYTxGameMonad m, HasCallStack) => User -> GYAssetClass -> m (OnchainRank, Value)
logRankState user rankRefAC = asUser user $ do
  (rank, value) <- getRankStateDataAndValue rankRefAC
  gyLogInfo' ("TESTLOG" :: GYLogNamespace) $ blueColorString $ "RANK STATE: \n" <> show rank
  gyLogInfo' ("TESTLOG" :: GYLogNamespace) $ blueColorString $ "RANK VALUE: \n" <> show value
  return (rank, value)

logProfileAndRank :: (GYTxGameMonad m, HasCallStack) => User -> GYAssetClass -> m ((CIP68Datum OnchainProfile, Value), (OnchainRank, Value))
logProfileAndRank user profileRefAC = do
  (profile, profileValue) <- logProfileState user profileRefAC
  rankRefAC <- assetClassFromPlutus' $ getCurrentRankId $ extra profile
  (rank, rankValue) <- logRankState user rankRefAC
  return ((profile, profileValue), (rank, rankValue))

