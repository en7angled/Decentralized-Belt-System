{-# LANGUAGE DataKinds #-}

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
import TxBuilding.Context
import TxBuilding.Interactions
import TxBuilding.Validators

------------------------------------------------------------------------------------------------

-- * Test Runs Helpers

------------------------------------------------------------------------------------------------

deployReferenceScriptRun :: (GYTxGameMonad m, HasCallStack) => GYScript PlutusV3 -> User -> User -> m GYTxOutRef
deployReferenceScriptRun validator fromWallet toWallet = asUser fromWallet $ addRefScript (userChangeAddress toWallet) (validatorToScript validator)

---------------------------------------------------------------------------------

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
  (skeleton, gyAC) <-
    runReaderT
      ( interactionToTxSkeleton
          ( Interaction
              { action = ProfileAction actionType,
                userAddresses = UserAddresses (toList $ User.userAddresses user) (User.userChangeAddress user) Nothing,
                recipient = mrecipient
              }
          )
      )
      txBuildingContext
  (_, gyTxId) <- sendSkeleton' skeleton
  return (gyTxId, gyAC)
