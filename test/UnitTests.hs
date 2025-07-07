{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UnitTests where

import Control.Monad
import DomainTypes.Profile.Types
import GHC.Stack
import GeniusYield.Test.Clb
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types
import Onchain.CIP68
import Test.Tasty
import TestRuns (bjjInteraction, deployBJJValidators, logProfileAndRank)
import TxBuilding.Interactions

unitTests :: (HasCallStack) => TestTree
unitTests =
  testGroup
    "BJJ Unit Tests"
    [promotionTests]

studentProfileData =
  ProfileData
    { profileName = "John Doe",
      profileDescription = "John Doe is a student",
      profileImageURI = "ipfs://QmReBRNMe7tBr6WbA89uwnHHW7f7Zoe8wY2mzVpA8STdAk"
    }

masterProfileData =
  ProfileData
    { profileName = "Master",
      profileDescription = "Master is a master",
      profileImageURI = "ipfs://Qmb3JXJHQxuReSUaH6rXAoP5oX9NRs6JmnRFGTj2RVhGwe"
    }

-- ------------------------------------------------------------------------------------------------

-- -- * Create new raffle scenarios

-- ------------------------------------------------------------------------------------------------
promotionTests :: (HasCallStack) => TestTree
promotionTests =
  testGroup
    "Promotion Tests"
    [ mkTestFor "Test Case 1.1: Verify that a black belt can promote a white belt to blue belt and the white belt can accept the promotion" blackPromotesWhiteToBlue
    ]
  where
    blackPromotesWhiteToBlue :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    blackPromotesWhiteToBlue TestInfo {..} = do
      txBuildingContext <- deployBJJValidators (w1 testWallets)
      (gyTxId, blackBeltAC) <-
        bjjInteraction
          txBuildingContext
          (w1 testWallets)
          (CreateProfileAction masterProfileData Practitioner 4)
          Nothing

      ((blackBeltProfile, blackBeltValue), (blackBeltRank, blackBeltRankValue)) <- logProfileAndRank (w1 testWallets) blackBeltAC

      (gyTxId, whiteBeltAC) <-
        bjjInteraction
          txBuildingContext
          (w1 testWallets)
          (CreateProfileAction studentProfileData Practitioner 0)
          Nothing

      ((whiteBeltProfile, whiteBeltValue), (whiteBeltRank, whiteBeltRankValue)) <- logProfileAndRank (w1 testWallets) whiteBeltAC

      return ()
