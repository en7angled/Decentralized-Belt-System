{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UnitTests where

import Control.Monad
import DomainTypes.Profile.Types
import GHC.Stack
import GeniusYield.Test.Clb
import GeniusYield.Test.Utils
import Test.Tasty
import TestRuns (bjjInteraction, deployBJJValidators)
import TxBuilding.Interactions

unitTests :: (HasCallStack) => TestTree
unitTests =
  testGroup
    "BJJ Unit Tests"
    [createProfileTests]

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
createProfileTests :: (HasCallStack) => TestTree
createProfileTests =
  testGroup
    "CREATE NEW PROFILE TEST CASES"
    [ mkTestFor "Test Case 1.1: Verify that a practitioner can create a profile" createNewProfile
    ]
  where
    createNewProfile :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    createNewProfile TestInfo {..} = do
      txBuildingContext <- deployBJJValidators (w1 testWallets)
      (gyTxId, gyAC) <-
        bjjInteraction
          txBuildingContext
          (w1 testWallets)
          (CreateProfileAction studentProfileData Practitioner 0)
          Nothing
      return ()
