{-# LANGUAGE RecordWildCards #-}
module UnitTests where
import Test.Tasty
import GHC.Stack
import GeniusYield.Test.Utils
import GeniusYield.Test.Clb
import Control.Monad
import TestRuns (deployBJJValidators)




unitTests :: (HasCallStack) => TestTree
unitTests =
  testGroup
    "BJJ Unit Tests"
    [ createProfileTests]

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
    createNewProfile TestInfo {..} = void $ deployBJJValidators (w1 testWallets)