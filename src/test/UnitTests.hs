{-# LANGUAGE OverloadedStrings #-}

module UnitTests where

import GHC.Stack
import Test.Tasty
import UnitTests.Achievement (achievementTests)
import UnitTests.Cleanup (cleanupTests)
import UnitTests.Membership (membershipTests)
import UnitTests.Oracle (oracleTests)
import UnitTests.Promotion (promotionTests)

unitTests :: (HasCallStack) => TestTree
unitTests =
  testGroup
    "BJJ Unit Tests"
    [ oracleTests,
      promotionTests,
      membershipTests,
      achievementTests,
      cleanupTests
    ]
