{-# LANGUAGE OverloadedStrings #-}

module UnitTests where

import GHC.Stack
import Test.Tasty
import UnitTests.Achievement (achievementTests)
import UnitTests.ApiHardening (apiHardeningTests)
import UnitTests.ChainSyncReplay (chainSyncReplayTests)
import UnitTests.Cleanup (cleanupTests)
import UnitTests.Conversions (conversionsTests)
import UnitTests.MCPTools (mcpToolsTests)
import UnitTests.Membership (membershipTests)
import UnitTests.Oracle (oracleTests)
import UnitTests.Promotion (promotionTests)
import UnitTests.StorageRoundtrip (storageRoundtripTests)

unitTests :: (HasCallStack) => TestTree
unitTests =
  testGroup
    "BJJ Unit Tests"
    [ oracleTests,
      promotionTests,
      membershipTests,
      achievementTests,
      cleanupTests,
      mcpToolsTests,
      chainSyncReplayTests,
      apiHardeningTests,
      conversionsTests,
      storageRoundtripTests
    ]
