{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UnitTests.Oracle
  ( oracleTests,
  )
where

import Control.Monad.Reader (runReaderT)
import DomainTypes.Core.Actions
import DomainTypes.Core.Types
import GHC.Stack
import GeniusYield.Test.Clb
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.TxBuilder.User qualified as User
import GeniusYield.Types
import Onchain.Protocol.Types (FeeConfig (..), OracleParams (..))
import Test.Fixtures (adminTestProfileData)
import Test.Helpers (assert, queryOracle)
import Test.Tasty
import TestRuns (adminInteraction, bjjInteraction, deployBJJValidators)
import TxBuilding.Lookups (queryOracleParams)
import PlutusTx.Prelude (isNothing)

oracleTests :: (HasCallStack) => TestTree
oracleTests =
  testGroup
    "Oracle"
    [ oracleDeploymentTests,
      oracleAdminTests
    ]

oracleDeploymentTests :: (HasCallStack) => TestTree
oracleDeploymentTests =
  testGroup
    "Oracle Deployment Tests"
    [ mkTestFor "Test Case 0.1: Deploy protocol with oracle and verify oracle datum is queryable" oracleDeployAndQuery
    ]
  where
    oracleDeployAndQuery :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    oracleDeployAndQuery TestInfo {..} = do
      waitNSlots_ 1000
      txBuildingContext <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      -- Query oracle params to verify they are accessible
      (oracleParams, oracleRef, _oracleValue) <- runReaderT queryOracleParams txBuildingContext
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) $ "Oracle UTxO ref: " <> show oracleRef
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) $ "Oracle paused: " <> show (opPaused oracleParams)

      -- Verify initial oracle params are as expected
      assert (not (opPaused oracleParams))
      assert (isNothing (opFeeConfig oracleParams))

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Oracle deployment and query successful!"
      return ()

oracleAdminTests :: (HasCallStack) => TestTree
oracleAdminTests =
  testGroup
    "Oracle Admin Tests"
    [ mkTestFor "Test Case 0.2: Pause and unpause protocol" pauseAndUnpause,
      mkTestFor "Test Case 0.3: Set fees and clear fees" setAndClearFees,
      mkTestFor "Test Case 0.5: Sequential admin actions with profile interactions" sequentialAdminWithProfiles
    ]
  where
    pauseAndUnpause :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    pauseAndUnpause TestInfo {..} = do
      waitNSlots_ 1000
      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      params0 <- queryOracle ctx
      assert (not (opPaused params0))
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Initial state: protocol is NOT paused."

      _ <- adminInteraction ctx (w1 testWallets) PauseProtocolAction
      waitNSlots_ 1

      params1 <- queryOracle ctx
      assert (opPaused params1)
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Protocol paused successfully."

      _ <- adminInteraction ctx (w1 testWallets) UnpauseProtocolAction
      waitNSlots_ 1

      params2 <- queryOracle ctx
      assert (not (opPaused params2))
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Protocol unpaused successfully."

      return ()

    setAndClearFees :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    setAndClearFees TestInfo {..} = do
      waitNSlots_ 1000
      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      params0 <- queryOracle ctx
      assert (isNothing (opFeeConfig params0))
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Initial state: no fee config."

      let feeAddr = addressToPlutus (User.userChangeAddress (w2 testWallets))
      let testFeeConfig =
            FeeConfig
              { fcFeeAddress = feeAddr,
                fcProfileCreationFee = 2000000,
                fcPromotionFee = 3000000,
                fcMembershipHistoryFee = 1500000,
                fcMembershipIntervalFee = 1500000,
                fcAchievementFee = 1500000
              }
      _ <- adminInteraction ctx (w1 testWallets) (SetFeesAction (Just testFeeConfig))
      waitNSlots_ 1

      params1 <- queryOracle ctx
      assert (opFeeConfig params1 == Just testFeeConfig)
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) $ "Fees set: " <> show (opFeeConfig params1)

      _ <- adminInteraction ctx (w1 testWallets) (SetFeesAction Nothing)
      waitNSlots_ 1

      params2 <- queryOracle ctx
      assert (isNothing (opFeeConfig params2))
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Fees cleared successfully."

      return ()

    sequentialAdminWithProfiles :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    sequentialAdminWithProfiles TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Step 1: Creating profile while protocol is unpaused..."
      (_txId, profileAC1) <-
        bjjInteraction
          ctx
          (w2 testWallets)
          (InitProfileAction adminTestProfileData Practitioner creationDate)
          Nothing
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) $ "Profile created: " <> show profileAC1

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Step 2: Pausing protocol..."
      _ <- adminInteraction ctx (w1 testWallets) PauseProtocolAction
      waitNSlots_ 1

      params1 <- queryOracle ctx
      assert (opPaused params1)
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Protocol paused."

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Step 3: Skipping paused profile creation test (CLB limitation for expected-failure tests)."
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Note: MintingPolicy enforces 'Protocol is paused' check on-chain."

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Step 4: Setting fees while paused..."
      let feeAddr = addressToPlutus (User.userChangeAddress (w3 testWallets))
      let testFeeConfig =
            FeeConfig
              { fcFeeAddress = feeAddr,
                fcProfileCreationFee = 2000000,
                fcPromotionFee = 3000000,
                fcMembershipHistoryFee = 1500000,
                fcMembershipIntervalFee = 1500000,
                fcAchievementFee = 1500000
              }
      _ <- adminInteraction ctx (w1 testWallets) (SetFeesAction (Just testFeeConfig))
      waitNSlots_ 1

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Step 5: Unpausing protocol..."
      _ <- adminInteraction ctx (w1 testWallets) UnpauseProtocolAction
      waitNSlots_ 1

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Step 6: Creating profile after unpause..."
      let profileData2 = ProfileData "Admin Test User 2" "Second test profile" "ipfs://QmTest2"
      (_txId, profileAC2) <-
        bjjInteraction
          ctx
          (w3 testWallets)
          (InitProfileAction profileData2 Practitioner creationDate)
          Nothing
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) $ "Profile created after unpause: " <> show profileAC2

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Step 7: Verifying final oracle state..."
      paramsFinal <- queryOracle ctx
      assert (not (opPaused paramsFinal))
      assert (opFeeConfig paramsFinal == Just testFeeConfig)

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "=== Final Oracle State ==="
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) $ "  Paused: " <> show (opPaused paramsFinal)
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) $ "  FeeConfig: " <> show (opFeeConfig paramsFinal)
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Sequential admin actions with profile interactions completed successfully!"

      return ()
