{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UnitTests.Cleanup
  ( cleanupTests,
  )
where

import DomainTypes.Core.Actions
import DomainTypes.Core.Types
import GHC.Stack
import GeniusYield.Test.Clb
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types
import Test.Fixtures (cleanupTestProfileData)
import Test.Tasty
import TestRuns (bjjInteraction, deployBJJValidators, logPractitionerProfileInformation, protocolInteraction, sendDustToValidator)
import TxBuilding.Validators (profilesValidatorGY, ranksValidatorGY)

cleanupTests :: (HasCallStack) => TestTree
cleanupTests =
  testGroup
    "Dust Cleanup Tests"
    [ mkTestFor "Test Case 3.1: Cleanup dust at ProfilesValidator" cleanupDustAtProfiles,
      mkTestFor "Test Case 3.2: Cleanup dust at RanksValidator" cleanupDustAtRanks,
      mkTestFor "Test Case 3.3: Cleanup dust at both validators in a single transaction" cleanupDustAtBothValidators,
      mkTestFor "Test Case 3.4: Cleanup does not affect legitimate protocol UTxOs" cleanupSafeWithProtocolState
    ]
  where
    -- Test Case 3.1: Send dust to ProfilesValidator and clean it up
    cleanupDustAtProfiles :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    cleanupDustAtProfiles TestInfo {..} = do
      waitNSlots_ 1000
      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      -- Simulate griefing: send ADA-only UTxO to profiles validator
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Sending dust UTxO to ProfilesValidator..."
      sendDustToValidator (w2 testWallets) profilesValidatorGY
      waitNSlots_ 1

      -- Any user (w3) can clean it up — permissionless
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Cleaning up dust via ProtocolAction CleanupDustAction..."
      _txId <- protocolInteraction ctx (w3 testWallets) CleanupDustAction
      waitNSlots_ 1

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Dust cleanup at ProfilesValidator succeeded!"
      return ()

    -- Test Case 3.2: Send dust to RanksValidator and clean it up
    cleanupDustAtRanks :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    cleanupDustAtRanks TestInfo {..} = do
      waitNSlots_ 1000
      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      -- Simulate griefing: send ADA-only UTxO to ranks validator
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Sending dust UTxO to RanksValidator..."
      sendDustToValidator (w2 testWallets) ranksValidatorGY
      waitNSlots_ 1

      -- Any user (w3) can clean it up — permissionless
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Cleaning up dust via ProtocolAction CleanupDustAction..."
      _txId <- protocolInteraction ctx (w3 testWallets) CleanupDustAction
      waitNSlots_ 1

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Dust cleanup at RanksValidator succeeded!"
      return ()

    -- Test Case 3.3: Send dust to both validators and clean up in a single transaction
    cleanupDustAtBothValidators :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    cleanupDustAtBothValidators TestInfo {..} = do
      waitNSlots_ 1000
      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      -- Simulate griefing at both validators
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Sending dust to ProfilesValidator and RanksValidator..."
      sendDustToValidator (w2 testWallets) profilesValidatorGY
      sendDustToValidator (w2 testWallets) ranksValidatorGY
      waitNSlots_ 1

      -- Single cleanup transaction sweeps both
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Cleaning up dust at both validators in a single transaction..."
      _txId <- protocolInteraction ctx (w3 testWallets) CleanupDustAction
      waitNSlots_ 1

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Dust cleanup at both validators succeeded!"
      return ()

    -- Test Case 3.4: Dust cleanup does not affect legitimate protocol UTxOs
    -- After creating a profile (which locks UTxOs at ProfilesValidator and RanksValidator),
    -- send dust to both validators, then clean up. Verify the profile is still intact.
    cleanupSafeWithProtocolState :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    cleanupSafeWithProtocolState TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      -- Create a legitimate profile (locks UTxOs at PV and RV)
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Creating legitimate profile..."
      (_txId, profileAC) <-
        bjjInteraction
          ctx
          (w1 testWallets)
          (InitProfileAction cleanupTestProfileData Practitioner creationDate)
          Nothing
      waitNSlots_ 1

      -- Verify profile is accessible
      logPractitionerProfileInformation (w1 testWallets) profileAC
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Profile created and verified."

      -- Simulate griefing at both validators
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Sending dust to both validators alongside legitimate state..."
      sendDustToValidator (w2 testWallets) profilesValidatorGY
      sendDustToValidator (w2 testWallets) ranksValidatorGY
      waitNSlots_ 1

      -- Clean up dust
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Cleaning up dust (should NOT touch legitimate protocol UTxOs)..."
      _txId <- protocolInteraction ctx (w3 testWallets) CleanupDustAction
      waitNSlots_ 1

      -- Verify profile is still intact after cleanup
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Verifying profile is still accessible after cleanup..."
      logPractitionerProfileInformation (w1 testWallets) profileAC
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Profile intact — cleanup correctly skipped legitimate protocol UTxOs!"
      return ()

-- ------------------------------------------------------------------------------------------------
-- Security Test Documentation (not included in automated test suite)
--
-- MALICIOUS AcceptPromotion TEST - MANUALLY VERIFIED
--
-- A test was conducted to verify RanksValidator security. The test:
-- 1. Creates a master and student profile
-- 2. Master promotes student to Blue
-- 3. An attacker (w3) attempts to accept the promotion WITHOUT spending student's User NFT
--
-- RESULT: Transaction FAILS at RanksValidator with error:
--   "Must spend profile User NFT to accept promotion"
--
-- This proves the on-chain security is working correctly:
-- - RanksValidator (via deriveUserFromRefAC) correctly validates User NFT consent
-- - ProfilesValidator intentionally omits this check (avoiding redundancy)
-- - The architecture is secure by design
--
-- The test code is preserved in TestRuns.hs (maliciousBjjAcceptPromotion) for reference.
-- ------------------------------------------------------------------------------------------------
