{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UnitTests.Achievement
  ( achievementTests,
  )
where

import Control.Monad (void)
import DomainTypes.Core.Actions
import DomainTypes.Core.Types
import GHC.Stack
import GeniusYield.Test.Clb
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types
import Onchain.BJJ (BJJBelt (Black, White))
import Test.Fixtures (achievementProfileData, masterProfileData, orgProfileData, seminarProfileData, studentProfileData)
import Test.Tasty
import TestRuns (bjjInteraction, deployBJJValidators, protocolInteraction, sendDustToValidator)
import TxBuilding.Validators (achievementsValidatorGY)

achievementTests :: (HasCallStack) => TestTree
achievementTests =
  testGroup
    "Achievement Tests"
    [ mkTestFor "Test Case 5.1: Organization awards achievement to practitioner" orgAwardsAchievementTest,
      mkTestFor "Test Case 5.2: Practitioner accepts achievement" practitionerAcceptsAchievementTest,
      mkTestFor "Test Case 5.3: Full achievement lifecycle (award + accept)" fullAchievementLifecycleTest,
      mkTestFor "Test Case 5.4: Double-accept achievement fails (TA)" doubleAcceptAchievementFailTest,
      mkTestFor "Test Case 5.5: Wrong user cannot accept achievement" wrongUserAcceptAchievementFailTest,
      mkTestFor "Test Case 5.6: Cleanup dust at AchievementsValidator" cleanupDustAtAchievementsTest
    ]
  where
    -- Test Case 5.1: Organization awards an achievement to a practitioner
    orgAwardsAchievementTest :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    orgAwardsAchievementTest TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      -- Create Organization (w1)
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Creating Organization profile..."
      (_txId, orgAC) <-
        bjjInteraction
          ctx
          (w1 testWallets)
          (CreateProfileWithRankAction orgProfileData Organization creationDate White)
          Nothing
      waitNSlots_ 1

      -- Create Practitioner (w2)
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Creating Practitioner profile..."
      (_txId, practitionerAC) <-
        bjjInteraction
          ctx
          (w2 testWallets)
          (InitProfileAction studentProfileData Practitioner creationDate)
          Nothing
      waitNSlots_ 1

      -- Organization awards achievement to practitioner
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Organization awarding achievement..."
      s' <- slotOfCurrentBlock
      achievementDate <- slotToBeginTime s'
      let achievementDatePast = timeFromPOSIX $ timeToPOSIX achievementDate - 1000

      (_txId, achievementAC) <-
        bjjInteraction
          ctx
          (w1 testWallets)
          ( AwardAchievementAction
              { aa_awarded_to_profile_id = practitionerAC,
                aa_awarded_by_profile_id = orgAC,
                aa_profile_data = achievementProfileData,
                aa_other_metadata = [],
                aa_achievement_date = achievementDatePast
              }
          )
          Nothing
      waitNSlots_ 1

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) $ "Achievement awarded: " <> show achievementAC
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Organization awards achievement test passed!"
      return ()

    -- Test Case 5.2: Practitioner accepts an achievement
    practitionerAcceptsAchievementTest :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    practitionerAcceptsAchievementTest TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      -- Create Organization (w1) and Practitioner (w2)
      (_txId, orgAC) <-
        bjjInteraction ctx (w1 testWallets)
          (CreateProfileWithRankAction orgProfileData Organization creationDate White)
          Nothing
      waitNSlots_ 1

      (_txId, practitionerAC) <-
        bjjInteraction ctx (w2 testWallets)
          (InitProfileAction studentProfileData Practitioner creationDate)
          Nothing
      waitNSlots_ 1

      -- Organization awards achievement
      s' <- slotOfCurrentBlock
      achievementDate <- slotToBeginTime s'
      let achievementDatePast = timeFromPOSIX $ timeToPOSIX achievementDate - 1000

      (_txId, achievementAC) <-
        bjjInteraction ctx (w1 testWallets)
          ( AwardAchievementAction
              { aa_awarded_to_profile_id = practitionerAC,
                aa_awarded_by_profile_id = orgAC,
                aa_profile_data = achievementProfileData,
                aa_other_metadata = [],
                aa_achievement_date = achievementDatePast
              }
          )
          Nothing
      waitNSlots_ 1

      -- Practitioner accepts the achievement
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Practitioner accepting achievement..."
      void $
        bjjInteraction ctx (w2 testWallets)
          (AcceptAchievementAction achievementAC)
          Nothing
      waitNSlots_ 1

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Achievement accepted!"
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Practitioner accepts achievement test passed!"
      return ()

    -- Test Case 5.3: Full lifecycle
    fullAchievementLifecycleTest :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    fullAchievementLifecycleTest TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      -- Create Master (w1) and Student (w2)
      (_txId, masterAC) <-
        bjjInteraction ctx (w1 testWallets)
          (CreateProfileWithRankAction masterProfileData Practitioner creationDate Black)
          Nothing
      waitNSlots_ 1

      (_txId, studentAC) <-
        bjjInteraction ctx (w2 testWallets)
          (InitProfileAction studentProfileData Practitioner creationDate)
          Nothing
      waitNSlots_ 1

      -- Master awards two achievements to student
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "=== Awarding first achievement ==="
      s' <- slotOfCurrentBlock
      t' <- slotToBeginTime s'
      let achievementDate1 = timeFromPOSIX $ timeToPOSIX t' - 1000

      (_txId, achievement1AC) <-
        bjjInteraction ctx (w1 testWallets)
          ( AwardAchievementAction
              { aa_awarded_to_profile_id = studentAC,
                aa_awarded_by_profile_id = masterAC,
                aa_profile_data = achievementProfileData,
                aa_other_metadata = [],
                aa_achievement_date = achievementDate1
              }
          )
          Nothing
      waitNSlots_ 1

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "=== Awarding second achievement ==="
      s'' <- slotOfCurrentBlock
      t'' <- slotToBeginTime s''
      let achievementDate2 = timeFromPOSIX $ timeToPOSIX t'' - 1000

      (_txId, achievement2AC) <-
        bjjInteraction ctx (w1 testWallets)
          ( AwardAchievementAction
              { aa_awarded_to_profile_id = studentAC,
                aa_awarded_by_profile_id = masterAC,
                aa_profile_data = seminarProfileData,
                aa_other_metadata = [],
                aa_achievement_date = achievementDate2
              }
          )
          Nothing
      waitNSlots_ 1

      -- Student accepts both achievements
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "=== Accepting first achievement ==="
      void $
        bjjInteraction ctx (w2 testWallets)
          (AcceptAchievementAction achievement1AC)
          Nothing
      waitNSlots_ 1

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "=== Accepting second achievement ==="
      void $
        bjjInteraction ctx (w2 testWallets)
          (AcceptAchievementAction achievement2AC)
          Nothing
      waitNSlots_ 1

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Full achievement lifecycle test passed!"
      return ()

    -- Test Case 5.4: Double-accept achievement fails (on-chain trace TA)
    doubleAcceptAchievementFailTest :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    doubleAcceptAchievementFailTest TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      -- Create Organization (w1) and Practitioner (w2)
      (_txId, orgAC) <-
        bjjInteraction ctx (w1 testWallets)
          (CreateProfileWithRankAction orgProfileData Organization creationDate White)
          Nothing
      waitNSlots_ 1

      (_txId, practitionerAC) <-
        bjjInteraction ctx (w2 testWallets)
          (InitProfileAction studentProfileData Practitioner creationDate)
          Nothing
      waitNSlots_ 1

      -- Award achievement
      s' <- slotOfCurrentBlock
      t' <- slotToBeginTime s'
      let achievementDatePast = timeFromPOSIX $ timeToPOSIX t' - 1000

      (_txId, achievementAC) <-
        bjjInteraction ctx (w1 testWallets)
          ( AwardAchievementAction
              { aa_awarded_to_profile_id = practitionerAC,
                aa_awarded_by_profile_id = orgAC,
                aa_profile_data = achievementProfileData,
                aa_other_metadata = [],
                aa_achievement_date = achievementDatePast
              }
          )
          Nothing
      waitNSlots_ 1

      -- First accept should succeed
      void $
        bjjInteraction ctx (w2 testWallets)
          (AcceptAchievementAction achievementAC)
          Nothing
      waitNSlots_ 1

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "First accept succeeded. Attempting double-accept (should fail with TA)..."

      -- Second accept should fail with on-chain trace "TA" (already accepted)
      mustFail $
        void $
          bjjInteraction ctx (w2 testWallets)
            (AcceptAchievementAction achievementAC)
            Nothing

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Double-accept achievement fail test passed!"
      return ()

    -- Test Case 5.5: Wrong user cannot accept achievement
    wrongUserAcceptAchievementFailTest :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    wrongUserAcceptAchievementFailTest TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      -- Create Organization (w1) and Practitioner A (w2)
      (_txId, orgAC) <-
        bjjInteraction ctx (w1 testWallets)
          (CreateProfileWithRankAction orgProfileData Organization creationDate White)
          Nothing
      waitNSlots_ 1

      (_txId, practitionerAC) <-
        bjjInteraction ctx (w2 testWallets)
          (InitProfileAction studentProfileData Practitioner creationDate)
          Nothing
      waitNSlots_ 1

      -- Award achievement to practitioner A (w2)
      s' <- slotOfCurrentBlock
      t' <- slotToBeginTime s'
      let achievementDatePast = timeFromPOSIX $ timeToPOSIX t' - 1000

      (_txId, achievementAC) <-
        bjjInteraction ctx (w1 testWallets)
          ( AwardAchievementAction
              { aa_awarded_to_profile_id = practitionerAC,
                aa_awarded_by_profile_id = orgAC,
                aa_profile_data = achievementProfileData,
                aa_other_metadata = [],
                aa_achievement_date = achievementDatePast
              }
          )
          Nothing
      waitNSlots_ 1

      -- w3 (who is NOT the awardedTo practitioner) tries to accept -> should fail
      -- w3 does not own practitioner's User NFT, so AchievementsValidator will reject
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Wrong user (w3) attempting to accept achievement (should fail)..."
      mustFail $
        void $
          bjjInteraction ctx (w3 testWallets)
            (AcceptAchievementAction achievementAC)
            Nothing

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Wrong user accept achievement fail test passed!"
      return ()

    -- Test Case 5.6: Cleanup dust at AchievementsValidator
    cleanupDustAtAchievementsTest :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    cleanupDustAtAchievementsTest TestInfo {..} = do
      waitNSlots_ 1000
      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      -- Simulate griefing: send ADA-only UTxO to achievements validator
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Sending dust UTxO to AchievementsValidator..."
      sendDustToValidator (w2 testWallets) achievementsValidatorGY
      waitNSlots_ 1

      -- Any user (w3) can clean it up — permissionless
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Cleaning up dust via ProtocolAction CleanupDustAction..."
      _txId <- protocolInteraction ctx (w3 testWallets) CleanupDustAction
      waitNSlots_ 1

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Dust cleanup at AchievementsValidator succeeded!"
      return ()

-- ------------------------------------------------------------------------------------------------

-- -- * Dust / Cleanup Tests

-- ------------------------------------------------------------------------------------------------
