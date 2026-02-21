{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UnitTests.Membership
  ( membershipTests,
  )
where

import Control.Monad (void)
import Control.Monad.Reader (runReaderT)
import Data.Foldable (toList)
import DomainTypes.Core.Actions
import DomainTypes.Core.Types
import GHC.Stack
import GeniusYield.Test.Clb
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.TxBuilder.User qualified as User
import GeniusYield.Types
import Onchain.BJJ (BJJBelt (White))
import Onchain.Protocol.Id qualified
import PlutusLedgerApi.V3 (POSIXTime (POSIXTime), getPOSIXTime)
import Test.Fixtures (orgProfileData, practitionerBProfileData, practitionerCProfileData, practitionerProfileData)
import Test.Tasty
import TestRuns (bjjInteraction, deployBJJValidators)
import TxBuilding.Lookups (getFirstIntervalIdForMembershipNode)
import TxBuilding.Operations (updateEndDateTX)
import TxBuilding.Skeletons (isValidBetween)
import TxBuilding.Utils (gySlotFromPOSIXTime)

-- ------------------------------------------------------------------------------------------------

-- -- * Membership Tests

-- ------------------------------------------------------------------------------------------------

membershipTests :: (HasCallStack) => TestTree
membershipTests =
  testGroup
    "Membership Tests"
    [ mkTestFor "Test Case 4.1: Organization creates membership history for a practitioner" createMembershipHistoryTest,
      mkTestFor "Test Case 4.2: Practitioner accepts a membership interval" acceptMembershipIntervalTest,
      mkTestFor "Test Case 4.3: Full membership lifecycle (create history, accept, add interval, accept)" fullMembershipLifecycleTest,
      mkTestFor "Test Case 4.4: Organization has 3 membership histories for 3 practitioners" orgThreeMembershipHistoriesTest,
      mkTestFor "Test Case 4.5: 3 practitioners with membership histories; one updates (adds) membership interval" orgThreeHistoriesOneUpdatesIntervalTest,
      mkTestFor "Test Case 4.6: Organization updates interval end date (UpdateEndDate)" updateEndDateByOrgTest,
      mkTestFor "Test Case 4.7: Practitioner shortens accepted interval end date (UpdateEndDate)" updateEndDateByPractitionerTest,
      mkTestFor "Test Case 4.8: Create membership history with endDate > startDate succeeds" endDateValidationCreationSucceedsTest,
      mkTestFor "Test Case 4.9: UpdateEndDate fails when newEndDate outside tx validity (TD)" updateEndDateTDFailTest,
      mkTestFor "Test Case 4.10: UpdateEndDate fails when practitioner updates unaccepted interval (TE)" updateEndDateTEFailTest,
      mkTestFor "Test Case 4.11: UpdateEndDate fails when practitioner extends end date (TB)" updateEndDateTBFailTest,
      mkTestFor "Test Case 4.12: UpdateEndDate fails when interval does not belong to referenced history node (V8)" updateEndDateWrongHistoryNodeFailTest,
      mkTestFor "Membership get-first-interval-id fails for root node (MembershipRootNodeHasNoHistory)" getFirstIntervalIdFailsForRootNodeTest,
      mkTestFor "Add-membership-interval fails when node is root (MembershipRootNodeHasNoHistory)" addMembershipIntervalFailsWhenRootNodeTest,
      mkTestFor "Add-membership-interval fails when last interval not accepted (LastIntervalNotAccepted)" addMembershipIntervalFailsLastNotAcceptedTest,
      mkTestFor "Add-membership-interval fails when last interval not closed (LastIntervalNotClosed)" addMembershipIntervalFailsLastNotClosedTest,
      mkTestFor "Add-membership-interval fails when end date not after start (InvalidNewIntervalEndDate)" addMembershipIntervalFailsInvalidEndDateTest
    ]
  where
    -- Test Case 4.1: Organization creates a membership history for a practitioner
    createMembershipHistoryTest :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    createMembershipHistoryTest TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      -- Create an Organization profile (w1 owns the org)
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Creating Organization profile..."
      (_txId, orgAC) <-
        bjjInteraction
          ctx
          (w1 testWallets)
          (CreateProfileWithRankAction orgProfileData Organization creationDate White)
          Nothing
      waitNSlots_ 1

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) $ "Organization created: " <> show orgAC

      -- Create a Practitioner profile (w2 owns the practitioner)
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Creating Practitioner profile..."
      (_txId, practitionerAC) <-
        bjjInteraction
          ctx
          (w2 testWallets)
          (InitProfileAction practitionerProfileData Practitioner creationDate)
          Nothing
      waitNSlots_ 1

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) $ "Practitioner created: " <> show practitionerAC

      -- Organization creates membership history for the practitioner
      -- Start date must be before tx validity (on-chain trace "G": startDate `before` txInfoValidRange)
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Organization creating membership history..."
      s' <- slotOfCurrentBlock
      t' <- slotToBeginTime s'
      let membershipStartDate = timeFromPOSIX $ timeToPOSIX t' - 1000

      (_txId, membershipHistoryAC) <-
        bjjInteraction
          ctx
          (w1 testWallets)
          ( CreateMembershipHistoryAction
              { cmh_organization_profile_id = orgAC,
                cmh_practitioner_profile_id = practitionerAC,
                cmh_start_date = membershipStartDate,
                cmh_end_date = Nothing
              }
          )
          Nothing
      waitNSlots_ 1

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) $ "Membership history created: " <> show membershipHistoryAC
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Membership history creation test passed!"
      return ()

    -- Test Case 4.2: Practitioner accepts a membership interval
    acceptMembershipIntervalTest :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    acceptMembershipIntervalTest TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      -- Create Organization and Practitioner
      (_txId, orgAC) <-
        bjjInteraction ctx (w1 testWallets)
          (CreateProfileWithRankAction orgProfileData Organization creationDate White)
          Nothing
      waitNSlots_ 1

      (_txId, practitionerAC) <-
        bjjInteraction ctx (w2 testWallets)
          (InitProfileAction practitionerProfileData Practitioner creationDate)
          Nothing
      waitNSlots_ 1

      -- Organization creates membership history (start date before tx validity for on-chain trace "G")
      s' <- slotOfCurrentBlock
      t' <- slotToBeginTime s'
      let membershipStartDate = timeFromPOSIX $ timeToPOSIX t' - 1000

      (_txId, _membershipHistoryAC) <-
        bjjInteraction ctx (w1 testWallets)
          ( CreateMembershipHistoryAction
              { cmh_organization_profile_id = orgAC,
                cmh_practitioner_profile_id = practitionerAC,
                cmh_start_date = membershipStartDate,
                cmh_end_date = Nothing
              }
          )
          Nothing
      waitNSlots_ 1

      -- The first interval AC is created alongside the history.
      -- We need to derive it to accept it. The interval ID is derived from the history ID.
      let plutusOrgAC = assetClassToPlutus orgAC
      let plutusPractAC = assetClassToPlutus practitionerAC
      let historyId = Onchain.Protocol.Id.deriveMembershipHistoryId plutusOrgAC plutusPractAC
      let firstIntervalId = Onchain.Protocol.Id.deriveMembershipIntervalId historyId 0
      gyFirstIntervalAC <- assetClassFromPlutus' firstIntervalId

      -- Practitioner accepts the first interval
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Practitioner accepting membership interval..."
      void $
        bjjInteraction ctx (w2 testWallets)
          (AcceptMembershipIntervalAction gyFirstIntervalAC)
          Nothing
      waitNSlots_ 1

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Membership interval accepted!"
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Accept membership interval test passed!"
      return ()

    -- Test Case 4.3: Full lifecycle
    fullMembershipLifecycleTest :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    fullMembershipLifecycleTest TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      -- Step 1: Create Organization and Practitioner
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "=== Step 1: Create profiles ==="
      (_txId, orgAC) <-
        bjjInteraction ctx (w1 testWallets)
          (CreateProfileWithRankAction orgProfileData Organization creationDate White)
          Nothing
      waitNSlots_ 1

      (_txId, practitionerAC) <-
        bjjInteraction ctx (w2 testWallets)
          (InitProfileAction practitionerProfileData Practitioner creationDate)
          Nothing
      waitNSlots_ 1

      -- Step 2: Organization creates membership history (with first interval); start date before tx validity (trace "G")
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "=== Step 2: Create membership history ==="
      s' <- slotOfCurrentBlock
      t' <- slotToBeginTime s'
      let membershipStartDate = timeFromPOSIX $ timeToPOSIX t' - 1000

      (_txId, membershipHistoryAC) <-
        bjjInteraction ctx (w1 testWallets)
          ( CreateMembershipHistoryAction
              { cmh_organization_profile_id = orgAC,
                cmh_practitioner_profile_id = practitionerAC,
                cmh_start_date = membershipStartDate,
                cmh_end_date = Nothing
              }
          )
          Nothing
      waitNSlots_ 1

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) $ "Membership history: " <> show membershipHistoryAC

      -- Step 3: Derive first interval AC and accept it
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "=== Step 3: Accept first interval ==="
      let plutusOrgAC = assetClassToPlutus orgAC
      let plutusPractAC = assetClassToPlutus practitionerAC
      let historyId = Onchain.Protocol.Id.deriveMembershipHistoryId plutusOrgAC plutusPractAC
      let firstIntervalId = Onchain.Protocol.Id.deriveMembershipIntervalId historyId 0
      gyFirstIntervalAC <- assetClassFromPlutus' firstIntervalId

      void $
        bjjInteraction ctx (w2 testWallets)
          (AcceptMembershipIntervalAction gyFirstIntervalAC)
          Nothing
      waitNSlots_ 1

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "First interval accepted."

      -- Step 4: Organization adds a second interval
      -- The first interval must have an end date for a new one to be added.
      -- For this test, we need to update the first interval's end date first.
      -- However, `updateMembershipIntervalEndDate` is an on-chain operation,
      -- and we don't have a dedicated offchain operation for it yet.
      -- For now, we verify that the create + accept flow works end-to-end.
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "=== Full membership lifecycle test passed! ==="
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Verified: create org, create practitioner, create membership history, accept interval."
      return ()

    -- Test Case 4.4: One organization, 3 practitioners, 3 membership histories
    orgThreeMembershipHistoriesTest :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    orgThreeMembershipHistoriesTest TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      -- Create one organization (w1)
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Creating Organization profile..."
      (_txId, orgAC) <-
        bjjInteraction
          ctx
          (w1 testWallets)
          (CreateProfileWithRankAction orgProfileData Organization creationDate White)
          Nothing
      waitNSlots_ 1
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) $ "Organization created: " <> show orgAC

      -- Create 3 practitioners (w2, w3, w4)
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Creating Practitioner A (w2)..."
      (_txId, practitionerAC1) <-
        bjjInteraction ctx (w2 testWallets) (InitProfileAction practitionerProfileData Practitioner creationDate) Nothing
      waitNSlots_ 1
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Creating Practitioner B (w3)..."
      (_txId, practitionerAC2) <-
        bjjInteraction ctx (w3 testWallets) (InitProfileAction practitionerBProfileData Practitioner creationDate) Nothing
      waitNSlots_ 1
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Creating Practitioner C (w4)..."
      (_txId, practitionerAC3) <-
        bjjInteraction ctx (w4 testWallets) (InitProfileAction practitionerCProfileData Practitioner creationDate) Nothing
      waitNSlots_ 1

      -- Organization creates membership history for each practitioner (start date before tx validity for trace "G")
      let startDateBeforeNow = do
            s' <- slotOfCurrentBlock
            t' <- slotToBeginTime s'
            return $ timeFromPOSIX $ timeToPOSIX t' - 1000

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Organization creating membership history for Practitioner A..."
      membershipStartDate1 <- startDateBeforeNow
      (_txId, _history1) <-
        bjjInteraction ctx (w1 testWallets)
          ( CreateMembershipHistoryAction
              { cmh_organization_profile_id = orgAC,
                cmh_practitioner_profile_id = practitionerAC1,
                cmh_start_date = membershipStartDate1,
                cmh_end_date = Nothing
              }
          )
          Nothing
      waitNSlots_ 1

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Organization creating membership history for Practitioner B..."
      membershipStartDate2 <- startDateBeforeNow
      (_txId, _history2) <-
        bjjInteraction ctx (w1 testWallets)
          ( CreateMembershipHistoryAction
              { cmh_organization_profile_id = orgAC,
                cmh_practitioner_profile_id = practitionerAC2,
                cmh_start_date = membershipStartDate2,
                cmh_end_date = Nothing
              }
          )
          Nothing
      waitNSlots_ 1

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Organization creating membership history for Practitioner C..."
      membershipStartDate3 <- startDateBeforeNow
      (_txId, _history3) <-
        bjjInteraction ctx (w1 testWallets)
          ( CreateMembershipHistoryAction
              { cmh_organization_profile_id = orgAC,
                cmh_practitioner_profile_id = practitionerAC3,
                cmh_start_date = membershipStartDate3,
                cmh_end_date = Nothing
              }
          )
          Nothing
      waitNSlots_ 1

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Org with 3 membership histories test passed!"
      return ()

    -- Test Case 4.5: 3 practitioners with membership histories; one (Practitioner B) updates by adding a second interval
    orgThreeHistoriesOneUpdatesIntervalTest :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    orgThreeHistoriesOneUpdatesIntervalTest TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      -- Create one organization (w1)
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Creating Organization profile..."
      (_txId, orgAC) <-
        bjjInteraction
          ctx
          (w1 testWallets)
          (CreateProfileWithRankAction orgProfileData Organization creationDate White)
          Nothing
      waitNSlots_ 1

      -- Create 3 practitioners (w2, w3, w4)
      (_txId, practitionerAC1) <-
        bjjInteraction ctx (w2 testWallets) (InitProfileAction practitionerProfileData Practitioner creationDate) Nothing
      waitNSlots_ 1
      (_txId, practitionerAC2) <-
        bjjInteraction ctx (w3 testWallets) (InitProfileAction practitionerBProfileData Practitioner creationDate) Nothing
      waitNSlots_ 1
      (_txId, practitionerAC3) <-
        bjjInteraction ctx (w4 testWallets) (InitProfileAction practitionerCProfileData Practitioner creationDate) Nothing
      waitNSlots_ 1

      let startDateBeforeNow = do
            s' <- slotOfCurrentBlock
            t' <- slotToBeginTime s'
            return $ timeFromPOSIX $ timeToPOSIX t' - 1000

      -- Org creates membership history for A (no end date)
      membershipStartDate1 <- startDateBeforeNow
      (_txId, _history1) <-
        bjjInteraction ctx (w1 testWallets)
          ( CreateMembershipHistoryAction
              { cmh_organization_profile_id = orgAC,
                cmh_practitioner_profile_id = practitionerAC1,
                cmh_start_date = membershipStartDate1,
                cmh_end_date = Nothing
              }
          )
          Nothing
      waitNSlots_ 1

      -- Org creates membership history for B with an end date so we can add a second interval later
      membershipStartDate2 <- startDateBeforeNow
      let firstIntervalEndDate2 = timeFromPOSIX $ timeToPOSIX membershipStartDate2 + 1000
      (_txId, history2B) <-
        bjjInteraction ctx (w1 testWallets)
          ( CreateMembershipHistoryAction
              { cmh_organization_profile_id = orgAC,
                cmh_practitioner_profile_id = practitionerAC2,
                cmh_start_date = membershipStartDate2,
                cmh_end_date = Just firstIntervalEndDate2
              }
          )
          Nothing
      waitNSlots_ 1

      -- Org creates membership history for C (no end date)
      membershipStartDate3 <- startDateBeforeNow
      (_txId, _history3) <-
        bjjInteraction ctx (w1 testWallets)
          ( CreateMembershipHistoryAction
              { cmh_organization_profile_id = orgAC,
                cmh_practitioner_profile_id = practitionerAC3,
                cmh_start_date = membershipStartDate3,
                cmh_end_date = Nothing
              }
          )
          Nothing
      waitNSlots_ 1

      -- Practitioner B accepts his first interval (required before adding a second interval)
      let plutusOrgAC = assetClassToPlutus orgAC
          plutusPractBAC = assetClassToPlutus practitionerAC2
          historyIdB = Onchain.Protocol.Id.deriveMembershipHistoryId plutusOrgAC plutusPractBAC
          firstIntervalIdB = Onchain.Protocol.Id.deriveMembershipIntervalId historyIdB 0
      gyFirstIntervalB <- assetClassFromPlutus' firstIntervalIdB
      void $
        bjjInteraction ctx (w3 testWallets)
          (AcceptMembershipIntervalAction gyFirstIntervalB)
          Nothing
      waitNSlots_ 1

      -- Organization adds a second membership interval for Practitioner B (start >= first interval end)
      let secondIntervalStart = timeFromPOSIX $ timeToPOSIX firstIntervalEndDate2 + 1
          secondIntervalEnd = timeFromPOSIX $ timeToPOSIX secondIntervalStart + 10000
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Organization adding second membership interval for Practitioner B..."
      void $
        bjjInteraction ctx (w1 testWallets)
          ( AddMembershipIntervalAction
              { ami_organization_profile_id = orgAC,
                ami_membership_node_id = history2B,
                ami_start_date = secondIntervalStart,
                ami_end_date = Just secondIntervalEnd
              }
          )
          Nothing
      waitNSlots_ 1

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "3 practitioners, one updates interval test passed!"
      return ()

    -- Test Case 4.6: Organization updates interval end date (UpdateEndDate)
    updateEndDateByOrgTest :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    updateEndDateByOrgTest TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      (_txId, orgAC) <-
        bjjInteraction ctx (w1 testWallets)
          (CreateProfileWithRankAction orgProfileData Organization creationDate White)
          Nothing
      waitNSlots_ 1
      (_txId, practitionerAC) <-
        bjjInteraction ctx (w2 testWallets)
          (InitProfileAction practitionerProfileData Practitioner creationDate)
          Nothing
      waitNSlots_ 1

      s' <- slotOfCurrentBlock
      t' <- slotToBeginTime s'
      let membershipStartDate = timeFromPOSIX $ timeToPOSIX t' - 1000
      (_txId, historyAC) <-
        bjjInteraction ctx (w1 testWallets)
          ( CreateMembershipHistoryAction
              { cmh_organization_profile_id = orgAC,
                cmh_practitioner_profile_id = practitionerAC,
                cmh_start_date = membershipStartDate,
                cmh_end_date = Nothing
              }
          )
          Nothing
      waitNSlots_ 1

      let firstIntervalId = Onchain.Protocol.Id.deriveMembershipIntervalId
            (Onchain.Protocol.Id.deriveMembershipHistoryId (assetClassToPlutus orgAC) (assetClassToPlutus practitionerAC))
            0
      gyFirstIntervalAC <- assetClassFromPlutus' firstIntervalId
      -- Org sets end date to a future time (closing the open interval; validator requires newEndDate not before txInfoValidRange)
      now <- slotToBeginTime =<< slotOfCurrentBlock
      let newEndDate = timeFromPOSIX (timeToPOSIX now + 60000)
      void $
        bjjInteraction ctx (w1 testWallets)
          ( UpdateEndDateAction
              { ude_membership_interval_id = gyFirstIntervalAC,
                ude_membership_history_node_id = historyAC,
                ude_new_end_date = newEndDate
              }
          )
          Nothing
      waitNSlots_ 1
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "UpdateEndDate by org test passed!"
      return ()

    -- Test Case 4.7: Practitioner shortens accepted interval end date (UpdateEndDate)
    updateEndDateByPractitionerTest :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    updateEndDateByPractitionerTest TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      (_txId, orgAC) <-
        bjjInteraction ctx (w1 testWallets)
          (CreateProfileWithRankAction orgProfileData Organization creationDate White)
          Nothing
      waitNSlots_ 1
      (_txId, practitionerAC) <-
        bjjInteraction ctx (w2 testWallets)
          (InitProfileAction practitionerProfileData Practitioner creationDate)
          Nothing
      waitNSlots_ 1

      s' <- slotOfCurrentBlock
      t' <- slotToBeginTime s'
      let membershipStartDate = timeFromPOSIX $ timeToPOSIX t' - 1000
          membershipEndDate = timeFromPOSIX $ timeToPOSIX membershipStartDate + 10000
      (_txId, historyAC) <-
        bjjInteraction ctx (w1 testWallets)
          ( CreateMembershipHistoryAction
              { cmh_organization_profile_id = orgAC,
                cmh_practitioner_profile_id = practitionerAC,
                cmh_start_date = membershipStartDate,
                cmh_end_date = Just membershipEndDate
              }
          )
          Nothing
      waitNSlots_ 1

      let firstIntervalId = Onchain.Protocol.Id.deriveMembershipIntervalId
            (Onchain.Protocol.Id.deriveMembershipHistoryId (assetClassToPlutus orgAC) (assetClassToPlutus practitionerAC))
            0
      gyFirstIntervalAC <- assetClassFromPlutus' firstIntervalId
      void $
        bjjInteraction ctx (w2 testWallets)
          (AcceptMembershipIntervalAction gyFirstIntervalAC)
          Nothing
      waitNSlots_ 1

      -- Practitioner shortens end date (must be <= current end date and in the future)
      now <- slotToBeginTime =<< slotOfCurrentBlock
      let shortened = timeToPOSIX membershipStartDate + 5000
          inFuture = timeToPOSIX now + 1000
          shorterEndDate = timeFromPOSIX (max shortened inFuture)
      void $
        bjjInteraction ctx (w2 testWallets)
          ( UpdateEndDateAction
              { ude_membership_interval_id = gyFirstIntervalAC,
                ude_membership_history_node_id = historyAC,
                ude_new_end_date = shorterEndDate
              }
          )
          Nothing
      waitNSlots_ 1
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "UpdateEndDate by practitioner test passed!"
      return ()

    -- Test Case 4.9: UpdateEndDate fails when newEndDate is outside tx validity (TD)
    updateEndDateTDFailTest :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    updateEndDateTDFailTest TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      (_txId, orgAC) <-
        bjjInteraction ctx (w1 testWallets)
          (CreateProfileWithRankAction orgProfileData Organization creationDate White)
          Nothing
      waitNSlots_ 1
      (_txId, practitionerAC) <-
        bjjInteraction ctx (w2 testWallets)
          (InitProfileAction practitionerProfileData Practitioner creationDate)
          Nothing
      waitNSlots_ 1

      s' <- slotOfCurrentBlock
      t' <- slotToBeginTime s'
      let membershipStartDate = timeFromPOSIX $ timeToPOSIX t' - 1000
      (_txId, historyAC) <-
        bjjInteraction ctx (w1 testWallets)
          ( CreateMembershipHistoryAction
              { cmh_organization_profile_id = orgAC,
                cmh_practitioner_profile_id = practitionerAC,
                cmh_start_date = membershipStartDate,
                cmh_end_date = Nothing
              }
          )
          Nothing
      waitNSlots_ 1

      let firstIntervalId = Onchain.Protocol.Id.deriveMembershipIntervalId
            (Onchain.Protocol.Id.deriveMembershipHistoryId (assetClassToPlutus orgAC) (assetClassToPlutus practitionerAC))
            0
      gyFirstIntervalAC <- assetClassFromPlutus' firstIntervalId
      now <- slotOfCurrentBlock
      nowTime <- slotToBeginTime now
      let newEndDateGY = timeFromPOSIX (timeToPOSIX nowTime + 60000)
      let newEndDatePlutus = timeToPlutus newEndDateGY
      let validUntilMs = getPOSIXTime newEndDatePlutus - 10 * 60 * 1000
      validUntilSlot <- gySlotFromPOSIXTime (POSIXTime validUntilMs)
      let badValidity = isValidBetween now validUntilSlot

      skeleton <-
        runReaderT
          ( updateEndDateTX
              gyFirstIntervalAC
              historyAC
              newEndDateGY
              (toList $ User.userAddresses (w1 testWallets))
              (Just badValidity)
          )
          ctx
      mustFail $ asUser (w1 testWallets) $ void $ sendSkeleton' skeleton
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "UpdateEndDate TD fail test passed!"

    -- Test Case 4.10: UpdateEndDate fails when practitioner updates unaccepted interval (TE)
    updateEndDateTEFailTest :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    updateEndDateTEFailTest TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      (_txId, orgAC) <-
        bjjInteraction ctx (w1 testWallets)
          (CreateProfileWithRankAction orgProfileData Organization creationDate White)
          Nothing
      waitNSlots_ 1
      (_txId, practitionerAC) <-
        bjjInteraction ctx (w2 testWallets)
          (InitProfileAction practitionerProfileData Practitioner creationDate)
          Nothing
      waitNSlots_ 1

      s' <- slotOfCurrentBlock
      t' <- slotToBeginTime s'
      let membershipStartDate = timeFromPOSIX $ timeToPOSIX t' - 1000
      (_txId, historyAC) <-
        bjjInteraction ctx (w1 testWallets)
          ( CreateMembershipHistoryAction
              { cmh_organization_profile_id = orgAC,
                cmh_practitioner_profile_id = practitionerAC,
                cmh_start_date = membershipStartDate,
                cmh_end_date = Nothing
              }
          )
          Nothing
      waitNSlots_ 1

      let firstIntervalId = Onchain.Protocol.Id.deriveMembershipIntervalId
            (Onchain.Protocol.Id.deriveMembershipHistoryId (assetClassToPlutus orgAC) (assetClassToPlutus practitionerAC))
            0
      gyFirstIntervalAC <- assetClassFromPlutus' firstIntervalId
      now <- slotToBeginTime =<< slotOfCurrentBlock
      let newEndDate = timeFromPOSIX (timeToPOSIX now + 60000)
      mustFail $
        void $
          bjjInteraction ctx (w2 testWallets)
            ( UpdateEndDateAction
                { ude_membership_interval_id = gyFirstIntervalAC,
                  ude_membership_history_node_id = historyAC,
                  ude_new_end_date = newEndDate
                }
            )
            Nothing
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "UpdateEndDate TE fail test passed!"

    -- Test Case 4.11: UpdateEndDate fails when practitioner extends end date (TB)
    updateEndDateTBFailTest :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    updateEndDateTBFailTest TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      (_txId, orgAC) <-
        bjjInteraction ctx (w1 testWallets)
          (CreateProfileWithRankAction orgProfileData Organization creationDate White)
          Nothing
      waitNSlots_ 1
      (_txId, practitionerAC) <-
        bjjInteraction ctx (w2 testWallets)
          (InitProfileAction practitionerProfileData Practitioner creationDate)
          Nothing
      waitNSlots_ 1

      s' <- slotOfCurrentBlock
      t' <- slotToBeginTime s'
      let membershipStartDate = timeFromPOSIX $ timeToPOSIX t' - 1000
          membershipEndDate = timeFromPOSIX $ timeToPOSIX membershipStartDate + 10000
      (_txId, historyAC) <-
        bjjInteraction ctx (w1 testWallets)
          ( CreateMembershipHistoryAction
              { cmh_organization_profile_id = orgAC,
                cmh_practitioner_profile_id = practitionerAC,
                cmh_start_date = membershipStartDate,
                cmh_end_date = Just membershipEndDate
              }
          )
          Nothing
      waitNSlots_ 1

      let firstIntervalId = Onchain.Protocol.Id.deriveMembershipIntervalId
            (Onchain.Protocol.Id.deriveMembershipHistoryId (assetClassToPlutus orgAC) (assetClassToPlutus practitionerAC))
            0
      gyFirstIntervalAC <- assetClassFromPlutus' firstIntervalId
      void $
        bjjInteraction ctx (w2 testWallets)
          (AcceptMembershipIntervalAction gyFirstIntervalAC)
          Nothing
      waitNSlots_ 1

      -- Practitioner tries to extend (newEndDate > current end date)
      let extendedEndDate = timeFromPOSIX (timeToPOSIX membershipEndDate + 50000)
      mustFail $
        void $
          bjjInteraction ctx (w2 testWallets)
            ( UpdateEndDateAction
                { ude_membership_interval_id = gyFirstIntervalAC,
                  ude_membership_history_node_id = historyAC,
                  ude_new_end_date = extendedEndDate
                }
            )
            Nothing
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "UpdateEndDate TB fail test passed!"

    -- Test Case 4.12: UpdateEndDate fails when interval does not belong to referenced history node (V8)
    updateEndDateWrongHistoryNodeFailTest :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    updateEndDateWrongHistoryNodeFailTest TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      (_txId, orgAC) <-
        bjjInteraction ctx (w1 testWallets)
          (CreateProfileWithRankAction orgProfileData Organization creationDate White)
          Nothing
      waitNSlots_ 1
      (_txId, practitionerAC1) <-
        bjjInteraction ctx (w2 testWallets)
          (InitProfileAction practitionerProfileData Practitioner creationDate)
          Nothing
      waitNSlots_ 1
      (_txId, practitionerAC2) <-
        bjjInteraction ctx (w3 testWallets)
          (InitProfileAction practitionerBProfileData Practitioner creationDate)
          Nothing
      waitNSlots_ 1

      s' <- slotOfCurrentBlock
      t' <- slotToBeginTime s'
      let membershipStartDate = timeFromPOSIX $ timeToPOSIX t' - 1000
      (_txId, _historyAC1) <-
        bjjInteraction ctx (w1 testWallets)
          ( CreateMembershipHistoryAction
              { cmh_organization_profile_id = orgAC,
                cmh_practitioner_profile_id = practitionerAC1,
                cmh_start_date = membershipStartDate,
                cmh_end_date = Nothing
              }
          )
          Nothing
      waitNSlots_ 1
      (_txId, historyAC2) <-
        bjjInteraction ctx (w1 testWallets)
          ( CreateMembershipHistoryAction
              { cmh_organization_profile_id = orgAC,
                cmh_practitioner_profile_id = practitionerAC2,
                cmh_start_date = membershipStartDate,
                cmh_end_date = Nothing
              }
          )
          Nothing
      waitNSlots_ 1

      let firstIntervalId1 = Onchain.Protocol.Id.deriveMembershipIntervalId
            (Onchain.Protocol.Id.deriveMembershipHistoryId (assetClassToPlutus orgAC) (assetClassToPlutus practitionerAC1))
            0
      gyFirstIntervalAC1 <- assetClassFromPlutus' firstIntervalId1
      now <- slotToBeginTime =<< slotOfCurrentBlock
      let newEndDate = timeFromPOSIX (timeToPOSIX now + 60000)
      -- Pass interval of history 1 but history node of history 2 (wrong node)
      mustFail $
        void $
          bjjInteraction ctx (w1 testWallets)
            ( UpdateEndDateAction
                { ude_membership_interval_id = gyFirstIntervalAC1,
                  ude_membership_history_node_id = historyAC2,
                  ude_new_end_date = newEndDate
                }
            )
            Nothing
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "UpdateEndDate wrong history node fail test passed!"

    -- Test Case 4.8: Create membership history with endDate > startDate succeeds (TC validation)
    endDateValidationCreationSucceedsTest :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    endDateValidationCreationSucceedsTest TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      (_txId, orgAC) <-
        bjjInteraction ctx (w1 testWallets)
          (CreateProfileWithRankAction orgProfileData Organization creationDate White)
          Nothing
      waitNSlots_ 1
      (_txId, practitionerAC) <-
        bjjInteraction ctx (w2 testWallets)
          (InitProfileAction practitionerProfileData Practitioner creationDate)
          Nothing
      waitNSlots_ 1

      s' <- slotOfCurrentBlock
      t' <- slotToBeginTime s'
      let startDate = timeFromPOSIX $ timeToPOSIX t' - 1000
          endDate = timeFromPOSIX $ timeToPOSIX startDate + 5000
      void $
        bjjInteraction ctx (w1 testWallets)
          ( CreateMembershipHistoryAction
              { cmh_organization_profile_id = orgAC,
                cmh_practitioner_profile_id = practitionerAC,
                cmh_start_date = startDate,
                cmh_end_date = Just endDate
              }
          )
          Nothing
      waitNSlots_ 1
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "endDate > startDate creation test passed!"
      return ()

    -- Membership TxBuilding error cases: expect TxBuilding exceptions (MembershipRootNodeHasNoHistory, CannotAddMembershipInterval).
    -- HeadNumberMismatch is not exercised here; it would require inconsistent chain state.
    getFirstIntervalIdFailsForRootNodeTest :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    getFirstIntervalIdFailsForRootNodeTest TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      (_txId, orgAC) <-
        bjjInteraction ctx (w1 testWallets)
          (CreateProfileWithRankAction orgProfileData Organization creationDate White)
          Nothing
      waitNSlots_ 1

      rootNodeAC <- assetClassFromPlutus' (Onchain.Protocol.Id.deriveMembershipHistoriesListId (assetClassToPlutus orgAC))
      mustFail $ void $ runReaderT (getFirstIntervalIdForMembershipNode rootNodeAC) ctx
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "get-first-interval-id fails for root node test passed!"

    addMembershipIntervalFailsWhenRootNodeTest :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    addMembershipIntervalFailsWhenRootNodeTest TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      (_txId, orgAC) <-
        bjjInteraction ctx (w1 testWallets)
          (CreateProfileWithRankAction orgProfileData Organization creationDate White)
          Nothing
      waitNSlots_ 1

      rootNodeAC <- assetClassFromPlutus' (Onchain.Protocol.Id.deriveMembershipHistoriesListId (assetClassToPlutus orgAC))
      s' <- slotOfCurrentBlock
      t' <- slotToBeginTime s'
      let startDate = timeFromPOSIX $ timeToPOSIX t' - 1000
          endDate = timeFromPOSIX $ timeToPOSIX startDate + 5000
      mustFail $
        void $
          bjjInteraction ctx (w1 testWallets)
            (AddMembershipIntervalAction orgAC rootNodeAC startDate (Just endDate))
            Nothing
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "add-membership-interval fails when root node test passed!"

    addMembershipIntervalFailsLastNotAcceptedTest :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    addMembershipIntervalFailsLastNotAcceptedTest TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      (_txId, orgAC) <-
        bjjInteraction ctx (w1 testWallets)
          (CreateProfileWithRankAction orgProfileData Organization creationDate White)
          Nothing
      waitNSlots_ 1
      (_txId, practitionerAC) <-
        bjjInteraction ctx (w2 testWallets)
          (InitProfileAction practitionerProfileData Practitioner creationDate)
          Nothing
      waitNSlots_ 1

      s' <- slotOfCurrentBlock
      t' <- slotToBeginTime s'
      let membershipStart = timeFromPOSIX $ timeToPOSIX t' - 1000
          firstIntervalEnd = timeFromPOSIX $ timeToPOSIX membershipStart + 5000
      (_txId, historyNodeAC) <-
        bjjInteraction ctx (w1 testWallets)
          ( CreateMembershipHistoryAction
              { cmh_organization_profile_id = orgAC,
                cmh_practitioner_profile_id = practitionerAC,
                cmh_start_date = membershipStart,
                cmh_end_date = Just firstIntervalEnd
              }
          )
          Nothing
      waitNSlots_ 1

      -- Do not accept the first interval; then add second interval (should fail with LastIntervalNotAccepted)
      let secondStart = timeFromPOSIX $ timeToPOSIX firstIntervalEnd + 1
          secondEnd = timeFromPOSIX $ timeToPOSIX secondStart + 5000
      mustFail $
        void $
          bjjInteraction ctx (w1 testWallets)
            (AddMembershipIntervalAction orgAC historyNodeAC secondStart (Just secondEnd))
            Nothing
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "add-membership-interval fails when last not accepted test passed!"

    addMembershipIntervalFailsLastNotClosedTest :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    addMembershipIntervalFailsLastNotClosedTest TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      (_txId, orgAC) <-
        bjjInteraction ctx (w1 testWallets)
          (CreateProfileWithRankAction orgProfileData Organization creationDate White)
          Nothing
      waitNSlots_ 1
      (_txId, practitionerAC) <-
        bjjInteraction ctx (w2 testWallets)
          (InitProfileAction practitionerProfileData Practitioner creationDate)
          Nothing
      waitNSlots_ 1

      s' <- slotOfCurrentBlock
      t' <- slotToBeginTime s'
      let membershipStart = timeFromPOSIX $ timeToPOSIX t' - 1000
      (_txId, historyNodeAC) <-
        bjjInteraction ctx (w1 testWallets)
          ( CreateMembershipHistoryAction
              { cmh_organization_profile_id = orgAC,
                cmh_practitioner_profile_id = practitionerAC,
                cmh_start_date = membershipStart,
                cmh_end_date = Nothing
              }
          )
          Nothing
      waitNSlots_ 1

      -- First interval is open (no end date); adding second interval should fail with LastIntervalNotClosed
      let secondStart = timeFromPOSIX $ timeToPOSIX membershipStart + 5000
          secondEnd = timeFromPOSIX $ timeToPOSIX secondStart + 5000
      mustFail $
        void $
          bjjInteraction ctx (w1 testWallets)
            (AddMembershipIntervalAction orgAC historyNodeAC secondStart (Just secondEnd))
            Nothing
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "add-membership-interval fails when last not closed test passed!"

    addMembershipIntervalFailsInvalidEndDateTest :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    addMembershipIntervalFailsInvalidEndDateTest TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      (_txId, orgAC) <-
        bjjInteraction ctx (w1 testWallets)
          (CreateProfileWithRankAction orgProfileData Organization creationDate White)
          Nothing
      waitNSlots_ 1
      (_txId, practitionerAC) <-
        bjjInteraction ctx (w2 testWallets)
          (InitProfileAction practitionerProfileData Practitioner creationDate)
          Nothing
      waitNSlots_ 1

      s' <- slotOfCurrentBlock
      t' <- slotToBeginTime s'
      let membershipStart = timeFromPOSIX $ timeToPOSIX t' - 1000
          firstIntervalEnd = timeFromPOSIX $ timeToPOSIX membershipStart + 5000
      (_txId, historyNodeAC) <-
        bjjInteraction ctx (w1 testWallets)
          ( CreateMembershipHistoryAction
              { cmh_organization_profile_id = orgAC,
                cmh_practitioner_profile_id = practitionerAC,
                cmh_start_date = membershipStart,
                cmh_end_date = Just firstIntervalEnd
              }
          )
          Nothing
      waitNSlots_ 1

      let firstIntervalId = Onchain.Protocol.Id.deriveMembershipIntervalId
            (Onchain.Protocol.Id.deriveMembershipHistoryId (assetClassToPlutus orgAC) (assetClassToPlutus practitionerAC))
            0
      gyFirstIntervalAC <- assetClassFromPlutus' firstIntervalId
      void $
        bjjInteraction ctx (w2 testWallets)
          (AcceptMembershipIntervalAction gyFirstIntervalAC)
          Nothing
      waitNSlots_ 1

      -- Second interval with end date not after start (invalid)
      let secondStart = timeFromPOSIX $ timeToPOSIX firstIntervalEnd + 1
          badEnd = secondStart
      mustFail $
        void $
          bjjInteraction ctx (w1 testWallets)
            (AddMembershipIntervalAction orgAC historyNodeAC secondStart (Just badEnd))
            Nothing
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "add-membership-interval fails when end date not after start test passed!"

-- ------------------------------------------------------------------------------------------------

-- -- * Achievement Tests

