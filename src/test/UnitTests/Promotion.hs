{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UnitTests.Promotion
  ( promotionTests,
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
import Onchain.BJJ
import Test.Fixtures (masterBProfileData, masterProfileData, studentProfileData)
import Test.Tasty
import TestRuns (bjjInteraction, deployBJJValidators, logPractitionerProfileInformation)

promotionTests :: (HasCallStack) => TestTree
promotionTests =
  testGroup
    "Promotion"
    [ promotionTestsGroup,
      promotionSecurityTests
    ]

promotionTestsGroup :: (HasCallStack) => TestTree
promotionTestsGroup =
  testGroup
    "Promotion Tests"
    [ mkTestFor "Test Case 1.1: Verify that a black belt can promote a white belt to blue belt and the white belt can accept the promotion" blackPromotesWhiteToBlue
    ]
  where
    blackPromotesWhiteToBlue :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    blackPromotesWhiteToBlue TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000
      txBuildingContext <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000
      (_gyTxId, masterAC) <-
        bjjInteraction
          txBuildingContext
          (w1 testWallets)
          (CreateProfileWithRankAction masterProfileData Practitioner creationDate Black)
          Nothing

      logPractitionerProfileInformation (w1 testWallets) masterAC

      (_gyTxId, studentAC) <-
        bjjInteraction
          txBuildingContext
          (w1 testWallets)
          (InitProfileAction studentProfileData Practitioner creationDate)
          Nothing

      logPractitionerProfileInformation (w1 testWallets) studentAC

      waitNSlots_ 2

      s' <- slotOfCurrentBlock
      blueBeltDate <- slotToBeginTime s'

      (_gyTxId, blueBeltPromotionAC) <-
        bjjInteraction
          txBuildingContext
          (w1 testWallets)
          ( PromoteProfileAction
              { promoted_profile_id = studentAC,
                promoted_by_profile_id = masterAC,
                achievement_date = blueBeltDate,
                promoted_belt = Blue
              }
          )
          Nothing

      void $
        bjjInteraction
          txBuildingContext
          (w1 testWallets)
          (AcceptPromotionAction blueBeltPromotionAC)
          Nothing

      logPractitionerProfileInformation (w1 testWallets) studentAC

      let purpleBeltDate = timeFromPlutus $ timeToPlutus blueBeltDate + monthsToPosixTime (minMonthsForBelt Purple)

      (_gyTxId, purpleBeltPromotionAC) <-
        bjjInteraction
          txBuildingContext
          (w1 testWallets)
          ( PromoteProfileAction
              { promoted_profile_id = studentAC,
                promoted_by_profile_id = masterAC,
                achievement_date = purpleBeltDate,
                promoted_belt = Purple
              }
          )
          Nothing

      void $
        bjjInteraction
          txBuildingContext
          (w1 testWallets)
          (AcceptPromotionAction purpleBeltPromotionAC)
          Nothing

      logPractitionerProfileInformation (w1 testWallets) studentAC

      return ()

promotionSecurityTests :: (HasCallStack) => TestTree
promotionSecurityTests =
  testGroup
    "Promotion Security Tests"
    [ mkTestFor "Test Case 2.1: Multiple masters can create promotions for same student" multipleMastersCanPromote,
      mkTestFor "Test Case 2.2: Sequential promotions from same master work correctly" sequentialPromotionsWork
    ]
  where
    multipleMastersCanPromote :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    multipleMastersCanPromote TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      txBuildingContext <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      (_gyTxId, masterA_AC) <-
        bjjInteraction
          txBuildingContext
          (w1 testWallets)
          (CreateProfileWithRankAction masterProfileData Practitioner creationDate Black)
          Nothing

      (_gyTxId, masterB_AC) <-
        bjjInteraction
          txBuildingContext
          (w2 testWallets)
          (CreateProfileWithRankAction masterBProfileData Practitioner creationDate Black)
          Nothing

      (_gyTxId, studentAC) <-
        bjjInteraction
          txBuildingContext
          (w3 testWallets)
          (InitProfileAction studentProfileData Practitioner creationDate)
          Nothing

      waitNSlots_ 2
      s' <- slotOfCurrentBlock
      blueBeltDate <- slotToBeginTime s'

      (_gyTxId, promotionA_AC) <-
        bjjInteraction
          txBuildingContext
          (w1 testWallets)
          ( PromoteProfileAction
              { promoted_profile_id = studentAC,
                promoted_by_profile_id = masterA_AC,
                achievement_date = blueBeltDate,
                promoted_belt = Blue
              }
          )
          Nothing

      waitNSlots_ 2
      s'' <- slotOfCurrentBlock
      blueBeltDate2 <- slotToBeginTime s''

      (_gyTxId, _promotionB_AC) <-
        bjjInteraction
          txBuildingContext
          (w2 testWallets)
          ( PromoteProfileAction
              { promoted_profile_id = studentAC,
                promoted_by_profile_id = masterB_AC,
                achievement_date = blueBeltDate2,
                promoted_belt = Blue
              }
          )
          Nothing

      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Both masters successfully created Blue belt promotions for the student."

      void $
        bjjInteraction
          txBuildingContext
          (w3 testWallets)
          (AcceptPromotionAction promotionA_AC)
          Nothing

      logPractitionerProfileInformation (w3 testWallets) studentAC
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Student successfully accepted first Blue belt promotion."
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Note: Accepting the second Blue belt promotion would fail on-chain with 'Promotion invalid - already at or past this rank'"

      return ()

    sequentialPromotionsWork :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    sequentialPromotionsWork TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000

      txBuildingContext <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000

      (_gyTxId, masterAC) <-
        bjjInteraction
          txBuildingContext
          (w1 testWallets)
          (CreateProfileWithRankAction masterProfileData Practitioner creationDate Black)
          Nothing

      (_gyTxId, studentAC) <-
        bjjInteraction
          txBuildingContext
          (w2 testWallets)
          (InitProfileAction studentProfileData Practitioner creationDate)
          Nothing

      logPractitionerProfileInformation (w2 testWallets) studentAC
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Student created as White belt."

      waitNSlots_ 2
      s' <- slotOfCurrentBlock
      blueBeltDate <- slotToBeginTime s'

      (_gyTxId, blueBeltPromotionAC) <-
        bjjInteraction
          txBuildingContext
          (w1 testWallets)
          ( PromoteProfileAction
              { promoted_profile_id = studentAC,
                promoted_by_profile_id = masterAC,
                achievement_date = blueBeltDate,
                promoted_belt = Blue
              }
          )
          Nothing

      void $
        bjjInteraction
          txBuildingContext
          (w2 testWallets)
          (AcceptPromotionAction blueBeltPromotionAC)
          Nothing

      logPractitionerProfileInformation (w2 testWallets) studentAC
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Student promoted to Blue belt."

      let purpleBeltDate = timeFromPlutus $ timeToPlutus blueBeltDate + monthsToPosixTime (minMonthsForBelt Blue + 1)

      (_gyTxId, purpleBeltPromotionAC) <-
        bjjInteraction
          txBuildingContext
          (w1 testWallets)
          ( PromoteProfileAction
              { promoted_profile_id = studentAC,
                promoted_by_profile_id = masterAC,
                achievement_date = purpleBeltDate,
                promoted_belt = Purple
              }
          )
          Nothing

      void $
        bjjInteraction
          txBuildingContext
          (w2 testWallets)
          (AcceptPromotionAction purpleBeltPromotionAC)
          Nothing

      logPractitionerProfileInformation (w2 testWallets) studentAC
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Student promoted to Purple belt."
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Sequential promotions work correctly with proper date ordering."

      return ()
