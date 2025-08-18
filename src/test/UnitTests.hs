{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UnitTests where

import Control.Monad
import DomainTypes.Transfer.Types
import GHC.Stack
import GeniusYield.Test.Clb
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types
import Onchain.BJJ
import Onchain.CIP68
import Test.Tasty
import TestRuns (bjjInteraction, deployBJJValidators, getProfileAndRank, logPractitionerProfileInformation)
import TxBuilding.Interactions
import GeniusYield.Imports (ToJSON(..))
import DomainTypes.Core.Types
import DomainTypes.Core.Actions

unitTests :: (HasCallStack) => TestTree
unitTests =
  testGroup
    "BJJ Unit Tests"
    [promotionTests]

studentProfileData =
  ProfileData
    { profileDataName = "John Doe",
      profileDataDescription = "John Doe is a student",
      profileDataImageURI = "ipfs://QmReBRNMe7tBr6WbA89uwnHHW7f7Zoe8wY2mzVpA8STdAk"
    }

masterProfileData =
  ProfileData
    { profileDataName = "Master",
      profileDataDescription = "Master is a master",
      profileDataImageURI = "ipfs://Qmb3JXJHQxuReSUaH6rXAoP5oX9NRs6JmnRFGTj2RVhGwe"
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
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 1
      txBuildingContext <- deployBJJValidators (w1 testWallets)
      (gyTxId, masterAC) <-
        bjjInteraction
          txBuildingContext
          (w1 testWallets)
          (CreateProfileWithRankAction masterProfileData Practitioner creationDate Black)
          Nothing

      logPractitionerProfileInformation (w1 testWallets) masterAC

      (gyTxId, studentAC) <-
        bjjInteraction
          txBuildingContext
          (w1 testWallets)
          (InitProfileAction studentProfileData Practitioner creationDate)
          Nothing

      logPractitionerProfileInformation (w1 testWallets) studentAC

      ((blackBeltProfile, blackBeltValue), (blackBeltRank, blackBeltRankValue)) <- getProfileAndRank (w1 testWallets) masterAC
      ((whiteBeltProfile, whiteBeltValue), (whiteBeltRank, whiteBeltRankValue)) <- getProfileAndRank (w1 testWallets) studentAC

      waitNSlots_ 2

      s <- slotOfCurrentBlock
      blueBeltDate <- slotToBeginTime s

      (gyTxId, blueBeltPromotionAC) <-
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

      (gyTxId, blueBeltRankAC) <-
        bjjInteraction
          txBuildingContext
          (w1 testWallets)
          (AcceptPromotionAction blueBeltPromotionAC)
          Nothing

      logPractitionerProfileInformation (w1 testWallets) studentAC

      let purpleBeltDate = timeFromPlutus $ timeToPlutus blueBeltDate + monthsToPosixTime (minMonthsForBelt Purple)

      (gyTxId, purpleBeltPromotionAC) <-
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

      (gyTxId, purpleBeltRank) <-
        bjjInteraction
          txBuildingContext
          (w1 testWallets)
          (AcceptPromotionAction purpleBeltPromotionAC)
          Nothing

      logPractitionerProfileInformation (w1 testWallets) studentAC

      return ()
