{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module UnitTests where

import Control.Monad
import DomainTypes.Profile.Types
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

unitTests :: (HasCallStack) => TestTree
unitTests =
  testGroup
    "BJJ Unit Tests"
    [promotionTests]

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
              { promotedProfileId = studentAC,
                promotedByProfileId = masterAC,
                achievementDate = blueBeltDate,
                promotedBelt = Blue
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
              { promotedProfileId = studentAC,
                promotedByProfileId = masterAC,
                achievementDate = purpleBeltDate,
                promotedBelt = Purple
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
