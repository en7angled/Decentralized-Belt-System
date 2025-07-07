{-# OPTIONS_GHC -Wno-partial-fields #-}

module DomainTypes.Profile.Types where

import Data.Aeson
import Data.Swagger.Internal.Schema (ToSchema)
import Data.Text
import GHC.Generics
import GeniusYield.Types (GYAssetClass)

-------------------------------------------------------------------------------

-- * Profile

-------------------------------------------------------------------------------

type RankNumber = Integer

type POSIXTimeInteger = Integer -- POSIXTime in milliseconds

data ProfileData
  = ProfileData
  { profileName :: Text,
    profileDescription :: Text,
    profileImageURI :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data ProfileType = Practitioner | Organization
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

type ProfileRefAC = GYAssetClass

type RankAC = GYAssetClass

data ProfileInformation
  = PractitionerProfileInformation
      { practitionerId :: ProfileRefAC,
        practitionerName :: Text,
        practitionerDescription :: Text,
        practitionerImageURI :: Text,
        practitionerCurrentRank :: RankInformation,
        practitionerPreviousRanks :: [RankInformation]
      }
  | OrganizationProfileInformation
      { organizationId :: ProfileRefAC,
        organizationName :: Text,
        organizationDescription :: Text,
        organizationImageURI :: Text
      }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data RankInformation
  = RankInformation
  { rankInfoId :: RankAC,
    rankInfoNumber :: Integer,
    rankInfoAchievedByProfileId :: ProfileRefAC,
    rankInfoAwardedByProfileId :: ProfileRefAC,
    rankInfoAchievementDate :: POSIXTimeInteger
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

data ProfileActionType
  = CreateProfileAction
      { profileData :: ProfileData,
        profileType :: ProfileType,
        creationDate :: POSIXTimeInteger
      }
  | UpdateProfileImageAction
      { profileId :: ProfileRefAC,
        imageURI :: Text
      }
  | DeleteProfileAction
      { profileId :: ProfileRefAC
      }
  | PromoteProfileAction
      { promotedProfileId :: ProfileRefAC,
        promotedByProfileId :: ProfileRefAC,
        achievementDate :: POSIXTimeInteger,
        rankNumber :: RankNumber
      }
  | AcceptPromotionAction
      { promotionId :: RankAC
      }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
