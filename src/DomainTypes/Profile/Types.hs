{-# OPTIONS_GHC -Wno-partial-fields #-}

module DomainTypes.Profile.Types where

import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Swagger.Internal.Schema (ToSchema)
import Data.Text hiding (init, tail)
import Data.Text qualified as T
import GHC.Generics
import GeniusYield.Types (GYAssetClass)
import GeniusYield.Types.Time
import Onchain.BJJ (BJJBelt)

-------------------------------------------------------------------------------

-- * Profile

-------------------------------------------------------------------------------

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
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data RankInformation
  = RankInformation
  { rankInfoId :: RankAC,
    rankInfoBelt :: BJJBelt,
    rankInfoAchievedByProfileId :: ProfileRefAC,
    rankInfoAwardedByProfileId :: ProfileRefAC,
    rankInfoAchievementDate :: GYTime
  }
  deriving (Generic, FromJSON, ToJSON, ToSchema)

data ProfileActionType
  = InitProfileAction
      { profileData :: ProfileData,
        profileType :: ProfileType,
        creationDate :: GYTime
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
        achievementDate :: GYTime,
        promotedBelt :: BJJBelt
      }
  | AcceptPromotionAction
      { promotionId :: RankAC
      }
  | CreateProfileWithRankAction
      { profileData :: ProfileData,
        profileType :: ProfileType,
        creationDate :: GYTime,
        belt :: BJJBelt
      }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- Custom Show instances for better formatting
instance Show ProfileInformation where
  show :: ProfileInformation -> String
  show (PractitionerProfileInformation {..}) =
    Prelude.unlines
      [ "🥋 Practitioner Profile",
        "┌─────────────────────────────────────────────────────────────",
        "│ Name: " <> T.unpack practitionerName,
        "│ Description: " <> T.unpack practitionerDescription,
        "│ Image URI: " <> T.unpack practitionerImageURI,
        "│ ID: " <> getRawString practitionerId,
        "│",
        "│ Current Rank:",
        show practitionerCurrentRank,
        "│",
        "│ Previous Ranks:",
        showRankChain practitionerPreviousRanks,
        "└─────────────────────────────────────────────────────────────"
      ]
  show (OrganizationProfileInformation {..}) =
    Prelude.unlines
      [ "🏢 Organization Profile",
        "┌─────────────────────────────────────────────────────────────",
        "│ Name: " <> getRawString organizationName,
        "│ Description: " <> getRawString organizationDescription,
        "│ Image URI: " <> getRawString organizationImageURI,
        "│ ID: " <> getRawString organizationId,
        "└─────────────────────────────────────────────────────────────"
      ]

-- Helper function to display rank chain
showRankChain :: [RankInformation] -> String
showRankChain [] = "│   No previous ranks"
showRankChain ranks =
  Prelude.unlines $ Prelude.zipWith (curry showRankWithIndex) (Prelude.reverse [0 .. (Prelude.length ranks)]) ranks
  where
    showRankWithIndex (i, rank) =
      "│   " <> show i <> ". \n" <> show rank

instance Show RankInformation where
  show (RankInformation {..}) =
    Prelude.init $
      Prelude.unlines
        [ "┌─────────────────────────────────────────────────────────────",
          "│ Belt: " <> getRawString rankInfoBelt,
          "│ ID: " <> getRawString rankInfoId,
          "│ Achieved by: " <> getRawString rankInfoAchievedByProfileId,
          "│ Awarded by: " <> getRawString rankInfoAwardedByProfileId,
          "│ Achievement Date: " <> getRawString rankInfoAchievementDate,
          "└─────────────────────────────────────────────────────────────"
        ]

getRawString :: (ToJSON a) => a -> String
getRawString = init . tail . BL.unpack . Aeson.encode
