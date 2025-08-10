{-# OPTIONS_GHC -Wno-partial-fields #-}

module DomainTypes.Profile.Types where

import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as AesonTypes
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.Swagger.Internal.Schema (ToSchema)
import Data.Swagger.SchemaOptions (SchemaOptions(..), fromAesonOptions)
import Data.Swagger (genericDeclareNamedSchema, ToSchema (..))
import Data.Text hiding (init, tail)
import Data.Text qualified as T
import GHC.Generics
import GeniusYield.Types (GYAssetClass)
import GeniusYield.Types.Time
import Onchain.BJJ (BJJBelt)
import Deriving.Aeson
import Data.List.Extra

-------------------------------------------------------------------------------

-- * Profile

-------------------------------------------------------------------------------



data ProfileData
  = ProfileData
  { profileName :: Text,
    profileDescription :: Text,
    profileImageURI :: Text
  }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "profile", CamelToSnake]] ProfileData

instance ToSchema ProfileData where
  declareNamedSchema = genericDeclareNamedSchema profileDataSchemaOptions
    where
    -- Schema options that match the JSON field modifiers
    profileDataSchemaOptions :: SchemaOptions
    profileDataSchemaOptions = fromAesonOptions $ AesonTypes.defaultOptions
      { AesonTypes.fieldLabelModifier = camelTo2 '_' . dropPrefix "profile"}

data ProfileType = Practitioner | Organization
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

type ProfileRefAC = GYAssetClass

type RankAC = GYAssetClass

data PractitionerProfileInformation
  = PractitionerProfileInformation
      { practitionerId :: ProfileRefAC,
        practitionerName :: Text,
        practitionerDescription :: Text,
        practitionerImageURI :: Text,
        practitionerCurrentRank :: RankInformation,
        practitionerPreviousRanks :: [RankInformation]
      }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "practitioner", CamelToSnake]] PractitionerProfileInformation

instance ToSchema PractitionerProfileInformation where
  declareNamedSchema = genericDeclareNamedSchema practitionerSchemaOptions
    where
      practitionerSchemaOptions :: SchemaOptions
      practitionerSchemaOptions = fromAesonOptions $ AesonTypes.defaultOptions
        { AesonTypes.fieldLabelModifier = camelTo2 '_' . dropPrefix "practitioner"}

data OrganizationProfileInformation
  = OrganizationProfileInformation
      { organizationId :: ProfileRefAC,
        organizationName :: Text,
        organizationDescription :: Text,
        organizationImageURI :: Text
      }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "organization", CamelToSnake]] OrganizationProfileInformation

instance ToSchema OrganizationProfileInformation where
  declareNamedSchema = genericDeclareNamedSchema organizationSchemaOptions
    where
      organizationSchemaOptions :: SchemaOptions
      organizationSchemaOptions = fromAesonOptions $ AesonTypes.defaultOptions
        { AesonTypes.fieldLabelModifier =  camelTo2 '_' . dropPrefix "organization"}

data RankInformation
  = RankInformation
  { rankInfoId :: RankAC,
    rankInfoBelt :: BJJBelt,
    rankInfoAchievedByProfileId :: ProfileRefAC,
    rankInfoAwardedByProfileId :: ProfileRefAC,
    rankInfoAchievementDate :: GYTime
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "rankInfo", CamelToSnake]] RankInformation

instance ToSchema RankInformation where
  declareNamedSchema = genericDeclareNamedSchema rankInfoSchemaOptions
    where
      rankInfoSchemaOptions :: SchemaOptions
      rankInfoSchemaOptions = fromAesonOptions $ AesonTypes.defaultOptions
        { AesonTypes.fieldLabelModifier = camelTo2 '_' . dropPrefix "rankInfo"}

data PromotionInformation
  = PromotionInformation
  { promotionInfoId :: RankAC,
    promotionInfoBelt :: BJJBelt,
    promotionInfoAchievedByProfileId :: ProfileRefAC,
    promotionInfoAwardedByProfileId :: ProfileRefAC,
    promotionInfoAchievementDate :: GYTime
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "promotionInfo", CamelToSnake]] PromotionInformation

instance ToSchema PromotionInformation where
  declareNamedSchema = genericDeclareNamedSchema promotionInfoSchemaOptions
    where
      promotionInfoSchemaOptions :: SchemaOptions
      promotionInfoSchemaOptions = fromAesonOptions $ AesonTypes.defaultOptions
        { AesonTypes.fieldLabelModifier = camelTo2 '_' . dropPrefix "promotionInfo"}



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
instance Show PractitionerProfileInformation where
  show :: PractitionerProfileInformation -> String
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

instance Show OrganizationProfileInformation where
  show :: OrganizationProfileInformation -> String
  show (OrganizationProfileInformation {..}) =
    "OrganizationProfileInformation {"
      ++ "organizationId = "
      ++ show organizationId
      ++ ", organizationName = "
      ++ show organizationName
      ++ ", organizationDescription = "
      ++ show organizationDescription
      ++ ", organizationImageURI = "
      ++ show organizationImageURI
      ++ "}"

-- | Unified profile information type for counting
data ProfileInformation = ProfileInformation
  { profileType :: ProfileType,
    profileCreationDate :: GYTime
  }
  deriving (Eq, Show, Generic)

instance FromJSON ProfileInformation
instance ToJSON ProfileInformation
instance ToSchema ProfileInformation

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
