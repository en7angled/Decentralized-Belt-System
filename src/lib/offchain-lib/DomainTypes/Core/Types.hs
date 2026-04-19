{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module DomainTypes.Core.Types where

import Data.Aeson
import Data.Aeson.Types qualified as AesonTypes
import Data.List.Extra
import Data.Swagger (NamedSchema (..), ToSchema (..), genericDeclareNamedSchema)
import Data.Swagger.Internal.Schema ()
import Data.Swagger.ParamSchema
import Data.Swagger.SchemaOptions (fromAesonOptions)
import Data.Text hiding (init, tail)
import Data.Text as T
import Database.Persist.TH
import Deriving.Aeson
import DomainTypes.Core.BJJ (BJJBelt)
import GHC.Generics ()
import GeniusYield.Types (GYAssetClass)
import GeniusYield.Types.Time
import Servant (FromHttpApiData (..))
import Utils

type ProfileRefAC = GYAssetClass

type RankAC = GYAssetClass

type MembershipHistoryAC = GYAssetClass

type MembershipIntervalAC = GYAssetClass

type AchievementAC = GYAssetClass

-- | Profile type
data ProfileType = Practitioner | Organization
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema, ToParamSchema, Eq, Ord)

derivePersistFieldJSON "ProfileType"

instance FromHttpApiData ProfileType where
  parseQueryParam :: Text -> Either Text ProfileType
  parseQueryParam = maybe (Left "Invalid profile type") Right . parseProfileType . T.unpack
    where
      parseProfileType s
        | s == "Practitioner" = Just Practitioner
        | s == "Organization" = Just Organization
        | otherwise = Nothing

-- | Profile
data Profile = Profile
  { profileId :: ProfileRefAC,
    profileName :: Text,
    profileDescription :: Text,
    profileImageURI :: Text,
    profileType :: ProfileType
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "profile", CamelToSnake]] Profile

derivePersistFieldJSON "Profile"

instance Show Profile where
  show :: Profile -> String
  show (Profile {..}) =
    Prelude.unlines
      [ "┌─────────────────────────────────────────────────────────────",
        "│ Name: " <> stringFromJSON profileName,
        "│ Description: " <> stringFromJSON profileDescription,
        "│ Image URI: " <> stringFromJSON profileImageURI,
        "│ ID: " <> stringFromJSON profileId,
        "└─────────────────────────────────────────────────────────────"
      ]

instance ToSchema Profile where
  declareNamedSchema = genericDeclareNamedSchema profileSchemaOptions
    where
      profileSchemaOptions :: SchemaOptions
      profileSchemaOptions =
        fromAesonOptions $
          AesonTypes.defaultOptions
            { AesonTypes.fieldLabelModifier = camelTo2 '_' . dropPrefix "profile"
            }

data Rank
  = Rank
  { rankId :: RankAC,
    rankBelt :: BJJBelt,
    rankAchievedByProfileId :: ProfileRefAC,
    rankAwardedByProfileId :: ProfileRefAC,
    rankAchievementDate :: GYTime
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "rank", CamelToSnake]] Rank

instance ToSchema Rank where
  declareNamedSchema = genericDeclareNamedSchema rankSchemaOptions
    where
      rankSchemaOptions :: SchemaOptions
      rankSchemaOptions =
        fromAesonOptions $
          AesonTypes.defaultOptions
            { AesonTypes.fieldLabelModifier = camelTo2 '_' . dropPrefix "rank"
            }

derivePersistFieldJSON "Rank"

instance Show Rank where
  show :: Rank -> String
  show (Rank {..}) =
    Data.List.Extra.intercalate
      "\n"
      [ "┌─────────────────────────────────────────────────────────────",
        "│ Belt: " <> stringFromJSON rankBelt,
        "│ ID: " <> stringFromJSON rankId,
        "│ Achieved by: " <> stringFromJSON rankAchievedByProfileId,
        "│ Awarded by: " <> stringFromJSON rankAwardedByProfileId,
        "│ Achievement Date: " <> stringFromJSON rankAchievementDate,
        "└─────────────────────────────────────────────────────────────"
      ]

-- | Promotion state: pending (not yet accepted), accepted, or superseded.
data PromotionState = PromotionPending | PromotionAccepted | PromotionSuperseded
  deriving (Generic, Show, Eq, Ord)

instance ToJSON PromotionState where
  toJSON PromotionPending = "pending"
  toJSON PromotionAccepted = "accepted"
  toJSON PromotionSuperseded = "superseded"

instance FromJSON PromotionState where
  parseJSON = withText "PromotionState" $ \case
    "pending" -> pure PromotionPending
    "accepted" -> pure PromotionAccepted
    "superseded" -> pure PromotionSuperseded
    other -> fail $ "Unknown PromotionState: " <> T.unpack other

instance ToSchema PromotionState where
  declareNamedSchema _ = pure $ NamedSchema (Just "PromotionState") mempty

instance ToParamSchema PromotionState where
  toParamSchema _ = mempty

derivePersistFieldJSON "PromotionState"

instance FromHttpApiData PromotionState where
  parseQueryParam :: Text -> Either Text PromotionState
  parseQueryParam t = case T.toLower t of
    "pending" -> Right PromotionPending
    "accepted" -> Right PromotionAccepted
    "superseded" -> Right PromotionSuperseded
    _ -> Left "Invalid promotion state. Use 'pending', 'accepted', or 'superseded'"

-- | API promotion state from the practitioner\'s current rank belt (latest by date) vs this
-- promotion belt. @Nothing@ = no rank rows for that practitioner.
promotionStateFromBelts :: Maybe BJJBelt -> BJJBelt -> PromotionState
promotionStateFromBelts Nothing _ = PromotionPending
promotionStateFromBelts (Just current) proposed
  | current > proposed = PromotionSuperseded
  | otherwise = PromotionPending

data Promotion
  = Promotion
  { promotionId :: RankAC,
    promotionBelt :: BJJBelt,
    promotionAchievedByProfileId :: ProfileRefAC,
    promotionAwardedByProfileId :: ProfileRefAC,
    promotionAchievementDate :: GYTime,
    promotionState :: PromotionState
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "promotion", CamelToSnake]] Promotion

derivePersistFieldJSON "Promotion"

instance Show Promotion where
  show :: Promotion -> String
  show (Promotion {..}) =
    Data.List.Extra.intercalate
      "\n"
      [ "┌─────────────────────────────────────────────────────────────",
        "│ Promotion: " <> stringFromJSON promotionBelt,
        "│ ID: " <> stringFromJSON promotionId,
        "│ Achieved by: " <> stringFromJSON promotionAchievedByProfileId,
        "│ Awarded by: " <> stringFromJSON promotionAwardedByProfileId,
        "│ Achievement Date: " <> stringFromJSON promotionAchievementDate,
        "│ State: " <> show promotionState,
        "└─────────────────────────────────────────────────────────────"
      ]

instance ToSchema Promotion where
  declareNamedSchema = genericDeclareNamedSchema promotionSchemaOptions
    where
      promotionSchemaOptions :: SchemaOptions
      promotionSchemaOptions =
        fromAesonOptions $
          AesonTypes.defaultOptions
            { AesonTypes.fieldLabelModifier = camelTo2 '_' . dropPrefix "promotion"
            }

-- | Convert an accepted 'Rank' to a 'Promotion' with state 'PromotionAccepted'.
rankToPromotion :: Rank -> Promotion
rankToPromotion r =
  Promotion
    { promotionId = rankId r,
      promotionBelt = rankBelt r,
      promotionAchievedByProfileId = rankAchievedByProfileId r,
      promotionAwardedByProfileId = rankAwardedByProfileId r,
      promotionAchievementDate = rankAchievementDate r,
      promotionState = PromotionAccepted
    }

-- | A practitioner's membership history with an organization.
data MembershipHistory = MembershipHistory
  { membershipHistoryId :: MembershipHistoryAC,
    membershipHistoryPractitionerId :: ProfileRefAC,
    membershipHistoryOrganizationId :: ProfileRefAC
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "membershipHistory", CamelToSnake]] MembershipHistory

derivePersistFieldJSON "MembershipHistory"

instance Show MembershipHistory where
  show :: MembershipHistory -> String
  show (MembershipHistory {..}) =
    Data.List.Extra.intercalate
      "\n"
      [ "┌─────────────────────────────────────────────────────────────",
        "│ MembershipHistory ID: " <> stringFromJSON membershipHistoryId,
        "│ Practitioner: " <> stringFromJSON membershipHistoryPractitionerId,
        "│ Organization: " <> stringFromJSON membershipHistoryOrganizationId,
        "└─────────────────────────────────────────────────────────────"
      ]

instance ToSchema MembershipHistory where
  declareNamedSchema = genericDeclareNamedSchema membershipHistorySchemaOptions
    where
      membershipHistorySchemaOptions :: SchemaOptions
      membershipHistorySchemaOptions =
        fromAesonOptions $
          AesonTypes.defaultOptions
            { AesonTypes.fieldLabelModifier = camelTo2 '_' . dropPrefix "membershipHistory"
            }

-- | A time-bounded membership interval within a membership history.
data MembershipInterval = MembershipInterval
  { membershipIntervalId :: MembershipIntervalAC,
    membershipIntervalStartDate :: GYTime,
    membershipIntervalEndDate :: Maybe GYTime,
    membershipIntervalAccepted :: Bool,
    membershipIntervalPractitionerId :: ProfileRefAC,
    membershipIntervalIntervalNumber :: Integer
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "membershipInterval", CamelToSnake]] MembershipInterval

derivePersistFieldJSON "MembershipInterval"

instance Show MembershipInterval where
  show :: MembershipInterval -> String
  show (MembershipInterval {..}) =
    Data.List.Extra.intercalate
      "\n"
      [ "┌─────────────────────────────────────────────────────────────",
        "│ MembershipInterval ID: " <> stringFromJSON membershipIntervalId,
        "│ Start Date: " <> stringFromJSON membershipIntervalStartDate,
        "│ End Date: " <> stringFromJSON membershipIntervalEndDate,
        "│ Accepted: " <> show membershipIntervalAccepted,
        "│ Practitioner: " <> stringFromJSON membershipIntervalPractitionerId,
        "│ Interval #: " <> show membershipIntervalIntervalNumber,
        "└─────────────────────────────────────────────────────────────"
      ]

instance ToSchema MembershipInterval where
  declareNamedSchema = genericDeclareNamedSchema membershipIntervalSchemaOptions
    where
      membershipIntervalSchemaOptions :: SchemaOptions
      membershipIntervalSchemaOptions =
        fromAesonOptions $
          AesonTypes.defaultOptions
            { AesonTypes.fieldLabelModifier = camelTo2 '_' . dropPrefix "membershipInterval"
            }

-- | JSON-serialized @[(Text, Text)]@ for achievement projections (array of @[key, value]@ string pairs).
newtype AchievementOtherMetadataJson = AchievementOtherMetadataJson
  { fromAchievementOtherMetadataJson :: [(Text, Text)]
  }
  deriving stock (Show, Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

-- | An achievement awarded to a practitioner.
data Achievement = Achievement
  { achievementId :: AchievementAC,
    achievementAwardedToProfileId :: ProfileRefAC,
    achievementAwardedByProfileId :: ProfileRefAC,
    achievementAchievementDate :: GYTime,
    achievementAccepted :: Bool,
    achievementName :: Text,
    achievementDescription :: Text,
    achievementImageURI :: Text,
    achievementOtherMetadata :: [(Text, Text)]
  }
  deriving (Generic)
  deriving (ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "achievement", CamelToSnake]] Achievement

instance FromJSON Achievement where
  parseJSON = withObject "Achievement" $ \o ->
    Achievement
      <$> o .: "id"
      <*> o .: "awarded_to_profile_id"
      <*> o .: "awarded_by_profile_id"
      <*> o .: "achievement_date"
      <*> o .: "accepted"
      <*> o .: "name"
      <*> o .: "description"
      <*> o .: "image_uri"
      <*> o .:? "other_metadata" .!= []

derivePersistFieldJSON "Achievement"
derivePersistFieldJSON "AchievementOtherMetadataJson"

instance Show Achievement where
  show :: Achievement -> String
  show (Achievement {..}) =
    Data.List.Extra.intercalate
      "\n"
      [ "┌─────────────────────────────────────────────────────────────",
        "│ Achievement ID: " <> stringFromJSON achievementId,
        "│ Name: " <> stringFromJSON achievementName,
        "│ Awarded To: " <> stringFromJSON achievementAwardedToProfileId,
        "│ Awarded By: " <> stringFromJSON achievementAwardedByProfileId,
        "│ Date: " <> stringFromJSON achievementAchievementDate,
        "│ Accepted: " <> show achievementAccepted,
        "│ Other metadata pairs: " <> show (Prelude.length achievementOtherMetadata),
        "└─────────────────────────────────────────────────────────────"
      ]

instance ToSchema Achievement where
  declareNamedSchema = genericDeclareNamedSchema achievementSchemaOptions
    where
      achievementSchemaOptions :: SchemaOptions
      achievementSchemaOptions =
        fromAesonOptions $
          AesonTypes.defaultOptions
            { AesonTypes.fieldLabelModifier = camelTo2 '_' . dropPrefix "achievement"
            }
