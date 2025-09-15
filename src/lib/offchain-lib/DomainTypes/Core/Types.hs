{-# LANGUAGE TemplateHaskell #-}

module DomainTypes.Core.Types where

import Data.Aeson
import Data.Aeson.Types qualified as AesonTypes
import Data.List.Extra
import Data.Swagger (ToSchema (..), genericDeclareNamedSchema)
import Data.Swagger.Internal.Schema ()
import Data.Swagger.ParamSchema
import Data.Swagger.SchemaOptions (fromAesonOptions)
import Data.Text hiding (init, tail)
import Data.Text as T
import Database.Persist.TH
import Deriving.Aeson
import GHC.Generics ()
import GeniusYield.Types (GYAssetClass)
import GeniusYield.Types.Time
import Onchain.BJJ (BJJBelt)
import Servant (FromHttpApiData (..))
import Utils

type ProfileRefAC = GYAssetClass

type RankAC = GYAssetClass

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
    Prelude.init $
      Prelude.unlines
        [ "┌─────────────────────────────────────────────────────────────",
          "│ Belt: " <> stringFromJSON rankBelt,
          "│ ID: " <> stringFromJSON rankId,
          "│ Achieved by: " <> stringFromJSON rankAchievedByProfileId,
          "│ Awarded by: " <> stringFromJSON rankAwardedByProfileId,
          "│ Achievement Date: " <> stringFromJSON rankAchievementDate,
          "└─────────────────────────────────────────────────────────────"
        ]

data Promotion
  = Promotion
  { promotionId :: RankAC,
    promotionBelt :: BJJBelt,
    promotionAchievedByProfileId :: ProfileRefAC,
    promotionAwardedByProfileId :: ProfileRefAC,
    promotionAchievementDate :: GYTime
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "promotion", CamelToSnake]] Promotion

derivePersistFieldJSON "Promotion"

instance Show Promotion where
  show :: Promotion -> String
  show (Promotion {..}) =
    Prelude.init $
      Prelude.unlines
        [ "┌─────────────────────────────────────────────────────────────",
          "│ Promotion: " <> stringFromJSON promotionBelt,
          "│ ID: " <> stringFromJSON promotionId,
          "│ Achieved by: " <> stringFromJSON promotionAchievedByProfileId,
          "│ Awarded by: " <> stringFromJSON promotionAwardedByProfileId,
          "│ Achievement Date: " <> stringFromJSON promotionAchievementDate,
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
