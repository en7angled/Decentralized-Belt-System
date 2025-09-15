{-# OPTIONS_GHC -Wno-partial-fields #-}

module DomainTypes.Core.Actions where

import Data.Aeson
import Data.Aeson.Types qualified as AesonTypes
import Data.List.Extra
import Data.Swagger (ToSchema (..), genericDeclareNamedSchema)
import Data.Swagger.Internal.Schema ()
import Data.Swagger.ParamSchema
import Data.Swagger.SchemaOptions (fromAesonOptions)
import Data.Text hiding (init, tail)
import Deriving.Aeson
import DomainTypes.Core.Types
import GHC.Generics ()
import GeniusYield.Types.Time
import Onchain.BJJ (BJJBelt)

-------------------------------------------------------------------------------

-- * Actions

-------------------------------------------------------------------------------

data ProfileData
  = ProfileData
  { profileDataName :: Text,
    profileDataDescription :: Text,
    profileDataImageURI :: Text
  }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "profileData", CamelToSnake]] ProfileData

instance ToSchema ProfileData where
  declareNamedSchema = genericDeclareNamedSchema profileDataSchemaOptions
    where
      -- Schema options that match the JSON field modifiers
      profileDataSchemaOptions :: SchemaOptions
      profileDataSchemaOptions =
        fromAesonOptions $
          AesonTypes.defaultOptions
            { AesonTypes.fieldLabelModifier = camelTo2 '_' . dropPrefix "profileData"
            }

data ProfileActionType
  = InitProfileAction
      { profile_data :: ProfileData,
        profile_type :: ProfileType,
        creation_date :: GYTime
      }
  | UpdateProfileImageAction
      { profile_id :: ProfileRefAC,
        image_uri :: Text
      }
  | DeleteProfileAction
      { profile_id :: ProfileRefAC
      }
  | PromoteProfileAction
      { promoted_profile_id :: ProfileRefAC,
        promoted_by_profile_id :: ProfileRefAC,
        achievement_date :: GYTime,
        promoted_belt :: BJJBelt
      }
  | AcceptPromotionAction
      { promotion_id :: RankAC
      }
  | CreateProfileWithRankAction
      { profile_data :: ProfileData,
        profile_type :: ProfileType,
        creation_date :: GYTime,
        belt :: BJJBelt
      }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)
