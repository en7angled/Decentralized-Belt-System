{-# OPTIONS_GHC -Wno-partial-fields #-}

module DomainTypes.Core.Actions where

import Data.Aeson
import Data.Aeson.Types qualified as AesonTypes
import Data.List.Extra
import Data.Swagger (ToSchema (..), genericDeclareNamedSchema)
import Data.Swagger.Internal.Schema ()
import Data.Swagger.ParamSchema
import Data.Swagger.SchemaOptions (fromAesonOptions)
import Data.Proxy (Proxy (..))
import Data.Text hiding (init, tail)
import Deriving.Aeson
import DomainTypes.Core.Types
import GHC.Generics ()
import GeniusYield.Types.Time
import DomainTypes.Core.BJJ (BJJBelt)
import Onchain.Protocol.Types (FeeConfig)

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

-- NOTE: DeleteProfileAction is intentionally not supported to preserve lineage integrity.
-- BJJ belt records are permanent historical facts that should not be erasable.
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

-------------------------------------------------------------------------------

-- * Admin Actions (oracle management)

-------------------------------------------------------------------------------

-- | Admin actions for managing protocol parameters via the oracle.
-- These modify the oracle UTxO datum and require the admin signing key.
--
-- Note: These actions are only constructed programmatically (admin CLI),
-- never deserialized from JSON. The ToJSON/FromJSON/ToSchema instances
-- are minimal stubs required because ActionType is embedded in Interaction
-- which needs full JSON support for the REST API.
data AdminActionType
  = PauseProtocolAction
  | UnpauseProtocolAction
  | SetFeesAction (Maybe FeeConfig)  -- ^ Nothing = clear fees, Just fc = set fees
  | SetMinLovelaceAction Integer     -- ^ Update minimum output lovelace
  deriving (Show, Generic)

instance ToJSON AdminActionType where
  toJSON PauseProtocolAction = object ["tag" .= ("PauseProtocolAction" :: Text)]
  toJSON UnpauseProtocolAction = object ["tag" .= ("UnpauseProtocolAction" :: Text)]
  toJSON (SetFeesAction _) = object ["tag" .= ("SetFeesAction" :: Text)]
  toJSON (SetMinLovelaceAction n) = object ["tag" .= ("SetMinLovelaceAction" :: Text), "value" .= n]

instance FromJSON AdminActionType where
  parseJSON = withObject "AdminActionType" $ \o -> do
    tag <- o .: "tag"
    case (tag :: Text) of
      "PauseProtocolAction" -> pure PauseProtocolAction
      "UnpauseProtocolAction" -> pure UnpauseProtocolAction
      _ -> fail "AdminActionType: unsupported action for JSON deserialization"

instance ToSchema AdminActionType where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
