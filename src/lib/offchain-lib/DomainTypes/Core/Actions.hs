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
import GeniusYield.Types (GYAssetClass)
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
  | CreateMembershipHistoryAction
      { cmh_organization_profile_id :: ProfileRefAC,
        cmh_practitioner_profile_id :: ProfileRefAC,
        cmh_start_date :: GYTime,
        cmh_end_date :: Maybe GYTime
      }
  | AddMembershipIntervalAction
      { ami_organization_profile_id :: ProfileRefAC,
        ami_membership_node_id :: GYAssetClass,
        ami_start_date :: GYTime,
        ami_end_date :: Maybe GYTime
      }
  | AcceptMembershipIntervalAction
      { aci_interval_id :: GYAssetClass
      }
  | UpdateEndDateAction
      { ude_membership_interval_id :: GYAssetClass,
        ude_membership_history_node_id :: GYAssetClass,
        ude_new_end_date :: GYTime
      }
  | AwardAchievementAction
      { aa_awarded_to_profile_id :: ProfileRefAC,
        aa_awarded_by_profile_id :: ProfileRefAC,
        aa_profile_data :: ProfileData,
        aa_other_metadata :: [(Text, Text)],
        aa_achievement_date :: GYTime
      }
  | AcceptAchievementAction
      { aca_achievement_id :: GYAssetClass
      }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-------------------------------------------------------------------------------

-- * Protocol Actions (permissionless maintenance)

-------------------------------------------------------------------------------

-- | Protocol-level maintenance actions that any user can trigger.
-- These do not require admin privileges and are available via the REST API.
data ProtocolActionType
  = -- | Permissionless cleanup of dust/griefing UTxOs at validator addresses.
    -- Anyone can call this — the recovered ADA goes to the caller.
    CleanupDustAction
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
  deriving (Show, Generic)

instance ToJSON AdminActionType where
  toJSON PauseProtocolAction = object ["tag" .= ("PauseProtocolAction" :: Text)]
  toJSON UnpauseProtocolAction = object ["tag" .= ("UnpauseProtocolAction" :: Text)]
  toJSON (SetFeesAction _) = object ["tag" .= ("SetFeesAction" :: Text)]

instance FromJSON AdminActionType where
  parseJSON = withObject "AdminActionType" $ \o -> do
    tag <- o .: "tag"
    case (tag :: Text) of
      "PauseProtocolAction" -> pure PauseProtocolAction
      "UnpauseProtocolAction" -> pure UnpauseProtocolAction
      _ -> fail "AdminActionType: unsupported action for JSON deserialization"

instance ToSchema AdminActionType where
  declareNamedSchema _ = declareNamedSchema (Proxy :: Proxy Text)
