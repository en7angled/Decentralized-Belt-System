{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Service-level request types for the interaction API's dedicated endpoints.
-- These are higher-level than 'ActionType' — they accept raw image bytes (via multipart)
-- and structured JSON data, then get resolved into 'Interaction' values by 'ServiceHandlers'.
module ServiceRequests where

import Control.Lens ((&), (.~), (?~), at, mapped)
import Data.Aeson (eitherDecodeStrict)
import Data.Proxy (Proxy (..))
import Data.Swagger (ToSchema (..), declareSchemaRef, description, genericDeclareNamedSchema, properties, required, type_)
import Data.Swagger qualified as Swagger
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable, typeRep)
import Deriving.Aeson
import DomainTypes.Core.BJJ (BJJBelt)
import DomainTypes.Core.Types (ProfileRefAC, ProfileType, RankAC)
import GeniusYield.Types (GYAddress, GYAssetClass)
import GeniusYield.Types.Time (GYTime)
import Servant.Multipart (FileData (..), FromMultipart (..), Mem, lookupFile, lookupInput)
import TxBuilding.Interactions (UserAddresses)
import Utils (mkStripPrefixSchemaOptions)

-------------------------------------------------------------------------------

-- * Helpers

-------------------------------------------------------------------------------

-- | Add a description to an inline field in a NamedSchema's properties.
describeField :: Text -> Text -> Swagger.NamedSchema -> Swagger.NamedSchema
describeField field desc ns =
  ns & Swagger.schema . properties . at field . mapped . Swagger._Inline . description ?~ desc

-------------------------------------------------------------------------------

-- * Multipart wrapper

-------------------------------------------------------------------------------

-- | Generic wrapper for endpoints that receive an image file + JSON-encoded data.
-- The client sends a multipart form with two fields:
--
--   * @"image"@ — the binary image file
--   * @"data"@  — a JSON string with all structured request fields
data WithImage a = WithImage
  { wiImage :: FileData Mem
  , wiData :: a
  }

instance FromJSON a => FromMultipart Mem (WithImage a) where
  fromMultipart multipartData = do
    imageFile <- lookupFile "image" multipartData
    jsonText <- lookupInput "data" multipartData
    case eitherDecodeStrict (encodeUtf8 jsonText) of
      Left err -> Left $ "Invalid JSON in 'data' field: " <> err
      Right val -> Right $ WithImage imageFile val

instance (ToSchema a, Typeable a) => ToSchema (WithImage a) where
  declareNamedSchema _ = do
    dataRef <- declareSchemaRef (Proxy :: Proxy a)
    let imageSchema = Swagger.Inline $ mempty
          & type_ ?~ Swagger.SwaggerString
          & Swagger.format ?~ "binary"
          & description ?~ "Image file (JPEG, PNG) to upload to IPFS"
    let typeName = show (typeRep (Proxy :: Proxy a))
    return $ Swagger.NamedSchema (Just ("WithImage_" <> pack typeName)) $ mempty
      & type_ ?~ Swagger.SwaggerObject
      & description ?~ "Multipart form with two fields: 'image' (binary file upload) and 'data' (JSON string — see referenced schema for fields)"
      & properties . at "image" ?~ imageSchema
      & properties . at "data" ?~ dataRef
      & required .~ ["image", "data"]

-------------------------------------------------------------------------------

-- * Smart request types (with IPFS upload or chain queries)

-------------------------------------------------------------------------------

-- | Create a profile with an image. The @belt@ field determines which action is used:
--
--   * @Nothing@  → 'InitProfileAction'
--   * @Just belt@ → 'CreateProfileWithRankAction'
data CreateProfileRequest = CreateProfileRequest
  { cprName :: Text
  , cprDescription :: Text
  , cprProfileType :: ProfileType
  , cprBelt :: Maybe BJJBelt
  , cprCreationDate :: GYTime
  , cprUserAddresses :: UserAddresses
  , cprRecipient :: Maybe GYAddress
  }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "cpr", CamelToSnake]] CreateProfileRequest

instance ToSchema CreateProfileRequest where
  declareNamedSchema proxy = do
    s <- genericDeclareNamedSchema (mkStripPrefixSchemaOptions "cpr") proxy
    return $ s
      & Swagger.schema . description ?~ "Create a new profile. If 'belt' is provided, the profile is created with an initial rank."
      & describeField "name" "Display name for the profile"
      & describeField "description" "Profile description / bio text"
      & describeField "belt" "Initial BJJ belt rank (omit for no initial rank). Values: White, Blue, Purple, Brown, Black, CoralWhite, CoralBlack, Red"
      & describeField "recipient" "Address to receive unlocked funds (defaults to change address if omitted)"

-- | Smart membership request. The service handler queries the chain to determine
-- whether a membership history already exists for the org+practitioner pair:
--
--   * If none exists → 'CreateMembershipHistoryAction'
--   * If one exists  → 'AddMembershipIntervalAction' (using the looked-up node ID)
data NewMembershipRequest = NewMembershipRequest
  { nmrOrganizationProfileId :: ProfileRefAC
  , nmrPractitionerProfileId :: ProfileRefAC
  , nmrStartDate :: GYTime
  , nmrEndDate :: Maybe GYTime
  , nmrUserAddresses :: UserAddresses
  , nmrRecipient :: Maybe GYAddress
  }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "nmr", CamelToSnake]] NewMembershipRequest

instance ToSchema NewMembershipRequest where
  declareNamedSchema proxy = do
    s <- genericDeclareNamedSchema (mkStripPrefixSchemaOptions "nmr") proxy
    return $ s
      & Swagger.schema . description ?~ "Create or extend a membership. The API auto-detects whether to create a new membership history or add an interval to an existing one."
      & describeField "end_date" "End date of the membership interval (omit for open-ended / ongoing membership)"
      & describeField "recipient" "Address to receive unlocked funds (defaults to change address if omitted)"

-- | Update a profile's metadata (description, image) via IPFS upload.
data UpdateProfileRequest = UpdateProfileRequest
  { uprProfileId :: ProfileRefAC
  , uprUserAddresses :: UserAddresses
  , uprRecipient :: Maybe GYAddress
  , uprDescription :: Maybe Text
  }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "upr", CamelToSnake]] UpdateProfileRequest

instance ToSchema UpdateProfileRequest where
  declareNamedSchema proxy = do
    s <- genericDeclareNamedSchema (mkStripPrefixSchemaOptions "upr") proxy
    return $ s
      & Swagger.schema . description ?~ "Update a profile's metadata (description, image). Name is immutable. The new image is uploaded to IPFS."
      & describeField "recipient" "Address to receive unlocked funds (defaults to change address if omitted)"
      & describeField "description" "New profile description (omit to keep current)"

-- | Award an achievement with an image upload.
data AwardAchievementRequest = AwardAchievementRequest
  { aarAwardedToProfileId :: ProfileRefAC
  , aarAwardedByProfileId :: ProfileRefAC
  , aarName :: Text
  , aarDescription :: Text
  , aarOtherMetadata :: [(Text, Text)]
  , aarAchievementDate :: GYTime
  , aarUserAddresses :: UserAddresses
  , aarRecipient :: Maybe GYAddress
  }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "aar", CamelToSnake]] AwardAchievementRequest

instance ToSchema AwardAchievementRequest where
  declareNamedSchema proxy = do
    s <- genericDeclareNamedSchema (mkStripPrefixSchemaOptions "aar") proxy
    return $ s
      & Swagger.schema . description ?~ "Award an achievement to a profile. The achievement image is uploaded to IPFS."
      & describeField "name" "Achievement name"
      & describeField "description" "Achievement description"
      & describeField "other_metadata" "Additional key-value metadata pairs (e.g. [[\"category\", \"competition\"]])"
      & describeField "recipient" "Address to receive unlocked funds (defaults to change address if omitted)"

-------------------------------------------------------------------------------

-- * Pass-through request types (thin wrappers, JSON only)

-------------------------------------------------------------------------------

-- | Promote a profile to a new belt rank.
data PromoteProfileRequest = PromoteProfileRequest
  { pprPromotedProfileId :: ProfileRefAC
  , pprPromotedByProfileId :: ProfileRefAC
  , pprAchievementDate :: GYTime
  , pprPromotedBelt :: BJJBelt
  , pprUserAddresses :: UserAddresses
  , pprRecipient :: Maybe GYAddress
  }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "ppr", CamelToSnake]] PromoteProfileRequest

instance ToSchema PromoteProfileRequest where
  declareNamedSchema proxy = do
    s <- genericDeclareNamedSchema (mkStripPrefixSchemaOptions "ppr") proxy
    return $ s
      & Swagger.schema . description ?~ "Promote a practitioner to a new belt rank. Creates a pending promotion that the practitioner must accept."
      & describeField "recipient" "Address to receive unlocked funds (defaults to change address if omitted)"

-- | Accept a pending promotion.
data AcceptPromotionRequest = AcceptPromotionRequest
  { aprPromotionId :: RankAC
  , aprUserAddresses :: UserAddresses
  , aprRecipient :: Maybe GYAddress
  }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "apr", CamelToSnake]] AcceptPromotionRequest

instance ToSchema AcceptPromotionRequest where
  declareNamedSchema proxy = do
    s <- genericDeclareNamedSchema (mkStripPrefixSchemaOptions "apr") proxy
    return $ s
      & Swagger.schema . description ?~ "Accept a pending promotion. Must be called by the promoted practitioner's wallet."
      & describeField "recipient" "Address to receive unlocked funds (defaults to change address if omitted)"

-- | Accept a pending membership interval.
data AcceptMembershipRequest = AcceptMembershipRequest
  { amrIntervalId :: GYAssetClass
  , amrUserAddresses :: UserAddresses
  , amrRecipient :: Maybe GYAddress
  }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "amr", CamelToSnake]] AcceptMembershipRequest

instance ToSchema AcceptMembershipRequest where
  declareNamedSchema proxy = do
    s <- genericDeclareNamedSchema (mkStripPrefixSchemaOptions "amr") proxy
    return $ s
      & Swagger.schema . description ?~ "Accept a pending membership interval. Must be called by the practitioner's wallet."
      & describeField "recipient" "Address to receive unlocked funds (defaults to change address if omitted)"

-- | Update the end date of a membership interval.
data UpdateEndDateRequest = UpdateEndDateRequest
  { uerIntervalId :: GYAssetClass
  , uerHistoryNodeId :: GYAssetClass
  , uerNewEndDate :: GYTime
  , uerUserAddresses :: UserAddresses
  , uerRecipient :: Maybe GYAddress
  }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "uer", CamelToSnake]] UpdateEndDateRequest

instance ToSchema UpdateEndDateRequest where
  declareNamedSchema proxy = do
    s <- genericDeclareNamedSchema (mkStripPrefixSchemaOptions "uer") proxy
    return $ s
      & Swagger.schema . description ?~ "Close a membership interval by setting its end date. Called by the organization."
      & describeField "recipient" "Address to receive unlocked funds (defaults to change address if omitted)"

-- | Accept a pending achievement.
data AcceptAchievementRequest = AcceptAchievementRequest
  { acrAchievementId :: GYAssetClass
  , acrUserAddresses :: UserAddresses
  , acrRecipient :: Maybe GYAddress
  }
  deriving (Show, Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "acr", CamelToSnake]] AcceptAchievementRequest

instance ToSchema AcceptAchievementRequest where
  declareNamedSchema proxy = do
    s <- genericDeclareNamedSchema (mkStripPrefixSchemaOptions "acr") proxy
    return $ s
      & Swagger.schema . description ?~ "Accept a pending achievement. Must be called by the recipient practitioner's wallet."
      & describeField "recipient" "Address to receive unlocked funds (defaults to change address if omitted)"
