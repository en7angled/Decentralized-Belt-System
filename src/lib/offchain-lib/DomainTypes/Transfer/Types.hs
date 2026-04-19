{-# OPTIONS_GHC -Wno-partial-fields #-}

module DomainTypes.Transfer.Types where

import Data.Aeson
import Data.Aeson.Types qualified as AesonTypes
import Data.List.Extra
import Data.Swagger (ToSchema (..), genericDeclareNamedSchema)
import Data.Swagger.Internal.Schema ()
import Data.Swagger.ParamSchema
import Data.Swagger.SchemaOptions (fromAesonOptions)
import Data.Text hiding (init, tail)
import Data.Text qualified as T
import Deriving.Aeson
import DomainTypes.Core.Types
import GeniusYield.Types.Time (GYTime)
import GHC.Generics ()
import Utils

data PractitionerProfileInformation
  = PractitionerProfileInformation
  { practitionerId :: ProfileRefAC,
    practitionerName :: Text,
    practitionerDescription :: Text,
    practitionerImageURI :: Text,
    practitionerCurrentRank :: Rank,
    practitionerPreviousRanks :: [Rank]
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "practitioner", CamelToSnake]] PractitionerProfileInformation

instance ToSchema PractitionerProfileInformation where
  declareNamedSchema = genericDeclareNamedSchema practitionerSchemaOptions
    where
      practitionerSchemaOptions :: SchemaOptions
      practitionerSchemaOptions =
        fromAesonOptions $
          AesonTypes.defaultOptions
            { AesonTypes.fieldLabelModifier = camelTo2 '_' . dropPrefix "practitioner"
            }

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
      organizationSchemaOptions =
        fromAesonOptions $
          AesonTypes.defaultOptions
            { AesonTypes.fieldLabelModifier = camelTo2 '_' . dropPrefix "organization"
            }

-- | Interval plus organization id (API view for membership intervals).
data MembershipIntervalInformation = MembershipIntervalInformation
  { membershipIntervalInformationId :: MembershipIntervalAC,
    membershipIntervalInformationStartDate :: GYTime,
    membershipIntervalInformationEndDate :: Maybe GYTime,
    membershipIntervalInformationAccepted :: Bool,
    membershipIntervalInformationPractitionerId :: ProfileRefAC,
    membershipIntervalInformationIntervalNumber :: Integer,
    membershipIntervalInformationOrganizationId :: ProfileRefAC
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "membershipIntervalInformation", CamelToSnake]] MembershipIntervalInformation

instance ToSchema MembershipIntervalInformation where
  declareNamedSchema = genericDeclareNamedSchema membershipIntervalInformationSchemaOptions
    where
      membershipIntervalInformationSchemaOptions :: SchemaOptions
      membershipIntervalInformationSchemaOptions =
        fromAesonOptions $
          AesonTypes.defaultOptions
            { AesonTypes.fieldLabelModifier = camelTo2 '_' . dropPrefix "membershipIntervalInformation"
            }

-- | History plus list of intervals (API view for membership histories).
data MembershipHistoryInformation = MembershipHistoryInformation
  { membershipHistoryInformationId :: MembershipHistoryAC,
    membershipHistoryInformationPractitionerId :: ProfileRefAC,
    membershipHistoryInformationOrganizationId :: ProfileRefAC,
    membershipHistoryInformationIntervals :: [MembershipIntervalInformation]
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "membershipHistoryInformation", CamelToSnake]] MembershipHistoryInformation

instance ToSchema MembershipHistoryInformation where
  declareNamedSchema = genericDeclareNamedSchema membershipHistoryInformationSchemaOptions
    where
      membershipHistoryInformationSchemaOptions :: SchemaOptions
      membershipHistoryInformationSchemaOptions =
        fromAesonOptions $
          AesonTypes.defaultOptions
            { AesonTypes.fieldLabelModifier = camelTo2 '_' . dropPrefix "membershipHistoryInformation"
            }

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
        "│ ID: " <> stringFromJSON practitionerId,
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
    Prelude.unlines
      [ "🏢 Organization Profile",
        "┌─────────────────────────────────────────────────────────────",
        "│ Name: " <> stringFromJSON organizationName,
        "│ Description: " <> stringFromJSON organizationDescription,
        "│ Image URI: " <> stringFromJSON organizationImageURI,
        "│ ID: " <> stringFromJSON organizationId,
        "└─────────────────────────────────────────────────────────────"
      ]

instance Show MembershipIntervalInformation where
  show (MembershipIntervalInformation {..}) =
    Prelude.unlines
      [ "┌─────────────────────────────────────────────────────────────",
        "│ MembershipIntervalInformation ID: " <> stringFromJSON membershipIntervalInformationId,
        "│ Organization: " <> stringFromJSON membershipIntervalInformationOrganizationId,
        "│ Practitioner: " <> stringFromJSON membershipIntervalInformationPractitionerId,
        "│ Start: " <> stringFromJSON membershipIntervalInformationStartDate,
        "│ End: " <> stringFromJSON membershipIntervalInformationEndDate,
        "│ Accepted: " <> show membershipIntervalInformationAccepted,
        "│ Number: " <> show membershipIntervalInformationIntervalNumber,
        "└─────────────────────────────────────────────────────────────"
      ]

instance Show MembershipHistoryInformation where
  show (MembershipHistoryInformation {..}) =
    Prelude.unlines
      [ "┌─────────────────────────────────────────────────────────────",
        "│ MembershipHistoryInformation ID: " <> stringFromJSON membershipHistoryInformationId,
        "│ Practitioner: " <> stringFromJSON membershipHistoryInformationPractitionerId,
        "│ Organization: " <> stringFromJSON membershipHistoryInformationOrganizationId,
        "│ Intervals: " <> show (Prelude.length membershipHistoryInformationIntervals),
        "└─────────────────────────────────────────────────────────────"
      ]

-- | Protocol status response DTO.
data ProtocolStatus = ProtocolStatus
  { protocolStatusOpPaused :: Bool,
    protocolStatusMinUtxoValue :: Integer,
    protocolStatusFeeConfig :: Maybe FeeConfigDTO,
    protocolStatusScriptHashes :: Maybe ScriptHashesDTO
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "protocolStatus", CamelToSnake]] ProtocolStatus

instance ToSchema ProtocolStatus where
  declareNamedSchema = genericDeclareNamedSchema $
    fromAesonOptions AesonTypes.defaultOptions {AesonTypes.fieldLabelModifier = camelTo2 '_' . dropPrefix "protocolStatus"}

-- | Fee configuration response DTO.
data FeeConfigDTO = FeeConfigDTO
  { feeConfigProfileCreationFee :: Integer,
    feeConfigPromotionFee :: Integer,
    feeConfigMembershipHistoryFee :: Integer,
    feeConfigMembershipIntervalFee :: Integer,
    feeConfigAchievementFee :: Integer,
    feeConfigFeeAddress :: Maybe Text
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "feeConfig", CamelToSnake]] FeeConfigDTO

instance ToSchema FeeConfigDTO where
  declareNamedSchema = genericDeclareNamedSchema $
    fromAesonOptions AesonTypes.defaultOptions {AesonTypes.fieldLabelModifier = camelTo2 '_' . dropPrefix "feeConfig"}

-- | Script hashes of all deployed validators and the minting policy.
data ScriptHashesDTO = ScriptHashesDTO
  { scriptHashesMintingPolicy :: Text,
    scriptHashesProfilesValidator :: Text,
    scriptHashesRanksValidator :: Text,
    scriptHashesMembershipsValidator :: Text,
    scriptHashesAchievementsValidator :: Text,
    scriptHashesOracleValidator :: Text
  }
  deriving (Generic, Show)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "scriptHashes", CamelToSnake]] ScriptHashesDTO

instance ToSchema ScriptHashesDTO where
  declareNamedSchema = genericDeclareNamedSchema $
    fromAesonOptions AesonTypes.defaultOptions {AesonTypes.fieldLabelModifier = camelTo2 '_' . dropPrefix "scriptHashes"}

-- Helper function to display rank chain
showRankChain :: [Rank] -> String
showRankChain [] = "│   No previous ranks"
showRankChain ranks =
  Prelude.unlines $ Prelude.zipWith (curry showRankWithIndex) (Prelude.reverse [0 .. (Prelude.length ranks)]) ranks
  where
    showRankWithIndex (i, rank) =
      "│   " <> show i <> ". \n" <> show rank
