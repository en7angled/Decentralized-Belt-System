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
import GHC.Generics ()

import DomainTypes.Core.Types
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

-- Helper function to display rank chain
showRankChain :: [Rank] -> String
showRankChain [] = "│   No previous ranks"
showRankChain ranks =
  Prelude.unlines $ Prelude.zipWith (curry showRankWithIndex) (Prelude.reverse [0 .. (Prelude.length ranks)]) ranks
  where
    showRankWithIndex (i, rank) =
      "│   " <> show i <> ". \n" <> show rank




