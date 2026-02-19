{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import Control.Lens
import Data.Aeson
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as AesonTypes
import Data.List.Extra
import Data.Swagger
import Data.Swagger.Internal.Schema ()
import Data.Text hiding (init, tail)
import Data.Text qualified as T
import Deriving.Aeson
import GHC.Generics ()
import Servant (FromHttpApiData (..))

data SortOrder = Asc | Desc
  deriving (Show, Generic, Eq)

instance ToSchema SortOrder where
  declareNamedSchema = genericDeclareNamedSchema sortOrderSchemaOptions
    where
      sortOrderSchemaOptions :: SchemaOptions
      sortOrderSchemaOptions =
        fromAesonOptions $
          AesonTypes.defaultOptions
            { AesonTypes.constructorTagModifier = camelTo2 '_' . dropPrefix "SortOrder"
            }

instance ToParamSchema SortOrder where
  toParamSchema _ =
    mempty
      & type_
        ?~ SwaggerString
      & enum_
        ?~ [Aeson.String (T.pack "asc"), Aeson.String (T.pack "desc")]

data ProfilesOrderBy
  = ProfilesOrderById
  | ProfilesOrderByName
  | ProfilesOrderByDescription
  | ProfilesOrderByType
  deriving (Show, Generic, Eq)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[StripPrefix "ProfilesOrderBy", CamelToSnake]] ProfilesOrderBy

instance ToParamSchema ProfilesOrderBy where
  toParamSchema _ =
    mempty
      & type_
        ?~ SwaggerString
      & enum_
        ?~ [ Aeson.String (T.pack "id"),
             Aeson.String (T.pack "name"),
             Aeson.String (T.pack "description"),
             Aeson.String (T.pack "type")
           ]

instance ToSchema ProfilesOrderBy where
  declareNamedSchema = genericDeclareNamedSchema profilesOrderBySchemaOptions
    where
      profilesOrderBySchemaOptions :: SchemaOptions
      profilesOrderBySchemaOptions =
        fromAesonOptions $
          AesonTypes.defaultOptions
            { AesonTypes.constructorTagModifier = camelTo2 '_' . dropPrefix "ProfilesOrderBy"
            }

data PromotionsOrderBy
  = PromotionsOrderById
  | PromotionsOrderByBelt
  | PromotionsOrderByAchievedBy
  | PromotionsOrderByAwardedBy
  | PromotionsOrderByDate
  deriving (Show, Generic, Eq)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[StripPrefix "PromotionsOrderBy", CamelToSnake]] PromotionsOrderBy

instance ToSchema PromotionsOrderBy where
  declareNamedSchema = genericDeclareNamedSchema promotionsOrderBySchemaOptions
    where
      promotionsOrderBySchemaOptions :: SchemaOptions
      promotionsOrderBySchemaOptions =
        fromAesonOptions $
          AesonTypes.defaultOptions
            { AesonTypes.constructorTagModifier = camelTo2 '_' . dropPrefix "PromotionsOrderBy"
            }

instance ToParamSchema PromotionsOrderBy where
  toParamSchema _ =
    mempty
      & type_
        ?~ SwaggerString
      & enum_
        ?~ [ Aeson.String (T.pack "id"),
             Aeson.String (T.pack "belt"),
             Aeson.String (T.pack "achieved_by"),
             Aeson.String (T.pack "awarded_by"),
             Aeson.String (T.pack "date")
           ]

data RanksOrderBy
  = RanksOrderById
  | RanksOrderByBelt
  | RanksOrderByAchievedBy
  | RanksOrderByAwardedBy
  | RanksOrderByDate
  deriving (Show, Generic, Eq)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[StripPrefix "RanksOrderBy", CamelToSnake]] RanksOrderBy

instance ToSchema RanksOrderBy where
  declareNamedSchema = genericDeclareNamedSchema ranksOrderBySchemaOptions
    where
      ranksOrderBySchemaOptions :: SchemaOptions
      ranksOrderBySchemaOptions =
        fromAesonOptions $
          AesonTypes.defaultOptions
            { AesonTypes.constructorTagModifier = camelTo2 '_' . dropPrefix "RanksOrderBy"
            }

instance ToParamSchema RanksOrderBy where
  toParamSchema _ =
    mempty
      & type_
        ?~ SwaggerString
      & enum_
        ?~ [ Aeson.String (T.pack "id"),
             Aeson.String (T.pack "belt"),
             Aeson.String (T.pack "achieved_by"),
             Aeson.String (T.pack "awarded_by"),
             Aeson.String (T.pack "date")
           ]

instance FromHttpApiData SortOrder where
  parseQueryParam :: Text -> Either Text SortOrder
  parseQueryParam t =
    case T.toLower t of
      "asc" -> Right Asc
      "desc" -> Right Desc
      _ -> Left "Invalid sort order. Use 'asc' or 'desc'"

instance FromHttpApiData ProfilesOrderBy where
  parseQueryParam :: Text -> Either Text ProfilesOrderBy
  parseQueryParam t =
    case T.toLower t of
      "name" -> Right ProfilesOrderByName
      "id" -> Right ProfilesOrderById
      "description" -> Right ProfilesOrderByDescription
      "type" -> Right ProfilesOrderByType
      _ -> Left "Invalid order by. Use 'name', 'id', 'description', or 'type'"

instance FromHttpApiData PromotionsOrderBy where
  parseQueryParam :: Text -> Either Text PromotionsOrderBy
  parseQueryParam t =
    case T.toLower t of
      "id" -> Right PromotionsOrderById
      "belt" -> Right PromotionsOrderByBelt
      "achieved_by" -> Right PromotionsOrderByAchievedBy
      "awarded_by" -> Right PromotionsOrderByAwardedBy
      "date" -> Right PromotionsOrderByDate
      _ -> Left "Invalid order by. Use 'id', 'belt', 'achieved_by', 'awarded_by', or 'date'"

instance FromHttpApiData RanksOrderBy where
  parseQueryParam :: Text -> Either Text RanksOrderBy
  parseQueryParam t =
    case T.toLower t of
      "id" -> Right RanksOrderById
      "belt" -> Right RanksOrderByBelt
      "achieved_by" -> Right RanksOrderByAchievedBy
      "awarded_by" -> Right RanksOrderByAwardedBy
      "date" -> Right RanksOrderByDate
      _ -> Left "Invalid order by. Use 'id', 'belt', 'achieved_by', 'awarded_by', or 'date'"

data AchievementsOrderBy
  = AchievementsOrderById
  | AchievementsOrderByDate
  | AchievementsOrderByAwardedTo
  | AchievementsOrderByAwardedBy
  | AchievementsOrderByName
  deriving (Show, Generic, Eq)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[StripPrefix "AchievementsOrderBy", CamelToSnake]] AchievementsOrderBy

instance ToSchema AchievementsOrderBy where
  declareNamedSchema = genericDeclareNamedSchema achievementsOrderBySchemaOptions
    where
      achievementsOrderBySchemaOptions :: SchemaOptions
      achievementsOrderBySchemaOptions =
        fromAesonOptions $
          AesonTypes.defaultOptions
            { AesonTypes.constructorTagModifier = camelTo2 '_' . dropPrefix "AchievementsOrderBy"
            }

instance ToParamSchema AchievementsOrderBy where
  toParamSchema _ =
    mempty
      & type_
        ?~ SwaggerString
      & enum_
        ?~ [ Aeson.String (T.pack "id"),
             Aeson.String (T.pack "date"),
             Aeson.String (T.pack "awarded_to"),
             Aeson.String (T.pack "awarded_by"),
             Aeson.String (T.pack "name")
           ]

instance FromHttpApiData AchievementsOrderBy where
  parseQueryParam t =
    case T.toLower t of
      "id" -> Right AchievementsOrderById
      "date" -> Right AchievementsOrderByDate
      "awarded_to" -> Right AchievementsOrderByAwardedTo
      "awarded_by" -> Right AchievementsOrderByAwardedBy
      "name" -> Right AchievementsOrderByName
      _ -> Left "Invalid order by. Use 'id', 'date', 'awarded_to', 'awarded_by', or 'name'"

data MembershipHistoriesOrderBy
  = MembershipHistoriesOrderById
  | MembershipHistoriesOrderByCreatedAt
  | MembershipHistoriesOrderByPractitioner
  | MembershipHistoriesOrderByOrganization
  deriving (Show, Generic, Eq)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[StripPrefix "MembershipHistoriesOrderBy", CamelToSnake]] MembershipHistoriesOrderBy

instance ToSchema MembershipHistoriesOrderBy where
  declareNamedSchema = genericDeclareNamedSchema membershipHistoriesOrderBySchemaOptions
    where
      membershipHistoriesOrderBySchemaOptions :: SchemaOptions
      membershipHistoriesOrderBySchemaOptions =
        fromAesonOptions $
          AesonTypes.defaultOptions
            { AesonTypes.constructorTagModifier = camelTo2 '_' . dropPrefix "MembershipHistoriesOrderBy"
            }

instance ToParamSchema MembershipHistoriesOrderBy where
  toParamSchema _ =
    mempty
      & type_
        ?~ SwaggerString
      & enum_
        ?~ [ Aeson.String (T.pack "id"),
             Aeson.String (T.pack "created_at"),
             Aeson.String (T.pack "practitioner"),
             Aeson.String (T.pack "organization")
           ]

instance FromHttpApiData MembershipHistoriesOrderBy where
  parseQueryParam t =
    case T.toLower t of
      "id" -> Right MembershipHistoriesOrderById
      "created_at" -> Right MembershipHistoriesOrderByCreatedAt
      "practitioner" -> Right MembershipHistoriesOrderByPractitioner
      "organization" -> Right MembershipHistoriesOrderByOrganization
      _ -> Left "Invalid order by. Use 'id', 'created_at', 'practitioner', or 'organization'"

data MembershipIntervalsOrderBy
  = MembershipIntervalsOrderById
  | MembershipIntervalsOrderByStartDate
  | MembershipIntervalsOrderByIntervalNumber
  | MembershipIntervalsOrderByPractitioner
  deriving (Show, Generic, Eq)
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[StripPrefix "MembershipIntervalsOrderBy", CamelToSnake]] MembershipIntervalsOrderBy

instance ToSchema MembershipIntervalsOrderBy where
  declareNamedSchema = genericDeclareNamedSchema membershipIntervalsOrderBySchemaOptions
    where
      membershipIntervalsOrderBySchemaOptions :: SchemaOptions
      membershipIntervalsOrderBySchemaOptions =
        fromAesonOptions $
          AesonTypes.defaultOptions
            { AesonTypes.constructorTagModifier = camelTo2 '_' . dropPrefix "MembershipIntervalsOrderBy"
            }

instance ToParamSchema MembershipIntervalsOrderBy where
  toParamSchema _ =
    mempty
      & type_
        ?~ SwaggerString
      & enum_
        ?~ [ Aeson.String (T.pack "id"),
             Aeson.String (T.pack "start_date"),
             Aeson.String (T.pack "interval_number"),
             Aeson.String (T.pack "practitioner")
           ]

instance FromHttpApiData MembershipIntervalsOrderBy where
  parseQueryParam t =
    case T.toLower t of
      "id" -> Right MembershipIntervalsOrderById
      "start_date" -> Right MembershipIntervalsOrderByStartDate
      "interval_number" -> Right MembershipIntervalsOrderByIntervalNumber
      "practitioner" -> Right MembershipIntervalsOrderByPractitioner
      _ -> Left "Invalid order by. Use 'id', 'start_date', 'interval_number', or 'practitioner'"