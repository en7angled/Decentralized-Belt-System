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