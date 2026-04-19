{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan 'ToHttpApiData' instances required by @servant-client@ to
-- serialize query parameters when calling @query-api@ from this MCP server.
--
-- The server side (query-api) only needs 'FromHttpApiData' (parse incoming
-- strings), so the existing instances in @offchain-lib@ stop there. Rather
-- than bloat @offchain-lib@ with client-only instances, we concentrate them
-- here where the client code lives.
--
-- Each instance mirrors its corresponding 'FromHttpApiData' parser so round
-- tripping (@parseQueryParam . toQueryParam = Right@) holds for every value
-- the API surface accepts. Where the parser accepts lower-case / snake_case
-- strings, we emit the same — not Haskell's constructor names.
module MCPServer.Orphans () where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (String), withText)
import Data.Text (Text)
import qualified Data.Text as T
import DomainTypes.Core.BJJ (BJJBelt)
import DomainTypes.Core.Types
  ( ProfileType
  , PromotionState
      ( PromotionAccepted
      , PromotionPending
      , PromotionSuperseded
      )
  )
import DomainTypes.Transfer.OrderBy
  ( AchievementsOrderBy
      ( AchievementsOrderByAwardedBy
      , AchievementsOrderByAwardedTo
      , AchievementsOrderByDate
      , AchievementsOrderById
      , AchievementsOrderByName
      )
  , ProfilesOrderBy
      ( ProfilesOrderByDescription
      , ProfilesOrderById
      , ProfilesOrderByName
      , ProfilesOrderByRegisteredAt
      , ProfilesOrderByType
      )
  , PromotionsOrderBy
      ( PromotionsOrderByAchievedBy
      , PromotionsOrderByAwardedBy
      , PromotionsOrderByBelt
      , PromotionsOrderByDate
      , PromotionsOrderById
      )
  , SortOrder (Asc, Desc)
  )
import Web.HttpApiData (ToHttpApiData (..))

-- 'BJJBelt' and 'ProfileType' round-trip via 'show' — the parsers in
-- offchain-lib accept the constructor names verbatim.

instance ToHttpApiData BJJBelt where
  toUrlPiece = T.pack . show

instance ToHttpApiData ProfileType where
  toUrlPiece = T.pack . show

-- The remaining enums have bespoke wire values that do not match @show@.
-- Each branch mirrors the case in the corresponding 'parseQueryParam'.

instance ToHttpApiData PromotionState where
  toUrlPiece = \case
    PromotionPending -> "pending"
    PromotionAccepted -> "accepted"
    PromotionSuperseded -> "superseded"

instance ToHttpApiData SortOrder where
  toUrlPiece = \case
    Asc -> "asc"
    Desc -> "desc"

-- 'SortOrder' in @offchain-lib@ has no JSON instances; add them here so
-- tool argument decoding via 'optionalArg' / 'requireArg' works for the
-- @sort_order@ tool field.
instance ToJSON SortOrder where
  toJSON Asc = String "asc"
  toJSON Desc = String "desc"

instance FromJSON SortOrder where
  parseJSON = withText "SortOrder" $ \case
    "asc" -> pure Asc
    "desc" -> pure Desc
    other -> fail ("Unknown SortOrder: " <> T.unpack other)

instance ToHttpApiData ProfilesOrderBy where
  toUrlPiece = \case
    ProfilesOrderById -> "id"
    ProfilesOrderByName -> "name"
    ProfilesOrderByDescription -> "description"
    ProfilesOrderByType -> "type"
    ProfilesOrderByRegisteredAt -> "registered_at"

instance ToHttpApiData PromotionsOrderBy where
  toUrlPiece = \case
    PromotionsOrderById -> "id"
    PromotionsOrderByBelt -> "belt"
    PromotionsOrderByAchievedBy -> "achieved_by"
    PromotionsOrderByAwardedBy -> "awarded_by"
    PromotionsOrderByDate -> "date"

instance ToHttpApiData AchievementsOrderBy where
  toUrlPiece = \case
    AchievementsOrderById -> "id"
    AchievementsOrderByDate -> "date"
    AchievementsOrderByAwardedTo -> "awarded_to"
    AchievementsOrderByAwardedBy -> "awarded_by"
    AchievementsOrderByName -> "name"

-- Silence the unused-import warning if any 'Text' reference is dropped.
_unusedText :: Text
_unusedText = T.pack ""
