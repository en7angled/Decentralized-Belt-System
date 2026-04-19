{-# LANGUAGE OverloadedStrings #-}

-- | JSON Schema Draft-7 helpers for MCP tool input schemas.
--
-- Every helper returns an 'Aeson.Value' so that field maps compose cleanly
-- into 'MCP.Server.InputSchema's @properties :: Maybe (Map Text Value)@
-- slot (see @mcp-types-0.1.1/src/MCP/Types.hs:664-668@).
--
-- The BJJ-specific helpers pin their vocabulary to the on-chain enum
-- 'DomainTypes.Core.BJJ.BJJBelt' so the tool surface cannot drift from the
-- protocol.
module MCPServer.Schema
  ( FieldSpec
  , objectSchema
  , emptyInputSchema
    -- * Primitive fields
  , stringField
  , intField
  , boolField
  , enumField
  , arrayOf
  , dateField
    -- * Asset-class / pattern fields
  , profileRefField
  , achievementRefField
  , rankRefField
  , beltField
    -- * Shared pagination / ordering
  , paginationFields
  , orderByField
  , sortOrderField
  ) where

import Data.Aeson (Value (Object, String), object, toJSON, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import DomainTypes.Core.BJJ (BJJBelt (Red10, White))
import MCP.Server (InputSchema (..))

-- | Shorthand for a JSON-Schema fragment describing one field.
type FieldSpec = Value

-- | Build the MCP 'InputSchema' wrapper from a list of @(name, schema)@
-- field entries and an explicit list of required field names.
objectSchema :: [(Text, FieldSpec)] -> [Text] -> InputSchema
objectSchema fields reqs =
  InputSchema
    { schemaType = "object"
    , properties =
        if null fields
          then Nothing
          else Just (Map.fromList fields)
    , required = if null reqs then Nothing else Just reqs
    }

-- | @{"type":"object"}@ with no declared properties.
emptyInputSchema :: InputSchema
emptyInputSchema = objectSchema [] []

-- | @{"type":"string","description":"..."}@ with optional human description.
stringField :: Maybe Text -> FieldSpec
stringField mDesc = withDesc mDesc (object ["type" .= ("string" :: Text)])

-- | @{"type":"integer"}@.
intField :: Maybe Text -> FieldSpec
intField mDesc = withDesc mDesc (object ["type" .= ("integer" :: Text)])

-- | @{"type":"boolean"}@.
boolField :: Maybe Text -> FieldSpec
boolField mDesc = withDesc mDesc (object ["type" .= ("boolean" :: Text)])

-- | @{"type":"string","enum":[...]}@.
enumField :: [Text] -> Maybe Text -> FieldSpec
enumField values mDesc =
  withDesc mDesc $
    object
      [ "type" .= ("string" :: Text)
      , "enum" .= toJSON values
      ]

-- | @{"type":"array","items":<inner>}@. Applied to any 'FieldSpec'.
arrayOf :: FieldSpec -> FieldSpec
arrayOf inner =
  object
    [ "type" .= ("array" :: Text)
    , "items" .= inner
    ]

-- | ISO-8601 date-time: @{"type":"string","format":"date-time"}@.
dateField :: Maybe Text -> FieldSpec
dateField mDesc =
  withDesc mDesc $
    object
      [ "type" .= ("string" :: Text)
      , "format" .= ("date-time" :: Text)
      ]

-- | Asset-class regex shared by profile / achievement / rank references on
-- Cardano: 56-hex policy id, dot, asset name hex.
assetClassPattern :: Text
assetClassPattern = "^[0-9a-f]{56}\\.[0-9a-f]*$"

-- | Practitioner or organization profile asset class.
profileRefField :: FieldSpec
profileRefField =
  object
    [ "type" .= ("string" :: Text)
    , "pattern" .= assetClassPattern
    , "description"
        .= ( "BJJ profile asset class (56-hex policy + '.' + asset-name hex)."
               :: Text
           )
    ]

-- | Achievement asset class; same pattern as 'profileRefField'.
achievementRefField :: FieldSpec
achievementRefField =
  object
    [ "type" .= ("string" :: Text)
    , "pattern" .= assetClassPattern
    , "description"
        .= ( "BJJ achievement asset class (56-hex policy + '.' + asset-name hex)."
               :: Text
           )
    ]

-- | Rank asset class; same pattern as 'profileRefField'.
rankRefField :: FieldSpec
rankRefField =
  object
    [ "type" .= ("string" :: Text)
    , "pattern" .= assetClassPattern
    , "description"
        .= ( "BJJ rank asset class (56-hex policy + '.' + asset-name hex)."
               :: Text
           )
    ]

-- | Enum over every 'BJJBelt' constructor. Generated at module load so the
-- enum can never drift from the on-chain type.
beltField :: FieldSpec
beltField =
  withDesc (Just "BJJ belt rank.") $
    object
      [ "type" .= ("string" :: Text)
      , "enum" .= toJSON (fmap show allBelts)
      ]
  where
    -- White is the first constructor; Red10 is the last. No 'Bounded' is
    -- derived on 'BJJBelt' (the onchain module derives only Generic/Show
    -- on the datum, plus custom Eq/Ord/Enum), so we name the range explicitly.
    allBelts :: [BJJBelt]
    allBelts = [White .. Red10]

-- | Shared pagination fields: @limit@ and @offset@, both optional integers.
paginationFields :: [(Text, FieldSpec)]
paginationFields =
  [ ("limit", intField (Just "Maximum number of items to return."))
  , ("offset", intField (Just "Offset into the result set."))
  ]

-- | Field for an order-by enum. Caller supplies the accepted constructor
-- names (e.g. @["Name","Belt","DateCreated"]@).
orderByField :: [Text] -> FieldSpec
orderByField values =
  enumField values (Just "Field to order the result set by.")

-- | @asc@ / @desc@ sort direction.
sortOrderField :: FieldSpec
sortOrderField =
  enumField
    ["asc", "desc"]
    (Just "Sort direction.")

-- Internal — append a @"description"@ key to an existing JSON object fragment.
withDesc :: Maybe Text -> Value -> Value
withDesc Nothing v = v
withDesc (Just d) (Data.Aeson.Object km) =
  Data.Aeson.Object (KM.insert "description" (String d) km)
withDesc _ v = v
