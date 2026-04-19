{-# LANGUAGE OverloadedStrings #-}

-- | MCP write tools for BJJ membership transaction building. Each tool
-- wraps @POST :8082/build-tx@ on the @interaction-api@ with a specific
-- 'ProfileActionType' constructor. All 4 tools are gated on
-- 'MCPServer.App.enableWriteTx'.
--
-- * @build_create_membership_history_tx@   — 'CreateMembershipHistoryAction'
-- * @build_add_membership_interval_tx@     — 'AddMembershipIntervalAction'
-- * @build_accept_membership_interval_tx@  — 'AcceptMembershipIntervalAction'
-- * @build_update_membership_end_date_tx@  — 'UpdateEndDateAction'
module MCPServer.Tools.Memberships
  ( tools
  ) where

import Data.Aeson (Value)
import Data.Map.Strict (Map)
import Data.Text (Text)
import DomainTypes.Core.Actions
  ( ProfileActionType
      ( AcceptMembershipIntervalAction
      , AddMembershipIntervalAction
      , CreateMembershipHistoryAction
      , UpdateEndDateAction
      )
  )
import MCP.Server (InputSchema, ToolHandler)
import MCPServer.App (AppCtx (..))
import MCPServer.Schema
  ( FieldSpec
  , dateField
  , objectSchema
  , profileRefField
  , stringField
  )
import MCPServer.Tools.Common (optionalArg, requireArg)
import MCPServer.Tools.Transactions
  ( buildTxTool
  , userAddressFields
  , userAddressRequired
  )

-- | All membership write tools, gated on 'enableWriteTx'. Read-side
-- membership data is served via @get_practitioner_detail@ /
-- @get_organization_detail@ in 'MCPServer.Tools.Profiles' (see Decision #9
-- of the parent plan) — no read tools live here.
tools :: AppCtx -> [ToolHandler]
tools ctx
  | enableWriteTx ctx =
      [ buildCreateMembershipHistoryTx ctx
      , buildAddMembershipIntervalTx ctx
      , buildAcceptMembershipIntervalTx ctx
      , buildUpdateMembershipEndDateTx ctx
      ]
  | otherwise = []

-- ---------------------------------------------------------------------------
-- build_create_membership_history_tx
-- ---------------------------------------------------------------------------

buildCreateMembershipHistoryTx :: AppCtx -> ToolHandler
buildCreateMembershipHistoryTx =
  buildTxTool
    "build_create_membership_history_tx"
    ( Just
        "Build an unsigned tx that initiates a practitioner's membership \
        \history at an organization. Returns hex-encoded CBOR; the wallet \
        \must sign and submit separately."
    )
    createMembershipHistorySchema
    decodeCreateMembershipHistory

createMembershipHistorySchema :: InputSchema
createMembershipHistorySchema =
  objectSchema
    (createMembershipHistoryFields <> userAddressFields)
    (createMembershipHistoryRequired <> userAddressRequired)

createMembershipHistoryFields :: [(Text, FieldSpec)]
createMembershipHistoryFields =
  [ ("cmh_organization_profile_id", profileRefField)
  , ("cmh_practitioner_profile_id", profileRefField)
  , ("cmh_start_date", dateField (Just "Membership start date (ISO-8601)."))
  , ("cmh_end_date", dateField (Just "Optional membership end date (ISO-8601)."))
  ]

createMembershipHistoryRequired :: [Text]
createMembershipHistoryRequired =
  [ "cmh_organization_profile_id"
  , "cmh_practitioner_profile_id"
  , "cmh_start_date"
  ]

decodeCreateMembershipHistory
  :: Maybe (Map Text Value) -> Either Text ProfileActionType
decodeCreateMembershipHistory args = do
  orgId <- requireArg "cmh_organization_profile_id" args
  pracId <- requireArg "cmh_practitioner_profile_id" args
  start <- requireArg "cmh_start_date" args
  mEnd <- optionalArg "cmh_end_date" args
  pure (CreateMembershipHistoryAction orgId pracId start mEnd)

-- ---------------------------------------------------------------------------
-- build_add_membership_interval_tx
-- ---------------------------------------------------------------------------

buildAddMembershipIntervalTx :: AppCtx -> ToolHandler
buildAddMembershipIntervalTx =
  buildTxTool
    "build_add_membership_interval_tx"
    ( Just
        "Build an unsigned tx that appends a new membership interval to an \
        \existing membership history (e.g. a practitioner rejoining an \
        \organization). Returns hex-encoded CBOR."
    )
    addMembershipIntervalSchema
    decodeAddMembershipInterval

addMembershipIntervalSchema :: InputSchema
addMembershipIntervalSchema =
  objectSchema
    (addMembershipIntervalFields <> userAddressFields)
    (addMembershipIntervalRequired <> userAddressRequired)

addMembershipIntervalFields :: [(Text, FieldSpec)]
addMembershipIntervalFields =
  [ ("ami_organization_profile_id", profileRefField)
  , ( "ami_membership_node_id"
    , stringField
        ( Just
            "Asset class of the membership-history node the interval is \
            \appended to (bech32/hex)."
        )
    )
  , ("ami_start_date", dateField (Just "Interval start date (ISO-8601)."))
  , ("ami_end_date", dateField (Just "Optional interval end date (ISO-8601)."))
  ]

addMembershipIntervalRequired :: [Text]
addMembershipIntervalRequired =
  [ "ami_organization_profile_id"
  , "ami_membership_node_id"
  , "ami_start_date"
  ]

decodeAddMembershipInterval
  :: Maybe (Map Text Value) -> Either Text ProfileActionType
decodeAddMembershipInterval args = do
  orgId <- requireArg "ami_organization_profile_id" args
  nodeId <- requireArg "ami_membership_node_id" args
  start <- requireArg "ami_start_date" args
  mEnd <- optionalArg "ami_end_date" args
  pure (AddMembershipIntervalAction orgId nodeId start mEnd)

-- ---------------------------------------------------------------------------
-- build_accept_membership_interval_tx
-- ---------------------------------------------------------------------------

buildAcceptMembershipIntervalTx :: AppCtx -> ToolHandler
buildAcceptMembershipIntervalTx =
  buildTxTool
    "build_accept_membership_interval_tx"
    ( Just
        "Build an unsigned tx that accepts a pending membership interval on \
        \behalf of the recipient. Returns hex-encoded CBOR."
    )
    acceptMembershipIntervalSchema
    decodeAcceptMembershipInterval

acceptMembershipIntervalSchema :: InputSchema
acceptMembershipIntervalSchema =
  objectSchema
    (acceptMembershipIntervalFields <> userAddressFields)
    (acceptMembershipIntervalRequired <> userAddressRequired)

acceptMembershipIntervalFields :: [(Text, FieldSpec)]
acceptMembershipIntervalFields =
  [
    ( "aci_interval_id"
    , stringField (Just "Asset class of the membership interval to accept.")
    )
  ]

acceptMembershipIntervalRequired :: [Text]
acceptMembershipIntervalRequired = ["aci_interval_id"]

decodeAcceptMembershipInterval
  :: Maybe (Map Text Value) -> Either Text ProfileActionType
decodeAcceptMembershipInterval args = do
  ivl <- requireArg "aci_interval_id" args
  pure (AcceptMembershipIntervalAction ivl)

-- ---------------------------------------------------------------------------
-- build_update_membership_end_date_tx
-- ---------------------------------------------------------------------------

buildUpdateMembershipEndDateTx :: AppCtx -> ToolHandler
buildUpdateMembershipEndDateTx =
  buildTxTool
    "build_update_membership_end_date_tx"
    ( Just
        "Build an unsigned tx that changes the end date of an existing \
        \membership interval. Returns hex-encoded CBOR."
    )
    updateEndDateSchema
    decodeUpdateEndDate

updateEndDateSchema :: InputSchema
updateEndDateSchema =
  objectSchema
    (updateEndDateFields <> userAddressFields)
    (updateEndDateRequired <> userAddressRequired)

updateEndDateFields :: [(Text, FieldSpec)]
updateEndDateFields =
  [
    ( "ude_membership_interval_id"
    , stringField (Just "Asset class of the membership interval to update.")
    )
  ,
    ( "ude_membership_history_node_id"
    , stringField
        ( Just
            "Asset class of the membership-history node the interval hangs \
            \off (used to locate the UTxO)."
        )
    )
  , ("ude_new_end_date", dateField (Just "New end date (ISO-8601)."))
  ]

updateEndDateRequired :: [Text]
updateEndDateRequired =
  [ "ude_membership_interval_id"
  , "ude_membership_history_node_id"
  , "ude_new_end_date"
  ]

decodeUpdateEndDate
  :: Maybe (Map Text Value) -> Either Text ProfileActionType
decodeUpdateEndDate args = do
  ivl <- requireArg "ude_membership_interval_id" args
  node <- requireArg "ude_membership_history_node_id" args
  newEnd <- requireArg "ude_new_end_date" args
  pure (UpdateEndDateAction ivl node newEnd)
