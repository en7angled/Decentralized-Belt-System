{-# LANGUAGE OverloadedStrings #-}

-- | MCP tools backed by @query-api@'s achievement routes and
-- @interaction-api@'s achievement tx-build flow:
--
-- * @get_achievements@              — wraps @GET \/achievements@
-- * @get_achievement_by_id@         — wraps @GET \/achievement\/{achievement-id}@
--   (returns 'AchievementInformationResponse', not 'AchievementResponse' —
--   this was the parent plan's table ambiguity flagged in the Phase 2 plan).
-- * @build_accept_achievement_tx@   — wraps @POST \/build-tx@ with
--   'AcceptAchievementAction'. Gated on 'MCPServer.App.enableWriteTx'.
--
-- Note: @build_award_achievement_tx@ (the issuance side) is intentionally
-- excluded — @POST \/award-achievement@ on @interaction-api@ is a multipart
-- endpoint that uploads an image to IPFS, which the LLM has no bytes to
-- produce. That flow stays in the existing DApp UI (see Decision #8 of
-- the parent plan).
module MCPServer.Tools.Achievements
  ( tools
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import Data.Map.Strict (Map)
import qualified Data.Maybe
import Data.Text (Text)
import DomainTypes.Core.Actions
  ( ProfileActionType (AcceptAchievementAction)
  )
import MCP.Server
  ( InputSchema
  , ProcessResult (ProcessSuccess)
  , ToolHandler
  , toolHandler
  )
import qualified MCPServer.Clients as C
import MCPServer.App (AppCtx (..))
import MCPServer.Schema
  ( FieldSpec
  , achievementRefField
  , arrayOf
  , boolField
  , dateField
  , objectSchema
  , orderByField
  , paginationFields
  , profileRefField
  , sortOrderField
  , stringField
  )
import MCPServer.Tools.Common
  ( errorResult
  , jsonResult
  , optionalArg
  , requireArg
  , runUpstreamQuery
  , sanitizeClientError
  )
import MCPServer.Tools.Transactions
  ( buildTxTool
  , userAddressFields
  , userAddressRequired
  )

tools :: AppCtx -> [ToolHandler]
tools ctx =
  [ getAchievements ctx
  , getAchievementById ctx
  ]
    <> [buildAcceptAchievementTx ctx | enableWriteTx ctx]

-- ---------------------------------------------------------------------------
-- Read tools
-- ---------------------------------------------------------------------------

getAchievements :: AppCtx -> ToolHandler
getAchievements ctx =
  toolHandler
    "get_achievements"
    ( Just
        "List BJJ achievements with filters: awarded-to/by profiles, accepted \
        \flag, time window, fuzzy text, pagination, ordering."
    )
    achievementsSchema
    $ \args ->
      case ( optionalArg "limit" args
           , optionalArg "offset" args
           , optionalArg "achievements" args
           , optionalArg "awarded_to" args
           , optionalArg "awarded_by" args
           , optionalArg "accepted" args
           , optionalArg "from" args
           , optionalArg "to" args
           , optionalArg "q" args
           , optionalArg "order_by" args
           , optionalArg "sort_order" args
           ) of
        ( Right lim
          , Right off
          , Right achIds
          , Right awTo
          , Right awBy
          , Right acc
          , Right from
          , Right to_
          , Right q
          , Right ord
          , Right dir
          ) -> do
            let call =
                  C.getAchievements
                    (upstreamAuth ctx)
                    lim
                    off
                    (Data.Maybe.fromMaybe [] achIds)
                    (Data.Maybe.fromMaybe [] awTo)
                    (Data.Maybe.fromMaybe [] awBy)
                    acc
                    from
                    to_
                    q
                    ord
                    dir
            r <- liftIO (runUpstreamQuery ctx call)
            pure . ProcessSuccess $ either (errorResult . sanitizeClientError) jsonResult r
        _ ->
          pure (ProcessSuccess (errorResult "invalid get_achievements arguments"))

achievementsSchema :: InputSchema
achievementsSchema =
  objectSchema
    ( paginationFields
        <> [ ("achievements", arrayOf achievementRefField)
           , ("awarded_to", arrayOf profileRefField)
           , ("awarded_by", arrayOf profileRefField)
           , ("accepted", boolField (Just "Only accepted (true) or only pending (false)."))
           , ("from", dateField (Just "Inclusive lower bound (ISO-8601)."))
           , ("to", dateField (Just "Exclusive upper bound (ISO-8601)."))
           , ("q", stringField (Just "Fuzzy text search."))
           ,
             ( "order_by"
             , orderByField ["id", "date", "awarded_to", "awarded_by", "name"]
             )
           , ("sort_order", sortOrderField)
           ]
    )
    []

getAchievementById :: AppCtx -> ToolHandler
getAchievementById ctx =
  toolHandler
    "get_achievement_by_id"
    (Just "Look up a single achievement by its asset-class ID.")
    (objectSchema [("achievement_id", achievementRefField)] ["achievement_id"])
    $ \args -> case requireArg "achievement_id" args of
      Left e -> pure (ProcessSuccess (errorResult e))
      Right aid -> do
        r <- liftIO (runUpstreamQuery ctx (C.getAchievementById (upstreamAuth ctx) aid))
        pure . ProcessSuccess $ either (errorResult . sanitizeClientError) jsonResult r

-- ---------------------------------------------------------------------------
-- Write tools (gated on enableWriteTx)
-- ---------------------------------------------------------------------------

buildAcceptAchievementTx :: AppCtx -> ToolHandler
buildAcceptAchievementTx =
  buildTxTool
    "build_accept_achievement_tx"
    ( Just
        "Build an unsigned tx that accepts a pending achievement on behalf \
        \of the recipient. Returns hex-encoded CBOR; the wallet must sign \
        \and submit separately."
    )
    acceptAchievementSchema
    decodeAcceptAchievement

acceptAchievementSchema :: InputSchema
acceptAchievementSchema =
  objectSchema
    (acceptAchievementFields <> userAddressFields)
    (acceptAchievementRequired <> userAddressRequired)

acceptAchievementFields :: [(Text, FieldSpec)]
acceptAchievementFields =
  [ ("aca_achievement_id", achievementRefField)
  ]

acceptAchievementRequired :: [Text]
acceptAchievementRequired = ["aca_achievement_id"]

decodeAcceptAchievement
  :: Maybe (Map Text Value) -> Either Text ProfileActionType
decodeAcceptAchievement args = do
  aid <- requireArg "aca_achievement_id" args
  pure (AcceptAchievementAction aid)
