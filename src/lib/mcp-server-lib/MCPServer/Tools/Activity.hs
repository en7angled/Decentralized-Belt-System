{-# LANGUAGE OverloadedStrings #-}

-- | MCP read tool backed by @query-api@'s cross-entity activity feed:
--
-- * @get_activity@ — wraps @GET \/activity@ (reverse-chronological feed across
--   profiles, promotions, achievements, and memberships; optional
--   @event_type@ / @actor@ / @since@ filters plus pagination).
module MCPServer.Tools.Activity
  ( tools
  ) where

import Control.Monad.IO.Class (liftIO)
import MCP.Server
  ( InputSchema
  , ProcessResult (ProcessSuccess)
  , ToolHandler
  , toolHandler
  )
import qualified MCPServer.Clients as C
import MCPServer.App (AppCtx (..))
import MCPServer.Schema
  ( dateField
  , enumField
  , intField
  , objectSchema
  , profileRefField
  )
import MCPServer.Tools.Common
  ( errorResult
  , jsonResult
  , optionalArg
  , runUpstreamQuery
  , sanitizeClientError
  )

tools :: AppCtx -> [ToolHandler]
tools ctx = [getActivity ctx]

getActivity :: AppCtx -> ToolHandler
getActivity ctx =
  toolHandler
    "get_activity"
    ( Just
        "Reverse-chronological activity feed across profiles, promotions, \
        \achievements, and memberships. All filters optional: `event_type` \
        \(one of ProfileCreated, PromotionIssued, PromotionAccepted, \
        \PromotionSuperseded, AchievementAwarded, AchievementAccepted, \
        \MembershipGranted, MembershipAccepted), `actor` (profile id), `since` \
        \(ISO-8601), plus `limit` / `offset`."
    )
    activitySchema
    $ \args ->
      case ( optionalArg "limit" args
           , optionalArg "offset" args
           , optionalArg "event_type" args
           , optionalArg "actor" args
           , optionalArg "since" args
           ) of
        (Right lim, Right off, Right evt, Right actor, Right since) -> do
          let call = C.getActivityFeed (upstreamAuth ctx) lim off evt actor since
          r <- liftIO (runUpstreamQuery ctx call)
          pure . ProcessSuccess $ either (errorResult . sanitizeClientError) jsonResult r
        _ ->
          pure (ProcessSuccess (errorResult "invalid get_activity arguments"))

activitySchema :: InputSchema
activitySchema =
  objectSchema
    [ ("limit", intField (Just "Maximum number of events to return (default 20)."))
    , ("offset", intField (Just "Offset into the feed."))
    ,
      ( "event_type"
      , enumField
          [ "ProfileCreated"
          , "PromotionIssued"
          , "PromotionAccepted"
          , "PromotionSuperseded"
          , "AchievementAwarded"
          , "AchievementAccepted"
          , "MembershipGranted"
          , "MembershipAccepted"
          ]
          (Just "Restrict the feed to one activity event type.")
      )
    , ("actor", profileRefField)
    , ("since", dateField (Just "Only events at or after this instant (ISO-8601)."))
    ]
    []
