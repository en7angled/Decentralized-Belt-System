{-# LANGUAGE OverloadedStrings #-}

-- | MCP read tools backed by @query-api@'s cross-entity search + pending-action
-- routes:
--
-- * @search@              — wraps @GET \/search?q=...@ (required query)
-- * @get_pending_actions@ — wraps @GET \/pages\/pending-actions\/{profile-id}@
module MCPServer.Tools.Search
  ( tools
  ) where

import Control.Monad.IO.Class (liftIO)
import MCP.Server
  ( ProcessResult (ProcessSuccess)
  , ToolHandler
  , toolHandler
  )
import qualified MCPServer.Clients as C
import MCPServer.App (AppCtx (..))
import MCPServer.Schema
  ( objectSchema
  , profileRefField
  , stringField
  )
import MCPServer.Tools.Common
  ( errorResult
  , jsonResult
  , requireArg
  , runUpstreamQuery
  , sanitizeClientError
  )

tools :: AppCtx -> [ToolHandler]
tools ctx =
  [ searchTool ctx
  , getPendingActions ctx
  ]

searchTool :: AppCtx -> ToolHandler
searchTool ctx =
  toolHandler
    "search"
    (Just
       "Unified search across practitioners, organizations, ranks, promotions, \
       \and achievements. The @q@ argument is required.")
    (objectSchema
        [("q", stringField (Just "Search query."))]
        ["q"])
    $ \args -> case requireArg "q" args of
      Left e -> pure (ProcessSuccess (errorResult e))
      Right q -> do
        r <- liftIO (runUpstreamQuery ctx (C.search (upstreamAuth ctx) q))
        pure . ProcessSuccess $ either (errorResult . sanitizeClientError) jsonResult r

getPendingActions :: AppCtx -> ToolHandler
getPendingActions ctx =
  toolHandler
    "get_pending_actions"
    (Just
       "Pending promotions, unaccepted achievements, and unaccepted membership \
       \intervals for a profile.")
    (objectSchema [("profile_id", profileRefField)] ["profile_id"])
    $ \args -> case requireArg "profile_id" args of
      Left e -> pure (ProcessSuccess (errorResult e))
      Right pid -> do
        r <- liftIO (runUpstreamQuery ctx (C.getPendingActions (upstreamAuth ctx) pid))
        pure . ProcessSuccess $ either (errorResult . sanitizeClientError) jsonResult r
