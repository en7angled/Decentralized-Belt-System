{-# LANGUAGE OverloadedStrings #-}

-- | MCP read tools that surface compile-time-embedded documentation so the
-- agent (which consumes tools, not resources) can answer product-level "what
-- is this?" questions from the canonical copy without going upstream.
--
-- Currently exposes:
--
-- * @get_faq@ — returns the full Decentralized Belt System FAQ as markdown.
--   Mirrors @bjj:\/\/docs\/faq@ in 'MCPServer.Resources' so the MCP resource
--   stays the single source of truth and the frontend + agent read the same
--   bytes.
module MCPServer.Tools.Docs
  ( tools
  ) where

import MCP.Server
  ( ProcessResult (ProcessSuccess)
  , ToolHandler
  , toolHandler
  , toolTextResult
  )
import MCPServer.App (AppCtx)
import MCPServer.Resources (faqBody)
import MCPServer.Schema (objectSchema)

tools :: AppCtx -> [ToolHandler]
tools _ = [getFaq]

getFaq :: ToolHandler
getFaq =
  toolHandler
    "get_faq"
    (Just
       "Returns the Decentralized Belt System FAQ (markdown) — product-level \
       \questions about profiles, belts, promotions, lineage, memberships, \
       \achievements, wallet, and trust. Use this when the user asks how the \
       \product works rather than a specific registry lookup.")
    (objectSchema [] [])
    $ \_args -> pure (ProcessSuccess (toolTextResult [faqBody]))
