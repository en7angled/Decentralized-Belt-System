{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | MCP resources exposed by the server. Phase 2 ships two URIs:
--
-- * @bjj:\/\/rules\/annex-3@ — authored markdown of the protocol's promotion
--   rules, embedded via @file-embed@ at compile time.
-- * @bjj:\/\/rules\/belt-hierarchy@ — JSON list of every 'BJJBelt'
--   constructor name, generated at module load. Because the list is built
--   from @[White .. Red10]@ and @Show BJJBelt@, it cannot drift from the
--   on-chain type.
--
-- The fees resource (parent plan's @bjj:\/\/config\/fees@) is deferred: fee
-- data lives on-chain inside @FeeConfig@ and is oracle-adjustable, so a
-- file-embedded snapshot would go stale. See the Phase 2 plan §Step 3.
--
-- 'registerResources' wires the handlers into a 'ProcessHandlers' record
-- — the @mcp-0.3.1.0@ package exports no @withResourceHandlers@ analogous
-- to @withToolHandlers@ (confirmed by Step 0 of the Phase 2 mini-spike).
module MCPServer.Resources
  ( bjjResources
  , faqBody
  , registerResources
  ) where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy as BL
import Data.FileEmbed (embedStringFile, makeRelativeToProject)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import DomainTypes.Core.BJJ (BJJBelt (Red10, White))
import MCP.Server
  ( ListResourcesResult (..)
  , ProcessHandlers (..)
  , ProcessResult (ProcessRPCError, ProcessSuccess)
  , ReadResourceParams (..)
  , ReadResourceResult (..)
  , Resource (..)
  , ResourceContents (TextResource)
  , TextResourceContents (..)
  )

-- ---------------------------------------------------------------------------
-- Resource metadata
-- ---------------------------------------------------------------------------

annex3Uri :: Text
annex3Uri = "bjj://rules/annex-3"

beltHierarchyUri :: Text
beltHierarchyUri = "bjj://rules/belt-hierarchy"

faqUri :: Text
faqUri = "bjj://docs/faq"

annex3Resource :: Resource
annex3Resource =
  Resource
    { uri = annex3Uri
    , name = "BJJ Protocol — Annex 3"
    , title = Just "Annex 3: Promotion Rules"
    , description =
        Just
          "LLM-consumable summary of the BJJ protocol's belt-promotion rules: \
          \belt hierarchy, minimum time in grade, and granting-authority matrix. \
          \Source of truth is docs/specification.md and Onchain.BJJ."
    , mimeType = Just "text/markdown"
    , size = Just (fromIntegral (BL.length (BL.fromStrict (TE.encodeUtf8 annex3Body))))
    , annotations = Nothing
    , _meta = Nothing
    }

beltHierarchyResource :: Resource
beltHierarchyResource =
  Resource
    { uri = beltHierarchyUri
    , name = "BJJ Belt Hierarchy"
    , title = Just "Belt Hierarchy (runtime-generated)"
    , description =
        Just
          "Ordered JSON array of every on-chain BJJ belt, junior-to-senior. \
          \Generated from the Haskell enum; cannot drift from the on-chain type."
    , mimeType = Just "application/json"
    , size = Just (fromIntegral (BL.length beltHierarchyBody))
    , annotations = Nothing
    , _meta = Nothing
    }

faqResource :: Resource
faqResource =
  Resource
    { uri = faqUri
    , name = "BJJ Belts — FAQ"
    , title = Just "Frequently Asked Questions"
    , description =
        Just
          "Product-level FAQ for the Decentralized Belt System: what it is, \
          \who it's for, how promotions / lineage / memberships / achievements \
          \work, wallet and trust model. Canonical copy shared with the web app."
    , mimeType = Just "text/markdown"
    , size = Just (fromIntegral (BL.length (BL.fromStrict (TE.encodeUtf8 faqBody))))
    , annotations = Nothing
    , _meta = Nothing
    }

bjjResources :: [Resource]
bjjResources = [annex3Resource, beltHierarchyResource, faqResource]

-- ---------------------------------------------------------------------------
-- Resource bodies
-- ---------------------------------------------------------------------------

-- | Compile-time-embedded Annex 3 markdown. 'makeRelativeToProject' anchors
-- the path to the nearest @.cabal@ file rather than GHC's CWD, so the
-- splice works whether cabal builds in-tree or from an extracted sdist
-- tarball (Docker builds use the latter).
annex3Body :: Text
annex3Body =
  T.pack $(makeRelativeToProject "src/lib/mcp-server-lib/resources/annex-3.md" >>= embedStringFile)

-- | JSON array of every 'BJJBelt' constructor name, built once per process.
beltHierarchyBody :: BL.ByteString
beltHierarchyBody = encode (fmap show [White .. Red10])

-- | Compile-time-embedded FAQ markdown (canonical product copy). Re-exported
-- so the @get_faq@ MCP tool can serve the same bytes without a second
-- @resources/read@ round-trip.
faqBody :: Text
faqBody =
  T.pack $(makeRelativeToProject "src/lib/mcp-server-lib/resources/bjj-faq.md" >>= embedStringFile)

-- ---------------------------------------------------------------------------
-- Handlers
-- ---------------------------------------------------------------------------

-- | @resources/list@ handler: returns the static 'bjjResources' list every
-- time, no pagination.
listHandler
  :: Monad m
  => params -> m (ProcessResult ListResourcesResult)
listHandler _ =
  pure $
    ProcessSuccess
      ListResourcesResult
        { resources = bjjResources
        , nextCursor = Nothing
        , _meta = Nothing
        }

-- | @resources/read@ handler: dispatch on 'ReadResourceParams.uri'.
readHandler
  :: Monad m
  => ReadResourceParams -> m (ProcessResult ReadResourceResult)
readHandler ReadResourceParams {uri = u}
  | u == annex3Uri =
      pure . ProcessSuccess $
        ReadResourceResult
          { contents =
              [ TextResource
                  TextResourceContents
                    { uri = annex3Uri
                    , text = annex3Body
                    , mimeType = Just "text/markdown"
                    , _meta = Nothing
                    }
              ]
          , _meta = Nothing
          }
  | u == beltHierarchyUri =
      pure . ProcessSuccess $
        ReadResourceResult
          { contents =
              [ TextResource
                  TextResourceContents
                    { uri = beltHierarchyUri
                    , text = TE.decodeUtf8 (BL.toStrict beltHierarchyBody)
                    , mimeType = Just "application/json"
                    , _meta = Nothing
                    }
              ]
          , _meta = Nothing
          }
  | u == faqUri =
      pure . ProcessSuccess $
        ReadResourceResult
          { contents =
              [ TextResource
                  TextResourceContents
                    { uri = faqUri
                    , text = faqBody
                    , mimeType = Just "text/markdown"
                    , _meta = Nothing
                    }
              ]
          , _meta = Nothing
          }
  | otherwise =
      -- JSON-RPC error code -32602 is "Invalid params" in the MCP wire spec;
      -- reuse the same for unknown resource URIs.
      pure (ProcessRPCError (-32602) ("Unknown resource URI: " <> u))

-- | Merge the resource handlers into an existing 'ProcessHandlers'.
-- Called from 'MCPServer.Server.buildMcpState' after tools are registered.
registerResources :: ProcessHandlers -> ProcessHandlers
registerResources ph =
  ph
    { listResourcesHandler = Just listHandler
    , readResourceHandler = Just readHandler
    }
