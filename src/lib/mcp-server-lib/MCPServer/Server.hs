{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | WAI composition for the MCP server: serves the MCP protocol under @/mcp@
-- via the third-party 'simpleHttpApp' and a Servant 'ServiceProbe' under
-- @/health@ + @/ready@. Both are combined by a 5-line prefix-dispatcher and
-- wrapped in CORS middleware.
module MCPServer.Server
  ( runMCPServer
  ) where

import Constants (appVersion)
import Control.Concurrent.MVar (MVar, newMVar)
import Data.String (fromString)
import Data.Text (Text, pack)
import MCP.Server
  ( Implementation (..)
  , MCPHandlerState
  , MCPHandlerUser
  , MCPServerState
  , ResourcesCapability (..)
  , ServerCapabilities (..)
  , ToolHandler
  , ToolsCapability (..)
  , defaultProcessHandlers
  , initMCPServerState
  , withToolHandlers
  )
import MCP.Server.HTTP (simpleHttpApp)
import MCPServer.App (AppCtx (..))
import MCPServer.Readiness (handleReady)
import MCPServer.Resources (registerResources)
import qualified MCPServer.Tools.Achievements as ToolsAchievements
import qualified MCPServer.Tools.Docs as ToolsDocs
import qualified MCPServer.Tools.Memberships as ToolsMemberships
import qualified MCPServer.Tools.Profiles as ToolsProfiles
import qualified MCPServer.Tools.Promotions as ToolsPromotions
import qualified MCPServer.Tools.Search as ToolsSearch
import Network.HTTP.Types (status404)
import Network.Wai (Application, pathInfo, responseLBS)
import Network.Wai.Handler.Warp
  ( defaultSettings
  , runSettings
  , setHost
  , setPort
  )
import Servant (Proxy (Proxy), Server, serve, (:<|>) ((:<|>)))
import WebAPI.CORS (setupCors)
import WebAPI.ServiceProbe
  ( ServiceProbe
  , alwaysHealthy
  )

-- The mcp package declares MCPHandlerState and MCPHandlerUser as open type
-- families. The Phase 2 read tools do not thread any per-session state and
-- simpleHttpApp is unauthenticated, so both resolve to unit.
type instance MCPHandlerState = ()
type instance MCPHandlerUser = ()

-- | Servant probe API — reuses 'WebAPI.ServiceProbe' as query-api does.
type ProbeAPI = ServiceProbe Text Text

proxyProbeAPI :: Proxy ProbeAPI
proxyProbeAPI = Proxy

-- | Health probe is liveness-only — never touches external state. Readiness
-- delegates to 'handleReady' which probes both upstream APIs and returns 503
-- if either is unreachable within 'readinessTimeoutMs'.
probeServer :: AppCtx -> Server ProbeAPI
probeServer ctx =
  alwaysHealthy (pack appVersion) "bjj-mcp-server"
    :<|> handleReady ctx

-- | 'ServerCapabilities' advertising the @tools@ and @resources@ method
-- families. Neither advertises notifications for changes — the MCP server's
-- tool / resource surface is static per process lifetime.
minimalCaps :: ServerCapabilities
minimalCaps =
  ServerCapabilities
    { logging = Nothing
    , prompts = Nothing
    , resources =
        Just
          ResourcesCapability
            { subscribe = Just False
            , listChanged = Just False
            }
    , tools = Just ToolsCapability {listChanged = Just False}
    , completions = Nothing
    , experimental = Nothing
    }

minimalImpl :: Implementation
minimalImpl =
  Implementation
    { name = "bjj-mcp-server"
    , version = pack appVersion
    , title = Nothing
    }

-- | Every tool exposed by this server, bound to the shared 'AppCtx'. Read
-- tools are always included; write tools (gated on 'enableWriteTx') are
-- assembled inside each domain module's 'tools' export.
allTools :: AppCtx -> [ToolHandler]
allTools ctx =
  concat
    [ ToolsProfiles.tools ctx
    , ToolsPromotions.tools ctx
    , ToolsAchievements.tools ctx
    , ToolsMemberships.tools ctx
    , ToolsSearch.tools ctx
    , ToolsDocs.tools ctx
    ]

buildMcpState :: AppCtx -> IO (MVar MCPServerState)
buildMcpState ctx =
  newMVar
    ( initMCPServerState
        ()
        Nothing
        Nothing
        minimalCaps
        minimalImpl
        Nothing
        (registerResources (withToolHandlers (allTools ctx) defaultProcessHandlers))
    )

-- | Route incoming requests to the MCP WAI app or the Servant probe WAI app.
-- MCP client SDKs POST JSON-RPC to @/mcp@ and may GET the same path for SSE;
-- orchestrators GET @/health@ or @/ready@.
dispatchByPrefix :: Application -> Application -> Application
dispatchByPrefix mcpApp probeApp req respond = case pathInfo req of
  ("mcp" : _) -> mcpApp req respond
  ("health" : _) -> probeApp req respond
  ("ready" : _) -> probeApp req respond
  _ -> respond (responseLBS status404 [] "")

runMCPServer :: AppCtx -> IO ()
runMCPServer ctx = do
  mcpStateVar <- buildMcpState ctx
  let mcpApp = simpleHttpApp mcpStateVar
      probeApp = serve proxyProbeAPI (probeServer ctx)
      app = setupCors (dispatchByPrefix mcpApp probeApp)
      settings =
        setHost (fromString "0.0.0.0")
          . setPort (port ctx)
          $ defaultSettings
  putStrLn $ "Started MCP server at 0.0.0.0:" <> show (port ctx)
  runSettings settings app
