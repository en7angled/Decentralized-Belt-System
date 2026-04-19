{-# LANGUAGE OverloadedStrings #-}

-- | Upstream readiness probes for the MCP server.
--
-- 'handleReady' runs behind the Servant @\/ready@ route and returns
-- 200 only when both @query-api@ and @interaction-api@ respond 2xx to an
-- unauthenticated @GET \/health@ within 'readinessTimeoutMs'; otherwise it
-- throws @err503@ with a body listing each upstream's outcome. Liveness
-- (@\/health@) stays on 'alwaysHealthy' — it must not depend on external
-- state.
module MCPServer.Readiness
  ( UpstreamCheck (..)
  , checkUpstreamReady
  , handleReady
  ) where

import Constants (appVersion)
import Control.Concurrent.Async (concurrently)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.List (dropWhileEnd)
import Data.Text (Text, pack)
import qualified Data.Text as T
import MCPServer.App (AppCtx (..))
import Network.HTTP.Client
  ( Manager
  , Request
  , ResponseTimeout
  , httpNoBody
  , parseRequest
  , responseStatus
  , responseTimeout
  , responseTimeoutMicro
  )
import Network.HTTP.Types.Status (statusCode, statusIsSuccessful)
import Servant (Handler, err503, errBody, throwError)
import Servant.Client (BaseUrl, showBaseUrl)
import WebAPI.ServiceProbe (ServiceProbeStatus, mkProbeStatus)

-- | Result of probing a single upstream.
data UpstreamCheck = UpstreamCheck
  { upstreamLabel :: Text
  -- ^ Short label used in the @\/ready@ body (e.g. @"query-api"@).
  , upstreamReady :: Bool
  , upstreamDetail :: Text
  -- ^ Human-readable status: @"ready"@, @"status 503"@, @"unreachable: …"@.
  }
  deriving (Show)

-- | Concurrently probe @queryBaseUrl\/health@ and @interactionBaseUrl\/health@
-- using the shared 'httpManager'. Bounded by 'readinessTimeoutMs' per
-- request, so the worst-case latency is @max(q, i)@ not @q + i@.
checkUpstreamReady :: AppCtx -> IO [UpstreamCheck]
checkUpstreamReady ctx = do
  let timeout = responseTimeoutMicro (readinessTimeoutMs ctx * 1000)
  (q, i) <-
    concurrently
      (probe (httpManager ctx) timeout "query-api" (queryBaseUrl ctx))
      (probe (httpManager ctx) timeout "interaction-api" (interactionBaseUrl ctx))
  pure [q, i]

-- | Issue an unauthenticated @GET \{base\}\/health@ and classify the outcome.
-- A 2xx response counts as ready; anything else (non-2xx, network failure,
-- timeout) counts as unready. Auth is intentionally omitted — @\/health@
-- on both upstreams is public.
probe :: Manager -> ResponseTimeout -> Text -> BaseUrl -> IO UpstreamCheck
probe mgr timeout label base = do
  let url = dropWhileEnd (== '/') (showBaseUrl base) <> "/health"
  result <- try $ do
    req0 <- parseRequest url
    let req = req0 {responseTimeout = timeout} :: Request
    resp <- httpNoBody req mgr
    pure (responseStatus resp)
  pure $ case result of
    Right s
      | statusIsSuccessful s ->
          UpstreamCheck label True "ready"
      | otherwise ->
          UpstreamCheck label False ("status " <> pack (show (statusCode s)))
    Left (e :: SomeException) ->
      UpstreamCheck label False ("unreachable: " <> httpExceptionShort e)
  where
    -- Keep the error string short — @show@ on 'HttpException' includes the
    -- full request record, which is noisy for a probe body. The first line
    -- of 'show' is stable enough across http-client versions.
    httpExceptionShort :: SomeException -> Text
    httpExceptionShort e = case T.lines (pack (show e)) of
      (first : _) -> first
      [] -> "unknown"

-- | Servant handler for @GET \/ready@. Returns a 'ServiceProbeStatus' with
-- the combined verdict in its @status@ field; throws @err503@ with a plain
-- body listing each upstream's outcome when at least one is unready.
handleReady :: AppCtx -> Handler (ServiceProbeStatus Text)
handleReady ctx = do
  checks <- liftIO (checkUpstreamReady ctx)
  let allReady = all upstreamReady checks
      summary =
        if allReady
          then "ready"
          else
            "unready: "
              <> T.intercalate ", " (fmap renderCheck (filter (not . upstreamReady) checks))
      renderCheck c = upstreamLabel c <> "=" <> upstreamDetail c
  status <- mkProbeStatus summary (pack appVersion) "bjj-mcp-server"
  if allReady
    then pure status
    else
      throwError
        err503
          { errBody = BL8.pack (T.unpack (renderBody checks))
          }
  where
    renderBody cs =
      T.unlines $
        "mcp-server: unready"
          : fmap (\c -> "  " <> upstreamLabel c <> ": " <> upstreamDetail c) cs
