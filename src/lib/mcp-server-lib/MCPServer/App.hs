{-# LANGUAGE OverloadedStrings #-}

-- | Runtime configuration for the MCP server: upstream API base URLs,
-- BasicAuth credentials, shared HTTP manager, listen port, and the write-tool
-- feature flag. 'withAppCtx' reads env vars and provides an 'AppCtx' to a
-- continuation.
module MCPServer.App
  ( AppCtx (..)
  , withAppCtx
  ) where

import Data.Maybe (fromMaybe)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant (BasicAuthData (..))
import Servant.Client (BaseUrl, parseBaseUrl)
import System.Environment (lookupEnv)
import WebAPI.Auth (AuthContext (..), getBasicAuthFromEnv)
import WebAPI.Utils (getPortFromEnvOrDefault)

data AppCtx = AppCtx
  { queryBaseUrl :: BaseUrl
  , interactionBaseUrl :: BaseUrl
  , upstreamAuth :: BasicAuthData
  , httpManager :: Manager
  , port :: Int
  , enableWriteTx :: Bool
  , readinessTimeoutMs :: Int
  -- ^ Per-upstream timeout for the @\/ready@ probe, in milliseconds. Override
  -- via @MCP_READINESS_TIMEOUT_MS@; default 2000.
  }

withAppCtx :: (AppCtx -> IO a) -> IO a
withAppCtx k = do
  qRaw <- fromMaybe "http://query-api:8083" <$> lookupEnv "QUERY_API_URL"
  iRaw <- fromMaybe "http://interaction-api:8082" <$> lookupEnv "INTERACTION_API_URL"
  qUrl <- parseBaseUrl qRaw
  iUrl <- parseBaseUrl iRaw
  authCtx <- getBasicAuthFromEnv
  let auth =
        BasicAuthData
          (TE.encodeUtf8 (authUser authCtx))
          (TE.encodeUtf8 (authPassword authCtx))
  mgr <- newManager tlsManagerSettings
  p <- getPortFromEnvOrDefault 8085
  writeTx <- (Just "1" ==) <$> lookupEnv "MCP_ENABLE_WRITE_TX"
  readyTimeoutMs <- readIntEnv "MCP_READINESS_TIMEOUT_MS" 2000
  k
    AppCtx
      { queryBaseUrl = qUrl
      , interactionBaseUrl = iUrl
      , upstreamAuth = auth
      , httpManager = mgr
      , port = p
      , enableWriteTx = writeTx
      , readinessTimeoutMs = readyTimeoutMs
      }

-- | Read an @Int@-valued env var with a default fallback. Non-numeric values
-- silently fall back to the default so the server still starts.
readIntEnv :: String -> Int -> IO Int
readIntEnv name fallback = do
  mv <- lookupEnv name
  pure $ fromMaybe fallback (mv >>= readInt)
  where
    readInt s = case reads s of
      [(n, "")] -> Just n
      _ -> Nothing
