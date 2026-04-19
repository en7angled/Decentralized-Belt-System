{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Health and readiness probe API for the chain-sync service.
-- Exposes sync metrics so orchestrators can determine whether the
-- service is caught up with the blockchain tip.
module ChainsyncAPI where

import Constants (appVersion)
import Control.Concurrent.MVar (MVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Swagger (ToSchema)
import Data.Text qualified as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Deriving.Aeson
import Servant
import WebAPI.CORS qualified
import WebAPI.ServiceProbe

data ChainSyncState = UpToDate | UpToDateButDifferentBlockHash | Behind {isWayBehind :: Bool}| Ahead
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | Snapshot of chain-sync progress, returned by health/readiness probes.
data SyncMetrics = SyncMetrics
  { smLocalTip :: Integer, -- ^ Slot number of the last processed checkpoint
    smBlockchainTip :: Integer, -- ^ Slot number of the latest known blockchain tip
    smLastSyncTime :: UTCTime, -- ^ Wall-clock time of the last successful sync step
    smDbReady :: Bool, -- ^ Whether the projection database connection is healthy
    smMigrationsComplete :: Bool, -- ^ Whether all DB migrations have been applied
    smChainSyncState :: ChainSyncState -- ^ Derived sync state (UpToDate, Behind, etc.)
  }
  deriving (Generic, ToSchema, Show)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "sm", CamelToSnake]] SyncMetrics

type ServiceProbeAPI = WebAPI.ServiceProbe.ServiceProbe SyncMetrics SyncMetrics

proxyServiceProbeAPI :: Proxy ServiceProbeAPI
proxyServiceProbeAPI = Proxy

-- | Servant server for the health and readiness endpoints.
serviceProbeServer :: MVar SyncMetrics -> ServerT ServiceProbeAPI Handler
serviceProbeServer metricsVar = handleHealth' :<|> handleReady'
  where
    mkProbe m = do
      now <- liftIO getCurrentTime
      let ts = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
      return ServiceProbeStatus {status = m, service = "chain-sync", version = T.pack appVersion, timestamp = ts}

    handleHealth' = do
      m <- liftIO $ readMVar metricsVar
      mkProbe m

    handleReady' = do
      m <- liftIO $ readMVar metricsVar
      probeStatus <- mkProbe m
      case smChainSyncState m of
        UpToDate -> pure probeStatus
        Behind False -> pure probeStatus
        _ -> throwError err503 {errBody = BL8.pack (show probeStatus)}

-- | Build a WAI 'Application' that serves the chain-sync probe endpoints with CORS.
mkServiceProbeApp :: MVar SyncMetrics -> Application
mkServiceProbeApp metricsVar =
  WebAPI.CORS.setupCors $
    serve proxyServiceProbeAPI (serviceProbeServer metricsVar)
