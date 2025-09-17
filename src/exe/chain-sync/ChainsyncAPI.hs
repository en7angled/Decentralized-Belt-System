{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module ChainsyncAPI where

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

data SyncMetrics = SyncMetrics
  { smLocalTip :: Integer,
    smBlockchainTip :: Integer,
    smLastSyncTime :: UTCTime,
    smDbReady :: Bool,
    smMigrationsComplete :: Bool,
    smChainSyncState :: ChainSyncState
  }
  deriving (Generic, ToSchema, Show)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "sm", CamelToSnake]] SyncMetrics

type ServiceProbeAPI = WebAPI.ServiceProbe.ServiceProbe SyncMetrics SyncMetrics

proxyServiceProbeAPI :: Proxy ServiceProbeAPI
proxyServiceProbeAPI = Proxy

serviceProbeServer :: MVar SyncMetrics -> ServerT ServiceProbeAPI Handler
serviceProbeServer metricsVar = handleHealth' :<|> handleReady'
  where
    handleHealth' = do
      now <- liftIO getCurrentTime
      let ts = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
      m <- liftIO $ readMVar metricsVar
      let probeStatus =
            ServiceProbeStatus
              { status = m,
                service = "chain-sync",
                version = "1.0.0",
                timestamp = ts
              }
      return probeStatus

    handleReady' = do
      m <- liftIO $ readMVar metricsVar
      now <- liftIO getCurrentTime
      let chainsyncState = smChainSyncState m
      let ts = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
      let probeStatus =
            ServiceProbeStatus
              { status = m,
                service = "chain-sync",
                version = "1.0.0",
                timestamp = ts
              }

      case chainsyncState of
        UpToDate -> pure probeStatus
        Behind False -> pure probeStatus
        _ ->
          throwError
            err503
              { errBody =
                  BL8.pack (show probeStatus)
              }

mkServiceProbeApp :: MVar SyncMetrics -> Application
mkServiceProbeApp metricsVar =
  WebAPI.CORS.setupCors $
    serve proxyServiceProbeAPI (serviceProbeServer metricsVar)
