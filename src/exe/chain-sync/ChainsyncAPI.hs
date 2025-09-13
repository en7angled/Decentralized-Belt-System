{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module ChainsyncAPI where

import Control.Concurrent.MVar (MVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Deriving.Aeson
import GHC.Generics (Generic)
import Network.Wai (Application)
import Servant
import qualified WebAPI.CORS
import qualified WebAPI.ServiceProbe

data ChainSyncState = UpToDate | UpToDateButDifferentBlockHash | Behind | Ahead
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data SyncMetrics = SyncMetrics
  { smLocalTip :: Integer,
    smBlockchainTip :: Integer,
    smLastSyncTime :: UTCTime,
    smDbReady :: Bool,
    smMigrationsComplete :: Bool,
    smChainSyncState :: ChainSyncState
  }
  deriving (Generic, ToSchema)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "sm", CamelToSnake]] SyncMetrics

type ServiceProbeAPI = WebAPI.ServiceProbe.ServiceProbe SyncMetrics T.Text

proxyServiceProbeAPI :: Proxy ServiceProbeAPI
proxyServiceProbeAPI = Proxy

serviceProbeServer :: MVar SyncMetrics -> ServerT ServiceProbeAPI Handler
serviceProbeServer metricsVar = handleHealth' :<|> handleReady'
  where
    handleHealth' = do
      m <- liftIO $ readMVar metricsVar
      now <- liftIO getCurrentTime
      let _lag = smBlockchainTip m - smLocalTip m
      let ts = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
      pure $
        WebAPI.ServiceProbe.ServiceProbeStatus
          { WebAPI.ServiceProbe.status = m,
            WebAPI.ServiceProbe.service = "chain-sync",
            WebAPI.ServiceProbe.version = "1.0.0",
            WebAPI.ServiceProbe.timestamp = ts
          }

    handleReady' = do
      m <- liftIO $ readMVar metricsVar
      now <- liftIO getCurrentTime
      let ready = smDbReady m && smMigrationsComplete m
      let ts = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
      pure $
        WebAPI.ServiceProbe.ServiceProbeStatus
          { WebAPI.ServiceProbe.status = if ready then "ready" else "not-ready",
            WebAPI.ServiceProbe.service = "chain-sync",
            WebAPI.ServiceProbe.version = "1.0.0",
            WebAPI.ServiceProbe.timestamp = ts
          }

mkServiceProbeApp :: MVar SyncMetrics -> Application
mkServiceProbeApp metricsVar =
  WebAPI.CORS.setupCors $
    serve proxyServiceProbeAPI (serviceProbeServer metricsVar)
