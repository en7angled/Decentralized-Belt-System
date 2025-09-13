{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module ChainsyncAPI where

import Control.Concurrent.MVar (MVar, readMVar)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Swagger (ToSchema)
import qualified Data.Text as T
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Deriving.Aeson
import GHC.Generics (Generic)
import KupoClient (runKupoHealth)
import Network.Wai (Application)
import Servant
import qualified WebAPI.CORS
import WebAPI.ServiceProbe
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
  deriving (Generic, ToSchema, Show)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "sm", CamelToSnake]] SyncMetrics

type ServiceProbeAPI = WebAPI.ServiceProbe.ServiceProbe SyncMetrics SyncMetrics

proxyServiceProbeAPI :: Proxy ServiceProbeAPI
proxyServiceProbeAPI = Proxy

serviceProbeServer :: String -> MVar SyncMetrics -> ServerT ServiceProbeAPI Handler
serviceProbeServer kupoUrl metricsVar = handleHealth' :<|> handleReady'
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

      -- Verify upstream Kupo health; if unhealthy, mark this service as unhealthy
      eHealth <- liftIO $ runKupoHealth kupoUrl
      case eHealth of
        Left _ ->
          throwError
            err503
              { errBody =
                  BL8.pack (show probeStatus)
              }
        Right _ -> pure probeStatus

    handleReady' = do
      handleHealth'
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
        _ ->
          throwError
            err503
              { errBody =
                  BL8.pack (show probeStatus)
              }

mkServiceProbeApp :: String -> MVar SyncMetrics -> Application
mkServiceProbeApp kupoUrl metricsVar =
  WebAPI.CORS.setupCors $
    serve proxyServiceProbeAPI (serviceProbeServer kupoUrl metricsVar)
