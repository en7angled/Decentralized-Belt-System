{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module WebAPI.ServiceProbe where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text, pack)
import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import GHC.Generics (Generic)
import Servant

-- Health check data type
data ServiceProbeStatus a = ServiceProbeStatus
  { status :: a,
    service :: Text,
    version :: Text,
    timestamp :: Text
  }
  deriving (Generic, Show)

instance (ToJSON a) => ToJSON (ServiceProbeStatus a)

instance (FromJSON a) => FromJSON (ServiceProbeStatus a)

instance (ToSchema a) => ToSchema (ServiceProbeStatus a)

type ServiceProbe h r =
  -- Health check endpoint
  ( Summary "Health Check"
      :> Description "Returns the health status of the service"
      :> "health"
      :> Get '[JSON] (ServiceProbeStatus h)
  )
    :<|>
    -- Readiness check endpoint
    ( Summary "Readiness Check"
        :> Description "Returns the readiness status of the service"
        :> "ready"
        :> Get '[JSON] (ServiceProbeStatus r)
    )

-- Generic health handlers that take service name as parameter
alwaysHealthy :: (MonadIO m) => Text -> m (ServiceProbeStatus Text)
alwaysHealthy serviceName = do
  now <- liftIO getCurrentTime
  return $
    ServiceProbeStatus
      { status = "healthy",
        service = serviceName,
        version = "1.0.0",
        timestamp = pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
      }

alwaysReady :: (MonadIO m) => Text -> m (ServiceProbeStatus Text)
alwaysReady serviceName = do
  -- Check if the service is ready to accept requests
  -- This could check:
  -- - Database connectivity
  -- - External service dependencies
  -- - Configuration validity
  now <- liftIO getCurrentTime
  return $
    ServiceProbeStatus
      { status = "ready",
        service = serviceName,
        version = "1.0.0",
        timestamp = pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
      }
