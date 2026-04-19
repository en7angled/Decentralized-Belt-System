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

-- | Build a 'ServiceProbeStatus' with the given status text, service name, and current timestamp.
mkProbeStatus :: (MonadIO m) => Text -> Text -> Text -> m (ServiceProbeStatus Text)
mkProbeStatus statusText versionText serviceName = do
  now <- liftIO getCurrentTime
  return $
    ServiceProbeStatus
      { status = statusText,
        service = serviceName,
        version = versionText,
        timestamp = pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
      }

-- | Always-healthy probe handler.
alwaysHealthy :: (MonadIO m) => Text -> Text -> m (ServiceProbeStatus Text)
alwaysHealthy = mkProbeStatus "healthy"

-- | Always-ready probe handler.
alwaysReady :: (MonadIO m) => Text -> Text -> m (ServiceProbeStatus Text)
alwaysReady = mkProbeStatus "ready"
