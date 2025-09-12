{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module WebAPI.Health where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text, pack)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import GHC.Generics (Generic)
import Servant

-- Health check data type
data HealthStatus = HealthStatus
  { status :: Text,
    service :: Text,
    version :: Text,
    timestamp :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type Health =
  -- Health check endpoint
  ( Summary "Health Check"
      :> Description "Returns the health status of the service"
      :> "health"
      :> Get '[JSON] HealthStatus
  )
    :<|>
    -- Readiness check endpoint
    ( Summary "Readiness Check"
        :> Description "Returns the readiness status of the service"
        :> "ready"
        :> Get '[JSON] HealthStatus
    )

-- Generic health handlers that take service name as parameter
handleHealth :: MonadIO m => Text -> m HealthStatus
handleHealth serviceName = do
  now <- liftIO getCurrentTime
  return $ HealthStatus
    { status = "healthy",
      service = serviceName,
      version = "1.0.0",
      timestamp = pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
    }

handleReady :: MonadIO m => Text -> m HealthStatus
handleReady serviceName = do
  -- Check if the service is ready to accept requests
  -- This could check:
  -- - Database connectivity
  -- - External service dependencies
  -- - Configuration validity
  now <- liftIO getCurrentTime
  return $ HealthStatus
    { status = "ready",
      service = serviceName,
      version = "1.0.0",
      timestamp = pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
    }

healthServer :: MonadIO m => Text -> ServerT Health m
healthServer serviceName = handleHealth serviceName :<|> handleReady serviceName
