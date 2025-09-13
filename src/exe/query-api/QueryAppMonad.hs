{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module QueryAppMonad where

import Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Text hiding (elem, reverse, take)
import Data.Time
import Servant
import System.Directory.Extra
import TxBuilding.Context
import WebAPI.Auth (AuthContext)
import WebAPI.ServiceProbe (ServiceProbeStatus (..))

------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

data QueryAppContext = QueryAppContext
  { authContext :: AuthContext,
    providerContext :: ProviderCtx,
    projectionDbPath :: Text
  }

newtype QueryAppMonad a = QueryAppMonad {unAppMonad :: ReaderT QueryAppContext Servant.Handler a}
  deriving (Functor, Applicative, Monad)

runAppMonad :: QueryAppContext -> QueryAppMonad a -> Servant.Handler a
runAppMonad ctx app = runReaderT (unAppMonad app) ctx

instance MonadIO QueryAppMonad where
  liftIO :: IO a -> QueryAppMonad a
  liftIO = QueryAppMonad . liftIO

instance MonadReader QueryAppContext QueryAppMonad where
  ask :: QueryAppMonad QueryAppContext
  ask = QueryAppMonad ask

  local :: (QueryAppContext -> QueryAppContext) -> QueryAppMonad a -> QueryAppMonad a
  local f (QueryAppMonad app) = QueryAppMonad (local f app)

verifyProjectionDbConnection :: QueryAppMonad (ServiceProbeStatus Text)
verifyProjectionDbConnection = QueryAppMonad $ do
  QueryAppContext {..} <- ask
  dbExists <- liftIO $ doesFileExist (unpack projectionDbPath)
  now <- liftIO getCurrentTime

  if dbExists
    then
      return
        ServiceProbeStatus
          { status = "ready" :: Text,
            service = "query-api",
            version = "1.0.0",
            timestamp = pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
          }
    else
      throwError
        err503
          { errBody =
              BL8.pack $
                show $
                  ServiceProbeStatus
                    { status = "readiness timeout, projection database not found" :: Text,
                      service = "query-api",
                      version = "1.0.0",
                      timestamp = pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
                    }
          }
