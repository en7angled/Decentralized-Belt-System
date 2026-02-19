{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module QueryAppMonad where

import Constants qualified
import Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Text hiding (elem, reverse, take)
import Data.Time
import Database.Persist.Sql (ConnectionPool, SqlPersistT, Single (..), rawSql, runSqlPool)
import Control.Exception (SomeException, try)
import Servant
-- import System.Directory.Extra
import TxBuilding.Context
import WebAPI.Auth (AuthContext)
import WebAPI.ServiceProbe (ServiceProbeStatus (..))

------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

data QueryAppContext = QueryAppContext
  { authContext :: AuthContext,
    providerContext :: ProviderCtx,
    pgPool :: ConnectionPool
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
  now <- liftIO getCurrentTime
  e <- liftIO (try (runSqlPool (rawSql "SELECT 1" [] :: SqlPersistT IO [Single Int]) pgPool) :: IO (Either SomeException [Single Int]))
  case e of
    Right _ ->
      return
        ServiceProbeStatus
          { status = "ready" :: Text,
            service = "query-api",
            version = pack Constants.appVersion,
            timestamp = pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
          }
    Left err ->
      throwError
        err503
          { errBody =
              BL8.pack $
                show $
                  ServiceProbeStatus
                    { status = "db not ready: " <> pack (show err),
                      service = "query-api",
                      version = pack Constants.appVersion,
                      timestamp = pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
                    }
          }
