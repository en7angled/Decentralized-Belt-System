{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Application monad and context for the query API server.
-- Wraps a 'ReaderT' over Servant's 'Handler' with access to auth, provider,
-- database pool, and optional deployed-script references.
module QueryAppMonad where

import Constants qualified
import Control.Monad.Reader
import Data.Text hiding (elem, reverse, take)
import Data.Time
import Database.Persist.Sql (ConnectionPool, SqlPersistT, Single (..), rawSql, runSqlPool)
import Control.Exception (SomeException, displayException, try)
import Data.Typeable (cast)
import GeniusYield.TxBuilder.Errors (GYTxMonadException (GYApplicationException))
import Servant
-- import System.Directory.Extra
import TxBuilding.Context
import TxBuilding.Exceptions (TxBuildingException, txBuildingExceptionToHttpStatus)
import WebAPI.Auth (AuthContext)
import WebAPI.Errors (genericErrorMessage, mkServantErr)
import WebAPI.ServiceProbe (ServiceProbeStatus (..))

------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

-- | Shared environment for all query-API request handlers.
data QueryAppContext = QueryAppContext
  { authContext :: AuthContext, -- ^ Basic-auth credentials for protected routes
    providerContext :: ProviderCtx, -- ^ Cardano provider for live on-chain queries
    pgPool :: ConnectionPool, -- ^ PostgreSQL connection pool for projected data
    deployedScriptsCtx :: Maybe DeployedScriptsContext, -- ^ Deployed validator script references (when available)
    liveProjection :: Bool -- ^ Use live on-chain queries instead of projected data (default: False)
  }

newtype QueryAppMonad a = QueryAppMonad {unAppMonad :: ReaderT QueryAppContext Servant.Handler a}
  deriving (Functor, Applicative, Monad)

-- | Run a 'QueryAppMonad' computation with the given context.
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

-- | Run an IO action, mapping a bare 'TxBuildingException' to its HTTP status.
-- Query handlers throw @TxBuildingException@ directly (no @GYApplicationException@
-- wrapper), so this catches the concrete type — it must NOT copy the
-- interaction-api handler's @GYApplicationException@/@cast@ shape.
runWithQueryErrorHandling :: IO a -> QueryAppMonad a
runWithQueryErrorHandling action = QueryAppMonad $ do
  res <- liftIO $ try action
  case res of
    Left (txEx :: TxBuildingException) -> do
      let status = txBuildingExceptionToHttpStatus txEx
      liftIO $ putStrLn $ "TxBuildingException (" <> show status <> "): " <> displayException txEx
      throwError $ mkServantErr status (displayException txEx)
    Right ok -> pure ok

-- | Run an IO action, mapping a 'TxBuildingException' that is wrapped in a
-- 'GYApplicationException' to its HTTP status. The live query backend runs inside
-- @GYTxQueryMonadIO@, whose @throwError = ioToQueryMonad . throwIO@ wraps thrown
-- @TxBuildingException@s in @GYApplicationException@ — so the bare-type
-- 'runWithQueryErrorHandling' cannot catch them. This mirrors interaction-api's
-- @runWithTxErrorHandling@ catch shape. Any other exception maps to a sanitized 500.
runWithQueryErrorHandlingWrapped :: IO a -> QueryAppMonad a
runWithQueryErrorHandlingWrapped action = QueryAppMonad $ do
  res <- liftIO $ try @GYTxMonadException action
  case res of
    Left ex ->
      case ex of
        GYApplicationException appE
          | Just txEx <- cast appE -> do
              let status = txBuildingExceptionToHttpStatus txEx
              let msg = displayException txEx
              liftIO $ putStrLn $ "TxBuildingException (" <> show status <> "): " <> msg
              throwError $ mkServantErr status msg
        _ -> do
          liftIO $ putStrLn $ "Unexpected exception: " <> show ex
          throwError $ mkServantErr 500 (genericErrorMessage 500)
    Right ok -> pure ok

-- | Health-check probe: verify the projection database is reachable.
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
    Left err -> do
      liftIO $ putStrLn $ "Projection DB not ready: " <> show err
      throwError $ mkServantErr 503 (genericErrorMessage 503)
