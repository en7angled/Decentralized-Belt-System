{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module InteractionAppMonad where

import Constants qualified
import Control.Exception (displayException, try)
import Control.Monad.Except
import Control.Monad.Reader
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Text hiding (elem, reverse, take)
import Data.Time (defaultTimeLocale, getCurrentTime)
import Data.Time.Format (formatTime)
import Data.Typeable (cast)
import GeniusYield.TxBuilder.Errors (GYTxMonadException (GYApplicationException))
import GeniusYield.Types
import Servant
import TxBuilding.Context
import TxBuilding.Exceptions (txBuildingExceptionToHttpStatus)
import TxBuilding.Interactions (Interaction)
import TxBuilding.Operations (ensureDeployedScriptsAreReady)
import TxBuilding.Transactions (interactionToHexEncodedCBOR, submitTx)
import WebAPI.Auth (AuthContext)
import WebAPI.ServiceProbe (ServiceProbeStatus (..))

------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

data InteractionAppContext = InteractionAppContext
  { authContext :: AuthContext,
    txBuildingContext :: TxBuildingContext
  }

newtype InteractionAppMonad a = InteractionAppMonad {unInteractionAppMonad :: ReaderT InteractionAppContext Servant.Handler a}
  deriving (Functor, Applicative, Monad)

runInteractionAppMonad :: InteractionAppContext -> InteractionAppMonad a -> Servant.Handler a
runInteractionAppMonad ctx app = runReaderT (unInteractionAppMonad app) ctx

instance MonadIO InteractionAppMonad where
  liftIO :: IO a -> InteractionAppMonad a
  liftIO = InteractionAppMonad . liftIO

instance MonadReader InteractionAppContext InteractionAppMonad where
  ask :: InteractionAppMonad InteractionAppContext
  ask = InteractionAppMonad ask

  local :: (InteractionAppContext -> InteractionAppContext) -> InteractionAppMonad a -> InteractionAppMonad a
  local f (InteractionAppMonad app) = InteractionAppMonad (local f app)

-- | Convert an HTTP status code to the corresponding Servant error.
mkServantErr :: Int -> String -> ServerError
mkServantErr 404 msg = err404 {errBody = BL8.pack msg}
mkServantErr 503 msg = err503 {errBody = BL8.pack msg}
mkServantErr _ msg = err400 {errBody = BL8.pack msg}

-- | Run an IO action in the TxBuilding context with standardized error handling.
-- Maps 'TxBuildingException' constructors to appropriate HTTP status codes
-- using the centralized 'txBuildingExceptionToHttpStatus' mapping.
runWithTxErrorHandling :: IO a -> InteractionAppMonad a
runWithTxErrorHandling action = InteractionAppMonad $ do
  res <- liftIO $ try action
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
          liftIO $ putStrLn $ "GYTxMonadException: \n" <> show ex
          throwError err400 {errBody = BL8.pack (show ex)}
    Right ok -> pure ok

buildInteractionApp :: Interaction -> InteractionAppMonad String
buildInteractionApp inter = do
  InteractionAppContext {..} <- ask
  runWithTxErrorHandling $ runReaderT (interactionToHexEncodedCBOR inter) txBuildingContext

submitTxApp :: GYTx -> InteractionAppMonad GYTxId
submitTxApp tx = do
  InteractionAppContext {..} <- ask
  runWithTxErrorHandling $ runReaderT (submitTx tx) txBuildingContext

checkDeployedScriptsAreReady :: InteractionAppMonad (ServiceProbeStatus Text)
checkDeployedScriptsAreReady = do
  InteractionAppMonad $ do
    InteractionAppContext {..} <- ask
    let queryScriptsReady = runReaderT ensureDeployedScriptsAreReady (deployedScriptsCtx txBuildingContext)
    res <- liftIO $ try $ runQuery (providerCtx txBuildingContext) queryScriptsReady
    now <- liftIO getCurrentTime
    case res of
      Left ex ->
        case ex of
          GYApplicationException appE
            | Just txEx <- cast appE -> do
                let status = txBuildingExceptionToHttpStatus txEx
                let msg = displayException txEx
                liftIO $ putStrLn $ "checkDeployedScriptsAreReady TxBuildingException (" <> show status <> "): " <> msg
                throwError $ mkServantErr status msg
          _ -> do
            liftIO $ putStrLn $ "checkDeployedScriptsAreReady GYTxMonadException: " <> show ex
            throwError err400 {errBody = BL8.pack (show ex)}
      Right () ->
        return $
          ServiceProbeStatus
            { status = "ready" :: Text,
              service = "interaction-api" :: Text,
              version = pack Constants.appVersion,
              timestamp = pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
            }
