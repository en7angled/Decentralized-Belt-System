{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module InteractionAppMonad where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Text hiding (elem, reverse, take)
import Data.Time (defaultTimeLocale, getCurrentTime)
import Data.Time.Format (formatTime)
import GeniusYield.TxBuilder.Errors (GYTxMonadException)
import GeniusYield.Types
import Servant
import TxBuilding.Context
import TxBuilding.Interactions (Interaction)
import TxBuilding.Operations (verifyDeployedScriptsAreReady)
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

buildInteractionApp :: Interaction -> InteractionAppMonad String
buildInteractionApp inter = do
  -- Lift into the TxBuildingContext reader
  InteractionAppMonad $ do
    InteractionAppContext {..} <- ask
    res <- liftIO $ try (runReaderT (interactionToHexEncodedCBOR inter) txBuildingContext)
    case res of
      Left (e :: GYTxMonadException) -> do
        liftIO $ putStrLn $ "GYTxMonadException: \n" <> show e
        throwError err400 {errBody = BL8.pack (show e)}
      Right ok -> pure ok

submitTxApp :: GYTx -> InteractionAppMonad GYTxId
submitTxApp tx = do
  InteractionAppMonad $ do
    InteractionAppContext {..} <- ask
    res <- liftIO $ try (runReaderT (submitTx tx) txBuildingContext)
    case res of
      Left (e :: GYTxMonadException) -> do
        liftIO $ putStrLn $ "GYTxMonadException: \n" <> show e
        throwError err400 {errBody = BL8.pack (show e)}
      Right ok -> pure ok

checkDeployedScriptsAreReady :: InteractionAppMonad (ServiceProbeStatus Text)
checkDeployedScriptsAreReady = do
  InteractionAppMonad $ do
    InteractionAppContext {..} <- ask
    let queryDeploydScripts = runReaderT verifyDeployedScriptsAreReady (deployedScriptsCtx txBuildingContext)
    res <- liftIO $ runQuery (providerCtx txBuildingContext) queryDeploydScripts
    now <- liftIO getCurrentTime
    if res
      then do
        return $
          ServiceProbeStatus
            { status = "ready" :: Text,
              service = "interaction-api" :: Text,
              version = "1.0.0",
              timestamp = pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
            }
      else
        throwError
          err503
            { errBody =
                BL8.pack $
                  show
                    ServiceProbeStatus
                      { status = "readiness timeout, deployed scripts are not found" :: Text,
                        service = "interaction-api",
                        version = "1.0.0",
                        timestamp = pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
                      }
            }
