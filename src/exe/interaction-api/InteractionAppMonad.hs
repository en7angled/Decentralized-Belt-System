{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module InteractionAppMonad where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.List (sortOn)
import Data.Maybe
import Data.MultiSet
import Data.Ord (Down (..))
import Data.Text hiding (elem, reverse, take)
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import Data.Time (defaultTimeLocale, getCurrentTime)
import Data.Time.Format (formatTime)
import Database.Esqueleto.Experimental
import Database.Persist (Entity (..), entityVal)
import qualified Database.Persist as P
import Database.Persist.Sqlite (runSqlite)
import DomainTypes.Core.Types
import DomainTypes.Transfer.Types
import GeniusYield.GYConfig (GYCoreConfig (..))
import GeniusYield.TxBuilder (GYTxQueryMonad)
import GeniusYield.TxBuilder.Errors (GYTxMonadException)
import GeniusYield.Types
import Onchain.BJJ
import Servant
import TxBuilding.Context
import TxBuilding.Interactions (Interaction)
import TxBuilding.Lookups
import TxBuilding.Operations (verifyDeployedScriptsAreReady)
import TxBuilding.Transactions (interactionToHexEncodedCBOR, submitTx)
import Types
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
            { status = "ready",
              service = "interaction-api",
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
                      { status = "readiness timeout, deployed scripts are not found",
                        service = "interaction-api",
                        version = "1.0.0",
                        timestamp = pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now
                      }
            }
