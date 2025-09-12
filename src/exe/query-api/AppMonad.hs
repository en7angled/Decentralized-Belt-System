{-# LANGUAGE RecordWildCards #-}

module AppMonad where

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
import qualified Query.Common as QC
import Servant
import Storage
import TxBuilding.Context
import TxBuilding.Interactions (Interaction)
import TxBuilding.Lookups
import TxBuilding.Transactions (interactionToHexEncodedCBOR, submitTx)
import Types

------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

data AuthContext = AuthContext
  { authUser :: Text,
    authPassword :: Text
  }
  deriving (Eq, Show)

data AppContext = AppContext
  { authContext :: AuthContext,
    txBuildingContext :: TxBuildingContext,
    projectionDbPath :: Text
  }

newtype AppMonad a = AppMonad {unAppMonad :: ReaderT AppContext Servant.Handler a}
  deriving (Functor, Applicative, Monad)

runAppMonad :: AppContext -> AppMonad a -> Servant.Handler a
runAppMonad ctx app = runReaderT (unAppMonad app) ctx

instance MonadIO AppMonad where
  liftIO :: IO a -> AppMonad a
  liftIO = AppMonad . liftIO

instance MonadReader AppContext AppMonad where
  ask :: AppMonad AppContext
  ask = AppMonad ask

  local :: (AppContext -> AppContext) -> AppMonad a -> AppMonad a
  local f (AppMonad app) = AppMonad (local f app)

buildInteractionApp :: Interaction -> AppMonad String
buildInteractionApp inter = do
  -- Lift into the TxBuildingContext reader
  AppMonad $ do
    AppContext {..} <- ask
    res <- liftIO $ try (runReaderT (interactionToHexEncodedCBOR inter) txBuildingContext)
    case res of
      Left (e :: GYTxMonadException) -> do
        liftIO $ putStrLn $ "GYTxMonadException: \n" <> show e
        throwError err400 {errBody = BL8.pack (show e)}
      Right ok -> pure ok

submitTxApp :: GYTx -> AppMonad GYTxId
submitTxApp tx = do
  AppMonad $ do
    AppContext {..} <- ask
    res <- liftIO $ try (runReaderT (submitTx tx) txBuildingContext)
    case res of
      Left (e :: GYTxMonadException) -> do
        liftIO $ putStrLn $ "GYTxMonadException: \n" <> show e
        throwError err400 {errBody = BL8.pack (show e)}
      Right ok -> pure ok
