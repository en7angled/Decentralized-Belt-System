{-# LANGUAGE RecordWildCards #-}

module AppMonad where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Data.List (sortOn)
import Data.Maybe
import Data.MultiSet
import Data.Ord (Down (..))
import Data.Text hiding (elem, take, reverse)
import qualified Data.Text.Encoding
import DomainTypes.Core.Types
import DomainTypes.Transfer.Types
import GeniusYield.GYConfig (GYCoreConfig (..))
import GeniusYield.TxBuilder (GYTxQueryMonad)
import GeniusYield.Types
import Onchain.BJJ
import Servant
import TxBuilding.Context
import TxBuilding.Lookups
import TxBuilding.Transactions (submitTx, interactionToHexEncodedCBOR)
import TxBuilding.Interactions (Interaction)
import Types
import qualified Query.Common as QC
import qualified Data.Text as Text
import qualified Data.Text as T
import qualified Database.Persist as P
import Database.Persist (Entity (..), entityVal)
import Database.Persist.Sqlite (runSqlite)
import Database.Esqueleto.Experimental
import Storage



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

instance MonadReader TxBuildingContext AppMonad where
  ask :: AppMonad TxBuildingContext
  ask = AppMonad (asks txBuildingContext)

  local :: (TxBuildingContext -> TxBuildingContext) -> AppMonad a -> AppMonad a
  local f (AppMonad app) =
    let f' :: AppContext -> AppContext
        f' AppContext {authContext, txBuildingContext, projectionDbPath} = AppContext {authContext, txBuildingContext = f txBuildingContext, projectionDbPath}
     in AppMonad (local f' app)

instance QC.HasProjectionDB AppContext where
  getProjectionDbPath :: AppContext -> Text
  getProjectionDbPath = projectionDbPath

-- Removed legacy instances in favor of constrained functions above

-- Wrappers to run TxBuilding actions that require MonadReader TxBuildingContext

buildInteractionApp :: Interaction -> AppMonad String
buildInteractionApp inter = do
  -- Lift into the TxBuildingContext reader
  AppMonad $ do
    AppContext {..} <- ask
    runReaderT (interactionToHexEncodedCBOR inter) txBuildingContext

submitTxApp :: GYTx -> AppMonad GYTxId
submitTxApp tx = do
  AppMonad $ do
    AppContext {..} <- ask
    runReaderT (submitTx tx) txBuildingContext
