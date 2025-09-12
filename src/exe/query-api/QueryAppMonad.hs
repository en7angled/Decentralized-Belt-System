{-# LANGUAGE RecordWildCards #-}

module QueryAppMonad where

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
import WebAPI.Auth (AuthContext)

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
