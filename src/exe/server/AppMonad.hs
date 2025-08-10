{-# LANGUAGE RecordWildCards #-}

module AppMonad where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Data.Maybe
import Data.Text hiding (elem)
import qualified Data.Text.Encoding
import DomainTypes.Profile.Types
import GeniusYield.GYConfig (GYCoreConfig (..))
import GeniusYield.TxBuilder (GYTxQueryMonad)
import GeniusYield.Types
import Onchain.BJJ
import Servant
import TxBuilding.Context
import TxBuilding.Lookups
import Data.MultiSet

------------------------------------------------------------------------------------------------

-- * Capabilities

------------------------------------------------------------------------------------------------

class ProfilesQueryMonad m where
  getPractitionerProfile :: ProfileRefAC -> m PractitionerProfileInformation
  getOrganizationProfile :: ProfileRefAC -> m OrganizationProfileInformation

class ProfilesStatsQueryMonad m where
  getProfilesCount :: Maybe ProfileFilter -> m Int

type Limit = Int

type Offset = Int

data ProfileFilter = ProfileFilter
  { profileFilterType :: Maybe [ProfileType],
    profileFilterDateInterval :: (Maybe GYTime, Maybe GYTime)
  }

data PromotionFilter = PromotionFilter
  { promotionFilterId :: Maybe [ProfileRefAC],
    promotionFilterBelt :: Maybe [BJJBelt],
    promotionFilterAchievedByProfileId :: Maybe [ProfileRefAC],
    promotionFilterAwardedByProfileId :: Maybe [ProfileRefAC],
    promotionFilterAchievementDateInterval :: (Maybe GYTime, Maybe GYTime)
  }

class PromotionsStatsQueryMonad m where
  getPromotions :: Maybe (Limit, Offset) -> Maybe PromotionFilter -> m [PromotionInformation]
  getPromotionsCount :: Maybe PromotionFilter -> m Int

data RankFilter = RankFilter
  { rankFilterId :: Maybe [RankAC],
    rankFilterBelt :: Maybe [BJJBelt],
    rankFilterAchievedByProfileId :: Maybe [ProfileRefAC],
    rankFilterAwardedByProfileId :: Maybe [ProfileRefAC],
    rankFilterAchievementDateInterval :: (Maybe GYTime, Maybe GYTime)
  }

class RanksStatsQueryMonad m where
  getRanks :: Maybe (Limit, Offset) -> Maybe RankFilter -> m [RankInformation]
  getRanksCount :: Maybe RankFilter -> m Int
  getBeltTotals :: m [(BJJBelt, Int)]

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
    txBuildingContext :: TxBuildingContext
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
        f' AppContext {authContext, txBuildingContext} = AppContext {authContext, txBuildingContext = f txBuildingContext}
     in AppMonad (local f' app)

instance ProfilesQueryMonad AppMonad where
  getPractitionerProfile :: ProfileRefAC -> AppMonad PractitionerProfileInformation
  getPractitionerProfile profileRefAC = do
    TxBuildingContext {..} <- ask
    liftIO $ runQuery providerCtx $ getPractiotionerInformation profileRefAC

  getOrganizationProfile :: ProfileRefAC -> AppMonad OrganizationProfileInformation
  getOrganizationProfile profileRefAC = do
    TxBuildingContext {..} <- ask
    liftIO $ runQuery providerCtx $ getOrganizationInformation profileRefAC

instance ProfilesStatsQueryMonad AppMonad where
  getProfilesCount :: Maybe ProfileFilter -> AppMonad Int
  getProfilesCount maybeProfileFilter = do
    TxBuildingContext {..} <- ask
    allProfiles <- liftIO $ runQuery providerCtx (getAllProfiles (cfgNetworkId . ctxCoreCfg $ providerCtx))
    return $ Prelude.length $ applyProfileFilter maybeProfileFilter allProfiles
    where
      applyProfileFilter :: Maybe ProfileFilter -> [ProfileInformation] -> [ProfileInformation]
      applyProfileFilter Nothing profiles = profiles
      applyProfileFilter (Just ProfileFilter {..}) profiles =
        let typeFilter = case profileFilterType of
              Just types -> Prelude.filter (\profile -> profileType profile `elem` types)
              Nothing -> id
            dateFilter = case profileFilterDateInterval of
              (Just from, Just to) -> Prelude.filter (\profile -> profileCreationDate profile >= from && profileCreationDate profile <= to)
              (Nothing, Just to) -> Prelude.filter (\profile -> profileCreationDate profile <= to)
              (Just from, Nothing) -> Prelude.filter (\profile -> profileCreationDate profile >= from)
              (Nothing, Nothing) -> id
         in typeFilter . dateFilter $ profiles

instance PromotionsStatsQueryMonad AppMonad where
  getPromotions :: Maybe (Limit, Offset) -> Maybe PromotionFilter -> AppMonad [PromotionInformation]
  getPromotions maybeLimitOffset maybePromotionFilter = do
    TxBuildingContext {..} <- ask
    allPromotions <- liftIO $ runQuery providerCtx (getAllPromotions (cfgNetworkId . ctxCoreCfg $ providerCtx))
    return $ applyLimits maybeLimitOffset $ applyPromotionFilter maybePromotionFilter allPromotions
    where
      applyLimits :: Maybe (Limit, Offset) -> [PromotionInformation] -> [PromotionInformation]
      applyLimits Nothing promotions = promotions
      applyLimits (Just (limit, offset)) promotions | limit > 0 && offset >= 0 = Prelude.take limit $ Prelude.drop offset promotions
      applyLimits _ promotions = promotions

      applyPromotionFilter :: Maybe PromotionFilter -> [PromotionInformation] -> [PromotionInformation]
      applyPromotionFilter Nothing promotions = promotions
      applyPromotionFilter (Just PromotionFilter {..}) promotions =
        let idFilter = case promotionFilterId of
              Just ids -> Prelude.filter ((`elem` ids) . promotionInfoId)
              Nothing -> id
            beltFilter = case promotionFilterBelt of
              Just belts -> Prelude.filter ((`