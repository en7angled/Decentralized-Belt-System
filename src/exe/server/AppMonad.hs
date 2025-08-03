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

type Limit = Int

type Offset = Int

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
              Just belts -> Prelude.filter ((`elem` belts) . promotionInfoBelt)
              Nothing -> id
            achievedByFilter = case promotionFilterAchievedByProfileId of
              Just ids -> Prelude.filter ((`elem` ids) . promotionInfoAchievedByProfileId)
              Nothing -> id
            awardedByFilter = case promotionFilterAwardedByProfileId of
              Just ids -> Prelude.filter ((`elem` ids) . promotionInfoAwardedByProfileId)
              Nothing -> id
            achievementDateFilter = case promotionFilterAchievementDateInterval of
              (Just from, Just to) -> Prelude.filter (\promotion -> promotionInfoAchievementDate promotion >= from && promotionInfoAchievementDate promotion <= to)
              (Nothing, Just to) -> Prelude.filter (\promotion -> promotionInfoAchievementDate promotion <= to)
              (Just from, Nothing) -> Prelude.filter (\promotion -> promotionInfoAchievementDate promotion >= from)
              (Nothing, Nothing) -> id
         in idFilter . beltFilter . achievedByFilter . awardedByFilter . achievementDateFilter $ promotions

  getPromotionsCount :: Maybe PromotionFilter -> AppMonad Int
  getPromotionsCount maybePromotionFilter = Prelude.length <$> getPromotions Nothing maybePromotionFilter

instance RanksStatsQueryMonad AppMonad where
  getRanks :: Maybe (Limit, Offset) -> Maybe RankFilter -> AppMonad [RankInformation]
  getRanks maybeLimitOffset maybeRankFilter = do
    TxBuildingContext {..} <- ask
    allRanks <- liftIO $ runQuery providerCtx (getAllRanks (cfgNetworkId . ctxCoreCfg $ providerCtx))
    return $ applyLimits maybeLimitOffset $ applyRankFilter maybeRankFilter allRanks
    where
      applyLimits :: Maybe (Limit, Offset) -> [RankInformation] -> [RankInformation]
      applyLimits Nothing ranks = ranks
      applyLimits (Just (limit, offset)) ranks | limit > 0 && offset >= 0 = Prelude.take limit $ Prelude.drop offset ranks
      applyLimits _ ranks = ranks

      applyRankFilter :: Maybe RankFilter -> [RankInformation] -> [RankInformation]
      applyRankFilter Nothing ranks = ranks
      applyRankFilter (Just RankFilter {..}) ranks =
        let idFilter = case rankFilterId of
              Just ids -> Prelude.filter ((`elem` ids) . rankInfoId)
              Nothing -> id
            beltFilter = case rankFilterBelt of
              Just belts -> Prelude.filter ((`elem` belts) . rankInfoBelt)
              Nothing -> id
            achievedByFilter = case rankFilterAchievedByProfileId of
              Just ids -> Prelude.filter ((`elem` ids) . rankInfoAchievedByProfileId)
              Nothing -> id
            awardedByFilter = case rankFilterAwardedByProfileId of
              Just ids -> Prelude.filter ((`elem` ids) . rankInfoAwardedByProfileId)
              Nothing -> id
            achievementDateFilter = case rankFilterAchievementDateInterval of
              (Just from, Just to) -> Prelude.filter (\rank -> rankInfoAchievementDate rank >= from && rankInfoAchievementDate rank <= to)
              (Nothing, Just to) -> Prelude.filter (\rank -> rankInfoAchievementDate rank <= to)
              (Just from, Nothing) -> Prelude.filter (\rank -> rankInfoAchievementDate rank >= from)
              (Nothing, Nothing) -> id
         in idFilter . beltFilter . achievedByFilter . awardedByFilter . achievementDateFilter $ ranks

  getRanksCount :: Maybe RankFilter -> AppMonad Int
  getRanksCount maybeRankFilter = Prelude.length <$> getRanks Nothing maybeRankFilter

  getBeltTotals :: AppMonad [(BJJBelt, Int)]
  getBeltTotals = do
    allRankInfo <- getRanks Nothing Nothing
    let allBelts = Prelude.map rankInfoBelt allRankInfo
    let beltTotals = toOccurList . fromList $ allBelts
    return beltTotals
