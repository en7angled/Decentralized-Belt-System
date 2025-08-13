{-# LANGUAGE RecordWildCards #-}

module AppMonad where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Data.List (sortOn)
import Data.Maybe
import Data.MultiSet
import Data.Ord (Down (..))
import Data.Text hiding (elem, take)
import qualified Data.Text.Encoding
import DomainTypes.Profile.Types
import GeniusYield.GYConfig (GYCoreConfig (..))
import GeniusYield.TxBuilder (GYTxQueryMonad)
import GeniusYield.Types
import Onchain.BJJ
import Servant
import TxBuilding.Context
import TxBuilding.Lookups

-- | Apply optional pagination limits to a list in a total and generic way.
-- Negative limits or offsets are clamped to 0. If the list is shorter than
-- the requested limit, the entire available suffix is returned safely.
applyLimits :: Maybe (Int, Int) -> [a] -> [a]
applyLimits Nothing xs = xs
applyLimits (Just (limit, offset)) xs =
  let safeLimit = Prelude.max 0 limit
      safeOffset = Prelude.max 0 offset
   in Prelude.take safeLimit (Prelude.drop safeOffset xs)

------------------------------------------------------------------------------------------------

-- * Capabilities

------------------------------------------------------------------------------------------------

type Limit = Int

type Offset = Int

data ProfileFilter = ProfileFilter
  { profileFilterId :: Maybe [ProfileRefAC],
    profileFilterType :: Maybe ProfileType,
    profileFilterName :: Maybe Text,
    profileFilterDescription :: Maybe Text
  }

class ProfilesQueryMonad m where
  getPractitionerProfile :: ProfileRefAC -> m PractitionerProfileInformation
  getOrganizationProfile :: ProfileRefAC -> m OrganizationProfileInformation
  getProfilesCount :: Maybe ProfileType -> m Int
  getProfiles :: Maybe (Limit, Offset) -> Maybe ProfileFilter -> Maybe (ProfilesOrderBy, SortOrder) -> m [Profile]

data PromotionFilter = PromotionFilter
  { promotionFilterId :: Maybe [ProfileRefAC],
    promotionFilterBelt :: Maybe [BJJBelt],
    promotionFilterAchievedByProfileId :: Maybe [ProfileRefAC],
    promotionFilterAwardedByProfileId :: Maybe [ProfileRefAC],
    promotionFilterAchievementDateInterval :: (Maybe GYTime, Maybe GYTime)
  }

class PromotionsStatsQueryMonad m where
  getPromotions :: Maybe (Limit, Offset) -> Maybe PromotionFilter -> Maybe (PromotionsOrderBy, SortOrder) -> m [PromotionInformation]
  getPromotionsCount :: Maybe PromotionFilter -> m Int

data RankFilter = RankFilter
  { rankFilterId :: Maybe [RankAC],
    rankFilterBelt :: Maybe [BJJBelt],
    rankFilterAchievedByProfileId :: Maybe [ProfileRefAC],
    rankFilterAwardedByProfileId :: Maybe [ProfileRefAC],
    rankFilterAchievementDateInterval :: (Maybe GYTime, Maybe GYTime)
  }

class RanksStatsQueryMonad m where
  getRanks :: Maybe (Limit, Offset) -> Maybe RankFilter -> Maybe (RanksOrderBy, SortOrder) -> m [RankInformation]
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

  getProfilesCount :: Maybe ProfileType -> AppMonad Int
  getProfilesCount maybeProfileType = do
    TxBuildingContext {..} <- ask
    liftIO $ runQuery providerCtx $ getAllProfilesCount (cfgNetworkId . ctxCoreCfg $ providerCtx)

  getProfiles :: Maybe (Limit, Offset) -> Maybe ProfileFilter -> Maybe (ProfilesOrderBy, SortOrder) -> AppMonad [Profile]
  getProfiles maybeLimitOffset maybeProfileFilter maybeOrder = do
    TxBuildingContext {..} <- ask
    allProfiles <- liftIO $ runQuery providerCtx (getAllProfiles (cfgNetworkId . ctxCoreCfg $ providerCtx))
    return $ applyLimits maybeLimitOffset $ applyOrdering maybeOrder $ applyProfileFilter maybeProfileFilter allProfiles
    where
      applyOrdering :: Maybe (ProfilesOrderBy, SortOrder) -> [Profile] -> [Profile]
      applyOrdering Nothing profiles = profiles
      applyOrdering (Just (orderBy, order)) profiles =
        case (orderBy, order) of
          (ProfilesOrderById, Asc) -> sortOn profileId profiles
          (ProfilesOrderById, Desc) -> sortOn (Down . profileId) profiles
          (ProfilesOrderByName, Asc) -> sortOn profileName profiles
          (ProfilesOrderByName, Desc) -> sortOn (Down . profileName) profiles
          (ProfilesOrderByDescription, Asc) -> sortOn profileDescription profiles
          (ProfilesOrderByDescription, Desc) -> sortOn (Down . profileDescription) profiles
          (ProfilesOrderByType, Asc) -> sortOn profileType profiles
          (ProfilesOrderByType, Desc) -> sortOn (Down . profileType) profiles

      applyProfileFilter :: Maybe ProfileFilter -> [Profile] -> [Profile]
      applyProfileFilter Nothing profiles = profiles
      applyProfileFilter (Just ProfileFilter {..}) profiles =
        let idFilter = case profileFilterId of
              Just ids -> Prelude.filter ((`elem` ids) . profileId)
              Nothing -> id
            typeFilter = case profileFilterType of
              Just pt -> Prelude.filter ((== pt) . profileType)
              Nothing -> id
            nameFilter = case profileFilterName of
              Just name -> Prelude.filter ((== name) . profileName)
              Nothing -> id
            descriptionFilter = case profileFilterDescription of
              Just description -> Prelude.filter ((== description) . profileDescription)
              Nothing -> id
         in idFilter . typeFilter . nameFilter . descriptionFilter $ profiles

instance PromotionsStatsQueryMonad AppMonad where
  getPromotions :: Maybe (Limit, Offset) -> Maybe PromotionFilter -> Maybe (PromotionsOrderBy, SortOrder) -> AppMonad [PromotionInformation]
  getPromotions maybeLimitOffset maybePromotionFilter maybeOrder = do
    TxBuildingContext {..} <- ask
    allPromotions <- liftIO $ runQuery providerCtx (getAllPromotions (cfgNetworkId . ctxCoreCfg $ providerCtx))
    return $ applyLimits maybeLimitOffset $ applyOrdering maybeOrder $ applyPromotionFilter maybePromotionFilter allPromotions
    where
      applyOrdering :: Maybe (PromotionsOrderBy, SortOrder) -> [PromotionInformation] -> [PromotionInformation]
      applyOrdering Nothing promotions = promotions
      applyOrdering (Just (orderBy, order)) promotions =
        case (orderBy, order) of
          (PromotionsOrderById, Asc) -> sortOn promotionInfoId promotions
          (PromotionsOrderById, Desc) -> sortOn (Down . promotionInfoId) promotions
          (PromotionsOrderByBelt, Asc) -> sortOn promotionInfoBelt promotions
          (PromotionsOrderByBelt, Desc) -> sortOn (Down . promotionInfoBelt) promotions
          (PromotionsOrderByAchievedBy, Asc) -> sortOn promotionInfoAchievedByProfileId promotions
          (PromotionsOrderByAchievedBy, Desc) -> sortOn (Down . promotionInfoAchievedByProfileId) promotions
          (PromotionsOrderByAwardedBy, Asc) -> sortOn promotionInfoAwardedByProfileId promotions
          (PromotionsOrderByAwardedBy, Desc) -> sortOn (Down . promotionInfoAwardedByProfileId) promotions
          (PromotionsOrderByDate, Asc) -> sortOn promotionInfoAchievementDate promotions
          (PromotionsOrderByDate, Desc) -> sortOn (Down . promotionInfoAchievementDate) promotions

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
  getPromotionsCount maybePromotionFilter = Prelude.length <$> getPromotions Nothing maybePromotionFilter Nothing

instance RanksStatsQueryMonad AppMonad where
  getRanks :: Maybe (Limit, Offset) -> Maybe RankFilter -> Maybe (RanksOrderBy, SortOrder) -> AppMonad [RankInformation]
  getRanks maybeLimitOffset maybeRankFilter maybeOrder = do
    TxBuildingContext {..} <- ask
    allRanks <- liftIO $ runQuery providerCtx (getAllRanks (cfgNetworkId . ctxCoreCfg $ providerCtx))
    return $ applyLimits maybeLimitOffset $ applyOrdering maybeOrder $ applyRankFilter maybeRankFilter allRanks
    where
      applyOrdering :: Maybe (RanksOrderBy, SortOrder) -> [RankInformation] -> [RankInformation]
      applyOrdering Nothing ranks = ranks
      applyOrdering (Just (orderBy, order)) ranks =
        case (orderBy, order) of
          (RanksOrderById, Asc) -> sortOn rankInfoId ranks
          (RanksOrderById, Desc) -> sortOn (Down . rankInfoId) ranks
          (RanksOrderByBelt, Asc) -> sortOn rankInfoBelt ranks
          (RanksOrderByBelt, Desc) -> sortOn (Down . rankInfoBelt) ranks
          (RanksOrderByAchievedBy, Asc) -> sortOn rankInfoAchievedByProfileId ranks
          (RanksOrderByAchievedBy, Desc) -> sortOn (Down . rankInfoAchievedByProfileId) ranks
          (RanksOrderByAwardedBy, Asc) -> sortOn rankInfoAwardedByProfileId ranks
          (RanksOrderByAwardedBy, Desc) -> sortOn (Down . rankInfoAwardedByProfileId) ranks
          (RanksOrderByDate, Asc) -> sortOn rankInfoAchievementDate ranks
          (RanksOrderByDate, Desc) -> sortOn (Down . rankInfoAchievementDate) ranks

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
  getRanksCount maybeRankFilter = Prelude.length <$> getRanks Nothing maybeRankFilter Nothing

  getBeltTotals :: AppMonad [(BJJBelt, Int)]
  getBeltTotals = do
    allRankInfo <- getRanks Nothing Nothing Nothing
    let allBelts = Prelude.map rankInfoBelt allRankInfo
    let beltTotals = toOccurList . fromList $ allBelts
    return beltTotals
