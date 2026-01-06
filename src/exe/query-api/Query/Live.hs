{-# LANGUAGE RecordWildCards #-}

module Query.Live where

import Control.Monad.Reader
import Data.List qualified as L
import Data.MultiSet (fromList, toOccurList)
import Data.Ord (Down (..))
import Data.Text qualified as Text
import DomainTypes.Core.Types
import DomainTypes.Transfer.Types
import GeniusYield.GYConfig (GYCoreConfig (..))
import Onchain.BJJ (BJJBelt)
import Query.Common
import QueryAppMonad (QueryAppContext (..))
import TxBuilding.Context
import TxBuilding.Lookups
import Types

getPractitionerProfile :: (MonadReader QueryAppContext m, MonadIO m) => ProfileRefAC -> m PractitionerProfileInformation
getPractitionerProfile profileRefAC = do
  providerCtx <- asks providerContext
  liftIO $ runQuery providerCtx $ getPractiotionerInformation profileRefAC

getOrganizationProfile :: (MonadReader QueryAppContext m, MonadIO m) => ProfileRefAC -> m OrganizationProfileInformation
getOrganizationProfile profileRefAC = do
  providerCtx <- asks providerContext
  liftIO $ runQuery providerCtx $ getOrganizationInformation profileRefAC

getProfilesCount :: (MonadReader QueryAppContext m, MonadIO m) => Maybe ProfileType -> m Int
getProfilesCount _ = do
  providerCtx <- asks providerContext
  liftIO $ runQuery providerCtx $ getAllProfilesCount (cfgNetworkId . ctxCoreCfg $ providerCtx)

getProfiles :: (MonadReader QueryAppContext m, MonadIO m) => Maybe (Limit, Offset) -> Maybe ProfileFilter -> Maybe (ProfilesOrderBy, SortOrder) -> m [Profile]
getProfiles maybeLimitOffset maybeProfileFilter maybeOrder = do
  providerCtx <- asks providerContext
  allProfiles <- liftIO $ runQuery providerCtx (getAllProfiles (cfgNetworkId . ctxCoreCfg $ providerCtx))
  return $ applyLimits maybeLimitOffset $ applyOrdering maybeOrder $ applyProfileFilter maybeProfileFilter allProfiles
  where
    applyOrdering Nothing profiles = profiles
    applyOrdering (Just (orderBy, order)) profiles =
      case (orderBy, order) of
        (ProfilesOrderById, Types.Asc) -> L.sortOn profileId profiles
        (ProfilesOrderById, Types.Desc) -> L.sortOn (Down . profileId) profiles
        (ProfilesOrderByName, Types.Asc) -> L.sortOn profileName profiles
        (ProfilesOrderByName, Types.Desc) -> L.sortOn (Down . profileName) profiles
        (ProfilesOrderByDescription, Types.Asc) -> L.sortOn profileDescription profiles
        (ProfilesOrderByDescription, Types.Desc) -> L.sortOn (Down . profileDescription) profiles
        (ProfilesOrderByType, Types.Asc) -> L.sortOn profileType profiles
        (ProfilesOrderByType, Types.Desc) -> L.sortOn (Down . profileType) profiles

    applyProfileFilter Nothing profiles = profiles
    applyProfileFilter (Just ProfileFilter {..}) profiles =
      let idFilter = case profileFilterId of
            Just ids -> Prelude.filter ((`elem` ids) . profileId)
            Nothing -> id
          typeFilter = case profileFilterType of
            Just pt -> Prelude.filter ((== pt) . profileType)
            Nothing -> id
          nameFilter = case profileFilterName of
            Just nameSubstring -> Prelude.filter ((Text.toLower nameSubstring `Text.isInfixOf`) . Text.toLower . profileName)
            Nothing -> id
          descriptionFilter = case profileFilterDescription of
            Just descriptionSubstring -> Prelude.filter ((Text.toLower descriptionSubstring `Text.isInfixOf`) . Text.toLower . profileDescription)
            Nothing -> id
       in idFilter . typeFilter . nameFilter . descriptionFilter $ profiles

getPromotions :: (MonadReader QueryAppContext m, MonadIO m) => Maybe (Limit, Offset) -> Maybe PromotionFilter -> Maybe (PromotionsOrderBy, SortOrder) -> m [Promotion]
getPromotions maybeLimitOffset maybePromotionFilter maybeOrder = do
  providerCtx <- asks providerContext
  allPromotions <- liftIO $ runQuery providerCtx (getAllPromotions (cfgNetworkId . ctxCoreCfg $ providerCtx))
  return $ applyLimits maybeLimitOffset $ applyOrdering maybeOrder $ applyPromotionFilter maybePromotionFilter allPromotions
  where
    applyOrdering Nothing promotions = promotions
    applyOrdering (Just (orderBy, order)) promotions =
      case (orderBy, order) of
        (PromotionsOrderById, Types.Asc) -> L.sortOn promotionId promotions
        (PromotionsOrderById, Types.Desc) -> L.sortOn (Down . promotionId) promotions
        (PromotionsOrderByBelt, Types.Asc) -> L.sortOn promotionBelt promotions
        (PromotionsOrderByBelt, Types.Desc) -> L.sortOn (Down . promotionBelt) promotions
        (PromotionsOrderByAchievedBy, Types.Asc) -> L.sortOn promotionAchievedByProfileId promotions
        (PromotionsOrderByAchievedBy, Types.Desc) -> L.sortOn (Down . promotionAchievedByProfileId) promotions
        (PromotionsOrderByAwardedBy, Types.Asc) -> L.sortOn promotionAwardedByProfileId promotions
        (PromotionsOrderByAwardedBy, Types.Desc) -> L.sortOn (Down . promotionAwardedByProfileId) promotions
        (PromotionsOrderByDate, Types.Asc) -> L.sortOn promotionAchievementDate promotions
        (PromotionsOrderByDate, Types.Desc) -> L.sortOn (Down . promotionAchievementDate) promotions

    applyPromotionFilter Nothing promotions = promotions
    applyPromotionFilter (Just PromotionFilter {..}) promotions =
      let idFilter = case promotionFilterId of
            Just ids -> Prelude.filter ((`elem` ids) . promotionId)
            Nothing -> id
          beltFilter = case promotionFilterBelt of
            Just belts -> Prelude.filter ((`elem` belts) . promotionBelt)
            Nothing -> id
          achievedByFilter = case promotionFilterAchievedByProfileId of
            Just ids -> Prelude.filter ((`elem` ids) . promotionAchievedByProfileId)
            Nothing -> id
          awardedByFilter = case promotionFilterAwardedByProfileId of
            Just ids -> Prelude.filter ((`elem` ids) . promotionAwardedByProfileId)
            Nothing -> id
          achievementDateFilter = case promotionFilterAchievementDateInterval of
            (Just from, Just to) -> Prelude.filter (\promotion -> promotionAchievementDate promotion >= from && promotionAchievementDate promotion <= to)
            (Nothing, Just to) -> Prelude.filter (\promotion -> promotionAchievementDate promotion <= to)
            (Just from, Nothing) -> Prelude.filter (\promotion -> promotionAchievementDate promotion >= from)
            (Nothing, Nothing) -> id
       in idFilter . beltFilter . achievedByFilter . awardedByFilter . achievementDateFilter $ promotions

getPromotionsCount :: (MonadReader QueryAppContext m, MonadIO m) => Maybe PromotionFilter -> m Int
getPromotionsCount maybePromotionFilter = Prelude.length <$> getPromotions Nothing maybePromotionFilter Nothing

getRanks :: (MonadReader QueryAppContext m, MonadIO m) => Maybe (Limit, Offset) -> Maybe RankFilter -> Maybe (RanksOrderBy, SortOrder) -> m [Rank]
getRanks maybeLimitOffset maybeRankFilter maybeOrder = do
  providerCtx <- asks providerContext
  allRanks <- liftIO $ runQuery providerCtx (getAllRanks (cfgNetworkId . ctxCoreCfg $ providerCtx))
  return $ applyLimits maybeLimitOffset $ applyOrdering maybeOrder $ applyRankFilter maybeRankFilter allRanks
  where
    applyOrdering Nothing ranks = ranks
    applyOrdering (Just (orderBy, order)) ranks =
      case (orderBy, order) of
        (RanksOrderById, Types.Asc) -> L.sortOn rankId ranks
        (RanksOrderById, Types.Desc) -> L.sortOn (Down . rankId) ranks
        (RanksOrderByBelt, Types.Asc) -> L.sortOn rankBelt ranks
        (RanksOrderByBelt, Types.Desc) -> L.sortOn (Down . rankBelt) ranks
        (RanksOrderByAchievedBy, Types.Asc) -> L.sortOn rankAchievedByProfileId ranks
        (RanksOrderByAchievedBy, Types.Desc) -> L.sortOn (Down . rankAchievedByProfileId) ranks
        (RanksOrderByAwardedBy, Types.Asc) -> L.sortOn rankAwardedByProfileId ranks
        (RanksOrderByAwardedBy, Types.Desc) -> L.sortOn (Down . rankAwardedByProfileId) ranks
        (RanksOrderByDate, Types.Asc) -> L.sortOn rankAchievementDate ranks
        (RanksOrderByDate, Types.Desc) -> L.sortOn (Down . rankAchievementDate) ranks

    applyRankFilter Nothing ranks = ranks
    applyRankFilter (Just RankFilter {..}) ranks =
      let idFilter = case rankFilterId of
            Just ids -> Prelude.filter ((`elem` ids) . rankId)
            Nothing -> id
          beltFilter = case rankFilterBelt of
            Just belts -> Prelude.filter ((`elem` belts) . rankBelt)
            Nothing -> id
          achievedByFilter = case rankFilterAchievedByProfileId of
            Just ids -> Prelude.filter ((`elem` ids) . rankAchievedByProfileId)
            Nothing -> id
          awardedByFilter = case rankFilterAwardedByProfileId of
            Just ids -> Prelude.filter ((`elem` ids) . rankAwardedByProfileId)
            Nothing -> id
          achievementDateFilter = case rankFilterAchievementDateInterval of
            (Just from, Just to) -> Prelude.filter (\rank -> rankAchievementDate rank >= from && rankAchievementDate rank <= to)
            (Nothing, Just to) -> Prelude.filter (\rank -> rankAchievementDate rank <= to)
            (Just from, Nothing) -> Prelude.filter (\rank -> rankAchievementDate rank >= from)
            (Nothing, Nothing) -> id
       in idFilter . beltFilter . achievedByFilter . awardedByFilter . achievementDateFilter $ ranks

getRanksCount :: (MonadReader QueryAppContext m, MonadIO m) => Maybe RankFilter -> m Int
getRanksCount maybeRankFilter = Prelude.length <$> getRanks Nothing maybeRankFilter Nothing

getBeltTotals :: (MonadReader QueryAppContext m, MonadIO m) => m [(BJJBelt, Int)]
getBeltTotals = do
  allRanks <- getRanks Nothing Nothing Nothing
  let allBelts = Prelude.map rankBelt allRanks
  let beltTotals = toOccurList . fromList $ allBelts
  return beltTotals

getProfileTypeTotals :: (MonadReader QueryAppContext m, MonadIO m) => m [(ProfileType, Int)]
getProfileTypeTotals = do
  allProfiles <- getProfiles Nothing Nothing Nothing
  let allTypes = Prelude.map profileType allProfiles
  let typeTotals = toOccurList . fromList $ allTypes
  return typeTotals

getPromotionBeltTotals :: (MonadReader QueryAppContext m, MonadIO m) => m [(BJJBelt, Int)]
getPromotionBeltTotals = do
  allPromotions <- getPromotions Nothing Nothing Nothing
  let allBelts = Prelude.map promotionBelt allPromotions
  let beltTotals = toOccurList . fromList $ allBelts
  return beltTotals
