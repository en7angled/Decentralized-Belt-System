{-# LANGUAGE RecordWildCards #-}

module Query.Live where

import Control.Monad.Reader
import Data.List qualified as L
import Data.MultiSet (fromList, toOccurList)
import Data.Ord (Down (..))
import Data.Text qualified as Text
import DomainTypes.Core.Types
import DomainTypes.Transfer.Types
import DomainTypes.Core.BJJ (BJJBelt)
import Query.Common
import QueryAppMonad (QueryAppContext (..))
import TxBuilding.Context
import TxBuilding.Lookups
import Types

getPractitionerProfile :: (MonadReader QueryAppContext m, MonadIO m) => ProfileRefAC -> m PractitionerProfileInformation
getPractitionerProfile profileRefAC = do
  providerCtx <- asks providerContext
  liftIO $ runQuery providerCtx $ getPractitionerInformation profileRefAC

getOrganizationProfile :: (MonadReader QueryAppContext m, MonadIO m) => ProfileRefAC -> m OrganizationProfileInformation
getOrganizationProfile profileRefAC = do
  providerCtx <- asks providerContext
  liftIO $ runQuery providerCtx $ getOrganizationInformation profileRefAC

getProfilesCount :: (MonadReader QueryAppContext m, MonadIO m) => Maybe ProfileFilter -> m Int
getProfilesCount maybeProfileFilter = Prelude.length <$> getProfiles Nothing maybeProfileFilter Nothing

getProfiles :: (MonadReader QueryAppContext m, MonadIO m) => Maybe (Limit, Offset) -> Maybe ProfileFilter -> Maybe (ProfilesOrderBy, SortOrder) -> m [Profile]
getProfiles maybeLimitOffset maybeProfileFilter maybeOrder = do
  providerCtx <- asks providerContext
  allProfiles <- liftIO $ runQuery providerCtx (getAllProfiles (getNetworkId providerCtx))
  return $ applyFilterOrderLimit maybeLimitOffset maybeProfileFilter maybeOrder applyProfileFilter applyOrdering allProfiles
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
  allPromotions <- liftIO $ runQuery providerCtx (getAllPromotions (getNetworkId providerCtx))
  return $ applyFilterOrderLimit maybeLimitOffset maybePromotionFilter maybeOrder applyPromotionFilter applyOrdering allPromotions
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

-- | Count by loading all matching rows then taking length. Use projected backend for large result sets if a dedicated count is needed.
getPromotionsCount :: (MonadReader QueryAppContext m, MonadIO m) => Maybe PromotionFilter -> m Int
getPromotionsCount maybePromotionFilter = Prelude.length <$> getPromotions Nothing maybePromotionFilter Nothing

getRanks :: (MonadReader QueryAppContext m, MonadIO m) => Maybe (Limit, Offset) -> Maybe RankFilter -> Maybe (RanksOrderBy, SortOrder) -> m [Rank]
getRanks maybeLimitOffset maybeRankFilter maybeOrder = do
  providerCtx <- asks providerContext
  allRanks <- liftIO $ runQuery providerCtx (getAllRanks (getNetworkId providerCtx))
  return $ applyFilterOrderLimit maybeLimitOffset maybeRankFilter maybeOrder applyRankFilter applyOrdering allRanks
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

getAchievements :: (MonadReader QueryAppContext m, MonadIO m) => Maybe (Limit, Offset) -> Maybe AchievementFilter -> Maybe (AchievementsOrderBy, SortOrder) -> m [Achievement]
getAchievements maybeLimitOffset maybeAchievementFilter maybeOrder = do
  providerCtx <- asks providerContext
  allAchievements <- liftIO $ runQuery providerCtx (getAllAchievements (getNetworkId providerCtx))
  return $ applyFilterOrderLimit maybeLimitOffset maybeAchievementFilter maybeOrder applyAchievementFilter applyOrdering allAchievements
  where
    applyOrdering Nothing achievements = achievements
    applyOrdering (Just (orderBy, order)) achievements =
      case (orderBy, order) of
        (AchievementsOrderById, Types.Asc) -> L.sortOn achievementId achievements
        (AchievementsOrderById, Types.Desc) -> L.sortOn (Down . achievementId) achievements
        (AchievementsOrderByDate, Types.Asc) -> L.sortOn achievementDate achievements
        (AchievementsOrderByDate, Types.Desc) -> L.sortOn (Down . achievementDate) achievements
        (AchievementsOrderByAwardedTo, Types.Asc) -> L.sortOn achievementAwardedTo achievements
        (AchievementsOrderByAwardedTo, Types.Desc) -> L.sortOn (Down . achievementAwardedTo) achievements
        (AchievementsOrderByAwardedBy, Types.Asc) -> L.sortOn achievementAwardedBy achievements
        (AchievementsOrderByAwardedBy, Types.Desc) -> L.sortOn (Down . achievementAwardedBy) achievements
        (AchievementsOrderByName, Types.Asc) -> L.sortOn achievementName achievements
        (AchievementsOrderByName, Types.Desc) -> L.sortOn (Down . achievementName) achievements

    applyAchievementFilter Nothing achievements = achievements
    applyAchievementFilter (Just AchievementFilter {..}) achievements =
      let idFilter = case achievementFilterId of
            Just ids -> Prelude.filter ((`elem` ids) . achievementId)
            Nothing -> id
          awardedToFilter = case achievementFilterAwardedToProfileId of
            Just ids -> Prelude.filter ((`elem` ids) . achievementAwardedTo)
            Nothing -> id
          awardedByFilter = case achievementFilterAwardedByProfileId of
            Just ids -> Prelude.filter ((`elem` ids) . achievementAwardedBy)
            Nothing -> id
          isAcceptedFilter = case achievementFilterIsAccepted of
            Just ac -> Prelude.filter ((== ac) . achievementIsAccepted)
            Nothing -> id
          dateFilter = case achievementFilterDateInterval of
            (Just from, Just to) -> Prelude.filter (\a -> achievementDate a >= from && achievementDate a <= to)
            (Nothing, Just to) -> Prelude.filter (\a -> achievementDate a <= to)
            (Just from, Nothing) -> Prelude.filter (\a -> achievementDate a >= from)
            (Nothing, Nothing) -> id
       in idFilter . awardedToFilter . awardedByFilter . isAcceptedFilter . dateFilter $ achievements

getAchievementsCount :: (MonadReader QueryAppContext m, MonadIO m) => Maybe AchievementFilter -> m Int
getAchievementsCount maybeAchievementFilter = Prelude.length <$> getAchievements Nothing maybeAchievementFilter Nothing

getMembershipHistories :: (MonadReader QueryAppContext m, MonadIO m) => Maybe (Limit, Offset) -> Maybe MembershipHistoryFilter -> Maybe (MembershipHistoriesOrderBy, SortOrder) -> m [MembershipHistoryInformation]
getMembershipHistories maybeLimitOffset maybeFilter maybeOrder = do
  providerCtx <- asks providerContext
  allInfos <- liftIO $ runQuery providerCtx (getAllMembershipHistoryInformation (getNetworkId providerCtx))
  return $ applyFilterOrderLimit maybeLimitOffset maybeFilter maybeOrder applyMembershipHistoryFilter applyOrdering allInfos
  where
    applyOrdering Nothing infos = infos
    applyOrdering (Just (orderBy, order)) infos =
      case (orderBy, order) of
        (MembershipHistoriesOrderById, Types.Asc) -> L.sortOn membershipHistoryInformationId infos
        (MembershipHistoriesOrderById, Types.Desc) -> L.sortOn (Down . membershipHistoryInformationId) infos
        (MembershipHistoriesOrderByPractitioner, Types.Asc) -> L.sortOn membershipHistoryInformationPractitionerId infos
        (MembershipHistoriesOrderByPractitioner, Types.Desc) -> L.sortOn (Down . membershipHistoryInformationPractitionerId) infos
        (MembershipHistoriesOrderByOrganization, Types.Asc) -> L.sortOn membershipHistoryInformationOrganizationId infos
        (MembershipHistoriesOrderByOrganization, Types.Desc) -> L.sortOn (Down . membershipHistoryInformationOrganizationId) infos
        -- Live backend: MembershipHistoryInformation has no created-at field; order_by=created_at leaves order unchanged. Use projected backend for created_at ordering.
        (MembershipHistoriesOrderByCreatedAt, _) -> infos

    applyMembershipHistoryFilter Nothing infos = infos
    applyMembershipHistoryFilter (Just MembershipHistoryFilter {..}) infos =
      let orgFilter = case membershipHistoryFilterOrganizationProfileId of
            Just ids -> Prelude.filter ((`elem` ids) . membershipHistoryInformationOrganizationId)
            Nothing -> id
          practFilter = case membershipHistoryFilterPractitionerProfileId of
            Just ids -> Prelude.filter ((`elem` ids) . membershipHistoryInformationPractitionerId)
            Nothing -> id
       in orgFilter . practFilter $ infos

getMembershipHistoriesCount :: (MonadReader QueryAppContext m, MonadIO m) => Maybe MembershipHistoryFilter -> m Int
getMembershipHistoriesCount maybeFilter = Prelude.length <$> getMembershipHistories Nothing maybeFilter Nothing

getMembershipIntervals :: (MonadReader QueryAppContext m, MonadIO m) => Maybe (Limit, Offset) -> Maybe MembershipIntervalFilter -> Maybe (MembershipIntervalsOrderBy, SortOrder) -> m [MembershipIntervalInformation]
getMembershipIntervals maybeLimitOffset maybeFilter maybeOrder = do
  providerCtx <- asks providerContext
  allInfos <- liftIO $ runQuery providerCtx (getAllMembershipIntervalInformation (getNetworkId providerCtx))
  return $ applyFilterOrderLimit maybeLimitOffset maybeFilter maybeOrder applyMembershipIntervalFilter applyOrdering allInfos
  where
    applyOrdering Nothing infos = infos
    applyOrdering (Just (orderBy, order)) infos =
      case (orderBy, order) of
        (MembershipIntervalsOrderById, Types.Asc) -> L.sortOn membershipIntervalInformationId infos
        (MembershipIntervalsOrderById, Types.Desc) -> L.sortOn (Down . membershipIntervalInformationId) infos
        (MembershipIntervalsOrderByStartDate, Types.Asc) -> L.sortOn membershipIntervalInformationStartDate infos
        (MembershipIntervalsOrderByStartDate, Types.Desc) -> L.sortOn (Down . membershipIntervalInformationStartDate) infos
        (MembershipIntervalsOrderByIntervalNumber, Types.Asc) -> L.sortOn membershipIntervalInformationNumber infos
        (MembershipIntervalsOrderByIntervalNumber, Types.Desc) -> L.sortOn (Down . membershipIntervalInformationNumber) infos
        (MembershipIntervalsOrderByPractitioner, Types.Asc) -> L.sortOn membershipIntervalInformationPractitionerId infos
        (MembershipIntervalsOrderByPractitioner, Types.Desc) -> L.sortOn (Down . membershipIntervalInformationPractitionerId) infos

    applyMembershipIntervalFilter Nothing infos = infos
    applyMembershipIntervalFilter (Just MembershipIntervalFilter {..}) infos =
      case membershipIntervalFilterPractitionerProfileId of
        Just ids -> Prelude.filter ((`elem` ids) . membershipIntervalInformationPractitionerId) infos
        Nothing -> infos

getMembershipIntervalsCount :: (MonadReader QueryAppContext m, MonadIO m) => Maybe MembershipIntervalFilter -> m Int
getMembershipIntervalsCount maybeFilter = Prelude.length <$> getMembershipIntervals Nothing maybeFilter Nothing
