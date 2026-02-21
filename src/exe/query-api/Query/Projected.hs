{-# LANGUAGE RecordWildCards #-}

module Query.Projected where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (for_)
import Data.List (groupBy, nub, sortOn)
import Control.Monad.Reader.Class
import Data.MultiSet (fromList, toOccurList)
import Data.Text qualified as T
import Data.Maybe 
import Database.Esqueleto.Experimental
import Database.Persist qualified as P
import DomainTypes.Core.Types
import DomainTypes.Transfer.Types
import DomainTypes.Core.BJJ (BJJBelt)
import GeniusYield.Types (GYTime)
import Query.Common qualified as C
import QueryAppMonad
import Storage
import TxBuilding.Exceptions (TxBuildingException (..))
import Types

-- | Apply sort direction (Asc/Desc) to an expression for orderBy.
orderByDir :: PersistField a => SortOrder -> SqlExpr (Value a) -> SqlExpr OrderBy
orderByDir so expr = case so of Types.Asc -> asc expr; Types.Desc -> desc expr

-- | Apply optional limit/offset in a select query.
applyLimitOffset :: Maybe (C.Limit, C.Offset) -> SqlQuery ()
applyLimitOffset Nothing = pure ()
applyLimitOffset (Just (l, o)) = do
  offset (fromIntegral o)
  limit (fromIntegral l)

-- | Add where_ clauses for an optional date interval (from, to).
whereDateInterval :: (Maybe GYTime, Maybe GYTime) -> SqlExpr (Value GYTime) -> SqlQuery ()
whereDateInterval (mf, mt) dateCol = case (mf, mt) of
  (Nothing, Nothing) -> pure ()
  (Just f, Nothing) -> where_ (dateCol >=. val f)
  (Nothing, Just t) -> where_ (dateCol <=. val t)
  (Just f, Just t) -> where_ (dateCol >=. val f &&. dateCol <=. val t)

applyPromotionFilter :: Maybe C.PromotionFilter -> SqlExpr (Entity PromotionProjection) -> SqlQuery ()
applyPromotionFilter Nothing _ = pure ()
applyPromotionFilter (Just C.PromotionFilter {..}) pr = do
  for_ promotionFilterId (\ids -> where_ (pr ^. PromotionProjectionPromotionId `in_` valList ids))
  for_ promotionFilterBelt (\belts -> where_ (pr ^. PromotionProjectionPromotionBelt `in_` valList belts))
  for_ promotionFilterAchievedByProfileId (\ids -> where_ (pr ^. PromotionProjectionPromotionAchievedByProfileId `in_` valList ids))
  for_ promotionFilterAwardedByProfileId (\ids -> where_ (pr ^. PromotionProjectionPromotionAwardedByProfileId `in_` valList ids))
  whereDateInterval promotionFilterAchievementDateInterval (pr ^. PromotionProjectionPromotionAchievementDate)

applyRankFilter :: Maybe C.RankFilter -> SqlExpr (Entity RankProjection) -> SqlQuery ()
applyRankFilter Nothing _ = pure ()
applyRankFilter (Just C.RankFilter {..}) rp = do
  for_ rankFilterId (\ids -> where_ (rp ^. RankProjectionRankId `in_` valList ids))
  for_ rankFilterBelt (\belts -> where_ (rp ^. RankProjectionRankBelt `in_` valList belts))
  for_ rankFilterAchievedByProfileId (\ids -> where_ (rp ^. RankProjectionRankAchievedByProfileId `in_` valList ids))
  for_ rankFilterAwardedByProfileId (\ids -> where_ (rp ^. RankProjectionRankAwardedByProfileId `in_` valList ids))
  whereDateInterval rankFilterAchievementDateInterval (rp ^. RankProjectionRankAchievementDate)

applyAchievementFilter :: Maybe C.AchievementFilter -> SqlExpr (Entity AchievementProjection) -> SqlQuery ()
applyAchievementFilter Nothing _ = pure ()
applyAchievementFilter (Just C.AchievementFilter {..}) ap = do
  for_ achievementFilterId (\ids -> where_ (ap ^. AchievementProjectionAchievementId `in_` valList ids))
  for_ achievementFilterAwardedToProfileId (\ids -> where_ (ap ^. AchievementProjectionAwardedToProfileId `in_` valList ids))
  for_ achievementFilterAwardedByProfileId (\ids -> where_ (ap ^. AchievementProjectionAwardedByProfileId `in_` valList ids))
  for_ achievementFilterIsAccepted (\ac -> where_ (ap ^. AchievementProjectionIsAccepted ==. val ac))
  whereDateInterval achievementFilterDateInterval (ap ^. AchievementProjectionAchievementDate)

applyMembershipHistoryFilter :: Maybe C.MembershipHistoryFilter -> SqlExpr (Entity MembershipHistoryProjection) -> SqlQuery ()
applyMembershipHistoryFilter Nothing _ = pure ()
applyMembershipHistoryFilter (Just C.MembershipHistoryFilter {..}) mhp = do
  for_ membershipHistoryFilterOrganizationProfileId (\ids -> where_ (mhp ^. MembershipHistoryProjectionOrganizationProfileId `in_` valList ids))
  for_ membershipHistoryFilterPractitionerProfileId (\ids -> where_ (mhp ^. MembershipHistoryProjectionPractitionerProfileId `in_` valList ids))

applyMembershipIntervalFilter :: Maybe C.MembershipIntervalFilter -> SqlExpr (Entity MembershipIntervalProjection) -> SqlQuery ()
applyMembershipIntervalFilter Nothing _ = pure ()
applyMembershipIntervalFilter (Just C.MembershipIntervalFilter {..}) mip = do
  for_ membershipIntervalFilterPractitionerProfileId (\ids -> where_ (mip ^. MembershipIntervalProjectionPractitionerProfileId `in_` valList ids))

applyProfileFilter :: Maybe C.ProfileFilter -> SqlExpr (Entity ProfileProjection) -> SqlQuery ()
applyProfileFilter Nothing _ = pure ()
applyProfileFilter (Just C.ProfileFilter {..}) pp = do
  for_ profileFilterId (\ids -> where_ (pp ^. ProfileProjectionProfileId `in_` valList ids))
  for_ profileFilterType (\pt -> where_ (pp ^. ProfileProjectionProfileType ==. val pt))
  for_ profileFilterName (\nameSubstring -> where_ (lower_ (pp ^. ProfileProjectionProfileName) `like` val (T.pack "%" <> T.toLower nameSubstring <> T.pack "%")))
  for_ profileFilterDescription (\descriptionSubstring -> where_ (lower_ (pp ^. ProfileProjectionProfileDescription) `like` val (T.pack "%" <> T.toLower descriptionSubstring <> T.pack "%")))

getPractitionerProfile :: (MonadIO m, MonadReader QueryAppContext m) => ProfileRefAC -> m PractitionerProfileInformation
getPractitionerProfile profileRefAC = do
  pool <- asks pgPool
  liftIO $ runSqlPool (do
    mProf <- P.getBy (UniqueProfileProjection profileRefAC)
    case mProf of
      Nothing -> liftIO $ throwIO ProfileNotFound
      Just (Entity _ prof) -> do
        ranksAsc <- select $ do
          rp <- from $ table @RankProjection
          where_ (rp ^. RankProjectionRankAchievedByProfileId ==. val profileRefAC)
          orderBy [asc (rp ^. RankProjectionRankAchievementDate)]
          pure rp
        let toRank rp =
              Rank
                { rankId = rankProjectionRankId rp,
                  rankBelt = rankProjectionRankBelt rp,
                  rankAchievedByProfileId = rankProjectionRankAchievedByProfileId rp,
                  rankAwardedByProfileId = rankProjectionRankAwardedByProfileId rp,
                  rankAchievementDate = rankProjectionRankAchievementDate rp
                }
            domainRanks = Prelude.map (toRank . entityVal) ranksAsc
        case Prelude.reverse domainRanks of
          [] -> liftIO $ throwIO RankListEmpty
          (current : restRev) ->
            return
              PractitionerProfileInformation
                { practitionerId = profileRefAC,
                  practitionerName = profileProjectionProfileName prof,
                  practitionerDescription = profileProjectionProfileDescription prof,
                  practitionerImageURI = profileProjectionProfileImageURI prof,
                  practitionerCurrentRank = current,
                  practitionerPreviousRanks = Prelude.reverse restRev
                }
    ) pool

getOrganizationProfile :: (MonadIO m, MonadReader QueryAppContext m) => ProfileRefAC -> m OrganizationProfileInformation
getOrganizationProfile profileRefAC = do
  pool <- asks pgPool
  liftIO $ runSqlPool (do
    mProf <- P.getBy (UniqueProfileProjection profileRefAC)
    case mProf of
      Nothing -> liftIO $ throwIO ProfileNotFound
      Just (Entity _ prof) ->
        return
          OrganizationProfileInformation
            { organizationId = profileRefAC,
              organizationName = profileProjectionProfileName prof,
              organizationDescription = profileProjectionProfileDescription prof,
              organizationImageURI = profileProjectionProfileImageURI prof
            }
    ) pool

getProfilesCount :: (MonadIO m, MonadReader QueryAppContext m) => Maybe C.ProfileFilter -> m Int
getProfilesCount maybeProfileFilter = do
  pool <- asks pgPool
  liftIO $ runSqlPool (do
    cnt <- selectOne $ do
      pp <- from $ table @ProfileProjection
      applyProfileFilter maybeProfileFilter pp
      pure countRows
    pure (maybe 0 unValue cnt)
    ) pool

getProfiles :: (MonadIO m, MonadReader QueryAppContext m) => Maybe (C.Limit, C.Offset) -> Maybe C.ProfileFilter -> Maybe (ProfilesOrderBy, SortOrder) -> m [Profile]
getProfiles maybeLimitOffset maybeProfileFilter maybeOrder = do
  pool <- asks pgPool
  liftIO $ runSqlPool (do
    rows <- select $ do
      pp <- from $ table @ProfileProjection
      applyProfileFilter maybeProfileFilter pp
      case maybeOrder of
        Nothing -> pure ()
        Just (ob, so) ->
          case ob of
            ProfilesOrderById -> orderBy [orderByDir so (pp ^. ProfileProjectionProfileId)]
            ProfilesOrderByName -> orderBy [orderByDir so (pp ^. ProfileProjectionProfileName)]
            ProfilesOrderByDescription -> orderBy [orderByDir so (pp ^. ProfileProjectionProfileDescription)]
            ProfilesOrderByType -> orderBy [orderByDir so (pp ^. ProfileProjectionProfileType)]
      applyLimitOffset maybeLimitOffset
      pure pp
    let toProfile pp =
          Profile
            { profileId = profileProjectionProfileId pp,
              profileName = profileProjectionProfileName pp,
              profileDescription = profileProjectionProfileDescription pp,
              profileImageURI = profileProjectionProfileImageURI pp,
              profileType = profileProjectionProfileType pp
            }
    pure (Prelude.map (toProfile . entityVal) rows)
    ) pool

getPromotions :: (MonadIO m, MonadReader QueryAppContext m) => Maybe (C.Limit, C.Offset) -> Maybe C.PromotionFilter -> Maybe (PromotionsOrderBy, SortOrder) -> m [Promotion]
getPromotions maybeLimitOffset maybePromotionFilter maybeOrder = do
  pool <- asks pgPool
  liftIO $ runSqlPool (do
    rows <- select $ do
      pr <- from $ table @PromotionProjection
      applyPromotionFilter maybePromotionFilter pr
      case maybeOrder of
        Nothing -> pure ()
        Just (ob, so) ->
          case ob of
            PromotionsOrderById -> orderBy [orderByDir so (pr ^. PromotionProjectionPromotionId)]
            PromotionsOrderByBelt -> orderBy [orderByDir so (pr ^. PromotionProjectionPromotionBelt)]
            PromotionsOrderByAchievedBy -> orderBy [orderByDir so (pr ^. PromotionProjectionPromotionAchievedByProfileId)]
            PromotionsOrderByAwardedBy -> orderBy [orderByDir so (pr ^. PromotionProjectionPromotionAwardedByProfileId)]
            PromotionsOrderByDate -> orderBy [orderByDir so (pr ^. PromotionProjectionPromotionAchievementDate)]
      applyLimitOffset maybeLimitOffset
      pure pr
    let toPromotion p =
          Promotion
            { promotionId = promotionProjectionPromotionId p,
              promotionBelt = promotionProjectionPromotionBelt p,
              promotionAchievedByProfileId = promotionProjectionPromotionAchievedByProfileId p,
              promotionAwardedByProfileId = promotionProjectionPromotionAwardedByProfileId p,
              promotionAchievementDate = promotionProjectionPromotionAchievementDate p
            }
    pure (Prelude.map (toPromotion . entityVal) rows)
    ) pool

getPromotionsCount :: (MonadIO m, MonadReader QueryAppContext m) => Maybe C.PromotionFilter -> m Int
getPromotionsCount maybePromotionFilter = do
  pool <- asks pgPool
  liftIO $ runSqlPool (do
    cnt <- selectOne $ do
      pr <- from $ table @PromotionProjection
      applyPromotionFilter maybePromotionFilter pr
      pure countRows
    pure (maybe 0 unValue cnt)
    ) pool

getAchievements :: (MonadIO m, MonadReader QueryAppContext m) => Maybe (C.Limit, C.Offset) -> Maybe C.AchievementFilter -> Maybe (AchievementsOrderBy, SortOrder) -> m [Achievement]
getAchievements maybeLimitOffset maybeAchievementFilter maybeOrder = do
  pool <- asks pgPool
  liftIO $ runSqlPool (do
    rows <- select $ do
      ap <- from $ table @AchievementProjection
      applyAchievementFilter maybeAchievementFilter ap
      case maybeOrder of
        Nothing -> pure ()
        Just (ob, so) ->
          case ob of
            AchievementsOrderById -> orderBy [orderByDir so (ap ^. AchievementProjectionAchievementId)]
            AchievementsOrderByDate -> orderBy [orderByDir so (ap ^. AchievementProjectionAchievementDate)]
            AchievementsOrderByAwardedTo -> orderBy [orderByDir so (ap ^. AchievementProjectionAwardedToProfileId)]
            AchievementsOrderByAwardedBy -> orderBy [orderByDir so (ap ^. AchievementProjectionAwardedByProfileId)]
            AchievementsOrderByName -> orderBy [orderByDir so (ap ^. AchievementProjectionAchievementName)]
      applyLimitOffset maybeLimitOffset
      pure ap
    let toAchievement ap =
          Achievement
            { achievementId = achievementProjectionAchievementId ap,
              achievementAwardedTo = achievementProjectionAwardedToProfileId ap,
              achievementAwardedBy = achievementProjectionAwardedByProfileId ap,
              achievementDate = achievementProjectionAchievementDate ap,
              achievementIsAccepted = achievementProjectionIsAccepted ap,
              achievementName = achievementProjectionAchievementName ap,
              achievementDescription = achievementProjectionAchievementDescription ap,
              achievementImageURI = achievementProjectionAchievementImageURI ap
            }
    pure (Prelude.map (toAchievement . entityVal) rows)
    ) pool

getAchievementsCount :: (MonadIO m, MonadReader QueryAppContext m) => Maybe C.AchievementFilter -> m Int
getAchievementsCount maybeAchievementFilter = do
  pool <- asks pgPool
  liftIO $ runSqlPool (do
    cnt <- selectOne $ do
      ap <- from $ table @AchievementProjection
      applyAchievementFilter maybeAchievementFilter ap
      pure countRows
    pure (maybe 0 unValue cnt)
    ) pool

getRanks :: (MonadIO m, MonadReader QueryAppContext m) => Maybe (C.Limit, C.Offset) -> Maybe C.RankFilter -> Maybe (RanksOrderBy, SortOrder) -> m [Rank]
getRanks maybeLimitOffset maybeRankFilter maybeOrder = do
  pool <- asks pgPool
  liftIO $ runSqlPool (do
    rows <- select $ do
      rp <- from $ table @RankProjection
      applyRankFilter maybeRankFilter rp
      case maybeOrder of
        Nothing -> pure ()
        Just (ob, so) ->
          case ob of
            RanksOrderById -> orderBy [orderByDir so (rp ^. RankProjectionRankId)]
            RanksOrderByBelt -> orderBy [orderByDir so (rp ^. RankProjectionRankBelt)]
            RanksOrderByAchievedBy -> orderBy [orderByDir so (rp ^. RankProjectionRankAchievedByProfileId)]
            RanksOrderByAwardedBy -> orderBy [orderByDir so (rp ^. RankProjectionRankAwardedByProfileId)]
            RanksOrderByDate -> orderBy [orderByDir so (rp ^. RankProjectionRankAchievementDate)]
      applyLimitOffset maybeLimitOffset
      pure rp
    let toRank rp =
          Rank
            { rankId = rankProjectionRankId rp,
              rankBelt = rankProjectionRankBelt rp,
              rankAchievedByProfileId = rankProjectionRankAchievedByProfileId rp,
              rankAwardedByProfileId = rankProjectionRankAwardedByProfileId rp,
              rankAchievementDate = rankProjectionRankAchievementDate rp
            }
    pure (Prelude.map (toRank . entityVal) rows)
    ) pool

getRanksCount :: (MonadIO m, MonadReader QueryAppContext m) => Maybe C.RankFilter -> m Int
getRanksCount maybeRankFilter = do
  pool <- asks pgPool
  liftIO $ runSqlPool (do
    cnt <- selectOne $ do
      rp <- from $ table @RankProjection
      applyRankFilter maybeRankFilter rp
      pure countRows
    pure (maybe 0 unValue cnt)
    ) pool

getBeltTotals :: (MonadIO m, MonadReader QueryAppContext m) => m [(BJJBelt, Int)]
getBeltTotals = do
  pool <- asks pgPool
  liftIO $ runSqlPool (do
    rows <- select $ do
      rp <- from $ table @RankProjection
      pure (rp ^. RankProjectionRankBelt)
    let belts = Prelude.map unValue rows
    pure (toOccurList . fromList $ belts)
    ) pool

getProfileTypeTotals :: (MonadIO m, MonadReader QueryAppContext m) => m [(ProfileType, Int)]
getProfileTypeTotals = do
  pool <- asks pgPool
  liftIO $ runSqlPool (do
    rows <- select $ do
      pp <- from $ table @ProfileProjection
      pure (pp ^. ProfileProjectionProfileType)
    let profileTypes = Prelude.map unValue rows
    pure (toOccurList . fromList $ profileTypes)
    ) pool

getPromotionBeltTotals :: (MonadIO m, MonadReader QueryAppContext m) => m [(BJJBelt, Int)]
getPromotionBeltTotals = do
  pool <- asks pgPool
  liftIO $ runSqlPool (do
    rows <- select $ do
      pr <- from $ table @PromotionProjection
      pure (pr ^. PromotionProjectionPromotionBelt)
    let belts = Prelude.map unValue rows
    pure (toOccurList . fromList $ belts)
    ) pool

getMembershipHistoriesAsHistory :: (MonadIO m, MonadReader QueryAppContext m) => Maybe (C.Limit, C.Offset) -> Maybe C.MembershipHistoryFilter -> Maybe (MembershipHistoriesOrderBy, SortOrder) -> m [MembershipHistory]
getMembershipHistoriesAsHistory maybeLimitOffset maybeFilter maybeOrder = do
  infos <- getMembershipHistories maybeLimitOffset maybeFilter maybeOrder
  pure $ membershipHistoryInformationToHistory <$> infos
  where
    membershipHistoryInformationToHistory info =
      MembershipHistory
        { membershipHistoryId = membershipHistoryInformationId info,
          membershipHistoryPractitionerId = membershipHistoryInformationPractitionerId info,
          membershipHistoryOrganizationId = membershipHistoryInformationOrganizationId info
        }

getMembershipHistories :: (MonadIO m, MonadReader QueryAppContext m) => Maybe (C.Limit, C.Offset) -> Maybe C.MembershipHistoryFilter -> Maybe (MembershipHistoriesOrderBy, SortOrder) -> m [MembershipHistoryInformation]
getMembershipHistories maybeLimitOffset maybeFilter maybeOrder = do
  pool <- asks pgPool
  liftIO $ runSqlPool (do
    rows <- select $ do
      mhp <- from $ table @MembershipHistoryProjection
      applyMembershipHistoryFilter maybeFilter mhp
      case maybeOrder of
        Nothing -> pure ()
        Just (ob, so) ->
          case ob of
            MembershipHistoriesOrderById -> orderBy [orderByDir so (mhp ^. MembershipHistoryProjectionMembershipHistoryId)]
            MembershipHistoriesOrderByCreatedAt -> orderBy [orderByDir so (mhp ^. MembershipHistoryProjectionCreatedAtSlot)]
            MembershipHistoriesOrderByPractitioner -> orderBy [orderByDir so (mhp ^. MembershipHistoryProjectionPractitionerProfileId)]
            MembershipHistoriesOrderByOrganization -> orderBy [orderByDir so (mhp ^. MembershipHistoryProjectionOrganizationProfileId)]
      applyLimitOffset maybeLimitOffset
      pure mhp
    intervalByKey <- if null rows then pure [] else do
      let pids = nub $ map (membershipHistoryProjectionPractitionerProfileId . entityVal) rows
      intervalRows <- select $ do
        mip <- from $ table @MembershipIntervalProjection
        where_ (mip ^. MembershipIntervalProjectionPractitionerProfileId `in_` valList pids)
        orderBy [asc (mip ^. MembershipIntervalProjectionPractitionerProfileId), asc (mip ^. MembershipIntervalProjectionOrganizationProfileId), asc (mip ^. MembershipIntervalProjectionIntervalNumber)]
        pure mip
      let key (Entity _ mip) = (membershipIntervalProjectionOrganizationProfileId mip, membershipIntervalProjectionPractitionerProfileId mip)
          sorted = sortOn key intervalRows
          groups = Data.List.groupBy (\a b -> key a == key b) sorted
      pure $ map (\g -> (key (head g), g)) groups
    let toIntervalInfo mip =
          (\org ->
            MembershipIntervalInformation
              { membershipIntervalInformationId = membershipIntervalProjectionMembershipIntervalId mip,
                membershipIntervalInformationStartDate = membershipIntervalProjectionStartDate mip,
                membershipIntervalInformationEndDate = membershipIntervalProjectionEndDate mip,
                membershipIntervalInformationIsAccepted = membershipIntervalProjectionIsAccepted mip,
                membershipIntervalInformationPractitionerId = membershipIntervalProjectionPractitionerProfileId mip,
                membershipIntervalInformationNumber = membershipIntervalProjectionIntervalNumber mip,
                membershipIntervalInformationOrganizationId = org
              })
            <$> membershipIntervalProjectionOrganizationProfileId mip
    let buildInfo mhp =
          let k = (Just (membershipHistoryProjectionOrganizationProfileId mhp), membershipHistoryProjectionPractitionerProfileId mhp)
              intervalRowsForHistory = Data.Maybe.fromMaybe [] $ lookup k intervalByKey
              intervalInfos = mapMaybe (toIntervalInfo . entityVal) intervalRowsForHistory
           in MembershipHistoryInformation
                { membershipHistoryInformationId = membershipHistoryProjectionMembershipHistoryId mhp,
                  membershipHistoryInformationPractitionerId = membershipHistoryProjectionPractitionerProfileId mhp,
                  membershipHistoryInformationOrganizationId = membershipHistoryProjectionOrganizationProfileId mhp,
                  membershipHistoryInformationIntervals = intervalInfos
                }
    pure $ map (buildInfo . entityVal) rows
    ) pool

getMembershipHistoriesCount :: (MonadIO m, MonadReader QueryAppContext m) => Maybe C.MembershipHistoryFilter -> m Int
getMembershipHistoriesCount maybeFilter = do
  pool <- asks pgPool
  liftIO $ runSqlPool (do
    cnt <- selectOne $ do
      mhp <- from $ table @MembershipHistoryProjection
      applyMembershipHistoryFilter maybeFilter mhp
      pure countRows
    pure (maybe 0 unValue cnt)
    ) pool

getMembershipIntervalsAsInterval :: (MonadIO m, MonadReader QueryAppContext m) => Maybe (C.Limit, C.Offset) -> Maybe C.MembershipIntervalFilter -> Maybe (MembershipIntervalsOrderBy, SortOrder) -> m [MembershipInterval]
getMembershipIntervalsAsInterval maybeLimitOffset maybeFilter maybeOrder = do
  infos <- getMembershipIntervals maybeLimitOffset maybeFilter maybeOrder
  pure $ membershipIntervalInformationToInterval <$> infos
  where
    membershipIntervalInformationToInterval info =
      MembershipInterval
        { membershipIntervalId = membershipIntervalInformationId info,
          membershipIntervalStartDate = membershipIntervalInformationStartDate info,
          membershipIntervalEndDate = membershipIntervalInformationEndDate info,
          membershipIntervalIsAccepted = membershipIntervalInformationIsAccepted info,
          membershipIntervalPractitionerId = membershipIntervalInformationPractitionerId info,
          membershipIntervalNumber = membershipIntervalInformationNumber info
        }

getMembershipIntervals :: (MonadIO m, MonadReader QueryAppContext m) => Maybe (C.Limit, C.Offset) -> Maybe C.MembershipIntervalFilter -> Maybe (MembershipIntervalsOrderBy, SortOrder) -> m [MembershipIntervalInformation]
getMembershipIntervals maybeLimitOffset maybeFilter maybeOrder = do
  pool <- asks pgPool
  liftIO $ runSqlPool (do
    rows <- select $ do
      mip <- from $ table @MembershipIntervalProjection
      applyMembershipIntervalFilter maybeFilter mip
      case maybeOrder of
        Nothing -> pure ()
        Just (ob, so) ->
          case ob of
            MembershipIntervalsOrderById -> orderBy [orderByDir so (mip ^. MembershipIntervalProjectionMembershipIntervalId)]
            MembershipIntervalsOrderByStartDate -> orderBy [orderByDir so (mip ^. MembershipIntervalProjectionStartDate)]
            MembershipIntervalsOrderByIntervalNumber -> orderBy [orderByDir so (mip ^. MembershipIntervalProjectionIntervalNumber)]
            MembershipIntervalsOrderByPractitioner -> orderBy [orderByDir so (mip ^. MembershipIntervalProjectionPractitionerProfileId)]
      applyLimitOffset maybeLimitOffset
      pure mip
    let toIntervalInfo mip =
          (\org ->
            MembershipIntervalInformation
              { membershipIntervalInformationId = membershipIntervalProjectionMembershipIntervalId mip,
                membershipIntervalInformationStartDate = membershipIntervalProjectionStartDate mip,
                membershipIntervalInformationEndDate = membershipIntervalProjectionEndDate mip,
                membershipIntervalInformationIsAccepted = membershipIntervalProjectionIsAccepted mip,
                membershipIntervalInformationPractitionerId = membershipIntervalProjectionPractitionerProfileId mip,
                membershipIntervalInformationNumber = membershipIntervalProjectionIntervalNumber mip,
                membershipIntervalInformationOrganizationId = org
              })
            <$> membershipIntervalProjectionOrganizationProfileId mip
    pure (mapMaybe (toIntervalInfo . entityVal) rows)
    ) pool

getMembershipIntervalsCount :: (MonadIO m, MonadReader QueryAppContext m) => Maybe C.MembershipIntervalFilter -> m Int
getMembershipIntervalsCount maybeFilter = do
  pool <- asks pgPool
  liftIO $ runSqlPool (do
    cnt <- selectOne $ do
      mip <- from $ table @MembershipIntervalProjection
      applyMembershipIntervalFilter maybeFilter mip
      pure countRows
    pure (maybe 0 unValue cnt)
    ) pool
