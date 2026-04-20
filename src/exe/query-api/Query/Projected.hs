{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Redundant pure" #-}
{-# HLINT ignore "Fuse on/on" #-}

module Query.Projected where

import Control.Exception (throwIO)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader.Class
import Data.Aeson (ToJSON)
import Data.Foldable (for_)
import Data.List (foldl', maximumBy, nub, sortOn)
import Data.List qualified as L (groupBy)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Ord (Down (..), comparing)
import Data.Sequence qualified as Seq
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Database.Esqueleto.Experimental
import Database.Esqueleto.Internal.Internal (unsafeSqlCastAs, unsafeSqlFunction)
import Database.Persist qualified as P
import DomainTypes.Core.BJJ (BJJBelt (..))
import DomainTypes.Core.Types
import DomainTypes.Transfer.Filters qualified as C
import DomainTypes.Transfer.Filters qualified as F
import DomainTypes.Transfer.OrderBy
import DomainTypes.Transfer.QueryResponses
  ( AchievementsMonthlyStatsResponse (..),
    ActivityEventDetails (..),
    ActivityEventResponse (..),
    LineageEdge (..),
    LineageGraphData (..),
    LineageNode (..),
    MonthlyPromotionsDataResponse (..),
    SearchGroup (..),
    SearchHit (..),
    SearchResults (..),
    SearchTarget (..),
  )
import DomainTypes.Transfer.Types
import GeniusYield.Types (GYTime, timeFromPOSIX, timeToPOSIX)
import Query.Common qualified as C
import Query.ExplorerMonthly (monthlyAchievementsFromList, monthlyPromotionsFromList)
import QueryAppMonad
import Storage
import TxBuilding.Exceptions (TxBuildingException (..))
import Utils (stringFromJSON)

-- | Convert a 'RankProjection' row to a domain 'Rank'.
toRankDomain :: RankProjection -> Rank
toRankDomain rp =
  Rank
    { rankId = rankProjectionRankId rp,
      rankBelt = rankProjectionRankBelt rp,
      rankAchievedByProfileId = rankProjectionRankAchievedByProfileId rp,
      rankAwardedByProfileId = rankProjectionRankAwardedByProfileId rp,
      rankAchievementDate = rankProjectionRankAchievementDate rp
    }

-- | Convert a 'PromotionProjection' row to a domain 'Promotion' given a belt map for state derivation.
toPromotionDomain :: M.Map ProfileRefAC (Maybe BJJBelt) -> PromotionProjection -> Promotion
toPromotionDomain beltMap p =
  Promotion
    { promotionId = promotionProjectionPromotionId p,
      promotionBelt = promotionProjectionPromotionBelt p,
      promotionAchievedByProfileId = promotionProjectionPromotionAchievedByProfileId p,
      promotionAwardedByProfileId = promotionProjectionPromotionAwardedByProfileId p,
      promotionAchievementDate = promotionProjectionPromotionAchievementDate p,
      promotionState =
        promotionStateFromBelts
          (M.findWithDefault Nothing (promotionProjectionPromotionAchievedByProfileId p) beltMap)
          (promotionProjectionPromotionBelt p)
    }

-- | Build a case-insensitive SQL LIKE pattern for text search: @%<lower q>%@.
likePat :: Text -> SqlExpr (Value Text)
likePat q = val (T.pack "%" <> T.toLower q <> T.pack "%")

-- | Latest rank by @rankAchievementDate@ per practitioner (same rule as practitioner profile).
currentBeltMapForPractitioners ::
  (MonadIO m) =>
  [ProfileRefAC] ->
  SqlPersistT m (M.Map ProfileRefAC (Maybe BJJBelt))
currentBeltMapForPractitioners [] = return M.empty
currentBeltMapForPractitioners pids = do
  let distinctPids = nub pids
  rankEntities <- select $ do
    rp <- from $ table @RankProjection
    where_ (rp ^. RankProjectionRankAchievedByProfileId `in_` valList distinctPids)
    pure rp
  let rows = map entityVal rankEntities
      beltFor pid =
        let rs = filter ((== pid) . rankProjectionRankAchievedByProfileId) rows
         in case rs of
              [] -> Nothing
              xs -> Just $ rankProjectionRankBelt $ maximumBy (comparing rankProjectionRankAchievementDate) xs
  return $ M.fromList [(pid, beltFor pid) | pid <- distinctPids]

-- | Apply sort direction (Asc/Desc) to an expression for orderBy.
orderByDir :: (PersistField a) => SortOrder -> SqlExpr (Value a) -> SqlExpr OrderBy
orderByDir so expr = case so of Asc -> asc expr; Desc -> desc expr

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

applyPromotionFilter :: Maybe F.PromotionFilter -> SqlExpr (Entity PromotionProjection) -> SqlQuery ()
applyPromotionFilter Nothing _ = pure ()
applyPromotionFilter (Just F.PromotionFilter {..}) pr = do
  for_ promotionFilterId (\ids -> where_ (pr ^. PromotionProjectionPromotionId `in_` valList ids))
  for_ promotionFilterBelt (\belts -> where_ (pr ^. PromotionProjectionPromotionBelt `in_` valList belts))
  for_ promotionFilterAchievedByProfileId (\ids -> where_ (pr ^. PromotionProjectionPromotionAchievedByProfileId `in_` valList ids))
  for_ promotionFilterAwardedByProfileId (\ids -> where_ (pr ^. PromotionProjectionPromotionAwardedByProfileId `in_` valList ids))
  whereDateInterval promotionFilterAchievementDateInterval (pr ^. PromotionProjectionPromotionAchievementDate)
  for_
    promotionFilterTextSearch
    ( \q ->
        let pat = likePat q
         in where_ $
              (lower_ (unsafeSqlCastAs "text" (pr ^. PromotionProjectionPromotionId)) `like` pat)
                ||. (lower_ (unsafeSqlCastAs "text" (pr ^. PromotionProjectionPromotionAchievedByProfileId)) `like` pat)
                ||. (lower_ (unsafeSqlCastAs "text" (pr ^. PromotionProjectionPromotionAwardedByProfileId)) `like` pat)
                ||. (lower_ (unsafeSqlCastAs "text" (pr ^. PromotionProjectionPromotionBelt)) `like` pat)
    )

-- | Apply a PromotionFilter's shared fields (id, belt, achievedBy, awardedBy, date, text)
-- to a RankProjection query. Used by the unified getPromotions to query accepted ranks.
applyPromotionFilterOnRank :: Maybe F.PromotionFilter -> SqlExpr (Entity RankProjection) -> SqlQuery ()
applyPromotionFilterOnRank Nothing _ = pure ()
applyPromotionFilterOnRank (Just F.PromotionFilter {..}) rp = do
  for_ promotionFilterId (\ids -> where_ (rp ^. RankProjectionRankId `in_` valList ids))
  for_ promotionFilterBelt (\belts -> where_ (rp ^. RankProjectionRankBelt `in_` valList belts))
  for_ promotionFilterAchievedByProfileId (\ids -> where_ (rp ^. RankProjectionRankAchievedByProfileId `in_` valList ids))
  for_ promotionFilterAwardedByProfileId (\ids -> where_ (rp ^. RankProjectionRankAwardedByProfileId `in_` valList ids))
  whereDateInterval promotionFilterAchievementDateInterval (rp ^. RankProjectionRankAchievementDate)
  for_
    promotionFilterTextSearch
    ( \q ->
        let pat = likePat q
         in where_ $
              (lower_ (unsafeSqlCastAs "text" (rp ^. RankProjectionRankId)) `like` pat)
                ||. (lower_ (unsafeSqlCastAs "text" (rp ^. RankProjectionRankAchievedByProfileId)) `like` pat)
                ||. (lower_ (unsafeSqlCastAs "text" (rp ^. RankProjectionRankAwardedByProfileId)) `like` pat)
                ||. (lower_ (unsafeSqlCastAs "text" (rp ^. RankProjectionRankBelt)) `like` pat)
    )

-- | Apply ordering to a list of promotions (used after merging pending + accepted).
applyPromotionOrdering :: Maybe (PromotionsOrderBy, SortOrder) -> [Promotion] -> [Promotion]
applyPromotionOrdering Nothing ps = ps
applyPromotionOrdering (Just (ob, so)) ps =
  let sorted = case ob of
        PromotionsOrderById -> sortOn promotionId ps
        PromotionsOrderByBelt -> sortOn promotionBelt ps
        PromotionsOrderByAchievedBy -> sortOn promotionAchievedByProfileId ps
        PromotionsOrderByAwardedBy -> sortOn promotionAwardedByProfileId ps
        PromotionsOrderByDate -> sortOn promotionAchievementDate ps
   in case so of
        Asc -> sorted
        Desc -> reverse sorted

applyAchievementFilter :: Maybe F.AchievementFilter -> SqlExpr (Entity AchievementProjection) -> SqlQuery ()
applyAchievementFilter Nothing _ = pure ()
applyAchievementFilter (Just F.AchievementFilter {..}) ap = do
  for_ achievementFilterId (\ids -> where_ (ap ^. AchievementProjectionAchievementId `in_` valList ids))
  for_ achievementFilterAwardedToProfileId (\ids -> where_ (ap ^. AchievementProjectionAwardedToProfileId `in_` valList ids))
  for_ achievementFilterAwardedByProfileId (\ids -> where_ (ap ^. AchievementProjectionAwardedByProfileId `in_` valList ids))
  for_ achievementFilterIsAccepted (\ac -> where_ (ap ^. AchievementProjectionIsAccepted ==. val ac))
  whereDateInterval achievementFilterDateInterval (ap ^. AchievementProjectionAchievementDate)
  for_
    achievementFilterTextSearch
    ( \q ->
        let pat = likePat q
         in where_ $
              (lower_ (unsafeSqlCastAs "text" (ap ^. AchievementProjectionAchievementId)) `like` pat)
                ||. (lower_ (unsafeSqlCastAs "text" (ap ^. AchievementProjectionAwardedToProfileId)) `like` pat)
                ||. (lower_ (unsafeSqlCastAs "text" (ap ^. AchievementProjectionAwardedByProfileId)) `like` pat)
                ||. (lower_ (ap ^. AchievementProjectionAchievementName) `like` pat)
                ||. (lower_ (ap ^. AchievementProjectionAchievementDescription) `like` pat)
    )

applyMembershipHistoryFilter :: Maybe F.MembershipHistoryFilter -> SqlExpr (Entity MembershipHistoryProjection) -> SqlQuery ()
applyMembershipHistoryFilter Nothing _ = pure ()
applyMembershipHistoryFilter (Just F.MembershipHistoryFilter {..}) mhp = do
  for_ membershipHistoryFilterOrganizationProfileId (\ids -> where_ (mhp ^. MembershipHistoryProjectionOrganizationProfileId `in_` valList ids))
  for_ membershipHistoryFilterPractitionerProfileId (\ids -> where_ (mhp ^. MembershipHistoryProjectionPractitionerProfileId `in_` valList ids))
  for_
    membershipHistoryFilterTextSearch
    ( \q ->
        let pat = likePat q
         in where_ $
              (lower_ (unsafeSqlCastAs "text" (mhp ^. MembershipHistoryProjectionMembershipHistoryId)) `like` pat)
                ||. (lower_ (unsafeSqlCastAs "text" (mhp ^. MembershipHistoryProjectionPractitionerProfileId)) `like` pat)
                ||. (lower_ (unsafeSqlCastAs "text" (mhp ^. MembershipHistoryProjectionOrganizationProfileId)) `like` pat)
    )

applyMembershipIntervalFilter :: Maybe F.MembershipIntervalFilter -> SqlExpr (Entity MembershipIntervalProjection) -> SqlQuery ()
applyMembershipIntervalFilter Nothing _ = pure ()
applyMembershipIntervalFilter (Just F.MembershipIntervalFilter {..}) mip = do
  for_ membershipIntervalFilterPractitionerProfileId (\ids -> where_ (mip ^. MembershipIntervalProjectionPractitionerProfileId `in_` valList ids))
  for_ membershipIntervalFilterIsAccepted (\acc -> where_ (mip ^. MembershipIntervalProjectionIsAccepted ==. val acc))
  for_
    membershipIntervalFilterTextSearch
    ( \q ->
        let pat = likePat q
         in where_ $
              (lower_ (unsafeSqlCastAs "text" (mip ^. MembershipIntervalProjectionMembershipIntervalId)) `like` pat)
                ||. (lower_ (unsafeSqlCastAs "text" (mip ^. MembershipIntervalProjectionPractitionerProfileId)) `like` pat)
    )

applyProfileFilter :: Maybe F.ProfileFilter -> SqlExpr (Entity ProfileProjection) -> SqlQuery ()
applyProfileFilter Nothing _ = pure ()
applyProfileFilter (Just F.ProfileFilter {..}) pp = do
  for_ profileFilterId (\ids -> where_ (pp ^. ProfileProjectionProfileId `in_` valList ids))
  for_ profileFilterType (\pt -> where_ (pp ^. ProfileProjectionProfileType ==. val pt))
  for_ profileFilterName (\nameSubstring -> where_ (lower_ (pp ^. ProfileProjectionProfileName) `like` likePat nameSubstring))
  for_ profileFilterDescription (\descriptionSubstring -> where_ (lower_ (pp ^. ProfileProjectionProfileDescription) `like` likePat descriptionSubstring))
  -- Text search: name, description, profile id; for Practitioners only, also match affiliated org id/name (§1.8)
  for_
    profileFilterTextSearch
    ( \q ->
        let pat = likePat q
            selfMatch =
              (lower_ (pp ^. ProfileProjectionProfileName) `like` pat)
                ||. (lower_ (pp ^. ProfileProjectionProfileDescription) `like` pat)
                ||. (lower_ (unsafeSqlCastAs "text" (pp ^. ProfileProjectionProfileId)) `like` pat)
         in case profileFilterType of
              Just Practitioner ->
                where_ $
                  selfMatch
                    ||. ( pp
                            ^. ProfileProjectionProfileId
                            `in_` subSelectList
                              ( do
                                  mhp <- from $ table @MembershipHistoryProjection
                                  org <- from $ table @ProfileProjection
                                  where_ (mhp ^. MembershipHistoryProjectionOrganizationProfileId ==. org ^. ProfileProjectionProfileId)
                                  where_ (org ^. ProfileProjectionProfileType ==. val Organization)
                                  where_ $
                                    (lower_ (org ^. ProfileProjectionProfileName) `like` pat)
                                      ||. (lower_ (unsafeSqlCastAs "text" (org ^. ProfileProjectionProfileId)) `like` pat)
                                  pure (mhp ^. MembershipHistoryProjectionPractitionerProfileId)
                              )
                        )
              _ -> where_ selfMatch
    )
  -- Filter by belt: profile must have a rank with matching belt
  for_
    profileFilterBelt
    ( \belts ->
        where_ $
          pp
            ^. ProfileProjectionProfileId
            `in_` subSelectList
              ( do
                  rp <- from $ table @RankProjection
                  where_ (rp ^. RankProjectionRankBelt `in_` valList belts)
                  pure (rp ^. RankProjectionRankAchievedByProfileId)
              )
    )
  -- Filter by membership_organization (all-time): profile has any membership history at org
  for_
    profileFilterMembershipOrganization
    ( \orgId ->
        where_ $
          pp
            ^. ProfileProjectionProfileId
            `in_` subSelectList
              ( do
                  mhp <- from $ table @MembershipHistoryProjection
                  where_ (mhp ^. MembershipHistoryProjectionOrganizationProfileId ==. val orgId)
                  pure (mhp ^. MembershipHistoryProjectionPractitionerProfileId)
              )
    )
  -- Filter by active_membership_organization: accepted interval at org with no end or end >= CURRENT_TIMESTAMP (§3.1)
  for_
    profileFilterActiveMembershipOrganization
    ( \orgId ->
        where_ $
          pp
            ^. ProfileProjectionProfileId
            `in_` subSelectList
              ( do
                  mip <- from $ table @MembershipIntervalProjection
                  where_ (mip ^. MembershipIntervalProjectionOrganizationProfileId ==. just (val orgId))
                  where_ (mip ^. MembershipIntervalProjectionIsAccepted ==. val True)
                  let nowTs = unsafeSqlFunction "NOW" ()
                  where_ $
                    (isNothing_ (mip ^. MembershipIntervalProjectionEndDate))
                      ||. (mip ^. MembershipIntervalProjectionEndDate >=. just nowTs)
                  pure (mip ^. MembershipIntervalProjectionPractitionerProfileId)
              )
    )
  -- Filter by registration date (insertedAt >= cutoff)
  for_ profileFilterRegisteredAfter (\cutoff -> where_ (pp ^. ProfileProjectionInsertedAt >=. val cutoff))

getPractitionerProfile :: (MonadIO m, MonadReader QueryAppContext m) => ProfileRefAC -> m PractitionerProfileInformation
getPractitionerProfile profileRefAC = do
  pool <- asks pgPool
  liftIO $
    runSqlPool
      ( do
          mProf <- P.getBy (UniqueProfileProjection profileRefAC)
          case mProf of
            Nothing -> liftIO $ throwIO ProfileNotFound
            Just (Entity _ prof) -> do
              ranksAsc <- select $ do
                rp <- from $ table @RankProjection
                where_ (rp ^. RankProjectionRankAchievedByProfileId ==. val profileRefAC)
                orderBy [asc (rp ^. RankProjectionRankAchievementDate)]
                pure rp
              let domainRanks = Prelude.map (toRankDomain . entityVal) ranksAsc
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
      )
      pool

-- | Batch-load practitioner profiles for a list of IDs (two SQL queries: profiles + ranks).
-- Skips IDs that are not found (no exception). Used by explorer page to avoid N+1.
getPractitionerProfilesBatch ::
  (MonadIO m, MonadReader QueryAppContext m) =>
  [ProfileRefAC] ->
  m (M.Map ProfileRefAC PractitionerProfileInformation)
getPractitionerProfilesBatch [] = return M.empty
getPractitionerProfilesBatch pids = do
  pool <- asks pgPool
  let uniqPids = nub pids
  liftIO $
    runSqlPool
      ( do
          profRows <- select $ do
            pp <- from $ table @ProfileProjection
            where_ (pp ^. ProfileProjectionProfileId `in_` valList uniqPids)
            pure pp
          rankRows <- select $ do
            rp <- from $ table @RankProjection
            where_ (rp ^. RankProjectionRankAchievedByProfileId `in_` valList uniqPids)
            orderBy [asc (rp ^. RankProjectionRankAchievementDate)]
            pure rp
          let ranksByPid =
                M.fromListWith
                  (flip (++))
                  [ (rankProjectionRankAchievedByProfileId (entityVal rp), [toRankDomain (entityVal rp)])
                  | rp <- rankRows
                  ]
              buildInfo prof =
                let pid = profileProjectionProfileId prof
                    domainRanks = M.findWithDefault [] pid ranksByPid
                 in case Prelude.reverse domainRanks of
                      [] -> Nothing
                      (current : restRev) ->
                        Just
                          ( pid,
                            PractitionerProfileInformation
                              { practitionerId = pid,
                                practitionerName = profileProjectionProfileName prof,
                                practitionerDescription = profileProjectionProfileDescription prof,
                                practitionerImageURI = profileProjectionProfileImageURI prof,
                                practitionerCurrentRank = current,
                                practitionerPreviousRanks = Prelude.reverse restRev
                              }
                          )
          return $ M.fromList $ mapMaybe (buildInfo . entityVal) profRows
      )
      pool

-- | Batch-load organization profiles for a list of IDs (single SQL query).
-- Skips IDs that are not found. Used by aggregate map builders to avoid N+1.
getOrganizationProfilesBatch ::
  (MonadIO m, MonadReader QueryAppContext m) =>
  [ProfileRefAC] ->
  m (M.Map ProfileRefAC OrganizationProfileInformation)
getOrganizationProfilesBatch [] = return M.empty
getOrganizationProfilesBatch oids = do
  pool <- asks pgPool
  liftIO $
    runSqlPool
      ( do
          rows <- select $ do
            pp <- from $ table @ProfileProjection
            where_ (pp ^. ProfileProjectionProfileId `in_` valList (nub oids))
            where_ (pp ^. ProfileProjectionProfileType ==. val Organization)
            pure pp
          return $
            M.fromList
              [ ( profileProjectionProfileId prof,
                  OrganizationProfileInformation
                    { organizationId = profileProjectionProfileId prof,
                      organizationName = profileProjectionProfileName prof,
                      organizationDescription = profileProjectionProfileDescription prof,
                      organizationImageURI = profileProjectionProfileImageURI prof
                    }
                )
              | Entity _ prof <- rows
              ]
      )
      pool

getOrganizationProfile :: (MonadIO m, MonadReader QueryAppContext m) => ProfileRefAC -> m OrganizationProfileInformation
getOrganizationProfile profileRefAC = do
  pool <- asks pgPool
  liftIO $
    runSqlPool
      ( do
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
      )
      pool

getProfilesCount :: (MonadIO m, MonadReader QueryAppContext m) => Maybe F.ProfileFilter -> m Int
getProfilesCount maybeProfileFilter = do
  pool <- asks pgPool
  liftIO $
    runSqlPool
      ( do
          cnt <- selectOne $ do
            pp <- from $ table @ProfileProjection
            applyProfileFilter maybeProfileFilter pp
            pure countRows
          pure (maybe 0 unValue cnt)
      )
      pool

getProfiles :: (MonadIO m, MonadReader QueryAppContext m) => Maybe (C.Limit, C.Offset) -> Maybe F.ProfileFilter -> Maybe (ProfilesOrderBy, SortOrder) -> m [Profile]
getProfiles maybeLimitOffset maybeProfileFilter maybeOrder = do
  pool <- asks pgPool
  liftIO $
    runSqlPool
      ( do
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
                  ProfilesOrderByRegisteredAt -> orderBy [orderByDir so (pp ^. ProfileProjectionInsertedAt)]
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
      )
      pool

-- | Unified promotions query: fetches pending/superseded from PromotionProjection and
-- accepted from RankProjection (via 'rankToPromotion'), merges, applies state filter,
-- ordering, and limit/offset in Haskell.
-- Self-promotions (where awardedBy == achievedBy) are excluded as they are not legitimate.
getPromotions :: (MonadIO m, MonadReader QueryAppContext m) => Maybe (C.Limit, C.Offset) -> Maybe F.PromotionFilter -> Maybe (PromotionsOrderBy, SortOrder) -> m [Promotion]
getPromotions maybeLimitOffset maybePromotionFilter maybeOrder = do
  pool <- asks pgPool
  -- Strip state filter for SQL queries (applied post-merge in Haskell)
  let sqlFilter = fmap (\f -> f {C.promotionFilterState = Nothing}) maybePromotionFilter
      stateFilter = maybePromotionFilter >>= C.promotionFilterState
      wantsAccepted = maybe True (elem PromotionAccepted) stateFilter
      wantsPendingOrSuperseded = maybe True (\ss -> PromotionPending `elem` ss || PromotionSuperseded `elem` ss) stateFilter
      -- Strip text search so applyPromotionFilter/applyPromotionFilterOnRank skip their
      -- text-only WHERE clauses; we handle text search via LEFT JOINs to ProfileProjection.
      nonTextFilter = fmap (\f -> f {C.promotionFilterTextSearch = Nothing}) sqlFilter
      mTextSearch = sqlFilter >>= C.promotionFilterTextSearch
  pendingPromotions <-
    if wantsPendingOrSuperseded
      then
        liftIO $
          runSqlPool
            ( do
                rows <- select $ do
                  case mTextSearch of
                    Nothing -> do
                      pr <- from $ table @PromotionProjection
                      applyPromotionFilter nonTextFilter pr
                      pure pr
                    Just q -> do
                      (pr :& mppAchieved :& mppAwarded) <-
                        from $
                          table @PromotionProjection
                            `leftJoin` table @ProfileProjection
                            `on` (\(pr :& ppA) -> just (pr ^. PromotionProjectionPromotionAchievedByProfileId) ==. ppA ?. ProfileProjectionProfileId)
                            `leftJoin` table @ProfileProjection
                            `on` (\(pr :& _ :& ppW) -> just (pr ^. PromotionProjectionPromotionAwardedByProfileId) ==. ppW ?. ProfileProjectionProfileId)
                      applyPromotionFilter nonTextFilter pr
                      let pat = likePat q
                      where_ $
                        (lower_ (unsafeSqlCastAs "text" (pr ^. PromotionProjectionPromotionId)) `like` pat)
                          ||. (lower_ (unsafeSqlCastAs "text" (pr ^. PromotionProjectionPromotionAchievedByProfileId)) `like` pat)
                          ||. (lower_ (unsafeSqlCastAs "text" (pr ^. PromotionProjectionPromotionAwardedByProfileId)) `like` pat)
                          ||. (lower_ (unsafeSqlCastAs "text" (pr ^. PromotionProjectionPromotionBelt)) `like` pat)
                          ||. (lower_ (coalesceDefault [mppAchieved ?. ProfileProjectionProfileName] (val "")) `like` pat)
                          ||. (lower_ (coalesceDefault [mppAwarded ?. ProfileProjectionProfileName] (val "")) `like` pat)
                      pure pr
                let pvals = map entityVal rows
                    pids = nub $ map promotionProjectionPromotionAchievedByProfileId pvals
                beltMap <- currentBeltMapForPractitioners pids
                pure (Prelude.map (toPromotionDomain beltMap) pvals)
            )
            pool
      else pure []
  acceptedPromotions <-
    if wantsAccepted
      then
        liftIO $
          runSqlPool
            ( do
                rows <- select $ do
                  case mTextSearch of
                    Nothing -> do
                      rp <- from $ table @RankProjection
                      applyPromotionFilterOnRank nonTextFilter rp
                      pure rp
                    Just q -> do
                      (rp :& mppAchieved :& mppAwarded) <-
                        from $
                          table @RankProjection
                            `leftJoin` table @ProfileProjection
                            `on` (\(rp :& ppA) -> just (rp ^. RankProjectionRankAchievedByProfileId) ==. ppA ?. ProfileProjectionProfileId)
                            `leftJoin` table @ProfileProjection
                            `on` (\(rp :& _ :& ppW) -> just (rp ^. RankProjectionRankAwardedByProfileId) ==. ppW ?. ProfileProjectionProfileId)
                      applyPromotionFilterOnRank nonTextFilter rp
                      let pat = likePat q
                      where_ $
                        (lower_ (unsafeSqlCastAs "text" (rp ^. RankProjectionRankId)) `like` pat)
                          ||. (lower_ (unsafeSqlCastAs "text" (rp ^. RankProjectionRankAchievedByProfileId)) `like` pat)
                          ||. (lower_ (unsafeSqlCastAs "text" (rp ^. RankProjectionRankAwardedByProfileId)) `like` pat)
                          ||. (lower_ (unsafeSqlCastAs "text" (rp ^. RankProjectionRankBelt)) `like` pat)
                          ||. (lower_ (coalesceDefault [mppAchieved ?. ProfileProjectionProfileName] (val "")) `like` pat)
                          ||. (lower_ (coalesceDefault [mppAwarded ?. ProfileProjectionProfileName] (val "")) `like` pat)
                      pure rp
                pure (Prelude.map (rankToPromotion . toRankDomain . entityVal) rows)
            )
            pool
      else pure []
  -- Self-promotions (awardedBy == achievedBy) are not legitimate; filter them out.
  let merged = Prelude.filter (\p -> promotionAchievedByProfileId p /= promotionAwardedByProfileId p) $ pendingPromotions ++ acceptedPromotions
      afterState = case stateFilter of
        Nothing -> merged
        Just ss -> let sSet = S.fromList ss in Prelude.filter ((`S.member` sSet) . promotionState) merged
      afterOrder = applyPromotionOrdering maybeOrder afterState
  pure $ C.applyLimits maybeLimitOffset afterOrder

getPromotionsCount :: (MonadIO m, MonadReader QueryAppContext m) => Maybe F.PromotionFilter -> m Int
getPromotionsCount maybePromotionFilter = Prelude.length <$> getPromotions Nothing maybePromotionFilter Nothing

getAchievements :: (MonadIO m, MonadReader QueryAppContext m) => Maybe (C.Limit, C.Offset) -> Maybe F.AchievementFilter -> Maybe (AchievementsOrderBy, SortOrder) -> m [Achievement]
getAchievements maybeLimitOffset maybeAchievementFilter maybeOrder = do
  pool <- asks pgPool
  liftIO $
    runSqlPool
      ( do
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
                    achievementAwardedToProfileId = achievementProjectionAwardedToProfileId ap,
                    achievementAwardedByProfileId = achievementProjectionAwardedByProfileId ap,
                    achievementAchievementDate = achievementProjectionAchievementDate ap,
                    achievementAccepted = achievementProjectionIsAccepted ap,
                    achievementName = achievementProjectionAchievementName ap,
                    achievementDescription = achievementProjectionAchievementDescription ap,
                    achievementImageURI = achievementProjectionAchievementImageURI ap,
                    achievementOtherMetadata =
                      maybe
                        []
                        fromAchievementOtherMetadataJson
                        (achievementProjectionOtherMetadata ap)
                  }
          pure (Prelude.map (toAchievement . entityVal) rows)
      )
      pool

getAchievementsCount :: (MonadIO m, MonadReader QueryAppContext m) => Maybe F.AchievementFilter -> m Int
getAchievementsCount maybeAchievementFilter = do
  pool <- asks pgPool
  liftIO $
    runSqlPool
      ( do
          cnt <- selectOne $ do
            ap <- from $ table @AchievementProjection
            applyAchievementFilter maybeAchievementFilter ap
            pure countRows
          pure (maybe 0 unValue cnt)
      )
      pool

-- | Internal: get all ranks from RankProjection (used by lineage, profile belt filtering).
getAllRanksProjected :: (MonadIO m, MonadReader QueryAppContext m) => m [Rank]
getAllRanksProjected = do
  pool <- asks pgPool
  liftIO $
    runSqlPool
      ( do
          rows <- select $ do
            rp <- from $ table @RankProjection
            pure rp
          pure (Prelude.map (toRankDomain . entityVal) rows)
      )
      pool

-- | Internal: count all ranks (used by dashboard).
getRanksCount :: (MonadIO m, MonadReader QueryAppContext m) => m Int
getRanksCount = do
  pool <- asks pgPool
  liftIO $
    runSqlPool
      ( do
          cnt <- selectOne $ do
            _ <- from $ table @RankProjection
            pure countRows
          pure (maybe 0 unValue cnt)
      )
      pool

-- | Internal: belt frequency from accepted ranks only (used by dashboard).
getBeltTotals :: (MonadIO m, MonadReader QueryAppContext m) => m [(BJJBelt, Int)]
getBeltTotals = do
  pool <- asks pgPool
  liftIO $
    runSqlPool
      ( do
          rows <- select $ do
            rp <- from $ table @RankProjection
            groupBy (rp ^. RankProjectionRankBelt)
            pure (rp ^. RankProjectionRankBelt, countRows)
          pure [(unValue b, unValue c) | (b, c) <- rows]
      )
      pool

getProfileTypeTotals :: (MonadIO m, MonadReader QueryAppContext m) => m [(ProfileType, Int)]
getProfileTypeTotals = do
  pool <- asks pgPool
  liftIO $
    runSqlPool
      ( do
          rows <- select $ do
            pp <- from $ table @ProfileProjection
            groupBy (pp ^. ProfileProjectionProfileType)
            pure (pp ^. ProfileProjectionProfileType, countRows)
          pure [(unValue t, unValue c) | (t, c) <- rows]
      )
      pool

-- | Unified belt frequency: counts from both PromotionProjection and RankProjection.
getPromotionBeltTotals :: (MonadIO m, MonadReader QueryAppContext m) => m [(BJJBelt, Int)]
getPromotionBeltTotals = do
  pool <- asks pgPool
  liftIO $
    runSqlPool
      ( do
          promoRows <- select $ do
            pr <- from $ table @PromotionProjection
            groupBy (pr ^. PromotionProjectionPromotionBelt)
            pure (pr ^. PromotionProjectionPromotionBelt, countRows)
          rankRows <- select $ do
            rp <- from $ table @RankProjection
            groupBy (rp ^. RankProjectionRankBelt)
            pure (rp ^. RankProjectionRankBelt, countRows)
          let promoMap = M.fromList [(unValue b, unValue c) | (b, c) <- promoRows]
              rankMap = M.fromList [(unValue b, unValue c) | (b, c) <- rankRows]
          pure $ M.toList $ M.unionWith (+) promoMap rankMap
      )
      pool

getMembershipHistoriesAsHistory :: (MonadIO m, MonadReader QueryAppContext m) => Maybe (C.Limit, C.Offset) -> Maybe F.MembershipHistoryFilter -> Maybe (MembershipHistoriesOrderBy, SortOrder) -> m [MembershipHistory]
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

getMembershipHistories :: (MonadIO m, MonadReader QueryAppContext m) => Maybe (C.Limit, C.Offset) -> Maybe F.MembershipHistoryFilter -> Maybe (MembershipHistoriesOrderBy, SortOrder) -> m [MembershipHistoryInformation]
getMembershipHistories maybeLimitOffset maybeFilter maybeOrder = do
  pool <- asks pgPool
  liftIO $
    runSqlPool
      ( do
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
          intervalByKey <-
            if null rows
              then pure []
              else do
                let pids = nub $ map (membershipHistoryProjectionPractitionerProfileId . entityVal) rows
                intervalRows <- select $ do
                  mip <- from $ table @MembershipIntervalProjection
                  where_ (mip ^. MembershipIntervalProjectionPractitionerProfileId `in_` valList pids)
                  orderBy [asc (mip ^. MembershipIntervalProjectionPractitionerProfileId), asc (mip ^. MembershipIntervalProjectionOrganizationProfileId), asc (mip ^. MembershipIntervalProjectionIntervalNumber)]
                  pure mip
                let key (Entity _ mip) = (membershipIntervalProjectionOrganizationProfileId mip, membershipIntervalProjectionPractitionerProfileId mip)
                    sorted = sortOn key intervalRows
                    groups = L.groupBy (\a b -> key a == key b) sorted
                -- groupBy never produces empty sublists, but pattern match is total
                pure [(key x, x : xs) | (x : xs) <- groups]
          let toIntervalInfo mip =
                ( \org ->
                    MembershipIntervalInformation
                      { membershipIntervalInformationId = membershipIntervalProjectionMembershipIntervalId mip,
                        membershipIntervalInformationStartDate = membershipIntervalProjectionStartDate mip,
                        membershipIntervalInformationEndDate = membershipIntervalProjectionEndDate mip,
                        membershipIntervalInformationAccepted = membershipIntervalProjectionIsAccepted mip,
                        membershipIntervalInformationPractitionerId = membershipIntervalProjectionPractitionerProfileId mip,
                        membershipIntervalInformationIntervalNumber = membershipIntervalProjectionIntervalNumber mip,
                        membershipIntervalInformationOrganizationId = org
                      }
                )
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
      )
      pool

getMembershipHistoriesCount :: (MonadIO m, MonadReader QueryAppContext m) => Maybe F.MembershipHistoryFilter -> m Int
getMembershipHistoriesCount maybeFilter = do
  pool <- asks pgPool
  liftIO $
    runSqlPool
      ( do
          cnt <- selectOne $ do
            mhp <- from $ table @MembershipHistoryProjection
            applyMembershipHistoryFilter maybeFilter mhp
            pure countRows
          pure (maybe 0 unValue cnt)
      )
      pool

getMembershipIntervalsAsInterval :: (MonadIO m, MonadReader QueryAppContext m) => Maybe (C.Limit, C.Offset) -> Maybe F.MembershipIntervalFilter -> Maybe (MembershipIntervalsOrderBy, SortOrder) -> m [MembershipInterval]
getMembershipIntervalsAsInterval maybeLimitOffset maybeFilter maybeOrder = do
  infos <- getMembershipIntervals maybeLimitOffset maybeFilter maybeOrder
  pure $ membershipIntervalInformationToInterval <$> infos
  where
    membershipIntervalInformationToInterval info =
      MembershipInterval
        { membershipIntervalId = membershipIntervalInformationId info,
          membershipIntervalStartDate = membershipIntervalInformationStartDate info,
          membershipIntervalEndDate = membershipIntervalInformationEndDate info,
          membershipIntervalAccepted = membershipIntervalInformationAccepted info,
          membershipIntervalPractitionerId = membershipIntervalInformationPractitionerId info,
          membershipIntervalIntervalNumber = membershipIntervalInformationIntervalNumber info
        }

getMembershipIntervals :: (MonadIO m, MonadReader QueryAppContext m) => Maybe (C.Limit, C.Offset) -> Maybe F.MembershipIntervalFilter -> Maybe (MembershipIntervalsOrderBy, SortOrder) -> m [MembershipIntervalInformation]
getMembershipIntervals maybeLimitOffset maybeFilter maybeOrder = do
  pool <- asks pgPool
  liftIO $
    runSqlPool
      ( do
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
                ( \org ->
                    MembershipIntervalInformation
                      { membershipIntervalInformationId = membershipIntervalProjectionMembershipIntervalId mip,
                        membershipIntervalInformationStartDate = membershipIntervalProjectionStartDate mip,
                        membershipIntervalInformationEndDate = membershipIntervalProjectionEndDate mip,
                        membershipIntervalInformationAccepted = membershipIntervalProjectionIsAccepted mip,
                        membershipIntervalInformationPractitionerId = membershipIntervalProjectionPractitionerProfileId mip,
                        membershipIntervalInformationIntervalNumber = membershipIntervalProjectionIntervalNumber mip,
                        membershipIntervalInformationOrganizationId = org
                      }
                )
                  <$> membershipIntervalProjectionOrganizationProfileId mip
          pure (mapMaybe (toIntervalInfo . entityVal) rows)
      )
      pool

profileSearchFilter :: ProfileType -> Text -> F.ProfileFilter
profileSearchFilter pt q =
  F.ProfileFilter
    { C.profileFilterId = Nothing,
      C.profileFilterType = Just pt,
      C.profileFilterName = Nothing,
      C.profileFilterDescription = Nothing,
      C.profileFilterActiveMembershipOrganization = Nothing,
      C.profileFilterMembershipOrganization = Nothing,
      C.profileFilterBelt = Nothing,
      C.profileFilterTextSearch = Just q,
      C.profileFilterRegisteredAfter = Nothing
    }

achievementSearchFilter :: Text -> F.AchievementFilter
achievementSearchFilter q =
  F.AchievementFilter
    { C.achievementFilterId = Nothing,
      C.achievementFilterAwardedToProfileId = Nothing,
      C.achievementFilterAwardedByProfileId = Nothing,
      C.achievementFilterIsAccepted = Nothing,
      C.achievementFilterDateInterval = (Nothing, Nothing),
      C.achievementFilterTextSearch = Just q
    }

-- | Search filter for promotions (text search on id, profile ids, belt).
promotionSearchFilter :: Text -> F.PromotionFilter
promotionSearchFilter q =
  F.PromotionFilter
    { C.promotionFilterId = Nothing,
      C.promotionFilterBelt = Nothing,
      C.promotionFilterAchievedByProfileId = Nothing,
      C.promotionFilterAwardedByProfileId = Nothing,
      C.promotionFilterAchievementDateInterval = (Nothing, Nothing),
      C.promotionFilterTextSearch = Just q,
      C.promotionFilterState = Nothing
    }

promotionMatchesSearch :: Text -> Promotion -> Bool
promotionMatchesSearch q p =
  let q' = T.toLower q
      t :: (ToJSON a) => a -> Text
      t = T.pack . stringFromJSON
   in T.isInfixOf q' (t (promotionId p))
        || T.isInfixOf q' (t (promotionAchievedByProfileId p))
        || T.isInfixOf q' (t (promotionAwardedByProfileId p))
        || T.isInfixOf q' (t (promotionBelt p))

-- | Extended promotion search that also matches achievedBy/awardedBy profile names.
promotionMatchesSearchWithNames :: M.Map ProfileRefAC Text -> Text -> Promotion -> Bool
promotionMatchesSearchWithNames names q p =
  promotionMatchesSearch q p
    || maybe False (T.isInfixOf (T.toLower q) . T.toLower) (M.lookup (promotionAchievedByProfileId p) names)
    || maybe False (T.isInfixOf (T.toLower q) . T.toLower) (M.lookup (promotionAwardedByProfileId p) names)

-- | Achievement text search matching id, profile ids, name, and description.
achievementMatchesSearch :: Text -> Achievement -> Bool
achievementMatchesSearch q a =
  let q' = T.toLower q
      t :: (ToJSON a) => a -> Text
      t = T.pack . stringFromJSON
   in T.isInfixOf q' (t (achievementId a))
        || T.isInfixOf q' (t (achievementAwardedToProfileId a))
        || T.isInfixOf q' (t (achievementAwardedByProfileId a))
        || T.isInfixOf q' (T.toLower (achievementName a))
        || T.isInfixOf q' (T.toLower (achievementDescription a))

profileToHitPractitioner :: Profile -> SearchHit
profileToHitPractitioner p =
  SearchHit
    { searchHitId = T.pack $ stringFromJSON (profileId p),
      searchHitTitle = profileName p,
      searchHitSubtitle = profileDescription p,
      searchHitBadges = ["Practitioner"],
      searchHitStatus = Nothing,
      searchHitTarget =
        SearchTarget
          { searchTargetKind = "practitioner_profile",
            searchTargetId = T.pack $ stringFromJSON (profileId p)
          }
    }

profileToHitOrganization :: Profile -> SearchHit
profileToHitOrganization p =
  SearchHit
    { searchHitId = T.pack $ stringFromJSON (profileId p),
      searchHitTitle = profileName p,
      searchHitSubtitle = profileDescription p,
      searchHitBadges = ["Organization"],
      searchHitStatus = Nothing,
      searchHitTarget =
        SearchTarget
          { searchTargetKind = "organization_profile",
            searchTargetId = T.pack $ stringFromJSON (profileId p)
          }
    }

-- | Default maximum results per search group.
searchGroupLimit :: Int
searchGroupLimit = 10

-- | Maximum allowed depth for lineage descendant subtree traversal.
maxDescendantDepth :: Int
maxDescendantDepth = 10

-- | Resolve a profile ID to a display name via the name map, falling back to the raw ID.
nameOrId :: M.Map ProfileRefAC Text -> ProfileRefAC -> Text
nameOrId names pid = M.findWithDefault (T.pack $ stringFromJSON pid) pid names

promotionToHit :: M.Map ProfileRefAC Text -> Promotion -> SearchHit
promotionToHit names p =
  SearchHit
    { searchHitId = T.pack $ stringFromJSON (promotionId p),
      searchHitTitle = T.pack (show (promotionBelt p)) <> " — " <> nameOrId names (promotionAchievedByProfileId p),
      searchHitSubtitle =
        nameOrId names (promotionAchievedByProfileId p)
          <> " ← "
          <> nameOrId names (promotionAwardedByProfileId p),
      searchHitBadges = ["Promotion"],
      searchHitStatus = Just $ T.pack $ stringFromJSON (promotionState p),
      searchHitTarget =
        SearchTarget
          { searchTargetKind = "promotion",
            searchTargetId = T.pack $ stringFromJSON (promotionId p)
          }
    }

achievementToHit :: Achievement -> SearchHit
achievementToHit a =
  SearchHit
    { searchHitId = T.pack $ stringFromJSON (achievementId a),
      searchHitTitle = achievementName a,
      searchHitSubtitle = achievementDescription a,
      searchHitBadges = ["Achievement"],
      searchHitStatus = Nothing,
      searchHitTarget =
        SearchTarget
          { searchTargetKind = "achievement",
            searchTargetId = T.pack $ stringFromJSON (achievementId a)
          }
    }

-- | Batch-load profile display names for a set of profile IDs. Used to enrich
-- rank and promotion search hits with human-readable names instead of raw IDs.
batchProfileNames :: (MonadIO m, MonadReader QueryAppContext m) => [ProfileRefAC] -> m (M.Map ProfileRefAC Text)
batchProfileNames [] = return M.empty
batchProfileNames pids = do
  pool <- asks pgPool
  liftIO $
    runSqlPool
      ( do
          rows <- select $ do
            pp <- from $ table @ProfileProjection
            where_ (pp ^. ProfileProjectionProfileId `in_` valList (nub pids))
            pure (pp ^. ProfileProjectionProfileId, pp ^. ProfileProjectionProfileName)
          return $ M.fromList [(unValue pid, unValue nm) | (pid, nm) <- rows]
      )
      pool

-- | Search ranks with profile name matching via LEFT JOIN to profile_projection.
-- Used only by unified search; general getRanks uses applyRankFilter without JOINs.
searchRanksWithNames :: (MonadIO m, MonadReader QueryAppContext m) => Text -> m [Rank]
searchRanksWithNames q = do
  pool <- asks pgPool
  liftIO $
    runSqlPool
      ( do
          rows <- select $ do
            (rp :& mppAchieved :& mppAwarded) <-
              from $
                table @RankProjection
                  `leftJoin` table @ProfileProjection
                  `on` (\(rp :& ppA) -> just (rp ^. RankProjectionRankAchievedByProfileId) ==. ppA ?. ProfileProjectionProfileId)
                  `leftJoin` table @ProfileProjection
                  `on` (\(rp :& _ :& ppW) -> just (rp ^. RankProjectionRankAwardedByProfileId) ==. ppW ?. ProfileProjectionProfileId)
            let pat = likePat q
            where_ $
              (lower_ (unsafeSqlCastAs "text" (rp ^. RankProjectionRankId)) `like` pat)
                ||. (lower_ (unsafeSqlCastAs "text" (rp ^. RankProjectionRankAchievedByProfileId)) `like` pat)
                ||. (lower_ (unsafeSqlCastAs "text" (rp ^. RankProjectionRankAwardedByProfileId)) `like` pat)
                ||. (lower_ (unsafeSqlCastAs "text" (rp ^. RankProjectionRankBelt)) `like` pat)
                ||. (lower_ (coalesceDefault [mppAchieved ?. ProfileProjectionProfileName] (val "")) `like` pat)
                ||. (lower_ (coalesceDefault [mppAwarded ?. ProfileProjectionProfileName] (val "")) `like` pat)
            pure rp
          pure (Prelude.map (toRankDomain . entityVal) rows)
      )
      pool

-- | Search promotions with profile name matching via LEFT JOIN to profile_projection.
-- Used only by unified search; general getPromotions uses applyPromotionFilter without JOINs.
searchPromotionsWithNames :: (MonadIO m, MonadReader QueryAppContext m) => Text -> m [Promotion]
searchPromotionsWithNames q = do
  pool <- asks pgPool
  liftIO $
    runSqlPool
      ( do
          rows <- select $ do
            (pr :& mppAchieved :& mppAwarded) <-
              from $
                table @PromotionProjection
                  `leftJoin` table @ProfileProjection
                  `on` (\(pr :& ppA) -> just (pr ^. PromotionProjectionPromotionAchievedByProfileId) ==. ppA ?. ProfileProjectionProfileId)
                  `leftJoin` table @ProfileProjection
                  `on` (\(pr :& _ :& ppW) -> just (pr ^. PromotionProjectionPromotionAwardedByProfileId) ==. ppW ?. ProfileProjectionProfileId)
            let pat = likePat q
            where_ $
              (lower_ (unsafeSqlCastAs "text" (pr ^. PromotionProjectionPromotionId)) `like` pat)
                ||. (lower_ (unsafeSqlCastAs "text" (pr ^. PromotionProjectionPromotionAchievedByProfileId)) `like` pat)
                ||. (lower_ (unsafeSqlCastAs "text" (pr ^. PromotionProjectionPromotionAwardedByProfileId)) `like` pat)
                ||. (lower_ (unsafeSqlCastAs "text" (pr ^. PromotionProjectionPromotionBelt)) `like` pat)
                ||. (lower_ (coalesceDefault [mppAchieved ?. ProfileProjectionProfileName] (val "")) `like` pat)
                ||. (lower_ (coalesceDefault [mppAwarded ?. ProfileProjectionProfileName] (val "")) `like` pat)
            pure pr
          -- Self-promotions (awardedBy == achievedBy) are not legitimate; filter them out.
          let pvals = Prelude.filter (\p -> promotionProjectionPromotionAchievedByProfileId p /= promotionProjectionPromotionAwardedByProfileId p) $ map entityVal rows
              pids = nub $ map promotionProjectionPromotionAchievedByProfileId pvals
          beltMap <- currentBeltMapForPractitioners pids
          pure (Prelude.map (toPromotionDomain beltMap) pvals)
      )
      pool

searchProjected :: (MonadIO m, MonadReader QueryAppContext m) => Text -> m SearchResults
searchProjected q = do
  practitionerProfiles <- getProfiles Nothing (Just (profileSearchFilter Practitioner q)) Nothing
  organizationProfiles <- getProfiles Nothing (Just (profileSearchFilter Organization q)) Nothing
  ranks <- searchRanksWithNames q
  promotions <- searchPromotionsWithNames q
  achievements <- getAchievements Nothing (Just (achievementSearchFilter q)) Nothing
  -- Merge ranks (as accepted promotions) into promotions for unified search
  let ranksAsPromotions = map rankToPromotion ranks
      allPromotions = promotions ++ ranksAsPromotions
  -- Batch-load profile names for promotion hit display enrichment
  let rpIds =
        concatMap (\p -> [promotionAchievedByProfileId p, promotionAwardedByProfileId p]) allPromotions
  names <- batchProfileNames rpIds
  let prItems = map profileToHitPractitioner practitionerProfiles
      orgItems = map profileToHitOrganization organizationProfiles
      promItems = map (promotionToHit names) allPromotions
      achItems = map achievementToHit achievements
      mkGroup items =
        SearchGroup
          { searchGroupTotal = length items,
            searchGroupItems = take searchGroupLimit items
          }
      total =
        length prItems
          + length orgItems
          + length promItems
          + length achItems
  return
    SearchResults
      { searchResultsQuery = q,
        searchResultsTotal = total,
        searchResultsPractitioners = mkGroup prItems,
        searchResultsOrganizations = mkGroup orgItems,
        searchResultsPromotions = mkGroup promItems,
        searchResultsAchievements = mkGroup achItems
      }

-- | Direct lineage tree from rank projections. Ancestor chain follows awarded_by links upward;
-- descendant subtree follows awarded_to links downward. Collateral branches are excluded.
getLineageGraph ::
  (MonadIO m, MonadReader QueryAppContext m) =>
  ProfileRefAC ->
  Int ->
  Int ->
  Maybe BJJBelt ->
  m LineageGraphData
getLineageGraph root ancestorDepth descendantDepth mMinBelt = do
  pool <- asks pgPool
  let cappedDesc = min descendantDepth maxDescendantDepth
  liftIO $
    runSqlPool
      ( do
          mRootProf <- P.getBy (UniqueProfileProjection root)
          case mRootProf of
            Nothing -> liftIO $ throwIO ProfileNotFound
            Just _ -> do
              let minBelt = fromMaybe White mMinBelt
                  -- BJJBelt is stored as a JSON string in PostgreSQL, so SQL >=
                  -- uses lexicographic ordering which does not match the belt
                  -- hierarchy.  Use an explicit IN list instead.
                  validBelts = [minBelt .. Red10]
              rankEntities <- select $ do
                rp <- from $ table @RankProjection
                where_ (rp ^. RankProjectionRankBelt `in_` valList validBelts)
                pure rp
              let edges =
                    [ ( rankProjectionRankAwardedByProfileId rp,
                        rankProjectionRankAchievedByProfileId rp,
                        rankProjectionRankBelt rp,
                        rankProjectionRankAchievementDate rp
                      )
                    | Entity _ rp <- rankEntities
                    ]
                  -- Upward: key = recipient, values = awarders (who promoted me?)
                  upAdj =
                    foldl'
                      (\m (awarder, recipient, _, _) -> M.insertWith S.union recipient (S.singleton awarder) m)
                      M.empty
                      edges
                  -- Downward: key = awarder, values = recipients (who did I promote?)
                  downAdj =
                    foldl'
                      (\m (awarder, recipient, _, _) -> M.insertWith S.union awarder (S.singleton recipient) m)
                      M.empty
                      edges
                  ancestorVertices = directedReachable root ancestorDepth upAdj
                  descendantVertices = directedReachable root cappedDesc downAdj
                  vertices = ancestorVertices `S.union` descendantVertices
                  edgesInSubgraph =
                    [ (fromP, toP, belt, dt)
                    | (fromP, toP, belt, dt) <- edges,
                      fromP `S.member` vertices,
                      toP `S.member` vertices
                    ]
              profileRows <-
                if S.null vertices
                  then pure []
                  else select $ do
                    pp <- from $ table @ProfileProjection
                    where_ (pp ^. ProfileProjectionProfileId `in_` valList (S.toList vertices))
                    pure pp
              let profById =
                    M.fromList
                      [ (profileProjectionProfileId (entityVal e), entityVal e)
                      | e <- profileRows
                      ]
              beltMap <- currentBeltMapForPractitioners (S.toList vertices)
              let nodes =
                    [ n
                    | pid <- S.toList vertices,
                      Just n <- [lineageNodeFor pid (M.lookup pid profById) (M.lookup pid beltMap)]
                    ]
                  lineageEdges =
                    [ LineageEdge
                        { lineageEdgeFrom = fromP,
                          lineageEdgeTo = toP,
                          lineageEdgeAwardedBelt = belt,
                          lineageEdgeDate = dt
                        }
                    | (fromP, toP, belt, dt) <- edgesInSubgraph,
                      fromP /= toP
                    ]
              return
                LineageGraphData
                  { lineageGraphDataNodes = nodes,
                    lineageGraphDataEdges = lineageEdges
                  }
      )
      pool
  where
    directedReachable :: ProfileRefAC -> Int -> M.Map ProfileRefAC (S.Set ProfileRefAC) -> S.Set ProfileRefAC
    directedReachable start maxD adj =
      let go !q !dist
            | Seq.null q =
                S.fromList
                  [ k
                  | (k, d) <- M.toList dist,
                    d <= maxD
                  ]
            | otherwise =
                let u = Seq.index q 0
                    qRest = Seq.drop 1 q
                    du = dist M.! u
                 in if du >= maxD
                      then go qRest dist
                      else
                        let nbrs = S.toList $ M.findWithDefault S.empty u adj
                            new = filter (not . (`M.member` dist)) nbrs
                            dist' = foldl' (\m v -> M.insert v (du + 1) m) dist new
                            q'' = qRest <> Seq.fromList new
                         in go q'' dist'
       in go (Seq.singleton start) (M.singleton start 0)

    lineageNodeFor ::
      ProfileRefAC ->
      Maybe ProfileProjection ->
      Maybe (Maybe BJJBelt) ->
      Maybe LineageNode
    lineageNodeFor _pid Nothing _ = Nothing
    lineageNodeFor pid (Just prof) mBelt =
      let pType = profileProjectionProfileType prof
          img = profileProjectionProfileImageURI prof
          mCurrentBelt = case pType of
            Practitioner -> fromMaybe White (join mBelt)
            Organization -> White
       in Just
            LineageNode
              { lineageNodeProfileId = pid,
                lineageNodeName = profileProjectionProfileName prof,
                lineageNodeImageUri = img,
                lineageNodeThumbnailUri = id img,
                lineageNodeCurrentBelt = mCurrentBelt,
                lineageNodeProfileType = pType,
                lineageNodeIsMasterCapable = case pType of
                  Practitioner -> C.beltIsMasterCapable mCurrentBelt
                  Organization -> False
              }

getMembershipIntervalsCount :: (MonadIO m, MonadReader QueryAppContext m) => Maybe F.MembershipIntervalFilter -> m Int
getMembershipIntervalsCount maybeFilter = do
  pool <- asks pgPool
  liftIO $
    runSqlPool
      ( do
          cnt <- selectOne $ do
            mip <- from $ table @MembershipIntervalProjection
            applyMembershipIntervalFilter maybeFilter mip
            pure countRows
          pure (maybe 0 unValue cnt)
      )
      pool

-- | Monthly promotion belt histogram (same filter as list queries; uses projection rows).
getMonthlyPromotionStats ::
  (MonadIO m, MonadReader QueryAppContext m) =>
  Maybe F.PromotionFilter ->
  m [MonthlyPromotionsDataResponse]
getMonthlyPromotionStats mf = do
  ps <- getPromotions Nothing mf Nothing
  pure $ monthlyPromotionsFromList ps

-- | Monthly achievement accepted / pending counts (same filter as list queries).
getMonthlyAchievementStats ::
  (MonadIO m, MonadReader QueryAppContext m) =>
  Maybe F.AchievementFilter ->
  m [AchievementsMonthlyStatsResponse]
getMonthlyAchievementStats mf = do
  as <- getAchievements Nothing mf Nothing
  pure $ monthlyAchievementsFromList as

-- ---------------------------------------------------------------------------
-- Activity feed (Option A: projected DB union)
-- ---------------------------------------------------------------------------

-- | Paginated activity feed built from projection tables.  Fetches recent
-- rows from profiles, promotions, ranks, achievements, and membership
-- intervals, then batch-resolves actor/target display names from
-- @profile_projection@.  Merges + sorts by timestamp descending in Haskell.
getActivityFeed ::
  (MonadIO m, MonadReader QueryAppContext m) =>
  Maybe (C.Limit, C.Offset) ->
  Maybe F.ActivityFilter ->
  m [ActivityEventResponse]
getActivityFeed maybeLimitOffset maybeFilter = do
  pool <- asks pgPool
  let wantType et = case maybeFilter >>= C.activityFilterEventType of
        Nothing -> True
        Just t -> t == et
      actorFilter = maybeFilter >>= C.activityFilterActor
      sinceUTC = maybeFilter >>= C.activityFilterSince >>= \t -> Just (posixSecondsToUTCTime (timeToPOSIX t))
  -- Intermediate type: event without resolved names.
  -- (eventType, actorId, targetId, timestamp, title, body)
  rawEvents <-
    liftIO $
      runSqlPool
        ( do
            -- 1. Profile registrations
            profileEvts <-
              if wantType EvtProfileCreated
                then do
                  rows <- select $ do
                    pp <- from $ table @ProfileProjection
                    for_ actorFilter (\a -> where_ (pp ^. ProfileProjectionProfileId ==. val a))
                    for_ sinceUTC (\s -> where_ (pp ^. ProfileProjectionInsertedAt >. val s))
                    orderBy [Database.Esqueleto.Experimental.desc (pp ^. ProfileProjectionInsertedAt)]
                    Database.Esqueleto.Experimental.limit 200
                    pure pp
                  pure
                    [ ( EvtProfileCreated,
                        profileProjectionProfileId p,
                        Nothing,
                        profileProjectionInsertedAt p,
                        "Profile Created" :: Text,
                        Just $ "New " <> T.toLower (T.pack (show (profileProjectionProfileType p))) <> " profile registered"
                      )
                    | Entity _ p <- rows
                    ]
                else pure []

            -- 2. Promotions (pending = PromotionIssued)
            promoEvts <-
              if wantType EvtPromotionIssued
                then do
                  rows <- select $ do
                    pr <- from $ table @PromotionProjection
                    for_ actorFilter (\a -> where_ (pr ^. PromotionProjectionPromotionAchievedByProfileId ==. val a))
                    for_ sinceUTC (\s -> where_ (pr ^. PromotionProjectionInsertedAt >. val s))
                    orderBy [Database.Esqueleto.Experimental.desc (pr ^. PromotionProjectionInsertedAt)]
                    Database.Esqueleto.Experimental.limit 200
                    pure pr
                  -- Self-promotions (awardedBy == achievedBy) are not legitimate; filter them out.
                  pure
                    [ ( EvtPromotionIssued,
                        promotionProjectionPromotionAchievedByProfileId p,
                        Just (promotionProjectionPromotionAwardedByProfileId p),
                        promotionProjectionInsertedAt p,
                        "Promotion Issued" :: Text,
                        Just $ "Promoted to " <> T.pack (show (promotionProjectionPromotionBelt p)) <> " belt"
                      )
                    | Entity _ p <- rows,
                      promotionProjectionPromotionAchievedByProfileId p /= promotionProjectionPromotionAwardedByProfileId p
                    ]
                else pure []

            -- 3. Ranks (accepted promotions)
            rankEvts <-
              if wantType EvtPromotionAccepted
                then do
                  rows <- select $ do
                    rp <- from $ table @RankProjection
                    for_ actorFilter (\a -> where_ (rp ^. RankProjectionRankAchievedByProfileId ==. val a))
                    for_ sinceUTC (\s -> where_ (rp ^. RankProjectionInsertedAt >. val s))
                    orderBy [Database.Esqueleto.Experimental.desc (rp ^. RankProjectionInsertedAt)]
                    Database.Esqueleto.Experimental.limit 200
                    pure rp
                  pure
                    [ ( EvtPromotionAccepted,
                        rankProjectionRankAchievedByProfileId r,
                        Just (rankProjectionRankAwardedByProfileId r),
                        rankProjectionInsertedAt r,
                        "Belt Promotion Accepted" :: Text,
                        Just $ "Promoted to " <> T.pack (show (rankProjectionRankBelt r)) <> " belt"
                      )
                    | Entity _ r <- rows,
                      rankProjectionRankAchievedByProfileId r /= rankProjectionRankAwardedByProfileId r
                    ]
                else pure []

            -- 4. Achievements
            achEvts <-
              if wantType EvtAchievementAwarded || wantType EvtAchievementAccepted
                then do
                  rows <- select $ do
                    ap_ <- from $ table @AchievementProjection
                    for_ actorFilter (\a -> where_ (ap_ ^. AchievementProjectionAwardedToProfileId ==. val a))
                    for_ sinceUTC (\s -> where_ (ap_ ^. AchievementProjectionInsertedAt >. val s))
                    orderBy [Database.Esqueleto.Experimental.desc (ap_ ^. AchievementProjectionInsertedAt)]
                    Database.Esqueleto.Experimental.limit 200
                    pure ap_
                  pure
                    [ ( evtType,
                        achievementProjectionAwardedToProfileId a,
                        Just (achievementProjectionAwardedByProfileId a),
                        achievementProjectionInsertedAt a,
                        if achievementProjectionIsAccepted a then "Achievement Accepted" else ("Achievement Awarded" :: Text),
                        Just $ "Achievement: " <> achievementProjectionAchievementName a
                      )
                    | Entity _ a <- rows,
                      let evtType = if achievementProjectionIsAccepted a then EvtAchievementAccepted else EvtAchievementAwarded,
                      wantType evtType
                    ]
                else pure []

            -- 5. Membership intervals
            mbrEvts <-
              if wantType EvtMembershipGranted || wantType EvtMembershipAccepted
                then do
                  rows <- select $ do
                    mip <- from $ table @MembershipIntervalProjection
                    for_ actorFilter (\a -> where_ (mip ^. MembershipIntervalProjectionPractitionerProfileId ==. val a))
                    for_ sinceUTC (\s -> where_ (mip ^. MembershipIntervalProjectionInsertedAt >. val s))
                    orderBy [Database.Esqueleto.Experimental.desc (mip ^. MembershipIntervalProjectionInsertedAt)]
                    Database.Esqueleto.Experimental.limit 200
                    pure mip
                  pure
                    [ ( evtType,
                        membershipIntervalProjectionPractitionerProfileId m,
                        membershipIntervalProjectionOrganizationProfileId m,
                        membershipIntervalProjectionInsertedAt m,
                        if membershipIntervalProjectionIsAccepted m then "Membership Accepted" else ("Membership Granted" :: Text),
                        Just $ if membershipIntervalProjectionIsAccepted m then "Membership interval accepted" else "New membership interval created"
                      )
                    | Entity _ m <- rows,
                      let evtType = if membershipIntervalProjectionIsAccepted m then EvtMembershipAccepted else EvtMembershipGranted,
                      wantType evtType
                    ]
                else pure []

            pure $ profileEvts <> promoEvts <> rankEvts <> achEvts <> mbrEvts
        )
        pool

  -- Batch-resolve profile display names (single SQL query).
  let allPids = nub $ concatMap (\(_, actor, target, _, _, _) -> actor : maybeToList target) rawEvents
  nameMap <-
    liftIO $
      runSqlPool
        ( do
            rows <- select $ do
              pp <- from $ table @ProfileProjection
              where_ (pp ^. ProfileProjectionProfileId `in_` valList allPids)
              pure (pp ^. ProfileProjectionProfileId, pp ^. ProfileProjectionProfileName)
            pure $ M.fromList [(unValue pid, unValue name) | (pid, name) <- rows]
        )
        pool

  let lookupName pid = M.lookup pid nameMap
      toResponse (evtType, actorId, targetId, ts, title, body) =
        ActivityEventResponse
          { activityEventEventType = evtType,
            activityEventActorId = actorId,
            activityEventTargetId = targetId,
            activityEventTimestamp = timeFromPOSIX (utcTimeToPOSIXSeconds ts),
            activityEventDetails =
              ActivityEventDetails
                { activityEventDetailsTitle = title,
                  activityEventDetailsBody = body,
                  activityEventDetailsActorName = lookupName actorId,
                  activityEventDetailsTargetName = targetId >>= lookupName
                }
          }
      sorted = sortOn (Down . activityEventTimestamp) $ map toResponse rawEvents
  pure $ C.applyLimits maybeLimitOffset sorted

-- | Top organizations by active (accepted, ongoing) member count.
-- Fetches distinct (org, practitioner) pairs for active intervals, then counts and ranks in Haskell.
getTopOrganizationsByMemberCount :: (MonadIO m, MonadReader QueryAppContext m) => Int -> m [(ProfileRefAC, Int)]
getTopOrganizationsByMemberCount topN = do
  pool <- asks pgPool
  pairs <- liftIO $
    flip runSqlPool pool $ do
      rows <- select $ do
        mip <- from $ table @MembershipIntervalProjection
        where_ (mip ^. MembershipIntervalProjectionIsAccepted ==. val True)
        let nowTs = unsafeSqlFunction "NOW" ()
        where_ $
          (isNothing_ (mip ^. MembershipIntervalProjectionEndDate))
            ||. (mip ^. MembershipIntervalProjectionEndDate >=. just nowTs)
        where_ $ not_ $ isNothing_ (mip ^. MembershipIntervalProjectionOrganizationProfileId)
        pure (mip ^. MembershipIntervalProjectionOrganizationProfileId, mip ^. MembershipIntervalProjectionPractitionerProfileId)
      pure [(oid, unValue pid) | (Value (Just oid), pid) <- rows]
  let distinctPairs = nub pairs
      counted = M.toList $ foldl' (\acc (oid, _) -> M.insertWith (+) oid (1 :: Int) acc) M.empty distinctPairs
      sorted = take topN $ sortOn (Down . snd) counted
  pure sorted
