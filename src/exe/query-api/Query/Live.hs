{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Query.Live where

import Control.Exception (SomeException, try)
import Control.Monad.Reader
import Data.Aeson (ToJSON)
import Data.Either.Extra (fromRight)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.MultiSet qualified as MultiSet
import Data.Ord (Down (..))
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.Persist.Sql (Entity (..), runSqlPool, selectList)
import DomainTypes.Core.BJJ (BJJBelt)
import DomainTypes.Core.Types
import DomainTypes.Transfer.Types
import GeniusYield.Types (GYAddress, timeFromPOSIX)
import DomainTypes.Transfer.Filters
  ( AchievementFilter (..)
  , ActivityFilter (..)
  , MembershipHistoryFilter (..)
  , MembershipIntervalFilter (..)
  , ProfileFilter (..)
  , PromotionFilter (..)
  )
import DomainTypes.Transfer.QueryResponses (ActivityEventDetails (..), ActivityEventResponse (..), SearchGroup (..), SearchResults (..))
import Query.Common
import Query.Projected
  ( achievementMatchesSearch,
    achievementSearchFilter,
    achievementToHit,
    profileSearchFilter,
    profileToHitOrganization,
    profileToHitPractitioner,
    promotionMatchesSearchWithNames,
    promotionToHit,
    searchGroupLimit,
  )
import QueryAppMonad (QueryAppContext (..))
import Storage (ProfileProjection (..))
import TxBuilding.Context
import TxBuilding.Lookups
import DomainTypes.Transfer.OrderBy
import DomainTypes.Transfer.OrderBy qualified as Types
import Utils (stringFromJSON)

-- | Build a filter from an optional list of values: when @Just@, keep items whose
-- projected field is in the set; when @Nothing@, pass everything through.
-- Uses 'S.Set' for O(log n) membership instead of O(n) list 'elem'.
filterBySet :: (Ord b) => Maybe [b] -> (a -> b) -> [a] -> [a]
filterBySet Nothing _ = id
filterBySet (Just ids) proj = let s = S.fromList ids in Prelude.filter ((`S.member` s) . proj)

-- | Substring match on membership history ids (aligned with projected SQL @q@).
membershipHistoryMatchesSearch :: Text -> MembershipHistoryInformation -> Bool
membershipHistoryMatchesSearch q info =
  let q' = Text.toLower q
      t :: (ToJSON a) => a -> Text
      t = Text.pack . stringFromJSON
   in Text.isInfixOf q' (t (membershipHistoryInformationId info))
        || Text.isInfixOf q' (t (membershipHistoryInformationPractitionerId info))
        || Text.isInfixOf q' (t (membershipHistoryInformationOrganizationId info))

-- | Substring match on interval id and practitioner id (aligned with projected SQL @q@).
membershipIntervalMatchesSearch :: Text -> MembershipIntervalInformation -> Bool
membershipIntervalMatchesSearch q info =
  let q' = Text.toLower q
      t :: (ToJSON a) => a -> Text
      t = Text.pack . stringFromJSON
   in Text.isInfixOf q' (t (membershipIntervalInformationId info))
        || Text.isInfixOf q' (t (membershipIntervalInformationPractitionerId info))

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
  let nid = getNetworkId providerCtx
  allProfiles <- liftIO $ runQuery providerCtx (getAllProfiles nid)
  -- When ordering by registered_at, fetch timestamps from the projected DB
  regTsMap <- case maybeOrder of
    Just (ProfilesOrderByRegisteredAt, _) -> fetchRegistrationTimestamps
    _ -> pure M.empty
  let practitionerQ = case maybeProfileFilter of
        Just ProfileFilter {profileFilterType = Just Practitioner, profileFilterTextSearch = Just q} -> Just q
        _ -> Nothing
      filterPass1 = case maybeProfileFilter of
        Just pf@ProfileFilter {profileFilterType = Just Practitioner, profileFilterTextSearch = Just _} ->
          Just pf {profileFilterTextSearch = Nothing}
        _ -> maybeProfileFilter
      base = applyFilterOrderLimit maybeLimitOffset filterPass1 maybeOrder applyProfileFilter (applyOrdering regTsMap) allProfiles

  -- Post-filter: practitioner text search including affiliated org names
  afterQ <- case practitionerQ of
    Just q -> do
      allMh <- liftIO $ runQuery providerCtx (getAllMembershipHistoryInformation nid)
      let orgs = Prelude.filter ((== Organization) . profileType) allProfiles
          orgMap = M.fromList [(profileId o, o) | o <- orgs]
          q' = Text.toLower q
          t :: (ToJSON a) => a -> Text
          t = Text.pack . stringFromJSON
          orgMatches oid =
            q' `Text.isInfixOf` Text.toLower (t oid)
              || maybe False (\o -> q' `Text.isInfixOf` Text.toLower (profileName o)) (M.lookup oid orgMap)
          selfMatch p =
            q' `Text.isInfixOf` Text.toLower (t (profileId p))
              || q' `Text.isInfixOf` Text.toLower (profileName p)
              || q' `Text.isInfixOf` Text.toLower (profileDescription p)
          orgAffiliateMatch p =
            profileType p == Practitioner
              && any (\mh -> membershipHistoryInformationPractitionerId mh == profileId p && orgMatches (membershipHistoryInformationOrganizationId mh)) allMh
      return $ Prelude.filter (\p -> selfMatch p || orgAffiliateMatch p) base
    Nothing -> return base
  -- Post-filter: membership organization (all-time)
  afterMembershipOrg <- case maybeProfileFilter >>= profileFilterMembershipOrganization of
    Nothing -> return afterQ
    Just orgId -> do
      allMh <- liftIO $ runQuery providerCtx (getAllMembershipHistoryInformation nid)
      let practitionerIds =
            S.fromList
              [ membershipHistoryInformationPractitionerId mh
                | mh <- allMh,
                  membershipHistoryInformationOrganizationId mh == orgId
              ]
      return $ Prelude.filter (\p -> profileId p `S.member` practitionerIds) afterQ
  -- Post-filter: active membership organization (accepted + no end date or end >= now)
  afterActiveOrg <- case maybeProfileFilter >>= profileFilterActiveMembershipOrganization of
    Nothing -> return afterMembershipOrg
    Just orgId -> do
      allMh <- liftIO $ runQuery providerCtx (getAllMembershipHistoryInformation nid)
      now <- liftIO $ timeFromPOSIX <$> getPOSIXTime
      let isActiveInterval iv =
            membershipIntervalInformationAccepted iv
              && case membershipIntervalInformationEndDate iv of
                Nothing -> True
                Just end -> end >= now
          activePractitionerIds =
            S.fromList
              [ membershipHistoryInformationPractitionerId mh
                | mh <- allMh,
                  membershipHistoryInformationOrganizationId mh == orgId,
                  any isActiveInterval (membershipHistoryInformationIntervals mh)
              ]
      return $ Prelude.filter (\p -> profileId p `S.member` activePractitionerIds) afterMembershipOrg
  -- Post-filter: belt (current rank matches given belts)
  case maybeProfileFilter >>= profileFilterBelt of
    Nothing -> return afterActiveOrg
    Just belts -> do
      allRanks <- liftIO $ runQuery providerCtx (getAllRanks nid)
      let beltSet = S.fromList belts
          -- Latest rank by achievement date per practitioner
          currentBeltMap =
            M.fromListWith
              (\(b1, d1) (b2, d2) -> if d1 >= d2 then (b1, d1) else (b2, d2))
              [ (rankAchievedByProfileId r, (rankBelt r, rankAchievementDate r))
                | r <- allRanks
              ]
          hasBelt p = case M.lookup (profileId p) currentBeltMap of
            Just (b, _) -> b `S.member` beltSet
            Nothing -> False
      return $ Prelude.filter hasBelt afterActiveOrg
  where
    fetchRegistrationTimestamps :: (MonadReader QueryAppContext m, MonadIO m) => m (M.Map ProfileRefAC UTCTime)
    fetchRegistrationTimestamps = do
      pool <- asks pgPool
      rows <- liftIO $ runSqlPool (selectList [] []) pool
      return $
        M.fromList
          [ (profileProjectionProfileId (entityVal e), profileProjectionInsertedAt (entityVal e))
            | e <- rows :: [Entity ProfileProjection]
          ]

    applyOrdering :: M.Map ProfileRefAC UTCTime -> Maybe (ProfilesOrderBy, SortOrder) -> [Profile] -> [Profile]
    applyOrdering _ Nothing profiles = profiles
    applyOrdering regTsMap (Just (orderBy, order)) profiles =
      case (orderBy, order) of
        (ProfilesOrderById, Types.Asc) -> L.sortOn profileId profiles
        (ProfilesOrderById, Types.Desc) -> L.sortOn (Down . profileId) profiles
        (ProfilesOrderByName, Types.Asc) -> L.sortOn profileName profiles
        (ProfilesOrderByName, Types.Desc) -> L.sortOn (Down . profileName) profiles
        (ProfilesOrderByDescription, Types.Asc) -> L.sortOn profileDescription profiles
        (ProfilesOrderByDescription, Types.Desc) -> L.sortOn (Down . profileDescription) profiles
        (ProfilesOrderByType, Types.Asc) -> L.sortOn profileType profiles
        (ProfilesOrderByType, Types.Desc) -> L.sortOn (Down . profileType) profiles
        -- Use projected DB registration timestamps when available; fall back to id ordering
        (ProfilesOrderByRegisteredAt, Types.Asc) -> L.sortOn (\p -> M.lookup (profileId p) regTsMap) profiles
        (ProfilesOrderByRegisteredAt, Types.Desc) -> L.sortOn (\p -> Down (M.lookup (profileId p) regTsMap)) profiles

    applyProfileFilter Nothing profiles = profiles
    applyProfileFilter (Just ProfileFilter {..}) profiles =
      let typeFilter = case profileFilterType of
            Just pt -> Prelude.filter ((== pt) . profileType)
            Nothing -> id
          nameFilter = case profileFilterName of
            Just nameSubstring -> Prelude.filter ((Text.toLower nameSubstring `Text.isInfixOf`) . Text.toLower . profileName)
            Nothing -> id
          descriptionFilter = case profileFilterDescription of
            Just descriptionSubstring -> Prelude.filter ((Text.toLower descriptionSubstring `Text.isInfixOf`) . Text.toLower . profileDescription)
            Nothing -> id
          textSearchFilter = case profileFilterTextSearch of
            Just q ->
              let q' = Text.toLower q
                  hay p =
                    q' `Text.isInfixOf` Text.toLower (Text.pack $ stringFromJSON (profileId p))
                      || q' `Text.isInfixOf` Text.toLower (profileName p)
                      || q' `Text.isInfixOf` Text.toLower (profileDescription p)
               in Prelude.filter hay
            Nothing -> id
       in filterBySet profileFilterId profileId . typeFilter . nameFilter . descriptionFilter . textSearchFilter $ profiles

-- | Unified promotions query: fetches pending/superseded from on-chain promotions and
-- accepted from on-chain ranks (via 'rankToPromotion'), merges, applies state filter,
-- ordering, and limit/offset in Haskell.
-- Self-promotions (where awardedBy == achievedBy) are excluded as they are not legitimate.
getPromotions :: (MonadReader QueryAppContext m, MonadIO m) => Maybe (Limit, Offset) -> Maybe PromotionFilter -> Maybe (PromotionsOrderBy, SortOrder) -> m [Promotion]
getPromotions maybeLimitOffset maybePromotionFilter maybeOrder = do
  providerCtx <- asks providerContext
  let nid = getNetworkId providerCtx
      stateFilter = maybePromotionFilter >>= promotionFilterState
      wantsAccepted = maybe True (elem PromotionAccepted) stateFilter
      wantsPendingOrSuperseded = maybe True (\ss -> PromotionPending `elem` ss || PromotionSuperseded `elem` ss) stateFilter
  pendingPromotions <-
    if wantsPendingOrSuperseded
      then do
        allPromotions <- liftIO $ runQuery providerCtx (getAllPromotions nid)
        liftIO $ mapM (assignPromotionState providerCtx) allPromotions
      else pure []
  acceptedPromotions <-
    if wantsAccepted
      then do
        allRanks <- liftIO $ runQuery providerCtx (getAllRanks nid)
        pure $ map rankToPromotion allRanks
      else pure []
  -- Build profile name map for name-aware text search (only when q is present)
  names <- case maybePromotionFilter >>= promotionFilterTextSearch of
    Just _ -> do
      allProfiles <- liftIO $ runQuery providerCtx (getAllProfiles nid)
      pure $ M.fromList [(profileId p, profileName p) | p <- allProfiles]
    Nothing -> pure M.empty
  -- Self-promotions (awardedBy == achievedBy) are not legitimate; filter them out.
  let merged = Prelude.filter (\p -> promotionAchievedByProfileId p /= promotionAwardedByProfileId p) $ pendingPromotions ++ acceptedPromotions
      afterState = case stateFilter of
        Nothing -> merged
        Just ss -> let sSet = S.fromList ss in Prelude.filter ((`S.member` sSet) . promotionState) merged
      afterFilter = applyPromotionFilter names (fmap (\f -> f {promotionFilterState = Nothing}) maybePromotionFilter) afterState
      afterOrder = applyOrdering maybeOrder afterFilter
  return $ applyLimits maybeLimitOffset afterOrder
  where
    assignPromotionState ctx promotion = do
      let pid = promotionAchievedByProfileId promotion
          belt = promotionBelt promotion
      r <- try @SomeException (runQuery ctx (getCurrentBeltForPractitioner pid))
      let currentBelt = fromRight Nothing r
      return promotion {promotionState = promotionStateFromBelts currentBelt belt}

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

    applyPromotionFilter _ Nothing promotions = promotions
    applyPromotionFilter names (Just PromotionFilter {..}) promotions =
      let dateFilter = case promotionFilterAchievementDateInterval of
            (Just from, Just to) -> Prelude.filter (\p -> promotionAchievementDate p >= from && promotionAchievementDate p <= to)
            (Nothing, Just to) -> Prelude.filter (\p -> promotionAchievementDate p <= to)
            (Just from, Nothing) -> Prelude.filter (\p -> promotionAchievementDate p >= from)
            (Nothing, Nothing) -> id
          textSearchFilter = case promotionFilterTextSearch of
            Just q -> Prelude.filter (promotionMatchesSearchWithNames names q)
            Nothing -> id
       in filterBySet promotionFilterId promotionId
            . filterBySet promotionFilterBelt promotionBelt
            . filterBySet promotionFilterAchievedByProfileId promotionAchievedByProfileId
            . filterBySet promotionFilterAwardedByProfileId promotionAwardedByProfileId
            . dateFilter
            . textSearchFilter
            $ promotions

-- | Count by loading all matching rows then taking length.
getPromotionsCount :: (MonadReader QueryAppContext m, MonadIO m) => Maybe PromotionFilter -> m Int
getPromotionsCount maybePromotionFilter = Prelude.length <$> getPromotions Nothing maybePromotionFilter Nothing

-- | Internal: get all ranks for profile belt filtering.
getAllRanksLive :: (MonadReader QueryAppContext m, MonadIO m) => m [Rank]
getAllRanksLive = do
  providerCtx <- asks providerContext
  liftIO $ runQuery providerCtx (getAllRanks (getNetworkId providerCtx))

-- | Internal: count all accepted ranks (used by dashboard).
getRanksCount :: (MonadReader QueryAppContext m, MonadIO m) => m Int
getRanksCount = Prelude.length <$> getAllRanksLive

-- | Count occurrences of each value extracted by @proj@ from a list.
frequencyOf :: (Ord b) => (a -> b) -> [a] -> [(b, Int)]
frequencyOf proj = MultiSet.toOccurList . MultiSet.fromList . map proj

-- | Internal: belt frequency from accepted ranks only (used by dashboard).
getBeltTotals :: (MonadReader QueryAppContext m, MonadIO m) => m [(BJJBelt, Int)]
getBeltTotals = frequencyOf rankBelt <$> getAllRanksLive

getProfileTypeTotals :: (MonadReader QueryAppContext m, MonadIO m) => m [(ProfileType, Int)]
getProfileTypeTotals = frequencyOf profileType <$> getProfiles Nothing Nothing Nothing

-- | Unified belt frequency: counts from both promotions and ranks.
getPromotionBeltTotals :: (MonadReader QueryAppContext m, MonadIO m) => m [(BJJBelt, Int)]
getPromotionBeltTotals = frequencyOf promotionBelt <$> getPromotions Nothing Nothing Nothing

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
        (AchievementsOrderByDate, Types.Asc) -> L.sortOn achievementAchievementDate achievements
        (AchievementsOrderByDate, Types.Desc) -> L.sortOn (Down . achievementAchievementDate) achievements
        (AchievementsOrderByAwardedTo, Types.Asc) -> L.sortOn achievementAwardedToProfileId achievements
        (AchievementsOrderByAwardedTo, Types.Desc) -> L.sortOn (Down . achievementAwardedToProfileId) achievements
        (AchievementsOrderByAwardedBy, Types.Asc) -> L.sortOn achievementAwardedByProfileId achievements
        (AchievementsOrderByAwardedBy, Types.Desc) -> L.sortOn (Down . achievementAwardedByProfileId) achievements
        (AchievementsOrderByName, Types.Asc) -> L.sortOn achievementName achievements
        (AchievementsOrderByName, Types.Desc) -> L.sortOn (Down . achievementName) achievements

    applyAchievementFilter Nothing achievements = achievements
    applyAchievementFilter (Just AchievementFilter {..}) achievements =
      let isAcceptedFilter = case achievementFilterIsAccepted of
            Just ac -> Prelude.filter ((== ac) . achievementAccepted)
            Nothing -> id
          dateFilter = case achievementFilterDateInterval of
            (Just from, Just to) -> Prelude.filter (\a -> achievementAchievementDate a >= from && achievementAchievementDate a <= to)
            (Nothing, Just to) -> Prelude.filter (\a -> achievementAchievementDate a <= to)
            (Just from, Nothing) -> Prelude.filter (\a -> achievementAchievementDate a >= from)
            (Nothing, Nothing) -> id
          textSearchFilter = case achievementFilterTextSearch of
            Just q -> Prelude.filter (achievementMatchesSearch q)
            Nothing -> id
       in filterBySet achievementFilterId achievementId
            . filterBySet achievementFilterAwardedToProfileId achievementAwardedToProfileId
            . filterBySet achievementFilterAwardedByProfileId achievementAwardedByProfileId
            . isAcceptedFilter
            . dateFilter
            . textSearchFilter
            $ achievements

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
      let textSearchFilter = case membershipHistoryFilterTextSearch of
            Just q -> Prelude.filter (membershipHistoryMatchesSearch q)
            Nothing -> id
       in filterBySet membershipHistoryFilterOrganizationProfileId membershipHistoryInformationOrganizationId
            . filterBySet membershipHistoryFilterPractitionerProfileId membershipHistoryInformationPractitionerId
            . textSearchFilter
            $ infos

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
        (MembershipIntervalsOrderByIntervalNumber, Types.Asc) -> L.sortOn membershipIntervalInformationIntervalNumber infos
        (MembershipIntervalsOrderByIntervalNumber, Types.Desc) -> L.sortOn (Down . membershipIntervalInformationIntervalNumber) infos
        (MembershipIntervalsOrderByPractitioner, Types.Asc) -> L.sortOn membershipIntervalInformationPractitionerId infos
        (MembershipIntervalsOrderByPractitioner, Types.Desc) -> L.sortOn (Down . membershipIntervalInformationPractitionerId) infos

    applyMembershipIntervalFilter Nothing infos = infos
    applyMembershipIntervalFilter (Just MembershipIntervalFilter {..}) infos =
      let textSearchFilter = case membershipIntervalFilterTextSearch of
            Just q -> Prelude.filter (membershipIntervalMatchesSearch q)
            Nothing -> id
          acceptedFilter = case membershipIntervalFilterIsAccepted of
            Just acc -> Prelude.filter (\i -> membershipIntervalInformationAccepted i == acc)
            Nothing -> id
       in filterBySet membershipIntervalFilterPractitionerProfileId membershipIntervalInformationPractitionerId
            . acceptedFilter
            . textSearchFilter
            $ infos

getMembershipIntervalsCount :: (MonadReader QueryAppContext m, MonadIO m) => Maybe MembershipIntervalFilter -> m Int
getMembershipIntervalsCount maybeFilter = Prelude.length <$> getMembershipIntervals Nothing maybeFilter Nothing

-- | Unified search over live chain reads (filters match projected semantics where implemented).
searchLive :: (MonadReader QueryAppContext m, MonadIO m) => Text -> m SearchResults
searchLive q = do
  practitionerProfiles <- getProfiles Nothing (Just (profileSearchFilter Practitioner q)) Nothing
  organizationProfiles <- getProfiles Nothing (Just (profileSearchFilter Organization q)) Nothing
  -- Load all promotions (unified: pending + accepted) unfiltered; post-filter with name-aware matchers
  allPromotions <- getPromotions Nothing Nothing Nothing
  achievements <- getAchievements Nothing (Just (achievementSearchFilter q)) Nothing
  -- Build profile name map from all loaded profiles for name-aware search + hit enrichment
  providerCtx <- asks providerContext
  allProfiles <- liftIO $ runQuery providerCtx (getAllProfiles (getNetworkId providerCtx))
  let names = M.fromList [(profileId p, profileName p) | p <- allProfiles]
      promotions = Prelude.filter (promotionMatchesSearchWithNames names q) allPromotions
      prItems = map profileToHitPractitioner practitionerProfiles
      orgItems = map profileToHitOrganization organizationProfiles
      promItems = map (promotionToHit names) promotions
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

-- ---------------------------------------------------------------------------
-- Activity feed (live chain)
-- ---------------------------------------------------------------------------

-- | Live-chain activity feed: builds events from on-chain profiles,
-- promotions, ranks, achievements, and membership intervals, resolves
-- actor/target names, merges + sorts by timestamp descending.
getActivityFeed ::
  (MonadReader QueryAppContext m, MonadIO m) =>
  Maybe (Limit, Offset) ->
  Maybe ActivityFilter ->
  m [ActivityEventResponse]
getActivityFeed maybeLimitOffset maybeFilter = do
  providerCtx <- asks providerContext
  let nid = getNetworkId providerCtx
      wantType et = case maybeFilter >>= activityFilterEventType of
        Nothing -> True
        Just t -> t == et
      actorMatch pid = case maybeFilter >>= activityFilterActor of
        Nothing -> True
        Just a -> a == pid
  -- Load all data sources
  allProfiles <- liftIO $ runQuery providerCtx (getAllProfiles nid)
  let nameMap = M.fromList [(profileId p, profileName p) | p <- allProfiles]
      lookupName pid = M.lookup pid nameMap
  -- Profile events
  let profileEvents =
        [ ActivityEventResponse
            { activityEventEventType = EvtProfileCreated,
              activityEventActorId = profileId p,
              activityEventTargetId = Nothing,
              activityEventTimestamp = timeFromPOSIX 0, -- No timestamp on live profiles (known limitation: sorts to epoch)
              activityEventDetails =
                ActivityEventDetails
                  { activityEventDetailsTitle = "Profile Created",
                    activityEventDetailsBody = Just $ "New " <> Text.toLower (Text.pack (show (profileType p))) <> " profile registered",
                    activityEventDetailsActorName = Just (profileName p),
                    activityEventDetailsTargetName = Nothing
                  }
            }
          | wantType EvtProfileCreated,
            p <- allProfiles,
            actorMatch (profileId p)
        ]
  -- Promotion events (pending/superseded = PromotionIssued)
  allPromotions <-
    if wantType EvtPromotionIssued || wantType EvtPromotionSuperseded
      then liftIO $ runQuery providerCtx (getAllPromotions nid)
      else pure []
  -- Self-promotions (awardedBy == achievedBy) are not legitimate; filter them out.
  let promoEvents =
        [ ActivityEventResponse
            { activityEventEventType = EvtPromotionIssued,
              activityEventActorId = promotionAchievedByProfileId p,
              activityEventTargetId = Just (promotionAwardedByProfileId p),
              activityEventTimestamp = promotionAchievementDate p,
              activityEventDetails =
                ActivityEventDetails
                  { activityEventDetailsTitle = "Promotion Issued",
                    activityEventDetailsBody = Just $ "Promoted to " <> Text.pack (show (promotionBelt p)) <> " belt",
                    activityEventDetailsActorName = lookupName (promotionAchievedByProfileId p),
                    activityEventDetailsTargetName = lookupName (promotionAwardedByProfileId p)
                  }
            }
          | wantType EvtPromotionIssued,
            p <- allPromotions,
            promotionAchievedByProfileId p /= promotionAwardedByProfileId p,
            actorMatch (promotionAchievedByProfileId p)
        ]
  -- Rank events (accepted promotions)
  allRanks <-
    if wantType EvtPromotionAccepted
      then liftIO $ runQuery providerCtx (getAllRanks nid)
      else pure []
  let rankEvents =
        [ ActivityEventResponse
            { activityEventEventType = EvtPromotionAccepted,
              activityEventActorId = rankAchievedByProfileId r,
              activityEventTargetId = Just (rankAwardedByProfileId r),
              activityEventTimestamp = rankAchievementDate r,
              activityEventDetails =
                ActivityEventDetails
                  { activityEventDetailsTitle = "Belt Promotion Accepted",
                    activityEventDetailsBody = Just $ "Promoted to " <> Text.pack (show (rankBelt r)) <> " belt",
                    activityEventDetailsActorName = lookupName (rankAchievedByProfileId r),
                    activityEventDetailsTargetName = lookupName (rankAwardedByProfileId r)
                  }
            }
          | r <- allRanks,
            rankAchievedByProfileId r /= rankAwardedByProfileId r,
            actorMatch (rankAchievedByProfileId r)
        ]
  -- Achievement events
  allAchievements <-
    if wantType EvtAchievementAwarded || wantType EvtAchievementAccepted
      then liftIO $ runQuery providerCtx (getAllAchievements nid)
      else pure []
  let achEvents =
        [ ActivityEventResponse
            { activityEventEventType = if achievementAccepted a then EvtAchievementAccepted else EvtAchievementAwarded,
              activityEventActorId = achievementAwardedToProfileId a,
              activityEventTargetId = Just (achievementAwardedByProfileId a),
              activityEventTimestamp = achievementAchievementDate a,
              activityEventDetails =
                ActivityEventDetails
                  { activityEventDetailsTitle = if achievementAccepted a then "Achievement Accepted" else "Achievement Awarded",
                    activityEventDetailsBody = Just $ "Achievement: " <> achievementName a,
                    activityEventDetailsActorName = lookupName (achievementAwardedToProfileId a),
                    activityEventDetailsTargetName = lookupName (achievementAwardedByProfileId a)
                  }
            }
          | a <- allAchievements,
            let evtType = if achievementAccepted a then EvtAchievementAccepted else EvtAchievementAwarded,
            wantType evtType,
            actorMatch (achievementAwardedToProfileId a)
        ]
  -- Membership interval events
  allIntervals <-
    if wantType EvtMembershipGranted || wantType EvtMembershipAccepted
      then liftIO $ runQuery providerCtx (getAllMembershipIntervalInformation nid)
      else pure []
  let mbrEvents =
        [ ActivityEventResponse
            { activityEventEventType = if membershipIntervalInformationAccepted iv then EvtMembershipAccepted else EvtMembershipGranted,
              activityEventActorId = membershipIntervalInformationPractitionerId iv,
              activityEventTargetId = Just (membershipIntervalInformationOrganizationId iv),
              activityEventTimestamp = membershipIntervalInformationStartDate iv,
              activityEventDetails =
                ActivityEventDetails
                  { activityEventDetailsTitle = if membershipIntervalInformationAccepted iv then "Membership Accepted" else "Membership Granted",
                    activityEventDetailsBody = Just $ if membershipIntervalInformationAccepted iv then "Membership interval accepted" else "New membership interval created",
                    activityEventDetailsActorName = lookupName (membershipIntervalInformationPractitionerId iv),
                    activityEventDetailsTargetName = Just (membershipIntervalInformationOrganizationId iv) >>= lookupName
                  }
            }
          | iv <- allIntervals,
            let evtType = if membershipIntervalInformationAccepted iv then EvtMembershipAccepted else EvtMembershipGranted,
            wantType evtType,
            actorMatch (membershipIntervalInformationPractitionerId iv)
        ]
  let allEvents = profileEvents <> promoEvents <> rankEvents <> achEvents <> mbrEvents
      sorted = L.sortOn (Down . activityEventTimestamp) allEvents
  return $ applyLimits maybeLimitOffset sorted

-- | Look up profiles owned by a wallet address (User NFT ownership).
getProfilesByWalletAddress :: (MonadReader QueryAppContext m, MonadIO m) => GYAddress -> m [Profile]
getProfilesByWalletAddress walletAddr = do
  providerCtx <- asks providerContext
  let nid = getNetworkId providerCtx
  refACs <- liftIO $ runQuery providerCtx (getProfileRefACsAtAddress [walletAddr])
  if null refACs
    then return []
    else do
      allProfiles <- liftIO $ runQuery providerCtx (getAllProfiles nid)
      let refSet = S.fromList refACs
      return $ Prelude.filter (\p -> profileId p `S.member` refSet) allProfiles
