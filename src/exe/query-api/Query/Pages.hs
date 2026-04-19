

{-# LANGUAGE ScopedTypeVariables #-}

-- | §12.5 explorer page aggregates ([backend-api-requirements.md](backend-api-requirements.md)).
module Query.Pages
  ( getPromotionsPage,
    getAchievementsPage,
    getProfilesPage,
    getPractitionerExplorerPage,
    getHomeExplorerHubPage,
    getDashboardPage,
    getPendingActionsPage,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.List.Extra (nub)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Data.Time (addUTCTime, getCurrentTime, nominalDay)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import DomainTypes.Core.BJJ (BJJBelt)
import DomainTypes.Core.Types
import DomainTypes.Transfer.Types (MembershipHistoryInformation (..), MembershipIntervalInformation (..), PractitionerProfileInformation (..))
import GeniusYield.Types (GYTime, timeFromPOSIX)
import Query.Aggregates qualified as Agg
import Query.ExplorerMonthly (monthlyAchievementsFromList, monthlyPromotionsFromList)
import DomainTypes.Transfer.Filters qualified as F
import DomainTypes.Transfer.QueryResponses
  ( AchievementsPageResponse (..),
    DashboardPageResponse (..),
    HomeExplorerHubPageResponse (..),
    PendingActionsResponse (..),
    pendingPromotionToResponse,
    topOrganizationResponse,
    PractitionerExplorerPageResponse (..),
    PractitionerExplorerRowResponse (..),
    ProfilesPageResponse (..),
    PromotionsPageResponse (..),
    achievementToResponse,
    organizationProfileToResponse,
    practitionerProfileToResponse,
    profileToResponse,
  )
import Query.Common qualified as C
import Query.Live qualified as L
import Query.Projected qualified as P
import QueryAppMonad (QueryAppMonad (..))
import DomainTypes.Transfer.Filters
  ( achievementFilterFromParams,
    profileFilterFromParams,
    promotionsFilterFromParams,
  )
import DomainTypes.Transfer.OrderBy
import RestAPI.Common (withBackend)

-- | Filter profiles by type only (counts and latest lists).
profileFilterOfType :: ProfileType -> F.ProfileFilter
profileFilterOfType pt =
  F.ProfileFilter Nothing (Just pt) Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- | §12.1 — dashboard aggregate (totals, frequencies, recent/latest slices).
getDashboardPage ::
  Int ->
  Int ->
  Int ->
  Int ->
  QueryAppMonad DashboardPageResponse
getDashboardPage recentPromoN latestOrgN latestPractN topOrgN = do
  let prFilter = Just $ profileFilterOfType Practitioner
      orgFilter = Just $ profileFilterOfType Organization
  totalPractitioners <- withBackend (L.getProfilesCount prFilter) (P.getProfilesCount prFilter)
  totalOrganizations <- withBackend (L.getProfilesCount orgFilter) (P.getProfilesCount orgFilter)
  totalPromotions <- withBackend (L.getPromotionsCount Nothing) (P.getPromotionsCount Nothing)
  totalAchievements <- withBackend (L.getAchievementsCount Nothing) (P.getAchievementsCount Nothing)
  beltFrequency <- withBackend L.getBeltTotals P.getBeltTotals
  -- Compute 12-month cutoff for monthly stats and recently-registered practitioners
  now <- liftIO getCurrentTime
  let twelveMonthsAgo = addUTCTime (negate $ 365 * nominalDay) now
      twelveMonthsAgoGY = timeFromPOSIX $ utcTimeToPOSIXSeconds twelveMonthsAgo
      last12mPromoFilter = Just $ F.PromotionFilter Nothing Nothing Nothing Nothing (Just twelveMonthsAgoGY, Nothing) Nothing Nothing
      last12mAchFilter = Just $ F.AchievementFilter Nothing Nothing Nothing Nothing (Just twelveMonthsAgoGY, Nothing) Nothing
  -- Opt 2: live path fetches all promotions once for both recent slice and monthly stats;
  -- projected path uses targeted queries (limited fetch + pre-aggregated monthly stats).
  recentInfos <-
    withBackend
      ( do
          allPromotions <- L.getPromotions Nothing Nothing (Just (PromotionsOrderByDate, Desc))
          Agg.promotionsToInformationBatch (take recentPromoN allPromotions)
      )
      ( do
          recentPromos <- P.getPromotions (Just (recentPromoN, 0)) Nothing (Just (PromotionsOrderByDate, Desc))
          Agg.promotionsToInformationBatch recentPromos
      )
  -- Monthly promotions: last 12 months only
  monthlyPromos <-
    withBackend
      ( do
          allPromotions <- L.getPromotions Nothing Nothing (Just (PromotionsOrderByDate, Desc))
          return $ monthlyPromotionsFromList $ filter (\p -> promotionAchievementDate p >= twelveMonthsAgoGY) allPromotions
      )
      (P.getMonthlyPromotionStats last12mPromoFilter)
  -- Monthly achievements: last 12 months only
  monthlyAchs <-
    withBackend
      (monthlyAchievementsFromList <$> L.getAchievements Nothing last12mAchFilter Nothing)
      (P.getMonthlyAchievementStats last12mAchFilter)
  -- Opt 3: batch org/practitioner profile loading (projected path)
  orgProfiles <- withBackend (L.getProfiles (Just (latestOrgN, 0)) orgFilter (Just (ProfilesOrderByName, Asc))) (P.getProfiles (Just (latestOrgN, 0)) orgFilter (Just (ProfilesOrderByName, Asc)))
  latestOrgResponses <-
    withBackend
      ( do
          infos <- mapM (L.getOrganizationProfile . profileId) orgProfiles
          return $ map organizationProfileToResponse infos
      )
      ( do
          let oids = map profileId orgProfiles
          orgMap <- P.getOrganizationProfilesBatch oids
          return [organizationProfileToResponse o | p <- orgProfiles, Just o <- [M.lookup (profileId p) orgMap]]
      )
  -- Recently registered practitioners: registered within the last 12 months, newest first
  let recentPrFilter = Just $ (profileFilterOfType Practitioner) {F.profileFilterRegisteredAfter = Just twelveMonthsAgo}
  practProfiles <- withBackend (L.getProfiles (Just (latestPractN, 0)) recentPrFilter (Just (ProfilesOrderByRegisteredAt, Desc))) (P.getProfiles (Just (latestPractN, 0)) recentPrFilter (Just (ProfilesOrderByRegisteredAt, Desc)))
  latestPractResponses <-
    withBackend
      ( do
          infos <- mapM (L.getPractitionerProfile . profileId) practProfiles
          return $ map practitionerProfileToResponse infos
      )
      ( do
          let pids = map profileId practProfiles
          practMap <- P.getPractitionerProfilesBatch pids
          return [practitionerProfileToResponse pr | p <- practProfiles, Just pr <- [M.lookup (profileId p) practMap]]
      )
  -- Top organizations by active member count (projected only; empty for live)
  topOrgRows <- withBackend (return []) (P.getTopOrganizationsByMemberCount topOrgN)
  topOrgResponses <- case topOrgRows of
    [] -> return []
    _ -> do
      let topOids = map fst topOrgRows
          countMap = M.fromList topOrgRows
      (_, orgResponseMap) <- Agg.buildOrgMaps topOids
      return
        [ topOrganizationResponse org cnt
          | oid <- topOids,
            Just org <- [M.lookup oid orgResponseMap],
            Just cnt <- [M.lookup oid countMap]
        ]
  return
    DashboardPageResponse
      { dashboardPageTotalPractitioners = totalPractitioners,
        dashboardPageTotalOrganizations = totalOrganizations,
        dashboardPageTotalPromotions = totalPromotions,
        dashboardPageTotalAchievements = totalAchievements,
        dashboardPageBeltFrequency = beltFrequency,
        dashboardPageRecentPromotions = recentInfos,
        dashboardPageMonthlyPromotions = monthlyPromos,
        dashboardPageMonthlyAchievements = monthlyAchs,
        dashboardPageLatestOrganizations = latestOrgResponses,
        dashboardPageLatestPractitioners = latestPractResponses,
        dashboardPageTopOrganizations = topOrgResponses
      }

-- | Promotions explorer page: enriched items, total, monthly belt histogram (same filter as list).
getPromotionsPage ::
  Maybe Int ->
  Maybe Int ->
  [RankAC] ->
  [BJJBelt] ->
  [ProfileRefAC] ->
  [ProfileRefAC] ->
  [ProfileRefAC] ->
  Maybe Text ->
  Maybe GYTime ->
  Maybe GYTime ->
  [PromotionState] ->
  Maybe PromotionsOrderBy ->
  Maybe SortOrder ->
  QueryAppMonad PromotionsPageResponse
getPromotionsPage limit offset promotionIds beltRefs achievedByRefs awardedByRefs profileRefs q fromTime toTime states orderBy sortOrder = do
  let limitOffset = C.normalizeLimitOffset limit offset
      promotionsFilter = promotionsFilterFromParams promotionIds beltRefs achievedByRefs awardedByRefs profileRefs q fromTime toTime states
      order = C.normalizeOrder orderBy sortOrder
  items <- withBackend (L.getPromotions limitOffset promotionsFilter order) (P.getPromotions limitOffset promotionsFilter order)
  total <- withBackend (L.getPromotionsCount promotionsFilter) (P.getPromotionsCount promotionsFilter)
  frequency <- withBackend L.getBeltTotals P.getBeltTotals
  monthly <- withBackend (monthlyPromotionsFromList <$> L.getPromotions Nothing promotionsFilter Nothing) (P.getMonthlyPromotionStats promotionsFilter)
  enriched <- Agg.promotionsToInformationBatch items
  return
    PromotionsPageResponse
      { promotionsPageItems = enriched,
        promotionsPageTotal = total,
        promotionsPageFrequency = frequency,
        promotionsPageMonthly = monthly
      }

-- | Achievements explorer page.
getAchievementsPage ::
  Maybe Int ->
  Maybe Int ->
  [AchievementAC] ->
  [ProfileRefAC] ->
  [ProfileRefAC] ->
  Maybe Bool ->
  Maybe GYTime ->
  Maybe GYTime ->
  Maybe Text ->
  Maybe AchievementsOrderBy ->
  Maybe SortOrder ->
  QueryAppMonad AchievementsPageResponse
getAchievementsPage limit offset achievementIds awardedToRefs awardedByRefs accepted fromTime toTime q orderBy sortOrder = do
  let limitOffset = C.normalizeLimitOffset limit offset
      achFilter = achievementFilterFromParams achievementIds awardedToRefs awardedByRefs accepted fromTime toTime q
      order = C.normalizeOrder orderBy sortOrder
  items <- withBackend (L.getAchievements limitOffset achFilter order) (P.getAchievements limitOffset achFilter order)
  total <- withBackend (L.getAchievementsCount achFilter) (P.getAchievementsCount achFilter)
  monthly <- withBackend (monthlyAchievementsFromList <$> L.getAchievements Nothing achFilter Nothing) (P.getMonthlyAchievementStats achFilter)
  return
    AchievementsPageResponse
      { achievementsPageItems = map achievementToResponse items,
        achievementsPageTotal = total,
        achievementsPageMonthly = monthly
      }

-- | Profiles explorer page (list + total + type frequency).
getProfilesPage ::
  Maybe Int ->
  Maybe Int ->
  [ProfileRefAC] ->
  Maybe ProfileType ->
  Maybe ProfileRefAC ->
  Maybe ProfileRefAC ->
  [BJJBelt] ->
  Maybe Text ->
  Maybe ProfilesOrderBy ->
  Maybe SortOrder ->
  QueryAppMonad ProfilesPageResponse
getProfilesPage limit offset profileRefs profileType activeMembershipOrg membershipOrg beltRefs q orderBy sortOrder = do
  let limitOffset = C.normalizeLimitOffset limit offset
      profileFilter = profileFilterFromParams profileRefs profileType activeMembershipOrg membershipOrg beltRefs q
      order = C.normalizeOrder orderBy sortOrder
  profiles <- withBackend (L.getProfiles limitOffset profileFilter order) (P.getProfiles limitOffset profileFilter order)
  total <- withBackend (L.getProfilesCount profileFilter) (P.getProfilesCount profileFilter)
  frequency <- withBackend L.getProfileTypeTotals P.getProfileTypeTotals
  return
    ProfilesPageResponse
      { profilesPageItems = map profileToResponse profiles,
        profilesPageTotal = total,
        profilesPageFrequency = frequency
      }

-- | Practitioner explorer: one row per practitioner profile with memberships and org name map.
getPractitionerExplorerPage ::
  Maybe Int ->
  Maybe Int ->
  [ProfileRefAC] ->
  Maybe ProfileRefAC ->
  Maybe ProfileRefAC ->
  [BJJBelt] ->
  Maybe Text ->
  Maybe ProfilesOrderBy ->
  Maybe SortOrder ->
  QueryAppMonad PractitionerExplorerPageResponse
getPractitionerExplorerPage limit offset profileRefs activeMembershipOrg membershipOrg beltRefs q orderBy sortOrder = do
  let limitOffset = C.normalizeLimitOffset limit offset
      profileFilter = profileFilterFromParams profileRefs (Just Practitioner) activeMembershipOrg membershipOrg beltRefs q
      order = C.normalizeOrder orderBy sortOrder
  profiles <- withBackend (L.getProfiles limitOffset profileFilter order) (P.getProfiles limitOffset profileFilter order)
  total <- withBackend (L.getProfilesCount profileFilter) (P.getProfilesCount profileFilter)
  let pids = map profileId profiles
      batchMhFilter =
        if null pids
          then Nothing
          else Just $ F.MembershipHistoryFilter Nothing (Just pids) Nothing
  allMemberships <- withBackend (L.getMembershipHistories Nothing batchMhFilter Nothing) (P.getMembershipHistories Nothing batchMhFilter Nothing)
  let membershipsByPractitioner =
        M.fromListWith
          (++)
          [ (membershipHistoryInformationPractitionerId mh, [mh])
            | mh <- allMemberships
          ]
  -- Batch-load practitioner profiles (2 SQL queries instead of N)
  practInfoMap <-
    withBackend
      (M.fromList <$> mapM (\p -> (profileId p,) <$> L.getPractitionerProfile (profileId p)) profiles)
      (P.getPractitionerProfilesBatch pids)
  -- Collect all org IDs across all memberships for a single batch org lookup
  let allOrgIds =
        nub $
          concatMap
            ( \mh ->
                membershipHistoryInformationOrganizationId mh
                  : map membershipIntervalInformationOrganizationId (membershipHistoryInformationIntervals mh)
            )
            allMemberships
  (allOrgNames, _) <- Agg.buildOrgMaps allOrgIds
  let rows =
        [ PractitionerExplorerRowResponse
            { practitionerExplorerRowPractitioner = practitionerProfileToResponse practInfo,
              practitionerExplorerRowMemberships = memberships
            }
          | p <- profiles,
            let pid = profileId p,
            let memberships = M.findWithDefault [] pid membershipsByPractitioner,
            Just practInfo <- [M.lookup pid practInfoMap]
        ]
  return
    PractitionerExplorerPageResponse
      { practitionerExplorerPageItems = rows,
        practitionerExplorerPageTotal = total,
        practitionerExplorerPageOrgIdToName = allOrgNames
      }

-- | Home hub: recent promotions and latest practitioners (§12.5).
getHomeExplorerHubPage ::
  Int ->
  Int ->
  QueryAppMonad HomeExplorerHubPageResponse
getHomeExplorerHubPage recentN latestN = do
  promotions <- withBackend (L.getPromotions Nothing Nothing (Just (PromotionsOrderByDate, Desc))) (P.getPromotions Nothing Nothing (Just (PromotionsOrderByDate, Desc)))
  let recentPromos = take recentN promotions
  recentInfos <- Agg.promotionsToInformationBatch recentPromos
  let explorerProfileFilter = profileFilterFromParams [] (Just Practitioner) Nothing Nothing [] Nothing
  profiles <- withBackend (L.getProfiles (Just (latestN, 0)) explorerProfileFilter (Just (ProfilesOrderByName, Asc))) (P.getProfiles (Just (latestN, 0)) explorerProfileFilter (Just (ProfilesOrderByName, Asc)))
  latestPractitionerResponses <-
    withBackend
      ( do
          infos <- mapM (L.getPractitionerProfile . profileId) profiles
          return $ map practitionerProfileToResponse infos
      )
      ( do
          let pids = map profileId profiles
          practMap <- P.getPractitionerProfilesBatch pids
          return [practitionerProfileToResponse pr | p <- profiles, Just pr <- [M.lookup (profileId p) practMap]]
      )
  return
    HomeExplorerHubPageResponse
      { homeExplorerHubPageRecentPromotions = recentInfos,
        homeExplorerHubPageLatestPractitioners = latestPractitionerResponses
      }

-- | Pending-actions inbox for a profile (My Dojo).
getPendingActionsPage :: ProfileRefAC -> QueryAppMonad PendingActionsResponse
getPendingActionsPage pid = do
  let promoFilter =
        Just
          F.PromotionFilter
            { F.promotionFilterId = Nothing,
              F.promotionFilterBelt = Nothing,
              F.promotionFilterAchievedByProfileId = Just [pid],
              F.promotionFilterAwardedByProfileId = Nothing,
              F.promotionFilterAchievementDateInterval = (Nothing, Nothing),
              F.promotionFilterTextSearch = Nothing,
              F.promotionFilterState = Just [PromotionPending]
            }
  let achFilter =
        Just
          F.AchievementFilter
            { F.achievementFilterId = Nothing,
              F.achievementFilterAwardedToProfileId = Just [pid],
              F.achievementFilterAwardedByProfileId = Nothing,
              F.achievementFilterIsAccepted = Just False,
              F.achievementFilterDateInterval = (Nothing, Nothing),
              F.achievementFilterTextSearch = Nothing
            }
  let mbrFilter =
        Just
          F.MembershipIntervalFilter
            { F.membershipIntervalFilterPractitionerProfileId = Just [pid],
              F.membershipIntervalFilterIsAccepted = Just False,
              F.membershipIntervalFilterTextSearch = Nothing
            }
  promotions <- withBackend (L.getPromotions Nothing promoFilter Nothing) (P.getPromotions Nothing promoFilter Nothing)
  achievements <- withBackend (L.getAchievements Nothing achFilter Nothing) (P.getAchievements Nothing achFilter Nothing)
  memberships <- withBackend (L.getMembershipIntervals Nothing mbrFilter Nothing) (P.getMembershipIntervals Nothing mbrFilter Nothing)

  -- Resolve awarded-by display names (promotions + achievements).
  let awardedByIds =
        nub $
          map (\p -> (promotionAwardedByProfileId p, promotionAchievementDate p)) promotions
            ++ map (\a -> (achievementAwardedByProfileId a, achievementAchievementDate a)) achievements
  awardedByMap <- Agg.resolveProfilesBatch awardedByIds
  let awardedByNameMap = M.map practitionerName awardedByMap

  -- Resolve organization display names (memberships).
  let orgIds = nub $ map membershipIntervalInformationOrganizationId memberships
  (orgNameMap, _) <- Agg.buildOrgMaps orgIds

  return
    PendingActionsResponse
      { pendingActionsPromotions = map pendingPromotionToResponse promotions,
        pendingActionsAchievements = map achievementToResponse achievements,
        pendingActionsMemberships = memberships,
        pendingActionsAwardedByName = awardedByNameMap,
        pendingActionsOrganizationName = orgNameMap
      }
