-- | Page-shaped aggregate queries (backend-api-requirements §12): compose existing
-- projected/live @get*@ calls and enrich with nested profile maps.
module Query.Aggregates
  ( getPractitionerDetail,
    getOrganizationDetail,
    promotionToInformation,
    promotionsToInformationBatch,
    achievementToInformation,
    buildOrgMaps,
    resolveProfilesBatch,
  )
where

import Control.Exception (SomeException, throwIO, try)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (asks)
import Data.List (nub)
import Data.Map.Strict qualified as M
import Data.Text (Text)
import Database.Persist (Entity (..))
import Database.Persist qualified as P
import Database.Persist.Sql (SqlPersistT, runSqlPool)
import DomainTypes.Core.BJJ (BJJBelt (..))
import DomainTypes.Core.Types
import DomainTypes.Transfer.Types
import GeniusYield.Types (GYTime)
import DomainTypes.Transfer.Filters qualified as F
import DomainTypes.Transfer.QueryResponses
  ( OrganizationDetailResponse (..),
    OrganizationProfileResponse,
    PractitionerDetailResponse (..),
    PractitionerProfileResponse,
    AchievementInformationResponse,
    PromotionInformationResponse,
    achievementInformationToResponse,
    achievementToResponse,
    organizationProfileToResponse,
    practitionerProfileToResponse,
    promotionInformationToResponse,
  )
import Query.Live qualified as L
import Query.Projected qualified as P
import QueryAppMonad (QueryAppContext (..), QueryAppMonad)
import RestAPI.Common (withBackend)
import Storage
import TxBuilding.Context (runQuery)
import TxBuilding.Exceptions (TxBuildingException (ProfileNotFound))
import TxBuilding.Lookups (getOrganizationInformation, getPractitionerInformation)

-- | When a promotion edge refers to an organization profile, we still emit
-- @PractitionerProfileInformation@-shaped JSON (§12): use a placeholder rank
-- at the promotion date so the object decodes on the frontend.
organizationToShimPractitioner :: OrganizationProfileInformation -> GYTime -> PractitionerProfileInformation
organizationToShimPractitioner org t =
  PractitionerProfileInformation
    { practitionerId = organizationId org,
      practitionerName = organizationName org,
      practitionerDescription = organizationDescription org,
      practitionerImageURI = organizationImageURI org,
      practitionerCurrentRank =
        Rank
          { rankId = organizationId org,
            rankBelt = White,
            rankAchievedByProfileId = organizationId org,
            rankAwardedByProfileId = organizationId org,
            rankAchievementDate = t
          },
      practitionerPreviousRanks = []
    }

lookupProfileTypeProjected ::
  ProfileRefAC ->
  SqlPersistT IO (Maybe ProfileType)
lookupProfileTypeProjected pid = do
  m <- P.getBy (UniqueProfileProjection pid)
  return $ case m of
    Nothing -> Nothing
    Just (Entity _ row) -> Just (profileProjectionProfileType row)

-- | Resolve a profile id to @PractitionerProfileInformation@ for promotion nesting:
-- practitioners load normally; organizations are shimmed (see 'organizationToShimPractitioner').
resolveProfileForPromotionSide ::
  ProfileRefAC ->
  GYTime ->
  QueryAppMonad PractitionerProfileInformation
resolveProfileForPromotionSide pid t = do
  live <- asks liveProjection
  if live
    then do
      ctx <- asks providerContext
      e <- liftIO $ try @SomeException $ runQuery ctx (getPractitionerInformation pid)
      case e of
        Right p -> return p
        Left _ -> do
          eOrg <- liftIO $ try @SomeException $ runQuery ctx (getOrganizationInformation pid)
          case eOrg of
            Right org -> return $ organizationToShimPractitioner org t
            Left _ -> liftIO $ throwIO ProfileNotFound
    else do
      pool <- asks pgPool
      mTy <- liftIO $ runSqlPool (lookupProfileTypeProjected pid) pool
      case mTy of
        Nothing -> liftIO $ throwIO ProfileNotFound
        Just Practitioner -> P.getPractitionerProfile pid
        Just Organization -> do
          org <- P.getOrganizationProfile pid
          return $ organizationToShimPractitioner org t

promotionToInformation :: Promotion -> QueryAppMonad PromotionInformationResponse
promotionToInformation p = do
  achieved <- resolveProfileForPromotionSide (promotionAchievedByProfileId p) (promotionAchievementDate p)
  awarded <- resolveProfileForPromotionSide (promotionAwardedByProfileId p) (promotionAchievementDate p)
  return $ promotionInformationToResponse achieved awarded p

-- | Resolve an achievement's awarded-to and awarded-by profiles (which can be
-- either practitioners or organizations) and build an enriched response.
achievementToInformation :: Achievement -> QueryAppMonad AchievementInformationResponse
achievementToInformation a = do
  let mkFilter ref =
        Just $ F.ProfileFilter (Just [ref]) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
      toFilter = mkFilter (achievementAwardedToProfileId a)
      byFilter = mkFilter (achievementAwardedByProfileId a)
  awardedToProfiles <- withBackend
    (L.getProfiles Nothing toFilter Nothing)
    (P.getProfiles Nothing toFilter Nothing)
  awardedByProfiles <- withBackend
    (L.getProfiles Nothing byFilter Nothing)
    (P.getProfiles Nothing byFilter Nothing)
  case (awardedToProfiles, awardedByProfiles) of
    (toProf : _, byProf : _) -> return $ achievementInformationToResponse toProf byProf a
    _ -> liftIO $ throwIO ProfileNotFound

-- | Batch-resolve a set of profile IDs to @PractitionerProfileInformation@ for
-- promotion/rank nesting. Organizations are shimmed via 'organizationToShimPractitioner'.
-- Each ID is resolved at most once; the caller supplies @(id, fallbackTime)@ pairs where
-- the time is used only for the org shim placeholder rank.
resolveProfilesBatch ::
  [(ProfileRefAC, GYTime)] ->
  QueryAppMonad (M.Map ProfileRefAC PractitionerProfileInformation)
resolveProfilesBatch [] = return M.empty
resolveProfilesBatch pairs = do
  let idTimeMap = M.fromList pairs -- keeps last time per id (any is fine for shim)
      distinctIds = M.keys idTimeMap
  withBackend
    ( M.fromList
        <$> mapM
          ( \pid -> do
              let t = idTimeMap M.! pid
              info <- resolveProfileForPromotionSide pid t
              return (pid, info)
          )
          distinctIds
    )
    ( do
        practMap <- P.getPractitionerProfilesBatch distinctIds
        orgMap <- P.getOrganizationProfilesBatch distinctIds
        return $
          M.union
            practMap
            ( M.mapWithKey
                ( \pid org ->
                    organizationToShimPractitioner org (idTimeMap M.! pid)
                )
                orgMap
            )
    )

-- | Batch-enrich a list of promotions with resolved profile information.
promotionsToInformationBatch :: [Promotion] -> QueryAppMonad [PromotionInformationResponse]
promotionsToInformationBatch [] = return []
promotionsToInformationBatch ps = do
  let idTimePairs =
        nub $
          concatMap
            ( \p ->
                [ (promotionAchievedByProfileId p, promotionAchievementDate p),
                  (promotionAwardedByProfileId p, promotionAchievementDate p)
                ]
            )
            ps
  profileMap <- resolveProfilesBatch idTimePairs
  return
    [ promotionInformationToResponse achieved awarded p
      | p <- ps,
        let achieved = profileMap M.! promotionAchievedByProfileId p,
        let awarded = profileMap M.! promotionAwardedByProfileId p
    ]

-- | Classify profile id as practitioner or organization (for awarder maps).
loadPractitionerOrOrg ::
  ProfileRefAC ->
  QueryAppMonad (Either PractitionerProfileInformation OrganizationProfileInformation)
loadPractitionerOrOrg pid = do
  live <- asks liveProjection
  if live
    then do
      ctx <- asks providerContext
      e <- liftIO $ try @SomeException $ runQuery ctx (getPractitionerInformation pid)
      case e of
        Right p -> return $ Left p
        Left _ -> do
          eOrg <- liftIO $ try @SomeException $ runQuery ctx (getOrganizationInformation pid)
          case eOrg of
            Right org -> return $ Right org
            Left _ -> liftIO $ throwIO ProfileNotFound
    else do
      pool <- asks pgPool
      mTy <- liftIO $ runSqlPool (lookupProfileTypeProjected pid) pool
      case mTy of
        Nothing -> liftIO $ throwIO ProfileNotFound
        Just Practitioner -> Left <$> P.getPractitionerProfile pid
        Just Organization -> Right <$> P.getOrganizationProfile pid

buildAwardedByMaps ::
  [ProfileRefAC] ->
  QueryAppMonad (M.Map ProfileRefAC PractitionerProfileResponse, M.Map ProfileRefAC OrganizationProfileResponse)
buildAwardedByMaps ids = do
  let distinctIds = nub ids
  entries <- mapM (\i -> (i,) <$> loadPractitionerOrOrg i) distinctIds
  let prs =
        M.fromList
          [ (i, practitionerProfileToResponse p)
            | (i, Left p) <- entries
          ]
      orgs =
        M.fromList
          [ (i, organizationProfileToResponse o)
            | (i, Right o) <- entries
          ]
  return (prs, orgs)

buildOrgMaps ::
  [ProfileRefAC] ->
  QueryAppMonad (M.Map ProfileRefAC Text, M.Map ProfileRefAC OrganizationProfileResponse)
buildOrgMaps [] = return (M.empty, M.empty)
buildOrgMaps oids = do
  orgInfoMap <-
    withBackend
      ( do
          let distinctOids = nub oids
          infos <- mapM L.getOrganizationProfile distinctOids
          return $ M.fromList [(organizationId o, o) | o <- infos]
      )
      (P.getOrganizationProfilesBatch oids)
  let orgIdToName = M.map organizationName orgInfoMap
      orgIdToOrg = M.map organizationProfileToResponse orgInfoMap
  return (orgIdToName, orgIdToOrg)

-- | §12.2 — single response for practitioner profile page.
getPractitionerDetail :: ProfileRefAC -> QueryAppMonad PractitionerDetailResponse
getPractitionerDetail pid = do
  practitionerInfo <- withBackend (L.getPractitionerProfile pid) (P.getPractitionerProfile pid)
  let currentRank = practitionerCurrentRank practitionerInfo
      prevRanks = practitionerPreviousRanks practitionerInfo
      achFilter =
        Just $
          F.AchievementFilter Nothing (Just [pid]) Nothing Nothing (Nothing, Nothing) Nothing
      mhFilter =
        Just $
          F.MembershipHistoryFilter Nothing (Just [pid]) Nothing
      promotionsGivenFilter =
        Just $
          F.PromotionFilter Nothing Nothing Nothing (Just [pid]) (Nothing, Nothing) Nothing Nothing
      promotionsReceivedFilter =
        Just $
          F.PromotionFilter Nothing Nothing (Just [pid]) Nothing (Nothing, Nothing) Nothing Nothing
  achievements <- withBackend(L.getAchievements Nothing achFilter Nothing) (P.getAchievements Nothing achFilter Nothing)
  memberships <- withBackend(L.getMembershipHistories Nothing mhFilter Nothing) (P.getMembershipHistories Nothing mhFilter Nothing)
  promotionsGiven <- withBackend(L.getPromotions Nothing promotionsGivenFilter Nothing) (P.getPromotions Nothing promotionsGivenFilter Nothing)
  promotionsReceived <- withBackend(L.getPromotions Nothing promotionsReceivedFilter Nothing) (P.getPromotions Nothing promotionsReceivedFilter Nothing)
  let allPromos = promotionsGiven ++ promotionsReceived
  allInfos <- promotionsToInformationBatch allPromos
  let (pgInfos, prInfos) = splitAt (length promotionsGiven) allInfos
  let orgIdsFromMh =
        nub $
          concatMap
            (\mh -> membershipHistoryInformationOrganizationId mh : map membershipIntervalInformationOrganizationId (membershipHistoryInformationIntervals mh))
            memberships
  (orgIdToName, orgIdToOrg) <- buildOrgMaps orgIdsFromMh
  let awarderIds =
        nub $
          map achievementAwardedByProfileId achievements
            ++ map rankAwardedByProfileId (currentRank : prevRanks)
  (awardedByProfiles, awardedByOrgs) <- buildAwardedByMaps awarderIds
  return
    PractitionerDetailResponse
      { practitionerDetailPractitioner = practitionerProfileToResponse practitionerInfo,
        practitionerDetailAchievements = map achievementToResponse achievements,
        practitionerDetailMemberships = memberships,
        practitionerDetailPromotionsGiven = pgInfos,
        practitionerDetailPromotionsReceived = prInfos,
        practitionerDetailOrgIdToName = orgIdToName,
        practitionerDetailOrgIdToOrg = orgIdToOrg,
        practitionerDetailAwardedByProfiles = awardedByProfiles,
        practitionerDetailAwardedByOrgs = awardedByOrgs
      }

-- | §12.3 — single response for organization profile page.
getOrganizationDetail :: ProfileRefAC -> QueryAppMonad OrganizationDetailResponse
getOrganizationDetail oid = do
  orgInfo <- withBackend (L.getOrganizationProfile oid) (P.getOrganizationProfile oid)
  let mhFilter =
        Just $
          F.MembershipHistoryFilter (Just [oid]) Nothing Nothing
      issuedFilter =
        Just $
          F.AchievementFilter Nothing Nothing (Just [oid]) Nothing (Nothing, Nothing) Nothing
      receivedFilter =
        Just $
          F.AchievementFilter Nothing (Just [oid]) Nothing Nothing (Nothing, Nothing) Nothing
  memberships <- withBackend(L.getMembershipHistories Nothing mhFilter Nothing) (P.getMembershipHistories Nothing mhFilter Nothing)
  achievementsIssued <- withBackend(L.getAchievements Nothing issuedFilter Nothing) (P.getAchievements Nothing issuedFilter Nothing)
  achievementsReceived <- withBackend(L.getAchievements Nothing receivedFilter Nothing) (P.getAchievements Nothing receivedFilter Nothing)
  let practitionerIds = nub $ map membershipHistoryInformationPractitionerId memberships
  memberProfiles <-
    withBackend
      ( do
          infos <- mapM L.getPractitionerProfile practitionerIds
          return $ M.fromList [(practitionerId i, practitionerProfileToResponse i) | i <- infos]
      )
      (M.map practitionerProfileToResponse <$> P.getPractitionerProfilesBatch practitionerIds)
  return
    OrganizationDetailResponse
      { organizationDetailOrganization = organizationProfileToResponse orgInfo,
        organizationDetailMemberships = memberships,
        organizationDetailAchievementsIssued = map achievementToResponse achievementsIssued,
        organizationDetailAchievementsReceived = map achievementToResponse achievementsReceived,
        organizationDetailMemberProfiles = memberProfiles
      }
