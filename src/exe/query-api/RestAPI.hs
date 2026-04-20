{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module RestAPI where

import Constants (appVersion)
import Control.Lens hiding (Context)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (lift)
import Data.Swagger
import Data.Text hiding (length, map)
import DomainTypes.Core.BJJ (BJJBelt)
import DomainTypes.Core.Types
import DomainTypes.Transfer.Types
import GeniusYield.Imports
import GeniusYield.Types hiding (description, title)
import Network.Wai
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Query.Aggregates qualified as Agg
import Query.Common qualified as C
import Query.Live qualified as L
import Query.Pages qualified as Pg
import Query.Projected qualified as P
import Query.Rules qualified as Rules
import Query.ServiceStatus (getProtocolStatusQuery, getReadyProbe)
import QueryAppMonad
import DomainTypes.Transfer.Filters
  ( achievementFilterFromParams,
    activityFilterFromParams,
    profileFilterFromParams,
  )
import DomainTypes.Transfer.QueryResponses
import RestAPI.Common (withBackend)
import Servant
import Servant.Swagger
import Servant.Swagger.UI
import TxBuilding.Exceptions (TxBuildingException (..))
import DomainTypes.Transfer.OrderBy
import WebAPI.Auth
import WebAPI.CORS
import WebAPI.ServiceProbe
import WebAPI.Utils (addSharedSwaggerDescriptions)

------------------------------------------------------------------------------------------------

--  Health API

------------------------------------------------------------------------------------------------

type ServiceProbeAPI = WebAPI.ServiceProbe.ServiceProbe Text Text

proxyServiceProbe :: Proxy ServiceProbeAPI
proxyServiceProbe = Proxy

handleReady :: QueryAppMonad (ServiceProbeStatus Text)
handleReady = getReadyProbe

serviceProbeServer :: ServerT ServiceProbeAPI QueryAppMonad
serviceProbeServer = alwaysHealthy (pack appVersion) "query-api" :<|> handleReady

------------------------------------------------------------------------------------------------

--  Profiles API

------------------------------------------------------------------------------------------------

type Profiles =
  -- Get practitioner profile endpoint
  ( Summary "Get Practitioner Profile Information"
      :> Description "Get Practitioner Profile Information"
      :> "practitioner"
      :> Capture "profile-id" ProfileRefAC
      :> Get '[JSON] PractitionerProfileResponse
  )
    :<|>
    -- Get organization profile endpoint
    ( Summary "Get Organization Profile Information"
        :> Description "Get Organization Profile Information"
        :> "organization"
        :> Capture "profile-id" ProfileRefAC
        :> Get '[JSON] OrganizationProfileResponse
    )
    :<|>
    -- Practitioner profile detail aggregate (§12.2)
    ( Summary "Get Practitioner Profile Detail"
        :> Description "Practitioner, achievements, memberships, promotions, org maps, and awarder profiles in one response"
        :> "practitioner"
        :> Capture "profile-id" ProfileRefAC
        :> "detail"
        :> Get '[JSON] PractitionerDetailResponse
    )
    :<|>
    -- Organization profile detail aggregate (§12.3)
    ( Summary "Get Organization Profile Detail"
        :> Description "Organization, memberships, achievements issued/received, and member practitioner profiles in one response"
        :> "organization"
        :> Capture "profile-id" ProfileRefAC
        :> "detail"
        :> Get '[JSON] OrganizationDetailResponse
    )
    :<|>
    -- Get profiles endpoint
    ( Summary "Get Profiles"
        :> Description "Get Profiles"
        :> "profiles"
        :> QueryParam' '[Optional] "limit" Int
        :> QueryParam' '[Optional] "offset" Int
        :> QueryParams "profile" ProfileRefAC
        :> QueryParam' '[Optional] "profile_type" ProfileType
        :> QueryParam' '[Optional] "active_membership_organization" ProfileRefAC
        :> QueryParam' '[Optional] "membership_organization" ProfileRefAC
        :> QueryParams "belt" BJJBelt
        :> QueryParam' '[Optional] "q" Text
        :> QueryParam' '[Optional] "order_by" ProfilesOrderBy
        :> QueryParam' '[Optional] "sort_order" SortOrder
        :> Get '[JSON] [ProfileResponse]
    )
    :<|>
    -- Get profiles by wallet address (live chain query)
    ( Summary "Get Profiles by Wallet Address"
        :> Description "Returns profiles owned by a wallet address (checks User NFT ownership on-chain)."
        :> "profiles"
        :> "by-wallet"
        :> Capture "wallet-address" WalletAddress
        :> Get '[JSON] [ProfileResponse]
    )

-- Health handlers moved to WebAPI.Health

handleGetPractitionerProfile :: ProfileRefAC -> QueryAppMonad PractitionerProfileResponse
handleGetPractitionerProfile profileRefAC =
  practitionerProfileToResponse
    <$> withBackend (L.getPractitionerProfile profileRefAC) (P.getPractitionerProfile profileRefAC)

handleGetOrganizationProfile :: ProfileRefAC -> QueryAppMonad OrganizationProfileResponse
handleGetOrganizationProfile profileRefAC =
  organizationProfileToResponse
    <$> withBackend (L.getOrganizationProfile profileRefAC) (P.getOrganizationProfile profileRefAC)

handleGetPractitionerDetail :: ProfileRefAC -> QueryAppMonad PractitionerDetailResponse
handleGetPractitionerDetail = Agg.getPractitionerDetail

handleGetOrganizationDetail :: ProfileRefAC -> QueryAppMonad OrganizationDetailResponse
handleGetOrganizationDetail = Agg.getOrganizationDetail

handleGetProfiles ::
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
  QueryAppMonad [ProfileResponse]
handleGetProfiles limit offset profileRefs profileType activeMembershipOrg membershipOrg beltRefs q orderBy sortOrder = do
  let limitOffset = C.normalizeLimitOffset limit offset
  let profileFilter = profileFilterFromParams profileRefs profileType activeMembershipOrg membershipOrg beltRefs q
  let order = C.normalizeOrder orderBy sortOrder
  map profileToResponse
    <$> withBackend (L.getProfiles limitOffset profileFilter order) (P.getProfiles limitOffset profileFilter order)

handleGetProfilesByWallet :: WalletAddress -> QueryAppMonad [ProfileResponse]
handleGetProfilesByWallet (WalletAddress addr) =
  map profileToResponse <$> L.getProfilesByWalletAddress addr

profilesServer :: ServerT Profiles QueryAppMonad
profilesServer =
  handleGetPractitionerProfile
    :<|> handleGetOrganizationProfile
    :<|> handleGetPractitionerDetail
    :<|> handleGetOrganizationDetail
    :<|> handleGetProfiles
    :<|> handleGetProfilesByWallet

------------------------------------------------------------------------------------------------

--  Promotions API

------------------------------------------------------------------------------------------------

type Promotions =
  -- Get promotions frequency endpoint
  ( Summary "Get Promotions Frequency"
      :> Description "Get Promotions Frequency by Belt"
      :> "promotions"
      :> "frequency"
      :> Get '[JSON] [(BJJBelt, Int)]
  )
    :<|>
    -- Check promotion eligibility (off-chain mirror of on-chain predicate)
    ( Summary "Check Promotion Eligibility"
        :> Description
             "Off-chain mirror of the on-chain promotion validator. \
             \Returns an eligibility verdict plus structured violations \
             \(master-belt-too-low, rung-skipped, insufficient-time-in-grade, \
             \master-date-after-promotion, student-date-not-monotonic), the \
             \earliest date the time-in-grade requirement will be satisfied, \
             \and the minimum belt required of any would-be granter. If \
             \`granter` is omitted, the authority check is skipped and the \
             \response reports `required_granter_rank` only — use this to \
             \answer 'am I eligible in principle?' before the practitioner \
             \picks a coach."
        :> "practitioner"
        :> Capture "profile-id" ProfileRefAC
        :> "eligibility"
        :> QueryParam' '[Required] "target" BJJBelt
        :> QueryParam "granter" ProfileRefAC
        :> Get '[JSON] PromotionEligibilityResponse
    )

handleGetPromotionsFrequency :: QueryAppMonad [(BJJBelt, Int)]
handleGetPromotionsFrequency =
  withBackend L.getPromotionBeltTotals P.getPromotionBeltTotals

handleGetPromotionEligibility ::
  ProfileRefAC ->
  BJJBelt ->
  Maybe ProfileRefAC ->
  QueryAppMonad PromotionEligibilityResponse
handleGetPromotionEligibility = Rules.getPromotionEligibility

promotionsServer :: ServerT Promotions QueryAppMonad
promotionsServer = handleGetPromotionsFrequency :<|> handleGetPromotionEligibility

-- Memberships API removed (no frontend call sites)

------------------------------------------------------------------------------------------------

--  Achievements API

------------------------------------------------------------------------------------------------

type Achievements =
  ( Summary "Get Achievements"
      :> Description "Get achievements with optional filters"
      :> "achievements"
      :> QueryParam' '[Optional] "limit" Int
      :> QueryParam' '[Optional] "offset" Int
      :> QueryParams "achievement" AchievementAC
      :> QueryParams "awarded_to" ProfileRefAC
      :> QueryParams "awarded_by" ProfileRefAC
      :> QueryParam' '[Optional] "accepted" Bool
      :> QueryParam' '[Optional] "from" GYTime
      :> QueryParam' '[Optional] "to" GYTime
      :> QueryParam' '[Optional] "q" Text
      :> QueryParam' '[Optional] "order_by" AchievementsOrderBy
      :> QueryParam' '[Optional] "sort_order" SortOrder
      :> Get '[JSON] [AchievementResponse]
  )
    :<|>
    -- Get single achievement by ID
    ( Summary "Get Achievement by ID"
        :> Description "Get a single achievement by its asset class ID"
        :> "achievement"
        :> Capture "achievement-id" AchievementAC
        :> Get '[JSON] AchievementInformationResponse
    )

handleGetAchievements ::
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
  QueryAppMonad [AchievementResponse]
handleGetAchievements limit offset achievementIds awardedToRefs awardedByRefs accepted fromTime toTime q orderBy sortOrder = do
  let limitOffset = C.normalizeLimitOffset limit offset
  let achievementFilter = achievementFilterFromParams achievementIds awardedToRefs awardedByRefs accepted fromTime toTime q
  let order = C.normalizeOrder orderBy sortOrder
  map achievementToResponse
    <$> withBackend (L.getAchievements limitOffset achievementFilter order) (P.getAchievements limitOffset achievementFilter order)

handleGetAchievementById :: AchievementAC -> QueryAppMonad AchievementInformationResponse
handleGetAchievementById achId = do
  let achFilter = achievementFilterFromParams [achId] [] [] Nothing Nothing Nothing Nothing
  results <- withBackend (L.getAchievements Nothing achFilter Nothing) (P.getAchievements Nothing achFilter Nothing)
  case results of
    (a : _) -> Agg.achievementToInformation a
    [] -> liftIO $ throwIO AchievementNotFound

achievementsServer :: ServerT Achievements QueryAppMonad
achievementsServer = handleGetAchievements :<|> handleGetAchievementById

------------------------------------------------------------------------------------------------

--  Core Function API

------------------------------------------------------------------------------------------------

------------------------------------------------------------------------------------------------

--  Search API

------------------------------------------------------------------------------------------------

type SearchAPI =
  Summary "Unified search"
    :> Description "Search across practitioners, organizations, ranks, promotions, and achievements. Query parameter q is required."
    :> "search"
    :> QueryParam' '[Required] "q" Text
    :> Get '[JSON] SearchResults

handleSearch :: Text -> QueryAppMonad SearchResults
handleSearch q = withBackend (L.searchLive q) (P.searchProjected q)

searchServer :: ServerT SearchAPI QueryAppMonad
searchServer = handleSearch

------------------------------------------------------------------------------------------------

--  Lineage API

------------------------------------------------------------------------------------------------

type LineageAPI =
  Summary "Lineage graph"
    :> Description "Direct lineage tree: ancestor chain (upward via awarded_by) and descendant subtree (downward via awarded_to) from root profile; optional minimum belt filter on edges."
    :> "lineage"
    :> QueryParam' '[Required] "root" ProfileRefAC
    :> QueryParam' '[Required] "ancestors" Int
    :> QueryParam' '[Required] "descendants" Int
    :> QueryParam' '[Optional] "min_belt" BJJBelt
    :> Get '[JSON] LineageGraphData

handleLineage :: ProfileRefAC -> Int -> Int -> Maybe BJJBelt -> QueryAppMonad LineageGraphData
handleLineage root anc desc minBelt
  | anc < 0 || desc < 0 = QueryAppMonad $ lift $ throwError err400
  | otherwise = P.getLineageGraph root anc desc minBelt

lineageServer :: ServerT LineageAPI QueryAppMonad
lineageServer = handleLineage

------------------------------------------------------------------------------------------------

--  Protocol Status API

------------------------------------------------------------------------------------------------

type ProtocolStatusAPI =
  Summary "Get Protocol Status"
    :> Description "Get protocol operational status including pause state, min UTxO value, and fee configuration"
    :> "protocol-status"
    :> Get '[JSON] ProtocolStatus

handleGetProtocolStatus :: QueryAppMonad ProtocolStatus
handleGetProtocolStatus = getProtocolStatusQuery

protocolStatusServer :: ServerT ProtocolStatusAPI QueryAppMonad
protocolStatusServer = handleGetProtocolStatus

------------------------------------------------------------------------------------------------

--  Activity feed API

------------------------------------------------------------------------------------------------

type ActivityAPI =
  Summary "Activity feed"
    :> Description "Reverse-chronological activity feed across profiles, promotions, achievements, and memberships."
    :> "activity"
    :> QueryParam' '[Optional] "limit" Int
    :> QueryParam' '[Optional] "offset" Int
    :> QueryParam' '[Optional] "event_type" ActivityEventType
    :> QueryParam' '[Optional] "actor" ProfileRefAC
    :> QueryParam' '[Optional] "since" GYTime
    :> Get '[JSON] [ActivityEventResponse]

handleGetActivity ::
  Maybe Int ->
  Maybe Int ->
  Maybe ActivityEventType ->
  Maybe ProfileRefAC ->
  Maybe GYTime ->
  QueryAppMonad [ActivityEventResponse]
handleGetActivity limit offset eventType actor since = do
  let limitOffset = C.normalizeLimitOffset (Just (fromMaybe 20 limit)) offset
      actFilter = activityFilterFromParams eventType actor since
  withBackend (L.getActivityFeed limitOffset actFilter) (P.getActivityFeed limitOffset actFilter)

activityServer :: ServerT ActivityAPI QueryAppMonad
activityServer = handleGetActivity

------------------------------------------------------------------------------------------------

--  Explorer pages API (§12.5)

------------------------------------------------------------------------------------------------

type PagesAPI =
  ( Summary "Promotions explorer page"
      :> Description "Paginated promotion information, total, and monthly belt histogram. Same query parameters as GET /promotions."
      :> "pages"
      :> "promotions"
      :> QueryParam' '[Optional] "limit" Int
      :> QueryParam' '[Optional] "offset" Int
      :> QueryParams "promotion" RankAC
      :> QueryParams "belt" BJJBelt
      :> QueryParams "achieved_by" ProfileRefAC
      :> QueryParams "awarded_by" ProfileRefAC
      :> QueryParams "profile" ProfileRefAC
      :> QueryParam' '[Optional] "q" Text
      :> QueryParam' '[Optional] "from" GYTime
      :> QueryParam' '[Optional] "to" GYTime
      :> QueryParams "state" PromotionState
      :> QueryParam' '[Optional] "order_by" PromotionsOrderBy
      :> QueryParam' '[Optional] "sort_order" SortOrder
      :> Get '[JSON] PromotionsPageResponse
  )
    :<|> ( Summary "Achievements explorer page"
             :> Description "Paginated achievements, total, and monthly accepted/pending counts. Same query parameters as GET /achievements."
             :> "pages"
             :> "achievements"
             :> QueryParam' '[Optional] "limit" Int
             :> QueryParam' '[Optional] "offset" Int
             :> QueryParams "achievement" AchievementAC
             :> QueryParams "awarded_to" ProfileRefAC
             :> QueryParams "awarded_by" ProfileRefAC
             :> QueryParam' '[Optional] "accepted" Bool
             :> QueryParam' '[Optional] "from" GYTime
             :> QueryParam' '[Optional] "to" GYTime
             :> QueryParam' '[Optional] "q" Text
             :> QueryParam' '[Optional] "order_by" AchievementsOrderBy
             :> QueryParam' '[Optional] "sort_order" SortOrder
             :> Get '[JSON] AchievementsPageResponse
         )
    :<|> ( Summary "Profiles explorer page"
             :> Description "Paginated profiles, total, and profile-type frequency. Same query parameters as GET /profiles."
             :> "pages"
             :> "profiles"
             :> QueryParam' '[Optional] "limit" Int
             :> QueryParam' '[Optional] "offset" Int
             :> QueryParams "profile" ProfileRefAC
             :> QueryParam' '[Optional] "profile_type" ProfileType
             :> QueryParam' '[Optional] "active_membership_organization" ProfileRefAC
             :> QueryParam' '[Optional] "membership_organization" ProfileRefAC
             :> QueryParams "belt" BJJBelt
             :> QueryParam' '[Optional] "q" Text
             :> QueryParam' '[Optional] "order_by" ProfilesOrderBy
             :> QueryParam' '[Optional] "sort_order" SortOrder
             :> Get '[JSON] ProfilesPageResponse
         )
    :<|> ( Summary "Practitioner explorer page"
             :> Description "Practitioner profiles with membership histories and organization id-to-name map. Filters match GET /profiles except profile_type is always Practitioner."
             :> "pages"
             :> "practitioner-explorer"
             :> QueryParam' '[Optional] "limit" Int
             :> QueryParam' '[Optional] "offset" Int
             :> QueryParams "profile" ProfileRefAC
             :> QueryParam' '[Optional] "active_membership_organization" ProfileRefAC
             :> QueryParam' '[Optional] "membership_organization" ProfileRefAC
             :> QueryParams "belt" BJJBelt
             :> QueryParam' '[Optional] "q" Text
             :> QueryParam' '[Optional] "order_by" ProfilesOrderBy
             :> QueryParam' '[Optional] "sort_order" SortOrder
             :> Get '[JSON] PractitionerExplorerPageResponse
         )
    :<|> ( Summary "Home explorer hub"
             :> Description "Recent promotions and latest practitioners."
             :> "pages"
             :> "home-explorer-hub"
             :> QueryParam' '[Optional] "recent_limit" Int
             :> QueryParam' '[Optional] "latest_practitioner_limit" Int
             :> Get '[JSON] HomeExplorerHubPageResponse
         )
    :<|> ( Summary "Dashboard page"
             :> Description "§12.1 totals, belt frequency, recent promotions, monthly histogram, latest organizations, practitioners, and top organizations by member count."
             :> "pages"
             :> "dashboard"
             :> QueryParam' '[Optional] "recent_promotions_limit" Int
             :> QueryParam' '[Optional] "latest_organizations_limit" Int
             :> QueryParam' '[Optional] "latest_practitioners_limit" Int
             :> QueryParam' '[Optional] "top_organizations_limit" Int
             :> Get '[JSON] DashboardPageResponse
         )
    :<|> ( Summary "Pending actions for a profile"
             :> Description "Pending promotions, unaccepted achievements, and unaccepted membership intervals for a profile."
             :> "pages"
             :> "pending-actions"
             :> Capture "profile-id" ProfileRefAC
             :> Get '[JSON] PendingActionsResponse
         )

handleGetPromotionsPage ::
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
handleGetPromotionsPage = Pg.getPromotionsPage

handleGetAchievementsPage ::
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
handleGetAchievementsPage = Pg.getAchievementsPage

handleGetProfilesPage ::
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
handleGetProfilesPage = Pg.getProfilesPage

handleGetPractitionerExplorerPage ::
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
handleGetPractitionerExplorerPage = Pg.getPractitionerExplorerPage

handleGetHomeExplorerHubPage ::
  Maybe Int ->
  Maybe Int ->
  QueryAppMonad HomeExplorerHubPageResponse
handleGetHomeExplorerHubPage recentLimit latestLimit =
  Pg.getHomeExplorerHubPage (fromMaybe 10 recentLimit) (fromMaybe 10 latestLimit)

handleGetDashboardPage ::
  Maybe Int ->
  Maybe Int ->
  Maybe Int ->
  Maybe Int ->
  QueryAppMonad DashboardPageResponse
handleGetDashboardPage recentPromotionsLimit latestOrganizationsLimit latestPractitionersLimit topOrganizationsLimit =
  Pg.getDashboardPage
    (fromMaybe 10 recentPromotionsLimit)
    (fromMaybe 10 latestOrganizationsLimit)
    (fromMaybe 10 latestPractitionersLimit)
    (fromMaybe 5 topOrganizationsLimit)

handleGetPendingActions :: ProfileRefAC -> QueryAppMonad PendingActionsResponse
handleGetPendingActions = Pg.getPendingActionsPage

pagesServer :: ServerT PagesAPI QueryAppMonad
pagesServer =
  handleGetPromotionsPage
    :<|> handleGetAchievementsPage
    :<|> handleGetProfilesPage
    :<|> handleGetPractitionerExplorerPage
    :<|> handleGetHomeExplorerHubPage
    :<|> handleGetDashboardPage
    :<|> handleGetPendingActions

------------------------------------------------------------------------------------------------

--  Core Function API

------------------------------------------------------------------------------------------------

-- | Combined API
type CoreFunctionAPI =
  Profiles
    :<|> Promotions
    :<|> Achievements
    :<|> SearchAPI
    :<|> LineageAPI
    :<|> ProtocolStatusAPI
    :<|> ActivityAPI
    :<|> PagesAPI

proxyCoreFunctionAPI :: Proxy CoreFunctionAPI
proxyCoreFunctionAPI = Proxy

coreFunctionServer :: ServerT CoreFunctionAPI QueryAppMonad
coreFunctionServer =
  profilesServer
    :<|> promotionsServer
    :<|> achievementsServer
    :<|> searchServer
    :<|> lineageServer
    :<|> protocolStatusServer
    :<|> activityServer
    :<|> pagesServer

------------------------------------------------------------------------------------------------

--  Swagger API

------------------------------------------------------------------------------------------------

type PublicAPI =
  ServiceProbeAPI
    :<|> CoreFunctionAPI

proxyPublicAPI :: Proxy PublicAPI
proxyPublicAPI = Proxy

apiSwagger :: Swagger
apiSwagger =
  toSwagger proxyPublicAPI
    & info . title .~ "Decentralized Belt System Query API"
    & info . Data.Swagger.version .~ "3.2.0"
    & info . Data.Swagger.description ?~ "This is the Query API for the Decentralized Belt System - handles data queries for profiles, promotions, membership histories and intervals, and achievements"
    & info . license ?~ "GPL-3.0 license"
    & host .~ Nothing
    -- Response descriptions
    & allOperations . responses . at 400 ?~ Inline (mempty {_responseDescription = "Invalid query parameters or malformed request"})
    & allOperations . responses . at 404 ?~ Inline (mempty {_responseDescription = "Requested entity not found (profile, achievement, membership, etc.)"})
    & allOperations . responses . at 503 ?~ Inline (mempty {_responseDescription = "Service not ready (chain-sync still syncing or protocol paused)"})
    -- Shared type descriptions
    & addSharedSwaggerDescriptions
    -- Query-API-specific type descriptions
    & definitions . at "PromotionState" . mapped . description ?~ "Promotion lifecycle state: Pending (awaiting acceptance), Accepted (confirmed), or Superseded (replaced by a newer promotion)"
    & definitions . at "ActivityEventType" . mapped . description ?~ "Type of activity event: ProfileCreated, PromotionIssued, PromotionAccepted, PromotionSuperseded, AchievementAwarded, AchievementAccepted, MembershipGranted, MembershipAccepted"
    -- Response type descriptions
    & definitions . at "ProfileResponse" . mapped . description ?~ "Basic profile summary with thumbnail URI derived from the on-chain image URI"
    & definitions . at "PractitionerProfileResponse" . mapped . description ?~ "Practitioner profile with current and previous belt ranks"
    & definitions . at "OrganizationProfileResponse" . mapped . description ?~ "Organization (academy/gym) profile"
    & definitions . at "AchievementResponse" . mapped . description ?~ "Achievement with awarded-to/by profile IDs and acceptance status"
    & definitions . at "AchievementInformationResponse" . mapped . description ?~ "Enriched achievement with full awarded-to and awarded-by profile objects resolved inline"
    & definitions . at "PromotionInformationResponse" . mapped . description ?~ "Enriched promotion with full achieved-by and awarded-by practitioner profiles resolved inline"
    & definitions . at "PractitionerDetailResponse" . mapped . description ?~ "Complete practitioner profile page: profile, achievements, memberships, promotions given/received, and related org/awarder maps"
    & definitions . at "OrganizationDetailResponse" . mapped . description ?~ "Complete organization profile page: profile, memberships, achievements issued/received, and member profile map"
    & definitions . at "SearchResults" . mapped . description ?~ "Unified search results grouped by category (practitioners, organizations, promotions, achievements)"
    & definitions . at "LineageGraphData" . mapped . description ?~ "Directed graph of belt lineage: nodes are profiles, edges are promotions (from awarder to recipient)"
    & definitions . at "PromotionsPageResponse" . mapped . description ?~ "Promotions explorer page: paginated items + total count + belt frequency breakdown + monthly trend data"
    & definitions . at "AchievementsPageResponse" . mapped . description ?~ "Achievements explorer page: paginated items + total count + monthly accepted/pending stats"
    & definitions . at "ProfilesPageResponse" . mapped . description ?~ "Profiles explorer page: paginated items + total count + practitioner/organization frequency"
    & definitions . at "PractitionerExplorerPageResponse" . mapped . description ?~ "Practitioner explorer page: practitioners with their memberships + org name lookup map"
    & definitions . at "HomeExplorerHubPageResponse" . mapped . description ?~ "Home page: recent promotions and latest practitioner profiles"
    & definitions . at "DashboardPageResponse" . mapped . description ?~ "Dashboard aggregate: totals, belt frequency, recent promotions, monthly trends, top organizations"
    & definitions . at "PendingActionsResponse" . mapped . description ?~ "Pending actions inbox for a profile: unaccepted promotions, achievements, and membership intervals"
    & definitions . at "ActivityEventResponse" . mapped . description ?~ "Single activity feed entry with event type, actor/target IDs, timestamp, and human-readable details"
    & definitions . at "PromotionEligibilityResponse" . mapped . description ?~ "Promotion eligibility verdict: off-chain mirror of the on-chain validator with structured violations, earliest-eligible date, and required granter rank"
    & definitions . at "PromotionViolation" . mapped . description ?~ "One rule the on-chain promotion validator would reject: master-belt-too-low, rung-skipped, insufficient-time-in-grade, master-date-after-promotion, or student-date-not-monotonic"

swaggerServer :: ServerT (SwaggerSchemaUI "swagger-ui" "swagger-api.json") QueryAppMonad
swaggerServer = swaggerSchemaUIServerT apiSwagger

------------------------------------------------------------------------------------------------

--  Combined API

------------------------------------------------------------------------------------------------

-- | Adding Basic Auth
type PrivateRestAPI =
  BasicAuth "user-realm" AuthUser :> CoreFunctionAPI

proxyPrivateRestAPI :: Proxy PrivateRestAPI
proxyPrivateRestAPI = Proxy

privateRestServer :: ServerT PrivateRestAPI QueryAppMonad
privateRestServer = const coreFunctionServer

-- | Adding Swagger UI on top of Private Rest API
type FullAPI =
  SwaggerSchemaUI "swagger-ui" "swagger-api.json"
    :<|> ServiceProbeAPI
    :<|> PrivateRestAPI

proxyFullAPI :: Proxy FullAPI
proxyFullAPI = Proxy

fullServer :: ServerT FullAPI QueryAppMonad
fullServer = swaggerServer :<|> serviceProbeServer :<|> privateRestServer

------------------------------------------------------------------------------------------------

-- | Servant Application

------------------------------------------------------------------------------------------------

mkBJJApp :: QueryAppContext -> Application
mkBJJApp ctx =
  WebAPI.CORS.setupCors $
    provideOptions proxyPublicAPI $
      serveWithContext proxyFullAPI basicCtx hoistedServer
  where
    basicCtx = basicAuthServerContext (authContext ctx)
    hoistedServer = hoistServerWithContext proxyFullAPI proxyBasicAuthContext (runAppMonad ctx) fullServer
