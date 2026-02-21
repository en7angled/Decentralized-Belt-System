{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module RestAPI where

import Control.Lens hiding (Context)
import Data.Swagger
import Data.Text hiding (length)
import DomainTypes.Core.BJJ (BJJBelt)
import DomainTypes.Core.Types
import DomainTypes.Transfer.Types
import GeniusYield.Imports
import GeniusYield.Types hiding (title)
import Network.Wai
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Query.Common qualified as C
import Query.Live qualified as L
import Query.Projected qualified as P
import QueryAppMonad
import RestAPI.Common
  ( achievementFilterFromParams,
    membershipHistoryFilterFromParams,
    membershipIntervalFilterFromParams,
    profileFilterFromParams,
    promotionsFilterFromParams,
    rankFilterFromParams,
    withBackend,
  )
import Servant
import Servant.Swagger
import Servant.Swagger.UI
import Types
import WebAPI.Auth
import WebAPI.CORS
import WebAPI.ServiceProbe

------------------------------------------------------------------------------------------------

--  Health API

------------------------------------------------------------------------------------------------

type ServiceProbeAPI = WebAPI.ServiceProbe.ServiceProbe Text Text

proxyServiceProbe :: Proxy ServiceProbeAPI
proxyServiceProbe = Proxy

handleReady :: QueryAppMonad (ServiceProbeStatus Text)
handleReady = verifyProjectionDbConnection

serviceProbeServer :: ServerT ServiceProbeAPI QueryAppMonad
serviceProbeServer = alwaysHealthy "query-api" :<|> handleReady

------------------------------------------------------------------------------------------------

--  Profiles API

------------------------------------------------------------------------------------------------

type Profiles =
  -- Get practitioner profile endpoint
  ( Summary "Get Practitioner Profile Information"
      :> Description "Get Practitioner Profile Information"
      :> "practitioner"
      :> Capture "profile-id" ProfileRefAC
      :> QueryFlag "liveprojection"
      :> Get '[JSON] PractitionerProfileInformation
  )
    :<|>
    -- Get organization profile endpoint
    ( Summary "Get Organization Profile Information"
        :> Description "Get Organization Profile Information"
        :> "organization"
        :> Capture "profile-id" ProfileRefAC
        :> QueryFlag "liveprojection"
        :> Get '[JSON] OrganizationProfileInformation
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
        :> QueryParam' '[Optional] "order_by" ProfilesOrderBy
        :> QueryParam' '[Optional] "sort_order" SortOrder
        :> QueryFlag "liveprojection"
        :> Get '[JSON] [Profile]
    )
    :<|>
    -- Get profiles count endpoint
    ( Summary "Get Profiles Count"
        :> Description "Get Profiles Count"
        :> "profiles"
        :> "count"
        :> QueryParams "profile" ProfileRefAC
        :> QueryParam' '[Optional] "profile_type" ProfileType
        :> QueryFlag "liveprojection"
        :> Get '[JSON] Int
    )
    :<|>
    -- Get profiles frequency endpoint
    ( Summary "Get Profiles Frequency"
        :> Description "Get Profiles Frequency by Type"
        :> "profiles"
        :> QueryFlag "liveprojection"
        :> "frequency"
        :> Get '[JSON] [(ProfileType, Int)]
    )

-- Health handlers moved to WebAPI.Health

handleGetPractitionerProfile :: ProfileRefAC -> Bool -> QueryAppMonad PractitionerProfileInformation
handleGetPractitionerProfile profileRefAC liveProjection =
  withBackend liveProjection (L.getPractitionerProfile profileRefAC) (P.getPractitionerProfile profileRefAC)

handleGetOrganizationProfile :: ProfileRefAC -> Bool -> QueryAppMonad OrganizationProfileInformation
handleGetOrganizationProfile profileRefAC liveProjection =
  withBackend liveProjection (L.getOrganizationProfile profileRefAC) (P.getOrganizationProfile profileRefAC)

handleGetProfiles ::
  Maybe Int ->
  Maybe Int ->
  [ProfileRefAC] ->
  Maybe ProfileType ->
  Maybe ProfilesOrderBy ->
  Maybe SortOrder ->
  Bool ->
  QueryAppMonad [Profile]
handleGetProfiles limit offset profileRefs profileType orderBy sortOrder liveProjection = do
  let limitOffset = C.normalizeLimitOffset limit offset
  let profileFilter = profileFilterFromParams profileRefs profileType
  let order = C.normalizeOrder orderBy sortOrder
  withBackend liveProjection (L.getProfiles limitOffset profileFilter order) (P.getProfiles limitOffset profileFilter order)

handleGetProfilesCount :: [ProfileRefAC] -> Maybe ProfileType -> Bool -> QueryAppMonad Int
handleGetProfilesCount profileRefs profileType liveProjection = do
  let profileFilter = profileFilterFromParams profileRefs profileType
  withBackend liveProjection (L.getProfilesCount profileFilter) (P.getProfilesCount profileFilter)

handleGetProfilesFrequency :: Bool -> QueryAppMonad [(ProfileType, Int)]
handleGetProfilesFrequency liveProjection =
  withBackend liveProjection L.getProfileTypeTotals P.getProfileTypeTotals

profilesServer :: ServerT Profiles QueryAppMonad
profilesServer =
  handleGetPractitionerProfile
    :<|> handleGetOrganizationProfile
    :<|> handleGetProfiles
    :<|> handleGetProfilesCount
    :<|> handleGetProfilesFrequency

------------------------------------------------------------------------------------------------

--  Promotions API

------------------------------------------------------------------------------------------------

type Promotions =
  -- Get pending promotions endpoint
  ( Summary "Get Pending Promotions"
      :> Description "Get Pending Promotions"
      :> "promotions"
      :> QueryParam' '[Optional] "limit" Int
      :> QueryParam' '[Optional] "offset" Int
      :> QueryParams "promotion" RankAC
      :> QueryParams "belt" BJJBelt
      :> QueryParams "achieved_by" ProfileRefAC
      :> QueryParams "awarded_by" ProfileRefAC
      :> QueryParam' '[Optional] "order_by" PromotionsOrderBy
      :> QueryParam' '[Optional] "sort_order" SortOrder
      :> QueryFlag "liveprojection"
      :> Get '[JSON] [Promotion]
  )
    :<|>
    -- Get promotions count endpoint
    ( Summary "Get Promotions Count"
        :> Description "Get Promotions Count"
        :> "promotions"
        :> "count"
        :> QueryParams "promotion" RankAC
        :> QueryParams "belt" BJJBelt
        :> QueryParams "achieved_by" ProfileRefAC
        :> QueryParams "awarded_by" ProfileRefAC
        :> QueryFlag "liveprojection"
        :> Get '[JSON] Int
    )
    :<|>
    -- Get promotions frequency endpoint
    ( Summary "Get Promotions Frequency"
        :> Description "Get Promotions Frequency by Belt"
        :> "promotions"
        :> QueryFlag "liveprojection"
        :> "frequency"
        :> Get '[JSON] [(BJJBelt, Int)]
    )

handleGetPromotions ::
  Maybe Int ->
  Maybe Int ->
  [RankAC] ->
  [BJJBelt] ->
  [ProfileRefAC] ->
  [ProfileRefAC] ->
  Maybe PromotionsOrderBy ->
  Maybe SortOrder ->
  Bool ->
  QueryAppMonad [Promotion]
handleGetPromotions limit offset promotionIds beltRefs achievedByRefs awardedByRefs orderBy sortOrder liveProjection = do
  let limitOffset = C.normalizeLimitOffset limit offset
  let promotionsFilter = promotionsFilterFromParams promotionIds beltRefs achievedByRefs awardedByRefs
  let order = C.normalizeOrder orderBy sortOrder
  withBackend liveProjection (L.getPromotions limitOffset promotionsFilter order) (P.getPromotions limitOffset promotionsFilter order)

handleGetPromotionsCount ::
  [RankAC] ->
  [BJJBelt] ->
  [ProfileRefAC] ->
  [ProfileRefAC] ->
  Bool ->
  QueryAppMonad Int
handleGetPromotionsCount promotionIds beltRefs achievedByRefs awardedByRefs liveProjection = do
  let promotionsFilter = promotionsFilterFromParams promotionIds beltRefs achievedByRefs awardedByRefs
  withBackend liveProjection (L.getPromotionsCount promotionsFilter) (P.getPromotionsCount promotionsFilter)

handleGetPromotionsFrequency :: Bool -> QueryAppMonad [(BJJBelt, Int)]
handleGetPromotionsFrequency liveProjection =
  withBackend liveProjection L.getPromotionBeltTotals P.getPromotionBeltTotals

promotionsServer :: ServerT Promotions QueryAppMonad
promotionsServer =
  handleGetPromotions
    :<|> handleGetPromotionsCount
    :<|> handleGetPromotionsFrequency

------------------------------------------------------------------------------------------------

--  Belts API

------------------------------------------------------------------------------------------------

type Belts =
  -- Get belts endpoint
  ( Summary "Get Belts"
      :> Description "Get Belts"
      :> "belts"
      :> QueryParam' '[Optional] "limit" Int
      :> QueryParam' '[Optional] "offset" Int
      :> QueryParams "rank" RankAC
      :> QueryParams "belt" BJJBelt
      :> QueryParams "achieved_by" ProfileRefAC
      :> QueryParams "awarded_by" ProfileRefAC
      :> QueryParam' '[Optional] "from" GYTime
      :> QueryParam' '[Optional] "to" GYTime
      :> QueryParam' '[Optional] "order_by" RanksOrderBy
      :> QueryParam' '[Optional] "sort_order" SortOrder
      :> QueryFlag "liveprojection"
      :> Get '[JSON] [Rank]
  )
    :<|>
    -- Get belts count endpoint
    ( Summary "Get Belts Count"
        :> Description "Get Belts Count"
        :> "belts"
        :> "count"
        :> QueryParams "rank" RankAC
        :> QueryParams "belt" BJJBelt
        :> QueryParams "achieved_by" ProfileRefAC
        :> QueryParams "awarded_by" ProfileRefAC
        :> QueryParam' '[Optional] "from" GYTime
        :> QueryParam' '[Optional] "to" GYTime
        :> QueryFlag "liveprojection"
        :> Get '[JSON] Int
    )
    :<|>
    -- Get belts frequency endpoint
    ( Summary "Get Belts Frequency"
        :> Description "Get Belts Frequency"
        :> "belts"
        :> QueryFlag "liveprojection"
        :> "frequency"
        :> Get '[JSON] [(BJJBelt, Int)]
    )

handleGetBelts :: Maybe Int -> Maybe Int -> [RankAC] -> [BJJBelt] -> [ProfileRefAC] -> [ProfileRefAC] -> Maybe GYTime -> Maybe GYTime -> Maybe RanksOrderBy -> Maybe SortOrder -> Bool -> QueryAppMonad [Rank]
handleGetBelts limit offset rankIds belt achievedByRefs awardedByRefs fromPractitioner toPractitioner maybeOrderBy maybeOrder liveProjection = do
  let limitOffset = C.normalizeLimitOffset limit offset
  let beltsFilter = rankFilterFromParams rankIds belt achievedByRefs awardedByRefs fromPractitioner toPractitioner
  let order = C.normalizeOrder maybeOrderBy maybeOrder
  withBackend liveProjection (L.getRanks limitOffset beltsFilter order) (P.getRanks limitOffset beltsFilter order)

handleGetBeltsCount :: [RankAC] -> [BJJBelt] -> [ProfileRefAC] -> [ProfileRefAC] -> Maybe GYTime -> Maybe GYTime -> Bool -> QueryAppMonad Int
handleGetBeltsCount rankIds belt achieved_by awarded_by fromPractitioner toPractitioner liveProjection = do
  let beltCountFilter = rankFilterFromParams rankIds belt achieved_by awarded_by fromPractitioner toPractitioner
  withBackend liveProjection (L.getRanksCount beltCountFilter) (P.getRanksCount beltCountFilter)

handleGetBeltsFrequency :: Bool -> QueryAppMonad [(BJJBelt, Int)]
handleGetBeltsFrequency liveProjection =
  withBackend liveProjection L.getBeltTotals P.getBeltTotals

beltsServer :: ServerT Belts QueryAppMonad
beltsServer = handleGetBelts :<|> handleGetBeltsCount :<|> handleGetBeltsFrequency

------------------------------------------------------------------------------------------------

--  Memberships API

------------------------------------------------------------------------------------------------

type Memberships =
  ( Summary "Get Membership Histories"
      :> Description "Get membership histories with optional filters"
      :> "membership-histories"
      :> QueryParam' '[Optional] "limit" Int
      :> QueryParam' '[Optional] "offset" Int
      :> QueryParams "organization" ProfileRefAC
      :> QueryParams "practitioner" ProfileRefAC
      :> QueryParam' '[Optional] "order_by" MembershipHistoriesOrderBy
      :> QueryParam' '[Optional] "sort_order" SortOrder
      :> QueryFlag "liveprojection"
      :> Get '[JSON] [MembershipHistoryInformation]
  )
    :<|> ( Summary "Get Membership Histories Count"
             :> Description "Get count of membership histories"
             :> "membership-histories"
             :> "count"
             :> QueryParams "organization" ProfileRefAC
             :> QueryParams "practitioner" ProfileRefAC
             :> QueryFlag "liveprojection"
             :> Get '[JSON] Int
         )
    :<|> ( Summary "Get Membership Intervals"
             :> Description "Get membership intervals with optional filters"
             :> "membership-intervals"
             :> QueryParam' '[Optional] "limit" Int
             :> QueryParam' '[Optional] "offset" Int
             :> QueryParams "practitioner" ProfileRefAC
             :> QueryParam' '[Optional] "order_by" MembershipIntervalsOrderBy
             :> QueryParam' '[Optional] "sort_order" SortOrder
             :> QueryFlag "liveprojection"
             :> Get '[JSON] [MembershipIntervalInformation]
         )
    :<|> ( Summary "Get Membership Intervals Count"
             :> Description "Get count of membership intervals"
             :> "membership-intervals"
             :> "count"
             :> QueryParams "practitioner" ProfileRefAC
             :> QueryFlag "liveprojection"
             :> Get '[JSON] Int
         )

handleGetMembershipHistories ::
  Maybe Int ->
  Maybe Int ->
  [ProfileRefAC] ->
  [ProfileRefAC] ->
  Maybe MembershipHistoriesOrderBy ->
  Maybe SortOrder ->
  Bool ->
  QueryAppMonad [MembershipHistoryInformation]
handleGetMembershipHistories limit offset orgRefs practRefs orderBy sortOrder liveProjection = do
  let limitOffset = C.normalizeLimitOffset limit offset
  let membershipHistoryFilter = membershipHistoryFilterFromParams orgRefs practRefs
  let order = C.normalizeOrder orderBy sortOrder
  withBackend liveProjection (L.getMembershipHistories limitOffset membershipHistoryFilter order) (P.getMembershipHistories limitOffset membershipHistoryFilter order)

handleGetMembershipHistoriesCount :: [ProfileRefAC] -> [ProfileRefAC] -> Bool -> QueryAppMonad Int
handleGetMembershipHistoriesCount orgRefs practRefs liveProjection = do
  let membershipHistoryFilter = membershipHistoryFilterFromParams orgRefs practRefs
  withBackend liveProjection (L.getMembershipHistoriesCount membershipHistoryFilter) (P.getMembershipHistoriesCount membershipHistoryFilter)

handleGetMembershipIntervals ::
  Maybe Int ->
  Maybe Int ->
  [ProfileRefAC] ->
  Maybe MembershipIntervalsOrderBy ->
  Maybe SortOrder ->
  Bool ->
  QueryAppMonad [MembershipIntervalInformation]
handleGetMembershipIntervals limit offset practRefs orderBy sortOrder liveProjection = do
  let limitOffset = C.normalizeLimitOffset limit offset
  let membershipIntervalFilter = membershipIntervalFilterFromParams practRefs
  let order = C.normalizeOrder orderBy sortOrder
  withBackend liveProjection (L.getMembershipIntervals limitOffset membershipIntervalFilter order) (P.getMembershipIntervals limitOffset membershipIntervalFilter order)

handleGetMembershipIntervalsCount :: [ProfileRefAC] -> Bool -> QueryAppMonad Int
handleGetMembershipIntervalsCount practRefs liveProjection = do
  let membershipIntervalFilter = membershipIntervalFilterFromParams practRefs
  withBackend liveProjection (L.getMembershipIntervalsCount membershipIntervalFilter) (P.getMembershipIntervalsCount membershipIntervalFilter)

membershipsServer :: ServerT Memberships QueryAppMonad
membershipsServer =
  handleGetMembershipHistories
    :<|> handleGetMembershipHistoriesCount
    :<|> handleGetMembershipIntervals
    :<|> handleGetMembershipIntervalsCount

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
      :> QueryParam' '[Optional] "is_accepted" Bool
      :> QueryParam' '[Optional] "from" GYTime
      :> QueryParam' '[Optional] "to" GYTime
      :> QueryParam' '[Optional] "order_by" AchievementsOrderBy
      :> QueryParam' '[Optional] "sort_order" SortOrder
      :> QueryFlag "liveprojection"
      :> Get '[JSON] [Achievement]
  )
    :<|> ( Summary "Get Achievements Count"
             :> Description "Get count of achievements"
             :> "achievements"
             :> "count"
             :> QueryParams "achievement" AchievementAC
             :> QueryParams "awarded_to" ProfileRefAC
             :> QueryParams "awarded_by" ProfileRefAC
             :> QueryParam' '[Optional] "is_accepted" Bool
             :> QueryParam' '[Optional] "from" GYTime
             :> QueryParam' '[Optional] "to" GYTime
             :> QueryFlag "liveprojection"
             :> Get '[JSON] Int
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
  Maybe AchievementsOrderBy ->
  Maybe SortOrder ->
  Bool ->
  QueryAppMonad [Achievement]
handleGetAchievements limit offset achievementIds awardedToRefs awardedByRefs isAccepted fromTime toTime orderBy sortOrder liveProjection = do
  let limitOffset = C.normalizeLimitOffset limit offset
  let achievementFilter = achievementFilterFromParams achievementIds awardedToRefs awardedByRefs isAccepted fromTime toTime
  let order = C.normalizeOrder orderBy sortOrder
  withBackend liveProjection (L.getAchievements limitOffset achievementFilter order) (P.getAchievements limitOffset achievementFilter order)

handleGetAchievementsCount ::
  [AchievementAC] ->
  [ProfileRefAC] ->
  [ProfileRefAC] ->
  Maybe Bool ->
  Maybe GYTime ->
  Maybe GYTime ->
  Bool ->
  QueryAppMonad Int
handleGetAchievementsCount achievementIds awardedToRefs awardedByRefs isAccepted fromTime toTime liveProjection = do
  let achievementFilter = achievementFilterFromParams achievementIds awardedToRefs awardedByRefs isAccepted fromTime toTime
  withBackend liveProjection (L.getAchievementsCount achievementFilter) (P.getAchievementsCount achievementFilter)

achievementsServer :: ServerT Achievements QueryAppMonad
achievementsServer = handleGetAchievements :<|> handleGetAchievementsCount

------------------------------------------------------------------------------------------------

--  Core Function API

------------------------------------------------------------------------------------------------

-- | Combined API
type CoreFunctionAPI =
  Profiles
    :<|> Promotions
    :<|> Belts
    :<|> Memberships
    :<|> Achievements

proxyCoreFunctionAPI :: Proxy CoreFunctionAPI
proxyCoreFunctionAPI = Proxy

coreFunctionServer :: ServerT CoreFunctionAPI QueryAppMonad
coreFunctionServer = profilesServer :<|> promotionsServer :<|> beltsServer :<|> membershipsServer :<|> achievementsServer

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
    & info . Data.Swagger.version .~ "1.0.0"
    & info . Data.Swagger.description ?~ "This is the Query API for the Decentralized Belt System - handles data queries for profiles, promotions, belts, membership histories and intervals, and achievements"
    & info
      . license
      ?~ "GPL-3.0 license"
    & host .~ Nothing

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
