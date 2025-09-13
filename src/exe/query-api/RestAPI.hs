{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module RestAPI where

import Control.Lens hiding (Context)
import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT), asks)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.List
import Data.String (IsString (..))
import Data.Swagger
import Data.Text hiding (length)
import qualified Data.Text as T
import qualified Data.Text.Encoding
import DomainTypes.Core.Types
import DomainTypes.Transfer.Types
import GHC.Generics (Generic)
import GeniusYield.Imports
import GeniusYield.Types hiding (title)
import qualified Network.HTTP.Types as HttpTypes
import Network.HTTP.Types.Header
import Network.Wai
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Onchain.BJJ (BJJBelt, parseBelt)
import qualified Query.Common as C
import qualified Query.Live as L
import qualified Query.Projected as P
import QueryAppMonad
import Servant
import Servant.Swagger
import Servant.Swagger.UI
import TxBuilding.Context (runQuery)
import TxBuilding.Interactions
import TxBuilding.Lookups (getOrganizationInformation, getPractiotionerInformation)
import TxBuilding.Transactions
import Types
import WebAPI.Auth
import WebAPI.CORS
import WebAPI.ServiceProbe

instance FromHttpApiData BJJBelt where
  parseQueryParam :: Text -> Either Text BJJBelt
  parseQueryParam = maybe (Left "Invalid belt") Right . parseBelt . T.unpack

instance FromHttpApiData ProfileType where
  parseQueryParam :: Text -> Either Text ProfileType
  parseQueryParam = maybe (Left "Invalid profile type") Right . parseProfileType . T.unpack
    where
      parseProfileType s
        | s == "Practitioner" = Just Practitioner
        | s == "Organization" = Just Organization
        | otherwise = Nothing

instance FromHttpApiData SortOrder where
  parseQueryParam :: Text -> Either Text SortOrder
  parseQueryParam t =
    case T.toLower t of
      "asc" -> Right Asc
      "desc" -> Right Desc
      _ -> Left "Invalid sort order. Use 'asc' or 'desc'"

instance FromHttpApiData ProfilesOrderBy where
  parseQueryParam :: Text -> Either Text ProfilesOrderBy
  parseQueryParam t =
    case T.toLower t of
      "name" -> Right ProfilesOrderByName
      "id" -> Right ProfilesOrderById
      "description" -> Right ProfilesOrderByDescription
      "type" -> Right ProfilesOrderByType
      _ -> Left "Invalid order by. Use 'name', 'id', 'description', or 'type'"

instance FromHttpApiData PromotionsOrderBy where
  parseQueryParam :: Text -> Either Text PromotionsOrderBy
  parseQueryParam t =
    case T.toLower t of
      "id" -> Right PromotionsOrderById
      "belt" -> Right PromotionsOrderByBelt
      "achieved_by" -> Right PromotionsOrderByAchievedBy
      "awarded_by" -> Right PromotionsOrderByAwardedBy
      "date" -> Right PromotionsOrderByDate
      _ -> Left "Invalid order by. Use 'id', 'belt', 'achieved_by', 'awarded_by', or 'date'"

instance FromHttpApiData RanksOrderBy where
  parseQueryParam :: Text -> Either Text RanksOrderBy
  parseQueryParam t =
    case T.toLower t of
      "id" -> Right RanksOrderById
      "belt" -> Right RanksOrderByBelt
      "achieved_by" -> Right RanksOrderByAchievedBy
      "awarded_by" -> Right RanksOrderByAwardedBy
      "date" -> Right RanksOrderByDate
      _ -> Left "Invalid order by. Use 'id', 'belt', 'achieved_by', 'awarded_by', or 'date'"

-- Auth code moved to WebAPI.Auth

------------------------------------------------------------------------------------------------

--  Health API

------------------------------------------------------------------------------------------------

type ServiceProbeAPI = WebAPI.ServiceProbe.ServiceProbe Text Text

proxyServiceProbe :: Proxy ServiceProbeAPI
proxyServiceProbe = Proxy

serviceProbeServer :: ServerT ServiceProbeAPI QueryAppMonad
serviceProbeServer = alwaysHealthy "query-api" :<|> alwaysReady "query-api"

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

-- Health handlers moved to WebAPI.Health

handleGetPractitionerProfile :: ProfileRefAC -> Bool -> QueryAppMonad PractitionerProfileInformation
handleGetPractitionerProfile profileRefAC liveProjection =
  if liveProjection
    then L.getPractitionerProfile profileRefAC
    else P.getPractitionerProfile profileRefAC

handleGetOrganizationProfile :: ProfileRefAC -> Bool -> QueryAppMonad OrganizationProfileInformation
handleGetOrganizationProfile profileRefAC liveProjection =
  if liveProjection
    then L.getOrganizationProfile profileRefAC
    else P.getOrganizationProfile profileRefAC

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
  let limitOffset = case (limit, offset) of
        (Just l, Just o) -> Just (l, o)
        (Just l, Nothing) -> Just (l, 0)
        (Nothing, Just o) -> Just (100, o)
        (Nothing, Nothing) -> Nothing
  let filter =
        Just $
          C.ProfileFilter
            { C.profileFilterId = if Prelude.null profileRefs then Nothing else Just profileRefs,
              C.profileFilterType = profileType,
              C.profileFilterName = Nothing,
              C.profileFilterDescription = Nothing
            }
  let order = case (orderBy, sortOrder) of
        (Just ob, Just o) -> Just (ob, o)
        (Just ob, Nothing) -> Just (ob, Asc)
        _ -> Nothing
  if liveProjection
    then L.getProfiles limitOffset filter order
    else P.getProfiles limitOffset filter order

profilesServer :: ServerT Profiles QueryAppMonad
profilesServer = handleGetPractitionerProfile :<|> handleGetOrganizationProfile :<|> handleGetProfiles

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
      :> QueryParams "profile" ProfileRefAC
      :> QueryParams "belt" BJJBelt
      :> QueryParams "achieved_by" ProfileRefAC
      :> QueryParams "awarded_by" ProfileRefAC
      :> QueryParam' '[Optional] "order_by" PromotionsOrderBy
      :> QueryParam' '[Optional] "sort_order" SortOrder
      :> QueryFlag "liveprojection"
      :> Get '[JSON] [Promotion]
  )

handleGetPromotions ::
  Maybe Int ->
  Maybe Int ->
  [ProfileRefAC] ->
  [BJJBelt] ->
  [ProfileRefAC] ->
  [ProfileRefAC] ->
  Maybe PromotionsOrderBy ->
  Maybe SortOrder ->
  Bool ->
  QueryAppMonad [Promotion]
handleGetPromotions limit offset profileRefs beltRefs achievedByRefs awardedByRefs orderBy sortOrder liveProjection = do
  let limitOffset = case (limit, offset) of
        (Just l, Just o) -> Just (l, o)
        (Just l, Nothing) -> Just (l, 0)
        (Nothing, Just o) -> Just (100, o)
        (Nothing, Nothing) -> Nothing
  let filter =
        Just $
          C.PromotionFilter
            { C.promotionFilterId = if Prelude.null profileRefs then Nothing else Just profileRefs,
              C.promotionFilterBelt = if Prelude.null beltRefs then Nothing else Just beltRefs,
              C.promotionFilterAchievedByProfileId = if Prelude.null achievedByRefs then Nothing else Just achievedByRefs,
              C.promotionFilterAwardedByProfileId = if Prelude.null awardedByRefs then Nothing else Just awardedByRefs,
              C.promotionFilterAchievementDateInterval = (Nothing, Nothing)
            }
  let order = case (orderBy, sortOrder) of
        (Just ob, Just o) -> Just (ob, o)
        (Just ob, Nothing) -> Just (ob, Asc)
        _ -> Nothing
  if liveProjection
    then L.getPromotions limitOffset filter order
    else P.getPromotions limitOffset filter order

promotionsServer :: ServerT Promotions QueryAppMonad
promotionsServer = handleGetPromotions

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
      :> QueryParams "profile" ProfileRefAC
      :> QueryParams "belt" BJJBelt
      :> QueryParams "achieved_by" ProfileRefAC
      :> QueryParams "awarded_by" ProfileRefAC
      :> QueryParam' '[Optional] "from" GYTime
      :> QueryParam' '[Optional] "to" GYTime
      :> QueryParam' '[Optional] "order_by" RanksOrderBy
      :> QueryParam' '[Optional] "order" SortOrder
      :> QueryFlag "liveprojection"
      :> Get '[JSON] [Rank]
  )
    :<|>
    -- Get belts count endpoint
    ( Summary "Get Belts Count"
        :> Description "Get Belts Count"
        :> "belts"
        :> "count"
        :> QueryParam' '[Optional] "limit" Int
        :> QueryParam' '[Optional] "offset" Int
        :> QueryParams "profile" ProfileRefAC
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

handleGetBelts :: Maybe Int -> Maybe Int -> [ProfileRefAC] -> [BJJBelt] -> [ProfileRefAC] -> [ProfileRefAC] -> Maybe GYTime -> Maybe GYTime -> Maybe RanksOrderBy -> Maybe SortOrder -> Bool -> QueryAppMonad [Rank]
handleGetBelts limit offset profiles belt achieved_by awarded_by from to maybeOrderBy maybeOrder live = do
  let limitOffset = case (limit, offset) of
        (Just l, Just o) -> Just (l, o)
        (Just l, Nothing) -> Just (l, 0)
        (Nothing, Just o) -> Just (100, o)
        (Nothing, Nothing) -> Nothing
  let filter =
        Just $
          C.RankFilter
            { C.rankFilterId = if Prelude.null profiles then Nothing else Just profiles,
              C.rankFilterBelt = if Prelude.null belt then Nothing else Just belt,
              C.rankFilterAchievedByProfileId = if Prelude.null achieved_by then Nothing else Just achieved_by,
              C.rankFilterAwardedByProfileId = if Prelude.null awarded_by then Nothing else Just awarded_by,
              C.rankFilterAchievementDateInterval = (from, to)
            }
  let order = case (maybeOrderBy, maybeOrder) of
        (Just ob, Just o) -> Just (ob, o)
        (Just ob, Nothing) -> Just (ob, Asc)
        _ -> Nothing
  if live
    then L.getRanks limitOffset filter order
    else P.getRanks limitOffset filter order

handleGetBeltsCount :: Maybe Int -> Maybe Int -> [ProfileRefAC] -> [BJJBelt] -> [ProfileRefAC] -> [ProfileRefAC] -> Maybe GYTime -> Maybe GYTime -> Bool -> QueryAppMonad Int
handleGetBeltsCount limit offset profiles belt achieved_by awarded_by from to live = do
  let filter =
        Just $
          C.RankFilter
            { C.rankFilterId = if Prelude.null profiles then Nothing else Just profiles,
              C.rankFilterBelt = if Prelude.null belt then Nothing else Just belt,
              C.rankFilterAchievedByProfileId = if Prelude.null achieved_by then Nothing else Just achieved_by,
              C.rankFilterAwardedByProfileId = if Prelude.null awarded_by then Nothing else Just awarded_by,
              C.rankFilterAchievementDateInterval = (from, to)
            }
  if live
    then L.getRanksCount filter
    else P.getRanksCount filter

handleGetBeltsFrequency :: Bool -> QueryAppMonad [(BJJBelt, Int)]
handleGetBeltsFrequency live =
  if live then L.getBeltTotals else P.getBeltTotals

beltsServer :: ServerT Belts QueryAppMonad
beltsServer = handleGetBelts :<|> handleGetBeltsCount :<|> handleGetBeltsFrequency

------------------------------------------------------------------------------------------------

--  Core Function API

------------------------------------------------------------------------------------------------

-- | Combined API
type CoreFunctionAPI =
  Profiles
    :<|> Promotions
    :<|> Belts

proxyCoreFunctionAPI :: Proxy CoreFunctionAPI
proxyCoreFunctionAPI = Proxy

coreFunctionServer :: ServerT CoreFunctionAPI QueryAppMonad
coreFunctionServer = profilesServer :<|> promotionsServer :<|> beltsServer

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
    & info . Data.Swagger.version .~ "1.0"
    & info . Data.Swagger.description ?~ "This is the Query API for the Decentralized Belt System - handles data queries for profiles, promotions, and belts"
    & info
      . license
      ?~ "GPL-3.0 license"
    & host .~ Nothing

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
