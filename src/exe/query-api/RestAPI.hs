{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RestAPI where

import AppMonad
import Control.Lens hiding (Context)
import Control.Monad.Reader (MonadIO (liftIO), ReaderT (runReaderT), asks)
import qualified Data.List
import Data.String (IsString (..))
import Data.Swagger
import Data.Text hiding (length)
import qualified Data.Text as T
import qualified Data.Text.Encoding
import DomainTypes.Core.Types
import DomainTypes.Transfer.Types
import GeniusYield.Imports
import GeniusYield.Types hiding (title)
import qualified Network.HTTP.Types as HttpTypes
import Network.HTTP.Types.Header
import Network.Wai
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Onchain.BJJ (BJJBelt, parseBelt)
import Servant
import Servant.Swagger
import Servant.Swagger.UI
import TxBuilding.Context (runQuery)
import TxBuilding.Interactions
import TxBuilding.Lookups (getOrganizationInformation, getPractiotionerInformation)
import TxBuilding.Transactions
import Types
import qualified Query.Common as C
import qualified Query.Live as L
import qualified Query.Projected as P

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
      "name" -> Right Name
      "created_at" -> Right CreatedAt
      "updated_at" -> Right UpdatedAt
      _ -> Left "Invalid order by. Use 'name', 'created_at', or 'updated_at'"

instance FromHttpApiData PromotionsOrderBy where
  parseQueryParam :: Text -> Either Text PromotionsOrderBy
  parseQueryParam t =
    case T.toLower t of
      "created_at" -> Right CreatedAt
      "updated_at" -> Right UpdatedAt
      _ -> Left "Invalid order by. Use 'created_at' or 'updated_at'"

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


newtype AuthUser = AuthUser
  { user :: Text
  }
  deriving (Eq, Show)

proxyBasicAuthContext :: Proxy '[BasicAuthCheck AuthUser]
proxyBasicAuthContext = Proxy

-- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
authCheck :: AuthContext -> BasicAuthCheck AuthUser
authCheck AuthContext {authUser, authPassword} =
  let check (BasicAuthData username password) =
        if Data.Text.Encoding.decodeUtf8 username == authUser && Data.Text.Encoding.decodeUtf8 password == authPassword
          then return (Authorized (AuthUser authUser))
          else return Unauthorized
   in BasicAuthCheck check

basicAuthServerContext :: AuthContext -> Context (BasicAuthCheck AuthUser ': '[])
basicAuthServerContext authContext = authCheck authContext :. EmptyContext

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

handleGetPractitionerProfile :: ProfileRefAC -> Bool -> AppMonad PractitionerProfileInformation
handleGetPractitionerProfile profileRefAC liveProjection = do
  if liveProjection
    then L.getPractitionerProfile profileRefAC
    else P.getPractitionerProfile profileRefAC

handleGetOrganizationProfile :: ProfileRefAC -> Bool -> AppMonad OrganizationProfileInformation
handleGetOrganizationProfile profileRefAC liveProjection = do
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
  AppMonad [Profile]
handleGetProfiles limit offset profileRefs profileType orderBy sortOrder liveProjection = do
  let limitOffset = case (limit, offset) of
        (Just l, Just o) -> Just (l, o)
        (Just l, Nothing) -> Just (l, 0)
        (Nothing, Just o) -> Just (100, o)
        (Nothing, Nothing) -> Nothing
  let filter = Just $ C.ProfileFilter
        { C.profileFilterId = if Prelude.null profileRefs then Nothing else Just profileRefs
        , C.profileFilterType = profileType
        , C.profileFilterName = Nothing
        , C.profileFilterDescription = Nothing
        }
  let order = case (orderBy, sortOrder) of
        (Just ob, Just o) -> Just (ob, o)
        (Just ob, Nothing) -> Just (ob, Asc)
        _ -> Nothing
  if liveProjection
    then L.getProfiles limitOffset filter order
    else P.getProfiles limitOffset filter order

profilesServer :: ServerT Profiles AppMonad
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
  AppMonad [Promotion]
handleGetPromotions limit offset profileRefs beltRefs achievedByRefs awardedByRefs orderBy sortOrder liveProjection = do
  let limitOffset = case (limit, offset) of
        (Just l, Just o) -> Just (l, o)
        (Just l, Nothing) -> Just (l, 0)
        (Nothing, Just o) -> Just (100, o)
        (Nothing, Nothing) -> Nothing
  let filter = Just $ C.PromotionFilter
        { C.promotionFilterId = if Prelude.null profileRefs then Nothing else Just profileRefs
        , C.promotionFilterBelt = if Prelude.null beltRefs then Nothing else Just beltRefs
        , C.promotionFilterAchievedByProfileId = if Prelude.null achievedByRefs then Nothing else Just achievedByRefs
        , C.promotionFilterAwardedByProfileId = if Prelude.null awardedByRefs then Nothing else Just awardedByRefs
        , C.promotionFilterAchievementDateInterval = (Nothing, Nothing)
        }
  let order = case (orderBy, sortOrder) of
        (Just ob, Just o) -> Just (ob, o)
        (Just ob, Nothing) -> Just (ob, Asc)
        _ -> Nothing
  if liveProjection
    then L.getPromotions limitOffset filter order
    else P.getPromotions limitOffset filter order

promotionsServer :: ServerT Promotions AppMonad
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

handleGetBelts :: Maybe Int -> Maybe Int -> [ProfileRefAC] -> [BJJBelt] -> [ProfileRefAC] -> [ProfileRefAC] -> Maybe GYTime -> Maybe GYTime -> Maybe RanksOrderBy -> Maybe SortOrder -> Bool -> AppMonad [Rank]
handleGetBelts limit offset profiles belt achieved_by awarded_by from to maybeOrderBy maybeOrder live = do
  let limitOffset = case (limit, offset) of
        (Just l, Just o) -> Just (l, o)
        (Just l, Nothing) -> Just (l, 0)
        (Nothing, Just o) -> Just (100, o)
        (Nothing, Nothing) -> Nothing
  let filter = Just $ C.RankFilter
        { C.rankFilterId = if Prelude.null profiles then Nothing else Just profiles
        , C.rankFilterBelt = if Prelude.null belt then Nothing else Just belt
        , C.rankFilterAchievedByProfileId = if Prelude.null achieved_by then Nothing else Just achieved_by
        , C.rankFilterAwardedByProfileId = if Prelude.null awarded_by then Nothing else Just awarded_by
        , C.rankFilterAchievementDateInterval = (from, to)
        }
  let order = case (maybeOrderBy, maybeOrder) of
        (Just ob, Just o) -> Just (ob, o)
        (Just ob, Nothing) -> Just (ob, Asc)
        _ -> Nothing
  if live
    then L.getRanks limitOffset filter order
    else P.getRanks limitOffset filter order

handleGetBeltsCount :: Maybe Int -> Maybe Int -> [ProfileRefAC] -> [BJJBelt] -> [ProfileRefAC] -> [ProfileRefAC] -> Maybe GYTime -> Maybe GYTime -> Bool -> AppMonad Int
handleGetBeltsCount limit offset profiles belt achieved_by awarded_by from to live = do
  let filter = Just $ C.RankFilter
        { C.rankFilterId = if Prelude.null profiles then Nothing else Just profiles
        , C.rankFilterBelt = if Prelude.null belt then Nothing else Just belt
        , C.rankFilterAchievedByProfileId = if Prelude.null achieved_by then Nothing else Just achieved_by
        , C.rankFilterAwardedByProfileId = if Prelude.null awarded_by then Nothing else Just awarded_by
        , C.rankFilterAchievementDateInterval = (from, to)
        }
  if live
    then L.getRanksCount filter
    else P.getRanksCount filter

handleGetBeltsFrequency :: Bool -> AppMonad [(BJJBelt, Int)]
handleGetBeltsFrequency live =
  (if live then L.getBeltTotals else P.getBeltTotals)

beltsServer :: ServerT Belts AppMonad
beltsServer = handleGetBelts :<|> handleGetBeltsCount :<|> handleGetBeltsFrequency

------------------------------------------------------------------------------------------------

--  Swagger API

------------------------------------------------------------------------------------------------

apiSwagger :: Swagger
apiSwagger =
  toSwagger proxyRestAPI
    & info . title .~ "Decentralized Belt System Query API"
    & info . version .~ "1.0"
    & info . Data.Swagger.description ?~ "This is the Query API for the Decentralized Belt System - handles data queries for profiles, promotions, and belts"
    & info
      . license
      ?~ "GPL-3.0 license"
    & host .~ Nothing

swaggerServer = swaggerSchemaUIServerT apiSwagger

------------------------------------------------------------------------------------------------

--  Combined API

------------------------------------------------------------------------------------------------

-- | Combined API
type RestAPI =
  Profiles
    :<|> Promotions
    :<|> Belts

proxyRestAPI :: Proxy RestAPI
proxyRestAPI = Proxy

restServer :: ServerT RestAPI AppMonad
restServer = profilesServer :<|> promotionsServer :<|> beltsServer

-- | Adding Basic Auth to the Rest API
type PrivateRestAPI =
  BasicAuth "user-realm" AuthUser :> RestAPI

proxyPrivateRestAPI :: Proxy PrivateRestAPI
proxyPrivateRestAPI = Proxy

privateRestServer :: ServerT PrivateRestAPI AppMonad
privateRestServer = const restServer

-- | Adding Swagger UI on top of Private Rest API
type FullAPI =
  SwaggerSchemaUI "swagger-ui" "swagger-api.json"
    :<|> PrivateRestAPI

proxyFullAPI :: Proxy FullAPI
proxyFullAPI = Proxy

fullServer :: ServerT FullAPI AppMonad
fullServer = swaggerServer :<|> privateRestServer

------------------------------------------------------------------------------------------------

-- | Servant Application

------------------------------------------------------------------------------------------------

mkBJJApp :: AppContext -> Application
mkBJJApp ctx =
  cors
    ( \req ->
        let originHeader = Data.List.lookup hOrigin (requestHeaders req)
         in case originHeader of
              Just o ->
                Just
                  simpleCorsResourcePolicy
                    { corsOrigins = Just ([o], True), -- Reflect request's Origin dynamically
                      corsMethods = ["GET", "POST", "PUT", "OPTIONS", "DELETE"],
                      corsRequestHeaders = simpleHeaders <> [HttpTypes.hAuthorization],
                      corsExposedHeaders = Just $ simpleHeaders <> [HttpTypes.hAuthorization],
                      corsVaryOrigin = True,
                      corsRequireOrigin = False,
                      corsIgnoreFailures = False,
                      corsMaxAge = Just 600
                    }
              Nothing -> Nothing -- If no origin set skips cors headers
    )
    $ provideOptions proxyRestAPI
    $ serveWithContext proxyFullAPI basicCtx hoistedServer
  where
    basicCtx = basicAuthServerContext (authContext ctx)
    hoistedServer = hoistServerWithContext proxyFullAPI proxyBasicAuthContext (runAppMonad ctx) fullServer
