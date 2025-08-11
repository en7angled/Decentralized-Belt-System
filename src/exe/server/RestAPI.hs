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
import qualified Data.Text.Encoding
import DomainTypes.Profile.Types
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

instance FromHttpApiData BJJBelt where
  parseQueryParam :: Text -> Either Text BJJBelt
  parseQueryParam = maybe (Left "Invalid belt") Right . parseBelt . Data.Text.unpack

instance FromHttpApiData ProfileType where
  parseQueryParam :: Text -> Either Text ProfileType
  parseQueryParam = maybe (Left "Invalid profile type") Right . parseProfileType . Data.Text.unpack
    where
      parseProfileType s
        | s == "Practitioner" = Just Practitioner
        | s == "Organization" = Just Organization
        | otherwise = Nothing

------------------------------------------------------------------------------------------------

--  Transactions API

------------------------------------------------------------------------------------------------

type Transactions =
  -- Build tx endpoint
  ( Summary "Build Transaction"
      :> Description "Builds Transaction for Interaction and returns it as a hex encoded CBOR"
      :> "build-tx"
      :> ReqBody '[JSON] Interaction
      :> Post '[JSON] String
  )
    :<|>
    -- Submit tx endpoint
    ( Summary "Submits Signed Transaction"
        :> Description "Submits Signed Transaction and returns the transaction id"
        :> "submit-tx"
        :> ReqBody '[JSON] AddWitAndSubmitParams
        :> Post '[JSON] GYTxId
    )

handleBuildTx :: Interaction -> AppMonad String
handleBuildTx = interactionToHexEncodedCBOR

handleSubmitTx :: AddWitAndSubmitParams -> AppMonad GYTxId
handleSubmitTx AddWitAndSubmitParams {..} = do
  let txBody = getTxBody awasTxUnsigned
  let signedTx = makeSignedTransaction awasTxWit txBody
  submitTx signedTx

transactionsServer :: ServerT Transactions AppMonad
transactionsServer = handleBuildTx :<|> handleSubmitTx

------------------------------------------------------------------------------------------------

--  Profiles API

------------------------------------------------------------------------------------------------

type Profiles =
  -- Get practitioner profile endpoint
  ( Summary "Get Practitioner Profile Information"
      :> Description "Get Practitioner Profile Information"
      :> "practitioner"
      :> Capture "profile-id" ProfileRefAC
      :> Get '[JSON] PractitionerProfileInformation
  )
    :<|>
    -- Get organization profile endpoint
    ( Summary "Get Organization Profile Information"
        :> Description "Get Organization Profile Information"
        :> "organization"
        :> Capture "organization-id" ProfileRefAC
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
        :> QueryParam "profile-type" ProfileType
        :> QueryParam' '[Optional] "name" Text
        :> QueryParam' '[Optional] "description" Text
        :> Get '[JSON] [ProfileSummary]
    )
    :<|>
    -- Get profiles count endpoint
    ( Summary "Get Profiles Count"
        :> Description "Get count of profiles by type"
        :> "profiles"
        :> "count"
        :> QueryParam "profile-type" ProfileType
        :> Get '[JSON] Int
    )

handleGetPractitionerProfile :: ProfileRefAC -> AppMonad PractitionerProfileInformation
handleGetPractitionerProfile = getPractitionerProfile

handleGetOrganizationProfile :: ProfileRefAC -> AppMonad OrganizationProfileInformation
handleGetOrganizationProfile = getOrganizationProfile

handleGetProfiles :: Maybe Int -> Maybe Int -> [ProfileRefAC] -> Maybe ProfileType -> Maybe Text -> Maybe Text -> AppMonad [ProfileSummary]
handleGetProfiles (Just limit) (Just offset) profiles maybeProfileType name description =
  getProfiles
    (Just (limit, offset))
    ( Just
        ( ProfileFilter
            { profileFilterId = if Prelude.null profiles then Nothing else Just profiles,
              profileFilterType = maybeProfileType,
              profileFilterName = name,
              profileFilterDescription = description
            }
        )
    )
handleGetProfiles _ _ _ _ _ _ = getProfiles Nothing Nothing

handleGetProfilesCount :: Maybe ProfileType -> AppMonad Int
handleGetProfilesCount maybeProfileType = do
  getProfilesCount maybeProfileType

profilesServer :: ServerT Profiles AppMonad
profilesServer = handleGetPractitionerProfile :<|> handleGetOrganizationProfile :<|> handleGetProfiles :<|> handleGetProfilesCount

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
      :> QueryParam' '[Optional] "from" GYTime
      :> QueryParam' '[Optional] "to" GYTime
      :> Get '[JSON] [PromotionInformation]
  )
    :<|>
    -- Get pending promotions count endpoint
    ( Summary "Get Pending Promotions Count"
        :> Description "Get Pending Promotions Count"
        :> "promotions"
        :> "count"
        :> QueryParam' '[Optional] "limit" Int
        :> QueryParam' '[Optional] "offset" Int
        :> QueryParams "profile" ProfileRefAC
        :> QueryParams "belt" BJJBelt
        :> QueryParams "achieved_by" ProfileRefAC
        :> QueryParams "awarded_by" ProfileRefAC
        :> QueryParam' '[Optional] "from" GYTime
        :> QueryParam' '[Optional] "to" GYTime
        :> Get '[JSON] Int
    )

handleGetPendingPromotions :: Maybe Int -> Maybe Int -> [ProfileRefAC] -> [BJJBelt] -> [ProfileRefAC] -> [ProfileRefAC] -> Maybe GYTime -> Maybe GYTime -> AppMonad [PromotionInformation]
handleGetPendingPromotions (Just limit) (Just offset) profiles belt achieved_by awarded_by from to =
  getPromotions
    (Just (limit, offset))
    ( Just
        ( PromotionFilter
            { promotionFilterId = if Prelude.null profiles then Nothing else Just profiles,
              promotionFilterBelt = if Prelude.null belt then Nothing else Just belt,
              promotionFilterAchievedByProfileId = if Prelude.null achieved_by then Nothing else Just achieved_by,
              promotionFilterAwardedByProfileId = if Prelude.null awarded_by then Nothing else Just awarded_by,
              promotionFilterAchievementDateInterval = (from, to)
            }
        )
    )
handleGetPendingPromotions _ _ _ _ _ _ _ _ = getPromotions Nothing Nothing

handleGetPendingPromotionsCount :: Maybe Int -> Maybe Int -> [ProfileRefAC] -> [BJJBelt] -> [ProfileRefAC] -> [ProfileRefAC] -> Maybe GYTime -> Maybe GYTime -> AppMonad Int
handleGetPendingPromotionsCount limit offset profiles belt achieved_by awarded_by from to =
  Prelude.length <$> handleGetPendingPromotions limit offset profiles belt achieved_by awarded_by from to

promotionsServer :: ServerT Promotions AppMonad
promotionsServer =
  handleGetPendingPromotions
    :<|> handleGetPendingPromotionsCount

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
      :> Get '[JSON] [RankInformation]
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
        :> Get '[JSON] Int
    )
    :<|>
    -- Get belts count endpoint
    ( Summary "Get Belts Frequency"
        :> Description "Get Belts Frequency"
        :> "belts"
        :> "frequency"
        :> Get '[JSON] [(BJJBelt, Int)]
    )

handleGetBelts :: Maybe Int -> Maybe Int -> [ProfileRefAC] -> [BJJBelt] -> [ProfileRefAC] -> [ProfileRefAC] -> Maybe GYTime -> Maybe GYTime -> AppMonad [RankInformation]
handleGetBelts (Just limit) (Just offset) profiles belt achieved_by awarded_by from to =
  getRanks
    (Just (limit, offset))
    ( Just
        ( RankFilter
            { rankFilterId = if Prelude.null profiles then Nothing else Just profiles,
              rankFilterBelt = if Prelude.null belt then Nothing else Just belt,
              rankFilterAchievedByProfileId = if Prelude.null achieved_by then Nothing else Just achieved_by,
              rankFilterAwardedByProfileId = if Prelude.null awarded_by then Nothing else Just awarded_by,
              rankFilterAchievementDateInterval = (from, to)
            }
        )
    )
handleGetBelts _ _ _ _ _ _ _ _ = getRanks Nothing Nothing

handleGetBeltsCount :: Maybe Int -> Maybe Int -> [ProfileRefAC] -> [BJJBelt] -> [ProfileRefAC] -> [ProfileRefAC] -> Maybe GYTime -> Maybe GYTime -> AppMonad Int
handleGetBeltsCount limit offset profiles belt achieved_by awarded_by from to =
  Prelude.length <$> handleGetBelts limit offset profiles belt achieved_by awarded_by from to

handleGetBeltFrequency :: AppMonad [(BJJBelt, Int)]
handleGetBeltFrequency = getBeltTotals

beltsServer :: ServerT Belts AppMonad
beltsServer = handleGetBelts :<|> handleGetBeltsCount :<|> handleGetBeltFrequency

------------------------------------------------------------------------------------------------

-- Authentication Context

------------------------------------------------------------------------------------------------

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

--  Swagger API

------------------------------------------------------------------------------------------------

apiSwagger :: Swagger
apiSwagger =
  toSwagger proxyRestAPI
    & info . title .~ "Decentralized Belt System API"
    & info . version .~ "1.0"
    & info . Data.Swagger.description ?~ "This is an API for the Decentralized Belt System"
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
    :<|> Transactions

proxyRestAPI :: Proxy RestAPI
proxyRestAPI = Proxy

restServer :: ServerT RestAPI AppMonad
restServer = profilesServer :<|> promotionsServer :<|> beltsServer :<|> transactionsServer

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
