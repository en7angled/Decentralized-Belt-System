{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import Data.Time (getCurrentTime)
import DomainTypes.Core.Types
import DomainTypes.Transfer.Types
import GHC.Generics (Generic)
import GeniusYield.Imports
import GeniusYield.Types hiding (title)
import InteractionAppMonad
import qualified Network.HTTP.Types as HttpTypes
import Network.HTTP.Types.Header
import Network.Wai
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Onchain.BJJ (BJJBelt, parseBelt)
import Servant
import Servant.Swagger
import Servant.Swagger.UI
import TxBuilding.Interactions
import TxBuilding.Transactions
import Types
import WebAPI.Auth
import WebAPI.CORS
import WebAPI.ServiceProbe (alwaysHealthy, alwaysReady)
import qualified WebAPI.ServiceProbe

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

-- Auth code moved to WebAPI.Auth

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

-- Health handlers moved to WebAPI.Health

handleBuildTx :: Interaction -> InteractionAppMonad String
handleBuildTx = buildInteractionApp

handleSubmitTx :: AddWitAndSubmitParams -> InteractionAppMonad GYTxId
handleSubmitTx AddWitAndSubmitParams {..} = do
  let txBody = getTxBody awasTxUnsigned
  let signedTx = makeSignedTransaction awasTxWit txBody
  submitTxApp signedTx

transactionsServer :: ServerT Transactions InteractionAppMonad
transactionsServer = handleBuildTx :<|> handleSubmitTx

------------------------------------------------------------------------------------------------

--  Probe API

------------------------------------------------------------------------------------------------

type ProbeAPI = WebAPI.ServiceProbe.ServiceProbe Text Text

type HealthStatus = WebAPI.ServiceProbe.ServiceProbeStatus Text

type ReadyStatus = WebAPI.ServiceProbe.ServiceProbeStatus Text

interactionHealthStatus = "healthy"

interactionReadyStatus = "ready"

interactionProbeServer :: ServerT ProbeAPI InteractionAppMonad
interactionProbeServer = alwaysHealthy "interaction-api" :<|> alwaysReady "interaction-api"

------------------------------------------------------------------------------------------------

--  Swagger API

------------------------------------------------------------------------------------------------

apiSwagger :: Swagger
apiSwagger =
  toSwagger proxyRestAPI
    & info . title .~ "Decentralized Belt System Interaction API"
    & info . Data.Swagger.version .~ "1.0"
    & info . Data.Swagger.description ?~ "This is the Interaction API for the Decentralized Belt System - handles transaction building and submission"
    & info
      . license
      ?~ "GPL-3.0 license"
    & host .~ Nothing

swaggerServer = swaggerSchemaUIServerT apiSwagger

------------------------------------------------------------------------------------------------

--  Combined API

------------------------------------------------------------------------------------------------

-- | Combined API
type RestAPI = ProbeAPI :<|> Transactions

proxyRestAPI :: Proxy RestAPI
proxyRestAPI = Proxy

restServer :: ServerT RestAPI InteractionAppMonad
restServer = interactionProbeServer :<|> transactionsServer

-- | Adding Basic Auth to the Rest API
type PrivateRestAPI =
  BasicAuth "user-realm" AuthUser :> RestAPI

proxyPrivateRestAPI :: Proxy PrivateRestAPI
proxyPrivateRestAPI = Proxy

privateRestServer :: ServerT PrivateRestAPI InteractionAppMonad
privateRestServer = const restServer

-- | Adding Swagger UI on top of Private Rest API
type FullAPI =
  SwaggerSchemaUI "swagger-ui" "swagger-api.json"
    :<|> PrivateRestAPI

proxyFullAPI :: Proxy FullAPI
proxyFullAPI = Proxy

fullServer :: ServerT FullAPI InteractionAppMonad
fullServer = swaggerServer :<|> privateRestServer

------------------------------------------------------------------------------------------------

-- | Servant Application

------------------------------------------------------------------------------------------------

mkBJJApp :: InteractionAppContext -> Application
mkBJJApp ctx =
  WebAPI.CORS.setupCors $
    provideOptions proxyRestAPI $
      serveWithContext proxyFullAPI basicCtx hoistedServer
  where
    basicCtx = basicAuthServerContext (authContext ctx)
    hoistedServer = hoistServerWithContext proxyFullAPI proxyBasicAuthContext (runInteractionAppMonad ctx) fullServer
