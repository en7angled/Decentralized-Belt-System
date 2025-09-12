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
import TxBuilding.Interactions
import TxBuilding.Transactions
import Types

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
handleBuildTx = buildInteractionApp

handleSubmitTx :: AddWitAndSubmitParams -> AppMonad GYTxId
handleSubmitTx AddWitAndSubmitParams {..} = do
  let txBody = getTxBody awasTxUnsigned
  let signedTx = makeSignedTransaction awasTxWit txBody
  submitTxApp signedTx

transactionsServer :: ServerT Transactions AppMonad
transactionsServer = handleBuildTx :<|> handleSubmitTx

------------------------------------------------------------------------------------------------

--  Swagger API

------------------------------------------------------------------------------------------------

apiSwagger :: Swagger
apiSwagger =
  toSwagger proxyRestAPI
    & info . title .~ "Decentralized Belt System Interaction API"
    & info . version .~ "1.0"
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
type RestAPI = Transactions

proxyRestAPI :: Proxy RestAPI
proxyRestAPI = Proxy

restServer :: ServerT RestAPI AppMonad
restServer = transactionsServer

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
