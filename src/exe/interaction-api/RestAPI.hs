{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RestAPI where

import Constants (appVersion)
import Control.Lens hiding (Context)
import Data.Swagger
import Data.Text hiding (length)
import Deriving.Aeson
import GeniusYield.Imports
import GeniusYield.Types hiding (description, title)
import InteractionAppMonad
import Network.Wai
import Network.Wai.Middleware.Servant.Options (provideOptions)
import Servant
import Servant.Multipart (Mem, MultipartForm)
import Servant.Swagger
import Servant.Swagger.UI
import ServiceHandlers
import ServiceRequests
import TxBuilding.Interactions
import WebAPI.Auth
import WebAPI.CORS
import WebAPI.ServiceProbe (alwaysHealthy)
import WebAPI.ServiceProbe qualified
import WebAPI.Utils (addSharedSwaggerDescriptions)

-- | Orphan 'FromHttpApiData' instance for 'GYTxId' to support URL capture.
instance FromHttpApiData GYTxId where
  parseUrlPiece t = case txIdFromHex (unpack t) of
    Just txId -> Right txId
    Nothing -> Left ("Invalid transaction ID: " <> t)

-- | Orphan 'ToParamSchema' instance for 'GYTxId' for Swagger generation.
instance ToParamSchema GYTxId where
  toParamSchema _ =
    mempty
      & type_ ?~ SwaggerString
      & Data.Swagger.format ?~ "hex"

-- | Orphan HasSwagger instance for MultipartForm.
-- Delegates to ReqBody for schema generation, then overrides consumes to multipart/form-data.
instance (HasSwagger api, ToSchema a) => HasSwagger (MultipartForm Mem a :> api) where
  toSwagger _ =
    toSwagger (Proxy :: Proxy (ReqBody '[JSON] a :> api))
      & allOperations . consumes ?~ MimeList ["multipart/form-data"]

-- Auth code moved to WebAPI.Auth

------------------------------------------------------------------------------------------------

--  Transaction Status Response

------------------------------------------------------------------------------------------------

-- | Response type for the tx-status polling endpoint.
data TxStatusResponse = TxStatusResponse
  { tsrTxId :: Text,
    tsrConfirmed :: Bool
  }
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "tsr", CamelToSnake]] TxStatusResponse

instance ToSchema TxStatusResponse where
  declareNamedSchema = genericDeclareNamedSchema $ fromAesonOptions $ aesonOptions @'[FieldLabelModifier '[StripPrefix "tsr", CamelToSnake]]

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
    :<|>
    -- Poll tx status endpoint
    ( Summary "Poll Transaction Status"
        :> Description "Polls the confirmation status of a submitted transaction. Returns whether the transaction has been confirmed on-chain."
        :> "tx-status"
        :> Capture "txId" GYTxId
        :> Get '[JSON] TxStatusResponse
    )

-- Health handlers moved to WebAPI.Health

handleBuildTx :: Interaction -> InteractionAppMonad String
handleBuildTx = buildInteractionApp

handleSubmitTx :: AddWitAndSubmitParams -> InteractionAppMonad GYTxId
handleSubmitTx AddWitAndSubmitParams {..} = do
  let txBody = getTxBody awasTxUnsigned
  let signedTx = makeSignedTransaction awasTxWit txBody
  submitTxApp signedTx

handleTxStatus :: GYTxId -> InteractionAppMonad TxStatusResponse
handleTxStatus txId = do
  confirmed <- pollTxConfirmation txId
  return
    TxStatusResponse
      { tsrTxId = pack (show txId),
        tsrConfirmed = confirmed
      }

transactionsServer :: ServerT Transactions InteractionAppMonad
transactionsServer = handleBuildTx :<|> handleSubmitTx :<|> handleTxStatus

------------------------------------------------------------------------------------------------

--  Service API (dedicated endpoints per action)

------------------------------------------------------------------------------------------------

type ServiceAPI =
  -- Smart endpoints (IPFS upload + resolution)
  ( Summary "Create Profile"
      :> Description "Creates a profile with IPFS image upload. Auto-selects InitProfile or CreateProfileWithRank based on belt parameter. Multipart form: 'image' (file) + 'data' (JSON)."
      :> "create-profile"
      :> MultipartForm Mem (WithImage CreateProfileRequest)
      :> Post '[JSON] String
  )
    :<|> ( Summary "New Membership"
             :> Description "Smart membership: auto-detects whether to create a new membership history or add an interval to an existing one."
             :> "new-membership"
             :> ReqBody '[JSON] NewMembershipRequest
             :> Post '[JSON] String
         )
    :<|> ( Summary "Update Profile"
             :> Description "Updates profile metadata (description, image). Name is immutable. Multipart form: 'image' (file) + 'data' (JSON with optional description)."
             :> "update-profile"
             :> MultipartForm Mem (WithImage UpdateProfileRequest)
             :> Post '[JSON] String
         )
    :<|> ( Summary "Award Achievement"
             :> Description "Awards achievement with IPFS image upload. Multipart form: 'image' (file) + 'data' (JSON)."
             :> "award-achievement"
             :> MultipartForm Mem (WithImage AwardAchievementRequest)
             :> Post '[JSON] String
         )
    -- Pass-through endpoints
    :<|> ( Summary "Promote Profile"
             :> Description "Promotes a profile to a new belt rank."
             :> "promote-profile"
             :> ReqBody '[JSON] PromoteProfileRequest
             :> Post '[JSON] String
         )
    :<|> ( Summary "Accept Promotion"
             :> Description "Accepts a pending promotion."
             :> "accept-promotion"
             :> ReqBody '[JSON] AcceptPromotionRequest
             :> Post '[JSON] String
         )
    :<|> ( Summary "Accept Membership"
             :> Description "Accepts a pending membership interval."
             :> "accept-membership"
             :> ReqBody '[JSON] AcceptMembershipRequest
             :> Post '[JSON] String
         )
    :<|> ( Summary "Update End Date"
             :> Description "Updates the end date of a membership interval."
             :> "update-end-date"
             :> ReqBody '[JSON] UpdateEndDateRequest
             :> Post '[JSON] String
         )
    :<|> ( Summary "Accept Achievement"
             :> Description "Accepts a pending achievement."
             :> "accept-achievement"
             :> ReqBody '[JSON] AcceptAchievementRequest
             :> Post '[JSON] String
         )

serviceServer :: ServerT ServiceAPI InteractionAppMonad
serviceServer =
  handleCreateProfile
    :<|> handleNewMembership
    :<|> handleUpdateProfile
    :<|> handleAwardAchievement
    :<|> handlePromoteProfile
    :<|> handleAcceptPromotion
    :<|> handleAcceptMembership
    :<|> handleUpdateEndDate
    :<|> handleAcceptAchievement

------------------------------------------------------------------------------------------------

--  Probe API

------------------------------------------------------------------------------------------------

type ProbeAPI = WebAPI.ServiceProbe.ServiceProbe Text Text

interactionProbeServer :: ServerT ProbeAPI InteractionAppMonad
interactionProbeServer = alwaysHealthy (pack appVersion) "interaction-api" :<|> checkDeployedScriptsAreReady

------------------------------------------------------------------------------------------------

--  Swagger API

------------------------------------------------------------------------------------------------

-- | Public API surface used for Swagger generation (excludes auth wrappers).
-- The HasSwagger instance for MultipartForm comes from servant-multipart-api.
type PublicAPI = ProbeAPI :<|> Transactions :<|> ServiceAPI

proxyPublicAPI :: Proxy PublicAPI
proxyPublicAPI = Proxy

apiSwagger :: Swagger
apiSwagger =
  toSwagger proxyPublicAPI
    & info . title .~ "Decentralized Belt System Interaction API"
    & info . Data.Swagger.version .~ "2.1.0"
    & info . Data.Swagger.description ?~ "This is the Interaction API for the Decentralized Belt System — handles transaction building, submission, and dedicated service endpoints with IPFS image upload."
    & info . license ?~ "GPL-3.0 license"
    & host .~ Nothing
    -- Response descriptions
    & allOperations . responses . at 200 . mapped . _Inline . description .~ ("Hex-encoded unsigned transaction CBOR. Sign with your wallet and submit via /submit-tx." :: Text)
    & allOperations . responses . at 400 ?~ Inline (mempty {_responseDescription = "Validation error (invalid dates, wrong profile type, conflicting state, etc.)"})
    & allOperations . responses . at 404 ?~ Inline (mempty {_responseDescription = "Referenced entity not found (profile, rank, membership history, achievement, or NFT)"})
    & allOperations . responses . at 502 ?~ Inline (mempty {_responseDescription = "IPFS service unavailable (applies to image upload endpoints)"})
    & allOperations . responses . at 503 ?~ Inline (mempty {_responseDescription = "Protocol is paused or deployed validator scripts are not yet available"})
    -- Shared type descriptions
    & addSharedSwaggerDescriptions
    -- Interaction-API-specific type descriptions
    & definitions . at "UserAddresses" . mapped . description ?~ "Wallet addresses used as inputs for transaction building. Provide all UTxO addresses from the connected wallet."

swaggerServer :: ServerT (SwaggerSchemaUI "swagger-ui" "swagger-api.json") InteractionAppMonad
swaggerServer = swaggerSchemaUIServerT apiSwagger

------------------------------------------------------------------------------------------------

--  Combined API

------------------------------------------------------------------------------------------------

-- | Adding Basic Auth to the Rest API
type PrivateTransactions =
  BasicAuth "user-realm" AuthUser :> Transactions

proxyPrivateTransactions :: Proxy PrivateTransactions
proxyPrivateTransactions = Proxy

privateTransactionsServer :: ServerT PrivateTransactions InteractionAppMonad
privateTransactionsServer = const transactionsServer

-- | Adding Basic Auth to the Service API
type PrivateServiceAPI =
  BasicAuth "user-realm" AuthUser :> ServiceAPI

privateServiceServer :: ServerT PrivateServiceAPI InteractionAppMonad
privateServiceServer = const serviceServer

-- | Adding Swagger UI on top of Private Rest API
type FullAPI =
  SwaggerSchemaUI "swagger-ui" "swagger-api.json"
    :<|> ProbeAPI
    :<|> PrivateTransactions
    :<|> PrivateServiceAPI

proxyFullAPI :: Proxy FullAPI
proxyFullAPI = Proxy

fullServer :: ServerT FullAPI InteractionAppMonad
fullServer =
  swaggerServer
    :<|> interactionProbeServer
    :<|> privateTransactionsServer
    :<|> privateServiceServer

------------------------------------------------------------------------------------------------

-- | Servant Application

------------------------------------------------------------------------------------------------

mkBJJApp :: InteractionAppContext -> Application
mkBJJApp ctx =
  WebAPI.CORS.setupCors $
    provideOptions proxyPublicAPI $
      serveWithContext proxyFullAPI basicCtx hoistedServer
  where
    basicCtx = basicAuthServerContext (authContext ctx)
    hoistedServer = hoistServerWithContext proxyFullAPI proxyBasicAuthContext (runInteractionAppMonad ctx) fullServer
