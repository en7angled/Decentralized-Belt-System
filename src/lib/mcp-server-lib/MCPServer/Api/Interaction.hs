{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Re-declaration of the single @interaction-api@ endpoint the MCP server
-- calls: @POST \/build-tx@ which takes an 'Interaction' JSON body and returns
-- a hex-encoded unsigned tx CBOR 'String'. Other interaction-api endpoints
-- (submit-tx, create-profile, update-profile, award-achievement) are out of
-- scope per Decision #8 of the parent plan — signing stays in the wallet and
-- the image-bearing multipart endpoints don't fit a chatbot tool surface.
--
-- Declared now in Phase 2 so Phase 3 doesn't churn this module's deps.
module MCPServer.Api.Interaction
  ( InteractionAPI
  , proxyInteractionAPI
  ) where

import Servant
  ( BasicAuth
  , JSON
  , Post
  , Proxy (Proxy)
  , ReqBody
  , (:>)
  )
import TxBuilding.Interactions (Interaction)
import WebAPI.Auth (AuthUser)

type InteractionAPI =
  BasicAuth "user-realm" AuthUser
    :> "build-tx"
    :> ReqBody '[JSON] Interaction
    :> Post '[JSON] String

proxyInteractionAPI :: Proxy InteractionAPI
proxyInteractionAPI = Proxy
