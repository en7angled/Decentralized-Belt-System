-- | CORS middleware that dynamically reflects the request's @Origin@ header.
module WebAPI.CORS where

import qualified Data.List
import qualified Network.HTTP.Types as HttpTypes
import Network.HTTP.Types.Header
import Network.Wai
import Network.Wai.Middleware.Cors

-- | CORS policy that reflects the request's @Origin@ and allows common HTTP methods.
defaultCorsPolicy :: Request -> Maybe CorsResourcePolicy
defaultCorsPolicy req =
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

-- | WAI middleware that applies the dynamic CORS policy to all requests.
setupCors :: Middleware
setupCors = cors defaultCorsPolicy
