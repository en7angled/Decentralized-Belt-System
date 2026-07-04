{-# LANGUAGE OverloadedStrings #-}

-- | CORS middleware driven by an explicit origin allowlist (fail-closed).
-- Only origins present in @CORS_ALLOWED_ORIGINS@ receive CORS headers; every
-- other origin gets none, so the browser blocks the cross-origin response.
module WebAPI.CORS where

import Data.ByteString (ByteString)
import qualified Data.List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types as HttpTypes
import Network.HTTP.Types.Header (hOrigin)
import Network.Wai
import Network.Wai.Middleware.Cors
import System.Environment (lookupEnv)

-- | Resolved CORS configuration. 'allowCredentials' is currently always 'False'
-- (frontend is same-origin / proxied / token-based).
data CorsConfig = CorsConfig
  { allowedOrigins :: [ByteString],
    allowCredentials :: Bool
  }
  deriving (Eq, Show)

-- | Parse a comma-separated origin allowlist: trims whitespace, drops empties.
parseCorsOrigins :: String -> [ByteString]
parseCorsOrigins raw =
  [ TE.encodeUtf8 t
  | chunk <- T.splitOn "," (T.pack raw),
    let t = T.strip chunk,
    not (T.null t)
  ]

-- | Read @CORS_ALLOWED_ORIGINS@; unset/empty yields an empty allowlist
-- (no cross-origin allowed). Credentials are disabled.
getCorsConfigFromEnv :: IO CorsConfig
getCorsConfigFromEnv = do
  mOrigins <- lookupEnv "CORS_ALLOWED_ORIGINS"
  return
    CorsConfig
      { allowedOrigins = maybe [] parseCorsOrigins mOrigins,
        allowCredentials = False
      }

-- | Policy: reflect the request 'Origin' only if it is in the allowlist,
-- otherwise emit no CORS headers.
corsPolicy :: CorsConfig -> Request -> Maybe CorsResourcePolicy
corsPolicy cfg req =
  case Data.List.lookup hOrigin (requestHeaders req) of
    Just o
      | o `elem` allowedOrigins cfg ->
          Just
            simpleCorsResourcePolicy
              { corsOrigins = Just ([o], allowCredentials cfg),
                corsMethods = ["GET", "POST", "PUT", "OPTIONS", "DELETE"],
                corsRequestHeaders = simpleHeaders <> [HttpTypes.hAuthorization],
                corsExposedHeaders = Just $ simpleHeaders <> [HttpTypes.hAuthorization],
                corsVaryOrigin = True,
                corsRequireOrigin = False,
                corsIgnoreFailures = False,
                corsMaxAge = Just 600
              }
    _ -> Nothing

-- | WAI middleware applying the allowlist CORS policy.
mkCorsMiddleware :: CorsConfig -> Middleware
mkCorsMiddleware cfg = cors (corsPolicy cfg)
