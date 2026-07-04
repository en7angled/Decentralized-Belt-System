{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Basic authentication middleware for Servant APIs.
-- Credentials come from @BASIC_USER@ and @BASIC_PASS@; the server refuses to
-- start if either is unset (fail-closed). Credential comparison is
-- constant-time and never decodes attacker-controlled bytes.
module WebAPI.Auth where

import Data.ByteArray (constEq)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Servant
import System.Environment (lookupEnv)
import System.Exit (die)

-- | Authenticated user identity extracted from a successful basic-auth check.
newtype AuthUser = AuthUser
  { user :: Text
  }
  deriving (Eq, Show)

-- | Expected credentials used to validate incoming basic-auth requests.
data AuthContext = AuthContext
  { authUser :: Text,
    authPassword :: Text
  }
  deriving (Eq, Show)

proxyBasicAuthContext :: Proxy '[BasicAuthCheck AuthUser]
proxyBasicAuthContext = Proxy

-- | Verify a username/password with constant-time byte comparison.
-- The incoming bytes are never UTF-8 decoded, so invalid UTF-8 credentials
-- fail the check rather than raising an exception.
authCheck :: AuthContext -> BasicAuthCheck AuthUser
authCheck AuthContext {authUser, authPassword} =
  let expectedUser = TE.encodeUtf8 authUser
      expectedPass = TE.encodeUtf8 authPassword
      check (BasicAuthData username password) =
        if constEq username expectedUser && constEq password expectedPass
          then return (Authorized (AuthUser authUser))
          else return Unauthorized
   in BasicAuthCheck check

-- | Build a Servant 'Context' containing the basic-auth check for use with 'serveWithContext'.
basicAuthServerContext :: AuthContext -> Context (BasicAuthCheck AuthUser ': '[])
basicAuthServerContext authContext = authCheck authContext :. EmptyContext

-- | Read basic-auth credentials from @BASIC_USER@ and @BASIC_PASS@.
-- Both must be set and non-empty, or the process exits (fail-closed) — there
-- are no default credentials.
getBasicAuthFromEnv :: IO AuthContext
getBasicAuthFromEnv = do
  mUser <- lookupEnv "BASIC_USER"
  mPass <- lookupEnv "BASIC_PASS"
  case (mUser, mPass) of
    (Just u, Just p)
      | not (null u), not (null p) ->
          return AuthContext {authUser = T.pack u, authPassword = T.pack p}
    _ -> die "BASIC_USER and BASIC_PASS must both be set to non-empty values (no default credentials)."
