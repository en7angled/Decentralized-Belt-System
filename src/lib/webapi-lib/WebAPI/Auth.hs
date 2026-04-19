-- | Basic authentication middleware for Servant APIs.
-- Reads credentials from @BASIC_USER@ and @BASIC_PASS@ environment variables.
module WebAPI.Auth where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding
import Servant
import System.Environment (lookupEnv)

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

-- | 'BasicAuthCheck' holds the handler we'll use to verify a username and password.
authCheck :: AuthContext -> BasicAuthCheck AuthUser
authCheck AuthContext {authUser, authPassword} =
  let check (BasicAuthData username password) =
        if Data.Text.Encoding.decodeUtf8 username == authUser && Data.Text.Encoding.decodeUtf8 password == authPassword
          then return (Authorized (AuthUser authUser))
          else return Unauthorized
   in BasicAuthCheck check

-- | Build a Servant 'Context' containing the basic-auth check for use with 'serveWithContext'.
basicAuthServerContext :: AuthContext -> Context (BasicAuthCheck AuthUser ': '[])
basicAuthServerContext authContext = authCheck authContext :. EmptyContext

-- | Read basic-auth credentials from @BASIC_USER@ and @BASIC_PASS@ env vars, defaulting to @cardano@/@lovelace@.
getBasicAuthFromEnv :: IO AuthContext
getBasicAuthFromEnv = do
  user <- fromMaybe "cardano" <$> lookupEnv "BASIC_USER"
  pass <- fromMaybe "lovelace" <$> lookupEnv "BASIC_PASS"
  return AuthContext {authUser = T.pack user, authPassword = T.pack pass}
