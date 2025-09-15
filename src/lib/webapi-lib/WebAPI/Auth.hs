module WebAPI.Auth where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding
import Servant
import System.Environment (lookupEnv)

newtype AuthUser = AuthUser
  { user :: Text
  }
  deriving (Eq, Show)

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

basicAuthServerContext :: AuthContext -> Context (BasicAuthCheck AuthUser ': '[])
basicAuthServerContext authContext = authCheck authContext :. EmptyContext

getBasicAuthFromEnv :: IO AuthContext
getBasicAuthFromEnv = do
  user <- fromMaybe "cardano" <$> lookupEnv "BASIC_USER"
  pass <- fromMaybe "lovelace" <$> lookupEnv "BASIC_PASS"
  return AuthContext {authUser = T.pack user, authPassword = T.pack pass}
