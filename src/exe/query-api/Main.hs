module Main where

import Constants (defaultAtlasCoreConfig, defaultTxBuldingContextFile)
import Control.Concurrent.Extra
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Maybe
import Data.String (IsString (..))
import Data.Text
import qualified Data.Text as T
import GeniusYield.GYConfig
import GeniusYield.Types
import Network.Wai.Handler.Warp
import QueryAppMonad (QueryAppContext (..))
import RestAPI (apiSwagger, mkBJJApp)
import System.Environment
import TxBuilding.Context
import TxBuilding.Utils
import Utils (decodeConfigEnvOrFile)
import WebAPI.Auth (AuthContext (..), getBasicAuthFromEnv)
import WebAPI.Utils (getPortFromEnvOrDefault)

defaultLookUpPath :: FilePath
defaultLookUpPath = "db/chainsync.sqlite"

main :: IO ()
main = do
  putStrLn "Writing Swagger file ..."
  BL8.writeFile "swagger-api.json" (encodePretty apiSwagger)

  atlasConfig <- Data.Maybe.fromMaybe (error "Atlas configuration failed") <$> decodeConfigEnvOrFile "ATLAS_CORE_CONFIG" defaultAtlasCoreConfig

  withCfgProviders atlasConfig (read @GYLogNamespace "BJJDApp") $ \providers -> do
    let providersContext = ProviderCtx atlasConfig providers
    authContext <- getBasicAuthFromEnv
    lookupContext <- Data.Text.pack . fromMaybe defaultLookUpPath <$> lookupEnv "LOOKUP_PATH"
    let appContext = QueryAppContext authContext providersContext lookupContext
    let host = "0.0.0.0"
    port <- getPortFromEnvOrDefault 8083

    let settings = setHost (fromString host :: HostPreference) $ setPort port defaultSettings
    let bjjDApp = mkBJJApp appContext
    forkIO $ runSettings settings bjjDApp

    putStrLn $ "Started Query API server at " <> host <> " " <> show port
    putStrLn $ "Atlas config: " <> show atlasConfig
    putStrLn $ "Lookup path: " <> show lookupContext
    putStrLn $ "Swagger-UI available at : http://" <> host <> ":" <> show port <> "/swagger-ui"
