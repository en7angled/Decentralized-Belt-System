module Main where

import Constants (defaultAtlasCoreConfig)
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Maybe
import Data.String (IsString (..))
import Data.Text
import GeniusYield.GYConfig
import GeniusYield.Types
import Network.Wai.Handler.Warp
import QueryAppMonad (QueryAppContext (..))
import RestAPI (apiSwagger, mkBJJApp)
import System.Environment
import TxBuilding.Context
import Utils (decodeConfigEnvOrFile)
import WebAPI.Auth (getBasicAuthFromEnv)
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

    putStrLn $ "Started Query API server at " <> host <> " " <> show port
    putStrLn $ "Atlas config: " <> show atlasConfig
    putStrLn $ "Lookup path: " <> show lookupContext
    putStrLn $ "Swagger-UI available at : http://" <> host <> ":" <> show port <> "/swagger-ui"

    runSettings settings bjjDApp
