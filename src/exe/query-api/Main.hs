module Main where

import QueryAppMonad (QueryAppContext (..))
import WebAPI.Auth (AuthContext (..), getBasicAuthFromEnv)
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Maybe
import Data.String (IsString (..))
import Data.Text
import GeniusYield.GYConfig
import GeniusYield.Types
import Network.Wai.Handler.Warp
import RestAPI (apiSwagger, mkBJJApp)
import System.Environment
import TxBuilding.Context
import TxBuilding.Utils
import Utils (decodeConfigEnvOrFile)
import Constants (defaultAtlasCoreConfig)
import Constants (defaultTxBuldingContextFile)
import Constants (defaultLookUpPath)

getPortFromEnv :: IO Int
getPortFromEnv = do
  eport <- lookupEnv "PORT"
  case eport of
    Nothing -> return 8083
    Just p -> return (read p)


main :: IO ()
main = do
  putStrLn "Writing Swagger file ..."
  BL8.writeFile "swagger-api.json" (encodePretty apiSwagger)

  atlasConfig <- Data.Maybe.fromMaybe (error "Atlas configuration failed") <$> decodeConfigEnvOrFile "ATLAS_CORE_CONFIG" defaultAtlasCoreConfig
  deployedScriptsContext <- Data.Maybe.fromMaybe (error "Deployed validators configuration failed") <$> decodeConfigEnvOrFile "DEPLOYED_VALIDATORS_CONFIG" defaultTxBuldingContextFile

  withCfgProviders atlasConfig (read @GYLogNamespace "BJJDApp") $ \providers -> do
    putStrLn "Starting Query API server"
    putStrLn $ "Atlas config: " <> show atlasConfig
    putStrLn $ "Deployed validators config: " <> show deployedScriptsContext

    let providersContext = ProviderCtx atlasConfig providers
    let txBuildingContext = TxBuildingContext deployedScriptsContext providersContext
    authContext <- getBasicAuthFromEnv
    lookupContext <- Data.Text.pack . fromMaybe defaultLookUpPath <$> lookupEnv "LOOKUP_PATH"
    let appContext = QueryAppContext authContext providersContext lookupContext

    let host = "0.0.0.0"
    port <- getPortFromEnv
    putStrLn $ "Starting Query API server at " <> host <> " " <> show port
    putStrLn $ "Lookup path: " <> show lookupContext

    putStrLn $ "Swagger-UI available at : http://" <> host <> ":" <> show port <> "/swagger-ui"

    let settings = setHost (fromString host :: HostPreference) $ setPort port defaultSettings
    let bjjDApp = mkBJJApp appContext
    runSettings settings bjjDApp
