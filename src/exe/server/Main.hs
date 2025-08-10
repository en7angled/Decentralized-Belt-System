module Main where

import AppMonad (AppContext (..), AuthContext (..))
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

getPortFromEnv :: IO Int
getPortFromEnv = do
  eport <- lookupEnv "PORT"
  case eport of
    Nothing -> return 8082
    Just p -> return (read p)

getBasicAuthFromEnv :: IO AuthContext
getBasicAuthFromEnv = do
  user <- fromMaybe "cardano" <$> lookupEnv "BASIC_USER"
  pass <- fromMaybe "lovelace" <$> lookupEnv "BASIC_PASS"
  return AuthContext {authUser = Data.Text.pack user, authPassword = Data.Text.pack pass}

defaultAtlasCoreConfig :: FilePath
defaultAtlasCoreConfig = "config/config_atlas.json"

defaultTxBuldingContextFile :: FilePath
defaultTxBuldingContextFile = "config/config_bjj_validators.json"

getDeployedValidatorsConfigFromEnv :: IO FilePath
getDeployedValidatorsConfigFromEnv = fromMaybe defaultTxBuldingContextFile <$> lookupEnv "DEPLOYED_VALIDATORS_CONFIG"

getAtlasCoreConfigFromEnv :: IO FilePath
getAtlasCoreConfigFromEnv = fromMaybe defaultAtlasCoreConfig <$> lookupEnv "ATLAS_CORE_CONFIG"

main :: IO ()
main = do
  putStrLn "Writing Swagger file ..."
  BL8.writeFile "swagger-api.json" (encodePretty apiSwagger)

  atlasCoreConfig <- getAtlasCoreConfigFromEnv
  atlasConfig <- Data.Maybe.fromMaybe (error "Atlas configuration file not found") <$> decodeConfigFile @GYCoreConfig atlasCoreConfig

  deployedValidatorsConfig <- getDeployedValidatorsConfigFromEnv
  deployedScriptsContext <- Data.Maybe.fromMaybe (error "Deployed validators configuration file not found") <$> decodeConfigFile @DeployedScriptsContext deployedValidatorsConfig

  withCfgProviders atlasConfig (read @GYLogNamespace "BJJDApp") $ \providers -> do
    let providersContext = ProviderCtx atlasConfig providers
    let txBuildingContext = TxBuildingContext deployedScriptsContext providersContext
    authContext <- getBasicAuthFromEnv
    let appContext = AppContext authContext txBuildingContext

    let host = "0.0.0.0"
    port <- getPortFromEnv
    putStrLn $ "Starting server at " <> host <> " " <> show port
    putStrLn $ "Swagget-UI available at : http://" <> host <> ":" <> show port <> "/swagger-ui"

    let settings = setHost (fromString host :: HostPreference) $ setPort port defaultSettings -- host and port customized for heroku
    let bjjDApp = mkBJJApp appContext
    runSettings settings bjjDApp
