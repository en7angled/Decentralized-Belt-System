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
import Utils (decodeConfigEnvOrFile)

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


main :: IO ()
main = do
  putStrLn "Writing Swagger file ..."
  BL8.writeFile "swagger-api.json" (encodePretty apiSwagger)


  atlasConfig <- Data.Maybe.fromMaybe (error "Atlas configuration failed") <$> decodeConfigEnvOrFile "ATLAS_CORE_CONFIG" defaultAtlasCoreConfig
  deployedScriptsContext <- Data.Maybe.fromMaybe (error "Deployed validators configuration failed") <$> decodeConfigEnvOrFile "DEPLOYED_VALIDATORS_CONFIG" defaultTxBuldingContextFile

  withCfgProviders atlasConfig (read @GYLogNamespace "BJJDApp") $ \providers -> do
    putStrLn "Starting" 
    putStrLn $ "Atlas config: " <> show atlasConfig
    putStrLn $ "Deployed validators config: " <> show deployedScriptsContext


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
