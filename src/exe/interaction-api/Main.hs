module Main where

import Constants
  ( defaultAtlasCoreConfig,
    defaultTxBuldingContextFile,
  )
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Maybe
import Data.String (IsString (..))
import Data.Text
import GeniusYield.GYConfig
import GeniusYield.Types
import InteractionAppMonad (InteractionAppContext (..))
import Network.Wai.Handler.Warp
import RestAPI (apiSwagger, mkBJJApp)
import System.Environment
import TxBuilding.Context
import TxBuilding.Utils
import Utils (decodeConfigEnvOrFile)
import WebAPI.Auth (AuthContext (..), getBasicAuthFromEnv)
import WebAPI.Utils (getPortFromEnvOrDefault)

main :: IO ()
main = do
  putStrLn "Writing Swagger file ..."
  BL8.writeFile "swagger-api.json" (encodePretty apiSwagger)

  atlasConfig <- Data.Maybe.fromMaybe (error "Atlas configuration failed") <$> decodeConfigEnvOrFile "ATLAS_CORE_CONFIG" defaultAtlasCoreConfig
  deployedScriptsContext <- Data.Maybe.fromMaybe (error "Deployed validators configuration failed") <$> decodeConfigEnvOrFile "DEPLOYED_VALIDATORS_CONFIG" defaultTxBuldingContextFile

  withCfgProviders atlasConfig (read @GYLogNamespace "BJJDApp") $ \providers -> do
    let providersContext = ProviderCtx atlasConfig providers
    let txBuildingContext = TxBuildingContext deployedScriptsContext providersContext
    authContext <- getBasicAuthFromEnv
    let appContext = InteractionAppContext authContext txBuildingContext

    let host = "0.0.0.0"
    port <- getPortFromEnvOrDefault 8082

    let settings = setHost (fromString host :: HostPreference) $ setPort port defaultSettings
    let bjjDApp = mkBJJApp appContext
    runSettings settings bjjDApp

    putStrLn $ "Started Interaction API server at " <> host <> " " <> show port
    putStrLn $ "Atlas config: " <> show atlasConfig
    putStrLn $ "Deployed validators config: " <> show deployedScriptsContext
    putStrLn $ "Swagger-UI available at : http://" <> host <> ":" <> show port <> "/swagger-ui"
