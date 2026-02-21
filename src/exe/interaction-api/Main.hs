module Main where

import Constants
  ( defaultAtlasCoreConfig,
    defaultTxBuildingContextFile,
  )
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.String (IsString (..))
import GeniusYield.GYConfig
import GeniusYield.Types
import InteractionAppMonad (InteractionAppContext (..))
import Network.Wai.Handler.Warp
import RestAPI (apiSwagger, mkBJJApp)
import System.Exit (die)
import TxBuilding.Context
import Utils (decodeConfigEnvOrFile)
import WebAPI.Auth (getBasicAuthFromEnv)
import WebAPI.Utils (getPortFromEnvOrDefault)

main :: IO ()
main = do
  putStrLn "Writing Swagger file ..."
  BL8.writeFile "docs/swagger/interaction-swagger-api.json" (encodePretty apiSwagger)

  atlasConfig <- maybe (die "Atlas configuration failed") return =<< decodeConfigEnvOrFile "ATLAS_CORE_CONFIG" defaultAtlasCoreConfig
  deployedScriptsContext <- maybe (die "Deployed validators configuration failed") return =<< decodeConfigEnvOrFile "DEPLOYED_VALIDATORS_CONFIG" defaultTxBuildingContextFile

  withCfgProviders atlasConfig (read @GYLogNamespace "BJJDApp") $ \providers -> do
    let providersContext = ProviderCtx atlasConfig providers
    let txBuildingContext = TxBuildingContext deployedScriptsContext providersContext
    authContext <- getBasicAuthFromEnv
    let appContext = InteractionAppContext authContext txBuildingContext

    let host = "0.0.0.0"
    port <- getPortFromEnvOrDefault 8082

    let settings = setHost (fromString host :: HostPreference) $ setPort port defaultSettings
    let bjjDApp = mkBJJApp appContext

    putStrLn $ "Started Interaction API server at " <> host <> " " <> show port
    putStrLn $ "Atlas config: " <> show atlasConfig
    putStrLn $ "Deployed validators config: " <> show deployedScriptsContext
    putStrLn $ "Swagger-UI available at : http://" <> host <> ":" <> show port <> "/swagger-ui"

    runSettings settings bjjDApp
