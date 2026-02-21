module Main where

import Constants (defaultAtlasCoreConfig)
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Maybe
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Database.Persist.Postgresql (createPostgresqlPool)
import GeniusYield.GYConfig
import GeniusYield.Types
import Network.Wai.Handler.Warp
import QueryAppMonad (QueryAppContext (..))
import RestAPI (apiSwagger, mkBJJApp)
import System.Environment
import System.Exit (die)
import TxBuilding.Context
import Utils (decodeConfigEnvOrFile)
import WebAPI.Auth (getBasicAuthFromEnv)
import WebAPI.Utils (getPortFromEnvOrDefault)

defaultConnStr :: String
defaultConnStr = "host=postgres user=postgres password=postgres dbname=chainsync port=5432"

main :: IO ()
main = do
  putStrLn "Writing Swagger file ..."
  BL8.writeFile "docs/swagger/query-swagger-api.json" (encodePretty apiSwagger)

  atlasConfig <- maybe (die "Atlas configuration failed") return =<< decodeConfigEnvOrFile "ATLAS_CORE_CONFIG" defaultAtlasCoreConfig

  withCfgProviders atlasConfig (read @GYLogNamespace "BJJDApp") $ \providers -> do
    let providerContext = ProviderCtx atlasConfig providers
    authContext <- getBasicAuthFromEnv
    connStr <- fromMaybe defaultConnStr <$> lookupEnv "PG_CONN_STR"
    pool <- runStdoutLoggingT $ createPostgresqlPool (T.encodeUtf8 (T.pack connStr)) 10
    let appContext = QueryAppContext authContext providerContext pool
    let host = "0.0.0.0"
    port <- getPortFromEnvOrDefault 8083

    let settings = setHost (fromString host :: HostPreference) $ setPort port defaultSettings
    let bjjDApp = mkBJJApp appContext

    putStrLn $ "Started Query API server at " <> host <> " " <> show port
    putStrLn $ "Atlas config: " <> show atlasConfig
    putStrLn $ "Postgres DSN: " <> connStr
    putStrLn $ "Swagger-UI available at : http://" <> host <> ":" <> show port <> "/swagger-ui"

    runSettings settings bjjDApp
