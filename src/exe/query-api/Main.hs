module Main where

import Constants (defaultAtlasCoreConfig)
import Data.Aeson.Encode.Pretty
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.Maybe
import Data.String (IsString (..))
import GeniusYield.GYConfig
import GeniusYield.Types
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Monad.Logger (runStdoutLoggingT)
import Database.Persist.Postgresql (createPostgresqlPool)
import Network.Wai.Handler.Warp
import QueryAppMonad (QueryAppContext (..))
import RestAPI (apiSwagger, mkBJJApp)
import System.Environment
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

  atlasConfig <- Data.Maybe.fromMaybe (error "Atlas configuration failed") <$> decodeConfigEnvOrFile "ATLAS_CORE_CONFIG" defaultAtlasCoreConfig

  withCfgProviders atlasConfig (read @GYLogNamespace "BJJDApp") $ \providers -> do
    let providersContext = ProviderCtx atlasConfig providers
    authContext <- getBasicAuthFromEnv
    connStr <- fromMaybe defaultConnStr <$> lookupEnv "PG_CONN_STR"
    pool <- runStdoutLoggingT $ createPostgresqlPool (T.encodeUtf8 (T.pack connStr)) 10
    let appContext = QueryAppContext authContext providersContext pool
    let host = "0.0.0.0"
    port <- getPortFromEnvOrDefault 8083

    let settings = setHost (fromString host :: HostPreference) $ setPort port defaultSettings
    let bjjDApp = mkBJJApp appContext

    putStrLn $ "Started Query API server at " <> host <> " " <> show port
    putStrLn $ "Atlas config: " <> show atlasConfig
    putStrLn $ "Postgres DSN: " <> connStr
    putStrLn $ "Swagger-UI available at : http://" <> host <> ":" <> show port <> "/swagger-ui"

    runSettings settings bjjDApp
