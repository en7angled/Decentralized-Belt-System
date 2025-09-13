module WebAPI.Utils where

import System.Environment (lookupEnv)
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as BL8



getPortFromEnvOrDefault :: Int -> IO Int
getPortFromEnvOrDefault defaultPort = do
  eport <- lookupEnv "PORT"
  case eport of
    Nothing -> return defaultPort
    Just p -> return (read p)


