{-# LANGUAGE OverloadedStrings #-}

module ChainSyncServer (startProbeServer) where

import ChainsyncAPI (SyncMetrics (..), mkServiceProbeApp)
import Control.Concurrent.MVar (MVar)
import Data.String (fromString)
import Network.Wai.Handler.Warp (HostPreference, defaultSettings, runSettings, setHost, setPort)

startProbeServer :: String -> Int -> MVar SyncMetrics -> IO ()
startProbeServer kupoUrl port metricsVar = do
  let host = "0.0.0.0"
  putStrLn $ "Starting chain-sync probe server at " <> host <> " " <> show port
  let settings = setHost (fromString host :: HostPreference) $ setPort port defaultSettings
  let app = mkServiceProbeApp kupoUrl metricsVar
  runSettings settings app


