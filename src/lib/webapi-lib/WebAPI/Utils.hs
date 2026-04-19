{-# LANGUAGE OverloadedStrings #-}

-- | Shared utilities for web API executables (port configuration, Swagger helpers, etc.).
module WebAPI.Utils
  ( getPortFromEnvOrDefault
  , addSharedSwaggerDescriptions
  ) where

import Control.Lens ((&), (?~), at, mapped)
import Data.Swagger (Swagger, description, definitions)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- | Read the @PORT@ environment variable, falling back to the given default if unset or unparseable.
getPortFromEnvOrDefault :: Int -> IO Int
getPortFromEnvOrDefault defaultPort = do
  eport <- lookupEnv "PORT"
  case eport of
    Nothing -> return defaultPort
    Just p -> case readMaybe p of
      Just n -> return n
      Nothing -> do
        putStrLn $ "Warning: invalid PORT value; defaulting to " <> show defaultPort
        return defaultPort

-- | Add descriptions to shared Cardano/domain type definitions in a Swagger spec.
-- Call this in both interaction-api and query-api to keep descriptions consistent.
addSharedSwaggerDescriptions :: Swagger -> Swagger
addSharedSwaggerDescriptions s = s
  & definitions . at "GYAssetClass" . mapped . description ?~ "Cardano native asset identifier in format: <56-char policy ID hex>.<asset name hex>"
  & definitions . at "GYAddress" . mapped . description ?~ "Bech32-encoded Cardano address (addr_test1... on testnet, addr1... on mainnet)"
  & definitions . at "GYTime" . mapped . description ?~ "Timestamp in ISO 8601 format (e.g. 2024-06-15T10:30:00Z)"
  & definitions . at "BJJBelt" . mapped . description ?~ "BJJ belt rank. Values: White, Blue, Purple, Brown, Black, CoralWhite, CoralBlack, Red"
  & definitions . at "ProfileType" . mapped . description ?~ "Profile type: either Practitioner (individual) or Organization (academy/gym)"
