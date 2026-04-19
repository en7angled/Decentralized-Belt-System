{-# LANGUAGE LambdaCase #-}

-- | Entrypoint for the @mcp-integration@ test-suite.
--
-- This suite exercises MCP tool handlers against a running @query-api@ and
-- so requires locally-booted upstream services plus a seeded database. To
-- avoid breaking the default @cabal test@ workflow for contributors without
-- that stack, the suite short-circuits with a passing no-op unless the
-- @MCP_INTEGRATION=1@ environment variable is set.
--
-- Run with:
--
-- @
-- MCP_INTEGRATION=1 cabal test mcp-integration
-- @
module Main where

import IntegrationTests.MCPTools (parityTests)
import System.Environment (lookupEnv)
import Test.Tasty (defaultMain)

main :: IO ()
main =
  lookupEnv "MCP_INTEGRATION" >>= \case
    Just "1" -> defaultMain parityTests
    _ ->
      putStrLn
        "skipping mcp-integration suite; set MCP_INTEGRATION=1 to run \
        \(requires local query-api + interaction-api + seeded DB)"
