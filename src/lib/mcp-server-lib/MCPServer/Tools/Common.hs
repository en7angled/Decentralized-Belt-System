{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shared helpers used by every MCP tool module: upstream HTTP runners,
-- argument decoding, JSON-envelope builders, and a 'ClientError' sanitizer
-- that strips headers / bodies from upstream error responses before they
-- reach the LLM transcript.
module MCPServer.Tools.Common
  ( -- * Upstream runners (re-exports from 'MCPServer.Clients')
    runUpstreamQuery
  , runUpstreamInteraction
    -- * Argument decoding
  , requireArg
  , optionalArg
    -- * Address-argument decoding (bech32-or-hex)
  , requireAddress
  , optionalAddress
  , requireAddressList
    -- * Result envelopes
  , jsonResult
  , errorResult
    -- * Error sanitization
  , sanitizeClientError
  ) where

import Data.Aeson (FromJSON, ToJSON, Value, encode, fromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Foldable as F
import GeniusYield.Types (GYAddress, addressFromTextMaybe)
import MCP.Server (CallToolResult, toolTextError, toolTextResult)
import MCPServer.Clients (runUpstreamInteraction, runUpstreamQuery)
import Network.HTTP.Types (Status (..))
import Servant.Client
  ( ClientError
      ( ConnectionError
      , DecodeFailure
      , FailureResponse
      , InvalidContentTypeHeader
      , UnsupportedContentType
      )
  )
import Servant.Client.Core.Response (responseBody, responseStatusCode)

-- ---------------------------------------------------------------------------
-- Argument decoding
-- ---------------------------------------------------------------------------

-- | Look up a required tool argument by name and decode its JSON value.
-- Returns a human-readable error message on missing key or decode failure.
requireArg :: (FromJSON a) => Text -> Maybe (Map Text Value) -> Either Text a
requireArg key mArgs = case mArgs >>= Map.lookup key of
  Nothing ->
    Left ("missing required field: " <> key)
  Just v -> decodeArg key v

-- | Like 'requireArg' but returns 'Nothing' when the key is absent.
optionalArg :: (FromJSON a) => Text -> Maybe (Map Text Value) -> Either Text (Maybe a)
optionalArg key mArgs = case mArgs >>= Map.lookup key of
  Nothing -> Right Nothing
  Just v -> Just <$> decodeArg key v

decodeArg :: (FromJSON a) => Text -> Value -> Either Text a
decodeArg key v = case fromJSON v of
  Aeson.Success a -> Right a
  Aeson.Error msg -> Left ("invalid " <> key <> ": " <> T.pack msg)

-- | Decode a Cardano address argument, accepting both bech32 strings (the
-- human-friendly form the tool schema advertises — @addr_test1...@,
-- @addr1...@) and whatever 'GYAddress's default 'FromJSON' instance
-- accepts (hex bytes). Bech32 is tried first; on failure we fall back to
-- the 'FromJSON' path so hex-passing callers continue to work.
--
-- This exists because the tool schemas document bech32 but the Atlas
-- 'GYAddress' 'FromJSON' instance uses hex-only 'RawBytesHex'. Without
-- this shim, LLM-generated bech32 arguments get rejected with the
-- opaque "RawBytesHexErrorBase16DecodeFail" message.
decodeAddress :: Text -> Value -> Either Text GYAddress
decodeAddress key v = case v of
  Aeson.String t -> case addressFromTextMaybe t of
    Just addr -> Right addr
    Nothing -> decodeArg key v
  _ -> decodeArg key v

-- | Required-field counterpart to 'decodeAddress'.
requireAddress :: Text -> Maybe (Map Text Value) -> Either Text GYAddress
requireAddress key mArgs = case mArgs >>= Map.lookup key of
  Nothing -> Left ("missing required field: " <> key)
  Just v -> decodeAddress key v

-- | Optional-field counterpart to 'decodeAddress'.
optionalAddress :: Text -> Maybe (Map Text Value) -> Either Text (Maybe GYAddress)
optionalAddress key mArgs = case mArgs >>= Map.lookup key of
  Nothing -> Right Nothing
  Just Aeson.Null -> Right Nothing
  Just v -> Just <$> decodeAddress key v

-- | Decode a required array-of-addresses argument (e.g. @used_addresses@)
-- element-wise via 'decodeAddress', so bech32 and hex both work per-entry.
requireAddressList :: Text -> Maybe (Map Text Value) -> Either Text [GYAddress]
requireAddressList key mArgs = case mArgs >>= Map.lookup key of
  Nothing -> Left ("missing required field: " <> key)
  Just (Aeson.Array arr) -> traverse (decodeAddress key) (F.toList arr)
  Just v -> decodeArg key v

-- ---------------------------------------------------------------------------
-- Result envelopes
-- ---------------------------------------------------------------------------

-- | Wrap any 'ToJSON' payload as a single-text-block 'CallToolResult'.
-- MCP tool output is always text; the LLM reads the JSON and the parity
-- diff in Step 7 decodes it back.
jsonResult :: (ToJSON a) => a -> CallToolResult
jsonResult a = toolTextResult [TE.decodeUtf8 (BL.toStrict (encode a))]

-- | Wrap an error message in an @isError: true@ tool result.
errorResult :: Text -> CallToolResult
errorResult = toolTextError

-- ---------------------------------------------------------------------------
-- Error sanitization
-- ---------------------------------------------------------------------------

-- | Strip credential-leak-prone content from a @servant-client@ 'ClientError'
-- before surfacing it to the LLM:
--
-- * response headers are dropped unconditionally (would echo
--   @WWW-Authenticate: Basic realm=...@ on 401);
-- * 4xx response bodies are dropped (may echo credential / PII inputs);
-- * 5xx response bodies are preserved (debugging value outweighs leak risk);
-- * network-level errors collapse to a short category label.
--
-- Phase 3 may extend the category labels for tx-specific upstream errors.
sanitizeClientError :: ClientError -> Text
sanitizeClientError = \case
  FailureResponse _req resp ->
    let Status code reason = responseStatusCode resp
        label =
          "upstream "
            <> T.pack (show code)
            <> " "
            <> TE.decodeUtf8With (\_ _ -> Just '?') reason
     in if code >= 500
          then label <> ": " <> sampleBody (responseBody resp)
          else label
  DecodeFailure _ resp ->
    let Status code _ = responseStatusCode resp
     in "upstream " <> T.pack (show code) <> " decode failure"
  UnsupportedContentType _ resp ->
    let Status code _ = responseStatusCode resp
     in "upstream " <> T.pack (show code) <> " unsupported content-type"
  InvalidContentTypeHeader _ ->
    "upstream invalid content-type header"
  ConnectionError _ ->
    "upstream connection error"

-- Truncate the 5xx response body at 1 KiB so a very large upstream error
-- does not blow out the LLM's context window.
sampleBody :: BL.ByteString -> Text
sampleBody bs =
  let sampled = BL.take 1024 bs
   in TE.decodeUtf8With (\_ _ -> Just '?') (BL.toStrict sampled)

-- Silence -Wunused-imports — BL8.pack stays available for debugging shims.
_unusedBL8 :: BL.ByteString
_unusedBL8 = BL8.pack ""
