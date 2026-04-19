{-# LANGUAGE OverloadedStrings #-}

-- | Pure unit tests for the MCP server's tool surface. No HTTP, no env vars.
-- Covers: 'InputSchema' shape, argument decoding, JSON-envelope builders,
-- the 'ClientError' sanitizer, 'bjjResources' metadata, 'UserAddresses'
-- decoding, and write-tool gating.
--
-- Integration tests that hit a running query-api live in the separate
-- @mcp-integration@ test suite (gated on @MCP_INTEGRATION=1@).
module UnitTests.MCPTools
  ( mcpToolsTests
  ) where

import Data.Aeson (Value (Object), decode, encode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import MCP.Server (InputSchema)
import MCPServer.App (AppCtx (..))
import MCPServer.Readiness
  ( UpstreamCheck (..)
  , checkUpstreamReady
  )
import MCPServer.Resources (bjjResources)
import MCPServer.Schema
  ( arrayOf
  , beltField
  , emptyInputSchema
  , objectSchema
  , paginationFields
  , profileRefField
  )
import qualified MCPServer.Tools.Achievements as ToolsAchievements
import MCPServer.Tools.Common
  ( optionalArg
  , requireArg
  )
import qualified MCPServer.Tools.Memberships as ToolsMemberships
import qualified MCPServer.Tools.Promotions as ToolsPromotions
import MCPServer.Tools.Transactions (decodeRecipient, decodeUserAddresses)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Types.Status (status200)
import Network.Wai (responseLBS)
import qualified Network.Wai.Handler.Warp as Warp
import Servant.API.BasicAuth (BasicAuthData (..))
import Servant.Client (parseBaseUrl)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase, (@?=))

mcpToolsTests :: TestTree
mcpToolsTests =
  testGroup
    "MCP Tools (pure)"
    [ testCase "emptyInputSchema encodes to {\"type\":\"object\"}" emptyInputSchemaEncodesObject
    , testCase "get_practitioner_profile schema requires profile_id with AC regex" practitionerSchemaShape
    , testCase "list_profiles schema has belts array with full BJJBelt enum" listProfilesBeltArray
    , testCase "requireArg Right on valid Text field" requireArgHappyPath
    , testCase "requireArg Left on Nothing args" requireArgNoArgs
    , testCase "requireArg Left on missing key" requireArgMissingKey
    , testCase "requireArg Left on wrong-typed value" requireArgTypeMismatch
    , testCase "optionalArg returns Right Nothing when key absent" optionalArgAbsent
    , testCase "bjjResources exposes annex-3 + belt-hierarchy + faq" bjjResourcesSet
    , testCase "checkUpstreamReady reports unready when upstreams are unreachable" readinessUnreachable
    , testCase "checkUpstreamReady reports ready when upstreams serve 200 on /health" readinessReady
    , testCase "decodeUserAddresses fails when used_addresses is missing" decodeUserAddressesMissingUsed
    , testCase "decodeUserAddresses fails when change_address is missing" decodeUserAddressesMissingChange
    , testCase "decodeRecipient returns Nothing when absent" decodeRecipientAbsent
    , testCase "Memberships.tools is empty when enableWriteTx is False" membershipsGatedOff
    , testCase "Memberships.tools has 4 write tools when enableWriteTx is True" membershipsGatedOn
    , testCase "Promotions.tools has 3 read tools when enableWriteTx is False" promotionsGatedOff
    , testCase "Promotions.tools has 3 read + 2 write tools when enableWriteTx is True" promotionsGatedOn
    , testCase "Achievements.tools has 2 read tools when enableWriteTx is False" achievementsGatedOff
    , testCase "Achievements.tools has 2 read + 1 write tool when enableWriteTx is True" achievementsGatedOn
    ]

-- ---------------------------------------------------------------------------
-- Schema shape
-- ---------------------------------------------------------------------------

emptyInputSchemaEncodesObject :: IO ()
emptyInputSchemaEncodesObject = do
  let encoded = encode (emptyInputSchema :: InputSchema)
  assertEqual
    "empty schema JSON"
    "{\"type\":\"object\"}"
    encoded

practitionerSchemaShape :: IO ()
practitionerSchemaShape = do
  let schema =
        objectSchema
          [("profile_id", profileRefField)]
          ["profile_id"]
      encoded = encode schema
  case decode encoded :: Maybe Value of
    Nothing -> assertBool "schema decodes" False
    Just v -> do
      -- Top-level "required" and the nested pattern round-trip.
      requireRoundTrip v "required" $ \r -> r @?= Aeson.toJSON ["profile_id" :: Text]
      requireNested v ["properties", "profile_id", "pattern"] $ \pat ->
        pat @?= Aeson.String "^[0-9a-f]{56}\\.[0-9a-f]*$"

listProfilesBeltArray :: IO ()
listProfilesBeltArray = do
  let schema =
        objectSchema
          (paginationFields <> [("belts", arrayOf beltField)])
          []
      encoded = encode schema
  case decode encoded :: Maybe Value of
    Nothing -> assertBool "belt-schema decodes" False
    Just v ->
      requireNested v ["properties", "belts", "items", "enum"] $ \enumVal ->
        case enumVal of
          Aeson.Array xs ->
            -- 15 BJJBelt constructors: White .. Red10.
            assertEqual "belt enum length" 15 (length xs)
          other -> assertEqual "belt enum is array" (Aeson.String "<array>") other

-- ---------------------------------------------------------------------------
-- Argument decoding
-- ---------------------------------------------------------------------------

requireArgHappyPath :: IO ()
requireArgHappyPath = do
  let args = Just (Map.singleton "name" (Aeson.String "alice"))
  (requireArg "name" args :: Either Text Text) @?= Right "alice"

requireArgNoArgs :: IO ()
requireArgNoArgs = do
  (requireArg "name" Nothing :: Either Text Text)
    @?= Left "missing required field: name"

requireArgMissingKey :: IO ()
requireArgMissingKey = do
  let args = Just (Map.singleton "other" (Aeson.String "bob"))
  (requireArg "name" args :: Either Text Text)
    @?= Left "missing required field: name"

requireArgTypeMismatch :: IO ()
requireArgTypeMismatch = do
  let args = Just (Map.singleton "name" (Aeson.Number 42))
  case (requireArg "name" args :: Either Text Text) of
    Left msg ->
      assertBool
        ("expected type-mismatch prefix, got: " <> T.unpack msg)
        ("invalid name" `T.isPrefixOf` msg)
    Right _ -> assertBool "expected Left on type mismatch" False

optionalArgAbsent :: IO ()
optionalArgAbsent = do
  let args = Just (Map.singleton "other" (Aeson.String "x")) :: Maybe (Map Text Value)
  (optionalArg "name" args :: Either Text (Maybe Text))
    @?= Right Nothing

-- ---------------------------------------------------------------------------
-- UserAddresses / recipient decoders (shared by every build_*_tx tool)
-- ---------------------------------------------------------------------------

decodeUserAddressesMissingUsed :: IO ()
decodeUserAddressesMissingUsed = do
  let args =
        Just
          ( Map.fromList
              [ ("change_address", Aeson.String "addr_test1xxx")
              ]
          )
  case decodeUserAddresses args of
    Left msg ->
      assertBool
        ("expected 'used_addresses' in error, got: " <> T.unpack msg)
        ("used_addresses" `T.isInfixOf` msg)
    Right _ -> assertBool "expected Left when used_addresses missing" False

decodeUserAddressesMissingChange :: IO ()
decodeUserAddressesMissingChange = do
  -- Empty used_addresses decodes successfully (zero elements to parse),
  -- so the short-circuit falls through to the change_address check.
  let args =
        Just
          ( Map.fromList
              [ ("used_addresses", Aeson.toJSON ([] :: [Text]))
              ]
          )
  case decodeUserAddresses args of
    Left msg ->
      assertBool
        ("expected 'change_address' in error, got: " <> T.unpack msg)
        ("change_address" `T.isInfixOf` msg)
    Right _ -> assertBool "expected Left when change_address missing" False

decodeRecipientAbsent :: IO ()
decodeRecipientAbsent = do
  let args = Just Map.empty :: Maybe (Map Text Value)
  decodeRecipient args @?= Right Nothing

-- ---------------------------------------------------------------------------
-- Write-tool gating
-- ---------------------------------------------------------------------------

mkTestCtx :: Bool -> IO AppCtx
mkTestCtx writeTx = do
  qUrl <- parseBaseUrl "http://localhost:8083"
  iUrl <- parseBaseUrl "http://localhost:8082"
  mgr <- newTlsManager
  pure
    AppCtx
      { queryBaseUrl = qUrl
      , interactionBaseUrl = iUrl
      , upstreamAuth = BasicAuthData "u" "p"
      , httpManager = mgr
      , port = 8085
      , enableWriteTx = writeTx
      , readinessTimeoutMs = 2000
      }

membershipsGatedOff :: IO ()
membershipsGatedOff = do
  ctx <- mkTestCtx False
  length (ToolsMemberships.tools ctx) @?= 0

membershipsGatedOn :: IO ()
membershipsGatedOn = do
  ctx <- mkTestCtx True
  length (ToolsMemberships.tools ctx) @?= 4

promotionsGatedOff :: IO ()
promotionsGatedOff = do
  ctx <- mkTestCtx False
  -- get_promotions_page, get_belt_frequency, check_promotion_eligibility
  length (ToolsPromotions.tools ctx) @?= 3

promotionsGatedOn :: IO ()
promotionsGatedOn = do
  ctx <- mkTestCtx True
  -- 3 read tools + build_promote_rank_tx + build_accept_promotion_tx
  length (ToolsPromotions.tools ctx) @?= 5

achievementsGatedOff :: IO ()
achievementsGatedOff = do
  ctx <- mkTestCtx False
  length (ToolsAchievements.tools ctx) @?= 2

achievementsGatedOn :: IO ()
achievementsGatedOn = do
  ctx <- mkTestCtx True
  length (ToolsAchievements.tools ctx) @?= 3

-- ---------------------------------------------------------------------------
-- Readiness probe
-- ---------------------------------------------------------------------------

-- | Build an 'AppCtx' pointing at two arbitrary base URLs for readiness
-- probes. Unlike 'mkTestCtx' we accept explicit URLs so tests can point at
-- either a dead port or a locally-spun-up stub.
mkReadinessCtx :: String -> String -> Int -> IO AppCtx
mkReadinessCtx qRaw iRaw timeoutMs = do
  qUrl <- parseBaseUrl qRaw
  iUrl <- parseBaseUrl iRaw
  -- Plain manager — tests run over http on localhost, no TLS in flight.
  mgr <- newManager defaultManagerSettings
  pure
    AppCtx
      { queryBaseUrl = qUrl
      , interactionBaseUrl = iUrl
      , upstreamAuth = BasicAuthData "u" "p"
      , httpManager = mgr
      , port = 8085
      , enableWriteTx = False
      , readinessTimeoutMs = timeoutMs
      }

-- | When neither upstream is reachable the probe must flag both as unready
-- and carry enough detail in each 'UpstreamCheck' to diagnose which one is
-- down. Port 1 is reliably unused on macOS/Linux development machines.
readinessUnreachable :: IO ()
readinessUnreachable = do
  ctx <- mkReadinessCtx "http://127.0.0.1:1" "http://127.0.0.1:1" 500
  checks <- checkUpstreamReady ctx
  length checks @?= 2
  assertEqual "query-api label" "query-api" (upstreamLabel (head checks))
  assertEqual "interaction-api label" "interaction-api" (upstreamLabel (checks !! 1))
  assertBool "both unready" (not (any upstreamReady checks))

-- | With a local WAI stub responding 200 to @GET \/health@, the probe must
-- classify both upstreams as ready. 'Warp.testWithApplication' binds an
-- ephemeral port and tears down when the continuation returns.
readinessReady :: IO ()
readinessReady =
  Warp.testWithApplication (pure healthStub) $ \port1 ->
    Warp.testWithApplication (pure healthStub) $ \port2 -> do
      ctx <-
        mkReadinessCtx
          ("http://127.0.0.1:" <> show port1)
          ("http://127.0.0.1:" <> show port2)
          2000
      checks <- checkUpstreamReady ctx
      length checks @?= 2
      assertBool "both ready" (all upstreamReady checks)
  where
    healthStub _req respond =
      respond (responseLBS status200 [] "{\"status\":\"healthy\"}")

-- ---------------------------------------------------------------------------
-- Resources
-- ---------------------------------------------------------------------------

bjjResourcesSet :: IO ()
bjjResourcesSet = do
  let uris = fmap uriOf bjjResources
  assertEqual
    "exposed resource URIs"
    ["bjj://rules/annex-3", "bjj://rules/belt-hierarchy", "bjj://docs/faq"]
    uris
  where
    uriOf res =
      case decode (encode res) :: Maybe Value of
        Just (Object km) ->
          case KM.lookup "uri" km of
            Just (Aeson.String t) -> t
            _ -> "<no uri>"
        _ -> "<decode failed>"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- Look up a single JSON-object key at the top level and hand it to the
-- caller.  Fails the test with a readable message if the key is missing.
requireRoundTrip :: Value -> Text -> (Value -> IO ()) -> IO ()
requireRoundTrip v key k = case v of
  Object km ->
    case KM.lookup (Aeson.fromString (T.unpack key)) km of
      Just sub -> k sub
      Nothing ->
        assertBool ("missing top-level key: " <> T.unpack key) False
  _ -> assertBool "expected top-level JSON object" False

-- Walk a list of nested JSON-object keys and run the continuation on the
-- final value.  Fails the test if any hop is missing or not an object.
requireNested :: Value -> [Text] -> (Value -> IO ()) -> IO ()
requireNested v [] k = k v
requireNested (Object km) (key : rest) k =
  case KM.lookup (Aeson.fromString (T.unpack key)) km of
    Just sub -> requireNested sub rest k
    Nothing ->
      assertBool ("missing nested key: " <> T.unpack key) False
requireNested _ _ _ = assertBool "expected JSON object at this hop" False
