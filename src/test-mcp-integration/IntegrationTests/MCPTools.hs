{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests that exercise the MCP client layer against a running
-- @query-api@ and @interaction-api@. These are a schema-drift guard
-- (Decision #2 in the plan): every read route declared in
-- 'MCPServer.Api.Query' is decoded through the re-declared Servant types,
-- so any drift between our re-declared API and what the query-api actually
-- serves fails the test.
--
-- Fixture-driven tests look up IDs at runtime from @listProfiles@ and skip
-- gracefully when the seeded DB lacks the needed data. Write-tool parity
-- tests are placeholders pending fixture tooling (seeded pending promotions,
-- pending achievements, active memberships) — see the module-level note.
--
-- Assumes:
--
-- * @query-api@ is listening on @$QUERY_API_URL@ (default @http:\/\/localhost:8083@).
-- * @interaction-api@ on @$INTERACTION_API_URL@ (default @http:\/\/localhost:8082@).
-- * The DB has been seeded by @scripts\/populate_testnet.sh@.
-- * @BASIC_USER@ \/ @BASIC_PASS@ env vars match the APIs' expected auth.
--
-- Skipped by default; run with @MCP_INTEGRATION=1 cabal test mcp-integration@.
module IntegrationTests.MCPTools
  ( parityTests
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import qualified Data.Text.Encoding as TE
import DomainTypes.Core.Types (ProfileType (..))
import DomainTypes.Transfer.QueryResponses
  ( AchievementResponse (..)
  , ProfileResponse (..)
  )
import qualified MCPServer.Clients as C
import MCPServer.App (AppCtx (..))
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Servant.API.BasicAuth (BasicAuthData (BasicAuthData))
import Servant.Client (ClientError, parseBaseUrl)
import System.Environment (lookupEnv)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertFailure, testCase)

-- ---------------------------------------------------------------------------
-- AppCtx + shared helpers
-- ---------------------------------------------------------------------------

-- | Build an 'AppCtx' from env vars, with sensible test-time defaults.
mkTestCtx :: IO AppCtx
mkTestCtx = do
  qRaw <- fromMaybe "http://localhost:8083" <$> lookupEnv "QUERY_API_URL"
  iRaw <- fromMaybe "http://localhost:8082" <$> lookupEnv "INTERACTION_API_URL"
  qUrl <- parseBaseUrl qRaw
  iUrl <- parseBaseUrl iRaw
  u <- fromMaybe "cardano" <$> lookupEnv "BASIC_USER"
  p <- fromMaybe "lovelace" <$> lookupEnv "BASIC_PASS"
  let auth =
        BasicAuthData
          (TE.encodeUtf8 (fromString u))
          (TE.encodeUtf8 (fromString p))
  mgr <- newManager tlsManagerSettings
  pure
    AppCtx
      { queryBaseUrl = qUrl
      , interactionBaseUrl = iUrl
      , upstreamAuth = auth
      , httpManager = mgr
      , port = 0
      , enableWriteTx = False
      , readinessTimeoutMs = 2000
      }
  where
    fromString = TE.decodeUtf8 . BL.toStrict . BL.pack . fmap (fromIntegral . fromEnum)

-- | Fail the test on upstream error; otherwise hand the result to the body.
expectRight :: String -> IO (Either ClientError a) -> (a -> Assertion) -> Assertion
expectRight label io k =
  io >>= \case
    Left e -> assertFailure (label <> ": upstream call failed: " <> show e)
    Right x -> k x

-- | Visibly skip with a printed message. Used when a fixture couldn't be
-- found on the seeded DB — the test name stays in the output but the body
-- is a no-op. Prefer this over @assertBool True@ so the skip reason lands
-- in the test log.
skip :: String -> Assertion
skip reason = putStrLn ("  [skip] " <> reason)

-- | Run @listProfiles@ with a modest limit and return the full page.
allProfiles :: AppCtx -> IO [ProfileResponse]
allProfiles ctx =
  C.runUpstreamQuery
    ctx
    ( C.listProfiles
        (upstreamAuth ctx)
        (Just 50)
        Nothing
        []
        Nothing
        Nothing
        Nothing
        []
        Nothing
        Nothing
        Nothing
    )
    >>= \case
      Left e -> assertFailure ("listProfiles failed: " <> show e) >> error "unreachable"
      Right ps -> pure ps

firstOfType :: ProfileType -> [ProfileResponse] -> Maybe ProfileResponse
firstOfType wanted =
  foldr (\p acc -> if profileType p == wanted then Just p else acc) Nothing

-- ---------------------------------------------------------------------------
-- Test tree
-- ---------------------------------------------------------------------------

parityTests :: TestTree
parityTests =
  testGroup
    "MCP tool parity against local query-api + interaction-api"
    [ testGroup
        "Read tools"
        [ testCase "get_belt_frequency" beltFrequencyShape
        , testCase "list_profiles" listProfilesShape
        , testCase "get_practitioner_profile" getPractitionerProfileShape
        , testCase "get_organization_profile" getOrganizationProfileShape
        , testCase "get_practitioner_detail" getPractitionerDetailShape
        , testCase "get_organization_detail" getOrganizationDetailShape
        , testCase "get_promotions_page" getPromotionsPageShape
        , testCase "get_achievements" getAchievementsShape
        , testCase "get_achievement_by_id" getAchievementByIdShape
        , testCase "search" searchShape
        , testCase "get_pending_actions" getPendingActionsShape
        ]
    , testGroup
        "Write tools (build-tx)"
        -- Each test currently skips with a visible reason. Implementing
        -- full decoded-tx-body parity requires:
        --   1. seeded fixtures (known pending promotions, pending
        --      achievements, active memberships) from the privnet;
        --   2. wired-in Atlas decoders to compare 'inputs', 'outputs',
        --      'mint', 'requiredSigners', 'validityRange' between two
        --      build-tx responses.
        -- Tracked in the plan's Phase B write section.
        [ testCase "build_promote_rank_tx" (writeToolSkip "build_promote_rank_tx")
        , testCase "build_accept_promotion_tx" (writeToolSkip "build_accept_promotion_tx")
        , testCase "build_accept_achievement_tx" (writeToolSkip "build_accept_achievement_tx")
        , testCase "build_new_membership_tx" (writeToolSkip "build_new_membership_tx")
        , testCase "build_accept_membership_tx" (writeToolSkip "build_accept_membership_tx")
        , testCase "build_end_membership_tx" (writeToolSkip "build_end_membership_tx")
        ]
    ]

-- ---------------------------------------------------------------------------
-- Read tools
-- ---------------------------------------------------------------------------

-- | Sanity-check the belt-frequency endpoint shape.
beltFrequencyShape :: Assertion
beltFrequencyShape = do
  ctx <- mkTestCtx
  expectRight "get_belt_frequency"
    (C.runUpstreamQuery ctx (C.getBeltFrequency (upstreamAuth ctx)))
    $ \pairs -> assertBool "at least one belt bucket" (not (null pairs))

-- | @list_profiles@ with no filters should return a JSON array. Empty is OK.
listProfilesShape :: Assertion
listProfilesShape = do
  ctx <- mkTestCtx
  ps <- allProfiles ctx
  assertBool ("list_profiles returned " <> show (length ps) <> " entries") True

-- | Per-ID practitioner fetch; skips when no practitioner fixture exists.
getPractitionerProfileShape :: Assertion
getPractitionerProfileShape = do
  ctx <- mkTestCtx
  ps <- allProfiles ctx
  case firstOfType Practitioner ps of
    Nothing -> skip "no practitioner fixture in seeded DB"
    Just p ->
      expectRight "get_practitioner_profile"
        (C.runUpstreamQuery ctx (C.getPractitioner (upstreamAuth ctx) (profileId p)))
        $ \_ -> pure ()

-- | Per-ID organization fetch.
getOrganizationProfileShape :: Assertion
getOrganizationProfileShape = do
  ctx <- mkTestCtx
  ps <- allProfiles ctx
  case firstOfType Organization ps of
    Nothing -> skip "no organization fixture in seeded DB"
    Just p ->
      expectRight "get_organization_profile"
        (C.runUpstreamQuery ctx (C.getOrganization (upstreamAuth ctx) (profileId p)))
        $ \_ -> pure ()

-- | Practitioner detail (memberships + promotions given/received + achievements).
getPractitionerDetailShape :: Assertion
getPractitionerDetailShape = do
  ctx <- mkTestCtx
  ps <- allProfiles ctx
  case firstOfType Practitioner ps of
    Nothing -> skip "no practitioner fixture in seeded DB"
    Just p ->
      expectRight "get_practitioner_detail"
        (C.runUpstreamQuery ctx (C.getPractitionerDetail (upstreamAuth ctx) (profileId p)))
        $ \_ -> pure ()

-- | Organization detail aggregate.
getOrganizationDetailShape :: Assertion
getOrganizationDetailShape = do
  ctx <- mkTestCtx
  ps <- allProfiles ctx
  case firstOfType Organization ps of
    Nothing -> skip "no organization fixture in seeded DB"
    Just p ->
      expectRight "get_organization_detail"
        (C.runUpstreamQuery ctx (C.getOrganizationDetail (upstreamAuth ctx) (profileId p)))
        $ \_ -> pure ()

-- | Paginated promotions page. Empty page is OK.
getPromotionsPageShape :: Assertion
getPromotionsPageShape = do
  ctx <- mkTestCtx
  expectRight "get_promotions_page"
    ( C.runUpstreamQuery
        ctx
        ( C.getPromotionsPage
            (upstreamAuth ctx)
            (Just 10)
            Nothing
            []
            []
            []
            []
            []
            Nothing
            Nothing
            Nothing
            []
            Nothing
            Nothing
        )
    )
    $ \_ -> pure ()

-- | Paginated achievements list. Empty is OK.
getAchievementsShape :: Assertion
getAchievementsShape = do
  ctx <- mkTestCtx
  expectRight "get_achievements"
    ( C.runUpstreamQuery
        ctx
        ( C.getAchievements
            (upstreamAuth ctx)
            (Just 10)
            Nothing
            []
            []
            []
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing
        )
    )
    $ \_ -> pure ()

-- | Per-ID achievement fetch; skips when no achievement fixture exists.
getAchievementByIdShape :: Assertion
getAchievementByIdShape = do
  ctx <- mkTestCtx
  items <-
    C.runUpstreamQuery
      ctx
      ( C.getAchievements
          (upstreamAuth ctx)
          (Just 1)
          Nothing
          []
          []
          []
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
          Nothing
      )
      >>= \case
        Left e -> assertFailure ("achievements lookup failed: " <> show e) >> error "unreachable"
        Right as -> pure as
  case items of
    [] -> skip "no achievement fixture in seeded DB"
    (a : _) ->
      expectRight "get_achievement_by_id"
        (C.runUpstreamQuery ctx (C.getAchievementById (upstreamAuth ctx) (achievementId a)))
        $ \_ -> pure ()

-- | Free-form search.
searchShape :: Assertion
searchShape = do
  ctx <- mkTestCtx
  expectRight "search"
    (C.runUpstreamQuery ctx (C.search (upstreamAuth ctx) "belt"))
    $ \_ -> pure ()

-- | Pending actions inbox for a practitioner fixture.
getPendingActionsShape :: Assertion
getPendingActionsShape = do
  ctx <- mkTestCtx
  ps <- allProfiles ctx
  case firstOfType Practitioner ps of
    Nothing -> skip "no practitioner fixture in seeded DB"
    Just p ->
      expectRight "get_pending_actions"
        (C.runUpstreamQuery ctx (C.getPendingActions (upstreamAuth ctx) (profileId p)))
        $ \_ -> pure ()

-- ---------------------------------------------------------------------------
-- Write tools — placeholder
-- ---------------------------------------------------------------------------

-- | Emit a visible skip for each write tool. Replacing this with a real
-- parity check requires seeded fixtures and Atlas decoders as noted in the
-- test-group docstring above.
writeToolSkip :: String -> Assertion
writeToolSkip toolName =
  skip (toolName <> ": pending fixture tooling (see plan Phase B write section)")
