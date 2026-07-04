# API Security Hardening Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Close the API/server security perimeter (fail-closed auth, env CORS allowlist, bounded inputs, sanitized errors, redacted logs) across the four HTTP servers and `webapi-lib`, plus the coordinated `bjj-frontend` deploy change.

**Architecture:** Security-critical pure helpers (auth compare, CORS policy, LIKE escaping, image magic-bytes, error mapping) live in **`webapi-lib`** so the test-suite can unit-test them (executables are not importable). Trivial arithmetic clamps and IO wiring stay in the executables, verified by build + reasoning. No new features.

**Tech Stack:** Haskell/GHC 9.6, Cabal, Servant, wai-cors, servant-multipart, `memory` (constant-time compare), Tasty/HUnit, Postgres/esqueleto, Docker Compose.

**Reference spec:** `docs/superpowers/specs/2026-07-04-api-security-hardening-design.md`.

## Global Constraints

- **Fail-closed.** Missing `BASIC_USER`/`BASIC_PASS` → `die` at startup, no defaults. Empty `CORS_ALLOWED_ORIGINS` → no cross-origin allowed. `allowCredentials` hard-coded `False`.
- **`webapi-lib` stays project-lib-free** (no offchain/onchain/chainsync imports). External deps (`bytestring`, `memory`, `wai`, `wai-cors`, `servant`) are fine.
- **Style:** max 120 chars/line, 2-space indent, one blank line between top-level decls, `-- |` Haddock on new exports. PascalCase types, camelCase functions.
- **Query error mapping:** query-api throws **bare** `TxBuildingException` via `throwIO` (no `GYApplicationException` wrapper). The handler MUST `try @TxBuildingException` directly — do **not** copy `runWithTxErrorHandling`'s `GYApplicationException`/`cast` shape.
- **LIKE escaping** goes in the shared `likePat` chokepoint, not per-endpoint.
- **No secret committed.** `.env.example` files use placeholders only.
- **After each task:** `cabal build all` then the relevant `cabal test` must pass.

---

### Task 1: webapi-lib fail-closed auth + constant-time compare (F-11, F-29) + test scaffolding

**Files:**
- Modify: `Decentralized-Belt-System.cabal` (webapi-lib deps; test-suite deps + module)
- Modify: `src/lib/webapi-lib/WebAPI/Auth.hs`
- Create: `src/test/UnitTests/ApiHardening.hs`
- Modify: `src/test/UnitTests.hs`

**Interfaces:**
- Produces: `getBasicAuthFromEnv :: IO AuthContext` (now dies on missing/empty env); `authCheck :: AuthContext -> BasicAuthCheck AuthUser` (constant-time, no input decode); `apiHardeningTests :: TestTree`.

- [ ] **Step 1: Add deps.** In `Decentralized-Belt-System.cabal`, the `library webapi-lib` `build-depends` block (currently `aeson, deriving-aeson, http-types, lens, servant, servant-server, swagger2, text, time, wai, wai-cors`) — add `bytestring` and `memory`:

```
  build-depends:
    , aeson
    , bytestring
    , deriving-aeson
    , http-types
    , lens
    , memory
    , servant
    , servant-server
    , swagger2
    , text
    , time
    , wai
    , wai-cors
```

  In the `test-suite test` first `build-depends` block (currently `base, chainsync-lib, mcp, mcp-server-lib, offchain-lib, onchain-lib, text`) add `webapi-lib`; in the second block add `servant-server`:

```
  build-depends:
    , base
    , chainsync-lib
    , mcp
    , mcp-server-lib
    , offchain-lib
    , onchain-lib
    , text
    , webapi-lib
```

  and add `, servant-server` alphabetically into the second `build-depends` block (the one containing `servant`, `servant-client`). In `test-suite test` `other-modules`, add `UnitTests.ApiHardening` (alphabetically, before `UnitTests.ChainSyncReplay`).

- [ ] **Step 2: Write the failing test.** Create `src/test/UnitTests/ApiHardening.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- | Pure unit tests for the API-hardening security helpers in webapi-lib
-- (fail-closed auth compare, CORS allowlist policy, LIKE escaping, image
-- magic-byte detection). IO wiring (startup die, middleware threading) is
-- covered by build + manual reasoning, not here.
module UnitTests.ApiHardening (apiHardeningTests) where

import Servant (BasicAuthData (..))
import Servant.Server (BasicAuthCheck (..), BasicAuthResult (..))
import Test.Tasty
import Test.Tasty.HUnit
import WebAPI.Auth (AuthContext (..), AuthUser (..), authCheck)

-- Run a BasicAuthCheck's handler against supplied raw credential bytes.
runCheck :: AuthContext -> BasicAuthData -> IO (BasicAuthResult AuthUser)
runCheck ctx bad = let BasicAuthCheck f = authCheck ctx in f bad

authTests :: TestTree
authTests =
  testGroup
    "authCheck"
    [ testCase "correct credentials authorize" $ do
        r <- runCheck (AuthContext "user" "pass") (BasicAuthData "user" "pass")
        case r of Authorized (AuthUser u) -> u @?= "user"; _ -> assertFailure "expected Authorized",
      testCase "wrong password is unauthorized" $ do
        r <- runCheck (AuthContext "user" "pass") (BasicAuthData "user" "nope")
        case r of Unauthorized -> pure (); _ -> assertFailure "expected Unauthorized",
      testCase "invalid-UTF-8 credential bytes do not crash, return Unauthorized" $ do
        r <- runCheck (AuthContext "user" "pass") (BasicAuthData "\xff\xfe" "\xff")
        case r of Unauthorized -> pure (); _ -> assertFailure "expected Unauthorized"
    ]

apiHardeningTests :: TestTree
apiHardeningTests = testGroup "API Hardening (webapi-lib)" [authTests]
```

  Wire it into `src/test/UnitTests.hs`: add `import UnitTests.ApiHardening (apiHardeningTests)` (alphabetically) and add `apiHardeningTests` to the `testGroup "BJJ Unit Tests" [...]` list.

- [ ] **Step 3: Run the test to verify it fails.** Run: `cabal test 2>&1 | tail -30`. Expected: compile error (`authCheck`'s new shape / `AuthUser` export) or assertion failure — it must not yet pass, because `authCheck` still decodes input.

- [ ] **Step 4: Implement.** Replace `src/lib/webapi-lib/WebAPI/Auth.hs` in full:

```haskell
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Basic authentication middleware for Servant APIs.
-- Credentials come from @BASIC_USER@ and @BASIC_PASS@; the server refuses to
-- start if either is unset (fail-closed). Credential comparison is
-- constant-time and never decodes attacker-controlled bytes.
module WebAPI.Auth where

import Data.ByteArray (constEq)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Servant
import System.Environment (lookupEnv)
import System.Exit (die)

-- | Authenticated user identity extracted from a successful basic-auth check.
newtype AuthUser = AuthUser
  { user :: Text
  }
  deriving (Eq, Show)

-- | Expected credentials used to validate incoming basic-auth requests.
data AuthContext = AuthContext
  { authUser :: Text,
    authPassword :: Text
  }
  deriving (Eq, Show)

proxyBasicAuthContext :: Proxy '[BasicAuthCheck AuthUser]
proxyBasicAuthContext = Proxy

-- | Verify a username/password with constant-time byte comparison.
-- The incoming bytes are never UTF-8 decoded, so invalid UTF-8 credentials
-- fail the check rather than raising an exception.
authCheck :: AuthContext -> BasicAuthCheck AuthUser
authCheck AuthContext {authUser, authPassword} =
  let expectedUser = TE.encodeUtf8 authUser
      expectedPass = TE.encodeUtf8 authPassword
      check (BasicAuthData username password) =
        if constEq username expectedUser && constEq password expectedPass
          then return (Authorized (AuthUser authUser))
          else return Unauthorized
   in BasicAuthCheck check

-- | Build a Servant 'Context' containing the basic-auth check for use with 'serveWithContext'.
basicAuthServerContext :: AuthContext -> Context (BasicAuthCheck AuthUser ': '[])
basicAuthServerContext authContext = authCheck authContext :. EmptyContext

-- | Read basic-auth credentials from @BASIC_USER@ and @BASIC_PASS@.
-- Both must be set and non-empty, or the process exits (fail-closed) — there
-- are no default credentials.
getBasicAuthFromEnv :: IO AuthContext
getBasicAuthFromEnv = do
  mUser <- lookupEnv "BASIC_USER"
  mPass <- lookupEnv "BASIC_PASS"
  case (mUser, mPass) of
    (Just u, Just p)
      | not (null u), not (null p) ->
          return AuthContext {authUser = T.pack u, authPassword = T.pack p}
    _ -> die "BASIC_USER and BASIC_PASS must both be set to non-empty values (no default credentials)."
```

- [ ] **Step 5: Run the test to verify it passes.** Run: `cabal build all 2>&1 | tail -5 && cabal test 2>&1 | tail -20`. Expected: build clean, `authCheck` group passes, full suite green.

- [ ] **Step 6: Commit.**

```bash
git add Decentralized-Belt-System.cabal src/lib/webapi-lib/WebAPI/Auth.hs src/test/UnitTests/ApiHardening.hs src/test/UnitTests.hs
git commit -m "feat(webapi): fail-closed basic-auth + constant-time compare (F-11, F-29)"
```

---

### Task 2: webapi-lib CORS allowlist (F-12)

**Files:**
- Modify: `src/lib/webapi-lib/WebAPI/CORS.hs`
- Modify: `src/test/UnitTests/ApiHardening.hs`

**Interfaces:**
- Consumes: `bytestring` (added to webapi-lib in Task 1).
- Produces: `data CorsConfig = CorsConfig { allowedOrigins :: [ByteString], allowCredentials :: Bool }`; `parseCorsOrigins :: String -> [ByteString]`; `getCorsConfigFromEnv :: IO CorsConfig`; `corsPolicy :: CorsConfig -> Request -> Maybe CorsResourcePolicy`; `mkCorsMiddleware :: CorsConfig -> Middleware`.

- [ ] **Step 1: Write the failing test.** In `src/test/UnitTests/ApiHardening.hs` add imports and a `corsTests` group, and add it to `apiHardeningTests`:

```haskell
import qualified Data.ByteString as BS
import Data.Maybe (isNothing)
import Network.Wai (Request, defaultRequest, requestHeaders)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..))
import Network.HTTP.Types.Header (hOrigin)
import WebAPI.CORS (CorsConfig (..), corsPolicy, parseCorsOrigins)
```

```haskell
reqWithOrigin :: BS.ByteString -> Request
reqWithOrigin o = defaultRequest {requestHeaders = [(hOrigin, o)]}

corsTests :: TestTree
corsTests =
  testGroup
    "CORS allowlist"
    [ testCase "parseCorsOrigins splits, trims, drops empties" $
        parseCorsOrigins " https://a.com , https://b.com ,, "
          @?= ["https://a.com", "https://b.com"],
      testCase "origin in allowlist is reflected" $
        case corsPolicy (CorsConfig ["https://a.com"] False) (reqWithOrigin "https://a.com") of
          Just p -> corsOrigins p @?= Just (["https://a.com"], False)
          Nothing -> assertFailure "expected a policy",
      testCase "origin not in allowlist yields no policy" $
        assertBool "expected Nothing" $
          isNothing (corsPolicy (CorsConfig ["https://a.com"] False) (reqWithOrigin "https://evil.com")),
      testCase "empty allowlist denies all" $
        assertBool "expected Nothing" $
          isNothing (corsPolicy (CorsConfig [] False) (reqWithOrigin "https://a.com"))
    ]
```

  Add `corsTests` to the `apiHardeningTests` list. (`CorsResourcePolicy` has no `Eq` instance, so tests compare the `corsOrigins` field — a `Maybe ([ByteString], Bool)`, which is `Eq`/`Show` — or use `isNothing`, never `@?=` on the whole record.)

- [ ] **Step 2: Run the test to verify it fails.** Run: `cabal test 2>&1 | tail -20`. Expected: compile error — `CorsConfig`/`corsPolicy`/`parseCorsOrigins` not defined.

- [ ] **Step 3: Implement.** Replace `src/lib/webapi-lib/WebAPI/CORS.hs` in full:

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- | CORS middleware driven by an explicit origin allowlist (fail-closed).
-- Only origins present in @CORS_ALLOWED_ORIGINS@ receive CORS headers; every
-- other origin gets none, so the browser blocks the cross-origin response.
module WebAPI.CORS where

import Data.ByteString (ByteString)
import qualified Data.List
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.HTTP.Types as HttpTypes
import Network.HTTP.Types.Header (hOrigin)
import Network.Wai
import Network.Wai.Middleware.Cors
import System.Environment (lookupEnv)

-- | Resolved CORS configuration. 'allowCredentials' is currently always 'False'
-- (frontend is same-origin / proxied / token-based).
data CorsConfig = CorsConfig
  { allowedOrigins :: [ByteString],
    allowCredentials :: Bool
  }
  deriving (Eq, Show)

-- | Parse a comma-separated origin allowlist: trims whitespace, drops empties.
parseCorsOrigins :: String -> [ByteString]
parseCorsOrigins raw =
  [ TE.encodeUtf8 t
  | chunk <- T.splitOn "," (T.pack raw),
    let t = T.strip chunk,
    not (T.null t)
  ]

-- | Read @CORS_ALLOWED_ORIGINS@; unset/empty yields an empty allowlist
-- (no cross-origin allowed). Credentials are disabled.
getCorsConfigFromEnv :: IO CorsConfig
getCorsConfigFromEnv = do
  mOrigins <- lookupEnv "CORS_ALLOWED_ORIGINS"
  return
    CorsConfig
      { allowedOrigins = maybe [] parseCorsOrigins mOrigins,
        allowCredentials = False
      }

-- | Policy: reflect the request 'Origin' only if it is in the allowlist,
-- otherwise emit no CORS headers.
corsPolicy :: CorsConfig -> Request -> Maybe CorsResourcePolicy
corsPolicy cfg req =
  case Data.List.lookup hOrigin (requestHeaders req) of
    Just o
      | o `elem` allowedOrigins cfg ->
          Just
            simpleCorsResourcePolicy
              { corsOrigins = Just ([o], allowCredentials cfg),
                corsMethods = ["GET", "POST", "PUT", "OPTIONS", "DELETE"],
                corsRequestHeaders = simpleHeaders <> [HttpTypes.hAuthorization],
                corsExposedHeaders = Just $ simpleHeaders <> [HttpTypes.hAuthorization],
                corsVaryOrigin = True,
                corsRequireOrigin = False,
                corsIgnoreFailures = False,
                corsMaxAge = Just 600
              }
    _ -> Nothing

-- | WAI middleware applying the allowlist CORS policy.
mkCorsMiddleware :: CorsConfig -> Middleware
mkCorsMiddleware cfg = cors (corsPolicy cfg)
```

  Note: the old zero-arg `setupCors` export is intentionally removed; Task 3 updates all call sites.

- [ ] **Step 4: Run the test to verify it passes.** Run: `cabal test 2>&1 | tail -20`. Expected: `CORS allowlist` group passes. (`cabal build all` will still fail in the four servers that reference `setupCors` — that is Task 3; the **test-suite** and `webapi-lib` build/test pass now.)

  Run `cabal build webapi-lib 2>&1 | tail -5` and `cabal test 2>&1 | tail -20` to confirm both are green in isolation.

- [ ] **Step 5: Commit.**

```bash
git add src/lib/webapi-lib/WebAPI/CORS.hs src/test/UnitTests/ApiHardening.hs
git commit -m "feat(webapi): env-driven CORS allowlist, credentials off (F-12)"
```

---

### Task 3: Thread CorsConfig through all four servers

**Files:**
- Modify: `src/exe/interaction-api/RestAPI.hs`, `src/exe/interaction-api/Main.hs`
- Modify: `src/exe/query-api/RestAPI.hs`, `src/exe/query-api/Main.hs`
- Modify: `src/exe/chain-sync/ChainsyncAPI.hs`, `src/exe/chain-sync/Main.hs`
- Modify: `src/lib/mcp-server-lib/MCPServer/App.hs`, `src/lib/mcp-server-lib/MCPServer/Server.hs`

**Interfaces:**
- Consumes: `mkCorsMiddleware`, `getCorsConfigFromEnv`, `CorsConfig` (Task 2).
- Produces: app builders that take a `CorsConfig`. (Note: Task 7 further widens interaction-api's `mkBJJApp`; keep the `CorsConfig` param first.)

- [ ] **Step 1: interaction-api.** In `src/exe/interaction-api/RestAPI.hs`, change `mkBJJApp` (currently `mkBJJApp :: InteractionAppContext -> Application`, using `WebAPI.CORS.setupCors $ ...`) to take a `CorsConfig`:

```haskell
mkBJJApp :: WebAPI.CORS.CorsConfig -> InteractionAppContext -> Application
mkBJJApp corsCfg ctx =
  WebAPI.CORS.mkCorsMiddleware corsCfg $
    provideOptions proxyPublicAPI $
      serveWithContext proxyFullAPI basicCtx hoistedServer
  where
    basicCtx = basicAuthServerContext (authContext ctx)
    hoistedServer = hoistServerWithContext proxyFullAPI proxyBasicAuthContext (runInteractionAppMonad ctx) fullServer
```

  In `src/exe/interaction-api/Main.hs`, add `import WebAPI.CORS (getCorsConfigFromEnv)`; after `authContext <- getBasicAuthFromEnv` add `corsCfg <- getCorsConfigFromEnv`; change `let bjjDApp = mkBJJApp appContext` to `let bjjDApp = mkBJJApp corsCfg appContext`.

- [ ] **Step 2: query-api.** In `src/exe/query-api/RestAPI.hs`, change `mkBJJApp`:

```haskell
mkBJJApp :: WebAPI.CORS.CorsConfig -> QueryAppContext -> Application
mkBJJApp corsCfg ctx =
  WebAPI.CORS.mkCorsMiddleware corsCfg $
    provideOptions proxyPublicAPI $
      serveWithContext proxyFullAPI basicCtx hoistedServer
  where
    basicCtx = basicAuthServerContext (authContext ctx)
    hoistedServer = hoistServerWithContext proxyFullAPI proxyBasicAuthContext (runAppMonad ctx) fullServer
```

  In `src/exe/query-api/Main.hs`, add `import WebAPI.CORS (getCorsConfigFromEnv)`; after `authContext <- getBasicAuthFromEnv` add `corsCfg <- getCorsConfigFromEnv`; change `let bjjDApp = mkBJJApp appContext` to `let bjjDApp = mkBJJApp corsCfg appContext`.

- [ ] **Step 3: chain-sync.** In `src/exe/chain-sync/ChainsyncAPI.hs`, change `mkServiceProbeApp`:

```haskell
mkServiceProbeApp :: WebAPI.CORS.CorsConfig -> MVar SyncMetrics -> Application
mkServiceProbeApp corsCfg metricsVar =
  WebAPI.CORS.mkCorsMiddleware corsCfg $
    serve proxyServiceProbeAPI (serviceProbeServer metricsVar)
```

  Find the caller of `mkServiceProbeApp`/`startProbeServer` (in `ChainsyncAPI.hs` — `startProbeServer`) and thread `CorsConfig`. If `startProbeServer :: Int -> MVar SyncMetrics -> IO ()`, change it to `startProbeServer :: WebAPI.CORS.CorsConfig -> Int -> MVar SyncMetrics -> IO ()` and pass `corsCfg` to `mkServiceProbeApp`. In `src/exe/chain-sync/Main.hs`, add `import WebAPI.CORS (getCorsConfigFromEnv)` (or `WebAPI.CORS` is already imported qualified — add the symbol), read `corsCfg <- getCorsConfigFromEnv` near the other env reads, and change `void $ forkIO $ startProbeServer port metricsVar` to `void $ forkIO $ startProbeServer corsCfg port metricsVar`.

- [ ] **Step 4: mcp-server.** In `src/lib/mcp-server-lib/MCPServer/App.hs`, add `corsConfig :: CorsConfig` to `AppCtx` (import `WebAPI.CORS (CorsConfig, getCorsConfigFromEnv)`), and in `withAppCtx` add `corsCfg <- getCorsConfigFromEnv` and set `corsConfig = corsCfg` in the record. In `src/lib/mcp-server-lib/MCPServer/Server.hs`, change `app = setupCors (dispatchByPrefix mcpApp probeApp)` to `app = mkCorsMiddleware (corsConfig ctx) (dispatchByPrefix mcpApp probeApp)` and update the import from `WebAPI.CORS (setupCors)` to `WebAPI.CORS (mkCorsMiddleware)`.

- [ ] **Step 5: Build.** Run: `cabal build all 2>&1 | tail -15`. Expected: clean build; no remaining reference to `setupCors`. Verify with `grep -rn "setupCors" src` → no hits.

- [ ] **Step 6: Run tests.** Run: `cabal test 2>&1 | tail -10`. Expected: full suite green.

- [ ] **Step 7: Commit.**

```bash
git add src/exe/interaction-api/RestAPI.hs src/exe/interaction-api/Main.hs src/exe/query-api/RestAPI.hs src/exe/query-api/Main.hs src/exe/chain-sync/ChainsyncAPI.hs src/exe/chain-sync/Main.hs src/lib/mcp-server-lib/MCPServer/App.hs src/lib/mcp-server-lib/MCPServer/Server.hs
git commit -m "feat(servers): thread CORS allowlist config through all four servers (F-12)"
```

---

### Task 4: webapi-lib pure helpers — LIKE escape, image magic-bytes, error mapping (supports F-14, F-15, F-16, F-27, F-30)

**Files:**
- Modify: `Decentralized-Belt-System.cabal` (webapi-lib exposed-modules)
- Modify: `src/lib/webapi-lib/WebAPI/Utils.hs`
- Create: `src/lib/webapi-lib/WebAPI/ImageType.hs`
- Create: `src/lib/webapi-lib/WebAPI/Errors.hs`
- Modify: `src/test/UnitTests/ApiHardening.hs`

**Interfaces:**
- Produces:
  - `escapeLikePattern :: Text -> Text` (in `WebAPI.Utils`) — escapes `\`, `%`, `_`.
  - `data ImageType = JPEG | PNG` + `detectImageType :: ByteString -> Maybe ImageType` (in `WebAPI.ImageType`).
  - `mkServantErr :: Int -> String -> ServerError`, `genericErrorMessage :: Int -> String` (in `WebAPI.Errors`).

- [ ] **Step 1: Write the failing tests.** In `src/test/UnitTests/ApiHardening.hs` add imports and two groups, add both to `apiHardeningTests`:

```haskell
import qualified Data.Text as T
import WebAPI.ImageType (ImageType (..), detectImageType)
import WebAPI.Utils (escapeLikePattern)
```

```haskell
escapeTests :: TestTree
escapeTests =
  testGroup
    "escapeLikePattern"
    [ testCase "plain text is unchanged" $ escapeLikePattern "hello" @?= "hello",
      testCase "percent is escaped" $ escapeLikePattern "50%" @?= "50\\%",
      testCase "underscore is escaped" $ escapeLikePattern "a_b" @?= "a\\_b",
      testCase "backslash is escaped first" $ escapeLikePattern "a\\b" @?= "a\\\\b",
      testCase "combined" $ escapeLikePattern "\\%_" @?= "\\\\\\%\\_"
    ]

imageTests :: TestTree
imageTests =
  testGroup
    "detectImageType"
    [ testCase "JPEG magic bytes" $ detectImageType (BS.pack [0xFF, 0xD8, 0xFF, 0xE0, 0x00]) @?= Just JPEG,
      testCase "PNG magic bytes" $ detectImageType (BS.pack [0x89, 0x50, 0x4E, 0x47, 0x0D]) @?= Just PNG,
      testCase "other bytes rejected" $ detectImageType (BS.pack [0x00, 0x01, 0x02, 0x03]) @?= Nothing,
      testCase "too-short input rejected" $ detectImageType (BS.pack [0xFF, 0xD8]) @?= Nothing
    ]
```

  Add `escapeTests` and `imageTests` to `apiHardeningTests`.

- [ ] **Step 2: Run to verify failure.** Run: `cabal test 2>&1 | tail -20`. Expected: compile error — modules/functions not defined.

- [ ] **Step 3: Implement `escapeLikePattern`.** In `src/lib/webapi-lib/WebAPI/Utils.hs`, add to the export list `escapeLikePattern`, add `import Data.Text (Text)` + `import qualified Data.Text as T`, and add:

```haskell
-- | Escape SQL @LIKE@ metacharacters in user-supplied search text so that
-- @%@ and @_@ match literally. Backslash is escaped first (it is the escape
-- character). Relies on PostgreSQL's default @LIKE@ escape character (@\\@);
-- esqueleto's @like@ emits no explicit @ESCAPE@ clause.
escapeLikePattern :: Text -> Text
escapeLikePattern =
  T.replace "_" "\\_"
    . T.replace "%" "\\%"
    . T.replace "\\" "\\\\"
```

- [ ] **Step 4: Implement `WebAPI.ImageType`.** Create `src/lib/webapi-lib/WebAPI/ImageType.hs`:

```haskell
-- | Image type detection by magic bytes, for validating uploads before they
-- are forwarded to storage.
module WebAPI.ImageType
  ( ImageType (..),
    detectImageType,
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- | Supported upload image formats.
data ImageType = JPEG | PNG
  deriving (Eq, Show)

-- | Detect an image type from a byte prefix, or 'Nothing' if unrecognized.
-- JPEG starts with @FF D8 FF@; PNG with @89 50 4E 47@.
detectImageType :: ByteString -> Maybe ImageType
detectImageType bs
  | jpegMagic `BS.isPrefixOf` bs = Just JPEG
  | pngMagic `BS.isPrefixOf` bs = Just PNG
  | otherwise = Nothing
  where
    jpegMagic = BS.pack [0xFF, 0xD8, 0xFF]
    pngMagic = BS.pack [0x89, 0x50, 0x4E, 0x47]
```

- [ ] **Step 5: Implement `WebAPI.Errors`.** Create `src/lib/webapi-lib/WebAPI/Errors.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- | Shared HTTP error construction and client-facing sanitization for the API
-- servers. Full internal detail is logged by callers; clients receive only the
-- generic messages produced here.
module WebAPI.Errors
  ( mkServantErr,
    genericErrorMessage,
  ) where

import qualified Data.ByteString.Lazy.Char8 as BL8
import Servant

-- | Build a 'ServerError' for an HTTP status code with the given body.
-- Unlike a naive mapping, non-4xx statuses are not collapsed to 400: a 5xx
-- stays a 5xx (fixes probe failures being reported as 400).
mkServantErr :: Int -> String -> ServerError
mkServantErr status msg = base {errBody = BL8.pack msg}
  where
    base = case status of
      400 -> err400
      404 -> err404
      500 -> err500
      502 -> err502
      503 -> err503
      _ -> err500

-- | Generic, non-sensitive client-facing message for an HTTP status code.
genericErrorMessage :: Int -> String
genericErrorMessage status = case status of
  400 -> "Bad request"
  404 -> "Not found"
  502 -> "Upstream service error"
  503 -> "Service temporarily unavailable"
  _ -> "Internal server error"
```

  In `Decentralized-Belt-System.cabal`, `library webapi-lib` `exposed-modules` (currently `WebAPI.Auth`, `WebAPI.CORS`, `WebAPI.ServiceProbe`, `WebAPI.Utils`) — add `WebAPI.Errors` and `WebAPI.ImageType`.

- [ ] **Step 6: Run tests.** Run: `cabal build webapi-lib 2>&1 | tail -5 && cabal test 2>&1 | tail -20`. Expected: `escapeLikePattern` and `detectImageType` groups pass; suite green.

- [ ] **Step 7: Commit.**

```bash
git add Decentralized-Belt-System.cabal src/lib/webapi-lib/WebAPI/Utils.hs src/lib/webapi-lib/WebAPI/ImageType.hs src/lib/webapi-lib/WebAPI/Errors.hs src/test/UnitTests/ApiHardening.hs
git commit -m "feat(webapi): LIKE-escape, image magic-byte, error-mapping helpers (F-14/15/16/27/30)"
```

---

### Task 5: query-api error mapping + sanitization (F-14, F-27, F-30)

**Files:**
- Modify: `src/exe/query-api/QueryAppMonad.hs`
- Modify: `src/exe/query-api/RestAPI.hs`, `src/exe/query-api/Query/ServiceStatus.hs`, `src/exe/query-api/Query/Aggregates.hs`, `src/exe/query-api/Query/Projected.hs`
- Modify: `src/exe/interaction-api/InteractionAppMonad.hs`

**Interfaces:**
- Consumes: `mkServantErr`, `genericErrorMessage` (Task 4); `txBuildingExceptionToHttpStatus`, `TxBuildingException`, `displayException`.
- Produces: `runWithQueryErrorHandling :: IO a -> QueryAppMonad a`.
- Note: interaction-api's IPFS error body (`uploadOrThrow`) is sanitized in **Task 7** (which rewrites that function anyway), not here — avoids a double-edit of `ServiceHandlers.hs`.

- [ ] **Step 1: Add `runWithQueryErrorHandling`.** In `src/exe/query-api/QueryAppMonad.hs`, add imports:

```haskell
import Control.Exception (displayException, try)
import Control.Monad.Except (throwError)
import TxBuilding.Exceptions (TxBuildingException, txBuildingExceptionToHttpStatus)
import WebAPI.Errors (genericErrorMessage, mkServantErr)
```

  (Remove the now-unused `SomeException`/`try` import line if it becomes redundant after Step 2; keep `try` — it is reused.) Add:

```haskell
-- | Run an IO action, mapping a bare 'TxBuildingException' to its HTTP status.
-- Query handlers throw @TxBuildingException@ directly (no @GYApplicationException@
-- wrapper), so this catches the concrete type — it must NOT copy the
-- interaction-api handler's @GYApplicationException@/@cast@ shape.
runWithQueryErrorHandling :: IO a -> QueryAppMonad a
runWithQueryErrorHandling action = QueryAppMonad $ do
  res <- liftIO $ try action
  case res of
    Left (txEx :: TxBuildingException) -> do
      let status = txBuildingExceptionToHttpStatus txEx
      liftIO $ putStrLn $ "TxBuildingException (" <> show status <> "): " <> displayException txEx
      throwError $ mkServantErr status (displayException txEx)
    Right ok -> pure ok
```

  (`ScopedTypeVariables` is needed for the `txEx :: TxBuildingException` annotation — add `{-# LANGUAGE ScopedTypeVariables #-}` to the pragma block if absent.)

- [ ] **Step 2: Route throwing handlers through it.** Each query handler that runs a `runSqlPool`/`runQuery` IO block ending in `throwIO SomeTxBuildingException` must have that IO wrapped by `runWithQueryErrorHandling` at the `QueryAppMonad` boundary. Concretely, for each handler in `RestAPI.hs:292`, `Query/ServiceStatus.hs:38`, `Query/Aggregates.hs:98,103,131,205,210`, `Query/Projected.hs:323,332,435,1137`: wrap the `liftIO $ runSqlPool (...) pool` (or `liftIO $ ...`) expression that may throw with `runWithQueryErrorHandling $ runSqlPool (...) pool`. The `throwIO` sites themselves stay unchanged (they throw bare `TxBuildingException`). Confirm the wrap is at the outermost IO of each affected handler so the exception is caught before Servant.

  If a handler currently returns `QueryAppMonad a` by other combinators (e.g. `asks pgPool >>= \pool -> liftIO (runSqlPool ...)`), replace the `liftIO` with `runWithQueryErrorHandling`.

- [ ] **Step 3: Sanitize the DB-probe leak.** In `QueryAppMonad.hs` `verifyProjectionDbConnection`, the `Left err` branch currently embeds `show err` (libpq detail, may contain host/user). Change it to log full detail and return a generic body:

```haskell
    Left err -> do
      liftIO $ putStrLn $ "Projection DB not ready: " <> show err
      throwError $ mkServantErr 503 (genericErrorMessage 503)
```

- [ ] **Step 4: Fix interaction-api `mkServantErr` + probe (F-30, F-27).** In `src/exe/interaction-api/InteractionAppMonad.hs`:
  - Remove the local `mkServantErr` definition (lines 66-70) and import the shared one: `import WebAPI.Errors (genericErrorMessage, mkServantErr)`.
  - In `runWithTxErrorHandling`, keep the `GYApplicationException`/`cast` path (interaction-api genuinely wraps in `GYApplicationException` — do not change it). In its `_ ->` fallback (currently `throwError err400 {errBody = BL8.pack (show ex)}`), log full and return generic 500:

```haskell
        _ -> do
          liftIO $ putStrLn $ "Unexpected exception: " <> show ex
          throwError $ mkServantErr 500 (genericErrorMessage 500)
```

  - In `checkDeployedScriptsAreReady`, apply the same fallback change, but return **503** for the non-`TxBuildingException` case (it is a readiness probe): `throwError $ mkServantErr 503 (genericErrorMessage 503)`. The `TxBuildingException` branch keeps `mkServantErr status (displayException txEx)`.

- [ ] **Step 5: Build + reasoning verification.** Run: `cabal build all 2>&1 | tail -15`. Expected: clean build (the `try @TxBuildingException`-typed catch compiles). Manually confirm: (a) no query handler still returns raw `show`/libpq/IPFS detail to the client; (b) `grep -rn "displayException\|show ex\|show err" src/exe/query-api src/exe/interaction-api` shows the only client-facing strings are curated `displayException txEx` (safe) and generic messages. **Note (documented limitation):** the `ProfileNotFound → 404` behavior is verified by build (the concrete-typed catch compiles) + the existing `txBuildingExceptionToHttpStatus ProfileNotFound = 404` mapping; a full HTTP integration test is deferred (no HTTP test harness exists, consistent with the stream-B apply-side deferral).

- [ ] **Step 6: Run tests.** Run: `cabal test 2>&1 | tail -10`. Expected: suite green (no behavior tested here regresses).

- [ ] **Step 7: Commit.**

```bash
git add src/exe/query-api/QueryAppMonad.hs src/exe/query-api/RestAPI.hs src/exe/query-api/Query/ServiceStatus.hs src/exe/query-api/Query/Aggregates.hs src/exe/query-api/Query/Projected.hs src/exe/interaction-api/InteractionAppMonad.hs
git commit -m "fix(api): query error mapping + sanitized error bodies + probe 503 (F-14/27/30)"
```

---

### Task 6: query-api limits + search hardening + LIKE escaping (F-15)

**Files:**
- Modify: `src/exe/query-api/Query/Common.hs`
- Modify: `src/exe/query-api/Query/Projected.hs`

**Interfaces:**
- Consumes: `escapeLikePattern` (Task 4).

- [ ] **Step 1: Clamp `normalizeLimitOffset`.** In `src/exe/query-api/Query/Common.hs`, replace `normalizeLimitOffset` so that both-absent defaults to a bounded page and the limit is clamped to `[1, 500]`, offset `>= 0`:

```haskell
-- | Normalize optional limit/offset. Both absent → a bounded default page
-- (100, 0) rather than unbounded. Limit is clamped to [1, 500]; offset to >= 0.
normalizeLimitOffset :: Maybe Int -> Maybe Int -> Maybe (Int, Int)
normalizeLimitOffset limit offset =
  Just (clampLimit (fromMaybe 100 limit), max 0 (fromMaybe 0 offset))
  where
    clampLimit l = max 1 (min 500 l)
```

  Add `import Data.Maybe (fromMaybe)` if not already imported. (`normalizeLimitOffset` now always returns `Just`; downstream `applyLimits`/`applyLimitOffset` already handle `Just`. The `Nothing` case is gone — unbounded queries are no longer reachable via this path.)

- [ ] **Step 2: Escape LIKE at the chokepoint.** In `src/exe/query-api/Query/Projected.hs`, change `likePat` (line 84-85) to escape metacharacters via the shared helper, so every `q`-filtered endpoint is covered:

```haskell
-- | Build a case-insensitive SQL LIKE pattern for text search: @%<escaped lower q>%@.
-- Metacharacters in @q@ are escaped so a user-supplied @%@/@_@ matches literally.
likePat :: Text -> SqlExpr (Value Text)
likePat q = val (T.pack "%" <> escapeLikePattern (T.toLower q) <> T.pack "%")
```

  Add `import WebAPI.Utils (escapeLikePattern)` to `Query/Projected.hs`.

- [ ] **Step 3: Reject blank search in `/search`.** In `searchProjected` (line 1082-1088), short-circuit a blank query to an empty result **before** running the five sub-queries (a blank `q` produces the `%%` pattern that matches every row). Returning empty `SearchResults` (rather than throwing) avoids adding a new exception constructor and matches "no query → nothing to show":

```haskell
searchProjected :: (MonadIO m, MonadReader QueryAppContext m) => Text -> m SearchResults
searchProjected q
  | T.null (T.strip q) =
      pure
        SearchResults
          { searchResultsQuery = q,
            searchResultsTotal = 0,
            searchResultsPractitioners = SearchGroup 0 [],
            searchResultsOrganizations = SearchGroup 0 [],
            searchResultsPromotions = SearchGroup 0 [],
            searchResultsAchievements = SearchGroup 0 []
          }
  | otherwise = do
      practitionerProfiles <- getProfiles Nothing (Just (profileSearchFilter Practitioner q)) Nothing
      ... (rest unchanged)
```

  Add `import qualified Data.Text as T` if not present (Projected.hs already imports `Data.Text qualified as T` — verify). This avoids five unbounded empty-pattern queries (`%%`) that would match every row.

  **Note on `COUNT(*)`:** the spec suggested `COUNT(*)` for `/search` totals. `searchProjected` currently derives group totals from already-materialized lists (`length items`) which, after Step 1's default page cap on the underlying `getProfiles`/`getAchievements`, are bounded — so no unbounded materialization remains for the practitioner/organization/achievement groups. The rank/promotion search paths (`searchRanksWithNames`, `searchPromotionsWithNames`) return full lists; leave their shape unchanged in this task (they are bounded by the escaped pattern and the DB), and record `COUNT(*)`-based totals as a **follow-up** in the plan's completion notes rather than expanding scope here. (Blank-query short-circuit is the material DoS fix; the `length`-based totals over a specific non-empty search are not an unbounded-input vector.)

- [ ] **Step 4: Build + reasoning.** Run: `cabal build all 2>&1 | tail -10`. Expected: clean. Manually confirm every `likePat` caller (grep `likePat` in `Query/Projected.hs`: `applyPromotionFilter`, `applyPromotionFilterOnRank`, `applyAchievementFilter`, `applyMembershipHistoryFilter`, `applyMembershipIntervalFilter`, `applyProfileFilter`, `searchRanksWithNames`, `searchPromotionsWithNames`) now routes user text through `escapeLikePattern` (they all call `likePat`, so the single change covers them).

- [ ] **Step 5: Run tests.** Run: `cabal test 2>&1 | tail -10`. Expected: green. (`escapeLikePattern` itself is unit-tested in Task 4.)

- [ ] **Step 6: Commit.**

```bash
git add src/exe/query-api/Query/Common.hs src/exe/query-api/Query/Projected.hs
git commit -m "fix(query): bounded pagination default + LIKE escaping + blank-search guard (F-15)"
```

---

### Task 7: interaction-api multipart size cap + image validation (F-16)

**Files:**
- Modify: `Decentralized-Belt-System.cabal` (interaction-api deps)
- Modify: `src/exe/interaction-api/RestAPI.hs`
- Modify: `src/exe/interaction-api/ServiceHandlers.hs`

**Interfaces:**
- Consumes: `detectImageType`, `ImageType` (Task 4).

- [ ] **Step 1: Add `wai-extra` dep.** In `Decentralized-Belt-System.cabal`, `executable interaction-api` build-depends — add `, wai-extra` (needed for `Network.Wai.Parse`; it is transitively present via `servant-multipart` but must be listed to import directly).

- [ ] **Step 2: Widen the Servant context with `MultipartOptions Mem`.** In `src/exe/interaction-api/RestAPI.hs`:
  - Extend imports: `import Servant.Multipart (Mem, MultipartForm, MultipartOptions (..), defaultMultipartOptions)` and `import Network.Wai.Parse (setMaxRequestFileSize)`.
  - Add the options value and a widened context proxy:

```haskell
-- | Cap in-RAM multipart uploads at 10 MB so a large body cannot exhaust memory.
multipartOptions :: MultipartOptions Mem
multipartOptions =
  base {generalOptions = setMaxRequestFileSize (10 * 1024 * 1024) (generalOptions base)}
  where
    base = defaultMultipartOptions (Proxy :: Proxy Mem)

proxyServerContext :: Proxy '[BasicAuthCheck AuthUser, MultipartOptions Mem]
proxyServerContext = Proxy
```

  - Update `mkBJJApp` (already `CorsConfig`-parameterized from Task 3) to serve with the widened context:

```haskell
mkBJJApp :: WebAPI.CORS.CorsConfig -> InteractionAppContext -> Application
mkBJJApp corsCfg ctx =
  WebAPI.CORS.mkCorsMiddleware corsCfg $
    provideOptions proxyPublicAPI $
      serveWithContext proxyFullAPI ctxEntries hoistedServer
  where
    ctxEntries = authCheck (authContext ctx) :. multipartOptions :. EmptyContext
    hoistedServer = hoistServerWithContext proxyFullAPI proxyServerContext (runInteractionAppMonad ctx) fullServer
```

  (`authCheck` is imported from `WebAPI.Auth` via the existing `import WebAPI.Auth`. The old `basicCtx`/`proxyBasicAuthContext` are no longer used in interaction-api's `mkBJJApp`; `proxyBasicAuthContext` may still be imported — leave the import, it is harmless, or drop it if `-Wunused` flags it.)

- [ ] **Step 3: Validate image bytes + sanitize IPFS error before upload.** In `src/exe/interaction-api/ServiceHandlers.hs`, add imports `import qualified Data.ByteString.Lazy as LBS` (present) and `import WebAPI.ImageType (detectImageType)`, `import Data.Maybe (isNothing)`, and `import WebAPI.Errors (genericErrorMessage, mkServantErr)`. Rewrite `uploadOrThrow` to reject non-image payloads before `uploadToIPFS` and to return a sanitized (generic) 502 on IPFS failure — logging the full detail server-side (this is the F-27 IPFS sanitization; it is done here, not in Task 5):

```haskell
uploadOrThrow :: IPFSConfig -> LBS.ByteString -> InteractionAppMonad Text
uploadOrThrow cfg bytes = do
  if isNothing (detectImageType (LBS.toStrict (LBS.take 16 bytes)))
    then InteractionAppMonad $ throwError $ mkServantErr 400 "Unsupported image type (expected JPEG or PNG)"
    else do
      result <- liftIO $ uploadToIPFS cfg bytes
      case result of
        Left err -> do
          liftIO $ putStrLn $ "IPFS upload failed: " <> unpack err
          InteractionAppMonad $ throwError $ mkServantErr 502 (genericErrorMessage 502)
        Right uri -> return uri
```

  (`LBS.take 16` reads only the prefix for magic-byte detection; the full `bytes` are still uploaded on success.)

- [ ] **Step 4: Build.** Run: `cabal build all 2>&1 | tail -15`. Expected: clean. The context-list widening must typecheck (`serveWithContext`/`hoistServerWithContext` proxies match `'[BasicAuthCheck AuthUser, MultipartOptions Mem]`).

- [ ] **Step 5: Run tests.** Run: `cabal test 2>&1 | tail -10`. Expected: green (`detectImageType` unit-tested in Task 4).

- [ ] **Step 6: Commit.**

```bash
git add Decentralized-Belt-System.cabal src/exe/interaction-api/RestAPI.hs src/exe/interaction-api/ServiceHandlers.hs
git commit -m "fix(interaction): 10MB multipart cap + JPEG/PNG magic-byte validation (F-16)"
```

---

### Task 8: log hygiene + Kupo default (F-13, F-32)

**Files:**
- Modify: `src/exe/query-api/Main.hs`
- Modify: `src/exe/chain-sync/Main.hs`

- [ ] **Step 1: Redact query-api DSN log.** In `src/exe/query-api/Main.hs`, replace `putStrLn $ "Postgres DSN: " <> connStr` (line 50) with a host-only log. Add a small pure redactor near the top of the module:

```haskell
-- | Show only the host and dbname fields of a Postgres DSN, hiding the password.
redactConnStr :: String -> String
redactConnStr s =
  unwords [kv | kv <- words s, let k = takeWhile (/= '=') kv, k `elem` ["host", "dbname", "port"]]
```

  and change the log line to `putStrLn $ "Postgres: " <> redactConnStr connStr`.

- [ ] **Step 2: Redact chain-sync DSN + Kupo URL logs; localhost default.** In `src/exe/chain-sync/Main.hs`:
  - Change `defaultKupoUrl` (line 48-49) to `defaultKupoUrl = "http://localhost:1442"`.
  - Add the same `redactConnStr` helper (or a shared one — if you prefer, place it in `WebAPI.Utils` and import in both; for minimal surface, duplicate the 3-line helper in each `Main`).
  - Replace `putStrLn ("Postgres DSN: " <> connStr)` (line 109) with `putStrLn ("Postgres: " <> redactConnStr connStr)`.
  - Replace `putStrLn ("Base URL: " <> kupoUrl)` (line 107) with a redacted form that hides the host: `putStrLn "Kupo: configured"` (the URL host is the Demeter credential; do not log it). Keep the `Pattern:` log.

- [ ] **Step 3: Build.** Run: `cabal build all 2>&1 | tail -8`. Expected: clean.

- [ ] **Step 4: Reasoning check.** `grep -rn "connStr\|kupoUrl" src/exe/query-api/Main.hs src/exe/chain-sync/Main.hs` — confirm no `putStrLn` emits the raw DSN or raw Kupo URL. (Runtime env-var reads of the real values are unchanged; only the logs are redacted.)

- [ ] **Step 5: Run tests.** Run: `cabal test 2>&1 | tail -8`. Expected: green.

- [ ] **Step 6: Commit.**

```bash
git add src/exe/query-api/Main.hs src/exe/chain-sync/Main.hs
git commit -m "fix(servers): redact DSN/Kupo logs; default Kupo to localhost (F-13, F-32)"
```

---

### Task 9: deploy tooling — this repo (docker-compose, .env.example, README §6.4)

**Files:**
- Modify: `docker-compose.yml`
- Create: `.env.example`
- Modify: `README.md`

- [ ] **Step 1: docker-compose creds + CORS.** In `docker-compose.yml`, for `interaction-api`, `query-api`, and `mcp-server` services, replace the commented `# - BASIC_USER=cardano` / `# - BASIC_PASS=lovelace` lines with references to env (fail-closed — the dev stack must supply them):

```yaml
      - BASIC_USER=${BASIC_USER}
      - BASIC_PASS=${BASIC_PASS}
```

  For `interaction-api` and `query-api`, also add:

```yaml
      - CORS_ALLOWED_ORIGINS=${CORS_ALLOWED_ORIGINS}
```

  (mcp-server does not need CORS_ALLOWED_ORIGINS for browser use, but add it for consistency since its `/mcp` may be probed cross-origin; include it.)

- [ ] **Step 2: Create `.env.example`.** Create `.env.example` at repo root:

```
# Basic-auth credentials shared by interaction-api, query-api, and mcp-server
# upstream client. REQUIRED — servers refuse to start (die) if unset. No default.
BASIC_USER=changeme
BASIC_PASS=changeme

# Comma-separated CORS origin allowlist for the HTTP servers. Empty = no
# cross-origin allowed (fail-closed). Example: https://app.example.com
CORS_ALLOWED_ORIGINS=

# Kupo indexer URL (default in source is http://localhost:1442). Set for remote.
KUPO_URL=http://localhost:1442

# Atlas + deployed-validator configs (JSON or file paths).
ATLAS_CORE_CONFIG=
DEPLOYED_VALIDATORS_CONFIG=
```

- [ ] **Step 3: Update README §6.4.** In `README.md`:
  - Line ~305: change "Defaults: `BASIC_USER=cardano`, `BASIC_PASS=lovelace` (override via env)." to "`BASIC_USER`/`BASIC_PASS` are **required** — the server exits at startup if either is unset. No default credentials."
  - Lines ~355-358 (per-service env lists): for Interaction API and Query API, add `CORS_ALLOWED_ORIGINS` (comma-separated origin allowlist; empty = no cross-origin) and mark `BASIC_USER`/`BASIC_PASS` as required. For MCP Server, note `BASIC_USER`/`BASIC_PASS` are required (die if unset).

- [ ] **Step 4: Verify.** Run: `grep -n "cardano\|lovelace" README.md docker-compose.yml` — expected: no remaining "cardano/lovelace default" wording in these files (the words may remain only in historical/context prose if any; the credential-default claim must be gone).

- [ ] **Step 5: Commit.**

```bash
git add docker-compose.yml .env.example README.md
git commit -m "docs(deploy): require BASIC_USER/PASS + CORS_ALLOWED_ORIGINS; .env.example (F-11/F-12)"
```

---

### Task 10: deploy tooling — bjj-frontend repo (coordinated, separate commit)

**Files (in the separate `bjj-frontend` repo at `/Users/mg/Projects/CardanoProjects/bjj-frontend`):**
- Modify: `docker-compose.yml`
- Modify: `.env.example`

**This is a separate git repository.** Do all git operations with `git -C /Users/mg/Projects/CardanoProjects/bjj-frontend`. Do not stage these files into the Decentralized-Belt-System branch.

- [ ] **Step 1: Set explicit creds + CORS on the three server services.** In `bjj-frontend/docker-compose.yml`, for `interaction-api` (env block ~77-81), `query-api` (~103-107), and `mcp-server` (~127-131), add:

```yaml
      - BASIC_USER=${BASIC_USER}
      - BASIC_PASS=${BASIC_PASS}
```

  and for `interaction-api` and `query-api` also add `- CORS_ALLOWED_ORIGINS=${CORS_ALLOWED_ORIGINS}`. (No `:-cardano` shell default — a missing var must fail, not silently use stale creds.)

- [ ] **Step 2: Drop the stale shell defaults on the downstream clients.** In `bjj-frontend/docker-compose.yml`:
  - `agent-service` (lines ~176-177): change `${BASIC_USER:-cardano}` → `${BASIC_USER}` and `${BASIC_PASS:-lovelace}` → `${BASIC_PASS}`.
  - `bjj-frontend` BFF (lines ~204-208): change all four `${BASIC_USER:-cardano}` / `${BASIC_PASS:-lovelace}` occurrences to `${BASIC_USER}` / `${BASIC_PASS}`.

- [ ] **Step 3: Update `.env.example`.** In `bjj-frontend/.env.example` (lines ~47-49), change the "default cardano/lovelace" comment and values to required placeholders, and add CORS:

```
# Basic auth shared by interaction-api, query-api, mcp-server, and the BFF/agent
# upstream clients. REQUIRED on all — the Haskell servers die at startup if unset,
# and the BFF/agent will otherwise send rejected credentials. No default.
BASIC_USER=changeme
BASIC_PASS=changeme

# CORS origin allowlist for the protocol HTTP servers (comma-separated).
# Set to the deployed frontend origin(s). Empty = no cross-origin allowed.
CORS_ALLOWED_ORIGINS=
```

- [ ] **Step 4: Verify.** Run: `grep -n "cardano\|lovelace" /Users/mg/Projects/CardanoProjects/bjj-frontend/docker-compose.yml /Users/mg/Projects/CardanoProjects/bjj-frontend/.env.example` — expected: no `:-cardano`/`:-lovelace` shell defaults and no cardano/lovelace credential values remain.

- [ ] **Step 5: Commit in the bjj-frontend repo.**

```bash
git -C /Users/mg/Projects/CardanoProjects/bjj-frontend add docker-compose.yml .env.example
git -C /Users/mg/Projects/CardanoProjects/bjj-frontend commit -m "chore(deploy): require explicit BASIC_USER/PASS + CORS; drop cardano/lovelace fallbacks

Coordinated with Decentralized-Belt-System API hardening (fail-closed auth).
Protocol servers now die without credentials; BFF/agent-service must not send
stale defaults."
```

  (Do not push. Report that this commit lives in the separate repo and must land together with the protocol change at deploy time.)

---

## Completion notes / follow-ups (not in scope, record in final review)

- `/search` group totals use `length` over (now page-bounded) lists; a `COUNT(*)`-based total for `searchRanksWithNames`/`searchPromotionsWithNames` is a possible future refinement (Task 6 Step 3 note) — not an unbounded-input vector once the blank-query guard is in place.
- `ProfileNotFound → 404` end-to-end HTTP behavior is verified by build + mapping test, not an HTTP integration test (no harness exists).
- This repo's `docker-compose.yml` still carries the stream-B-obsolete `BATCH_SIZE` env on chainsync (now ignored in favor of `ROLLBACK_MARGIN`); out of scope here.
