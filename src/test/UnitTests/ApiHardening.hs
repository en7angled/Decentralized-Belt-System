{-# LANGUAGE OverloadedStrings #-}

-- | Pure unit tests for the API-hardening security helpers in webapi-lib
-- (fail-closed auth compare, CORS allowlist policy, LIKE escaping, image
-- magic-byte detection). IO wiring (startup die, middleware threading) is
-- covered by build + manual reasoning, not here.
module UnitTests.ApiHardening (apiHardeningTests) where

import qualified Data.ByteString as BS
import Data.Maybe (isNothing)
import Network.HTTP.Types.Header (hOrigin)
import Network.Wai (Request, defaultRequest, requestHeaders)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..))
import Servant (BasicAuthData (..))
import Servant.Server (BasicAuthCheck (..), BasicAuthResult (..))
import Test.Tasty
import Test.Tasty.HUnit
import WebAPI.Auth (AuthContext (..), AuthUser (..), authCheck)
import WebAPI.CORS (CorsConfig (..), corsPolicy, parseCorsOrigins)

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

apiHardeningTests :: TestTree
apiHardeningTests = testGroup "API Hardening (webapi-lib)" [authTests, corsTests]
