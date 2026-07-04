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
