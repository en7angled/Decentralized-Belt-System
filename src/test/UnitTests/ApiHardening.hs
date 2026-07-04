{-# LANGUAGE OverloadedStrings #-}

-- | Pure unit tests for the API-hardening security helpers in webapi-lib
-- (fail-closed auth compare, CORS allowlist policy, LIKE escaping, image
-- magic-byte detection). IO wiring (startup die, middleware threading) is
-- covered by build + manual reasoning, not here.
module UnitTests.ApiHardening (apiHardeningTests) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Maybe (isNothing)
import Network.HTTP.Types.Header (hOrigin)
import Network.Wai (Request, defaultRequest, requestHeaders)
import Network.Wai.Middleware.Cors (CorsResourcePolicy (..))
import Servant (BasicAuthData (..), errHTTPCode)
import Servant.Server (BasicAuthCheck (..), BasicAuthResult (..))
import Test.Tasty
import Test.Tasty.HUnit
import WebAPI.Auth (AuthContext (..), AuthUser (..), authCheck)
import WebAPI.CORS (CorsConfig (..), corsPolicy, parseCorsOrigins)
import WebAPI.Errors (genericErrorMessage, mkServantErr)
import WebAPI.ImageType (ImageType (..), detectImageType)
import WebAPI.Utils (escapeLikePattern)

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

errorTests :: TestTree
errorTests =
  testGroup
    "mkServantErr / genericErrorMessage"
    [ testCase "503 stays 503" $ errHTTPCode (mkServantErr 503 "x") @?= 503,
      testCase "502 stays 502" $ errHTTPCode (mkServantErr 502 "x") @?= 502,
      testCase "500 stays 500" $ errHTTPCode (mkServantErr 500 "x") @?= 500,
      testCase "404 stays 404" $ errHTTPCode (mkServantErr 404 "x") @?= 404,
      -- Unknown status must NOT collapse to 400 — pins the F-30 invariant.
      testCase "unknown status maps to 500, not 400" $ errHTTPCode (mkServantErr 418 "x") @?= 500,
      testCase "generic message for 503" $ genericErrorMessage 503 @?= "Service temporarily unavailable"
    ]

apiHardeningTests :: TestTree
apiHardeningTests = testGroup "API Hardening (webapi-lib)" [authTests, corsTests, escapeTests, imageTests, errorTests]
