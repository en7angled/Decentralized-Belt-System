{-# LANGUAGE OverloadedStrings #-}

-- | Pure tests for on-chain text conversions (F-09): the encode/decode round-trip
-- must preserve non-ASCII, and decoding invalid UTF-8 must not throw.
module UnitTests.Conversions (conversionsTests) where

import Control.Exception (evaluate, try, SomeException)
import qualified Data.ByteString
import qualified Data.Text as T
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit
import TxBuilding.Conversions (fromBuiltinByteStringUtf8, textToBuiltinByteString)

roundTrip :: T.Text -> T.Text
roundTrip = fromBuiltinByteStringUtf8 . textToBuiltinByteString

conversionsTests :: TestTree
conversionsTests =
  testGroup
    "Conversions (F-09 UTF-8)"
    [ testCase "ASCII round-trips" $ roundTrip "Hello World" @?= "Hello World",
      testCase "accented text round-trips" $ roundTrip "José Gração" @?= "José Gração",
      testCase "emoji round-trips" $ roundTrip "belt \128081 test" @?= "belt \128081 test",
      testCase "empty round-trips" $ roundTrip "" @?= "",
      testCase "invalid UTF-8 bytes do not throw (lenient)" $ do
        -- 0xFF 0xFE are not valid UTF-8; decoding must yield a Text without raising.
        let bad = toBuiltin (Data.ByteString.pack [0x41, 0xFF, 0xFE, 0x42])
        r <- try (evaluate (T.length (fromBuiltinByteStringUtf8 bad)))
        case r of
          Right _ -> pure ()
          Left (e :: SomeException) -> assertFailure ("decode threw: " <> show e)
    ]
