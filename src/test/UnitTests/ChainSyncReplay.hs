{-# LANGUAGE OverloadedStrings #-}

-- | Pure tests for the chain-sync replay/rollback ordering (F-02/F-05/F-07).
module UnitTests.ChainSyncReplay (chainSyncReplayTests) where

import Data.Map qualified as Map
import KupoClient (CreatedAt (..), KupoMatch (..), KupoValue (..))
import Storage
  ( OnchainMatchEvent (..),
    onchainMatchEventCreatedOutputIndex,
    onchainMatchEventCreatedSlot,
    onchainMatchEventCreatedTxIndex,
    replayOrder,
  )
import Test.Tasty
import Test.Tasty.HUnit

-- A raw match whose Kupo payload is irrelevant to ordering.
dummyMatch :: KupoMatch
dummyMatch =
  KupoMatch
    { transaction_index = 0,
      transaction_id = "tx",
      output_index = 0,
      address = "addr",
      value = KupoValue 0 Map.empty,
      datum_hash = Nothing,
      datum_type = Nothing,
      datum = Nothing,
      script_hash = Nothing,
      created_at = CreatedAt 0 "h",
      spent_at = Nothing
    }

ev :: Integer -> Int -> Int -> OnchainMatchEvent
ev slot txIx outIx = OnchainMatchEvent slot "hdr" "tx" txIx outIx dummyMatch

chainSyncReplayTests :: TestTree
chainSyncReplayTests =
  testGroup
    "Chain-sync replay ordering"
    [ testCase "sorts across the 8-digit/9-digit slot boundary numerically (F-02)" $
        map onchainMatchEventCreatedSlot (replayOrder [ev 100000000 0 0, ev 99999999 0 0])
          @?= [99999999, 100000000],
      testCase "same-block matches ordered by (txIndex, outputIndex); all retained (F-05/F-07)" $
        map (\e -> (onchainMatchEventCreatedTxIndex e, onchainMatchEventCreatedOutputIndex e))
          (replayOrder [ev 5 2 0, ev 5 1 3, ev 5 1 0])
          @?= [(1, 0), (1, 3), (2, 0)]
    ]
