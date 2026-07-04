{-# LANGUAGE OverloadedStrings #-}

-- | Pure tests for the chain-sync replay/rollback ordering (F-02/F-05/F-07) and for the real
-- classify + deletion-sequencing mechanism that ordering exists to support (F-33).
module UnitTests.ChainSyncReplay (chainSyncReplayTests) where

import Control.Monad.Except (runExceptT)
import Data.ByteString qualified as BS
import Data.Either (isLeft, isRight)
import Data.Map qualified as Map
import GeniusYield.Types
  ( GYAddress,
    GYNetworkId (GYTestnetPreview),
    GYOutDatum (GYOutDatumInline),
    addressFromScriptHash,
    datumFromPlutusData,
    unsafeSlotFromInteger,
    valueFromLovelace,
  )
import Ingestion (ChainEventProjection (PromotionEvent, RankEvent), projectChainEvent)
import KupoAtlas (AtlasMatch (..), toGYAssetClass)
import KupoClient (CreatedAt (..), KupoMatch (..), KupoValue (..))
import Onchain.Protocol.Types (OnchainRank (..), ProtocolParams (..))
import PlutusLedgerApi.V1.Value (AssetClass (..), CurrencySymbol (..), TokenName (..))
import PlutusLedgerApi.V3 (POSIXTime (POSIXTime), ScriptHash (..))
import PlutusTx.Builtins (toBuiltin)
import Storage
  ( OnchainMatchEvent (..),
    onchainMatchEventCreatedOutputIndex,
    onchainMatchEventCreatedSlot,
    onchainMatchEventCreatedTxIndex,
    replayOrder,
  )
import Test.Tasty
import Test.Tasty.HUnit
import TxBuilding.Validators (ranksValidatorHashGY)

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

valueConversionTests :: TestTree
valueConversionTests =
  testGroup
    "Kupo asset conversion (F-06)"
    [ testCase "dotted policyId.assetName parses" $
        isRight (toGYAssetClass "00000000000000000000000000000000000000000000000000000000.6162") @?= True,
      testCase "dotless empty-name policyId parses instead of crashing" $
        isRight (toGYAssetClass "00000000000000000000000000000000000000000000000000000000") @?= True,
      testCase "garbage returns Left, not a crash" $
        isLeft (toGYAssetClass "xyz") @?= True
    ]

------------------------------------------------------------------------------------------------
-- Fixtures for the classify + deletion-sequencing tests (F-33 item 2 and 3, design spec §7).
--
-- These drive the REAL 'projectChainEvent' (Ingestion.hs) on synthetic 'AtlasMatch' values, at
-- no DB and no live provider: 'projectChainEvent' is 'MonadError GYTxMonadException'-polymorphic
-- and depends only on a 'GYNetworkId' and pure validator-hash constants, so it runs purely via
-- 'runExceptT'. Both a 'Rank' and a 'Promotion' onchain datum decode at the ranks-validator
-- address (see 'rankFromGYOutDatum' / TxBuilding.Utils) as a *bare* 'OnchainRank' — no CIP-68
-- wrapper — so no ambiguous metadata map is needed to build a valid fixture.
------------------------------------------------------------------------------------------------

-- | Test-only network id; the specific choice does not matter as long as it's used consistently
-- to both build the fixture address and to invoke 'projectChainEvent'.
testNetworkId :: GYNetworkId
testNetworkId = GYTestnetPreview

-- | A 28-byte currency symbol tagged by @salt@, so distinct fixture asset classes are distinct.
-- 'assetClassFromPlutus'' (used inside 'projectChainEvent's conversions) requires exactly 28
-- bytes to parse as a minting policy id.
dummyCurrencySymbol :: Int -> CurrencySymbol
dummyCurrencySymbol salt = CurrencySymbol (toBuiltin (BS.replicate 28 (fromIntegral salt)))

dummyScriptHash :: Int -> ScriptHash
dummyScriptHash salt = ScriptHash (toBuiltin (BS.replicate 28 (fromIntegral salt)))

dummyAssetClass :: Int -> TokenName -> AssetClass
dummyAssetClass salt tn = AssetClass (dummyCurrencySymbol salt, tn)

-- | Carried in the datum but never read by classify/conversion; content is irrelevant.
dummyProtocolParams :: ProtocolParams
dummyProtocolParams =
  ProtocolParams
    { ranksValidatorScriptHash = dummyScriptHash 0x40,
      profilesValidatorScriptHash = dummyScriptHash 0x41,
      membershipsValidatorScriptHash = dummyScriptHash 0x42,
      achievementsValidatorScriptHash = dummyScriptHash 0x43,
      oracleToken = dummyAssetClass 0x44 (TokenName "oracle")
    }

practitionerProfileId, masterProfileId, sharedRankId :: AssetClass
practitionerProfileId = dummyAssetClass 0x10 (TokenName "practitioner")
masterProfileId = dummyAssetClass 0x20 (TokenName "master")
-- | Same asset class used as both the promotion's id and the rank's id below: the rank datum is
-- the on-chain "acceptance" of that specific promotion.
sharedRankId = dummyAssetClass 0x30 (TokenName "belt-rank-01")

-- | A pending promotion for 'sharedRankId', awarded at an earlier date than the confirming rank.
promotionDatum :: OnchainRank
promotionDatum =
  Promotion
    { promotionId = sharedRankId,
      promotionRankNumber = 1,
      promotionAwardedTo = practitionerProfileId,
      promotionAwardedBy = masterProfileId,
      promotionAchievementDate = POSIXTime 1000,
      promotionProtocolParams = dummyProtocolParams
    }

-- | The confirmed rank for the same 'sharedRankId' — the on-chain acceptance of 'promotionDatum'.
rankDatum :: OnchainRank
rankDatum =
  Rank
    { rankId = sharedRankId,
      rankNumber = 1,
      rankAchievedByProfileId = practitionerProfileId,
      rankAwardedByProfileId = masterProfileId,
      rankAchievementDate = POSIXTime 2000,
      rankPreviousRankId = Nothing,
      rankProtocolParams = dummyProtocolParams
    }

ranksAddress :: GYAddress
ranksAddress = addressFromScriptHash testNetworkId ranksValidatorHashGY

-- | Wrap an 'OnchainRank' datum as an unspent inline-datum match at the ranks validator address —
-- the exact shape 'projectChainEvent' inspects to classify Rank vs Promotion events.
mkRanksMatch :: OnchainRank -> AtlasMatch
mkRanksMatch onchainRank =
  AtlasMatch
    { amTransactionIndex = 0,
      amTransactionId = "6c751d3e198c5608dfafdfdffe16aeac8a28f88f3a769cf22dd45e8bc84f47e8",
      amOutputIndex = 0,
      amAddress = ranksAddress,
      amValue = valueFromLovelace 2_000_000,
      amDatum = GYOutDatumInline (datumFromPlutusData onchainRank),
      amScriptHash = Just ranksValidatorHashGY,
      amCreatedAt = unsafeSlotFromInteger 0,
      amCreatedAtHeaderHash = "hdr",
      amSpentAt = Nothing,
      amSpentAtHeaderHash = Nothing,
      amSpentAtTransactionId = Nothing,
      amSpentAtInputIndex = Nothing,
      amSpentAtRedeemer = Nothing
    }

promotionMatch, rankMatch :: AtlasMatch
promotionMatch = mkRanksMatch promotionDatum
rankMatch = mkRanksMatch rankDatum

-- | Pins the real 'projectChainEvent' classify logic against synthetic Rank/Promotion fixtures —
-- no DB, no live provider (design spec §7 item 2).
classifyTests :: TestTree
classifyTests =
  testGroup
    "projectChainEvent classify on real Ingestion code (F-33)"
    [ testCase "Promotion datum at the ranks validator classifies as PromotionEvent" $ do
        result <- runExceptT (projectChainEvent testNetworkId promotionMatch)
        case result of
          Right (PromotionEvent _) -> pure ()
          other -> assertFailure ("expected Right (PromotionEvent _), got: " <> show other),
      testCase "Rank datum at the ranks validator classifies as RankEvent" $ do
        result <- runExceptT (projectChainEvent testNetworkId rankMatch)
        case result of
          Right (RankEvent _) -> pure ()
          other -> assertFailure ("expected Right (RankEvent _), got: " <> show other)
    ]

-- | Pins the precondition 'deletePromotionProjection' relies on for correct rollback replay: a
-- profile's promotion-then-rank-confirms raw log, even if the two matches were stored out of
-- true chain order, replays (via 'replayOrder') so the rank is classified strictly after its
-- confirming promotion — which is what makes 'putRankProjection' + 'deletePromotionProjection'
-- fire in the right order on replay (design spec §7 item 3).
deletionSequencingTests :: TestTree
deletionSequencingTests =
  testGroup
    "Deletion sequencing: replayOrder + projectChainEvent classify promotion before its confirming rank (F-07/F-33)"
    [ testCase "promotion-then-rank raw log, fed out of order, replays as [PromotionEvent, RankEvent]" $ do
        let promotionRaw = OnchainMatchEvent 10 "hdr" "promotion-tx" 0 0 dummyMatch
            rankRaw = OnchainMatchEvent 11 "hdr" "rank-tx" 0 0 dummyMatch
            -- Fed in reverse of chain order on purpose, so a broken replayOrder would be caught.
            sortedRaws = replayOrder [rankRaw, promotionRaw]
            atlasMatchFor raw
              | onchainMatchEventCreatedTxId raw == "promotion-tx" = promotionMatch
              | otherwise = rankMatch
        classified <- mapM (runExceptT . projectChainEvent testNetworkId . atlasMatchFor) sortedRaws
        case classified of
          [Right (PromotionEvent _), Right (RankEvent _)] -> pure ()
          other ->
            assertFailure
              ("expected [Right (PromotionEvent _), Right (RankEvent _)], got: " <> show other)
    ]

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
          @?= [(1, 0), (1, 3), (2, 0)],
      valueConversionTests,
      classifyTests,
      deletionSequencingTests
    ]
