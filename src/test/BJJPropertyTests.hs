{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BJJPropertyTests where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (when)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Onchain.BJJ
import Onchain.CIP68 (CIP68Datum (..), MetadataFields (..))
import Onchain.LinkedList (NodeDatum (..))
import Onchain.Protocol.Types
import PlutusLedgerApi.V1.Value (AssetClass (..), CurrencySymbol (..), TokenName (..))
import PlutusLedgerApi.V3 (Address (..), Credential (..), POSIXTime (POSIXTime), PubKeyHash (..), ScriptHash (..), StakingCredential (..))
import PlutusTx (fromBuiltinData, toBuiltinData)
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Builtins (BuiltinByteString, toBuiltin)
import Test.Tasty
import Test.Tasty.Hedgehog

-- Helper to extract Integer from POSIXTime
unPOSIXTime :: POSIXTime -> Integer
unPOSIXTime (POSIXTime i) = i

-- =============================================================================
-- BJJ-Specific Property Tests
-- =============================================================================

bjjPropertyTests :: TestTree
bjjPropertyTests =
  testGroup
    "BJJ Property Tests"
    [ testGroup
        "Belt Conversion Properties"
        [ testProperty "beltToInt and intToBelt are bijective" prop_beltIntBijective,
          testProperty "beltToInt preserves ordering" prop_beltIntOrdering,
          testProperty "intToBelt handles valid ranges" prop_intToBeltValidRanges,
          testProperty "intToBelt handles invalid ranges gracefully" prop_intToBeltInvalidRanges
        ],
      testGroup
        "Belt Comparison Properties"
        [ testProperty "belt comparison is transitive" prop_beltComparisonTransitive,
          testProperty "belt comparison is antisymmetric" prop_beltComparisonAntisymmetric,
          testProperty "belt comparison is reflexive" prop_beltComparisonReflexive,
          testProperty "belt comparison is total" prop_beltComparisonTotal
        ],
      testGroup
        "Belt Enumeration Properties"
        [ testProperty "belt enumeration is consistent" prop_beltEnumerationConsistent,
          testProperty "belt succ/pred are inverses" prop_beltSuccPredInverse,
          testProperty "belt succ increases rank" prop_beltSuccIncreases,
          testProperty "belt pred decreases rank" prop_beltPredDecreases
        ],
      testGroup
        "Minimum Time Properties"
        [ testProperty "minMonthsForBelt is non-negative" prop_minMonthsNonNegative,
          testProperty "minMonthsForBelt has expected values" prop_minMonthsExpectedValues,
          testProperty "monthsToPosixTime is non-negative" prop_monthsToPosixTimeNonNegative
        ],
      testGroup
        "Promotion Validation Properties"
        [ testProperty "time requirements enforced" prop_timeRequirementsEnforced,
          testProperty "same belt promotion fails" prop_sameBeltPromotionFails,
          testProperty "downgrade promotion fails" prop_downgradePromotionFails
        ],
      testGroup
        "Serialization Roundtrip Properties"
        [ testGroup
            "Oracle Types"
            [ testProperty "OracleParams roundtrip (no fee)" prop_oracleParamsRoundtrip,
              testProperty "FeeConfig roundtrip" prop_feeConfigRoundtrip,
              testProperty "OracleParams roundtrip (with fee)" prop_oracleParamsWithFeeConfigRoundtrip
            ],
          testGroup
            "Core Datum Types"
            [ testProperty "BJJBelt roundtrip" prop_bjjBeltRoundtrip,
              testProperty "BeltSnapshot roundtrip" prop_beltSnapshotRoundtrip,
              testProperty "OnchainProfileType roundtrip" prop_onchainProfileTypeRoundtrip,
              testProperty "OnchainProfile roundtrip" prop_onchainProfileRoundtrip,
              testProperty "OnchainRank (Rank) roundtrip" prop_onchainRankRoundtrip,
              testProperty "OnchainRank (Promotion) roundtrip" prop_onchainPromotionRoundtrip
            ],
          testGroup
            "Membership Types"
            [ testProperty "OnchainMembershipHistory roundtrip" prop_membershipHistoryRoundtrip,
              testProperty "OnchainMembershipInterval roundtrip" prop_membershipIntervalRoundtrip,
              testProperty "MembershipHistoriesListNode roundtrip" prop_membershipListNodeRoundtrip,
              testProperty "MembershipDatum (ListNodeDatum) roundtrip" prop_membershipDatumListNodeRoundtrip,
              testProperty "MembershipDatum (IntervalDatum) roundtrip" prop_membershipDatumIntervalRoundtrip
            ],
          testGroup
            "CIP-68 Types"
            [ testProperty "MetadataFields roundtrip" prop_metadataFieldsRoundtrip,
              testProperty "CIP68Datum OnchainProfile roundtrip" prop_cip68DatumProfileRoundtrip,
              testProperty "CIP68Datum OnchainRank roundtrip" prop_cip68DatumRankRoundtrip,
              testProperty "NodeDatum roundtrip" prop_nodeDatumRoundtrip
            ]
        ]
    ]

-- =============================================================================
-- Belt Conversion Properties
-- =============================================================================

prop_beltIntBijective :: Property
prop_beltIntBijective = property $ do
  belt <- forAll genBelt
  intToBelt (beltToInt belt) === belt

prop_beltIntOrdering :: Property
prop_beltIntOrdering = property $ do
  belt1 <- forAll genBelt
  belt2 <- forAll genBelt
  (belt1 < belt2) === (beltToInt belt1 < beltToInt belt2)

prop_intToBeltValidRanges :: Property
prop_intToBeltValidRanges = property $ do
  validInt <- forAll (Gen.integral (Range.linear 0 14))
  let belt = intToBelt validInt
  beltToInt belt === validInt

prop_intToBeltInvalidRanges :: Property
prop_intToBeltInvalidRanges = property $ do
  invalidInt <- forAll (Gen.integral (Range.linear 15 100))
  result <- evalIO $ try (evaluate (intToBelt invalidInt)) :: PropertyT IO (Either SomeException BJJBelt)
  case result of
    Left _ -> Hedgehog.assert True -- Exception was thrown, as expected
    Right _ -> Hedgehog.assert False -- No exception, which is a failure

-- =============================================================================
-- Belt Comparison Properties
-- =============================================================================

prop_beltComparisonTransitive :: Property
prop_beltComparisonTransitive = property $ do
  belt1 <- forAll genBelt
  belt2 <- forAll genBelt
  belt3 <- forAll genBelt
  when (belt1 < belt2 && belt2 < belt3) $ do
    Hedgehog.assert $ belt1 < belt3

prop_beltComparisonAntisymmetric :: Property
prop_beltComparisonAntisymmetric = property $ do
  belt1 <- forAll genBelt
  belt2 <- forAll genBelt
  when (belt1 < belt2) $ do
    Hedgehog.assert (belt2 >= belt1)

prop_beltComparisonReflexive :: Property
prop_beltComparisonReflexive = property $ do
  belt <- forAll genBelt
  Hedgehog.assert $ belt <= belt

prop_beltComparisonTotal :: Property
prop_beltComparisonTotal = property $ do
  belt1 <- forAll genBelt
  belt2 <- forAll genBelt
  Hedgehog.assert $ belt1 <= belt2 || belt2 <= belt1

-- =============================================================================
-- Belt Enumeration Properties
-- =============================================================================

prop_beltEnumerationConsistent :: Property
prop_beltEnumerationConsistent = property $ do
  start <- forAll genBelt
  end <- forAll genBelt
  when (start <= end) $ do
    let enumList = enumFromTo start end
    length enumList === (fromEnum end - fromEnum start + 1)

prop_beltSuccPredInverse :: Property
prop_beltSuccPredInverse = property $ do
  belt <- forAll genBelt
  when (belt /= Red10) $ do
    pred (succ belt) === belt

prop_beltSuccIncreases :: Property
prop_beltSuccIncreases = property $ do
  belt <- forAll genBelt
  when (belt /= Red10) $ do
    Hedgehog.assert $ succ belt > belt

prop_beltPredDecreases :: Property
prop_beltPredDecreases = property $ do
  belt <- forAll genBelt
  when (belt /= White) $ do
    Hedgehog.assert $ pred belt < belt

-- =============================================================================
-- Minimum Time Properties
-- =============================================================================

prop_minMonthsNonNegative :: Property
prop_minMonthsNonNegative = property $ do
  belt <- forAll genBelt
  Hedgehog.assert $ minMonthsForBelt belt >= 0

prop_minMonthsExpectedValues :: Property
prop_minMonthsExpectedValues = property $ do
  minMonthsForBelt White === 0
  minMonthsForBelt Blue === 12
  minMonthsForBelt Purple === 18
  minMonthsForBelt Brown === 12
  minMonthsForBelt Black === 12
  minMonthsForBelt Black1 === 36
  minMonthsForBelt Black2 === 36
  minMonthsForBelt Black3 === 36
  minMonthsForBelt Black4 === 60
  minMonthsForBelt Black5 === 60
  minMonthsForBelt Black6 === 60
  minMonthsForBelt RedAndBlack === 84
  minMonthsForBelt RedAndWhite === 84
  minMonthsForBelt Red === 120
  minMonthsForBelt Red10 === 0

prop_monthsToPosixTimeNonNegative :: Property
prop_monthsToPosixTimeNonNegative = property $ do
  months <- forAll (Gen.integral (Range.linear 0 100))
  let posixTime = monthsToPosixTime months
  Hedgehog.assert $ posixTime >= POSIXTime 0

-- =============================================================================
-- Promotion Validation Properties
-- =============================================================================

prop_timeRequirementsEnforced :: Property
prop_timeRequirementsEnforced = property $ do
  (master, studentCurrent, studentNext) <- forAll genInvalidTimePromotion
  Hedgehog.assert $ not (validatePromotion master studentCurrent studentNext)

prop_sameBeltPromotionFails :: Property
prop_sameBeltPromotionFails = property $ do
  b <- forAll genBelt
  date <- forAll genPOSIXTime
  let snapshot = BeltSnapshot b date
  Hedgehog.assert $ not (validatePromotion snapshot snapshot snapshot)

prop_downgradePromotionFails :: Property
prop_downgradePromotionFails = property $ do
  higherBelt <- forAll genBelt
  lowerBelt <- forAll genBelt
  date <- forAll genPOSIXTime
  when (higherBelt > lowerBelt) $ do
    Hedgehog.assert $ not (validatePromotion (BeltSnapshot higherBelt date) (BeltSnapshot higherBelt date) (BeltSnapshot lowerBelt date))

-- =============================================================================
-- Generators
-- =============================================================================

genBelt :: Gen BJJBelt
genBelt = Gen.element [White, Blue, Purple, Brown, Black, Black1, Black2, Black3, Black4, Black5, Black6, RedAndBlack, RedAndWhite, Red, Red10]

genPOSIXTime :: Gen POSIXTime
genPOSIXTime = POSIXTime <$> Gen.integral (Range.linear 0 1000000000)

-- Invalid time promotion generator
genInvalidTimePromotion :: Gen (BeltSnapshot, BeltSnapshot, BeltSnapshot)
genInvalidTimePromotion = do
  masterBelt <- Gen.element [Black, Black1, Black2]
  masterDate <- genPOSIXTime
  studentCurrentBelt <- Gen.element [White, Blue, Purple, Brown]
  studentCurrentDate <- genPOSIXTime
  studentNextBelt <- Gen.element [Blue, Purple, Brown, Black]

  -- Set next date before current date
  let invalidNextDate = POSIXTime (unPOSIXTime studentCurrentDate - 2629800000) -- Before current date
  pure (BeltSnapshot masterBelt masterDate, BeltSnapshot studentCurrentBelt studentCurrentDate, BeltSnapshot studentNextBelt invalidNextDate)

-- =============================================================================
-- Serialization Roundtrip Properties — Oracle Types
-- =============================================================================

prop_oracleParamsRoundtrip :: Property
prop_oracleParamsRoundtrip = property $ do
  oracleParams <- forAll genOracleParamsNoFee
  fromBuiltinData (toBuiltinData oracleParams) === Just oracleParams

prop_feeConfigRoundtrip :: Property
prop_feeConfigRoundtrip = property $ do
  feeConfig <- forAll genFeeConfig
  fromBuiltinData (toBuiltinData feeConfig) === Just feeConfig

prop_oracleParamsWithFeeConfigRoundtrip :: Property
prop_oracleParamsWithFeeConfigRoundtrip = property $ do
  oracleParams <- forAll genOracleParamsWithFee
  fromBuiltinData (toBuiltinData oracleParams) === Just oracleParams

-- =============================================================================
-- Serialization Roundtrip Properties — Core Datum Types
-- =============================================================================

prop_bjjBeltRoundtrip :: Property
prop_bjjBeltRoundtrip = property $ do
  b <- forAll genBelt
  fromBuiltinData (toBuiltinData b) === Just b

prop_beltSnapshotRoundtrip :: Property
prop_beltSnapshotRoundtrip = property $ do
  snap <- forAll genBeltSnapshot
  fromBuiltinData (toBuiltinData snap) === Just snap

prop_onchainProfileTypeRoundtrip :: Property
prop_onchainProfileTypeRoundtrip = property $ do
  pt <- forAll genOnchainProfileType
  fromBuiltinData (toBuiltinData pt) === Just pt

prop_onchainProfileRoundtrip :: Property
prop_onchainProfileRoundtrip = property $ do
  p <- forAll genOnchainProfile
  fromBuiltinData (toBuiltinData p) === Just p

prop_onchainRankRoundtrip :: Property
prop_onchainRankRoundtrip = property $ do
  r <- forAll genOnchainRank
  fromBuiltinData (toBuiltinData r) === Just r

prop_onchainPromotionRoundtrip :: Property
prop_onchainPromotionRoundtrip = property $ do
  p <- forAll genOnchainPromotion
  fromBuiltinData (toBuiltinData p) === Just p

-- =============================================================================
-- Serialization Roundtrip Properties — Membership Types
-- =============================================================================

prop_membershipHistoryRoundtrip :: Property
prop_membershipHistoryRoundtrip = property $ do
  mh <- forAll genOnchainMembershipHistory
  fromBuiltinData (toBuiltinData mh) === Just mh

prop_membershipIntervalRoundtrip :: Property
prop_membershipIntervalRoundtrip = property $ do
  mi <- forAll genOnchainMembershipInterval
  fromBuiltinData (toBuiltinData mi) === Just mi

prop_membershipListNodeRoundtrip :: Property
prop_membershipListNodeRoundtrip = property $ do
  n <- forAll genMembershipHistoriesListNode
  fromBuiltinData (toBuiltinData n) === Just n

prop_membershipDatumListNodeRoundtrip :: Property
prop_membershipDatumListNodeRoundtrip = property $ do
  n <- forAll genMembershipHistoriesListNode
  let d = ListNodeDatum n
  fromBuiltinData (toBuiltinData d) === Just d

prop_membershipDatumIntervalRoundtrip :: Property
prop_membershipDatumIntervalRoundtrip = property $ do
  mi <- forAll genOnchainMembershipInterval
  let d = IntervalDatum mi
  fromBuiltinData (toBuiltinData d) === Just d

-- =============================================================================
-- Serialization Roundtrip Properties — CIP-68 Types
-- =============================================================================

prop_metadataFieldsRoundtrip :: Property
prop_metadataFieldsRoundtrip = property $ do
  mf <- forAll genMetadataFields
  fromBuiltinData (toBuiltinData mf) === Just mf

prop_cip68DatumProfileRoundtrip :: Property
prop_cip68DatumProfileRoundtrip = property $ do
  d <- forAll genCIP68DatumProfile
  fromBuiltinData (toBuiltinData d) === Just d

prop_cip68DatumRankRoundtrip :: Property
prop_cip68DatumRankRoundtrip = property $ do
  d <- forAll genCIP68DatumRank
  fromBuiltinData (toBuiltinData d) === Just d

prop_nodeDatumRoundtrip :: Property
prop_nodeDatumRoundtrip = property $ do
  nd <- forAll genNodeDatum
  fromBuiltinData (toBuiltinData nd) === Just nd

-- =============================================================================
-- Generators — Primitive / Shared
-- =============================================================================

genPubKeyHash :: Gen PubKeyHash
genPubKeyHash = PubKeyHash . toBuiltin <$> Gen.bytes (Range.singleton 28)

genScriptHash :: Gen ScriptHash
genScriptHash = ScriptHash . toBuiltin <$> Gen.bytes (Range.singleton 28)

genCurrencySymbol :: Gen CurrencySymbol
genCurrencySymbol = CurrencySymbol . toBuiltin <$> Gen.bytes (Range.singleton 28)

genTokenName :: Gen TokenName
genTokenName = TokenName . toBuiltin <$> Gen.bytes (Range.linear 0 32)

genAssetClass :: Gen AssetClass
genAssetClass = AssetClass <$> ((,) <$> genCurrencySymbol <*> genTokenName)

genAddress :: Gen Address
genAddress = do
  cred <- PubKeyCredential <$> genPubKeyHash
  mStake <- Gen.maybe (StakingHash . PubKeyCredential <$> genPubKeyHash)
  pure $ Address cred mStake

genBuiltinByteString :: Range Int -> Gen BuiltinByteString
genBuiltinByteString range = toBuiltin <$> Gen.bytes range

-- =============================================================================
-- Generators — Oracle Types
-- =============================================================================

genFeeConfig :: Gen FeeConfig
genFeeConfig =
  FeeConfig
    <$> genAddress
    <*> Gen.integral (Range.linear 1000000 100000000)
    <*> Gen.integral (Range.linear 1000000 100000000)
    <*> Gen.integral (Range.linear 1000000 100000000)
    <*> Gen.integral (Range.linear 1000000 100000000)
    <*> Gen.integral (Range.linear 1000000 100000000)

genOracleParamsNoFee :: Gen OracleParams
genOracleParamsNoFee =
  OracleParams
    <$> genPubKeyHash
    <*> Gen.bool
    <*> pure Nothing
    <*> Gen.integral (Range.linear 1000000 50000000)

genOracleParamsWithFee :: Gen OracleParams
genOracleParamsWithFee =
  OracleParams
    <$> genPubKeyHash
    <*> Gen.bool
    <*> (Just <$> genFeeConfig)
    <*> Gen.integral (Range.linear 1000000 50000000)

-- =============================================================================
-- Generators — Core Datum Types
-- =============================================================================

genBeltSnapshot :: Gen BeltSnapshot
genBeltSnapshot = BeltSnapshot <$> genBelt <*> genPOSIXTime

genOnchainProfileType :: Gen OnchainProfileType
genOnchainProfileType = Gen.element [Practitioner, Organization]

genProtocolParams :: Gen ProtocolParams
genProtocolParams =
  ProtocolParams
    <$> genScriptHash
    <*> genScriptHash
    <*> genScriptHash
    <*> genScriptHash
    <*> genAssetClass

genOnchainProfile :: Gen OnchainProfile
genOnchainProfile =
  OnchainProfile
    <$> genAssetClass
    <*> genOnchainProfileType
    <*> Gen.maybe genAssetClass
    <*> genProtocolParams

genOnchainRank :: Gen OnchainRank
genOnchainRank =
  Rank
    <$> genAssetClass
    <*> Gen.integral (Range.linear 0 14)
    <*> genAssetClass
    <*> genAssetClass
    <*> genPOSIXTime
    <*> Gen.maybe genAssetClass
    <*> genProtocolParams

genOnchainPromotion :: Gen OnchainRank
genOnchainPromotion =
  Promotion
    <$> genAssetClass
    <*> Gen.integral (Range.linear 0 14)
    <*> genAssetClass
    <*> genAssetClass
    <*> genPOSIXTime
    <*> genProtocolParams

-- =============================================================================
-- Generators — Membership Types
-- =============================================================================

genOnchainMembershipHistory :: Gen OnchainMembershipHistory
genOnchainMembershipHistory =
  OnchainMembershipHistory
    <$> genAssetClass
    <*> genAssetClass
    <*> Gen.integral (Range.linear 0 100)

genOnchainMembershipInterval :: Gen OnchainMembershipInterval
genOnchainMembershipInterval =
  OnchainMembershipInterval
    <$> genPOSIXTime
    <*> Gen.maybe genPOSIXTime
    <*> Gen.bool
    <*> Gen.integral (Range.linear 0 100)
    <*> genAssetClass

genNodeDatum :: Gen (NodeDatum (Maybe OnchainMembershipHistory))
genNodeDatum =
  NodeDatum
    <$> Gen.maybe genAssetClass
    <*> Gen.maybe genAssetClass
    <*> Gen.maybe genOnchainMembershipHistory

genMembershipHistoriesListNode :: Gen MembershipHistoriesListNode
genMembershipHistoriesListNode =
  MembershipHistoriesListNode
    <$> genAssetClass
    <*> genNodeDatum

-- =============================================================================
-- Generators — CIP-68 Types
-- =============================================================================

genMetadataFields :: Gen MetadataFields
genMetadataFields =
  Metadata222
    <$> genBuiltinByteString (Range.linear 1 64)
    <*> genBuiltinByteString (Range.linear 1 128)
    <*> genBuiltinByteString (Range.linear 1 64)

genMetadata :: Gen (AssocMap.Map BuiltinByteString BuiltinByteString)
genMetadata = do
  pairs <- Gen.list (Range.linear 0 5) $ do
    k <- genBuiltinByteString (Range.linear 1 16)
    v <- genBuiltinByteString (Range.linear 1 32)
    pure (k, v)
  pure $ AssocMap.unsafeFromList pairs

genCIP68DatumProfile :: Gen (CIP68Datum OnchainProfile)
genCIP68DatumProfile =
  CIP68Datum
    <$> genMetadata
    <*> Gen.integral (Range.linear 1 5)
    <*> genOnchainProfile

genCIP68DatumRank :: Gen (CIP68Datum OnchainRank)
genCIP68DatumRank =
  CIP68Datum
    <$> genMetadata
    <*> Gen.integral (Range.linear 1 5)
    <*> Gen.choice [genOnchainRank, genOnchainPromotion]