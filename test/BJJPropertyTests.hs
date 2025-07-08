{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BJJPropertyTests where

import Control.Monad (when)
import Control.Exception (try, evaluate, SomeException)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Onchain.BJJ
import PlutusLedgerApi.V3 (POSIXTime (POSIXTime))
import Test.Tasty
import Test.Tasty.Hedgehog

-- Helper to extract Integer from POSIXTime
unPOSIXTime :: POSIXTime -> Integer
unPOSIXTime (POSIXTime i) = i

-- =============================================================================
-- BJJ-Specific Property Tests
-- =============================================================================

bjjPropertyTests :: TestTree
bjjPropertyTests = testGroup "BJJ Property Tests"
  [ testGroup "Belt Conversion Properties"
    [ testProperty "beltToInt and intToBelt are bijective" prop_beltIntBijective
    , testProperty "beltToInt preserves ordering" prop_beltIntOrdering
    , testProperty "intToBelt handles valid ranges" prop_intToBeltValidRanges
    , testProperty "intToBelt handles invalid ranges gracefully" prop_intToBeltInvalidRanges
    ]
  , testGroup "Belt Comparison Properties"
    [ testProperty "belt comparison is transitive" prop_beltComparisonTransitive
    , testProperty "belt comparison is antisymmetric" prop_beltComparisonAntisymmetric
    , testProperty "belt comparison is reflexive" prop_beltComparisonReflexive
    , testProperty "belt comparison is total" prop_beltComparisonTotal
    ]
  , testGroup "Belt Enumeration Properties"
    [ testProperty "belt enumeration is consistent" prop_beltEnumerationConsistent
    , testProperty "belt succ/pred are inverses" prop_beltSuccPredInverse
    , testProperty "belt succ increases rank" prop_beltSuccIncreases
    , testProperty "belt pred decreases rank" prop_beltPredDecreases
    ]
  , testGroup "Minimum Time Properties"
    [ testProperty "minMonthsForBelt is non-negative" prop_minMonthsNonNegative
    , testProperty "minMonthsForBelt has expected values" prop_minMonthsExpectedValues
    , testProperty "monthsToPosixTime is non-negative" prop_monthsToPosixTimeNonNegative
    ]
  , testGroup "Promotion Validation Properties"
    [ testProperty "time requirements enforced" prop_timeRequirementsEnforced
    , testProperty "same belt promotion fails" prop_sameBeltPromotionFails
    , testProperty "downgrade promotion fails" prop_downgradePromotionFails
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
    Left _  -> Hedgehog.assert True  -- Exception was thrown, as expected
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
    Hedgehog.assert $ not (belt2 < belt1)

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
  (masterBelt, masterDate, studentCurrentBelt, studentCurrentDate, studentNextBelt, studentNextDate) <- forAll genInvalidTimePromotion
  Hedgehog.assert $ not (validatePromotion masterBelt masterDate studentCurrentBelt studentCurrentDate studentNextBelt studentNextDate)


prop_sameBeltPromotionFails :: Property
prop_sameBeltPromotionFails = property $ do
  belt <- forAll genBelt
  date <- forAll genPOSIXTime
  Hedgehog.assert $ not (validatePromotion belt date belt date belt date)

prop_downgradePromotionFails :: Property
prop_downgradePromotionFails = property $ do
  higherBelt <- forAll genBelt
  lowerBelt <- forAll genBelt
  date <- forAll genPOSIXTime
  when (higherBelt > lowerBelt) $ do
    Hedgehog.assert $ not (validatePromotion higherBelt date higherBelt date lowerBelt date)

-- =============================================================================
-- Generators
-- =============================================================================

genBelt :: Gen BJJBelt
genBelt = Gen.element [White, Blue, Purple, Brown, Black, Black1, Black2, Black3, Black4, Black5, Black6, RedAndBlack, RedAndWhite, Red, Red10]

genPOSIXTime :: Gen POSIXTime
genPOSIXTime = POSIXTime <$> Gen.integral (Range.linear 0 1000000000)



-- Invalid time promotion generator
genInvalidTimePromotion :: Gen (BJJBelt, POSIXTime, BJJBelt, POSIXTime, BJJBelt, POSIXTime)
genInvalidTimePromotion = do
  masterBelt <- Gen.element [Black, Black1, Black2]
  masterDate <- genPOSIXTime
  studentCurrentBelt <- Gen.element [White, Blue, Purple, Brown]
  studentCurrentDate <- genPOSIXTime
  studentNextBelt <- Gen.element [Blue, Purple, Brown, Black]
  
  -- Set next date before current date
  studentNextDate <- genPOSIXTime
  let invalidNextDate = POSIXTime (unPOSIXTime studentCurrentDate - 2629800000) -- Before current date
  
  pure (masterBelt, masterDate, studentCurrentBelt, studentCurrentDate, studentNextBelt, invalidNextDate) 