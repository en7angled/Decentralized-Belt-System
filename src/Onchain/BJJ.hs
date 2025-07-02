{-# LANGUAGE NoImplicitPrelude #-}

module Onchain.BJJ where

import GHC.Generics (Generic)
import PlutusLedgerApi.V3 (POSIXTime (POSIXTime))
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude
import Prelude qualified

-------------------------------------------------------------------------------

-- * BJJ Rank Types

-------------------------------------------------------------------------------

data BJJBelt
  = White
  | Blue
  | Purple
  | Brown
  | Black
  | Black1 -- 1st Degree
  | Black2 -- 2nd Degree
  | Black3 -- 3rd Degree
  | Black4 -- 4th Degree
  | Black5 -- 5th Degree
  | Black6 -- 6th Degree
  | RedAndBlack -- 7th Degree
  | RedAndWhite -- 8th Degree
  | Red -- 9th Degree
  | Red10 -- 10th Degree
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''BJJBelt [('White, 0), ('Blue, 1), ('Purple, 2), ('Brown, 3), ('Black, 4), ('Black1, 5), ('Black2, 6), ('Black3, 7), ('Black4, 8), ('Black5, 9), ('Black6, 10), ('RedAndBlack, 11), ('RedAndWhite, 12), ('Red, 13), ('Red10, 14)]

beltToInt :: BJJBelt -> Integer
beltToInt belt = case belt of
  White -> 0
  Blue -> 1
  Purple -> 2
  Brown -> 3
  Black -> 4
  Black1 -> 5
  Black2 -> 6
  Black3 -> 7
  Black4 -> 8
  Black5 -> 9
  Black6 -> 10
  RedAndBlack -> 11
  RedAndWhite -> 12
  Red -> 13
  Red10 -> 14

intToBelt :: Integer -> BJJBelt
intToBelt n = case n of
  0 -> White
  1 -> Blue
  2 -> Purple
  3 -> Brown
  4 -> Black
  5 -> Black1
  6 -> Black2
  7 -> Black3
  8 -> Black4
  9 -> Black5
  10 -> Black6
  11 -> RedAndBlack
  12 -> RedAndWhite
  13 -> Red
  14 -> Red10
  _ -> traceError "Invalid belt"

instance Eq BJJBelt where
  (==) :: BJJBelt -> BJJBelt -> Bool
  (==) x y = beltToInt x == beltToInt y

instance Ord BJJBelt where
  compare :: BJJBelt -> BJJBelt -> Ordering
  compare x y = compare (beltToInt x) (beltToInt y)

instance Enum BJJBelt where
  succ :: BJJBelt -> BJJBelt
  succ Red10 = traceError "Cannot succ a red 10 belt"
  succ belt = intToBelt . (+ 1) . beltToInt $ belt
  pred :: BJJBelt -> BJJBelt
  pred White = traceError "Cannot pred a white belt"
  pred belt = intToBelt . (+ (-1)) . beltToInt $ belt
  toEnum :: Integer -> BJJBelt
  toEnum = intToBelt
  fromEnum :: BJJBelt -> Integer
  fromEnum = beltToInt
  enumFromTo :: BJJBelt -> BJJBelt -> [BJJBelt]
  enumFromTo start end = map toEnum [fromEnum start .. fromEnum end]
  enumFromThenTo :: BJJBelt -> BJJBelt -> BJJBelt -> [BJJBelt]
  enumFromThenTo start next end = map toEnum [fromEnum start, fromEnum next .. fromEnum end]

minMonthsForBelt :: BJJBelt -> Integer
minMonthsForBelt belt = case belt of
  White -> 0
  Blue -> 12
  Purple -> 18
  Brown -> 12
  Black -> 12
  Black1 -> 36
  Black2 -> 36
  Black3 -> 36
  Black4 -> 60
  Black5 -> 60
  Black6 -> 60
  RedAndBlack -> 84
  RedAndWhite -> 84
  Red -> 120
  Red10 -> 0

monthsToPosixTime :: Integer -> POSIXTime
monthsToPosixTime months = POSIXTime $ months * 2629800000

-- -------------------------------------------------------------------------------

-- -- * BJJ Promotion Rules

-- -------------------------------------------------------------------------------

validatePromotion :: BJJBelt -> POSIXTime -> POSIXTime -> BJJBelt -> POSIXTime -> BJJBelt -> Bool
validatePromotion masterRankValue masterRankDate nextRankDate studentCurrentRankValue studentCurrentRankDate nextRankValue =
  case masterRankValue of
    r | r < Black -> traceIfFalse "Belts lower than black are not allowed to promote" False
    r | r == Black1 -> traceIfFalse "Only 2 degree black belts can promote to black" $ nextRankValue /= Black && generalRules
    _ -> generalRules
 where
  generalRules =
    and
      [ -- 1. Master Rank must be greater than the student's next rank
        masterRankValue > nextRankValue
      , -- 2. Master Rank date must be before the student's next rank date
        masterRankDate < nextRankDate
      , -- 3. Next rank must be greater than the student's current rank
        nextRankValue > studentCurrentRankValue
      , -- 4. Next rank date must be after the student's current rank date
        nextRankDate > studentCurrentRankDate
      , -- 5. Time in the current rank must be greater than the minimum time for the next rank
        nextRankDate - studentCurrentRankDate > monthsToPosixTime (minMonthsForBelt studentCurrentRankValue)
      ]