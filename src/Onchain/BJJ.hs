{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use guards" #-}

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

{-# INLINEABLE beltToInt #-}
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



{-# INLINEABLE intToBelt #-}
intToBelt :: Integer -> BJJBelt
intToBelt n =
  if n == 0 then White
  else if n == 1 then Blue
  else if n == 2 then Purple
  else if n == 3 then Brown
  else if n == 4 then Black
  else if n == 5 then Black1
  else if n == 6 then Black2
  else if n == 7 then Black3
  else if n == 8 then Black4
  else if n == 9 then Black5
  else if n == 10 then Black6
  else if n == 11 then RedAndBlack
  else if n == 12 then RedAndWhite
  else if n == 13 then Red
  else if n == 14 then Red10
  else traceError "Invalid belt"

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

{-# INLINEABLE minMonthsForBelt #-}
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

{-# INLINEABLE monthsToPosixTime #-}
monthsToPosixTime :: Integer -> POSIXTime
monthsToPosixTime months = POSIXTime $ months * 2629800000

-- -------------------------------------------------------------------------------

-- -- * BJJ Promotion Rules

-- -------------------------------------------------------------------------------
{-# INLINEABLE validatePromotion #-}
validatePromotion :: BJJBelt -> POSIXTime -> BJJBelt -> POSIXTime -> BJJBelt -> POSIXTime -> Bool
validatePromotion masterBelt masterBeltDate studentCurrentBelt studentCurrentBeltDate studentNextBelt studentNextBeltDate =
  case masterBelt of
    r | r < Black -> traceIfFalse "Belts lower than black are not allowed to promote" False
    r | r == Black1 -> traceIfFalse "Only 2 degree black belts can promote to black" $ studentNextBelt < Black && generalRules
    _ -> generalRules
 where
  generalRules =
    and
      [ -- 1. Master belt must be greater than the student's next belt
        masterBelt > studentNextBelt
      , -- 2. Master belt date must be before the student's next belt date
        masterBeltDate < studentNextBeltDate
      , -- 3. Student's next belt must be greater than the student's current belt
        studentNextBelt > studentCurrentBelt
      , -- 4. Student Next belt date must be after the student's current belt date
        studentNextBeltDate > studentCurrentBeltDate
      , -- 5. Time in the current belt must be greater than the minimum time for the next belt
        studentNextBeltDate - studentCurrentBeltDate > monthsToPosixTime (minMonthsForBelt studentCurrentBelt)
      ]