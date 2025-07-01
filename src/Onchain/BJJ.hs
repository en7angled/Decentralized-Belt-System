{-# LANGUAGE NoImplicitPrelude #-}

module Onchain.BJJ where

import GHC.Generics (Generic)
import PlutusLedgerApi.V3
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude
import Prelude qualified

-------------------------------------------------------------------------------

-- * BJJ Rank Types

-------------------------------------------------------------------------------
type Stripe = Integer

data Belt
  = White
  | Blue
  | Purple
  | Brown
  | Black
  | RedAndBlack
  | RedAndWhite
  | Red
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''Belt [('White, 0), ('Blue, 1), ('Purple, 2), ('Brown, 3), ('Black, 4), ('RedAndBlack, 5), ('RedAndWhite, 6), ('Red, 7)]

instance Eq Belt where
  (==) :: Belt -> Belt -> Bool
  (==) x y = baseRank x == baseRank y

baseRank :: Belt -> Integer
baseRank belt = case belt of
  White -> 0
  Blue -> 5
  Purple -> 10
  Brown -> 15
  Black -> 20
  RedAndBlack -> 27
  RedAndWhite -> 28
  Red -> 29

data RankValue
  = RankValue Belt Stripe
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''RankValue [('RankValue, 0)]

instance Eq RankValue where
  (==) :: RankValue -> RankValue -> Bool
  (==) x y = rankToInt x == rankToInt y

instance Ord RankValue where
  compare :: RankValue -> RankValue -> Ordering
  compare x y = compare (rankToInt x) (rankToInt y)

rankToInt :: RankValue -> Integer
rankToInt (RankValue belt stripe) = baseRank belt + stripe

-- | Convert an Integer to a RankValue (Belt, Stripe), if possible.
intToRank :: Integer -> RankValue
intToRank n
  | n == 0 = RankValue White 0
  | n < 5 = RankValue White n
  | n < 10 = RankValue Blue (n - 5)
  | n < 15 = RankValue Purple (n - 10)
  | n < 20 = RankValue Brown (n - 15)
  | n < 27 = RankValue Black (n - 20)
  | n == 27 = RankValue RedAndBlack 0
  | n == 28 = RankValue RedAndWhite 0
  | n == 29 = RankValue Red 0
  | otherwise = traceError "Invalid rank"

instance Enum RankValue where
  toEnum :: Integer -> RankValue
  toEnum = intToRank
  fromEnum :: RankValue -> Integer
  fromEnum (RankValue belt stripe) = baseRank belt + stripe
  succ :: RankValue -> RankValue
  succ (RankValue Red 0) = traceError "Cannot succ a red belt"
  succ r = intToRank . (+ 1) . rankToInt $ r
  pred :: RankValue -> RankValue
  pred (RankValue White 0) = traceError "Cannot pred a white belt"
  pred r = intToRank . (+ (-1)) . rankToInt $ r
  enumFromTo :: RankValue -> RankValue -> [RankValue]
  enumFromTo start end = map toEnum [fromEnum start .. fromEnum end]
  enumFromThenTo :: RankValue -> RankValue -> RankValue -> [RankValue]
  enumFromThenTo start next end = map toEnum [fromEnum start, fromEnum next .. fromEnum end]
