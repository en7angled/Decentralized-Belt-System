{-# LANGUAGE NoImplicitPrelude #-}

module Onchain.Types where

import GHC.Generics (Generic)
import PlutusLedgerApi.Data.V1 (POSIXTime)
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Eq
import PlutusTx.Ord
import PlutusTx.Prelude
import Prelude qualified

-------------------------------------------------------------------------------

-- * Profile

-------------------------------------------------------------------------------
data ProfileType
  = Practitioner
  | Organization
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''ProfileType [('Practitioner, 0), ('Organization, 1)]

newtype ProfileId = ProfileId BuiltinByteString
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''ProfileId [('ProfileId, 0)]

newtype RankId = RankId BuiltinByteString
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''RankId [('RankId, 0)]

data Profile
  = Profile
  { profileId :: ProfileId,
    profileType :: ProfileType,
    currentRank :: Maybe RankId -- ˆ Organisations have no rank
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''Profile [('Profile, 0)]

-------------------------------------------------------------------------------

-- * Rank

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

data BJJRank
  = BJJRank Belt Stripe
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''BJJRank [('BJJRank, 0)]

instance Eq BJJRank where
  (==) :: BJJRank -> BJJRank -> Bool
  (==) x y = rankToInt x == rankToInt y

instance Ord BJJRank where
  compare :: BJJRank -> BJJRank -> Ordering
  compare x y = compare (rankToInt x) (rankToInt y)

rankToInt :: BJJRank -> Integer
rankToInt (BJJRank belt stripe) = baseRank belt + stripe

newtype RankType
  = BJJ BJJRank
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''RankType [('BJJ, 0)]

data Rank
  = Rank
  { rankId :: RankId,
    rank :: RankType,
    rankAchievedById :: ProfileId,
    rankAwardedBy :: [ProfileId],
    rankAchievementDate :: POSIXTime,
    previousRankId :: Maybe RankId
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''Rank [('Rank, 0)]

-------------------------------------------------------------------------------

-- * Promotion

-------------------------------------------------------------------------------

data Promotion
  = Promotion
  { promotionAwardedTo :: ProfileId,
    promotionAwardedBy :: [ProfileId],
    promotionAchievementDate :: POSIXTime,
    promotionRank :: RankType
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''Promotion [('Promotion, 0)]

-------------------------------------------------------------------------------

-- * Membership

-------------------------------------------------------------------------------
