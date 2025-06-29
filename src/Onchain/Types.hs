{-# LANGUAGE NoImplicitPrelude #-}

module Onchain.Types where

import GHC.Generics (Generic)
import Onchain.Utils
import PlutusLedgerApi.Data.V1 (POSIXTime)
import PlutusLedgerApi.V1.Value (AssetClass (..))
import PlutusLedgerApi.V3 (CurrencySymbol, TxOutRef)
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

type ProfileId = AssetClass

type RankId = AssetClass

data Profile
  = Profile
  { profileId :: ProfileId,
    profileType :: ProfileType,
    currentRank :: Maybe RankId -- ˆ Organisations have no rank
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''Profile [('Profile, 0)]

mkProfile :: CurrencySymbol -> CurrencySymbol -> TxOutRef -> ProfileType -> Profile
mkProfile profilesMintingPolicy ranksMintingPolicy seedTxOutRef Practitioner =
  Profile
    { profileId = AssetClass (profilesMintingPolicy, tokenNameFromTxOutRef seedTxOutRef),
      profileType = Practitioner,
      currentRank = Just (AssetClass (ranksMintingPolicy, tokenNameFromTxOutRef seedTxOutRef))
    }
mkProfile profilesMintingPolicy _ranksMintingPolicy seedTxOutRef Organization =
  Profile
    { profileId = AssetClass (profilesMintingPolicy, tokenNameFromTxOutRef seedTxOutRef),
      profileType = Organization,
      currentRank = Nothing
    }

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

data RankData
  = RankData
  { rankId :: RankId,
    rankValue :: RankValue,
    rankAchievedByProfileId :: ProfileId,
    rankAwardedByProfileIds :: [ProfileId],
    rankAchievementDate :: POSIXTime,
    rankPreviousRankId :: Maybe RankId
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''RankData [('RankData, 0)]

mkFirstRank :: CurrencySymbol -> CurrencySymbol -> TxOutRef -> POSIXTime -> RankData
mkFirstRank profilesMintingPolicy ranksMintingPolicy seedTxOutRef achievementDate =
  RankData
    { rankId = AssetClass (ranksMintingPolicy, tokenNameFromTxOutRef seedTxOutRef),
      rankValue = RankValue White 0,
      rankAchievedByProfileId = AssetClass (profilesMintingPolicy, tokenNameFromTxOutRef seedTxOutRef),
      rankAwardedByProfileIds = [],
      rankAchievementDate = achievementDate,
      rankPreviousRankId = Nothing
    }

-------------------------------------------------------------------------------

-- * Promotion

-------------------------------------------------------------------------------
type PromotionId = AssetClass

data Promotion
  = Promotion
  { promotionAwardedTo :: ProfileId,
    promotionAwardedBy :: [ProfileId],
    promotionAchievementDate :: POSIXTime,
    promotionRank :: RankValue
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''Promotion [('Promotion, 0)]

-------------------------------------------------------------------------------

-- * Membership

-------------------------------------------------------------------------------
