{-# LANGUAGE NoImplicitPrelude #-}
-- Required for `makeLift`:
{-# LANGUAGE MultiParamTypeClasses #-}
-- Required for `makeLift`:
{-# LANGUAGE ScopedTypeVariables #-}

module Onchain.Types where

import GHC.Generics (Generic)
import PlutusLedgerApi.V1 (POSIXTime, TokenName (..), ScriptHash, Lovelace)
import PlutusLedgerApi.V1.Value (AssetClass (..))
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Eq
import PlutusTx.Ord
import PlutusTx.Prelude
import Prelude qualified
import PlutusTx.Builtins (serialiseData)


-- | Parameters :
data ProtocolParams = ProtocolParams
  { ranksValidatorScriptHash :: ScriptHash,
    collateral :: Lovelace
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

-- | Generate `Lift` instance for the custom parameter type with Template Haskell.
--  Allows argument value to be pre-compiled to UPLC, so the compiled parameterized script can be applied to it.
makeLift ''ProtocolParams

makeIsDataSchemaIndexed ''ProtocolParams [('ProtocolParams, 0)]



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


generateRankId:: ProfileId -> Integer -> RankId
generateRankId (AssetClass (cs,TokenName bs)) i = AssetClass (cs,  TokenName (takeByteString 28 $ blake2b_256 (bs <> (serialiseData . toBuiltinData) i)))
--  TODO : Replace with builtins 
{-# INLINEABLE generateRankId #-}


data Profile
  = Profile
  { profileId :: ProfileId,
    profileType :: ProfileType,
    currentRank :: Maybe RankId, -- ˆ Organisations have no rank
    protocolParams :: ProtocolParams
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






mkPractitionerProfile :: ProfileId -> POSIXTime -> ProtocolParams -> (Profile, RankData)
mkPractitionerProfile  profileId creationDate protocolParams =
      let 
        rankValue = RankValue White 0
        firstRank = RankData
                      { rankId = generateRankId profileId (rankToInt rankValue),
                        rankValue = rankValue,
                        rankAchievedByProfileId = profileId,
                        rankAwardedByProfileIds = [],
                        rankAchievementDate = creationDate,
                        rankPreviousRankId = Nothing
                      }
        profile = Profile
                    { profileId = profileId,
                      profileType = Practitioner,
                      currentRank = Just (generateRankId profileId 0),
                      protocolParams = protocolParams
                    }
      in (profile, firstRank)


mkOrganizationProfile :: ProfileId -> ProtocolParams -> Profile
mkOrganizationProfile profileId protocolParams =
  Profile
    { profileId = profileId,
      profileType = Organization,
      currentRank = Nothing,
      protocolParams = protocolParams
    }

-------------------------------------------------------------------------------

-- * Promotion

-------------------------------------------------------------------------------
type PromotionId = AssetClass

data Promotion
  = Promotion
  { promotionId :: RankId,
    promotionAwardedTo :: ProfileId,
    promotionAwardedBy :: [ProfileId],
    promotionAchievementDate :: POSIXTime,
    promotionRank :: RankValue
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''Promotion [('Promotion, 0)]

mkPromotion :: ProfileId -> [ProfileId] -> POSIXTime -> RankValue -> Promotion
mkPromotion promotionAwardedTo promotionAwardedBy promotionAchievementDate promotionRank  =
  Promotion
    { promotionId = generateRankId promotionAwardedTo (rankToInt promotionRank),
      promotionAwardedTo = promotionAwardedTo,
      promotionAwardedBy = promotionAwardedBy,
      promotionAchievementDate = promotionAchievementDate,
      promotionRank = promotionRank
    }


promotionToRank :: Promotion -> RankId -> RankData
promotionToRank promotion previousRankId = RankData
  { rankId = promotionId promotion,
    rankValue = promotionRank promotion,
    rankAchievedByProfileId = promotionAwardedTo promotion,
    rankAwardedByProfileIds = promotionAwardedBy promotion,
    rankAchievementDate = promotionAchievementDate promotion,
    rankPreviousRankId = Just previousRankId 
  }

updateProfileRank :: Profile -> RankData -> Profile
updateProfileRank Profile{..} rankData = Profile
  { profileId = profileId,
    profileType = profileType,
    currentRank = Just (rankId rankData),
    protocolParams = protocolParams
  }

promoteProfile :: Profile -> Promotion -> (Profile, RankData)
promoteProfile profile@Profile{..} promotion = case currentRank of
  Just currentRankId -> let 
                          newRankData = promotionToRank promotion currentRankId
                          updatedProfile = updateProfileRank profile newRankData
                         in (updatedProfile, newRankData)
  Nothing -> traceError "Profile has no rank"



-------------------------------------------------------------------------------

-- * Membership

-------------------------------------------------------------------------------
