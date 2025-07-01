-- Required for `makeLift`:
{-# LANGUAGE MultiParamTypeClasses #-}
-- Required for `makeLift`:
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Onchain.Protocol where

import GHC.Generics (Generic)
import PlutusLedgerApi.V1 (Lovelace, POSIXTime, ScriptHash, TokenName (..))
import PlutusLedgerApi.V1.Value (AssetClass (..))
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Builtins (serialiseData)
import PlutusTx.Prelude
import Prelude qualified

-------------------------------------------------------------------------------

-- * Protocol Parameters

-------------------------------------------------------------------------------
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
type RankId = AssetClass

type ProfileId = AssetClass

data ProfileType
  = Practitioner
  | Organization
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''ProfileType [('Practitioner, 0), ('Organization, 1)]

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

data Rank
  = Rank
  { rankId :: RankId,
    rankNumber :: Integer,
    rankAchievedByProfileId :: ProfileId,
    rankAwardedByProfileIds :: [ProfileId],
    rankAchievementDate :: POSIXTime,
    rankPreviousRankId :: Maybe RankId
  } | PendingRank
  { pendingRankId :: RankId,
    pendingRankNumber :: Integer,
    pendingRankAwardedTo :: ProfileId,
    pendingRankAwardedBy :: [ProfileId],
    pendingRankAchievementDate :: POSIXTime
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''Rank [('Rank, 0), ('PendingRank, 1)]



-------------------------------------------------------------------------------

-- * Protocol Logic

-------------------------------------------------------------------------------


mkPendingRank :: ProfileId -> [ProfileId] -> POSIXTime -> Integer -> Rank
mkPendingRank awardedTo awardedBy achievementDate rankNumber =
  PendingRank
    { pendingRankId = generateRankId awardedTo rankNumber,
      pendingRankAwardedTo = awardedTo,
      pendingRankAwardedBy = awardedBy,
      pendingRankAchievementDate = achievementDate,
      pendingRankNumber = rankNumber
    }

acceptRank :: Rank -> RankId -> Rank
acceptRank (Rank {}) _ = traceError "Cannot accept a rank that is not pending"
acceptRank PendingRank {..} previousRankId =
  Rank
    { rankId = pendingRankId,
      rankNumber = pendingRankNumber,
      rankAchievedByProfileId = pendingRankAwardedTo,
      rankAwardedByProfileIds = pendingRankAwardedBy,
      rankAchievementDate = pendingRankAchievementDate,
      rankPreviousRankId = Just previousRankId
    }

updateProfileRank :: Profile -> Rank -> Profile
updateProfileRank Profile {..} rank =
  Profile
    { profileId = profileId,
      profileType = profileType,
      currentRank = Just (rankId rank),
      protocolParams = protocolParams
    }

promoteProfile :: Profile -> Rank -> (Profile, Rank)
promoteProfile profile@Profile {..} rank = case currentRank of
  Just currentRankId ->
    let newRank = acceptRank rank currentRankId
        updatedProfile = updateProfileRank profile newRank
     in (updatedProfile, newRank)
  Nothing -> traceError "Profile has no rank"


mkPractitionerProfile :: ProfileId -> POSIXTime -> ProtocolParams -> (Profile, Rank)
mkPractitionerProfile profileId creationDate protocolParams =
  let rankNumber = 0
      firstRank =
        Rank
          { rankId = generateRankId profileId rankNumber,
            rankNumber = rankNumber,
            rankAchievedByProfileId = profileId,
            rankAwardedByProfileIds = [],
            rankAchievementDate = creationDate,
            rankPreviousRankId = Nothing
          }
      profile =
        Profile
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


{-# INLINEABLE generateRankId #-}
generateRankId :: ProfileId -> Integer -> RankId
generateRankId (AssetClass (cs, TokenName bs)) i = AssetClass (cs, TokenName (takeByteString 28 $ blake2b_256 (bs <> (serialiseData . toBuiltinData) i)))

--  TODO : Replace with builtins

-- -------------------------------------------------------------------------------

-- -- * Rank

-- -------------------------------------------------------------------------------
-- type Stripe = Integer

-- data Belt
--   = White
--   | Blue
--   | Purple
--   | Brown
--   | Black
--   | RedAndBlack
--   | RedAndWhite
--   | Red
--   deriving stock (Generic, Prelude.Show)
--   deriving anyclass (HasBlueprintDefinition)

-- makeIsDataSchemaIndexed ''Belt [('White, 0), ('Blue, 1), ('Purple, 2), ('Brown, 3), ('Black, 4), ('RedAndBlack, 5), ('RedAndWhite, 6), ('Red, 7)]

-- instance Eq Belt where
--   (==) :: Belt -> Belt -> Bool
--   (==) x y = baseRank x == baseRank y

-- baseRank :: Belt -> Integer
-- baseRank belt = case belt of
--   White -> 0
--   Blue -> 5
--   Purple -> 10
--   Brown -> 15
--   Black -> 20
--   RedAndBlack -> 27
--   RedAndWhite -> 28
--   Red -> 29

-- data RankValue
--   = RankValue Belt Stripe
--   deriving stock (Generic, Prelude.Show)
--   deriving anyclass (HasBlueprintDefinition)

-- makeIsDataSchemaIndexed ''RankValue [('RankValue, 0)]

-- instance Eq RankValue where
--   (==) :: RankValue -> RankValue -> Bool
--   (==) x y = rankToInt x == rankToInt y

-- instance Ord RankValue where
--   compare :: RankValue -> RankValue -> Ordering
--   compare x y = compare (rankToInt x) (rankToInt y)

-- rankToInt :: RankValue -> Integer
-- rankToInt (RankValue belt stripe) = baseRank belt + stripe