-- Required for `makeLift`:
{-# LANGUAGE MultiParamTypeClasses #-}
-- Required for `makeLift`:
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Onchain.Protocol where

import GHC.Generics (Generic)
import PlutusLedgerApi.V1.Value (AssetClass (..))
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Builtins (serialiseData)
import PlutusTx.Prelude
import Prelude qualified
import PlutusLedgerApi.V3
import Onchain.Utils
import Onchain.CIP68 (CIP68Datum (extra))



-------------------------------------------------------------------------------

-- * Protocol Parameters

-------------------------------------------------------------------------------
data ProtocolParams = ProtocolParams
  { ranksValidatorScriptHash :: ScriptHash,
    profilesValidatorScriptHash :: ScriptHash,
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
    rankAwardedByProfileId :: ProfileId,
    rankAchievementDate :: POSIXTime,
    rankPreviousRankId :: Maybe RankId,
    rankProtocolParams :: ProtocolParams
  } | PendingRank
  { pendingRankId :: RankId,
    pendingRankNumber :: Integer,
    pendingRankAwardedTo :: ProfileId,
    pendingRankAwardedBy :: ProfileId,
    pendingRankAchievementDate :: POSIXTime,
    pendingRankProtocolParams :: ProtocolParams
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''Rank [('Rank, 0), ('PendingRank, 1)]



-------------------------------------------------------------------------------

-- * Protocol Logic

-------------------------------------------------------------------------------


mkPendingRank :: ProfileId -> ProfileId -> POSIXTime -> Integer -> ProtocolParams -> Rank
mkPendingRank awardedTo awardedBy achievementDate rankNumber protocolParams =
  PendingRank
    { pendingRankId = generateRankId awardedTo rankNumber,
      pendingRankAwardedTo = awardedTo,
      pendingRankAwardedBy = awardedBy,
      pendingRankAchievementDate = achievementDate,
      pendingRankNumber = rankNumber,
      pendingRankProtocolParams = protocolParams
    }

acceptRank :: Rank -> RankId -> Rank
acceptRank (Rank {}) _ = traceError "Cannot accept a rank that is not pending"
acceptRank PendingRank {..} previousRankId =
  Rank
    { rankId = pendingRankId,
      rankNumber = pendingRankNumber,
      rankAchievedByProfileId = pendingRankAwardedTo,
      rankAwardedByProfileId = pendingRankAwardedBy,
      rankAchievementDate = pendingRankAchievementDate,
      rankPreviousRankId = Just previousRankId,
      rankProtocolParams = pendingRankProtocolParams
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
            rankAwardedByProfileId = profileId,
            rankAchievementDate = creationDate,
            rankPreviousRankId = Nothing,
            rankProtocolParams = protocolParams
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




-------------------------------------------------------------------------------

-- * Protocol Onchain Helpers

-------------------------------------------------------------------------------


unsafeGetRankDatumAndValue :: RankId -> Address -> [TxInInfo] -> (Value, Rank)
unsafeGetRankDatumAndValue ac addr txins =
  let (v, b) = unsafeGetCurrentStateDatumAndValue ac addr txins
   in (v, unsafeFromBuiltinData b)
{-# INLINEABLE unsafeGetRankDatumAndValue #-}

unsafeGetProfileDatumAndValue :: RankId -> Address -> [TxInInfo] -> (Value, Profile)
unsafeGetProfileDatumAndValue ac addr txins =
  let (v, b) = unsafeGetCurrentStateDatumAndValue ac addr txins
   in (v, extra (unsafeFromBuiltinData b :: CIP68Datum Profile))
{-# INLINEABLE unsafeGetProfileDatumAndValue #-}

