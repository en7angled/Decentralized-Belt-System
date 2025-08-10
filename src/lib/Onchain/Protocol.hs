{-# LANGUAGE DerivingVia #-}
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
    ( TokenName(TokenName),
      Address,
      Value,
      ScriptHash,
      POSIXTime,
      TxInInfo )
import Onchain.Utils
import Onchain.CIP68 (CIP68Datum (extra, CIP68Datum))




-------------------------------------------------------------------------------

-- * Protocol Parameters

-------------------------------------------------------------------------------
newtype ProtocolParams = ProtocolParams (ScriptHash, ScriptHash)
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''ProtocolParams [('ProtocolParams, 0)]
makeLift ''ProtocolParams

profilesValidatorScriptHash :: ProtocolParams -> ScriptHash
profilesValidatorScriptHash (ProtocolParams (_, p)) = p

ranksValidatorScriptHash :: ProtocolParams -> ScriptHash
ranksValidatorScriptHash (ProtocolParams (r, _)) = r


-------------------------------------------------------------------------------

-- * OnchainProfile

-------------------------------------------------------------------------------
type RankId = AssetClass


type ProfileId = AssetClass

data OnChainProfileType
  = Practitioner
  | Organization
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''OnChainProfileType [('Practitioner, 0), ('Organization, 1)]

data OnchainProfile
  = OnchainProfile
  { profileId :: ProfileId,
    profileType :: OnChainProfileType,
    currentRank :: Maybe RankId, -- ˆ Organisations have no rank
    protocolParams :: ProtocolParams
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''OnchainProfile [('OnchainProfile, 0)]




-------------------------------------------------------------------------------

-- * OnchainRank

-------------------------------------------------------------------------------

data OnchainRank
  = Rank
  { rankId :: RankId,
    rankNumber :: Integer,
    rankAchievedByProfileId :: ProfileId,
    rankAwardedByProfileId :: ProfileId,
    rankAchievementDate :: POSIXTime,
    rankPreviousRankId :: Maybe RankId,
    rankProtocolParams :: ProtocolParams
  } | Promotion
  { promotionId :: RankId,
    promotionRankNumber :: Integer,
    promotionAwardedTo :: ProfileId,
    promotionAwardedBy :: ProfileId,
    promotionAchievementDate :: POSIXTime,
    promotionProtocolParams :: ProtocolParams
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''OnchainRank [('Rank, 0), ('Promotion, 1)]



-------------------------------------------------------------------------------

-- * Protocol Logic

-------------------------------------------------------------------------------


mkPendingRank :: ProfileId -> ProfileId -> POSIXTime -> Integer -> ProtocolParams -> OnchainRank
mkPendingRank awardedTo awardedBy achievementDate rankNumber protocolParams =
  Promotion
    { promotionId = generateRankId awardedTo rankNumber,
      promotionAwardedTo = awardedTo,
      promotionAwardedBy = awardedBy,
      promotionAchievementDate = achievementDate,
      promotionRankNumber = rankNumber,
      promotionProtocolParams = protocolParams
    }

acceptRank :: OnchainRank -> RankId -> OnchainRank
acceptRank (Rank {}) _ = traceError "Cannot accept a rank that is not pending"
acceptRank Promotion {..} previousRankId =
  Rank
    { rankId = promotionId,
      rankNumber = promotionRankNumber,
      rankAchievedByProfileId = promotionAwardedTo,
      rankAwardedByProfileId = promotionAwardedBy,
      rankAchievementDate = promotionAchievementDate,
      rankPreviousRankId = Just previousRankId,
      rankProtocolParams = promotionProtocolParams
    }

updateProfileRank :: OnchainProfile -> OnchainRank -> OnchainProfile
updateProfileRank OnchainProfile {..} rank =
  OnchainProfile
    { profileId = profileId,
      profileType = profileType,
      currentRank = Just (rankId rank),
      protocolParams = protocolParams
    }

promoteProfile :: CIP68Datum OnchainProfile -> OnchainRank -> (CIP68Datum OnchainProfile, OnchainRank)
promoteProfile (CIP68Datum metadata version profile@OnchainProfile {..}) promotion = case currentRank of
  Just currentRankId ->
    let newRank = acceptRank promotion currentRankId
        updatedProfile = updateProfileRank profile newRank
     in (CIP68Datum  metadata version updatedProfile, newRank)
  Nothing -> traceError "OnchainProfile has no rank"


mkPractitionerProfile :: ProfileId -> POSIXTime -> ProtocolParams -> Integer -> (OnchainProfile, OnchainRank)
mkPractitionerProfile profileId creationDate protocolParams rankNumber =
    let newRankId = generateRankId profileId rankNumber
        firstRank =
         Rank
          { rankId = newRankId,
            rankNumber = rankNumber,
            rankAchievedByProfileId = profileId,
            rankAwardedByProfileId = profileId,
            rankAchievementDate = creationDate,
            rankPreviousRankId = Nothing,
            rankProtocolParams = protocolParams
          }
        profile =
          OnchainProfile
            { profileId = profileId,
              profileType = Practitioner,
              currentRank = Just newRankId,
              protocolParams = protocolParams
            }
   in (profile, firstRank)


mkOrganizationProfile :: ProfileId -> ProtocolParams -> OnchainProfile
mkOrganizationProfile profileId protocolParams =
  OnchainProfile
    { profileId = profileId,
      profileType = Organization,
      currentRank = Nothing,
      protocolParams = protocolParams
    }

getCurrentRankId :: OnchainProfile ->  RankId
getCurrentRankId (OnchainProfile _ Practitioner (Just rankId) _ ) = rankId
getCurrentRankId _ = traceError "OnchainProfile has no rank"

{-# INLINEABLE generateRankId #-}
generateRankId :: ProfileId -> Integer -> RankId
generateRankId (AssetClass (cs, TokenName bs)) i = AssetClass (cs, TokenName (takeByteString 28 $ blake2b_256 (bs <> (serialiseData . toBuiltinData) i)))

--  TODO : Replace with builtins




-------------------------------------------------------------------------------

-- * Protocol Onchain Helpers

-------------------------------------------------------------------------------


unsafeGetRankDatumAndValue :: RankId -> Address -> [TxInInfo] -> (Value, OnchainRank)
unsafeGetRankDatumAndValue ac addr txins =
  let (v, b) = unsafeGetCurrentStateDatumAndValue ac addr txins
   in (v, unsafeFromBuiltinData b)
{-# INLINEABLE unsafeGetRankDatumAndValue #-}

unsafeGetProfileDatumAndValue :: RankId -> Address -> [TxInInfo] -> (Value, OnchainProfile)
unsafeGetProfileDatumAndValue ac addr txins =
  let (v, b) = unsafeGetCurrentStateDatumAndValue ac addr txins
   in (v, extra (unsafeFromBuiltinData b :: CIP68Datum OnchainProfile))
{-# INLINEABLE unsafeGetProfileDatumAndValue #-}

