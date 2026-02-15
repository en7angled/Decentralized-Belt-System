{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Core on-chain data types for the BJJ Belt protocol.
module Onchain.Protocol.Types where

import GHC.Generics (Generic)
import Onchain.LinkedList (NodeDatum (..))
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusLedgerApi.V3 (POSIXTime, ScriptHash)
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude
import Prelude qualified

-------------------------------------------------------------------------------

-- * Protocol Parameters

-------------------------------------------------------------------------------

-- | Protocol parameters containing validator script hashes.
-- The 3-tuple layout is intentional for on-chain size minimization:
--
--   * Position 0: RanksValidator script hash
--   * Position 1: ProfilesValidator script hash
--   * Position 2: MembershipsValidator script hash
newtype ProtocolParams = ProtocolParams (ScriptHash, ScriptHash, ScriptHash)
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''ProtocolParams [('ProtocolParams, 0)]
makeLift ''ProtocolParams

-- | Extract the ProfilesValidator script hash (position 1).
profilesValidatorScriptHash :: ProtocolParams -> ScriptHash
profilesValidatorScriptHash (ProtocolParams (_, p, _)) = p

-- | Extract the RanksValidator script hash (position 0).
ranksValidatorScriptHash :: ProtocolParams -> ScriptHash
ranksValidatorScriptHash (ProtocolParams (r, _, _)) = r

-- | Extract the MembershipsValidator script hash (position 2).
membershipsValidatorScriptHash :: ProtocolParams -> ScriptHash
membershipsValidatorScriptHash (ProtocolParams (_, _, m)) = m

-------------------------------------------------------------------------------

-- * Type Aliases

-------------------------------------------------------------------------------

type RankId = AssetClass

type ProfileId = AssetClass

type MembershipHistoriesListNodeId = AssetClass

type MembershipHistoryId = AssetClass

type MembershipIntervalId = AssetClass

-------------------------------------------------------------------------------

-- * Profiles

-------------------------------------------------------------------------------

data OnchainProfileType
  = Practitioner
  | Organization
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''OnchainProfileType [('Practitioner, 0), ('Organization, 1)]

data OnchainProfile
  = OnchainProfile
  { profileId :: ProfileId,
    profileType :: OnchainProfileType,
    currentRank :: Maybe RankId, -- ˆ Organisations have no rank
    protocolParams :: ProtocolParams
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''OnchainProfile [('OnchainProfile, 0)]

-------------------------------------------------------------------------------

-- * Ranks

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
      }
  | Promotion
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

-- * Memberships

-------------------------------------------------------------------------------

data OnchainMembershipHistory = OnchainMembershipHistory
  { membershipHistoryId :: MembershipHistoryId, -- ^ AssetClass of the membership history NFT
    membershipHistoryPractitionerId :: ProfileId, -- ^ Practitioner ProfileId
    membershipHistoryOrganizationId :: ProfileId, -- ^ Organization ProfileId
    membershipHistoryIntervalsHeadId :: MembershipIntervalId -- ^ AssetClass of the head of the intervals list
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''OnchainMembershipHistory [('OnchainMembershipHistory, 0)]

data OnchainMembershipInterval = OnchainMembershipInterval
  { membershipIntervalId :: MembershipIntervalId, -- ^ AssetClass of the membership interval NFT
    membershipIntervalStartDate :: POSIXTime, -- ^ Start date of the membership interval
    membershipIntervalEndDate :: Maybe POSIXTime, -- ^ End date of the membership interval
    membershipIntervalIsAck :: Bool, -- ^ Is the membership interval accepted
    membershipIntervalPrevId :: Maybe MembershipIntervalId, -- ^ Maybe AssetClass of the previous interval, Nothing for the first interval
    membershipIntervalNumber :: Integer, -- ^ Number of the membership interval (starts from 0)
    membershipIntervalPractitionerId :: ProfileId -- ^ Practitioner ProfileId
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''OnchainMembershipInterval [('OnchainMembershipInterval, 0)]

-------------------------------------------------------------------------------

-- * Membership Histories List

-------------------------------------------------------------------------------

data MembershipHistoriesListNode
  = MembershipHistoriesListNode
  { organizationId :: ProfileId,
    nodeInfo :: NodeDatum (Maybe OnchainMembershipHistory)
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''MembershipHistoriesListNode [('MembershipHistoriesListNode, 0)]

-- | Sum type for datums stored at the MembershipsValidator address.
-- Distinguishes between linked list nodes (membership histories) and interval records.
data MembershipDatum
  = ListNodeDatum MembershipHistoriesListNode
  | IntervalDatum OnchainMembershipInterval
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''MembershipDatum [('ListNodeDatum, 0), ('IntervalDatum, 1)]
