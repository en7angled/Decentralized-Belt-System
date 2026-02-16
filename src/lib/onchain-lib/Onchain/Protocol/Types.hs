{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

-- | Core on-chain data types for the BJJ Belt protocol.
module Onchain.Protocol.Types where

import GHC.Generics (Generic)
import Onchain.LinkedList (NodeDatum (..))
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusLedgerApi.V3 (Address, POSIXTime, PubKeyHash, ScriptHash)
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Prelude
import Prelude qualified

-------------------------------------------------------------------------------

-- * Protocol Parameters

-------------------------------------------------------------------------------

-- | Protocol parameters containing validator script hashes and the oracle token.
-- Baked into the MintingPolicy at compile time and embedded in datums for
-- cross-validator communication.
--
-- Field layout:
--   * Field 0: RanksValidator script hash
--   * Field 1: ProfilesValidator script hash
--   * Field 2: MembershipsValidator script hash
--   * Field 3: Oracle NFT AssetClass (identifies the oracle UTxO)
data ProtocolParams = ProtocolParams ScriptHash ScriptHash ScriptHash AssetClass
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''ProtocolParams [('ProtocolParams, 0)]
makeLift ''ProtocolParams

-- | Extract the RanksValidator script hash.
{-# INLINEABLE ranksValidatorScriptHash #-}
ranksValidatorScriptHash :: ProtocolParams -> ScriptHash
ranksValidatorScriptHash (ProtocolParams r _ _ _) = r

-- | Extract the ProfilesValidator script hash.
{-# INLINEABLE profilesValidatorScriptHash #-}
profilesValidatorScriptHash :: ProtocolParams -> ScriptHash
profilesValidatorScriptHash (ProtocolParams _ p _ _) = p

-- | Extract the MembershipsValidator script hash.
{-# INLINEABLE membershipsValidatorScriptHash #-}
membershipsValidatorScriptHash :: ProtocolParams -> ScriptHash
membershipsValidatorScriptHash (ProtocolParams _ _ m _) = m

-- | Extract the oracle NFT AssetClass used to locate the oracle UTxO.
{-# INLINEABLE oracleToken #-}
oracleToken :: ProtocolParams -> AssetClass
oracleToken (ProtocolParams _ _ _ o) = o

-------------------------------------------------------------------------------

-- * Oracle Parameters

-------------------------------------------------------------------------------

-- | Fee configuration for protocol operations.
-- When set, users must include a fee payment output in their transactions.
data FeeConfig = FeeConfig
  { fcFeeAddress :: Address           -- ^ Address that collects fees
  , fcProfileCreationFee :: Integer   -- ^ Fee in lovelace for creating a profile
  , fcPromotionFee :: Integer         -- ^ Fee in lovelace for creating a promotion
  , fcMembershipFee :: Integer        -- ^ Fee in lovelace for membership operations
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''FeeConfig [('FeeConfig, 0)]

-- | Mutable operational parameters stored in the oracle UTxO datum.
-- Read as a reference input by the MintingPolicy at execution time.
data OracleParams = OracleParams
  { opAdminPkh :: PubKeyHash         -- ^ Admin public key hash (can rotate itself)
  , opPaused :: Bool                  -- ^ Protocol pause switch
  , opFeeConfig :: Maybe FeeConfig   -- ^ Optional fee configuration
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''OracleParams [('OracleParams, 0)]

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
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
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
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
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
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
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
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
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
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''MembershipHistoriesListNode [('MembershipHistoriesListNode, 0)]

-- | Sum type for datums stored at the MembershipsValidator address.
-- Distinguishes between linked list nodes (membership histories) and interval records.
data MembershipDatum
  = ListNodeDatum MembershipHistoriesListNode
  | IntervalDatum OnchainMembershipInterval
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''MembershipDatum [('ListNodeDatum, 0), ('IntervalDatum, 1)]
