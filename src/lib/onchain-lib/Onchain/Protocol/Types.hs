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
data ProtocolParams = ProtocolParams
  { ranksValidatorScriptHash :: ScriptHash,
    profilesValidatorScriptHash :: ScriptHash,
    membershipsValidatorScriptHash :: ScriptHash,
    achievementsValidatorScriptHash :: ScriptHash,
    oracleToken :: AssetClass
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''ProtocolParams [('ProtocolParams, 0)]
makeLift ''ProtocolParams

-------------------------------------------------------------------------------

-- * Oracle Parameters

-------------------------------------------------------------------------------

-- | Fee configuration for protocol operations.
-- When set, users must include a fee payment output in their transactions.
data FeeConfig = FeeConfig
  { -- | Address that collects fees
    fcFeeAddress :: Address,
    -- | Fee in lovelace for creating a profile
    fcProfileCreationFee :: Integer,
    -- | Fee in lovelace for creating a promotion
    fcPromotionFee :: Integer,
    -- | Fee in lovelace for creating a membership history
    fcMembershipHistoryFee :: Integer,
    -- | Fee in lovelace for creating a membership interval
    fcMembershipIntervalFee :: Integer,
    -- | Fee in lovelace for creating an achievement
    fcAchievementFee :: Integer
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''FeeConfig [('FeeConfig, 0)]

-- | Mutable operational parameters stored in the oracle UTxO datum.
-- Read as a reference input by the MintingPolicy at execution time.
data OracleParams = OracleParams
  { -- | Admin public key hash (can rotate itself)
    opAdminPkh :: PubKeyHash,
    -- | Protocol pause switch
    opPaused :: Bool,
    -- | Optional fee configuration
    opFeeConfig :: Maybe FeeConfig,
    -- | Minimum lovelace for protocol state outputs (used on-chain only from oracle)
    opMinUTxOValue :: Integer
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''OracleParams [('OracleParams, 0)]

-- | On-chain representation of an admin action on the oracle.
-- Mirrors 'DomainTypes.Core.Actions.AdminActionType' for use in the Oracle redeemer.
data OracleAdminAction
  = OraclePause
  | OracleUnpause
  | OracleSetFees (Maybe FeeConfig)
  | OracleSetMinUTxOValue Integer
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''OracleAdminAction [('OraclePause, 0), ('OracleUnpause, 1), ('OracleSetFees, 2), ('OracleSetMinUTxOValue, 3)]

-- | Apply an admin action to current oracle params to obtain the new params.
-- Same logic as off-chain 'TxBuilding.Operations.applyAdminAction'.
{-# INLINEABLE applyOracleAdminAction #-}
applyOracleAdminAction :: OracleAdminAction -> OracleParams -> OracleParams
applyOracleAdminAction OraclePause params = params {opPaused = True}
applyOracleAdminAction OracleUnpause params = params {opPaused = False}
applyOracleAdminAction (OracleSetFees mFeeConfig) params = params {opFeeConfig = mFeeConfig}
applyOracleAdminAction (OracleSetMinUTxOValue lovelace) params = params {opMinUTxOValue = lovelace}

-------------------------------------------------------------------------------

-- * Type Aliases

-------------------------------------------------------------------------------

type RankId = AssetClass

type ProfileId = AssetClass

type MembershipHistoriesListNodeId = AssetClass

type MembershipHistoryId = AssetClass

type MembershipIntervalId = AssetClass

type AchievementId = AssetClass

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
  { -- | Practitioner ProfileId
    membershipHistoryPractitionerId :: ProfileId,
    -- | Organization ProfileId
    membershipHistoryOrganizationId :: ProfileId,
    -- | Sequential number of the head (most recent) interval (0-based).
    -- The head interval's NFT ID is derived at runtime via
    -- @deriveMembershipIntervalId (deriveMembershipHistoryIdFromHistory h) headNumber@.
    membershipHistoryIntervalsHeadNumber :: Integer
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''OnchainMembershipHistory [('OnchainMembershipHistory, 0)]

data OnchainMembershipInterval = OnchainMembershipInterval
  { -- | Start date of the membership interval
    membershipIntervalStartDate :: POSIXTime,
    -- | End date of the membership interval
    membershipIntervalEndDate :: Maybe POSIXTime,
    -- | Is the membership interval accepted
    membershipIntervalIsAck :: Bool,
    -- | Sequential number within the history (0-based).
    -- The interval's NFT ID is derived at runtime via
    -- @deriveMembershipIntervalId historyId number@.
    membershipIntervalNumber :: Integer,
    -- | Practitioner ProfileId (for User NFT derivation in AcceptInterval)
    membershipIntervalPractitionerId :: ProfileId
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

-------------------------------------------------------------------------------

-- * Achievement

-------------------------------------------------------------------------------

data OnchainAchievement = OnchainAchievement
  { achievementId :: AchievementId,
    achievementAwardedBy :: ProfileId,
    achievementAwardedTo :: ProfileId,
    achievementDate :: POSIXTime,
    achievementIsAccepted :: Bool
  }
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''OnchainAchievement [('OnchainAchievement, 0)]