{-# LANGUAGE DerivingVia #-}
-- Required for `makeLift`:
{-# LANGUAGE MultiParamTypeClasses #-}
-- Required for `makeLift`:
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use &&" #-}

module Onchain.Protocol where

import GHC.Generics (Generic)
import Onchain.CIP68 (CIP68Datum (CIP68Datum, extra))
import Onchain.LinkedList (NodeDatum (..))
import Onchain.LinkedList qualified as LinkedList
import Onchain.Utils (checkAndGetCurrentStateDatumAndValue, nameFromTxOutRef)
import PlutusLedgerApi.V1.Value (AssetClass (..))
import PlutusLedgerApi.V3
  ( Address,
    CurrencySymbol,
    POSIXTime,
    ScriptHash,
    TokenName (TokenName),
    TxInInfo,
    TxOutRef,
    Value,
  )
import PlutusTx
import PlutusTx.Blueprint
import PlutusTx.Builtins
import PlutusTx.Prelude
import Prelude qualified

-------------------------------------------------------------------------------

-- * Protocol Parameters

-------------------------------------------------------------------------------
newtype ProtocolParams = ProtocolParams (ScriptHash, ScriptHash, ScriptHash)
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''ProtocolParams [('ProtocolParams, 0)]
makeLift ''ProtocolParams

profilesValidatorScriptHash :: ProtocolParams -> ScriptHash
profilesValidatorScriptHash (ProtocolParams (_, p, _)) = p

ranksValidatorScriptHash :: ProtocolParams -> ScriptHash
ranksValidatorScriptHash (ProtocolParams (r, _, _)) = r

membershipsValidatorScriptHash :: ProtocolParams -> ScriptHash
membershipsValidatorScriptHash (ProtocolParams (_, _, m)) = m

-------------------------------------------------------------------------------

-- * OnchainProfile

-------------------------------------------------------------------------------
type RankId = AssetClass

type ProfileId = AssetClass

type MembershipHistoriesListNodeId = AssetClass

type MembershipHistoryId = AssetClass

type MembershipIntervalId = AssetClass

data OnChainProfileType
  = Practitioner
  | Organization
  deriving stock (Generic, Prelude.Show, Prelude.Eq)
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

-- * MembershipHistory

-------------------------------------------------------------------------------

data OnchainMembershipHistory = OnchainMembershipHistory
  { -- | AssetClass of the membership history NFT
    membershipHistoryId :: MembershipHistoryId,
    -- | Practitioner ProfileId
    membershipHistoryPractitionerId :: ProfileId,
    -- | Organization ProfileId
    membershipHistoryOrganizationId :: ProfileId,
    -- |  AssetClass of the head of the intervals list
    membershipHistoryIntervalsHeadId :: MembershipIntervalId
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''OnchainMembershipHistory [('OnchainMembershipHistory, 0)]

-------------------------------------------------------------------------------

-- * MembershipInterval

-------------------------------------------------------------------------------

data OnchainMembershipInterval = OnchainMembershipInterval
  { -- | AssetClass of the membership interval NFT
    membershipIntervalId :: MembershipIntervalId,
    -- | Start date of the membership interval
    membershipIntervalStartDate :: POSIXTime,
    -- | End date of the membership interval
    membershipIntervalEndDate :: Maybe POSIXTime,
    -- | Is the membership interval accepted
    membershipIntervalIsAck :: Bool,
    -- | Maybe AssetClass of the previous interval, Nothing for the first interval
    membershipIntervalPrevId :: Maybe MembershipIntervalId,
    -- | Number of the membership interval (starts from 0)
    membershipIntervalNumber :: Integer,
    -- | Practitioner ProfileId
    membershipIntervalPractitionerId :: ProfileId
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''OnchainMembershipInterval [('OnchainMembershipInterval, 0)]

-------------------------------------------------------------------------------

-- * Onchain Membership Histories List

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

-------------------------------------------------------------------------------

-- * Protocol Logic

-------------------------------------------------------------------------------

-- | Initialize an empty membership histories list
{-# INLINEABLE initEmptyMembershipHistoriesList #-}
initEmptyMembershipHistoriesList :: ProfileId -> MembershipHistoriesListNode
initEmptyMembershipHistoriesList organizationId =
  MembershipHistoriesListNode
    { organizationId = organizationId,
      nodeInfo =
        NodeDatum
          { nodeKey = Nothing, -- Root
            nextNodeKey = Nothing, -- Empty list
            nodeData = Nothing -- No memberships histories yet
          }
    }

{-# INLINEABLE mkMembershipHistoriesListNode #-}
mkMembershipHistoriesListNode :: OnchainMembershipHistory -> Maybe MembershipHistoriesListNodeId -> MembershipHistoriesListNode
mkMembershipHistoriesListNode history maybeNextNodeId =
  let nodeInfo =
        NodeDatum
          { nodeKey = Just (membershipHistoryPractitionerId history),
            nextNodeKey = maybeNextNodeId,
            nodeData = Just history
          }
   in MembershipHistoriesListNode
        { organizationId = membershipHistoryOrganizationId history,
          nodeInfo = nodeInfo
        }

{-# INLINEABLE unsafeGetMembershipHistory #-}
unsafeGetMembershipHistory :: MembershipHistoriesListNode -> OnchainMembershipHistory
unsafeGetMembershipHistory node = case nodeData (nodeInfo node) of
  Just history -> history
  Nothing -> traceError "Root node has no history"




-- | Derive a unique membership history ID from two profile IDs
{-# INLINEABLE deriveMembershipHistoryId #-}
deriveMembershipHistoryId :: ProfileId -> ProfileId -> MembershipHistoryId
deriveMembershipHistoryId (AssetClass (cs, TokenName orgId)) (AssetClass (_cs, TokenName prId)) =
  AssetClass (cs, TokenName (blake2b_224 (orgId `appendByteString` prId)))

-- | Derive a unique membership interval ID from a membership history ID and a number
{-# INLINEABLE deriveMembershipIntervalId #-}
deriveMembershipIntervalId :: MembershipHistoryId -> Integer -> MembershipIntervalId
deriveMembershipIntervalId (AssetClass (cs, TokenName bs)) i =
  AssetClass (cs, TokenName (blake2b_224 (bs `appendByteString` integerToByteString BigEndian 0 i)))

-- | Derive a unique membership histories list ID from a profile ID as the hash of the profile ID
{-# INLINEABLE deriveMembershipHistoriesListId #-}
deriveMembershipHistoriesListId :: ProfileId -> MembershipHistoriesListNodeId
deriveMembershipHistoriesListId (AssetClass (cs, TokenName bs)) = AssetClass (cs, TokenName (blake2b_224 bs))




{-# INLINEABLE insertMembershipHistoryInBetween #-}
insertMembershipHistoryInBetween :: (MembershipHistoriesListNode, MembershipHistoriesListNode, MembershipHistoriesListNode) -> MembershipHistoriesListNode
insertMembershipHistoryInBetween (oldLeftNode, rightNode, insertedNode) =
  let updatedLeftNode = LinkedList.checkInputsAndInsertInBetweenNodes (nodeInfo oldLeftNode, nodeInfo rightNode, nodeInfo insertedNode)
      leftOrganizationId = organizationId oldLeftNode
      rightOrganizationId = organizationId rightNode
      insertedOrganizationId = organizationId insertedNode
      sameOrganization = leftOrganizationId == rightOrganizationId && leftOrganizationId == insertedOrganizationId
   in if sameOrganization
        then
          MembershipHistoriesListNode
            { organizationId = leftOrganizationId,
              nodeInfo = updatedLeftNode
            }
        else traceError "Cannot insert membership history in between nodes from different organizations"

{-# INLINEABLE appendMembershipHistory #-}
appendMembershipHistory :: (MembershipHistoriesListNode, MembershipHistoriesListNode) -> MembershipHistoriesListNode
appendMembershipHistory (lastNode, appendedNode) =
  let updatedLastNode = LinkedList.checkInputsAndAppendNode (nodeInfo lastNode, nodeInfo appendedNode)
      lastOrganizationId = organizationId lastNode
      appendedOrganizationId = organizationId appendedNode
      sameOrganization = lastOrganizationId == appendedOrganizationId
   in if sameOrganization
        then
          MembershipHistoriesListNode
            { organizationId = lastOrganizationId,
              nodeInfo = updatedLastNode
            }
        else traceError "Cannot append membership history to nodes from different organizations"

{-# INLINEABLE updateNodeMembershipHistory #-}
updateNodeMembershipHistory :: MembershipHistoriesListNode -> OnchainMembershipHistory -> MembershipHistoriesListNode
updateNodeMembershipHistory node history =
  MembershipHistoriesListNode
    { organizationId = organizationId node,
      nodeInfo =
        NodeDatum
          { nodeKey = nodeKey (nodeInfo node),
            nextNodeKey = nextNodeKey (nodeInfo node),
            nodeData = Just history
          }
    }

{-# INLINEABLE initMembershipHistory #-}
initMembershipHistory :: AssetClass -> AssetClass -> POSIXTime -> Maybe POSIXTime -> (OnchainMembershipHistory, OnchainMembershipInterval)
initMembershipHistory practitionerId organizationId startDate endDate =
  (membershipHistory, firstInterval)
  where
    membershipHistoryId = deriveMembershipHistoryId organizationId practitionerId
    firstIntervalNumber = 0
    firstMembershipIntervalId = deriveMembershipIntervalId membershipHistoryId firstIntervalNumber
    firstInterval =
      OnchainMembershipInterval
        { membershipIntervalId = firstMembershipIntervalId,
          membershipIntervalStartDate = startDate,
          membershipIntervalEndDate = endDate,
          membershipIntervalIsAck = False, -- False (when interval is created by organization)
          membershipIntervalPrevId = Nothing, -- Nothing for first interval
          membershipIntervalNumber = firstIntervalNumber,
          membershipIntervalPractitionerId = practitionerId
        }
    membershipHistory =
      OnchainMembershipHistory
        { membershipHistoryId = membershipHistoryId,
          membershipHistoryPractitionerId = practitionerId,
          membershipHistoryOrganizationId = organizationId,
          membershipHistoryIntervalsHeadId = firstMembershipIntervalId -- First interval ID
        }

{-# INLINEABLE addMembershipIntervalToHistory #-}
addMembershipIntervalToHistory :: OnchainMembershipHistory -> OnchainMembershipInterval -> POSIXTime -> Maybe POSIXTime -> (OnchainMembershipHistory, OnchainMembershipInterval)
addMembershipIntervalToHistory currentHistory lastInterval startDate maybeEndDate =
  let lastIntervalId = membershipHistoryIntervalsHeadId currentHistory
      lastIntervalPractitionerId = membershipIntervalPractitionerId lastInterval
      lastIntervalNumber = membershipIntervalNumber lastInterval
      newIntervalNumber = lastIntervalNumber + 1
      newIntervalId = deriveMembershipIntervalId (membershipHistoryId currentHistory) newIntervalNumber
      newInterval =
        OnchainMembershipInterval
          { membershipIntervalId = newIntervalId,
            membershipIntervalStartDate = startDate,
            membershipIntervalEndDate = maybeEndDate,
            membershipIntervalIsAck = False,
            membershipIntervalPrevId = Just lastIntervalId,
            membershipIntervalNumber = newIntervalNumber,
            membershipIntervalPractitionerId = lastIntervalPractitionerId
          }
      newHistory = currentHistory {membershipHistoryIntervalsHeadId = newIntervalId}
   in if addMembershipsValidations
        then (newHistory, newInterval)
        else traceError "failed addMembershipIntervalToHistory validations"
  where
    addMembershipsValidations =
      and
        [ traceIfFalse "last interval is not the head of the history" validLastInterval,
          traceIfFalse "last interval is not closed" lastIntervalIsClosed,
          traceIfFalse "last interval is not accepted" lastIntervalIsAccepted
        ]
    validLastInterval = membershipHistoryIntervalsHeadId currentHistory == membershipIntervalId lastInterval -- Required: prevents head-bypass attacks (see OnchainSecurityAudit.md)
    lastIntervalIsAccepted = membershipIntervalIsAck lastInterval
    lastIntervalIsClosed = case membershipIntervalEndDate lastInterval of
      Just lastIntervalEndDate -> startDate >= lastIntervalEndDate
      Nothing -> False -- If the last interval is not closed, we can add a new interval

{-# INLINEABLE updateMembershipIntervalEndDate #-}
updateMembershipIntervalEndDate :: OnchainMembershipInterval -> POSIXTime -> OnchainMembershipInterval
updateMembershipIntervalEndDate mi newEndDate = case membershipIntervalEndDate mi of
  Nothing -> mi {membershipIntervalEndDate = Just newEndDate}
  Just currentEndDate ->
    if newEndDate >= currentEndDate
      then mi {membershipIntervalEndDate = Just newEndDate}
      else traceError "New end date is before current end date"

{-# INLINEABLE acceptMembershipInterval #-}
acceptMembershipInterval :: OnchainMembershipInterval -> OnchainMembershipInterval
acceptMembershipInterval mi =
  if membershipIntervalIsAck mi
    then traceError "interval is already accepted"
    else mi {membershipIntervalIsAck = True}

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

{-# INLINEABLE mkPendingRank #-}
mkPendingRank :: AssetClass -> ProfileId -> ProfileId -> POSIXTime -> Integer -> ProtocolParams -> OnchainRank
mkPendingRank pendingRankId awardedTo awardedBy achievementDate rankNumber protocolParams =
  Promotion
    { promotionId = pendingRankId,
      promotionAwardedTo = awardedTo,
      promotionAwardedBy = awardedBy,
      promotionAchievementDate = achievementDate,
      promotionRankNumber = rankNumber,
      promotionProtocolParams = protocolParams
    }

{-# INLINEABLE acceptRank #-}
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

{-# INLINEABLE promoteProfile #-}
promoteProfile :: CIP68Datum OnchainProfile -> OnchainRank -> (CIP68Datum OnchainProfile, OnchainRank)
promoteProfile (CIP68Datum metadata version profile@OnchainProfile {..}) promotion = case currentRank of
  Just currentRankId ->
    let newRank = acceptRank promotion currentRankId
        updatedProfile = profile {currentRank = Just (rankId newRank)}
     in (CIP68Datum metadata version updatedProfile, newRank)
  Nothing -> traceError "OnchainProfile has no rank"

{-# INLINEABLE mkPractitionerProfile #-}
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

{-# INLINEABLE mkOrganizationProfile #-}
mkOrganizationProfile :: ProfileId -> ProtocolParams -> OnchainProfile
mkOrganizationProfile profileId protocolParams =
  OnchainProfile
    { profileId = profileId,
      profileType = Organization,
      currentRank = Nothing,
      protocolParams = protocolParams
    }

{-# INLINEABLE getCurrentRankId #-}
getCurrentRankId :: OnchainProfile -> RankId
getCurrentRankId (OnchainProfile _ Practitioner (Just rankId) _) = rankId
getCurrentRankId _ = traceError "OnchainProfile has no rank"

{-# INLINEABLE generateRankId #-}
generateRankId :: ProfileId -> Integer -> RankId
generateRankId (AssetClass (cs, TokenName bs)) i = AssetClass (cs, TokenName (blake2b_224 (bs `appendByteString` integerToByteString BigEndian 0 i))) -- takeByteString 28 $ blake2b_256 (bs <> (serialiseData . toBuiltinData) i))

-- | Generate a unique promotion rank ID from a seed TxOutRef
-- The seed ensures uniqueness since each TxOutRef can only be spent once
{-# INLINEABLE generatePromotionRankId #-}
generatePromotionRankId :: TxOutRef -> CurrencySymbol -> RankId
generatePromotionRankId seed cs = AssetClass (cs, TokenName (nameFromTxOutRef seed))

{-# INLINEABLE hasCurrencySymbol #-}
hasCurrencySymbol :: AssetClass -> CurrencySymbol -> Bool
hasCurrencySymbol (AssetClass (cs, _)) cs' = cs == cs'

-------------------------------------------------------------------------------

-- * Protocol Onchain Helpers

-------------------------------------------------------------------------------
unsafeGetRank :: RankId -> Address -> [TxInInfo] -> OnchainRank
unsafeGetRank ac addr txins =
  let (_, b) = checkAndGetCurrentStateDatumAndValue ac addr txins
   in unsafeFromBuiltinData b

{-# INLINEABLE unsafeGetRankDatumAndValue #-}
unsafeGetRankDatumAndValue :: RankId -> Address -> [TxInInfo] -> (Value, OnchainRank)
unsafeGetRankDatumAndValue ac addr txins =
  let (v, b) = checkAndGetCurrentStateDatumAndValue ac addr txins
   in (v, unsafeFromBuiltinData b)

{-# INLINEABLE unsafeGetProfile #-}
unsafeGetProfile :: RankId -> Address -> [TxInInfo] -> OnchainProfile
unsafeGetProfile ac addr txins =
  let (_, b) = checkAndGetCurrentStateDatumAndValue ac addr txins
   in extra (unsafeFromBuiltinData b :: CIP68Datum OnchainProfile)

{-# INLINEABLE unsafeGetProfileDatumAndValue #-}
unsafeGetProfileDatumAndValue :: RankId -> Address -> [TxInInfo] -> (Value, CIP68Datum OnchainProfile)
unsafeGetProfileDatumAndValue ac addr txins =
  let (v, b) = checkAndGetCurrentStateDatumAndValue ac addr txins
   in (v, unsafeFromBuiltinData b :: CIP68Datum OnchainProfile)

{-# INLINEABLE unsafeGetListNodeDatumAndValue #-}
unsafeGetListNodeDatumAndValue :: MembershipHistoriesListNodeId -> Address -> [TxInInfo] -> (Value, MembershipHistoriesListNode)
unsafeGetListNodeDatumAndValue listNodeId addr txins =
  let (v, b) = checkAndGetCurrentStateDatumAndValue listNodeId addr txins
   in case unsafeFromBuiltinData b :: MembershipDatum of
        ListNodeDatum node -> (v, node)
        _ -> traceError "Expected ListNodeDatum"

{-# INLINEABLE unsafeGetMembershipInterval #-}
unsafeGetMembershipInterval :: MembershipIntervalId -> Address -> [TxInInfo] -> (Value, OnchainMembershipInterval)
unsafeGetMembershipInterval intervalId addr txins =
  let (v, b) = checkAndGetCurrentStateDatumAndValue intervalId addr txins
   in case unsafeFromBuiltinData b :: MembershipDatum of
        IntervalDatum interval -> (v, interval)
        _ -> traceError "Expected IntervalDatum"