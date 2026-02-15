{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use &&" #-}

-- | Business logic for the BJJ Belt protocol.
--
-- Re-exports 'Onchain.Protocol.Types', 'Onchain.Protocol.Id', and
-- 'Onchain.Protocol.Lookup' so that existing @import Onchain.Protocol@
-- statements continue to work unchanged.
module Onchain.Protocol
  ( -- * Re-exports
    module Onchain.Protocol.Types,
    module Onchain.Protocol.Id,
    module Onchain.Protocol.Lookup,

    -- * Membership List Operations
    initEmptyMembershipHistoriesList,
    mkMembershipHistoriesListNode,
    unsafeGetMembershipHistory,
    insertMembershipHistoryInBetween,
    appendMembershipHistory,
    updateNodeMembershipHistory,

    -- * Membership History / Interval Operations
    initMembershipHistory,
    addMembershipIntervalToHistory,
    updateMembershipIntervalEndDate,
    acceptMembershipInterval,

    -- * Profile / Rank Smart Constructors
    mkPromotion,
    acceptRank,
    promoteProfileDatum,
    promoteProfile,
    mkPractitionerProfile,
    mkOrganizationProfile,
    getCurrentRankId,
  )
where

import Onchain.CIP68 (CIP68Datum (CIP68Datum))
import Onchain.LinkedList (NodeDatum (..))
import Onchain.LinkedList qualified as LinkedList
import Onchain.Protocol.Id
import Onchain.Protocol.Lookup
import Onchain.Protocol.Types
import PlutusLedgerApi.V1.Value (AssetClass)
import PlutusLedgerApi.V3 (POSIXTime)
import PlutusTx.Prelude

-------------------------------------------------------------------------------

-- * Membership List Operations

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

-- | Wrap a membership history into a linked-list node with an optional next-node pointer.
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

-- | Extract the membership history from a list node. Fails on root nodes.
{-# INLINEABLE unsafeGetMembershipHistory #-}
unsafeGetMembershipHistory :: MembershipHistoriesListNode -> OnchainMembershipHistory
unsafeGetMembershipHistory node = case nodeData (nodeInfo node) of
  Just history -> history
  Nothing -> traceError "3" -- Root node has no history

-- | Insert a membership history node between two existing nodes in the sorted list.
-- Validates that all nodes belong to the same organization.
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
        else traceError "4" -- Cannot insert: different orgs

-- | Append a membership history node to the end of the sorted list.
-- Validates that both nodes belong to the same organization.
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
        else traceError "5" -- Cannot append: different orgs

-- | Replace the membership history inside a list node, preserving node pointers.
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

-------------------------------------------------------------------------------

-- * Membership History / Interval Operations

-------------------------------------------------------------------------------

-- | Create a new membership history with its first interval.
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
        else traceError "6" -- Cannot add interval: validation failed
  where
    addMembershipsValidations =
      and
        [ traceIfFalse "7" validLastInterval, -- last interval is not the head
          traceIfFalse "8" lastIntervalIsClosed, -- last interval not closed
          traceIfFalse "9" lastIntervalIsAccepted -- last interval not accepted
        ]
    validLastInterval = membershipHistoryIntervalsHeadId currentHistory == membershipIntervalId lastInterval -- Required: prevents head-bypass attacks (see OnchainSecurityAudit.md)
    lastIntervalIsAccepted = membershipIntervalIsAck lastInterval
    lastIntervalIsClosed = case membershipIntervalEndDate lastInterval of
      Just lastIntervalEndDate -> startDate >= lastIntervalEndDate
      Nothing -> False -- If the last interval is not closed, we can add a new interval

-- | Update the end date of a membership interval. Only allows extending, not shortening.
{-# INLINEABLE updateMembershipIntervalEndDate #-}
updateMembershipIntervalEndDate :: OnchainMembershipInterval -> POSIXTime -> OnchainMembershipInterval
updateMembershipIntervalEndDate mi newEndDate = case membershipIntervalEndDate mi of
  Nothing -> mi {membershipIntervalEndDate = Just newEndDate}
  Just currentEndDate ->
    if newEndDate >= currentEndDate
      then mi {membershipIntervalEndDate = Just newEndDate}
      else traceError "A" -- Cannot update interval: end date before current

-- | Mark a membership interval as accepted by the practitioner. Fails if already accepted.
{-# INLINEABLE acceptMembershipInterval #-}
acceptMembershipInterval :: OnchainMembershipInterval -> OnchainMembershipInterval
acceptMembershipInterval mi =
  if membershipIntervalIsAck mi
    then traceError "M" -- Cannot accept interval: already accepted
    else mi {membershipIntervalIsAck = True}

-------------------------------------------------------------------------------

-- * Profile / Rank Smart Constructors

-------------------------------------------------------------------------------

-- | Construct a pending 'Promotion' value awaiting acceptance by the student.
{-# INLINEABLE mkPromotion #-}
mkPromotion :: AssetClass -> ProfileId -> ProfileId -> POSIXTime -> Integer -> ProtocolParams -> OnchainRank
mkPromotion pendingRankId awardedTo awardedBy achievementDate rankNumber protocolParams =
  Promotion
    { promotionId = pendingRankId,
      promotionAwardedTo = awardedTo,
      promotionAwardedBy = awardedBy,
      promotionAchievementDate = achievementDate,
      promotionRankNumber = rankNumber,
      promotionProtocolParams = protocolParams
    }

-- | Transform a pending 'Promotion' into a confirmed 'Rank' by recording the previous rank.
{-# INLINEABLE acceptRank #-}
acceptRank :: OnchainRank -> RankId -> OnchainRank
acceptRank (Rank {}) _ = traceError "N" -- Cannot accept a rank that is not pending
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

{-# INLINEABLE promoteProfileDatum #-}

-- | Compute only the updated profile datum for a promotion.
-- Unlike 'promoteProfile', this does NOT construct the full Rank record —
-- it only updates the profile's @currentRank@ pointer to the promotion's ID.
-- This is the lightweight alternative for validators that only need the
-- updated profile datum (e.g., ProfilesValidator — see R4 optimization in OnchainSecurityAudit.md).
promoteProfileDatum :: CIP68Datum OnchainProfile -> OnchainRank -> CIP68Datum OnchainProfile
promoteProfileDatum (CIP68Datum metadata version profile@OnchainProfile {..}) Promotion {..} = case currentRank of
  Just _currentRankId ->
    CIP68Datum metadata version (profile {currentRank = Just promotionId})
  Nothing -> traceError "O" -- OnchainProfile has no rank
promoteProfileDatum _ _ = traceError "P" -- Cannot accept: not pending

{-# INLINEABLE promoteProfile #-}

-- | Compute both the updated profile datum and the accepted rank.
-- For on-chain code that only needs the profile datum, prefer 'promoteProfileDatum'
-- which avoids constructing the full 7-field Rank record (R4 optimization).
promoteProfile :: CIP68Datum OnchainProfile -> OnchainRank -> (CIP68Datum OnchainProfile, OnchainRank)
promoteProfile (CIP68Datum metadata version profile@OnchainProfile {..}) promotion = case currentRank of
  Just currentRankId ->
    let newRank = acceptRank promotion currentRankId
        updatedProfile = profile {currentRank = Just (rankId newRank)}
     in (CIP68Datum metadata version updatedProfile, newRank)
  Nothing -> traceError "Q" -- OnchainProfile has no rank

-- | Create a new practitioner profile together with its initial rank (white belt).
{-# INLINEABLE mkPractitionerProfile #-}
mkPractitionerProfile :: ProfileId -> POSIXTime -> ProtocolParams -> Integer -> (OnchainProfile, OnchainRank)
mkPractitionerProfile profileId creationDate protocolParams rankNumber =
  let newRankId = deriveRankId profileId rankNumber
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

-- | Create a new organization profile (organizations have no rank).
{-# INLINEABLE mkOrganizationProfile #-}
mkOrganizationProfile :: ProfileId -> ProtocolParams -> OnchainProfile
mkOrganizationProfile profileId protocolParams =
  OnchainProfile
    { profileId = profileId,
      profileType = Organization,
      currentRank = Nothing,
      protocolParams = protocolParams
    }

-- | Extract the current rank ID from a practitioner profile. Fails on organizations.
{-# INLINEABLE getCurrentRankId #-}
getCurrentRankId :: OnchainProfile -> RankId
getCurrentRankId (OnchainProfile _ Practitioner (Just rankId) _) = rankId
getCurrentRankId _ = traceError "R" -- OnchainProfile has no rank
