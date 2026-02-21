-- | Safe offchain wrappers for onchain functions that can 'traceError'.
-- Pre-validate (or document) and throw 'TxBuildingException' instead of raw Plutus/Haskell errors.
-- See [offchain-rules.mdc](.cursor/rules/architecture/offchain-rules.mdc) § Onchain functions used offchain.
module TxBuilding.SafeOnchainLogic where

import Control.Monad (unless, when)
import Control.Monad.Except ( throwError, MonadError )
import GeniusYield.TxBuilder.Errors (GYTxMonadException (GYApplicationException))
import Onchain.BJJ (BJJBelt, intToBelt)
import Onchain.CIP68 (CIP68Datum, extra)
import Onchain.Protocol qualified as Onchain
import Onchain.Protocol.Types
  ( MembershipHistoriesListNode (organizationId),
    OnchainAchievement (achievementIsAccepted),
    OnchainMembershipInterval (membershipIntervalIsAck),
    OnchainProfile,
    OnchainProfileType (Practitioner),
    OnchainRank (Promotion, Rank),
  )
import PlutusLedgerApi.V3 (POSIXTime)
import TxBuilding.Exceptions (TxBuildingException (..))

------------------------------------------------------------------------------------------------

-- * Safe onchain call wrappers

------------------------------------------------------------------------------------------------

-- | Offchain analogue of 'Onchain.getCurrentRankId'. Throws 'ProfileHasNoRank' if profile has no rank.
safeGetCurrentRankId :: (MonadError GYTxMonadException m) => OnchainProfile -> m Onchain.RankId
safeGetCurrentRankId p =
  case (Onchain.profileType p, Onchain.currentRank p) of
    (Practitioner, Just rankId) -> return rankId
    _ -> throwError (GYApplicationException ProfileHasNoRank)

-- | Offchain analogue of 'Onchain.promoteProfile'. Throws 'ProfileHasNoRank' or 'PromotionNotPending' on invalid input.
safePromoteProfile :: (MonadError GYTxMonadException m) => CIP68Datum OnchainProfile -> OnchainRank -> m (CIP68Datum OnchainProfile, Onchain.OnchainRank)
safePromoteProfile profileDatum rank =
  case rank of
    Rank {} -> throwError (GYApplicationException PromotionNotPending)
    Promotion {} -> case Onchain.currentRank (extra profileDatum) of
      Nothing -> throwError (GYApplicationException ProfileHasNoRank)
      Just _ -> return (Onchain.promoteProfile profileDatum rank)

-- | Offchain analogue of 'Onchain.initMembershipHistory'. Throws 'InitMembershipHistoryInvalidDates' if end date is not after start date.
safeInitMembershipHistory :: (MonadError GYTxMonadException m) => Onchain.ProfileId -> Onchain.ProfileId -> POSIXTime -> Maybe POSIXTime -> m (Onchain.OnchainMembershipHistory, Onchain.OnchainMembershipInterval)
safeInitMembershipHistory practitionerId organizationId startDate endDate = do
  let validEndDate = case endDate of Just ed -> ed > startDate; Nothing -> True
  unless validEndDate $ throwError (GYApplicationException InitMembershipHistoryInvalidDates)
  return (Onchain.initMembershipHistory practitionerId organizationId startDate endDate)

-- | Offchain analogue of 'Onchain.appendMembershipHistory'. Throws 'MembershipListAppendInvalid' if nodes belong to different orgs.
safeAppendMembershipHistory :: (MonadError GYTxMonadException m) => (MembershipHistoriesListNode, MembershipHistoriesListNode) -> m MembershipHistoriesListNode
safeAppendMembershipHistory (lastNode, appendedNode) = do
  unless (organizationId lastNode == organizationId appendedNode) $
    throwError (GYApplicationException MembershipListAppendInvalid)
  return (Onchain.appendMembershipHistory (lastNode, appendedNode))

-- | Offchain analogue of 'Onchain.insertMembershipHistoryInBetween'. Throws 'MembershipListInsertInvalid' if nodes belong to different orgs.
safeInsertMembershipHistoryInBetween :: (MonadError GYTxMonadException m) => (MembershipHistoriesListNode, MembershipHistoriesListNode, MembershipHistoriesListNode) -> m MembershipHistoriesListNode
safeInsertMembershipHistoryInBetween (oldLeftNode, rightNode, insertedNode) = do
  let leftOrg = organizationId oldLeftNode
  unless (leftOrg == organizationId rightNode && leftOrg == organizationId insertedNode) $
    throwError (GYApplicationException MembershipListInsertInvalid)
  return (Onchain.insertMembershipHistoryInBetween (oldLeftNode, rightNode, insertedNode))

-- | Offchain analogue of 'Onchain.acceptMembershipInterval'. Throws 'MembershipIntervalAlreadyAccepted' if already accepted.
safeAcceptMembershipInterval :: (MonadError GYTxMonadException m) => OnchainMembershipInterval -> m OnchainMembershipInterval
safeAcceptMembershipInterval mi = do
  when (membershipIntervalIsAck mi) $
    throwError (GYApplicationException MembershipIntervalAlreadyAccepted)
  return (Onchain.acceptMembershipInterval mi)

-- | Offchain analogue of 'Onchain.acceptAchievement'. Throws 'AchievementAlreadyAccepted' if already accepted.
safeAcceptAchievement :: (MonadError GYTxMonadException m) => OnchainAchievement -> m OnchainAchievement
safeAcceptAchievement achievement = do
  when (achievementIsAccepted achievement) $
    throwError (GYApplicationException AchievementAlreadyAccepted)
  return (Onchain.acceptAchievement achievement)

-- | Offchain analogue of 'Onchain.BJJ.intToBelt'. Throws 'InvalidBeltNumber' if integer is not in 0–14.
safeIntToBelt :: (MonadError GYTxMonadException m) => Integer -> m BJJBelt
safeIntToBelt n
  | n >= 0 && n <= 14 = return (intToBelt n)
  | otherwise = throwError (GYApplicationException InvalidBeltNumber)
