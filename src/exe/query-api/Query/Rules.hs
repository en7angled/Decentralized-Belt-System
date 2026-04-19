{-# LANGUAGE NumericUnderscores #-}

-- | Handlers for rule-verdict endpoints (off-chain mirrors of on-chain
-- predicates), distinct from 'Query.Aggregates' which enriches stored data
-- and from 'Query.Projected'\/'Query.Live' which perform direct lookups.
--
-- Today this exposes only promotion-eligibility; membership and achievement
-- rule checks can slot in alongside 'getPromotionEligibility' as they are
-- added.
module Query.Rules
  ( getPromotionEligibility,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import DomainTypes.Core.BJJ
  ( BJJBelt,
    BeltSnapshot (..),
    minMonthsForBelt,
  )
import DomainTypes.Core.Types
  ( ProfileRefAC,
    Rank (..),
  )
import DomainTypes.Rules.Promotion
  ( checkPromotionWithOptionalMaster,
    earliestEligibleDate,
    requiredGranterRank,
  )
import DomainTypes.Transfer.QueryResponses (PromotionEligibilityResponse (..))
import DomainTypes.Transfer.Types qualified as T
import GeniusYield.Types (timeFromPlutus, timeToPlutus)
import PlutusLedgerApi.V3 (POSIXTime (..))
import Query.Live qualified as L
import Query.Projected qualified as P
import QueryAppMonad (QueryAppMonad)
import RestAPI.Common (withBackend)

-- | Compute promotion eligibility for a practitioner against a target belt,
-- optionally under a specific granter's authority.
--
-- Fetches the subject's current rank from live or projected backend, reads
-- server UTC for the "proposed promotion now" snapshot, mirrors the
-- on-chain predicate off-chain via 'checkPromotionWithOptionalMaster', and
-- assembles the structured response. When @granter@ is omitted the
-- master-authority check is skipped and the response reports only the
-- minimum granter rank required for the practitioner's next rung.
getPromotionEligibility ::
  ProfileRefAC ->
  BJJBelt ->
  Maybe ProfileRefAC ->
  QueryAppMonad PromotionEligibilityResponse
getPromotionEligibility subjectId targetBelt mGranterId = do
  subject <- withBackend (L.getPractitionerProfile subjectId) (P.getPractitionerProfile subjectId)
  let currentRank = T.practitionerCurrentRank subject
      currentBelt = rankBelt currentRank
      currentDate = timeToPlutus (rankAchievementDate currentRank)
      currentSnap = BeltSnapshot currentBelt currentDate
  mMasterSnap <- case mGranterId of
    Nothing -> pure Nothing
    Just gId -> do
      granter <- withBackend (L.getPractitionerProfile gId) (P.getPractitionerProfile gId)
      let grank = T.practitionerCurrentRank granter
      pure $
        Just
          BeltSnapshot
            { belt = rankBelt grank,
              beltDate = timeToPlutus (rankAchievementDate grank)
            }
  nowUtc <- liftIO getCurrentTime
  let nowMs :: Integer
      nowMs = floor (utcTimeToPOSIXSeconds nowUtc * 1000)
      nowPlutus = POSIXTime nowMs
      proposedSnap = BeltSnapshot targetBelt nowPlutus
      violations = checkPromotionWithOptionalMaster mMasterSnap currentSnap proposedSnap
      daysInGrade = max 0 ((nowMs - getPOSIXTime currentDate) `div` 86_400_000)
  pure
    PromotionEligibilityResponse
      { eligibilityEligible = null violations,
        eligibilityCurrentBelt = currentBelt,
        eligibilityTargetBelt = targetBelt,
        eligibilityDaysInCurrentGrade = daysInGrade,
        eligibilityRequiredMonthsInGrade = minMonthsForBelt currentBelt,
        eligibilityEarliestEligibleDate = timeFromPlutus (earliestEligibleDate currentSnap),
        eligibilityRequiredGranterRank = requiredGranterRank currentBelt,
        eligibilityGranterRank = fmap belt mMasterSnap,
        eligibilityViolations = violations
      }
