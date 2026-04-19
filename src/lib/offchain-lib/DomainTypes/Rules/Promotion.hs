{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Off-chain mirror of the on-chain promotion-validation rules.
--
-- 'Onchain.BJJ.validatePromotion' returns a 'Bool' — sufficient for the
-- validator but useless for explaining /why/ a promotion would be rejected.
-- 'checkPromotion' in this module re-encodes the same predicates and returns
-- the list of rules that failed, so HTTP callers (the eligibility endpoint,
-- the frontend, the admin CLI, the MCP tool) can display structured reasons.
--
-- === Mirror-and-pin contract
--
-- This module __mirrors__ the five predicates inside 'validatePromotion'
-- rather than calling it. Rationale:
--
-- * 'validatePromotion' returns 'Bool'; we need a violation list.
-- * Calling 'validatePromotion' off-chain can trip on-chain @traceError@
--   paths (e.g. @succ Red10@), whereas a mirror can be total.
--
-- Drift between the two implementations is prevented by a property test at
-- "BJJPropertyTests" asserting
-- @null (checkPromotion m c n) ≡ validatePromotion m c n@ across 1000
-- generated 'BeltSnapshot' triples. __If you edit either implementation you
-- must edit both — the property test will fail otherwise.__
module DomainTypes.Rules.Promotion
  ( -- * Violations
    PromotionViolation (..),

    -- * Rule check
    checkPromotion,
    checkPromotionWithOptionalMaster,

    -- * Derived helpers
    earliestEligibleDate,
    requiredGranterRank,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema (..), NamedSchema (..))
import DomainTypes.Core.BJJ
  ( BJJBelt (..),
    BeltSnapshot (..),
    minMonthsForBelt,
    monthsToPosixTime,
  )
import GHC.Generics (Generic)
import PlutusLedgerApi.V3 (POSIXTime (..))

-- | A single reason a promotion would be rejected by the on-chain validator.
--
-- Shapes mirror the predicates in 'Onchain.BJJ.validatePromotion':
--
-- * 'MasterBeltTooLow' — master's belt does not authorize promotion to this
--   target (either too low overall, or 'Black1' cannot promote to 'Black').
-- * 'RungSkipped' — target is not exactly the next rung above the
--   practitioner's current belt.
-- * 'InsufficientTimeInGrade' — the practitioner has not spent the minimum
--   time-in-grade at their current belt.
-- * 'MasterDateAfterPromotion' — the master's own rank achievement date is
--   on or after the proposed promotion date.
-- * 'StudentDateNotMonotonic' — the proposed promotion date is on or before
--   the current rank's achievement date.
data PromotionViolation
  = -- | Master's belt is not authorized to promote to the proposed target.
    MasterBeltTooLow
      { violationMasterBelt :: BJJBelt
      }
  | -- | Proposed target belt is not exactly one rung above the current belt.
    RungSkipped
      { violationCurrentBelt :: BJJBelt,
        violationProposedBelt :: BJJBelt
      }
  | -- | Practitioner has not met the minimum time-in-grade at their current belt.
    InsufficientTimeInGrade
      { violationRequiredMonths :: Integer,
        violationActualMs :: Integer
      }
  | -- | Master's achievement date is not strictly before the promotion date.
    MasterDateAfterPromotion
  | -- | Promotion date is not strictly after the current rank's date.
    StudentDateNotMonotonic
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | A permissive Swagger schema — the concrete sum-encoding is documented in
-- prose on the enclosing 'PromotionEligibilityResponse'. swagger2's generic
-- deriver for sum-of-records is noisy; matching the 'PromotionState'
-- precedent keeps the generated document readable.
instance ToSchema PromotionViolation where
  declareNamedSchema _ = pure $ NamedSchema (Just "PromotionViolation") mempty

-- | Apply the five promotion predicates and return every rule that failed.
-- Empty list iff 'Onchain.BJJ.validatePromotion' would return 'True'.
--
-- Mirrors 'Onchain.BJJ.validatePromotion' — see module docs for the
-- mirror-and-pin contract.
checkPromotion ::
  -- | Master snapshot (granter's rank at their achievement date).
  BeltSnapshot ->
  -- | Student's current rank.
  BeltSnapshot ->
  -- | Proposed next rank (target belt + promotion date).
  BeltSnapshot ->
  [PromotionViolation]
checkPromotion master current next =
  masterAuthorityViolations ++ generalRuleViolations
  where
    m = belt master
    c = belt current
    n = belt next

    masterAuthorityViolations
      -- Below Black is not allowed to promote anyone.
      | m < Black = [MasterBeltTooLow m]
      -- Black1 can only promote to below-Black belts.
      | m == Black1 && n >= Black = [MasterBeltTooLow m]
      -- Master must be strictly above the proposed target.
      | m <= n = [MasterBeltTooLow m]
      | otherwise = []

    generalRuleViolations =
      dateOrderingViolations ++ rungViolation ++ timeInGradeViolation

    dateOrderingViolations =
      [MasterDateAfterPromotion | not (beltDate master < beltDate next)]
        ++ [StudentDateNotMonotonic | not (beltDate next > beltDate current)]

    -- Target must be exactly the successor of the current belt. 'Red10' has
    -- no successor on-chain (@succ Red10@ trips @traceError@), so any target
    -- off a Red10 current is a rung-skip.
    rungViolation
      | c == Red10 = [RungSkipped c n]
      | n == succ c = []
      | otherwise = [RungSkipped c n]

    -- Time-in-grade is indexed by the /current/ belt: to leave belt X you
    -- must have spent at least 'minMonthsForBelt X' at X.
    timeInGradeViolation =
      let requiredMonths = minMonthsForBelt c
          requiredMs = getPOSIXTime (monthsToPosixTime requiredMonths)
          actualMs = getPOSIXTime (beltDate next - beltDate current)
       in if actualMs > requiredMs
            then []
            else [InsufficientTimeInGrade requiredMonths actualMs]

-- | 'checkPromotion' variant that omits the master-authority check when the
-- granter is not known (e.g. the HTTP endpoint was called without a
-- @granter@ query parameter). Date, rung, and time-in-grade checks still
-- run — the caller can answer "am I eligible in principle?" before the
-- practitioner picks a coach.
checkPromotionWithOptionalMaster ::
  Maybe BeltSnapshot ->
  BeltSnapshot ->
  BeltSnapshot ->
  [PromotionViolation]
checkPromotionWithOptionalMaster mMaster current next = case mMaster of
  Just master -> checkPromotion master current next
  Nothing -> studentOnlyViolations
  where
    c = belt current
    n = belt next

    studentOnlyViolations =
      studentDateViolation ++ rungViolation ++ timeInGradeViolation

    studentDateViolation =
      [StudentDateNotMonotonic | not (beltDate next > beltDate current)]

    rungViolation
      | c == Red10 = [RungSkipped c n]
      | n == succ c = []
      | otherwise = [RungSkipped c n]

    timeInGradeViolation =
      let requiredMonths = minMonthsForBelt c
          requiredMs = getPOSIXTime (monthsToPosixTime requiredMonths)
          actualMs = getPOSIXTime (beltDate next - beltDate current)
       in if actualMs > requiredMs
            then []
            else [InsufficientTimeInGrade requiredMonths actualMs]

-- | Earliest moment at which the time-in-grade requirement to leave the
-- given belt is satisfied. The actual promotion date must be __strictly__
-- greater than this value (the on-chain predicate uses strict inequality).
earliestEligibleDate :: BeltSnapshot -> POSIXTime
earliestEligibleDate (BeltSnapshot b d) =
  d + monthsToPosixTime (minMonthsForBelt b)

-- | Minimum master belt required to promote a practitioner from their given
-- current belt to the next rung. 'Nothing' when no promotion is possible:
--
-- * @current == Red10@ — no successor, no next belt.
-- * @current == Red@ — target would be 'Red10', and no belt is strictly
--   above 'Red10'.
requiredGranterRank :: BJJBelt -> Maybe BJJBelt
requiredGranterRank current
  | current == Red10 = Nothing
  | current == Red = Nothing
  | target < Black = Just Black
  | target == Black = Just Black2
  | otherwise = Just (succ target)
  where
    -- safe: guarded against Red10 above
    target = succ current
