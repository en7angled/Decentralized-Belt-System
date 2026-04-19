{-# LANGUAGE OverloadedStrings #-}

-- | Query listing filter record types and query-param constructors (shared by query-api handlers).
module DomainTypes.Transfer.Filters
  ( ProfileFilter (..),
    PromotionFilter (..),
    MembershipHistoryFilter (..),
    MembershipIntervalFilter (..),
    AchievementFilter (..),
    ActivityFilter (..),
    profileFilterFromParams,
    promotionsFilterFromParams,
    membershipHistoryFilterFromParams,
    membershipIntervalFilterFromParams,
    achievementFilterFromParams,
    activityFilterFromParams,
  )
where

import Data.Text (Text)
import Data.Time (UTCTime)
import DomainTypes.Core.BJJ (BJJBelt)
import DomainTypes.Core.Types
import DomainTypes.Transfer.OrderBy (ActivityEventType)
import GeniusYield.Types (GYTime)

optionalNonEmpty :: [a] -> Maybe [a]
optionalNonEmpty xs = if null xs then Nothing else Just xs

-- | Optional filters for the profiles listing endpoint.
data ProfileFilter = ProfileFilter
  { profileFilterId :: Maybe [ProfileRefAC], -- ^ Restrict to these profile IDs
    profileFilterType :: Maybe ProfileType, -- ^ Practitioner or Organization
    profileFilterName :: Maybe Text, -- ^ Exact or prefix name match
    profileFilterDescription :: Maybe Text, -- ^ Substring match on description
    profileFilterActiveMembershipOrganization :: Maybe ProfileRefAC, -- ^ Currently active membership in this org
    profileFilterMembershipOrganization :: Maybe ProfileRefAC, -- ^ Any (active or past) membership in this org
    profileFilterBelt :: Maybe [BJJBelt], -- ^ Current belt is one of these
    profileFilterTextSearch :: Maybe Text, -- ^ Free-text search across name and description
    profileFilterRegisteredAfter :: Maybe UTCTime -- ^ Registered on or after this timestamp
  }

-- | Optional filters for the promotions listing endpoint.
data PromotionFilter = PromotionFilter
  { promotionFilterId :: Maybe [RankAC], -- ^ Restrict to these rank asset classes
    promotionFilterBelt :: Maybe [BJJBelt], -- ^ Belt level is one of these
    promotionFilterAchievedByProfileId :: Maybe [ProfileRefAC], -- ^ Promotee profile IDs
    promotionFilterAwardedByProfileId :: Maybe [ProfileRefAC], -- ^ Promoter profile IDs
    promotionFilterAchievementDateInterval :: (Maybe GYTime, Maybe GYTime), -- ^ Date range (from, to) inclusive
    promotionFilterTextSearch :: Maybe Text, -- ^ Free-text search
    promotionFilterState :: Maybe [PromotionState] -- ^ Pending, accepted, etc.
  }

-- | Optional filters for the membership-history listing endpoint.
data MembershipHistoryFilter = MembershipHistoryFilter
  { membershipHistoryFilterOrganizationProfileId :: Maybe [ProfileRefAC], -- ^ Filter by organization
    membershipHistoryFilterPractitionerProfileId :: Maybe [ProfileRefAC], -- ^ Filter by practitioner
    membershipHistoryFilterTextSearch :: Maybe Text -- ^ Free-text search
  }

-- | Optional filters for the membership-interval listing endpoint.
data MembershipIntervalFilter = MembershipIntervalFilter
  { membershipIntervalFilterPractitionerProfileId :: Maybe [ProfileRefAC], -- ^ Filter by practitioner
    membershipIntervalFilterIsAccepted :: Maybe Bool, -- ^ Filter by acceptance status
    membershipIntervalFilterTextSearch :: Maybe Text -- ^ Free-text search
  }

-- | Optional filters for the achievements listing endpoint.
data AchievementFilter = AchievementFilter
  { achievementFilterId :: Maybe [AchievementAC], -- ^ Restrict to these achievement asset classes
    achievementFilterAwardedToProfileId :: Maybe [ProfileRefAC], -- ^ Recipient profile IDs
    achievementFilterAwardedByProfileId :: Maybe [ProfileRefAC], -- ^ Issuer profile IDs
    achievementFilterIsAccepted :: Maybe Bool, -- ^ Acceptance status
    achievementFilterDateInterval :: (Maybe GYTime, Maybe GYTime), -- ^ Date range (from, to) inclusive
    achievementFilterTextSearch :: Maybe Text -- ^ Free-text search
  }

-- | Optional filters for the activity-feed endpoint.
data ActivityFilter = ActivityFilter
  { activityFilterEventType :: Maybe ActivityEventType, -- ^ Restrict to this event type
    activityFilterActor :: Maybe ProfileRefAC, -- ^ Filter by the acting profile
    activityFilterSince :: Maybe GYTime -- ^ Only return events after this timestamp
  }

-- | Build profile filter from query params (single source of truth for list handlers).
profileFilterFromParams :: [ProfileRefAC] -> Maybe ProfileType -> Maybe ProfileRefAC -> Maybe ProfileRefAC -> [BJJBelt] -> Maybe Text -> Maybe ProfileFilter
profileFilterFromParams profileRefs profileType activeMembershipOrg membershipOrg beltRefs q =
  Just $
    ProfileFilter
      { profileFilterId = optionalNonEmpty profileRefs,
        profileFilterType = profileType,
        profileFilterName = Nothing,
        profileFilterDescription = Nothing,
        profileFilterActiveMembershipOrganization = activeMembershipOrg,
        profileFilterMembershipOrganization = membershipOrg,
        profileFilterBelt = optionalNonEmpty beltRefs,
        profileFilterTextSearch = q,
        profileFilterRegisteredAfter = Nothing
      }

-- | Build promotion filter from query params.
-- The @profileRefs@ param is merged into @achievedByRefs@ (frontend sends "profile" meaning "achieved_by").
promotionsFilterFromParams :: [RankAC] -> [BJJBelt] -> [ProfileRefAC] -> [ProfileRefAC] -> [ProfileRefAC] -> Maybe Text -> Maybe GYTime -> Maybe GYTime -> [PromotionState] -> Maybe PromotionFilter
promotionsFilterFromParams promotionIds beltRefs achievedByRefs awardedByRefs profileRefs q fromTime toTime states =
  Just $
    PromotionFilter
      { promotionFilterId = optionalNonEmpty promotionIds,
        promotionFilterBelt = optionalNonEmpty beltRefs,
        promotionFilterAchievedByProfileId = optionalNonEmpty (achievedByRefs <> profileRefs),
        promotionFilterAwardedByProfileId = optionalNonEmpty awardedByRefs,
        promotionFilterAchievementDateInterval = (fromTime, toTime),
        promotionFilterTextSearch = q,
        promotionFilterState = optionalNonEmpty states
      }

-- | Build membership history filter from query params.
membershipHistoryFilterFromParams :: [ProfileRefAC] -> [ProfileRefAC] -> Maybe Text -> Maybe MembershipHistoryFilter
membershipHistoryFilterFromParams orgRefs practRefs q =
  Just
    MembershipHistoryFilter
      { membershipHistoryFilterOrganizationProfileId = optionalNonEmpty orgRefs,
        membershipHistoryFilterPractitionerProfileId = optionalNonEmpty practRefs,
        membershipHistoryFilterTextSearch = q
      }

-- | Build membership interval filter from query params.
membershipIntervalFilterFromParams :: [ProfileRefAC] -> Maybe Bool -> Maybe Text -> Maybe MembershipIntervalFilter
membershipIntervalFilterFromParams practRefs isAccepted q =
  Just
    MembershipIntervalFilter
      { membershipIntervalFilterPractitionerProfileId = optionalNonEmpty practRefs,
        membershipIntervalFilterIsAccepted = isAccepted,
        membershipIntervalFilterTextSearch = q
      }

-- | Build achievement filter from query params.
achievementFilterFromParams ::
  [AchievementAC] ->
  [ProfileRefAC] ->
  [ProfileRefAC] ->
  Maybe Bool ->
  Maybe GYTime ->
  Maybe GYTime ->
  Maybe Text ->
  Maybe AchievementFilter
achievementFilterFromParams achievementIds awardedToRefs awardedByRefs isAccepted fromTime toTime q =
  Just $
    AchievementFilter
      { achievementFilterId = optionalNonEmpty achievementIds,
        achievementFilterAwardedToProfileId = optionalNonEmpty awardedToRefs,
        achievementFilterAwardedByProfileId = optionalNonEmpty awardedByRefs,
        achievementFilterIsAccepted = isAccepted,
        achievementFilterDateInterval = (fromTime, toTime),
        achievementFilterTextSearch = q
      }

-- | Build activity filter from query params.
activityFilterFromParams :: Maybe ActivityEventType -> Maybe ProfileRefAC -> Maybe GYTime -> Maybe ActivityFilter
activityFilterFromParams eventType actor since =
  case (eventType, actor, since) of
    (Nothing, Nothing, Nothing) -> Nothing
    _ ->
      Just
        ActivityFilter
          { activityFilterEventType = eventType,
            activityFilterActor = actor,
            activityFilterSince = since
          }
