{-# LANGUAGE DataKinds #-}

module RestAPI.Common where

import Data.Functor ((<&>))
import DomainTypes.Core.Types
import GeniusYield.Types (GYTime)
import Query.Common qualified as C
import QueryAppMonad
import DomainTypes.Core.BJJ (BJJBelt)

-- | Dispatch to live or projected backend based on the query flag.
withBackend :: Bool -> QueryAppMonad a -> QueryAppMonad a -> QueryAppMonad a
withBackend live liveAct projectedAct = if live then liveAct else projectedAct

-- | Build profile filter from query params (single source of truth for list handlers).
profileFilterFromParams :: [ProfileRefAC] -> Maybe ProfileType -> Maybe C.ProfileFilter
profileFilterFromParams profileRefs profileType =
  Just $
    C.ProfileFilter
      { C.profileFilterId = C.optionalNonEmpty profileRefs,
        C.profileFilterType = profileType,
        C.profileFilterName = Nothing,
        C.profileFilterDescription = Nothing
      }

-- | Build promotion filter from query params.
promotionsFilterFromParams :: [RankAC] -> [BJJBelt] -> [ProfileRefAC] -> [ProfileRefAC] -> Maybe C.PromotionFilter
promotionsFilterFromParams promotionIds beltRefs achievedByRefs awardedByRefs =
  Just $
    C.PromotionFilter
      { C.promotionFilterId = C.optionalNonEmpty promotionIds,
        C.promotionFilterBelt = C.optionalNonEmpty beltRefs,
        C.promotionFilterAchievedByProfileId = C.optionalNonEmpty achievedByRefs,
        C.promotionFilterAwardedByProfileId = C.optionalNonEmpty awardedByRefs,
        C.promotionFilterAchievementDateInterval = (Nothing, Nothing)
      }

-- | Build rank (belts) filter from query params.
rankFilterFromParams :: [RankAC] -> [BJJBelt] -> [ProfileRefAC] -> [ProfileRefAC] -> Maybe GYTime -> Maybe GYTime -> Maybe C.RankFilter
rankFilterFromParams rankIds belt achievedByRefs awardedByRefs fromTime toTime =
  Just $
    C.RankFilter
      { C.rankFilterId = C.optionalNonEmpty rankIds,
        C.rankFilterBelt = C.optionalNonEmpty belt,
        C.rankFilterAchievedByProfileId = C.optionalNonEmpty achievedByRefs,
        C.rankFilterAwardedByProfileId = C.optionalNonEmpty awardedByRefs,
        C.rankFilterAchievementDateInterval = (fromTime, toTime)
      }

-- | Build membership history filter from query params.
membershipHistoryFilterFromParams :: [ProfileRefAC] -> [ProfileRefAC] -> Maybe C.MembershipHistoryFilter
membershipHistoryFilterFromParams orgRefs practRefs =
  if Prelude.null orgRefs && Prelude.null practRefs
    then Nothing
    else
      Just
        C.MembershipHistoryFilter
          { C.membershipHistoryFilterOrganizationProfileId = C.optionalNonEmpty orgRefs,
            C.membershipHistoryFilterPractitionerProfileId = C.optionalNonEmpty practRefs
          }

-- | Build membership interval filter from query params.
membershipIntervalFilterFromParams :: [ProfileRefAC] -> Maybe C.MembershipIntervalFilter
membershipIntervalFilterFromParams practRefs =
  C.optionalNonEmpty practRefs <&> \ids ->
    C.MembershipIntervalFilter {C.membershipIntervalFilterPractitionerProfileId = Just ids}

-- | Build achievement filter from query params.
achievementFilterFromParams ::
  [AchievementAC] ->
  [ProfileRefAC] ->
  [ProfileRefAC] ->
  Maybe Bool ->
  Maybe GYTime ->
  Maybe GYTime ->
  Maybe C.AchievementFilter
achievementFilterFromParams achievementIds awardedToRefs awardedByRefs isAccepted fromTime toTime =
  Just $
    C.AchievementFilter
      { C.achievementFilterId = C.optionalNonEmpty achievementIds,
        C.achievementFilterAwardedToProfileId = C.optionalNonEmpty awardedToRefs,
        C.achievementFilterAwardedByProfileId = C.optionalNonEmpty awardedByRefs,
        C.achievementFilterIsAccepted = isAccepted,
        C.achievementFilterDateInterval = (fromTime, toTime)
      }
