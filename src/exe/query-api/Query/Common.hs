module Query.Common where

import Data.Text (Text)
import DomainTypes.Core.Types
import GeniusYield.Types (GYTime)
import DomainTypes.Core.BJJ (BJJBelt)

type Limit = Int

type Offset = Int

applyLimits :: Maybe (Int, Int) -> [a] -> [a]
applyLimits Nothing xs = xs
applyLimits (Just (limit, offset)) xs =
  let safeLimit = Prelude.max 0 limit
      safeOffset = Prelude.max 0 offset
   in Prelude.take safeLimit (Prelude.drop safeOffset xs)

data ProfileFilter = ProfileFilter
  { profileFilterId :: Maybe [ProfileRefAC],
    profileFilterType :: Maybe ProfileType,
    profileFilterName :: Maybe Text,
    profileFilterDescription :: Maybe Text
  }

data PromotionFilter = PromotionFilter
  { promotionFilterId :: Maybe [ProfileRefAC],
    promotionFilterBelt :: Maybe [BJJBelt],
    promotionFilterAchievedByProfileId :: Maybe [ProfileRefAC],
    promotionFilterAwardedByProfileId :: Maybe [ProfileRefAC],
    promotionFilterAchievementDateInterval :: (Maybe GYTime, Maybe GYTime)
  }

data RankFilter = RankFilter
  { rankFilterId :: Maybe [RankAC],
    rankFilterBelt :: Maybe [BJJBelt],
    rankFilterAchievedByProfileId :: Maybe [ProfileRefAC],
    rankFilterAwardedByProfileId :: Maybe [ProfileRefAC],
    rankFilterAchievementDateInterval :: (Maybe GYTime, Maybe GYTime)
  }

data MembershipHistoryFilter = MembershipHistoryFilter
  { membershipHistoryFilterOrganizationProfileId :: Maybe [ProfileRefAC],
    membershipHistoryFilterPractitionerProfileId :: Maybe [ProfileRefAC]
  }

data MembershipIntervalFilter = MembershipIntervalFilter
  { membershipIntervalFilterPractitionerProfileId :: Maybe [ProfileRefAC]
  }

data AchievementFilter = AchievementFilter
  { achievementFilterId :: Maybe [AchievementAC],
    achievementFilterAwardedToProfileId :: Maybe [ProfileRefAC],
    achievementFilterAwardedByProfileId :: Maybe [ProfileRefAC],
    achievementFilterIsAccepted :: Maybe Bool,
    achievementFilterDateInterval :: (Maybe GYTime, Maybe GYTime)
  }

-- Abstraction for retrieving the projection DB connection info from an environment
class HasProjectionDB r where
  getProjectionDbPath :: r -> Text
