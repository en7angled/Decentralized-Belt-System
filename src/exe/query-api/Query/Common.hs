module Query.Common where

import Data.Text (Text)
import DomainTypes.Core.Types
import GeniusYield.Types (GYTime)
import Onchain.BJJ (BJJBelt)

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

whenJust :: Maybe a -> (a -> b) -> b -> b
whenJust Nothing _ def = def
whenJust (Just a) f _ = f a

-- Abstraction for retrieving the projection DB connection info from an environment
class HasProjectionDB r where
  getProjectionDbPath :: r -> Text
