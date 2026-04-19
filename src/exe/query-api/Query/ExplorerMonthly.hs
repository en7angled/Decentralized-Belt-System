{-# LANGUAGE OverloadedStrings #-}

-- | Pure monthly rollups for §12.5 explorer pages (shared by live list scans and projected helpers).
module Query.ExplorerMonthly
  ( monthKeyFromGYTime,
    monthlyPromotionsFromList,
    monthlyAchievementsFromList,
  )
where

import Data.Function (on)
import Data.List (foldl', groupBy, sortOn)
import Data.Map.Strict qualified as M
import Data.Text (Text, pack)
import DomainTypes.Core.Types (Achievement (..), Promotion (..))
import GeniusYield.Types (GYTime)
import DomainTypes.Transfer.QueryResponses (AchievementsMonthlyStatsResponse (..), MonthlyPromotionsDataResponse (..))
import Utils (stringFromJSON)

-- | ISO8601 @YYYY-MM@ prefix from JSON-encoded time (aligned with list ordering).
monthKeyFromGYTime :: GYTime -> Text
monthKeyFromGYTime t = pack $ take 7 $ stringFromJSON t

-- | Group items by calendar month using a date accessor, then summarise each group.
groupByMonth :: (a -> GYTime) -> (Text -> [a] -> b) -> b -> [a] -> [b]
groupByMonth getDate summarise empty items =
  let sorted = sortOn (monthKeyFromGYTime . getDate) items
      groups = groupBy ((==) `on` (monthKeyFromGYTime . getDate)) sorted
   in map toRow groups
  where
    toRow [] = empty
    toRow xs@(x : _) = summarise (monthKeyFromGYTime (getDate x)) xs

-- | Group promotions by calendar month; belt counts per month.
monthlyPromotionsFromList :: [Promotion] -> [MonthlyPromotionsDataResponse]
monthlyPromotionsFromList = groupByMonth promotionAchievementDate summarise (MonthlyPromotionsDataResponse "" [])
  where
    summarise m monthPs =
      let beltMap = foldl' (\acc pr -> M.insertWith (+) (promotionBelt pr) 1 acc) M.empty monthPs
       in MonthlyPromotionsDataResponse m (M.toList beltMap)

-- | Group achievements by month with accepted vs pending counts.
monthlyAchievementsFromList :: [Achievement] -> [AchievementsMonthlyStatsResponse]
monthlyAchievementsFromList = groupByMonth achievementAchievementDate summarise (AchievementsMonthlyStatsResponse "" 0 0)
  where
    summarise m monthAs =
      let accepted = length $ filter achievementAccepted monthAs
          pending = length $ filter (not . achievementAccepted) monthAs
       in AchievementsMonthlyStatsResponse m accepted pending
