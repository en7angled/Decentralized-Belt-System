{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Ingestion where

import Data.Maybe (isJust)
import DomainTypes.Core.Types
import GeniusYield.TxBuilder
import GeniusYield.Types
import KupoAtlas
import Onchain.Protocol.Types (MembershipDatum (..), MembershipHistoriesListNode (..))
import Onchain.LinkedList (NodeDatum (..))
import TxBuilding.Functors
import TxBuilding.Utils
import TxBuilding.Validators (achievementsValidatorHashGY, membershipsValidatorHashGY, profilesValidatorHashGY, ranksValidatorHashGY)

data ChainEventProjection
  = RankEvent Rank
  | ProfileEvent Profile
  | PromotionEvent Promotion
  | MembershipHistoryEvent MembershipHistory
  | MembershipIntervalEvent MembershipInterval
  | AchievementEvent Achievement
  | NoEvent AtlasMatch
  deriving (Show)

projectChainEvent :: (MonadError GYTxMonadException m) => GYNetworkId -> AtlasMatch -> m ChainEventProjection
projectChainEvent nid am@AtlasMatch {..} =
  if isJust amSpentAt
    then return $ NoEvent am
    else case amAddress of
      add | add == addressFromScriptHash nid ranksValidatorHashGY -> do
        case rankFromGYOutDatum amDatum of
          Just onchainRank -> do
            rank <- onchainRankToRankInformation onchainRank
            case rank of
              Just r -> return $ RankEvent r
              Nothing -> do
                promotion <- onchainPromotionToPromotionInformation onchainRank
                case promotion of
                  Nothing -> return $ NoEvent am
                  Just p -> return $ PromotionEvent p
          Nothing -> return $ NoEvent am
      add | add == addressFromScriptHash nid profilesValidatorHashGY -> do
        case profileFromGYOutDatum amDatum of
          Nothing -> return $ NoEvent am
          Just onchainProfile -> do
            profile <- profileDatumToProfile onchainProfile
            return $ ProfileEvent profile
      add | add == addressFromScriptHash nid membershipsValidatorHashGY -> do
        case membershipDatumFromGYOutDatum amDatum of
          Nothing -> return $ NoEvent am
          Just (ListNodeDatum MembershipHistoriesListNode {nodeInfo}) ->
            case nodeData nodeInfo of
              Nothing -> return $ NoEvent am  -- Root node, skip
              Just onchainHistory -> do
                history <- onchainMembershipHistoryToMembershipHistory onchainHistory
                return $ MembershipHistoryEvent history
          Just (IntervalDatum onchainInterval) -> do
            case extractNFTAssetClass amValue of
              Nothing -> return $ NoEvent am
              Just gyIntervalId -> do
                interval <- onchainMembershipIntervalToMembershipInterval gyIntervalId onchainInterval
                return $ MembershipIntervalEvent interval
      add | add == addressFromScriptHash nid achievementsValidatorHashGY -> do
        case achievementFromGYOutDatum amDatum of
          Nothing -> return $ NoEvent am
          Just onchainAchievement -> do
            achievement <- onchainAchievementToAchievement onchainAchievement
            return $ AchievementEvent achievement
      _ -> return $ NoEvent am

--- No need to delete profile projections since the profile cannot be deleted onchain;
