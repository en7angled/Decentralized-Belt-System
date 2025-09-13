{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Ingestion where

import Data.Maybe (isJust)
import DomainTypes.Core.Types
import GeniusYield.TxBuilder
import GeniusYield.Types
import KupoAtlas
import TxBuilding.Functors
import TxBuilding.Utils
import TxBuilding.Validators

data ChainEventProjection
  = RankEvent Rank
  | ProfileEvent Profile
  | PromotionEvent Promotion
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
      _ -> return $ NoEvent am


--- TODO: Delete profile projections when the profile is deleted (use spentAt and redeemer)
