{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Ingestion where

import DomainTypes.Core.Types
import GeniusYield.TxBuilder
import GeniusYield.Types
import GeniusYield.Types.Address
import KupoAtlas
import TxBuilding.Functors
import TxBuilding.Utils
import TxBuilding.Validators

data ChainEventProjection
  = RankProjection Rank
  | ProfileProjection Profile
  | PromotionProjection Promotion
  | NoProjection AtlasMatch
  deriving (Show)

projectChainEvent :: (MonadError GYTxMonadException m) => GYNetworkId -> AtlasMatch -> m ChainEventProjection
projectChainEvent networkId am@AtlasMatch {..} = case amAddress of
  add | add == addressFromScriptHash networkId ranksValidatorHashGY -> do
    case rankFromGYOutDatum amDatum of
      Just onchainRank -> do
        rank <- onchainRankToRankInformation onchainRank
        case rank of
          Just rank -> return $ RankProjection rank
          Nothing -> do
            promotion <- onchainPromotionToPromotionInformation onchainRank
            case promotion of
              Nothing -> return $ NoProjection am
              Just promotion -> return $ PromotionProjection promotion
      Nothing -> return $ NoProjection am
  add | add == addressFromScriptHash networkId profilesValidatorHashGY -> do
    case profileFromGYOutDatum amDatum of
      Nothing -> return $ NoProjection am
      Just onchainProfile -> do
        profile <- profileDatumToProfile onchainProfile
        return $ ProfileProjection profile
  _ -> return $ NoProjection am
