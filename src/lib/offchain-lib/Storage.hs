{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage where

import Control.Monad (void)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import DomainTypes.Core.Types
import GeniusYield.Types
import Ingestion
import KupoAtlas (kupoMatchToAtlasMatch)
import KupoClient (CreatedAt (..), KupoMatch (..))
import Onchain.BJJ (BJJBelt)

derivePersistFieldJSON "BJJBelt"
derivePersistFieldJSON "GYAssetClass"
derivePersistFieldJSON "GYTime"
derivePersistFieldJSON "Integer"

-- Persistent entities for chain sync state and events.
-- We store a singleton cursor row keyed by a Unique flag to always upsert/update the same row.

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
ChainCursor
    singleton        Bool
    slotNo           Integer
    headerHash       Text
    lastTxId         Text Maybe
    lastOutputIndex  Int  Maybe
    UniqueCursor singleton
    deriving Show

-- Raw Atlas match storage (flattened Kupo match fields)
OnchainMatchEvent
    createdSlot      Integer
    createdHeader    Text
    kupoMatch        KupoMatch
    UniqueKupoMatch createdSlot createdHeader
    deriving Show

-- Projection-specific flattened tables (domain fields + created_at)
RankProjection
    createdAtSlot    Integer
    createdAtHash    Text
    rankId           GYAssetClass
    rankBelt         BJJBelt
    rankAchievedByProfileId GYAssetClass
    rankAwardedByProfileId  GYAssetClass
    rankAchievementDate     GYTime
    insertedAt       UTCTime
    UniqueRankProjection rankId
    deriving Show

ProfileProjection
    createdAtSlot    Integer
    createdAtHash    Text
    profileId        GYAssetClass
    profileName      Text
    profileDescription Text
    profileImageURI  Text
    profileType      ProfileType
    insertedAt       UTCTime
    UniqueProfileProjection profileId
    deriving Show

PromotionProjection
    createdAtSlot    Integer
    createdAtHash    Text
    promotionId      GYAssetClass
    promotionBelt    BJJBelt
    promotionAchievedByProfileId GYAssetClass
    promotionAwardedByProfileId  GYAssetClass
    promotionAchievementDate     GYTime
    insertedAt       UTCTime
    UniquePromotionProjection promotionId
    deriving Show
|]

runMigrations :: (MonadIO m) => SqlPersistT m ()
runMigrations = runMigration migrateAll

-- Fetch current cursor if exists
getCursorValue :: (MonadIO m) => SqlPersistT m (Maybe ChainCursor)
getCursorValue = fmap entityVal <$> getBy (UniqueCursor True)

-- Upsert the singleton cursor
putCursor :: (MonadIO m) => ChainCursor -> SqlPersistT m ()
putCursor cur = do
  mExisting <- getBy (UniqueCursor True)
  case mExisting of
    Nothing -> void (insert cur)
    Just (Entity key _) -> replace key cur

putMatchAndProjections :: (MonadIO m) => GYNetworkId -> KupoMatch -> SqlPersistT m ()
putMatchAndProjections networkId km = do
  liftIO $ putStrLn ("Putting match and projections for " <> show (slot_no (created_at km)))
  putKupoMatch km
  case kupoMatchToAtlasMatch km of
    Left convErr -> liftIO $ putStrLn ("Conversion error: " <> convErr)
    Right am -> do
      let slotNoInt = slot_no (created_at km)
          header = header_hash (created_at km)
      ev <- runExceptT (projectChainEvent networkId am)
      case ev of
        Left e -> liftIO $ putStrLn ("Projection error: " <> show e)
        Right proj -> case proj of
          RankEvent r -> do
            putRankProjection slotNoInt header r
            deletePromotionProjection (rankId r)
          ProfileEvent p -> putProfileProjection slotNoInt header p
          PromotionEvent pr -> putPromotionProjection slotNoInt header pr
          NoEvent _ -> pure ()

putKupoMatch :: (MonadIO m) => KupoMatch -> SqlPersistT m ()
putKupoMatch km = do
  let cSlot = slot_no (created_at km)
      cHash = header_hash (created_at km)
      ev = OnchainMatchEvent cSlot cHash km
  mExisting <- getBy (UniqueKupoMatch cSlot cHash)
  case mExisting of
    Nothing -> void (insert ev)
    Just (Entity key _) -> replace key ev

putRankProjection :: (MonadIO m) => Integer -> Text -> Rank -> SqlPersistT m ()
putRankProjection createdSlot createdHash r = do
  now <- liftIO getCurrentTime
  let ev =
        RankProjection
          createdSlot
          createdHash
          (rankId r)
          (rankBelt r)
          (rankAchievedByProfileId r)
          (rankAwardedByProfileId r)
          (rankAchievementDate r)
          now
  mExisting <- getBy (UniqueRankProjection (rankId r))
  case mExisting of
    Nothing -> void (insert ev)
    Just (Entity key _) -> replace key ev

putProfileProjection :: (MonadIO m) => Integer -> Text -> Profile -> SqlPersistT m ()
putProfileProjection createdSlot createdHash p = do
  now <- liftIO getCurrentTime
  let ev =
        ProfileProjection
          createdSlot
          createdHash
          (profileId p)
          (profileName p)
          (profileDescription p)
          (profileImageURI p)
          (profileType p)
          now
  mExisting <- getBy (UniqueProfileProjection (profileId p))
  case mExisting of
    Nothing -> void (insert ev)
    Just (Entity key _) -> replace key ev

putPromotionProjection :: (MonadIO m) => Integer -> Text -> Promotion -> SqlPersistT m ()
putPromotionProjection createdSlot createdHash pr = do
  now <- liftIO getCurrentTime
  let ev =
        PromotionProjection
          createdSlot
          createdHash
          (promotionId pr)
          (promotionBelt pr)
          (promotionAchievedByProfileId pr)
          (promotionAwardedByProfileId pr)
          (promotionAchievementDate pr)
          now
  mExisting <- getBy (UniquePromotionProjection (promotionId pr))
  case mExisting of
    Nothing -> void (insert ev)
    Just (Entity key _) -> replace key ev

deletePromotionProjection :: (MonadIO m) => GYAssetClass -> SqlPersistT m ()
deletePromotionProjection promotionId = do
  deleteBy (UniquePromotionProjection promotionId)

-- | Rollback all stored events and projections strictly beyond the given slot,
--   and any rows at the slot with a mismatching block header hash.
rollbackTo :: (MonadIO m) => Integer -> Text -> SqlPersistT m ()
rollbackTo slotNo headerHash = do
  -- Remove Onchain matches beyond tip or same slot but different header
  deleteWhere [OnchainMatchEventCreatedSlot >. slotNo]
  deleteWhere [OnchainMatchEventCreatedSlot ==. slotNo, OnchainMatchEventCreatedHeader !=. headerHash]

  -- Remove projections beyond tip or same slot but different header
  deleteWhere [ProfileProjectionCreatedAtSlot >. slotNo]
  deleteWhere [ProfileProjectionCreatedAtSlot ==. slotNo, ProfileProjectionCreatedAtHash !=. headerHash]

  deleteWhere [RankProjectionCreatedAtSlot >. slotNo]
  deleteWhere [RankProjectionCreatedAtSlot ==. slotNo, RankProjectionCreatedAtHash !=. headerHash]

  deleteWhere [PromotionProjectionCreatedAtSlot >. slotNo]
  deleteWhere [PromotionProjectionCreatedAtSlot ==. slotNo, PromotionProjectionCreatedAtHash !=. headerHash]
