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
{-# LANGUAGE UndecidableInstances #-}

module Storage where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Aeson as Aeson
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time (UTCTime, getCurrentTime)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import DomainTypes.Core.Types
import GeniusYield.Types
import Ingestion
import KupoAtlas (kupoMatchToAtlasMatch)
import KupoClient (CreatedAt (..), KupoMatch (..), KupoValue (..), SpentAt (..))
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
upsertCursor :: (MonadIO m) => ChainCursor -> SqlPersistT m ()
upsertCursor cur =
  void $
    upsert
      cur
      [ ChainCursorSlotNo =. chainCursorSlotNo cur,
        ChainCursorHeaderHash =. chainCursorHeaderHash cur,
        ChainCursorLastTxId =. chainCursorLastTxId cur,
        ChainCursorLastOutputIndex =. chainCursorLastOutputIndex cur
      ]

upsertMatchAndProjections :: (MonadIO m) => GYNetworkId -> KupoMatch -> SqlPersistT m ()
upsertMatchAndProjections networkId km = do
  upsertKupoMatch km
  case kupoMatchToAtlasMatch km of
    Left convErr -> liftIO $ putStrLn ("Conversion error: " <> convErr)
    Right am -> do
      let slotNoInt = slot_no (created_at km)
          header = header_hash (created_at km)
          txIdTxt = transaction_id km
          outIx = output_index km
      ev <- runExceptT (projectChainEvent networkId am)
      case ev of
        Left e -> liftIO $ putStrLn ("Projection error: " <> show e)
        Right proj -> case proj of
          RankEvent r -> upsertRankProjection slotNoInt header r
          ProfileEvent p -> upsertProfileProjection slotNoInt header p
          PromotionEvent pr -> upsertPromotionProjection slotNoInt header pr
          NoEvent _ -> pure ()

upsertKupoMatch :: (MonadIO m) => KupoMatch -> SqlPersistT m ()
upsertKupoMatch km = do
  let cSlot = slot_no (created_at km)
      cHash = header_hash (created_at km)
      ev = OnchainMatchEvent cSlot cHash km
  void $ upsert ev [OnchainMatchEventCreatedSlot =. cSlot, OnchainMatchEventCreatedHeader =. cHash]

upsertRankProjection :: (MonadIO m) => Integer -> Text -> Rank -> SqlPersistT m ()
upsertRankProjection createdSlot createdHash r = do
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
  void $ upsert ev []

upsertProfileProjection :: (MonadIO m) => Integer -> Text -> Profile -> SqlPersistT m ()
upsertProfileProjection createdSlot createdHash p = do
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
  void $ upsert ev []

upsertPromotionProjection :: (MonadIO m) => Integer -> Text -> Promotion -> SqlPersistT m ()
upsertPromotionProjection createdSlot createdHash pr = do
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
  void $ upsert ev []
