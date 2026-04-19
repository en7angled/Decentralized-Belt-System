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

-- | Persistent storage layer for chain-sync projections, cursor management, and rollback.
module Storage where

import Control.Monad (forM_, void)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Data.List qualified as L
import Data.Maybe (isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import DomainTypes.Core.BJJ (BJJBelt)
import DomainTypes.Core.Types
import GeniusYield.Types
import Ingestion
import KupoAtlas (kupoMatchToAtlasMatch)
import KupoClient (CreatedAt (..), KupoMatch (..))
import Onchain.Protocol.Id (deriveMembershipHistoryId, deriveMembershipIntervalId)

derivePersistFieldJSON "BJJBelt"
derivePersistFieldJSON "GYAssetClass"
derivePersistFieldJSON "Integer"

-- | Custom PersistField for GYTime stored as @timestamptz@ in PostgreSQL.
-- Converts via UTCTime so the column type matches the parameter type,
-- avoiding @varchar >= timestamptz@ mismatches in SQL comparisons.
-- Reads back both @PersistUTCTime@ (native timestamptz) and @PersistText@
-- (legacy rows written before this change) for backward compatibility.
instance PersistField GYTime where
  toPersistValue gt =
    PersistUTCTime $ posixSecondsToUTCTime (timeToPOSIX gt)
  fromPersistValue (PersistUTCTime utc) =
    Right $ timeFromPOSIX (utcTimeToPOSIXSeconds utc)
  fromPersistValue (PersistText t) =
    case gyIso8601ParseM (Text.unpack (stripQuotes t)) of
      Just gt -> Right gt
      Nothing -> Left $ "Cannot parse GYTime from: " <> t
    where
      -- Handle legacy data stored via derivePersistFieldJSON with surrounding quotes
      stripQuotes s
        | Just rest <- Text.stripPrefix "\"" s, Just inner <- Text.stripSuffix "\"" rest = inner
        | otherwise = s
  fromPersistValue x = Left $ "Expected PersistUTCTime or PersistText for GYTime, got: " <> Text.pack (show x)

instance PersistFieldSql GYTime where
  sqlType _ = SqlDayTime

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

-- Singleton config row: stored minting policy hex; used only at startup to detect policy change and optionally wipe.
ChainSyncConfig
    singleton        Bool
    policyHexText    Text
    UniqueChainSyncConfig singleton
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

MembershipHistoryProjection
    createdAtSlot             Integer
    createdAtHash             Text
    membershipHistoryId       GYAssetClass
    practitionerProfileId     GYAssetClass
    organizationProfileId     GYAssetClass
    insertedAt                UTCTime
    UniqueMembershipHistoryProjection membershipHistoryId
    deriving Show

MembershipIntervalProjection
    createdAtSlot             Integer
    createdAtHash             Text
    membershipIntervalId      GYAssetClass
    startDate                 GYTime
    endDate                   GYTime Maybe
    isAccepted                Bool
    practitionerProfileId     GYAssetClass
    organizationProfileId     GYAssetClass Maybe
    intervalNumber            Integer
    insertedAt                UTCTime
    UniqueMembershipIntervalProjection membershipIntervalId
    deriving Show

AchievementProjection
    createdAtSlot             Integer
    createdAtHash             Text
    achievementId             GYAssetClass
    awardedToProfileId        GYAssetClass
    awardedByProfileId        GYAssetClass
    achievementDate           GYTime
    isAccepted                Bool
    achievementName           Text
    achievementDescription    Text
    achievementImageURI       Text
    otherMetadata             (Maybe AchievementOtherMetadataJson)
    insertedAt                UTCTime
    UniqueAchievementProjection achievementId
    deriving Show
|]

runMigrations :: (MonadIO m) => SqlPersistT m ()
runMigrations = runMigration migrateAll

-- | Upsert a record by its unique key: insert if absent, replace if present.
upsertByUnique ::
  (PersistEntity a, PersistEntityBackend a ~ SqlBackend, SafeToInsert a, MonadIO m) =>
  (a -> Unique a) ->
  a ->
  SqlPersistT m ()
upsertByUnique getUnique val = do
  mExisting <- getBy (getUnique val)
  case mExisting of
    Nothing -> void (insert val)
    Just (Entity key _) -> replace key val

-- Fetch current cursor if exists
getCursorValue :: (MonadIO m) => SqlPersistT m (Maybe ChainCursor)
getCursorValue = fmap entityVal <$> getBy (UniqueCursor True)

-- Upsert the singleton cursor
putCursor :: (MonadIO m) => ChainCursor -> SqlPersistT m ()
putCursor = upsertByUnique (const (UniqueCursor True))

-- | Read the stored policy hex from the singleton config row, or Nothing if no row exists.
getStoredPolicyHexText :: (MonadIO m) => SqlPersistT m (Maybe Text)
getStoredPolicyHexText = fmap (chainSyncConfigPolicyHexText . entityVal) <$> getBy (UniqueChainSyncConfig True)

-- | Upsert the singleton config row with the given policy hex (idempotent, single row only).
putStoredPolicyHexText :: (MonadIO m) => Text -> SqlPersistT m ()
putStoredPolicyHexText policyHexText =
  upsertByUnique (const (UniqueChainSyncConfig True)) (ChainSyncConfig True policyHexText)

-- | Table names for all entities in this persist block (persistLowerCase).
-- When adding a new entity to this block, add its table name here so wipeChainSyncTables drops it.
chainSyncTableNames :: [Text]
chainSyncTableNames =
  [ "achievement_projection",
    "chain_cursor",
    "chain_sync_config",
    "membership_history_projection",
    "membership_interval_projection",
    "onchain_match_event",
    "profile_projection",
    "promotion_projection",
    "rank_projection"
  ]

-- | Drop all chain-sync tables (this persist block), then run migrations to recreate them.
-- Does not drop tables used only by other services; this module has no such tables.
wipeChainSyncTables :: (MonadIO m) => SqlPersistT m ()
wipeChainSyncTables = do
  forM_ chainSyncTableNames $ \tableName ->
    rawExecute ("DROP TABLE IF EXISTS " <> tableName <> " CASCADE") []
  runMigrations

-- | Convert a raw Kupo match to domain projections and upsert them alongside the raw match.
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
          MembershipHistoryEvent mh -> putMembershipHistoryProjection slotNoInt header mh
          MembershipIntervalEvent mi -> do
            mOrg <- resolveOrganizationForInterval mi
            putMembershipIntervalProjection slotNoInt header mi mOrg
          AchievementEvent a -> putAchievementProjection slotNoInt header a
          NoEvent _ -> pure ()

-- | Store a raw Kupo match event, upserting by slot + header hash.
putKupoMatch :: (MonadIO m) => KupoMatch -> SqlPersistT m ()
putKupoMatch km = do
  let cSlot = slot_no (created_at km)
      cHash = header_hash (created_at km)
      ev = OnchainMatchEvent cSlot cHash km
  upsertByUnique (\e -> UniqueKupoMatch (onchainMatchEventCreatedSlot e) (onchainMatchEventCreatedHeader e)) ev

-- | Store a rank projection, upserting by rank ID.
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
  upsertByUnique (UniqueRankProjection . rankProjectionRankId) ev

-- | Store a profile projection, upserting by profile ID.
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
  upsertByUnique (UniqueProfileProjection . profileProjectionProfileId) ev

-- | Store a promotion projection, upserting by promotion ID.
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
  upsertByUnique (UniquePromotionProjection . promotionProjectionPromotionId) ev

-- | Remove a promotion projection when the corresponding rank is confirmed on-chain.
deletePromotionProjection :: (MonadIO m) => GYAssetClass -> SqlPersistT m ()
deletePromotionProjection pid =
  deleteBy (UniquePromotionProjection pid)

-- | Store a membership history projection and backfill any interval projections missing their organization.
putMembershipHistoryProjection :: (MonadIO m) => Integer -> Text -> MembershipHistory -> SqlPersistT m ()
putMembershipHistoryProjection createdSlot createdHash mh = do
  now <- liftIO getCurrentTime
  let ev =
        MembershipHistoryProjection
          createdSlot
          createdHash
          (membershipHistoryId mh)
          (membershipHistoryPractitionerId mh)
          (membershipHistoryOrganizationId mh)
          now
  upsertByUnique (UniqueMembershipHistoryProjection . membershipHistoryProjectionMembershipHistoryId) ev
  backfillIntervalOrganizationsForHistory mh

-- | Resolve the organization profile id for an interval by matching against stored membership histories.
-- Filters by practitioner to avoid loading all history rows.
resolveOrganizationForInterval :: (MonadIO m) => MembershipInterval -> SqlPersistT m (Maybe ProfileRefAC)
resolveOrganizationForInterval mi = do
  histories <-
    selectList
      [MembershipHistoryProjectionPractitionerProfileId ==. membershipIntervalPractitionerId mi]
      []
  let plutusIntervalId = assetClassToPlutus (membershipIntervalId mi)
      matches (Entity _ proj) =
        let plutusOrg = assetClassToPlutus (membershipHistoryProjectionOrganizationProfileId proj)
            plutusPract = assetClassToPlutus (membershipHistoryProjectionPractitionerProfileId proj)
            historyId = deriveMembershipHistoryId plutusOrg plutusPract
            derivedIntervalId = deriveMembershipIntervalId historyId (membershipIntervalIntervalNumber mi)
         in derivedIntervalId == plutusIntervalId
  pure $ membershipHistoryProjectionOrganizationProfileId . entityVal <$> L.find matches histories

-- | When a membership history is stored, backfill organizationProfileId on any interval
-- projections that belong to this history and currently have NULL org (e.g. interval was
-- processed before the history event in the same block).
backfillIntervalOrganizationsForHistory :: (MonadIO m) => MembershipHistory -> SqlPersistT m ()
backfillIntervalOrganizationsForHistory mh = do
  candidates <-
    selectList [MembershipIntervalProjectionPractitionerProfileId ==. membershipHistoryPractitionerId mh] []
  let plutusOrg = assetClassToPlutus (membershipHistoryOrganizationId mh)
      plutusPract = assetClassToPlutus (membershipHistoryPractitionerId mh)
      historyId = deriveMembershipHistoryId plutusOrg plutusPract
      belongsToHistory (Entity _ proj) =
        isNothing (membershipIntervalProjectionOrganizationProfileId proj)
          && deriveMembershipIntervalId historyId (membershipIntervalProjectionIntervalNumber proj)
            == assetClassToPlutus (membershipIntervalProjectionMembershipIntervalId proj)
  forM_ (filter belongsToHistory candidates) $ \entity ->
    update (entityKey entity) [MembershipIntervalProjectionOrganizationProfileId =. Just (membershipHistoryOrganizationId mh)]

-- | Store a membership interval projection, upserting by interval ID.
putMembershipIntervalProjection :: (MonadIO m) => Integer -> Text -> MembershipInterval -> Maybe ProfileRefAC -> SqlPersistT m ()
putMembershipIntervalProjection createdSlot createdHash mi mOrganizationProfileId = do
  now <- liftIO getCurrentTime
  let ev =
        MembershipIntervalProjection
          createdSlot
          createdHash
          (membershipIntervalId mi)
          (membershipIntervalStartDate mi)
          (membershipIntervalEndDate mi)
          (membershipIntervalAccepted mi)
          (membershipIntervalPractitionerId mi)
          mOrganizationProfileId
          (membershipIntervalIntervalNumber mi)
          now
  upsertByUnique (UniqueMembershipIntervalProjection . membershipIntervalProjectionMembershipIntervalId) ev

-- | Store an achievement projection, upserting by achievement ID.
putAchievementProjection :: (MonadIO m) => Integer -> Text -> Achievement -> SqlPersistT m ()
putAchievementProjection createdSlot createdHash a = do
  now <- liftIO getCurrentTime
  let ev =
        AchievementProjection
          createdSlot
          createdHash
          (achievementId a)
          (achievementAwardedToProfileId a)
          (achievementAwardedByProfileId a)
          (achievementAchievementDate a)
          (achievementAccepted a)
          (achievementName a)
          (achievementDescription a)
          (achievementImageURI a)
          (Just $ AchievementOtherMetadataJson $ achievementOtherMetadata a)
          now
  upsertByUnique (UniqueAchievementProjection . achievementProjectionAchievementId) ev

putMembershipHistoryProjection :: (MonadIO m) => Integer -> Text -> MembershipHistory -> SqlPersistT m ()
putMembershipHistoryProjection createdSlot createdHash mh = do
  now <- liftIO getCurrentTime
  let ev =
        MembershipHistoryProjection
          createdSlot
          createdHash
          (membershipHistoryId mh)
          (membershipHistoryPractitionerId mh)
          (membershipHistoryOrganizationId mh)
          now
  upsertByUnique (UniqueMembershipHistoryProjection . membershipHistoryProjectionMembershipHistoryId) ev
  backfillIntervalOrganizationsForHistory mh

-- | Resolve the organization profile id for an interval by matching against stored membership histories.
resolveOrganizationForInterval :: (MonadIO m) => MembershipInterval -> SqlPersistT m (Maybe ProfileRefAC)
resolveOrganizationForInterval mi = do
  histories <- selectList [] []
  let plutusIntervalId = assetClassToPlutus (membershipIntervalId mi)
      matches (Entity _ proj) =
        let plutusOrg = assetClassToPlutus (membershipHistoryProjectionOrganizationProfileId proj)
            plutusPract = assetClassToPlutus (membershipHistoryProjectionPractitionerProfileId proj)
            historyId = deriveMembershipHistoryId plutusOrg plutusPract
            derivedIntervalId = deriveMembershipIntervalId historyId (membershipIntervalNumber mi)
         in derivedIntervalId == plutusIntervalId
              && membershipIntervalPractitionerId mi == membershipHistoryProjectionPractitionerProfileId proj
  pure $ membershipHistoryProjectionOrganizationProfileId . entityVal <$> L.find matches histories

-- | When a membership history is stored, backfill organizationProfileId on any interval
-- projections that belong to this history and currently have NULL org (e.g. interval was
-- processed before the history event in the same block).
backfillIntervalOrganizationsForHistory :: (MonadIO m) => MembershipHistory -> SqlPersistT m ()
backfillIntervalOrganizationsForHistory mh = do
  candidates <-
    selectList [MembershipIntervalProjectionPractitionerProfileId ==. membershipHistoryPractitionerId mh] []
  let plutusOrg = assetClassToPlutus (membershipHistoryOrganizationId mh)
      plutusPract = assetClassToPlutus (membershipHistoryPractitionerId mh)
      historyId = deriveMembershipHistoryId plutusOrg plutusPract
      belongsToHistory (Entity _ proj) =
        isNothing (membershipIntervalProjectionOrganizationProfileId proj)
          && deriveMembershipIntervalId historyId (membershipIntervalProjectionIntervalNumber proj)
            == assetClassToPlutus (membershipIntervalProjectionMembershipIntervalId proj)
  forM_ (filter belongsToHistory candidates) $ \entity ->
    update (entityKey entity) [MembershipIntervalProjectionOrganizationProfileId =. Just (membershipHistoryOrganizationId mh)]

putMembershipIntervalProjection :: (MonadIO m) => Integer -> Text -> MembershipInterval -> Maybe ProfileRefAC -> SqlPersistT m ()
putMembershipIntervalProjection createdSlot createdHash mi mOrganizationProfileId = do
  now <- liftIO getCurrentTime
  let ev =
        MembershipIntervalProjection
          createdSlot
          createdHash
          (membershipIntervalId mi)
          (membershipIntervalStartDate mi)
          (membershipIntervalEndDate mi)
          (membershipIntervalIsAccepted mi)
          (membershipIntervalPractitionerId mi)
          mOrganizationProfileId
          (membershipIntervalNumber mi)
          now
  upsertByUnique (UniqueMembershipIntervalProjection . membershipIntervalProjectionMembershipIntervalId) ev

putAchievementProjection :: (MonadIO m) => Integer -> Text -> Achievement -> SqlPersistT m ()
putAchievementProjection createdSlot createdHash a = do
  now <- liftIO getCurrentTime
  let ev =
        AchievementProjection
          createdSlot
          createdHash
          (achievementId a)
          (achievementAwardedTo a)
          (achievementAwardedBy a)
          (achievementDate a)
          (achievementIsAccepted a)
          (achievementName a)
          (achievementDescription a)
          (achievementImageURI a)
          now
  upsertByUnique (UniqueAchievementProjection . achievementProjectionAchievementId) ev

-- | Rollback all stored events and projections strictly beyond the given slot,
--   and any rows at the slot with a mismatching block header hash.
rollbackTo :: (MonadIO m) => Integer -> Text -> SqlPersistT m ()
rollbackTo slot hdrHash = do
  -- Remove onchain matches beyond tip or same slot but different header
  rollbackEntity OnchainMatchEventCreatedSlot OnchainMatchEventCreatedHeader

  -- Remove projections beyond tip or same slot but different header
  rollbackEntity ProfileProjectionCreatedAtSlot ProfileProjectionCreatedAtHash
  rollbackEntity RankProjectionCreatedAtSlot RankProjectionCreatedAtHash
  rollbackEntity PromotionProjectionCreatedAtSlot PromotionProjectionCreatedAtHash
  rollbackEntity MembershipHistoryProjectionCreatedAtSlot MembershipHistoryProjectionCreatedAtHash
  rollbackEntity MembershipIntervalProjectionCreatedAtSlot MembershipIntervalProjectionCreatedAtHash
  rollbackEntity AchievementProjectionCreatedAtSlot AchievementProjectionCreatedAtHash
  where
    rollbackEntity ::
      (PersistEntity a, PersistEntityBackend a ~ SqlBackend, MonadIO m) =>
      EntityField a Integer ->
      EntityField a Text ->
      SqlPersistT m ()
    rollbackEntity slotField hashField = do
      deleteWhere [slotField >. slot]
      deleteWhere [slotField ==. slot, hashField !=. hdrHash]
