{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant ^." #-}

module Query.Projected where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader.Class
import Data.MultiSet (fromList, toOccurList)
import qualified Data.Text as T
import Database.Esqueleto.Experimental
import Database.Persist (Entity (..), entityVal)
import qualified Database.Persist as P
import Database.Persist.Sqlite (runSqlite)
import DomainTypes.Core.Types
import DomainTypes.Transfer.Types
import Onchain.BJJ (BJJBelt)
import qualified Query.Common as C
import QueryAppMonad
import Storage
import Types

whenJust :: Maybe a -> (a -> SqlQuery ()) -> SqlQuery ()
whenJust Nothing _ = pure ()
whenJust (Just a) f = f a

getPractitionerProfile :: (MonadIO m, MonadReader QueryAppContext m) => ProfileRefAC -> m PractitionerProfileInformation
getPractitionerProfile profileRefAC = do
  chainsyncDBPath <- asks projectionDbPath
  liftIO $ runSqlite chainsyncDBPath $ do
    mProf <- P.getBy (UniqueProfileProjection profileRefAC)
    case mProf of
      Nothing -> liftIO $ ioError (userError "Practitioner profile not found")
      Just (Entity _ prof) -> do
        ranksAsc <- select $ do
          rp <- from $ table @RankProjection
          where_ (rp ^. RankProjectionRankAchievedByProfileId ==. val profileRefAC)
          orderBy [asc (rp ^. RankProjectionRankAchievementDate)]
          pure rp
        let toRank rp =
              Rank
                { rankId = rankProjectionRankId rp,
                  rankBelt = rankProjectionRankBelt rp,
                  rankAchievedByProfileId = rankProjectionRankAchievedByProfileId rp,
                  rankAwardedByProfileId = rankProjectionRankAwardedByProfileId rp,
                  rankAchievementDate = rankProjectionRankAchievementDate rp
                }
            domainRanks = Prelude.map (toRank . entityVal) ranksAsc
        case Prelude.reverse domainRanks of
          [] -> liftIO $ ioError (userError "No ranks found for practitioner")
          (current : restRev) ->
            return
              PractitionerProfileInformation
                { practitionerId = profileRefAC,
                  practitionerName = profileProjectionProfileName prof,
                  practitionerDescription = profileProjectionProfileDescription prof,
                  practitionerImageURI = profileProjectionProfileImageURI prof,
                  practitionerCurrentRank = current,
                  practitionerPreviousRanks = Prelude.reverse restRev
                }

getOrganizationProfile :: (MonadIO m, MonadReader QueryAppContext m) => ProfileRefAC -> m OrganizationProfileInformation
getOrganizationProfile profileRefAC = do
  chainsyncDBPath <- asks projectionDbPath
  liftIO $ runSqlite chainsyncDBPath $ do
    mProf <- P.getBy (UniqueProfileProjection profileRefAC)
    case mProf of
      Nothing -> liftIO $ ioError (userError "Organization profile not found")
      Just (Entity _ prof) ->
        return
          OrganizationProfileInformation
            { organizationId = profileRefAC,
              organizationName = profileProjectionProfileName prof,
              organizationDescription = profileProjectionProfileDescription prof,
              organizationImageURI = profileProjectionProfileImageURI prof
            }

getProfilesCount :: (MonadIO m, MonadReader QueryAppContext m) => Maybe ProfileType -> m Int
getProfilesCount maybeProfileType = do
  chainsyncDBPath <- asks projectionDbPath
  liftIO $ runSqlite chainsyncDBPath $ do
    cnt <- selectOne $ do
      pp <- from $ table @ProfileProjection
      case maybeProfileType of
        Nothing -> pure countRows
        Just pt -> do
          where_ (pp ^. ProfileProjectionProfileType ==. val pt)
          pure countRows
    pure (maybe 0 unValue cnt)

getProfiles :: (MonadIO m, MonadReader QueryAppContext m) => Maybe (C.Limit, C.Offset) -> Maybe C.ProfileFilter -> Maybe (ProfilesOrderBy, SortOrder) -> m [Profile]
getProfiles maybeLimitOffset maybeProfileFilter maybeOrder = do
  chainsyncDBPath <- asks projectionDbPath
  liftIO $ runSqlite chainsyncDBPath $ do
    rows <- select $ do
      pp <- from $ table @ProfileProjection
      case maybeProfileFilter of
        Nothing -> pure ()
        Just C.ProfileFilter {..} -> do
          whenJust profileFilterId (\ids -> where_ (pp ^. ProfileProjectionProfileId `in_` valList ids))
          whenJust profileFilterType (\pt -> where_ (pp ^. ProfileProjectionProfileType ==. val pt))
          whenJust profileFilterName (\nameSubstring -> where_ (lower_ (pp ^. ProfileProjectionProfileName) `like` val (T.pack "%" <> T.toLower nameSubstring <> T.pack "%")))
          whenJust profileFilterDescription (\descriptionSubstring -> where_ (lower_ (pp ^. ProfileProjectionProfileDescription) `like` val (T.pack "%" <> T.toLower descriptionSubstring <> T.pack "%")))
      case maybeOrder of
        Nothing -> pure ()
        Just (ob, so) ->
          let dir f = case so of Types.Asc -> asc f; Types.Desc -> desc f
           in case ob of
                ProfilesOrderById -> orderBy [dir (pp ^. ProfileProjectionProfileId)]
                ProfilesOrderByName -> orderBy [dir (pp ^. ProfileProjectionProfileName)]
                ProfilesOrderByDescription -> orderBy [dir (pp ^. ProfileProjectionProfileDescription)]
                ProfilesOrderByType -> orderBy [dir (pp ^. ProfileProjectionProfileType)]
      case maybeLimitOffset of
        Nothing -> pure ()
        Just (l, o) -> do
          offset (fromIntegral o)
          limit (fromIntegral l)
      pure pp
    let toProfile pp =
          Profile
            { profileId = profileProjectionProfileId pp,
              profileName = profileProjectionProfileName pp,
              profileDescription = profileProjectionProfileDescription pp,
              profileImageURI = profileProjectionProfileImageURI pp,
              profileType = profileProjectionProfileType pp
            }
    pure (Prelude.map (toProfile . entityVal) rows)

getPromotions :: (MonadIO m, MonadReader QueryAppContext m) => Maybe (C.Limit, C.Offset) -> Maybe C.PromotionFilter -> Maybe (PromotionsOrderBy, SortOrder) -> m [Promotion]
getPromotions maybeLimitOffset maybePromotionFilter maybeOrder = do
  chainsyncDBPath <- asks projectionDbPath
  liftIO $ runSqlite chainsyncDBPath $ do
    rows <- select $ do
      pr <- from $ table @PromotionProjection
      case maybePromotionFilter of
        Nothing -> pure ()
        Just C.PromotionFilter {..} -> do
          whenJust promotionFilterId (\ids -> where_ (pr ^. PromotionProjectionPromotionId `in_` valList ids))
          whenJust promotionFilterBelt (\belts -> where_ (pr ^. PromotionProjectionPromotionBelt `in_` valList belts))
          whenJust promotionFilterAchievedByProfileId (\ids -> where_ (pr ^. PromotionProjectionPromotionAchievedByProfileId `in_` valList ids))
          whenJust promotionFilterAwardedByProfileId (\ids -> where_ (pr ^. PromotionProjectionPromotionAwardedByProfileId `in_` valList ids))
          case promotionFilterAchievementDateInterval of
            (Nothing, Nothing) -> pure ()
            (Just from, Nothing) -> where_ (pr ^. PromotionProjectionPromotionAchievementDate >=. val from)
            (Nothing, Just to) -> where_ (pr ^. PromotionProjectionPromotionAchievementDate <=. val to)
            (Just from, Just to) -> where_ (pr ^. PromotionProjectionPromotionAchievementDate >=. val from &&. pr ^. PromotionProjectionPromotionAchievementDate <=. val to)
      case maybeOrder of
        Nothing -> pure ()
        Just (ob, so) ->
          let dir f = case so of Types.Asc -> asc f; Types.Desc -> desc f
           in case ob of
                PromotionsOrderById -> orderBy [dir (pr ^. PromotionProjectionPromotionId)]
                PromotionsOrderByBelt -> orderBy [dir (pr ^. PromotionProjectionPromotionBelt)]
                PromotionsOrderByAchievedBy -> orderBy [dir (pr ^. PromotionProjectionPromotionAchievedByProfileId)]
                PromotionsOrderByAwardedBy -> orderBy [dir (pr ^. PromotionProjectionPromotionAwardedByProfileId)]
                PromotionsOrderByDate -> orderBy [dir (pr ^. PromotionProjectionPromotionAchievementDate)]
      case maybeLimitOffset of
        Nothing -> pure ()
        Just (l, o) -> do
          offset (fromIntegral o)
          limit (fromIntegral l)
      pure pr
    let toPromotion p =
          Promotion
            { promotionId = promotionProjectionPromotionId p,
              promotionBelt = promotionProjectionPromotionBelt p,
              promotionAchievedByProfileId = promotionProjectionPromotionAchievedByProfileId p,
              promotionAwardedByProfileId = promotionProjectionPromotionAwardedByProfileId p,
              promotionAchievementDate = promotionProjectionPromotionAchievementDate p
            }
    pure (Prelude.map (toPromotion . entityVal) rows)

getPromotionsCount :: (MonadIO m, MonadReader QueryAppContext m) => Maybe C.PromotionFilter -> m Int
getPromotionsCount maybePromotionFilter = do
  chainsyncDBPath <- asks projectionDbPath
  liftIO $ runSqlite chainsyncDBPath $ do
    cnt <- selectOne $ do
      pr <- from $ table @PromotionProjection
      case maybePromotionFilter of
        Nothing -> pure countRows
        Just C.PromotionFilter {..} -> do
          whenJust promotionFilterId (\ids -> where_ (pr ^. PromotionProjectionPromotionId `in_` valList ids))
          whenJust promotionFilterBelt (\belts -> where_ (pr ^. PromotionProjectionPromotionBelt `in_` valList belts))
          whenJust promotionFilterAchievedByProfileId (\ids -> where_ (pr ^. PromotionProjectionPromotionAchievedByProfileId `in_` valList ids))
          whenJust promotionFilterAwardedByProfileId (\ids -> where_ (pr ^. PromotionProjectionPromotionAwardedByProfileId `in_` valList ids))
          case promotionFilterAchievementDateInterval of
            (Nothing, Nothing) -> pure ()
            (Just from, Nothing) -> where_ (pr ^. PromotionProjectionPromotionAchievementDate >=. val from)
            (Nothing, Just to) -> where_ (pr ^. PromotionProjectionPromotionAchievementDate <=. val to)
            (Just from, Just to) -> where_ (pr ^. PromotionProjectionPromotionAchievementDate >=. val from &&. pr ^. PromotionProjectionPromotionAchievementDate <=. val to)
          pure countRows
    pure (maybe 0 unValue cnt)

getRanks :: (MonadIO m, MonadReader QueryAppContext m) => Maybe (C.Limit, C.Offset) -> Maybe C.RankFilter -> Maybe (RanksOrderBy, SortOrder) -> m [Rank]
getRanks maybeLimitOffset maybeRankFilter maybeOrder = do
  chainsyncDBPath <- asks projectionDbPath
  liftIO $ runSqlite chainsyncDBPath $ do
    rows <- select $ do
      rp <- from $ table @RankProjection
      case maybeRankFilter of
        Nothing -> pure ()
        Just C.RankFilter {..} -> do
          whenJust rankFilterId (\ids -> where_ (rp ^. RankProjectionRankId `in_` valList ids))
          whenJust rankFilterBelt (\belts -> where_ (rp ^. RankProjectionRankBelt `in_` valList belts))
          whenJust rankFilterAchievedByProfileId (\ids -> where_ (rp ^. RankProjectionRankAchievedByProfileId `in_` valList ids))
          whenJust rankFilterAwardedByProfileId (\ids -> where_ (rp ^. RankProjectionRankAwardedByProfileId `in_` valList ids))
          case rankFilterAchievementDateInterval of
            (Nothing, Nothing) -> pure ()
            (Just from, Nothing) -> where_ (rp ^. RankProjectionRankAchievementDate >=. val from)
            (Nothing, Just to) -> where_ (rp ^. RankProjectionRankAchievementDate <=. val to)
            (Just from, Just to) -> where_ (rp ^. RankProjectionRankAchievementDate >=. val from &&. rp ^. RankProjectionRankAchievementDate <=. val to)
      case maybeOrder of
        Nothing -> pure ()
        Just (ob, so) ->
          let dir f = case so of Types.Asc -> asc f; Types.Desc -> desc f
           in case ob of
                RanksOrderById -> orderBy [dir (rp ^. RankProjectionRankId)]
                RanksOrderByBelt -> orderBy [dir (rp ^. RankProjectionRankBelt)]
                RanksOrderByAchievedBy -> orderBy [dir (rp ^. RankProjectionRankAchievedByProfileId)]
                RanksOrderByAwardedBy -> orderBy [dir (rp ^. RankProjectionRankAwardedByProfileId)]
                RanksOrderByDate -> orderBy [dir (rp ^. RankProjectionRankAchievementDate)]
      case maybeLimitOffset of
        Nothing -> pure ()
        Just (l, o) -> do
          offset (fromIntegral o)
          limit (fromIntegral l)
      pure rp
    let toRank rp =
          Rank
            { rankId = rankProjectionRankId rp,
              rankBelt = rankProjectionRankBelt rp,
              rankAchievedByProfileId = rankProjectionRankAchievedByProfileId rp,
              rankAwardedByProfileId = rankProjectionRankAwardedByProfileId rp,
              rankAchievementDate = rankProjectionRankAchievementDate rp
            }
    pure (Prelude.map (toRank . entityVal) rows)

getRanksCount :: (MonadIO m, MonadReader QueryAppContext m) => Maybe C.RankFilter -> m Int
getRanksCount maybeRankFilter = do
  chainsyncDBPath <- asks projectionDbPath
  liftIO $ runSqlite chainsyncDBPath $ do
    cnt <- selectOne $ do
      rp <- from $ table @RankProjection
      case maybeRankFilter of
        Nothing -> pure countRows
        Just C.RankFilter {..} -> do
          whenJust rankFilterId (\ids -> where_ (rp ^. RankProjectionRankId `in_` valList ids))
          whenJust rankFilterBelt (\belts -> where_ (rp ^. RankProjectionRankBelt `in_` valList belts))
          whenJust rankFilterAchievedByProfileId (\ids -> where_ (rp ^. RankProjectionRankAchievedByProfileId `in_` valList ids))
          whenJust rankFilterAwardedByProfileId (\ids -> where_ (rp ^. RankProjectionRankAwardedByProfileId `in_` valList ids))
          case rankFilterAchievementDateInterval of
            (Nothing, Nothing) -> pure ()
            (Just from, Nothing) -> where_ (rp ^. RankProjectionRankAchievementDate >=. val from)
            (Nothing, Just to) -> where_ (rp ^. RankProjectionRankAchievementDate <=. val to)
            (Just from, Just to) -> where_ (rp ^. RankProjectionRankAchievementDate >=. val from &&. rp ^. RankProjectionRankAchievementDate <=. val to)
          pure countRows
    pure (maybe 0 unValue cnt)

getBeltTotals :: (MonadIO m, MonadReader QueryAppContext m) => m [(BJJBelt, Int)]
getBeltTotals = do
  chainsyncDBPath <- asks projectionDbPath
  liftIO $ runSqlite chainsyncDBPath $ do
    rows <- select $ do
      rp <- from $ table @RankProjection
      pure (rp ^. RankProjectionRankBelt)
    let belts = Prelude.map unValue rows
    pure (toOccurList . fromList $ belts)
