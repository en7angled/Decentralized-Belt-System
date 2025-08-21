{-# LANGUAGE RecordWildCards #-}

module AppMonad where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Data.List (sortOn)
import Data.Maybe
import Data.MultiSet
import Data.Ord (Down (..))
import Data.Text hiding (elem, take, reverse)
import qualified Data.Text.Encoding
import DomainTypes.Core.Types
import DomainTypes.Transfer.Types
import GeniusYield.GYConfig (GYCoreConfig (..))
import GeniusYield.TxBuilder (GYTxQueryMonad)
import GeniusYield.Types
import Onchain.BJJ
import Servant
import TxBuilding.Context
import TxBuilding.Lookups
import Types
import qualified Data.Text as Text
import qualified Data.Text as T
import qualified Database.Persist as P
import Database.Persist (Entity (..), entityVal)
import Database.Persist.Sqlite (runSqlite)
import Database.Esqueleto.Experimental
import Storage

-- | Apply optional pagination limits to a list in a total and generic way.
-- Negative limits or offsets are clamped to 0. If the list is shorter than
-- the requested limit, the entire available suffix is returned safely.
applyLimits :: Maybe (Int, Int) -> [a] -> [a]
applyLimits Nothing xs = xs
applyLimits (Just (limit, offset)) xs =
  let safeLimit = Prelude.max 0 limit
      safeOffset = Prelude.max 0 offset
   in Prelude.take safeLimit (Prelude.drop safeOffset xs)

------------------------------------------------------------------------------------------------

-- * Capabilities

------------------------------------------------------------------------------------------------

type Limit = Int

type Offset = Int

data ProfileFilter = ProfileFilter
  { profileFilterId :: Maybe [ProfileRefAC],
    profileFilterType :: Maybe ProfileType,
    profileFilterName :: Maybe Text,
    profileFilterDescription :: Maybe Text
  }

-- | Profiles queries
getPractitionerProfile :: (MonadReader TxBuildingContext m, MonadIO m) => ProfileRefAC -> m PractitionerProfileInformation
getPractitionerProfile profileRefAC = do
  TxBuildingContext {..} <- ask
  liftIO $ runQuery providerCtx $ getPractiotionerInformation profileRefAC

getOrganizationProfile :: (MonadReader TxBuildingContext m, MonadIO m) => ProfileRefAC -> m OrganizationProfileInformation
getOrganizationProfile profileRefAC = do
  TxBuildingContext {..} <- ask
  liftIO $ runQuery providerCtx $ getOrganizationInformation profileRefAC

getProfilesCount :: (MonadReader TxBuildingContext m, MonadIO m) => Maybe ProfileType -> m Int
getProfilesCount maybeProfileType = do
  TxBuildingContext {..} <- ask
  liftIO $ runQuery providerCtx $ getAllProfilesCount (cfgNetworkId . ctxCoreCfg $ providerCtx)

getProfiles :: (MonadReader TxBuildingContext m, MonadIO m) => Maybe (Limit, Offset) -> Maybe ProfileFilter -> Maybe (ProfilesOrderBy, SortOrder) -> m [Profile]
getProfiles maybeLimitOffset maybeProfileFilter maybeOrder = do
  TxBuildingContext {..} <- ask
  allProfiles <- liftIO $ runQuery providerCtx (getAllProfiles (cfgNetworkId . ctxCoreCfg $ providerCtx))
  return $ applyLimits maybeLimitOffset $ applyOrdering maybeOrder $ applyProfileFilter maybeProfileFilter allProfiles
  where
    applyOrdering :: Maybe (ProfilesOrderBy, SortOrder) -> [Profile] -> [Profile]
    applyOrdering Nothing profiles = profiles
    applyOrdering (Just (orderBy, order)) profiles =
      case (orderBy, order) of
        (ProfilesOrderById, Types.Asc) -> sortOn profileId profiles
        (ProfilesOrderById, Types.Desc) -> sortOn (Down . profileId) profiles
        (ProfilesOrderByName, Types.Asc) -> sortOn profileName profiles
        (ProfilesOrderByName, Types.Desc) -> sortOn (Down . profileName) profiles
        (ProfilesOrderByDescription, Types.Asc) -> sortOn profileDescription profiles
        (ProfilesOrderByDescription, Types.Desc) -> sortOn (Down . profileDescription) profiles
        (ProfilesOrderByType, Types.Asc) -> sortOn profileType profiles
        (ProfilesOrderByType, Types.Desc) -> sortOn (Down . profileType) profiles

    applyProfileFilter :: Maybe ProfileFilter -> [Profile] -> [Profile]
    applyProfileFilter Nothing profiles = profiles
    applyProfileFilter (Just ProfileFilter {..}) profiles =
      let idFilter = case profileFilterId of
            Just ids -> Prelude.filter ((`elem` ids) . profileId)
            Nothing -> id
          typeFilter = case profileFilterType of
            Just pt -> Prelude.filter ((== pt) . profileType)
            Nothing -> id
          nameFilter = case profileFilterName of
            Just name -> Prelude.filter ((Text.toLower name `Text.isInfixOf`) . Text.toLower . profileName)
            Nothing -> id
          descriptionFilter = case profileFilterDescription of
            Just description -> Prelude.filter ((== description) . profileDescription)
            Nothing -> id
       in idFilter . typeFilter . nameFilter . descriptionFilter $ profiles

data PromotionFilter = PromotionFilter
  { promotionFilterId :: Maybe [ProfileRefAC],
    promotionFilterBelt :: Maybe [BJJBelt],
    promotionFilterAchievedByProfileId :: Maybe [ProfileRefAC],
    promotionFilterAwardedByProfileId :: Maybe [ProfileRefAC],
    promotionFilterAchievementDateInterval :: (Maybe GYTime, Maybe GYTime)
  }

-- | Promotions queries
getPromotions :: (MonadReader TxBuildingContext m, MonadIO m) => Maybe (Limit, Offset) -> Maybe PromotionFilter -> Maybe (PromotionsOrderBy, SortOrder) -> m [Promotion]
getPromotions maybeLimitOffset maybePromotionFilter maybeOrder = do
  TxBuildingContext {..} <- ask
  allPromotions <- liftIO $ runQuery providerCtx (getAllPromotions (cfgNetworkId . ctxCoreCfg $ providerCtx))
  return $ applyLimits maybeLimitOffset $ applyOrdering maybeOrder $ applyPromotionFilter maybePromotionFilter allPromotions
  where
    applyOrdering :: Maybe (PromotionsOrderBy, SortOrder) -> [Promotion] -> [Promotion]
    applyOrdering Nothing promotions = promotions
    applyOrdering (Just (orderBy, order)) promotions =
      case (orderBy, order) of
        (PromotionsOrderById, Types.Asc) -> sortOn promotionId promotions
        (PromotionsOrderById, Types.Desc) -> sortOn (Down . promotionId) promotions
        (PromotionsOrderByBelt, Types.Asc) -> sortOn promotionBelt promotions
        (PromotionsOrderByBelt, Types.Desc) -> sortOn (Down . promotionBelt) promotions
        (PromotionsOrderByAchievedBy, Types.Asc) -> sortOn promotionAchievedByProfileId promotions
        (PromotionsOrderByAchievedBy, Types.Desc) -> sortOn (Down . promotionAchievedByProfileId) promotions
        (PromotionsOrderByAwardedBy, Types.Asc) -> sortOn promotionAwardedByProfileId promotions
        (PromotionsOrderByAwardedBy, Types.Desc) -> sortOn (Down . promotionAwardedByProfileId) promotions
        (PromotionsOrderByDate, Types.Asc) -> sortOn promotionAchievementDate promotions
        (PromotionsOrderByDate, Types.Desc) -> sortOn (Down . promotionAchievementDate) promotions

    applyPromotionFilter :: Maybe PromotionFilter -> [Promotion] -> [Promotion]
    applyPromotionFilter Nothing promotions = promotions
    applyPromotionFilter (Just PromotionFilter {..}) promotions =
      let idFilter = case promotionFilterId of
            Just ids -> Prelude.filter ((`elem` ids) . promotionId)
            Nothing -> id
          beltFilter = case promotionFilterBelt of
            Just belts -> Prelude.filter ((`elem` belts) . promotionBelt)
            Nothing -> id
          achievedByFilter = case promotionFilterAchievedByProfileId of
            Just ids -> Prelude.filter ((`elem` ids) . promotionAchievedByProfileId)
            Nothing -> id
          awardedByFilter = case promotionFilterAwardedByProfileId of
            Just ids -> Prelude.filter ((`elem` ids) . promotionAwardedByProfileId)
            Nothing -> id
          achievementDateFilter = case promotionFilterAchievementDateInterval of
            (Just from, Just to) -> Prelude.filter (\promotion -> promotionAchievementDate promotion >= from && promotionAchievementDate promotion <= to)
            (Nothing, Just to) -> Prelude.filter (\promotion -> promotionAchievementDate promotion <= to)
            (Just from, Nothing) -> Prelude.filter (\promotion -> promotionAchievementDate promotion >= from)
            (Nothing, Nothing) -> id
       in idFilter . beltFilter . achievedByFilter . awardedByFilter . achievementDateFilter $ promotions

getPromotionsCount :: (MonadReader TxBuildingContext m, MonadIO m) => Maybe PromotionFilter -> m Int
getPromotionsCount maybePromotionFilter = Prelude.length <$> getPromotions Nothing maybePromotionFilter Nothing

data RankFilter = RankFilter
  { rankFilterId :: Maybe [RankAC],
    rankFilterBelt :: Maybe [BJJBelt],
    rankFilterAchievedByProfileId :: Maybe [ProfileRefAC],
    rankFilterAwardedByProfileId :: Maybe [ProfileRefAC],
    rankFilterAchievementDateInterval :: (Maybe GYTime, Maybe GYTime)
  }

-- | Ranks queries
getRanks :: (MonadReader TxBuildingContext m, MonadIO m) => Maybe (Limit, Offset) -> Maybe RankFilter -> Maybe (RanksOrderBy, SortOrder) -> m [Rank]
getRanks maybeLimitOffset maybeRankFilter maybeOrder = do
  TxBuildingContext {..} <- ask
  allRanks <- liftIO $ runQuery providerCtx (getAllRanks (cfgNetworkId . ctxCoreCfg $ providerCtx))
  return $ applyLimits maybeLimitOffset $ applyOrdering maybeOrder $ applyRankFilter maybeRankFilter allRanks
  where
    applyOrdering :: Maybe (RanksOrderBy, SortOrder) -> [Rank] -> [Rank]
    applyOrdering Nothing ranks = ranks
    applyOrdering (Just (orderBy, order)) ranks =
      case (orderBy, order) of
        (RanksOrderById, Types.Asc) -> sortOn rankId ranks
        (RanksOrderById, Types.Desc) -> sortOn (Down . rankId) ranks
        (RanksOrderByBelt, Types.Asc) -> sortOn rankBelt ranks
        (RanksOrderByBelt, Types.Desc) -> sortOn (Down . rankBelt) ranks
        (RanksOrderByAchievedBy, Types.Asc) -> sortOn rankAchievedByProfileId ranks
        (RanksOrderByAchievedBy, Types.Desc) -> sortOn (Down . rankAchievedByProfileId) ranks
        (RanksOrderByAwardedBy, Types.Asc) -> sortOn rankAwardedByProfileId ranks
        (RanksOrderByAwardedBy, Types.Desc) -> sortOn (Down . rankAwardedByProfileId) ranks
        (RanksOrderByDate, Types.Asc) -> sortOn rankAchievementDate ranks
        (RanksOrderByDate, Types.Desc) -> sortOn (Down . rankAchievementDate) ranks

    applyRankFilter :: Maybe RankFilter -> [Rank] -> [Rank]
    applyRankFilter Nothing ranks = ranks
    applyRankFilter (Just RankFilter {..}) ranks =
      let idFilter = case rankFilterId of
            Just ids -> Prelude.filter ((`elem` ids) . rankId)
            Nothing -> id
          beltFilter = case rankFilterBelt of
            Just belts -> Prelude.filter ((`elem` belts) . rankBelt)
            Nothing -> id
          achievedByFilter = case rankFilterAchievedByProfileId of
            Just ids -> Prelude.filter ((`elem` ids) . rankAchievedByProfileId)
            Nothing -> id
          awardedByFilter = case rankFilterAwardedByProfileId of
            Just ids -> Prelude.filter ((`elem` ids) . rankAwardedByProfileId)
            Nothing -> id
          achievementDateFilter = case rankFilterAchievementDateInterval of
            (Just from, Just to) -> Prelude.filter (\rank -> rankAchievementDate rank >= from && rankAchievementDate rank <= to)
            (Nothing, Just to) -> Prelude.filter (\rank -> rankAchievementDate rank <= to)
            (Just from, Nothing) -> Prelude.filter (\rank -> rankAchievementDate rank >= from)
            (Nothing, Nothing) -> id
       in idFilter . beltFilter . achievedByFilter . awardedByFilter . achievementDateFilter $ ranks

getRanksCount :: (MonadReader TxBuildingContext m, MonadIO m) => Maybe RankFilter -> m Int
getRanksCount maybeRankFilter = Prelude.length <$> getRanks Nothing maybeRankFilter Nothing

getBeltTotals :: (MonadReader TxBuildingContext m, MonadIO m) => m [(BJJBelt, Int)]
getBeltTotals = do
  allRanks <- getRanks Nothing Nothing Nothing
  let allBelts = Prelude.map rankBelt allRanks
  let beltTotals = toOccurList . fromList $ allBelts
  return beltTotals

------------------------------------------------------------------------------------------------

-- | Database-backed implementations (projection queries)

chainsyncDBPath :: T.Text
chainsyncDBPath = T.pack "db/chainsync.sqlite"

whenJust :: Maybe a -> (a -> SqlQuery ()) -> SqlQuery ()
whenJust Nothing _ = pure ()
whenJust (Just a) f = f a

-- Profiles (DB)
getPractitionerProfileProjected :: (MonadIO m) => ProfileRefAC -> m PractitionerProfileInformation
getPractitionerProfileProjected profileRefAC = liftIO $ runSqlite chainsyncDBPath $ do
  mProf <- P.getBy (UniqueProfileProjection profileRefAC)
  case mProf of
    Nothing -> liftIO $ throwIO (userError "Practitioner profile not found")
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
      case reverse domainRanks of
        [] -> liftIO $ throwIO (userError "No ranks found for practitioner")
        (current:restRev) ->
          return
            PractitionerProfileInformation
              { practitionerId = profileRefAC,
                practitionerName = profileProjectionProfileName prof,
                practitionerDescription = profileProjectionProfileDescription prof,
                practitionerImageURI = profileProjectionProfileImageURI prof,
                practitionerCurrentRank = current,
                practitionerPreviousRanks = reverse restRev
              }

getOrganizationProfileProjected :: (MonadIO m) => ProfileRefAC -> m OrganizationProfileInformation
getOrganizationProfileProjected profileRefAC = liftIO $ runSqlite chainsyncDBPath $ do
  mProf <- P.getBy (UniqueProfileProjection profileRefAC)
  case mProf of
    Nothing -> liftIO $ throwIO (userError "Organization profile not found")
    Just (Entity _ prof) ->
      return
        OrganizationProfileInformation
          { organizationId = profileRefAC,
            organizationName = profileProjectionProfileName prof,
            organizationDescription = profileProjectionProfileDescription prof,
            organizationImageURI = profileProjectionProfileImageURI prof
          }

getProfilesCountProjected :: (MonadIO m) => Maybe ProfileType -> m Int
getProfilesCountProjected maybeProfileType = liftIO $ runSqlite chainsyncDBPath $ do
  cnt <- selectOne $ do
    pp <- from $ table @ProfileProjection
    case maybeProfileType of
      Nothing -> pure countRows
      Just pt -> do
        where_ (pp ^. ProfileProjectionProfileType ==. val pt)
        pure countRows
  pure (maybe 0 unValue cnt)

getProfilesProjected :: (MonadIO m) => Maybe (Limit, Offset) -> Maybe ProfileFilter -> Maybe (ProfilesOrderBy, SortOrder) -> m [Profile]
getProfilesProjected maybeLimitOffset maybeProfileFilter maybeOrder = liftIO $ runSqlite chainsyncDBPath $ do
  rows <- select $ do
    pp <- from $ table @ProfileProjection
    -- Filters pushed to SQL when possible
    case maybeProfileFilter of
      Nothing -> pure ()
      Just ProfileFilter {..} -> do
        whenJust profileFilterId $ \ids -> where_ (pp ^. ProfileProjectionProfileId `in_` valList ids)
        whenJust profileFilterType $ \pt -> where_ (pp ^. ProfileProjectionProfileType ==. val pt)
    -- Ordering
    case maybeOrder of
      Nothing -> pure ()
      Just (ob, so) ->
        let dir f = case so of Asc -> asc f; Desc -> desc f
         in case ob of
              ProfilesOrderById -> orderBy [dir (pp ^. ProfileProjectionProfileId)]
              ProfilesOrderByName -> orderBy [dir (pp ^. ProfileProjectionProfileName)]
              ProfilesOrderByDescription -> orderBy [dir (pp ^. ProfileProjectionProfileDescription)]
              ProfilesOrderByType -> orderBy [dir (pp ^. ProfileProjectionProfileType)]
    -- Pagination
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
      rawProfiles = Prelude.map (toProfile . entityVal) rows
      -- Apply name/description filters in Haskell for portability
      applyFilter Nothing = id
      applyFilter (Just ProfileFilter {..}) =
        let nameFilterFn = case profileFilterName of
              Nothing -> id
              Just n -> Prelude.filter ((Text.toLower n `Text.isInfixOf`) . Text.toLower . profileName)
            descFilterFn = case profileFilterDescription of
              Nothing -> id
              Just d -> Prelude.filter ((== d) . profileDescription)
         in nameFilterFn . descFilterFn
  return (applyFilter maybeProfileFilter rawProfiles)

-- Promotions (DB)
getPromotionsProjected :: (MonadIO m) => Maybe (Limit, Offset) -> Maybe PromotionFilter -> Maybe (PromotionsOrderBy, SortOrder) -> m [Promotion]
getPromotionsProjected maybeLimitOffset maybePromotionFilter maybeOrder = liftIO $ runSqlite chainsyncDBPath $ do
  rows <- select $ do
    pr <- from $ table @PromotionProjection
    case maybePromotionFilter of
      Nothing -> pure ()
      Just PromotionFilter {..} -> do
        whenJust promotionFilterId $ \ids -> where_ (pr ^. PromotionProjectionPromotionId `in_` valList ids)
        whenJust promotionFilterBelt $ \belts -> where_ (pr ^. PromotionProjectionPromotionBelt `in_` valList belts)
        whenJust promotionFilterAchievedByProfileId $ \ids -> where_ (pr ^. PromotionProjectionPromotionAchievedByProfileId `in_` valList ids)
        whenJust promotionFilterAwardedByProfileId $ \ids -> where_ (pr ^. PromotionProjectionPromotionAwardedByProfileId `in_` valList ids)
        case promotionFilterAchievementDateInterval of
          (Nothing, Nothing) -> pure ()
          (Just from, Nothing) -> where_ (pr ^. PromotionProjectionPromotionAchievementDate >=. val from)
          (Nothing, Just to) -> where_ (pr ^. PromotionProjectionPromotionAchievementDate <=. val to)
          (Just from, Just to) -> where_ (pr ^. PromotionProjectionPromotionAchievementDate >=. val from &&. pr ^. PromotionProjectionPromotionAchievementDate <=. val to)
    case maybeOrder of
      Nothing -> pure ()
      Just (ob, so) ->
        let dir f = case so of Asc -> asc f; Desc -> desc f
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

getPromotionsCountProjected :: (MonadIO m) => Maybe PromotionFilter -> m Int
getPromotionsCountProjected maybePromotionFilter = liftIO $ runSqlite chainsyncDBPath $ do
  cnt <- selectOne $ do
    pr <- from $ table @PromotionProjection
    case maybePromotionFilter of
      Nothing -> pure countRows
      Just PromotionFilter {..} -> do
        whenJust promotionFilterId $ \ids -> where_ (pr ^. PromotionProjectionPromotionId `in_` valList ids)
        whenJust promotionFilterBelt $ \belts -> where_ (pr ^. PromotionProjectionPromotionBelt `in_` valList belts)
        whenJust promotionFilterAchievedByProfileId $ \ids -> where_ (pr ^. PromotionProjectionPromotionAchievedByProfileId `in_` valList ids)
        whenJust promotionFilterAwardedByProfileId $ \ids -> where_ (pr ^. PromotionProjectionPromotionAwardedByProfileId `in_` valList ids)
        case promotionFilterAchievementDateInterval of
          (Nothing, Nothing) -> pure ()
          (Just from, Nothing) -> where_ (pr ^. PromotionProjectionPromotionAchievementDate >=. val from)
          (Nothing, Just to) -> where_ (pr ^. PromotionProjectionPromotionAchievementDate <=. val to)
          (Just from, Just to) -> where_ (pr ^. PromotionProjectionPromotionAchievementDate >=. val from &&. pr ^. PromotionProjectionPromotionAchievementDate <=. val to)
        pure countRows
  pure (maybe 0 unValue cnt)

-- Ranks (DB)
getRanksProjected :: (MonadIO m) => Maybe (Limit, Offset) -> Maybe RankFilter -> Maybe (RanksOrderBy, SortOrder) -> m [Rank]
getRanksProjected maybeLimitOffset maybeRankFilter maybeOrder = liftIO $ runSqlite chainsyncDBPath $ do
  rows <- select $ do
    rp <- from $ table @RankProjection
    case maybeRankFilter of
      Nothing -> pure ()
      Just RankFilter {..} -> do
        whenJust rankFilterId $ \ids -> where_ (rp ^. RankProjectionRankId `in_` valList ids)
        whenJust rankFilterBelt $ \belts -> where_ (rp ^. RankProjectionRankBelt `in_` valList belts)
        whenJust rankFilterAchievedByProfileId $ \ids -> where_ (rp ^. RankProjectionRankAchievedByProfileId `in_` valList ids)
        whenJust rankFilterAwardedByProfileId $ \ids -> where_ (rp ^. RankProjectionRankAwardedByProfileId `in_` valList ids)
        case rankFilterAchievementDateInterval of
          (Nothing, Nothing) -> pure ()
          (Just from, Nothing) -> where_ (rp ^. RankProjectionRankAchievementDate >=. val from)
          (Nothing, Just to) -> where_ (rp ^. RankProjectionRankAchievementDate <=. val to)
          (Just from, Just to) -> where_ (rp ^. RankProjectionRankAchievementDate >=. val from &&. rp ^. RankProjectionRankAchievementDate <=. val to)
    case maybeOrder of
      Nothing -> pure ()
      Just (ob, so) ->
        let dir f = case so of Asc -> asc f; Desc -> desc f
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

getRanksCountProjected :: (MonadIO m) => Maybe RankFilter -> m Int
getRanksCountProjected maybeRankFilter = liftIO $ runSqlite chainsyncDBPath $ do
  cnt <- selectOne $ do
    rp <- from $ table @RankProjection
    case maybeRankFilter of
      Nothing -> pure countRows
      Just RankFilter {..} -> do
        whenJust rankFilterId $ \ids -> where_ (rp ^. RankProjectionRankId `in_` valList ids)
        whenJust rankFilterBelt $ \belts -> where_ (rp ^. RankProjectionRankBelt `in_` valList belts)
        whenJust rankFilterAchievedByProfileId $ \ids -> where_ (rp ^. RankProjectionRankAchievedByProfileId `in_` valList ids)
        whenJust rankFilterAwardedByProfileId $ \ids -> where_ (rp ^. RankProjectionRankAwardedByProfileId `in_` valList ids)
        case rankFilterAchievementDateInterval of
          (Nothing, Nothing) -> pure ()
          (Just from, Nothing) -> where_ (rp ^. RankProjectionRankAchievementDate >=. val from)
          (Nothing, Just to) -> where_ (rp ^. RankProjectionRankAchievementDate <=. val to)
          (Just from, Just to) -> where_ (rp ^. RankProjectionRankAchievementDate >=. val from &&. rp ^. RankProjectionRankAchievementDate <=. val to)
        pure countRows
  pure (maybe 0 unValue cnt)

getBeltTotalsProjected :: (MonadIO m) => m [(BJJBelt, Int)]
getBeltTotalsProjected = liftIO $ runSqlite chainsyncDBPath $ do
  rows <- select $ do
    rp <- from $ table @RankProjection
    pure (rp ^. RankProjectionRankBelt)
  let belts = Prelude.map unValue rows
  pure (toOccurList . fromList $ belts)

------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------

data AuthContext = AuthContext
  { authUser :: Text,
    authPassword :: Text
  }
  deriving (Eq, Show)

data AppContext = AppContext
  { authContext :: AuthContext,
    txBuildingContext :: TxBuildingContext
  }

newtype AppMonad a = AppMonad {unAppMonad :: ReaderT AppContext Servant.Handler a}
  deriving (Functor, Applicative, Monad)

runAppMonad :: AppContext -> AppMonad a -> Servant.Handler a
runAppMonad ctx app = runReaderT (unAppMonad app) ctx

instance MonadIO AppMonad where
  liftIO :: IO a -> AppMonad a
  liftIO = AppMonad . liftIO

instance MonadReader TxBuildingContext AppMonad where
  ask :: AppMonad TxBuildingContext
  ask = AppMonad (asks txBuildingContext)

  local :: (TxBuildingContext -> TxBuildingContext) -> AppMonad a -> AppMonad a
  local f (AppMonad app) =
    let f' :: AppContext -> AppContext
        f' AppContext {authContext, txBuildingContext} = AppContext {authContext, txBuildingContext = f txBuildingContext}
     in AppMonad (local f' app)

-- Removed legacy instances in favor of constrained functions above
