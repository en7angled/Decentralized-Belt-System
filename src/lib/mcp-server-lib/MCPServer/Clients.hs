{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- | @servant-client@-derived @ClientM@ functions over the re-declared
-- @query-api@ and @interaction-api@ types. One helper 'runUpstream' wraps the
-- IO plumbing around 'ClientM' so tool modules don't see @runClientM@ or
-- @mkClientEnv@ directly.
module MCPServer.Clients
  ( -- * Query API clients
    getPractitioner
  , getOrganization
  , getPractitionerDetail
  , getOrganizationDetail
  , listProfiles
  , getPromotionsPage
  , getBeltFrequency
  , getAchievements
  , getAchievementById
  , search
  , getPendingActions
  , checkPromotionEligibility
    -- * Interaction API clients
  , buildTx
    -- * Runners
  , runUpstreamQuery
  , runUpstreamInteraction
  ) where

import Data.Text (Text)
import DomainTypes.Core.BJJ (BJJBelt)
import DomainTypes.Core.Types
  ( AchievementAC
  , ProfileRefAC
  , ProfileType
  , PromotionState
  , RankAC
  )
import DomainTypes.Transfer.OrderBy
  ( AchievementsOrderBy
  , ProfilesOrderBy
  , PromotionsOrderBy
  , SortOrder
  )
import DomainTypes.Transfer.QueryResponses
  ( AchievementInformationResponse
  , AchievementResponse
  , OrganizationDetailResponse
  , OrganizationProfileResponse
  , PendingActionsResponse
  , PractitionerDetailResponse
  , PractitionerProfileResponse
  , ProfileResponse
  , PromotionEligibilityResponse
  , PromotionsPageResponse
  , SearchResults
  )
import GeniusYield.Types (GYTime)
import MCPServer.Api.Interaction (proxyInteractionAPI)
import MCPServer.Api.Query (proxyQueryAPI)
import MCPServer.App (AppCtx (..))
import MCPServer.Orphans ()
import Servant (BasicAuthData, (:<|>) ((:<|>)))
import Servant.Client
  ( BaseUrl
  , ClientEnv
  , ClientError
  , ClientM
  , client
  , mkClientEnv
  , runClientM
  )
import TxBuilding.Interactions (Interaction)

-- ---------------------------------------------------------------------------
-- Query API clients
-- ---------------------------------------------------------------------------

-- @client proxyQueryAPI :: BasicAuthData -> <eleven-way ':<|>' alternation>@
-- so each exposed function takes 'BasicAuthData' first, applies it, and
-- destructures to the route it wants. This keeps call sites unchanged
-- (@runUpstreamQuery ctx (getPractitioner auth pid)@) at the cost of
-- repeating the wildcard pattern per function. The underscore-count tracks
-- the route's position in the re-declared @QueryAPI@.
getPractitioner :: BasicAuthData -> ProfileRefAC -> ClientM PractitionerProfileResponse
getPractitioner auth = case client proxyQueryAPI auth of
  f :<|> _ -> f

getOrganization :: BasicAuthData -> ProfileRefAC -> ClientM OrganizationProfileResponse
getOrganization auth = case client proxyQueryAPI auth of
  _ :<|> f :<|> _ -> f

getPractitionerDetail :: BasicAuthData -> ProfileRefAC -> ClientM PractitionerDetailResponse
getPractitionerDetail auth = case client proxyQueryAPI auth of
  _ :<|> _ :<|> f :<|> _ -> f

getOrganizationDetail :: BasicAuthData -> ProfileRefAC -> ClientM OrganizationDetailResponse
getOrganizationDetail auth = case client proxyQueryAPI auth of
  _ :<|> _ :<|> _ :<|> f :<|> _ -> f

listProfiles
  :: BasicAuthData
  -> Maybe Int
  -> Maybe Int
  -> [ProfileRefAC]
  -> Maybe ProfileType
  -> Maybe ProfileRefAC
  -> Maybe ProfileRefAC
  -> [BJJBelt]
  -> Maybe Text
  -> Maybe ProfilesOrderBy
  -> Maybe SortOrder
  -> ClientM [ProfileResponse]
listProfiles auth = case client proxyQueryAPI auth of
  _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _ -> f

getPromotionsPage
  :: BasicAuthData
  -> Maybe Int
  -> Maybe Int
  -> [RankAC]
  -> [BJJBelt]
  -> [ProfileRefAC]
  -> [ProfileRefAC]
  -> [ProfileRefAC]
  -> Maybe Text
  -> Maybe GYTime
  -> Maybe GYTime
  -> [PromotionState]
  -> Maybe PromotionsOrderBy
  -> Maybe SortOrder
  -> ClientM PromotionsPageResponse
getPromotionsPage auth = case client proxyQueryAPI auth of
  _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _ -> f

getBeltFrequency :: BasicAuthData -> ClientM [(BJJBelt, Int)]
getBeltFrequency auth = case client proxyQueryAPI auth of
  _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _ -> f

getAchievements
  :: BasicAuthData
  -> Maybe Int
  -> Maybe Int
  -> [AchievementAC]
  -> [ProfileRefAC]
  -> [ProfileRefAC]
  -> Maybe Bool
  -> Maybe GYTime
  -> Maybe GYTime
  -> Maybe Text
  -> Maybe AchievementsOrderBy
  -> Maybe SortOrder
  -> ClientM [AchievementResponse]
getAchievements auth = case client proxyQueryAPI auth of
  _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _ -> f

getAchievementById :: BasicAuthData -> AchievementAC -> ClientM AchievementInformationResponse
getAchievementById auth = case client proxyQueryAPI auth of
  _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _ -> f

search :: BasicAuthData -> Text -> ClientM SearchResults
search auth = case client proxyQueryAPI auth of
  _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _ -> f

getPendingActions :: BasicAuthData -> ProfileRefAC -> ClientM PendingActionsResponse
getPendingActions auth = case client proxyQueryAPI auth of
  _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f :<|> _ -> f

checkPromotionEligibility
  :: BasicAuthData
  -> ProfileRefAC
  -> BJJBelt
  -> Maybe ProfileRefAC
  -> ClientM PromotionEligibilityResponse
checkPromotionEligibility auth = case client proxyQueryAPI auth of
  _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> f -> f

-- ---------------------------------------------------------------------------
-- Interaction API clients
-- ---------------------------------------------------------------------------

buildTx :: BasicAuthData -> Interaction -> ClientM String
buildTx = client proxyInteractionAPI

-- ---------------------------------------------------------------------------
-- Runners
-- ---------------------------------------------------------------------------

-- | Run a 'ClientM' against @query-api@ using the shared 'httpManager' and
-- base URL from 'AppCtx'.
runUpstreamQuery :: AppCtx -> ClientM a -> IO (Either ClientError a)
runUpstreamQuery ctx = runClientM' (mkEnv (queryBaseUrl ctx) ctx)

-- | Run a 'ClientM' against @interaction-api@.
runUpstreamInteraction :: AppCtx -> ClientM a -> IO (Either ClientError a)
runUpstreamInteraction ctx = runClientM' (mkEnv (interactionBaseUrl ctx) ctx)

mkEnv :: BaseUrl -> AppCtx -> ClientEnv
mkEnv url ctx = mkClientEnv (httpManager ctx) url

runClientM' :: ClientEnv -> ClientM a -> IO (Either ClientError a)
runClientM' = flip runClientM
