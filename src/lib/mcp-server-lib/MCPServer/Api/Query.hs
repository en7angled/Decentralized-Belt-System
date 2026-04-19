{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

-- | Re-declaration of the subset of @query-api@'s 'PublicAPI' that the MCP
-- server calls as an HTTP client. Only the eleven read routes from
-- [plan-the-implementation-frolicking-wall.md:312-324] and the Phase-2
-- sub-plan's tool table are included; `profiles-by-wallet` and the other
-- @pages\/*@ aggregates are intentionally omitted until a tool is defined
-- for them. No @liveprojection@ flag — every route pins to the default
-- (projected/DB) path per Phase 2 Decision (see Plan §2b).
--
-- The Phase 0 DTO promotion lets us reuse the exact response types from
-- @offchain-lib/DomainTypes/Transfer/QueryResponses.hs@ so there is no
-- schema drift risk between @query-api@ and this client.
module MCPServer.Api.Query
  ( QueryAPI
  , proxyQueryAPI
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
import Servant
  ( BasicAuth
  , Capture
  , Get
  , JSON
  , Optional
  , Proxy (Proxy)
  , QueryParam'
  , QueryParams
  , Required
  , (:<|>)
  , (:>)
  )
import WebAPI.Auth (AuthUser)

-- | Shape of every route the MCP server invokes against @query-api@. The
-- leading @BasicAuth@ combinator is attached at the wrapper type below so
-- each route definition mirrors what the @query-api@ publishes verbatim.
type QueryRoutes =
  -- GET /practitioner/{profile-id}
  ( "practitioner"
      :> Capture "profile-id" ProfileRefAC
      :> Get '[JSON] PractitionerProfileResponse
  )
    :<|>
    -- GET /organization/{profile-id}
    ( "organization"
        :> Capture "profile-id" ProfileRefAC
        :> Get '[JSON] OrganizationProfileResponse
    )
    :<|>
    -- GET /practitioner/{profile-id}/detail
    ( "practitioner"
        :> Capture "profile-id" ProfileRefAC
        :> "detail"
        :> Get '[JSON] PractitionerDetailResponse
    )
    :<|>
    -- GET /organization/{profile-id}/detail
    ( "organization"
        :> Capture "profile-id" ProfileRefAC
        :> "detail"
        :> Get '[JSON] OrganizationDetailResponse
    )
    :<|>
    -- GET /profiles
    ( "profiles"
        :> QueryParam' '[Optional] "limit" Int
        :> QueryParam' '[Optional] "offset" Int
        :> QueryParams "profile" ProfileRefAC
        :> QueryParam' '[Optional] "profile_type" ProfileType
        :> QueryParam' '[Optional] "active_membership_organization" ProfileRefAC
        :> QueryParam' '[Optional] "membership_organization" ProfileRefAC
        :> QueryParams "belt" BJJBelt
        :> QueryParam' '[Optional] "q" Text
        :> QueryParam' '[Optional] "order_by" ProfilesOrderBy
        :> QueryParam' '[Optional] "sort_order" SortOrder
        :> Get '[JSON] [ProfileResponse]
    )
    :<|>
    -- GET /pages/promotions
    ( "pages"
        :> "promotions"
        :> QueryParam' '[Optional] "limit" Int
        :> QueryParam' '[Optional] "offset" Int
        :> QueryParams "promotion" RankAC
        :> QueryParams "belt" BJJBelt
        :> QueryParams "achieved_by" ProfileRefAC
        :> QueryParams "awarded_by" ProfileRefAC
        :> QueryParams "profile" ProfileRefAC
        :> QueryParam' '[Optional] "q" Text
        :> QueryParam' '[Optional] "from" GYTime
        :> QueryParam' '[Optional] "to" GYTime
        :> QueryParams "state" PromotionState
        :> QueryParam' '[Optional] "order_by" PromotionsOrderBy
        :> QueryParam' '[Optional] "sort_order" SortOrder
        :> Get '[JSON] PromotionsPageResponse
    )
    :<|>
    -- GET /promotions/frequency
    ( "promotions"
        :> "frequency"
        :> Get '[JSON] [(BJJBelt, Int)]
    )
    :<|>
    -- GET /achievements
    ( "achievements"
        :> QueryParam' '[Optional] "limit" Int
        :> QueryParam' '[Optional] "offset" Int
        :> QueryParams "achievement" AchievementAC
        :> QueryParams "awarded_to" ProfileRefAC
        :> QueryParams "awarded_by" ProfileRefAC
        :> QueryParam' '[Optional] "accepted" Bool
        :> QueryParam' '[Optional] "from" GYTime
        :> QueryParam' '[Optional] "to" GYTime
        :> QueryParam' '[Optional] "q" Text
        :> QueryParam' '[Optional] "order_by" AchievementsOrderBy
        :> QueryParam' '[Optional] "sort_order" SortOrder
        :> Get '[JSON] [AchievementResponse]
    )
    :<|>
    -- GET /achievement/{achievement-id}
    ( "achievement"
        :> Capture "achievement-id" AchievementAC
        :> Get '[JSON] AchievementInformationResponse
    )
    :<|>
    -- GET /search?q=...
    ( "search"
        :> QueryParam' '[Required] "q" Text
        :> Get '[JSON] SearchResults
    )
    :<|>
    -- GET /pages/pending-actions/{profile-id}
    ( "pages"
        :> "pending-actions"
        :> Capture "profile-id" ProfileRefAC
        :> Get '[JSON] PendingActionsResponse
    )
    :<|>
    -- GET /practitioner/{profile-id}/eligibility?target=...&granter=...
    ( "practitioner"
        :> Capture "profile-id" ProfileRefAC
        :> "eligibility"
        :> QueryParam' '[Required] "target" BJJBelt
        :> QueryParam' '[Optional] "granter" ProfileRefAC
        :> Get '[JSON] PromotionEligibilityResponse
    )

-- | Full Servant client type: every @QueryRoutes@ path wrapped in HTTP Basic
-- authentication, matching the server-side wrapper at
-- [src/exe/query-api/RestAPI.hs:650](../../../../exe/query-api/RestAPI.hs#L650).
type QueryAPI =
  BasicAuth "user-realm" AuthUser :> QueryRoutes

proxyQueryAPI :: Proxy QueryAPI
proxyQueryAPI = Proxy
