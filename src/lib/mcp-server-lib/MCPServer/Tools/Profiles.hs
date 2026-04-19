{-# LANGUAGE OverloadedStrings #-}

-- | MCP tools backed by @query-api@'s profile read routes:
--
-- * @get_practitioner_profile@ — wraps @GET \/practitioner\/{profile-id}@
-- * @get_organization_profile@ — wraps @GET \/organization\/{profile-id}@
-- * @get_practitioner_detail@  — wraps @GET \/practitioner\/{profile-id}\/detail@
-- * @get_organization_detail@  — wraps @GET \/organization\/{profile-id}\/detail@
-- * @list_profiles@            — wraps @GET \/profiles@ with every query-api filter
module MCPServer.Tools.Profiles
  ( tools
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Maybe
import qualified MCPServer.Clients as C
import MCP.Server
  ( InputSchema
  , ProcessResult (ProcessSuccess)
  , ToolHandler
  , toolHandler
  )
import MCPServer.App (AppCtx (..))
import MCPServer.Schema
  ( beltField
  , enumField
  , orderByField
  , paginationFields
  , profileRefField
  , sortOrderField
  , stringField
  , objectSchema
  , arrayOf
  )
import MCPServer.Tools.Common
  ( errorResult
  , jsonResult
  , optionalArg
  , requireArg
  , runUpstreamQuery
  , sanitizeClientError
  )

-- | Export every read tool in this module bound to the current 'AppCtx'.
tools :: AppCtx -> [ToolHandler]
tools ctx =
  [ getPractitionerProfile ctx
  , getOrganizationProfile ctx
  , getPractitionerDetail ctx
  , getOrganizationDetail ctx
  , listProfiles ctx
  ]

getPractitionerProfile :: AppCtx -> ToolHandler
getPractitionerProfile ctx =
  toolHandler
    "get_practitioner_profile"
    (Just "Look up a BJJ practitioner's profile, rank, and lineage.")
    (objectSchema [("profile_id", profileRefField)] ["profile_id"])
    $ \args -> case requireArg "profile_id" args of
      Left e -> pure (ProcessSuccess (errorResult e))
      Right pid -> do
        r <- liftIO (runUpstreamQuery ctx (C.getPractitioner (upstreamAuth ctx) pid))
        pure . ProcessSuccess $ either (errorResult . sanitizeClientError) jsonResult r

getOrganizationProfile :: AppCtx -> ToolHandler
getOrganizationProfile ctx =
  toolHandler
    "get_organization_profile"
    (Just "Look up a BJJ organization (academy/gym) profile.")
    (objectSchema [("profile_id", profileRefField)] ["profile_id"])
    $ \args -> case requireArg "profile_id" args of
      Left e -> pure (ProcessSuccess (errorResult e))
      Right pid -> do
        r <- liftIO (runUpstreamQuery ctx (C.getOrganization (upstreamAuth ctx) pid))
        pure . ProcessSuccess $ either (errorResult . sanitizeClientError) jsonResult r

getPractitionerDetail :: AppCtx -> ToolHandler
getPractitionerDetail ctx =
  toolHandler
    "get_practitioner_detail"
    (Just
       "Practitioner profile with achievements, memberships, promotions, \
       \organization lookup map, and awarder profiles in one aggregate response.")
    (objectSchema [("profile_id", profileRefField)] ["profile_id"])
    $ \args -> case requireArg "profile_id" args of
      Left e -> pure (ProcessSuccess (errorResult e))
      Right pid -> do
        r <- liftIO (runUpstreamQuery ctx (C.getPractitionerDetail (upstreamAuth ctx) pid))
        pure . ProcessSuccess $ either (errorResult . sanitizeClientError) jsonResult r

getOrganizationDetail :: AppCtx -> ToolHandler
getOrganizationDetail ctx =
  toolHandler
    "get_organization_detail"
    (Just
       "Organization profile with memberships, achievements issued/received, \
       \and member practitioner profiles in one aggregate response.")
    (objectSchema [("profile_id", profileRefField)] ["profile_id"])
    $ \args -> case requireArg "profile_id" args of
      Left e -> pure (ProcessSuccess (errorResult e))
      Right pid -> do
        r <- liftIO (runUpstreamQuery ctx (C.getOrganizationDetail (upstreamAuth ctx) pid))
        pure . ProcessSuccess $ either (errorResult . sanitizeClientError) jsonResult r

listProfiles :: AppCtx -> ToolHandler
listProfiles ctx =
  toolHandler
    "list_profiles"
    (Just
       "List BJJ profiles with filters: profile-type, belt, current / historical \
       \membership organization, fuzzy text, pagination, ordering.")
    profilesSchema
    $ \args ->
      case ( optionalArg "limit" args
           , optionalArg "offset" args
           , optionalArg "profile_ids" args
           , optionalArg "profile_type" args
           , optionalArg "active_membership_organization" args
           , optionalArg "membership_organization" args
           , optionalArg "belts" args
           , optionalArg "q" args
           , optionalArg "order_by" args
           , optionalArg "sort_order" args
           ) of
        ( Right lim
          , Right off
          , Right pids
          , Right ptype
          , Right activeOrg
          , Right histOrg
          , Right belts
          , Right q
          , Right ord
          , Right dir
          ) -> do
            let call =
                  C.listProfiles
                    (upstreamAuth ctx)
                    lim
                    off
                    (Data.Maybe.fromMaybe [] pids)
                    ptype
                    activeOrg
                    histOrg
                    (Data.Maybe.fromMaybe [] belts)
                    q
                    ord
                    dir
            r <- liftIO (runUpstreamQuery ctx call)
            pure . ProcessSuccess $ either (errorResult . sanitizeClientError) jsonResult r
        _ -> pure (ProcessSuccess (errorResult "invalid list_profiles arguments"))

profilesSchema :: InputSchema
profilesSchema =
  objectSchema
    ( paginationFields
        <> [ ("profile_ids", arrayOf profileRefField)
           , ("profile_type", enumField ["Practitioner", "Organization"] (Just "Filter by profile type."))
           , ("active_membership_organization", profileRefField)
           , ("membership_organization", profileRefField)
           , ("belts", arrayOf beltField)
           , ("q", stringField (Just "Fuzzy text search across name and description."))
           , ( "order_by"
             , orderByField ["id", "name", "description", "type", "registered_at"]
             )
           , ("sort_order", sortOrderField)
           ]
    )
    []
