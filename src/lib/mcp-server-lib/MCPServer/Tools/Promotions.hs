{-# LANGUAGE OverloadedStrings #-}

-- | MCP tools backed by @query-api@'s promotion routes and
-- @interaction-api@'s promotion tx-build flow:
--
-- * @get_promotions_page@           — wraps @GET \/pages\/promotions@ (paginated
--   list + total + monthly histogram; the parent plan consolidated the bare
--   list variant with the page aggregate since the page form is a strict
--   superset).
-- * @get_belt_frequency@            — wraps @GET \/promotions\/frequency@.
-- * @check_promotion_eligibility@   — wraps
--   @GET \/practitioner\/{id}\/eligibility@ (off-chain mirror of the on-chain
--   validator, with structured violations + earliest-eligible date + required
--   granter rank).
-- * @build_promote_rank_tx@         — wraps @POST \/build-tx@ with
--   'PromoteProfileAction'.
-- * @build_accept_promotion_tx@     — wraps @POST \/build-tx@ with
--   'AcceptPromotionAction'.
--
-- Write tools are gated on 'MCPServer.App.enableWriteTx' and are absent from
-- @tools/list@ when disabled.
module MCPServer.Tools.Promotions
  ( tools
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value)
import Data.Map.Strict (Map)
import qualified Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import DomainTypes.Core.Actions
  ( ProfileActionType
      ( AcceptPromotionAction
      , PromoteProfileAction
      )
  )
import MCP.Server
  ( InputSchema
  , ProcessResult (ProcessSuccess)
  , ToolHandler
  , toolHandler
  )
import qualified MCPServer.Clients as C
import MCPServer.App (AppCtx (..))
import MCPServer.Schema
  ( FieldSpec
  , arrayOf
  , beltField
  , dateField
  , emptyInputSchema
  , enumField
  , objectSchema
  , orderByField
  , paginationFields
  , profileRefField
  , rankRefField
  , sortOrderField
  , stringField
  )
import MCPServer.Tools.Common
  ( errorResult
  , jsonResult
  , optionalArg
  , requireArg
  , runUpstreamQuery
  , sanitizeClientError
  )
import MCPServer.Tools.Transactions
  ( buildTxTool
  , userAddressFields
  , userAddressRequired
  )

-- | Read tools are always registered; write tools appear only when
-- 'enableWriteTx' is set, so @tools/list@ is an honest reflection of
-- deployed capability.
tools :: AppCtx -> [ToolHandler]
tools ctx =
  [ getPromotionsPage ctx
  , getBeltFrequency ctx
  , checkPromotionEligibility ctx
  ]
    <> [buildPromoteRankTx ctx | enableWriteTx ctx]
    <> [buildAcceptPromotionTx ctx | enableWriteTx ctx]

-- ---------------------------------------------------------------------------
-- Read tools
-- ---------------------------------------------------------------------------

getPromotionsPage :: AppCtx -> ToolHandler
getPromotionsPage ctx =
  toolHandler
    "get_promotions_page"
    ( Just
        "Paginated promotion list with total count and monthly belt histogram. \
        \Every filter from /pages/promotions is exposed."
    )
    promotionsPageSchema
    $ \args ->
      case ( optionalArg "limit" args
           , optionalArg "offset" args
           , optionalArg "promotions" args
           , optionalArg "belts" args
           , optionalArg "achieved_by" args
           , optionalArg "awarded_by" args
           , optionalArg "profiles" args
           , optionalArg "q" args
           , optionalArg "from" args
           , optionalArg "to" args
           , optionalArg "states" args
           , optionalArg "order_by" args
           , optionalArg "sort_order" args
           ) of
        ( Right lim
          , Right off
          , Right promos
          , Right belts
          , Right achieved
          , Right awarded
          , Right profiles
          , Right q
          , Right from
          , Right to_
          , Right states
          , Right ord
          , Right dir
          ) -> do
            let call =
                  C.getPromotionsPage
                    (upstreamAuth ctx)
                    lim
                    off
                    (Data.Maybe.fromMaybe [] promos)
                    (Data.Maybe.fromMaybe [] belts)
                    (Data.Maybe.fromMaybe [] achieved)
                    (Data.Maybe.fromMaybe [] awarded)
                    (Data.Maybe.fromMaybe [] profiles)
                    q
                    from
                    to_
                    (Data.Maybe.fromMaybe [] states)
                    ord
                    dir
            r <- liftIO (runUpstreamQuery ctx call)
            pure . ProcessSuccess $ either (errorResult . sanitizeClientError) jsonResult r
        _ ->
          pure (ProcessSuccess (errorResult "invalid get_promotions_page arguments"))

promotionsPageSchema :: InputSchema
promotionsPageSchema =
  objectSchema
    ( paginationFields
        <> [ ("promotions", arrayOf rankRefField)
           , ("belts", arrayOf beltField)
           , ("achieved_by", arrayOf profileRefField)
           , ("awarded_by", arrayOf profileRefField)
           , ("profiles", arrayOf profileRefField)
           , ("q", stringField (Just "Fuzzy text search."))
           , ("from", dateField (Just "Inclusive lower bound (ISO-8601)."))
           , ("to", dateField (Just "Exclusive upper bound (ISO-8601)."))
           ,
             ( "states"
             , arrayOf (enumField ["pending", "accepted", "superseded"] (Just "Promotion state filter."))
             )
           ,
             ( "order_by"
             , orderByField ["id", "belt", "achieved_by", "awarded_by", "date"]
             )
           , ("sort_order", sortOrderField)
           ]
    )
    []

getBeltFrequency :: AppCtx -> ToolHandler
getBeltFrequency ctx =
  toolHandler
    "get_belt_frequency"
    (Just "Count of promotions per BJJ belt across the protocol.")
    emptyInputSchema
    $ \_args -> do
      r <- liftIO (runUpstreamQuery ctx (C.getBeltFrequency (upstreamAuth ctx)))
      pure . ProcessSuccess $ either (errorResult . sanitizeClientError) jsonResult r

-- | Off-chain promotion-eligibility check. Omitting @granter_profile_id@
-- skips the master-authority check and reports the minimum granter rank
-- required — use this to answer "am I eligible in principle?" before the
-- practitioner picks a coach.
checkPromotionEligibility :: AppCtx -> ToolHandler
checkPromotionEligibility ctx =
  toolHandler
    "check_promotion_eligibility"
    ( Just
        "Off-chain mirror of the on-chain promotion validator. Given a \
        \practitioner profile id and a target belt, returns an eligibility \
        \verdict plus structured violations (master-belt-too-low, \
        \rung-skipped, insufficient-time-in-grade, master-date-after-promotion, \
        \student-date-not-monotonic), the earliest date the time-in-grade \
        \requirement will be satisfied, and the minimum belt a granter must \
        \hold. If granter_profile_id is omitted the authority check is \
        \skipped and the response still reports required_granter_rank — use \
        \this to answer 'am I eligible in principle?' without a specific \
        \coach."
    )
    eligibilitySchema
    $ \args ->
      case ( requireArg "profile_id" args
           , requireArg "target_belt" args
           , optionalArg "granter_profile_id" args
           ) of
        (Right pid, Right belt, Right mGranter) -> do
          let call = C.checkPromotionEligibility (upstreamAuth ctx) pid belt mGranter
          r <- liftIO (runUpstreamQuery ctx call)
          pure . ProcessSuccess $ either (errorResult . sanitizeClientError) jsonResult r
        (eP, eB, eG) ->
          let msgs = errsOf eP <> errsOf eB <> errsOf eG
           in pure (ProcessSuccess (errorResult (T.intercalate "; " msgs)))
  where
    errsOf = either pure (const [])

eligibilitySchema :: InputSchema
eligibilitySchema =
  objectSchema
    [ ("profile_id", profileRefField)
    , ("target_belt", beltField)
    , ("granter_profile_id", profileRefField)
    ]
    ["profile_id", "target_belt"]

-- ---------------------------------------------------------------------------
-- Write tools (gated on enableWriteTx)
-- ---------------------------------------------------------------------------

buildPromoteRankTx :: AppCtx -> ToolHandler
buildPromoteRankTx =
  buildTxTool
    "build_promote_rank_tx"
    ( Just
        "Build an unsigned tx that nominates a practitioner for promotion to \
        \the given BJJ belt. Returns hex-encoded CBOR; the wallet must sign \
        \and submit separately. Note: the promotion is pending until the \
        \recipient accepts via build_accept_promotion_tx."
    )
    promoteRankSchema
    decodePromoteRank

promoteRankSchema :: InputSchema
promoteRankSchema =
  objectSchema
    (promoteRankFields <> userAddressFields)
    (promoteRankRequired <> userAddressRequired)

promoteRankFields :: [(Text, FieldSpec)]
promoteRankFields =
  [ ("promoted_profile_id", profileRefField)
  , ("promoted_by_profile_id", profileRefField)
  , ("achievement_date", dateField (Just "Promotion award date (ISO-8601)."))
  , ("promoted_belt", beltField)
  ]

promoteRankRequired :: [Text]
promoteRankRequired =
  [ "promoted_profile_id"
  , "promoted_by_profile_id"
  , "achievement_date"
  , "promoted_belt"
  ]

decodePromoteRank :: Maybe (Map Text Value) -> Either Text ProfileActionType
decodePromoteRank args = do
  promotedId <- requireArg "promoted_profile_id" args
  promotedBy <- requireArg "promoted_by_profile_id" args
  date <- requireArg "achievement_date" args
  belt <- requireArg "promoted_belt" args
  pure (PromoteProfileAction promotedId promotedBy date belt)

buildAcceptPromotionTx :: AppCtx -> ToolHandler
buildAcceptPromotionTx =
  buildTxTool
    "build_accept_promotion_tx"
    ( Just
        "Build an unsigned tx that accepts a pending promotion on behalf of \
        \the recipient. Returns hex-encoded CBOR."
    )
    acceptPromotionSchema
    decodeAcceptPromotion

acceptPromotionSchema :: InputSchema
acceptPromotionSchema =
  objectSchema
    (acceptPromotionFields <> userAddressFields)
    (acceptPromotionRequired <> userAddressRequired)

acceptPromotionFields :: [(Text, FieldSpec)]
acceptPromotionFields = [("promotion_id", rankRefField)]

acceptPromotionRequired :: [Text]
acceptPromotionRequired = ["promotion_id"]

decodeAcceptPromotion :: Maybe (Map Text Value) -> Either Text ProfileActionType
decodeAcceptPromotion args = do
  pid <- requireArg "promotion_id" args
  pure (AcceptPromotionAction pid)
