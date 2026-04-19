{-# LANGUAGE OverloadedStrings #-}

-- | Shared infrastructure for every @build_*_tx@ MCP write tool.
--
-- All 7 write tools wrap @POST :8082/build-tx@ on the @interaction-api@.
-- They differ only in their action-specific schema fragment and the closure
-- that decodes tool arguments into a 'ProfileActionType' constructor.
-- Address parsing, 'Interaction' assembly, upstream call, error sanitization,
-- and @{"cbor_hex": ...}@ result rendering are identical across every tool
-- and are centralized here in 'buildTxTool'.
module MCPServer.Tools.Transactions
  ( -- * Helper for individual tool modules
    buildTxTool
    -- * Shared address-schema fragment
  , userAddressFields
  , userAddressRequired
    -- * Address decoders exposed for unit-testing
  , decodeUserAddresses
  , decodeRecipient
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value, object, (.=))
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import DomainTypes.Core.Actions (ProfileActionType)
import GeniusYield.Types (GYAddress)
import MCP.Server
  ( InputSchema
  , ProcessResult (ProcessSuccess)
  , ToolHandler
  , toolHandler
  )
import qualified MCPServer.Clients as C
import MCPServer.App (AppCtx (..))
import MCPServer.Schema (FieldSpec, arrayOf, stringField)
import MCPServer.Tools.Common
  ( errorResult
  , jsonResult
  , optionalAddress
  , optionalArg
  , requireAddress
  , requireAddressList
  , runUpstreamInteraction
  , sanitizeClientError
  )
import TxBuilding.Interactions
  ( ActionType (ProfileAction)
  , Interaction (..)
  , UserAddresses (..)
  )

-- | Shared schema fragment describing the wallet addresses every
-- @build_*_tx@ tool needs. Merges into each tool's own @objectSchema@.
--
-- Address fields accept either bech32 (@addr_test1…@ / @addr1…@) or raw
-- hex bytes; decoding tries bech32 first, then falls back to the default
-- 'GYAddress' 'FromJSON' path. Bech32 is preferred because it is what
-- CIP-30 wallets emit and what LLMs generate reliably.
--
-- * @used_addresses@      — required, every wallet address that may contribute UTxOs;
-- * @change_address@      — required, address receiving balancer change;
-- * @reserved_collateral@ — optional CIP-30 reserved-collateral tx-out-ref (hex CBOR);
-- * @recipient@           — optional override for any funds unlocked by the tx
--                            (defaults to @change_address@ upstream).
userAddressFields :: [(Text, FieldSpec)]
userAddressFields =
  [
    ( "used_addresses"
    , arrayOf (stringField (Just "Wallet used address (bech32 preferred, hex accepted)."))
    )
  ,
    ( "change_address"
    , stringField (Just "Wallet change address (bech32 preferred, hex accepted).")
    )
  ,
    ( "reserved_collateral"
    , stringField (Just "Optional CIP-30 reserved-collateral tx-out-ref (hex CBOR).")
    )
  ,
    ( "recipient"
    , stringField (Just "Optional recipient address (bech32 preferred, hex accepted) for funds unlocked by this tx.")
    )
  ]

-- | Names from 'userAddressFields' that are schema-required. The remaining
-- fields are nullable.
userAddressRequired :: [Text]
userAddressRequired = ["used_addresses", "change_address"]

-- | Decode the wallet 'UserAddresses' block out of a tool-argument map.
-- Required fields missing or malformed produce a specific 'Text' error.
-- Addresses accept either bech32 or hex (see 'userAddressFields').
decodeUserAddresses :: Maybe (Map Text Value) -> Either Text UserAddresses
decodeUserAddresses args = do
  used <- requireAddressList "used_addresses" args
  change <- requireAddress "change_address" args
  reserved <- optionalArg "reserved_collateral" args
  pure
    UserAddresses
      { usedAddresses = used
      , changeAddress = change
      , reservedCollateral = reserved
      }

-- | Decode the optional @recipient@ override address (bech32 or hex).
decodeRecipient :: Maybe (Map Text Value) -> Either Text (Maybe GYAddress)
decodeRecipient = optionalAddress "recipient"

-- | The pipeline shared by every write tool:
--
-- 1. Decode 'UserAddresses' and the optional recipient.
-- 2. Run the per-tool action decoder to produce a 'ProfileActionType'.
-- 3. Collect /all/ decoder errors on failure — the LLM sees every missing
--    field in one pass instead of discovering them one at a time.
-- 4. On success, assemble an 'Interaction' wrapped with the 'ProfileAction'
--    tag (per 'ActionType'), POST it to @interaction-api@, and render the
--    returned hex CBOR as a JSON @{"cbor_hex": ...}@ object. Upstream
--    errors route through 'sanitizeClientError' before reaching the LLM.
buildTxTool
  :: Text
  -- ^ Tool name (e.g. @build_promote_rank_tx@).
  -> Maybe Text
  -- ^ Human-readable description.
  -> InputSchema
  -- ^ Full schema: action fields + 'userAddressFields'.
  -> (Maybe (Map Text Value) -> Either Text ProfileActionType)
  -- ^ Closure that parses tool arguments into the 'ProfileActionType'
  -- constructor this tool targets.
  -> AppCtx
  -> ToolHandler
buildTxTool name desc schema decodeAction ctx =
  toolHandler name desc schema $ \args ->
    case (decodeUserAddresses args, decodeRecipient args, decodeAction args) of
      (Right uaddrs, Right mRecipient, Right action) -> do
        let interaction =
              Interaction
                { action = ProfileAction action
                , userAddresses = uaddrs
                , recipient = mRecipient
                }
        r <-
          liftIO $
            runUpstreamInteraction ctx (C.buildTx (upstreamAuth ctx) interaction)
        pure . ProcessSuccess $
          either
            (errorResult . sanitizeClientError)
            (\cborHex -> jsonResult (object ["cbor_hex" .= cborHex]))
            r
      (eU, eR, eA) ->
        let msgs = errsOf eU <> errsOf eR <> errsOf eA
         in pure (ProcessSuccess (errorResult (T.intercalate "; " msgs)))

-- Extract the 'Left' payload of an 'Either Text a' as a singleton list so
-- multiple independent decoder errors can be concatenated in one message.
errsOf :: Either Text a -> [Text]
errsOf = either pure (const [])
