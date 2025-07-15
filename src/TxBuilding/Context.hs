module TxBuilding.Context where

import Data.Aeson
import GHC.Generics (Generic)
import GeniusYield.GYConfig
import GeniusYield.TxBuilder
import GeniusYield.Types
import Prelude

------------------------------------------------------------------------------------------------

-- * Profile Transaction Building Context

------------------------------------------------------------------------------------------------

-- | Context for profile transaction building operations
data ProfileTxBuildingContext = ProfileTxBuildingContext
  { mintingPolicyRef :: GYTxOutRef,
    profilesValidatorRef :: GYTxOutRef,
    ranksValidatorRef :: GYTxOutRef
  }
  deriving stock (Generic, Prelude.Show)
  deriving anyclass (ToJSON, FromJSON)

------------------------------------------------------------------------------------------------

-- * Default Context

------------------------------------------------------------------------------------------------

data ProviderCtx = ProviderCtx
  { ctxCoreCfg :: !GYCoreConfig,
    ctxProviders :: !GYProviders
  }

-- | To run for simple queries, the one which don't requiring building for transaction skeleton.
runQuery :: ProviderCtx -> GYTxQueryMonadIO a -> IO a
runQuery ctx q = do
  let nid = cfgNetworkId $ ctxCoreCfg ctx
      providers = ctxProviders ctx
  runGYTxQueryMonadIO nid providers q

-- | Tries to build for given skeleton.
runTx' ::
  ProviderCtx ->
  -- | User's used addresses.
  [GYAddress] ->
  -- | User's change address.
  GYAddress ->
  -- | Browser wallet's reserved collateral (if set).
  Maybe GYTxOutRefCbor ->
  GYTxBuilderMonadIO (GYTxSkeleton v) ->
  IO GYTxBody
runTx' ctx addrs addr collateral skeleton = do
  let nid = cfgNetworkId $ ctxCoreCfg ctx
      providers = ctxProviders ctx
  runGYTxBuilderMonadIO
    nid
    providers
    addrs
    addr
    ( collateral
        >>= ( \c ->
                Just
                  ( getTxOutRefHex c,
                    True -- Make this as `False` to not do 5-ada-only check for value in this given UTxO to be used as collateral.
                  )
            )
    )
    (skeleton >>= buildTxBody)

-- | Tries to build for given skeleton.
runTx ::
  ProviderCtx ->
  -- | User's used addresses.
  [GYAddress] ->
  -- | User's change address.
  GYAddress ->
  -- | Browser wallet's reserved collateral (if set).
  Maybe GYTxOutRefCbor ->
  GYTxBuilderMonadIO (GYTxSkeleton 'PlutusV3, GYAssetClass) ->
  IO (GYTxBody, GYAssetClass)
runTx ctx addrs addr collateral skeleton = do
  let nid = cfgNetworkId $ ctxCoreCfg ctx
      providers = ctxProviders ctx

  runGYTxBuilderMonadIO
    nid
    providers
    addrs
    addr
    ( collateral
        >>= ( \c ->
                Just
                  ( getTxOutRefHex c,
                    True -- Make this as `False` to not do 5-ada-only check for value in this given UTxO to be used as collateral.
                  )
            )
    )
    (skeleton >>= \(txSklt, mac) -> (,mac) <$> buildTxBody txSklt)

data TxBuildingContext = TxBuildingContext
  { ledgerCtx :: ProfileTxBuildingContext,
    providerCtx :: ProviderCtx
  }
