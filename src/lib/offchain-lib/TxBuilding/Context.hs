-- | Deployed scripts context, provider context, and runTx/runQuery execution.
module TxBuilding.Context where

import Data.Aeson
import GHC.Generics (Generic)
import GeniusYield.GYConfig
import GeniusYield.TxBuilder
import GeniusYield.Types
import Onchain.Protocol (ProtocolParams)
import Prelude
import TxBuilding.Validators (compileMintingPolicy, mkProtocolParams)

------------------------------------------------------------------------------------------------

-- * Deployed Scripts Context

------------------------------------------------------------------------------------------------

-- | Context for deployed scripts transaction building operations.
-- Includes the oracle validator reference script and the oracle NFT asset class.
-- The oracle datum UTxO is looked up dynamically at the oracle validator address
-- using 'oracleNFTAssetClass' — its TxOutRef changes on every update.
data DeployedScriptsContext = DeployedScriptsContext
  { mintingPolicyHashAndRef :: (GYScriptHash, GYTxOutRef),
    profilesValidatorHashAndRef :: (GYScriptHash, GYTxOutRef),
    ranksValidatorHashAndRef :: (GYScriptHash, GYTxOutRef),
    membershipsValidatorHashAndRef :: (GYScriptHash, GYTxOutRef),
    achievementsValidatorHashAndRef :: (GYScriptHash, GYTxOutRef),
    oracleValidatorHashAndRef :: (GYScriptHash, GYTxOutRef),
    oracleNFTAssetClass :: GYAssetClass
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON, FromJSON)

getMintingPolicyHash :: DeployedScriptsContext -> GYScriptHash
getMintingPolicyHash ctx = fst $ mintingPolicyHashAndRef ctx

getMintingPolicyRef :: DeployedScriptsContext -> GYTxOutRef
getMintingPolicyRef ctx = snd $ mintingPolicyHashAndRef ctx

getProfilesValidatorRef :: DeployedScriptsContext -> GYTxOutRef
getProfilesValidatorRef ctx = snd $ profilesValidatorHashAndRef ctx

getRanksValidatorRef :: DeployedScriptsContext -> GYTxOutRef
getRanksValidatorRef ctx = snd $ ranksValidatorHashAndRef ctx

getMembershipsValidatorRef :: DeployedScriptsContext -> GYTxOutRef
getMembershipsValidatorRef ctx = snd $ membershipsValidatorHashAndRef ctx

getAchievementsValidatorRef :: DeployedScriptsContext -> GYTxOutRef
getAchievementsValidatorRef ctx = snd $ achievementsValidatorHashAndRef ctx

getOracleValidatorRef :: DeployedScriptsContext -> GYTxOutRef
getOracleValidatorRef ctx = snd $ oracleValidatorHashAndRef ctx

-- | Get the compiled minting policy from the oracle NFT in context.
getMintingPolicyFromCtx :: DeployedScriptsContext -> GYScript 'PlutusV3
getMintingPolicyFromCtx ctx =
  compileMintingPolicy (assetClassToPlutus $ oracleNFTAssetClass ctx)

-- | Get the 'ProtocolParams' from the oracle NFT in context.
getProtocolParamsFromCtx :: DeployedScriptsContext -> ProtocolParams
getProtocolParamsFromCtx ctx =
  mkProtocolParams (assetClassToPlutus $ oracleNFTAssetClass ctx)

------------------------------------------------------------------------------------------------

-- * Default Context

------------------------------------------------------------------------------------------------

data ProviderCtx = ProviderCtx
  { ctxCoreCfg :: !GYCoreConfig,
    ctxProviders :: !GYProviders
  }

-- | Extract the network ID from a 'ProviderCtx'.
getNetworkId :: ProviderCtx -> GYNetworkId
getNetworkId = cfgNetworkId . ctxCoreCfg

-- | To run for simple queries, the one which don't requiring building for transaction skeleton.
runQuery :: ProviderCtx -> GYTxQueryMonadIO a -> IO a
runQuery ctx q = do
  let nid = getNetworkId ctx
      providers = ctxProviders ctx
  runGYTxQueryMonadIO nid providers q

-- | Convert optional collateral to the parameter expected by 'runGYTxBuilderMonadIO'.
-- Set the 'Bool' to 'False' to disable the 5-ada-only check for the collateral UTxO.
collateralToRunParam :: Maybe GYTxOutRefCbor -> Maybe (GYTxOutRef, Bool)
collateralToRunParam = fmap (\c -> (getTxOutRefHex c, True))

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
  let nid = getNetworkId ctx
      providers = ctxProviders ctx
  runGYTxBuilderMonadIO
    nid
    providers
    addrs
    addr
    (collateralToRunParam collateral)
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
  GYTxBuilderMonadIO (GYTxSkeleton 'PlutusV3, Maybe GYAssetClass) ->
  IO (GYTxBody, Maybe GYAssetClass)
runTx ctx addrs addr collateral skeleton = do
  let nid = getNetworkId ctx
      providers = ctxProviders ctx
  runGYTxBuilderMonadIO
    nid
    providers
    addrs
    addr
    (collateralToRunParam collateral)
    (skeleton >>= \(txSklt, mac) -> (,mac) <$> buildTxBody txSklt)

data TxBuildingContext = TxBuildingContext
  { deployedScriptsCtx :: DeployedScriptsContext,
    providerCtx :: ProviderCtx
  }
  deriving stock (Generic)
