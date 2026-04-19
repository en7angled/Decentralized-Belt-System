{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Shared health, readiness, and protocol status for probe routes and §12.5 home hub.
module Query.ServiceStatus
  ( getHealthProbe,
    getReadyProbe,
    getProtocolStatusQuery,
  )
where

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (asks, runReaderT)
import Data.Text (Text, pack)
import DomainTypes.Transfer.Types (FeeConfigDTO (..), ProtocolStatus (..), ScriptHashesDTO (..))
import GeniusYield.TxBuilder (addressFromPlutus')
import GeniusYield.Types (addressToText)
import Onchain.Protocol.Types qualified as OnchainTypes
import QueryAppMonad (QueryAppMonad (..), QueryAppContext (..), verifyProjectionDbConnection)
import TxBuilding.Context (DeployedScriptsContext (..), runQuery)
import TxBuilding.Exceptions (TxBuildingException (..))
import TxBuilding.Lookups (queryOracleParams)
import Utils (stringFromJSON)
import Constants (appVersion)
import WebAPI.ServiceProbe (ServiceProbeStatus, alwaysHealthy)

getHealthProbe :: (MonadIO m) => m (ServiceProbeStatus Text)
getHealthProbe = alwaysHealthy (pack appVersion) "query-api"

getReadyProbe :: QueryAppMonad (ServiceProbeStatus Text)
getReadyProbe = verifyProjectionDbConnection

getProtocolStatusQuery :: QueryAppMonad ProtocolStatus
getProtocolStatusQuery = do
  mCtx <- asks deployedScriptsCtx
  case mCtx of
    Nothing -> liftIO $ throwIO OracleNotFound
    Just dCtx -> do
      provCtx <- asks providerContext
      liftIO $ runQuery provCtx $ do
        (oracleParams, _, _) <- runReaderT queryOracleParams dCtx
        -- Convert fee address inside GYTxQueryMonad where addressFromPlutus' is available
        mFeeAddr <- case OnchainTypes.opFeeConfig oracleParams of
          Nothing -> return Nothing
          Just fc -> Just . addressToText <$> addressFromPlutus' (OnchainTypes.fcFeeAddress fc)
        return $ oracleParamsToProtocolStatus mFeeAddr (scriptHashesFromCtx dCtx) oracleParams

-- | Extract script hashes from the deployed scripts context as hex text.
scriptHashesFromCtx :: DeployedScriptsContext -> ScriptHashesDTO
scriptHashesFromCtx DeployedScriptsContext {..} =
  ScriptHashesDTO
    { scriptHashesMintingPolicy = hashToText $ fst mintingPolicyHashAndRef,
      scriptHashesProfilesValidator = hashToText $ fst profilesValidatorHashAndRef,
      scriptHashesRanksValidator = hashToText $ fst ranksValidatorHashAndRef,
      scriptHashesMembershipsValidator = hashToText $ fst membershipsValidatorHashAndRef,
      scriptHashesAchievementsValidator = hashToText $ fst achievementsValidatorHashAndRef,
      scriptHashesOracleValidator = hashToText $ fst oracleValidatorHashAndRef
    }
  where
    hashToText = pack . stringFromJSON

-- | Pure conversion from oracle params to protocol status DTO.
-- Fee address is pre-converted to bech32 text by the caller.
oracleParamsToProtocolStatus :: Maybe Text -> ScriptHashesDTO -> OnchainTypes.OracleParams -> ProtocolStatus
oracleParamsToProtocolStatus feeAddrText hashes OnchainTypes.OracleParams {..} =
  ProtocolStatus
    { protocolStatusOpPaused = opPaused,
      protocolStatusMinUtxoValue = opMinUTxOValue,
      protocolStatusFeeConfig = feeConfigToDTO <$> opFeeConfig,
      protocolStatusScriptHashes = Just hashes
    }
  where
    feeConfigToDTO OnchainTypes.FeeConfig {..} =
      FeeConfigDTO
        { feeConfigProfileCreationFee = fcProfileCreationFee,
          feeConfigPromotionFee = fcPromotionFee,
          feeConfigMembershipHistoryFee = fcMembershipHistoryFee,
          feeConfigMembershipIntervalFee = fcMembershipIntervalFee,
          feeConfigAchievementFee = fcAchievementFee,
          feeConfigFeeAddress = feeAddrText
        }
