
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant $" #-}
module TxBuilding.Transactions where

import Control.Monad (when)
import Control.Monad.Reader
import Data.Text
import Data.Tuple.Extra (first)
import GeniusYield.GYConfig
import GeniusYield.Types
import Text.Printf (printf)
import qualified Text.Printf as Printf
import TxBuilding.Context
import TxBuilding.Interactions
import TxBuilding.Skeletons
import TxBuilding.Utils
import TxBuilding.Validators
import Utils

------------------------------------------------------------------------------------------------

-- * Transaction Submitting

------------------------------------------------------------------------------------------------
submitTxAndWaitForConfirmation :: Bool -> GYProviders -> GYTx -> IO GYTxId
submitTxAndWaitForConfirmation wait ctxProviders gyTx = do
  gyTxId <- gySubmitTx ctxProviders gyTx
  printf (yellowColorString "Submitted transaction: \n\t %s") gyTxId
  when wait do
    putStrLn (yellowColorString "Waiting for confirmation ...")
    gyAwaitTxConfirmed ctxProviders (GYAwaitTxParameters 30 10000000 1) gyTxId
    printf (greenColorString "Confirmed:  \n\t %s") gyTxId
  return gyTxId

interactionToTxBody ::
  (MonadReader TxBuildingContext m, MonadIO m) =>
  Interaction ->
  m (GYTxBody, GYAssetClass)
interactionToTxBody i@Interaction {..} = do
  TxBuildingContext {..} <- ask
  let builderMonadSkeleton = runReaderT (interactionToTxSkeleton i) validatorsCtx
  liftIO $ runTx providerCtx (usedAddresses userAddresses) (changeAddress userAddresses) Nothing builderMonadSkeleton

interactionToUnsignedTx :: (MonadReader TxBuildingContext m, MonadIO m) => Interaction -> m (GYTx, GYAssetClass)
interactionToUnsignedTx = fmap (first unsignedTx) . interactionToTxBody

------------------------------------------------------------------------------------------------

-- * Transaction Running

------------------------------------------------------------------------------------------------

addressFromSkey :: ProviderCtx -> GYExtendedPaymentSigningKey -> GYAddress
addressFromSkey pCtx skey =
  let nid = (cfgNetworkId . ctxCoreCfg) pCtx
   in addressFromPaymentSigningKey nid skey

runBJJActionWithPK :: TxBuildingContext -> GYExtendedPaymentSigningKey -> ActionType -> Maybe GYAddress -> IO (GYTxId, GYAssetClass)
runBJJActionWithPK txBuildingCtx@TxBuildingContext {..} skey action optionalRecipient = do
  let my_addr = addressFromSkey providerCtx skey
  let userAddrs = UserAddresses [my_addr] my_addr Nothing
  let interaction = Interaction action userAddrs optionalRecipient
  print interaction
  putStrLn (yellowColorString "Building transaction...")
  (txBody, assetClass) <- runReaderT (interactionToTxBody interaction) txBuildingCtx
  let txSigned = signGYTxBody txBody [skey]
  putStrLn $ yellowColorString ("Built and signed by: \n\t" <> Data.Text.unpack (addressToText my_addr))
  txId <- submitTxAndWaitForConfirmation True (ctxProviders providerCtx) txSigned
  return (txId, assetClass)

deployReferenceScript :: ProviderCtx -> GYScript 'PlutusV3 -> GYExtendedPaymentSigningKey -> IO GYTxOutRef
deployReferenceScript providerCtx script skey = do
  let my_addr = addressFromSkey providerCtx skey
  putStrLn $ yellowColorString $ "Deploying reference script"
  putStrLn $ yellowColorString $ "Deployed Script Hash: \n\t" <> Printf.printf "%s" (scriptHash script)
  putStrLn $ yellowColorString $ "Deployed to Address: \n\t" <> Printf.printf "%s" (addressToText my_addr)
  txBody <- runTx' providerCtx [my_addr] my_addr Nothing (addRefScriptToAddressSkeleton my_addr script)
  let txSigned = signGYTxBody txBody [skey]
  putStrLn $ yellowColorString ("Built and signed by: \n\t" <> Data.Text.unpack (addressToText my_addr))
  gyTxId <- submitTxAndWaitForConfirmation True (ctxProviders providerCtx) txSigned
  return $ txOutRefFromTuple (gyTxId, 0)

deployReferenceScripts :: ProviderCtx -> GYExtendedPaymentSigningKey -> IO ProfileTxBuildingContext
deployReferenceScripts providerCtx skey = do
  refMintingPolicy <- deployReferenceScript providerCtx mintingPolicyGY skey
  refProfilesValidator <- deployReferenceScript providerCtx profilesValidatorGY skey
  refRanksValidator <- deployReferenceScript providerCtx ranksValidatorGY skey
  return
    ProfileTxBuildingContext
      { profilesValidatorRef = refProfilesValidator,
        ranksValidatorRef = refRanksValidator,
        mintingPolicyRef = refMintingPolicy
      }
