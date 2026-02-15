{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant $" #-}
module TxBuilding.Transactions where

import Control.Monad (when)
import Control.Monad.Reader
import Data.Text
import GeniusYield.TxBuilder
import GeniusYield.Types
import Onchain.Protocol.Types (OracleParams (..))
import Text.Printf (printf)
import Text.Printf qualified as Printf
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

submitTx :: (MonadReader TxBuildingContext m, MonadIO m) => GYTx -> m GYTxId
submitTx gyTx = do
  providerCtx <- asks providerCtx
  gyTxId <- liftIO $ gySubmitTx (ctxProviders providerCtx) gyTx
  liftIO $ printf (yellowColorString "Submitted transaction: \n\t %s") gyTxId
  return gyTxId

------------------------------------------------------------------------------------------------

-- * Transaction Building

------------------------------------------------------------------------------------------------

-- | Builds a transaction body from an interaction
interactionToTxBody ::
  (MonadReader TxBuildingContext m, MonadIO m) =>
  Interaction ->
  m (GYTxBody, Maybe GYAssetClass)
interactionToTxBody i@Interaction {..} = do
  TxBuildingContext {..} <- ask
  let builderMonadSkeleton = runReaderT (interactionToTxSkeleton i) deployedScriptsCtx
  liftIO $ runTx providerCtx (usedAddresses userAddresses) (changeAddress userAddresses) Nothing builderMonadSkeleton

-- | Builds an unsigned transaction from an interaction
interactionToUnsignedTx :: (MonadReader TxBuildingContext m, MonadIO m) => Interaction -> m GYTx
interactionToUnsignedTx = fmap (unsignedTx . fst) . interactionToTxBody

-- | Builds a hex encoded CBOR from an interaction
interactionToHexEncodedCBOR :: (MonadReader TxBuildingContext m, MonadIO m) => Interaction -> m String
interactionToHexEncodedCBOR = (txToHex <$>) . interactionToUnsignedTx

------------------------------------------------------------------------------------------------

-- * Transaction Running

------------------------------------------------------------------------------------------------

addressFromSkey :: ProviderCtx -> GYExtendedPaymentSigningKey -> GYAddress
addressFromSkey pCtx skey =
  let nid = getNetworkId pCtx
   in addressFromPaymentSigningKey nid skey

pkhFromSkey :: GYExtendedPaymentSigningKey -> GYPaymentKeyHash
pkhFromSkey = pkhFromExtendedSkey

runBJJActionWithPK :: TxBuildingContext -> GYExtendedPaymentSigningKey -> ActionType -> Maybe GYAddress -> IO (GYTxId, Maybe GYAssetClass)
runBJJActionWithPK txBuildingCtx@TxBuildingContext {..} skey action optionalRecipient = do
  let my_addr = addressFromSkey providerCtx skey
  let userAddrs = UserAddresses [my_addr] my_addr Nothing
  let interaction = Interaction action userAddrs optionalRecipient
  print interaction
  putStrLn (yellowColorString "Building transaction...")
  (txBody, mAssetClass) <- runReaderT (interactionToTxBody interaction) txBuildingCtx
  let txSigned = signGYTxBody txBody [skey]
  putStrLn $ yellowColorString ("Built and signed by: \n\t" <> Data.Text.unpack (addressToText my_addr))
  txId <- submitTxAndWaitForConfirmation True (ctxProviders providerCtx) txSigned
  return (txId, mAssetClass)

------------------------------------------------------------------------------------------------

-- * Reference Script Deployment

------------------------------------------------------------------------------------------------

-- | Deploy a single reference script and return its hash and the TxOutRef.
deployReferenceScript :: ProviderCtx -> GYScript 'PlutusV3 -> GYExtendedPaymentSigningKey -> IO (GYScriptHash, GYTxOutRef)
deployReferenceScript providerCtx script skey = do
  let my_addr = addressFromSkey providerCtx skey
  putStrLn $ yellowColorString "Deploying reference script"
  putStrLn $ yellowColorString $ "Deployed Script Hash: \n\t" <> Printf.printf "%s" (scriptHash script)
  putStrLn $ yellowColorString $ "Deployed to Address: \n\t" <> Printf.printf "%s" (addressToText my_addr)
  txBody <- runTx' providerCtx [my_addr] my_addr Nothing (addRefScriptToAddressSkeleton my_addr script)
  let txSigned = signGYTxBody txBody [skey]
  putStrLn $ yellowColorString ("Built and signed by: \n\t" <> Data.Text.unpack (addressToText my_addr))
  gyTxId <- submitTxAndWaitForConfirmation True (ctxProviders providerCtx) txSigned
  return (scriptHash script, txOutRefFromTuple (gyTxId, 0))

-- | Full deployment flow for all protocol scripts and the oracle.
-- Steps:
--   1. Deploy oracle validator reference script
--   2. Mint oracle NFT and lock initial OracleParams at oracle validator
--   3. Deploy profiles, ranks, memberships validator reference scripts
--   4. Compile minting policy with oracle NFT asset class and deploy its reference script
deployReferenceScripts :: ProviderCtx -> GYExtendedPaymentSigningKey -> IO DeployedScriptsContext
deployReferenceScripts providerCtx skey = do
  let my_pkh = pkhFromSkey skey

  -- Step 1: Deploy oracle validator reference script
  putStrLn $ yellowColorString "=== Step 1: Deploy Oracle Validator ==="
  ov <- deployReferenceScript providerCtx oracleValidatorGY skey

  -- Step 2: Mint oracle NFT and lock initial OracleParams
  putStrLn $ yellowColorString "=== Step 2: Mint Oracle NFT and Lock Initial OracleParams ==="
  oracleNFTAC <- mintOracleNFTAndLockDatum providerCtx skey my_pkh

  -- Step 3: Deploy spending validator reference scripts
  putStrLn $ yellowColorString "=== Step 3: Deploy Spending Validators ==="
  pv <- deployReferenceScript providerCtx profilesValidatorGY skey
  rv <- deployReferenceScript providerCtx ranksValidatorGY skey

  -- Step 4: Compile and deploy the minting policy (parameterized by oracle NFT)
  putStrLn $ yellowColorString "=== Step 4: Compile and Deploy Minting Policy ==="
  let mpGY = compileMintingPolicy (assetClassToPlutus oracleNFTAC)
  mp <- deployReferenceScript providerCtx mpGY skey

  return
    DeployedScriptsContext
      { mintingPolicyHashAndRef = mp,
        profilesValidatorHashAndRef = pv,
        ranksValidatorHashAndRef = rv,
        oracleValidatorHashAndRef = ov,
        oracleNFTAssetClass = oracleNFTAC
      }

-- | Mint the oracle NFT using a one-shot policy and lock the initial oracle datum.
mintOracleNFTAndLockDatum :: ProviderCtx -> GYExtendedPaymentSigningKey -> GYPaymentKeyHash -> IO GYAssetClass
mintOracleNFTAndLockDatum providerCtx skey adminPKH = do
  let nid = getNetworkId providerCtx
  let my_addr = addressFromSkey providerCtx skey
  let providers = ctxProviders providerCtx

  -- Build the transaction; also return the oracle NFT asset class from inside the builder.
  (txBody, oracleNFTAC) <- runGYTxBuilderMonadIO nid providers [my_addr] my_addr Nothing $ do
    -- Find a seed UTxO for the one-shot policy
    seedGYRef <- someUTxOWithoutRefScript
    let seedPlutus = txOutRefToV3Plutus seedGYRef

    -- Compile the one-shot oracle NFT policy
    let oracleNFTPolicyGY = compileOracleNFTPolicy seedPlutus
    let oracleNFTMPId = mintingPolicyId oracleNFTPolicyGY
    let oracleNFTTN = ""  -- empty token name for the NFT
    let theOracleNFTAC = GYToken oracleNFTMPId oracleNFTTN

    -- Build initial oracle params
    let initialOracleParams =
          OracleParams
            { opAdminPkh = paymentKeyHashToPlutus adminPKH,
              opPaused = False,
              opFeeConfig = Nothing,
              opMinOutputLovelace = 3500000
            }

    -- Spend seed UTxO
    let spendSeed = mustHaveInput (GYTxIn seedGYRef GYTxInWitnessKey)

    -- Mint oracle NFT
    let mp = GYMintScript @'PlutusV3 oracleNFTPolicyGY
    let gyRedeemer = redeemerFromPlutusData ()
    let mintNFT = mustMint mp gyRedeemer oracleNFTTN 1

    -- Lock oracle NFT + datum at oracle validator address
    lockOutput <- txMustLockStateWithInlineDatumAndValue oracleValidatorGY initialOracleParams (valueSingleton theOracleNFTAC 1 <> valueFromLovelace 3500000)

    body <- buildTxBody $ mconcat [spendSeed, mintNFT, lockOutput]
    return (body, theOracleNFTAC)

  let txSigned = signGYTxBody txBody [skey]
  _gyTxId <- submitTxAndWaitForConfirmation True (ctxProviders providerCtx) txSigned

  putStrLn $ yellowColorString $ "Oracle NFT AssetClass: " <> show oracleNFTAC
  return oracleNFTAC


