module TxBuilding.Skeletons where

import GeniusYield.Examples.Limbo
import GeniusYield.TxBuilder
import GeniusYield.Types
import Onchain.CIP68 (deriveUserFromRefTN, generateRefAndUserTN)
import Onchain.Utils (nameFromTxOutRef)
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3 qualified as V3
import TxBuilding.Exceptions (ProfileException (..))
import TxBuilding.Lookups (getUTxOWithNFT, getUtxoWithTokenAtAddresses)
import TxBuilding.Utils
import TxBuilding.Validators

------------------------------------------------------------------------------------------------

-- * CIP-68 Utils

------------------------------------------------------------------------------------------------

gyGenerateRefAndUserAC :: (GYTxUserQueryMonad m) => GYTxOutRef -> m (GYAssetClass, GYAssetClass)
gyGenerateRefAndUserAC seedTxOutRef = do
  let (V1.TxOutRef (V1.TxId bs) i) = txOutRefToPlutus seedTxOutRef
  let seedTxOutRefPlutus = V3.TxOutRef (V3.TxId bs) i
  let (pRefTN, pUserTN) = generateRefAndUserTN $ nameFromTxOutRef seedTxOutRefPlutus
  let refAC = AssetClass (mintingPolicyCurrencySymbol mintingPolicyGY, pRefTN)
  let userAC = AssetClass (mintingPolicyCurrencySymbol mintingPolicyGY, pUserTN)
  gyRefAC <- assetClassFromPlutus' refAC
  gyUserAC <- assetClassFromPlutus' userAC
  return (gyRefAC, gyUserAC)

gyDeriveUserFromRefAC :: (GYTxUserQueryMonad m) => GYAssetClass -> m GYAssetClass
gyDeriveUserFromRefAC (GYToken mp gyProfileRefTN) = do
  gyProfileUserTN <- gyDeriveUserFromRefTN gyProfileRefTN
  return $ GYToken mp gyProfileUserTN
gyDeriveUserFromRefAC _ = throwError (GYApplicationException InvalidAssetClass)

gyDeriveUserFromRefTN :: (GYTxUserQueryMonad m) => GYTokenName -> m GYTokenName
gyDeriveUserFromRefTN gyProfileRefTN = tokenNameFromPlutus' $ deriveUserFromRefTN (tokenNameToPlutus gyProfileRefTN)

------------------------------------------------------------------------------------------------

-- * Construct Transaction Components

------------------------------------------------------------------------------------------------

txIsPayingValueToAddress :: (GYTxUserQueryMonad m) => GYAddress -> GYValue -> m (GYTxSkeleton 'PlutusV3)
txIsPayingValueToAddress recipient gyValue = do
  return $
    mustHaveOutput
      GYTxOut
        { gyTxOutAddress = recipient,
          gyTxOutDatum = Nothing,
          gyTxOutValue = gyValue,
          gyTxOutRefS = Nothing
        }

txIsPayingValueToAddressWithInlineDatum :: (GYTxUserQueryMonad m) => GYDatum -> GYAddress -> GYValue -> m (GYTxSkeleton 'PlutusV3)
txIsPayingValueToAddressWithInlineDatum gyDatum recipient gyValue = do
  return $
    mustHaveOutput
      GYTxOut
        { gyTxOutAddress = recipient,
          gyTxOutDatum = Just (gyDatum, GYTxOutUseInlineDatum),
          gyTxOutValue = gyValue,
          gyTxOutRefS = Nothing
        }

isValidBetween :: GYSlot -> GYSlot -> GYTxSkeleton 'PlutusV3
isValidBetween s1 s2 =
  mconcat
    [ isInvalidBefore s1,
      isInvalidAfter s2
    ]

safeEraTime :: Natural
safeEraTime = 25920 -- ~7h in seconds (era safe zone)

txIsValidByDDL :: (GYTxQueryMonad m) => POSIXTime -> m (GYTxSkeleton 'PlutusV3)
txIsValidByDDL ddl = do
  now <- slotOfCurrentBlock
  afterSafeEraSlot <- advanceSlot' now safeEraTime
  afterSafeEraTime <- pPOSIXTimeFromGYSlot afterSafeEraSlot
  validUntil <- gySlotFromPOSIXTime (min ddl afterSafeEraTime)
  return $ isValidBetween now validUntil

txIsValidForSafeEra :: (GYTxQueryMonad m) => m (GYTxSkeleton 'PlutusV3)
txIsValidForSafeEra = do
  now <- slotOfCurrentBlock
  afterSafeEraSlot <- advanceSlot' now safeEraTime
  afterSafeEraTime <- pPOSIXTimeFromGYSlot afterSafeEraSlot
  validUntil <- gySlotFromPOSIXTime afterSafeEraTime
  return $ isValidBetween now validUntil

txMustSpendStateFromRefScriptWithRedeemer :: (GYTxUserQueryMonad m) => GYTxOutRef -> GYAssetClass -> GYRedeemer -> GYScript 'PlutusV3 -> m (GYTxSkeleton 'PlutusV3)
txMustSpendStateFromRefScriptWithRedeemer refScript stateTokenId gyRedeemer gyValidator =
  do
    stateUTxO <- getUTxOWithNFT stateTokenId 
    (gyDatum, _v) <- gyGetInlineDatumAndValue' stateUTxO
    return $
      mustHaveInput
        GYTxIn
          { gyTxInTxOutRef = utxoRef stateUTxO,
            gyTxInWitness = GYTxInWitnessScript (GYInReference refScript $ validatorToScript gyValidator) (Just gyDatum) gyRedeemer
          }
  where
    gyGetInlineDatumAndValue' :: (MonadError GYTxMonadException m) => GYUTxO -> m (GYDatum, GYValue)
    gyGetInlineDatumAndValue' utxo = maybe (throwError (GYApplicationException ProfileNotFound)) return $ getInlineDatumAndValue utxo

txMustHaveUTxOAsRefInput :: (GYTxUserQueryMonad m) => GYAssetClass -> m (GYTxSkeleton 'PlutusV3)
txMustHaveUTxOAsRefInput gyAC  = do
  utxo <- getUTxOWithNFT gyAC 
  return $ mustHaveRefInput (utxoRef utxo)

txMustHaveUTxOsAsRefInputs :: (GYTxUserQueryMonad m) => [GYAssetClass] -> m (GYTxSkeleton 'PlutusV3)
txMustHaveUTxOsAsRefInputs gyACs  =  mconcat <$> mapM txMustHaveUTxOAsRefInput gyACs

txMustSpendFromAddress :: (GYTxUserQueryMonad m) => GYAssetClass -> [GYAddress] -> m (GYTxSkeleton 'PlutusV3)
txMustSpendFromAddress tokenId addrs = do
  do
    tokenUtxo <- getUtxoWithTokenAtAddresses tokenId addrs
    return $
      mustHaveInput
        GYTxIn
          { gyTxInTxOutRef = utxoRef tokenUtxo,
            gyTxInWitness = GYTxInWitnessKey
          }

txMustLockStateWithInlineDatumAndValue :: (GYTxUserQueryMonad m, ToData a) => GYScript 'PlutusV3 -> a -> GYValue -> m (GYTxSkeleton 'PlutusV3)
txMustLockStateWithInlineDatumAndValue validator todata value = do
  validatorAddressGY <- scriptAddress validator

  let gyDatum = datumFromPlutusData todata
  return $
    mustHaveOutput
      GYTxOut
        { gyTxOutAddress = validatorAddressGY,
          gyTxOutDatum = Just (gyDatum, GYTxOutUseInlineDatum),
          gyTxOutValue = value,
          gyTxOutRefS = Nothing
        }

txMustMintWithMintRef :: (GYTxUserQueryMonad m) => Bool -> GYTxOutRef -> GYScript 'PlutusV3 -> GYRedeemer -> GYAssetClass -> m (GYTxSkeleton 'PlutusV3)
txMustMintWithMintRef mintOrBurn mpRefScript mpScript gyRedeemer gyAC = do
  let mp = GYMintReference @'PlutusV3 mpRefScript mpScript
  gyTN <- tnFromGYAssetClass gyAC
  return $
    mconcat
      [ mustMint mp gyRedeemer gyTN (if mintOrBurn then 1 else negate 1)
      ]

txCIP68UserAndRef :: (GYTxUserQueryMonad m) => Bool -> GYTxOutRef -> GYScript 'PlutusV3 -> GYRedeemer -> GYAssetClass -> m (GYTxSkeleton 'PlutusV3)
txCIP68UserAndRef mintOrBurn mpRefScript mpScript gyRedeemer gyProfileRefAC = do
  let mp = GYMintReference @'PlutusV3 mpRefScript mpScript
  gyProfileRefTN <- tnFromGYAssetClass gyProfileRefAC
  gyProfileUserTN <- gyDeriveUserFromRefTN gyProfileRefTN
  return $
    mconcat
      [ mustMint mp gyRedeemer gyProfileRefTN (if mintOrBurn then 1 else negate 1),
        mustMint mp gyRedeemer gyProfileUserTN (if mintOrBurn then 1 else negate 1)
      ]

txMustMintCIP68UserAndRef :: (GYTxUserQueryMonad m) => GYTxOutRef -> GYScript 'PlutusV3 -> GYRedeemer -> GYAssetClass -> m (GYTxSkeleton 'PlutusV3)
txMustMintCIP68UserAndRef = txCIP68UserAndRef True

txMustBurnCIP68UserAndRef :: (GYTxUserQueryMonad m) => GYTxOutRef -> GYScript 'PlutusV3 -> GYRedeemer -> GYAssetClass -> m (GYTxSkeleton 'PlutusV3)
txMustBurnCIP68UserAndRef = txCIP68UserAndRef False

addRefScriptSkeleton :: (GYTxQueryMonad m) => GYScript 'PlutusV3 -> m (GYTxSkeleton v)
addRefScriptSkeleton sc = do
  addr <- scriptAddress limboValidatorV2
  addRefScriptToAddressSkeleton addr sc

addRefScriptToAddressSkeleton :: (GYTxQueryMonad m) => GYAddress -> GYScript 'PlutusV3 -> m (GYTxSkeleton v)
addRefScriptToAddressSkeleton addr sc = do
  return $ mustHaveOutput (mkGYTxOut addr mempty (datumFromPlutusData ())) {gyTxOutRefS = Just $ GYPlutusScript sc}
