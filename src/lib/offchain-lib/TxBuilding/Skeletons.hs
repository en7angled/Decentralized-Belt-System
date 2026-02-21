-- | Reusable transaction skeleton components (CIP-68, validity, ref inputs, state locks, mints).
module TxBuilding.Skeletons where

import GeniusYield.Examples.Limbo
import GeniusYield.TxBuilder
import GeniusYield.Types
import Onchain.CIP68 (deriveUserFromRefTN, generateRefAndUserTN)
import Onchain.Utils (nameFromTxOutRef)
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V3
import TxBuilding.Exceptions (TxBuildingException (..))
import TxBuilding.Lookups (getUTxOWithNFT, getUTxOWithTokenAtAddresses)
import TxBuilding.Utils (getInlineDatumAndValue, getInlineDatumAndValueOrThrow, tnFromGYAssetClass, txOutRefToV3Plutus)

------------------------------------------------------------------------------------------------

-- * Query Skeletons

------------------------------------------------------------------------------------------------

getRefScriptAtTxOutRef :: (GYTxQueryMonad m) => GYTxOutRef -> m GYAnyScript
getRefScriptAtTxOutRef refScript = do
  utxo <- utxoAtTxOutRef refScript
  let ms = utxoRefScript =<< utxo
  case ms of
    Just s -> return s
    Nothing -> throwError (GYApplicationException ScriptNotFound)

------------------------------------------------------------------------------------------------

-- * CIP-68 Utils

------------------------------------------------------------------------------------------------

gyGenerateRefAndUserAC :: (GYTxUserQueryMonad m) => GYScript 'PlutusV3 -> GYTxOutRef -> m (GYAssetClass, GYAssetClass)
gyGenerateRefAndUserAC mpScript seedTxOutRef = do
  let seedTxOutRefPlutus = txOutRefToV3Plutus seedTxOutRef
  let (pRefTN, pUserTN) = generateRefAndUserTN $ nameFromTxOutRef seedTxOutRefPlutus
  let refAC = AssetClass (mintingPolicyCurrencySymbol mpScript, pRefTN)
  let userAC = AssetClass (mintingPolicyCurrencySymbol mpScript, pUserTN)
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

-- * Transaction validity

------------------------------------------------------------------------------------------------

-- | Minimum validity window in seconds (~7h) when building validity range for UpdateEndDate.
safeEraTime :: Integer
safeEraTime = 25920

-- | Transaction valid only in the slot range [s1, s2] (inclusive).
-- Implemented as invalid before s1 and invalid after s2.
isValidBetween :: GYSlot -> GYSlot -> GYTxSkeleton 'PlutusV3
isValidBetween s1 s2 = mconcat [isInvalidBefore s1, isInvalidAfter s2]

txIsValidForSafeEra :: GYSlot -> GYTxSkeleton PlutusV3
txIsValidForSafeEra now = isValidBetween now (unsafeSlotFromInteger $ slotToInteger now + safeEraTime)

------------------------------------------------------------------------------------------------

-- * Construct Transaction Components

------------------------------------------------------------------------------------------------

txMustPayValueToAddress :: (GYTxQueryMonad m) => GYAddress -> GYValue -> m (GYTxSkeleton 'PlutusV3)
txMustPayValueToAddress recipient gyValue = do
  return $
    mustHaveOutput
      GYTxOut
        { gyTxOutAddress = recipient,
          gyTxOutDatum = Nothing,
          gyTxOutValue = gyValue,
          gyTxOutRefS = Nothing
        }

txMustSpendStateFromRefScriptWithRedeemer :: (GYTxUserQueryMonad m) => GYTxOutRef -> GYAssetClass -> GYRedeemer -> GYScript 'PlutusV3 -> m (GYTxSkeleton 'PlutusV3)
txMustSpendStateFromRefScriptWithRedeemer refScript stateTokenId gyRedeemer gyValidator =
  do
    stateUTxO <- getUTxOWithNFT stateTokenId
    (gyDatum, _v) <- getInlineDatumAndValueOrThrow stateUTxO
    return $
      mustHaveInput
        GYTxIn
          { gyTxInTxOutRef = utxoRef stateUTxO,
            gyTxInWitness = GYTxInWitnessScript (GYInReference refScript $ validatorToScript gyValidator) (Just gyDatum) gyRedeemer
          }

-- | Build a spend skeleton for a UTxO whose ref and datum are already known (e.g. from a prior query).
-- Unlike 'txMustSpendStateFromRefScriptWithRedeemer', this does not look up the UTxO by NFT.
txMustSpendFromRefScriptWithKnownDatum :: GYTxOutRef -> GYTxOutRef -> GYDatum -> GYRedeemer -> GYScript 'PlutusV3 -> GYTxSkeleton 'PlutusV3
txMustSpendFromRefScriptWithKnownDatum refScript utxoRef' gyDatum gyRedeemer gyValidator =
  mustHaveInput
    GYTxIn
      { gyTxInTxOutRef = utxoRef',
        gyTxInWitness = GYTxInWitnessScript (GYInReference refScript $ validatorToScript gyValidator) (Just gyDatum) gyRedeemer
      }

-- | Build a spend skeleton for a dust/griefing UTxO that may lack a valid datum.
-- Extracts the inline datum if present; passes 'Nothing' if the UTxO has no datum.
-- Used by the permissionless 'Cleanup' redeemer to sweep non-protocol UTxOs.
txMustSpendUTxOFromRefScript :: GYTxOutRef -> GYUTxO -> GYRedeemer -> GYScript 'PlutusV3 -> GYTxSkeleton 'PlutusV3
txMustSpendUTxOFromRefScript refScript utxo gyRedeemer gyValidator =
  let maybeDatum = fst <$> getInlineDatumAndValue utxo
   in mustHaveInput
        GYTxIn
          { gyTxInTxOutRef = utxoRef utxo,
            gyTxInWitness = GYTxInWitnessScript (GYInReference refScript $ validatorToScript gyValidator) maybeDatum gyRedeemer
          }

txMustHaveUTxOAsRefInput :: (GYTxUserQueryMonad m) => GYAssetClass -> m (GYTxSkeleton 'PlutusV3)
txMustHaveUTxOAsRefInput gyAC = do
  utxo <- getUTxOWithNFT gyAC
  return $ mustHaveRefInput (utxoRef utxo)

txMustHaveUTxOsAsRefInputs :: (GYTxUserQueryMonad m) => [GYAssetClass] -> m (GYTxSkeleton 'PlutusV3)
txMustHaveUTxOsAsRefInputs gyACs = mconcat <$> mapM txMustHaveUTxOAsRefInput gyACs

txMustSpendFromAddress :: (GYTxUserQueryMonad m) => GYAssetClass -> [GYAddress] -> m (GYTxSkeleton 'PlutusV3)
txMustSpendFromAddress tokenId addrs = do
  tokenUtxo <- getUTxOWithTokenAtAddresses tokenId addrs
  return $
    mustHaveInput
      GYTxIn
        { gyTxInTxOutRef = utxoRef tokenUtxo,
          gyTxInWitness = GYTxInWitnessKey
        }

txMustLockStateWithInlineDatumAndValue :: (GYTxQueryMonad m, ToData a) => GYScript 'PlutusV3 -> a -> GYValue -> m (GYTxSkeleton 'PlutusV3)
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

-- | Reserved for future profile deletion support.
txMustBurnCIP68UserAndRef :: (GYTxUserQueryMonad m) => GYTxOutRef -> GYScript 'PlutusV3 -> GYRedeemer -> GYAssetClass -> m (GYTxSkeleton 'PlutusV3)
txMustBurnCIP68UserAndRef = txCIP68UserAndRef False

addRefScriptSkeleton :: (GYTxQueryMonad m) => GYScript 'PlutusV3 -> m (GYTxSkeleton v)
addRefScriptSkeleton sc = do
  addr <- scriptAddress limboValidatorV2
  addRefScriptToAddressSkeleton addr sc

addRefScriptToAddressSkeleton :: (GYTxQueryMonad m) => GYAddress -> GYScript 'PlutusV3 -> m (GYTxSkeleton v)
addRefScriptToAddressSkeleton addr sc = do
  return $ mustHaveOutput (mkGYTxOut addr mempty (datumFromPlutusData ())) {gyTxOutRefS = Just $ GYPlutusScript sc}
