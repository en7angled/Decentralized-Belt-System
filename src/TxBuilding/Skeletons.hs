module TxBuilding.Skeletons where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Reader.Class
import Data.Text (Text)
import GeniusYield.Examples.Limbo
import GeniusYield.TxBuilder
import GeniusYield.Types
import Onchain.CIP68 (MetadataFields, deriveUserFromRefTN, generateRefAndUserTN)
import Onchain.ProfilesValidator (ProfilesRedeemer (..))
import Onchain.Types qualified as Onchain
import Onchain.Utils (tokenNameFromTxOutRef)
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V3
import TxBuilding.Exceptions (ProfileException (..))
import TxBuilding.Utils
import TxBuilding.Validators

------------------------------------------------------------------------------------------------

-- * CIP-68 Utils

------------------------------------------------------------------------------------------------

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

txMustSpendStateFromRefScriptWithRedeemer :: (GYTxUserQueryMonad m, MonadError GYTxMonadException m, ToData a) => GYTxOutRef -> GYAssetClass -> a -> GYScript 'PlutusV3 -> m (GYTxSkeleton 'PlutusV3)
txMustSpendStateFromRefScriptWithRedeemer refScript stateTokenId redeemer gyValidator =
  do
    let gyRedeemer = redeemerFromPlutusData redeemer
    validatorAddr <- scriptAddress gyValidator
    stateUTxO <- getUTxOWithStateToken stateTokenId validatorAddr
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

txMustHaveStateAsRefInput :: (GYTxUserQueryMonad m, MonadError GYTxMonadException m) => GYAssetClass -> GYScript 'PlutusV3 -> m (GYTxSkeleton 'PlutusV3)
txMustHaveStateAsRefInput stateTokenId gyValidator = do
  validatorAddr <- scriptAddress gyValidator
  stateUTxO <- getUTxOWithStateToken stateTokenId validatorAddr
  return $ mustHaveRefInput (utxoRef stateUTxO)

txMustSpendFromAddress :: (GYTxUserQueryMonad m, MonadError GYTxMonadException m) => GYAssetClass -> [GYAddress] -> m (GYTxSkeleton 'PlutusV3)
txMustSpendFromAddress tokenId addrs = do
  do
    tokenUtxo <- getUTxOWithStateTokenAtAddresses tokenId addrs
    return $
      mustHaveInput
        GYTxIn
          { gyTxInTxOutRef = utxoRef tokenUtxo,
            gyTxInWitness = GYTxInWitnessKey
          }

txMustLockStateWithInlineDatumAndValue :: (GYTxUserQueryMonad m, ToData a) => GYScript 'PlutusV3 -> a -> Value -> m (GYTxSkeleton 'PlutusV3)
txMustLockStateWithInlineDatumAndValue validator todata pValue = do
  validatorAddressGY <- scriptAddress validator
  gyValue <- valueFromPlutus' pValue
  let gyDatum = datumFromPlutusData todata
  return $
    mustHaveOutput -- pays profile ref token to validator address with valid datum
      GYTxOut
        { gyTxOutAddress = validatorAddressGY,
          gyTxOutDatum = Just (gyDatum, GYTxOutUseInlineDatum),
          gyTxOutValue = gyValue,
          gyTxOutRefS = Nothing
        }

createProfileSkeleton :: (GYTxUserQueryMonad m) => GYTxOutRef -> TxOutRef -> MetadataFields -> Onchain.ProfileType -> m (GYTxSkeleton 'PlutusV3, GYAssetClass)
createProfileSkeleton mpRefScript seedTxOutRef metadata profileType = do
  let redeemer = CreateProfile seedTxOutRef metadata profileType
      gyRedeemer = redeemerFromPlutus' . toBuiltinData $ redeemer
      profilesMP = GYMintReference @'PlutusV3 mpRefScript profilesValidatorGY
  let (profileRefTN, profileUserTN) = generateRefAndUserTN $ tokenNameFromTxOutRef seedTxOutRef
  let profileRefAC = AssetClass (mintingPolicyCurrencySymbol profilesValidatorGY, profileRefTN)
  let profileUserAC = AssetClass (mintingPolicyCurrencySymbol profilesValidatorGY, profileUserTN)
  gyProfileRefTN <- tokenNameFromPlutus' (snd . unAssetClass $ profileRefAC)
  gyProfileUserTN <- tokenNameFromPlutus' (snd . unAssetClass $ profileUserAC)
  gyProfileRefAC <- assetClassFromPlutus' profileRefAC
  return
    ( mconcat
        [ mustMint profilesMP gyRedeemer gyProfileRefTN 1,
          mustMint profilesMP gyRedeemer gyProfileUserTN 1
        ],
      gyProfileRefAC
    )

deleteProfileSkeleton :: (GYTxUserQueryMonad m) => GYTxOutRef -> GYAssetClass -> m (GYTxSkeleton 'PlutusV3)
deleteProfileSkeleton mpRefScript (GYToken _ gyProfileRefTN) = do
  let redeemer = DeleteProfile
      gyRedeemer = redeemerFromPlutus' . toBuiltinData $ redeemer
      profilesMP = GYMintReference @'PlutusV3 mpRefScript profilesValidatorGY
  gyProfileUserTN <- tokenNameFromPlutus' $ deriveUserFromRefTN (tokenNameToPlutus gyProfileRefTN)
  return $ mconcat $ (\tn -> mustMint profilesMP gyRedeemer tn (negate 1)) <$> [gyProfileUserTN, gyProfileUserTN]
deleteProfileSkeleton _ _ = throwError (GYApplicationException InvalidAssetClass)

addRefScriptSkeleton :: (GYTxQueryMonad m) => GYScript 'PlutusV3 -> m (GYTxSkeleton v)
addRefScriptSkeleton sc = do
  addr <- scriptAddress limboValidatorV2
  addRefScriptToAddressSkeleton addr sc

addRefScriptToAddressSkeleton :: (GYTxQueryMonad m) => GYAddress -> GYScript 'PlutusV3 -> m (GYTxSkeleton v)
addRefScriptToAddressSkeleton addr sc = do
  return $ mustHaveOutput (mkGYTxOut addr mempty (datumFromPlutusData ())) {gyTxOutRefS = Just $ GYPlutusScript sc}

payProfileUserNFTToAddressSkeleton :: (GYTxUserQueryMonad m) => GYAddress -> TxOutRef -> m (GYTxSkeleton 'PlutusV3)
payProfileUserNFTToAddressSkeleton recipient seedTxOutRef = do
  let (_profileRefTN, profileUserTN) = generateRefAndUserTN $ tokenNameFromTxOutRef seedTxOutRef
  let profileUserAC = AssetClass (mintingPolicyCurrencySymbol profilesValidatorGY, profileUserTN)
  let profileUserNFTp = assetClassValue profileUserAC 1
  profileUserNFT <- valueFromPlutus' profileUserNFTp
  txIsPayingValueToAddress recipient profileUserNFT