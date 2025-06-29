module TxBuilding.Skeletons where

import Control.Monad.Reader.Class
import Control.Monad.Except (MonadError, throwError)
import GeniusYield.Examples.Limbo
import GeniusYield.TxBuilder
import GeniusYield.Types
import PlutusLedgerApi.V1.Value
import PlutusLedgerApi.V3
import Onchain.CIP68 (CIP68Datum, MetadataFields, mkCIP68Datum)
import Onchain.ProfilesValidator (ProfilesRedeemer (..))
import Onchain.Types (Profile)
import TxBuilding.Validators
import TxBuilding.Utils
import Data.Text (Text)
import TxBuilding.Exceptions (ProfileException(..))

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

-- TODO: Add NFT minting actions when minting policy is implemented
-- txNFTAction :: (GYTxUserQueryMonad m) => GYTxOutRef -> ProfilesMintingRedeemer -> [AssetClass] -> m (GYTxSkeleton 'PlutusV3)
-- txNFTAction mpRefScript redeemer burnACs = do
--   let gyRedeemer = redeemerFromPlutus' . toBuiltinData $ redeemer
--       profilesMP = GYMintReference @'PlutusV3 mpRefScript profilesMintingPolicyGY
--   case redeemer of
--     MintProfile _ tor -> do
--       let (profileRefTN, profileUserTN) = generateRefAndUserTN $ tokenNameFromTxOutRef tor
--       let profileRefAC = AssetClass (mintingPolicyCurrencySymbol profilesMintingPolicyGY, profileRefTN)
--       let profileUserAC = AssetClass (mintingPolicyCurrencySymbol profilesMintingPolicyGY, profileUserTN)
--       gyProfileRefTN <- tokenNameFromPlutus' (snd . unAssetClass $ profileRefAC)
--       gyProfileUserTN <- tokenNameFromPlutus' (snd . unAssetClass $ profileUserAC)
--       return $
--         mconcat
--           [ mustMint profilesMP gyRedeemer gyProfileRefTN 1,
--             mustMint profilesMP gyRedeemer gyProfileUserTN 1
--           ]
--     Burn -> do
--       gyTN <- mapM (tokenNameFromPlutus' . snd . unAssetClass) burnACs
--       return $ mconcat $ (\tn -> mustMint profilesMP gyRedeemer tn (negate 1)) <$> gyTN

addRefScriptSkeleton :: (GYTxQueryMonad m) => GYScript 'PlutusV3 -> m (GYTxSkeleton v)
addRefScriptSkeleton sc = do
  addr <- scriptAddress limboValidatorV2
  addRefScriptToAddressSkeleton addr sc

addRefScriptToAddressSkeleton :: (GYTxQueryMonad m) => GYAddress -> GYScript 'PlutusV3 -> m (GYTxSkeleton v)
addRefScriptToAddressSkeleton addr sc = do
  return $ mustHaveOutput (mkGYTxOut addr mempty (datumFromPlutusData ())) {gyTxOutRefS = Just $ GYPlutusScript sc} 