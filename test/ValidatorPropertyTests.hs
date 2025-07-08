{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ValidatorPropertyTests where

import Control.Monad
import Data.Maybe (fromJust)
import DomainTypes.Profile.Types
import GHC.Stack
import GeniusYield.Test.Clb
import GeniusYield.Test.Utils
import GeniusYield.TxBuilder
import GeniusYield.Types
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Onchain.BJJ
import Onchain.CIP68
import Onchain.Protocol
import Onchain.Protocol as Onchain
import Onchain.ProfilesValidator
import Onchain.RanksValidator
import Onchain.MintingPolicy
import PlutusLedgerApi.V1 qualified as V1
import PlutusLedgerApi.V3
import PlutusLedgerApi.V3.Contexts
import PlutusTx.AssocMap qualified
import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog.Gen qualified as Gen
import TxBuilding.Interactions

-- ValidityInterval is not exported by PlutusLedgerApi.V3, so define it here
-- This matches the usual Plutus representation

type ValidityInterval = (Maybe V1.POSIXTime, Maybe V1.POSIXTime)

-- =============================================================================
-- Validator-Specific Property Tests
-- =============================================================================

validatorPropertyTests :: TestTree
validatorPropertyTests = testGroup "Validator Property Tests"
  [ testGroup "Profiles Validator"
    [ testProperty "valid profile update passes validation" prop_validProfileUpdate
    , testProperty "invalid profile deletion fails validation" prop_invalidProfileDeletion
    , testProperty "valid profile deletion passes validation" prop_validProfileDeletion
    , testProperty "valid promotion acceptance passes validation" prop_validPromotionAcceptance
    ]
  , testGroup "Ranks Validator"
    [ testProperty "valid promotion validation passes" prop_validPromotionValidation
    , testProperty "invalid promotion validation fails" prop_invalidPromotionValidation
    , testProperty "insufficient time between belts fails" prop_insufficientTimeBetweenBelts
    , testProperty "invalid belt progression fails" prop_invalidBeltProgression
    ]
  , testGroup "Minting Policy"
    [ testProperty "valid profile creation passes" prop_validProfileCreation
    , testProperty "invalid profile creation fails" prop_invalidProfileCreation
    , testProperty "valid promotion creation passes" prop_validPromotionCreation
    , testProperty "valid profile burning passes" prop_validProfileBurning
    ]
  ]

-- =============================================================================
-- Profiles Validator Properties
-- =============================================================================

prop_validProfileUpdate :: Property
prop_validProfileUpdate = property $ do
  context <- forAll genValidProfileUpdateContext
  profilesLambda context === True

prop_invalidProfileDeletion :: Property
prop_invalidProfileDeletion = property $ do
  context <- forAll genInvalidProfileDeletionContext
  profilesLambda context === False

prop_validProfileDeletion :: Property
prop_validProfileDeletion = property $ do
  context <- forAll genValidProfileDeletionContext
  profilesLambda context === True

prop_validPromotionAcceptance :: Property
prop_validPromotionAcceptance = property $ do
  context <- forAll genValidPromotionAcceptanceContext
  profilesLambda context === True

-- =============================================================================
-- Ranks Validator Properties
-- =============================================================================

prop_validPromotionValidation :: Property
prop_validPromotionValidation = property $ do
  context <- forAll genValidPromotionValidationContext
  ranksLambda context === True

prop_invalidPromotionValidation :: Property
prop_invalidPromotionValidation = property $ do
  context <- forAll genInvalidPromotionValidationContext
  ranksLambda context === False

prop_insufficientTimeBetweenBelts :: Property
prop_insufficientTimeBetweenBelts = property $ do
  context <- forAll genInsufficientTimeContext
  ranksLambda context === False

prop_invalidBeltProgression :: Property
prop_invalidBeltProgression = property $ do
  context <- forAll genInvalidBeltProgressionContext
  ranksLambda context === False

-- =============================================================================
-- Minting Policy Properties
-- =============================================================================

prop_validProfileCreation :: Property
prop_validProfileCreation = property $ do
  protocolParams <- forAll genProtocolParams
  context <- forAll genValidProfileCreationContext
  mintingPolicyLambda protocolParams context === True

prop_invalidProfileCreation :: Property
prop_invalidProfileCreation = property $ do
  protocolParams <- forAll genProtocolParams
  context <- forAll genInvalidProfileCreationContext
  mintingPolicyLambda protocolParams context === False

prop_validPromotionCreation :: Property
prop_validPromotionCreation = property $ do
  protocolParams <- forAll genProtocolParams
  context <- forAll genValidPromotionCreationContext
  mintingPolicyLambda protocolParams context === True

prop_validProfileBurning :: Property
prop_validProfileBurning = property $ do
  protocolParams <- forAll genProtocolParams
  context <- forAll genValidProfileBurningContext
  mintingPolicyLambda protocolParams context === True

-- =============================================================================
-- Context Generators
-- =============================================================================

-- Profiles Validator Context Generators

genValidProfileUpdateContext :: Gen ScriptContext
genValidProfileUpdateContext = do
  profileId <- genProfileId
  newImageURI <- genByteString
  profileDatum <- genValidProfileCIP68Datum
  txInfo <- genValidTxInfo
  redeemer <- pure $ UpdateProfileImage profileId newImageURI
  pure $ ScriptContext txInfo (Redeemer $ toBuiltinData redeemer) (SpendingScript (TxOutRef (TxId "") 0) (Just $ Datum $ toBuiltinData profileDatum))

genInvalidProfileDeletionContext :: Gen ScriptContext
genInvalidProfileDeletionContext = do
  profileId <- genProfileId
  txInfo <- genInvalidTxInfo -- Missing required NFTs
  redeemer <- pure $ DeleteProfile profileId
  pure $ ScriptContext txInfo (Redeemer $ toBuiltinData redeemer) (SpendingScript (TxOutRef (TxId "") 0) Nothing)

genValidProfileDeletionContext :: Gen ScriptContext
genValidProfileDeletionContext = do
  profileId <- genProfileId
  txInfo <- genValidDeletionTxInfo profileId
  redeemer <- pure $ DeleteProfile profileId
  pure $ ScriptContext txInfo (Redeemer $ toBuiltinData redeemer) (SpendingScript (TxOutRef (TxId "") 0) Nothing)

genValidPromotionAcceptanceContext :: Gen ScriptContext
genValidPromotionAcceptanceContext = do
  promotionId <- genRankId
  profileDatum <- genValidProfileCIP68Datum
  pendingRank <- genValidPendingRank
  txInfo <- genValidPromotionAcceptanceTxInfo promotionId
  redeemer <- pure $ AcceptPromotion promotionId
  pure $ ScriptContext txInfo (Redeemer $ toBuiltinData redeemer) (SpendingScript (TxOutRef (TxId "") 0) (Just $ Datum $ toBuiltinData profileDatum))

-- Ranks Validator Context Generators

genValidPromotionValidationContext :: Gen ScriptContext
genValidPromotionValidationContext = do
  promotionRank <- genValidPromotionRank
  txInfo <- genValidPromotionValidationTxInfo promotionRank
  redeemer <- genRedeemer
  pure $ ScriptContext txInfo (Redeemer $ toBuiltinData redeemer) (SpendingScript (TxOutRef (TxId "") 0) (Just $ Datum $ toBuiltinData promotionRank))

genInvalidPromotionValidationContext :: Gen ScriptContext
genInvalidPromotionValidationContext = do
  promotionRank <- genInvalidPromotionRank
  txInfo <- genInvalidPromotionValidationTxInfo promotionRank
  redeemer <- genRedeemer
  pure $ ScriptContext txInfo (Redeemer $ toBuiltinData redeemer) (SpendingScript (TxOutRef (TxId "") 0) (Just $ Datum $ toBuiltinData promotionRank))

genInsufficientTimeContext :: Gen ScriptContext
genInsufficientTimeContext = do
  promotionRank <- genInsufficientTimePromotionRank
  txInfo <- genValidPromotionValidationTxInfo promotionRank
  redeemer <- genRedeemer
  pure $ ScriptContext txInfo (Redeemer $ toBuiltinData redeemer) (SpendingScript (TxOutRef (TxId "") 0) (Just $ Datum $ toBuiltinData promotionRank))

genInvalidBeltProgressionContext :: Gen ScriptContext
genInvalidBeltProgressionContext = do
  promotionRank <- genInvalidBeltProgressionRank
  txInfo <- genValidPromotionValidationTxInfo promotionRank
  redeemer <- genRedeemer
  pure $ ScriptContext txInfo (Redeemer $ toBuiltinData redeemer) (SpendingScript (TxOutRef (TxId "") 0) (Just $ Datum $ toBuiltinData promotionRank))

-- Minting Policy Context Generators

genValidProfileCreationContext :: Gen ScriptContext
genValidProfileCreationContext = do
  seedTxOutRef <- genTxOutRef
  metadata <- genMetadataFields
  profileType <- genProfileType
  creationDate <- genPOSIXTime
  rankNumber <- Gen.integral (Range.linear 1 5)
  txInfo <- genValidProfileCreationTxInfo seedTxOutRef
  redeemer <- pure $ CreateProfile seedTxOutRef metadata profileType creationDate rankNumber
  pure $ ScriptContext txInfo (Redeemer $ toBuiltinData redeemer) (MintingScript (V1.CurrencySymbol ""))

genInvalidProfileCreationContext :: Gen ScriptContext
genInvalidProfileCreationContext = do
  seedTxOutRef <- genTxOutRef
  metadata <- genMetadataFields
  profileType <- genProfileType
  creationDate <- genPOSIXTime
  rankNumber <- Gen.integral (Range.linear 1 5)
  txInfo <- genInvalidProfileCreationTxInfo seedTxOutRef
  redeemer <- pure $ CreateProfile seedTxOutRef metadata profileType creationDate rankNumber
  pure $ ScriptContext txInfo (Redeemer $ toBuiltinData redeemer) (MintingScript (V1.CurrencySymbol ""))

genValidPromotionCreationContext :: Gen ScriptContext
genValidPromotionCreationContext = do
  profileId <- genProfileId
  awardedByRef <- genProfileId
  achievementDate <- genPOSIXTime
  rankNumber <- Gen.integral (Range.linear 1 10)
  txInfo <- genValidPromotionCreationTxInfo awardedByRef
  redeemer <- pure $ Promote profileId awardedByRef achievementDate rankNumber
  pure $ ScriptContext txInfo (Redeemer $ toBuiltinData redeemer) (MintingScript (V1.CurrencySymbol ""))

genValidProfileBurningContext :: Gen ScriptContext
genValidProfileBurningContext = do
  txInfo <- genValidProfileBurningTxInfo
  redeemer <- pure BurnProfileId
  pure $ ScriptContext txInfo (Redeemer $ toBuiltinData redeemer) (MintingScript (V1.CurrencySymbol ""))

-- =============================================================================
-- Helper Generators
-- =============================================================================

genValidProfileCIP68Datum :: Gen (CIP68Datum OnchainProfile)
genValidProfileCIP68Datum = do
  profile <- genValidOnchainProfile
  metadata <- genMetadata
  version <- Gen.integral (Range.linear 1 10)
  pure $ CIP68Datum metadata version profile

genValidOnchainProfile :: Gen OnchainProfile
genValidOnchainProfile = do
  profileId <- genProfileId
  protocolParams <- genProtocolParams
  rankId <- genRankId
  pure $ OnchainProfile profileId Onchain.Practitioner (Just rankId) protocolParams

genValidPendingRank :: Gen OnchainRank
genValidPendingRank = do
  promotionId <- genRankId
  rankNumber <- Gen.integral (Range.linear 1 10)
  awardedTo <- genProfileId
  awardedBy <- genProfileId
  achievementDate <- genPOSIXTime
  protocolParams <- genProtocolParams
  pure $ PendingRank promotionId rankNumber awardedTo awardedBy achievementDate protocolParams

genValidPromotionRank :: Gen OnchainRank
genValidPromotionRank = do
  promotionId <- genRankId
  rankNumber <- Gen.integral (Range.linear 2 5) -- Higher rank
  awardedTo <- genProfileId
  awardedBy <- genProfileId
  achievementDate <- genPOSIXTime
  protocolParams <- genProtocolParams
  pure $ PendingRank promotionId rankNumber awardedTo awardedBy achievementDate protocolParams

genInvalidPromotionRank :: Gen OnchainRank
genInvalidPromotionRank = do
  promotionId <- genRankId
  rankNumber <- Gen.integral (Range.linear 1 2) -- Lower rank
  awardedTo <- genProfileId
  awardedBy <- genProfileId
  achievementDate <- genPOSIXTime
  protocolParams <- genProtocolParams
  pure $ PendingRank promotionId rankNumber awardedTo awardedBy achievementDate protocolParams

genInsufficientTimePromotionRank :: Gen OnchainRank
genInsufficientTimePromotionRank = do
  promotionId <- genRankId
  rankNumber <- Gen.integral (Range.linear 2 5)
  awardedTo <- genProfileId
  awardedBy <- genProfileId
  achievementDate <- genPOSIXTime -- Too recent
  protocolParams <- genProtocolParams
  pure $ PendingRank promotionId rankNumber awardedTo awardedBy achievementDate protocolParams

genInvalidBeltProgressionRank :: Gen OnchainRank
genInvalidBeltProgressionRank = do
  promotionId <- genRankId
  rankNumber <- Gen.integral (Range.linear 6 10) -- Invalid progression
  awardedTo <- genProfileId
  awardedBy <- genProfileId
  achievementDate <- genPOSIXTime
  protocolParams <- genProtocolParams
  pure $ PendingRank promotionId rankNumber awardedTo awardedBy achievementDate protocolParams

genValidTxInfo :: Gen TxInfo
genValidTxInfo = TxInfo <$> genTxInInfoList <*> genTxOutList <*> genFee <*> genMintValue <*> genValidityInterval <*> genScriptPurpose

genInvalidTxInfo :: Gen TxInfo
genInvalidTxInfo = TxInfo <$> genEmptyTxInInfoList <*> genTxOutList <*> genFee <*> genMintValue <*> genValidityInterval <*> genScriptPurpose

genValidDeletionTxInfo :: ProfileId -> Gen TxInfo
genValidDeletionTxInfo profileId = do
  inputs <- genTxInInfoList
  outputs <- genTxOutList
  fee <- genFee
  mintValue <- genValidDeletionMintValue profileId
  validityInterval <- genValidityInterval
  scriptPurpose <- genScriptPurpose
  pure $ TxInfo inputs outputs fee mintValue validityInterval scriptPurpose

genValidPromotionAcceptanceTxInfo :: RankId -> Gen TxInfo
genValidPromotionAcceptanceTxInfo promotionId = do
  inputs <- genTxInInfoList
  outputs <- genTxOutList
  fee <- genFee
  mintValue <- genMintValue
  validityInterval <- genValidityInterval
  scriptPurpose <- genScriptPurpose
  pure $ TxInfo inputs outputs fee mintValue validityInterval scriptPurpose

genValidPromotionValidationTxInfo :: OnchainRank -> Gen TxInfo
genValidPromotionValidationTxInfo promotionRank = do
  inputs <- genTxInInfoList
  outputs <- genTxOutList
  fee <- genFee
  mintValue <- genMintValue
  validityInterval <- genValidityInterval
  scriptPurpose <- genScriptPurpose
  pure $ TxInfo inputs outputs fee mintValue validityInterval scriptPurpose

genInvalidPromotionValidationTxInfo :: OnchainRank -> Gen TxInfo
genInvalidPromotionValidationTxInfo promotionRank = do
  inputs <- genEmptyTxInInfoList
  outputs <- genTxOutList
  fee <- genFee
  mintValue <- genMintValue
  validityInterval <- genValidityInterval
  scriptPurpose <- genScriptPurpose
  pure $ TxInfo inputs outputs fee mintValue validityInterval scriptPurpose

genValidProfileCreationTxInfo :: TxOutRef -> Gen TxInfo
genValidProfileCreationTxInfo seedTxOutRef = do
  inputs <- genTxInInfoListWithSeed seedTxOutRef
  outputs <- genTxOutList
  fee <- genFee
  mintValue <- genMintValue
  validityInterval <- genValidityInterval
  scriptPurpose <- genScriptPurpose
  pure $ TxInfo inputs outputs fee mintValue validityInterval scriptPurpose

genInvalidProfileCreationTxInfo :: TxOutRef -> Gen TxInfo
genInvalidProfileCreationTxInfo seedTxOutRef = do
  inputs <- genEmptyTxInInfoList
  outputs <- genTxOutList
  fee <- genFee
  mintValue <- genMintValue
  validityInterval <- genValidityInterval
  scriptPurpose <- genScriptPurpose
  pure $ TxInfo inputs outputs fee mintValue validityInterval scriptPurpose

genValidPromotionCreationTxInfo :: ProfileId -> Gen TxInfo
genValidPromotionCreationTxInfo awardedByRef = do
  inputs <- genTxInInfoList
  outputs <- genTxOutList
  fee <- genFee
  mintValue <- genMintValue
  validityInterval <- genValidityInterval
  scriptPurpose <- genScriptPurpose
  pure $ TxInfo inputs outputs fee mintValue validityInterval scriptPurpose

genValidProfileBurningTxInfo :: Gen TxInfo
genValidProfileBurningTxInfo = do
  inputs <- genTxInInfoList
  outputs <- genTxOutList
  fee <- genFee
  mintValue <- genEmptyMintValue
  validityInterval <- genValidityInterval
  scriptPurpose <- genScriptPurpose
  pure $ TxInfo inputs outputs fee mintValue validityInterval scriptPurpose

-- Basic generators
genTxInInfoList :: Gen [TxInInfo]
genTxInInfoList = Gen.list (Range.linear 1 5) genTxInInfo

genEmptyTxInInfoList :: Gen [TxInInfo]
genEmptyTxInInfoList = pure []

genTxInInfoListWithSeed :: TxOutRef -> Gen [TxInInfo]
genTxInInfoListWithSeed seedTxOutRef = do
  otherInputs <- Gen.list (Range.linear 0 3) genTxInInfo
  seedInput <- genTxInInfoWithRef seedTxOutRef
  pure $ seedInput : otherInputs

genTxInInfo :: Gen TxInInfo
genTxInInfo = TxInInfo <$> genTxOutRef <*> genTxOut

genTxInInfoWithRef :: TxOutRef -> Gen TxInInfo
genTxInInfoWithRef ref = TxInInfo ref <$> genTxOut

genTxOutRef :: Gen TxOutRef
genTxOutRef = TxOutRef <$> genTxId <*> Gen.integral (Range.linear 0 100)

genTxId :: Gen TxId
genTxId = TxId <$> genByteString

genTxOut :: Gen TxOut
genTxOut = TxOut <$> genAddress <*> genValue <*> genOutputDatum

genAddress :: Gen Address
genAddress = Address <$> genCredential <*> genStakingCredential

genCredential :: Gen Credential
genCredential = PubKeyCredential <$> genPubKeyHash


genPubKeyHash :: Gen PubKeyHash
genPubKeyHash = PubKeyHash <$> genByteString

genStakingCredential :: Gen StakingCredential
genStakingCredential = StakingHash <$> genCredential

genValue :: Gen Value
genValue = V1.Value <$> genMap

genMap :: Gen (PlutusTx.AssocMap.Map V1.CurrencySymbol (PlutusTx.AssocMap.Map V1.TokenName Integer))
genMap = PlutusTx.AssocMap.safeFromList <$> Gen.list (Range.linear 0 3) genValuePair

genValuePair :: Gen (V1.CurrencySymbol, PlutusTx.AssocMap.Map V1.TokenName Integer)
genValuePair = (,) <$> genCurrencySymbol <*> genTokenMap

genTokenMap :: Gen (PlutusTx.AssocMap.Map V1.TokenName Integer)
genTokenMap = PlutusTx.AssocMap.safeFromList <$> Gen.list (Range.linear 0 3) genTokenPair

genTokenPair :: Gen (V1.TokenName, Integer)
genTokenPair = (,) <$> genTokenName <*> Gen.integral (Range.linear 0 1000)

genOutputDatum :: Gen OutputDatum
genOutputDatum = Gen.maybe genDatumHash

genDatumHash :: Gen DatumHash
genDatumHash = DatumHash <$> genByteString

genTxOutList :: Gen [TxOut]
genTxOutList = Gen.list (Range.linear 1 5) genTxOut

genFee :: Gen Value
genFee = V1.lovelaceValue <$> Gen.integral (Range.linear 1000000 5000000)

genMintValue :: Gen MintValue
genMintValue = MintValue <$> genValue <*> genValue

genEmptyMintValue :: Gen MintValue
genEmptyMintValue = pure $ MintValue (V1.Value (PlutusTx.AssocMap.safeFromList [])) (V1.Value (PlutusTx.AssocMap.safeFromList []))

genValidDeletionMintValue :: ProfileId -> Gen MintValue
genValidDeletionMintValue profileId = do
  refNFT <- V1.assetClassValue profileId 1
  userNFT <- V1.assetClassValue (deriveUserFromRefAC profileId) 1
  pure $ MintValue (refNFT + userNFT) (V1.Value (PlutusTx.AssocMap.safeFromList []))

genValidityInterval :: Gen ValidityInterval
genValidityInterval = (,) <$> Gen.maybe genPOSIXTime <*> Gen.maybe genPOSIXTime

genScriptPurpose :: Gen ScriptPurpose
genScriptPurpose = SpendingScript <$> genTxOutRef <*> Gen.maybe genDatumHash

genRedeemer :: Gen BuiltinData
genRedeemer = toBuiltinData <$> Gen.integral (Range.linear 0 100)

-- Reuse generators from PropertyTests.hs
genByteString :: Gen BuiltinByteString
genByteString = Gen.bytes (Range.linear 0 100)

genTokenName :: Gen TokenName
genTokenName = TokenName <$> genByteString

genProfileId :: Gen ProfileId
genProfileId = V1.AssetClass <$> genCurrencySymbol <*> genTokenName

genRankId :: Gen RankId
genRankId = V1.AssetClass <$> genCurrencySymbol <*> genTokenName

genCurrencySymbol :: Gen V1.CurrencySymbol
genCurrencySymbol = V1.CurrencySymbol <$> genByteString

genPOSIXTime :: Gen POSIXTime
genPOSIXTime = POSIXTime <$> Gen.integral (Range.linear 0 1000000000)

genProtocolParams :: Gen ProtocolParams
genProtocolParams = ProtocolParams <$> genScriptHash <*> genScriptHash

genScriptHash :: Gen ScriptHash
genScriptHash = ScriptHash <$> genByteString

genMetadataFields :: Gen MetadataFields
genMetadataFields = Metadata222 <$> genByteString <*> genByteString <*> genByteString

genMetadata :: Gen Metadata
genMetadata = PlutusTx.AssocMap.safeFromList <$> Gen.list (Range.linear 0 5) genMetadataPair

genMetadataPair :: Gen (BuiltinByteString, BuiltinByteString)
genMetadataPair = (,) <$> genByteString <*> genByteString

genProfileType :: Gen OnChainProfileType
genProfileType = Gen.element [Onchain.Practitioner, Onchain.Organization] 