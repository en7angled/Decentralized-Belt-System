{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module TxBuilding.Validators where

import Constants qualified
import Data.ByteString.Short qualified as SBS
import GeniusYield.Types
import Onchain.MembershipsValidator qualified as MembershipsValidator (membershipsCompile)
import Onchain.MintingPolicy qualified as MintingPolicy (mintingPolicyCompile)
import Onchain.OracleNFTPolicy qualified as OracleNFTPolicy (oracleNFTPolicyCompile)
import Onchain.OracleValidator qualified as OracleValidator (oracleCompile)
import Onchain.ProfilesValidator qualified as ProfilesValidator (profilesCompile)
import Onchain.Protocol
import Onchain.RanksValidator qualified as RanksValidator (ranksCompile)
import PlutusLedgerApi.V1.Value qualified as V1
import PlutusLedgerApi.V3
import PlutusTx
import PlutusTx.Prelude hiding (($))

------------------------------------------------------------------------------------------------

-- * Define Profile Validator

------------------------------------------------------------------------------------------------

profilesValidatorPlutus :: CompiledCode (BuiltinData -> BuiltinUnit)
profilesValidatorPlutus = ProfilesValidator.profilesCompile

profilesValidatorGY :: GYScript 'PlutusV3
profilesValidatorGY = validatorFromPlutus profilesValidatorPlutus

profilesValidatorHashGY :: GYScriptHash
profilesValidatorHashGY = validatorHash profilesValidatorGY

profilesValidatorHashPlutus :: ScriptHash
profilesValidatorHashPlutus = validatorHashToPlutus profilesValidatorHashGY

------------------------------------------------------------------------------------------------

-- * Define Ranks Validator

------------------------------------------------------------------------------------------------

ranksValidatorPlutus :: CompiledCode (BuiltinData -> BuiltinUnit)
ranksValidatorPlutus = RanksValidator.ranksCompile

ranksValidatorGY :: GYScript 'PlutusV3
ranksValidatorGY = validatorFromPlutus ranksValidatorPlutus

ranksValidatorHashGY :: GYScriptHash
ranksValidatorHashGY = validatorHash ranksValidatorGY

ranksValidatorHashPlutus :: ScriptHash
ranksValidatorHashPlutus = validatorHashToPlutus ranksValidatorHashGY

------------------------------------------------------------------------------------------------

-- * Define Memberships Validator

------------------------------------------------------------------------------------------------

membershipsValidatorPlutus :: CompiledCode (BuiltinData -> BuiltinUnit)
membershipsValidatorPlutus = MembershipsValidator.membershipsCompile

membershipsValidatorGY :: GYScript 'PlutusV3
membershipsValidatorGY = validatorFromPlutus membershipsValidatorPlutus

membershipsValidatorHashGY :: GYScriptHash
membershipsValidatorHashGY = validatorHash membershipsValidatorGY

membershipsValidatorHashPlutus :: ScriptHash
membershipsValidatorHashPlutus = validatorHashToPlutus membershipsValidatorHashGY

------------------------------------------------------------------------------------------------

-- * Define Oracle Validator (unparameterized)

------------------------------------------------------------------------------------------------

oracleValidatorPlutus :: CompiledCode (BuiltinData -> BuiltinUnit)
oracleValidatorPlutus = OracleValidator.oracleCompile

oracleValidatorGY :: GYScript 'PlutusV3
oracleValidatorGY = validatorFromPlutus oracleValidatorPlutus

oracleValidatorHashGY :: GYScriptHash
oracleValidatorHashGY = validatorHash oracleValidatorGY

------------------------------------------------------------------------------------------------

-- * Define Oracle NFT Policy (parameterized by seed TxOutRef)

------------------------------------------------------------------------------------------------

-- | Compile the one-shot oracle NFT minting policy for a given seed UTxO.
compileOracleNFTPolicy :: TxOutRef -> GYScript 'PlutusV3
compileOracleNFTPolicy seedRef =
  validatorFromPlutus (OracleNFTPolicy.oracleNFTPolicyCompile seedRef)

------------------------------------------------------------------------------------------------

-- * Protocol Parameters

------------------------------------------------------------------------------------------------

-- | Build 'ProtocolParams' given the oracle NFT 'AssetClass'.
-- The oracle token is dynamic (depends on the oracle NFT minting tx)
-- so this cannot be a top-level constant.
mkProtocolParams :: V1.AssetClass -> ProtocolParams
mkProtocolParams = ProtocolParams
    ranksValidatorHashPlutus
    profilesValidatorHashPlutus
    membershipsValidatorHashPlutus

-- | Protocol params with a placeholder oracle token, for blueprint generation only.
-- The actual oracle token is determined at deployment time.
blueprintProtocolParams :: ProtocolParams
blueprintProtocolParams = mkProtocolParams (V1.AssetClass (V1.CurrencySymbol "", V1.TokenName ""))

------------------------------------------------------------------------------------------------

-- * Define Minting Policy (parameterized by oracle token)

------------------------------------------------------------------------------------------------

-- | Compile the minting policy with the given oracle NFT 'AssetClass'.
-- The minting policy depends on 'ProtocolParams' which includes the oracle token,
-- so it must be compiled after the oracle NFT has been minted.
compileMintingPolicy :: V1.AssetClass -> GYScript 'PlutusV3
compileMintingPolicy oracleAC =
  validatorFromPlutus (MintingPolicy.mintingPolicyCompile (mkProtocolParams oracleAC))

------------------------------------------------------------------------------------------------

-- * Script Sizes (flat-encoded bytes)

------------------------------------------------------------------------------------------------

-- | Size of a compiled script in bytes (flat-encoded, as deployed on-chain)
compiledCodeSize :: CompiledCode a -> Int
compiledCodeSize cc = SBS.length (serialiseCompiledCode cc)

profilesValidatorSize :: Int
profilesValidatorSize = compiledCodeSize profilesValidatorPlutus

ranksValidatorSize :: Int
ranksValidatorSize = compiledCodeSize ranksValidatorPlutus

membershipsValidatorSize :: Int
membershipsValidatorSize = compiledCodeSize membershipsValidatorPlutus

oracleValidatorSize :: Int
oracleValidatorSize = compiledCodeSize oracleValidatorPlutus

-- | Size of the minting policy compiled with the given oracle NFT (parameterized script).
mintingPolicySize :: V1.AssetClass -> Int
mintingPolicySize oracleAC = compiledCodeSize (MintingPolicy.mintingPolicyCompile (mkProtocolParams oracleAC))

------------------------------------------------------------------------------------------------

-- * Export Validators

------------------------------------------------------------------------------------------------

exportProfilesValidator :: IO ()
exportProfilesValidator = writeScript @'PlutusV3 Constants.defaultProfilesValidatorFile $ validatorToScript profilesValidatorGY

exportRanksValidator :: IO ()
exportRanksValidator = writeScript @'PlutusV3 Constants.defaultRanksValidatorFile $ validatorToScript ranksValidatorGY

exportMembershipsValidator :: IO ()
exportMembershipsValidator = writeScript @'PlutusV3 Constants.defaultMembershipsValidatorFile $ validatorToScript membershipsValidatorGY

exportOracleValidator :: IO ()
exportOracleValidator = writeScript @'PlutusV3 Constants.defaultOracleValidatorFile $ validatorToScript oracleValidatorGY

exportValidators :: IO ()
exportValidators = do
  exportProfilesValidator
  exportRanksValidator
  exportMembershipsValidator
  exportOracleValidator