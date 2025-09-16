{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module TxBuilding.Validators where

import GeniusYield.Types
import Onchain.MintingPolicy qualified as MintingPolicy (mintingPolicyCompile)
import Onchain.ProfilesValidator qualified as ProfilesValidator (profilesCompile)
import Onchain.Protocol
import Onchain.RanksValidator qualified as RanksValidator (ranksCompile)
import PlutusLedgerApi.V3
import PlutusTx
import PlutusTx.Prelude hiding (($))
import qualified Constants

------------------------------------------------------------------------------------------------

-- *  Define Profile Validator

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

-- *  Define Profile Validator

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

-- *  Protocol Parameters

------------------------------------------------------------------------------------------------

defaultProtocolParams :: ProtocolParams
defaultProtocolParams = ProtocolParams (ranksValidatorHashPlutus, profilesValidatorHashPlutus)

------------------------------------------------------------------------------------------------

-- *  Define Minting Policy

------------------------------------------------------------------------------------------------

mintingPolicyPlutus :: CompiledCode (BuiltinData -> BuiltinUnit)
mintingPolicyPlutus = MintingPolicy.mintingPolicyCompile defaultProtocolParams

mintingPolicyGY :: GYScript 'PlutusV3
mintingPolicyGY = validatorFromPlutus mintingPolicyPlutus

mintingPolicyHashGY :: GYScriptHash
mintingPolicyHashGY = validatorHash mintingPolicyGY

------------------------------------------------------------------------------------------------

-- *  Export Validators

------------------------------------------------------------------------------------------------

exportProfilesValidator :: IO ()
exportProfilesValidator = writeScript @'PlutusV3 Constants.defaultProfilesValidatorFile $ validatorToScript profilesValidatorGY

exportRanksValidator :: IO ()
exportRanksValidator = writeScript @'PlutusV3 Constants.defaultRanksValidatorFile $ validatorToScript ranksValidatorGY

exportMintingPolicy :: IO ()
exportMintingPolicy = writeScript @'PlutusV3 Constants.defaultMintingPolicyFile $ mintingPolicyToScript mintingPolicyGY


exportValidators :: IO ()
exportValidators = do
  exportProfilesValidator
  exportRanksValidator
  exportMintingPolicy