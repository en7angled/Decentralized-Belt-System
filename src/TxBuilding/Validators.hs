module TxBuilding.Validators where

import GeniusYield.Types
import Onchain.MintingPolicy
import Onchain.MintingPolicy (mintingPolicyCompile)
import Onchain.ProfilesValidator ( profilesCompile)
import PlutusLedgerApi.V3
import PlutusTx
import PlutusTx.Prelude

------------------------------------------------------------------------------------------------

-- *  Define Profile Validator

------------------------------------------------------------------------------------------------

miintingPolicyPlutus :: CompiledCode (BuiltinData -> BuiltinUnit)
miintingPolicyPlutus = mintingPolicyCompile (MintingPolicyParams ranksValidatorHashPlutus 2_000_000)

mintingPolicyGY :: GYScript 'PlutusV3
mintingPolicyGY = validatorFromPlutus miintingPolicyPlutus

mintingPolicyHashGY :: GYScriptHash
mintingPolicyHashGY = validatorHash mintingPolicyGY

------------------------------------------------------------------------------------------------

-- *  Define Profile Validator

------------------------------------------------------------------------------------------------

profilesValidatorPlutus :: CompiledCode (BuiltinData -> BuiltinUnit)
profilesValidatorPlutus = profilesCompile 

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
ranksValidatorPlutus = profilesCompile 

ranksValidatorGY :: GYScript 'PlutusV3
ranksValidatorGY = validatorFromPlutus ranksValidatorPlutus

ranksValidatorHashGY :: GYScriptHash
ranksValidatorHashGY = validatorHash ranksValidatorGY

ranksValidatorHashPlutus :: ScriptHash
ranksValidatorHashPlutus = validatorHashToPlutus ranksValidatorHashGY

------------------------------------------------------------------------------------------------

-- *  Define Minting Policy

------------------------------------------------------------------------------------------------

-- TODO: Add minting policy for profile tokens when needed
-- profilesMintingPolicyPlutus :: CompiledCode (BuiltinData -> BuiltinUnit)
-- profilesMintingPolicyPlutus = compileProfilesMP profilesParams

-- profilesMintingPolicyGY :: GYScript 'PlutusV3
-- profilesMintingPolicyGY = mintingPolicyFromPlutus profilesMintingPolicyPlutus

------------------------------------------------------------------------------------------------

-- * Export Functions

------------------------------------------------------------------------------------------------

exportProfilesScript :: IO ()
exportProfilesScript = writeScript @'PlutusV3 "profiles-validator.plutus" Prelude.$ validatorToScript profilesValidatorGY

-- exportProfilesMintingPolicy :: IO ()
-- exportProfilesMintingPolicy = writeScript @'PlutusV3 "profiles-minting-policy.plutus" $ mintingPolicyToScript profilesMintingPolicyGY