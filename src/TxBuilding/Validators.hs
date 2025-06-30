module TxBuilding.Validators where

import GeniusYield.Types
import Onchain.ProfilesValidator (ProfilesParams (..), profilesCompile)
import PlutusLedgerApi.V3
import PlutusTx
import PlutusTx.Prelude

------------------------------------------------------------------------------------------------

-- *  Define Profile Validator

------------------------------------------------------------------------------------------------

profilesValidatorPlutus :: CompiledCode (BuiltinData -> BuiltinUnit)
profilesValidatorPlutus = profilesCompile (ProfilesParams ranksValidatorHashPlutus 2_000_000)

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
ranksValidatorPlutus = profilesCompile (ProfilesParams undefined 2_000_000)

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