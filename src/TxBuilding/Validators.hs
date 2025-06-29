module TxBuilding.Validators where

import GeniusYield.Types
import PlutusLedgerApi.V3
import PlutusTx
import PlutusTx.Prelude
import Onchain.CIP68 (CIP68Datum, MetadataFields, mkCIP68Datum)
import Onchain.ProfilesValidator (ProfilesParams (..), ProfilesRedeemer (..), profilesCompile)
import Onchain.Types (Profile)

------------------------------------------------------------------------------------------------

-- *  Define Profile Validator

------------------------------------------------------------------------------------------------

profilesValidatorPlutus :: CompiledCode (BuiltinData -> BuiltinUnit)
profilesValidatorPlutus = profilesCompile (ProfilesParams ())

profilesValidatorGY :: GYScript 'PlutusV3
profilesValidatorGY = validatorFromPlutus profilesValidatorPlutus

profilesValidatorHashGY :: GYScriptHash
profilesValidatorHashGY = validatorHash profilesValidatorGY

profilesValidatorHashPlutus :: ScriptHash
profilesValidatorHashPlutus = validatorHashToPlutus profilesValidatorHashGY

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