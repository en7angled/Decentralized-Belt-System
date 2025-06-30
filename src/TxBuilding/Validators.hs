module TxBuilding.Validators where

import GeniusYield.Types
import Onchain.MintingPolicy
import Onchain.ProfilesValidator ( profilesCompile)
import PlutusLedgerApi.V3
import PlutusTx
import PlutusTx.Prelude
import Onchain.Types

------------------------------------------------------------------------------------------------

-- *  Define Profile Validator

------------------------------------------------------------------------------------------------

miintingPolicyPlutus :: CompiledCode (BuiltinData -> BuiltinUnit)
miintingPolicyPlutus = mintingPolicyCompile (ProtocolParams ranksValidatorHashPlutus 2_000_000)

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

-- *  Protocol Parameters

------------------------------------------------------------------------------------------------


defaultProtocolParams :: ProtocolParams
defaultProtocolParams = ProtocolParams ranksValidatorHashPlutus  2_000_000