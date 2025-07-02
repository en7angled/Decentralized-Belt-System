module TxBuilding.Validators where

import GeniusYield.Types
import qualified Onchain.MintingPolicy as MintingPolicy (mintingPolicyCompile)
import qualified Onchain.ProfilesValidator as ProfilesValidator ( profilesCompile)
import qualified Onchain.RanksValidator as RanksValidator (ranksCompile)
import PlutusLedgerApi.V3
import PlutusTx
import PlutusTx.Prelude
import Onchain.Protocol

------------------------------------------------------------------------------------------------

-- *  Define Profile Validator

------------------------------------------------------------------------------------------------

miintingPolicyPlutus :: CompiledCode (BuiltinData -> BuiltinUnit)
miintingPolicyPlutus = MintingPolicy.mintingPolicyCompile (ProtocolParams ranksValidatorHashPlutus profilesValidatorHashPlutus 2_000_000)

mintingPolicyGY :: GYScript 'PlutusV3
mintingPolicyGY = validatorFromPlutus miintingPolicyPlutus

mintingPolicyHashGY :: GYScriptHash
mintingPolicyHashGY = validatorHash mintingPolicyGY

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
defaultProtocolParams = ProtocolParams {
    ranksValidatorScriptHash = ranksValidatorHashPlutus,
    profilesValidatorScriptHash = profilesValidatorHashPlutus,
    collateral = 2_000_000
}