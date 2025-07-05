module TxBuilding.Validators where

import GeniusYield.Types
import qualified Onchain.MintingPolicy as MintingPolicy (mintingPolicyCompile)
import qualified Onchain.ProfilesValidator as ProfilesValidator (profilesCompile)
import Onchain.Protocol
import qualified Onchain.RanksValidator as RanksValidator (ranksCompile)
import PlutusLedgerApi.V3
import PlutusTx
import PlutusTx.Prelude

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

defaultProtocolParams2 :: ProtocolParams2
defaultProtocolParams2 =
  ProtocolParams2
    { ranksValidatorScriptHash2 = ranksValidatorHashPlutus,
      profilesValidatorScriptHash2 = profilesValidatorHashPlutus
    }

defaultProtocolParams :: ProtocolParams
defaultProtocolParams =
  ProtocolParams
    { ranksValidatorScriptHash = ranksValidatorHashPlutus,
      profilesValidatorScriptHash = profilesValidatorHashPlutus,
      collateral = 2_000_000
    }

------------------------------------------------------------------------------------------------

-- *  Define Minting Policy

------------------------------------------------------------------------------------------------

mintingPolicyPlutus :: CompiledCode (BuiltinData -> BuiltinUnit)
mintingPolicyPlutus = MintingPolicy.mintingPolicyCompile defaultProtocolParams2

mintingPolicyGY :: GYScript 'PlutusV3
mintingPolicyGY = validatorFromPlutus mintingPolicyPlutus

mintingPolicyHashGY :: GYScriptHash
mintingPolicyHashGY = validatorHash mintingPolicyGY
