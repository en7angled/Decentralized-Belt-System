{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module TxBuilding.Validators where

import Data.ByteString.Short qualified as SBS
import GeniusYield.Types
import Onchain.MintingPolicy qualified as MintingPolicy (mintingPolicyCompile)
import Onchain.ProfilesValidator qualified as ProfilesValidator (profilesCompile)
import Onchain.Protocol
import Onchain.RanksValidator qualified as RanksValidator (ranksCompile)
import Onchain.MembershipsValidator qualified as MembershipsValidator (membershipsCompile)
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

-- *  Define Ranks Validator

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

-- *  Define Memberships Validator

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

-- *  Protocol Parameters

------------------------------------------------------------------------------------------------

defaultProtocolParams :: ProtocolParams
defaultProtocolParams = ProtocolParams (ranksValidatorHashPlutus, profilesValidatorHashPlutus, membershipsValidatorHashPlutus)

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

-- *  Script Sizes (flat-encoded bytes)

------------------------------------------------------------------------------------------------

-- | Size of a compiled script in bytes (flat-encoded, as deployed on-chain)
compiledCodeSize :: CompiledCode a -> Int
compiledCodeSize cc = SBS.length (serialiseCompiledCode cc)

mintingPolicySize :: Int
mintingPolicySize = compiledCodeSize mintingPolicyPlutus

profilesValidatorSize :: Int
profilesValidatorSize = compiledCodeSize profilesValidatorPlutus

ranksValidatorSize :: Int
ranksValidatorSize = compiledCodeSize ranksValidatorPlutus

membershipsValidatorSize :: Int
membershipsValidatorSize = compiledCodeSize membershipsValidatorPlutus

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