{-# LANGUAGE DataKinds #-}
module TestRuns where
import GeniusYield.TxBuilder
import GHC.Stack
import GeniusYield.Types
import TxBuilding.Context
import TxBuilding.Validators
import GeniusYield.Test.Utils


------------------------------------------------------------------------------------------------

-- * Test Runs Helpers

------------------------------------------------------------------------------------------------

deployReferenceScriptRun :: (GYTxGameMonad m, HasCallStack) => GYScript PlutusV3 -> User -> User -> m GYTxOutRef
deployReferenceScriptRun validator fromWallet toWallet = asUser fromWallet $ addRefScript (userChangeAddress toWallet) (validatorToScript validator)


------------------------------------------------------------------------------------------------

-- * Test Runs 

------------------------------------------------------------------------------------------------

deployBJJValidators:: (GYTxGameMonad m, HasCallStack) => User -> m ProfileTxBuildingContext
deployBJJValidators w = do
  refProfilesValidator <- deployReferenceScriptRun profilesValidatorGY w w
  refRanksValidator <- deployReferenceScriptRun ranksValidatorGY w w
  refMintingPolicy <- deployReferenceScriptRun mintingPolicyGY w w
  return
    ProfileTxBuildingContext
      { profilesValidatorRef = refProfilesValidator,
        ranksValidatorRef = refRanksValidator,
        mintingPolicyRef = refMintingPolicy
      }