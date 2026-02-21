module Constants where

defaultAtlasCoreConfig :: FilePath
defaultAtlasCoreConfig = "config/config_atlas.json"

defaultTxBuildingContextFile :: FilePath
defaultTxBuildingContextFile = "config/config_bjj_validators.json"

defaultProfilesValidatorFile :: FilePath
defaultProfilesValidatorFile = "docs/validators/profiles_validator.plutus"

defaultRanksValidatorFile :: FilePath
defaultRanksValidatorFile = "docs/validators/ranks_validator.plutus"

defaultMembershipsValidatorFile :: FilePath
defaultMembershipsValidatorFile = "docs/validators/memberships_validator.plutus"

defaultMintingPolicyFile :: FilePath
defaultMintingPolicyFile = "docs/validators/minting_policy.plutus"

defaultAchievementsValidatorFile :: FilePath
defaultAchievementsValidatorFile = "docs/validators/achievements_validator.plutus"

defaultOracleValidatorFile :: FilePath
defaultOracleValidatorFile = "docs/validators/oracle_validator.plutus"

defaultBlueprintFile :: FilePath
defaultBlueprintFile = "docs/bjj-belt-system-blueprint.json"

-- | Application version used in service probe responses.
appVersion :: String
appVersion = "1.0.0"
