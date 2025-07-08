module Onchain.Blueprint where

import Data.Set qualified as Set
import Onchain.Protocol (ProtocolParams, OnchainProfile, OnchainRank)
import Onchain.MintingPolicy (mintingPolicyCompile, MintingRedeemer)
import Onchain.ProfilesValidator (profilesCompile, ProfilesRedeemer)
import Onchain.RanksValidator (ranksCompile)
import Onchain.CIP68 (CIP68Datum)
import PlutusLedgerApi.V3 (serialiseCompiledCode)
import PlutusTx.Blueprint

import Data.ByteString.Short (fromShort)

-- | Complete contract blueprint for the BJJ Belt System
contractBlueprint :: ProtocolParams -> ContractBlueprint
contractBlueprint mp =
    MkContractBlueprint
        { contractId = Just "BJJ Belt System"
        , contractPreamble = myPreamble
        , contractValidators = Set.fromList 
            [ mintingPolicyValidatorBlueprint mp
            , profilesValidatorBlueprint mp
            , ranksValidatorBlueprint mp
            ]
        , contractDefinitions = deriveDefinitions @[ProtocolParams, CIP68Datum OnchainProfile, OnchainRank, MintingRedeemer, ProfilesRedeemer]
        }

myPreamble :: Preamble
myPreamble =
    MkPreamble
        { preambleTitle = "BJJ Belt System"
        , preambleDescription = Just "A decentralized belt system for Brazilian Jiu-Jitsu practitioners and organizations"
        , preambleVersion = "1.0.0"
        , preamblePlutusVersion = PlutusV3
        , preambleLicense = Just "MIT"
        }

-- | Minting Policy Blueprint
mintingPolicyValidatorBlueprint :: ProtocolParams -> ValidatorBlueprint referencedTypes
mintingPolicyValidatorBlueprint mp =
    MkValidatorBlueprint
        { validatorTitle = "BJJ Belt System Minting Policy"
        , validatorDescription = Just "Minting policy for creating and managing BJJ profiles and ranks"
        , validatorParameters =
            [ MkParameterBlueprint
                { parameterTitle = Just "Protocol Parameters"
                , parameterDescription = Just "Compile-time protocol parameters including validator script hashes"
                , parameterPurpose = Set.singleton Mint
                , parameterSchema = definitionRef @ProtocolParams
                }
            ]
        , validatorRedeemer =
            MkArgumentBlueprint
                { argumentTitle = Just "Minting Redeemer"
                , argumentDescription = Just "Redeemer for minting operations: CreateProfile, Promote, or BurnProfileId"
                , argumentPurpose = Set.fromList [Mint]
                , argumentSchema = definitionRef @MintingRedeemer
                }
        , validatorDatum =
            Nothing -- Minting policies don't have datums
        , validatorCompiled =
            Just $
                compiledValidator PlutusV3 (fromShort $ serialiseCompiledCode $ mintingPolicyCompile mp)
        }

-- | Profiles Validator Blueprint
profilesValidatorBlueprint :: ProtocolParams -> ValidatorBlueprint referencedTypes
profilesValidatorBlueprint _mp =
    MkValidatorBlueprint
        { validatorTitle = "BJJ Belt System Profiles Validator"
        , validatorDescription = Just "Validator for managing BJJ profiles: updating images, deleting profiles, and accepting promotions"
        , validatorParameters = [] -- No parameters for this validator
        , validatorRedeemer =
            MkArgumentBlueprint
                { argumentTitle = Just "Profiles Redeemer"
                , argumentDescription = Just "Redeemer for profile operations: UpdateProfileImage, DeleteProfile, or AcceptPromotion"
                , argumentPurpose = Set.fromList [Spend]
                , argumentSchema = definitionRef @ProfilesRedeemer
                }
        , validatorDatum =
            Just
                MkArgumentBlueprint
                    { argumentTitle = Just "Profile Datum"
                    , argumentDescription = Just "CIP68 datum containing profile information"
                    , argumentPurpose = Set.singleton Spend
                    , argumentSchema = definitionRef @(CIP68Datum OnchainProfile)
                    }
        , validatorCompiled =
            Just $
                compiledValidator PlutusV3 (fromShort $ serialiseCompiledCode profilesCompile)
        }

-- | Ranks Validator Blueprint
ranksValidatorBlueprint :: ProtocolParams -> ValidatorBlueprint referencedTypes
ranksValidatorBlueprint _mp =
    MkValidatorBlueprint
        { validatorTitle = "BJJ Belt System Ranks Validator"
        , validatorDescription = Just "Validator for managing BJJ ranks and validating promotions"
        , validatorParameters = [] -- No parameters for this validator
        , validatorRedeemer =
            MkArgumentBlueprint
                { argumentTitle = Just "Ranks Redeemer"
                , argumentDescription = Just "Redeemer for rank operations (currently no specific redeemer)"
                , argumentPurpose = Set.fromList [Spend]
                , argumentSchema = definitionRef @()
                }
        , validatorDatum =
            Just
                MkArgumentBlueprint
                    { argumentTitle = Just "Rank Datum"
                    , argumentDescription = Just "Datum containing rank information (Rank or PendingRank)"
                    , argumentPurpose = Set.singleton Spend
                    , argumentSchema = definitionRef @OnchainRank
                    }
        , validatorCompiled =
            Just $
                compiledValidator PlutusV3 (fromShort $ serialiseCompiledCode ranksCompile)
        }

-- | Legacy function for backward compatibility
mintingPolicyBlueprint :: ProtocolParams -> ContractBlueprint
mintingPolicyBlueprint mp =
    MkContractBlueprint
        { contractId = Just "BJJ Belt System Minting Policy"
        , contractPreamble = myPreamble
        , contractValidators = Set.singleton (mintingPolicyValidatorBlueprint mp)
        , contractDefinitions = deriveDefinitions @[ProtocolParams, CIP68Datum OnchainProfile, MintingRedeemer]
        }