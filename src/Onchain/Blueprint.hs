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
        , preambleDescription = Just "A decentralized belt system for Brazilian Jiu-Jitsu practitioners and organizations that implements a comprehensive smart contract architecture for managing BJJ practitioner profiles and rank promotions on the Cardano blockchain. The system consists of three main validators that work together to manage the complete BJJ belt system: Minting Policy (controls token creation and destruction), Profiles Validator (manages profile lifecycle and updates), and Ranks Validator (enforces BJJ promotion rules and rank state transitions)."
        , preambleVersion = "1.0.0"
        , preamblePlutusVersion = PlutusV3
        , preambleLicense = Just "MIT"
        }

-- | Minting Policy Blueprint
mintingPolicyValidatorBlueprint :: ProtocolParams -> ValidatorBlueprint referencedTypes
mintingPolicyValidatorBlueprint mp =
    MkValidatorBlueprint
        { validatorTitle = "BJJ Belt System Minting Policy"
        , validatorDescription = Just "Governs the rules for issuing new profiles, ranks, and promotions. It is parameterized by the Profiles and Ranks validator script hashes, ensuring secure cross-validator communication. Handles profile creation with CIP-68 standard metadata, initial rank assignment for practitioners, promotion issuance, and profile deletion. Features single minting authority for consistency, CIP-68 standard integration for NFT metadata, deterministic token generation to prevent collisions, and cross-validator parameterization for secure communication."
        , validatorParameters =
            [ MkParameterBlueprint
                { parameterTitle = Just "Protocol Parameters"
                , parameterDescription = Just "Compile-time protocol parameters including validator script hashes for secure cross-validator communication. Contains the Profiles Validator and Ranks Validator script hashes to ensure secure communication between validators."
                , parameterPurpose = Set.singleton Mint
                , parameterSchema = definitionRef @ProtocolParams
                }
            ]
        , validatorRedeemer =
            MkArgumentBlueprint
                { argumentTitle = Just "Minting Redeemer"
                , argumentDescription = Just "Redeemer for minting operations: CreateProfile (creates new profile with initial rank), Promote (creates promotion tokens), or BurnProfileId (deletes profile and burns tokens). Validates creation dates against transaction validity range, ensures seed TxOutRef is spent to prevent replay attacks, enforces exact token minting to prevent other-token-name attacks, and requires spending of awarding authority's user token for promotions."
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
        , validatorDescription = Just "Governs the rules for profile lifecycle and promotion acceptance. Handles profile information updates, profile deletion with token burning, and allows practitioners to accept pending promotions. Features token-based authorization requiring user tokens for profile modifications, full CIP-68 metadata support for extensible metadata updates, and atomic promotion acceptance that updates both profile and rank datums atomically. Manages state where profile datums reference current rank information and promotion acceptance updates both profile and rank datums atomically."
        , validatorParameters = [] -- No parameters for this validator
        , validatorRedeemer =
            MkArgumentBlueprint
                { argumentTitle = Just "Profiles Redeemer"
                , argumentDescription = Just "Redeemer for profile operations: UpdateProfileImage (updates profile image metadata), DeleteProfile (deletes profile and burns both Ref and User tokens), or AcceptPromotion (accepts pending promotion and updates both profile and rank datums atomically). Requires user token authorization for all operations."
                , argumentPurpose = Set.fromList [Spend]
                , argumentSchema = definitionRef @ProfilesRedeemer
                }
        , validatorDatum =
            Just
                MkArgumentBlueprint
                    { argumentTitle = Just "Profile Datum"
                    , argumentDescription = Just "CIP68 datum containing profile information with extensible metadata structure. Contains profile ID, profile type (Practitioner or Organization), current rank reference, and protocol parameters. Supports CIP-68 standard for interoperability with wallets and marketplaces while maintaining extensible metadata structure."
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
        , validatorDescription = Just "Enforces BJJ promotion rules and validates rank progression. Handles promotion rule enforcement using BJJ-specific logic with comprehensive BJJ promotion rules and time requirements, time-based validation between ranks, and rank state transitions from pending to confirmed. Features reference input validation to validate promotion rules without consuming tokens, comprehensive BJJ rule engine implementing BJJ promotion rules with time requirements, and two-phase promotion supporting pending promotions that can be accepted or rejected. Supports promotion flow with promotion creation (pending promotion locked at validator) and promotion acceptance (transforms pending promotion into new rank upon successful validation)."
        , validatorParameters = [] -- No parameters for this validator
        , validatorRedeemer =
            MkArgumentBlueprint
                { argumentTitle = Just "Ranks Redeemer"
                , argumentDescription = Just "Redeemer for rank operations (currently no specific redeemer as rank operations are primarily validation-based). Used for future extensibility of rank operations."
                , argumentPurpose = Set.fromList [Spend]
                , argumentSchema = definitionRef @()
                }
        , validatorDatum =
            Just
                MkArgumentBlueprint
                    { argumentTitle = Just "Rank Datum"
                    , argumentDescription = Just "Datum containing rank information with two states: Rank (confirmed rank with all promotion details) or PendingRank (pending promotion awaiting acceptance). Contains rank ID, rank number, achievement details, award details, achievement date, previous rank reference, and protocol parameters. Supports two-phase promotion process with pending promotions that can be accepted or rejected."
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