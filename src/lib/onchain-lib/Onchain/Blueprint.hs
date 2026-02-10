module Onchain.Blueprint where

import Data.ByteString.Short (fromShort)
import Data.Set qualified as Set
import Onchain.CIP68 (CIP68Datum)
import Onchain.MembershipsValidator (MembershipsRedeemer, membershipsCompile)
import Onchain.MintingPolicy (MintingRedeemer, mintingPolicyCompile)
import Onchain.ProfilesValidator (ProfilesRedeemer, profilesCompile)
import Onchain.Protocol (OnchainProfile, OnchainRank, ProtocolParams)
import Onchain.RanksValidator (RanksRedeemer, ranksCompile)
import PlutusLedgerApi.V3 (serialiseCompiledCode)
import PlutusTx.Blueprint

-- | Complete contract blueprint for the BJJ Belt System
contractBlueprint :: ProtocolParams -> ContractBlueprint
contractBlueprint mp =
  MkContractBlueprint
    { contractId = Just "BJJ Belt System",
      contractPreamble = myPreamble,
      contractValidators =
        Set.fromList
          [ mintingPolicyValidatorBlueprint mp,
            profilesValidatorBlueprint mp,
            ranksValidatorBlueprint mp,
            membershipsValidatorBlueprint mp
          ],
      -- Note: MembershipDatum is excluded from deriveDefinitions because it transitively contains
      -- NodeDatum (polymorphic type from LinkedList.hs) which cannot derive HasBlueprintSchema
      -- for concrete type parameter instantiations. The MembershipsValidator datum uses @() as a placeholder.
      contractDefinitions = deriveDefinitions @[ProtocolParams, CIP68Datum OnchainProfile, OnchainRank, MintingRedeemer, ProfilesRedeemer, RanksRedeemer, MembershipsRedeemer]
    }

myPreamble :: Preamble
myPreamble =
  MkPreamble
    { preambleTitle = "BJJ Belt System",
      preambleDescription = Just "A decentralized belt system for Brazilian Jiu-Jitsu practitioners and organizations that implements a comprehensive smart contract architecture for managing BJJ practitioner profiles, rank promotions, and membership histories on the Cardano blockchain. The system consists of four main validators: Minting Policy (controls token creation), Profiles Validator (manages profile lifecycle and updates), Ranks Validator (enforces promotion acceptance and rank state transitions), and Memberships Validator (manages membership histories and intervals in a sorted linked list).",
      preambleVersion = "1.0.0",
      preamblePlutusVersion = PlutusV3,
      preambleLicense = Just "MIT"
    }

-- | Minting Policy Blueprint
mintingPolicyValidatorBlueprint :: ProtocolParams -> ValidatorBlueprint referencedTypes
mintingPolicyValidatorBlueprint mp =
  MkValidatorBlueprint
    { validatorTitle = "BJJ Belt System Minting Policy",
      validatorDescription = Just "Governs the rules for issuing new profiles, ranks, promotions, and membership tokens. It is parameterized by ProtocolParams (containing ProfilesValidator, RanksValidator, and MembershipsValidator script hashes), enabling secure cross-validator communication. Handles profile creation with CIP-68 standard metadata, initial rank assignment for practitioners, organization profile creation with membership histories root, promotion issuance, and membership history/interval creation. Features single minting authority for consistency, CIP-68 standard integration for NFT metadata, deterministic token generation to prevent collisions, exact mint checks to prevent other-token-name attacks, and cross-validator parameterization for secure communication.",
      validatorParameters =
        [ MkParameterBlueprint
            { parameterTitle = Just "Protocol Parameters",
              parameterDescription = Just "Compile-time protocol parameters including validator script hashes for secure cross-validator communication. Contains the ProfilesValidator, RanksValidator, and MembershipsValidator script hashes.",
              parameterPurpose = Set.singleton Mint,
              parameterSchema = definitionRef @ProtocolParams
            }
        ],
      validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Minting Redeemer",
            argumentDescription = Just "Redeemer for minting operations: CreateProfile (creates new profile with initial rank or membership root), Promote (creates pending promotion tokens), NewMembershipHistory (creates membership history node and first interval), or NewMembershipInterval (adds a new interval to an existing membership history). Validates creation dates against transaction validity range, ensures seed TxOutRef is spent to prevent replay attacks, enforces exact token minting to prevent other-token-name attacks, and requires spending of awarding authority's user token.",
            argumentPurpose = Set.fromList [Mint],
            argumentSchema = definitionRef @MintingRedeemer
          },
      validatorDatum =
        Nothing, -- Minting policies don't have datums
      validatorCompiled =
        Just $
          compiledValidator PlutusV3 (fromShort $ serialiseCompiledCode $ mintingPolicyCompile mp)
    }

-- | Profiles Validator Blueprint
profilesValidatorBlueprint :: ProtocolParams -> ValidatorBlueprint referencedTypes
profilesValidatorBlueprint _mp =
  MkValidatorBlueprint
    { validatorTitle = "BJJ Belt System Profiles Validator",
      validatorDescription = Just "Governs the rules for profile lifecycle and promotion acceptance. Handles profile information updates (image URI) and allows practitioners to accept pending promotions. Profile deletion is intentionally unsupported to preserve lineage integrity — BJJ belt records are permanent historical facts. Features token-based authorization requiring user tokens for profile modifications, full CIP-68 metadata support with size-limited fields, and atomic promotion acceptance. Uses the lightweight promoteProfileDatum function (R4 optimization) to avoid unnecessary Rank record construction.",
      validatorParameters = [], -- No parameters for this validator
      validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Profiles Redeemer",
            argumentDescription = Just "Redeemer for profile operations: UpdateProfileImage (updates profile image metadata with size validation) or AcceptPromotion (accepts pending promotion by updating the profile's currentRank pointer). Requires user token authorization for UpdateProfileImage; AcceptPromotion defers consent to RanksValidator via forced co-execution.",
            argumentPurpose = Set.fromList [Spend],
            argumentSchema = definitionRef @ProfilesRedeemer
          },
      validatorDatum =
        Just
          MkArgumentBlueprint
            { argumentTitle = Just "Profile Datum",
              argumentDescription = Just "CIP68 datum containing profile information with extensible metadata structure. Contains profile ID, profile type (Practitioner or Organization), current rank reference, and protocol parameters. Supports CIP-68 standard for interoperability with wallets and marketplaces while maintaining extensible metadata structure.",
              argumentPurpose = Set.singleton Spend,
              argumentSchema = definitionRef @(CIP68Datum OnchainProfile)
            },
      validatorCompiled =
        Just $
          compiledValidator PlutusV3 (fromShort $ serialiseCompiledCode profilesCompile)
    }

-- | Ranks Validator Blueprint
ranksValidatorBlueprint :: ProtocolParams -> ValidatorBlueprint referencedTypes
ranksValidatorBlueprint _mp =
  MkValidatorBlueprint
    { validatorTitle = "BJJ Belt System Ranks Validator",
      validatorDescription = Just "Enforces promotion acceptance and rank state transitions. When a promotion UTxO is spent, this validator verifies practitioner consent (User NFT), computes the updated profile datum and accepted rank via promoteProfile, validates both the profile output (R1 — prevents promotion consumption via alternative PV redeemer) and rank output, transforming the Promotion datum into a confirmed Rank datum. Works in conjunction with ProfilesValidator through forced co-execution.",
      validatorParameters = [], -- No parameters for this validator
      validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Ranks Redeemer",
            argumentDescription = Just "Redeemer for rank operations: PromotionAcceptance (profileOutputIdx, rankOutputIdx) — accepts a pending promotion by verifying practitioner consent and updating both profile and rank outputs atomically.",
            argumentPurpose = Set.fromList [Spend],
            argumentSchema = definitionRef @RanksRedeemer
          },
      validatorDatum =
        Just
          MkArgumentBlueprint
            { argumentTitle = Just "Rank Datum",
              argumentDescription = Just "Datum containing rank information with two states: Rank (confirmed rank with all promotion details including rankId, rankNumber, achievement/award profile IDs, achievement date, previous rank reference, and protocol parameters) or Promotion (pending promotion awaiting acceptance with promotionId, rank number, awarded-to/by profile IDs, achievement date, and protocol parameters).",
              argumentPurpose = Set.singleton Spend,
              argumentSchema = definitionRef @OnchainRank
            },
      validatorCompiled =
        Just $
          compiledValidator PlutusV3 (fromShort $ serialiseCompiledCode ranksCompile)
    }

-- | Memberships Validator Blueprint
membershipsValidatorBlueprint :: ProtocolParams -> ValidatorBlueprint referencedTypes
membershipsValidatorBlueprint _mp =
  MkValidatorBlueprint
    { validatorTitle = "BJJ Belt System Memberships Validator",
      validatorDescription = Just "Manages membership histories and intervals for practitioner-organization relationships. Membership histories are stored in a sorted linked list per organization. Intervals represent time-bounded membership periods that must be acknowledged by the practitioner. Enforces linked list invariants (ordering, adjacency, organization consistency), exact mint checks to force MintingPolicy co-execution for authorization, and practitioner consent for interval acceptance.",
      validatorParameters = [], -- No parameters for this validator
      validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Memberships Redeemer",
            argumentDescription = Just "Redeemer for membership operations: InsertNodeToMHList (inserts a new membership history node into the sorted linked list), UpdateNodeInMHList (adds a new interval to an existing membership history), or AcceptInterval (practitioner acknowledges a membership interval). InsertNodeToMHList and UpdateNodeInMHList require organization User NFT authorization; AcceptInterval requires practitioner User NFT.",
            argumentPurpose = Set.fromList [Spend],
            argumentSchema = definitionRef @MembershipsRedeemer
          },
      validatorDatum =
        Just
          MkArgumentBlueprint
            { argumentTitle = Just "Membership Datum",
              argumentDescription = Just "Sum type for datums stored at the MembershipsValidator address: ListNodeDatum (membership history node in the sorted linked list, containing organization ID, practitioner ID, interval head pointer, and interval count) or IntervalDatum (individual time-bounded membership period with start/end dates, practitioner reference, previous interval link, and acknowledgement flag).",
              argumentPurpose = Set.singleton Spend,
              -- MembershipDatum cannot be referenced here because NodeDatum (polymorphic) lacks HasBlueprintSchema.
              -- The actual datum type is MembershipDatum = ListNodeDatum MembershipHistoriesListNode | IntervalDatum OnchainMembershipInterval.
              argumentSchema = definitionRef @()
            },
      validatorCompiled =
        Just $
          compiledValidator PlutusV3 (fromShort $ serialiseCompiledCode membershipsCompile)
    }

-- | Legacy function for backward compatibility
mintingPolicyBlueprint :: ProtocolParams -> ContractBlueprint
mintingPolicyBlueprint mp =
  MkContractBlueprint
    { contractId = Just "BJJ Belt System Minting Policy",
      contractPreamble = myPreamble,
      contractValidators = Set.singleton (mintingPolicyValidatorBlueprint mp),
      contractDefinitions = deriveDefinitions @[ProtocolParams, CIP68Datum OnchainProfile, MintingRedeemer]
    }
