# Onchain Architecture Documentation

## Overview

The Decentralized BJJ Belt System implements a comprehensive smart contract architecture for managing Brazilian Jiu-Jitsu (BJJ) practitioner profiles and rank promotions on the Cardano blockchain. 

## Architecture Overview

### Core Validators

The system consists of three main validators that work together to manage the complete BJJ belt system:

1. **Minting Policy** - Controls token creation and destruction
2. **Profiles Validator** - Manages profile lifecycle and updates
3. **Ranks Validator** - Enforces BJJ promotion rules and rank state transitions

### Token Architecture

The system uses a sophisticated token architecture based on CIP-68 standards:

- **Profile Ref Token**: NFT locked at Profiles Validator containing profile datum
- **Profile User Token**: NFT held by profile owner for authorization
- **Rank State Token**: NFT locked at Ranks Validator containing rank datum (a rank can be accepted rank or promotion) 


#### Minting Policy

**Purpose**: 
Governs the rules for issuing new profiles, ranks, and promotions. It is parameterized by the Profiles and Ranks validator script hashes, ensuring secure cross-validator communication. It handles:
- Profile creation with CIP-68 standard metadata 
- Initial rank assignment for practitioners
- Promotion issuance 
- Profile deletion

**Design Decisions**:
- **Single Minting Authority**: All tokens minted through one policy ensures consistency
- **CIP-68 Standard Integration**: Leverages Cardano's CIP-68 standard for NFT metadata, ensuring interoperability with wallets and marketplaces while maintaining extensible metadata structure.
- **Deterministic Token Generation**: Hash-based token naming to prevent token name collisions and ensures uniqueness using `generateRankId` and `nameFromTxOutRef`
- **Cross-Validator Parameterization**: Validator script hashes are embedded in the minting policy to ensure secure communication between validators

**Security Considerations**:
- Validates creation dates against transaction validity range
- Ensures seed TxOutRef is spent (prevents replay attacks)
- Enforces exact token minting (prevents other-token-name attacks)
- Requires spending of awarding authority's user token for promotions

**Profile Creation Flow**:
1. User spends a UTxO based on which generates ProfileRef, ProfileUser token names (and Rank State Token Name of the first rank in case of practitioner profiles)
2. Creates initial profile datum with CIP-68 metadata and initial rank datum
3. Locks ProfileRef token with profile datum at Profiles Validator
4. Locks RankState Token with first rank datum at Ranks Validator (for practitioners)
5. User receives the User token

#### Profiles Validator 

**Purpose**:  
Governs the rules for profile lifecycle and promotion acceptance. It handles:
- Profile information updates 
- Profile deletion with token burning
- Allows practitioners to accept pending promotions

**Design Decisions**:
- **Token-based Authorization**: User tokens required for profile modifications
- **CIP-68 Metadata Support**: Full support for CIP-68 metadata updates
- **Atomic Promotion Acceptance**: Promotion acceptance updates both profile and rank datums atomically

**State Management**:
- Profile datums reference current rank information
- Promotion acceptance updates both profile and rank datums atomically
- Deletion burns both Ref and User tokens


#### Ranks Validator

**Purpose**:
Enforces BJJ promotion rules and validates rank progression. It handles:
- Promotion rule enforcement using BJJ-specific logic
- Time-based validation between ranks
- Rank state transitions from pending to confirmed

**Design Decisions**:
- **Reference Input Validation**: Uses reference inputs to validate promotion rules without consuming tokens
- **BJJ Rule Engine**: Implements comprehensive BJJ promotion rules with time requirements
- **Two-Phase Promotion**: Supports pending promotions that can be accepted or rejected

**Promotion Flow**: 
Rank promotion involves a two-phase process:
1. **Promotion Creation**: A pending promotion is created and locked at the Ranks Validator
2. **Promotion Acceptance**: Upon successful validation, the pending promotion transforms into the new rank


