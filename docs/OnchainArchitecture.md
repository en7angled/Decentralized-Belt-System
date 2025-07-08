# Onchain Architecture Documentation


## Overview

The Decentralized BJJ Belt System implements a comprehensive smart contract architecture for managing Brazilian Jiu-Jitsu (BJJ) practitioner profiles and rank promotions on the Cardano blockchain. 

## Architecture Overview

### Core Validators


#### Minting Policy


**Purpose**: 
Governs the rules for issuing new profiles, ranks, and promotions. It is parameterized by the Profiles and Ranks validator script hashes, ensuring secure cross-validator communication. It handles:
- Profile creation with CIP-68 standard metadata 
- Initial rank assignment for practitioners
- Promotion issuance 
- Profile deletion

**Design Decisions**:
- **Single Minting Authority**: All tokens minted through one policy ensures consistency
- **CIP-68 Standard Integration**: Leverages Cardano's CIP-68 standard for NFT metadata, ensuring interoperability of the Profile NFT, with wallets and marketplaces while maintaining extensible metadata structure.
- **Deterministic Token Generation**:  Hash-based token naming to prevent token name collisions and ensures uniqueness `generateRankId` and `nameFromTxOutRef`


**Security Considerations**:
- Validates creation dates against transaction validity range
- Ensures seed TxOutRef is spent (prevents replay attacks)
- Enforces exact token minting (prevents other-token-name attacks)

**Profile Creation Flow**

1. User spends a UTxO based on which generates ProfileRef, ProfileUser token names (and Rank State Token Name of the first rank in case of practitioner profiles)
4. Creates initial profile datum with CIP-68 metadata and initial rank datum
5. Locks ProfileRef token with profile datum at Profiles Validator
6. Locks RankState Token with first rank datum at Ranks Validator
6. User receives the User token 



####  Profiles Validator 

**Purpose**:  
Governs the rules for profile lifecycle and promotion acceptance. It handles:
- Profile information updates
- Profile deletion
- Allows practitioners to accept pending promotions
**Design Decisions**:
- **Token-based Authorization**: User tokens required for profile modifications

**State Management**:
- Profile datums references current rank information
- Promotion acceptance updates both profile and rank datums atomically
- Deletion burns both Ref and User tokens




####  Ranks Validator

**Purpose**:
Enforces BJJ promotion rules and validates rank progression. It handles:
- Promotion rule enforcement using BJJ-specific logic
- Time-based validation between ranks
- Rank state transitions from pending to confirmed


**Design Decisions**:
- **Reference Input Validation**: Uses reference inputs to validate promotion rules without consuming tokens
- **BJJ Rule Engine**: Implements comprehensive BJJ promotion rules
- **Time-based Validation**: Enforces minimum time requirements between ranks


**Promotion Flow**: Rank promotion involves a two-phase process:
1. Promotion Creation: A pending promotion is created and locked at the Ranks Validator
2. Promotion Acceptance: Upon successful validation, the pending promotion transforms into the new rank

### 2. **Promotion Flow**

1. Master initiates promotion, issues new RankState Token and creates promotion datum 
2. Locks RankState Token with promotion datum at Ranks Validator
3. Student accepts promotion (Profiles Validator), provides User token
4. Validates promotion rules (Ranks Validator)
5. Updates both profile and rank datums




