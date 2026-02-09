# Onchain Architecture Documentation


- [Onchain Architecture Documentation](#onchain-architecture-documentation)
  - [Overview](#overview)
  - [Script Dependencies & Parameterization](#script-dependencies--parameterization)
  - [Minting Policy](#minting-policy)
  - [Profiles Validator](#profiles-validator)
  - [Ranks Validator](#ranks-validator)
  - [Memberships Validator](#memberships-validator)
  - [Promotion Flow](#promotion-flow)
  - [Membership Flow](#membership-flow)
  - [BJJ Promotion Rules](#bjj-promotion-rules)
  - [Security Model](#security-model)


## Overview

The Decentralized BJJ Belt System implements a comprehensive smart contract architecture for managing Brazilian Jiu-Jitsu (BJJ) practitioner profiles, rank promotions, and organization memberships on the Cardano blockchain. 

The system consists of four main validators that work together to manage the complete BJJ belt system:

1. **Minting Policy** - Controls token creation and validates promotions/memberships at creation time
2. **Profiles Validator** - Manages profile updates and validates promotion acceptance
3. **Ranks Validator** - Handles promotion consumption with consent validation
4. **Memberships Validator** - Manages membership histories and intervals for organization-practitioner relationships

**Immutability Principle**: Profiles are permanent by design. BJJ belt records are historical facts that should not be erasable, preserving lineage integrity and verification.

**Specific tokens**: 

- **Profile Ref Token**: NFT locked at Profiles Validator containing profile datum
- **Profile User Token**: NFT held by profile owner for authorization
- **Rank State Token**: NFT locked at Ranks Validator containing rank datum (can be an accepted Rank or pending Promotion)
- **Membership Histories Root Token**: NFT locked at Memberships Validator; root of the sorted linked list of membership histories for an organization
- **Membership History Token**: NFT locked at Memberships Validator; node in the membership histories linked list representing a practitioner's membership at an organization
- **Membership Interval Token**: NFT locked at Memberships Validator; represents a specific time period of membership


## Script Dependencies & Parameterization

The BJJ Belt System uses a carefully designed script dependency architecture that balances security, flexibility, and deployment complexity.

### Deployment Order

Scripts must be deployed in a specific order due to hash dependencies:

```
1. ProfilesValidator (unparameterized) → hash known
2. RanksValidator (unparameterized) → hash known
3. MembershipsValidator (unparameterized) → hash known
4. MintingPolicy (parameterized by all 3 hashes) → compiled with ProtocolParams
```

### ProtocolParams Structure

```haskell
newtype ProtocolParams = ProtocolParams (ScriptHash, ScriptHash, ScriptHash)
-- Tuple: (RanksValidatorHash, ProfilesValidatorHash, MembershipsValidatorHash)
```

The `ProtocolParams` is embedded in:
- **MintingPolicy**: At compile time via `mintingPolicyCompile params`
- **OnchainProfile datum**: Via `protocolParams :: ProtocolParams` field
- **OnchainRank datum**: Via `rankProtocolParams` / `promotionProtocolParams` field

### Why This Design?

| Script | Parameterized? | How It Gets Cross-Validator Addresses |
|--------|----------------|--------------------------------------|
| MintingPolicy | Yes (by ProtocolParams) | Directly from compiled-in params |
| ProfilesValidator | No | From profile datum's `protocolParams` |
| RanksValidator | No | From rank datum's `promotionProtocolParams` |
| MembershipsValidator | No | No cross-validator lookups needed (authorization via User NFTs) |

**Benefits**:

1. **Single Point of Truth**: ProtocolParams defined once at deployment, then propagated through datums
2. **Unparameterized Validators**: ProfilesValidator, RanksValidator, and MembershipsValidator have fixed script hashes, making them reusable across protocol instances
3. **Self-Contained Datums**: Each profile/rank datum carries the protocol configuration, enabling cross-validator lookups at runtime
4. **Upgrade Path**: New MintingPolicy versions can be deployed without changing validator hashes

**Trade-offs**:

1. **Larger Datums**: Each datum carries ProtocolParams (~96 bytes for three script hashes)
2. **Trust at Creation**: The MintingPolicy embeds correct addresses at profile creation; profiles trust the params they were created with
3. **No Runtime Flexibility**: A profile cannot switch to a different protocol instance after creation

### Security Implications

The parameterization creates a **trust chain**:

```
MintingPolicy (trusted at deployment)
    ↓ embeds ProtocolParams
Creates Profile with protocolParams
    ↓ datum contains addresses
ProfilesValidator uses datum.protocolParams to find RanksValidator
    ↓ looks up ranks at correct address
RanksValidator validates consent
```

**Attack Prevention**:
- Malicious MintingPolicy with wrong params would create profiles pointing to wrong validators
- This is mitigated by: (1) verifying MintingPolicy source before use, (2) profiles only work with the validators they reference
- A profile created with wrong ProtocolParams would be "orphaned" - unable to interact with the real validators

### CurrencySymbol Inheritance

All tokens (ProfileRef, ProfileUser, Rank, Promotion, MembershipHistoriesRoot, MembershipHistory, MembershipInterval) share the same `CurrencySymbol` because they're minted by the same MintingPolicy. This enables:
- Easy identification of protocol tokens
- `deriveUserFromRefAC` works because Ref and User tokens share the CurrencySymbol
- `hasCurrencySymbol` validates that any referenced AssetClass belongs to the protocol
- ProfileRef and ProfileUser are distinguishable by CIP-68 prefixes (`0x000643b0` vs `0x000de140`)

Note: Rank, Promotion, and Membership tokens are **not distinguishable by TokenName** - all are 28-byte `blake2b_224` hashes. They're distinguished by their **datum type** and **validator address**:
- Rank vs Promotion: by datum constructor (`Rank` vs `Promotion`) at the RanksValidator address
- MembershipHistoriesListNode vs MembershipInterval: by the `MembershipDatum` wrapper (`ListNodeDatum` vs `IntervalDatum`) at the MembershipsValidator address


## Minting Policy

**Purpose**: 
Governs the rules for issuing new profiles, ranks, promotions, and membership tokens. It is **parameterized by `ProtocolParams`** (containing ProfilesValidator, RanksValidator, and MembershipsValidator script hashes), enabling secure cross-validator communication. It handles:
- Profile creation with CIP-68 standard metadata 
- Initial rank assignment for practitioners
- **Organization profile creation with membership histories root**
- **Promotion creation with full BJJ rule validation**
- **Membership history initialization (new member enrollment)**
- **Membership interval creation (adding intervals to existing members)**

Note: Profile deletion is intentionally NOT supported to preserve lineage integrity.

**Redeemers**:
- `CreateProfile TxOutRef MetadataFields OnChainProfileType POSIXTime Integer Integer Integer` - Create a new profile (seedTxOutRef, metadata, profileType, creationDate, rankNumber, profileOutputIdx, rankOrMembershipRootOutputIdx). For Organization profiles, also creates the membership histories root.
- `Promote TxOutRef ProfileId ProfileId POSIXTime Integer Integer` - Create a promotion (seedTxOutRef, studentProfileId, masterProfileId, achievementDate, rankNumber, pendingRankOutputIdx)
- `NewMembershipHistory ProfileId ProfileId POSIXTime (Maybe POSIXTime) MembershipHistoriesListNodeId Integer` - Initialize a membership history for a new member (organizationProfileId, practitionerId, startDate, maybeEndDate, leftNodeId, firstIntervalOutputIdx)
- `NewMembershipInterval ProfileId MembershipHistoriesListNodeId POSIXTime (Maybe POSIXTime) Integer` - Add a new interval to an existing membership history (organizationProfileId, membershipNodeId, startDate, maybeEndDate, intervalOutputIdx)

**Parameterization**:
```haskell
mintingPolicyCompile :: ProtocolParams -> CompiledCode (BuiltinData -> BuiltinUnit)
mintingPolicyCompile params = $$(compile [||mintingPolicyUntyped||]) `unsafeApplyCode` liftCode plcVersion110 params
```
The `ProtocolParams` is **baked into the compiled script** at deployment time. This means:
- The MintingPolicy's script hash depends on the validator hashes
- Each unique triple of validators produces a unique MintingPolicy hash (and thus unique CurrencySymbol)
- The MintingPolicy cannot be changed to point to different validators after deployment

**Design Decisions**:
- **Single Minting Authority**: All tokens minted through one policy ensures consistency
- **CIP-68 Standard Integration**: Leverages Cardano's CIP-68 standard for NFT metadata, ensuring interoperability with wallets and marketplaces while maintaining extensible metadata structure
- **Deterministic Token Generation**: Hash-based token naming using `blake2b_224` for 28-byte token names:
  - `generateRankId`: from profile ID + rank number
  - `generatePromotionRankId`: from seed TxOutRef
  - `deriveMembershipHistoriesListId`: from organization profile ID
  - `deriveMembershipHistoryId`: from organization ID + practitioner ID
  - `deriveMembershipIntervalId`: from membership history ID + interval number
  - Integer values are encoded efficiently using PlutusV3's `integerToByteString` builtin
- **Embedded ProtocolParams**: The `ProtocolParams` is also embedded in every created profile and rank datum, enabling validators to resolve cross-validator addresses
- **Fail-Fast Promotion Validation**: Full BJJ rule validation at mint time prevents invalid promotions from being created
- **Metadata Size Enforcement**: `validateMetadataFields` enforces size limits at creation time
- **Atomic Membership Initialization**: `initMembershipHistory` creates both the membership history and first interval atomically, ensuring a history always has at least one interval

**Security Considerations**:
- Validates creation dates against transaction validity range
- Ensures seed TxOutRef is spent (prevents replay attacks and guarantees uniqueness)
- Enforces exact token minting via `mintValueMinted` (prevents other-token-name attacks)
- Requires spending of awarding authority's user token for promotions
- Requires spending of organization's user token for membership operations
- Validates CurrencySymbol of all referenced AssetClasses (`hasCurrencySymbol`)
- Validates that membership-related UTxOs are spent from the correct validator address
- Validates metadata field sizes to prevent DoS via oversized datums

**Profile Creation Flow (Practitioner)**:
1. User spends a seed UTxO which generates ProfileRef, ProfileUser, and Rank State token names
2. Creates initial profile datum with CIP-68 metadata and initial rank datum
3. Locks ProfileRef token with profile datum at Profiles Validator
4. Locks RankState Token with first rank datum at Ranks Validator
5. User receives the User token

**Profile Creation Flow (Organization)**:
1. User spends a seed UTxO which generates ProfileRef and ProfileUser token names
2. Creates initial profile datum with CIP-68 metadata
3. Derives `MembershipHistoriesRootId` from the profile ID via `deriveMembershipHistoriesListId`
4. Creates empty membership histories root datum via `initEmptyMembershipHistoriesList`
5. Locks ProfileRef token with profile datum at Profiles Validator
6. Locks MembershipHistoriesRoot token with root datum (wrapped in `ListNodeDatum`) at Memberships Validator
7. User receives the User token

**Promotion Creation Flow** (at mint time):
1. Master spends a seed UTxO for unique promotion ID generation
2. Master spends their User token (authorization)
3. References student's profile and current rank (validation)
4. References master's profile and current rank (validation)
5. **Full BJJ rule validation** via `validatePromotion`:
   - Master belt level is sufficient (≥ Black)
   - Master outranks the target belt
   - Sequential promotion (next belt > current belt)
   - Time-in-belt requirements met
   - Date ordering is valid
6. Locks pending Promotion datum at Ranks Validator

**New Membership History Flow** (at mint time, `NewMembershipHistory` redeemer):
1. Organization spends their User token (authorization)
2. Organization spends the left node UTxO from the membership histories linked list at Memberships Validator
3. `initMembershipHistory` creates both `OnchainMembershipHistory` and first `OnchainMembershipInterval` atomically
4. Derives deterministic IDs: `deriveMembershipHistoryId(orgId, practitionerId)` and `deriveMembershipIntervalId(historyId, 0)`
5. Mints membership history NFT + first interval NFT (exact mint check)
6. Locks first interval (wrapped in `IntervalDatum`) at Memberships Validator
7. Left node update, inserted node lock, and linked list ordering are **enforced by MembershipsValidator** (`InsertNodeToMHList`)

**New Membership Interval Flow** (at mint time, `NewMembershipInterval` redeemer):
1. Organization spends their User token (authorization)
2. Organization spends the membership history node UTxO from Memberships Validator
3. References the last interval (head) from Memberships Validator
4. `addMembershipIntervalToHistory` validates: last interval is the head, is accepted, is closed (endDate in past relative to new startDate)
5. Derives deterministic ID: `deriveMembershipIntervalId(historyId, lastNumber + 1)`
6. Mints new interval NFT (exact mint check)
7. Locks new interval (wrapped in `IntervalDatum`) at Memberships Validator
8. Membership history node update is **enforced by MembershipsValidator** (`UpdateNodeInMHList`)


## Profiles Validator 

**Purpose**:  
Governs the rules for profile updates and promotion acceptance. It handles:
- Profile image updates (with metadata size validation)
- **Promotion acceptance with state validation**

Note: Profile deletion is intentionally NOT supported to preserve lineage integrity.

**Redeemers**:
- `UpdateProfileImage ProfileId ImageURI Integer` - Update the profile's image URI (profileId, newImageURI, profileOutputIdx)
- `AcceptPromotion RankId Integer Integer` - Accept a pending promotion (rankId, profileOutputIdx, rankOutputIdx)

**Cross-Validator Communication**:
The ProfilesValidator is **not parameterized**. Instead, it retrieves the RanksValidator address from the profile datum's embedded `protocolParams`:
```haskell
ranksValidatorAddress = V1.scriptHashAddress $ ranksValidatorScriptHash $ protocolParams profile
```
This design means the validator dynamically resolves addresses based on datum content.

**Design Decisions**:
- **Token-based Authorization**: User tokens required for profile modifications
- **CIP-68 Metadata Support**: Full support for CIP-68 metadata updates with size limits
- **Atomic Promotion Acceptance**: Promotion acceptance updates both profile and rank datums atomically
- **Acceptance-Time Validation**: Validates that promotion is still valid given current state
- **Immutability**: Profiles cannot be deleted once created

**Metadata Size Limits** (enforced during creation and updates):
| Field | Max Size |
|-------|----------|
| `name` | 128 bytes |
| `description` | 1024 bytes |
| `imageURI` | 256 bytes |

**UpdateProfileImage Validation**:
1. User must spend their Profile User NFT (authorization)
2. Image URI must pass size validation (`validateImageURI`)
3. Updated datum must be locked at the same address with same value

**AcceptPromotion Validation**:
1. Reads promotion from `txInfoInputs` (spending the Promotion UTxO)
2. Reads current rank from `txInfoReferenceInputs` (non-destructive read)
3. Validates promotion is still valid:
   - `nextBelt > currentBelt` - Prevents double-acceptance
   - `nextBeltDate > currentBeltDate` - Prevents out-of-order acceptance
4. Outputs correctly formed updated profile and rank datums

Note: User NFT consent is NOT checked here because RanksValidator always runs (Promotion UTxO must be spent) and guarantees consent. This avoids redundant validation.

**State Management**:
- Profile datums reference current rank via `currentRank :: Maybe RankId`
- Promotion acceptance atomically updates both profile datum (new `currentRank`) and transforms Promotion into Rank


## Ranks Validator

**Purpose**:
Handles the consumption of pending promotions with minimal validation. Since full BJJ rule validation happens at mint time in the Minting Policy, this validator is simplified to only verify consent.

**Redeemers**:
- `PromotionAcceptance Integer Integer` - Accept a pending promotion (profileOutputIdx, rankOutputIdx)

**Not Parameterized**: The RanksValidator is unparameterized. It extracts the ProfilesValidator address from the datum's `promotionProtocolParams` and all needed identity information from the `OnchainRank` datum:
```haskell
let studentProfileId = promotionAwardedTo promotionRank
    profileUserAssetClass = deriveUserFromRefAC studentProfileId
    profilesValidatorAddress = V1.scriptHashAddress $ profilesValidatorScriptHash $ promotionProtocolParams promotionRank
```

**Design Decisions**:
- **Consent + Output Validation**: Checks that the student spends their User token and validates both profile and rank output datums
- **Trust the Mint**: Since promotions are fully validated at creation time, no re-validation of BJJ rules needed
- **Datum-Driven**: All needed information comes from the Promotion datum (student profile ID, protocol params)
- **Atomic State Update**: Validates both the updated profile and the transformed rank datum in the same transaction

**Validation Logic**:
1. Student must spend their Profile User NFT (consent via `deriveUserFromRefAC`)
2. Student's profile is spent from `txInfoInputs` and must contain the Profile Ref NFT
3. `promoteProfile` transforms the Promotion into a Rank and updates the profile's `currentRank`
4. Updated profile datum must be locked at ProfilesValidator address (output idx check)
5. New rank datum must be locked at RanksValidator address (output idx check)


## Memberships Validator

**Purpose**:
Manages the membership histories linked list and membership intervals for organization-practitioner relationships. It handles:
- Inserting new membership history nodes into the sorted linked list
- Updating membership history nodes when new intervals are added
- Accepting membership intervals (practitioner consent)

Note: The Memberships Validator is **not parameterized**. All authorization is done through User NFT spending.

**Datum Type** (`MembershipDatum`):
All datums stored at the Memberships Validator address are wrapped in the `MembershipDatum` sum type:
```haskell
data MembershipDatum
  = ListNodeDatum MembershipHistoriesListNode
  | IntervalDatum OnchainMembershipInterval
```

This allows the validator to distinguish between linked list nodes and interval records at the same address.

**Data Types**:

```haskell
-- Wrapper containing the linked list node + organization identity
data MembershipHistoriesListNode = MembershipHistoriesListNode
  { organizationId :: ProfileId
  , nodeInfo :: NodeDatum (Maybe OnchainMembershipHistory)
  }

-- One per practitioner per organization
data OnchainMembershipHistory = OnchainMembershipHistory
  { membershipHistoryId :: MembershipHistoryId
  , membershipHistoryPractitionerId :: ProfileId
  , membershipHistoryOrganizationId :: ProfileId
  , membershipHistoryIntervalsHeadId :: MembershipIntervalId  -- always points to the head interval
  }

-- Specific time period of membership (prepend-only linked list via miPrevId)
data OnchainMembershipInterval = OnchainMembershipInterval
  { membershipIntervalId :: MembershipIntervalId
  , membershipIntervalStartDate :: POSIXTime
  , membershipIntervalEndDate :: Maybe POSIXTime     -- Nothing = open-ended (active)
  , membershipIntervalIsAck :: Bool                   -- True = accepted by practitioner
  , membershipIntervalPrevId :: Maybe MembershipIntervalId  -- Nothing for first interval
  , membershipIntervalNumber :: Integer               -- sequential number (0-based)
  , membershipIntervalPractitionerId :: ProfileId     -- for User NFT derivation in AcceptInterval
  }
```

**Redeemers**:
- `InsertNodeToMHList { maybeRightNodeId, insertedMembershipHistory, updatedLeftNodeTxOutIdx, insertedNodeTxOutIdx }` - Insert a new membership history node into the sorted linked list when a new member joins an organization
- `UpdateNodeInMHList { lastIntervalId, startDate, endDate, updatedNodeTxOutIdx }` - Update a membership history node when adding a new interval to an existing member
- `AcceptInterval { updatedIntervalTxOutIdx }` - Accept a membership interval (practitioner consent, Variant A: same NFT, datum update only)

**Design Decisions**:
- **Sorted Linked List**: Membership histories are stored in a sorted linked list (by `practitionerId`) per organization, enabling deterministic insertion and preventing duplicates
- **MembershipDatum Wrapper**: A sum type distinguishes between `ListNodeDatum` and `IntervalDatum` at the same validator address, ensuring correct datum parsing
- **Variant A Accept**: Membership interval acceptance updates the existing datum (`miIsAck = True`) without burning/minting new tokens, reducing transaction costs
- **Prepend-Only Intervals**: New intervals are prepended to the history's interval chain, with the head always being the most recent. This simplifies "is current interval closed?" checks.
- **Practitioner ID in Interval**: Stored directly in the interval datum to allow `AcceptInterval` to derive the User NFT without requiring a reference input to the membership history
- **Interval Number**: Sequential numbering enables deterministic interval ID derivation via `deriveMembershipIntervalId(historyId, number)`

**Linked List Structure** (per organization):
```
Root (nodeKey=Nothing, nextNodeKey=Just practA)
  → Node practA (MembershipHistory for practitioner A, nextNodeKey=Just practB)
    → Node practB (MembershipHistory for practitioner B, nextNodeKey=Nothing)
```

The root is created atomically with the organization profile (`CreateProfile Organization`). List uniqueness is guaranteed because only one root per organization can be minted (derived deterministically from the organization's profile ID).

**InsertNodeToMHList Validation**:
1. Spending node must be a `ListNodeDatum` (the left node in the linked list)
2. Validates linked list insertion rules (adjacency, ordering, same organization) via `insertMembershipHistoryInBetween` or `appendMembershipHistory`
3. Verifies exact mint: membership history NFT + first interval NFT
4. Locks updated left node with `ListNodeDatum` wrapper at own address
5. Locks inserted node with `ListNodeDatum` wrapper at own address

**UpdateNodeInMHList Validation**:
1. Spending node must be a `ListNodeDatum` (the membership history node)
2. References the last (head) interval from Memberships Validator via `unsafeGetMembershipInterval`
3. Validates via `addMembershipIntervalToHistory`: last interval == head, accepted, closed (end date past relative to new start)
4. Verifies exact mint: new interval NFT only
5. Locks updated node with `ListNodeDatum` wrapper at own address

**AcceptInterval Validation**:
1. Spending datum must be an `IntervalDatum` (the unaccepted interval)
2. Updates `membershipIntervalIsAck` to `True` via `acceptMembershipInterval` (fails if already accepted)
3. Derives practitioner User NFT from `membershipIntervalPractitionerId` via `deriveUserFromRefAC`
4. Practitioner must spend their User NFT (consent)
5. Locks updated interval with `IntervalDatum` wrapper at own address with same value (tokens never leave the validator)
6. No minting/burning occurs

**Security Considerations**:
- Tokens locked at the validator never leave (output checks guarantee same address + same value)
- Exact mint checks prevent other-token-name attacks
- Organization User NFT required for `InsertNodeToMHList` and `UpdateNodeInMHList` (enforced by MintingPolicy)
- Practitioner User NFT required for `AcceptInterval` (enforced by MembershipsValidator)
- Linked list ordering and same-organization constraints prevent cross-organization manipulation
- CurrencySymbol validation in MintingPolicy prevents use of foreign tokens


## Promotion Flow

Rank promotion involves a two-phase process with validation split across phases:

### Phase 1: Promotion Creation (Minting Policy)

**Who pays**: The master (promoter)

**Validations performed**:
- Master is authorized (spends their User token)
- Master's belt level is sufficient to promote
- Student exists and is a practitioner
- Student's current rank allows this promotion
- Time-in-belt requirements are met
- All date orderings are valid
- Unique promotion ID generated from seed TxOutRef

**Result**: Pending Promotion datum locked at Ranks Validator

### Phase 2: Promotion Acceptance (Profiles + Ranks Validators)

**Who pays**: The student (recipient)

**Validations performed**:
- **RanksValidator**: Student consents (spends their User token via `deriveUserFromRefAC`)
- **ProfilesValidator**: Promotion is still valid given current state:
  - `nextBelt > currentBelt`
  - `nextBeltDate > currentBeltDate`
- **ProfilesValidator**: Output datums are correctly formed

Note: ProfilesValidator intentionally does NOT check User NFT consent because RanksValidator always runs (Promotion UTxO must be spent) and guarantees it. This avoids redundant checks.

**Result**: 
- Promotion datum transforms into accepted Rank datum
- Profile datum updated with new current rank


## Membership Flow

Organization membership involves a multi-phase process for enrolling practitioners and managing membership periods.

### Requirements

- A practitioner can have one or more "membership histories" (one per organization)
- At a single organization, a participant can have at most one "membership history"
- Within a membership history, a participant can have multiple "membership intervals" (historical records), but at most one active interval (the most recent, if its end date is in the future or unset)
- Previous intervals are closed (have an end date in the past); they represent history
- An organization can have multiple membership histories, one per participant it has ever had

### Phase 1: Organization Creates Membership (Minting Policy + Memberships Validator)

**Who pays**: The organization

Two sub-cases:

#### Case A: New Member (no existing membership history at this organization)

**Transaction involves two scripts**: `MintingPolicy` (`NewMembershipHistory`) + `MembershipsValidator` (`InsertNodeToMHList`)

1. Organization spends their User token (authorization, enforced by MP)
2. Organization spends the left node UTxO from the membership histories linked list (enforced by MP + MV)
3. MP creates both `OnchainMembershipHistory` and first `OnchainMembershipInterval` atomically
4. MP mints membership history NFT + first interval NFT
5. MV validates linked list insertion (ordering, adjacency, same organization)
6. MV locks updated left node and inserted node (both wrapped in `ListNodeDatum`)
7. MP locks first interval (wrapped in `IntervalDatum`) at MV address

**Result**: New membership history node in the linked list + first unaccepted interval locked at Memberships Validator

#### Case B: Existing Member (membership history already exists)

**Transaction involves two scripts**: `MintingPolicy` (`NewMembershipInterval`) + `MembershipsValidator` (`UpdateNodeInMHList`)

1. Organization spends their User token (authorization, enforced by MP)
2. Organization spends the membership history node UTxO (enforced by MP + MV)
3. MP references the last interval to validate it's closed and accepted
4. MP mints new interval NFT
5. MV validates the update and locks the updated history node (wrapped in `ListNodeDatum`)
6. MP locks new interval (wrapped in `IntervalDatum`) at MV address

**Result**: Updated membership history head + new unaccepted interval locked at Memberships Validator

### Phase 2: Practitioner Accepts Membership (Memberships Validator only)

**Who pays**: The practitioner

**Transaction involves one script**: `MembershipsValidator` (`AcceptInterval`)

1. Practitioner spends their User token (consent, enforced by MV)
2. MV spends the unaccepted interval UTxO
3. MV updates `membershipIntervalIsAck` to `True` (Variant A: same NFT, no burn/mint)
4. MV locks the updated interval (wrapped in `IntervalDatum`) at the same address

**Result**: Interval datum updated with `isAck = True`

Note: No minting or burning occurs in this phase. This is "Variant A" from the design — the simplest and cheapest approach.

### Phase 3: Update Membership End Date (Memberships Validator only)

**Who pays**: The organization or practitioner

**Transaction involves one script**: `MembershipsValidator` (`UpdateEndDate` — *not yet implemented*)

1. Organization or practitioner spends their User token
2. MV spends the active interval UTxO
3. MV validates the new end date is in the future (>= current time from validity range)
4. If an end date already exists, the new end date must be >= existing end date
5. MV locks the updated interval (wrapped in `IntervalDatum`) at the same address

**Result**: Interval datum updated with `membershipIntervalEndDate = Just newEndDate`

### Membership Token ID Derivation

| Token | Derivation | Purpose |
|-------|-----------|---------|
| Membership Histories Root | `blake2b_224(profileRefTokenName)` | One per organization, created with org profile |
| Membership History | `blake2b_224(orgTokenName ++ practitionerTokenName)` | One per org-practitioner pair |
| Membership Interval | `blake2b_224(historyTokenName ++ intervalNumber)` | Sequential per history |

All share the same `CurrencySymbol` as other protocol tokens.


## BJJ Promotion Rules

The system enforces authentic BJJ promotion rules on-chain via `validatePromotion` in `Onchain.BJJ`.

### Belt Hierarchy

| Index | Belt | Min Time at Previous |
|-------|------|---------------------|
| 0 | White | - |
| 1 | Blue | 12 months |
| 2 | Purple | 18 months |
| 3 | Brown | 12 months |
| 4 | Black | 12 months |
| 5-10 | Black 1st-6th Degree | 36-60 months |
| 11 | Red & Black (7th Degree) | 84 months |
| 12 | Red & White (8th Degree) | 84 months |
| 13 | Red (9th Degree) | 120 months |
| 14 | Red 10th Degree | - |

### Validation Rules

The `validatePromotion` function enforces:

1. **Master Belt Authority**: Only Black belts and above can promote
2. **Master Belt Restriction**: 1st Degree Black can only promote up to Brown (not to Black)
3. **Master Outranks Target**: Master's belt must be higher than student's target belt
4. **Date Ordering**: Master's belt date must precede the promotion date
5. **Sequential Promotion**: Target belt must be exactly one level above current belt
6. **Date Progression**: Promotion date must be after current belt date
7. **Time-in-Belt**: Student must have held current belt for the minimum required duration

### Organization Handling

Organizations (`profileType = Organization`) have `currentRank = Nothing`. They:
- Cannot receive promotions (no rank to promote from)
- Cannot be promoted by the system (getCurrentRankId fails for them)
- This is enforced by the MintingPolicy's reference input lookup failing


## Output Index Optimization

All validators use an **output index optimization** for efficient O(1) output validation instead of O(n) search.

### How It Works

Instead of searching through all transaction outputs to find a specific output (via `hasTxOutWithInlineDatumAndValue`), validators receive the expected output index directly in the redeemer and verify the output at that specific index (via `checkTxOutAtIndex`).

**Before** (O(n) search):
```haskell
hasTxOutWithInlineDatumAndValue datum value address txInfoOutputs
-- Searches all outputs until match found
```

**After** (O(1) indexed lookup):
```haskell
checkTxOutAtIndex outputIdx datum value address txInfoOutputs
-- Directly accesses output at specified index
```

### Output Index Conventions

Off-chain transaction builders track output indices and include them in redeemers. The order in the skeleton `mconcat` determines the output indices:

| Transaction | Output 0 | Output 1 | Output 2 | Output 3 |
|-------------|----------|----------|----------|----------|
| CreateProfile (Practitioner) | Profile state | User NFT | Rank state | - |
| CreateProfile (Organization) | Profile state | User NFT | MH Root state | - |
| UpdateProfile | Updated profile | - | - | - |
| Promote | Pending rank | - | - | - |
| AcceptPromotion | Updated profile | Updated rank | - | - |
| NewMembershipHistory | Updated left node | Inserted MH node | First interval | - |
| NewMembershipInterval | Updated MH node | New interval | - | - |
| AcceptInterval | Updated interval | - | - | - |

### Benefits

| Aspect | Before | After |
|--------|--------|-------|
| Time complexity | O(n) per output check | O(1) per output check |
| Execution units | Higher (iteration overhead) | Lower (direct access) |
| Script size | Slightly smaller | Slightly larger (index params) |

### Security

The output index is provided by the off-chain code but validated on-chain:
- If an incorrect index is provided, the output at that index won't match the expected datum/value/address
- The transaction will fail with a clear error message
- There's no security risk from malicious indices - they just cause validation failure


## Security Model

### Multi-Layer Validation

| Stage | Validator | What's Validated | Who Pays |
|-------|-----------|-----------------|----------|
| Promotion Creation | MintingPolicy | Full BJJ rules, master authorization, uniqueness | Master |
| Promotion Acceptance | ProfilesValidator + RanksValidator | Consent, state validity | Student |
| Membership History Init | MintingPolicy + MembershipsValidator | Org authorization, linked list integrity, exact mint | Organization |
| Membership Interval Add | MintingPolicy + MembershipsValidator | Org authorization, interval chain integrity, exact mint | Organization |
| Membership Acceptance | MembershipsValidator | Practitioner consent, datum update | Practitioner |

### Attack Prevention

| Attack Vector | Prevention Mechanism |
|--------------|---------------------|
| Invalid promotion creation | Full `validatePromotion` at mint time |
| Replay attacks | Seed TxOutRef must be spent (one-time use) |
| Token name collisions | `blake2b_224` hash-based deterministic naming from TxOutRef or composite keys |
| Unauthorized promotion | Master must spend their User NFT |
| Promoting Organizations | `getCurrentRankId` fails for profiles with no rank |
| Double-acceptance (same rank) | `nextBelt > currentBelt` check at acceptance |
| Out-of-order acceptance | `nextBeltDate > currentBeltDate` check at acceptance |
| Unauthorized acceptance | Student must spend User NFT (RanksValidator) |
| Other-token-name attacks | `mintValueMinted == exact expected tokens` (enforced in both MP and MV) |
| Oversized metadata | Per-field size limits enforced on-chain |
| Malicious ProtocolParams | MintingPolicy hash includes params; wrong params = orphaned profiles |
| Profile deletion | Not supported by design (immutability) |
| Unauthorized membership creation | Organization must spend their User NFT |
| Duplicate membership histories | Deterministic ID from org+practitioner; linked list ordering prevents duplicates |
| Cross-organization manipulation | `organizationId` field checked on all nodes in same transaction; sorted linked list enforces ordering |
| Membership interval overlap | New interval only allowed if head interval is closed (end date in past) and accepted |
| Unauthorized membership acceptance | Practitioner must spend their User NFT |
| Double membership acceptance | `acceptMembershipInterval` fails if `isAck` is already `True` |
| Foreign token injection | `hasCurrencySymbol` validates all referenced AssetClasses belong to the protocol |
| Datum type confusion at MV | `MembershipDatum` sum type ensures correct parsing; validator pattern-matches on constructor |

### Reference Input Usage

| Transaction | Reference Inputs | Purpose |
|-------------|-----------------|---------|
| Create Profile | None | - |
| Create Promotion | Student profile, student rank, master profile, master rank | Validate BJJ rules |
| Accept Promotion | Student's current rank | Validate state is still valid |
| Update Profile | None | User NFT sufficient for authorization |
| New Membership History | Right node (if insert-between) | Validate linked list ordering |
| New Membership Interval | Last (head) interval | Validate interval is closed and accepted |
| Accept Interval | None | Practitioner User NFT sufficient |

### Token Flow Summary

```
Profile Creation (Practitioner):
  Mint: ProfileRef + ProfileUser + RankState
  Lock: ProfileRef @ ProfilesValidator, RankState @ RanksValidator
  Send: ProfileUser to user

Profile Creation (Organization):
  Mint: ProfileRef + ProfileUser + MembershipHistoriesRoot
  Lock: ProfileRef @ ProfilesValidator, MHRoot (ListNodeDatum) @ MembershipsValidator
  Send: ProfileUser to user

Promotion Creation:
  Mint: PromotionToken
  Lock: PromotionToken @ RanksValidator

Promotion Acceptance:
  Spend: PromotionToken @ RanksValidator (transforms to RankState)
  Update: Profile datum @ ProfilesValidator

New Membership History (new member):
  Mint: MembershipHistoryToken + FirstIntervalToken
  Spend: LeftNode @ MembershipsValidator
  Lock: UpdatedLeftNode (ListNodeDatum) @ MembershipsValidator
  Lock: InsertedNode (ListNodeDatum) @ MembershipsValidator
  Lock: FirstInterval (IntervalDatum) @ MembershipsValidator

New Membership Interval (existing member):
  Mint: IntervalToken
  Spend: MembershipHistoryNode @ MembershipsValidator
  Ref: LastInterval @ MembershipsValidator
  Lock: UpdatedHistoryNode (ListNodeDatum) @ MembershipsValidator
  Lock: NewInterval (IntervalDatum) @ MembershipsValidator

Accept Membership Interval:
  Spend: UnacceptedInterval @ MembershipsValidator
  Lock: AcceptedInterval (IntervalDatum, isAck=True) @ MembershipsValidator
  No mint/burn (Variant A)
```


