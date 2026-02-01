# Onchain Architecture Documentation


- [Onchain Architecture Documentation](#onchain-architecture-documentation)
  - [Overview](#overview)
  - [Script Dependencies & Parameterization](#script-dependencies--parameterization)
  - [Minting Policy](#minting-policy)
  - [Profiles Validator](#profiles-validator)
  - [Ranks Validator](#ranks-validator)
  - [Promotion Flow](#promotion-flow)
  - [BJJ Promotion Rules](#bjj-promotion-rules)
  - [Security Model](#security-model)


## Overview

The Decentralized BJJ Belt System implements a comprehensive smart contract architecture for managing Brazilian Jiu-Jitsu (BJJ) practitioner profiles and rank promotions on the Cardano blockchain. 

The system consists of three main validators that work together to manage the complete BJJ belt system:

1. **Minting Policy** - Controls token creation and validates promotions at creation time
2. **Profiles Validator** - Manages profile updates and validates promotion acceptance
3. **Ranks Validator** - Handles promotion consumption with consent validation

**Immutability Principle**: Profiles are permanent by design. BJJ belt records are historical facts that should not be erasable, preserving lineage integrity and verification.

**Specific tokens**: 

- **Profile Ref Token**: NFT locked at Profiles Validator containing profile datum
- **Profile User Token**: NFT held by profile owner for authorization
- **Rank State Token**: NFT locked at Ranks Validator containing rank datum (can be an accepted Rank or pending Promotion)


## Script Dependencies & Parameterization

The BJJ Belt System uses a carefully designed script dependency architecture that balances security, flexibility, and deployment complexity.

### Deployment Order

Scripts must be deployed in a specific order due to hash dependencies:

```
1. ProfilesValidator (unparameterized) → hash known
2. RanksValidator (unparameterized) → hash known
3. MintingPolicy (parameterized by both hashes) → compiled with ProtocolParams
```

### ProtocolParams Structure

```haskell
newtype ProtocolParams = ProtocolParams (ScriptHash, ScriptHash)
-- Tuple: (RanksValidatorHash, ProfilesValidatorHash)
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
| RanksValidator | No | From rank datum (or not needed) |

**Benefits**:

1. **Single Point of Truth**: ProtocolParams defined once at deployment, then propagated through datums
2. **Unparameterized Validators**: ProfilesValidator and RanksValidator have fixed script hashes, making them reusable across protocol instances
3. **Self-Contained Datums**: Each profile/rank datum carries the protocol configuration, enabling cross-validator lookups at runtime
4. **Upgrade Path**: New MintingPolicy versions can be deployed without changing validator hashes

**Trade-offs**:

1. **Larger Datums**: Each datum carries ProtocolParams (~64 bytes for two script hashes)
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

All tokens (ProfileRef, ProfileUser, Rank, Promotion) share the same `CurrencySymbol` because they're minted by the same MintingPolicy. This enables:
- Easy identification of protocol tokens
- `deriveUserFromRefAC` works because Ref and User tokens share the CurrencySymbol
- ProfileRef and ProfileUser are distinguishable by CIP-68 prefixes (`0x000643b0` vs `0x000de140`)

Note: Rank and Promotion tokens are **not distinguishable by TokenName** - both are 28-byte `blake2b_224` hashes. They're distinguished by their **datum type** (`Rank` vs `Promotion` constructor) at the RanksValidator address.


## Minting Policy

**Purpose**: 
Governs the rules for issuing new profiles, ranks, and promotions. It is **parameterized by `ProtocolParams`** (containing ProfilesValidator and RanksValidator script hashes), enabling secure cross-validator communication. It handles:
- Profile creation with CIP-68 standard metadata 
- Initial rank assignment for practitioners
- **Promotion creation with full BJJ rule validation**

Note: Profile deletion is intentionally NOT supported to preserve lineage integrity.

**Redeemers**:
- `CreateProfile TxOutRef MetadataFields OnChainProfileType POSIXTime Integer Integer Integer` - Create a new profile (seedTxOutRef, metadata, profileType, creationDate, rankNumber, profileOutputIdx, rankOutputIdx). For Organization profiles, rankOutputIdx is ignored.
- `Promote TxOutRef ProfileId ProfileId POSIXTime Integer Integer` - Create a promotion (seedTxOutRef, studentProfileId, masterProfileId, achievementDate, rankNumber, pendingRankOutputIdx)

**Parameterization**:
```haskell
mintingPolicyCompile :: ProtocolParams -> CompiledCode (BuiltinData -> BuiltinUnit)
mintingPolicyCompile params = $$(compile [||mintingPolicyUntyped||]) `unsafeApplyCode` liftCode plcVersion110 params
```
The `ProtocolParams` is **baked into the compiled script** at deployment time. This means:
- The MintingPolicy's script hash depends on the validator hashes
- Each unique pair of validators produces a unique MintingPolicy hash (and thus unique CurrencySymbol)
- The MintingPolicy cannot be changed to point to different validators after deployment

**Design Decisions**:
- **Single Minting Authority**: All tokens minted through one policy ensures consistency
- **CIP-68 Standard Integration**: Leverages Cardano's CIP-68 standard for NFT metadata, ensuring interoperability with wallets and marketplaces while maintaining extensible metadata structure
- **Deterministic Token Generation**: Hash-based token naming using `blake2b_224` for 28-byte token names. `generateRankId` derives rank token names from profile ID and rank number; `generatePromotionRankId` uses seed TxOutRef for unique promotion IDs. Integer values are encoded efficiently using PlutusV3's `integerToByteString` builtin.
- **Embedded ProtocolParams**: The `ProtocolParams` is also embedded in every created profile and rank datum, enabling validators to resolve cross-validator addresses
- **Fail-Fast Promotion Validation**: Full BJJ rule validation at mint time prevents invalid promotions from being created
- **Metadata Size Enforcement**: `validateMetadataFields` enforces size limits at creation time

**Security Considerations**:
- Validates creation dates against transaction validity range
- Ensures seed TxOutRef is spent (prevents replay attacks and guarantees uniqueness)
- Enforces exact token minting via `mintValueMinted` (prevents other-token-name attacks)
- Requires spending of awarding authority's user token for promotions
- Validates metadata field sizes to prevent DoS via oversized datums

**Profile Creation Flow**:
1. User spends a seed UTxO which generates ProfileRef, ProfileUser token names (and Rank State Token Name of the first rank in case of practitioner profiles)
2. Creates initial profile datum with CIP-68 metadata and initial rank datum
3. Locks ProfileRef token with profile datum at Profiles Validator
4. Locks RankState Token with first rank datum at Ranks Validator (for practitioners)
5. User receives the User token

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

**Redeemer**: None required (implicit spending)

**Not Parameterized**: The RanksValidator is unparameterized. It extracts all needed information from the `OnchainRank` datum:
```haskell
let studentProfileId = promotionAwardedTo promotionRank
    profileUserAssetClass = deriveUserFromRefAC studentProfileId
```

**Design Decisions**:
- **Consent-Only Validation**: Only checks that the student spends their User token (consent to accept promotion)
- **Trust the Mint**: Since promotions are fully validated at creation time, no re-validation needed
- **Minimal Script Size**: Simplified logic reduces script size and execution costs
- **Datum-Driven**: All needed information (student profile ID) comes from the Promotion datum

**Validation Logic**:
```haskell
traceIfFalse "Must spend profile User NFT to accept promotion"
  $ V1.assetClassValueOf (valueSpent txInfo) profileUserAssetClass == 1
```
- Derives User NFT from the `promotionAwardedTo` field via `deriveUserFromRefAC`
- Student must spend their Profile User NFT to accept the promotion
- This proves consent and ownership

**Benefits of Simplified Design**:
| Aspect | Value |
|--------|-------|
| Reference inputs required | 0 |
| Script size | ~15 lines of logic |
| Execution units | Very low |
| Transaction fee for student | Lower |
| Datum access | Single field read |


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

| Transaction | Output 0 | Output 1 | Output 2 |
|-------------|----------|----------|----------|
| CreateProfile (Practitioner) | Profile state | User NFT | Rank state |
| CreateProfile (Organization) | Profile state | User NFT | - |
| UpdateProfile | Updated profile | - | - |
| Promote | Pending rank | - | - |
| AcceptPromotion | Updated profile | Updated rank | - |

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

### Two-Layer Validation

| Stage | Validator | What's Validated | Who Pays |
|-------|-----------|-----------------|----------|
| Creation | MintingPolicy | Full BJJ rules, master authorization, uniqueness | Master |
| Acceptance | ProfilesValidator + RanksValidator | Consent, state validity | Student |

### Attack Prevention

| Attack Vector | Prevention Mechanism |
|--------------|---------------------|
| Invalid promotion creation | Full `validatePromotion` at mint time |
| Replay attacks | Seed TxOutRef must be spent (one-time use) |
| Token name collisions | `blake2b_224` hash-based deterministic naming from TxOutRef |
| Unauthorized promotion | Master must spend their User NFT |
| Promoting Organizations | `getCurrentRankId` fails for profiles with no rank |
| Double-acceptance (same rank) | `nextBelt > currentBelt` check at acceptance |
| Out-of-order acceptance | `nextBeltDate > currentBeltDate` check at acceptance |
| Unauthorized acceptance | Student must spend User NFT (RanksValidator) |
| Other-token-name attacks | `mintValueMinted == exact expected tokens` |
| Oversized metadata | Per-field size limits enforced on-chain |
| Malicious ProtocolParams | MintingPolicy hash includes params; wrong params = orphaned profiles |
| Profile deletion | Not supported by design (immutability) |

### Reference Input Usage

| Transaction | Reference Inputs | Purpose |
|-------------|-----------------|---------|
| Create Profile | None | - |
| Create Promotion | Student profile, student rank, master profile, master rank | Validate BJJ rules |
| Accept Promotion | Student's current rank | Validate state is still valid |
| Update Profile | None | User NFT sufficient for authorization |

### Token Flow Summary

```
Profile Creation (Practitioner):
  Mint: ProfileRef + ProfileUser + RankState
  Lock: ProfileRef @ ProfilesValidator, RankState @ RanksValidator
  Send: ProfileUser to user

Profile Creation (Organization):
  Mint: ProfileRef + ProfileUser
  Lock: ProfileRef @ ProfilesValidator
  Send: ProfileUser to user

Promotion Creation:
  Mint: PromotionToken
  Lock: PromotionToken @ RanksValidator

Promotion Acceptance:
  Spend: PromotionToken @ RanksValidator (transforms to RankState)
  Update: Profile datum @ ProfilesValidator
```


