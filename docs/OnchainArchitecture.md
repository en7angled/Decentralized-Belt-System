# Onchain Architecture Documentation


- [Onchain Architecture Documentation](#onchain-architecture-documentation)
  - [Overview](#overview)
    - [State NFT Token Name Derivation](#state-nft-token-name-derivation)
  - [Script Dependencies \& Parameterization](#script-dependencies--parameterization)
    - [Deployment Order](#deployment-order)
    - [Oracle NFT Policy: Inlined, Not Deployed](#oracle-nft-policy-inlined-not-deployed)
    - [ProtocolParams Structure](#protocolparams-structure)
    - [Why This Design?](#why-this-design)
    - [Security Implications](#security-implications)
    - [CurrencySymbol Inheritance](#currencysymbol-inheritance)
  - [Oracle Hub for Parameters](#oracle-hub-for-parameters)
    - [Core Design Decision](#core-design-decision)
    - [Architecture](#architecture)
    - [OracleParams](#oracleparams)
    - [OracleValidator](#oraclevalidator)
    - [OracleNFTPolicy](#oraclenftpolicy)
    - [How the MintingPolicy Uses the Oracle](#how-the-mintingpolicy-uses-the-oracle)
    - [Protocol Operational Parameters](#protocol-operational-parameters)
    - [Admin CLI Commands](#admin-cli-commands)
    - [Security Considerations](#security-considerations)
  - [Minting Policy](#minting-policy)
  - [Profiles Validator](#profiles-validator)
  - [Ranks Validator](#ranks-validator)
  - [Memberships Validator](#memberships-validator)
  - [Promotion Flow](#promotion-flow)
    - [Phase 1: Promotion Creation (Minting Policy)](#phase-1-promotion-creation-minting-policy)
    - [Phase 2: Promotion Acceptance (Profiles + Ranks Validators)](#phase-2-promotion-acceptance-profiles--ranks-validators)
  - [Membership Flow](#membership-flow)
    - [Requirements](#requirements)
    - [Phase 1: Organization Creates Membership (Minting Policy + Memberships Validator)](#phase-1-organization-creates-membership-minting-policy--memberships-validator)
      - [Case A: New Member (no existing membership history at this organization)](#case-a-new-member-no-existing-membership-history-at-this-organization)
      - [Case B: Existing Member (membership history already exists)](#case-b-existing-member-membership-history-already-exists)
    - [Phase 2: Practitioner Accepts Membership (Memberships Validator only)](#phase-2-practitioner-accepts-membership-memberships-validator-only)
    - [Phase 3: Update Membership End Date (Memberships Validator only)](#phase-3-update-membership-end-date-memberships-validator-only)
    - [Membership Token ID Derivation](#membership-token-id-derivation)
  - [BJJ Promotion Rules](#bjj-promotion-rules)
    - [Belt Hierarchy](#belt-hierarchy)
    - [Validation Rules](#validation-rules)
    - [Organization Handling](#organization-handling)
  - [Output Index Optimization](#output-index-optimization)
    - [How It Works](#how-it-works)
    - [Output Index Conventions](#output-index-conventions)
    - [Benefits](#benefits)
    - [Security](#security)
  - [Security Model](#security-model)
    - [Multi-Layer Validation](#multi-layer-validation)
    - [Attack Prevention](#attack-prevention)
    - [Reference Input Usage](#reference-input-usage)
    - [Token Flow Summary](#token-flow-summary)
  - [Achievements Validator](#achievements-validator)
    - [Achievement Data Model](#achievement-data-model)
    - [Achievement Redeemers](#achievement-redeemers)
    - [Security Properties](#security-properties)
  - [Achievement Flow](#achievement-flow)
  - [Membership Datum Optimization](#membership-datum-optimization)
    - [Principle: Derive, Don't Store](#principle-derive-dont-store)
    - [Fields Removed](#fields-removed)
    - [Why Not Rank Datums?](#why-not-rank-datums)
    - [Trade-offs](#trade-offs)
    - [Security Implications](#security-implications-1)
    - [Convenience Helpers](#convenience-helpers)


## Overview

The Decentralized BJJ Belt System implements a comprehensive smart contract architecture for managing Brazilian Jiu-Jitsu (BJJ) practitioner profiles, rank promotions, and organization memberships on the Cardano blockchain. 

The system consists of six main validators and an oracle hub that work together to manage the complete BJJ belt system:

1. **Oracle Validator** - Guards the oracle UTxO containing dynamic protocol parameters
2. **Oracle NFT Policy** - One-shot minting policy to create the unique oracle identifier
3. **Minting Policy** - Controls token creation and validates promotions/memberships/achievements at creation time; reads oracle parameters via reference input
4. **Profiles Validator** - Manages profile updates and validates promotion acceptance
5. **Ranks Validator** - Handles promotion consumption with consent validation
6. **Memberships Validator** - Manages membership histories and intervals for organization-practitioner relationships
7. **Achievements Validator** - Manages achievement NFTs awarded to practitioners, supporting acceptance and permissionless dust cleanup

**Immutability Principle**: Profiles are permanent by design. BJJ belt records are historical facts that should not be erasable, preserving lineage integrity and verification.

**Specific tokens**: 

- **Profile Ref Token**: NFT locked at Profiles Validator containing profile datum
- **Profile User Token**: NFT held by profile owner for authorization
- **Rank State Token**: NFT locked at Ranks Validator containing rank datum (can be an accepted Rank or pending Promotion)
- **Membership Histories Root Token**: NFT locked at Memberships Validator; root of the sorted linked list of membership histories for an organization
- **Membership History Token**: NFT locked at Memberships Validator; node in the membership histories linked list representing a practitioner's membership at an organization
- **Membership Interval Token**: NFT locked at Memberships Validator; represents a specific time period of membership

### State NFT Token Name Derivation

All state and user NFTs share the same minting policy (currency symbol). Token names are derived deterministically as below. `blake2b_224` yields 28 bytes; CIP-67 prefixes are 4 bytes.

| Token                                                    | Token name derivation                                                                               | CIP-67 prefix           | Size          |
| -------------------------------------------------------- | --------------------------------------------------------------------------------------------------- | ----------------------- | ------------- |
| **Profile Ref** (state at Profiles)                      | `refPrefix <> blake2b_224(txId \|\| integerToByteString(BigEndian, 0, txIdx))` from seed `TxOutRef` | Yes (ref `0x000643b0`)  | 32 B (4 + 28) |
| **Profile User** (held by owner)                         | `userPrefix <> blake2b_224(txId \|\| integerToByteString(BigEndian, 0, txIdx))` from same seed      | Yes (user `0x000de140`) | 32 B (4 + 28) |
| **First rank** (initial, with practitioner profile)      | `blake2b_224(profileRefTokenName \|\| integerToByteString(BigEndian, 0, rankNumber))`               | No                      | 28 B          |
| **Promotion / Accepted rank** (pending then accepted)    | `blake2b_224(txId \|\| integerToByteString(BigEndian, 0, txIdx))` from seed `TxOutRef`              | No                      | 28 B          |
| **Membership histories root** (list node at Memberships) | `blake2b_224(organizationProfileRefTokenName)`                                                      | No                      | 28 B          |
| **Membership history** (at Memberships)                  | `blake2b_224(orgProfileTokenName \|\| practitionerProfileTokenName)`                                | No                      | 28 B          |
| **Membership interval** (at Memberships)                 | `blake2b_224(membershipHistoryTokenName \|\| integerToByteString(BigEndian, 0, intervalNumber))`    | No                      | 28 B          |
| **Achievement** (state at Achievements)                  | `refPrefix <> blake2b_224(txId \|\| integerToByteString(BigEndian, 0, txIdx))` from seed `TxOutRef` | Yes (ref `0x000643b0`)  | 32 B (4 + 28) |


## Script Dependencies & Parameterization

The BJJ Belt System uses a carefully designed script dependency architecture that balances security, flexibility, and deployment complexity.

### Deployment Order

The **only strict ordering constraint** is that the main MintingPolicy is parameterized by the oracle NFT `AssetClass`, so the oracle NFT must exist before the MintingPolicy can be compiled and deployed. The order of deploying the other validators does not matter.

**Required sequence:**

1. **Deploy reference scripts** for the spending validators (Profiles, Ranks, Memberships, Achievements) and the Oracle Validator — in **any order**. All of these are unparameterized, so their script hashes are known at compile time.
2. **Mint the oracle NFT** in a single transaction that uses the Oracle NFT Policy **inlined** (see below). Lock the oracle NFT and initial `OracleParams` datum at the Oracle Validator address. The Oracle Validator must already be deployed (as ref script or otherwise) so that the lock output is valid.
3. **Compile and deploy the MintingPolicy** with the newly minted oracle NFT `AssetClass` in `ProtocolParams`, then deploy it as a reference script (or use it inlined; this codebase deploys it as ref script for subsequent minting transactions).

**Why this order:** The MintingPolicy is compiled with `mintingPolicyCompile (mkProtocolParams oracleAC)`; `oracleAC` is only known after the oracle NFT mint transaction is confirmed. No other script depends on another’s hash for deployment — only the MintingPolicy depends on the oracle token identity.

### Oracle NFT Policy: Inlined, Not Deployed

The **Oracle NFT Policy** (OracleNFTPolicy) is a one-shot minting policy parameterized by a seed `TxOutRef`. It is **never deployed as a reference script**. It is compiled on the fly for the single transaction that mints the oracle NFT and is attached to that transaction as an **inlined script** (e.g. `GYMintScript` in the off-chain code).

**Architecture decision — reasons for inlining rather than deploying:**

1. **One-shot, deployment-specific policy:** The policy is parameterized by the seed `TxOutRef` chosen at deployment time. Each deployment uses a different seed, so each deployment produces a **different script** (different currency symbol). There is no single “canonical” Oracle NFT Policy to reuse; the script is unique per protocol instance.
2. **No reuse:** The oracle NFT is minted exactly once per protocol instance. Deploying the policy as a reference script would consume UTxO space and add a deployment step for a script that is used in only one transaction and never again.
3. **Simplicity:** Inlining keeps the deployment flow to: (a) deploy the validators that need to be reused (including Oracle Validator), (b) run one mint transaction that carries the one-shot policy, (c) compile and deploy the main MintingPolicy with the resulting oracle `AssetClass`. The main MintingPolicy (used for all profile, rank, membership, and achievement minting) is the one deployed as a reference script for ongoing use.

### ProtocolParams Structure

```haskell
data ProtocolParams = ProtocolParams ScriptHash ScriptHash ScriptHash AssetClass
-- Fields: RanksValidatorHash, ProfilesValidatorHash, MembershipsValidatorHash, OracleToken
```

The `ProtocolParams` is embedded in:
- **MintingPolicy**: At compile time via `mintingPolicyCompile params`
- **OnchainProfile datum**: Via `protocolParams :: ProtocolParams` field
- **OnchainRank datum**: Via `rankProtocolParams` / `promotionProtocolParams` field

The `oracleToken` field (an `AssetClass`) is used by the MintingPolicy to locate the oracle UTxO in the transaction's reference inputs at runtime.

### Why This Design?

| Script               | Parameterized?                             | How It Gets Cross-Validator Addresses                              |
| -------------------- | ------------------------------------------ | ------------------------------------------------------------------ |
| MintingPolicy        | Yes (by ProtocolParams incl. oracle token) | Directly from compiled-in params; reads oracle via reference input |
| ProfilesValidator    | No                                         | From profile datum's `protocolParams`                              |
| RanksValidator       | No                                         | From rank datum's `promotionProtocolParams`                        |
| MembershipsValidator | No                                         | No cross-validator lookups needed (authorization via User NFTs)    |
| OracleValidator      | No                                         | N/A (guards oracle UTxO; admin-gated)                              |
| OracleNFTPolicy      | Yes (by seed TxOutRef)                     | N/A (one-shot minting; ensures oracle NFT uniqueness)              |

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


## Oracle Hub for Parameters

The Oracle Hub provides dynamic, updatable protocol parameters without requiring validator redeployment. It consists of two on-chain scripts and a datum type.

### Core Design Decision

Keep BJJ rules, belt hierarchy, and metadata limits hardcoded. Oracle-ize only things an operator would need to change at runtime: pause, fees, min lovelace, and admin identity.

**Rationale**: BJJ rules are domain invariants — if IBJJF changes belt requirements in 10 years, you deploy a new MintingPolicy (new `CurrencySymbol` = new protocol version). That's the right semantic boundary. But pausing the protocol or adjusting fees during an incident must happen in minutes, not hours of recompilation and redeployment.

### Architecture

```
OracleNFTPolicy (one-shot, parameterized by TxOutRef)
    ↓ mints unique NFT
OracleValidator (unparameterized, admin-gated)
    ↓ guards UTxO containing
OracleParams datum (inline datum with operational parameters)
    ↓ read via reference input by
MintingPolicy (has oracle AssetClass compiled in)
```

### OracleParams

```haskell
data OracleParams = OracleParams
  { opAdminPkh          :: PubKeyHash      -- admin who can update oracle
  , opPaused            :: Bool             -- global pause gate
  , opFeeConfig         :: Maybe FeeConfig  -- optional fee configuration
  , opMinUTxOValue      :: Integer          -- minimum lovelace for protocol state outputs (used on-chain; no fixed constant)
  }

data FeeConfig = FeeConfig
  { fcFeeAddress         :: Address   -- where fees are sent
  , fcProfileCreationFee :: Integer   -- fee for creating a profile
  , fcPromotionFee       :: Integer   -- fee for creating a promotion
  , fcMembershipFee      :: Integer   -- fee for membership operations
  }
```

### OracleValidator

Unparameterized spending validator. Rules:
- The admin (`opAdminPkh` from the **current** datum) must sign the transaction
- The oracle UTxO must be returned to the **same address** with value **≥** spent value (min-value check; the tx builder/balancer may add lovelace to script outputs)
- The new datum is accepted freely (allows updating all oracle parameters including admin key rotation)

### OracleNFTPolicy

One-shot minting policy parameterized by a `TxOutRef` (seed UTxO):
- Must spend the seed `TxOutRef` (ensures uniqueness across all time)
- Must mint exactly 1 token

The resulting `AssetClass` is embedded in `ProtocolParams` and used by the `MintingPolicy` to locate the oracle UTxO at runtime via `txInfoReferenceInputs`.

### How the MintingPolicy Uses the Oracle

At runtime, every minting transaction must include the oracle UTxO as a reference input. The MintingPolicy:

1. Reads `oracleToken` from its compiled-in `ProtocolParams`
2. Calls `readOracleParams oracleToken txInfoReferenceInputs` to find and parse the oracle datum
3. Checks `opPaused` — if `True`, the transaction fails with "Protocol is paused"
4. Calls `checkFee oracle feeSelector txInfoOutputs` per redeemer to validate fee payment (if fees are configured)

Minimum output lovelace for protocol state outputs comes **only** from the oracle: the MintingPolicy and MembershipsValidator use `opMinUTxOValue` from the oracle datum. There is no fixed constant in `Onchain.Utils` for min lovelace.

**Minimum lovelace**: On-chain, validators use only the oracle's `opMinUTxOValue`. Off-chain, the minimum lovelace for each state output can be computed as `max(oracleMinUTxOValue, max(ledgerMinUTxOValue, serializedOutputSize × coinsPerUtxoByte))` using node protocol parameters and the serialized size of the output; the oracle value acts as a floor.

**Output value checks**: When the MintingPolicy (or any validator) checks that a state output is locked, it uses **min-value** checks (output value **≥** expected) via `Onchain.Utils.checkTxOutAtIndexWithDatumMinValueAndAddress`, not exact equality. The actual locked value can exceed the minimum because (1) min-ADA depends on datum size and (2) the tx builder/balancer may add lovelace to script outputs. Continuing outputs (same validator, updated datum) also use min-value with the spent input's value for the same reason.

### Protocol Operational Parameters

| Parameter        | Effect                                      | Default               |
| ---------------- | ------------------------------------------- | --------------------- |
| `opAdminPkh`     | Who can update the oracle                   | Deployer's PubKeyHash |
| `opPaused`       | Blocks all minting operations when True     | False                 |
| `opFeeConfig`    | Optional per-action fee configuration       | Nothing (no fees)     |
| `opMinUTxOValue` | Minimum lovelace for protocol state outputs | e.g. 1_000_000        |

### Admin CLI Commands

| Command                                           | Description                                           |
| ------------------------------------------------- | ----------------------------------------------------- |
| `pause-protocol`                                  | Set `opPaused = True`                                 |
| `unpause-protocol`                                | Set `opPaused = False`                                |
| `set-fees --fee-address ADDR --profile-fee N ...` | Configure fees                                        |
| `set-fees --clear-fees`                           | Remove fee configuration                              |
| `set-min-utxo-value --lovelace N`                 | Set `opMinUTxOValue` (min lovelace for state outputs) |
| `query-oracle`                                    | Display current oracle parameters                     |

### Oracle datum schema and migration

`OracleParams` has four fields: `opAdminPkh`, `opPaused`, `opFeeConfig`, `opMinUTxOValue`. Existing oracle UTxOs created before this extension have a **3-field** datum (no `opMinUTxOValue`). New validators and off-chain code expect a **4-field** datum. For zero-downtime upgrades, use backward-compatible decoding when reading the oracle: if the datum has only 3 elements, treat `opMinUTxOValue` as a default (e.g. 10_000_000). Otherwise, the admin must update the oracle (e.g. run `set-min-utxo-value`) with new code that can write the 4-field datum; ensure the decoder can read both 3- and 4-field datums so the update transaction can succeed.

### Security Considerations

- The oracle NFT is unique (one-shot policy ensures exactly one exists)
- Only the current admin can update oracle parameters (signature check on `opAdminPkh`)
- Admin key rotation is possible (update `opAdminPkh` in the datum)
- The oracle UTxO value cannot be decreased (output must preserve address and value ≥ spent value; min-value check accommodates balancer-added lovelace)
- The MintingPolicy reads the oracle as a reference input (non-destructive; no contention)
- If fees are enabled, fee payment is validated on-chain in every minting transaction


## Minting Policy

**Purpose**: 
Governs the rules for issuing new profiles, ranks, promotions, and membership tokens. It is **parameterized by `ProtocolParams`** (containing ProfilesValidator, RanksValidator, MembershipsValidator script hashes, and the oracle NFT `AssetClass`), enabling secure cross-validator communication and dynamic parameter reading. At runtime, it reads `OracleParams` from the oracle UTxO (via reference input) to enforce the global pause gate, fee payments, and minimum output lovelace. It handles:
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
- `UpdateProfileImage ImageURI Integer` - Update the profile's image URI (newImageURI, profileOutputIdx). The profile identity is derived from the spent UTxO's datum, not passed as a redeemer parameter.
- `AcceptPromotion RankId Integer` - Accept a pending promotion (rankId, profileOutputIdx). Rank output validation is delegated to RanksValidator (R2 redundancy removed — see OnchainSecurityAudit.md).
- `Cleanup` - Permissionless dust cleanup: anyone can spend a UTxO at this address if its datum is absent or does not parse as a valid profile datum.

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
| Field         | Max Size   |
| ------------- | ---------- |
| `name`        | 128 bytes  |
| `description` | 1024 bytes |
| `imageURI`    | 256 bytes  |

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
4. Outputs correctly formed updated profile datum (rank output validation is delegated to RanksValidator — R2)

Note: User NFT consent is NOT checked here because RanksValidator always runs (Promotion UTxO must be spent) and guarantees consent. This avoids redundant validation.

**State Management**:
- Profile datums reference current rank via `currentRank :: Maybe RankId`
- Promotion acceptance atomically updates both profile datum (new `currentRank`) and transforms Promotion into Rank


## Ranks Validator

**Purpose**:
Handles the consumption of pending promotions with minimal validation. Since full BJJ rule validation happens at mint time in the Minting Policy, this validator is simplified to only verify consent.

**Redeemers**:
- `PromotionAcceptance Integer Integer` - Accept a pending promotion (profileOutputIdx, rankOutputIdx)
- `Cleanup` - Permissionless dust cleanup: anyone can spend a UTxO at this address if its datum is absent or does not parse as a valid rank datum.

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
3. `promoteProfile` transforms the Promotion into a Rank and updates the profile's `currentRank` (RV uses the combined function since it validates both outputs; PV uses the lightweight `promoteProfileDatum` which only computes the updated profile — see R4 optimization)
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

-- One per practitioner per organization.
-- The history NFT ID is derived at runtime: deriveMembershipHistoryId(orgId, practitionerId).
data OnchainMembershipHistory = OnchainMembershipHistory
  { membershipHistoryPractitionerId :: ProfileId
  , membershipHistoryOrganizationId :: ProfileId
  , membershipHistoryIntervalsHeadNumber :: Integer   -- sequential number of the head interval (0-based)
  }

-- Specific time period of membership.
-- The interval NFT ID is derived at runtime: deriveMembershipIntervalId(historyId, number).
data OnchainMembershipInterval = OnchainMembershipInterval
  { membershipIntervalStartDate :: POSIXTime
  , membershipIntervalEndDate :: Maybe POSIXTime     -- Nothing = open-ended (active)
  , membershipIntervalIsAck :: Bool                   -- True = accepted by practitioner
  , membershipIntervalNumber :: Integer               -- sequential number (0-based)
  , membershipIntervalPractitionerId :: ProfileId     -- for User NFT derivation in AcceptInterval
  }
```

> **Datum Optimization — Derive, Don't Store**: Membership datums omit fields that are deterministically derivable from other datum fields. See [Membership Datum Optimization](#membership-datum-optimization) for the full design rationale.

**Redeemers**:
- `InsertNodeToMHList { maybeRightNodeId, insertedMembershipHistory, updatedLeftNodeTxOutIdx, insertedNodeTxOutIdx }` - Insert a new membership history node into the sorted linked list when a new member joins an organization
- `UpdateNodeInMHList { lastIntervalId, startDate, endDate, updatedNodeTxOutIdx }` - Update a membership history node when adding a new interval to an existing member
- `AcceptInterval { updatedIntervalTxOutIdx }` - Accept a membership interval (practitioner consent, Variant A: same NFT, datum update only)
- `UpdateEndDate { membershipHistoryNodeId, newEndDate, updatedIntervalTxOutIdx }` - Update the end date of a membership interval (org: any future; practitioner: shorten/close accepted only)
- `Cleanup` - Permissionless dust cleanup: anyone can spend a UTxO at this address if its datum is absent or does not parse as a valid `MembershipDatum`.

**Design Decisions**:
- **Sorted Linked List**: Membership histories are stored in a sorted linked list (by `practitionerId`) per organization, enabling deterministic insertion and preventing duplicates
- **MembershipDatum Wrapper**: A sum type distinguishes between `ListNodeDatum` and `IntervalDatum` at the same validator address, ensuring correct datum parsing
- **Variant A Accept**: Membership interval acceptance updates the existing datum (`miIsAck = True`) without burning/minting new tokens, reducing transaction costs
- **Prepend-Only Intervals**: New intervals are prepended to the history's interval chain, with the head always being the most recent. This simplifies "is current interval closed?" checks.
- **Practitioner ID in Interval**: Stored directly in the interval datum to allow `AcceptInterval` to derive the User NFT without requiring a reference input to the membership history
- **Interval Number**: Sequential numbering enables deterministic interval ID derivation via `deriveMembershipIntervalId(historyId, number)`
- **Derive, Don't Store**: Membership datum fields that are deterministically derivable from other fields are omitted to reduce datum size and locked min-ADA. See [Membership Datum Optimization](#membership-datum-optimization)

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

**Datum–redeemer restrictions**: `ListNodeDatum` accepts only `InsertNodeToMHList` or `UpdateNodeInMHList`; `IntervalDatum` accepts only `AcceptInterval` or `UpdateEndDate`; `Cleanup` applies when the datum is absent or does not parse as a valid `MembershipDatum`.

**Security Considerations**:
- Tokens locked at the validator never leave (output checks guarantee same address + value ≥ spent value; min-value check used because balancer may add lovelace; off-chain locks with exact value from spent UTxO)
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

**Transaction involves one script**: `MembershipsValidator` (`UpdateEndDate`)

1. Organization or practitioner spends their User token (exactly one; MV enforces)
2. MV spends the active interval UTxO and references the membership history node (to derive org and validate interval belongs to history)
3. MV validates the new end date is within the transaction validity range (trace TD if outside)
4. **Organization**: may set any future end date. **Practitioner**: may only update **accepted** intervals (trace TE if unaccepted) and may only shorten or close (`newEndDate <= currentEndDate`); trace TB if they try to extend
5. MV locks the updated interval (wrapped in `IntervalDatum`) at the same address

**Off-chain build**: The transaction builder computes the output datum using `updateEndDateWithoutValidations` (interval with `membershipIntervalEndDate = Just newEndDate`) so that the requested update is always buildable. The validator enforces TD/TE/TB via `updateMembershipIntervalEndDate`; invalid attempts (e.g. practitioner extend or unaccepted interval) are submitted and rejected on-chain.

**Result**: Interval datum updated with `membershipIntervalEndDate = Just newEndDate`

### Membership Token ID Derivation

| Token                     | Derivation                                           | Purpose                                        |
| ------------------------- | ---------------------------------------------------- | ---------------------------------------------- |
| Membership Histories Root | `blake2b_224(profileRefTokenName)`                   | One per organization, created with org profile |
| Membership History        | `blake2b_224(orgTokenName ++ practitionerTokenName)` | One per org-practitioner pair                  |
| Membership Interval       | `blake2b_224(historyTokenName ++ intervalNumber)`    | Sequential per history                         |

All share the same `CurrencySymbol` as other protocol tokens.


## BJJ Promotion Rules

The system enforces authentic BJJ promotion rules on-chain via `validatePromotion` in `Onchain.BJJ`.

### Belt Hierarchy

| Index | Belt                     | More Than (months) |
| ----- | ------------------------ | ------------------ |
| 0     | White                    | -                  |
| 1     | Blue                     | 12 months          |
| 2     | Purple                   | 18 months          |
| 3     | Brown                    | 12 months          |
| 4     | Black                    | 12 months          |
| 5-10  | Black 1st-6th Degree     | 36-60 months       |
| 11    | Red & Black (7th Degree) | 84 months          |
| 12    | Red & White (8th Degree) | 84 months          |
| 13    | Red (9th Degree)         | 120 months         |
| 14    | Red 10th Degree          | -                  |

### Validation Rules

The `validatePromotion` function enforces:

1. **Master Belt Authority**: Only Black belts and above can promote
2. **Master Belt Restriction**: 1st Degree Black can only promote up to Brown (not to Black)
3. **Master Outranks Target**: Master's belt must be higher than student's target belt
4. **Date Ordering**: Master's belt date must precede the promotion date
5. **Sequential Promotion**: Target belt must be exactly one level above current belt
6. **Date Progression**: Promotion date must be after current belt date
7. **Time-in-Belt**: Student must have held current belt for more than the required duration (strict `>`)

### Organization Handling

Organizations (`profileType = Organization`) have `currentRank = Nothing`. They:
- Cannot receive promotions (no rank to promote from)
- Cannot be promoted by the system (getCurrentRankId fails for them)
- This is enforced by the MintingPolicy's reference input lookup failing


## Output Index Optimization

All validators use an **output index optimization** for efficient O(1) output validation instead of O(n) search.

### How It Works

Instead of searching through all transaction outputs to find a specific output (via `hasTxOutWithInlineDatumAndValue`), validators receive the expected output index directly in the redeemer and verify the output at that specific index. Two helpers in `Onchain.Utils` are used:

- **Min-value** (`checkTxOutAtIndexWithDatumMinValueAndAddress`): Require output value **≥** expected (same datum and address). Use for **continuing outputs** (validator spends a UTxO and locks updated state at the same address) and for **MintingPolicy** checks of newly created state outputs. The tx builder/balancer may add lovelace to script outputs; exact equality would cause validation failures after balancing. Off-chain still locks with the exact value from the spent UTxO where applicable.
- **Exact-value** (`checkTxOutAtIndexWithDatumValueAndAddress`): Require output value **=** expected. Use only when the output value is not modified by the balancer (rare for script outputs).

**Before** (O(n) search):
```haskell
hasTxOutWithInlineDatumAndValue datum value address txInfoOutputs
-- Searches all outputs until match found
```

**After** (O(1) indexed lookup):
```haskell
checkTxOutAtIndexWithDatumMinValueAndAddress outputIdx datum minValue address txInfoOutputs
-- or checkTxOutAtIndexWithDatumValueAndAddress for exact value when appropriate
-- Directly accesses output at specified index
```

### Output Index Conventions

Off-chain transaction builders track output indices and include them in redeemers. The order in the skeleton `mconcat` determines the output indices:

| Transaction                  | Output 0                             | Output 1         | Output 2       | Output 3 |
| ---------------------------- | ------------------------------------ | ---------------- | -------------- | -------- |
| CreateProfile (Practitioner) | Profile state                        | User NFT         | Rank state     | -        |
| CreateProfile (Organization) | Profile state                        | User NFT         | MH Root state  | -        |
| UpdateProfile                | Updated profile                      | -                | -              | -        |
| Promote                      | Pending rank                         | -                | -              | -        |
| AcceptPromotion              | Updated profile                      | Updated rank     | -              | -        |
| NewMembershipHistory         | Updated left node                    | Inserted MH node | First interval | -        |
| NewMembershipInterval        | Updated MH node                      | New interval     | -              | -        |
| AcceptInterval               | Updated interval                     | -                | -              | -        |
| UpdateEndDate                | Updated interval                     | -                | -              | -        |
| CleanupDust                  | (dust UTxOs consumed; ADA to caller) | -                | -              | -        |

### Benefits

| Aspect          | Before                      | After                          |
| --------------- | --------------------------- | ------------------------------ |
| Time complexity | O(n) per output check       | O(1) per output check          |
| Execution units | Higher (iteration overhead) | Lower (direct access)          |
| Script size     | Slightly smaller            | Slightly larger (index params) |

### Security

The output index is provided by the off-chain code but validated on-chain:
- If an incorrect index is provided, the output at that index won't match the expected datum/value/address
- The transaction will fail with a clear error message
- There's no security risk from malicious indices - they just cause validation failure


## Security Model

### Multi-Layer Validation

| Stage                   | Validator                            | What's Validated                                        | Who Pays     |
| ----------------------- | ------------------------------------ | ------------------------------------------------------- | ------------ |
| Promotion Creation      | MintingPolicy                        | Full BJJ rules, master authorization, uniqueness        | Master       |
| Promotion Acceptance    | ProfilesValidator + RanksValidator   | Consent, state validity                                 | Student      |
| Membership History Init | MintingPolicy + MembershipsValidator | Org authorization, linked list integrity, exact mint    | Organization |
| Membership Interval Add | MintingPolicy + MembershipsValidator | Org authorization, interval chain integrity, exact mint | Organization |
| Membership Acceptance   | MembershipsValidator                 | Practitioner consent, datum update                      | Practitioner |

### Attack Prevention

| Attack Vector                      | Prevention Mechanism                                                                                  |
| ---------------------------------- | ----------------------------------------------------------------------------------------------------- |
| Invalid promotion creation         | Full `validatePromotion` at mint time                                                                 |
| Replay attacks                     | Seed TxOutRef must be spent (one-time use)                                                            |
| Token name collisions              | `blake2b_224` hash-based deterministic naming from TxOutRef or composite keys                         |
| Unauthorized promotion             | Master must spend their User NFT                                                                      |
| Promoting Organizations            | `getCurrentRankId` fails for profiles with no rank                                                    |
| Double-acceptance (same rank)      | `nextBelt > currentBelt` check at acceptance                                                          |
| Out-of-order acceptance            | `nextBeltDate > currentBeltDate` check at acceptance                                                  |
| Unauthorized acceptance            | Student must spend User NFT (RanksValidator)                                                          |
| Other-token-name attacks           | `mintValueMinted == exact expected tokens` (enforced in both MP and MV)                               |
| Oversized metadata                 | Per-field size limits enforced on-chain                                                               |
| Malicious ProtocolParams           | MintingPolicy hash includes params; wrong params = orphaned profiles                                  |
| Profile deletion                   | Not supported by design (immutability)                                                                |
| Unauthorized membership creation   | Organization must spend their User NFT                                                                |
| Duplicate membership histories     | Deterministic ID from org+practitioner; linked list ordering prevents duplicates                      |
| Cross-organization manipulation    | `organizationId` field checked on all nodes in same transaction; sorted linked list enforces ordering |
| Membership interval overlap        | New interval only allowed if head interval is closed (end date in past) and accepted                  |
| Unauthorized membership acceptance | Practitioner must spend their User NFT                                                                |
| Double membership acceptance       | `acceptMembershipInterval` fails if `isAck` is already `True`                                         |
| Foreign token injection            | `hasCurrencySymbol` validates all referenced AssetClasses belong to the protocol                      |
| Datum type confusion at MV         | `MembershipDatum` sum type ensures correct parsing; validator pattern-matches on constructor          |
| Unauthorized oracle update         | `opAdminPkh` signature check; only current admin can modify oracle                                    |
| Oracle value theft                 | Oracle output must preserve same address + value ≥ spent value (min-value check)                      |
| Oracle NFT duplication             | One-shot minting policy (seed TxOutRef consumed) prevents duplicate NFTs                              |
| Missing oracle reference input     | `readOracleParams` fails if oracle NFT not found in reference inputs                                  |
| Protocol pause bypass              | MintingPolicy checks `opPaused` on every mint; cannot be skipped                                      |
| Fee evasion                        | `checkFee` validates fee payment output on-chain per redeemer                                         |

### Reference Input Usage

| Transaction             | Reference Inputs                                                        | Purpose                                  |
| ----------------------- | ----------------------------------------------------------------------- | ---------------------------------------- |
| Create Profile          | Oracle UTxO                                                             | Read pause gate, fees, minLovelace       |
| Create Promotion        | Student profile, student rank, master profile, master rank, Oracle UTxO | Validate BJJ rules + oracle params       |
| Accept Promotion        | Student's current rank                                                  | Validate state is still valid            |
| Update Profile          | None                                                                    | User NFT sufficient for authorization    |
| New Membership History  | Right node (if insert-between)                                          | Validate linked list ordering            |
| New Membership Interval | Last (head) interval                                                    | Validate interval is closed and accepted |
| Accept Interval         | None                                                                    | Practitioner User NFT sufficient         |

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

Achievement Award:
  Mint: AchievementRef NFT (CIP-68)
  Lock: AchievementRef @ AchievementsValidator (with CIP68Datum OnchainAchievement, isAccepted=False)
  Spend: Awarder's User NFT (authorization)

Achievement Accept:
  Spend: UnacceptedAchievement @ AchievementsValidator
  Lock: AcceptedAchievement (isAccepted=True) @ AchievementsValidator
  Spend: Practitioner's User NFT (consent)
  No mint/burn
```

## Achievements Validator

The Achievements Validator manages achievement NFTs that are awarded to practitioners by organizations or other practitioners. Achievements represent seminars, camps, competition medals, diplomas, and other recognitions.

### Achievement Data Model

Each achievement is a CIP-68 token locked at the Achievements Validator with an `OnchainAchievement` datum:

| Field                   | Type         | Description                                                    |
| ----------------------- | ------------ | -------------------------------------------------------------- |
| `achievementId`         | `AssetClass` | Unique identifier (derived from seed TxOutRef via blake2b_224) |
| `achievementAwardedTo`  | `ProfileId`  | Ref AC of the practitioner receiving the achievement           |
| `achievementAwardedBy`  | `ProfileId`  | Ref AC of the profile granting the achievement                 |
| `achievementDate`       | `POSIXTime`  | Date of the achievement (must be before tx validity range)     |
| `achievementIsAccepted` | `Bool`       | Whether the practitioner has acknowledged the achievement      |

### Achievement Redeemers

- **AcceptAchievement** `Integer` — The practitioner accepts the achievement. The `Integer` is the output index of the updated achievement UTxO. Requires spending the practitioner's User NFT.
- **Cleanup** — Permissionless dust removal for UTxOs that don't contain a valid achievement datum.

### Security Properties

1. **Award authorization**: The awarder must spend their User NFT (checked by MintingPolicy via trace `Mf`)
2. **Date validation**: Achievement date must be before the transaction's validity range (trace `Ml`)
3. **Metadata validation**: CIP-68 metadata fields must pass size constraints (trace `Mm`)
4. **Profile validation**: Both awardedTo and awardedBy must have the correct protocol currency symbol (trace `Mg`)
5. **Accept consent**: Only the practitioner (awardedTo) can accept by spending their User NFT (AchievementsValidator)
6. **Double-accept prevention**: An already-accepted achievement cannot be accepted again (trace `TA`)

## Achievement Flow

```
┌──────────────────┐        ┌──────────────────────────┐
│   Award          │        │   Accept                 │
│   Achievement    │        │   Achievement            │
│                  │        │                          │
│ MintingPolicy:   │        │ AchievementsValidator:   │
│  - Validate      │        │  - Validate practitioner │
│    awarder NFT   │        │    User NFT spent        │
│  - Validate date │        │  - Set isAccepted=True   │
│  - Validate meta │        │  - Lock updated datum    │
│  - Mint Ref NFT  │        │                          │
│  - Lock at AV    │        │                          │
└──────────────────┘        └──────────────────────────┘
```


## Membership Datum Optimization

### Principle: Derive, Don't Store

When an NFT's `AssetClass` (its on-chain identity) is deterministically derivable from other fields already present in the same datum, it should not be stored. Instead, validators and off-chain code derive it at runtime via the existing `blake2b_224`-based functions in `Onchain.Protocol.Id`.

This principle applies specifically to **membership datums**, where all token IDs are deterministic compositions of organization ID, practitioner ID, and sequential numbers. It does **not** apply to rank datums, where promotion IDs are seed-based (derived from a one-time `TxOutRef`) and therefore not recoverable from other datum fields.

### Fields Removed

| Datum                       | Removed / Changed Field                                                                                        | Derivation                                                                                                                                      |
| --------------------------- | -------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------- |
| `OnchainMembershipHistory`  | `membershipHistoryId :: MembershipHistoryId` (removed)                                                         | `deriveMembershipHistoryId(orgId, practitionerId)` — both fields remain in the datum                                                            |
| `OnchainMembershipHistory`  | `membershipHistoryIntervalsHeadId :: MembershipIntervalId` → `membershipHistoryIntervalsHeadNumber :: Integer` | `deriveMembershipIntervalId(historyId, headNumber)` — compose with the above                                                                    |
| `OnchainMembershipInterval` | `membershipIntervalId :: MembershipIntervalId` (removed)                                                       | `deriveMembershipIntervalId(historyId, intervalNumber)` — `intervalNumber` remains in the datum; `historyId` is derived from the parent history |
| `OnchainMembershipInterval` | `membershipIntervalPrevId :: Maybe MembershipIntervalId` (removed)                                             | Dead field: never read on-chain or off-chain. If needed: `number > 0 → Just(deriveMembershipIntervalId(historyId, number - 1))`                 |

### Why Not Rank Datums?

Rank IDs use **two different derivation schemes**:
- Initial rank (white belt): `deriveRankId(profileId, rankNumber)` — deterministic
- Promoted ranks: `derivePromotionRankId(seedTxOutRef, currencySymbol)` — seed-based, unique by construction

After any promotion, the rank's ID is seed-based and **not recoverable** from `(profileId, rankNumber)`. Therefore `currentRank :: Maybe RankId`, `rankId`, and `rankPreviousRankId` must remain as stored `AssetClass` values. Switching to deterministic promotion IDs was evaluated and rejected because it would (a) prevent concurrent promotions to the same belt level, (b) require burn-and-remint on acceptance adding a third script and oracle dependency, and (c) break the "tokens never leave the validator" invariant.

### Trade-offs

**Datum size savings** (~120 bytes per history, ~124 bytes per interval):

| Datum                       | Before                | After                 | Saving     |
| --------------------------- | --------------------- | --------------------- | ---------- |
| `OnchainMembershipHistory`  | 4 fields (~258 bytes) | 3 fields (~135 bytes) | ~121 bytes |
| `OnchainMembershipInterval` | 7 fields (~220 bytes) | 5 fields (~93 bytes)  | ~124 bytes |

At `coinsPerUTxOByte = 4,310`, this reduces the chain-required min-ADA by ~0.5 ADA per UTxO. For an organization with 50 members (50 history nodes + 50+ intervals), this saves 50–100 ADA of permanently locked capital.

**Additional hash computation cost** (1–4 extra `blake2b_224` calls per transaction):

| Transaction           | Extra hashes | Extra CPU steps | % of 10B limit | Extra ADA fee |
| --------------------- | ------------ | --------------- | -------------- | ------------- |
| NewMembershipHistory  | +2           | ~1,200,000      | 0.012%         | < 0.001       |
| NewMembershipInterval | +4           | ~3,400,000      | 0.034%         | < 0.001       |
| UpdateEndDate         | +1           | ~700,000        | 0.007%         | < 0.001       |
| AcceptInterval        | 0            | 0               | 0%             | 0             |

The datum size savings dominate: ~10x more saved in transaction size fees than spent on extra hashing, plus the locked capital reduction.

### Security Implications

**`validLastInterval` check** (in `addMembershipIntervalToHistory`): Previously compared stored head ID against stored interval ID. Now compares `membershipHistoryIntervalsHeadNumber` against `membershipIntervalNumber` — equivalent because both the history and interval are created by the MintingPolicy with consistent numbering. The caller (MintingPolicy or MembershipsValidator) derives the head interval ID and uses it for the lookup, ensuring the interval found is the actual head.

**`validRedeemerId` check** (new, in MembershipsValidator `handleUpdateNode`): The redeemer carries `lastIntervalId` (used for lookup). The validator now derives `deriveIntervalsHeadId(oldHistory)` and verifies it equals the redeemer-provided ID. This prevents cross-history attacks where an attacker provides an interval ID from a different history with a matching number.

**`intervalBelongsToHistory` check** (in MembershipsValidator `handleUpdateEndDate`): Previously compared the interval's stored ID against the derivation from the history's stored ID. Now derives the expected interval ID from the history's fields and verifies it exists in `ownValue` (the spending UTxO's token value). This is strictly **stronger** — it validates against the actual NFT locked in the UTxO rather than a datum field that could theoretically be inconsistent.

### Convenience Helpers

Two on-chain helper functions in `Onchain.Protocol.Id` simplify the derivation pattern:

```haskell
-- Derive the history NFT ID from its own datum fields
deriveMembershipHistoryIdFromHistory :: OnchainMembershipHistory -> MembershipHistoryId

-- Derive the head interval NFT ID from a history datum
deriveIntervalsHeadId :: OnchainMembershipHistory -> MembershipIntervalId
```

