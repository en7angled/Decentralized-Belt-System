# Developer Guide: Adding New Concepts to the Decentralized Belt System

> **Audience**: Developers extending this repository with new domain concepts (e.g., Achievements, Tournaments, Certifications).
>
> **Goal**: Ensure consistency across on-chain validators, off-chain transaction building, API endpoints, chain sync projections, and tests when adding new features.

---

- [Developer Guide: Adding New Concepts to the Decentralized Belt System](#developer-guide-adding-new-concepts-to-the-decentralized-belt-system)
  - [1. Architecture Overview](#1-architecture-overview)
    - [1.1 Library Stack](#11-library-stack)
    - [1.2 Executable Services](#12-executable-services)
    - [1.3 Data Flow](#13-data-flow)
  - [2. On-Chain Layer (`onchain-lib`)](#2-on-chain-layer-onchain-lib)
    - [2.1 Module Map](#21-module-map)
    - [2.2 Type Definition Pattern](#22-type-definition-pattern)
    - [2.3 Validator Pattern](#23-validator-pattern)
    - [2.4 Minting Policy Integration](#24-minting-policy-integration)
    - [2.5 ID Derivation](#25-id-derivation)
    - [2.6 Cross-Validator Communication](#26-cross-validator-communication)
    - [2.7 Oracle Integration](#27-oracle-integration)
  - [3. Off-Chain Layer (`offchain-lib`)](#3-off-chain-layer-offchain-lib)
    - [3.1 Module Map](#31-module-map)
    - [3.2 Transaction Building Pipeline](#32-transaction-building-pipeline)
    - [3.3 Key Abstractions](#33-key-abstractions)
    - [3.4 TxBuilding/Context — Deployed Scripts Context](#34-txbuildingcontext--deployed-scripts-context)
    - [3.5 TxBuilding/Skeletons — Composable Skeleton Builders](#35-txbuildingskeletons--composable-skeleton-builders)
    - [3.6 TxBuilding/Operations — Domain Operations](#36-txbuildingoperations--domain-operations)
    - [3.7 TxBuilding/Lookups — UTxO and State Lookups](#37-txbuildinglookups--utxo-and-state-lookups)
    - [3.8 TxBuilding/Interactions — User Intent Mapping](#38-txbuildinginteractions--user-intent-mapping)
    - [3.9 TxBuilding/Transactions — Execution and Submission](#39-txbuildingtransactions--execution-and-submission)
    - [3.10 TxBuilding/Conversions — Domain ↔ On-Chain Conversions](#310-txbuildingconversions--domain--on-chain-conversions)
    - [3.11 TxBuilding/Exceptions — Error Handling](#311-txbuildingexceptions--error-handling)
    - [3.12 TxBuilding/Validators — Compiled Script Access](#312-txbuildingvalidators--compiled-script-access)
    - [3.13 Storage — Persistent Projections](#313-storage--persistent-projections)
    - [3.14 Ingestion — Chain Event Projection](#314-ingestion--chain-event-projection)
    - [3.15 DomainTypes — Off-Chain Domain Model](#315-domaintypes--off-chain-domain-model)
  - [4. API Layer](#4-api-layer)
    - [4.1 Interaction API (Port 8082)](#41-interaction-api-port-8082)
    - [4.2 Query API (Port 8083)](#42-query-api-port-8083)
    - [4.3 Admin CLI](#43-admin-cli)
    - [4.4 Chain Sync Service (Port 8084)](#44-chain-sync-service-port-8084)
  - [5. Step-by-Step: Adding a New Concept](#5-step-by-step-adding-a-new-concept)
    - [Phase 1: On-Chain Types](#phase-1-on-chain-types)
    - [Phase 2: On-Chain Validator (if needed)](#phase-2-on-chain-validator-if-needed)
    - [Phase 3: Minting Policy Redeemer](#phase-3-minting-policy-redeemer)
    - [Phase 4: Off-Chain Domain Types](#phase-4-off-chain-domain-types)
    - [Phase 5: Off-Chain Transaction Building](#phase-5-off-chain-transaction-building)
    - [Phase 6: API Integration](#phase-6-api-integration)
    - [Phase 7: Chain Sync and Projections](#phase-7-chain-sync-and-projections)
    - [Phase 8: Admin CLI](#phase-8-admin-cli)
    - [Phase 9: Tests](#phase-9-tests)
    - [Phase 10: Deployment and Configuration](#phase-10-deployment-and-configuration)
  - [6. Consistency Checklist](#6-consistency-checklist)
  - [7. Worked Example: Adding "Achievements"](#7-worked-example-adding-achievements)
    - [Step 1: On-Chain Types](#step-1-on-chain-types)
    - [Step 2: On-Chain Validator](#step-2-on-chain-validator)
    - [Step 3: Minting Policy Redeemer](#step-3-minting-policy-redeemer)
    - [Step 4: Off-Chain Domain Types](#step-4-off-chain-domain-types)
    - [Step 5: Off-Chain Transaction Building](#step-5-off-chain-transaction-building)
    - [Step 6: API Integration](#step-6-api-integration)
    - [Step 7: Chain Sync Projection](#step-7-chain-sync-projection)
  - [8. Common Pitfalls](#8-common-pitfalls)
  - [9. File Reference Matrix](#9-file-reference-matrix)

---

## 1. Architecture Overview

### 1.1 Library Stack

```
┌─────────────────────────────────────────────────────┐
│                   webapi-lib                         │  Auth, CORS, Health probes
├─────────────────────────────────────────────────────┤
│                  offchain-lib                        │  Domain types, Tx building,
│  (depends on onchain-lib + chainsync-lib)            │  Storage, Ingestion
├──────────────────────┬──────────────────────────────┤
│    onchain-lib       │      chainsync-lib            │  Plutus validators   │ Kupo/Atlas sync
│  (PlutusTx only)     │   (Cardano API + Persistent)  │
└──────────────────────┴──────────────────────────────┘
```

| Library | Purpose | Key Constraint |
|---------|---------|----------------|
| `onchain-lib` | Plutus validators, minting policies, on-chain types | **No off-chain imports** (no Aeson, Servant, etc.) |
| `chainsync-lib` | Kupo ↔ Atlas match conversion, Kupo HTTP client | Network-facing only |
| `offchain-lib` | Tx building, lookups, domain types, storage, ingestion | Core business logic; depends on `onchain-lib` |
| `webapi-lib` | Auth, CORS middleware, health probes | Shared web infrastructure |

### 1.2 Executable Services

| Executable | Port | Purpose |
|-----------|------|---------|
| `interaction-api` | 8082 | Build and submit transactions (Servant REST) |
| `query-api` | 8083 | Query profiles, ranks, promotions (Servant REST) |
| `chainsync-service` | 8084 | Sync chain data → PostgreSQL projections |
| `admin` | CLI | Deploy scripts, manage oracle, run interactions |

### 1.3 Data Flow

```
User Wallet ──POST /build-tx──→ interaction-api
                                    │
                                    ├─ Deserializes Interaction (action + addresses)
                                    ├─ Maps action → Operation (TxBuilding/Operations)
                                    ├─ Operation builds GYTxSkeleton (composable Monoid)
                                    ├─ Skeleton → GYTxBody → hex CBOR
                                    └─ Returns unsigned tx to wallet for signing
                                    
User Wallet ──POST /submit-tx──→ interaction-api
                                    │
                                    └─ Adds witness, submits to chain

Cardano Chain ──Kupo──→ chainsync-service
                            │
                            ├─ Fetches new UTxO matches
                            ├─ Ingestion: projectChainEvent → domain events
                            ├─ Storage: upsert into PostgreSQL projections
                            └─ Rollback: delete beyond tip on reorgs

query-api ──reads──→ PostgreSQL (projected queries, fast)
query-api ──reads──→ Chain via GeniusYield (live queries, slower)
```

---

## 2. On-Chain Layer (`onchain-lib`)

### 2.1 Module Map

```
Onchain/
├── Protocol.hs              -- ProtocolParams, re-exports
├── Protocol/Types.hs        -- ProtocolParams, OracleParams, FeeConfig, OnchainProfile,
│                               OnchainRank, OnchainMembershipHistory, OnchainMembershipInterval,
│                               MembershipDatum, MembershipHistoriesListNode
├── Protocol/Id.hs           -- Deterministic ID derivation (deriveRankId, deriveMembershipHistoryId, etc.)
├── Protocol/Lookup.hs       -- ProtocolParams accessor helpers
├── BJJ.hs                   -- Belt types, promotion validation, time requirements
├── CIP68.hs                 -- CIP-68 datum structure, metadata helpers
├── LinkedList.hs            -- Generic sorted linked list (NodeDatum, insertion, validation)
├── Utils.hs                 -- Shared on-chain utilities (mkUntypedLambda, checkTxOutAtIndex, etc.)
├── MintingPolicy.hs         -- Main minting policy (parameterized by ProtocolParams)
├── OracleValidator.hs       -- Oracle UTxO guard (admin-gated)
├── OracleNFTPolicy.hs       -- One-shot oracle NFT minting
├── ProfilesValidator.hs     -- Profile updates and promotion acceptance
├── RanksValidator.hs        -- Promotion consumption with consent
├── MembershipsValidator.hs  -- Membership histories linked list and intervals
└── Blueprint.hs             -- CIP-57 blueprint generation
```

### 2.2 Type Definition Pattern

Every on-chain type follows this pattern:

```haskell
-- 1. Define the type with Haddock documentation
-- | Description of what this type represents on-chain.
data MyOnchainType = MyOnchainType
  { myField1 :: SomeType       -- ^ Field documentation
  , myField2 :: AnotherType    -- ^ Field documentation
  }

-- 2. Generate PlutusTx serialization with indexed constructors
makeIsDataSchemaIndexed ''MyOnchainType [('MyOnchainType, 0)]

-- 3. For sum types, index each constructor
data MyDatum = VariantA FieldsA | VariantB FieldsB
makeIsDataSchemaIndexed ''MyDatum [('VariantA, 0), ('VariantB, 1)]

-- 4. Add Blueprint definition for CIP-57
deriving via (PlutusTx.Blueprints.Schema.Def MyOnchainType) instance HasBlueprintDefinition MyOnchainType
```

**Rules:**
- All types go in `Onchain/Protocol/Types.hs`
- All ID derivation functions go in `Onchain/Protocol/Id.hs`
- Constructor indices must be stable (never reorder existing constructors)
- Use `makeIsDataSchemaIndexed` (not `unstableMakeIsData`)

### 2.3 Validator Pattern

```haskell
-- In Onchain/MyValidator.hs

-- 1. Define redeemer type (if the validator needs one)
data MyRedeemer
  = ActionA { fieldA :: Type, outputIdx :: Integer }
  | ActionB { fieldB :: Type }

makeIsDataSchemaIndexed ''MyRedeemer [('ActionA, 0), ('ActionB, 1)]

-- 2. Define the typed validator lambda
{-# INLINEABLE myValidatorLambda #-}
myValidatorLambda :: ScriptContext -> Bool
myValidatorLambda (ScriptContext txInfo (Redeemer rawRedeemer) (SpendingScript _ _maybeDatum)) =
  case unsafeFromBuiltinData rawRedeemer of
    ActionA {..} -> handleActionA txInfo fieldA outputIdx
    ActionB {..} -> handleActionB txInfo fieldB
myValidatorLambda _ = traceError "Wrong script purpose"

-- 3. Extract per-redeemer logic into helpers (keep validator under ~80 lines)
{-# INLINEABLE handleActionA #-}
handleActionA :: TxInfo -> Type -> Integer -> Bool
handleActionA txInfo fieldA outputIdx = ...

-- 4. Convert to untyped
myValidatorUntyped :: BuiltinData -> BuiltinUnit
myValidatorUntyped = mkUntypedLambda myValidatorLambda

-- 5. Compile
myValidatorCompile :: CompiledCode (BuiltinData -> BuiltinUnit)
myValidatorCompile = $$(compile [||myValidatorUntyped||])
```

**For parameterized validators:**

```haskell
{-# INLINEABLE myValidatorLambda #-}
myValidatorLambda :: MyParams -> ScriptContext -> Bool
myValidatorLambda params ctx = ...

myValidatorUntyped :: MyParams -> BuiltinData -> BuiltinUnit
myValidatorUntyped params = mkUntypedLambda (myValidatorLambda params)

myValidatorCompile :: MyParams -> CompiledCode (BuiltinData -> BuiltinUnit)
myValidatorCompile params =
  $$(compile [||myValidatorUntyped||])
    `unsafeApplyCode` liftCode plcVersion110 params
```

### 2.4 Minting Policy Integration

All tokens are minted through the **single MintingPolicy** (`Onchain/MintingPolicy.hs`). To add a new token type:

1. **Add a new redeemer constructor** to `MintingRedeemer` in `Onchain/MintingPolicy.hs`
2. **Add a handler** in `mintingPolicyLambda`'s case expression
3. **Validate** the exact tokens minted (`mintValueMinted == expected`)
4. **Read oracle params** via `readOracleParams` (for pause gate, fees, min lovelace)
5. **Check fees** via `checkFee` if applicable

```haskell
-- In MintingRedeemer, add:
| CreateAchievement TxOutRef ProfileId Text POSIXTime Integer
--                  seed     awardee   name date       outputIdx

-- In mintingPolicyLambda, add case:
CreateAchievement seedRef awardee name date outputIdx ->
  traceIfFalse "Must not be paused" (not $ opPaused oracle)
  && traceIfFalse "Must spend seed" (txOutRefIsSpent seedRef txInfo)
  && traceIfFalse "Must mint exactly achievement token" (mintValueMinted == expectedMint)
  && traceIfFalse "Must lock achievement at validator" (checkTxOutAtIndex outputIdx ...)
  && checkFee oracle fcSomeFee (txInfoOutputs txInfo)
```

### 2.5 ID Derivation

All token IDs are derived deterministically in `Onchain/Protocol/Id.hs`:

```haskell
-- Pattern: derive<ConceptId> :: inputs → TokenName
-- Uses blake2b_224 for 28-byte token names

{-# INLINEABLE deriveAchievementId #-}
deriveAchievementId :: ProfileId -> BuiltinByteString -> TokenName
deriveAchievementId profileId achievementName =
  TokenName $ blake2b_224 (unTokenName (snd (unAssetClass profileId)) <> achievementName)
```

**Rules:**
- IDs must be deterministic (same inputs → same ID)
- Use `blake2b_224` for 28-byte token names
- Use `integerToByteString` for encoding integers into the hash input
- Document the uniqueness guarantee in Haddock comments

### 2.6 Cross-Validator Communication

The system uses two patterns for cross-validator communication:

| Pattern | Used By | How It Works |
|---------|---------|-------------|
| **Compiled-in params** | MintingPolicy | `ProtocolParams` baked in at compile time |
| **Datum-carried params** | ProfilesValidator, RanksValidator | `protocolParams` field in datum → resolve addresses at runtime |

If your new concept needs to reference another validator's address:
- **Option A**: Embed `ProtocolParams` in the datum (like Profiles/Ranks)
- **Option B**: Use the MintingPolicy (which has `ProtocolParams` compiled in) to enforce output addresses

### 2.7 Oracle Integration

Every minting transaction reads the oracle via reference input:

```haskell
-- Read oracle params (in MintingPolicy or any tx that needs them)
let oracle = readOracleParams oracleToken (txInfoReferenceInputs txInfo)
    minLv  = Utils.minLovelaceValue  -- fixed constant, not from oracle

-- Check pause gate
traceIfFalse "Protocol is paused" (not $ opPaused oracle)

-- Check fee (if applicable)
checkFee oracle fcProfileCreationFee (txInfoOutputs txInfo)
```

**If your concept needs a new fee type**, add it to `FeeConfig` in `Onchain/Protocol/Types.hs`:

```haskell
data FeeConfig = FeeConfig
  { fcFeeAddress         :: Address
  , fcProfileCreationFee :: Integer
  , fcPromotionFee       :: Integer
  , fcMembershipFee      :: Integer
  , fcAchievementFee     :: Integer   -- NEW
  }
```

> **Warning**: Adding a field to `FeeConfig` changes the oracle datum format. Existing oracle UTxOs must be migrated or the system redeployed.

---

## 3. Off-Chain Layer (`offchain-lib`)

### 3.1 Module Map

```
offchain-lib/
├── Constants.hs                    -- File paths, version
├── Utils.hs                        -- JSON decoding, color strings
├── DomainTypes/
│   ├── Core/
│   │   ├── Types.hs               -- Profile, Rank, Promotion (off-chain domain model)
│   │   ├── Actions.hs             -- ProfileActionType, ProtocolActionType, AdminActionType, ProfileData
│   │   └── BJJ.hs                 -- BJJBelt with JSON/Swagger instances
│   └── Transfer/
│       └── Types.hs               -- PractitionerProfileInformation, OrganizationProfileInformation, MembershipHistoryInformation, MembershipIntervalInformation
├── Ingestion.hs                    -- Chain event → domain event projection
├── Storage.hs                      -- Persistent entities, PostgreSQL operations
└── TxBuilding/
    ├── Context.hs                  -- DeployedScriptsContext, ProviderCtx, runners
    ├── Exceptions.hs               -- TxBuildingException, HTTP status mapping
    ├── Conversions.hs                 -- Domain ↔ on-chain type conversions
    ├── Interactions.hs             -- Interaction type, action → skeleton mapping
    ├── Lookups.hs                  -- UTxO lookups, state extraction
    ├── Operations.hs               -- Domain tx operations (createProfileTX, promoteTX, etc.)
    ├── Skeletons.hs                -- Low-level skeleton builders (mint, lock, spend, etc.)
    ├── Transactions.hs             -- Tx body building, signing, submission
    ├── Utils.hs                    -- Conversions, datum parsing helpers
    └── Validators.hs               -- Compiled validator access, deployment
```

### 3.2 Transaction Building Pipeline

```
Interaction           →  interactionToTxSkeleton  →  GYTxSkeleton  →  GYTxBody  →  GYTx (hex CBOR)
(user intent)              (Interactions.hs)           (Monoid)        (Context)      (Transactions)
     │                           │                        │
     │                           ▼                        │
     │                    Operations.hs                   │
     │                    (domain logic)                  │
     │                           │                        │
     │                           ▼                        │
     │                    Skeletons.hs                    │
     │                    (composable builders)           │
     │                           │                        │
     │                           ▼                        │
     │                    Lookups.hs                      │
     │                    (UTxO queries)                  │
     │                                                    │
     └────────────────────────────────────────────────────┘
```

### 3.3 Key Abstractions

| Abstraction | Type | Purpose |
|-------------|------|---------|
| `GYTxSkeleton 'PlutusV3` | **Monoid** | Composable transaction components; combine with `mconcat` |
| `DeployedScriptsContext` | Record | Holds script hashes and reference script TxOutRefs |
| `ProviderCtx` | Record | Network configuration + Cardano providers |
| `Interaction` | Record | User intent = action + addresses + optional recipient |
| `TxBuildingException` | Sum type | Domain-specific errors with HTTP status mapping |

**Monad constraints used across the off-chain layer:**

| Constraint | What It Provides |
|-----------|------------------|
| `GYTxQueryMonad m` | UTxO lookups, slot/time queries |
| `GYTxUserQueryMonad m` | Extends `GYTxQueryMonad` with user-specific queries |
| `GYTxBuilderMonadIO` | Builds transaction bodies from skeletons |
| `MonadReader DeployedScriptsContext m` | Access to deployed script refs and hashes |
| `MonadError GYTxMonadException m` | Error handling with `throwError` |

### 3.4 TxBuilding/Context — Deployed Scripts Context

The `DeployedScriptsContext` is the central configuration for all off-chain transaction building. It holds the hash and reference script TxOutRef for each deployed validator.

```haskell
data DeployedScriptsContext = DeployedScriptsContext
  { mintingPolicyHashAndRef       :: (GYScriptHash, GYTxOutRef)
  , profilesValidatorHashAndRef   :: (GYScriptHash, GYTxOutRef)
  , ranksValidatorHashAndRef      :: (GYScriptHash, GYTxOutRef)
  , membershipsValidatorHashAndRef :: (GYScriptHash, GYTxOutRef)
  , oracleValidatorHashAndRef     :: (GYScriptHash, GYTxOutRef)
  , oracleNFTAssetClass           :: GYAssetClass
  }
```

**Config file requirement:** The file at `defaultTxBuldingContextFile` (`config/config_bjj_validators.json`) must include all six fields: `mintingPolicyHashAndRef`, `profilesValidatorHashAndRef`, `ranksValidatorHashAndRef`, `membershipsValidatorHashAndRef`, `oracleValidatorHashAndRef`, and `oracleNFTAssetClass`. Chain-sync and interaction-api load this file at startup; missing fields will cause parse failures. After deployment, replace any placeholder values with the actual script hashes, TxOutRefs, and oracle NFT asset class from the deployed scripts.

**When adding a new validator**, you must:
1. Add a new field: `myValidatorHashAndRef :: (GYScriptHash, GYTxOutRef)`
2. Add accessor helpers: `getMyValidatorRef`, `getMyValidatorHash`
3. Update JSON serialization (it derives `FromJSON`/`ToJSON` generically)
4. Update `config/config_bjj_validators.json` after deployment
5. Update `deployReferenceScripts` in `TxBuilding/Transactions.hs`

**Runners** (in the same module):

| Function | Purpose |
|----------|---------|
| `runQuery` | Run read-only queries (no tx building) |
| `runTx` | Build a transaction body from a skeleton |
| `runTx'` | Build a transaction body (simpler return type) |

### 3.5 TxBuilding/Skeletons — Composable Skeleton Builders

These are the **low-level building blocks** for constructing transactions. Each returns a `GYTxSkeleton 'PlutusV3` that you compose with `mconcat`.

**Minting:**

| Function | Purpose |
|----------|---------|
| `txMustMintWithMintRef` | Mint/burn tokens using a reference script |
| `txMustMintCIP68UserAndRef` | Mint a CIP-68 Ref + User NFT pair |
| `gyGenerateRefAndUserAC` | Generate CIP-68 asset classes from a seed TxOutRef |
| `gyDeriveUserFromRefAC` | Derive User NFT asset class from Ref NFT |

**Locking (outputs):**

| Function | Purpose |
|----------|---------|
| `txMustLockStateWithInlineDatumAndValue` | Lock state with inline datum at a validator |
| `txMustPayValueToAddress` | Pay value to an address (no datum) |

**Spending (inputs):**

| Function | Purpose |
|----------|---------|
| `txMustSpendStateFromRefScriptWithRedeemer` | Spend a UTxO at a validator using reference script |
| `txMustSpendFromRefScriptWithKnownDatum` | Spend with known datum (skip lookup) |
| `txMustSpendFromAddress` | Spend a User NFT from wallet addresses |

**Reference inputs:**

| Function | Purpose |
|----------|---------|
| `txMustHaveUTxOAsRefInput` | Add a UTxO as reference input |
| `txMustHaveUTxOsAsRefInputs` | Add multiple reference inputs |

**Deployment:**

| Function | Purpose |
|----------|---------|
| `addRefScriptSkeleton` | Deploy a script as a reference script |
| `addRefScriptToAddressSkeleton` | Deploy a script to a specific address |

### 3.6 TxBuilding/Operations — Domain Operations

Operations are **domain-level transaction builders**. Each one:
1. Looks up required state (via `Lookups`)
2. Computes new datums
3. Composes skeleton components (via `Skeletons`)
4. Returns a `GYTxSkeleton 'PlutusV3`

**Existing operations:**

| Operation | What It Does |
|-----------|-------------|
| `createProfileTX` | Creates a profile (mint CIP-68 pair, lock state) |
| `createProfileWithRankTX` | Creates profile + initial rank or membership root |
| `updateProfileTX` | Updates profile image URI |
| `promoteProfileTX` | Creates a pending promotion |
| `acceptPromotionTX` | Accepts a promotion (updates profile + rank) |
| `createMembershipHistoryTX` | Creates a membership history for a practitioner at an org (append to list, mint history + first interval) |
| `addMembershipIntervalTX` | Adds a new membership interval to an existing history |
| `acceptMembershipIntervalTX` | Practitioner accepts a membership interval |
| `updateOracleTX` | Admin updates oracle parameters |

**Standard operation signature:**

```haskell
myOperationTX ::
  (GYTxUserQueryMonad m, MonadReader DeployedScriptsContext m) =>
  Param1 -> Param2 -> [GYAddress] ->    -- domain params + user's addresses
  m (GYTxSkeleton 'PlutusV3)            -- returns composable skeleton
```

**Standard operation body:**

```haskell
myOperationTX param1 param2 ownAddrs = do
  -- 1. Get deployed script refs from context
  DeployedScriptsContext {..} <- ask
  let mpRef = snd mintingPolicyHashAndRef
      mvRef = snd myValidatorHashAndRef

  -- 2. Get oracle params (if minting)
  (oracleRefSkeleton, oracleParams) <- getOracleRefInputSkeleton
  let minLv = protocolMinLovelace  -- fixed constant in Operations.hs
  feeSkeleton <- getFeeSkeleton oracleParams fcSomeFee

  -- 3. Look up existing state
  (existingDatum, existingValue) <- getMyStateDataAndValue myRefAC

  -- 4. Compute new datum
  let newDatum = updateDatum existingDatum param1

  -- 5. Build skeleton components
  isMinting <- txMustMintWithMintRef mpRef redeemer tokensToBeMinted
  isSpending <- txMustSpendStateFromRefScriptWithRedeemer mvRef myRefAC spendRedeemer myValidatorGY
  isLocking <- txMustLockStateWithInlineDatumAndValue myValidatorGY newDatum newValue

  -- 6. Compose and return
  return $ mconcat
    [ isMinting
    , isSpending
    , isLocking
    , oracleRefSkeleton
    , feeSkeleton
    ]
```

**Important:** The order of components in `mconcat` determines **output indices**. On-chain validators verify outputs at specific indices passed in redeemers. Keep the order consistent between off-chain skeleton building and on-chain redeemer index values.

### 3.7 TxBuilding/Lookups — UTxO and State Lookups

Lookups find UTxOs on-chain and extract their datum/value.

**Core lookup:**

```haskell
getUTxOWithNFT :: (GYTxQueryMonad m) => GYAssetClass -> m GYUTxO
-- Finds the unique UTxO containing a specific NFT. Throws if not found or multiple found.
```

**State extraction pattern:**

```haskell
getMyStateDataAndValue ::
  (GYTxUserQueryMonad m, MonadReader DeployedScriptsContext m) =>
  GYAssetClass ->                          -- The Ref NFT identifying the state
  m (MyOnchainDatum, GYValue)              -- Parsed datum + value
getMyStateDataAndValue refAC = do
  utxo <- getUTxOWithNFT refAC
  (datum, value) <- getInlineDatumAndValue @MyOnchainDatum utxo
  return (datum, value)
```

**When adding lookups for a new concept:**
1. Add a `getMyStateDatumAndValue` function following the pattern above
2. Add a `getAllMyStates` function if needed for queries
3. Add a datum parser in `TxBuilding/Utils.hs`
4. Throw `TxBuildingException` on errors (not raw strings)

### 3.8 TxBuilding/Interactions — User Intent Mapping

The `Interaction` type represents a user's intent and maps it to a skeleton:

```haskell
data Interaction = Interaction
  { action        :: ActionType        -- What to do
  , userAddresses :: UserAddresses     -- Wallet addresses
  , recipient     :: Maybe GYAddress   -- Where to send unlocked funds
  }

data ActionType
  = ProfileAction ProfileActionType       -- User-facing actions
  | ProtocolAction ProtocolActionType     -- Permissionless protocol maintenance
  | AdminAction AdminActionType           -- Admin oracle actions

-- Core mapping function:
interactionToTxSkeleton ::
  (HasCallStack, GYTxUserQueryMonad m, MonadReader DeployedScriptsContext m) =>
  Interaction ->
  m (GYTxSkeleton 'PlutusV3, Maybe GYAssetClass)
```

**To add a new action type:**

1. Add constructor to `ProfileActionType` in `DomainTypes/Core/Actions.hs`
2. Add case in `interactionToTxSkeleton` in `TxBuilding/Interactions.hs`
3. The `Maybe GYAssetClass` in the return type is the newly created asset (if any)

### 3.9 TxBuilding/Transactions — Execution and Submission

| Function | Purpose |
|----------|---------|
| `interactionToTxBody` | `Interaction` → `GYTxBody` (for signing) |
| `interactionToUnsignedTx` | `Interaction` → `GYTx` (unsigned, for API response) |
| `interactionToHexEncodedCBOR` | `Interaction` → hex string (for wallet integration) |
| `submitTxAndWaitForConfirmation` | Sign + submit + wait |
| `deployReferenceScripts` | Full deployment flow (oracle, validators, minting policy) |

**When adding a new validator to the deployment flow**, update `deployReferenceScripts` to:
1. Compile and deploy the new validator as a reference script
2. Include the new script hash in `ProtocolParams` (if needed)
3. Update the `DeployedScriptsContext` construction

### 3.10 TxBuilding/Conversions — Domain ↔ On-Chain Conversions

Conversions convert between off-chain domain types and on-chain Plutus types:

| Function | Direction |
|----------|-----------|
| `profileDataToMetadataFields` | `ProfileData` → `MetadataFields` (off-chain → on-chain) |
| `metadataFieldsToProfileData` | `MetadataFields` → `ProfileData` (on-chain → off-chain) |
| `profileDatumToProfile` | `CIP68Datum OnchainProfile` → `Profile` |
| `onchainRankToRankInformation` | `OnchainRank` → `Maybe Rank` |
| `profileTypeToOnchainProfileType` | `ProfileType` → `OnchainProfileType` |
| `textToBuiltinByteString` | `Text` → `BuiltinByteString` |

**When adding a new concept:**
1. Add conversion functions in `TxBuilding/Conversions.hs`
2. Both directions: off-chain → on-chain (for building tx) and on-chain → off-chain (for queries/projections)

### 3.11 TxBuilding/Exceptions — Error Handling

```haskell
data TxBuildingException
  = ProfileNotFound | WrongProfileType
  | RankNotFound | RankListEmpty | WrongRankDataType
  | PromotionNotFound
  | OracleNotFound | OracleDatumInvalid | ProtocolPaused
  | ScriptNotFound | DeployedScriptsNotReady
  | InvalidAssetClass | MultipleUtxosFound | DatumParseError
  -- Add new exceptions here for your concept
```

**Key instances:**
- `Exception` — allows `throw`/`catch`
- `IsGYApiError` — integrates with GeniusYield error handling
- `txBuildingExceptionToHttpStatus` — maps to HTTP status codes

**When adding exceptions for a new concept:**
1. Add constructors (e.g., `AchievementNotFound`)
2. Update `txBuildingExceptionToHttpStatus` (usually 404 for "not found")
3. Update the `show` / `displayException` if you have custom messages

### 3.12 TxBuilding/Validators — Compiled Script Access

This module provides access to compiled validators and minting policies:

```haskell
-- Pre-compiled validators (no parameters needed)
profilesValidatorGY :: GYScript 'PlutusV3
ranksValidatorGY    :: GYScript 'PlutusV3

-- Parameterized compilation
compileMintingPolicy :: GYAssetClass -> GYScript 'PlutusV3
compileOracleNFTPolicy :: GYTxOutRef -> GYScript 'PlutusV3

-- Protocol params construction
mkProtocolParams :: GYAssetClass -> ProtocolParams
```

**When adding a new validator:**
1. Add a compiled validator accessor (e.g., `myValidatorGY :: GYScript 'PlutusV3`)
2. Add the script hash constant (e.g., `myValidatorHashGY :: GYScriptHash`)
3. If parameterized, add a compilation function
4. Update `mkProtocolParams` if the new validator hash needs to be in `ProtocolParams`
5. Update `exportValidators` for writing to files

### 3.13 Storage — Persistent Projections

Storage uses Persistent/PostgreSQL with Template Haskell entity definitions:

```haskell
-- Entity definition pattern (in Storage.hs)
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
MyProjection
    createdAtSlot    Integer
    createdAtHash    Text
    myId             GYAssetClass
    myField1         Text
    myField2         SomeType
    insertedAt       UTCTime
    UniqueMyProjection myId
    deriving Show
|]
```

**For each new projection, add:**
1. Entity definition in the `share` block
2. `putMyProjection :: MonadIO m => Integer -> Text -> MyDomainType -> SqlPersistT m ()`
3. Update `putMatchAndProjections` to handle the new event type
4. Update `rollbackTo` to delete the new projection on rollbacks

**Important**: The `UniqueMyProjection` constraint ensures upsert semantics. Always use `upsertByUnique` for idempotent writes.

**Membership interval organization backfill:** When a membership history projection is stored, Storage backfills `organizationProfileId` on any existing interval projections that (a) have the same practitioner and NULL org, and (b) belong to that history (same ID derivation as in `resolveOrganizationForInterval`). This handles the case where the interval event was processed before the history event (e.g. first membership created in one transaction).

### 3.14 Ingestion — Chain Event Projection

The ingestion module converts raw chain matches into domain events:

```haskell
data ChainEventProjection
  = RankEvent Rank
  | ProfileEvent Profile
  | PromotionEvent Promotion
  -- Add: | AchievementEvent Achievement
  | NoEvent AtlasMatch

projectChainEvent :: GYNetworkId -> AtlasMatch -> m ChainEventProjection
projectChainEvent nid am@AtlasMatch{..} =
  if isJust amSpentAt
    then return $ NoEvent am    -- Spent UTxOs are ignored
    else case amAddress of
      add | add == addressFromScriptHash nid ranksValidatorHashGY -> ...
      add | add == addressFromScriptHash nid profilesValidatorHashGY -> ...
      -- Add: add | add == addressFromScriptHash nid myValidatorHashGY -> ...
      _ -> return $ NoEvent am
```

**Pattern**: Match on validator address → parse datum → convert to domain type → return event.

### 3.15 DomainTypes — Off-Chain Domain Model

```
DomainTypes/
├── Core/
│   ├── Types.hs      -- Profile, Rank, Promotion (query results)
│   ├── Actions.hs    -- ProfileActionType, AdminActionType, ProfileData (API inputs)
│   └── BJJ.hs        -- BJJBelt type with JSON/Swagger/Persistent instances
└── Transfer/
    └── Types.hs      -- Composite types for API responses (PractitionerProfileInformation, OrganizationProfileInformation, MembershipHistoryInformation, MembershipIntervalInformation)
```

**When adding a new concept's domain types:**

1. **Core type** in `DomainTypes/Core/Types.hs`:
   ```haskell
   data Achievement = Achievement
     { achievementId :: GYAssetClass
     , achievementName :: Text
     , achievementProfileId :: ProfileRefAC
     , achievementDate :: GYTime
     }
     deriving (Generic)
     deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "achievement", CamelToSnake]] Achievement
   ```

2. **Action type** in `DomainTypes/Core/Actions.hs`:
   ```haskell
   | CreateAchievementAction
       { achievement_name :: Text
       , achievement_profile_id :: ProfileRefAC
       , achievement_date :: GYTime
       }
   ```

3. **JSON field naming convention**: Use `deriving-aeson` with `StripPrefix` + `CamelToSnake` for consistent `snake_case` JSON fields.

4. **Swagger schema**: Add `ToSchema` instance for API documentation.

---

## 4. API Layer

### 4.1 Interaction API (Port 8082)

**App Monad:**
```haskell
newtype InteractionAppMonad a = InteractionAppMonad (ReaderT InteractionAppContext Handler a)

data InteractionAppContext = InteractionAppContext
  { authContext       :: AuthContext           -- Basic Auth credentials
  , txBuildingContext :: TxBuildingContext      -- Providers + deployed scripts
  }
```

**Key pattern — error handling:**
```haskell
runWithTxErrorHandling :: InteractionAppMonad a -> InteractionAppMonad a
-- Catches TxBuildingException and maps to appropriate HTTP status codes
```

**Routes (`RestAPI.hs`):**
```haskell
type TransactionsAPI =
       "build-tx" :> ReqBody '[JSON] Interaction :> Post '[JSON] BuildTxResponse
  :<|> "submit-tx" :> ReqBody '[JSON] AddWitAndSubmitParams :> Post '[JSON] SubmitTxResponse
```

**No changes needed here** when adding a new concept — the `Interaction` type is polymorphic over `ActionType`. Adding a new action constructor to `ProfileActionType` automatically makes it available through the existing `/build-tx` endpoint.

### 4.2 Query API (Port 8083)

**App Monad:**
```haskell
newtype QueryAppMonad a = QueryAppMonad (ReaderT QueryAppContext Handler a)

data QueryAppContext = QueryAppContext
  { authContext     :: AuthContext
  , providerContext :: ProviderCtx          -- Cardano providers
  , pgPool          :: Pool SqlBackend      -- PostgreSQL pool
  }
```

**Query modules:**
- `Query.Projected` — SQL queries against PostgreSQL projections (fast)
- `Query.Live` — On-chain queries via GeniusYield providers (slower, always current)
- `Query.Common` — Shared filter types and utilities

**Membership endpoints and transfer types:**  
Membership history endpoints return **history plus its list of intervals**; interval endpoints include **organization id** so clients get the org without an extra lookup. The response types are the *Information* transfer types from `DomainTypes/Transfer/Types.hs`: `MembershipHistoryInformation` (history + `intervals` as `[MembershipIntervalInformation]`) and `MembershipIntervalInformation` (interval + `organization_id`). Implementors should use these types for enriched membership views.

**To add query endpoints for a new concept:**

1. Add route in `RestAPI.hs`:
   ```haskell
   type AchievementsAPI =
          "achievements" :> QueryParam "profile_id" GYAssetClass :> Get '[JSON] [Achievement]
     :<|> "achievements" :> "count" :> Get '[JSON] Integer
   ```

2. Add handler in `Query.Projected` or `Query.Live`

3. Wire into the server composition

### 4.3 Admin CLI

The admin CLI (`src/exe/admin/Main.hs`) uses `optparse-applicative`:

```haskell
data Command
  = DeployReferenceScripts DeployOpts
  | WriteBlueprint
  | PauseProtocol AdminOpts
  | UnpauseProtocol AdminOpts
  | SetFees SetFeesOpts
  | QueryOracle
  | InitProfile InitProfileOpts
  | ...
```

**To add admin commands for a new concept:**
1. Add a `Command` constructor
2. Add a parser (`Opt.command "my-command" ...`)
3. Add execution logic in the `main` case expression

### 4.4 Chain Sync Service (Port 8084)

The chain sync service (`src/exe/chain-sync/`) polls Kupo for new UTxOs matching the minting policy pattern and feeds them through the ingestion pipeline. Event order within a block is not guaranteed; for membership, Storage backfills interval `organizationProfileId` when the corresponding history is stored, so no separate ordering step is required for correctness.

**When adding a new concept:** If the new concept's tokens are minted by the existing `MintingPolicy`, the chain sync will automatically pick them up. You only need to:
1. Update `Ingestion.hs` to recognize the new datum type
2. Update `Storage.hs` to store the new projection
3. Update `rollbackTo` to handle rollbacks for the new projection

---

## 5. Step-by-Step: Adding a New Concept

### Phase 1: On-Chain Types

**File**: `src/lib/onchain-lib/Onchain/Protocol/Types.hs`

1. Define the on-chain datum type
2. Add `makeIsDataSchemaIndexed`
3. Add `HasBlueprintDefinition` deriving
4. If needed, define a redeemer type
5. If the concept has a sum-type wrapper (like `MembershipDatum`), define it

**File**: `src/lib/onchain-lib/Onchain/Protocol/Id.hs`

6. Add deterministic ID derivation function

### Phase 2: On-Chain Validator (if needed)

**File**: `src/lib/onchain-lib/Onchain/MyValidator.hs` (new file)

1. Define validator lambda
2. Convert to untyped
3. Compile

**File**: `src/lib/onchain-lib/Onchain/Protocol.hs`

4. Re-export from the protocol module

**File**: `Decentralized-Belt-System.cabal`

5. Add new module to `onchain-lib` exposed-modules

### Phase 3: Minting Policy Redeemer

**File**: `src/lib/onchain-lib/Onchain/MintingPolicy.hs`

1. Add redeemer constructor to `MintingRedeemer`
2. Add handler in `mintingPolicyLambda`
3. Validate exact mint, oracle params, fees, output placement

### Phase 4: Off-Chain Domain Types

**File**: `src/lib/offchain-lib/DomainTypes/Core/Types.hs`

1. Add domain type (with JSON/Swagger/Persistent instances)

**File**: `src/lib/offchain-lib/DomainTypes/Core/Actions.hs`

2. Add action type constructor to `ProfileActionType`

### Phase 5: Off-Chain Transaction Building

**File**: `src/lib/offchain-lib/TxBuilding/Conversions.hs`

1. Add domain ↔ on-chain conversion functions

**File**: `src/lib/offchain-lib/TxBuilding/Utils.hs`

2. Add datum parsing helpers

**File**: `src/lib/offchain-lib/TxBuilding/Lookups.hs`

3. Add UTxO lookup functions

**File**: `src/lib/offchain-lib/TxBuilding/Exceptions.hs`

4. Add error constructors

**File**: `src/lib/offchain-lib/TxBuilding/Operations.hs`

5. Add domain operation(s) (the main transaction builder)

**File**: `src/lib/offchain-lib/TxBuilding/Interactions.hs`

6. Map new action type to operation

**File**: `src/lib/offchain-lib/TxBuilding/Validators.hs`

7. Add compiled validator accessor (if new validator)

**File**: `src/lib/offchain-lib/TxBuilding/Context.hs`

8. Add field to `DeployedScriptsContext` (if new validator)

**File**: `src/lib/offchain-lib/TxBuilding/Transactions.hs`

9. Update `deployReferenceScripts` (if new validator)

### Phase 6: API Integration

**File**: `src/exe/interaction-api/RestAPI.hs`

1. Usually **no changes** — `/build-tx` accepts any `ActionType`

**File**: `src/exe/query-api/RestAPI.hs`

2. Add query endpoints
3. Add handler implementations
4. Wire into server

### Phase 7: Chain Sync and Projections

**File**: `src/lib/offchain-lib/Ingestion.hs`

1. Add event constructor to `ChainEventProjection`
2. Add case to `projectChainEvent`

**File**: `src/lib/offchain-lib/Storage.hs`

3. Add Persistent entity
4. Add `putMyProjection` function
5. Update `putMatchAndProjections`
6. Update `rollbackTo`

### Phase 8: Admin CLI

**File**: `src/exe/admin/Main.hs`

1. Add CLI command
2. Add execution logic

### Phase 9: Tests

**File**: `src/test/UnitTests.hs`

1. Add unit tests for the new concept
2. Test happy path + error cases

**File**: `src/test/TestRuns.hs`

3. Add test helpers if needed

### Phase 10: Deployment and Configuration

1. Update `deployReferenceScripts` to deploy new validator
2. Update `config/config_bjj_validators.json` format
3. Update `docs/OnchainArchitecture.md`
4. Update Swagger docs (auto-generated from `ToSchema` instances)
5. Update scripts in `scripts/` for testing

---

## 6. Consistency Checklist

Use this checklist when adding any new concept:

### On-Chain
- [ ] On-chain datum type defined in `Protocol/Types.hs`
- [ ] `makeIsDataSchemaIndexed` with stable constructor indices
- [ ] `HasBlueprintDefinition` instance
- [ ] ID derivation function in `Protocol/Id.hs`
- [ ] Redeemer type (if new validator)
- [ ] Validator follows the standard pattern (typed lambda → untyped → compile)
- [ ] Minting policy redeemer added (if tokens are minted)
- [ ] Exact mint check in minting policy
- [ ] Oracle pause gate checked
- [ ] Fee check added (if applicable)
- [ ] Output index optimization used (not O(n) search)
- [ ] `traceIfFalse` with descriptive error messages
- [ ] `{-# INLINEABLE #-}` on all cross-module functions
- [ ] Module added to cabal `exposed-modules`

### Off-Chain
- [ ] Domain type in `DomainTypes/Core/Types.hs` with JSON/Swagger instances
- [ ] Action type constructor in `DomainTypes/Core/Actions.hs`
- [ ] Conversion functions in `TxBuilding/Conversions.hs` (both directions)
- [ ] Datum parser in `TxBuilding/Utils.hs`
- [ ] Lookup functions in `TxBuilding/Lookups.hs`
- [ ] Error constructors in `TxBuilding/Exceptions.hs`
- [ ] Operation function in `TxBuilding/Operations.hs`
- [ ] Interaction mapping in `TxBuilding/Interactions.hs`
- [ ] Compiled validator accessor in `TxBuilding/Validators.hs` (if new validator)
- [ ] Context field in `TxBuilding/Context.hs` (if new validator)
- [ ] Deployment updated in `TxBuilding/Transactions.hs` (if new validator)
- [ ] Output index consistency between Operations and on-chain redeemers

### Storage & Ingestion
- [ ] Event type in `Ingestion.hs`
- [ ] Projection logic in `projectChainEvent`
- [ ] Persistent entity in `Storage.hs`
- [ ] Put function for new projection
- [ ] `putMatchAndProjections` updated
- [ ] `rollbackTo` handles new projection

### API & CLI
- [ ] Query endpoints in `query-api/RestAPI.hs` (if queryable)
- [ ] Query handlers (projected and/or live)
- [ ] Admin CLI command (if admin-controllable)
- [ ] Swagger metadata (Summary, Description)

### Tests & Docs
- [ ] Unit tests for new operations
- [ ] Property tests for new types (if applicable)
- [ ] `OnchainArchitecture.md` updated
- [ ] Blueprint regenerated (`admin write-blueprint`)
- [ ] Test scripts updated

## 7. Worked Example: Adding "Achievements"

Suppose we want to add **achievements** — badges that organizations can award to practitioners (e.g., "Tournament Winner", "Seminar Completed"). An achievement is a simple NFT locked at a new `AchievementsValidator`.

### Step 1: On-Chain Types

In `Onchain/Protocol/Types.hs`:

```haskell
-- | An achievement awarded to a practitioner by an organization.
data OnchainAchievement = OnchainAchievement
  { achievementId             :: AssetClass       -- ^ Unique achievement NFT
  , achievementName           :: BuiltinByteString -- ^ Achievement name
  , achievementDescription    :: BuiltinByteString -- ^ Achievement description
  , achievementAwardedTo      :: ProfileId         -- ^ Practitioner profile
  , achievementAwardedBy      :: ProfileId         -- ^ Organization profile
  , achievementDate           :: POSIXTime         -- ^ Date awarded
  , achievementProtocolParams :: ProtocolParams    -- ^ Protocol context
  }

makeIsDataSchemaIndexed ''OnchainAchievement [('OnchainAchievement, 0)]
```

In `Onchain/Protocol/Id.hs`:

```haskell
{-# INLINEABLE deriveAchievementId #-}
deriveAchievementId :: TxOutRef -> TokenName
deriveAchievementId (TxOutRef txId txIdx) =
  TokenName $ blake2b_224 (getTxId txId <> integerToByteString BigEndian 0 txIdx)
```

### Step 2: On-Chain Validator

In `Onchain/AchievementsValidator.hs`:

```haskell
data AchievementsRedeemer = RevokeAchievement { revokeOutputIdx :: Integer }
makeIsDataSchemaIndexed ''AchievementsRedeemer [('RevokeAchievement, 0)]

{-# INLINEABLE achievementsLambda #-}
achievementsLambda :: ScriptContext -> Bool
achievementsLambda (ScriptContext txInfo (Redeemer rawRedeemer) (SpendingScript _ maybeDatum)) =
  case unsafeFromBuiltinData rawRedeemer of
    RevokeAchievement {..} ->
      let achievement = getDatum maybeDatum
          orgUserAC = deriveUserFromRefAC (achievementAwardedBy achievement)
      in traceIfFalse "Organization must spend User NFT"
           (assetClassValueOf (valueSpent txInfo) orgUserAC == 1)
         && traceIfFalse "Must burn achievement token"
           (assetClassValueOf (mintValueMinted txInfo) (achievementId achievement) == -1)
achievementsLambda _ = traceError "Wrong script purpose"
```

### Step 3: Minting Policy Redeemer

In `Onchain/MintingPolicy.hs`, add to `MintingRedeemer`:

```haskell
| CreateAchievement
    TxOutRef           -- seed for unique ID
    ProfileId          -- practitioner (awarded to)
    ProfileId          -- organization (awarded by)
    BuiltinByteString  -- achievement name
    BuiltinByteString  -- achievement description
    POSIXTime          -- achievement date
    Integer            -- achievement output index
```

### Step 4: Off-Chain Domain Types

In `DomainTypes/Core/Types.hs`:

```haskell
data Achievement = Achievement
  { achievementId          :: GYAssetClass
  , achievementName        :: Text
  , achievementDescription :: Text
  , achievementAwardedTo   :: ProfileRefAC
  , achievementAwardedBy   :: ProfileRefAC
  , achievementDate        :: GYTime
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via CustomJSON '[FieldLabelModifier '[StripPrefix "achievement", CamelToSnake]] Achievement
```

In `DomainTypes/Core/Actions.hs`:

```haskell
| CreateAchievementAction
    { achievement_name        :: Text
    , achievement_description :: Text
    , practitioner_profile_id :: ProfileRefAC
    , achievement_date        :: GYTime
    }
```

### Step 5: Off-Chain Transaction Building

In `TxBuilding/Operations.hs`:

```haskell
createAchievementTX ::
  (GYTxUserQueryMonad m, MonadReader DeployedScriptsContext m) =>
  ProfileRefAC -> Text -> Text -> GYTime -> [GYAddress] ->
  m (GYTxSkeleton 'PlutusV3, GYAssetClass)
createAchievementTX practitionerId name desc date ownAddrs = do
  DeployedScriptsContext {..} <- ask
  -- 1. Oracle check
  (oracleRefSkeleton, oracleParams) <- getOracleRefInputSkeleton
  feeSkeleton <- getFeeSkeleton oracleParams fcAchievementFee
  -- 2. Find seed UTxO and derive ID
  -- 3. Build datum
  -- 4. Compose skeleton
  -- 5. Return (skeleton, newAchievementAC)
  ...
```

### Step 6: API Integration

In `DomainTypes/Core/Actions.hs`, the new `CreateAchievementAction` is already part of `ProfileActionType`, so it's automatically available via `/build-tx`.

For queries, add to `query-api/RestAPI.hs`:

```haskell
type AchievementsAPI =
       "achievements" :> QueryParam "profile_id" GYAssetClass :> Get '[JSON] [Achievement]
  :<|> "achievements" :> "count" :> Get '[JSON] Integer
```

### Step 7: Chain Sync Projection

In `Storage.hs`, add to the `share` block:

```haskell
AchievementProjection
    createdAtSlot    Integer
    createdAtHash    Text
    achievementId    GYAssetClass
    achievementName  Text
    achievementDescription Text
    awardedTo        GYAssetClass
    awardedBy        GYAssetClass
    achievementDate  GYTime
    insertedAt       UTCTime
    UniqueAchievementProjection achievementId
    deriving Show
```

---

## 8. Common Pitfalls

| Pitfall | Prevention |
|---------|-----------|
| **Output index mismatch** | The order in `mconcat` must match the indices in on-chain redeemers. Add a comment documenting the expected output layout. |
| **Missing exact mint check** | Every minting redeemer must validate `mintValueMinted == expectedTokens`. Without this, attackers can mint arbitrary tokens. |
| **Forgetting oracle reference** | All minting transactions must include the oracle as a reference input. Use `getOracleRefInputSkeleton`. |
| **Constructor index collision** | `makeIsDataSchemaIndexed` indices must be unique and stable. Never reorder existing constructors. |
| **Missing rollback handling** | Every new projection table needs a `deleteWhere` clause in `rollbackTo`. |
| **Inconsistent JSON field naming** | Always use `deriving-aeson` with `StripPrefix` + `CamelToSnake` for API types. |
| **Importing off-chain deps in on-chain** | `onchain-lib` must never import Aeson, Servant, Swagger, etc. Define Plutus instances in `onchain-lib`, off-chain instances in `offchain-lib`. |
| **Missing `{-# INLINEABLE #-}` pragma** | Every on-chain function used cross-module must have this pragma. Without it, PlutusTx may fail to compile. |
| **Forgetting to update ProtocolParams** | If a new validator hash needs to be known by other scripts, update `ProtocolParams` and redeploy the minting policy. |
| **Not checking `opPaused`** | If the new concept involves minting, the pause gate must be checked. |
| **Missing CurrencySymbol validation** | On-chain code should validate that referenced AssetClasses share the protocol's CurrencySymbol via `hasCurrencySymbol`. |
| **Datum type confusion** | If multiple datum types live at the same validator address, use a wrapper sum type (like `MembershipDatum`) to distinguish them. |

---

## 9. File Reference Matrix

Quick reference for which files to modify when adding different types of changes:

| Change | Files to Modify |
|--------|----------------|
| **New on-chain type** | `Protocol/Types.hs`, `Protocol/Id.hs`, `.cabal` |
| **New validator** | `Onchain/MyValidator.hs`, `Protocol.hs`, `Validators.hs`, `Context.hs`, `Transactions.hs`, `.cabal` |
| **New minting redeemer** | `MintingPolicy.hs` |
| **New off-chain domain type** | `DomainTypes/Core/Types.hs`, `DomainTypes/Core/Actions.hs` |
| **New tx operation** | `Operations.hs`, `Lookups.hs`, `Interactions.hs`, `Conversions.hs` |
| **New error type** | `Exceptions.hs` |
| **New query endpoint** | `query-api/RestAPI.hs`, `Query/Projected.hs` or `Query/Live.hs` |
| **New admin command** | `admin/Main.hs` |
| **New projection** | `Storage.hs`, `Ingestion.hs` |
| **New fee type** | `Protocol/Types.hs` (FeeConfig), `Operations.hs`, `MintingPolicy.hs` |
| **Updating oracle format** | `Protocol/Types.hs`, requires migration/redeployment |
| **New test** | `UnitTests.hs`, `TestRuns.hs` |
