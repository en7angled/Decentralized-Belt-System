# Revision history for Decentralized-Belt-System


## 0.2.6.0 -- 2026-02-01

### PlutusV3 Optimizations

#### Token ID Generation Optimization

Improved efficiency of token ID generation functions by leveraging PlutusV3-native builtins.

**Changed Functions**:
- `nameFromTxOutRef` in `Onchain.Utils`
- `generateRankId` in `Onchain.Protocol`

**Before** (PlutusV2 workaround):
```haskell
takeByteString 28 $ blake2b_256 (bs <> (serialiseData . toBuiltinData) i)
```

**After** (PlutusV3 native):
```haskell
blake2b_224 (bs <> integerToByteString BigEndian 0 i)
```

**Benefits**:
| Aspect | Old Approach | New Approach |
|--------|-------------|--------------|
| Hash function | `blake2b_256` + truncate | `blake2b_224` (native 28-byte) |
| Integer encoding | CBOR via `serialiseData . toBuiltinData` | Raw bytes via `integerToByteString` |
| Overhead | Two operations + truncation | Single builtin each |
| Execution units | Higher (extra computation discarded) | Lower (direct output) |

**Note**: This is a **breaking change** for token name generation. Existing on-chain tokens are not affected, but new tokens will have different names for the same inputs compared to the old algorithm.

---

### Testnet Scripts & Documentation

#### New Testnet Population Script

Added `scripts/populate_testnet.sh` - a comprehensive script to populate the testnet with realistic sample data for testing and demonstrations.

**Profiles Created**:
| Type | Name | Belt Level |
|------|------|------------|
| Organization | Gracie Barra Academy | N/A |
| Organization | Alliance Jiu-Jitsu | N/A |
| Grand Master | Grand Master Helio | Red (9th degree) |
| Master | Master Ricardo Silva | Black → Black1 |
| Master | Master Ana Santos | Black |
| Student | John Martinez | White → Blue → Purple |
| Student | Maria Garcia | White → Blue |
| Student | Carlos Oliveira | White |
| Student | Emma Thompson | White |

**Promotion Scenarios**:
- John's Journey: White → Blue → Purple (promoted by Master Ricardo)
- Maria's Progress: White → Blue (promoted by Master Ana)
- Master Degree: Black → Black1 (Master Ricardo promoted by Grand Master)

#### Script Improvements

Both test scripts (`populate_testnet.sh` and `test_black_promotes_white_to_blue.sh`) now include:
- **Helper functions** with proper error handling and output suppression
- **Error detection** for common issues (missing reference scripts, deployment failures)
- **Clean output** - verbose CLI messages are suppressed, only relevant info shown
- **Colored progress indicators** with section headers
- **Detailed summaries** with all created profile/promotion IDs

#### Documentation Updates

- **README.md**: Updated CLI help (removed deprecated `delete-profile` command), added testnet scripts section
- **docs/Documentation.md**: Updated backend component descriptions
- **docs/OnchainArchitecture.md**: Already up-to-date from v0.2.5.0

---

## 0.2.5.0 -- 2026-02-01

### Security Review & Architectural Changes

This release addresses issues discovered during a comprehensive security review and implements important architectural decisions.

#### Profile Deletion Removed (Breaking Change)

**Decision**: Profile deletion functionality has been intentionally removed from the protocol.

**Rationale**:
- BJJ belt records are permanent historical facts that should not be erasable
- Lineage integrity requires that promotion relationships remain verifiable
- If a master deletes their profile, all promotions they granted become unverifiable
- Immutability aligns with blockchain's core value proposition

**Changes**:
- Removed `DeleteProfile` redeemer from `ProfilesValidator`
- Removed `BurnProfileId` redeemer from `MintingPolicy`
- Removed `DeleteProfileAction` from offchain API
- Removed `deleteProfileTX` from transaction builders

#### ProfilesValidator Optimization

**AcceptPromotion - Redundant Check Removed**
- **Original Issue**: Validator checked `promotionAwardedTo` (Ref AC) instead of deriving User AC
- **Analysis**: RanksValidator already correctly checks User NFT consent via `deriveUserFromRefAC`
- **Resolution**: Removed redundant check since RanksValidator guarantees user consent
- **Rationale**: AcceptPromotion must spend Promotion UTxO → RanksValidator always runs → consent verified
- **Benefit**: Smaller script size, lower execution costs

#### Metadata Size Limits (CIP68.hs)

**Unbounded Metadata Vulnerability**
- **Bug**: No size limits on CIP68 metadata fields allowed DoS via oversized datums
- **Fix**: Added per-field size validation

| Field | Max Size | Rationale |
|-------|----------|-----------|
| `name` | 128 bytes | Full names with titles, accents, academy |
| `description` | 1024 bytes | Detailed bio with achievements and lineage |
| `imageURI` | 256 bytes | IPFS/Arweave/HTTPS URLs |

- **Applied to**: `MintingPolicy` (CreateProfile) and `ProfilesValidator` (UpdateProfileImage)

#### New Validation Functions
- `validateMetadataFields` - Validates all metadata fields for profile creation
- `validateImageURI` - Validates image URI size for profile updates

#### Tests
- **Malicious AcceptPromotion Test**: Constructs attack transaction without User NFT to verify RanksValidator protection

#### Summary

| Change | Type | Impact |
|--------|------|--------|
| Profile deletion removed | Breaking | Profiles are now permanent (by design) |
| Metadata size limits | Security | Prevents DoS via oversized datums |
| AcceptPromotion optimized | Optimization | Smaller script, lower costs |


## 0.2.4.0 -- 2025-02-01

### Promotion Security Enhancements

**Comprehensive Promotion Validation Overhaul**

This release significantly improves the security and robustness of the promotion system by implementing a two-layer validation architecture.

#### Minting Policy Enhancements
- **Full BJJ Rule Validation at Mint Time**: Promotions are now fully validated when created, not just when accepted
- **Seed TxOutRef for Uniqueness**: Each promotion now uses a consumed TxOutRef as a seed, guaranteeing unique promotion IDs
- **Reference Input Validation**: Minting policy now references student and master profiles/ranks to validate promotion rules
- **New `generatePromotionRankId` Function**: Creates unique promotion IDs from seed TxOutRef

#### ProfilesValidator Enhancements
- **Acceptance-Time State Validation**: Added checks to prevent invalid acceptance scenarios:
  - `nextBelt > currentBelt` - Prevents double-acceptance of same-rank promotions
  - `nextBeltDate > currentBeltDate` - Prevents out-of-order date acceptance
- **Current Rank Reference Input**: Now references the student's current rank to validate state

#### RanksValidator Simplification
- **Consent-Only Validation**: Simplified to only verify student spends their User NFT
- **Reduced Script Size**: Removed redundant validation (now done at mint time)
- **Lower Execution Costs**: Fewer reference inputs and simpler logic

#### Protocol Changes
- **Updated `Promote` Redeemer**: Now includes `TxOutRef` seed parameter for uniqueness
- **Updated `mkPendingRank`**: Now takes seed TxOutRef and CurrencySymbol parameters
- **New `generatePromotionRankId`**: Generates unique promotion IDs from seed

#### Security Improvements
| Attack Vector | Prevention Mechanism |
|--------------|---------------------|
| Invalid promotion creation | Full validation at mint time |
| Token ID collisions | Seed TxOutRef guarantees uniqueness |
| Double-acceptance (same rank) | `nextBelt > currentBelt` check |
| Out-of-order acceptance | `nextBeltDate > currentBeltDate` check |

#### Documentation Updates
- **OnchainArchitecture.md**: Complete rewrite with new security model documentation
- **New Sections**: Promotion Flow, Security Model, Attack Prevention tables

#### Test Updates
- **New Security Tests**: Added tests for multiple master promotion scenarios
- **Sequential Promotion Tests**: Verify correct promotion flow with date ordering


## 0.2.3.0 -- 2025-01-06

### Query API Enhancements

**Extended Profiles and Promotions APIs with Count and Frequency Endpoints**

#### Profiles API Extensions
- **GET /profiles/count**: Get total count of profiles with optional `profile_type` filter
- **GET /profiles/frequency**: Get profile counts grouped by type (`Practitioner`, `Organization`)

#### Promotions API Extensions  
- **GET /promotions/count**: Get total count of promotions with optional filters (`profile`, `belt`, `achieved_by`, `awarded_by`)
- **GET /promotions/frequency**: Get promotion counts grouped by belt type

#### Implementation Details
- Added `getProfileTypeTotals` to `Query/Projected.hs` and `Query/Live.hs`
- Added `getPromotionBeltTotals` to `Query/Projected.hs` and `Query/Live.hs`
- All new endpoints support `liveprojection` query flag for live blockchain vs projected database queries
- Follows the same pattern as existing Belts API Count and Frequency endpoints

## 0.2.2.0 -- 2024-12-21

### API Architecture Split

**Service Separation for Better Scalability**

#### API Split Implementation
- **Interaction API Service**: Dedicated service for transaction building and submission (`build-tx`, `submit-tx`)
- **Query API Service**: Dedicated service for data queries (profiles, promotions, belts)
- **Independent Deployment**: Each service can be deployed and scaled independently
- **Docker Support**: Separate Dockerfiles for each service (`Dockerfile.interaction-api`, `Dockerfile.query-api`)

#### Infrastructure Updates
- **Nginx Configuration**: Updated routing to direct requests to appropriate services
- **Docker Compose**: Updated to orchestrate both services independently
- **Build Scripts**: Updated to build and deploy both services separately

#### Documentation Updates
- **Architecture Documentation**: Updated to reflect split service architecture
- **API Documentation**: Service-specific Swagger documentation for each API

## 0.2.1.0 -- 2024-12-20

### Milestone 2 Enhancements & Feedback Response

**Additional Evidence and Improvements**

#### Testnet Deployment Evidence
- **Testnet Transactions**: `test_evidence_preview.txt` - Successful testnet transaction execution
- **Automated Test Script**: `test_black_promotes_white_to_blue.sh` - Self-service test script for reviewers

#### Documentation Updates
- **Enhanced README**: Complete installation, setup, and usage instructions


#### Admin Command Line Tools
- **Deployment Commands**: admin cli


## 0.2.0.0 -- 2024-12-19

### Milestone 2 - Smart Contract Architecture ✅ COMPLETED

**Project Catalyst F13 - ID: #1300081**

Milestone 2 has been successfully completed with all deliverables delivered and acceptance criteria met. The project has delivered a complete on-chain smart contract architecture for managing BJJ practitioner profiles and rank promotions on the Cardano blockchain.

#### Deliverables Evidence

**1. Complete Smart Contract Architecture** ✅
- **Blueprint Definition**: `src/lib/Onchain/Blueprint.hs` 
- **Contract Specification**: `bjj-belt-system-blueprint.json`
- **Core Validators**: 
  - `src/lib/Onchain/MintingPolicy.hs` 
  - `src/lib/Onchain/ProfilesValidator.hs` 
  - `src/lib/Onchain/RanksValidator.hs` 
- **BJJ Logic**: `src/lib/Onchain/BJJ.hs` 

**2. Transaction Building Infrastructure** ✅
- **Core Operations**: `src/lib/TxBuilding/Operations.hs`  
- **Interaction Layer**: `src/lib/TxBuilding/Interactions.hs`  
- **Supporting Services**: `src/lib/TxBuilding/Lookups.hs`, `src/lib/TxBuilding/Skeletons.hs` 

**3. Comprehensive Testing Suite** ✅
- **Unit Tests**: `src/test/UnitTests.hs`
- **Property Tests**: `src/test/BJJPropertyTests.hs` 
- **Tests Evidence**: `tests_evidence.txt` 

**4. Complete Documentation** ✅
- **Technical Documentation**: `docs/Documentation.md`  
- **Architecture Documentation**: `docs/OnchainArchitecture.md`
- **Visual Documentation**: `puml/` 
- **Project Overview**: `README.md`

#### Acceptance Criteria Evidence

- **AC1: Smart Contract Completeness** ✅ - All three validators implemented with BJJ-specific logic
- **AC2: Transaction Building Completeness** ✅ - Full operation set for profiles, ranks, and promotions  
- **AC3: Testing Completeness** ✅ - Unit and property tests covering all core functionality
- **AC4: Documentation Completeness** ✅ - Complete technical specs and user documentation
- **AC5: Blueprint Integration** ✅ - Contract blueprint with validator specifications

#### Repository Structure Evidence

```
src/
├── lib/               # Core library modules
│   ├── Onchain/      # Smart contract implementations
│   ├── TxBuilding/   # Transaction building infrastructure  
│   └── DomainTypes/  # Domain type definitions
├── exe/              # Executable applications
│   ├── admin/        # Command-line admin tool
│   ├── interaction-api/  # Transaction building and submission API
│   └── query-api/    # Data querying API
└── test/             # Comprehensive testing suite
docs/                 # Complete documentation
puml/                 # Visual documentation diagrams
bjj-belt-system-blueprint.json  # Contract specification
```

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
