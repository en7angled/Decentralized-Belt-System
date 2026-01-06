# Revision history for Decentralized-Belt-System


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
