![Banner](out/puml/CARDANO-BJJ-BANNER.jpeg)

# Decentralized Belt System for Brazilian Jiu Jitsu 🥋 

- [Decentralized Belt System for Brazilian Jiu Jitsu 🥋](#decentralized-belt-system-for-brazilian-jiu-jitsu-)
  - [1. Overview](#1-overview)
  - [2. Key Features](#2-key-features)
  - [3. Architecture \& Components](#3-architecture--components)
  - [4. Project Structure](#4-project-structure)
  - [5. Installation \& Setup](#5-installation--setup)
  - [6. Usage](#6-usage)
  - [7. License](#7-license)
  - [8. Contributions, Feedback and Support](#8-contributions-feedback-and-support)
  - [9. Future Milestones](#9-future-milestones)
  - [10. Acknowledgments](#10-acknowledgments)

---

## 1. Overview

The **Decentralized Belt System** aims to bring **transparency** and **trust** to the Brazilian Jiu Jitsu rank promotion process by recording practitioner profiles, belt lineages, and achievements on the **Cardano** blockchain. This approach eliminates reliance on siloed or inconsistent records, ensuring that every belt rank is **verifiable** by anyone in the community.

---

## 2. Key Features

- **Immutable Rank Tracking**: Every rank promotion is recorded on-chain along with the awarding authority.  
- **Ensure transparency** in rank progression and lineage  
- **Achievements & Memberships**: Practitioners can showcase achievements, and organizational memberships in one place.  

---

## 3. Architecture & Components

![ComponentDiagram](/out/puml/ComponentDiagram/ComponentDiagram.png)

> - **Web Browser**  
>   - **BJJ-DApp Frontend**: The web interface for practitioners, masters, and organizations.  
>   - **3rd Party Browser Wallet** (e.g., Eternl, Lace): For signing transactions.
> - **Backend**  
>   - **Interaction API Service**: Builds and submits transactions for promotions, achievements, membership.  
>   - **Query API Service**: Provides quick queries for ranks, achievements, profiles, etc.  
>   - **Chain Sync Service**: Monitors the Cardano blockchain for updates.  
>   - **Cardano Node**: Submits signed transactions to the Cardano network.
> - **Persistence** : A database or index for quick lookups of ranks, achievements, memberships (off-chain).



For more details, see :  
[Detailed Documentation](docs/Documentation.md)

---

## 4. Project Structure

```plaintext
.
├── src/
│   ├── lib/                          # 📁 All libraries organized here
│   │   ├── onchain-lib/             # 🔗 Onchain logic (Plutus smart contracts)
│   │   │   └── Onchain/             # Validators, minting policies, protocols
│   │   ├── webapi-lib/              # 🌐 Web infrastructure
│   │   │   └── WebAPI/              # Auth, Health, CORS modules
│   │   ├── chainsync-lib/           # ⛓️ Generic chain sync utilities
│   │   │   ├── KupoClient.hs        # Kupo API client
│   │   │   └── KupoAtlas.hs         # Data conversion utilities
│   │   └── offchain-lib/            # 🏛️ Domain + infrastructure
│   │       ├── DomainTypes/         # Domain-specific types and DTOs
│   │       ├── TxBuilding/          # Transaction building utilities
│   │       ├── Storage.hs           # Database operations
│   │       ├── Ingestion.hs         # Event projection
│   │       ├── Constants.hs         # Configuration constants
│   │       └── Utils.hs             # Common utilities
│   ├── exe/                         # 📁 Executable applications
│   │   ├── admin/                   # Command-line admin tool
│   │   ├── chain-sync/              # Blockchain synchronization service
│   │   ├── interaction-api/         # Transaction building and submission API
│   │   └── query-api/               # Data querying API
│   └── test/                        # Test suites
│       ├── TestRuns.hs              # Integration tests
│       ├── UnitTests.hs             # Unit tests
│       └── BJJPropertyTests.hs      # Property-based tests
├── docs/                            # Documentation, specifications, diagrams
├── puml/                            # Plantuml diagrams
├── out/                             # Images of plantuml diagrams
└── README.md                        # This file
```

### 4.1 Library Architecture

The project is organized into **4 distinct libraries** with clear separation of concerns:

- **📦 onchain** - Plutus smart contracts and blockchain logic
- **📦 webapi** - Web infrastructure (Auth, Health, CORS)  
- **📦 chainsync** - Generic chain synchronization utilities
- **📦 offchain** - Domain logic and supporting infrastructure

**Dependency Flow:**
```
chainsync → offchain → webapi
    ↓         ↓         ↓
   onchain ← onchain ← onchain
```

This architecture ensures:
- **🔧 Maximum Reusability**: Generic components can be used by other projects
- **🏗️ Clean Separation**: Domain logic is separate from infrastructure
- **📈 Scalability**: Each library can evolve independently
- **🧪 Testability**: Components can be tested in isolation

## 5. Installation & Setup

## Prerequisites
- Unix-like operating system (Linux, macOS, or WSL2)
- Git installed
- Nix package manager
- Direnv 
***

## 5.1. Clone and Setup

This project uses the [The Developer Experience Shell](https://github.com/input-output-hk/devx/#the-developer-experience-shell) to build a fully-functioning and reproducible Cardano development shell for Haskell quickly and across multiple operating systems (and architectures).


```bash
git clone https://github.com/en7angled/Decentralized-Belt-System.git
cd Decentralized-Belt-System
```

 * After installing and configuring `nix` and `direnv`, clone the repo and type:
```bash
 direnv allow
``` 

 * Build:
```bash
cabal build all
``` 

 * The test suite for operations (transactions) can be run with the following command:

```bash
cabal test 
```
***

### 5.2. Configure Atlas
* Building transaction bodies requires gathering suitable information from the blockchain.  For this purpose, we'll require a provider. So in config directory a file named **"config_atlas.json"**, which should have the following format

```json
{
  "coreProvider": {
    "maestroToken": "YOUR_TOKEN_HERE",
    "turboSubmit": true
  },
  "networkId": "preview"
}
```
 * More info about the provider config can be found [here](https://atlas-app.io/getting-started/endpoints#defining-provider-configuration)


***

### 5.3. Configure Operation Key 
Create `operation.prv` with your private key mnemonic (24 words). Make sure you have enough funds available for covering the validators deployment. For testnet, you can get funds from the [Cardano Testnet Faucet](https://docs.cardano.org/cardano-testnets/tools/faucet).

***
### 5.4. Deploy validators

* This service requires validators to be deployed and to be used as reference UTxOs in the transactions.  The validators can be deployed from the TUI.

```bash
admin deploy-reference-scripts
```

* After deployment a file named **config_bjj_validators.json** which contains the validators reference UTxO should be present in the config folder.


## 6. Usage

### 6.1 Command Line Interface

```bash
admin --help
```

```
BJJ Belt System - Decentralized Belt Management

Usage: admin COMMAND

  A command-line tool for managing Brazilian Jiu Jitsu profiles, belt
  promotions, and achievements on the Cardano blockchain. Supports deploying
  reference scripts, initializing and updating profiles, handling promotions,
  and more.

Available options:
  -h,--help                Show this help text

Available commands:
  deploy-reference-scripts Deploy reference scripts for the BJJ belt system
  init-profile             Initialize a new profile
  update-profile-image     Update profile image
  delete-profile           Delete a profile
  promote-profile          Promote a profile to a new belt
  accept-promotion         Accept a promotion
  create-profile-with-rank Create a profile with initial rank
```

### 6.2 API Services

The system provides two independent API services:

#### **Interaction API** (Port 8082)
- **Build Transaction**: `POST /build-tx` - Builds transaction for interactions
- **Submit Transaction**: `POST /submit-tx` - Submits signed transactions
- **Swagger UI**: `http://localhost:8082/swagger-ui/`
- **Authentication**: All endpoints require HTTP Basic Auth. Defaults: `BASIC_USER=cardano`, `BASIC_PASS=lovelace` (override via env).

#### **Query API** (Port 8083)  
- **Profiles**: `GET /practitioner/{id}`, `GET /organization/{id}`, `GET /profiles`
- **Promotions**: `GET /promotions` - Query pending promotions
- **Belts**: `GET /belts`, `GET /belts/count`, `GET /belts/frequency`
- **Swagger UI**: `http://localhost:8083/swagger-ui/`
- **Authentication**: All endpoints require HTTP Basic Auth (same defaults). Swagger UI is public.
- **Projection mode**: add `?liveprojection=true` to query live data; otherwise the projected SQLite DB is used. Standard `limit`, `offset`, filter params are available per Swagger.

### 6.3 Executables & Local Run

Executables produced by the build:

- `admin` — CLI for deploying scripts and managing actions
- `interaction-api` — HTTP API for building and submitting transactions (port 8082)
- `query-api` — HTTP API for reading data and statistics (port 8083)
- `chainsync-service` — Chain sync and probe service (port 8084)

Run locally:

```bash
# Start interaction API (uses ATLAS_CORE_CONFIG, DEPLOYED_VALIDATORS_CONFIG)
cabal run interaction-api

# Start query API (uses ATLAS_CORE_CONFIG, LOOKUP_PATH)
cabal run query-api

# Start chain-sync service (uses KUPO_URL, LOOKUP_PATH)
cabal run chainsync-service
```

Environment variables:

- Interaction API: `ATLAS_CORE_CONFIG` (JSON or default file `config/config_atlas.json`), `DEPLOYED_VALIDATORS_CONFIG` (JSON or default file `config/config_bjj_validators.json`), `BASIC_USER`, `BASIC_PASS`, `PORT` (default 8082)
- Query API: `ATLAS_CORE_CONFIG`, `LOOKUP_PATH` (default `db/chainsync.sqlite`), `BASIC_USER`, `BASIC_PASS`, `PORT` (default 8083)
- Chain Sync: `KUPO_URL` (default `http://localhost:1442`), `LOOKUP_PATH` (default `db/chainsync.sqlite`), `BATCH_SIZE`, `FETCH_BATCH_SIZE`, `PORT` (default 8084)

#### **Chain Sync Probe** (Port 8084)
- **Health**: `GET /health` - Returns service health and current sync metrics
- **Readiness**: `GET /ready` - Indicates readiness (DB/migrations complete)


## 7. License
This project is licensed under the MIT License.  
See the `LICENSE` file for details.


## 8. Contributions, Feedback and Support

We welcome contributions from the community! Your feedback is invaluable!
Use the following channels for support or feedback:

1. Report bugs or suggest features via [GitHub Issues](https://github.com/en7angled/Decentralized-Belt-System/issues).
2. Join the Conversation: [GitHub Discussions](https://github.com/en7angled/Decentralized-Belt-System/discussions/1)
3. Submit pull requests (PRs) that align with the project’s goals.

## 9. Future Milestones

[Milestones](https://milestones.projectcatalyst.io/projects/1300081/milestones)


## 10. Acknowledgments
Thanks to the Cardano community for support.
This project is funded by [F13 - Project Catalyst ID: #1300081](https://projectcatalyst.io/funds/10/f13-cardano-use-cases-concept/decentralized-belt-system-for-brazilian-jiu-jitsu-bjj)

---

