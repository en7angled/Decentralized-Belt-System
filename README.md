![Banner](out/puml/CARDANO-BJJ-BANNER.jpeg)

# Decentralized Belt System for Brazilian Jiu Jitsu 🥋 

- [Decentralized Belt System for Brazilian Jiu Jitsu 🥋](#decentralized-belt-system-for-brazilian-jiu-jitsu-)
  - [1. Overview](#1-overview)
  - [2. Key Features](#2-key-features)
  - [3. Architecture \& Components](#3-architecture--components)
  - [4. Project Structure](#4-project-structure)
    - [4.1 Library Architecture](#41-library-architecture)
  - [5. Installation \& Setup](#5-installation--setup)
  - [Prerequisites](#prerequisites)
  - [5.1. Clone and Setup](#51-clone-and-setup)
    - [5.2. Configure Atlas](#52-configure-atlas)
    - [5.3. Configure Operation Key](#53-configure-operation-key)
    - [5.4. Deploy validators](#54-deploy-validators)
  - [6. Usage](#6-usage)
    - [6.1 Command Line Interface](#61-command-line-interface)
    - [6.2 Testnet Scripts](#62-testnet-scripts)
      - [**Test Script** - `scripts/test_black_promotes_white_to_blue.sh`](#test-script---scriptstest_black_promotes_white_to_bluesh)
      - [**Population Script** - `scripts/populate_testnet.sh`](#population-script---scriptspopulate_testnetsh)
    - [6.3 API Services](#63-api-services)
      - [**Interaction API** (Port 8082)](#interaction-api-port-8082)
      - [**Query API** (Port 8083)](#query-api-port-8083)
    - [6.4 Executables \& Local Run](#64-executables--local-run)
      - [**Chain Sync Probe** (Port 8084)](#chain-sync-probe-port-8084)
  - [7. License](#7-license)
  - [8. Contributions, Feedback and Support](#8-contributions-feedback-and-support)
  - [9. Future Milestones](#9-future-milestones)
  - [10. Acknowledgments](#10-acknowledgments)

---

## 1. Overview

The **Decentralized Belt System** aims to bring **transparency** and **trust** to the Brazilian Jiu Jitsu rank promotion process by recording practitioner profiles, belt lineages, and achievements on the **Cardano** blockchain. This approach eliminates reliance on siloed or inconsistent records, ensuring that every belt rank is **verifiable** by anyone in the community.

> **Product repo.** The web frontend, BFF, and LLM agent-service live in a separate (private) repo, `bjj-frontend`. That repo pulls this repo's Haskell services as published Docker Hub images (`mariusgeorgescu/bjj-{chainsync,interaction-api,query-api,mcp-server}`, pinned by a `PROTOCOL_TAG`). The `docker-compose.yml` in this repo is a **dev/audit** stack for running the protocol services standalone — no TLS, no ingress, localhost ports only.

---

## 2. Key Features

- **Immutable Rank Tracking**: Every rank promotion is recorded on-chain along with the awarding authority. Profiles are permanent by design, preserving lineage integrity.
- **Ensure transparency** in rank progression and lineage  
- **Achievements & Memberships**: Practitioners can showcase achievements, and organizational memberships in one place.
- **Full Security Model**: Two-layer validation with BJJ rule enforcement at mint time and consent verification at acceptance.  

---

## 3. Architecture & Components

![ComponentDiagram](/out/puml/ComponentDiagram/ComponentDiagram.png)


> - **Web Browser**  
>   - **BJJ-DApp Frontend**: The web interface for practitioners, masters, and organizations.  
>   - **3rd Party Browser Wallet** (e.g., Eternl, Lace): For signing transactions.
> - **Backend**  
>   - **Interaction API Service**: Builds and submits transactions for promotions, achievements, membership.  
>   - **Query API Service**: Provides quick queries for ranks, achievements, profiles, lineage, etc.
>   - **Chain Sync Service**: Monitors the Cardano blockchain for updates.  
>   - **Cardano Node**: Submits signed transactions to the Cardano network.
> - **Persistence** : A database or index for quick lookups of ranks, achievements, memberships (off-chain).



For more details, see [Detailed Documentation](docs/specification.md).  
To regenerate diagram images after editing `puml/*.puml`, run `scripts/regenerate_diagrams.sh` (requires [PlantUML](https://plantuml.com/); e.g. `brew install plantuml`).

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
│   │   ├── offchain-lib/            # 🏛️ Domain + infrastructure
│   │   │   ├── DomainTypes/         # Domain-specific types and DTOs
│   │   │   ├── TxBuilding/          # Transaction building utilities
│   │   │   ├── Storage.hs           # Database operations
│   │   │   ├── Ingestion.hs         # Event projection
│   │   │   ├── Constants.hs         # Configuration constants
│   │   │   └── Utils.hs             # Common utilities
│   │   └── mcp-server-lib/          # 🤖 MCP (Model Context Protocol) server
│   │       ├── MCPServer/           # Tools, resources, handlers
│   │       └── resources/           # Compile-time-embedded markdown (annex-3, faq)
│   ├── exe/                         # 📁 Executable applications
│   │   ├── admin/                   # Command-line admin tool
│   │   ├── chain-sync/              # Blockchain synchronization service (binary: chainsync-service)
│   │   ├── interaction-api/         # Transaction building and submission API
│   │   ├── mcp-server/              # MCP server exposing Query + Interaction APIs to LLM clients
│   │   └── query-api/               # Data querying API
│   └── test/                        # Test suites
│       ├── TestRuns.hs              # Integration tests
│       ├── UnitTests.hs             # Unit test entrypoint
│       ├── UnitTests/               # Unit test modules (Achievement, Cleanup, Membership, Oracle, Promotion)
│       ├── Test/                    # Fixtures and helpers
│       └── BJJPropertyTests.hs      # Property-based tests
├── docs/                            # Documentation, specifications, diagrams
├── puml/                            # Plantuml diagrams
├── out/                             # Images of plantuml diagrams
├── scripts/                         # Test and utility scripts
│   ├── test_black_promotes_white_to_blue.sh  # Core promotion test
│   ├── populate_testnet.sh          # Testnet data population
│   ├── regenerate_diagrams.sh       # Regenerate PlantUML images (puml/ → out/puml/)
│   ├── test_exunits.sh              # Execution units / cost testing
│   └── build-images.sh              # Docker/image build
└── README.md                        # This file
```

### 4.1 Library Architecture

The project is organized into **5 distinct libraries** with clear separation of concerns:

- **📦 onchain-lib** - Plutus smart contracts and blockchain logic
- **📦 webapi-lib** - Web infrastructure (Auth, CORS, ServiceProbe); no project-library deps
- **📦 chainsync-lib** - Generic chain synchronization utilities (Kupo client, Atlas adapter)
- **📦 offchain-lib** - Domain logic, transaction building, storage, ingestion
- **📦 mcp-server-lib** - MCP (Model Context Protocol) server library, consumed by the `mcp-server` executable

**Dependency layering** (inner → outer; deps only point inward):
```
onchain-lib + chainsync-lib  →  offchain-lib  →  webapi-lib
                                                       ↑
                                                 mcp-server-lib
                                       (also depends on offchain-lib + onchain-lib)
```

`onchain-lib` must never import off-chain deps (no Aeson, Servant, Swagger). `webapi-lib` carries HTTP/auth/CORS only and has no project-library dependencies.

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
  reference scripts, writing CIP-57 blueprints, initializing and updating
  profiles, handling promotions, and more.

Available options:
  -h,--help                Show this help text

Available commands:
  deploy-reference-scripts   Deploy reference scripts for the BJJ belt system
  write-blueprint            Write the CIP-57 contract blueprint JSON to a file
  pause-protocol             Pause the protocol (oracle opPaused = True)
  unpause-protocol           Unpause the protocol
  set-fees                   Set or clear fee configuration in the oracle
  set-min-utxo-value         Set minimum UTxO value (lovelace) for protocol state outputs
  query-oracle               Display current oracle parameters (read-only)
  init-profile               Initialize a new profile (White belt)
  update-profile             Update profile metadata (description, image)
  promote-profile            Promote a profile to a new belt
  accept-promotion           Accept a promotion
  create-profile-with-rank   Create a profile with initial rank (for masters)
  create-membership-history  Create a membership history for a practitioner at an organization
  get-first-interval-id      Get first membership interval ID for a history node
  add-membership-interval    Add a membership interval to an existing history
  accept-membership-interval Accept a membership interval (practitioner acknowledges)
  update-end-date            Update membership interval end date
  award-achievement          Award an achievement to a practitioner
  accept-achievement         Accept an achievement (practitioner acknowledges)
  cleanup-dust               Sweep dust/griefing UTxOs from validator addresses (permissionless)
```

> **Note**: Profile deletion is intentionally not supported. BJJ belt records are permanent historical facts that preserve lineage integrity.

### 6.2 Testnet Scripts

Two shell scripts are provided for testnet testing and demonstrations:

#### **Test Script** - `scripts/test_black_promotes_white_to_blue.sh`

A focused test that demonstrates the core promotion flow:
1. Creates a master profile with Black belt
2. Creates a student profile (White belt)
3. Master promotes student to Blue belt
4. Student accepts the promotion

```bash
./scripts/test_black_promotes_white_to_blue.sh
```

#### **Population Script** - `scripts/populate_testnet.sh`

A comprehensive script that populates the testnet with realistic sample data:
- 2 Organizations (Gracie Barra Academy, Alliance Jiu-Jitsu)
- 1 Grand Master (Red belt)
- 2 Masters (Black belts)
- 4 Students (White to Purple belts)
- Multiple promotion scenarios

```bash
./scripts/populate_testnet.sh
```

Both scripts:
- Automatically deploy reference scripts if needed
- Show clean progress output with colored indicators
- Display detailed summaries with all created IDs
- Handle errors gracefully with clear messages

### 6.3 API Services

The system provides three independent HTTP services (plus a chain-sync probe):

#### **Interaction API** (Port 8082)
- **Build Transaction**: `POST /build-tx` - Builds transaction for interactions
- **Submit Transaction**: `POST /submit-tx` - Submits signed transactions
- **Swagger UI**: `http://localhost:8082/swagger-ui/`
- **Authentication**: All endpoints require HTTP Basic Auth. Defaults: `BASIC_USER=cardano`, `BASIC_PASS=lovelace` (override via env).

#### **Query API** (Port 8083)  
- **Profiles**: `GET /practitioner/{id}`, `GET /organization/{id}`, `GET /profiles`, `GET /profiles/count`, `GET /profiles/frequency`
- **Promotions**: `GET /promotions`, `GET /promotions/count`
- **Belts**: `GET /belts`, `GET /belts/count`, `GET /belts/frequency`
- **Memberships**: `GET /memberships`, `GET /memberships/count`, `GET /membership-intervals`, `GET /membership-intervals/count`
- **Achievements**: `GET /achievements`, `GET /achievements/count`
- **Lineage**: `GET /lineage?root=...&ancestors=...&descendants=...` (direct lineage tree: ancestor chain + descendant subtree; projected DB only)
- **Protocol**: `GET /protocol-status` (oracle pause, min UTxO, fee config)
- **Swagger UI**: `http://localhost:8083/swagger-ui/`
- **Authentication**: All endpoints require HTTP Basic Auth (same defaults). Swagger UI is public.
- **Projection mode**: add `?liveprojection=true` to query live on-chain data; otherwise reads come from the **PostgreSQL** projection populated by **chainsync-service**. Standard `limit`, `offset`, filter params are available per Swagger.

#### **MCP Server** (Port 8085)
- **Transport**: MCP JSON-RPC over streamable HTTP at `POST /mcp`. Reuses the same probe endpoints as the other services (`/health`, `/ready`).
- **Tool surface**: read tools wrap Query API endpoints; write tools wrap Interaction API `build-tx` endpoints and return **unsigned** tx bodies for wallet signing downstream.
- **Resources**: serves authored markdown (`bjj://rules/annex-3`, `bjj://docs/faq`) and a runtime-generated JSON belt hierarchy (`bjj://rules/belt-hierarchy`).
- **Write-tool gating**: write tools are absent from `tools/list` unless `MCP_ENABLE_WRITE_TX=1`. Off by default.
- **Authentication**: the `/mcp` endpoint itself has **no** auth — deploy behind a private network or auth proxy. Upstream API credentials are read from `BASIC_USER`/`BASIC_PASS`.
- See [docs/architecture/mcp-server.md](docs/architecture/mcp-server.md) for the as-built ADR.

### 6.4 Executables & Local Run

Executables produced by the build:

- `admin` — CLI for deploying scripts and managing actions
- `interaction-api` — HTTP API for building and submitting transactions (port 8082)
- `query-api` — HTTP API for reading data and statistics (port 8083)
- `chainsync-service` — Chain sync and probe service (port 8084)
- `mcp-server` — MCP server exposing Query + Interaction APIs to LLM clients (port 8085)

Run locally:

```bash
# Start interaction API (uses ATLAS_CORE_CONFIG, DEPLOYED_VALIDATORS_CONFIG)
cabal run interaction-api

# Start query API (uses ATLAS_CORE_CONFIG, PG_CONN_STR, optional DEPLOYED_VALIDATORS_CONFIG)
cabal run query-api

# Start chain-sync service (uses KUPO_URL, PG_CONN_STR, ATLAS_CORE_CONFIG, DEPLOYED_VALIDATORS_CONFIG)
cabal run chainsync-service

# Start MCP server (proxies Query + Interaction APIs; needs both reachable)
cabal run mcp-server
```

Environment variables:

- Interaction API: `ATLAS_CORE_CONFIG` (JSON or default file `config/config_atlas.json`), `DEPLOYED_VALIDATORS_CONFIG` (JSON or default file `config/config_bjj_validators.json`), `BASIC_USER`, `BASIC_PASS`, `PORT` (default 8082)
- Query API: `ATLAS_CORE_CONFIG`, `PG_CONN_STR` (default `host=postgres user=postgres password=postgres dbname=chainsync port=5432` — Docker-oriented; override in `.env` for a local Postgres), `DEPLOYED_VALIDATORS_CONFIG` (JSON or default file `config/config_bjj_validators.json`; used for deployed-script context e.g. protocol status), `BASIC_USER`, `BASIC_PASS`, `PORT` (default 8083)
- Chain Sync: `ATLAS_CORE_CONFIG`, `DEPLOYED_VALIDATORS_CONFIG`, `KUPO_URL` (default in source is a Demeter preview Kupo URL; set to `http://localhost:1442` or your Kupo when running locally), `PG_CONN_STR` (default `host=localhost user=postgres password=postgres dbname=chainsync port=5432`), `BATCH_SIZE`, `FETCH_BATCH_SIZE`, `PORT` (default 8084)
- MCP Server: `QUERY_API_URL` (default `http://query-api:8083`), `INTERACTION_API_URL` (default `http://interaction-api:8082`), `BASIC_USER`/`BASIC_PASS` (forwarded to upstream APIs), `MCP_ENABLE_WRITE_TX` (set to `1` to surface write tools; off by default), `MCP_READINESS_TIMEOUT_MS` (default 2000), `PORT` (default 8085)

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

