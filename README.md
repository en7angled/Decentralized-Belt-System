# Decentralized Belt System for Brazilian Jiu Jitsu



- [Decentralized Belt System for Brazilian Jiu Jitsu](#decentralized-belt-system-for-brazilian-jiu-jitsu)
  - [1. Overview](#1-overview)
  - [2. Key Features](#2-key-features)
  - [3. Architecture \& Components](#3-architecture--components)
  - [4. Project Structure](#4-project-structure)
  - [5. Installation \& Setup](#5-installation--setup)
  - [6. Usage](#6-usage)
  - [8. License](#8-license)
  - [8. Acknowledgments](#8-acknowledgments)

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
>   - **Interactions Service**: Builds transactions for promotions, achievements, membership.  
>   - **Chain Sync Service**: Monitors the Cardano blockchain for updates.  
>   - **Lookups Service**: Provides quick queries for ranks, achievements, etc.  
>   - **Cardano Node**: Submits signed transactions to the Cardano network.
> - **Persistence** : A database or index for quick lookups of ranks, achievements, memberships (off-chain).



For more details, see :  
[Detailed Documentation](docs/Documentation.md)

---

## 4. Project Structure

```plaintext
.
├── on-chain          # Cardano smart contracts (spending validators and minting policies)
├── frontend          # Frontend code 
├── docs              # Documentation, specifications, diagrams
├── puml              # Plantuml diagrams
├── out               # Images of plantuml diagrams
└── README.md         # This file

```

## 5. Installation & Setup

## 6. Usage

## 8. License
This project is licensed under the GNU GENERAL PUBLIC LICENSE v3.   
See the [LICENSE](https://www.gnu.org/licenses/gpl-3.0.html) file for details.

## 8. Acknowledgments
Thanks to the Cardano community for support.
This project is funded by [F13 - Project Catalyst ID: #1300081](https://projectcatalyst.io/funds/10/f13-cardano-use-cases-concept/decentralized-belt-system-for-brazilian-jiu-jitsu-bjj-a438b)