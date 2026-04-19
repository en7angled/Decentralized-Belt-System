# Documentation Index

Hand-written documentation for the Decentralized Belt System. Top-level repo
files (`README.md`, `CHANGELOG.md`, `ENGINEERING-LOG.md`, `CLAUDE.md`,
`LICENSE`) live at the repo root, not here.

Each doc opens with an audience banner so you can decide quickly whether it's
the one you want. The sections below are grouped by audience too.

## Start here

- [`../README.md`](../README.md) — repo overview: install, configure, and run the services.
- [`specification.md`](specification.md) — the formal Catalyst-style functional spec: scope, actors, use cases, user stories.

## For engineers extending the system

- [`developer-guide.md`](developer-guide.md) — workflow + consistency checklist + common-pitfalls table for adding a new concept. Cross-links to the CLAUDE.md files for layer-specific detail.
- [`haskell-style-guide.md`](haskell-style-guide.md) — naming, formatting, INLINEABLE, JSON conventions.

## For auditors & protocol reviewers

- [`onchain-architecture.md`](onchain-architecture.md) — validators, oracle hub, minting policy, output-index conventions, security model.
- [`onchain-security-audit.md`](onchain-security-audit.md) — threat analysis and resolution log per validator.
- [`onchain-trace-codes.md`](onchain-trace-codes.md) — two-character failure-code lookup table.

## For operators & integrators

- [`architecture/chain-sync.md`](architecture/chain-sync.md) — what the ChainSync service does, consistency model, reorg handling.
- [`architecture/mcp-server.md`](architecture/mcp-server.md) — MCP tool/resource surface, tool inventory, deployment posture.

## Generated (regenerable — do not hand-edit)

- [`generated/README.md`](generated/README.md) — what's in here and how to regenerate it.
- [`generated/bjj-belt-system-blueprint.json`](generated/bjj-belt-system-blueprint.json) — CIP-57 blueprint from `admin write-blueprint`.
- [`generated/swagger/`](generated/swagger/) — Interaction + Query API OpenAPI specs, written at API startup.

## Process

- [`../CHANGELOG.md`](../CHANGELOG.md) — terse, ship-impact summaries.
- [`../ENGINEERING-LOG.md`](../ENGINEERING-LOG.md) — long-form engineering journal (historical, pre-split).
