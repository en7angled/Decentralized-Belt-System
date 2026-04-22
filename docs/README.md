# Documentation Index

Hand-written documentation for the Decentralized Belt System. Top-level repo
files (`README.md`, `CLAUDE.md`, `LICENSE`) live at the repo root, not here.

Each doc opens with an audience banner so you can decide quickly whether it's
the one you want. The sections below are grouped by audience too.

## Start here

- [`../README.md`](../README.md) — repo overview: install, configure, and run the services.
- [`specification.md`](specification.md) — the formal Catalyst-style functional spec: scope, actors, use cases, user stories.

## For engineers extending the system

- [`../CLAUDE.md`](../CLAUDE.md) — architecture, file-change reference, Haskell style, and workflow. The canonical development reference; per-library `CLAUDE.md` files add layer-specific detail.
- [`../.cursor/rules/`](../.cursor/rules/) — glob-scoped rules covering the new-concept checklist, Plutus conventions, and style. Mirrors the CLAUDE.md content for Cursor users.

## For auditors & protocol reviewers

- [`onchain-architecture.md`](onchain-architecture.md) — validators, oracle hub, minting policy, output-index conventions, security model.
- [`onchain-trace-codes.md`](onchain-trace-codes.md) — two-character failure-code lookup table.

## For operators & integrators

- [`architecture/chain-sync.md`](architecture/chain-sync.md) — what the ChainSync service does, consistency model, reorg handling.
- [`architecture/mcp-server.md`](architecture/mcp-server.md) — MCP tool/resource surface, tool inventory, deployment posture.

## Generated (regenerable — do not hand-edit)

- [`generated/README.md`](generated/README.md) — what's in here and how to regenerate it.
- [`generated/bjj-belt-system-blueprint.json`](generated/bjj-belt-system-blueprint.json) — CIP-57 blueprint from `admin write-blueprint`.
- [`generated/swagger/`](generated/swagger/) — Interaction + Query API OpenAPI specs, written at API startup.
