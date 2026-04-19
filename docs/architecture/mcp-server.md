# BJJ MCP Server — As-built ADR

*Audience: operators, integrators, and engineers extending the MCP tool surface. Cross-references: `onchain-architecture.md` for validator-level security; `developer-guide.md` for the library layering this server reuses.*

## Status

**Built and shipping.** Lives in the existing Cabal project as `mcp-server-lib` + `mcp-server` executable. Serves on port 8085 by default.

Consumers: any MCP-speaking client — Claude Desktop, IDE extensions, the private `bjj-frontend` BFF, third-party agents. The server is protocol-layer and not coupled to any one consumer.

---

## 1. Goal

Expose the Query API (:8083) and Interaction API (:8082) to MCP clients as a reusable tool surface. Read tools wrap Query API endpoints; write tools wrap Interaction API `build-tx` endpoints and return **unsigned** tx bodies for wallet signing downstream.

---

## 2. Decision: MCP server over direct tool use

|                  | Option A: Direct tool use      | Option B (chosen): MCP server                            |
| ---------------- | ------------------------------ | -------------------------------------------------------- |
| Tool definitions | Hardcoded in each client       | Standalone server, reusable                              |
| Other AI clients | Each rolls its own             | Any MCP client (Claude Desktop, IDE, BFF, third-party)   |
| Write tools (tx) | Wallet bridge custom in client | Same — wallet bridge is custom either way                |
| Read tools       | Client calls APIs directly     | Client calls MCP, MCP calls APIs                         |
| Extra hop        | None                           | +1 (client → MCP → API) — negligible vs LLM latency      |

The ecosystem payoff (any MCP-compatible client can query BJJ data, no client-specific API) outweighed the extra hop.

---

## 3. Decision: Haskell in-repo, not a standalone service

Reusing `offchain-lib` + `webapi-lib` buys:

- **Shared types.** `PractitionerProfile`, `Rank`, `Achievement`, `Membership` are imported directly from `offchain-lib` — no duplication, no schema drift risk.
- **Shared infra.** CORS, service probes, and basic-auth helpers come from `webapi-lib`.
- **Shared toolchain.** `cabal build all`, same Nix flake, same `direnv`, same CI pipeline.
- **Compile-time schema safety.** A tool handler's input type *is* its MCP schema — changing a `DomainTypes` field flags every consumer.

The `mcp` package (from DPella, Hackage) is Servant-based, which matches `webapi-lib`'s stack.

---

## 4. Component layout

The MCP server is split into a reusable library + a thin executable. This keeps the executable trivially stubbable for tests and makes the tool/resource logic importable from integration tests.

```
src/
├── lib/
│   └── mcp-server-lib/
│       └── MCPServer/
│           ├── App.hs            # runtime config (env vars) → AppCtx
│           ├── Server.hs         # WAI composition + Warp
│           ├── Readiness.hs      # upstream /health probes
│           ├── Resources.hs      # MCP resources (annex-3, belt hierarchy, FAQ)
│           ├── Schema.hs         # shared schema fragments
│           ├── Orphans.hs        # ToolParamSchema orphans for DomainTypes
│           ├── Clients.hs        # shared HTTP client utilities
│           ├── Api/
│           │   ├── Query.hs      # Servant client to query-api
│           │   └── Interaction.hs # Servant client to interaction-api
│           └── Tools/
│               ├── Common.hs       # shared arg/result helpers
│               ├── Transactions.hs # shared buildTx helper (consumed by write tools)
│               ├── Profiles.hs
│               ├── Promotions.hs
│               ├── Achievements.hs
│               ├── Memberships.hs
│               ├── Search.hs
│               └── Docs.hs         # get_faq tool (mirrors bjj://docs/faq)
└── exe/
    └── mcp-server/
        └── Main.hs              # withAppCtx runMCPServer
```

---

## 5. Cabal entry

```cabal
library mcp-server-lib
  import:             common-options
  default-language:   GHC2021
  hs-source-dirs:     src/lib/mcp-server-lib
  exposed-modules:
    MCPServer.Api.Interaction
    MCPServer.Api.Query
    MCPServer.App
    MCPServer.Clients
    MCPServer.Orphans
    MCPServer.Readiness
    MCPServer.Resources
    MCPServer.Schema
    MCPServer.Server
    MCPServer.Tools.Achievements
    MCPServer.Tools.Common
    MCPServer.Tools.Docs
    MCPServer.Tools.Memberships
    MCPServer.Tools.Profiles
    MCPServer.Tools.Promotions
    MCPServer.Tools.Search
    MCPServer.Tools.Transactions
  build-depends:
    , offchain-lib
    , onchain-lib
    , webapi-lib
    , mcp
    , aeson
    , async
    , bytestring
    , containers
    , file-embed
    , http-api-data
    , http-client
    , http-client-tls
    , http-types
    , mtl
    , servant
    , servant-client
    , servant-client-core
    , servant-server
    , text
    , time
    , wai
    , warp
    , atlas-cardano

executable mcp-server
  import:           common-options
  main-is:          Main.hs
  hs-source-dirs:   src/exe/mcp-server
  default-language: GHC2021
  ghc-options:      -threaded -rtsopts -with-rtsopts=-T -with-rtsopts=-N -O2
  build-depends:    base, mcp-server-lib
```

Notably absent: `servant-auth-server`. See § 8 Security posture.

The cabal file also declares `extra-source-files: src/lib/mcp-server-lib/resources/annex-3.md` so the file-embed splice works from sdist tarballs (Docker builds).

---

## 6. Tool inventory

All tools are exposed via MCP `tools/list`. Write tools are gated on `MCP_ENABLE_WRITE_TX=1`; when off they are absent from `tools/list` — so `tools/list` is an honest reflection of what the server will accept.

### Profiles (read-only)

- `get_practitioner_profile`
- `get_organization_profile`
- `get_practitioner_detail`
- `get_organization_detail`
- `list_profiles`

### Promotions

- `get_promotions_page` (read)
- `get_belt_frequency` (read)
- `check_promotion_eligibility` (read) — off-chain mirror of the on-chain validator, returns structured violations + earliest-eligible date + required granter rank.
- `build_promote_rank_tx` (write, gated)
- `build_accept_promotion_tx` (write, gated)

### Achievements

- `get_achievements` (read)
- `get_achievement_by_id` (read)
- `build_accept_achievement_tx` (write, gated)

### Memberships (all write, all gated)

- `build_create_membership_history_tx`
- `build_add_membership_interval_tx`
- `build_accept_membership_interval_tx`
- `build_update_membership_end_date_tx`

### Search

- `search` (read) — cross-entity unified search.
- `get_pending_actions` (read)

### Docs

- `get_faq` (read) — serves the product-level FAQ markdown embedded at compile time (same bytes as the `bjj://docs/faq` resource).

### Deferred

The original plan proposed these but they have not shipped yet:

- `build_create_profile_tx`
- `build_award_achievement_tx`
- `get_belt_statistics` (distinct from `get_belt_frequency`)
- `get_membership_histories`

Add as needed when consumers surface the requirement. **Ground-truth the inventory against `tools/list` at runtime** — the server is the single source of truth.

---

## 7. MCP resources

Three resources are advertised via MCP `resources/list`:

| URI                          | Source                                                                                                  | MIME type          |
| ---------------------------- | ------------------------------------------------------------------------------------------------------- | ------------------ |
| `bjj://rules/annex-3`        | `src/lib/mcp-server-lib/resources/annex-3.md` embedded at compile time via `file-embed`                 | `text/markdown`    |
| `bjj://rules/belt-hierarchy` | JSON array of `[White .. Red10]` generated from the `BJJBelt` enum                                      | `application/json` |
| `bjj://docs/faq`             | `src/lib/mcp-server-lib/resources/bjj-faq.md` embedded at compile time (also served by `get_faq` tool)  | `text/markdown`    |

**Why file-embed** — `makeRelativeToProject` anchors the path to the nearest `.cabal`, so the splice works whether cabal builds in-tree or from an extracted sdist tarball (the latter is how Docker builds work). The authored markdown is a single source of truth shared with human-readable docs.

**Why enum-generated belt hierarchy** — the JSON cannot drift from the on-chain `BJJBelt` type. Adding a new belt is a one-line change in `DomainTypes.Core.BJJ`; the resource body rebuilds automatically.

**Why both a resource and a tool for the FAQ** — many agent clients consume tools but not resources. Exposing the FAQ as both keeps the canonical copy in one file while letting any client reach it.

**Deferred: `bjj://config/fees`.** Fee data lives on-chain in `FeeConfig` and is oracle-adjustable, so a file-embedded snapshot would go stale. Needs a live read path (or a cached read-through) before it ships.

---

## 8. Runtime configuration

All via env vars (see `MCPServer.App.withAppCtx`):

| Env var                     | Default                               | Purpose                                                                |
| --------------------------- | ------------------------------------- | ---------------------------------------------------------------------- |
| `QUERY_API_URL`             | `http://query-api:8083`               | Upstream Query API base URL                                            |
| `INTERACTION_API_URL`       | `http://interaction-api:8082`         | Upstream Interaction API base URL                                      |
| `PORT`                      | `8085`                                | Listen port (shared helper `getPortFromEnvOrDefault`)                  |
| `MCP_ENABLE_WRITE_TX`       | unset                                 | Set to `1` to surface write tools in `tools/list`                      |
| `MCP_READINESS_TIMEOUT_MS`  | `2000`                                | Per-upstream timeout for `/ready` probes                               |
| Basic auth                  | via `getBasicAuthFromEnv`             | Credentials used to call the upstream APIs (shared with other execs)   |

---

## 9. Health and readiness

Follows the same `ServiceProbe` pattern as the other executables:

- `GET /health` — `alwaysHealthy`. Liveness only. Never touches external state.
- `GET /ready` — concurrently probes `{queryBaseUrl}/health` and `{interactionBaseUrl}/health` (unauthenticated; upstream `/health` is public). Returns 503 with a per-upstream status body if either is unreachable within `MCP_READINESS_TIMEOUT_MS`. Worst-case latency is `max(q, i)`, not `q + i`.
- `POST /mcp` (and GET for SSE) — MCP JSON-RPC via `simpleHttpApp`.

Prefix dispatch and CORS live in `MCPServer.Server.runMCPServer`.

---

## 10. Security posture

**The MCP server is unauthenticated.** `simpleHttpApp` does not terminate auth; no JWT or BasicAuth guards `/mcp`. Write tools are gated only by the `MCP_ENABLE_WRITE_TX` env flag and by the downstream Interaction API's HTTP Basic Auth (which the server holds on behalf of all callers).

This has consequences that must be understood before deploying outside a private network:

1. **Tool authority (e.g. "does this wallet own this profile?") is UX, not security.** A consumer-side middleware can reject unauthorized tool calls for a better user experience, but any third-party MCP client bypasses that middleware. **Do not treat it as a security boundary.**

2. **Real security for tx operations lives in two places:**
   - The user's **wallet signature** — the MCP server never touches keys; it only returns unsigned tx bodies.
   - The **onchain Plutus validators** — they reject unauthorized actions regardless of who built the tx body.

3. **Production deployment requires one of:**
   - Bind the MCP server to a private network (docker-compose internal network, VPC, Tailscale, etc.), **or**
   - Front it with an authenticating reverse proxy (NGINX + OAuth2 Proxy, Cloudflare Access, Traefik forwardauth), **or**
   - Add auth to the MCP server directly (not currently supported by `simpleHttpApp`; would require a custom HTTP adapter).

**This deployment constraint is non-negotiable when `MCP_ENABLE_WRITE_TX=1`.** An unauthenticated, publicly reachable MCP server with write tools enabled is a tx-build endpoint for anyone on the internet.

---

## 11. What stays untouched

- Query API (:8083) — no code changes
- Interaction API (:8082) — no code changes
- Chain Sync Service (:8084) — no code changes
- Cardano node — no changes
- Smart contracts (Plutus) — no changes
- PostgreSQL/SQLite projections — no changes

The MCP server is a purely **additive layer** that consumes the existing APIs. This is the cleanest integration path because the APIs are already well-separated (read vs write, with Swagger docs and HTTP Basic Auth).
