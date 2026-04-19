# Executable Rules

Five executables: `admin` (CLI), `interaction-api` (8082), `query-api` (8083), `chainsync-service` (8084), `mcp-server` (8085). Sources in `src/exe/<name>/` (chain-sync dir = `chain-sync`).

## Admin CLI

- Single `Main.hs` with `Command` sum type, optparse-applicative `hsubparser`, `executeCommand` dispatch.
- **Context**: `Either ProviderCtx TxBuildingContext` — `Left` for deploy/blueprint/limited queries, `Right` for full tx-building.
- **Tx commands**: Convert CLI args → `ActionType` → `runBJJActionWithPK txBuildingCtx signKey actionType Nothing`.
- **Read-only commands**: `runQuery providerCtx (runReaderT lookupFn deployedScriptsCtx)` — no signing.
- **Parsing**: Use `maybeReader` for asset classes (JSON-decode) and custom parsers for enums (e.g. BJJ belt: case-insensitive, hyphenated). Share parsers (e.g. `profileDataParser`, `assetClassParser`, `posixTimeParser`, `outputIdParser`) across commands.
- **New command**: Add `Command` constructor + `*Args` → parser with `command "kebab-name"` → conversion to `ActionType` → case in `executeCommand`.

## HTTP API Servers (interaction-api, query-api)

- **Startup**: Write Swagger JSON → load config → `withCfgProviders` → build app context → `runSettings`.
- **App monad**: `*AppContext` record + `newtype *AppMonad a = *AppMonad (ReaderT *AppContext Handler a)`. Hoist with `hoistServerWithContext`.
- **RestAPI module**: API types with `Summary`/`Description`. Include `ProbeAPI` + Swagger UI. Full server = swagger :<|> probe :<|> private (auth-protected).
- **WAI app**: `mkBJJApp ctx = setupCors . provideOptions proxyPublicAPI $ serveWithContext ... basicCtx ...` with `basicAuthServerContext`.
- **Error handling**: Map `TxBuildingException` to HTTP status. Use `throwError (err404 { errBody = ... })`.
- **New endpoint**: Route type in RestAPI → handler in app monad → wire in server → `ToSchema` for Swagger sync.

## Build-tx vs Submit

- **Build-tx**: Receive used addresses + change address + optional collateral → run builder → return **unsigned** tx body CBOR. Do not sign in server.
- **Submit**: Receive unsigned body + witness → add witness → submit via provider.
- **Multi-address wallets**: Accept **list** of used addresses + change address.
- **Collateral**: Use frontend-provided collateral when valid; otherwise framework picks UTxO.
- **Provider config**: Atlas core config (`config_atlas.json`) includes `coreProvider` (Maestro, local node + Kupo, or Blockfrost), `networkId`, and optional `logging`/`logTiming`. Servers run inside `withCfgProviders`.

## Query API Specifics

- `QueryAppContext` = auth + `ProviderCtx` + PostgreSQL `ConnectionPool`.
- **Live vs projected**: `QueryFlag "liveprojection"` on read endpoints. `withBackend liveFlag liveHandler projectedHandler`.
- Live queries in `Query.Live`, projected in `Query.Projected`, shared in `Query.Common`.
- **RestAPI.Common**: Centralize building of filter types from query params (e.g. `profileFilterFromParams`, `rankFilterFromParams`) and use `withBackend` so RestAPI stays thin and handlers delegate to Query.*.

## Chain-Sync Service

- Startup: port/env → PG pool + migrations → initial tip → `SyncMetrics` MVar → fork probe server → load config → derive Kupo match pattern → align checkpoint → `forever` sync loop.
- Sync loop: get tips → `evaluateChainSyncState` (UpToDate|Behind|Ahead|UpToDateButDifferentBlockHash) → fetch/rollback/update accordingly.
- Probe server: separate module, `mkServiceProbeApp`, returns 503 when not synced.

## MCP Server

- Thin `Main.hs` shim: `main = withAppCtx runMCPServer` — all logic lives in `mcp-server-lib`.
- **Transport**: MCP JSON-RPC over streamable HTTP at `/mcp` (via `simpleHttpApp` from the `mcp` package); reuses `ServiceProbe` at `/health` + `/ready`; prefix dispatch composes them into one WAI app in `MCPServer.Server.runMCPServer`.
- **Upstreams**: Calls query-api (:8083) and interaction-api (:8082) via Servant clients in `MCPServer.Api.{Query,Interaction}`. Uses `getBasicAuthFromEnv` for upstream credentials; **does not** authenticate its own `/mcp` endpoint — deploy behind a private network or auth proxy. See `docs/architecture/mcp-server.md` § 10.
- **Write-tool gating**: `MCP_ENABLE_WRITE_TX=1` surfaces write tools in `tools/list`; off by default. Flag is consulted per-module inside each `tools :: AppCtx -> [ToolHandler]`, so `tools/list` always reflects what the server will accept.
- **Resources**: Embedded at compile time via `file-embed` (`src/lib/mcp-server-lib/resources/annex-3.md` → `bjj://rules/annex-3`; add `extra-source-files` so sdist tarballs/Docker builds work) or generated from enums (`[White .. Red10]` → `bjj://rules/belt-hierarchy`). Wired in `MCPServer.Resources.registerResources`.
- **Readiness**: `/ready` concurrently probes both upstreams' `/health`; returns 503 + body listing per-upstream status if either is unreachable within `MCP_READINESS_TIMEOUT_MS` (default 2000). `/health` is `alwaysHealthy` — never touches external state.
- **New tool**: Add handler in `MCPServer.Tools.<Domain>.hs`, include in that module's `tools ctx` export (`<> [writeTool ctx | enableWriteTx ctx]` for write tools), ensure domain types have `ToolParamSchema` instances (orphans in `MCPServer.Orphans` if needed). Wire the module into `allTools` in `MCPServer.Server`.
- **New resource**: Add URI + `Resource` record in `MCPServer.Resources`, extend `readHandler` dispatch, append to `bjjResources`.

## Shared Conventions

- **Config**: `Constants` for defaults. `decodeConfigEnvOrFile`. `getPortFromEnvOrDefault`.
- **Auth**: `getBasicAuthFromEnv` + `basicAuthServerContext`. `BasicAuth "user-realm" AuthUser :>` for private routes.
- **CORS**: `setupCors` as outermost middleware.
- **Swagger**: Written to `docs/generated/swagger/` on startup. Do not edit by hand.
