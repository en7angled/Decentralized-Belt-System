# Revision history for Decentralized-Belt-System

Major changes only, latest first.

---

## Unreleased

- **Agent service — Week 6 polish: tx-status + pending-tx + loadtest:** Three new endpoints on the agent service. (1) `GET /tx-status/{tx_id}` proxies the Interaction API's confirmation check (`tsrTxId`, `tsrConfirmed`) and normalizes the response shape to `{"tx_id": str, "confirmed": bool}` so widgets don't need to know the upstream naming convention. Preserves 404 semantics; collapses upstream 5xx / unreachable into 502. (2) `GET /session/{session_id}/pending-tx` lists unexpired checkpoints for the session — scans `checkpoint:{session_id}:*` in Redis via cursor-based `SCAN_ITER` (non-blocking, sessions typically have 0-1 pending), returns `{session_id, pending: [{tx_body_hash, tx_body_cbor, description, created_at, signing_deadline}]}`. Widgets use this on reconnect to re-render any outstanding signing card without restarting the conversation. (3) `POST /submit-tx` response shape upgraded: now returns `{"status": "submitted", "tx_id": str, "poll_url": "/tx-status/..."}` so widgets have a direct polling path without constructing URLs client-side. Also: `submit_tx.submitted`, `submit_tx.upstream_error`, `submit_tx.upstream_unreachable` structured-log events emitted. `interaction_client.py` gains `tx_status(tx_id)` alongside `submit_signed_tx`. `checkpoint.py` gains `list_pending(session_id)`. New `scripts/loadtest.py` — concurrent chat sessions driver with latency percentiles (mean/p50/p95/max), success/failure counts, per-request CSV output, and a `--concurrency` / `--messages-per-session` knob; drains the full SSE body to measure end-to-end rather than first-byte. Writes `loadtest.csv` for post-hoc analysis. Two new e2e tests. 49 total passing. Deferred: Last-Event-ID proper SSE resume (pending-tx endpoint covers most of the UX gap without the complexity).

- **Agent service — Week 5: rate limits + structured observability:** New `agent-service/app/ratelimit.py` with Redis-backed atomic limiters matching roadmap § 10 — per-session hourly token budget (100k tokens, warn at 80%), per-wallet build-tx rate (5 per 10s burst, keyed by blake2b-16 of the first used_address to avoid leaking raw addresses into Redis keys), per-session injection strikes (3 per hour → temp block), per-IP session-creation cap (10 per hour). Each limiter exposes `check_*` (non-mutating) and `consume_*` (atomic `INCRBY` + lazy `EXPIRE NX`, which requires Redis 7 — already pinned in docker-compose). New `agent-service/app/observability.py` — per-turn `trace_id` via `ContextVar` (`{session_prefix}:{turn_suffix}`, both 8-hex), JSON log formatter on the root logger (so `langchain-openai`, `httpx`, and all sub-loggers emit JSON with `trace_id` inherited via task context), `event(name, **fields)` helper for structured event lines. `/chat` rewritten to: (1) reject up front with SSE `rejected` event on injection-strike block OR sanitizer rejection (strike counter incremented); (2) pre-LLM token budget check (hard reject with SSE `rejected` if hourly cap exhausted); (3) tag every SSE event with `id: {trace_id}` per SSE spec + `trace_id` in the JSON payload for reconnect support; (4) during streaming, compute per-turn token usage from AIMessage `usage_metadata.total_tokens`, per-wallet build-tx rate-limit checked at `tx_action` emission time (emits `rate_limited` SSE event instead of `tx_action` on overflow); (5) after turn, `INCRBY` the hourly token counter and include `tokens_used_*`, `token_budget`, `near_cap` in the final `done` event + a `chat.done` log line. Events emitted: `service.startup`, `chat.start`, `chat.rejected`, `chat.build_tx.rate_limited`, `chat.tx_action`, `chat.error`, `chat.done`. Unit tests in `tests/test_ratelimit.py` (6 tests covering budget, burst, strikes, per-IP independence) + `tests/test_observability.py` (4 tests covering trace_id format, ContextVar binding, uniqueness, JSON wire format). **Deferred for future sessions:** LangSmith integration (auto-detected via `LANGCHAIN_TRACING_V2=true` + `LANGCHAIN_API_KEY` env vars — no code change needed), OpenTelemetry collector wiring (requires Jaeger/Tempo running). 47 agent-service tests pass.

- **MCP server — accept bech32 addresses in `build_*_tx` tool args (not just hex):** Added `decodeAddress`, `requireAddress`, `optionalAddress`, `requireAddressList` helpers to `MCPServer.Tools.Common`. Each tries bech32 via `GeniusYield.Types.addressFromTextMaybe` first and falls back to the default Atlas `GYAddress` `FromJSON` instance (hex bytes via `RawBytesHex`). Wired into `MCPServer.Tools.Transactions.decodeUserAddresses` (for `used_addresses`, `change_address`) and `decodeRecipient`. Schema descriptions in `userAddressFields` updated from "bech32" to "bech32 preferred, hex accepted" to reflect the actual behavior. Motivation: the previous schema documented bech32 but the decoder only accepted hex, so LLM-generated `addr_test1…` values were rejected with an opaque `RawBytesHexErrorBase16DecodeFail ... invalid character at offset: 3` error. Caught during Week 4.5 end-to-end testing on preview testnet. All 103 tests pass. Existing hex-passing callers continue to work unchanged. Requires rebuilding `mariusgeorgescu/bjj-mcp-server` docker image for the running stack to pick up the fix — see `Dockerfile.mcp-server`.

- **Agent service — Week 4.5: wallet integration test harness + pycardano signing:** New pytest harness under `agent-service/tests/` — 37 tests covering (1) pure-function middleware (sanitizer regex + two-qualifier injection variants like "ignore all previous instructions", output-filter secret redaction for `sk-*` / bearer / api_key patterns, reflection tool-call validation for hex IDs + belt names + promotion authority, response grounding for fabricated hex IDs in prose, tx-bridge `is_write_tool` / `extract_cbor_hex` detection across list-of-blocks and raw-string content shapes), (2) Redis-backed checkpoint roundtrip (`put` / `get` / `delete` with TTL + auto-skip if Redis unreachable via `monkeypatch.setattr(settings, "redis_url", ...)`), and (3) HTTP + SSE integration against the live service (`/health`, `/ready`, `/submit-tx` 400/410 paths, session wallet-context roundtrip, injection-rejection SSE event stream). Sanitizer regex hardened: `ignore\s+(?:all|previous|above|prior|earlier)[\s\w]*\binstructions?\b` and `disregard\s+(?:the|your|all)[\s\w]*\b(?:prompt|instructions?|rules)\b` now handle multi-qualifier variants (e.g. "ignore all previous instructions") that the previous single-qualifier pattern missed — regression caught by `tests/test_e2e.py::test_chat_rejects_injection_before_calling_llm`. Settings: `openai_api_key` is now defaulted to empty string so test collection doesn't require a live key. New `tests/signing.py` + `tests/test_signing.py` — pycardano-based CIP-30 signing helper: `load_signing_key(path)` parses Shelley-format `*.skey` JSON (`cborHex` field), `sign_tx_cbor(cbor_hex, sk)` parses the unsigned transaction, hashes its body, and emits a hex-encoded `VerificationKeyWitness` suitable as `awasTxWit` on Interaction API `/submit-tx`. Signing unit tests use pycardano-generated ephemeral keys and a synthetic but CBOR-valid transaction fixture (requires a non-None `TransactionWitnessSet`, per pycardano); deterministic, no external state. New `scripts/sign_and_submit.py` — runnable helper that takes a `tx_action` SSE payload (`session_id`, `tx_body_hash`, `cbor_hex`) + a local `*.skey`, signs via `tests.signing`, and POSTs `/submit-tx`. pycardano added to `[dev]` deps (>=0.11). README gains a Test suite section + Week 4.5 end-to-end signing recipe. Tracker updated: Week 4.5 marked "harness scaffold done", end-to-end funded-wallet submission blocked only on real testnet UTxOs.

- **Agent service — Week 4 backend: wallet bridge + checkpoint + submit-tx endpoint:** New Redis-backed checkpoint store in `agent-service/app/checkpoint.py` — keyed by `checkpoint:{session_id}:{tx_body_hash}` where `tx_body_hash` is a blake2b-16 digest of the CBOR hex (good enough for idempotency; not the canonical tx hash), 10-minute TTL matching roadmap § 8.1. Tx-body detection helpers in `agent-service/app/middleware/tx_bridge.py` — `is_write_tool()` matches `build_*_tx` naming; `extract_cbor_hex()` parses MCP's content-block list shape, tolerates both list and string inputs. `agent-service/app/interaction_client.py` wraps Interaction API `/submit-tx` with the `{awasTxUnsigned, awasTxWit}` payload and optional HTTP Basic Auth. Session store (`agent-service/app/sessions.py`) gains `save_wallet_context` / `load_wallet_context` — `wallet_addresses` provided on `/chat` call (`used_addresses`, `change_address`, `reserved_collateral`, `recipient`) persisted under `session:{session_id}:wallet` with the same 30-day TTL; can be read back via new `GET /session/{session_id}/wallet` for widget context restoration. `/chat` endpoint now detects tool messages carrying `cbor_hex`, calls `checkpoint.put` to record the pending transaction, and emits a new `tx_action` SSE event with `{session_id, tx_body_hash, tx_body_cbor, tool_name}` so the client widget can pop a confirmation card and trigger CIP-30 signing. Deduplication via a per-turn `seen_cbor` set prevents double-emission if the same tool result is yielded twice. New `POST /submit-tx` endpoint: validates the checkpoint still exists (returns 410 with "signing window expired or tx not found" otherwise), forwards the `(tx_body_cbor, witness_cbor)` pair to Interaction API, deletes the checkpoint on success, surfaces upstream 4xx/5xx as 502 with sanitized detail. Config bumps: new `INTERACTION_API_URL` (default `http://interaction-api:8082`) and optional `INTERACTION_API_USER` / `INTERACTION_API_PASS` for upstream Basic Auth. Docker Compose updated: `agent-service` now injects `INTERACTION_API_URL=http://interaction-api:8082`. **Architectural note:** agent loop does NOT block for signing — the turn completes normally (agent responds with a summary like "I built the tx for you to sign"), the checkpoint only exists to validate the signed witness when it arrives via `/submit-tx`, and the wallet interaction happens client-side asynchronously. This matches the "checkpoint-based, stateless" mode from roadmap § 8.1 and sidesteps the LangGraph Checkpointer serialization open decision — no full agent state needs serializing. **Open decisions resolved:** (1) Checkpoint durability — Redis RDB+AOF already configured since Week 2b; dual-write to PostgreSQL deferred unless zero-data-loss SLO required; (2) Checkpointer serialization — non-issue under this design. Known gaps carried to Week 4 continuation: wallet-address auto-injection middleware (client must pass `wallet_addresses` per `/chat` call until then), Last-Event-ID SSE reconnect (not needed until React widget exists), React chat widget itself (separate `bjj-frontend` repo), end-to-end tx test (deferred to Week 4.5 wallet harness with testnet burner UTxOs).

- **Agent service — Week 3: middleware + reflection + session store, migrated to LangChain 1.0 `create_agent`:** Switched `agent-service/app/main.py` from `langgraph.prebuilt.create_react_agent` to `langchain.agents.create_agent` ahead of Week 4's tx-bridge work, which requires explicit middleware hooks (`after_model`, `wrap_tool_call`). Two middleware classes ship: `ReflectionMiddleware` (`app/middleware/reflection.py`, rule-based — hex-ID validation, belt-name validity, promotion-authority table, response grounding check; retry counter via `ContextVar` for per-request scoping; max 2 retries, after which bad responses pass through) and `OutputFilterMiddleware` (`app/middleware/output_filter.py`, regex redaction of API keys, bearer tokens, `api_key=...` patterns). Both implement synchronous `after_model(state)` returning `{"messages": [...]}` or `None` (LangChain 1.0 middleware contract; the hook is NOT awaited, signature omits the `response` arg, and `None` produces stream chunks like `{"MiddlewareName.after_model": null}` that consumers must guard against). `BJJ_SYSTEM_PROMPT` in `app/system_prompt.py` instructs the LLM to re-query for fresh IDs on anaphoric references ("the first one") rather than fabricate — eliminates the most common hallucination failure mode observed during Week 2b testing. Redis session store in `app/sessions.py`: 30-day TTL, 50-message cap (per roadmap § 9), pipelined writes, `redis.asyncio` client. Input sanitizer (`app/middleware/sanitizer.py`) runs as a pre-agent check in `main.py` (not a middleware — rejection needs to surface as an SSE `rejected` event, not an exception); regex patterns for injection detection + 4000-char limit. SSE event stream now wraps `agent.astream` in try/except, emits `error` event on failure, always emits `done` with `error` field, and guards against null node states from no-op middleware. `mcp.py` simplified: removed tool-wrapping reflection (moved to middleware); retained `_normalize_schema` (OpenAI zero-param tool compat) and `handle_tool_error=True` (surfaces upstream MCP errors as tool-result messages for LLM self-correction). Dependency bumps: `langchain>=1.0`, `langchain-openai>=0.3`, `langgraph>=0.6`. Smoke tests pass: multi-turn session persistence, hex-ID grounding, injection rejection, graceful error surfacing. Tracker updated with framework-migration notes + API quirks for Week 4 reference.

- **Agent service — Week 2b skeleton:** New Python service at `agent-service/` (port 8090) — FastAPI + SSE, LangGraph ReAct agent wired to OpenAI via `langchain-openai` and to the BJJ MCP server via `langchain-mcp-adapters` (`MultiServerMCPClient`, `streamable_http` transport). Modules: `app/config.py` (pydantic-settings env vars: `OPENAI_API_KEY`, `OPENAI_MODEL`, `OPENAI_BASE_URL`, `MCP_SERVER_URL`, `REDIS_URL`, `PORT`, `READINESS_TIMEOUT_MS`), `app/llm.py` (ChatOpenAI factory), `app/mcp.py` (MCP client + tool loader with `_normalize_schema` post-processor that injects `properties: {}` into object schemas missing it — required because OpenAI rejects zero-param MCP tools like `get_belt_frequency`; handles both JSON-schema dicts and Pydantic model classes, recurses through `items`/`additionalProperties`/`anyOf`/`oneOf`/`allOf`), `app/health.py` (concurrent `/ready` probe of OpenAI `/models` + MCP `/health`, worst-case `max(q, i)` latency), `app/main.py` (FastAPI app with `lifespan` preloading tools + agent, endpoints: `GET /health`, `GET /ready`, `POST /chat` streaming LangGraph `updates` via `sse-starlette`). Dockerfile (`python:3.12-slim`, `pip install .`), `.dockerignore`, `.env.example`, README. Docker Compose updated: new `redis` service (Redis 7 alpine with AOF + RDB persistence, volume `redis-data`) and new `agent-service` service (depends on `mcp-server` and `redis` health, exposes 8090, healthcheck on `/health`). Smoke test passes end-to-end: user message → OpenAI tool selection → MCP → Query API → streamed markdown response. **Architectural pivot:** replaced the originally-planned on-prem vLLM inference server (Llama 3.3 70B bake-off) with OpenAI API for Phase 1 — trades data sovereignty for faster MVP, fewer hardware dependencies, and trivial model swaps via `OPENAI_MODEL` env var. `OPENAI_BASE_URL` override preserves a path back to on-prem if sovereignty becomes required. New `bjj-chatbot-implementation-tracker.md` at repo root — living execution doc with revised 6-week timeline, decisions log, and per-week task checklists; original roadmap + ADR + Redis docs stay as reference material.

- **MCP Server — Phase 2 complete, documentation updated:** New `mcp-server-lib` library (Haskell, Servant-based) + thin `mcp-server` executable expose Query API and Interaction API as MCP protocol tools for AI clients. Library split (`src/lib/mcp-server-lib/` + `src/exe/mcp-server/Main.hs`) enables reuse across tests and future consumers. New exposed modules: `MCPServer.{App,Clients,Orphans,Readiness,Resources,Schema,Server}`, `MCPServer.Api.{Query,Interaction}`, `MCPServer.Tools.{Achievements,Common,Memberships,Profiles,Promotions,Search,Transactions}`. Tool inventory: 5 read (profiles, promotions, achievements, search) + 4 write (promotions, achievements, memberships) = 17 tools; write tools gated on `MCP_ENABLE_WRITE_TX`. Two MCP resources: `bjj://rules/annex-3` (embedded markdown via `file-embed`), `bjj://rules/belt-hierarchy` (generated from `BJJBelt` enum). Readiness probe (`/ready`) concurrently checks both upstreams. Security posture documented: MCP server is unauthenticated (deploy behind private network or auth proxy); real tx security lives in wallet signature + onchain validators. Cabal updated with library + executable entries; `extra-source-files` includes `docs/mcp-resources/annex-3.md` for sdist builds. New 5th executable, 5th library (previous: 4 exes, 4 libs).

- **Architecture documentation — plan split into ADR + roadmap:** Original `bjj-chatbot-architecture-plan.md` split into three documents: (1) `bjj-mcp-server-adr.md` — as-built ADR for the shipping MCP server (layout, tool inventory, resources, readiness, security posture, no-code-changes guarantee to existing APIs); (2) `bjj-chatbot-roadmap.md` — forward roadmap for agent service + chat UI (inference server bake-off, middleware pipeline, tx-bridge checkpoint semantics with Redis durability decision, rate limiting with per-turn cost cap, privacy/retention, observability, 6-week timeline with week 3 pre-checkpoint validation gate, week 4.5 wallet integration test checkpoint, week 6 finality closure mechanism); (3) `bjj-chatbot-architecture-plan.md` (current file) → short index pointing to the two new docs. Index-based structure preserves external references while making it clear what's built vs. pending. Added `bjj-chatbot-sequence-diagram.md` (Mermaid diagram of promotion flow across 9 components: Chat UI, Agent, vLLM, MCP, Query API, Interaction API, Wallet, Blockchain, Redis) with latency budget, failure paths, and security checkpoints. Added `bjj-redis-architecture.md` (detailed breakdown of Redis role: session history, tx-bridge checkpoint, rate limiting; why Redis not PostgreSQL/in-memory; component-by-component usage; durability options — RDB+AOF, Sentinel, Cluster; memory estimates ~6MB for 1000 users; TTL cleanup strategy).

- **CLAUDE.md + src/exe/CLAUDE.md updated:** Bumped executable count from "three executables (APIs) + admin CLI" to "four HTTP servers + admin CLI" (5 total). Added `mcp-server` (8085) to executables list. Updated library layering to include `mcp-server-lib` (outermost, consumed only by `mcp-server` executable). New § MCP Server in `src/exe/CLAUDE.md` with transport details, upstreams, write-tool gating, resources, readiness, and checklists for adding new tools/resources.

- **Architectural improvements (design review):** Applied 7 improvements from architecture review: (1) Tx-bridge state durability — Redis checkpoint persistence (RDB/AOF) or PostgreSQL fallback decision required before week 4 implementation. (2) Session state serialization contract — week 3 pre-checkpoint validation gate confirms `LangChain 1.0` `Checkpointer` works with full agent state (message history + tool results + reflection outputs); fallback to custom serializer or server-held loop if needed. (3) Agent service health/readiness — `/health` (liveness) and `/ready` (vLLM + MCP reachable) endpoints required for safe orchestration. (4) Tool-call schema compatibility — inference server bake-off must validate JSON schema compatibility between models (Llama 3.3 vs. Qwen 2.5 vs. Llama 4 Scout); commit matrix + decision to repo. (5) Wallet integration testing — new week 4.5 checkpoint for end-to-end SSE→wallet→submit flow validation before observability work. (6) SSE reconnection protocol — explicit `:id: {trace_id}` requirement on events so browser can unambiguously resume from checkpoint. (7) Reflection retry circuit breaker — per-turn cost cap (>50% of session budget → stop retrying, surface "try simpler request") prevents adversarial prompts from burning hourly budget in one turn.

- **Refactor — DTO consolidation for cross-executable reuse:** Promoted query-api response DTOs, order-by enums, and filter constructors out of the executable's `other-modules` into new `offchain-lib` modules so they become a single source of truth shared across the project. Three new `offchain-lib` exposed modules: `DomainTypes.Transfer.QueryResponses` (29 response types + 8 domain→response converters, moved from `Query.ApiTypes`), `DomainTypes.Transfer.OrderBy` (`SortOrder`, `ProfilesOrderBy`, `PromotionsOrderBy`, `AchievementsOrderBy`, `MembershipHistoriesOrderBy`, `MembershipIntervalsOrderBy`, `ActivityEventType`, `WalletAddress`, moved from `query-api/Types.hs`), and `DomainTypes.Transfer.Filters` (six filter record types + six filter-from-params constructors, moved from `Query.Common` and `RestAPI.Common`). `mkStripPrefixSchemaOptions` promoted from `webapi-lib/WebAPI.Utils` to `offchain-lib/Utils` so offchain-lib modules can reference it without a layering violation; `interaction-api/ServiceRequests.hs` import updated accordingly. `deriveThumbnailUri` (previously `Query.ThumbnailURI`) inlined as a one-liner in `QueryResponses`. Pure module-move refactor — no API behavior change, no Swagger schema change, no endpoint contract change; all 83 tests pass. Deleted files: `src/exe/query-api/Query/ApiTypes.hs`, `src/exe/query-api/Query/ThumbnailURI.hs`, `src/exe/query-api/Types.hs`. `Query.Common` retains only pagination/ordering helpers (`normalizeLimitOffset`, `applyLimits`, `normalizeOrder`, `applyFilterOrderLimit`, `beltIsMasterCapable`); `RestAPI.Common` retains only `withBackend`.

- **Achievement `other_metadata` support:** Achievements now carry arbitrary key-value metadata pairs alongside the standard CIP-68 fields (name, description, image). End-to-end: new `AchievementOtherMetadataJson` newtype and `achievementOtherMetadata :: [(Text, Text)]` field on `Achievement`. `onchainAchievementToAchievement` extracts non-standard CIP-68 metadata pairs via `cip68DatumOtherMetadataPairs`. `awardAchievementTX` validates that `other_metadata` keys do not collide with reserved CIP-68 keys (`name`, `description`, `image`), raising `AchievementOtherMetadataReservedKeys` (HTTP 400). `AchievementProjection` gains an `otherMetadata` column. `AchievementResponse` and `AchievementInformationResponse` expose the field in the Query API. Two new unit tests: success path with custom metadata and reserved-key rejection. Custom `FromJSON Achievement` instance defaults `other_metadata` to `[]` for backward compatibility.

- **API version bumps:** Interaction API `2.0.0` → `2.1.0`, Query API `3.0.0` → `3.1.0`.

- **Validity window fix:** Fixed validity window when new end date is not after now.

- **Docker Compose:** Image tags bumped to `69159d5`.

- **Code formatting:** Fourmolu-style reformatting across interaction-api, query-api, offchain-lib (indentation, import order, blank lines).

- **Generalize update profile**: `update-profile-image` endpoint renamed to `update-profile`. Supports updating description and image in a single transaction. Name is immutable after minting. **Breaking**: `ProfilesRedeemer` changed (`UpdateProfileImage` → `UpdateProfile MetadataFields Integer`), requires validator redeployment. Interaction API version bumped to 2.0.0, blueprint version bumped to 2.0.0.

- **Query API — pending-actions name enrichment:** `GET /pages/pending-actions/:profileId` now returns `awarded_by_name` and `organization_name` fields — JSON objects mapping asset-class strings to display names. Awarder names are batch-resolved via `resolveProfilesBatch` (handles both practitioner and organization awarders). Organization names are resolved via `buildOrgMaps` from membership intervals. `resolveProfilesBatch` newly exported from `Query.Aggregates`.

- **Onchain — INLINEABLE Enum BJJBelt:** Added `{-# INLINEABLE #-}` pragmas to all `Enum BJJBelt` methods (`succ`, `pred`, `toEnum`, `fromEnum`, `enumFromTo`, `enumFromThenTo`) for on-chain compilation.

- **Tests — property test fix for strict promotion:** Added `genBeltNotMax` generator (excludes `Red10`) for `prop_sameBeltPromotionFails` and `prop_downgradePromotionFails`, since `succ Red10` correctly errors on-chain. New unit test `updateProfileWithDescriptionTest` exercises `UpdateProfileAction` with both description and image. Existing tests renamed to match `UpdateProfile` API.

- **Offchain — updateEndDateTX org priority fix:** When updating a membership interval end date, if the caller holds both an organization User NFT and a practitioner User NFT, the organization NFT now takes priority. Previously the `(True, False)` match required the caller to *not* hold a practitioner NFT, which would fail for org admins who are also practitioners. Changed to `(True, _)`.

- **Scripts — populate_testnet `--force-redeploy`:** `populate_testnet.sh` now accepts `--force-redeploy` to delete `config_bjj_validators.json` and redeploy reference scripts from scratch. Without the flag, existing config is reused as before.

- **Docker Compose:** Image tags bumped to `0917ea2`.

- **Docs:** CR-UpdateProfile status updated to Implemented. OnchainArchitecture, OnchainSecurityAudit, frontend-validation-requirements, onchain-trace-codes, and README updated to reflect `UpdateProfileImage` → `UpdateProfile` rename throughout.

## 0.3.3.5 -- 2026-04-09

- **Interaction API — tx-status polling endpoint:** New `GET /tx-status/:txId` endpoint polls whether a submitted transaction has been confirmed on-chain. Returns `TxStatusResponse` with `tx_id` and `confirmed` fields. Uses `gyAwaitTxConfirmed` with a single-attempt check (100ms delay, 1 confirmation). New `FromHttpApiData` and `ToParamSchema` orphan instances for `GYTxId`. Swagger auto-generated.

- **Onchain — strict sequential promotion validation:** Changed `validatePromotion` to require the student's next belt to be exactly `succ` of the current belt, replacing the previous `>` check. Prevents skipping belts during promotion.

- **Docker Compose — IPFS and image updates:** IPFS container switched to explicit `ipfs init --profile=server` + `ipfs daemon --enable-gc` entrypoint with `Gateway.NoFetch true`. Added UDP QUIC swarm port. Image tags bumped to `0d4280f`. Kupo URL sourced from `${KUPO_URL}` env var instead of hardcoded.

- **Chain-sync — Kupo URL default update:** Updated default Kupo URL in chain-sync service to use `dmtr.host` domain with explicit port 443.

- **Query API — ThumbnailURI cleanup:** Removed unused `OverloadedStrings` pragma from `ThumbnailURI.hs`.

- **Docs:** Added `CR-UpdateProfile.md` (change request for generalizing update profile endpoint) and `frontend-validation-requirements.md` (form validation rules derived from on-chain validators).

## 0.3.3.4 -- 2026-04-09

- **Query API — fix current rank in batch profile loading:** Fixed `getPractitionerProfilesBatch` returning the oldest rank as `current_rank` instead of the newest. `M.fromListWith (++)` reversed the SQL ASC ordering because `fromListWith` calls `f newValue oldValue`, flipping the concatenation. Changed to `M.fromListWith (flip (++))` to preserve chronological order. This caused promotions page, dashboard, explorer, and any batch-loaded practitioner profile to display the registration belt (e.g. White) instead of the actual current belt.

## 0.3.3.3 -- 2026-03-28

- **Storage — GYTime column type fix:** Changed `PersistFieldSql GYTime` from `SqlString` (varchar) to `SqlDayTime` (timestamptz) and `toPersistValue` from `PersistText` to `PersistUTCTime`. Fixes `operator does not exist: character varying >= timestamp with time zone` errors on dashboard and any query using date-range filters or `NOW()` comparisons on GYTime columns (achievement_date, start_date, end_date, rank/promotion achievement dates). `fromPersistValue` retains `PersistText` handling for backward compatibility with legacy rows. Requires auto-migration to `ALTER COLUMN ... TYPE timestamptz`; legacy double-quoted rows should be cleaned first.

## 0.3.3.2 -- 2026-03-28

- **Query API — pending-actions endpoint:** New `GET /pages/pending-actions/:profileId` returns pending promotions, unaccepted achievements, and unaccepted membership intervals for a profile (My Dojo inbox). Builds hardcoded filters for each entity type and dispatches through `withBackend`. New response types: `PendingActionsPromotionResponse`, `PendingActionsResponse`.

- **Query API — profiles by wallet address:** New `GET /profiles/by-wallet/:bech32addr` looks up profile User NFTs held at a wallet address and returns the corresponding profiles. Live-only (no projected storage for wallet→profile mapping). New `WalletAddress` newtype with `FromHttpApiData`/`ToParamSchema`/`ToSchema` instances.

- **MembershipIntervalFilter — `isAccepted` support:** Added `membershipIntervalFilterIsAccepted :: Maybe Bool` to `MembershipIntervalFilter`. Applied in both Esqueleto (projected) and in-memory (live) filter paths. `membershipIntervalFilterFromParams` updated to accept the new parameter.

- **Onchain CIP68 — reverse derivation:** Added `deriveRefFromUserTN` and `deriveRefFromUserAC` to derive reference NFT token names/asset classes from user NFTs (reverse of existing `deriveUserFromRef*`). Exported `userTokenPrefixBS`.

- **Offchain Lookups — `getProfileRefACsAtAddress`:** New function in `TxBuilding.Lookups` scans UTxO values at given addresses for CIP-67 user-prefixed tokens and returns the corresponding profile reference asset classes.

## 0.3.3.1 -- 2026-03-27

- **Docker build refactoring:** Extracted shared runtime base image (`Dockerfile.runtime-base`) with crypto libs (libsodium, secp256k1, BLST), eliminating ~70 lines of duplicated runtime setup from each service Dockerfile. `Dockerfile.base` now only caches dependencies; source copy moved to per-service Dockerfiles for better layer caching. `build-images.sh` updated to build `bjj-runtime-base`. Docker Compose image tags bumped to `5dc37e8`.

- **Query API — promotions page date/state filters:** `GET /pages/promotions` gains `from`, `to` (date range) and `state` (repeatable) query params, threaded through `RestAPI.hs` → `Pages.hs` → filter construction. Swagger updated.

- **Query API — dashboard simplification:** Removed `total_accepted_promotions` from `DashboardPageResponse` and the `totalBelts` query that backed it.

- **Query API — live backend `registered_at` ordering:** Live backend now fetches registration timestamps from projected DB (`ProfileProjection.insertedAt`) instead of falling back to ID ordering for `order_by=registered_at`.

- **Storage — GYTime PersistField:** Custom `PersistField GYTime` instance stores ISO8601 text directly instead of JSON double-quoting via `derivePersistFieldJSON`. Handles legacy double-quoted data with `stripQuotes`.

- **Storage — rollback and org resolution:** Extracted `rollbackEntity` helper to deduplicate 7 pairs of `deleteWhere` calls in `rollbackTo`. `resolveOrganizationForInterval` now filters by practitioner ID in SQL instead of loading all history rows.

- **Code quality:** Formatting fixes (list comprehension indentation), removed unused pragmas, `fromRight` over `either (const Nothing) id`, `mapMaybe` over `concatMap . maybe`, Haddock comments on Storage functions.

- **Docs:** DeveloperGuide updated with current Persistent entities, operations table, query modules, and code examples. Documentation.md typo fix. CLAUDE.md and onchain-lib CLAUDE.md updated (Plutus V3 version note, query endpoint file references).

## 0.3.3.0 -- 2026-03-26

- **Query API — unified promotions (breaking):** Removed standalone `GET /belts` endpoints. Promotions now return all states: **pending**, **accepted** (from `RankProjection`), and **superseded**. New query params: `state` (repeatable), `from`, `to` (date range). Self-promotions (awarder == recipient) are excluded. `rankToPromotion` converts accepted ranks to the `Promotion` domain type. Removed `RanksOrderBy`, `RankFilter`, `rankFilterFromParams`; ranks are queried via `applyPromotionFilterOnRank`. Post-merge ordering and limit/offset applied in Haskell.

- **Query API — activity feed:** New `GET /activity` endpoint returns a reverse-chronological feed across profiles, promotions, achievements, and memberships. Supports `event_type` and `actor` query params. `ActivityEventType`, `ActivityEventResponse`, `ActivityEventDetails` types in `Query.ApiTypes`; handlers in both projected and live backends.

- **Query API — profiles `registered_at` ordering:** `GET /profiles` and `GET /profiles/count` accept `order_by=registered_at` (`ProfilesOrderByRegisteredAt`), sorting by `ProfileProjection.insertedAt`. `ProfileFilter` gains `profileFilterRegisteredAfter` for future use.

- **Docker Compose:** Images pinned to `d7ad3ea`; Kupo URL updated to `dmtr.host`; batch sizes reduced (1M/100K); `ATLAS_CORE_CONFIG` and `DEPLOYED_VALIDATORS_CONFIG` env vars forwarded to all services.

- **Service probes:** `alwaysHealthy`/`alwaysReady` replaced by `mkProbeStatus` in `WebAPI.ServiceProbe`; health endpoint now reports `appVersion` from `Constants`. Haddock documentation added across `WebAPI.Auth`, `WebAPI.CORS`, `WebAPI.ServiceProbe`.

- **Chain-sync hardening:** Replaced `head` with pattern match and `error` with safe fallback in `ChainSyncLogic`; `batch_size` renamed to `batchSize` (camelCase); Haddock module and function docs added throughout `ChainSyncLogic` and `ChainsyncAPI`.

- **Code quality:** `Show` instances for domain types (`Rank`, `Promotion`, `MembershipHistory`, `MembershipInterval`, `Achievement`) use `Data.List.Extra.intercalate` instead of `init . unlines`. `PromotionState` gains `FromHttpApiData` instance for query param parsing.

## 0.3.2.0 -- 2026-03-24

- **Query API — protocol-status script hashes:** `GET /protocol-status` now includes **`script_hashes`** (`ScriptHashesDTO`): hex script hashes for all six deployed scripts (minting policy, profiles/ranks/memberships/achievements/oracle validators). Extracted from `DeployedScriptsContext` in `Query.ServiceStatus`. `backend-api-requirements.md` §8 updated with JSON shape and field documentation.

- **Query API — code quality refactoring:**
  - **Eliminated duplication in `Query.Projected`:** Extracted `toRankDomain`, `toPromotionDomain` (replaced 5 identical inline conversion functions) and `likePat` (replaced 10 inline SQL LIKE pattern constructions).
  - **Reduced `ToSchema` boilerplate in `Query.ApiTypes`:** Extracted `mkStripPrefixSchemaOptions` helper — all 20 `ToSchema` instances are now single-line declarations (~80 lines removed).
  - **Improved filter performance in `Query.Live`:** Extracted `filterBySet` helper using `Data.Set.member` (O(log n)) to replace 15 list `elem` calls (O(n)) in all filter functions. Extracted `frequencyOf` to unify 3 identical frequency-counting functions.
  - **Optimized frequency queries in `Query.Projected`:** `getBeltTotals`, `getProfileTypeTotals`, and `getPromotionBeltTotals` now use SQL `GROUP BY` + `COUNT` instead of loading all rows into Haskell and counting in-memory.
  - **Unified live/projected dispatch in `Query.Aggregates` and `Query.Pages`:** Replaced ~40 inline `if live then L.xxx else P.xxx` branches with `withBackend` from `RestAPI.Common`.

- **Query API — search filtering improvements:**
  - **Unified search — profile name matching:** `GET /search` now matches rank and promotion results against achievedBy/awardedBy **profile names** (not just raw asset class IDs). Projected backend uses LEFT JOIN to `profile_projection` (`searchRanksWithNames`, `searchPromotionsWithNames`); live backend post-filters with `rankMatchesSearchWithNames`/`promotionMatchesSearchWithNames` against the loaded profile name map.
  - **Unified search — achievement search fields:** Achievement text search (`q`) now matches `achievement_id`, `awarded_to_profile_id`, and `awarded_by_profile_id` in addition to name and description — consistent with rank/promotion/membership `q` semantics. Projected SQL uses `unsafeSqlCastAs "text"` `LIKE`; live uses shared `achievementMatchesSearch` helper.

- **Query API — lineage endpoint redesign (breaking):** `GET /lineage` now takes `ancestors` and `descendants` parameters (replacing single `depth`). Returns a directed lineage tree: ancestor chain follows `awarded_by` links upward (no server cap), descendant subtree follows `awarded_to` links downward (capped at 10). Collateral branches (other students of root's instructor) are excluded. Response shape (`LineageGraphData`) is unchanged. `backend-api-requirements.md` §10 rewritten with functional definition of BJJ lineage.

- **Query API — search, aggregate, and lineage improvements:**
  - **Unified search (`GET /search`):** Rank and promotion search hits now display profile **names** in title and subtitle (batch profile name lookup via `batchProfileNames`) instead of raw asset class hex strings. Each search group caps `items` at **10** (configurable via `searchGroupLimit`); `total` still reflects the full untruncated count.
  - **Practitioner explorer page:** Batch-loads practitioner profiles in **2 SQL queries** (`getPractitionerProfilesBatch`) instead of N individual lookups. Organization maps (`buildOrgMaps`) use a single batch query (`getOrganizationProfilesBatch`) for the projected backend.
  - **Lineage (`GET /lineage`):** Rank edges now filtered in SQL (`WHERE belt >= minBelt`) instead of loading all ranks into memory. Depth is capped at **10** (`maxLineageDepth`). Nodes referencing profiles that no longer exist in the database are omitted from the response rather than emitting placeholder nodes with empty names.
  - **Bug fixes:** Live backend profile filters (`active_membership_organization`, `membership_organization`, `belt`) were silently ignored — now implemented. Fee address in `GET /protocol-status` serialized via `addressFromPlutus'` + `addressToText` (bech32) instead of Haskell `show`. `searchProjected` uses SQL-filtered queries for ranks/promotions instead of loading all rows. `resolveProfileForPromotionSide` and `loadPractitionerOrOrg` wrap fallback organization lookups in `try` to return `ProfileNotFound` instead of uncaught 500 errors.
  - **`backend-api-requirements.md`:** §12 JSON examples updated to snake\_case (matching `StripPrefix + CamelToSnake` convention); §13.1 row added; §10 lineage depth cap documented; search enrichment noted in implementation status.

- **Query API — §12.1 dashboard and §12.4 belts page:** **`GET /pages/dashboard`** (`DashboardPageResponse`: totals, belt frequency, recent promotions, monthly histogram, latest orgs/practitioners) with optional **`recent_promotions_limit`**, **`latest_organizations_limit`**, **`latest_practitioners_limit`** (default **10**); **`GET /pages/belts`** (`BeltsPageResponse`: enriched **`RankInformationResponse`** rows, total, frequency) with the same query params as **`GET /belts`**. **`rankToInformation`** in **`Query.Aggregates`**. Swagger **1.5.0**. **`backend-api-requirements.md`** §12.1, §12.4, §12.5 table, §10 lineage note, §13.2, §14.
- **Query API — practitioner explorer:** single batched membership-history query per page (then grouped per practitioner) to reduce round-trips.
- **Query API — §12.5 explorer page aggregates:** **`GET /pages/promotions`**, **`/pages/achievements`**, **`/pages/profiles`**, **`/pages/memberships`**, **`/pages/practitioner-explorer`**, **`/pages/home-explorer-hub`** — list-shaped pages with totals and extras (monthly promotion/achievement stats, profile-type frequency, practitioner rows with org maps, home hub with health/ready/protocol). Handlers in **`Query.Pages`**; shared monthly rollups in **`Query.ExplorerMonthly`** and **`Query.Projected.getMonthlyPromotionStats`** / **`getMonthlyAchievementStats`**; probes/protocol via **`Query.ServiceStatus`**. (Superseded by Swagger **1.5.0** together with §12.1/§12.4 routes above.)
- **Query API — §12.2–12.3 detail aggregates:** **`GET /practitioner/{id}/detail`** and **`GET /organization/{id}/detail`** return **`PractitionerDetailResponse`** / **`OrganizationDetailResponse`** (`backend-api-requirements.md` §12) in one round-trip each (optional **`liveprojection`**). Composition and enrichment live in **`Query.Aggregates`**; wire types and **`PromotionInformationResponse`** in **`Query.ApiTypes`**. **`backend-api-requirements.md`** updated (§12 status, §1.8 `q` + affiliated org).
- **Query API — BJJ frontend alignment (Sprints 1–3):** JSON field names and routes aligned with `backend-api-requirements.md` so PureScript generic decoding works. **Achievement:** `awarded_to_profile_id`, `awarded_by_profile_id`, `achievement_date`, `accepted`, `thumbnail_uri`. **MembershipInterval** (in membership histories): `accepted`, `interval_number`. **Profile** and org/practitioner JSON include `thumbnail_uri` (see next bullet). **Promotion `state`:** `DomainTypes.Core.Types.promotionStateFromBelts` — list/count/frequency return **`pending`** or **`superseded`** by comparing `achieved_by_profile_id`’s current rank belt (latest by date) to the promotion belt; **`accepted`** is not returned on `GET /promotions` (accepted promotions become ranks). Implemented in **`Query.Projected`** (batched rank lookup) and **`Query.Live`** (`getCurrentBeltForPractitioner` in **`TxBuilding.Lookups`**). **Paths:** membership list/count at **`/memberships`** (replacing `membership-histories`). **Query params:** repeatable **`profile`** on belts and promotions (merged into `achieved_by`); **`accepted`** on achievements; **`q`** on every list/count; profile list/count adds **`active_membership_organization`**, **`membership_organization`**, repeatable **`belt`** (projected SQL with subqueries). **New endpoints:** `GET /achievement/{id}` (404 when missing); **`GET /protocol-status`** (oracle UTxO → `op_paused`, `min_utxo_value`, `fee_config`) with **`DeployedScriptsContext`** loaded in query-api (`Main.hs`, `QueryAppMonad.hs`). **Search:** projected mode applies case-insensitive substring SQL on profile and achievement name/description; belts/promotions/memberships accept `q` but projected filters do not narrow rows yet; live projection may not apply all new filters (see `backend-api-requirements.md` implementation status). **Tests:** `tasty-hunit` cases for `promotionStateFromBelts`. **Docs:** `backend-api-requirements.md` updated with implementation status, §13–§14 checklist, and remaining gaps (page aggregates, practitioner `q` + affiliated org, §13.2 `q` semantics where noted).
- **Query API — unified search and lineage (Sprint 4):** **`GET /search?q=...`** returns **`SearchResults`** (§9); **`GET /lineage?root=&depth=&min_belt=`** returns **`LineageGraphData`** (§10), built from rank edges with BFS in **`Query.Projected`** / **`Query.Live`**. **`Query.ApiTypes`** (`ProfileResponse`, `AchievementResponse`, search, lineage) uses **`deriving-aeson`** `CustomJSON` with `StripPrefix` + `CamelToSnake` for snake_case API fields. **Lineage** JSON keys (`profile_id`, `image_uri`, `thumbnail_uri`, `current_belt`, `profile_type`, `is_master_capable`, `awarded_belt`, …) match §1.5 like the rest of the query API. **Swagger** query API version **1.2.0** (`RestAPI.hs`). **`backend-api-requirements.md`:** §10, §13.1 lineage mapping; implementation status refreshed.
- **Query API — `thumbnail_uri` (query-only):** `thumbnail_uri` is derived only in query-api (`Query.ThumbnailURI.deriveThumbnailUri`, `Query.ApiTypes` response types). Chain-sync **`Storage`** projections for profiles and achievements persist **image** URIs only (thumbnail columns removed). **`TxBuilding.Conversions`** and **`Lookups`** no longer set thumbnails. JSON field names for clients are unchanged; **`deriveThumbnailUri`** is identity on `image_uri` until a cache/CDN path is added in query-api. **`backend-api-requirements.md`** updated accordingly. **Build/test:** from repo root, **`direnv allow`** then **`cabal build all`** and **`cabal test`** (Nix/devx shell per README).

## 0.3.1.9 -- 2026-02-21

- **Dynamic min ADA and oracle `opMinUTxOValue`**: Minimum lovelace for protocol state outputs is now taken only from the oracle (`OracleParams.opMinUTxOValue`), not from a fixed constant. MintingPolicy and MembershipsValidator read min lovelace from the oracle; off-chain uses `opMinUTxOValue` for each state output (with optional future use of protocol params and serialized size). Removed `protocolMinLovelace` and `protocolMinLovelaceValue` from `Onchain.Utils`. Admin action `SetMinUTxOValueAction` and CLI `set-min-utxo-value --lovelace N`; initial oracle and tests use `opMinUTxOValue = 1_000_000`. **Breaking**: Oracle datum gains a fourth field (`opMinUTxOValue`); see OnchainArchitecture.md § Oracle datum schema and migration.

## 0.3.1.8 -- 2026-02-21

- Unit tests split into modules (Achievement, Cleanup, Membership, Oracle, Promotion); Test.Fixtures and Test.Helpers added.
- Off-chain membership interval validation: structured exceptions (`AddMembershipIntervalReason`, `CannotAddMembershipInterval`); pre-validation in `addMembershipIntervalTX`.
- `protocolMinLovelace` increased to 10_000_000.

## 0.3.1.7 -- 2026-02-20

- `GET /profiles/count` supports same filters as `GET /profiles`; RestAPI refactor (Common.hs, shared filter helpers).
- `TxBuilding.Functors` → `TxBuilding.Conversions`; `getUtxoWithTokenAtAddresses` → `getUTxOWithTokenAtAddresses`.

## 0.3.1.6 -- 2026-02-17

- **Achievements**: AchievementsValidator (award/accept/cleanup); MintingPolicy `NewAchievement`; full off-chain ops, storage, ingestion; REST `GET /achievements` and `GET /achievements/count` with filters and liveprojection.
- Validators moved under `Onchain/Validators/`; protocol logic extracted to `Onchain/Protocol/Core.hs`. Membership endpoints support liveprojection.

## 0.3.1.5 -- 2026-02-16

- **UpdateEndDate security**: TD/TE/TB enforced on-chain (no longer dead code); off-chain uses `updateEndDateWithoutValidations` so invalid attempts fail on-chain.
- Trace codes made globally unique and consolidated; single `onchain-trace-codes.md` (61 codes).

## 0.3.1.4 -- 2026-02-15

- Scripts: `populate_testnet.sh` and `test_black_promotes_white_to_blue.sh` extended for memberships; `test_exunits.sh` parses membership actions.

## 0.3.1.3 -- 2026-02-15

- **Memberships (off-chain)**: Full flow for histories and intervals (create, add interval, accept); storage, ingestion, REST API, admin CLI. Permissionless `Cleanup` redeemer for dust on Profiles/Ranks/Memberships validators.

## 0.3.1.2 -- 2026-02-15

- **TxBuilding exceptions**: `ProfileException` → `TxBuildingException` (RankListEmpty, PromotionNotFound, OracleDatumInvalid, etc.); HTTP mapping 404/503/400. Added `docs/DeveloperGuide.md`.

## 0.3.1.1 -- 2026-02-15

- Consistency: shared lookups (`getAllParsedDatumsAtValidator`, `profileDatumToProfileData`), `upsertByUnique` in storage; typo fix `getPractitionerInformation`.

## 0.3.1.0 -- 2026-02-15

- Codebase consistency: config via env vars, shared helpers (`txOutRefToV3Plutus`, `pkhFromExtendedSkey`), `runWithTxErrorHandling`, `appVersion`; dead code removed.

## 0.3.0.0 -- 2026-02-15

- **Oracle Hub**: Dynamic protocol parameters (pause, fees, min lovelace, admin) via on-chain oracle; OracleValidator, OracleNFTPolicy, OracleParams/FeeConfig. MintingPolicy reads oracle from reference input. Admin actions (pause/unpause, set-fees, query-oracle) via shared Interaction pipeline.

## 0.2.9.0 -- 2026-02-15

- Protocol split into `Protocol/Types.hs`, `Protocol/Lookup.hs`, `Protocol/Id.hs`. Haskell style: Haddock, export lists, INLINEABLE; `BeltSnapshot` refactor in BJJ.

## 0.2.8.2 -- 2026-02-10

- Script size reporting in test output (bytes and % of 16 KB limit).

## 0.2.8.1 -- 2026-02-10

- Admin CLI `write-blueprint`; Blueprint includes MembershipsValidator and fixes. Bug fix: `unsafeGetProfileDatumAndValue` type (ProfileId not RankId); OnchainSecurityAudit R5 corrected.

## 0.2.8.0 -- 2026-02-09

- **Memberships (on-chain)**: LinkedList, MembershipsValidator; membership history/interval datums and redeemers; init history, add interval, accept flows.
- **Security audit**: Cross-org membership fix (MV checks org User NFT); CurrencySymbol and startDate validation. Redeemer **breaking**: AcceptPromotion no longer has `rankOutputIdx`; new mint redeemers for membership. `promoteProfileDatum` split for PV cost reduction.

## 0.2.7.0 -- 2026-02-01

- **Output index optimization**: O(n) → O(1) output validation via `checkTxOutAtIndex`. **Breaking**: Redeemers now include output indices (CreateProfile, Promote, UpdateProfileImage, AcceptPromotion).

## 0.2.6.0 -- 2026-02-01

- **PlutusV3**: Token ID generation via `blake2b_224` + `integerToByteString`. **Breaking** for new token names. Added `populate_testnet.sh`; script improvements and docs.

## 0.2.5.0 -- 2026-02-01

- **Profile deletion removed** (**breaking**): DeleteProfile, BurnProfileId, deleteProfileTX removed; profiles permanent by design.
- Metadata size limits (name 128, description 1024, imageURI 256 bytes) to prevent DoS. AcceptPromotion redundant check removed.

## 0.2.4.0 -- 2025-02-01

- **Promotion security**: Full BJJ validation at mint; seed TxOutRef for unique promotion IDs; acceptance-time checks (nextBelt > currentBelt, date ordering). RanksValidator simplified to consent-only.

## 0.2.3.0 -- 2025-01-06

- Query API: `GET /profiles/count`, `/profiles/frequency`, `GET /promotions/count`, `/promotions/frequency`; all support liveprojection.

## 0.2.2.0 -- 2024-12-21

- **API split**: Interaction API (build/submit tx) and Query API (profiles, promotions, belts) as separate services; separate Dockerfiles and deployment.

## 0.2.1.0 -- 2024-12-20

- Testnet evidence, automated test script, admin CLI deployment commands; README enhancements.

## 0.2.0.0 -- 2024-12-19

- **Milestone 2**: Smart contract architecture — MintingPolicy, ProfilesValidator, RanksValidator, BJJ.hs; TxBuilding (Operations, Interactions, Lookups, Skeletons); unit and property tests; blueprint and documentation.

## 0.1.0.0 -- YYYY-mm-dd

- First version. Released on an unsuspecting world.
