# API Security Hardening — Design Spec

**Date:** 2026-07-04
**Sub-project:** C (from `FableReview-0407.md`, action-plan rows 7–8, security subset)
**Status:** design approved; adversarially verified (see Verification record); pending plan

## Goal

Close the API/server security perimeter so that no HTTP server starts in an
insecure configuration, no unbounded input can exhaust the server, and no
internal detail leaks to clients. Zero new features — only hardening of the
four HTTP servers (`interaction-api`, `query-api`, `chainsync-service`,
`mcp-server`) and the shared `webapi-lib`.

## Principle

**Fail-closed.** A server refuses to start rather than run with default
credentials or an origin-reflecting CORS policy. Inputs are bounded by
default. Errors return a correct status and a generic body; the detail is
logged server-side only.

## Scope

**In scope** (10 findings):

| Finding | One-line |
| ------- | -------- |
| F-11 | Basic-auth defaults `cardano`/`lovelace` → `die` on missing env |
| F-12 | CORS reflects any Origin with credentials → env allowlist, credentials off |
| F-13 | Postgres DSN (with password) + Kupo URL logged at startup → redact |
| F-14 | query-api throws raw `TxBuildingException` → 500 → mirror error handler |
| F-15 | Unbounded list/`/search` endpoints, unescaped LIKE → clamp + escape |
| F-16 | Unbounded in-RAM multipart uploads → size cap + magic-byte validation |
| F-27 | Exception internals leaked in HTTP error bodies → sanitize |
| F-29 | Basic-auth: non-constant-time compare, throws on invalid UTF-8 → fix both |
| F-30 | interaction-api `/ready` returns 400, duplicates error logic → 503 + share |
| F-32 | Default Kupo URL embeds Demeter instance id → default to localhost |

**Out of scope** (decided during brainstorming):

- **F-28** (MCP server unauthenticated) — left as-is; documented "deploy behind
  auth proxy". Owner decision.
- **F-17, F-24** (limit-applied-before-filter; partial `M.!`) — query
  *correctness* bugs, belong to stream D, not the security perimeter.
- Operational credential **rotation** (Maestro token, Demeter Kupo instance) —
  stream A. This spec changes only *code* (localhost default, redacted logs); it
  does not rotate any live secret.

## Config surface (new / changed environment variables)

| Variable | Server(s) | Behavior |
| -------- | --------- | -------- |
| `BASIC_USER` | interaction-api, query-api, mcp-server (upstream) | **Required.** Missing → `die` at startup. No default. |
| `BASIC_PASS` | interaction-api, query-api, mcp-server (upstream) | **Required.** Missing → `die` at startup. No default. |
| `CORS_ALLOWED_ORIGINS` | all four | Comma-separated origin allowlist. Empty/unset → no cross-origin allowed (fail-closed). |
| `KUPO_URL` | chainsync-service | Default changes from the Demeter URL to `http://localhost:1442`. Remote requires explicit set. |

`allowCredentials` is hard-coded `False` (frontend is same-origin / proxied /
token-based, per brainstorming decision). It is not env-configurable in this
iteration — a future change can add it if in-browser cross-origin basic-auth
is ever adopted.

## Cluster 1 — Auth & startup (F-11, F-29)

**File:** `webapi-lib/WebAPI/Auth.hs`. **New deps:** `bytestring`, `memory`
(both already in the build plan transitively; add to `webapi-lib` stanza).

- `getBasicAuthFromEnv :: IO AuthContext` → `die` (from `System.Exit`) when
  `BASIC_USER` or `BASIC_PASS` is unset. Remove the `fromMaybe "cardano"` /
  `fromMaybe "lovelace"` defaults. Message names the missing variable.
- `authCheck`: compare the **raw** `ByteString` credentials from
  `BasicAuthData` (username, password) using `Data.ByteArray.constEq`
  (constant-time) against the expected values (the expected `Text` is encoded
  to `ByteString` once). Do not `decodeUtf8` the incoming bytes for the
  comparison; if the identity string is needed for `AuthUser`, use
  `decodeUtf8'` and treat `Left` as `Unauthorized` (never a 500).

**Contract:** invalid UTF-8 credentials → `Unauthorized` (401), never a crash.
Wrong credentials and right credentials take the same comparison path length.

## Cluster 2 — CORS allowlist (F-12)

**File:** `webapi-lib/WebAPI/CORS.hs`. **Dep:** `bytestring` (for origin bytes;
`wai-cors` already present).

```haskell
data CorsConfig = CorsConfig
  { allowedOrigins  :: [ByteString]  -- exact Origin header values
  , allowCredentials :: Bool
  }

getCorsConfigFromEnv :: IO CorsConfig   -- reads CORS_ALLOWED_ORIGINS, credentials = False
mkCorsMiddleware     :: CorsConfig -> Middleware
```

- `mkCorsMiddleware` reflects the request `Origin` **only if** it is a member
  of `allowedOrigins`; otherwise it emits no CORS headers (the browser blocks
  the response). `corsMethods`/headers stay as today. `corsVaryOrigin = True`.
- Empty `allowedOrigins` ⇒ no origin ever allowed (fail-closed default).
- `setupCors :: Middleware` (the current zero-arg export) is replaced by
  `mkCorsMiddleware`. Each of the four servers reads `getCorsConfigFromEnv` at
  startup and threads the config into its WAI-app builder
  (`interaction-api/RestAPI.hs:298`, `query-api/RestAPI.hs:710`,
  `chain-sync/ChainsyncAPI.hs:68`, `mcp-server-lib/MCPServer/Server.hs:146`).

## Cluster 3 — Error mapping & sanitization (F-14, F-27, F-30)

**Shared helper** in `webapi-lib` (`WebAPI.Utils` or new `WebAPI.Errors`):

```haskell
-- Log the full detail server-side; return a generic client-facing message.
sanitizeClientError :: String -> ServerError -> IO ServerError
-- or a pure body-rewriter paired with a log call; exact shape decided in plan
```

- **F-14:** add `runWithQueryErrorHandling :: IO a -> QueryAppMonad a` in
  `QueryAppMonad.hs`, log full detail, return generic body. Route the query-api
  handlers that currently `throwIO` (`RestAPI.hs:292`, `Query/ServiceStatus.hs:38`,
  `Query/Aggregates.hs`, `Query/Projected.hs`) through it.
  **⚠ Do NOT copy `runWithTxErrorHandling`'s catch shape.** That sibling catches
  `GYTxMonadException` and unwraps `GYApplicationException appE | Just txEx <- cast appE`
  because interaction-api throws `TxBuildingException` *wrapped* in
  `GYApplicationException` (inside `GYTxMonad`). Every query-api site throws a
  **bare** `TxBuildingException` via `Control.Exception.throwIO` — no wrapper.
  `runWithQueryErrorHandling` must therefore `try @TxBuildingException`
  **directly** and map via `txBuildingExceptionToHttpStatus`. A literal mirror
  would never match, silently returning 500 for `ProfileNotFound` (F-14 unfixed).
  Pin with a test that throws `ProfileNotFound` through the real handler and
  asserts **404**, not just a type-level review.
- **F-27:** client error bodies carry a generic message only. The full
  exception / libpq / IPFS detail is logged, not returned
  (`InteractionAppMonad.hs:89,136`, `IPFS.hs:79`, `QueryAppMonad.hs:75`).
  Note: the bjj-frontend `parseBackendError` already scrapes constructor-name
  substrings from the body, but the current body is human prose (`displayException`),
  so that matching is **already dead** — this change does not regress it. Any
  future fine-grained client-side error UI must key off an explicit, intentionally
  public error-code field, not body text (body is now permanently generic).
- **F-30:** `mkServantErr` must stop collapsing every non-404/503 status to
  400. The interaction-api readiness probe
  (`checkDeployedScriptsAreReady`) shares the single error handler, and probe
  failure maps to **503**, not 400.

## Cluster 4 — Query limits & search (F-15)

**Files:** `Query/Common.hs`, `Query/Projected.hs`.

- `normalizeLimitOffset`: when both `limit` and `offset` are absent, return
  `Just (100, 0)` (was `Nothing` ⇒ unbounded). Clamp `limit` to `[1, 500]`,
  `offset` to `>= 0`. This one chokepoint governs every list endpoint
  (3 direct call sites + `applyLimits`/`applyLimitOffset`).
- `/search` (`Projected.hs:1084-1088`): reject empty or too-short `q`
  (minimum length TBD in plan, e.g. 1–2 chars) with a 4xx; compute totals with
  `COUNT(*)` instead of materializing full lists for `length`.
- **LIKE escaping — fix at the chokepoint, not at `/search`.** `escapeLike :: Text -> Text`
  (escapes `%`, `_`, `\`) must be applied **inside `likePat`** (`Projected.hs:84-85`),
  the single shared helper that builds `%…%`. `likePat` is used by **every** `q`-filtered
  endpoint (`applyPromotionFilter`, `applyAchievementFilter`, `applyProfileFilter`,
  `applyMembership*Filter`, `searchRanksWithNames`, `searchPromotionsWithNames`), not
  just `/search`. Escaping only the `/search` call site leaves every other `q` endpoint
  wildcard-injectable. Pin with a test that a literal `%`/`_` in `q` matches only literal
  occurrences via a non-`/search` route.
- **Escape-character note:** esqueleto's `like` emits a bare `LIKE` with no `ESCAPE`
  clause, so `escapeLike` relies on PostgreSQL's default escape character (`\`). This is
  correct against the only backend (Postgres); document the reliance so a future
  backend/`ILIKE` swap does not silently break it.

**Behavior change (documented):** list endpoints now return at most 100 rows by
default and 500 max. Any consumer that relied on receiving the full table must
paginate. Applies to the frontend and MCP query tools.

## Cluster 5 — Multipart uploads (F-16)

**Files:** `interaction-api/RestAPI.hs`, `ServiceHandlers.hs`.

- Add `MultipartOptions Mem` with `setMaxRequestFileSize` (10 MB) to the
  interaction-api Servant `Context` (alongside the existing
  `BasicAuthCheck AuthUser`). The context type widens accordingly and
  `hoistServerWithContext` / `serveWithContext` are updated.
- Add a pure `detectImageType :: ByteString -> Maybe ImageType` magic-byte
  check (JPEG `FF D8 FF`, PNG `89 50 4E 47`). In `ServiceHandlers` reject a
  non-image body with a 4xx **before** `uploadToIPFS`.

## Cluster 6 — Log hygiene & defaults (F-13, F-32)

**Files:** `query-api/Main.hs`, `chain-sync/Main.hs`.

- Replace `putStrLn ("Postgres DSN: " <> connStr)` with a log of host + dbname
  only (parse or redact the `password=` field). Redact the Kupo base-URL log
  in `chain-sync/Main.hs` (it carries the Demeter credential-host).
- `defaultKupoUrl` → `http://localhost:1442`. Remote deployments set `KUPO_URL`
  explicitly (already read via `lookupEnv`).

## Deploy / dev impact (forced by fail-closed)

Because servers now `die` without credentials, all dev tooling must supply
them:

- `docker-compose*.yml` — set `BASIC_USER`, `BASIC_PASS`, `CORS_ALLOWED_ORIGINS`
  for each service.
- `scripts/populate_*.sh`, test/populate scripts that hit the APIs — export the
  same vars.
- `README.md` §6.4 env table — document the new required vars and the changed
  `KUPO_URL` default; add a `.env.example` (or extend the existing example)
  covering them.

**No secret is committed by this work.** `.env.example` uses placeholders.

### Cross-repo coordination (blocker) — `bjj-frontend`

The real product deployment lives in the **separate `bjj-frontend` repo**, not
this one. Its `docker-compose.yml` sets **no** `BASIC_USER`/`BASIC_PASS` for the
`interaction-api`/`query-api`/`mcp-server` services, and its BFF + agent-service
upstream clients use `${BASIC_USER:-cardano}` / `${BASIC_PASS:-lovelace}` shell
defaults. Shipping fail-closed without a lockstep change there causes: (1) the
three server containers `die` at startup; (2) even with server creds set, the BFF
and agent-service keep sending the stale `cardano`/`lovelace` defaults, now
permanently rejected → silent auth failure across the stack. This break is
invisible in this repo's diff.

**Decision: bundled into this sub-project.** This work also edits the
`bjj-frontend` repo so the two ship consistently. In `bjj-frontend`
(`docker-compose.yml`, `.env.example`): set explicit `BASIC_USER`/`BASIC_PASS` on
the three server services; **drop** the `:-cardano`/`:-lovelace` shell-default
fallbacks on the BFF and agent-service upstream clients (a missing var must fail
loudly, not send stale creds); add `CORS_ALLOWED_ORIGINS` for the servers.
`bjj-frontend` is a **separate git repo** (present in the working dirs) — its
changes are a separate commit there, not part of this repo's feature branch, and
must land together at deploy time. No secret committed; placeholders only.

## Testing strategy

Mirror stream B: TDD the pure cores, build + manual reasoning for IO/wiring.

**Pure, TDD (new `UnitTests/ApiHardening.hs` or per-module test files):**

- `escapeLike` — `%`/`_`/`\` escaped, plain text untouched.
- `normalizeLimitOffset` — both-absent → `(100,0)`; over-max → clamped to 500;
  negative offset → 0; explicit values preserved when in range.
- CORS: `mkCorsMiddleware` allowlist membership — origin in list reflected,
  origin absent → no CORS headers (test the policy function, not the socket).
- `detectImageType` — JPEG/PNG magic bytes recognized; other/short input →
  `Nothing`.
- `authCheck` — correct creds → `Authorized`; wrong creds → `Unauthorized`;
  invalid-UTF-8 bytes → `Unauthorized` (no exception).
- `getCorsConfigFromEnv` parsing — comma split, whitespace trimmed, empty → `[]`
  (test the pure parser split out from the `IO`).

**Build + reasoning (no unit test):** startup `die` behavior, middleware
threading to four servers, multipart context widening, DSN/Kupo log redaction.

## Verification record

Adversarial design-verification workflow (6 review dimensions × independent
skeptic re-verification per finding, 14 agents). **Clean** dimensions: CORS
correctness, constant-time auth (`memory`/`constEq` confirmed reachable and safe
on unequal lengths), multipart OOM + Servant-context widening (the
`setMaxRequestFileSize` cap and context-list change verified sound — the primary
OOM worry). Confirmed findings, folded into the spec above:

- **Blocker — cross-repo fail-closed break (`bjj-frontend`).** Fail-closed
  removal of the credential defaults breaks the actual product deployment in the
  separate `bjj-frontend` repo (no server creds set; BFF/agent-service use
  `:-cardano`/`:-lovelace` shell defaults). → new "Cross-repo coordination"
  section; **owner decision: bundled** — this sub-project also patches
  `bjj-frontend` (separate repo, separate commit).
- **Blocker — error-handler mirror mismatch (F-14).** Copying
  `runWithTxErrorHandling`'s `GYApplicationException`/`cast` shape would never
  match query-api's bare `TxBuildingException` throws → `try @TxBuildingException`
  directly; pin with a `ProfileNotFound → 404` test. → Cluster 3 updated.
- **Major — LIKE escaping scoped too narrowly (F-15).** `escapeLike` must live
  inside the shared `likePat` chokepoint, not only `/search`, or every other `q`
  endpoint stays wildcard-injectable. → Cluster 4 updated.
- **Minor — no explicit `ESCAPE` clause.** `escapeLike` relies on Postgres's
  default `\` escape (esqueleto `like` emits no `ESCAPE`); documented. → Cluster 4.
- **Minor — frontend body-scraping already dead.** F-27's generic bodies don't
  regress the (already non-functional) `parseBackendError` substring matching;
  future fine-grained errors need a structured code field. → Cluster 3 note.

**Refuted** (correctly filtered out): this repo's own `docker-compose.yml`
already in the remediation list; `cardano-project-template` is a scratch dir, not
a maintained repo; `handleLineage`'s direct `err400` is legitimate.
