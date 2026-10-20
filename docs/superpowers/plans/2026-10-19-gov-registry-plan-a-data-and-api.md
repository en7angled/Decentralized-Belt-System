# Palmares — Plan A: governance data substrate and public read path

> **STATUS: header complete, task blocks pending assembly.** The six reviewed task blocks live in
> `docs/superpowers/plans/drafts/plan-a-*.md` and get appended below (renumbered) by the assembly step.
> See `docs/superpowers/F1-PROGRESS.md` step P4 for the resume command.

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended)
> or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax
> for tracking.

**Goal:** Stand up a public, open-source Cardano governance track-record API on **preprod** — every DRep's
complete voting, delegation and rationale history, with reproducible metrics — derived independently from
chain data and reconciled against Koios.

**Architecture:** `cardano-node` (preprod) feeds `cardano-db-sync` (preset `only_governance`, ledger
enabled), which owns all governance ledger math in its own Postgres database. Our Haskell service adds two
schemas to that same database: `palmares` (fourteen read-only SQL views, one materialized view, one SQL
function — no projection tables, so the governance read path is deterministic by construction) and
`palmares_attest` (reserved for Plan B's attestation pipeline). A Servant query-api serves the public,
unauthenticated REST surface over those views, auto-generates its Swagger, and publishes the exact SQL of
every metric so any number on the site can be re-derived by a third party.

**Tech Stack:** Haskell GHC 9.6.6 · Servant + deriving-aeson + persistent · Atlas 0.14.1 · PlutusTx V3
(Plan B) · cardano-db-sync 13.7.1.0 (arm64) · cardano-node 11.1.2 · PostgreSQL 17 · Docker Compose for
local dev · Kustomize + Flux on an arm64 k8s cluster · GitHub Actions on `ubuntu-24.04-arm`.

**Spec:** `docs/superpowers/specs/2026-10-19-gov-registry-spec.md` (the contract; F1's 16 acceptance checks
are §9). **Binding design decisions:** `docs/superpowers/design/f1-synthesis.md` (canonical names §1,
resolutions D-1…D-26, hard task ordering §3, cut list §4, riskiest unknown §5). Where a design note in
`docs/superpowers/design/` contradicts the synthesis, the synthesis wins.

**Scope:** Plan A delivers F1 checks 1–7 and 12, plus the API half of check 16. The attestation protocol
(checks 8–11) is **Plan B**; the React UI (checks 13–15) is **Plan C**; MCP/copilot is F3. Nothing on the
synthesis §4 cut list may appear as a task.

## Global Constraints

Every task's requirements implicitly include this section. Values are copied verbatim from the spec and
the binding synthesis; **none of them may be "improved" by an implementer.**

**Toolchain pins — copied from the Belt System's `cabal.project`, with no bumps during extraction:**
- GHC **9.6.6** (forced by `plutus-tx-plugin`; not a preference).
- `index-state`: hackage.haskell.org **2026-02-20T00:00:00Z**, cardano-haskell-packages **2025-04-11T16:42:25Z**.
- Atlas via `source-repository-package`, tag **`bf0d2d13db3583f5b399344ef9893afc1ccbf335`** (= 0.14.1).
- `clb` (MLabs) tag **`867ff70b4d3086f728733f25509692cda41ea0ef`**, subdir `clb`.
- The Maestro SDK fork pin is carried forward only if still required by Atlas 0.14.1.
- `postgresql-libpq` keeps `flags: +use-pkg-config`.
- **No dependency bumps anywhere in Plan A.** Bumps are a separate post-F0 workstream.

**Runtime versions:**
- `cardano-db-sync` **13.7.1.0**, image `ghcr.io/blinklabs-io/cardano-db-sync` pinned by digest
  `sha256:2a341eb32f15a3e65435f4123c3186f467bf2ed9d3ea2eac449592b5a54eb950` (arm64 exists for 13.7.1.0;
  **13.7.2.1 has no arm64 build** and must not be used).
- `cardano-node` **11.1.2**; PostgreSQL **17**.

**db-sync configuration — exactly this `insert_options` block and nothing more:**
```yaml
insert_options:
  preset: "only_governance"
  offchain_vote_data: "enable"
  shelley:
    enable: true
```
plus top-level `ipfs_gateway: ["https://ipfs.io/ipfs", "https://cloudflare-ipfs.com/ipfs"]` and
`NodeConfigFile`. **No** `ledger_backend`, **no** `disable_epoch`, **no** `RequiresNetworkMagic`, **no**
`remove_jsonb_from_schema`, and **no** restating of preset keys. Base file: db-sync's shipped
`config/preprod-config.yaml` **at tag 13.7.1.0**, so the required logging `Representation` block is verbatim.
`ledger` stays **enabled** (disabling it empties `drep_distr`). `shelley: enable: true` must be in the
config **before the first sync starts** — flipping it later costs a full re-sync.

**Database and naming (canonical — aliases are defects):**
- One database, **`cexplorer`**, one connection string. db-sync owns `public` (read-only to us, except our
  indexes). We add **`palmares`** (governance) and **`palmares_attest`** (Plan B).
- Views are **bare, schema-qualified**: `palmares.<name>`. No `v_`/`mv_` prefixes.
- Our indexes on db-sync's tables are **`palmares_idx_*`**.
- Roles: **`dbsync`** (owns `public`), **`palmares_owner`** (owns our schemas, `SELECT` on `public.*`,
  refreshes the matview), **`palmares_ro`** (query-api runtime: `USAGE` + `SELECT` only).
- Exactly **fourteen views, one materialized view (`palmares.drep_metrics`), one SQL function
  (`palmares.eligible_actions(drep_hash_id bigint, as_of_epoch int)`)**. **Zero governance projection tables.**

**Haskell code:**
- Module namespace **`Palmares.*`**, package **`palmares`**, four libraries (`onchain-lib`, `offchain-lib`,
  `chainsync-lib`, `webapi-lib` — no fifth), four executables: **`admin`**, **`query-api` (8083)**,
  **`interaction-api` (8082)**, **`attest-sync` (8084)**.
- House style (Belt System `CLAUDE.md`, carried over): max 120 chars, 2-space indent, no tabs; PascalCase
  types, camelCase functions; `mk` smart constructors; module-level Haddock required; JSON via
  `deriving-aeson` with `StripPrefix` + `CamelToSnake` for snake_case wire names; no redundant record field
  prefixes; errors as typed exceptions mapped to HTTP, never raw strings.
- `webapi-lib` keeps **no project-library dependencies**; `onchain-lib` imports **no** off-chain deps.

**Service contract:**
- The query-api is **public: no Basic auth** (rate-limiting lives at the BFF). Swagger is **generated**, never
  hand-edited, written by `query-api --write-swagger-only` to
  `docs/generated/swagger/query-swagger-api.json`, with a CI guard that fails on drift.
- `GET /status` is the single status endpoint (no `/protocol-status`). Freshness (epoch, slot, block time,
  `lag_seconds`) is on every relevant response.
- `PALMARES_READY_MAX_LAG_SECONDS=300` (`/ready` red), `PALMARES_AMBER_LAG_SECONDS=120` (UI amber).
- `pageTotal :: Maybe Int` (`null` on `/activity` — no magic `-1`). `age_days :: Maybe Int` everywhere.
- Anchor verdict enum, canonical wire values:
  `valid | invalid_schema | hash_mismatch | oversized | unreachable | inline | not_fetched`, plus a separate
  `cip100_valid :: Maybe Bool`.
- The view bootstrap (`sql/000`–`sql/030`) runs at query-api boot as `palmares_owner` in **one transaction**
  and is **fail-closed: `die` on failure**. `sql/040_indexes.sql` runs **outside** any transaction, from a
  one-shot `admin` Job (`CREATE INDEX CONCURRENTLY` cannot run in a transaction block).

**Product constraints that override convenience:**
- **No opaque composite scores, ever.** Only reproducible facts whose formula is published. The metric
  catalog (embedded `.sql` + `metric-formulas.json`) is the single source of truth; `docs/metrics.md` is
  **generated** from it by `admin write-metrics-doc`, with CI failing on drift.
- **No protocol fees**; no monetization hooks anywhere in the read path.
- Pseudo-DReps (`always_abstain`, `always_no_confidence`) appear in `palmares.drep_epoch_power` with
  `target_kind` and are counted in dashboard abstain figures, but are **never ranked or rated as DReps**.
- The independence claim is worded precisely: *re-derivable by running open-source software —
  `cardano-node` + `cardano-db-sync` + this repo — from genesis.* Koios is a cross-check, never a source.

**Repo and CI:**
- **Apache-2.0**, public from the first commit, with a `NOTICE` carrying the attribution for the extracted
  MIT-licensed Belt System code.
- Images to **GHCR** (`ghcr.io/<owner>/palmares-<svc>`), built `platforms: linux/arm64`. **No Docker Hub, no
  `runs-on: self-hosted` anywhere.**
- CI on `ubuntu-24.04-arm` with `haskell-actions/setup@v2`; the named fallback is a GHCR-published
  `palmares-builder:9.6.6` run via `docker run` from the host. **Never `container: benz0li/ghc-musl`**
  (musl + glibc `node20` breaks JS actions, and on arm64 the runner refuses outright).
- Kubernetes manifests live **only** in the `apps-k8s-configs` repo; this repo ships `compose.yaml` and no
  `deploy/k8s/`. No secret is required by the read path; `.gitignore`/`.dockerignore` must make it impossible
  to bake a provider token into an image or commit one to history.
- Any step that mutates the **live** cluster says so on its first line and is separable, because it costs a
  preprod node pod restart that only the owner may authorize.

## Review Focus

Five failure modes the spec implies but no task's own happy-path tests would exercise. Each one's test is
added to the task that owns the code, in that task's step style.

1. **db-sync mid-sync or behind, serving partial governance data as if complete.** A visitor checking a
   DRep during a voting window must never see a truncated vote history presented as authoritative. Expected:
   `/ready` fails closed past `PALMARES_READY_MAX_LAG_SECONDS`, every response carries freshness, and the
   `drep_distr_epoch >= epoch_no - 1` guard catches the one misconfiguration that silently empties voting
   power. → test owned by the `/ready` + `/status` task.
2. **Pseudo-DReps leaking into DRep lists, rankings or rate denominators.** `always_abstain` holds ~46% of
   preprod's epoch-319 voting power; counting it as a DRep distorts every aggregate and fails Koios parity
   by that margin. Expected: `target_kind = 'drep'` filtering on `/dreps` and every rate, while the dashboard
   still reports abstain totals from the same view. → test owned by the `drep_epoch_power` view task and the
   `/dreps` route task.
3. **A DRep eligible for zero actions, or registered mid-window, or retired and re-registered.** Expected:
   `n/a` (JSON `null`), never `0%`, never a negative or double-counted tenure. → test owned by the
   `eligible_actions` function task and the metric-catalog task.
4. **An unreachable, oversized, inline (`data:`) or hash-mismatched rationale anchor.** Expected: never shown
   as a verified rationale; `inline` counts as a real rationale (db-sync refuses to fetch `data:` URIs);
   unknown failure text defaults to `unreachable`, never to an accusation against the DRep. → golden test on
   the exact `fetch_error` strings, owned by the `palmares.anchor` view task.
5. **Activity-feed paging across epoch-boundary events, which have no transaction or slot of their own.**
   Expected: a total cursor — no skipped or repeated events when `since` lands on an epoch boundary.
   → test owned by the `palmares.activity` view task and the `/activity` route task.

---

## Corrections that MUST be applied during assembly

The six task blocks in `drafts/` were drafted **before** the Yaci-Store re-evaluation landed
(`docs/superpowers/design/f1-yaci-store-evaluation.md`, 2026-10-19). Three of its findings change
drafted content, and the drafters could not know them. Apply all three while appending:

1. **`gov_action_dropped` must narrow its predicate** (supersedes synthesis D-19's event list, which the
   `sql-metrics` and `api` drafters were told was "non-negotiable"). `dropped_epoch` is not an independent
   outcome: db-sync's `doc/schema.md` line 910 is stale per its own maintainer (issue #1932); on enactment
   it stays NULL, on **expiry it is set to `expired_epoch + 1`**. Our own measured preprod data agrees —
   102 expired actions all carry `dropped_epoch`, plus 6 genuinely dropped-only rows. So the event and the
   API's `dropped` outcome fire **only** when
   `dropped_epoch IS NOT NULL AND ratified_epoch IS NULL AND enacted_epoch IS NULL AND expired_epoch IS NULL`.
   Emitting it for expired actions duplicates the expiry event one epoch later. **No change to
   `close_epoch = LEAST(ratified, expired, dropped)`** — `LEAST` still picks the smaller `expired_epoch`.
   The API outcome enum is `ratified | enacted | expired | dropped | open`.
   → fix in the `palmares.activity` view task, the `palmares.gov_action` view task, and the gov-action
   route/DTO tasks; the restricted predicate gets its own assertion (expect 6 rows on preprod today).
2. **The from-genesis sync task must state the correctness rationale**, not only completeness: db-sync
   issue #1975 (open since 2025-05) records two identical instances disagreeing on one DRep's epoch-538
   voting power by ~740×, and the 13.6.0.4 Known Issue names **preprod** explicitly — pre-upgrade
   `drep_distribution` values are "not well defined [and] could be slightly different between different
   executions". The documented remedy is deleting ledger-state files and recomputing, which is what a
   from-genesis sync does. Consequence for the plan: **a snapshot-restored or upgraded db-sync is not an
   acceptable shortcut for the substrate**, and the Mithril shortcut applies to the node only.
   → fix in the substrate block's sync task and in ADR `0001`.
3. **ADR `0001-governance-data-substrate.md` must cite the Yaki-Store rejection record** with its reasons
   (mandatory UTxO indexing to get `drep_dist`; `GovActionStatus` lacking ENACTED/DROPPED, issue #1194;
   no off-chain anchor fetching or hash verification; per-epoch range partitioning needing ~3,160 locks
   for one unbounded query, issue #1200; a baseline migration opening with
   `drop table if exists drep_dist cascade`) and the T1–T7 monitorable triggers from that note's §8.
   Also record that Yaci-Store is **not** a fallback in the disk-pressure chain (it is structurally
   heavier); the fallback for "does not fit the Pi" is the x86_64 box budgeted for F2.
   → fix in the bootstrap block's ADR task.

Assembly also: renumber tasks sequentially, cross-match every `Interfaces: Consumes` against some task's
`Produces`, and collect every `**(OPEN: …)**` line into a single "Open decisions" section at the end of
the plan, each tagged with who decides it.

<!-- ASSEMBLY MARKER: reviewed task blocks from docs/superpowers/plans/drafts/plan-a-*.md are appended
     below in this order: bootstrap, extraction, substrate, sql-metrics, api, parity — renumbered
     sequentially, with Interfaces blocks cross-matched, and the three corrections above applied. -->

## Task index

Task ids are **block-prefixed and stable** (they are referenced by `Interfaces: Consumes` blocks across
blocks, so they are deliberately not renumbered). Execution order is the order below, which is the hard
ordering of `f1-synthesis.md` §3.

| # | Block | Tasks | Delivers |
|---|---|---|---|
| 1 | Bootstrap & CI | T-BOOTSTRAP-1…5 | Experiment B, the public repo, a skeleton that compiles empty, three CI workflows, `compose.yaml` |
| 2 | Extraction | T-EXTRACTION-1…6 | as-is → rename → strip (GREEN GATE) → CIP-129 ids → inherited fixes #3 and #5 |
| 3 | Substrate | T-SUBSTRATE-1…8 | Experiment A, node IPC/config on hostPath, Postgres 17 + 50 Gi, the db-sync Deployment, the from-genesis sync (F1 check 1), the index Job, ADR 0001, roles and grants |
| 4 | SQL & metrics | T-SQL-METRICS-1…**13** | fail-closed schema bootstrap, 14 views + `eligible_actions`, the metric catalog, the epoch-320 fixture, the one matview and its refresh thread, **the determinism gate (F1 check 3, added by self-review)** |
| 5 | API | T-API-1…18 | DTOs, `Query.Common`, CSV, the boot DDL, every route family, the page composites, `--write-swagger-only` |
| 6 | Parity | T-PARITY-1…6 | Koios decoders, the ten reconciliation rows, the pinned sample, `admin reconcile-koios`, the nightly workflow |

**Blocks 3 and 4/5 overlap deliberately:** Experiment A (T-SUBSTRATE-1) starts on day 1 and runs for ~48 h
in the background, and T-SQL-METRICS-1 seeds a db-sync skeleton so the view tasks are testable in CI
before the real sync reaches tip. Only T-SUBSTRATE-5…8 and block 6 genuinely wait on the synced database.

---

## Block 1 — Repo bootstrap and CI

# Plan A — block: repo bootstrap and CI

This block delivers the `palmares` repo itself: the arm64-CI feasibility probe that decides `ci.yml`'s
shape (synthesis §3 Phase 0 item 3), a legally complete and leak-proofed public repo from commit #1,
a cabal skeleton that solves the 551-constraint dependency plan and goes green on `cabal build all`
before a single extracted line arrives, two of D-8's three GitHub workflows (`reconcile.yml` ships with
the Phase-5 script it runs), and the contributor-facing
`compose.yaml` plus the one db-sync config file it and the cluster share. It implements synthesis
decisions **D-8** (hosted `ubuntu-24.04-arm`, GHCR, never `container: benz0li/ghc-musl`, no
self-hosted runner anywhere), **D-26** (no `deploy/k8s/**` and no `manifests` CI job in
this repo), **D-1** (no `projector` service or image), **D-5** (the `insert_options` block, exactly),
**D-4** (`shelley: enable: true` in the config before the first sync), **D-11** (`cassava` +
`Palmares.WebAPI.CSV` in `webapi-lib`), **D-9** (`resources/metrics/*.sql` +
`resources/metric-formulas.json` as `extra-source-files`), **D-14** (four libraries), and
T-EXTRACT §1.4's "no pin moves during extraction". Its deliverables are prerequisites for the
extraction block (T-BOOTSTRAP-3's component list and freeze file) and for every later CI gate.

**Phase-0 owner decisions this block assumes** (synthesis §3 item 1), and what changes if the owner
decides otherwise:

1. **Name = `palmares`** (repo `palmares`, package `palmares.cabal`, namespace `Palmares.*`, env prefix
   `PALMARES_`, images `ghcr.io/<owner>/palmares-<svc>`). Assumed by every file in this block. *If the
   owner picks another name:* run T-EXTRACT §5.2's three commands before T-BOOTSTRAP-4 lands, while the
   repo is still ~20 files; rules 1–4 of §5.2 (no identifier, filename, config key or URI path carries
   the project name) are what keep it at three commands, and this block obeys them.
2. **`shelley: enable: true` accepted (D-4).** Touches exactly one file in this block:
   `deploy/dbsync/db-sync-config.yaml` (T-BOOTSTRAP-5). *If refused:* delete the three-line `shelley`
   key, drop the Postgres estimate back to 6–10 GB, and the F1 check-12 delegator metrics become a
   documented upper bound. The flip is only free **before** the first sync starts.
3. **db-sync as a separate k8s Deployment on a hostPath socket (D-6).** Not assumed anywhere in this
   block — `compose.yaml` brings up its own node and its own socket volume, so the cluster topology
   decision cannot change any file here.
4. **Our schemas co-located in db-sync's `cexplorer` with role separation (D-15).** Assumed by
   `POSTGRES_DB: cexplorer` in `compose.yaml` and by the (still commented) `extra-source-files:
   sql/*.sql` line in `palmares.cabal`
   (one database ⇒ one `PG_CONN_STR` ⇒ views are possible at all). *If the owner wants a second
   database:* cross-database views are impossible in PostgreSQL and the entire read path changes — this
   block's compose file would then need a second Postgres service and the `sql/` glob would be dead.
5. **db-sync pinned at blinklabs 13.7.1.0, arm64 (digest `sha256:2a341eb3…54eb950`).** Assumed by
   `compose.yaml`'s image line. *If re-pinned:* the digest is invalidated and the two behaviours this
   plan depends on must be re-verified at the new tag — `parseOverrides obj baseOptions` (the preset is
   a base, not an override) and `ledger_backend` defaulting to `inmemory`.

Two further assumptions, taken here rather than deferred: the GitHub repo is created **under
`en7angled`** and is **public from commit #1** (see T-BOOTSTRAP-2 step 3); and Plan A's `images.yml`
matrix is the four Haskell services only — `bff` and `frontend` are added by Plan C, and there is no
`projector` ever (D-1).

---

### T-BOOTSTRAP-1: Experiment B — hosted arm64 GHC 9.6.6 probe

Decides `ci.yml`'s shape before it is written. The probe runs on a throwaway branch of the owner's
existing **public** repo `mariusgeorgescu/solana-haskell-sdk` (public ⇒ free hosted runners, and that
repo's Actions are proven working as of 2026-10-19T18:31Z), so it does not block repo creation and
needs no `delete_repo` scope. It must not be run in `palmares` first: T-BOOTSTRAP-2 pushes a public
repo, and this probe is 15 minutes of throwaway history.

**Files:**
- Create (on branch `ci-probe/arm64-ghc-966` of `mariusgeorgescu/solana-haskell-sdk`):
  `.github/workflows/probe-arm64-ghc.yml`
- Test: none possible as a unit test — the subject is a GitHub-hosted runner image. The probe *is* the
  test, with every premise asserted inside it so a false premise turns the run red.

**Interfaces:**
- Consumes: nothing.
- Produces: the decision value `CI_SHAPE ∈ {hosted-setup, ghcr-builder}`, consumed by T-BOOTSTRAP-4
  step 3; the verified `apt-get install` package list for `ubuntu-24.04-arm`; the measured answer to
  "is `libblst-any` resolvable by `pkg-config` on the hosted runner, or must `ci.yml` generate the
  alias `.pc` file".

- [ ] **Step 1: Write the failing test**

```yaml
# .github/workflows/probe-arm64-ghc.yml  (throwaway branch; delete after reading the run)
name: probe-arm64-ghc
on:
  workflow_dispatch: {}
  push:
    branches: [ci-probe/arm64-ghc-966]

jobs:
  probe:
    runs-on: ubuntu-24.04-arm
    timeout-minutes: 20
    steps:
      - name: Premise 1 — the runner is really arm64
        run: |
          set -euo pipefail
          test "${{ runner.arch }}" = "ARM64"
          uname -m | grep -Fx aarch64

      - name: Premise 2 — every system library the Cardano stack links resolves on arm64
        run: |
          set -euo pipefail
          sudo apt-get update
          sudo apt-get install -y build-essential pkg-config libffi-dev libgmp-dev \
            libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev libpq-dev liblmdb-dev \
            libsodium-dev libsecp256k1-dev libblst-dev

      - uses: haskell-actions/setup@v2
        id: setup
        with:
          ghc-version: "9.6.6"
          cabal-version: "3.10"

      - name: Premise 3 — ghcup served the aarch64 GHC 9.6.6 bindist
        run: |
          set -euo pipefail
          ghc --numeric-version | grep -Fx 9.6.6
          cabal --numeric-version
          test -n "${{ steps.setup.outputs.cabal-store }}"
          echo "cabal-store=${{ steps.setup.outputs.cabal-store }}"

      - name: Measurement — pkg-config state (recorded, not asserted for libblst-any)
        run: |
          set -euo pipefail
          for p in libsodium libsecp256k1 libblst; do
            echo "$p=$(pkg-config --modversion "$p")"
          done
          echo "libblst-any=$(pkg-config --modversion libblst-any 2>/dev/null || echo MISSING)"
```

- [ ] **Step 2: Run the probe and read the observable**

There is no prior red state to show: the premises are properties of a runner image, so the first run is
the measurement.

Run:
```bash
cd /path/to/solana-haskell-sdk && git checkout -b ci-probe/arm64-ghc-966
git add .github/workflows/probe-arm64-ghc.yml
git commit -m "chore: throwaway arm64 GHC 9.6.6 CI probe" && git push -u origin ci-probe/arm64-ghc-966
gh run watch "$(gh run list --branch ci-probe/arm64-ghc-966 --limit 1 --json databaseId --jq '.[0].databaseId')"
```

PASS = the job concludes `success`, with these four lines in the log: `ARM64` asserted silently (step 1
green), `apt-get install` exits 0 with no `E: Unable to locate package`, `9.6.6` printed by
`ghc --numeric-version`, and the `cabal-store=` line non-empty. Wall clock for the whole job is the
second number worth recording (expected ≈ 3–6 min; it contains no project build).

FAIL = any of: `ubuntu-24.04-arm` unrecognised label (job never starts), `E: Unable to locate package
libblst-dev`, or `haskell-actions/setup` erroring on an unavailable aarch64 9.6.6 bindist.

- [ ] **Step 3: Take the decision**

- Green ⇒ `CI_SHAPE=hosted-setup`: T-BOOTSTRAP-4 writes `ci.yml` with `runs-on: ubuntu-24.04-arm` +
  `haskell-actions/setup@v2` + `actions/cache@v4`, i.e. T-INFRA §5.1 verbatim.
- `libblst-dev` missing, everything else green ⇒ still `hosted-setup`, plus one extra `ci.yml` step that
  builds blst from source exactly as `Dockerfile.base` does (`BLST_REF=v0.3.15`, copy
  `bindings/blst*.h*` to `/usr/include`, `libblst.a` to `/usr/lib`, `libblst.pc` to
  `/usr/lib/pkgconfig/`).
- `libblst-any=MISSING` ⇒ still `hosted-setup`, plus the one-line alias generation already proven in
  `.envrc`: `sed 's/^Name: libblst/Name: libblst-any/' "$(pkg-config --variable pcfiledir libblst)/libblst.pc" > …/libblst-any.pc` with `PKG_CONFIG_PATH` extended.
- Runner label or GHC bindist unavailable ⇒ `CI_SHAPE=ghcr-builder`: `ci.yml` keeps
  `runs-on: ubuntu-24.04-arm` but builds inside `docker run` **from the host** — `docker build -f
  Dockerfile.base -t ghcr.io/en7angled/palmares-builder:9.6.6 .`, push once, then
  `docker run --rm -v "$PWD:/palmares" -w /palmares ghcr.io/en7angled/palmares-builder:9.6.6 bash -lc
  "cabal update && cabal build all && cabal test all --test-show-details=direct"`.
  **Never `container: benz0li/ghc-musl:9.6.6`** (D-8): it is musl, the runner injects a glibc `node20`
  for JavaScript actions, and on arm64 the runner refuses outright.
  **(OPEN: publishing `palmares-builder:9.6.6` to GHCR from the laptop needs `write:packages`; the
  local `gh` token carries only `gist, read:org, repo, workflow` — run `gh auth refresh -s write:packages`
  or mint a classic PAT. Needed only on this branch.)**

- [ ] **Step 4: Verify the decision is recorded where T-BOOTSTRAP-4 reads it**

Append the chosen value and the probe's run URL to the `## Open items` section of the assembled plan
file (this block, wherever the assembler put it), as a one-line resolution.

Run: `grep -n 'CI_SHAPE=' <the assembled plan file>`
Expected: PASS — exactly one line matching `CI_SHAPE=(hosted-setup|ghcr-builder)` followed by a
`https://github.com/mariusgeorgescu/solana-haskell-sdk/actions/runs/<id>` URL. T-BOOTSTRAP-4 does not
start before this grep matches.

- [ ] **Step 5: Commit**

```bash
# in solana-haskell-sdk: remove the throwaway branch, keep nothing
git checkout main && git push origin --delete ci-probe/arm64-ghc-966 && git branch -D ci-probe/arm64-ghc-966
```
No commit lands in `palmares` from this task; its only product is the decision value.

---

### T-BOOTSTRAP-2: Repo creation, licence, leak guards, ADR stubs

The deliverable is a public repo that is legally complete and cannot leak at commit #1. The two
historical leaks get their file-level fixes here; leak #1's bundle grep belongs to Plan C (there is no
frontend in Plan A), and leak #2's blocking CI gate to T-BOOTSTRAP-4.

**Files:**
- Create: `/Users/mg/Projects/CardanoProjects/palmares/LICENSE`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/NOTICE`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/licenses/LICENSE.decentralized-belt-system.MIT`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/.gitignore`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/.dockerignore`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/README.md`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/docs/adr/0001-governance-data-substrate.md`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/docs/adr/0002-write-path-provider.md`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/docs/generated/.gitkeep`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/.githooks/pre-push`
- Test: no test file — the deliverable is a set of ignore rules and licence texts. The checks are the
  three runnable commands in step 1, one of which is a real `docker build` context.

**Interfaces:**
- Consumes: nothing.
- Produces: repo `github.com/en7angled/palmares` (public, default branch `main`) ⇒ `GH_OWNER=en7angled`
  and image namespace `ghcr.io/en7angled/palmares-<svc>` (consumed by T-BOOTSTRAP-4's `images.yml` and
  T-BOOTSTRAP-5's `compose.yaml`); `docs/adr/0001-governance-data-substrate.md` with the section
  anchors `## Decision`, `## Measurements`, `## Consequences`, and
  `docs/adr/0002-write-path-provider.md` with `## Decision`, `## Spike`, `## Consequences` (filled by
  the Phase-2 measurement task and by the Plan B write-path task respectively);
  `.githooks/pre-push`; `docs/generated/` as the **committed** (not ignored) swagger/blueprint output
  dir, present from commit #1 via `.gitkeep`.

- [ ] **Step 1: Write the failing test**

Save as `/tmp/palmares-hygiene.sh`:

```bash
#!/usr/bin/env bash
# Run from the repo root. Three checks, in the order the two historical leaks happened.
set -euo pipefail
cd /Users/mg/Projects/CardanoProjects/palmares

# A — leak #1: a real .env must never enter a docker build context.
#     (bjj-frontend's .dockerignore was four lines and did not list .env; docker build baked it in.)
printf 'MAESTRO_API_KEY=real-looking-token\n' > .env
printf 'placeholder\n' > operation.prv
printf 'FROM alpine:3.20\nCOPY . /ctx\nRUN test ! -e /ctx/.env && test ! -e /ctx/operation.prv\n' \
  | docker build --no-cache -f - -t palmares-ctx-probe .

# B — both ignore files cover the whole secret set.
for f in .env .env.local operation.prv config/config_atlas.json x.skey x.vkey; do
  git check-ignore -q "$f" || { echo "NOT IGNORED: $f"; exit 1; }
done

# C — leak #2: nothing credential-shaped in the history that is about to become public.
#     The two planted files are removed FIRST: `gitleaks detect` scans commits, but leaving a
#     token-shaped working-tree file next to the scan makes a green run ambiguous.
rm -f .env operation.prv
gitleaks detect --source . --no-banner

echo "HYGIENE OK"
```

- [ ] **Step 2: Run test to verify it fails**

Run (the empty repo must exist for the script's `cd` and for `git check-ignore`; `git init` is
idempotent and step 3 re-runs it):
```bash
mkdir -p /Users/mg/Projects/CardanoProjects/palmares
cd /Users/mg/Projects/CardanoProjects/palmares && git init -b main
bash /tmp/palmares-hygiene.sh
```
Expected: FAIL at check A with
`ERROR: process "/bin/sh -c test ! -e /ctx/.env && test ! -e /ctx/operation.prv" did not complete successfully: exit code: 1`
(and, with `.dockerignore` present but `.gitignore` absent, FAIL at check B with `NOT IGNORED: .env`).

- [ ] **Step 3: Implement the repo scaffolding**

```bash
set -euo pipefail
SRC=/Users/mg/Projects/CardanoProjects/Decentralized-Belt-System
DST=/Users/mg/Projects/CardanoProjects/palmares
mkdir -p "$DST"/{licenses,docs/adr,docs/generated,.githooks} && cd "$DST" && git init -b main
curl -sL https://www.apache.org/licenses/LICENSE-2.0.txt -o LICENSE
cp "$SRC/LICENSE" licenses/LICENSE.decentralized-belt-system.MIT   # byte-for-byte; MIT, (c) 2025 Marius Georgescu
touch docs/generated/.gitkeep
```

`NOTICE` — verbatim, Apache-2.0 §4(d) (T-EXTRACT §3.3), with `<owner>` resolved to `en7angled`:

```
Palmares
Copyright 2026 Marius Georgescu

This product includes software developed as part of the Decentralized Belt System
(https://github.com/en7angled/Decentralized-Belt-System), Copyright (c) 2025 Marius Georgescu,
originally released under the MIT License. Portions of src/lib/ and src/exe/ are derived
from that work and are redistributed here under the Apache License, Version 2.0 by the
copyright holder. The original MIT license text is retained in
licenses/LICENSE.decentralized-belt-system.MIT.

This product includes third-party software under its own licenses; see the dependency
pins in cabal.project and the generated license report.
```

No per-file licence headers (T-EXTRACT §3.3 decision): `NOTICE` + the extraction commit + the manifest
give better provenance than 70 header blocks that rot.

`.dockerignore` — T-INFRA §6.2 verbatim, plus `src/` deliberately **not** excluded (the service
Dockerfiles `COPY src ./src`). This replaces T-EXTRACT §1.1's "copy as-is": the Belt System's
`.dockerignore` excludes `.envrc` but **not** `.env`, which is precisely leak #1, so it must not be
copied.

```
.env
.env.*
!.env.example
**/.env
**/.env.*
.git
.github
node_modules
dist
dist-newstyle
.direnv
result
result-*
.hie
*.hi
*.o
operation.prv
*.skey
*.vkey
config/*.prv
docs
*.md
!README.md
.DS_Store
```

`.gitignore` — the Belt System's file minus its PureScript block (`.psci_modules`, `.spago`,
`bower_components`, `output`, `.psci`), plus T-INFRA §6.2's and T-EXTRACT §1.9's lines:

```
# Haskell
dist
dist-*
dist-newstyle/
cabal-dev
.cabal
.cabal-fmt*
*.o
*.hi
*.hie
.hie/
*.chi
*.chs.h
*.dyn_o
*.dyn_hi
.hpc
.hsenv
.cabal-sandbox/
cabal.sandbox.config
*.prof
*.aux
*.hp
*.eventlog
cabal.project.local
cabal.project.local~
# cabal.project.freeze is COMMITTED on purpose: it is the CI cache key.
.HTF/
.ghc.environment.*
compiled

# Nix / direnv
.direnv/
result
result-*

# Env and secrets
.env
.env.*
!.env.example
operation.prv
*.skey
*.vkey
*.prv
config/config_atlas.json
config/config_*.local.json

# Generated artifacts
# docs/generated/** is COMMITTED on purpose and NOT ignored: `docs/generated/swagger/
# query-swagger-api.json` is what T-BOOTSTRAP-4's parity guard diffs (`git diff --exit-code`)
# and what the frontend's `gen:api` reads (synthesis §3 step 20), and the CIP-57 blueprint is a
# spec-§7 reproducibility artifact. The Belt System commits both (`git ls-files docs/generated`
# → README.md, the blueprint, two swagger files). T-EXTRACT §1.9's `docs/generated/*` +
# `!.gitkeep` pair is deleted here: it would make both of those gates silently vacuous.
db/

# OS / editors
*.DS_STORE
*.link
.pre-commit-config.yaml
node_modules
```

`.githooks/pre-push` — a gitleaks run, not a duplicate of CI. On a repo that is public from commit #1 a
push *is* a publication, so this is the only gate that fires **before** the secret is public; CI's
gitleaks fires after. Same hook name and `core.hooksPath` contract as `apps-k8s-configs/.githooks/`
(which ships exactly one file, `pre-push`), and the same "missing tool warns, does not block" rule:

```bash
#!/usr/bin/env bash
# Local pre-publication gate. CI's gitleaks job is the real blocker, but on a public repo a push IS a
# publication, so this runs first.
#   ACTIVATE (once per clone — git never propagates hooks): git config core.hooksPath .githooks
#   BYPASS, when you must:                                  git push --no-verify
set -uo pipefail
cd "$(git rev-parse --show-toplevel)" || exit 0
if command -v gitleaks >/dev/null 2>&1; then
  gitleaks detect --source . --no-banner || { echo "pre-push: BLOCKED — gitleaks found a secret" >&2; exit 1; }
  echo "pre-push: gitleaks OK"
else
  echo "pre-push: gitleaks missing — skipping (CI will still block)" >&2
fi
exit 0
```

`README.md` — the ten T-EXTRACT §1.6 headings, **bodies empty except the three below**: §1, §2 and §10
are written here, and §3–§9 are a heading plus the one-line parenthetical from §1.6, filled by the task
that owns each (§5 "Build from source" and §6 "Configuration" by T-BOOTSTRAP-3, which owns the build
command sequence and `.env.example`; §4 by T-BOOTSTRAP-5; §3 and §7 by the read-path block; §8 is §1's
passage restated; §9 by Plan B). Two passages are fixed text, not a heading:

§1's independence claim, worded exactly as spec §7 allows and no stronger:

> **Self-hostable, stated precisely.** The read path — your own `cardano-node`, your own
> `cardano-db-sync`, this API — is 100% autonomous and needs no third-party credential of any kind.
> The write path (building attestation transactions) needs your own provider key; a free tier covers
> small volumes.

§2's two-doors wording (T-INFRA §4.3 verbatim), which must not overclaim:

> Two ways to get the data. **Re-derive it** — `MITHRIL_DOWNLOAD=false docker compose --profile chain up`,
> and in **1–2 days** you have the governance database from genesis with no trust in us at all. Or
> **restore our snapshot** — set `RESTORE_SNAPSHOT=<release url>` and you are querying in **~25 minutes**.
> The snapshot is a convenience, not the claim: the claim is that the first path exists, is documented,
> and that `scripts/reconcile_koios.sh` will tell you whether either database agrees with an
> independent source.
>
> **Mithril does not speed up `cardano-db-sync`.** It restores the *node's* chain database.
> `cardano-db-sync` still streams every block from genesis and maintains its own ledger state. There is
> no published db-sync snapshot for preprod — ours is the only one.

§10 carries the three contributor setup commands in place of a `scripts/preflight.sh`
(skipped: a one-time script that runs three commands is read less often than the README that would
have to point at it):

```bash
git config core.hooksPath .githooks   # hooks never propagate through git
git check-ignore -v .env              # must print a .gitignore line
cp .env.example .env                  # then fill it; .env is ignored by git AND docker
```

ADR stubs — each one states the decision that is already made (so the file is useful on day 1) and
leaves named empty sections for the task that fills it:

`docs/adr/0001-governance-data-substrate.md`:
```markdown
# 0001 — Governance data substrate

Status: accepted (measurements pending)

### Decision
cardano-db-sync 13.7.1.0 (blinklabs arm64, pinned by digest) with
`insert_options: {preset: only_governance, offchain_vote_data: enable, shelley: {enable: true}}`
and the default in-memory ledger backend, in db-sync's own `cexplorer` database. Our governance layer
is SQL views in schema `palmares` plus one materialized view — not a projection service, and not an
Ogmios follower. Our own metadata fetcher is cut: db-sync fetches and hash-verifies anchors itself.

### Measurements
<!-- filled by the Phase-2 measurement task from Experiment A + the preprod sync:
     wall clock to tip, pg_total_relation_size per table (top 20), peak db-sync RSS,
     count(*) on stake_deregistration / pool_update / drep_distr / off_chain_vote_data /
     off_chain_vote_fetch_error -->

### Consequences
<!-- filled by the same task; must include the two documented holes in "db-sync already does it":
     constitution anchors are never fetched, and duplicate off_chain_vote_* child rows (PR #2137) -->
```

`docs/adr/0002-write-path-provider.md`:
```markdown
# 0002 — Write-path provider

Status: accepted for F1

### Decision
Maestro, via Atlas, with the key injected from the environment and never committed. The read path has
no provider dependency at all. `maestro-sdk` is on neither Hackage nor CHaP and upstream is archived,
so the `sourabhxyz/haskell-sdk` fork pin in `cabal.project` is load-bearing.

### Spike
<!-- filled by the Plan B write-path task: does Atlas's local-node provider
     (src/GeniusYield/Providers/Node.hs) carry Conway governance certificates?
     Atlas has NO db-sync provider — that option does not exist. -->

### Consequences
<!-- filled by the same task -->
```

Repo creation — **public from commit #1** (spec §7), under the org that already holds the Belt System,
so that `github.repository` = `en7angled/palmares` makes `images.yml`'s
`ghcr.io/${{ github.repository }}-<svc>` resolve to `ghcr.io/en7angled/palmares-<svc>`, which is
exactly the string synthesis §1.3 and `compose.yaml`'s `GH_OWNER:-en7angled` default already assume.
`gh` is authenticated as `mariusgeorgescu` with `gist, read:org, repo, workflow` (verified
2026-10-19), which is sufficient to create an org repo the user may create in. **The `gh repo create`
command runs once, in step 5, after the first commit exists** — `--source=. --push` needs a commit,
and a second invocation fails with `name already exists on this account`.
If org creation is refused, create `mariusgeorgescu/palmares` instead and change `GH_OWNER`'s default
in `compose.yaml` in the same commit — do not leave the two out of step.

- [ ] **Step 4: Run test to verify it passes**

Run: `bash /tmp/palmares-hygiene.sh`
Expected: PASS — `HYGIENE OK`, with check A's build reporting
`Successfully tagged palmares-ctx-probe:latest` and check C reporting `no leaks found`.

- [ ] **Step 5: Commit**

```bash
git add LICENSE NOTICE licenses/ .gitignore .dockerignore README.md docs/adr/ docs/generated/.gitkeep .githooks/
git config core.hooksPath .githooks
git commit -m "chore: bootstrap Apache-2.0 public repo with leak guards and ADR stubs"
gh repo create en7angled/palmares --public \
  --description "Cardano governance reputation and track-record registry" \
  --source=. --remote=origin --push
```

---

### T-BOOTSTRAP-3: Build skeleton that compiles empty

Proves the 551-constraint dependency plan solves on this machine, the C libraries link, and the four
container images build and statically link — all against a two-line `Main.hs`, before 10,600 lines of
extracted code can confuse a build failure with a solve failure. The module lists from T-EXTRACT §1.3
are written into `palmares.cabal` **commented out**, one block per component, and the extraction block
uncomments each line as the file lands; that is what makes "compiles empty" and "the manifest is
recorded verbatim" both true.

**Files:**
- Create: `/Users/mg/Projects/CardanoProjects/palmares/cabal.project` (`cp` of
  `/Users/mg/Projects/CardanoProjects/Decentralized-Belt-System/cabal.project`, unchanged)
- Create: `/Users/mg/Projects/CardanoProjects/palmares/cabal.project.freeze` (`cp` of the Belt System's,
  unchanged) — **before the first build, not after it.** Corrected 2026-10-20 after the first build
  failed: `index-state` bounds the Hackage snapshot but does not pin versions, so the solver chose a
  newer `microlens` (`Lens.Micro.Internal` no longer exports `Field1..Field5` → `cardano-prelude-0.2.1.0`
  fails) and a newer `mem-pack` (`packBytes` arity changed → `cardano-crypto-class-2.2.2.0` fails at
  `PackedBytes.hs:126`). The Belt System's 707-line freeze is the known-good resolution and already
  covers every package Plan A adds (`cassava ==0.5.3.2`, `esqueleto ==3.6.0.0`, `file-embed ==0.0.16.0`),
  so copying it is what "no bumps" actually means. T-BOOTSTRAP-4 already assumed this file exists.
- Create: `/Users/mg/Projects/CardanoProjects/palmares/palmares.cabal`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/.envrc`, `.env.example`, `.justfile`,
  `hie.yaml`, `libblst.pc`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/Dockerfile.base`, `Dockerfile.runtime-base`,
  `Dockerfile.admin`, `Dockerfile.query-api`, `Dockerfile.interaction-api`, `Dockerfile.attest-sync`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/config/config_atlas.example.json`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/scripts/build-images.sh`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/CLAUDE.md`, `src/exe/CLAUDE.md`,
  `src/lib/onchain-lib/CLAUDE.md`, `src/lib/offchain-lib/CLAUDE.md`, `src/test/CLAUDE.md`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/src/exe/admin/Main.hs`,
  `src/exe/query-api/Main.hs`, `src/exe/interaction-api/Main.hs`, `src/exe/attest-sync/Main.hs`
- Modify: `/Users/mg/Projects/CardanoProjects/palmares/README.md` — §5 "Build from source" is
  T-EXTRACT §6.1's two command blocks verbatim (minus the `write-blueprint` and `test_exunits.sh`
  lines, which have nothing to run yet); §6 "Configuration" is one row per name in `.env.example`,
  each marked `required` / `default <value>` / `fail-closed`
- Test: `/Users/mg/Projects/CardanoProjects/palmares/src/test/Main.hs`

**Interfaces:**
- Consumes: the repo and `.gitignore`/`.dockerignore` from T-BOOTSTRAP-2.
- Produces: cabal component names `lib:onchain-lib`, `lib:chainsync-lib`, `lib:offchain-lib`,
  `lib:webapi-lib`, `exe:admin`, `exe:query-api`, `exe:interaction-api`, `exe:attest-sync`,
  `test-suite:test`; `cabal.project.freeze` (the CI cache key); builder image tags
  `palmares-builder:9.6.6` and `palmares-runtime-base:3.20`; env var names `PALMARES_ENV_LOADED`,
  `PG_CONN_STR`, `PORT`, `KUPO_URL`, `BASIC_USER`, `BASIC_PASS`, `CORS_ALLOWED_ORIGINS`,
  `ATLAS_CORE_CONFIG`, `DEPLOYED_VALIDATORS_CONFIG`, `BATCH_SIZE`, `FETCH_BATCH_SIZE`,
  `ROLLBACK_MARGIN`, `PALMARES_VIEW_REFRESH_SECONDS`, `PALMARES_READY_MAX_LAG_SECONDS`,
  `PALMARES_AMBER_LAG_SECONDS`.

- [ ] **Step 1: Write the failing test**

```haskell
-- src/test/Main.hs
-- | Stub suite. The real tree (PropertyTests, TestRuns, UnitTests.*) arrives with the extraction
-- block; this stub exists so `cabal test all` is a green gate from the first build.
module Main (main) where

import System.Exit (exitSuccess)

main :: IO ()
main = exitSuccess
```

Each executable gets the same two-line stub, e.g.:

```haskell
-- src/exe/query-api/Main.hs
module Main (main) where

main :: IO ()
main = pure ()
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cd /Users/mg/Projects/CardanoProjects/palmares && cabal build all && cabal test all`
Expected: FAIL with `cabal: Error: Could not find the package description file`
(no `.cabal` file exists yet).

- [ ] **Step 3: Implement the skeleton**

`cabal.project`: `cp` verbatim. Every pin stays — Atlas `bf0d2d13db3583f5b399344ef9893afc1ccbf335`,
maestro fork `1e84ccb43aa0f56dd2776e64a9a7341441fc623e`, clb `867ff70b4d3086f728733f25509692cda41ea0ef`,
cardano-addresses `d611632fc3d616d5b4038a70f864bf2613c125d0`, `index-state`
`hackage.haskell.org 2026-02-20T00:00:00Z` / `cardano-haskell-packages 2025-04-11T16:42:25Z`, the six
CHaP root keys, and the three `package` flag blocks. **No bumps during extraction** (T-EXTRACT §1.4);
bumps are a post-F0 workstream.

`palmares.cabal`: T-EXTRACT §1.3 verbatim, with these four deltas and one mechanical change:
- `webapi-lib`: add `cassava` to `build-depends` and `Palmares.WebAPI.CSV` to the commented
  `exposed-modules` (D-11). `webapi-lib` still has no project-library dependency.
- `query-api`: `other-modules` also lists `Query.Metrics`, `Query.Refresh`, `Metrics.Catalog`
  (synthesis §1.3), commented with the rest.
- `extra-source-files`: `NOTICE` and `licenses/LICENSE.decentralized-belt-system.MIT` active;
  `sql/*.sql`, `resources/metrics/*.sql` and `resources/metric-formulas.json` present but commented,
  with a one-line note that the SQL block (T-BOOTSTRAP-? of the read-path block) and the metrics
  catalog task uncomment them — a glob that matches nothing is a build warning today and a silent
  packaging hole later.
- Every `exposed-modules` / `other-modules` list is present but comment-prefixed; the extraction block
  uncomments per file.
- `onchain-lib`'s 28-entry `default-extensions` list is copied verbatim from
  `Decentralized-Belt-System.cabal:98-126` (`default-extensions:` on 98, `BangPatterns` on 99 …
  `ViewPatterns` on 126 — verified on the working tree 2026-10-19), and the
  `plinth-ghc-options` common stanza byte-for-byte — those flags encode the `plutus-tx-plugin`
  contract.

`.envrc`: `cp` + exactly two substitutions —
`sed -i '' 's/BJJ_ENV_LOADED/PALMARES_ENV_LOADED/; s/Decentralized Belt System environment/Palmares environment/' .envrc`.
Everything else is load-bearing (`GC_DONT_GC=1` on Darwin, the nix-direnv 3.0.6 `source_url` with its
sha256, `CABAL_DIR="$PROJECT_ROOT/.cabal"`, the `xz.dev` pkgconfig `PATH_add`, the
`PKG_CONFIG_PATH_FOR_TARGET` merge, the `libblst` → `libblst-any` `.pc` alias generation). devx flake
pin unchanged: `github:input-output-hk/devx?rev=d316c0394de3c72abd2d79d1b55a3b6abc9d0f0c#ghc96-iog-full`.

`.env.example` — T-EXTRACT §1.8, with the two `/ready` thresholds renamed to their canonical forms
(synthesis §1.3 supersedes `PALMARES_MAX_DBSYNC_LAG_SECONDS`):

```sh
# ---- required by interaction-api + admin (fail-closed: they `die` if unset) ----
BASIC_USER=
BASIC_PASS=
CORS_ALLOWED_ORIGINS=        # comma-separated allowlist; empty = deny all cross-origin

# ---- Postgres: ONE pool, db-sync's own database ----
PG_CONN_STR=host=postgres user=postgres password=postgres dbname=cexplorer port=5432

# ---- attest-sync (Kupo path only) ----
KUPO_URL=http://localhost:1442
BATCH_SIZE=1000000
FETCH_BATCH_SIZE=100000
ROLLBACK_MARGIN=2160

# ---- Atlas + deployed validators (JSON blob or file path) ----
ATLAS_CORE_CONFIG=
DEPLOYED_VALIDATORS_CONFIG=
# MAESTRO_API_KEY is read by Atlas from the core config, never from here. Never commit a real one.

# ---- query-api ----
PORT=8083
PALMARES_VIEW_REFRESH_SECONDS=300   # Query/Refresh.hs: interval of the one matview's
                                    # REFRESH ... CONCURRENTLY (D-1)
PALMARES_READY_MAX_LAG_SECONDS=300  # /ready red above this (D-7)
PALMARES_AMBER_LAG_SECONDS=120      # UI amber above this (D-7)
```

`.justfile`, `hie.yaml`, `libblst.pc`, `config/config_atlas.example.json`: `cp` as-is.

`Dockerfile.base`: `cp` + `sed -i '' 's#/Decentralized-Belt-System#/palmares#g'`. It keeps
`FROM --platform=$TARGETPLATFORM benz0li/ghc-musl:9.6.6` — that image is a real multi-arch
`{amd64, arm64}` manifest, and D-8's prohibition is on `container: benz0li/ghc-musl` as a **GitHub job
container** (musl vs the runner's glibc `node20`), not on using it as a `docker build` base. The two are
different things and the plan must not conflate them.

`Dockerfile.runtime-base`: `cp` as-is (nothing in it names the project).

The four service Dockerfiles: copy `Dockerfile.query-api`, `Dockerfile.interaction-api`, and
`Dockerfile.chainsync` → `Dockerfile.attest-sync`, then
`sed -i '' 's/bjj-builder/palmares-builder/; s/bjj-runtime-base/palmares-runtime-base/; s#/Decentralized-Belt-System#/palmares#g; s/chainsync-service/attest-sync/g'`.
`Dockerfile.admin` is new, same two stages, `RUN cabal install admin --enable-executable-static
--overwrite-policy=always --installdir=. --install-method=copy --ghc-options="-optl=-static
-optl=-pthread -optc=-static"`, `ENTRYPOINT ["/app/admin"]`, no `EXPOSE`.

`scripts/build-images.sh`: `cp` + the same `sed` (`bjj-builder`→`palmares-builder`,
`bjj-runtime-base`→`palmares-runtime-base`), `IMAGE_PREFIX` default `ghcr.io/en7angled`, and the
service list `(admin query-api interaction-api attest-sync)`. This is the documented manual escape
hatch the July-2026 runner outage turned into a first-class path. `scripts/test_exunits.sh` is **not**
copied here — it parses the test log and has nothing to parse until the extraction block lands the
suite.

The five `CLAUDE.md` files. **The Belt System's five are root, `src/exe/`,
`src/lib/offchain-lib/`, `src/lib/onchain-lib/`, `src/test/`** (verified on disk) — not "one per
library"; `chainsync-lib` and `webapi-lib` have none, and none are invented here. Port all five with
the project renamed, the executable table rewritten to `admin`, `interaction-api` (8082),
`query-api` (8083), `attest-sync` (8084), and the root architecture paragraph's five-library sentence
reduced to four (no `mcp-server-lib` until F3). `src/lib/onchain-lib/CLAUDE.md` additionally loses four
specific contracts that this codebase no longer has:

| Delete from `src/lib/onchain-lib/CLAUDE.md` | Because |
|---|---|
| L38 — "`checkFee` … takes `OracleParams`, a selector `(FeeConfig -> Integer)` (e.g. `fcProfileCreationFee`, `fcPromotionFee`), and outputs" | fix #1 deletes the selector argument. Replace with: "**Fee check**: `feeFor :: MintAction -> FeeConfig -> Integer` in `Palmares.Onchain.Protocol.Lookup` is the only place a `FeeConfig` field is ever named, on or off chain; `checkFee :: OracleParams -> MintAction -> [TxOut] -> Bool` takes the action, never a selector." |
| L26 — the "**Linked lists**: Generic list logic in `Onchain.LinkedList` …" bullet | `LinkedList.hs` is skipped (T-EXTRACT §0.5) |
| the "**Business logic**: In `Onchain.BJJ` and `Protocol.Core`" bullet | both skipped (T-EXTRACT §2.2) |
| the "**Authorization**: Promotion requires master User NFT; membership ops …" bullet | belt/membership redeemers do not exist. Keep the `deriveUserFromRefAC` sentence, restated for the three new redeemers. |

Keep, verbatim, the doctrine that outlives the domain: exact-mint (`mintValueMinted == expectedValue`),
oracle-UTxO-as-reference-input with `opPaused`, min-lovelace from `opMinUTxOValue` only, seed-`TxOutRef`
one-shot names, `hasCurrencySymbol`, the `Cleanup`-redeemer rule, and the output-layout comment rule.
Also carry forward into `src/lib/offchain-lib/CLAUDE.md` the one piece of doctrine whose *code* is
skipped: never call a `traceError`-ing on-chain function off-chain — wrap it with a dedicated exception
(T-EXTRACT §2.4, `SafeOnchainLogic.hs`).

- [ ] **Step 4: Run test to verify it passes**

Run:
```bash
cd /Users/mg/Projects/CardanoProjects/palmares
direnv allow && cabal update && cabal build all && cabal test all --test-show-details=direct
```
Expected: PASS — `cabal build all` ends with `Build profile: -w ghc-9.6.6 -O1` and exit 0 (budget
**40–75 min cold** on an Apple-silicon laptop, 20–40 min of that `direnv allow` pulling the ~8–12 GB
devx closure on first run); `cabal test all` prints `1 of 1 test suites (1 of 1 test cases) passed`.
Then, in the same task:
```bash
# The freeze was COPIED in step 3. Re-running it here only proves the solve did not drift from it.
cabal freeze && git diff --exit-code cabal.project.freeze
docker buildx build --platform linux/arm64 --load -f Dockerfile.base -t palmares-builder:9.6.6 .
docker buildx build --platform linux/arm64 --load -f Dockerfile.runtime-base -t palmares-runtime-base:3.20 .
for s in admin query-api interaction-api attest-sync; do
  docker buildx build --platform linux/arm64 --load -f "Dockerfile.$s" -t "palmares-$s:skeleton" . ; done
docker run --rm palmares-query-api:skeleton; echo "exit=$?"
```
Expected: `cabal.project.freeze` exists with ~551 constraints; all six images build; the stub
`query-api` container exits 0 — which is the proof that the `-optl=-static` musl link works on arm64
before any real code depends on it.

- [ ] **Step 5: Commit**

```bash
git add cabal.project palmares.cabal .envrc .env.example .justfile hie.yaml libblst.pc \
        Dockerfile.base Dockerfile.runtime-base Dockerfile.admin Dockerfile.query-api \
        Dockerfile.interaction-api Dockerfile.attest-sync config/config_atlas.example.json \
        scripts/build-images.sh CLAUDE.md src/exe/CLAUDE.md src/lib/onchain-lib/CLAUDE.md \
        src/lib/offchain-lib/CLAUDE.md src/test/CLAUDE.md src/exe/*/Main.hs src/test/Main.hs \
        README.md
git commit -m "build: cabal skeleton, pinned toolchain and container images

Pins copied verbatim from Decentralized-Belt-System's cabal.project; no bumps.
Module lists from the extraction manifest are present but commented until the files land."
git add cabal.project.freeze
git commit -m "chore: freeze dependency plan"
```

---

### T-BOOTSTRAP-4: CI workflows

Two of D-8's three workflows, no self-hosted runner anywhere. Five explicit scope decisions, stated
rather than implied:

- **No `reconcile.yml` here.** D-8's third workflow runs `./scripts/reconcile_koios.sh`, which does not
  exist until the Phase-5 parity block (synthesis §3 step 25). Shipping it now would mean either a
  nightly red run for weeks or a `continue-on-error` job with nothing to verify — the same
  "gate that decides nothing" this task refuses below. **The Phase-5 block creates `reconcile.yml`**
  (T-INFRA §5.3 verbatim, with `scripts/reconcile_koios.sh`, `PALMARES_API=https://gov.cardano.vip/api`,
  `KOIOS_API=https://preprod.koios.rest/api/v1`, `continue-on-error: true`, and the uploaded artifact
  `koios-divergences` → `out/divergences.json` that the F1 check-6 release gate reads), in the same
  commit as the script, where a real `workflow_dispatch` run can gate it.

- **No `typescript` job.** Plan A has no frontend. Leak #1's credential-shaped-`VITE_*` grep and the
  bundle entropy scan land with Plan C's frontend task, in the same commit that creates `src/`.
- **No `manifests` job and no `deploy/k8s/**`** (D-26): `apps-k8s-configs`'s own `validate.yaml`
  already runs kustomize + kubeconform + the plaintext-Secret guard, and Flux reads nothing else.
- **No `format` job.** The Belt System ships no `stylish-haskell`/`fourmolu`/`hlint` config (verified:
  none at its root), `-Wall -Wunused-packages` in `common-options` is the style gate that exists, and
  T-INFRA §5.4 lists `hlint`/`stylish-haskell` as warn-only. Add a non-blocking job on the day a config
  file exists.
- **The Swagger parity guard and the generated-docs guard are added by the tasks that create their
  generators**, not here behind an `if [ -f ]`. A gate that silently passes when its input is missing
  decides nothing. The read-path block's `query-api --write-swagger-only` task adds
  `cabal run query-api -- --write-swagger-only && git diff --exit-code docs/generated/swagger/`
  (file: `docs/generated/swagger/query-swagger-api.json`), and the metrics-catalog task adds
  `cabal run admin -- write-metrics-doc && git diff --exit-code docs/metrics.md`. Both as steps of the
  existing `haskell` job.

**Files:**
- Create: `/Users/mg/Projects/CardanoProjects/palmares/.github/workflows/ci.yml`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/.github/workflows/images.yml`
- Test: the workflow runs themselves (the subject is GitHub Actions; there is nothing to unit-test).

**Interfaces:**
- Consumes: `CI_SHAPE` from T-BOOTSTRAP-1; `cabal.project.freeze`, the component names and the six
  Dockerfiles from T-BOOTSTRAP-3; `GH_OWNER=en7angled` from T-BOOTSTRAP-2.
- Produces: job names `haskell` and `secrets` in `ci.yml` (later tasks add steps to `haskell`); the
  published image tags `ghcr.io/en7angled/palmares-{admin,query-api,interaction-api,attest-sync}:<sha>`
  and `:latest` (consumed by `compose.yaml` and by the GitOps overlay in `apps-k8s-configs`).

- [ ] **Step 1: Write the failing test**

`ci.yml` — two blocking jobs (`CI_SHAPE=hosted-setup` form; swap the `haskell` job's body for the
`docker run` form from T-BOOTSTRAP-1 step 3 if the probe went red):

```yaml
name: CI
on:
  pull_request:
  push:
    branches: [main]
concurrency:
  group: ci-${{ github.ref }}
  cancel-in-progress: true

jobs:
  haskell:
    runs-on: ubuntu-24.04-arm     # arm64: same arch as the target cluster
    timeout-minutes: 350          # the first uncached run is the expensive one; GitHub's max is 360
    steps:
      - uses: actions/checkout@v4
      - name: System libraries for plutus-tx / cardano-crypto
        run: |
          set -euo pipefail
          sudo apt-get update
          sudo apt-get install -y build-essential pkg-config libffi-dev libgmp-dev \
            libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev libpq-dev liblmdb-dev \
            libsodium-dev libsecp256k1-dev libblst-dev
      - uses: haskell-actions/setup@v2
        id: setup
        with:
          ghc-version: "9.6.6"      # forced by plutus-tx-plugin; do not widen
          cabal-version: "3.10"
      - uses: actions/cache@v4
        with:
          path: |
            ${{ steps.setup.outputs.cabal-store }}
            dist-newstyle
          key: ${{ runner.os }}-${{ runner.arch }}-ghc9.6.6-${{ hashFiles('cabal.project.freeze', 'cabal.project') }}
          restore-keys: ${{ runner.os }}-${{ runner.arch }}-ghc9.6.6-
      - run: cabal update
      - run: cabal build all --enable-tests
      - run: cabal test all --test-show-details=direct

  secrets:
    runs-on: ubuntu-24.04-arm
    steps:
      - uses: actions/checkout@v4
        with: { fetch-depth: 0 }            # full history: leak #2 was a token COMMITTED
      - uses: gitleaks/gitleaks-action@v2
        env: { GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }} }
```

`images.yml` — post-merge, GHCR, arm64. It **must build the two base images first**: every
`Dockerfile.<svc>` starts `FROM palmares-builder:9.6.6` / `FROM palmares-runtime-base:3.20`, which are
local tags, so T-INFRA §5.2's version would fail on the very first line of every service build. The
matrix is the four Haskell services (no `projector` — D-1; `bff`/`frontend` are Plan C), and the
`VITE_*` `build-args` are dropped for the same reason.

```yaml
name: images
on:
  push:
    branches: [main]
    tags: ["v*.*.*"]
  workflow_dispatch:

jobs:
  build:
    runs-on: ubuntu-24.04-arm
    permissions:
      contents: read
      packages: write          # GHCR via GITHUB_TOKEN — no DOCKERHUB_* secrets at all
    strategy:
      fail-fast: false
      matrix:
        svc: [admin, query-api, interaction-api, attest-sync]
    steps:
      - uses: actions/checkout@v4
      - uses: docker/setup-buildx-action@v3
      - uses: docker/login-action@v3
        with:
          registry: ghcr.io
          username: ${{ github.actor }}
          password: ${{ secrets.GITHUB_TOKEN }}
      - name: Shared base images (local tags the service Dockerfiles FROM)
        run: |
          set -euo pipefail
          docker buildx build --platform linux/arm64 --load -f Dockerfile.base -t palmares-builder:9.6.6 .
          docker buildx build --platform linux/arm64 --load -f Dockerfile.runtime-base -t palmares-runtime-base:3.20 .
      - uses: docker/build-push-action@v6
        with:
          context: .
          file: Dockerfile.${{ matrix.svc }}
          platforms: linux/arm64
          push: true
          tags: |
            ghcr.io/${{ github.repository }}-${{ matrix.svc }}:${{ github.sha }}
            ghcr.io/${{ github.repository }}-${{ matrix.svc }}:latest
          cache-from: type=gha,scope=${{ matrix.svc }}
          cache-to: type=gha,mode=max,scope=${{ matrix.svc }}
```

- [ ] **Step 2: Run test to verify it fails**

Run:
```bash
cd /Users/mg/Projects/CardanoProjects/palmares && git checkout -b ci/bootstrap
git push -u origin ci/bootstrap && gh pr create --fill --base main
gh pr checks --watch
```
Expected: before the workflows are committed, `gh pr checks` reports `no checks reported on the
'ci/bootstrap' branch` — there is no gate at all, which is the state this task removes.

- [ ] **Step 3: Implement the two workflows in `.github/workflows/`**

Caching strategy, stated: `actions/cache@v4` over `${{ steps.setup.outputs.cabal-store }}` **and**
`dist-newstyle`, keyed on `hashFiles('cabal.project.freeze', 'cabal.project')`. `cabal.project.freeze`
is committed (T-BOOTSTRAP-3) precisely so the key is stable across Hackage/CHaP index moves; without it
the cache never hits. **No GHC matrix** — one GHC 9.6.6 store fits GitHub's 10 GB per-repo cache, two do
not, and the `base` bounds pin 9.6 anyway. Realistic times: **cold 60–120 min** on the arm64 runner
(T-INFRA §5.1; T-EXTRACT §6.2's 2–3 h is the pessimistic bound for a 4-core hosted runner, which is why
`timeout-minutes: 350` is set and not 120), **warm 10–20 min**. `images.yml` pays its own cold cost once
per freeze change through `type=gha` build cache, scoped per service.

- [ ] **Step 4: Run test to verify it passes**

Run (the commit is the task's commit; it has to land on the branch for CI to run at all):
```bash
git add .github/workflows/ci.yml .github/workflows/images.yml
git commit -m "ci: blocking build+test on arm64, GHCR images on push to main"
git push
gh pr checks --watch
```
Expected: PASS — `gh pr checks` shows `haskell` and `secrets` both `pass`; the `haskell` job's cache
step prints `Cache saved with key: Linux-ARM64-ghc9.6.6-<hash>`, and a re-run (push an empty commit)
prints `Cache restored from key: Linux-ARM64-ghc9.6.6-<hash>` and finishes under 20 min.
Record the first run's wall clock into this plan's `## Open items` — it is the number the schedule
depends on, and the two notes disagree by 2×.

- [ ] **Step 5: Merge, and verify `images.yml` published four packages**

`images.yml` has no PR trigger, so merging is what first exercises it. `gh workflow run --ref
ci/bootstrap` is **not** an alternative: `workflow_dispatch` is only offered for workflows that already
exist on the default branch.

```bash
gh pr merge --squash --delete-branch
gh run watch "$(gh run list --workflow images.yml --branch main --limit 1 --json databaseId --jq '.[0].databaseId')"
gh api /orgs/en7angled/packages?package_type=container --jq '.[].name'
```
Expected: the `images.yml` run concludes `success` and the last command prints exactly
`palmares-admin`, `palmares-query-api`, `palmares-interaction-api`, `palmares-attest-sync`.
(If the repo ended up under the user rather than the org, the listing is
`gh api /users/mariusgeorgescu/packages?package_type=container`; both need `read:packages` on the local
token — `gh auth refresh -s read:packages`.)

---

### T-BOOTSTRAP-5: `compose.yaml` and the one db-sync config file

The contributor-facing local stack, and the single source file for db-sync's config. T-EXTRACT §1.1 put
that file at `config/db-sync/preprod-db-sync-config.json`; T-INFRA §4.1 put it at
`deploy/dbsync/db-sync-config.yaml` and decided that the compose bind and the k8s ConfigMap are two
`sed`-generated variants of **one** file. Infrastructure collisions go to T-INFRA (synthesis §6), so:
**`deploy/dbsync/db-sync-config.yaml`**, YAML, byte-identical to the ConfigMap content in T-INFRA §1.3
except for `NodeConfigFile`. This is not `deploy/k8s/**` and does not reopen D-26.

Before writing anything, check whether Experiment A's drafter already left files behind:
`ls /Users/mg/Projects/CardanoProjects/palmares/experiments/dbsync-preprod/`. **Verified absent
2026-10-19** (the whole `palmares` directory did not exist), but re-check at execution time — if a
compose file and a db-sync config are there, `git mv` them to `compose.yaml` and
`deploy/dbsync/db-sync-config.yaml` and reconcile against the content below rather than writing a
second copy.

**Files:**
- Create: `/Users/mg/Projects/CardanoProjects/palmares/compose.yaml`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/deploy/dbsync/db-sync-config.yaml`
- Modify: `/Users/mg/Projects/CardanoProjects/palmares/README.md` — §4 "Quick start (dev/audit)" gets
  the two profiles, the two doors and the time-until-useful numbers
- Test: no unit test — the subject is a 1–2 day external sync. The verification is the two commands in
  step 4 with an exact row-count observable, and what is measured is stated there.

**Interfaces:**
- Consumes: `GH_OWNER=en7angled` (T-BOOTSTRAP-2); the image tag
  `ghcr.io/en7angled/palmares-query-api:latest` (T-BOOTSTRAP-4).
- Produces: compose project name `palmares`; profiles `chain` and `app`; service names `cardano-node`,
  `postgres`, `cardano-db-sync`, `query-api`; volumes `node-db`, `node-ipc`, `postgres`,
  `db-sync-state`; env vars `POSTGRES_PASSWORD`, `POSTGRES_PORT`, `RESTORE_SNAPSHOT`, `GH_OWNER`,
  `TAG`; the config path `deploy/dbsync/db-sync-config.yaml` (the k8s ConfigMap in `apps-k8s-configs`
  is generated from it by a one-line `sed` repointing `NodeConfigFile`).

- [ ] **Step 1: Write the failing test**

Save as `/tmp/palmares-compose-check.sh`:

```bash
#!/usr/bin/env bash
# The compose stack must (a) validate, (b) resolve the pinned db-sync digest, and (c) actually insert
# blocks. (c) is the only check that proves the config parsed: a malformed logging Representation block
# makes db-sync exit in the first seconds, and a wrong insert_options silently fills the wrong tables.
set -euo pipefail
cd /Users/mg/Projects/CardanoProjects/palmares

docker compose --profile chain config -q
docker compose --profile chain config | grep -F \
  'ghcr.io/blinklabs-io/cardano-db-sync:13.7.1.0@sha256:2a341eb32f15a3e65435f4123c3186f467bf2ed9d3ea2eac449592b5a54eb950'
docker compose --profile chain up -d

# db-sync must be inserting within 10 minutes of the node having a socket.
for _ in $(seq 1 60); do
  n=$(docker compose exec -T postgres psql -U postgres -d cexplorer -tAc \
        'select count(*) from block' 2>/dev/null || echo 0)
  [ "$n" -gt 0 ] && { echo "blocks=$n"; exit 0; }
  sleep 10
done
echo "FAIL: no rows in block after 10 minutes"; docker compose logs --tail 40 cardano-db-sync; exit 1
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bash /tmp/palmares-compose-check.sh`
Expected: FAIL with `no configuration file provided: not found` (no `compose.yaml` yet).

- [ ] **Step 3: Implement `compose.yaml` and `deploy/dbsync/db-sync-config.yaml`**

`compose.yaml` — T-INFRA §4.1, with four changes: the `projector` service is **gone** (D-1), the db-sync
image carries its **digest** (spec §8 requires the digest pin), `bff` and `frontend` are **not here**
(Plan C adds them, in the same commit that produces their images), and the footprint line is corrected
for D-4.

```yaml
# compose.yaml — preprod governance stack, re-derivable from scratch.
# Minimum host: 4 cores, 12 GB RAM, 120 GB free SSD. arm64 and amd64 both work.
#
#   docker compose --profile chain up -d        # node + db-sync + postgres
#   docker compose --profile app   up -d        # + query API
#
# TIME UNTIL USEFUL:
#   node, via Mithril          : ~20 min   (default; MITHRIL_DOWNLOAD=false for a genesis audit run)
#   db-sync from our snapshot  : ~25 min   (set RESTORE_SNAPSHOT; needs FRESH postgres+state volumes)
#   db-sync from genesis       : ~1-2 DAYS (db-sync, not the node, is the cost)
# Footprint: node 21 GB + db-sync state ~2 GB + Postgres 10-18 GB = 33-41 GB, peak RAM ~6 GB.
# (The Postgres figure is 10-18 GB, not 6-10: `shelley: enable: true` is on — see D-4.)

name: palmares

services:
  cardano-node:
    profiles: [chain, app]
    image: ghcr.io/blinklabs-io/cardano-node:11.1.2
    environment:
      NETWORK: preprod
      MITHRIL_DOWNLOAD: "true"
      MITHRIL_AGGREGATOR_ENDPOINT: https://aggregator.release-preprod.api.mithril.network/aggregator
      MITHRIL_GENESIS_VERIFICATION_KEY: "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d"
    volumes:
      - node-db:/data/db
      - node-ipc:/ipc
    healthcheck:
      test: ["CMD-SHELL", "test -S /ipc/node.socket"]
      interval: 30s
      timeout: 5s
      retries: 20
    restart: on-failure

  postgres:
    profiles: [chain, app]
    image: postgres:17-alpine
    environment:
      POSTGRES_DB: cexplorer
      POSTGRES_USER: postgres
      POSTGRES_PASSWORD: ${POSTGRES_PASSWORD:-devonly}
    command: >
      -c shared_buffers=768MB -c work_mem=32MB -c maintenance_work_mem=512MB
      -c max_wal_size=4GB -c checkpoint_timeout=30min -c synchronous_commit=off
    ports: ["${POSTGRES_PORT:-5432}:5432"]
    volumes: ["postgres:/var/lib/postgresql/data"]
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U postgres -d cexplorer"]
      interval: 10s
      timeout: 5s
      retries: 10
    restart: on-failure

  cardano-db-sync:
    profiles: [chain, app]
    image: ghcr.io/blinklabs-io/cardano-db-sync:13.7.1.0@sha256:2a341eb32f15a3e65435f4123c3186f467bf2ed9d3ea2eac449592b5a54eb950
    environment:
      NETWORK: ""                      # MUST be present and empty: we pass an explicit --config
      POSTGRES_HOST: postgres
      POSTGRES_PORT: "5432"
      POSTGRES_DB: cexplorer
      POSTGRES_USER: postgres
      POSTGRES_PASSWORD: ${POSTGRES_PASSWORD:-devonly}
      RESTORE_SNAPSHOT: ${RESTORE_SNAPSHOT:-}
      # RESTORE_RECREATE_DB is left at the upstream default Y on purpose: "N" skips the recreate and
      # makes pg_restore collide with an existing db-sync schema.
    working_dir: /var/lib/cexplorer   # the .restored marker and the multi-GB download must land on
                                      # the volume; the image has no WORKDIR, so the default is /
    command:
      ["--config", "/dbsync-config/db-sync-config.yaml", "--socket-path", "/node-ipc/node.socket"]
    volumes:
      - db-sync-state:/var/lib/cexplorer
      - node-ipc:/node-ipc
      - node-db:/node-db:ro            # see the OPEN item below: this mount must expose the dir that
                                       # actually holds config.json + the genesis files
      - ./deploy/dbsync/:/dbsync-config/:ro
    depends_on:
      cardano-node: { condition: service_healthy }
      postgres:     { condition: service_healthy }
    restart: on-failure

  query-api:
    profiles: [app]
    image: ghcr.io/${GH_OWNER:-en7angled}/palmares-query-api:${TAG:-latest}
    environment:
      PORT: "8083"
      PG_CONN_STR: postgresql://postgres:${POSTGRES_PASSWORD:-devonly}@postgres:5432/cexplorer
    ports: ["8083:8083"]
    depends_on:
      postgres: { condition: service_healthy }
    restart: on-failure

volumes:
  node-db:
  node-ipc:
  postgres:
  db-sync-state:
```

**(OPEN: the `node-db` → `NodeConfigFile` pairing is unverified and it is what the step-4 gate turns
on.** T-INFRA §4.1 mounts the **chain-db** volume (`node-db:/data/db` in `cardano-node`) at `/node-db`
and then points db-sync at `/node-db/config.json`, but blinklabs' `cardano-node` image writes its
rendered config and genesis files to its own config dir, not into the chain-db directory — in which
case `config.json` is not under that mount and db-sync exits before inserting a single block.
Settle it with one command before writing the file, and make the mount match what it prints:
`docker run --rm --entrypoint sh ghcr.io/blinklabs-io/cardano-node:11.1.2 -c 'ls /opt/cardano/config/preprod /data 2>&1'`.
Then either mount that directory read-only at `/node-configs` and set
`NodeConfigFile: /node-configs/config.json` — which also collapses the compose/k8s `sed` variant to
zero diff — or keep `/node-db` if the listing shows `config.json` there. Decided by whoever runs
T-BOOTSTRAP-5, from that listing; no owner input needed.)**

`deploy/dbsync/db-sync-config.yaml` — T-INFRA §1.3 verbatim, with `NodeConfigFile` repointed to the
blinklabs node image's own path and with D-5's `shelley` key added. `insert_options` carries **exactly
three keys and nothing else**: no restated preset keys, no `ledger_backend`, no `disable_epoch` (it
does not exist at 13.7.1.0), no `RequiresNetworkMagic` (not a db-sync key), no
`remove_jsonb_from_schema` (already the default). The logging block is the shipped
`config/preprod-config.yaml` **at tag 13.7.1.0**, because `minSeverity`, `setupScribes`,
`defaultScribes`, `setupBackends`, `defaultBackends` and `options` are non-`Maybe` and a minimal object
fails to parse at startup.

```yaml
NetworkName: preprod
EnableLogMetrics: False
EnableLogging: True
# PrometheusPort defaults to 8080 — we scrape it; do not change it.

# The compose variant points at the node image's own config dir; the k8s ConfigMap variant is this
# same file with NodeConfigFile: /node-configs/config.json (one-line sed, never a second copy).
# Genesis files are referenced by BARE FILENAME inside config.json, so they must sit beside it.
NodeConfigFile: /node-db/config.json

# ledger_backend is DELIBERATELY ABSENT. Default is "inmemory"; "lsm" is documented as crashing on
# Raspberry Pi. Do not add it.

# ipfs_gateway is a TOP-LEVEL option. With an EMPTY list every ipfs:// anchor fails
# OCFErrNoIpfsGateway, and F1 check 7 would be hostage to one host.
ipfs_gateway:
  - "https://ipfs.io/ipfs"
  - "https://cloudflare-ipfs.com/ipfs"

insert_options:
  # USE THE PRESET. The preset is only the BASE and every explicit key below overrides it
  # (Config/Types.hs parseOverrides, tag 13.7.1.0); configuration.md's "overrides all other
  # properties" prose is wrong. Without `preset` the base is `def`, which has tx_metadata AND reward
  # enabled, and sioRewards cannot be set from JSON at all.
  preset: "only_governance"
  offchain_vote_data: "enable"   # db-sync becomes our metadata fetcher: fetch + blake2b + retry
  shelley:
    enable: true                 # F1 check 12 needs stake_deregistration and pool_update.
                                 # Flipping this AFTER the first sync costs a full re-sync.

minSeverity: Info
setupBackends: [KatipBK]
defaultBackends: [KatipBK]
setupScribes:
  - scKind: StdoutSK
    scName: stdout
    scFormat: ScText
    scRotation: null
defaultScribes:
  - [StdoutSK, stdout]
options:
  mapSeverity:
    db-sync-node.Subscription: Error
    db-sync-node.Mux: Error
    db-sync-node: Info
```

- [ ] **Step 4: Run test to verify it passes**

Run: `bash /tmp/palmares-compose-check.sh`
Expected: PASS. What is measured, in order: `docker compose --profile chain config -q` exits 0 (the file
is valid compose); the digest line is present (the pin is by digest, not by tag); and
`select count(*) from block` is **> 0 within 10 minutes** of the node socket appearing — that last one is
the real gate, because it can only be reached if the logging `Representation` block parsed, the
`NodeConfigFile` path resolved, and the socket mount worked. Two follow-up measurements, recorded into
`docs/adr/0001-governance-data-substrate.md` by the Phase-2 task rather than asserted here:

```bash
docker compose exec -T postgres psql -U postgres -d cexplorer -tAc \
  "select count(*) from stake_deregistration"   # must be > 0: proves the shelley flag took (D-4)
docker compose exec -T postgres psql -U postgres -d cexplorer -tAc \
  "select count(*) from pool_update"            # same probe, second table
```
`param_proposal` is **not** a valid probe for the `shelley` flag — it is populated regardless.

- [ ] **Step 5: Commit**

```bash
git add compose.yaml deploy/dbsync/db-sync-config.yaml README.md
git commit -m "feat(dev): compose stack with pinned db-sync 13.7.1.0 and the only_governance config"
```

---

### Block interfaces — produces

Names later blocks may consume, by the task that produces them.

**T-BOOTSTRAP-1**
- `CI_SHAPE` ∈ `{hosted-setup, ghcr-builder}` — the decision `ci.yml`'s `haskell` job is written from.
- The verified `ubuntu-24.04-arm` apt package list (identical to the one in `ci.yml`).
- The measured `pkg-config` state for `libblst` / `libblst-any` on the hosted runner.

**T-BOOTSTRAP-2**
- Repo `github.com/en7angled/palmares`, public, default branch `main`.
- `GH_OWNER=en7angled`; image namespace `ghcr.io/en7angled/palmares-<svc>`.
- `LICENSE` (Apache-2.0), `NOTICE`, `licenses/LICENSE.decentralized-belt-system.MIT`.
- `.gitignore`, `.dockerignore`, `.githooks/pre-push`, `docs/generated/.gitkeep`.
- `docs/adr/0001-governance-data-substrate.md` — sections `## Decision`, `## Measurements`,
  `## Consequences`.
- `docs/adr/0002-write-path-provider.md` — sections `## Decision`, `## Spike`, `## Consequences`.
- `README.md` with the ten §1.6 headings; §1 holds the independence claim, §2 the two-doors wording,
  §10 the three contributor setup commands.

**T-BOOTSTRAP-3**
- Cabal components: `lib:onchain-lib`, `lib:chainsync-lib`, `lib:offchain-lib`, `lib:webapi-lib`,
  `exe:admin`, `exe:query-api`, `exe:interaction-api`, `exe:attest-sync`, `test-suite:test`.
- `cabal.project` (pins verbatim), `cabal.project.freeze` (the CI cache key).
- Commented-out `exposed-modules` / `other-modules` blocks in `palmares.cabal`, one per component,
  verbatim from the extraction manifest — the extraction block uncomments per file.
- Builder/runtime image tags `palmares-builder:9.6.6`, `palmares-runtime-base:3.20`.
- `Dockerfile.base`, `Dockerfile.runtime-base`, `Dockerfile.admin`, `Dockerfile.query-api`,
  `Dockerfile.interaction-api`, `Dockerfile.attest-sync`.
- `scripts/build-images.sh` (manual image path, `IMAGE_PREFIX=ghcr.io/en7angled`).
- Env var names: `PALMARES_ENV_LOADED`, `PG_CONN_STR`, `PORT`, `KUPO_URL`, `BASIC_USER`, `BASIC_PASS`,
  `CORS_ALLOWED_ORIGINS`, `ATLAS_CORE_CONFIG`, `DEPLOYED_VALIDATORS_CONFIG`, `BATCH_SIZE`,
  `FETCH_BATCH_SIZE`, `ROLLBACK_MARGIN`, `PALMARES_VIEW_REFRESH_SECONDS=300`,
  `PALMARES_READY_MAX_LAG_SECONDS=300`, `PALMARES_AMBER_LAG_SECONDS=120`.
- Five `CLAUDE.md` files at `./`, `src/exe/`, `src/lib/onchain-lib/`, `src/lib/offchain-lib/`,
  `src/test/`, with the `checkFee`-selector / `LinkedList` / `Onchain.BJJ` / belt-authorization
  contracts deleted and `feeFor` documented in their place.

**T-BOOTSTRAP-4**
- `ci.yml` jobs `haskell` and `secrets` — later tasks add steps to `haskell`
  (`--write-swagger-only` guard, `write-metrics-doc` guard, and Plan C's `typescript` job).
- Published images `ghcr.io/en7angled/palmares-{admin,query-api,interaction-api,attest-sync}` tagged
  `<sha>` and `latest`.
- **Not** `reconcile.yml` and **not** the `koios-divergences` artifact: the Phase-5 parity block owns
  both, together with `scripts/reconcile_koios.sh` (see this task's scope decisions).

**T-BOOTSTRAP-5**
- `compose.yaml`: project `palmares`; profiles `chain`, `app`; services `cardano-node`, `postgres`,
  `cardano-db-sync`, `query-api`; volumes `node-db`, `node-ipc`, `postgres`, `db-sync-state`;
  env `POSTGRES_PASSWORD`, `POSTGRES_PORT`, `RESTORE_SNAPSHOT`, `GH_OWNER`, `TAG`.
- `deploy/dbsync/db-sync-config.yaml` — the single db-sync config source; the k8s ConfigMap in
  `apps-k8s-configs` is this file with `NodeConfigFile: /node-configs/config.json`.
- Database `cexplorer` on Postgres 17, reachable at `PG_CONN_STR=postgresql://postgres:<pw>@postgres:5432/cexplorer`.

---

### Block open items

- **(OPEN: `libblst-dev` may not exist in the Ubuntu 24.04 arm64 archive.** T-BOOTSTRAP-1's step-2 log
  settles it. If missing, `ci.yml` builds blst from source exactly as `Dockerfile.base` does,
  `BLST_REF=v0.3.15`.)
- **(OPEN: whether `pkg-config` resolves `libblst-any` on the hosted runner.** `cardano-crypto-class`
  `pkgconfig-depends` on `libblst-any` while the distro ships `libblst.pc`; `.envrc` already generates
  the alias locally. T-BOOTSTRAP-1's last step records it; if `MISSING`, `ci.yml` needs the same
  one-line `sed` alias step.)
- **(OPEN: publishing `palmares-builder:9.6.6` to GHCR from the laptop needs `write:packages`;** the
  local `gh` token carries only `gist, read:org, repo, workflow`. Run `gh auth refresh -s write:packages`
  or mint a classic PAT. Needed **only** on the `CI_SHAPE=ghcr-builder` branch.)
- **(OPEN: whether `gh repo create en7angled/palmares` succeeds with the current token.** `repo` +
  `read:org` should suffice for a member who may create org repos. If refused, create
  `mariusgeorgescu/palmares` and change `GH_OWNER`'s default in `compose.yaml` in the same commit.)
- **(OPEN: the first `ci.yml` cold-run wall clock.** The two notes disagree — T-INFRA §5.1 says
  60–120 min, T-EXTRACT §6.2 says 2–3 h on a 4-core hosted runner. `timeout-minutes: 350` covers both;
  record the real number from T-BOOTSTRAP-4 step 4, because the plan's schedule depends on it.)
- **(OPEN: whether Experiment A's drafter left files at
  `/Users/mg/Projects/CardanoProjects/palmares/experiments/dbsync-preprod/`.** Verified absent
  2026-10-19 — the whole `palmares` directory did not exist. Re-check at execution time and `git mv`
  rather than duplicate.)
- **(OPEN: where blinklabs' `cardano-node:11.1.2` keeps `config.json` + the genesis files**, and
  therefore which directory `cardano-db-sync` must mount for `NodeConfigFile`. T-INFRA §4.1's
  `node-db:/node-db:ro` + `NodeConfigFile: /node-db/config.json` pairing is unverified and is exactly
  what T-BOOTSTRAP-5's `select count(*) from block > 0` gate turns on. Settled by one `docker run … ls`
  before the file is written — see T-BOOTSTRAP-5 step 3. Implementer decides from the listing.)
- **(OPEN: `reconcile.yml` now belongs to the Phase-5 parity block**, not to T-BOOTSTRAP-4. The
  assembler must confirm that block creates it with `scripts/reconcile_koios.sh`, or D-8's third
  workflow and the `koios-divergences` artifact the F1 check-6 release gate reads are lost.)
- **(OPEN: `CI_SHAPE` and T-BOOTSTRAP-1's run URL** are appended here once the probe has run, as
  `CI_SHAPE=<value>` + the run URL, on one line.)

## Block 2 — Extraction from the Belt System

# Plan A — extraction from the Belt System + the CIP-129 id module

This block delivers synthesis §3 Phase 1 items 6–7: the Belt System engine, extracted into
`/Users/mg/Projects/CardanoProjects/palmares` as `Palmares.*` / package `palmares`, through the prescribed commit
sequence **as-is → rename → strip → GREEN GATE → rewrites** (the gate after *strip*, per T-EXTRACT §3.2 rule 3 as
corrected: 29 kept files import skipped modules); the `Palmares.DomainTypes.Core.Ids` CIP-129/CIP-105 encoder with its
golden test (synthesis §1.3, T-DATA §1.3); the Plan-A half of the test harness (green `cabal test`); and the two
inherited fixes that live in extracted off-chain code — #3 `authenticateStateUTxO` (one site after D-1/§4.3 cut
`Query/Live.hs`) and #5 chain-time-on-read. It implements D-14 (four libraries, no `govdata-lib`), D-1/§0.4 (no
`Query.Live`, no `RestAPI.Common`, no `multiset`), D-11 (`cassava` + `Palmares.WebAPI.CSV` in `webapi-lib`), D-9
(`Query/Rules.hs` stays skipped), and the §1.3 canonical names (`attest-sync`, `Query/Gov.hs`, `Query/Views.hs`).
**Assumptions:** the bootstrap task (synthesis §3 item 5) has already created the repo with `LICENSE`, `NOTICE`,
`licenses/`, `cabal.project` (T-EXTRACT §1.4 verbatim, no pin bumps), `palmares.cabal` (T-EXTRACT §1.3), the five
`CLAUDE.md` files, `Dockerfile.base`, `.envrc`, `.gitignore`, `hie.yaml`, `libblst.pc`, `scripts/test_exunits.sh`;
`direnv allow` + `cabal update` have run once. **Paths below are relative to that repo root**; `$SRC` =
`/Users/mg/Projects/CardanoProjects/Decentralized-Belt-System` (READ-ONLY). Fixes #1, #2 and #4 touch validators and are
**Plan B's**; the attestation payload of the 25 rewrite files is **Plan B's**; the frontend is Plan C's.
**On fixes #3 and #5 (Tasks T-EXTRACTION-5 and -6):** synthesis §3 lists both under Phase 4 step 22, because their
*wiring* (`Ingestion.projectChainEvent`, `getActivityFeed`) is attest-sync behaviour. What lands here is only the part
that is extraction hygiene — a pure helper, a deletion the §4 cut list already assigns to the extraction, and the
removal of a wall clock from a file being extracted anyway. Both wirings and both behavioural gates stay Plan B's, and
are named as such. If the assembler would rather not carry them in Plan A at all, these two tasks move intact to
Plan B's Phase 4 with no effect on Tasks 1–4.

---

### Task T-EXTRACTION-1: As-is extraction commit (byte-identical copy)

**Files:**
- Create: `extraction-keep.txt` (**69** lines — every manifest row whose verdict is not `skip`: as-is 19 + rename/strip **25** + rewrite 25. T-EXTRACT §2.8 says `rename/strip 24`; counted row by row across §2.1–§2.7 there are 25 strip rows, so the non-skip total is 69, not 68. All 69 verified to exist in `$SRC`.)
- Create: the 69 copied files, at their **Belt System paths** (`src/lib/onchain-lib/Onchain/…`, `src/exe/chain-sync/…`, `src/test/…`) — the list is in Step 3
- Test: `/tmp/verify-extraction.sh` (scratch, never committed)

**Interfaces:**
- Consumes: the bootstrapped repo; `$SRC` at a recorded `BELT_SHA`.
- Produces: `extraction-keep.txt`; `BELT_SHA` in the commit message (provenance for `NOTICE`).

- [ ] **Step 1: Write the failing test**

Write it to `/tmp/verify-extraction.sh` (scratch, not in the repo):

```bash
# every kept path must exist in the new repo and be byte-identical to the Belt System
SRC=/Users/mg/Projects/CardanoProjects/Decentralized-Belt-System
n=0; bad=0
while read -r rel; do
  n=$((n+1))
  diff -q "$SRC/$rel" "$rel" >/dev/null 2>&1 || { echo "DIFFERS/MISSING: $rel"; bad=$((bad+1)); }
done < extraction-keep.txt
echo "checked=$n differing=$bad"
test "$n" -eq 69 && test "$bad" -eq 0
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cd /Users/mg/Projects/CardanoProjects/palmares && bash /tmp/verify-extraction.sh`
Expected: FAIL — `extraction-keep.txt: No such file or directory`, `checked=0 differing=0`, exit 1.

- [ ] **Step 3: Write `extraction-keep.txt` (these 69 lines, in this order) and copy the files**

```
src/lib/webapi-lib/WebAPI/Auth.hs
src/lib/webapi-lib/WebAPI/CORS.hs
src/lib/webapi-lib/WebAPI/Errors.hs
src/lib/webapi-lib/WebAPI/ServiceProbe.hs
src/lib/webapi-lib/WebAPI/Utils.hs
src/lib/onchain-lib/Onchain/CIP68.hs
src/lib/onchain-lib/Onchain/Utils.hs
src/lib/onchain-lib/Onchain/Blueprint.hs
src/lib/onchain-lib/Onchain/Protocol.hs
src/lib/onchain-lib/Onchain/Protocol/Types.hs
src/lib/onchain-lib/Onchain/Protocol/Lookup.hs
src/lib/onchain-lib/Onchain/Protocol/Id.hs
src/lib/onchain-lib/Onchain/Validators/OracleNFTPolicy.hs
src/lib/onchain-lib/Onchain/Validators/OracleValidator.hs
src/lib/onchain-lib/Onchain/Validators/AchievementsValidator.hs
src/lib/onchain-lib/Onchain/Validators/ProfilesValidator.hs
src/lib/onchain-lib/Onchain/Validators/MintingPolicy.hs
src/lib/chainsync-lib/KupoClient.hs
src/lib/chainsync-lib/KupoAtlas.hs
src/lib/offchain-lib/Constants.hs
src/lib/offchain-lib/Utils.hs
src/lib/offchain-lib/Storage.hs
src/lib/offchain-lib/Ingestion.hs
src/lib/offchain-lib/DomainTypes/Core/Types.hs
src/lib/offchain-lib/DomainTypes/Core/Actions.hs
src/lib/offchain-lib/DomainTypes/Transfer/Types.hs
src/lib/offchain-lib/DomainTypes/Transfer/Filters.hs
src/lib/offchain-lib/DomainTypes/Transfer/OrderBy.hs
src/lib/offchain-lib/DomainTypes/Transfer/QueryResponses.hs
src/lib/offchain-lib/TxBuilding/Skeletons.hs
src/lib/offchain-lib/TxBuilding/Transactions.hs
src/lib/offchain-lib/TxBuilding/Context.hs
src/lib/offchain-lib/TxBuilding/Utils.hs
src/lib/offchain-lib/TxBuilding/Exceptions.hs
src/lib/offchain-lib/TxBuilding/Validators.hs
src/lib/offchain-lib/TxBuilding/Interactions.hs
src/lib/offchain-lib/TxBuilding/Lookups.hs
src/lib/offchain-lib/TxBuilding/Operations.hs
src/lib/offchain-lib/TxBuilding/Conversions.hs
src/exe/admin/Main.hs
src/exe/query-api/Main.hs
src/exe/query-api/QueryAppMonad.hs
src/exe/query-api/RestAPI.hs
src/exe/query-api/Query/Common.hs
src/exe/query-api/Query/Projected.hs
src/exe/query-api/Query/Aggregates.hs
src/exe/query-api/Query/Pages.hs
src/exe/query-api/Query/ServiceStatus.hs
src/exe/interaction-api/Main.hs
src/exe/interaction-api/InteractionAppMonad.hs
src/exe/interaction-api/RestAPI.hs
src/exe/interaction-api/ServiceHandlers.hs
src/exe/interaction-api/ServiceRequests.hs
src/exe/chain-sync/Main.hs
src/exe/chain-sync/ChainSyncLogic.hs
src/exe/chain-sync/ChainSyncServer.hs
src/exe/chain-sync/ChainsyncAPI.hs
src/test/Main.hs
src/test/UnitTests.hs
src/test/TestRuns.hs
src/test/Test/Helpers.hs
src/test/Test/Fixtures.hs
src/test/UnitTests/ApiHardening.hs
src/test/UnitTests/StorageRoundtrip.hs
src/test/UnitTests/ChainSyncReplay.hs
src/test/UnitTests/Conversions.hs
src/test/UnitTests/Achievement.hs
src/test/UnitTests/Oracle.hs
src/test/UnitTests/Cleanup.hs
```

```bash
SRC=/Users/mg/Projects/CardanoProjects/Decentralized-Belt-System
while read -r rel; do mkdir -p "$(dirname "$rel")" && cp "$SRC/$rel" "$rel"; done < extraction-keep.txt
```
Never `cp -r src` — the explicit list is what makes the `skip` column real (20 files / 5,590 lines of BJJ domain and
18 `mcp-server-lib` modules / 2,831 lines stay behind; `BJJPropertyTests.hs`, `UnitTests/{Membership,Promotion,MCPTools}.hs`,
`Query/{Live,Rules,ExplorerMonthly}.hs`, `RestAPI/Common.hs`, `IPFS.hs`, `WebAPI/ImageType.hs`, `Onchain/{LinkedList,BJJ}.hs`,
`Onchain/Protocol/Core.hs`, `Onchain/Validators/{Ranks,Memberships}Validator.hs`, `TxBuilding/SafeOnchainLogic.hs`,
`DomainTypes/Core/BJJ.hs`, `DomainTypes/Rules/Promotion.hs` are the absences a reviewer checks for).
Do not touch a single byte of the copies in this commit: module headers, `BJJ` identifiers and imports of skipped
modules all stay, so a reviewer can `diff` the commit to zero.

- [ ] **Step 4: Run test to verify it passes**

Run: `bash /tmp/verify-extraction.sh`
Expected: PASS — `checked=69 differing=0`, exit 0

- [ ] **Step 5: Commit**

```bash
BELT_SHA=$(git -C /Users/mg/Projects/CardanoProjects/Decentralized-Belt-System rev-parse HEAD)
git add extraction-keep.txt src/
git commit -m "feat: extract reusable libraries from Decentralized-Belt-System

Source: Decentralized-Belt-System @ ${BELT_SHA} (MIT, Copyright (c) 2025 Marius Georgescu).
Relicensed to Apache-2.0 by the copyright holder; the MIT notice is retained in
licenses/LICENSE.decentralized-belt-system.MIT per NOTICE.
File-by-file provenance and verdicts: docs/superpowers/design/f1-library-extraction.md §2.
Byte-identical copy; renames, strips and rewrites follow as separate commits."
```

---

### Task T-EXTRACTION-2: Rename commit (namespace, paths, binary names, project-named identifiers)

**Files:**
- Modify: all 69 copied `.hs` files (module header + import lines only)
- Modify: `palmares.cabal` (component names + `hs-source-dirs` + module lists to the `Palmares.*` spellings of T-EXTRACT §1.3)
- Rename: `src/lib/{onchain,offchain,chainsync,webapi}-lib/*` → `…/Palmares/*`; `src/lib/chainsync-lib/Palmares/Kupo*.hs` → `src/lib/chainsync-lib/Palmares/ChainSync/`; `src/exe/chain-sync/` → `src/exe/attest-sync/`; `src/lib/onchain-lib/Palmares/Onchain/Validators/AchievementsValidator.hs` → `AttestationsValidator.hs`; `src/test/UnitTests/Achievement.hs` → `src/test/UnitTests/Attestation.hs`; `src/test/BJJPropertyTests.hs` → not copied (skip-the-file, port-the-pattern — Plan B)
- Test: inline `grep` assertions (no committed script)

**Interfaces:**
- Consumes: Task T-EXTRACTION-1's tree.
- Produces: the `Palmares.*` namespace; executables `admin`, `query-api` (8083), `interaction-api` (8082), `attest-sync` (8084); identifiers `mkWebApp`, `runActionWithPK`, `deployValidators`, `profileInteraction`, `webApp`; log namespace `"Palmares"`.

- [ ] **Step 1: Write the failing test**

```bash
# 1. no library/exe module may still live in an old top-level namespace
grep -rn -E '^module (Onchain|WebAPI|TxBuilding|DomainTypes)\.|^module (Storage|Ingestion|Constants|Utils|KupoClient|KupoAtlas)\b' src/ --include='*.hs'
# 2. no import of an old top-level namespace
grep -rn -E '^import +(qualified +)?(Onchain|WebAPI|TxBuilding|DomainTypes)\.|^import +(qualified +)?(Storage|Ingestion|Constants|Utils|KupoClient|KupoAtlas)\b' src/ --include='*.hs'
# 3. no project-named identifier survives (T-EXTRACT §5.2 rule 1)
grep -rn -E '\b(mkBJJApp|runBJJActionWithPK|deployBJJValidators|bjjInteraction|bjjDApp|BJJDApp)\b' src/ --include='*.hs'
# 4. the old chain-sync exe dir is gone
test ! -d src/exe/chain-sync
# all four must print nothing / succeed
```

- [ ] **Step 2: Run test to verify it fails**

Run: the four checks above
Expected: FAIL — grep 1 prints **39 lines**, one per copied library module (`module Onchain.CIP68 where`,
`module Storage where`, …: webapi 5 + onchain 12 + chainsync 2 + offchain 20); grep 3 prints **99 lines**
(measured over the 69 kept files: `mkBJJApp` 8 lines, `runBJJActionWithPK` 18, `deployBJJValidators` 27,
`bjjInteraction` 44, `bjjDApp` 4, `BJJDApp` 2, minus the 4 lines matching two patterns at once —
`bjjDApp`/`BJJDApp` sit at `src/exe/interaction-api/Main.hs:31,43,50` and `src/exe/query-api/Main.hs:40,53,60`);
check 4 exits 1.

- [ ] **Step 3: Run the mechanical rename**

```bash
for lib in onchain-lib offchain-lib chainsync-lib webapi-lib; do
  mkdir -p "src/lib/$lib/Palmares"
  # CLAUDE.md must NOT move: onchain-lib/ and offchain-lib/ each carry one (bootstrap task, §3 item 5).
  (cd "src/lib/$lib" && for e in *; do
     case "$e" in Palmares|CLAUDE.md) continue;; esac
     git mv "$e" Palmares/
   done)
done
mkdir -p src/lib/chainsync-lib/Palmares/ChainSync
git mv src/lib/chainsync-lib/Palmares/KupoClient.hs src/lib/chainsync-lib/Palmares/ChainSync/KupoClient.hs
git mv src/lib/chainsync-lib/Palmares/KupoAtlas.hs  src/lib/chainsync-lib/Palmares/ChainSync/KupoAtlas.hs
git mv src/exe/chain-sync src/exe/attest-sync
git mv src/lib/onchain-lib/Palmares/Onchain/Validators/AchievementsValidator.hs \
       src/lib/onchain-lib/Palmares/Onchain/Validators/AttestationsValidator.hs
git mv src/test/UnitTests/Achievement.hs src/test/UnitTests/Attestation.hs

grep -rl . --include='*.hs' src | xargs sed -i '' -E \
  -e 's/^module (Onchain|WebAPI|TxBuilding|DomainTypes)\./module Palmares.\1./' \
  -e 's/^(import +(qualified +)?)(Onchain|WebAPI|TxBuilding|DomainTypes)\./\1Palmares.\3./' \
  -e 's/^module (Storage|Ingestion|Constants|Utils)\b/module Palmares.\1/' \
  -e 's/^(import +(qualified +)?)(Storage|Ingestion|Constants|Utils)\b/\1Palmares.\3/' \
  -e 's/\b(KupoClient|KupoAtlas)\b/Palmares.ChainSync.\1/g' \
  -e 's/\bAchievementsValidator\b/AttestationsValidator/g' \
  -e 's/\bmkBJJApp\b/mkWebApp/g' \
  -e 's/\brunBJJActionWithPK\b/runActionWithPK/g' \
  -e 's/\bdeployBJJValidators\b/deployValidators/g' \
  -e 's/\bbjjInteraction\b/profileInteraction/g' \
  -e 's/\bbjjDApp\b/webApp/g' \
  -e 's/"BJJDApp"/"Palmares"/g'
```
`BJJBelt` and the other BJJ *type* names are deliberately **not** renamed — their uses are deleted in Task T-EXTRACTION-3, not
carried forward. `sed -i ''` is the BSD/macOS form.

- [ ] **Step 4: Run test to verify it passes**

Run: the four Step-1 checks
Expected: PASS — all four print nothing and exit 0. (`cabal build all` is **not** expected to pass yet.)

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "refactor(extract): move to the Palmares.* namespace and rename components

Mechanical: module headers + imports, src/exe/chain-sync -> src/exe/attest-sync,
Achievements -> Attestations, mkBJJApp -> mkWebApp, runBJJActionWithPK -> runActionWithPK,
deployBJJValidators -> deployValidators, bjjInteraction -> profileInteraction.
No behaviour change; git show -M shows renames plus header lines only."
```

---

### Task T-EXTRACTION-3: Strip commit — GREEN GATE (`cabal build all` + `cabal test`)

**Files (29 kept files whose imports must change — measured, complete; this is every kept file that imports a
skipped module, and the line numbers are the Belt System's. Module names below are the pre-rename spellings;
after Task T-EXTRACTION-2 the library ones read `Palmares.<name>`):**
- Modify: `src/exe/admin/Main.hs:15` (`DomainTypes.Core.BJJ (BJJBelt (..), parseBelt)`)
- Modify: `src/exe/interaction-api/InteractionAppMonad.hs:31` (`IPFS (IPFSConfig)`) · `Main.hs:12` (`IPFS (initIPFSConfig)`) · `ServiceHandlers.hs:19,27` (`IPFS`, `WebAPI.ImageType`) · `ServiceRequests.hs:21` (`BJJBelt`)
- Modify: `src/exe/query-api/Query/Aggregates.hs:24,42,45` (`BJJBelt`, `Query.Live`, `RestAPI.Common`) · `Query/Common.hs:6` (`BJJBelt`) · `Query/Pages.hs:24,29,49,58` (`BJJBelt`, `Query.ExplorerMonthly`, `Query.Live`, `RestAPI.Common`) · `Query/Projected.hs:30,51` (`BJJBelt`, `Query.ExplorerMonthly`) · `RestAPI.hs:15,24,27,36` (`BJJBelt`, `Query.Live`, `Query.Rules`, `RestAPI.Common`)
- Modify: `src/lib/offchain-lib/Palmares/DomainTypes/Core/Actions.hs:15` · `Core/Types.hs:18` · `Transfer/Filters.hs:22` · `Transfer/QueryResponses.hs:60,62` (`BJJBelt`, `DomainTypes.Rules.Promotion (PromotionViolation)`)
- Modify: `src/lib/offchain-lib/Palmares/Ingestion.hs:13` (`Onchain.LinkedList (NodeDatum (..))`) · `Storage.hs:34` (`BJJBelt`) · `TxBuilding/Conversions.hs:21` (`SafeOnchainLogic (safeIntToBelt)`) · `TxBuilding/Lookups.hs:9,18,28` · `TxBuilding/Operations.hs:12,14,19,24,25,27,31` (`Onchain.BJJ`, `Onchain.LinkedList`, `MembershipsValidator`, `RanksValidator` ×2, the ranks/memberships accessors of `TxBuilding.Context`, `SafeOnchainLogic`) · `TxBuilding/Validators.hs:10,15` (`MembershipsValidator.membershipsCompile`, `RanksValidator.ranksCompile`)
- Modify: `src/lib/onchain-lib/Palmares/Onchain/Blueprint.hs:15,19` · `Onchain/Protocol.hs:14` (`Onchain.Protocol.Core`) · `Onchain/Protocol/Types.hs:8` (`Onchain.LinkedList (NodeDatum (..))`) · `Onchain/Validators/MintingPolicy.hs:25` (`Onchain.BJJ (BeltSnapshot (..), intToBelt, validatePromotion)`)
- Modify: `src/test/Main.hs:1` (`BJJPropertyTests`) · `TestRuns.hs:38` (`RanksValidator`) · `UnitTests.hs:12,13,15` (`UnitTests.MCPTools`, `.Membership`, `.Promotion`) · `UnitTests/Attestation.hs:17` (`Onchain.BJJ`) · `UnitTests/ApiHardening.hs:21` (`WebAPI.ImageType`)
- Modify: `src/lib/offchain-lib/Palmares/TxBuilding/Context.hs` (strip `DeployedScriptsContext` to minting policy / profiles / attestations / disputes / oracle + `oracleNFTAssetClass`, per T-EXTRACT §2.4) · `Palmares/Onchain/Protocol/Lookup.hs` (drop `unsafeGetRank`, `unsafeGetListNodeDatumAndValue`, `unsafeGetMembershipInterval`) · `Palmares/Constants.hs` · `Palmares/WebAPI/Utils.hs` · `src/exe/query-api/QueryAppMonad.hs` (drop `liveProjection`) · `Query/ServiceStatus.hs` · `src/test/UnitTests/Conversions.hs` (the one stripped test module Plan A builds; `Oracle.hs` and `Cleanup.hs` are **not** stripped here — they are out of `other-modules`, so nothing compiles them, and their strip is Plan B's when it puts them back)
- Modify: `palmares.cabal` — module lists to T-EXTRACT §1.3 exactly (`webapi-lib` gains `Palmares.WebAPI.CSV` + `cassava` per D-11; `query-api` has no `Query.Live`/`Query.Rules`/`Query.ExplorerMonthly`/`RestAPI.Common` and no `multiset`; test-suite `other-modules` = `Test.Helpers, UnitTests, UnitTests.ApiHardening, UnitTests.ChainSyncReplay, UnitTests.Conversions, UnitTests.StorageRoundtrip` — `Main` is `main-is`, not an other-module, and §1.3's `UnitTests.Profile` names no file in the manifest), test-suite `build-depends` loses `hedgehog`/`tasty-hedgehog` until Plan B's `PropertyTests` (`-Wunused-packages` fails the build otherwise)
- Test: `cabal build all`, `cabal test --test-show-details=direct`

**Interfaces:**
- Consumes: Task T-EXTRACTION-2's tree.
- Produces: a compiling four-library/four-executable package; `Palmares.WebAPI.{Auth,CORS,Errors,ServiceProbe,Utils,CSV}`; `Palmares.ChainSync.{KupoClient,KupoAtlas}`; `Palmares.Storage` (`OnchainMatchEvent`, `putKupoMatch`, `putMatchAndProjections`, `rollbackTo`, `replayOrder`, `runMigrations`, `readSchemaProbe`, `wipeChainSyncTablesRaw`, `chainSyncTableNames`, `upsertByUnique`, `currentSchemaVersion = 1`); `Palmares.TxBuilding.{Skeletons,Transactions,Context,Utils,Exceptions,Validators,Interactions}`; `Query/Common.hs`'s `normalizeLimitOffset` (default 100, clamp [1,500]), `normalizeOrder`, `applyLimits`, `applyFilterOrderLimit`; `Test.Helpers.{assert,queryOracle}`.

**Harness disposition** — Plan A builds: `Test/Helpers.hs` (as-is), `UnitTests/StorageRoundtrip.hs` (as-is, in-memory
SQLite `runSqlite ":memory:"`), `UnitTests/ApiHardening.hs` (minus the 4-case `imageTests` group + its import + its
list entry; 22 → 18 cases), `UnitTests/Conversions.hs`, `UnitTests/ChainSyncReplay.hs` (keep the 2 bare
`replayOrder` cases in `chainSyncReplayTests` and the 3-case `valueConversionTests`; drop `classifyTests` (2 cases)
and `deletionSequencingTests` (1 case) with their `promotionMatch`/`rankMatch` fixtures — they drive
`projectChainEvent` on `OnchainRank` datums at `ranksValidatorHashGY`; 8 → 5 cases).
**Plan B's**, in two groups. Copied by Task T-EXTRACTION-1 and tracked, but *not* in `other-modules` until rewritten
(no stub payloads typed twice): `TestRuns.hs` (the CLB `asUser … buildTxBody >>= signAndSubmitConfirmed` runners,
`deployReferenceScriptRun`, `mintTestOracleNFT`, `sendDustToValidator`), `Test/Fixtures.hs`,
`UnitTests/Attestation.hs`, `UnitTests/Oracle.hs`, `UnitTests/Cleanup.hs`. Created from nothing by Plan B (no Belt
System source, so not in `extraction-keep.txt`): `UnitTests/Dispute.hs` and `PropertyTests.hs` (the ~60-line
mirror-and-pin port:
one hedgehog generator over the new datum + one `property` asserting off-chain predicate ≡ on-chain predicate). The
`mkTestFor` emulator pattern itself is `atlas-cardano`'s and needs no extraction — only a `deployValidators` that
compiles, which is Plan B's.

**Rewrite set left stubbed by this commit (25 files / 10,614 source lines, T-EXTRACT §2.8) — one line each:**

| File (under `src/`) | The rewrite | Owner |
|---|---|---|
| `lib/onchain-lib/Palmares/Onchain/Protocol/Types.hs` | keep `ProtocolParams`/`FeeConfig`/`OracleParams`/`OracleAdminAction`/`applyOracleAdminAction` + the `makeIsDataSchemaIndexed` stable-index discipline; new attestation/profile/dispute payload types; fixes #1+#2 | Plan B |
| `lib/onchain-lib/Palmares/Onchain/Protocol/Id.hs` | keep the `blake2b_224` 28-byte-name derivation; new `deriveAttestationId`, `deriveDisputeId` | Plan B |
| `lib/onchain-lib/Palmares/Onchain/Protocol.hs` | re-export barrel; new list | Plan B |
| `lib/onchain-lib/Palmares/Onchain/Validators/MintingPolicy.hs` | keep `opPaused` gate + per-redeemer `checkFee` + exact-mint + seed-TxOutRef one-shot names; redeemers `ClaimProfile`/`IssueAttestation`/`OpenDispute`; fixes #1+#4 | Plan B |
| `lib/offchain-lib/Palmares/Storage.hs` | keep the machinery (`PersistField Integer`/`GYTime`, `upsertByUnique`, cursor, raw log, `replayOrder`, `rollbackTo`); 6 projection entities → 3; `currentSchemaVersion = 1`; fix #5 as a removal (Task T-EXTRACTION-6) | Plan B payload, Task T-EXTRACTION-6 removal |
| `lib/offchain-lib/Palmares/Ingestion.hs` | same address-dispatch shape, 3 branches; fix #3's single call site (Task T-EXTRACTION-5 guard) | Plan B |
| `lib/offchain-lib/Palmares/TxBuilding/Lookups.hs` | keep `getUTxOWithNFT`, `getUTxOWithTokenAtAddresses`, `queryOracleParams`; `getAllParsedDatumsAtValidator` + its 4 wrappers **deleted** (Task T-EXTRACTION-5) | Task T-EXTRACTION-5 + Plan B |
| `lib/offchain-lib/Palmares/TxBuilding/Operations.hs` | keep `getFeeSkeleton`, `getOracleFeeAndValiditySkeleton`, `applyAdminAction`, `adminActionToOracleAdminAction`; 6 BJJ ops → 5; fixes #1+#4 | Plan B |
| `lib/offchain-lib/Palmares/TxBuilding/Conversions.hs` | the onchain↔domain bridge; shape survives, types do not | Plan B |
| `lib/offchain-lib/Palmares/DomainTypes/Core/Types.hs` | new domain types; keep `deriving-aeson` StripPrefix+CamelToSnake + `ToSchema` conventions | Plan B |
| `lib/offchain-lib/Palmares/DomainTypes/Core/Actions.hs` | keep `AdminActionType` + the action-sum shape; add `SetAdminAction` (fix #2) | Plan B |
| `lib/offchain-lib/Palmares/DomainTypes/Transfer/Types.hs` | new transfer DTOs | Plan B |
| `lib/offchain-lib/Palmares/DomainTypes/Transfer/Filters.hs` | governance + attestation filters; keep the `*FromParams` convention | Plan A (API block) |
| `lib/offchain-lib/Palmares/DomainTypes/Transfer/OrderBy.hs` | governance sort enums; keep `SortOrder` + the enum/`FromHttpApiData`/`ToParamSchema` triple | Plan A (API block) |
| `lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs` | all new DTOs (`DRepSummary`, `GovActionSummary`, `VoteRecord`, `Page`, …) | Plan A (API block) |
| `exe/query-api/RestAPI.hs` | keep the `swagger :<|> probe :<|> public` composition + `Summary`/`Description` discipline; **no `BasicAuth` on read routes**; `Header "Accept"` CSV on the nine endpoints (D-11) | Plan A (API block) |
| `exe/query-api/Query/Projected.hs` | esqueleto over `palmares_attest` (profiles/attestations/disputes); `getActivityFeed` orders on `created_at_slot` (Task T-EXTRACTION-6 contract) | Plan B |
| `exe/query-api/Query/Aggregates.hs` | network dashboard aggregates | Plan A (API block) |
| `exe/query-api/Query/Pages.hs` | one-screen-one-request composites; keep the pattern | Plan A (API block) |
| `exe/interaction-api/ServiceRequests.hs` | keep the used-addresses + change-address + optional-collateral request shape exactly (the CIP-30 contract) | Plan B |
| `exe/admin/Main.hs` | keep the `Either ProviderCtx TxBuildingContext` split, `Command` + `hsubparser` + `executeCommand`, the shared parsers, `deploy-reference-scripts` + `write-blueprint`; new `rotate-admin`, `resolve-dispute`, `apply-indexes`, `reconcile-koios`, `write-metrics-doc` | Plan A (infra/parity blocks) + Plan B |
| `test/TestRuns.hs` | `deployValidators` over the new validator set; keep the runner + `malicious*` conventions | Plan B |
| `test/UnitTests/Attestation.hs` | two-phase pending→accept coverage incl. the double-accept negative; fix #4's cases | Plan B |
| `test/UnitTests/ChainSyncReplay.hs` | keep replay-order + idempotence + rollback shape; fixes #3+#5 cases | Tasks T-EXTRACTION-5–6 + Plan B |
| `test/Test/Fixtures.hs` | new fixtures | Plan B |

- [ ] **Step 1: Write the failing test**

Run: `cabal build all` (it is the test: the gate is "compiles with every skipped module gone").

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal build all`
Expected: FAIL with `Could not find module ‘Palmares.Onchain.LinkedList’` / `‘Palmares.DomainTypes.Core.BJJ’` /
`‘IPFS’` / `‘Query.Live’` — the library-namespace ones carry the `Palmares.` prefix Task T-EXTRACTION-2's `sed` put on
them, the executable-local ones (`IPFS`, `Query.Live`, `Query.Rules`, `Query.ExplorerMonthly`, `RestAPI.Common`) do
not, because exe modules are not namespaced. One error per row of the 29-file list above.

- [ ] **Step 3: Remove each skipped import and every declaration that uses it**

Rule for the pass: delete the import, then delete the smallest enclosing declaration that no longer type-checks;
never re-type a payload here — a rewrite-class file may end this commit as a module that exports less. Named
survivors (do not delete): `escapeLikePattern` + `getPortFromEnvOrDefault` (`WebAPI/Utils.hs`),
`normalizeLimitOffset` (`Query/Common.hs`), both error-handling shapes in `QueryAppMonad.hs`,
`txBuildingExceptionToHttpStatus` with its insufficient-funds→400 mapping (`TxBuilding/Exceptions.hs`),
`safeEraTime = 25920` + `txIsValidForSafeEra` (`TxBuilding/Skeletons.hs`), `defaultRollbackMargin = 2160`
(`Constants.hs`), the startup order in `src/exe/attest-sync/Main.hs` (schema probe → wipe → migrate → cursor → fork
probe → loop). In `WebAPI/Utils.hs`'s `addSharedSwaggerDescriptions`, replace the `BJJBelt`/`ProfileType` entries with
`GYPubKeyHash`, `DRepId`, `GovActionId`, `AttestationType`. In `src/test/UnitTests.hs` the tree becomes
`[apiHardeningTests, chainSyncReplayTests, conversionsTests, storageRoundtripTests]` and the group name drops "BJJ";
`src/test/Main.hs` becomes `defaultMain (testGroup "Tests" [unitTests])`.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal build all && cabal test --test-show-details=direct`
Expected: PASS — build exits 0; `cabal test` reports **31 cases in 4 groups** (ApiHardening 18, ChainSyncReplay 5,
Conversions 5, StorageRoundtrip 3), `All 31 tests passed`.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "refactor(extract): strip BJJ-specific members; green gate

Removes every import of a skipped module (29 files) and the declarations that used them,
reduces palmares.cabal to the F1 module set, and reduces the test tree to the 31 cases that
survive extraction. cabal build all + cabal test are green from here; the 25 rewrite files
carry no payload yet."
```

---

### Task T-EXTRACTION-4: `Palmares.DomainTypes.Core.Ids` — CIP-129 / CIP-105 identifiers

**Files:**
- Create: `src/lib/offchain-lib/Palmares/DomainTypes/Core/Ids.hs`
- Modify: `palmares.cabal` — `offchain-lib`: `exposed-modules` += `Palmares.DomainTypes.Core.Ids`, `build-depends` += `bech32`; test-suite: `other-modules` += `UnitTests.GovIds`, `build-depends` += `bech32, base16-bytestring` (the golden test decodes our own output and hex-decodes the fixture; both are already in `cabal.project.freeze` — `any.bech32 ==1.1.8`, `any.base16-bytestring ==1.0.2.0` — so neither is a new pin)
- Create: `src/test/UnitTests/GovIds.hs`
- Modify: `src/test/UnitTests.hs` (add `govIdsTests` to the tree)

**Interfaces:**
- Consumes: nothing from earlier tasks but Task T-EXTRACTION-3's green gate.
- Produces: `drepIdCip129 :: Bool -> ByteString -> Text` · `drepIdCip105 :: ByteString -> Text` · `ccHotIdCip129 :: Bool -> ByteString -> Text` · `ccColdIdCip129 :: Bool -> ByteString -> Text` · `govActionIdCip129 :: ByteString -> Word8 -> Text` · `parseDrepIdAny :: Text -> Either Text (Maybe Bool, ByteString)`.

We compute **every** governance bech32 ourselves: db-sync's `drep_hash.view` is **CIP-105** (`serialiseToBech32
"drep"` over the raw 28 bytes, same HRP for key and script, so it cannot be compared against Koios/GovTool/wallets),
`committee_hash` has **no `view` column at all** (only `id`, `raw`, `has_script`) so all four `cc_hot`/`cc_cold` forms
are ours, and `gov_action_proposal` has no bech32 id. Header byte = `(key-type << 4) | credential-type`:

| Identifier | HRP | header (key hash) | header (script hash) | payload |
|---|---|---|---|---|
| CC hot | `cc_hot` | `0x02` | `0x03` | 1 + 28 bytes |
| CC cold | `cc_cold` | `0x12` | `0x13` | 1 + 28 bytes |
| DRep (CIP-129) | `drep` | `0x22` | `0x23` | 1 + 28 bytes |
| DRep (CIP-105) | `drep` | *(none)* | *(none)* | 28 bytes |
| gov action | `gov_action` | *(none)* | *(none)* | 32-byte tx id ‖ 1-byte index = 33 bytes |

**(DEVIATION from T-DATA §1.3)** `parseDrepIdAny` returns `Maybe Bool`, not `Bool`: the CIP-105 and raw-hex forms do
**not** carry the script flag (HRP `drep` is used for both), so `Nothing` means "resolve `has_script` from
`drep_hash` by `raw`". Returning `False` there would silently mis-identify every script DRep at the id boundary that
the Koios reconciliation joins on. The two reserved views `drep_always_abstain` / `drep_always_no_confidence` are
**not** this module's business — they are literals handled in the DTO layer (T-API §7.2).

- [ ] **Step 1: Write the failing test**

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- | Golden vectors for the CIP-129/CIP-105 identifier codec. One real preprod id read from Koios on
-- 2026-10-19 (T-METRIC §4); the rest are the same 28 bytes re-encoded per T-DATA §1.3's header table.
module UnitTests.GovIds (govIdsTests) where

import Codec.Binary.Bech32 qualified as Bech32
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.Text (Text)
import Data.Text qualified as T
import Palmares.DomainTypes.Core.Ids
import Test.Tasty
import Test.Tasty.HUnit

-- | "Bitcows" preprod DRep, key credential (T-METRIC §4).
bitcowsRaw :: ByteString
bitcowsRaw = B16.decodeLenient "81378d3ea89b68f0fc09944b1f93f2feb851318f196f98ba4029d8a8"

bitcowsCip129 :: Text
bitcowsCip129 = "drep1y2qn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qmwj3xg"

-- | The other six renderings of the SAME 28 bytes. Derived from T-DATA §1.3's header table with an
-- independent BIP-173 reference implementation, so they pin header byte, HRP and 5-bit regrouping.
bitcowsCip129Script, bitcowsCip105, bitcowsCcHot, bitcowsCcHotScript, bitcowsCcCold, bitcowsCcColdScript :: Text
bitcowsCip129Script = "drep1ywqn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qmuzqx0"
bitcowsCip105 = "drep1symc604gnd50plqfj393lyljl6u9zvv0r9he3wjq98v2sz3vtkp"
bitcowsCcHot = "cc_hot1q2qn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qf6qm49"
bitcowsCcHotScript = "cc_hot1qwqn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qfgs24z"
bitcowsCcCold = "cc_cold1z2qn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qlx580v"
bitcowsCcColdScript = "cc_cold1zwqn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32ql5yk0t"

-- | 32-byte tx id placeholder for the gov-action shape test.
txid32 :: ByteString
txid32 = BS.replicate 32 0xab

-- | Decode one of our own ids back to (hrp, payload bytes).
dec :: Text -> (Text, ByteString)
dec t = case Bech32.decode t of
  Right (hrp, dp) | Just bs <- Bech32.dataPartToBytes dp -> (Bech32.humanReadablePartToText hrp, bs)
  _ -> error ("undecodable: " <> T.unpack t)

govIdsTests :: TestTree
govIdsTests =
  testGroup
    "CIP-129 / CIP-105 identifiers"
    [ testCase "Bitcows DRep encodes to the Koios CIP-129 id" $
        drepIdCip129 False bitcowsRaw @?= bitcowsCip129,
      testCase "DRep key credential: hrp drep, 29 bytes, header 0x22" $
        let (hrp, bs) = dec (drepIdCip129 False bitcowsRaw)
         in (hrp, BS.length bs, BS.head bs, BS.tail bs) @?= ("drep", 29, 0x22, bitcowsRaw),
      testCase "DRep script credential: header 0x23" $
        (drepIdCip129 True bitcowsRaw, BS.head (snd (dec (drepIdCip129 True bitcowsRaw))))
          @?= (bitcowsCip129Script, 0x23),
      testCase "CIP-105 DRep: hrp drep, 28 bytes, no header" $
        (drepIdCip105 bitcowsRaw, dec (drepIdCip105 bitcowsRaw)) @?= (bitcowsCip105, ("drep", bitcowsRaw)),
      testCase "cc_hot key credential: header 0x02" $
        let (hrp, bs) = dec (ccHotIdCip129 False bitcowsRaw)
         in (hrp, BS.head bs, ccHotIdCip129 False bitcowsRaw) @?= ("cc_hot", 0x02, bitcowsCcHot),
      testCase "cc_hot script credential: header 0x03" $
        (ccHotIdCip129 True bitcowsRaw, BS.head (snd (dec (ccHotIdCip129 True bitcowsRaw))))
          @?= (bitcowsCcHotScript, 0x03),
      testCase "cc_cold key credential: header 0x12" $
        let (hrp, bs) = dec (ccColdIdCip129 False bitcowsRaw)
         in (hrp, BS.head bs, ccColdIdCip129 False bitcowsRaw) @?= ("cc_cold", 0x12, bitcowsCcCold),
      testCase "cc_cold script credential: header 0x13" $
        (ccColdIdCip129 True bitcowsRaw, BS.head (snd (dec (ccColdIdCip129 True bitcowsRaw))))
          @?= (bitcowsCcColdScript, 0x13),
      testCase "gov action: hrp gov_action, 33 bytes = txid || index, no header" $
        dec (govActionIdCip129 txid32 0) @?= ("gov_action", txid32 <> BS.singleton 0x00),
      testCase "parseDrepIdAny round-trips all three forms and rejects garbage" $ do
        parseDrepIdAny (drepIdCip129 True bitcowsRaw) @?= Right (Just True, bitcowsRaw)
        parseDrepIdAny (drepIdCip129 False bitcowsRaw) @?= Right (Just False, bitcowsRaw)
        parseDrepIdAny (drepIdCip105 bitcowsRaw) @?= Right (Nothing, bitcowsRaw)
        parseDrepIdAny "81378d3ea89b68f0fc09944b1f93f2feb851318f196f98ba4029d8a8"
          @?= Right (Nothing, bitcowsRaw)
        parseDrepIdAny "drep_always_abstain" @?= Left "invalid_drep_id"
    ]
```
The first case is the one that catches a wrong header byte, a wrong HRP **and** a wrong 5-bit regrouping at once:
`drep1y2qn0rf…` is the live Koios rendering of exactly `0x22 ‖ 81378d3e…29d8a8` (T-METRIC §4). The other six
renderings of the same hash are exact-equality vectors computed from T-DATA §1.3's header table with an independent
BIP-173 implementation; `0x02`/`0x03`, `0x12`/`0x13` and `0x22`/`0x23` differ only below the top 5 bits, so each
key/script pair shares its first data char (`q`, `z`, `y`) and diverges only at the second (`2` vs `w`) — which is
exactly where a hand-rolled 5-bit regrouping goes wrong.
**(OPEN: only the `drep1y2qn0rf…` vector is a value read off Koios. T-DATA §1.3's live `cc_hot1qddl5lv…` /
`cc_cold1zds4k4q…` / `gov_action1qnergwfc…` observations are truncated AND belong to other credentials whose raw
hex the notes do not record, so they cannot be turned into vectors. To cross-check the committee forms against a
third party, the **owner or the data block** must supply one real preprod `committee_hash.raw` + its Koios
`cc_hot_id`/`cc_cold_id`; until then the six derived vectors stand as a self-consistency pin, not independent
confirmation.)**

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-show-details=direct`
Expected: FAIL at compile time with `Could not find module ‘Palmares.DomainTypes.Core.Ids’`.

- [ ] **Step 3: Implement the six exports in `src/lib/offchain-lib/Palmares/DomainTypes/Core/Ids.hs`**

Signatures exactly as in **Interfaces** above, Haddock on each. One helper, over the measured bech32-1.1.8 API
(`encodeLenient :: HumanReadablePart -> DataPart -> Text`,
`humanReadablePartFromText :: Text -> Either HumanReadablePartError HumanReadablePart`):

```haskell
encodeWith :: Text -> ByteString -> Text
encodeWith h bs =
  Bech32.encodeLenient
    (either (error "Palmares.DomainTypes.Core.Ids: literal HRP rejected") id (Bech32.humanReadablePartFromText h))
    (Bech32.dataPartFromBytes bs)
```

`encodeLenient`, not `encode`: the 90-char BIP-173 limit is irrelevant at 29/33 bytes and `encode`'s `Either` would
force a partial function on a total operation. The four HRPs are module-level literals, so the `error` is
unreachable by construction. Every exported encoder is then `encodeWith hrp (BS.cons header raw)`, and
`govActionIdCip129 txid ix = encodeWith "gov_action" (txid <> BS.singleton ix)`. `parseDrepIdAny` dispatches on the
decoded payload length: 29 → `(Just (BS.head == 0x23), BS.tail)` rejecting any other header byte, 28 →
`(Nothing, payload)`, 56-char hex → `(Nothing, decoded)`, anything else → `Left "invalid_drep_id"` (the error code
T-API §1.1 returns as 400).

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-show-details=direct`
Expected: PASS — 41 cases (31 from Task T-EXTRACTION-3 + the 10-case `CIP-129 / CIP-105 identifiers` group).

- [ ] **Step 5: Commit**

```bash
git add src/lib/offchain-lib/Palmares/DomainTypes/Core/Ids.hs src/test/UnitTests/GovIds.hs src/test/UnitTests.hs palmares.cabal
git commit -m "feat(ids): CIP-129 and CIP-105 governance identifiers with golden vectors

db-sync stores only the CIP-105 drep view and no committee bech32 at all, so every id the API
publishes and the Koios reconciliation joins on is computed here. Golden test pins the live
preprod Bitcows DRep id and the four header bytes."
```

---

### Task T-EXTRACTION-5: Inherited fix #3 — `authenticateStateUTxO`, and the unguarded scanner deleted

**Files:**
- Modify: `src/lib/offchain-lib/Palmares/TxBuilding/Utils.hs` (add `authenticateStateUTxO` next to `extractNFTAssetClass`)
- Modify: `src/lib/offchain-lib/Palmares/TxBuilding/Lookups.hs` (delete `getAllParsedDatumsAtValidator` and its four wrappers `getAllOnchainValidRanks`, `getAllOnchainProfiles`, `getAllMembershipDatums`, `getAllAchievements` — Belt System L134-138 and L141/154/353/468. Their five same-file dependents — `getAllPromotions` L143, `getAllRanks` L148, `getAllProfiles` L156, and the two membership-history readers at L366/L379 — are BJJ-typed and are already gone with Task T-EXTRACTION-3's strip; `cabal build all` names any that are not.)
- Test: `src/test/UnitTests/ChainSyncReplay.hs` (new `authenticationTests` group)

**Interfaces:**
- Consumes: `Palmares.ChainSync.KupoAtlas.toGYAssetClass`, `Palmares.TxBuilding.Utils.extractNFTAssetClass`.
- Produces: `authenticateStateUTxO :: GYMintingPolicyId -> GYAssetClass -> GYValue -> Bool`.

Fix #3 has **exactly one implementation site**, `Ingestion.projectChainEvent` (synthesis §4 / T-EXTRACT §4.3): the
second site, `getAllParsedDatumsAtValidator`, is dead code once `Query/Live.hs` is cut (every caller of its four
wrappers lived there), so it is **deleted, not guarded** — and nothing may reintroduce a whole-address-scan helper
without the guard. The call-site wiring is Plan B's, because `projectChainEvent`'s three branches are the attestation
payload: **Plan B's Ingestion rewrite is gated on** passing `amValue` on **every** branch, returning `NoEvent am`
unless `authenticateStateUTxO ourPolicy (idOf datum) amValue`, still writing the raw match to `onchain_match_event`,
and on the two negative cases of T-EXTRACT §4.3 ("a well-formed datum with no NFT in the value is not projected",
"a datum naming id X in a UTxO holding NFT Y is not projected") asserting projection count unchanged **and** raw-log
count +1.

- [ ] **Step 1: Write the failing test**

```haskell
-- in src/test/UnitTests/ChainSyncReplay.hs; add authenticationTests to the module's testGroup list
ac :: Text -> GYAssetClass
ac t = either (error "bad fixture") id (toGYAssetClass t)

-- 56 hex chars = the 28-byte script hash GYMintingPolicyId's IsString instance requires; 58 would `error`.
ourPolicy :: GYMintingPolicyId
ourPolicy = "c0ffee00000000000000000000000000000000000000000000000001"

acX, acY, acForeign :: GYAssetClass
acX = ac "c0ffee00000000000000000000000000000000000000000000000001.5801"  -- id X under our policy
acY = ac "c0ffee00000000000000000000000000000000000000000000000001.5902"  -- id Y under our policy
acForeign = ac "deadbeef000000000000000000000000000000000000000000000002.5801"  -- someone else's policy

authenticationTests :: TestTree
authenticationTests =
  testGroup
    "state-UTxO authentication (inherited fix #3)"
    [ testCase "a datum's own NFT, under our policy, authenticates" $
        authenticateStateUTxO ourPolicy acX (valueSingleton acX 1 <> valueFromLovelace 2_000_000) @?= True,
      testCase "no NFT in the value is not authentic (the free-dust forgery)" $
        authenticateStateUTxO ourPolicy acX (valueFromLovelace 2_000_000) @?= False,
      testCase "a datum naming id X in a UTxO holding NFT Y is not authentic" $
        authenticateStateUTxO ourPolicy acX (valueSingleton acY 1 <> valueFromLovelace 2_000_000) @?= False,
      testCase "the right token name under someone else's policy is not authentic" $
        authenticateStateUTxO ourPolicy acForeign (valueSingleton acForeign 1) @?= False,
      testCase "two copies of the NFT is not authentic" $
        authenticateStateUTxO ourPolicy acX (valueSingleton acX 2) @?= False,
      testCase "GYLovelace as the claimed id is never authentic" $
        authenticateStateUTxO ourPolicy GYLovelace (valueFromLovelace 2_000_000) @?= False
    ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-show-details=direct`
Expected: FAIL at compile time — `Variable not in scope: authenticateStateUTxO`.

- [ ] **Step 3: Implement `authenticateStateUTxO :: GYMintingPolicyId -> GYAssetClass -> GYValue -> Bool` in `Palmares/TxBuilding/Utils.hs`**

```haskell
-- | A protocol state UTxO is authentic iff its value carries exactly one of the NFT named by
-- (or derived from) its own datum, under OUR minting policy. Address + parseable datum is not
-- enough: anyone can pay a forged datum to a script address.
authenticateStateUTxO :: GYMintingPolicyId -> GYAssetClass -> GYValue -> Bool
authenticateStateUTxO ourPolicy ac v =
  case ac of
    GYToken pid _ -> pid == ourPolicy && valueAssetClass v ac == 1
    GYLovelace -> False
```
Then delete `getAllParsedDatumsAtValidator` and its four wrappers from `Lookups.hs`.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal build all && cabal test --test-show-details=direct && ! grep -rn "getAllParsedDatumsAtValidator\|getAllOnchainValidRanks\|getAllOnchainProfiles\|getAllMembershipDatums\|getAllAchievements\|getAllAuthenticStateAtValidator" src/`
Expected: PASS — build exits 0 (which is also what proves no caller of the five deleted names survives), 47 cases,
and the `grep` prints nothing (exit 1, so the `!` succeeds).

- [ ] **Step 5: Commit**

```bash
git add src/lib/offchain-lib/Palmares/TxBuilding/Utils.hs src/lib/offchain-lib/Palmares/TxBuilding/Lookups.hs src/test/UnitTests/ChainSyncReplay.hs
git commit -m "fix(ingestion): authenticate state UTxOs by their own NFT (inherited fix #3)

A parseable datum at a script address proves nothing: 2 ADA plus a hand-crafted inline datum
poisoned the projections for free. One guard in the shared helper; the unguarded
whole-address-scan helper is deleted with Query/Live.hs rather than fixed in four callers.
Wiring into projectChainEvent's branches lands with the Ingestion rewrite."
```

---

### Task T-EXTRACTION-6: Inherited fix #5 — chain time on read, no stored wall clock

**Files:**
- Modify: `src/lib/offchain-lib/Palmares/Storage.hs` (remove `getCurrentTime`; Belt System sites L29 import + L340, L358, L374, L397, L446, L464)
- Modify: `src/lib/offchain-lib/Palmares/TxBuilding/Utils.hs` (the two pure converters, next to the existing `pPOSIXTimeFromGYSlot`/`gySlotFromPOSIXTime`)
- Modify: `src/exe/query-api/QueryAppMonad.hs` (`requestSlotConfig`, next to `providerContext`)
- Test: `src/test/UnitTests/ChainSyncReplay.hs` (add a `chainTimeTests` group to the module's `testGroup` list)

**Interfaces:**
- Consumes: `QueryAppContext`'s `providerContext :: ProviderCtx` (`QueryAppMonad.hs:34`), `Palmares.TxBuilding.Context.runQuery :: ProviderCtx -> GYTxQueryMonadIO a -> IO a` (`Context.hs:90`), Atlas's `slotConfig`, `slotToBeginTimePure`, `enclosingSlotFromTimePure`, `simpleSlotConfig`.
- Produces: `slotToUTC :: GYSlotConfig -> Integer -> UTCTime` and `utcToSlot :: GYSlotConfig -> UTCTime -> Integer` in `Palmares.TxBuilding.Utils` · `requestSlotConfig :: QueryAppMonad GYSlotConfig` in `QueryAppMonad` · the contract "`created_at_slot` is the only time in a projection row".

The converters live in `offchain-lib`, **not** in `src/exe/query-api/Query/Common.hs`: a cabal test-suite cannot
import an executable's `other-modules`, so sited there they would be untestable. `requestSlotConfig` is the only
half that needs `ProviderCtx`, so it is the only half that stays in the executable.

The fix is a **removal plus a read-time conversion**, not a new column: `createdAtSlot` is already the chain's own
clock and is immune to replay, while `insertedAt` is re-stamped by every `rollbackTo` + replay, which silently
renumbers the whole activity feed. So: `insertedAt` is never serialized (it appears in no `QueryResponses` DTO and in
no `SELECT` list of a public endpoint), `slotToBeginTime` is **monadic** (`GYTxQueryMonad f => GYSlot -> f GYTime`,
Atlas `TxBuilder/Class.hs:556`) so it must never be called per row — fetch the slot config once per request
and map rows with the pure `slotToBeginTimePure`; translate a `since :: UTCTime` query param time→slot with
`enclosingSlotFromTimePure` **before** the SQL so the `created_at_slot` index is used.
**Plan B's gate on the Storage/Ingestion rewrite** is the replay-stability case of T-EXTRACT §4.5: project slots
[S1,S2,S3] → snapshot the API-shaped feed → `rollbackTo (S1-1)` → re-project → assert the snapshot is
**byte-identical**. That case needs a projection entity, which is Plan B's payload; step 3 alone would pass on a
half-fix that stamps a chain-derived `createdAt` at projection time, so it must not be dropped.
Atlas's pure API, read off the 0.14.1 interface (`GeniusYield/Types/SlotConfig.hs`): `slotToBeginTimePure ::
GYSlotConfig -> GYSlot -> GYTime`, `enclosingSlotFromTimePure :: GYSlotConfig -> GYTime -> Maybe GYSlot` (`Nothing`
only before system start), `simpleSlotConfig :: UTCTime -> NominalDiffTime -> GYSlotConfig`,
`unsafeSlotFromInteger :: Integer -> GYSlot`, and `GYTime` converts via `timeToPOSIX`/`timeFromPOSIX` only — there is
no `timeToUTC`/`timeFromUTC`.

- [ ] **Step 1: Write the failing test**

```bash
# 1. the wall clock must be gone from the projection writers
grep -c "getCurrentTime" src/lib/offchain-lib/Palmares/Storage.hs
# 2. insertedAt must be confined to Storage.hs's persist block: no DTO and no query module may name it
grep -rn "insertedAt" src/lib/offchain-lib/Palmares/DomainTypes/ src/exe/query-api/
```
```haskell
-- chainTimeTests, in src/test/UnitTests/ChainSyncReplay.hs
-- 1-second slots from the POSIX epoch, so the expected time IS the slot number.
testSlotConfig :: GYSlotConfig
testSlotConfig = simpleSlotConfig (posixSecondsToUTCTime 0) 1

chainTimeTests :: TestTree
chainTimeTests =
  testGroup
    "chain time on read (inherited fix #5)"
    [ testCase "slotToUTC is the slot's begin time, from the slot alone" $
        slotToUTC testSlotConfig 12345 @?= posixSecondsToUTCTime 12345,
      testCase "slotToUTC is strictly monotone in the slot" $
        compare (slotToUTC testSlotConfig 100) (slotToUTC testSlotConfig 101) @?= LT,
      testCase "utcToSlot . slotToUTC is the identity on slot boundaries" $
        utcToSlot testSlotConfig (slotToUTC testSlotConfig 777) @?= 777
    ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `grep -c "getCurrentTime" src/lib/offchain-lib/Palmares/Storage.hs && cabal test --test-show-details=direct`
Expected: FAIL — the `grep -c` prints **`7`** (the `Data.Time` import at Belt System L29 plus the six
`now <- liftIO getCurrentTime` lines in the `put*Projection` writers), and the Haskell cases fail to compile with
`Variable not in scope: slotToUTC`.

- [ ] **Step 3: Delete the wall clock and add the two converters**

In `Storage.hs`: drop `getCurrentTime` from the `Data.Time` import list and the `now <- liftIO getCurrentTime` line
plus the `insertedAt = now` field from each writer; the `insertedAt` **column stays** (ingest telemetry) and is filled
by the DB default — nothing reads it. In `Palmares/TxBuilding/Utils.hs`:

```haskell
-- | Chain time for a stored @created_at_slot@. Pure, so a handler maps a whole page with one slot config.
slotToUTC :: GYSlotConfig -> Integer -> UTCTime
slotToUTC cfg = posixSecondsToUTCTime . timeToPOSIX . slotToBeginTimePure cfg . unsafeSlotFromInteger

-- | A @since@ query param as a slot, so the comparison happens on the indexed column.
-- A time before the system start clamps to slot 0 — i.e. "no lower bound", which is what @since@ means.
utcToSlot :: GYSlotConfig -> UTCTime -> Integer
utcToSlot cfg =
  maybe 0 slotToInteger . enclosingSlotFromTimePure cfg . timeFromPOSIX . utcTimeToPOSIXSeconds
```

In `QueryAppMonad.hs`, one definition: `requestSlotConfig = asks providerContext >>= \ctx -> liftIO (runQuery ctx slotConfig)`.
The only `insertedAt` outside `Storage.hs`'s persist block is a stale comment, Belt System
`Query/Projected.hs:320` (`-- Filter by registration date (insertedAt >= cutoff)`); delete it if Task
T-EXTRACTION-3's strip left it standing, or Step 4's grep gate fails on a comment.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal build all && cabal test --test-show-details=direct && test "$(grep -c getCurrentTime src/lib/offchain-lib/Palmares/Storage.hs)" -eq 0 && ! grep -rqn "insertedAt" src/lib/offchain-lib/Palmares/DomainTypes/ src/exe/query-api/`
Expected: PASS — build exits 0, `All 50 tests passed`, the `grep -c` prints `0`, and the `insertedAt` grep prints
nothing (exit 1, so the `!` succeeds).

- [ ] **Step 5: Commit**

```bash
git add src/lib/offchain-lib/Palmares/Storage.hs src/lib/offchain-lib/Palmares/TxBuilding/Utils.hs src/exe/query-api/QueryAppMonad.hs src/test/UnitTests/ChainSyncReplay.hs
git commit -m "fix(storage): derive timestamps from chain slots, never from ingest time (inherited fix #5)

rollbackTo wipes and replays the projections, so a stored getCurrentTime re-stamped every row
and renumbered the activity feed on every re-sync. created_at_slot is the only time stored;
query-api converts once per request with the pure slot config."
```

---

### Block interfaces — produces

**Repo / build**
- `/Users/mg/Projects/CardanoProjects/palmares` with the `Palmares.*` namespace, package `palmares`, four libraries
  (`onchain-lib`, `chainsync-lib`, `offchain-lib`, `webapi-lib`) and four executables `admin`, `query-api` (8083),
  `interaction-api` (8082), `attest-sync` (8084).
- `extraction-keep.txt` (69 kept paths) — the drift guard between the manifest and the tree.
- Green `cabal build all` + `cabal test` from the strip commit onward (31 → 50 cases across this block).
- Rename footprint held to three commands (T-EXTRACT §5.2): one `sed` over `Palmares|palmares`, one `git mv` per
  `Palmares` namespace dir, one `git mv` of `palmares.cabal`; no identifier, filename, config key or URI path carries
  the project name (`PALMARES_` env prefix is the single exception).

**Modules later blocks consume**
- `Palmares.DomainTypes.Core.Ids`: `drepIdCip129 :: Bool -> ByteString -> Text`, `drepIdCip105 :: ByteString -> Text`,
  `ccHotIdCip129 :: Bool -> ByteString -> Text`, `ccColdIdCip129 :: Bool -> ByteString -> Text`,
  `govActionIdCip129 :: ByteString -> Word8 -> Text`, `parseDrepIdAny :: Text -> Either Text (Maybe Bool, ByteString)`.
- `Palmares.TxBuilding.Utils`: `authenticateStateUTxO :: GYMintingPolicyId -> GYAssetClass -> GYValue -> Bool`,
  `slotToUTC :: GYSlotConfig -> Integer -> UTCTime`, `utcToSlot :: GYSlotConfig -> UTCTime -> Integer`
  (+ `extractNFTAssetClass`, `getInlineDatumAndValue(OrThrow)`, `pPOSIXTimeFromGYSlot`, `gySlotFromPOSIXTime`,
  `txOutRefToV3Plutus`, `tnFromGYAssetClass`).
- `src/exe/query-api/QueryAppMonad.hs`: `requestSlotConfig :: QueryAppMonad GYSlotConfig`.
- `src/exe/query-api/Query/Common.hs`: `normalizeLimitOffset` (100, clamp [1,500]), `normalizeOrder`,
  `applyLimits`, `applyFilterOrderLimit` — unchanged by this block beyond the `beltIsMasterCapable` strip.
- `Palmares.Storage`: `OnchainMatchEvent`, `putKupoMatch`, `putMatchAndProjections`, `rollbackTo`, `replayOrder`,
  `runMigrations`, `readSchemaProbe`, `wipeChainSyncTablesRaw`, `chainSyncTableNames`, `upsertByUnique`,
  `PersistField Integer`/`GYTime`, `currentSchemaVersion = 1`; contract: `created_at_slot` is the only time in a row.
- `Palmares.WebAPI.{Auth,CORS,Errors,ServiceProbe,Utils}` as-is + `Palmares.WebAPI.CSV` slot (D-11, `cassava`).
- `Palmares.ChainSync.{KupoClient,KupoAtlas}` as-is (incl. `toGYAssetClass`'s separatorless fallback).
- `Palmares.TxBuilding.{Skeletons,Transactions,Context,Exceptions,Validators,Interactions}` (incl.
  `safeEraTime = 25920`, `txIsValidForSafeEra`, `txBuildingExceptionToHttpStatus`, `runActionWithPK`).
- Test harness: `Test.Helpers.{assert,queryOracle}`, `UnitTests.StorageRoundtrip` (in-memory SQLite),
  `UnitTests.ApiHardening`, `UnitTests.Conversions`, `UnitTests.ChainSyncReplay`, `UnitTests.GovIds`.
- `bech32` added to `offchain-lib`'s `build-depends`, `bech32` + `base16-bytestring` to the test-suite's
  (`any.bech32 ==1.1.8`, `any.base16-bytestring ==1.0.2.0`, both already in the freeze — no new pin).

**Handoffs**
- **Plan B**: fixes #1 (`feeFor`), #2 (`OracleSetAdmin` index 4), #4 (`checkTimestampInFiniteRange`) — all three touch
  the validators; the `projectChainEvent` wiring of `authenticateStateUTxO` with the two §4.3 negative cases; the
  §4.5 replay-stability case; `TestRuns.hs`, `Test/Fixtures.hs`, `UnitTests/{Attestation,Oracle,Cleanup,Dispute}.hs`,
  `PropertyTests.hs` (mirror-and-pin), and the attestation payload of the rewrite table in Task T-EXTRACTION-3.
- **Plan A API block**: `DomainTypes/Transfer/{Filters,OrderBy,QueryResponses}.hs`, `query-api/RestAPI.hs`,
  `Query/{Aggregates,Pages}.hs`, and the new `Query/{Gov,Views,Refresh,Metrics}.hs` + `Metrics/Catalog.hs`.

### Block open items

- **(OPEN: of the seven `UnitTests/GovIds.hs` id literals, only `drep1y2qn0rf…` is a value read off Koios
  (T-METRIC §4); the other six are the same 28 bytes re-encoded from T-DATA §1.3's header table with an independent BIP-173
  implementation. T-DATA's live `cc_hot1qddl5lv…` / `cc_cold1zds4k4q…` / `gov_action1qnergwfc…` observations are
  truncated **and** belong to credentials whose raw hex the notes never record, so they cannot become vectors. To get
  third-party confirmation of the committee forms, the **owner or the data block** must supply one real preprod
  `committee_hash.raw` together with Koios's `cc_hot_id`/`cc_cold_id` for it.)** — Task T-EXTRACTION-4.
- **(OPEN: blocker B2 — the script-DRep profile claim is still unspecified (T-EXTRACT §7.5, synthesis §3 step 24).
  Key-DReps prove control with one extra signatory; script-DReps cannot sign, so `MintClaimProfile` needs a different
  proof (spend a UTxO at the DRep script address? require a script witness?). It is on-chain design, it blocks F1
  check 11, and it belongs to **Plan B** — it must be settled in Plan B's task list, not discovered at implementation
  time. Nothing in Plan A depends on it.)**
- **(OPEN: T-EXTRACT §2.5 and §4.5 place fix #5's read-side conversion in `Query/Aggregates.hs`; measured, the
  activity feed's `since`/`ORDER BY` on `insertedAt` is in `Query/Projected.hs:1331` (`getActivityFeed`), and
  `Aggregates.hs`/`Pages.hs` carry no `insertedAt` at all. The converters therefore live in
  `Palmares.TxBuilding.Utils` (library, so the test-suite can import them) and the per-row conversion lands wherever
  a projection row is serialized — **Plan B's** `getActivityFeed`, since the attestation projections are its payload.
  Nothing in Plan A's governance read path consumes `slotToUTC`: db-sync's own `block.time` is already a timestamp.)**
- **(OPEN: `parseDrepIdAny` deviates from T-DATA §1.3's `Either Text (Bool, ByteString)` to
  `Either Text (Maybe Bool, ByteString)` because CIP-105 and raw-hex ids do not carry the script flag. The DTO layer
  must therefore resolve `has_script` from `drep_hash` by `raw` for those two forms; note that `drep_hash` is
  `UNIQUE(raw, has_script)`, so a raw-only lookup can in principle match two rows. The deviation itself is **decided
  here**; what is open is the tie-break for that two-row case, and the **Plan A API block** owns it when it writes
  T-API §1.1's `{drep-id}` capture.)**

## Block 3 — The governance data substrate

# Plan A — block: the governance data substrate

This block delivers the Postgres that everything else in Palmares reads: a measured off-cluster
from-genesis preprod `cardano-db-sync` run (synthesis §5, Experiment A), the cluster-side provisioning
(node IPC on hostPath, Postgres 17 + 50 Gi PVC, the db-sync Deployment), the real from-genesis sync that
**is** F1 check 1, `sql/040_indexes.sql` through the house gate-Job pattern, the sizing measurement that
writes ADR `0001-governance-data-substrate.md`, and the three Postgres roles with their grants and
`search_path`. It implements synthesis §3 Phase 2 steps 8–13 plus Phase 3 step 14, and the decisions
D-4 (`shelley: enable: true`), D-5 (the four-key `insert_options`), D-6 (separate Deployment on a
hostPath socket), D-7 (the two lag thresholds), D-13 (50 Gi, measured, with ordered fallbacks),
D-15 (two DDL mechanisms split by transactionality) and D-26 (manifests only in `apps-k8s-configs`).
Assumptions: the preprod `cardano-node` 11.1.2 on `minirack-pi5-1` stays as it is (D-12 — no new node,
no Mithril on the critical path); the owner has approved the five Phase-0 decisions and the one node
pod restart; `/Users/mg/Projects/CardanoProjects/palmares` has been created by Plan A's bootstrap task
before T-SUBSTRATE-2 (every cluster task here commits its check script to `palmares/scripts/`;
T-SUBSTRATE-1 needs only the `experiments/` directory, and T-SUBSTRATE-6 needs the `admin`
executable); and the GitOps repo is at
`/Users/mg/Projects/apps-k8s-configs` (**not** under `CardanoProjects/` — the design notes' path is
wrong, the one here is verified on disk).

Nothing in this block makes a performance claim before T-SUBSTRATE-6, and nothing in it reads
`palmares.*` — those views are the read-path block's.

---

### Task T-SUBSTRATE-1: Experiment A — off-cluster from-genesis preprod db-sync, measured

The riskiest unknown in F1 (synthesis §5). Runs on the owner's Apple-silicon Mac (Docker 29.8.2,
aarch64, 4 CPU, 16.5 GB RAM, 161 GB free — verified 2026-10-19), **off-cluster, zero cluster churn**, in
parallel with the whole extraction track. Its deliverable is a measurement table, and that table decides
the PVC size, the db-sync memory limit and whether the substrate decision reopens.

**Files:**
- Create: `/Users/mg/Projects/CardanoProjects/palmares/experiments/dbsync-genesis/compose.yaml`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/experiments/dbsync-genesis/db-sync-config.yaml`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/experiments/dbsync-genesis/node-configs/config.json`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/experiments/dbsync-genesis/node-configs/{byron,shelley,alonzo,conway}-genesis.json`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/experiments/dbsync-genesis/measure.sh`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/experiments/dbsync-genesis/rss-sample.sh`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/experiments/dbsync-genesis/RESULTS.md`
- Test: `measure.sh` is the test; `RESULTS.md` is its recorded output

**Interfaces:**
- Consumes: nothing.
- Produces: `experiments/dbsync-genesis/RESULTS.md` carrying `WALLCLOCK_TO_TIP_HOURS`,
  `PG_TOTAL_BYTES`, `PEAK_RSS_BYTES`, `EPOCH_SYNC_TIME` table, and the five `count(*)` probes.
  Also: the verdict on whether db-sync 13.7.1.0 parses the cluster's `LedgerDB: V2LSM` node config.
  The `chain`-profile services here become the root `compose.yaml`'s `chain` profile verbatim in the
  Phase-1 bootstrap task.

- [ ] **Step 1: Write the failing test**

`measure.sh` — the whole measurement, idempotent, runnable mid-sync and at the end. It exits non-zero
until every probe is satisfiable.

```bash
#!/usr/bin/env bash
# Experiment A measurement. Run any time; it reports partial progress and exits 1 until done.
# Usage: ./measure.sh [>> RESULTS.md]
set -euo pipefail
PSQL=(docker compose exec -T postgres psql -U dbsync -d cexplorer -At -F$'\t')

echo "## measured $(date -u +%FT%TZ)"

# --- 1. progress and wall-clock to tip -------------------------------------------------------
"${PSQL[@]}" -c "SELECT 'tip_epoch', max(epoch_no) FROM block;"
"${PSQL[@]}" -c "SELECT 'lag_seconds',
  EXTRACT(EPOCH FROM (now() AT TIME ZONE 'UTC'
    - (SELECT time FROM block ORDER BY id DESC LIMIT 1)))::bigint;"

# --- 2. per-epoch timings: db-sync records them itself, no log parsing -----------------------
echo "### epoch_sync_time"
"${PSQL[@]}" -c "SELECT no, seconds, state FROM epoch_sync_time ORDER BY no;"

# --- 3. THE PVC ANSWER: hardware-independent -------------------------------------------------
echo "### pg_total_relation_size top 20"
"${PSQL[@]}" -c "SELECT relname, pg_total_relation_size(c.oid)
  FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace
  WHERE n.nspname = 'public' AND c.relkind = 'r'
  ORDER BY pg_total_relation_size(c.oid) DESC LIMIT 20;"
"${PSQL[@]}" -c "SELECT 'PG_TOTAL_BYTES', pg_database_size('cexplorer');"

# --- 4. peak RSS: written by rss-sample.sh, which runs for the whole experiment ---------------
# rss.csv column 2 is RAW BYTES, so this sort is numeric. (docker stats' MemUsage is a human
# string — "3.5GiB" sorts below "900MiB" under `sort -n` — which is why we do not sample it.)
echo "### PEAK_RSS_BYTES"
sort -t, -k2 -n rss.csv | tail -1

# --- 5. the five count(*) probes -------------------------------------------------------------
echo "### probes"
"${PSQL[@]}" -c "SELECT 'stake_deregistration', count(*) FROM stake_deregistration;"
"${PSQL[@]}" -c "SELECT 'pool_update',          count(*) FROM pool_update;"
"${PSQL[@]}" -c "SELECT 'drep_distr',           count(*) FROM drep_distr;"
"${PSQL[@]}" -c "SELECT 'off_chain_vote_data',  count(*) FROM off_chain_vote_data;"
"${PSQL[@]}" -c "SELECT 'off_chain_vote_fetch_error', count(*) FROM off_chain_vote_fetch_error;"
# drep_distr epoch coverage: no gap, and current. Preprod's first Conway epoch is 163.
"${PSQL[@]}" -c "SELECT 'drep_distr_epochs',
  min(epoch_no), max(epoch_no), count(DISTINCT epoch_no),
  (max(epoch_no) - min(epoch_no) + 1) = count(DISTINCT epoch_no) AS no_gap
  FROM drep_distr;"

# --- 6. the gate -----------------------------------------------------------------------------
fail=0
chk() { # name, sql returning boolean
  local ok; ok=$("${PSQL[@]}" -c "SELECT $2;")
  [ "$ok" = t ] || { echo "FAIL: $1"; fail=1; }
}
chk "shelley flip took (stake_deregistration non-empty)" "(SELECT count(*) FROM stake_deregistration) > 0"
chk "shelley flip took (pool_update non-empty)"          "(SELECT count(*) FROM pool_update) > 0"
chk "drep_distr current"      "(SELECT max(epoch_no) FROM drep_distr) >= (SELECT max(epoch_no) FROM block) - 1"
chk "drep_distr has no gaps"  "(SELECT (max(epoch_no)-min(epoch_no)+1) = count(DISTINCT epoch_no) FROM drep_distr)"
chk "anchors were fetched"    "(SELECT count(*) FROM off_chain_vote_data) > 0"
chk "at tip (<300s)"          "EXTRACT(EPOCH FROM (now() AT TIME ZONE 'UTC' - (SELECT time FROM block ORDER BY id DESC LIMIT 1))) < 300"
exit $fail
```

`rss-sample.sh` — peak RSS in **bytes**, sampled from the container's own cgroup, addressed by compose
service name so it does not depend on the compose project name:

```bash
#!/usr/bin/env bash
# Append db-sync's memory use (RAW BYTES) every 60s for the life of the experiment. Run with nohup.
set -uo pipefail
while true; do
  bytes=$(docker compose exec -T cardano-db-sync sh -c \
    'cat /sys/fs/cgroup/memory.current 2>/dev/null || cat /sys/fs/cgroup/memory/memory.usage_in_bytes' \
    2>/dev/null | tr -d '\r') || bytes=
  printf '%s,%s\n' "$(date -u +%FT%TZ)" "${bytes:-0}" >> rss.csv
  sleep 60
done
```

- [ ] **Step 2: Run test to verify it fails**

Run:
```bash
cd /Users/mg/Projects/CardanoProjects/palmares/experiments/dbsync-genesis && ./measure.sh
```
Expected: FAIL — `psql: error: connection to server ... failed` (no stack is up yet).

- [ ] **Step 3: Assemble the experiment's configs, pre-check the host, and launch**

`node-configs/` is the **cluster's own** `/configs` directory, reassembled exactly as the preprod
StatefulSet's `fetch-configs` initContainer builds it
(`statefulset-cardano-preprod.yaml:30-71`), so the experiment also answers T-INFRA §8's open question.
Both halves are required: `config.json` references the four genesis files **by bare filename**
(T-INFRA §1.3), so db-sync cannot start if they are not in the same directory — and `config.json`
carries their hashes, so a mismatched download is fail-closed, not silent.

```bash
mkdir -p node-configs
yq '.data."config.json"' -r \
  /Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod/config/configmap-cardano-preprod-node-config.yaml \
  > node-configs/config.json   # carries "LedgerDB": {"Backend": "V2LSM", "LSMDatabasePath": "/data/lsm", ...}
BASE="https://book.play.dev.cardano.org/environments/preprod"
for f in byron-genesis.json shelley-genesis.json alonzo-genesis.json conway-genesis.json; do
  curl -fsSL "$BASE/$f" -o "node-configs/$f"
done
```

`db-sync-config.yaml`: download db-sync's shipped `config/preprod-config.yaml` **at tag 13.7.1.0**
verbatim (it carries the logging `Representation` block that db-sync refuses to start without), then make
exactly three edits — `NodeConfigFile: /node-configs/config.json`, add the top-level `ipfs_gateway` list,
and replace `insert_options` with the four keys of D-5:

```yaml
ipfs_gateway:
  - "https://ipfs.io/ipfs"
  - "https://cloudflare-ipfs.com/ipfs"

insert_options:
  preset: "only_governance"
  offchain_vote_data: "enable"
  shelley:
    enable: true
```

No `ledger_backend` (default `inmemory`; `lsm` is D-13 fallback 2 only). No `disable_epoch` (the key does
not exist at 13.7.1.0). No `RequiresNetworkMagic` (not a db-sync key). No `remove_jsonb_from_schema`
(already the default). No restated preset keys.

`compose.yaml`: T-INFRA §4.1's `chain` profile verbatim, including `name: palmares`, with
`cardano-node` pinned to the cluster's exact digest
`ghcr.io/blinklabs-io/cardano-node:11.1.2@sha256:3620624c4cb9ac4e750b6c5c2d674c478010f4a193fdf7418f4930820562c11d`
(`MITHRIL_DOWNLOAD: "true"`, aggregator
`https://aggregator.release-preprod.api.mithril.network/aggregator`, genesis verification key from
T-INFRA §3) + `postgres:17-alpine` + `cardano-db-sync` pinned to
`ghcr.io/blinklabs-io/cardano-db-sync:13.7.1.0@sha256:2a341eb32f15a3e65435f4123c3186f467bf2ed9d3ea2eac449592b5a54eb950`,
with `./node-configs:/node-configs:ro` replacing T-INFRA's `node-db:/node-db:ro` (we test the
cluster's config, not the image's). Four fields where a wrong value silently voids the run:

| Field | Value | Why |
|---|---|---|
| `environment.NETWORK` | `""` — present **and** empty | the entrypoint runs `set -u` and reads `${NETWORK}` with no default; unset is an unbound-variable abort, empty selects the "use my `--config`" branch |
| `working_dir` | `/var/lib/cexplorer` | the image sets no `WORKDIR`, so with CWD `/` the `.restored` marker and any dump land on the container layer |
| `POSTGRES_USER` | `dbsync` | the bootstrap role is db-sync's own role (synthesis §1.1); it is also the superuser, so a future `RESTORE_SNAPSHOT` still passes `check_psql_superuser` |
| `command` | `["--config","/dbsync-config/db-sync-config.yaml","--socket-path","/node-ipc/node.socket"]` | args are appended verbatim to the entrypoint's `exec` |

Pre-check and launch:
```bash
docker info --format '{{.MemTotal}}'     # must be >= 12884901888 (12 GiB); raise Docker Desktop if not
df -g .                                  # must show >= 120 GB free
docker compose up -d
nohup ./rss-sample.sh >/dev/null 2>&1 &
date -u +%FT%TZ > started_at
docker compose logs -f cardano-db-sync   # the V2LSM parse verdict appears in the first 30 seconds
```

- [ ] **Step 4: Run test to verify it passes**

Run, at t+1 h (sanity), then daily, then once the log reports the tip:
```bash
cd /Users/mg/Projects/CardanoProjects/palmares/experiments/dbsync-genesis && ./measure.sh >> RESULTS.md
```
Expected: PASS — exit 0, all six `chk` lines silent.

Pass criteria, and what each measurement decides:

| Measurement | Pass | Decides |
|---|---|---|
| wall-clock to tip (`started_at` → first passing run) | ≤ 96 h | the duration band T-SUBSTRATE-5 quotes. Expected 24–48 h, floor ~8 h. > 96 h on this host ⇒ D-13 fallback 3 (db-sync stays off-cluster) |
| `PG_TOTAL_BYTES` | ≤ 25 GB | **the PVC size.** ≤ 25 GB ⇒ 50 Gi stands. 25–40 GB ⇒ 50 Gi stands, schedule D-13 fallback 1 (`DELETE FROM epoch_stake WHERE epoch_no < tip-2`, only after the `validateEpochStake` test exists). > 40 GB ⇒ edit the PVC manifest **before** T-SUBSTRATE-5; `local-path` has `allowVolumeExpansion: false` |
| `PEAK_RSS_BYTES` | ≤ 6 GiB | the db-sync Deployment's `limits.memory`. 6–9 GiB ⇒ raise the limit and re-check pi5-1's 9.51 Gi real headroom. > 9 GiB ⇒ D-13 fallback 2 (`ledger_backend: lsm`, accepting the Raspberry-Pi thread crash and the >4096-item `blockio-uring` bug), then fallback 3 |
| `epoch_sync_time` per epoch | recorded | where the time goes; the per-epoch curve is what bounds the Pi by the single-core ratio (~3–5×) instead of a 2× guess |
| `stake_deregistration`, `pool_update` counts | both > 0 | **the `shelley: enable: true` flip took.** Zero on either ⇒ the config is wrong and the run is void; a flip after a cluster sync costs a full re-sync. `param_proposal` is **not** a valid probe — it fills regardless of this flag |
| `drep_distr`: `no_gap` true, `max(epoch_no) >= tip-1` | both | F1 check 1's `drep_distr` assertion, and that `ledger: enable` survived the preset |
| `off_chain_vote_data` / `off_chain_vote_fetch_error` counts | recorded | the anchor-coverage figure T-API §8.5 defers; a `not_fetched` share > ~20 % at 48 h after completion is the trigger for the deferred supplementary re-fetcher (measure in F1, build in F2) |
| db-sync's first 30 s of log against `node-configs/config.json` | no `LedgerDB` parse error | T-INFRA §8's open question. On failure: vendor a db-sync-only `config.json` with the `LedgerDB` block removed, keeping the genesis hashes, and carry it into T-SUBSTRATE-4's ConfigMap |

- [ ] **Step 5: Commit**

```bash
git add experiments/dbsync-genesis
git commit -m "chore(experiment): measured from-genesis preprod db-sync run (Experiment A)"
```

---

### Task T-SUBSTRATE-2: publish the preprod node's IPC socket and config dir on hostPath

⚠️ **This task edits a live cluster workload and restarts the preprod `cardano-node` pod, taking
`ogmios` and the `hydra-node` sidecar with it. It needs the owner's go-ahead and nothing else in this
block depends on being done first — stop here if the approval is not in hand.** Expect
seconds-to-minutes plus a `V2LSM` snapshot reload; the "~26 s" in `apps-k8s-configs/CLAUDE.md` is a
post-conversion catch-up, not a restart measurement.

**Files:**
- Modify: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod/workloads/statefulset-cardano-preprod.yaml:26,27,236-240`
- Test: `/Users/mg/Projects/CardanoProjects/palmares/scripts/check_node_hostpath.sh`

**Interfaces:**
- Consumes: the `palmares` repo skeleton from Plan A's Phase-1 bootstrap task (for `scripts/`).
- Produces: `scripts/check_node_hostpath.sh`; hostPaths `/var/lib/cardano-preprod/ipc` (carrying `node.socket`) and
  `/var/lib/cardano-preprod/configs` (carrying `config.json`, `topology.json`, `peer-snapshot.json`
  and the four genesis files, assembled by the existing `fetch-configs` initContainer) on
  `minirack-pi5-1`.

- [ ] **Step 1: Write the failing test**

```bash
#!/usr/bin/env bash
# scripts/check_node_hostpath.sh — run from any host with the minirack kubeconfig.
set -euo pipefail
kubectl --context minirack-lan -n cardano-preprod debug node/minirack-pi5-1 \
  -it --image=busybox:1.36 -- ls -la /host/var/lib/cardano-preprod/ipc/node.socket \
                                      /host/var/lib/cardano-preprod/configs/config.json \
                                      /host/var/lib/cardano-preprod/configs/conway-genesis.json
```

- [ ] **Step 2: Run test to verify it fails**

Run: `./scripts/check_node_hostpath.sh`
Expected: FAIL with `ls: /host/var/lib/cardano-preprod/ipc/node.socket: No such file or directory`
(today both volumes are `emptyDir: {}`).

- [ ] **Step 3: Convert the two volumes to hostPath and bump the stamp**

At lines 236–240, replace `emptyDir: {}` on `configs` and `ipc` with
`hostPath: {path: /var/lib/cardano-preprod/{configs,ipc}, type: DirectoryOrCreate}`; add
`nodeSelector: {kubernetes.io/hostname: minirack-pi5-1}` at the pod `spec:` (line 27) so the socket path
can never be orphaned; and bump `cardano.io/node-config-stamp` (line 26) from `"2026-09-20-1"` in the
**same commit** — the repo rule. Both volumes, not one: db-sync needs the socket **and** `config.json`
plus the four genesis files, which `config.json` references by bare filename.

Do **not** touch the stale comment at `statefulset-cardano-preprod.yaml:80` ("Rămâne V2InMemory —
trecerea la V2LSM e un task separat, ulterior.") — the committed ConfigMap really is `V2LSM`
(`configmap-cardano-preprod-node-config.yaml:73-74`) and fixing that comment is not this commit.

- [ ] **Step 4: Run test to verify it passes**

```bash
cd /Users/mg/Projects/apps-k8s-configs
kustomize build --load-restrictor LoadRestrictionsNone ./chains/cardano/preprod | kubeconform -ignore-missing-schemas -summary -
git push          # Flux root cardano-preprod, interval 10m0s
flux --context minirack-lan reconcile kustomization cardano-preprod --with-source
kubectl --context minirack-lan -n cardano-preprod rollout status statefulset/statefulset-cardano-preprod --timeout=10m
/Users/mg/Projects/CardanoProjects/palmares/scripts/check_node_hostpath.sh
```
Expected: PASS — `rollout status` reports `statefulset rolling update complete 1 pods at revision …`
and all three paths list. Then confirm the dependants survived:
`kubectl -n cardano-preprod get pods -l app=kupo-preprod` Ready, and ogmios answering on its Service.

- [ ] **Step 5: Commit**

```bash
# in apps-k8s-configs
git add chains/cardano/preprod/workloads/statefulset-cardano-preprod.yaml
git commit -m "feat(cardano-preprod): publish node ipc + configs on hostPath for db-sync"
# in palmares
git add scripts/check_node_hostpath.sh
git commit -m "feat(ops): assert the preprod node's ipc + configs hostPaths"
```

---

### Task T-SUBSTRATE-3: Postgres 17 + 50 Gi PVC, pinned to minirack-pi5-1

⚠️ **This task creates new workloads on the live cluster** (a new Flux root in namespace
`cardano-preprod`). It restarts nothing that exists.

**Files:**
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/kustomization.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/config/configmap-palmares-dbsync-env.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/secrets/secret-palmares-dbsync.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/storage/pvc-palmares-dbsync-pg.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/storage/pvc-palmares-dbsync-state.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/services/service-palmares-postgres.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/workloads/deployment-palmares-postgres.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/policies/networkpolicy-palmares-postgres.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/clusters/minirack/chains/cardano-preprod-dbsync.yaml`
- Modify: `/Users/mg/Projects/apps-k8s-configs/clusters/minirack/chains/kustomization.yaml`
- Test: `/Users/mg/Projects/CardanoProjects/palmares/scripts/check_postgres.sh`

**Interfaces:**
- Consumes: T-SUBSTRATE-2's hostPaths (not yet, but the root is shaped for them).
- Produces: `scripts/check_postgres.sh`; `service-palmares-postgres.cardano-preprod.svc.cluster.local:5432`, database `cexplorer`,
  bootstrap superuser role `dbsync`; ConfigMap `configmap-palmares-dbsync-env` with
  `POSTGRES_HOST`/`POSTGRES_PORT`/`POSTGRES_DB`/`POSTGRES_USER`/`PGDATA`; Secret
  `secret-palmares-dbsync` with `POSTGRES_PASSWORD`; PVCs `pvc-palmares-dbsync-pg` (50 Gi) and
  `pvc-palmares-dbsync-state` (10 Gi); Flux Kustomization `cardano-preprod-dbsync`.

- [ ] **Step 1: Write the failing test**

```bash
#!/usr/bin/env bash
# scripts/check_postgres.sh
set -euo pipefail
K="kubectl --context minirack-lan -n cardano-preprod"
$K get pvc pvc-palmares-dbsync-pg -o jsonpath='{.spec.resources.requests.storage}' | grep -qx 50Gi
$K get pod -l app=palmares-postgres-pod-label -o jsonpath='{.items[0].spec.nodeSelector}' \
  | grep -q minirack-pi5-1
$K exec deploy/deployment-palmares-postgres -- \
  psql -U dbsync -d cexplorer -At -c "SELECT current_user, rolsuper FROM pg_roles WHERE rolname=current_user;" \
  | grep -qx 'dbsync|t'
```

- [ ] **Step 2: Run test to verify it fails**

Run: `./scripts/check_postgres.sh`
Expected: FAIL with `Error from server (NotFound): persistentvolumeclaims "pvc-palmares-dbsync-pg" not found`.

- [ ] **Step 3: Write the manifests**

Copy T-INFRA §2.3's `configmap-palmares-dbsync-env` and §2.4 / §2.5 / §2.9 verbatim, with these
corrections applied:

- **`POSTGRES_USER: "dbsync"`**, not `postgres` (synthesis §1.1 — db-sync's own role owns `public`; the
  bare-`postgres` form is an alias to delete). Consequences, all in this task: both `pg_isready` probes
  become `["pg_isready","-U","dbsync","-d","cexplorer"]`; the image makes `dbsync` the bootstrap
  **superuser** so `postgresql-setup.sh`'s `check_psql_superuser` still passes for a future
  `RESTORE_SNAPSHOT`; and `ALTER DEFAULT PRIVILEGES FOR ROLE dbsync` in T-SUBSTRATE-8 names a real role.
- The ConfigMap's own comment says "our projections live in schema `registry`" — write **`palmares`**
  (synthesis §1.1; `registry` is an alias to delete).
- `nodeSelector: {kubernetes.io/hostname: minirack-pi5-1}` on the Postgres pod — same node as db-sync,
  which is the hottest link in the stack and closes T-DATA's cross-node write-path question.
- Both PVCs carry `kustomize.toolkit.fluxcd.io/prune: disabled`: `local-path` is
  `reclaimPolicy: Delete`, so deleting the file from Git would destroy a multi-day sync.
- **50 Gi, sized once.** `local-path` has `allowVolumeExpansion: false`. Estimate 10–18 GB with D-4's
  flip; T-SUBSTRATE-1 either confirms it or forces an edit here before T-SUBSTRATE-5.
- Postgres args as T-INFRA §2.4 (`shared_buffers=768MB`, `work_mem=32MB`,
  `maintenance_work_mem=512MB`, `max_parallel_maintenance_workers=2`, `max_wal_size=4GB`,
  `checkpoint_timeout=30min`, `wal_compression=on`, `synchronous_commit=off` with its `ponytail:`
  comment naming the ceiling).
- `secret-palmares-dbsync` is SOPS-encrypted. `.sops.yaml`'s second rule already matches
  `^chains/.*/secrets/.*\.ya?ml$` — **no `.sops.yaml` change**.
- The NetworkPolicy's `namespaceSelector` names `kubernetes.io/metadata.name: palmares`; that namespace
  does not exist yet and the selector correctly matches nothing until the app block creates it.

Flux root `clusters/minirack/chains/cardano-preprod-dbsync.yaml` — a sibling of `cardano-preprod.yaml`
(D-26), `path: ./chains/cardano/preprod-dbsync`, `prune: true`, SOPS decryption,
`dependsOn: [infrastructure, cardano-preprod]`, and **`wait: true` with `timeout: 1m0s`** — a short
timeout on purpose: readiness is reported honestly but a Flux worker is never held for the length of a
multi-day sync (the 2026-09-23 audit lesson: four default workers, all blocked). Add the file to
`clusters/minirack/chains/kustomization.yaml`.

- [ ] **Step 4: Run test to verify it passes**

```bash
cd /Users/mg/Projects/apps-k8s-configs
kustomize build --load-restrictor LoadRestrictionsNone ./chains/cardano/preprod-dbsync | kubeconform -ignore-missing-schemas -summary -
grep -rlE '^kind: Secret' chains/cardano/preprod-dbsync | xargs -r grep -L 'ENC\[AES256_GCM'   # must print nothing
git push && flux --context minirack-lan reconcile kustomization cardano-preprod-dbsync --with-source
/Users/mg/Projects/CardanoProjects/palmares/scripts/check_postgres.sh
```
Expected: PASS — `kubeconform` summary with 0 invalid, the plaintext-Secret grep silent, and all three
assertions in `check_postgres.sh` succeeding.

- [ ] **Step 5: Commit**

```bash
# in apps-k8s-configs
git add chains/cardano/preprod-dbsync clusters/minirack/chains/cardano-preprod-dbsync.yaml clusters/minirack/chains/kustomization.yaml
git commit -m "feat(palmares): Postgres 17 + 50Gi PVC for the preprod db-sync substrate"
# in palmares
git add scripts/check_postgres.sh
git commit -m "feat(ops): assert the substrate Postgres PVC, placement and dbsync role"
```

---

### Task T-SUBSTRATE-4: the db-sync Deployment

⚠️ **This task creates a new workload on the live cluster.** It restarts nothing that exists; it starts
the from-genesis sync, which T-SUBSTRATE-5 then measures. The config is frozen by that sync: a `shelley`
or `preset` correction after this point costs a full re-sync.

**Files:**
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/config/configmap-palmares-dbsync-config.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/workloads/deployment-palmares-dbsync.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/services/service-palmares-dbsync-metrics.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/monitoring/servicemonitor-palmares-dbsync.yaml`
- Modify: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/kustomization.yaml`
- Test: `/Users/mg/Projects/CardanoProjects/palmares/scripts/check_dbsync.sh`

**Interfaces:**
- Consumes: T-SUBSTRATE-2's hostPaths; T-SUBSTRATE-3's `configmap-palmares-dbsync-env`,
  `secret-palmares-dbsync`, `pvc-palmares-dbsync-state`, `service-palmares-postgres`;
  T-SUBSTRATE-1's `db-sync-config.yaml` (the same source file).
- Produces: `scripts/check_dbsync.sh`; `deployment-palmares-dbsync` writing `public.*` in `cexplorer`;
  `service-palmares-dbsync-metrics:8080` scraped on `path: /`; the six
  `cardano_db_sync_*` gauges.

- [ ] **Step 1: Write the failing test**

```bash
#!/usr/bin/env bash
# scripts/check_dbsync.sh
set -euo pipefail
K="kubectl --context minirack-lan -n cardano-preprod"
# 1. the four-key insert_options reached the pod, unexpanded and unaugmented
$K exec deploy/deployment-palmares-dbsync -- cat /dbsync-config/db-sync-config.yaml \
  | yq -o=json '.insert_options' | jq -e '
      . == {"preset":"only_governance","offchain_vote_data":"enable","shelley":{"enable":true}}'
# 2. NETWORK present and EMPTY (unset is an unbound-variable abort under set -u)
$K get deploy deployment-palmares-dbsync \
  -o jsonpath='{.spec.template.spec.containers[0].env[?(@.name=="NETWORK")].value}{"\n"}' | grep -qx ''
# 3. workingDir on the PVC, not the container layer
$K get deploy deployment-palmares-dbsync -o jsonpath='{.spec.template.spec.containers[0].workingDir}' \
  | grep -qx /var/lib/cexplorer
# 4. metrics answer on "/" — the probe path that cascades if wrong. If the image ships neither
#    wget nor curl, run the same GET from a throwaway pod:
#    $K run m --rm -i --image=busybox:1.36 --restart=Never -- \
#      wget -qO- http://service-palmares-dbsync-metrics:8080/
$K exec deploy/deployment-palmares-dbsync -- \
  sh -c 'wget -qO- http://127.0.0.1:8080/ | head -1' | grep -q cardano_db_sync_
# 5. the ServiceMonitor and the Service that Prometheus scrapes agree on that path and have an endpoint
$K get servicemonitor servicemonitor-palmares-dbsync -o jsonpath='{.spec.endpoints[0].path}' | grep -qx /
$K get endpoints service-palmares-dbsync-metrics -o jsonpath='{.subsets[0].addresses[0].ip}' | grep -qE '^[0-9]'
# 6. it is actually inserting
$K exec deploy/deployment-palmares-postgres -- psql -U dbsync -d cexplorer -At \
  -c "SELECT count(*) > 0 FROM block;" | grep -qx t
```

- [ ] **Step 2: Run test to verify it fails**

Run: `./scripts/check_dbsync.sh`
Expected: FAIL with `Error from server (NotFound): deployments.apps "deployment-palmares-dbsync" not found`.

- [ ] **Step 3: Write the ConfigMap and the Deployment**

`configmap-palmares-dbsync-config.yaml` carries one key, `db-sync-config.yaml`, whose content is **the
same file T-SUBSTRATE-1 built** — db-sync's shipped `config/preprod-config.yaml` at tag **13.7.1.0**
verbatim, with `NodeConfigFile: /node-configs/config.json`, the top-level two-entry `ipfs_gateway`, and
the four-key `insert_options` of D-5. Keep one source file and generate the compose and ConfigMap
variants from it with a one-line `sed` on `NodeConfigFile` — never two hand-maintained copies. Bump
`palmares.io/dbsync-config-stamp` on the Deployment in the **same commit** as any edit to this
ConfigMap; it does not restart the pod on its own.

Deployment: T-INFRA §2.3 verbatim. The five fields where a wrong value silently breaks the sync, spelled
out because none of them fails loudly:

| Field | Value | What goes wrong otherwise |
|---|---|---|
| `env.NETWORK` | present, `value: ""` | the entrypoint runs `set -euo pipefail` then `[[ -z ${NETWORK} ]]`; **unset** is an unbound-variable abort |
| `workingDir` | `/var/lib/cexplorer` | the image sets no `WORKDIR`; with CWD `/` a `RESTORE_SNAPSHOT` download and its `.restored` marker land on the container's writable layer — re-downloaded on every restart, charged to ephemeral storage |
| `insert_options` | exactly the four keys | no `preset` ⇒ base is `def` ⇒ `tx_metadata` **and** `reward` come back on (and `sioRewards` is `pure (sioRewards baseOptions)`, i.e. unsettable from JSON, so only a preset can disable rewards). A restated preset key that later drifts is itself a silent misconfiguration |
| `insert_options.shelley.enable` | `true` | without it `stake_deregistration`/`delegation`/`pool_update` are empty, F1 check 12's churn and retention become an upper bound, and flipping it later is a **full re-sync** |
| probe + ServiceMonitor `path` | `/`, not `/metrics` | db-sync calls `serveMetricsT port []`, so only `/` answers 200. Wrong ⇒ the pod is never Ready ⇒ the `cardano-preprod-dbsync` Flux root (`wait: true`) stays `Ready=False` ⇒ the app root that `dependsOn` it never reconciles at all |

Also: **no startup and no liveness probe** — the first sync runs for days and nothing may kill it;
readiness only, `initialDelaySeconds: 10` (not 60) to fit the root's `timeout: 1m0s`.
`strategy: Recreate` (one writer, ever). `terminationGracePeriodSeconds: 120`. `/configuration` is an
`emptyDir` because `gen-pgpass` writes `pgpass` there. `RESTORE_SNAPSHOT` stays **unset** for the first
sync. `ledger_backend` stays absent. Requests `750m`/`4Gi`, limits `2`/`6Gi` — revised by
T-SUBSTRATE-1's `PEAK_RSS_BYTES` if that lands above 6 GiB.

If T-SUBSTRATE-1 reported a `LedgerDB` parse failure, add the vendored db-sync-only `config.json` (that
block removed, genesis hashes kept) to this ConfigMap and repoint `NodeConfigFile` at it; the genesis
files still come from the shared hostPath, so fail-closed hash verification is preserved.

The ServiceMonitor needs no extra label — `kube-prometheus-stack` runs `serviceMonitorSelector: {}` on
this cluster.

- [ ] **Step 4: Run test to verify it passes**

```bash
cd /Users/mg/Projects/apps-k8s-configs
kustomize build --load-restrictor LoadRestrictionsNone ./chains/cardano/preprod-dbsync | kubeconform -ignore-missing-schemas -summary -
git push && flux --context minirack-lan reconcile kustomization cardano-preprod-dbsync --with-source
kubectl --context minirack-lan -n cardano-preprod rollout status deploy/deployment-palmares-dbsync --timeout=5m
/Users/mg/Projects/CardanoProjects/palmares/scripts/check_dbsync.sh
```
Expected: PASS — all six assertions, and
`kubectl -n cardano-preprod logs deploy/deployment-palmares-dbsync | head -40` shows no `LedgerDB`
parse error and an `Starting epoch` / `Insert Byron Block` progression.

- [ ] **Step 5: Commit**

```bash
# in apps-k8s-configs
git add chains/cardano/preprod-dbsync
git commit -m "feat(palmares): cardano-db-sync 13.7.1.0 preprod Deployment (only_governance + shelley)"
# in palmares
git add scripts/check_dbsync.sh
git commit -m "feat(ops): assert db-sync's five silent-failure fields and its metrics path"
```

---

### Task T-SUBSTRATE-5: the from-genesis preprod sync — F1 check 1

⚠️ **This task observes the live cluster for days.** It changes nothing until its last step, which adds
one PrometheusRule. Its deliverable is the signed F1 check 1.

> **ASSEMBLY CORRECTION 2** (2026-10-19, `f1-yaci-store-evaluation.md` §4.1 C2). "From genesis" is a
> **correctness** requirement, not merely a completeness one, and the task must say so: db-sync issue
> #1975 (open since 2025-05) records two *identical* mainnet instances disagreeing on one DRep's
> epoch-538 voting power by ~740×, and the 13.6.0.4 Known Issue names **preprod** explicitly —
> pre-upgrade `drep_distribution` values "are not well defined and could be slightly different between
> different executions". The documented remedy is deleting the ledger-state files and letting the ledger
> rules recompute, which is exactly what this sync does. **Consequence: a Postgres snapshot restore or an
> in-place db-sync upgrade is NOT an acceptable shortcut for this task** — it would inherit ill-defined
> values in the one column the whole product rests on. Mithril remains fine: it bootstraps the *node*,
> not db-sync's ledger state. Record this rationale in `docs/deployment/preprod-dbsync-sync.md` so a
> future operator does not "optimize" the sync away.

**Files:**
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/monitoring/prometheusrule-palmares-dbsync.yaml`
- Modify: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/kustomization.yaml`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/docs/deployment/preprod-dbsync-sync.md`
- Test: `/Users/mg/Projects/CardanoProjects/palmares/scripts/check_dbsync_tip.sh`

**Interfaces:**
- Consumes: T-SUBSTRATE-4's running `deployment-palmares-dbsync` and its
  `servicemonitor-palmares-dbsync`; T-SUBSTRATE-1's `WALLCLOCK_TO_TIP_HOURS`.
- Produces: a `public.*` schema at tip; `scripts/check_dbsync_tip.sh` (reused by the runbook, by
  T-SUBSTRATE-6's precondition and by `scripts/measure_substrate.sh`'s precondition); the recorded
  wall-clock number that replaces the 24–48 h band; PrometheusRule `prometheusrule-palmares-dbsync`
  with alert `PalmaresDbSyncLagging`.

- [ ] **Step 1: Write the failing test**

`scripts/check_dbsync_tip.sh` — F1 check 1's four assertions, as raw SQL. `/status` does not exist yet
(it is the read-path block's `GET /status`, Phase 3 step 18); these are the same quantities, read
directly, and the read-path block re-asserts them through the endpoint for check 16's API half.

```bash
#!/usr/bin/env bash
# F1 check 1. Exit 0 only when db-sync is at tip with the governance data F1 needs.
# PSQL may be overridden for compose: PSQL="docker compose exec -T postgres psql -U dbsync -d cexplorer"
set -euo pipefail
: "${PSQL:=kubectl --context minirack-lan -n cardano-preprod exec -i deploy/deployment-palmares-postgres -- psql -U dbsync -d cexplorer}"
q() { $PSQL -At -c "$1"; }

printf 'lag_seconds        %s\n' "$(q "SELECT EXTRACT(EPOCH FROM (now() AT TIME ZONE 'UTC' - (SELECT time FROM block ORDER BY id DESC LIMIT 1)))::bigint;")"
printf 'tip_epoch          %s\n' "$(q 'SELECT max(epoch_no) FROM block;')"
printf 'drep_distr_epochs  %s\n' "$(q 'SELECT min(epoch_no)||".."||max(epoch_no)||" n="||count(DISTINCT epoch_no) FROM drep_distr;')"
printf 'stake_deregistration %s\n' "$(q 'SELECT count(*) FROM stake_deregistration;')"
printf 'pool_update          %s\n' "$(q 'SELECT count(*) FROM pool_update;')"

fail=0
chk() { [ "$(q "SELECT $2;")" = t ] || { echo "FAIL: $1"; fail=1; }; }
# A1: < 120 s sustained. Asserted from this value over a soak window, NOT from the /ready probe
# (which is red at 300 s, amber at 120 s in the UI — D-7).
chk "lag < 120s" "EXTRACT(EPOCH FROM (now() AT TIME ZONE 'UTC' - (SELECT time FROM block ORDER BY id DESC LIMIT 1))) < 120"
chk "drep_distr current"         "(SELECT max(epoch_no) FROM drep_distr) >= (SELECT max(epoch_no) FROM block) - 1"
chk "drep_distr has no gaps"     "(SELECT (max(epoch_no)-min(epoch_no)+1) = count(DISTINCT epoch_no) FROM drep_distr)"
chk "stake_deregistration non-empty" "(SELECT count(*) FROM stake_deregistration) > 0"
chk "pool_update non-empty"          "(SELECT count(*) FROM pool_update) > 0"
chk "network is preprod"             "(SELECT network_name = 'preprod' FROM meta LIMIT 1)"
exit $fail
```

- [ ] **Step 2: Run test to verify it fails**

Run: `./scripts/check_dbsync_tip.sh`
Expected: FAIL — `FAIL: lag < 120s` plus `FAIL: drep_distr current` while the sync is mid-chain
(`lag_seconds` prints a number in the millions on the first day).

- [ ] **Step 3: Observe the sync to completion, then write the lag alert**

Progress is observed from db-sync's own records, not from log tailing:
```bash
# the authoritative progress curve
kubectl --context minirack-lan -n cardano-preprod exec -i deploy/deployment-palmares-postgres -- \
  psql -U dbsync -d cexplorer -c "SELECT no, seconds, state FROM epoch_sync_time ORDER BY no DESC LIMIT 10;"
# the two gauges the alert will use
kubectl --context minirack-lan -n cardano-preprod exec deploy/deployment-palmares-dbsync -- \
  sh -c 'wget -qO- http://127.0.0.1:8080/ | grep -E "cardano_db_sync_(node|db)_block_height"'
```

Expected duration: **24–48 h, floor ~8 h** — a derived band, no published figure exists for any preset on
any architecture. **T-SUBSTRATE-1 replaces this band with a measured number**; record the cluster's own
wall-clock in `docs/deployment/preprod-dbsync-sync.md` together with the `epoch_sync_time` dump, and
state the Pi-vs-Mac ratio that fell out.

Write the one alert worth having, **only now** — during the sync it would fire continuously and teach
everyone to ignore it:

```yaml
# monitoring/prometheusrule-palmares-dbsync.yaml — the one rule, in one group; add the file to
# chains/cardano/preprod-dbsync/kustomization.yaml in the same commit or Flux never applies it.
alert: PalmaresDbSyncLagging
expr: cardano_db_sync_node_block_height - cardano_db_sync_db_block_height > 20
for: 15m
labels:
  severity: warning
annotations:
  summary: >-
    db-sync is {{ $value }} blocks behind the preprod node for 15 min — /ready is red past 300 s
    and every palmares.* read is stale.
```

House conventions, from `infrastructure/configs/monitoring/prometheusrule-node-thermal.yaml`:
`severity` label + a `summary` annotation and **no `description`** (the Telegram receiver renders only
`summary`). `namespace: cardano-preprod` is fine — the chart sets `ruleSelector: {}` with no namespace
restriction (`helmrelease-kube-prometheus-stack.yaml:88-91`).

`cardano_db_sync_node_slot_height` **does not exist** — db-sync exports exactly six gauges and an alert
on that name would produce an empty series and never fire. The `> 20` threshold is the 300 s `/ready`
red line expressed in blocks at preprod's measured ~22.7 s mean block interval (300/22.7 ≈ 13; 20 leaves
margin for a legitimate burst).

The runbook note that belongs in `docs/deployment/preprod-dbsync-sync.md`: **Mithril does not speed up
db-sync.** It restores the *node's* chain database; db-sync still streams every block from genesis and
maintains its own ledger state, and no preprod Postgres snapshot is published
(`update-cardano-preprod.iohk.io` does not resolve). This is the single most likely thing for a
contributor to get wrong.

- [ ] **Step 4: Run test to verify it passes**

```bash
./scripts/check_dbsync_tip.sh                    # must exit 0
# ~1 h soak: 12 samples, 5 min apart
for i in $(seq 1 12); do ./scripts/check_dbsync_tip.sh | grep ^lag_seconds; sleep 300; done
```
Expected: PASS — exit 0, and all 12 `lag_seconds` samples < 120, which is how spec check A1 is signed.
Then the alert, gated on Prometheus actually having the two series (an alert over an empty series never
fires, which is the failure mode this task exists to avoid):
```bash
flux --context minirack-lan reconcile kustomization cardano-preprod-dbsync --with-source
kubectl --context minirack-lan -n cardano-preprod get prometheusrule prometheusrule-palmares-dbsync
kubectl --context minirack-lan -n monitoring port-forward svc/kube-prometheus-stack-prometheus 9090:9090 &
sleep 3
curl -sG --data-urlencode \
  'query=cardano_db_sync_node_block_height - cardano_db_sync_db_block_height' \
  http://127.0.0.1:9090/api/v1/query | jq -e '.data.result | length == 1'
curl -s 'http://127.0.0.1:9090/api/v1/rules' \
  | jq -e '[.data.groups[].rules[] | select(.name=="PalmaresDbSyncLagging")] | length == 1'
kill %1
```
Expected: PASS — the `get prometheusrule` prints the object, the instant query returns exactly one
sample (not an empty `result`), and `PalmaresDbSyncLagging` is loaded in Prometheus's rule set.

- [ ] **Step 5: Commit**

```bash
# in apps-k8s-configs
git add chains/cardano/preprod-dbsync/monitoring/prometheusrule-palmares-dbsync.yaml chains/cardano/preprod-dbsync/kustomization.yaml
git commit -m "feat(palmares): db-sync block-height lag alert (post first sync)"
# in palmares
git add scripts/check_dbsync_tip.sh docs/deployment/preprod-dbsync-sync.md
git commit -m "feat(ops): F1 check 1 tip assertions + measured preprod sync record"
```

---

### Task T-SUBSTRATE-6: `sql/040_indexes.sql` through the one-shot admin Job

**Cannot usefully run before T-SUBSTRATE-5.** db-sync's own `idx_drep_distr_hash_id_epoch_no` and
`idx_block_time` live in `migration-4-*` files, i.e. **stage 4**, which db-sync executes only once it is
within 30 minutes of the tip. Any query benchmarked before that is measuring a plan that will never run
in production, and `CREATE INDEX CONCURRENTLY` against db-sync's long bulk-insert transactions blocks or
fails. This task gates every later performance claim.

**Files:**
- Create: `/Users/mg/Projects/CardanoProjects/palmares/sql/040_indexes.sql`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/src/lib/offchain-lib/Palmares/Admin/ApplySql.hs`
- Modify: `/Users/mg/Projects/CardanoProjects/palmares/src/exe/admin/Main.hs`
- Modify: `/Users/mg/Projects/CardanoProjects/palmares/palmares.cabal`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/sql-jobs/job-palmares-apply-indexes.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/sql-jobs/kustomization.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/clusters/minirack/chains/cardano-preprod-dbsync-sql.yaml`
- Modify: `/Users/mg/Projects/apps-k8s-configs/clusters/minirack/chains/kustomization.yaml`
- Test: `/Users/mg/Projects/CardanoProjects/palmares/src/test/UnitTests/ApplySql.hs`

**Interfaces:**
- Consumes: T-SUBSTRATE-5's synced `public.*` and `scripts/check_dbsync_tip.sh`; T-SUBSTRATE-3's
  `configmap-palmares-dbsync-env` + `secret-palmares-dbsync` (the Job assembles its connection string
  from them).
- Produces: 22 indexes named `palmares_idx_*` on `public.*`; the `pg_trgm` extension;
  `admin apply-indexes`; env var `PALMARES_ADMIN_CONN_STR` (libpq keyword/value form, the `dbsync`
  superuser — **not** `PG_CONN_STR`, which T-SUBSTRATE-8 produces as the `palmares_ro` serving
  string); module `Palmares.Admin.ApplySql` in `offchain-lib` exposing
  `applySqlAutocommit :: ByteString -> ByteString -> IO ()` (connection string, then SQL),
  `indexesSql :: ByteString` and `assertIndexesValid :: Connection -> IO [Text]`; Flux Kustomization
  `cardano-preprod-dbsync-sql`.

- [ ] **Step 1: Write the failing test**

```haskell
-- src/test/UnitTests/ApplySql.hs
module UnitTests.ApplySql (tests) where

import Data.ByteString.Char8 qualified as BS8
import Palmares.Admin.ApplySql (indexesSql)
import Test.Tasty
import Test.Tasty.HUnit

-- isPrefixOf, never isInfixOf: the file's own header comment contains the words
-- "CREATE INDEX CONCURRENTLY", so an infix filter counts 23 statements and then asserts a
-- comment line is an index definition.
tests :: TestTree
tests = testGroup "sql/040_indexes.sql"
  [ testCase "pg_trgm is created before any index" $
      let ls = BS8.lines indexesSql
          ix s = length (takeWhile (not . BS8.isPrefixOf s) ls)
       in assertBool "CREATE EXTENSION precedes the first CREATE INDEX"
            (ix "CREATE EXTENSION IF NOT EXISTS pg_trgm" < ix "CREATE INDEX")
  , testCase "every index is CONCURRENTLY IF NOT EXISTS and palmares_idx_*" $
      let stmts = filter (BS8.isPrefixOf "CREATE INDEX") (BS8.lines indexesSql)
       in do length stmts @?= 22
             mapM_ (\s -> assertBool (BS8.unpack s)
                      (BS8.isInfixOf "CONCURRENTLY IF NOT EXISTS palmares_idx_" s)) stmts
  , testCase "no statement is wrapped in a transaction" $
      mapM_ (\kw -> assertBool (BS8.unpack kw) (not (BS8.isInfixOf kw indexesSql)))
            ["BEGIN", "COMMIT"]
  , testCase "the two trigram indexes that replace the cut projection tables are present" $
      mapM_ (\n -> assertBool (BS8.unpack n) (BS8.isInfixOf n indexesSql))
            [ "palmares_idx_ocvdd_given_name_trgm", "palmares_idx_ocvgad_title_trgm" ]
  ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test test --test-options='-p ApplySql'`
Expected: FAIL with `Could not find module 'Palmares.Admin.ApplySql'`. (The component is named `test`,
per T-EXTRACT's `palmares.cabal`.)

- [ ] **Step 3: Write `sql/040_indexes.sql`, then `applySqlAutocommit` + `assertIndexesValid` in `src/lib/offchain-lib/Palmares/Admin/ApplySql.hs`**

The SQL is an exact copy, so it is given in full. Every name is T-DATA §3.4's, re-prefixed `palmares_idx_*`
per synthesis §1.1 (`gov_idx_*` and the bare `idx_delegation_vote_drep`, which collided with upstream's
`idx_*`, are aliases to delete).

```sql
-- sql/040_indexes.sql — applied OUTSIDE any transaction by `admin apply-indexes`, connected as the
-- owner of db-sync's public tables (role `dbsync`). CREATE INDEX CONCURRENTLY cannot run inside a
-- transaction block, which is why this file is NOT part of query-api's boot set (sql/000-030).
-- Additive and migration-surviving (db-sync's migrations only add), but they must be re-applied
-- after any --rollback-to-slot + resync. Do NOT also use --schema-dir.
CREATE EXTENSION IF NOT EXISTS pg_trgm;

CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_voting_procedure_gap    ON public.voting_procedure (gov_action_proposal_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_voting_procedure_drep   ON public.voting_procedure (drep_voter) WHERE drep_voter IS NOT NULL;
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_voting_procedure_pool   ON public.voting_procedure (pool_voter) WHERE pool_voter IS NOT NULL;
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_voting_procedure_cc     ON public.voting_procedure (committee_voter) WHERE committee_voter IS NOT NULL;
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_voting_procedure_tx     ON public.voting_procedure (tx_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_drep_registration_drep  ON public.drep_registration (drep_hash_id, tx_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_delegation_vote_drep    ON public.delegation_vote (drep_hash_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_delegation_vote_addr    ON public.delegation_vote (addr_id, tx_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_gov_action_proposal_tx  ON public.gov_action_proposal (tx_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_gov_action_prev         ON public.gov_action_proposal (prev_gov_action_proposal) WHERE prev_gov_action_proposal IS NOT NULL;
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_ocvd_anchor             ON public.off_chain_vote_data (voting_anchor_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_ocvfe_anchor            ON public.off_chain_vote_fetch_error (voting_anchor_id, retry_count DESC);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_treasury_withdrawal_gap ON public.treasury_withdrawal (gov_action_proposal_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_committee_reg_cold      ON public.committee_registration (cold_key_id);
-- not in T-DATA §3.4: D-24 resolves a CC vote's COLD credential by LATERAL over committee_registration
-- keyed on hot_key_id, which the cold-only index cannot drive.
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_committee_reg_hot       ON public.committee_registration (hot_key_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_committee_dereg_cold    ON public.committee_de_registration (cold_key_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_stake_dereg_addr        ON public.stake_deregistration (addr_id, tx_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_reward_rest_type        ON public.reward_rest (type, spendable_epoch);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_epoch_state_epoch       ON public.epoch_state (epoch_no);
-- D-2 killed drep_distr as the eligibility source, but the per-epoch voting-power series still
-- filters on epoch_no ALONE, which upstream's (hash_id, epoch_no) index cannot drive.
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_drep_distr_epoch        ON public.drep_distr (epoch_no, hash_id);
-- D-1: these two replace the five cut projection tables' search text. GIN + gin_trgm_ops.
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_ocvdd_given_name_trgm   ON public.off_chain_vote_drep_data USING gin (given_name gin_trgm_ops);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_ocvgad_title_trgm       ON public.off_chain_vote_gov_action_data USING gin (title gin_trgm_ops);
```

The module lives in **`offchain-lib`**, not in `src/exe/admin/`: GHC ties the module name
`Palmares.Admin.ApplySql` to the path under `hs-source-dirs`, and the `test` component
(`hs-source-dirs: src/test`, depending on the four libraries) cannot import an executable's
`other-modules`. So `palmares.cabal` gains `Palmares.Admin.ApplySql` to `offchain-lib`'s
`exposed-modules`, `file-embed` + `postgresql-simple` to its `build-depends`,
`UnitTests.ApplySql` to the `test` component's `other-modules`, and
`extra-source-files: sql/*.sql` at the top level. `src/exe/admin/Main.hs` only wires the subcommand.

It embeds the file with `makeRelativeToProject "sql/040_indexes.sql" >>= embedFile` (one
`embedFile` per `.sql`, never `embedDir`), splits on
`;` and executes each statement with `postgresql-simple`'s `execute_`, which is autocommit —
`persistent`'s `runSqlPool` would wrap them in a transaction and `CONCURRENTLY` would fail.
`postgresql-simple ==0.7.0.0` and `file-embed ==0.0.16.0` are already in `cabal.project.freeze`
(verified: lines 471 and 251), so these are `build-depends` lines, not pin moves.

The one piece the signature does not determine — a `CREATE INDEX CONCURRENTLY` that fails leaves an
**invalid** index behind, which `IF NOT EXISTS` then silently accepts on every later run, so the Job
would report success forever over an index Postgres will never use:

```haskell
-- | Index names in sql/040_indexes.sql that exist but are not usable. Non-empty ⇒ the caller dies.
assertIndexesValid :: Connection -> IO [Text]
assertIndexesValid conn = map fromOnly <$> query_ conn
  "SELECT c.relname::text FROM pg_class c JOIN pg_index i ON i.indexrelid = c.oid \
  \ WHERE c.relname LIKE 'palmares\\_idx\\_%' AND NOT i.indisvalid"
```

`admin apply-indexes` = read `PALMARES_ADMIN_CONN_STR` → `applySqlAutocommit conn indexesSql` →
`assertIndexesValid` → `die` listing the invalid names, or print the count of valid `palmares_idx_*`.
Re-running it after a failure needs those names dropped first; put that one line in the runbook.

The k8s side is the repo's documented **gate Job** pattern (`apps-k8s-configs/CLAUDE.md`): a `batch/v1`
Job in **its own** Flux Kustomization with `wait: true`, carrying
`kustomize.toolkit.fluxcd.io/force: enabled` (only `enabled`/`Enabled` work, not `true`) because
`spec.template` is immutable, **no `ttlSecondsAfterFinished`** (a git-managed Job deleted by TTL is
recreated and re-run at every reconcile), `backoffLimit: 20` as the wait-for-Postgres mechanism, and an
idempotent command — the completed Job is the "it ran" marker. Its re-run trigger is the image digest,
exactly like `job-defibot-migrate`: new SQL ⇒ new `palmares-admin` image ⇒ new pod template ⇒ `force`
recreates ⇒ it re-runs, and only then. Image
`ghcr.io/<owner>/palmares-admin:<sha>` **(OPEN: the GHCR owner handle — T-INFRA's compose defaults to
`en7angled`, the account that runs public CI is `mariusgeorgescu`)**; `command: [admin, apply-indexes]`;
`PALMARES_ADMIN_CONN_STR` assembled in the pod from `configmap-palmares-dbsync-env` +
`secret-palmares-dbsync`, i.e. as the `dbsync` superuser, which is what ownership of `public.*` and
`CREATE EXTENSION` require.

**Precondition, enforced not assumed:** run `scripts/check_dbsync_tip.sh` and require exit 0 before
pushing the Job — T-SUBSTRATE-5's gate is this task's entry condition, because
`CREATE INDEX CONCURRENTLY` against db-sync's bulk-insert transactions blocks or leaves an invalid
index behind.

Flux root `clusters/minirack/chains/cardano-preprod-dbsync-sql.yaml`:
`path: ./chains/cardano/preprod-dbsync/sql-jobs`, `wait: true`, **`timeout: 30m0s`** (a cold GIN build
over `off_chain_vote_*` is minutes, not seconds), `dependsOn: [cardano-preprod-dbsync]`. The app root
that the read-path block adds then `dependsOn` this one, so the views are never created over an
unindexed `public`. (D-26 names two Flux roots; this third one is what the house gate-Job pattern
requires, and it keeps the long-running Job out of the 1-minute db-sync root.)

- [ ] **Step 4: Run test to verify it passes**

```bash
cd /Users/mg/Projects/CardanoProjects/palmares && cabal build all && cabal test test --test-options='-p ApplySql'
./scripts/check_dbsync_tip.sh                     # precondition: must exit 0 before the Job is pushed
# then against the real, synced database
cd /Users/mg/Projects/apps-k8s-configs && git push \
  && flux --context minirack-lan reconcile kustomization cardano-preprod-dbsync-sql --with-source
kubectl --context minirack-lan -n cardano-preprod wait --for=condition=complete job/job-palmares-apply-indexes --timeout=30m
kubectl --context minirack-lan -n cardano-preprod exec -i deploy/deployment-palmares-postgres -- \
  psql -U dbsync -d cexplorer -At -c \
  "SELECT count(*) FROM pg_class c JOIN pg_index i ON i.indexrelid=c.oid
   WHERE c.relname LIKE 'palmares\_idx\_%' AND i.indisvalid;"
```
Expected: PASS — the four HUnit cases green, the Job `condition=complete`, and the last query printing
exactly `22`.

- [ ] **Step 5: Commit**

```bash
# in palmares
git add sql/040_indexes.sql src/lib/offchain-lib/Palmares/Admin/ApplySql.hs src/exe/admin/Main.hs palmares.cabal src/test/UnitTests/ApplySql.hs
git commit -m "feat(admin): apply-indexes — palmares_idx_* on db-sync's public schema"
# in apps-k8s-configs
git add chains/cardano/preprod-dbsync/sql-jobs clusters/minirack/chains/cardano-preprod-dbsync-sql.yaml clusters/minirack/chains/kustomization.yaml
git commit -m "feat(palmares): gate Job for sql/040_indexes.sql"
```

---

### Task T-SUBSTRATE-7: sizing measurement and ADR `0001-governance-data-substrate.md`

The F0 exit criterion of spec §9: the ADR that fixes the data source, carrying Experiment A's and the
cluster's numbers rather than an estimate.

**Files:**
- Create: `/Users/mg/Projects/CardanoProjects/palmares/docs/adr/0001-governance-data-substrate.md`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/scripts/measure_substrate.sh`
- Modify: `/Users/mg/Projects/CardanoProjects/palmares/docs/deployment/preprod-dbsync-sync.md`

**Interfaces:**
- Consumes: `experiments/dbsync-preprod/measurements.md` (**ASSEMBLY CORRECTION: the Experiment A bundle
  that actually exists on disk is `experiments/dbsync-preprod/` with `measurements.md`, not
  `experiments/dbsync-genesis/RESULTS.md` — the two drafters named it differently; use the real path**);
  the synced cluster database; `scripts/check_dbsync_tip.sh`.

> **ASSEMBLY CORRECTION 3** (2026-10-19). ADR `0001` must also carry the **Yaci-Store rejection record**,
> citing `docs/superpowers/design/f1-yaci-store-evaluation.md`: rejected as substrate because its
> `drep_dist` depends on the UTxO store (account → stake-address-balance → snapshot), so it must index
> the entire UTxO set to produce per-epoch DRep power — heavier than `only_governance`, which gets it
> from the ledger replay with `tx_out` disabled; `GovActionStatus` has only `{ACTIVE, RATIFIED, EXPIRED}`
> (no ENACTED/DROPPED, open issue #1194); it stores `anchor_url`/`anchor_hash` only, with no off-chain
> fetching or hash verification, which would resurrect the fetcher we deleted; and its per-epoch range
> partitioning needs ~3,160 locks for one unbounded `drep_dist` query (open issue #1200) while its
> baseline migration opens with `drop table if exists drep_dist cascade`, which would silently drop our
> views. Record the T1–T7 monitorable triggers from that note's §8, and that Yaci-Store is **not** a
> fallback in the disk-pressure chain (being structurally heavier) — the fallback for "does not fit the
> Pi" is the x86_64 box budgeted for F2.
- Produces: `scripts/measure_substrate.sh`; ADR 0001 with the PVC verdict, the peak-RSS verdict and the
  recorded wall-clock.

- [ ] **Step 1: Write the failing test**

`scripts/measure_substrate.sh` — the measurement, re-runnable, and the ADR's only source of numbers.

```bash
#!/usr/bin/env bash
# Substrate sizing. Everything the ADR asserts must come out of this script, not out of a note.
set -euo pipefail
: "${PSQL:=kubectl --context minirack-lan -n cardano-preprod exec -i deploy/deployment-palmares-postgres -- psql -U dbsync -d cexplorer}"
K="kubectl --context minirack-lan -n cardano-preprod"

# Precondition. Measuring a mid-sync database produces numbers the ADR would then assert as final.
"$(dirname "$0")/check_dbsync_tip.sh" >/dev/null 2>&1 \
  || { echo "FAIL: db-sync is not at tip; ADR 0001 numbers are not final"; exit 1; }

echo '## pg_total_relation_size, public, top 20'
$PSQL -c "SELECT relname, pg_size_pretty(pg_total_relation_size(c.oid)) AS total
  FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace
  WHERE n.nspname = 'public' AND c.relkind = 'r'
  ORDER BY pg_total_relation_size(c.oid) DESC LIMIT 20;"

echo '## database and PVC'
$PSQL -At -c "SELECT 'pg_database_size_bytes', pg_database_size('cexplorer');"
$K exec deploy/deployment-palmares-postgres -- df -h /var/lib/postgresql/data | tail -1

echo '## db-sync peak RSS (cgroup v2; the sampled figure from Experiment A is the cross-check)'
$K exec deploy/deployment-palmares-dbsync -- sh -c \
  'cat /sys/fs/cgroup/memory.peak 2>/dev/null || cat /sys/fs/cgroup/memory/memory.max_usage_in_bytes' \
  || echo 'peak_rss unavailable — use Experiment A rss.csv'
$K top pod -l app=palmares-dbsync-pod-label --no-headers || echo 'metrics-server unavailable'

echo '## wall-clock, from db-sync itself'
$PSQL -At -c "SELECT 'epochs_timed', count(*), 'total_seconds', sum(seconds)::bigint FROM epoch_sync_time;"

# The gate: does the measured size still fit the PVC that cannot be expanded?
used=$($PSQL -At -c "SELECT pg_database_size('cexplorer');")
[ "$used" -lt 26843545600 ] || { echo "FAIL: cexplorer $used B >= 25 GB — see ADR 0001 fallbacks"; exit 1; }
```

- [ ] **Step 2: Run test to verify it fails**

Run: `./scripts/measure_substrate.sh` — before T-SUBSTRATE-5's gate has gone green.
Expected: FAIL with exactly `FAIL: db-sync is not at tip; ADR 0001 numbers are not final`, exit 1.

- [ ] **Step 3: Write the ADR from the script's output**

ADR 0001 records, as measured values with the command that produced each: the substrate decision
(cardano-db-sync 13.7.1.0, `preset: only_governance`, `ledger: enable`, `shelley: enable: true`,
`offchain_vote_data: enable`, the blinklabs arm64 digest, Postgres 17 in the same `cexplorer` database
with two added schemas); the `pg_total_relation_size` table; `pg_database_size`; peak RSS from both the
cluster cgroup and Experiment A's sampled figure; the wall-clock; and the three verdicts:

- **PVC verdict** — 50 Gi, **sized once**: `local-path` has `allowVolumeExpansion: false`, so the number
  in the manifest is final for the life of that volume. State the measured headroom multiple.
- **Peak-RSS verdict** — the `requests: 4Gi` / `limits: 6Gi` pair confirmed, raised, or escalated to
  D-13 fallback 2.
- **Fallbacks, ordered, with their triggers** — (1) `DELETE FROM epoch_stake WHERE epoch_no < tip-2` as
  a CronJob, **only after the `validateEpochStake` test exists** (db-sync reads that table back for
  validation); (2) `ledger_backend: lsm`, accepting the Raspberry-Pi system-thread crash and the
  >4096-item `blockio-uring` bug; (3) db-sync off-cluster.

Also record what was deliberately **not** configured away: `epoch_stake` is inserted unconditionally
whenever the ledger is enabled (`insertStakeSlice` has no option gate), so it is the largest table in the
database and 100 % unused by F1 — that is the fact fallback (1) exists for.

Honest limits to write down: `tx.fee` is `0` under this preset (`tx_out: disable`), `off_chain_pool_data`
is empty so SPO pages show hashes not tickers, and constitution anchors are never fetched
(`queryNewVoteWorkQueueDataStmt` has `AND va.type != 'constitution'`).

- [ ] **Step 4: Run test to verify it passes**

```bash
cd /Users/mg/Projects/CardanoProjects/palmares && ./scripts/measure_substrate.sh | tee /tmp/substrate.txt
# the ADR must quote the measured size, not an estimate
size=$(grep '^pg_database_size_bytes' /tmp/substrate.txt | cut -f2)
grep -q "$size" docs/adr/0001-governance-data-substrate.md
```
Expected: PASS — the script exits 0 (database under 25 GB) and the measured `pg_database_size`
byte count appears verbatim in ADR 0001.

- [ ] **Step 5: Commit**

```bash
git add docs/adr/0001-governance-data-substrate.md scripts/measure_substrate.sh docs/deployment/preprod-dbsync-sync.md
git commit -m "docs(adr): 0001 governance data substrate, with measured sizing"
```

---

### Task T-SUBSTRATE-8: Postgres roles, grants and `search_path`

The gap T-EXTRACT flagged and nobody closed. `palmares_owner` and `palmares_ro` are created here, as the
`dbsync` superuser, by the same gate-Job mechanism as T-SUBSTRATE-6 — not by an `initdb.d` script, which
would duplicate the file across two repos and only ever run on a fresh volume.

Its *cluster* precondition is only that db-sync's stage-1/2 migrations have run (seconds into
T-SUBSTRATE-4), not a completed sync. It is nonetheless placed after T-SUBSTRATE-6 because it consumes
`applySqlAutocommit` and the `sql-jobs/` Flux root from it, and because synthesis §3 puts roles in
Phase 3 (step 14) behind all of Phase 2. If the schedule needs the wait filled, the only safe reordering
is to move the whole `Palmares.Admin.ApplySql` + `sql-jobs/` machinery into this task and leave
T-SUBSTRATE-6 holding `indexesSql` alone.

**Files:**
- Create: `/Users/mg/Projects/CardanoProjects/palmares/sql/050_roles.sql`
- Modify: `/Users/mg/Projects/CardanoProjects/palmares/src/lib/offchain-lib/Palmares/Admin/ApplySql.hs`
- Modify: `/Users/mg/Projects/CardanoProjects/palmares/src/exe/admin/Main.hs`
- Modify: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/secrets/secret-palmares-dbsync.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/sql-jobs/job-palmares-apply-roles.yaml`
- Modify: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/sql-jobs/kustomization.yaml`
- Test: `/Users/mg/Projects/CardanoProjects/palmares/scripts/check_roles.sh`

**Interfaces:**
- Consumes: T-SUBSTRATE-3's `dbsync` superuser and `cexplorer`; `applySqlAutocommit`,
  `PALMARES_ADMIN_CONN_STR`, the `sql-jobs/` directory and the Flux root
  `cardano-preprod-dbsync-sql`, all from T-SUBSTRATE-6.
- Produces: roles `palmares_owner` and `palmares_ro`; schema `palmares_attest` owned by
  `palmares_owner`; `admin apply-roles`; `rolesSql :: ByteString`; secret keys
  `PALMARES_OWNER_PASSWORD` and `PALMARES_RO_PASSWORD`; env vars `PG_CONN_STR` (runtime,
  `palmares_ro`) and `PALMARES_OWNER_CONN_STR` (boot DDL + matview refresh, `palmares_owner`);
  `scripts/check_roles.sh`.

- [ ] **Step 1: Write the failing test**

```bash
#!/usr/bin/env bash
# scripts/check_roles.sh — the privilege separation is the control for co-locating our schemas in
# db-sync's database (T-DATA §10). Assert it, do not assume it.
set -euo pipefail
: "${PSQL:=kubectl --context minirack-lan -n cardano-preprod exec -i deploy/deployment-palmares-postgres -- psql -U dbsync -d cexplorer}"
q() { $PSQL -At -c "$1"; }
fail=0
chk() { [ "$(q "SELECT $2;")" = t ] || { echo "FAIL: $1"; fail=1; }; }

chk "palmares_owner exists, can log in, is not superuser" \
  "(SELECT rolcanlogin AND NOT rolsuper FROM pg_roles WHERE rolname='palmares_owner')"
chk "palmares_ro exists, can log in, is not superuser" \
  "(SELECT rolcanlogin AND NOT rolsuper FROM pg_roles WHERE rolname='palmares_ro')"
chk "palmares_ro may read db-sync's tables" \
  "has_table_privilege('palmares_ro','public.voting_procedure','SELECT')"
chk "palmares_ro may NOT write db-sync's tables" \
  "NOT has_table_privilege('palmares_ro','public.voting_procedure','INSERT')"
chk "palmares_ro may NOT create in public" \
  "NOT has_schema_privilege('palmares_ro','public','CREATE')"
chk "palmares_owner may create the palmares schema" \
  "has_database_privilege('palmares_owner','cexplorer','CREATE')"
chk "palmares_attest is owned by palmares_owner" \
  "(SELECT nspowner = 'palmares_owner'::regrole FROM pg_namespace WHERE nspname='palmares_attest')"
chk "search_path resolves unqualified names for palmares_ro" \
  "(SELECT 'search_path=palmares, palmares_attest, public' = ANY(rolconfig) FROM pg_roles WHERE rolname='palmares_ro')"
# future-proofing: a db-sync migration that adds a table must not need a re-GRANT
chk "default privileges cover tables dbsync creates later in public" \
  "(SELECT count(*) > 0 FROM pg_default_acl d JOIN pg_namespace n ON n.oid = d.defaclnamespace
      WHERE n.nspname='public' AND d.defaclobjtype='r'
        AND array_to_string(d.defaclacl,',') LIKE '%palmares_ro=r%')"
exit $fail
```

- [ ] **Step 2: Run test to verify it fails**

Run: `./scripts/check_roles.sh`
Expected: FAIL with all nine lines, starting `FAIL: palmares_owner exists, can log in, is not superuser`
(`pg_roles` has no such row, so the subselect is NULL, not `t`), and with
`ERROR: role "palmares_ro" does not exist` on stderr from the `has_*_privilege` checks. Exit 1.

- [ ] **Step 3: Write `sql/050_roles.sql` and `admin apply-roles`**

Exact content, because the grant set is the whole deliverable:

```sql
-- sql/050_roles.sql — applied OUTSIDE any transaction by `admin apply-roles`, connected as `dbsync`
-- (the Postgres bootstrap superuser). Idempotent; safe to re-run after any db-sync migration.
-- Schema `palmares` is deliberately NOT created here: see the note at the bottom.

-- 1. The two roles. CREATE ROLE has no IF NOT EXISTS.
DO $$ BEGIN CREATE ROLE palmares_owner LOGIN; EXCEPTION WHEN duplicate_object THEN NULL; END $$;
DO $$ BEGIN CREATE ROLE palmares_ro    LOGIN; EXCEPTION WHEN duplicate_object THEN NULL; END $$;

-- 2. palmares_owner creates and owns our two schemas; palmares_ro only connects.
GRANT CONNECT, CREATE ON DATABASE cexplorer TO palmares_owner;
GRANT CONNECT             ON DATABASE cexplorer TO palmares_ro;
CREATE SCHEMA IF NOT EXISTS palmares_attest AUTHORIZATION palmares_owner;

-- 3. Read-only access to db-sync's schema for both roles. ALL TABLES covers what exists now;
--    ALTER DEFAULT PRIVILEGES covers every table a future db-sync migration adds — without it,
--    each upgrade would silently leave new tables unreadable until someone re-granted by hand.
GRANT USAGE ON SCHEMA public TO palmares_owner, palmares_ro;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO palmares_owner, palmares_ro;
ALTER DEFAULT PRIVILEGES FOR ROLE dbsync IN SCHEMA public
  GRANT SELECT ON TABLES TO palmares_owner, palmares_ro;
-- never granted: INSERT/UPDATE/DELETE on public, and CREATE on public. The public read API must
-- never hold a role that can write the substrate.
REVOKE CREATE ON SCHEMA public FROM PUBLIC;

-- 4. palmares_ro reads the attestation projections that attest-sync creates as palmares_owner.
GRANT USAGE ON SCHEMA palmares_attest TO palmares_ro;
GRANT SELECT ON ALL TABLES IN SCHEMA palmares_attest TO palmares_ro;
ALTER DEFAULT PRIVILEGES FOR ROLE palmares_owner IN SCHEMA palmares_attest
  GRANT SELECT ON TABLES TO palmares_ro;

-- 5. search_path, so an unqualified `drep` resolves to palmares.drep, an unqualified
--    `profile_projection` to palmares_attest.profile_projection, and an unqualified `block` to
--    db-sync's public.block — in every session of either role, including psql and the Job.
ALTER ROLE palmares_owner SET search_path = palmares, palmares_attest, public;
ALTER ROLE palmares_ro    SET search_path = palmares, palmares_attest, public;

-- NOTE, and it is load-bearing: query-api's boot transaction runs
--   DROP SCHEMA IF EXISTS palmares CASCADE; CREATE SCHEMA palmares;
-- (D-15), which destroys every grant on that schema and the pg_default_acl row with it. So
-- ALTER DEFAULT PRIVILEGES cannot carry it, and sql/000_schema.sql MUST itself end with
--   GRANT USAGE ON SCHEMA palmares TO palmares_ro;
-- and the boot set's last statement MUST be
--   GRANT SELECT ON ALL TABLES IN SCHEMA palmares TO palmares_ro;
-- Both inside the same transaction. Omit either and query-api serves 500s on every governance route
-- after its first restart.
```

`admin apply-roles` = `applySqlAutocommit <PALMARES_ADMIN_CONN_STR> rolesSql` (reusing T-SUBSTRATE-6's function and
`makeRelativeToProject "sql/050_roles.sql" >>= embedFile`), then set the two passwords from env so no
credential is ever in Git:

```haskell
-- After the file. postgresql-simple substitutes client-side, so `?` yields a correctly escaped
-- literal in the PASSWORD position; the role name cannot be a parameter and is a constant.
setPw :: Connection -> Query -> Text -> IO ()
setPw conn stmt pw = void (execute conn stmt (Only pw))
-- stmts: "ALTER ROLE palmares_owner PASSWORD ?" and "ALTER ROLE palmares_ro PASSWORD ?"
-- values: PALMARES_OWNER_PASSWORD, PALMARES_RO_PASSWORD; die if either is unset or empty.
```

Passwords are required, not optional: the `postgres:17-alpine` default `pg_hba.conf` uses
`scram-sha-256` for host connections, so a `LOGIN` role with no password cannot connect at all.
Add `PALMARES_OWNER_PASSWORD` and `PALMARES_RO_PASSWORD` to the existing SOPS Secret
`secret-palmares-dbsync` (`openssl rand -base64 24` each) — no new secret file, `.sops.yaml` unchanged.

The two connection strings the read path consumes, both libpq keyword/value form as the Belt System
already uses, both assembled in the consuming pod and **never** sharing a password with `dbsync`:

| Env var | Role | Used by |
|---|---|---|
| `PG_CONN_STR` | `palmares_ro` | query-api's serving pool; attest-sync's reads |
| `PALMARES_OWNER_CONN_STR` | `palmares_owner` | query-api's `sql/000`–`030` boot transaction and `Query/Refresh.hs`; attest-sync's `CREATE SCHEMA` + `runMigrations` |

The Secret lives in namespace `cardano-preprod` and the app pods live in `palmares`; a Secret cannot be
read across namespaces, so the app overlay carries its own SOPS copy of the two passwords. That is the
app-deploy block's file, named here so it is not discovered at deploy time.

The Job is T-SUBSTRATE-6's Job with `command: [admin, apply-roles]` and the same gate-Job rules
(`force: enabled`, no `ttlSecondsAfterFinished`, `backoffLimit: 20`), in the same
`sql-jobs/` directory and therefore the same Flux root.

- [ ] **Step 4: Run test to verify it passes**

```bash
cd /Users/mg/Projects/apps-k8s-configs && git push \
  && flux --context minirack-lan reconcile kustomization cardano-preprod-dbsync-sql --with-source
kubectl --context minirack-lan -n cardano-preprod wait --for=condition=complete job/job-palmares-apply-roles --timeout=5m
cd /Users/mg/Projects/CardanoProjects/palmares && ./scripts/check_roles.sh
# and prove the credential actually works end to end
kubectl --context minirack-lan -n cardano-preprod exec -i deploy/deployment-palmares-postgres -- \
  env PGPASSWORD="$PALMARES_RO_PASSWORD" psql -h 127.0.0.1 -U palmares_ro -d cexplorer -At \
  -c 'SELECT count(*) FROM voting_procedure;'     # unqualified: resolved via search_path
```
Expected: PASS — the Job `condition=complete`, `check_roles.sh` exits 0 with no `FAIL:` line, and the
unqualified `voting_procedure` count returns a number as `palmares_ro`.

- [ ] **Step 5: Commit**

```bash
# in palmares
git add sql/050_roles.sql src/lib/offchain-lib/Palmares/Admin/ApplySql.hs src/exe/admin/Main.hs scripts/check_roles.sh
git commit -m "feat(admin): apply-roles — palmares_owner/palmares_ro grants and search_path"
# in apps-k8s-configs
git add chains/cardano/preprod-dbsync/sql-jobs chains/cardano/preprod-dbsync/secrets/secret-palmares-dbsync.yaml
git commit -m "feat(palmares): gate Job for sql/050_roles.sql + role passwords"
```

---

### Block interfaces — produces

**Postgres / SQL**
- Database `cexplorer` on `service-palmares-postgres.cardano-preprod.svc.cluster.local:5432`, one
  database with two added schemas.
- Roles `dbsync` (bootstrap superuser, owns `public`), `palmares_owner` (owns `palmares` +
  `palmares_attest`, `SELECT` on `public.*`, refreshes the matview), `palmares_ro`
  (`USAGE` + `SELECT` only).
- Schema `palmares_attest`, owned by `palmares_owner` (attest-sync's own
  `CREATE SCHEMA IF NOT EXISTS` stays, and is a no-op).
- `search_path = palmares, palmares_attest, public` on both roles.
- `ALTER DEFAULT PRIVILEGES FOR ROLE dbsync IN SCHEMA public GRANT SELECT ON TABLES TO
  palmares_owner, palmares_ro` — new db-sync tables are readable without a re-grant.
- 22 indexes `palmares_idx_*` on `public.*`, all valid, including
  `palmares_idx_drep_distr_epoch`, `palmares_idx_committee_reg_hot`,
  `palmares_idx_ocvdd_given_name_trgm` and `palmares_idx_ocvgad_title_trgm`; extension `pg_trgm`.
- **Contract on the read-path block:** `sql/000_schema.sql` must end with
  `GRANT USAGE ON SCHEMA palmares TO palmares_ro;` and the boot transaction's last statement must be
  `GRANT SELECT ON ALL TABLES IN SCHEMA palmares TO palmares_ro;` — `DROP SCHEMA … CASCADE` destroys
  every grant on `palmares` and the `pg_default_acl` row with it.

**Haskell / repo files**
- `admin apply-indexes`, `admin apply-roles`.
- `Palmares.Admin.ApplySql` (in **`offchain-lib`**, file
  `src/lib/offchain-lib/Palmares/Admin/ApplySql.hs`):
  `applySqlAutocommit :: ByteString -> ByteString -> IO ()` (connection string, then SQL),
  `indexesSql :: ByteString`, `rolesSql :: ByteString`,
  `assertIndexesValid :: Connection -> IO [Text]`.
- `sql/040_indexes.sql`, `sql/050_roles.sql`.
- `scripts/check_node_hostpath.sh`, `scripts/check_postgres.sh`, `scripts/check_dbsync.sh`,
  `scripts/check_dbsync_tip.sh`, `scripts/measure_substrate.sh`, `scripts/check_roles.sh`.
- `experiments/dbsync-genesis/{compose.yaml,db-sync-config.yaml,node-configs/,measure.sh,rss-sample.sh,RESULTS.md}`
  — the `chain`-profile services become the root `compose.yaml`'s `chain` profile verbatim.
- `docs/adr/0001-governance-data-substrate.md`, `docs/deployment/preprod-dbsync-sync.md`.
- Cabal: `postgresql-simple` + `file-embed` and `Palmares.Admin.ApplySql` on **`offchain-lib`**;
  `UnitTests.ApplySql` on the `test` component; `extra-source-files: sql/*.sql`.

**Env vars and secrets**
- `PALMARES_ADMIN_CONN_STR` — `dbsync` superuser, the two gate Jobs only.
- `PG_CONN_STR` — `palmares_ro`, serving.
- `PALMARES_OWNER_CONN_STR` — `palmares_owner`, boot DDL + matview refresh + attest migrations.
- Secret keys `POSTGRES_PASSWORD`, `PALMARES_OWNER_PASSWORD`, `PALMARES_RO_PASSWORD` in
  `secret-palmares-dbsync` (ns `cardano-preprod`); the app overlay needs its own SOPS copy of the last
  two, since Secrets do not cross namespaces.
- ConfigMap `configmap-palmares-dbsync-env`: `POSTGRES_HOST`, `POSTGRES_PORT`, `POSTGRES_DB=cexplorer`,
  `POSTGRES_USER=dbsync`, `PGDATA`.

**Cluster objects (`apps-k8s-configs` only — D-26)**
- Manifest dirs `chains/cardano/preprod-dbsync/` and `chains/cardano/preprod-dbsync/sql-jobs/`.
- Flux Kustomizations `cardano-preprod-dbsync` (`wait: true`, `timeout: 1m0s`) and
  `cardano-preprod-dbsync-sql` (`wait: true`, `timeout: 30m0s`); the app root must `dependsOn` the
  latter.
- `deployment-palmares-dbsync`, `deployment-palmares-postgres`, `service-palmares-postgres`,
  `service-palmares-dbsync-metrics`, `pvc-palmares-dbsync-pg` (50 Gi),
  `pvc-palmares-dbsync-state` (10 Gi), `networkpolicy-palmares-postgres`,
  `servicemonitor-palmares-dbsync` (`path: /`), `prometheusrule-palmares-dbsync` with alert
  `PalmaresDbSyncLagging`
  (`cardano_db_sync_node_block_height - cardano_db_sync_db_block_height > 20` for `15m`),
  `job-palmares-apply-indexes`, `job-palmares-apply-roles`.
- hostPaths `/var/lib/cardano-preprod/{ipc,configs}` on `minirack-pi5-1`.

**Measurements that later blocks quote**
- `WALLCLOCK_TO_TIP_HOURS`, `PG_TOTAL_BYTES`, `PEAK_RSS_BYTES`, the `epoch_sync_time` curve, and the
  five `count(*)` probes — all in `experiments/dbsync-genesis/RESULTS.md` and ADR 0001.
- The anchor-coverage figure (`off_chain_vote_data` vs `off_chain_vote_fetch_error`) that T-API §8.5
  defers and that triggers the deferred supplementary re-fetcher at > ~20 % `not_fetched`.

### Block open items

- **(OPEN: the GHCR owner handle.)** `ghcr.io/<owner>/palmares-admin` — T-INFRA's compose defaults to
  `en7angled`; the account with proven public CI is `mariusgeorgescu`. Blocks the two Job manifests
  (T-SUBSTRATE-6, T-SUBSTRATE-8). Owner's call, one find/replace.
- **(OPEN: `drep_distr` coverage of the earliest Conway epochs.)** F1 check 1 is written as
  "populated for every Conway epoch", and preprod's first Conway epoch is 163 — but zero DReps may have
  been registered in the first epochs after Chang, in which case an empty `drep_distr` for them is
  correct, not a defect. T-SUBSTRATE-1 and T-SUBSTRATE-5 assert the defensible form (no gap between
  `min` and `max`, and `max >= tip-1`) and **measure** `min(epoch_no)`; if that comes back as 163 the
  stronger wording stands, and if not the check's wording is amended with the measured value.
- **(OPEN: whether db-sync 13.7.1.0 tolerates the node config's `LedgerDB: V2LSM` block.)** Answered for
  free by T-SUBSTRATE-1 using the cluster's real `config.json`; the fallback (a vendored db-sync-only
  `config.json` with that block removed, genesis hashes kept) is written into T-SUBSTRATE-4 and costs one
  ConfigMap key.
- **(OPEN: `minirack-m4`'s live taint.)** T-INFRA §1.2 records that
  `apps-k8s-configs/CLAUDE.md` documents `dedicated=m4-keys` on `minirack-m4-keys`, not the
  `dedicated=m4:NoSchedule` the first draft assumed, and that `minirack-m4-keys` is absent from the live
  node list. Irrelevant to this block (everything here pins `minirack-pi5-1`) but it must be re-read live
  before the app overlay's placement patch is written.
- **(OPEN: re-check `ghcr.io/blinklabs-io/cardano-db-sync` tags before T-SUBSTRATE-4.)** Upstream aarch64
  images are in CHANGELOG "Unreleased"; if an official arm64 13.7.2.1+ has landed, the pin decision is
  revisited **before** the one-shot sync, never after.

## Block 4 — Views, eligibility and the metric catalog

# Plan A — block: the SQL layer and the metric catalog

This block delivers the whole `palmares` Postgres schema (synthesis §3 Phase 3 steps 15–17, plus `Query/Views.hs`
and `Query/Refresh.hs` out of step 18): `sql/000_schema.sql` → `sql/030_metrics.sql` applied by query-api at boot as
`palmares_owner` in one transaction that begins `DROP SCHEMA IF EXISTS palmares CASCADE; CREATE SCHEMA palmares;`
and **`die`s on failure**; the seventeen plain views of synthesis §1.2 plus `palmares.threshold_map` (D-25, which
§1.2 does not enumerate), the one SQL function and the one materialized view, under the canonical names of §1.1
and the canonical metric column names of §1.4; the `file-embed`ed metric catalog that
is the single source of truth for every published formula; the epoch-320 fixture test; and the single
`REFRESH … CONCURRENTLY` thread. It implements D-2 (eligibility from `drep_interval` + `drep_votable`, never
`drep_distr` membership), D-3 (five rationale grades summing to `votes_cast`, numerator `verified + inline`),
D-10 (one anchor verdict `CASE` in SQL with its mandatory golden test on the exact `fetch_error` strings),
D-15 (`palmares.eligible_actions` is a SQL function; the DDL runs in one transaction; `sql/040_indexes.sql` is
*not* ours), D-19 (`palmares.activity` unions the five epoch-boundary kinds with the corrected cursor), D-20 (one
unfiltered `drep_epoch_power` with `target_kind`), D-24 (`palmares.vote` carries `voter_hot_hash` **and**
`voter_cold_hash`), D-25 (the static (role, action-type) → threshold-column table as a named deliverable with its
own test) and D-9 (the catalog, not hand-written prose, generates `docs/metrics.md`). Assumptions: the repo
bootstrap and extraction (synthesis §3 steps 5–7) have landed, so `palmares.cabal`, `src/exe/query-api/`,
`src/exe/admin/` and `src/test/` exist; roles `palmares_owner` / `palmares_ro` and their grants (step 14) exist
before the first boot against a real db-sync; `sql/040_indexes.sql` and the preprod sync (steps 8–13) are other
blocks' work, and every *measured* assertion here therefore runs against a caught-up preprod db-sync while every
*shape* assertion runs in CI against a seeded skeleton. File split (ours, not in the notes): `000` = schema,
`010_gov_views.sql` = views over db-sync only, `020_views.sql` = views over `palmares` views + the function,
`030_metrics.sql` = the matview. T-EXTRACT's `sql/020_anchors.sql` is an alias to delete on sight — the anchor
view must precede `gov_action` and `vote` in lexical order, so it opens `010_gov_views.sql`. All four files are
created (three of them as comment-only stubs) in T-SQL-METRICS-1, because `palmaresSqlFiles` embeds them by name
at compile time; every later task appends to a file that already exists. `sql/030_metrics.sql` is written last
(T-SQL-METRICS-12) rather than at its §3 step-16 position: nothing in steps 15–17 reads the matview, and its
expected numbers are the step-17 fixture's, so building it after the fixture removes a round of re-derivation.

---

### Task T-SQL-METRICS-1: Schema bootstrap, fail-closed, plus the db-sync skeleton that makes every later view testable in CI

**Files:**
- Create: `sql/000_schema.sql`
- Create: `sql/010_gov_views.sql`, `sql/020_views.sql`, `sql/030_metrics.sql` — comment-only stubs; later tasks append
- Create: `src/exe/query-api/Query/Views.hs`
- Create: `test/fixtures/dbsync-skeleton.sql`
- Create: `src/test/Test/Pg.hs`
- Test: `src/test/UnitTests/Views.hs`
- Modify: `palmares.cabal` (`extra-source-files: sql/*.sql` — already in T-EXTRACT §1.3; `query-api` `other-modules: Query.Views`; test-suite `hs-source-dirs: src/test, src/exe/query-api` **and** `other-modules: Query.Views, Test.Pg, UnitTests.Views` + `build-depends: file-embed, persistent, persistent-postgresql`. The extra source dir is load-bearing: a test-suite cannot import an executable's `other-modules`, and synthesis §1.3 fixes the module at `src/exe/query-api/Query/Views.hs`.)
- Modify: `.github/workflows/ci.yml` (add a `postgres:17-alpine` service to the `haskell` job)

**Interfaces:**
- Consumes: `PG_CONN_STR`; roles `palmares_owner`, `palmares_ro` (synthesis §3 step 14).
- Produces: `applyPalmaresSchema :: ConnectionPool -> IO ()` · `applySchemaSql :: [(FilePath, ByteString)] -> SqlPersistT IO ()` · `palmaresSqlFiles :: [(FilePath, ByteString)]` · `Test.Pg.withScratchDb :: (ConnectionPool -> IO a) -> IO a` · `Test.Pg.runPg :: ConnectionPool -> SqlPersistT IO a -> IO a` · `test/fixtures/dbsync-skeleton.sql` · `PALMARES_TEST_PG_CONN_STR` **(OPEN: the notes name no test-database env var; this name is mine. Owner/CI decides, and `ci.yml` must set it on the `haskell` job's test step.)**

- [ ] **Step 1: Write the failing test**

```haskell
-- src/test/UnitTests/Views.hs
module UnitTests.Views (tests) where

import Data.Text (Text)
import Database.Persist.Sql (Single (..), rawSql)
import Query.Views (applyPalmaresSchema, applySchemaSql, palmaresSqlFiles)
import Test.Pg (withScratchDb, runPg)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "palmares schema bootstrap"
    [ testCase "applies, and applies again (DROP SCHEMA CASCADE makes it idempotent)" $
        withScratchDb $ \pool -> do
          applyPalmaresSchema pool
          applyPalmaresSchema pool
          [Single (n :: Int)] <-
            runPg pool $ rawSql "SELECT count(*)::int FROM pg_namespace WHERE nspname = 'palmares'" []
          n @?= 1
    , testCase "every file in palmaresSqlFiles is applied, in lexical order" $
        map fst palmaresSqlFiles
          @?= ["000_schema.sql", "010_gov_views.sql", "020_views.sql", "030_metrics.sql"]
    , testCase "a failing statement rolls the whole set back — the previous schema survives" $
        withScratchDb $ \pool -> do
          applyPalmaresSchema pool
          [Single (before :: Int)] <- runPg pool countPalmaresViews
          r <- try @SomeException $ runPg pool (applySchemaSql [("999_bad.sql", "DROP SCHEMA palmares CASCADE; SELECT 1/0;")])
          assertBool "the bad set must fail" (isLeft r)
          [Single (after :: Int)] <- runPg pool countPalmaresViews
          after @?= before
    ]
  where
    countPalmaresViews :: SqlPersistT IO [Single Int]
    countPalmaresViews =
      rawSql "SELECT count(*)::int FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace WHERE n.nspname = 'palmares'" []
```

- [ ] **Step 2: Run test to verify it fails**

Run: `PALMARES_TEST_PG_CONN_STR="host=localhost user=postgres password=devonly dbname=postgres port=5432" cabal test test --test-show-details=direct --test-option='--pattern=palmares schema bootstrap'`
Expected: FAIL with `Could not find module 'Query.Views'`

- [ ] **Step 3: Implement**

`sql/000_schema.sql` is exactly two statements — `DROP SCHEMA IF EXISTS palmares CASCADE;` then `CREATE SCHEMA palmares;` — and **must not** mention `palmares_attest` (attest-sync creates that one before `runMigrations`, or `persistent` migrates into db-sync's `public`).

`sql/010_gov_views.sql`, `sql/020_views.sql` and `sql/030_metrics.sql` are created here holding one header comment each and no statements (`-- palmares views over db-sync's public schema; objects appended by later tasks.`). They must exist now because `palmaresSqlFiles` embeds all four by name at compile time; an absent file is a build error, not a runtime one.

`Query/Views.hs` — `palmaresSqlFiles` is one `makeRelativeToProject … >>= embedFile` per file, listed explicitly in lexical order (never `embedDir`); `applySchemaSql` issues each file's bytes with `rawExecute` inside the caller's transaction; `applyPalmaresSchema pool = either (\e -> die ("palmares schema DDL failed: " <> show (e :: SomeException))) pure =<< try (runSqlPool (applySchemaSql palmaresSqlFiles) pool)`. One `runSqlPool` call ⇒ one transaction ⇒ the rollback test passes for free.

`test/fixtures/dbsync-skeleton.sql` creates, in `public`, the db-sync enums (`anchortype`, `govactiontype`, `voterrole`, `vote`, `rewardtype`) and empty stand-ins for exactly the tables our views read, with the columns the notes' column map names: `block`, `tx`, `meta`, `schema_version`, `drep_hash`, `drep_registration`, `drep_distr`, `delegation_vote`, `stake_address`, `stake_registration`, `stake_deregistration`, `gov_action_proposal`, `voting_procedure`, `voting_anchor`, `off_chain_vote_data`, `off_chain_vote_gov_action_data`, `off_chain_vote_drep_data`, `off_chain_vote_author`, `off_chain_vote_reference`, `off_chain_vote_external_update`, `off_chain_vote_fetch_error`, `committee`, `committee_hash`, `committee_member`, `committee_registration`, `committee_de_registration`, `epoch_state`, `constitution`, `treasury_withdrawal`, `reward_rest`, `epoch_param`, `param_proposal`, `pool_hash`, `pool_update`, `pool_retire`. `ponytail:` comment on the file naming its ceiling: it is a *shape* fixture, and `sql/checks/*.sql` against the real preprod db-sync is what catches skeleton drift.

`Test/Pg.hs` — `withScratchDb` creates `palmares_test_<pid>`, connects, applies the skeleton, hands over a pool, drops the database in `finally`; `runPg = flip runSqlPool`. The `CREATE DATABASE` / `DROP DATABASE` pair runs through **`runSqlPoolNoTransaction … Nothing`** on a pool against the admin database — PostgreSQL forbids both inside a transaction block, and `runSqlPool` opens one. It `die`s with the env var name when `PALMARES_TEST_PG_CONN_STR` is unset so a skipped DB test can never read as a pass.

`ci.yml`, in the `haskell` job, as a job-level `services:` key (sibling of `steps:`):

```yaml
    services:
      postgres:
        image: postgres:17-alpine
        env: { POSTGRES_PASSWORD: devonly }
        ports: ["5432:5432"]
        options: >-
          --health-cmd "pg_isready -U postgres" --health-interval 10s
          --health-timeout 5s --health-retries 5
```
and `PALMARES_TEST_PG_CONN_STR: host=localhost user=postgres password=devonly dbname=postgres port=5432` on the `cabal test all --test-show-details=direct` step that T-INFRA §5.1 already defines.

- [ ] **Step 4: Run test to verify it passes**

Run: `PALMARES_TEST_PG_CONN_STR="host=localhost user=postgres password=devonly dbname=postgres port=5432" cabal test test --test-show-details=direct --test-option='--pattern=palmares schema bootstrap'`
Expected: PASS (3 tests)

- [ ] **Step 5: Commit**

```bash
git add sql/000_schema.sql sql/010_gov_views.sql sql/020_views.sql sql/030_metrics.sql src/exe/query-api/Query/Views.hs test/fixtures/dbsync-skeleton.sql src/test/Test/Pg.hs src/test/UnitTests/Views.hs palmares.cabal .github/workflows/ci.yml
git commit -m "feat(sql): fail-closed palmares schema bootstrap in one transaction"
```

---

### Task T-SQL-METRICS-2: `palmares.anchor` — the single verdict `CASE` (D-10) and its mandatory golden test

**Files:**
- Modify: `sql/010_gov_views.sql` (the T-1 stub; `palmares.anchor` is its first object)
- Test: `src/test/UnitTests/Anchor.hs`
- Modify: `palmares.cabal` (test-suite `other-modules: UnitTests.Anchor`)

**Interfaces:**
- Consumes: `applyPalmaresSchema`, `Test.Pg.withScratchDb`, `test/fixtures/dbsync-skeleton.sql`.
- Produces: `palmares.anchor (voting_anchor_id, url, data_hash, anchor_type, verdict, cip100_valid, language, comment, warning, json, fetch_error, fetch_time, retry_count)`; `verdict ∈ valid | invalid_schema | hash_mismatch | oversized | unreachable | inline | not_fetched`.

- [ ] **Step 1: Write the failing test**

```haskell
-- src/test/UnitTests/Anchor.hs — D-10: pins the exact upstream `show OffChainFetchError` strings.
tests :: TestTree
tests = testCase "anchor verdicts, seeded rows" $ withScratchDb $ \pool -> do
  applyPalmaresSchema pool
  runPg pool $ rawExecute seedRows []
  verdicts <- runPg pool $ rawSql "SELECT url, verdict, cip100_valid FROM palmares.anchor ORDER BY voting_anchor_id" []
  map (\(Single u, Single v, Single c) -> (u, v, c)) verdicts
    @?= [ ("https://ok.example/a.jsonld",        "valid",          Just True)
        , ("https://schema.example/b.jsonld",    "invalid_schema", Just False)
        , ("https://notjson.example/c.txt",      "invalid_schema", Nothing)     -- is_valid IS NULL
        , ("https://mismatch.example/d.jsonld",  "hash_mismatch",  Nothing)
        , ("https://big.example/e.jsonld",       "oversized",      Nothing)
        , ("ftp://nope.example/f.jsonld",        "unreachable",    Nothing)     -- default arm, never a false accusation
        , ("data:text/plain;utf8,Approve parameter change action on preprod testnet", "inline", Nothing)
        , ("https://queued.example/h.jsonld",    "not_fetched",    Nothing)
        ]

seedRows :: Text
seedRows =
  "INSERT INTO public.voting_anchor (id, block_id, data_hash, url, type) VALUES \
  \ (1,1,'\\x01','https://ok.example/a.jsonld','vote'), \
  \ (2,1,'\\x02','https://schema.example/b.jsonld','vote'), \
  \ (3,1,'\\x03','https://notjson.example/c.txt','vote'), \
  \ (4,1,'\\x04','https://mismatch.example/d.jsonld','vote'), \
  \ (5,1,'\\x05','https://big.example/e.jsonld','vote'), \
  \ (6,1,'\\x06','ftp://nope.example/f.jsonld','vote'), \
  \ (7,1,'\\x07','data:text/plain;utf8,Approve parameter change action on preprod testnet','vote'), \
  \ (8,1,'\\x08','https://queued.example/h.jsonld','vote'); \
  \ INSERT INTO public.off_chain_vote_data (id, voting_anchor_id, hash, language, is_valid) VALUES \
  \ (1,1,'\\x01','en',TRUE), (2,2,'\\x02','en',FALSE), (3,3,'\\x03',NULL,NULL); \
  \ INSERT INTO public.off_chain_vote_fetch_error (id, voting_anchor_id, fetch_error, fetch_time, retry_count) VALUES \
  \ (1,4,'Hash mismatch when fetching metadata from https://mismatch.example/d.jsonld. Expected \"ab\" but got \"cd\".','2026-10-19 00:00:00',0), \
  \ (2,5,'Error Offchain Voting Anchor: Size error, fetching metadata from https://big.example/e.jsonld exceeded 512 bytes.','2026-10-19 00:00:00',0), \
  \ (3,6,'Error Offchain Voting Anchor: URL parse error for ftp://nope.example/f.jsonld','2026-10-19 00:00:00',2), \
  \ (4,7,'Error Offchain Voting Anchor: URL parse error for data:text/plain;utf8,Approve parameter change action on preprod testnet','2026-10-19 00:00:00',1);"
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test test --test-show-details=direct --test-option='--pattern=anchor verdicts'`
Expected: FAIL with `relation "palmares.anchor" does not exist`

- [ ] **Step 3: Implement `palmares.anchor` as the first object in `sql/010_gov_views.sql`**

T-DATA §6.3's `v_anchor` verbatim, with four changes and nothing else: the canonical name; the canonical verdict
strings (`valid_hash_bad_schema` and `valid_hash_not_json` both collapse to `invalid_schema`, `pending` becomes
`not_fetched`); `d.is_valid AS cip100_valid` as its own column (synthesis §1.4); and the arm order below, in which
`inline` is tested **first** because a `data:` anchor also carries a URL-parse `off_chain_vote_fetch_error` row.

```sql
CASE
  WHEN va.url LIKE 'data:%'                                        THEN 'inline'
  WHEN d.id IS NOT NULL AND d.is_valid IS TRUE                     THEN 'valid'
  WHEN d.id IS NOT NULL                                            THEN 'invalid_schema'
  WHEN e.fetch_error LIKE 'Hash mismatch%'                         THEN 'hash_mismatch'
  WHEN e.fetch_error LIKE '%Size error, fetching metadata from%'    THEN 'oversized'
  WHEN e.id IS NOT NULL                                            THEN 'unreachable'
  ELSE                                                                  'not_fetched'
END AS verdict
```

`LEFT JOIN LATERAL (… ORDER BY x.retry_count DESC LIMIT 1) e ON true` picks the newest failure. Every read of
`off_chain_vote_{author,reference,external_update}` in this file carries `DISTINCT` with a `ponytail:` comment
naming PR #2137 as the version that removes it.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test test --test-show-details=direct --test-option='--pattern=anchor verdicts'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add sql/010_gov_views.sql src/test/UnitTests/Anchor.hs palmares.cabal
git commit -m "feat(sql): palmares.anchor single verdict CASE with golden test on upstream fetch_error strings"
```

---

### Task T-SQL-METRICS-3: DRep identity, certificate log, registration intervals, sync state

**Files:**
- Modify: `sql/010_gov_views.sql` (append after `palmares.anchor`)
- Test: `sql/checks/010_drep.sql`
- Create: `scripts/verify_views.sh`

**Interfaces:**
- Consumes: `palmares.anchor`.
- Produces: `palmares.sync_state (block_no, slot_no, epoch_no, tip_hash, tip_time, lag_seconds, drep_distr_epoch, dbsync_schema_stage_two, network_name)` · `palmares.drep_cert (drep_hash_id, cert_kind, tx_hash, slot_no, epoch_no, cert_time, deposit, voting_anchor_id)` · `palmares.drep_interval (drep_hash_id, from_epoch, from_slot, from_time, to_epoch)` · `palmares.drep (drep_hash_id, drep_raw, has_script, registration_status, is_active, active_until_epoch, voting_anchor_id, first_cert_slot, first_cert_time, last_cert_slot, last_cert_time, given_name, image_url, language)`

- [ ] **Step 1: Write the failing test**

A measured assertion against the caught-up preprod db-sync — the row counts are the point, and no seeded fixture
can produce them.

```sql
-- sql/checks/010_drep.sql — preprod, tip epoch >= 320. Figures: T-DATA §7.1, T-METRIC §4.
DO $$ BEGIN
  ASSERT (SELECT count(*) FROM public.drep_hash)                                     = 802,  'drep_hash rows';
  ASSERT (SELECT count(*) FROM palmares.drep WHERE registration_status = 'registered') = 294, 'registered DReps';
  ASSERT (SELECT count(*) FROM palmares.drep WHERE drep_raw IS NULL)                 = 0,    'pseudo-DReps are not DReps (3.7)';
  -- the fixture DRep: registered in epoch 173, one open interval, no retirement
  ASSERT (SELECT count(*) FROM palmares.drep_interval i JOIN public.drep_hash h ON h.id = i.drep_hash_id
          WHERE h.raw = decode('81378d3ea89b68f0fc09944b1f93f2feb851318f196f98ba4029d8a8','hex')) = 1;
  ASSERT (SELECT from_epoch FROM palmares.drep_interval i JOIN public.drep_hash h ON h.id = i.drep_hash_id
          WHERE h.raw = decode('81378d3ea89b68f0fc09944b1f93f2feb851318f196f98ba4029d8a8','hex')) = 173;
  ASSERT (SELECT to_epoch IS NULL FROM palmares.drep_interval i JOIN public.drep_hash h ON h.id = i.drep_hash_id
          WHERE h.raw = decode('81378d3ea89b68f0fc09944b1f93f2feb851318f196f98ba4029d8a8','hex'));
  ASSERT (SELECT from_time FROM palmares.drep_interval i JOIN public.drep_hash h ON h.id = i.drep_hash_id
          WHERE h.raw = decode('81378d3ea89b68f0fc09944b1f93f2feb851318f196f98ba4029d8a8','hex'))
         = '2024-10-16 20:38:10'::timestamp;
  -- drep_cert carries all three kinds; the fixture DRep has register + update
  ASSERT (SELECT count(DISTINCT cert_kind) FROM palmares.drep_cert) = 3, 'register/update/retire';
  -- sync_state is exactly one row and guards the network
  ASSERT (SELECT count(*) FROM palmares.sync_state) = 1;
  ASSERT (SELECT network_name FROM palmares.sync_state) = 'preprod';
  ASSERT (SELECT drep_distr_epoch >= epoch_no - 1 FROM palmares.sync_state), 'ledger: enable took';
END $$;
```

- [ ] **Step 2: Run test to verify it fails**

Run: `psql "$PG_CONN_STR" -v ON_ERROR_STOP=1 -f sql/checks/010_drep.sql`
Expected: FAIL with `ERROR: relation "palmares.drep" does not exist` (exit status 3)

- [ ] **Step 3: Implement the four views in `sql/010_gov_views.sql`, in this order: `sync_state`, `drep_cert`, `drep_interval`, `drep`**

- `sync_state`: T-DATA §5.1 verbatim under the canonical name — `ORDER BY b.id DESC LIMIT 1`, and
  `EXTRACT(EPOCH FROM (now() AT TIME ZONE 'UTC' - b.time))::bigint AS lag_seconds` (`block.time` has no zone).
- `drep_cert`: `drep_registration ⋈ tx ⋈ block`, `cert_kind = CASE WHEN deposit > 0 THEN 'register' WHEN deposit < 0 THEN 'retire' ELSE 'update' END`.
- `drep_interval`: T-METRIC §0.3 (2) verbatim, **including `from_slot`** (T-METRIC §5.3's correction — §5.3's
  retire lookup is bounded by `slot_no`, not `epoch_no`).
- `drep`: one row per `drep_hash.raw IS NOT NULL`; `registration_status` derived from the **latest** `drep_cert`
  row (`retire` ⇒ `retired`, else `registered`), **never** from the existence of a `drep_hash` row; `is_active` =
  `drep_distr.active_until >= (SELECT epoch_no FROM palmares.sync_state)`; the CIP-119 display fields joined
  through `palmares.anchor` → `off_chain_vote_drep_data`. No bech32 in SQL: expose `(drep_raw, has_script)` and
  let `Palmares.DomainTypes.Core.Ids` encode.

`scripts/verify_views.sh` runs every `sql/checks/*.sql` in lexical order with `psql -v ON_ERROR_STOP=1 -f` and
exits non-zero on the first failed assertion. **Never `--single-transaction`**: `070_drep_metrics.sql` ends with a
bare `REFRESH … CONCURRENTLY`, which cannot run inside a transaction block.

- [ ] **Step 4: Run test to verify it passes**

Run: `psql "$PG_CONN_STR" -v ON_ERROR_STOP=1 -f sql/checks/010_drep.sql`
Expected: PASS — `DO` printed once, exit status 0

Run (the CI half — the appended DDL must still apply against the seeded skeleton): `cabal test test --test-show-details=direct --test-option='--pattern=palmares schema bootstrap'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add sql/010_gov_views.sql sql/checks/010_drep.sql scripts/verify_views.sh
git commit -m "feat(sql): drep, drep_cert, drep_interval and sync_state views"
```

---

### Task T-SQL-METRICS-4: `palmares.action_window` and the `palmares.eligible_actions` SQL function (D-2, D-15)

**Files:**
- Modify: `sql/010_gov_views.sql` (append), `sql/020_views.sql` (the T-1 stub; `palmares.eligible_actions` is its first object)
- Test: `sql/checks/020_eligibility.sql`

**Interfaces:**
- Consumes: `palmares.drep_interval`.
- Produces: `palmares.action_window (gov_action_id, action_type, proposed_epoch, proposed_slot, proposed_at, proposed_proto_major, expiration, last_votable_epoch, drep_votable, spo_votable, param_proposal)` · `palmares.eligible_actions(drep_hash_id bigint, as_of_epoch int) RETURNS TABLE (gov_action_id bigint, action_type public.govactiontype)`

- [ ] **Step 1: Write the failing test**

```sql
-- sql/checks/020_eligibility.sql — preprod @ epoch 320. Figures: T-METRIC §4, §3.4, §10.
DO $$
DECLARE d bigint;
BEGIN
  ASSERT (SELECT count(*) FROM palmares.action_window)                                      = 131, 'all proposals';
  ASSERT (SELECT count(*) FROM palmares.action_window WHERE last_votable_epoch < 320)       = 119, 'settled';
  ASSERT (SELECT count(*) FROM palmares.action_window WHERE last_votable_epoch >= 320)      = 12,  'still open';
  ASSERT (SELECT count(*) FROM palmares.action_window WHERE last_votable_epoch < 320 AND drep_votable) = 115,
         'settled and DRep-votable: the 4 excluded are the bootstrap-era 3 ParameterChange + 1 HardForkInitiation';
  -- govActionLifetime is 7 on preprod, on all 131 rows (network-specific: mainnet is 6)
  ASSERT (SELECT count(*) FROM palmares.action_window WHERE expiration - proposed_epoch <> 7) = 0;
  -- the bootstrap window is epochs 163..180 and holds exactly 6 proposals of the 3 permitted types
  ASSERT (SELECT count(*) FROM palmares.action_window WHERE proposed_epoch BETWEEN 163 AND 180) = 6;
  ASSERT (SELECT count(*) FROM palmares.action_window
          WHERE proposed_epoch BETWEEN 163 AND 180 AND action_type = 'ParameterChange')     = 3;
  ASSERT (SELECT count(*) FROM palmares.action_window
          WHERE proposed_epoch BETWEEN 163 AND 180 AND action_type = 'InfoAction')          = 2;
  ASSERT (SELECT count(*) FROM palmares.action_window
          WHERE proposed_epoch BETWEEN 163 AND 180 AND action_type = 'HardForkInitiation')   = 1;
  -- D-15: the function, not a copy-pasted CTE. 114 = 115 settled-and-votable minus 1 closing before epoch 173.
  SELECT id INTO d FROM public.drep_hash
    WHERE raw = decode('81378d3ea89b68f0fc09944b1f93f2feb851318f196f98ba4029d8a8','hex');
  ASSERT (SELECT count(*) FROM palmares.eligible_actions(d, 320)) = 114, 'fixture DRep eligible_actions';
  ASSERT (SELECT count(*) FROM palmares.eligible_actions(d, 320) WHERE action_type = 'TreasuryWithdrawals') = 79;
  ASSERT (SELECT count(*) FROM palmares.eligible_actions(d, 320) WHERE action_type = 'InfoAction')          = 13;
  ASSERT (SELECT count(*) FROM palmares.eligible_actions(d, 320) WHERE action_type = 'ParameterChange')     = 11;
  ASSERT (SELECT count(*) FROM palmares.eligible_actions(d, 320) WHERE action_type = 'NewCommittee')        = 7;
  ASSERT (SELECT count(*) FROM palmares.eligible_actions(d, 320) WHERE action_type = 'HardForkInitiation')  = 2;
  ASSERT (SELECT count(*) FROM palmares.eligible_actions(d, 320) WHERE action_type = 'NoConfidence')        = 1;
  ASSERT (SELECT count(*) FROM palmares.eligible_actions(d, 320) WHERE action_type = 'NewConstitution')     = 1;
  -- the 'n/a' case, asserted so it can never pass vacuously: as of its own registration epoch the
  -- fixture DRep has nothing settled behind it, so its eligibility set is empty (T-METRIC §3.8).
  ASSERT (SELECT count(*) FROM palmares.eligible_actions(d, 173)) = 0, 'empty eligibility set';
END $$;
```

- [ ] **Step 2: Run test to verify it fails**

Run: `psql "$PG_CONN_STR" -v ON_ERROR_STOP=1 -f sql/checks/020_eligibility.sql`
Expected: FAIL with `ERROR: relation "palmares.action_window" does not exist`

- [ ] **Step 3: Implement `palmares.action_window` (in `010_gov_views.sql`) and `palmares.eligible_actions` (opening `020_views.sql`)**

`action_window` is T-METRIC §0.3 (1) verbatim: `last_votable_epoch = LEAST(expiration, COALESCE(ratified_epoch, expiration), COALESCE(expired_epoch, expiration), COALESCE(dropped_epoch - 1, expiration))` — `ratified_epoch`
un-decremented, `dropped_epoch` decremented, both empirically pinned — and `drep_votable` as the **window** test
(`action_type = 'InfoAction' OR EXISTS (SELECT 1 FROM public.epoch_param ep WHERE ep.epoch_no BETWEEN proposed_epoch AND last_votable_epoch AND ep.protocol_major <> 9)`), never `proposed_proto_major`, which stays
for display only. `spo_votable` is T-METRIC §3.3's `CASE` with all eleven `param_proposal` SecurityGroup columns.

`eligible_actions` is §5.1's `elig` CTE promoted to `LANGUAGE sql STABLE`, taking `(drep_hash_id, as_of_epoch)` and
returning `(gov_action_id, action_type)` — `GROUP BY` both columns, because a DRep may have several intervals.
Eligibility is `drep_interval` overlap plus `drep_votable` plus `last_votable_epoch < as_of_epoch` (D-2: never
`drep_distr` membership).

- [ ] **Step 4: Run test to verify it passes**

Run: `psql "$PG_CONN_STR" -v ON_ERROR_STOP=1 -f sql/checks/020_eligibility.sql`
Expected: PASS — exit status 0

Run (the CI half): `cabal test test --test-show-details=direct --test-option='--pattern=palmares schema bootstrap'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add sql/010_gov_views.sql sql/020_views.sql sql/checks/020_eligibility.sql
git commit -m "feat(sql): action_window with window-based drep_votable + eligible_actions SQL function"
```

---

### Task T-SQL-METRICS-5: `palmares.vote` (D-24), `palmares.thresholds`, `palmares.threshold_map` (D-25)

**Files:**
- Modify: `sql/010_gov_views.sql` (append)
- Test: `src/test/UnitTests/ThresholdMap.hs`, `sql/checks/030_votes.sql`
- Modify: `palmares.cabal` (test-suite `other-modules: UnitTests.ThresholdMap`)

**Interfaces:**
- Consumes: `palmares.action_window`, `palmares.anchor`, `applyPalmaresSchema`, `Test.Pg.withScratchDb`.
- Produces: `palmares.vote (gov_action_id, voter_role, voter_id_hash, voter_hot_hash, voter_cold_hash, has_script, vote, voting_anchor_id, vote_epoch, vote_slot, cast_at, vote_tx_hash, tx_index, vote_index, seq_first, seq_last, vote_rows)` · `palmares.thresholds (epoch_no, dvt_*, pvt_*, pvtpp_security_group, committee_min_size, committee_max_term_length, gov_action_lifetime, gov_action_deposit, drep_deposit, drep_activity)` · `palmares.threshold_map (action_type, voter_role, threshold_param)` **(OPEN: D-25 requires this table as a named deliverable but no note names the object; `palmares.threshold_map` is mine, and synthesis §1.2 does not enumerate it. Owner confirms the name before the API block echoes `threshold_param`.)**
- `palmares.vote_tally` consumes `palmares.drep_epoch_power`, so it is built in T-SQL-METRICS-6, not here.

- [ ] **Step 1: Write the failing test**

```haskell
-- src/test/UnitTests/ThresholdMap.hs — D-25. The whole point is `pvtpp_security_group`, which a
-- `pvt_%` prefix scan misses, and the NULLs where no threshold exists.
tests :: TestTree
tests = testCase "threshold_map is the API contract's static table" $ withScratchDb $ \pool -> do
  applyPalmaresSchema pool
  rows <- runPg pool $ rawSql "SELECT action_type::text, voter_role::text, threshold_param FROM palmares.threshold_map ORDER BY action_type::text, voter_role::text" []
  map (\(Single a, Single r, Single p) -> (a, r, p)) rows
    @?= [ ("HardForkInitiation",  "ConstitutionalCommittee", Nothing)                      -- quorum, not a column
        , ("HardForkInitiation",  "DRep", Just "dvt_hard_fork_initiation")
        , ("HardForkInitiation",  "SPO",  Just "pvt_hard_fork_initiation")
        , ("InfoAction",          "ConstitutionalCommittee", Nothing)
        , ("InfoAction",          "DRep", Nothing)                                          -- never ratifiable
        , ("InfoAction",          "SPO",  Nothing)
        , ("NewCommittee",        "ConstitutionalCommittee", Nothing)                       -- does not vote
        , ("NewCommittee",        "DRep", Just "dvt_committee_normal")                      -- _normal always, in F1
        , ("NewCommittee",        "SPO",  Just "pvt_committee_normal")
        , ("NewConstitution",     "ConstitutionalCommittee", Nothing)
        , ("NewConstitution",     "DRep", Just "dvt_update_to_constitution")
        , ("NewConstitution",     "SPO",  Nothing)
        , ("NoConfidence",        "ConstitutionalCommittee", Nothing)                       -- does not vote
        , ("NoConfidence",        "DRep", Just "dvt_motion_no_confidence")
        , ("NoConfidence",        "SPO",  Just "pvt_motion_no_confidence")
        , ("ParameterChange",     "ConstitutionalCommittee", Nothing)
        , ("ParameterChange",     "DRep", Nothing)                                          -- max over touched groups
        , ("ParameterChange",     "SPO",  Just "pvtpp_security_group")                      -- NOT pvt_pp_security_group
        , ("TreasuryWithdrawals", "ConstitutionalCommittee", Nothing)
        , ("TreasuryWithdrawals", "DRep", Just "dvt_treasury_withdrawal")
        , ("TreasuryWithdrawals", "SPO",  Nothing)
        ]
```

```sql
-- sql/checks/030_votes.sql — preprod @ epoch 320. Figures: T-METRIC §3.2, §4 network table.
DO $$ BEGIN
  ASSERT (SELECT count(*) FROM palmares.vote WHERE voter_role = 'DRep')                       = 816, 'DRep vote rows';
  ASSERT (SELECT count(DISTINCT voter_hot_hash) FROM palmares.vote WHERE voter_role = 'DRep')  = 511, 'distinct DRep voters, all time';
  ASSERT (SELECT count(*) FROM (SELECT 1 FROM palmares.vote WHERE voter_role = 'DRep'
          GROUP BY voter_hot_hash, gov_action_id HAVING count(*) > 1) x)                       = 69,  're-voted pairs';
  ASSERT (SELECT max(vote_rows) FROM palmares.vote WHERE voter_role = 'DRep')                  = 20,  'worst re-voted pair';
  ASSERT (SELECT count(*) FROM palmares.vote
          WHERE voter_role = 'DRep' AND voting_anchor_id IS NOT NULL)                          = 65,  'DRep votes carrying an anchor';
  ASSERT (SELECT count(*) FROM palmares.vote WHERE voter_role = 'SPO')                         = 90;
  ASSERT (SELECT count(DISTINCT voter_hot_hash) FROM palmares.vote WHERE voter_role = 'SPO')    = 45;
  ASSERT (SELECT count(*) FROM palmares.vote
          WHERE voter_role = 'SPO' AND voting_anchor_id IS NOT NULL)                           = 0;
  -- exactly one seq_last = 1 and one seq_first = 1 per (voter identity, action) — the tally depends on it
  ASSERT (SELECT count(*) FROM (SELECT 1 FROM palmares.vote GROUP BY voter_role, voter_id_hash, gov_action_id
          HAVING count(*) FILTER (WHERE seq_last = 1) <> 1 OR count(*) FILTER (WHERE seq_first = 1) <> 1) x) = 0;
  -- D-24: CC votes are cast by hot keys; the cold credential must resolve for every CC vote
  ASSERT (SELECT count(*) FROM palmares.vote
          WHERE voter_role = 'ConstitutionalCommittee' AND voter_cold_hash IS NULL)            = 0;
  -- D-27: the CC member identity is the COLD key. Preprod carries 648.766 CC hot keys mapping to only
  -- 43 cold keys (an AuthCommitteeHotKey stress test in epoch 187), so partitioning the windows by
  -- voter_hot_hash reports 3.243.798 final CC votes where the truth is 193.
  ASSERT (SELECT count(*) FROM palmares.vote WHERE voter_role = 'ConstitutionalCommittee')     = 3243802;
  ASSERT (SELECT count(*) FROM palmares.vote
          WHERE voter_role = 'ConstitutionalCommittee' AND seq_last = 1)                       = 193;
  ASSERT (SELECT count(DISTINCT voter_cold_hash) FROM palmares.vote
          WHERE voter_role = 'ConstitutionalCommittee')                                        = 43;
  -- gov_action_lifetime is 6, not 7. The ledger's own `expiration` is proposed_epoch + lifetime + 1
  -- (delta exactly 7 on all 131 preprod actions), which is why `last_votable_epoch` must read
  -- `gov_action_proposal.expiration` and never arithmetic over this parameter.
  ASSERT (SELECT gov_action_lifetime FROM palmares.thresholds WHERE epoch_no = 320) = 6;
END $$;
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test test --test-show-details=direct --test-option='--pattern=threshold_map'` then `psql "$PG_CONN_STR" -v ON_ERROR_STOP=1 -f sql/checks/030_votes.sql`
Expected: FAIL with `relation "palmares.threshold_map" does not exist`, then `relation "palmares.vote" does not exist`

- [ ] **Step 3: Implement the three objects, all in `010_gov_views.sql`**

`thresholds` (`epoch_param` restricted to the governance columns, **explicitly listed** —
`pvtpp_security_group` does not share the `pvt_` prefix); `threshold_map` as a plain view over a 21-row `VALUES`
list, the exact content of the test above; `vote`, which is T-METRIC §0.3 (3)'s `drep_vote` generalised to all
three roles — drop the `voter_role = 'DRep'` filter, keep `seq_first` / `seq_last` / `vote_rows` with the same
window specs partitioned by `(voter_role, voter_id_hash, gov_action_proposal_id)` — `voter_id_hash` is
`coalesce(voter_cold_hash, voter_hot_hash)` computed in an outer SELECT over the one below, i.e. the **cold**
credential for the CC and the signing credential itself for DRep/SPO (D-27: hot keys rotate, and preprod has
648.766 CC hot keys over 43 cold keys, so a hot-key partition reports 3.243.798 final CC votes instead of
193) — and resolve the voter per D-24:

```sql
-- voter_hot_hash: the credential that signed. voter_cold_hash: NULL except for the CC, where it is
-- resolved as of the vote's tx, because a member's history splits across hot-key rotations.
CASE v.voter_role
  WHEN 'DRep' THEN (SELECT dh.raw FROM public.drep_hash dh WHERE dh.id = v.drep_voter)
  WHEN 'SPO'  THEN (SELECT ph.hash_raw FROM public.pool_hash ph WHERE ph.id = v.pool_voter)
  ELSE             (SELECT ch.raw FROM public.committee_hash ch WHERE ch.id = v.committee_voter)
END AS voter_hot_hash,
cold.raw AS voter_cold_hash
...
LEFT JOIN LATERAL (
  SELECT ch.raw
  FROM public.committee_registration cr
  JOIN public.committee_hash ch ON ch.id = cr.cold_key_id
  WHERE v.voter_role = 'ConstitutionalCommittee' AND cr.hot_key_id = v.committee_voter
    AND cr.tx_id <= v.tx_id
  ORDER BY cr.tx_id DESC, cr.cert_index DESC LIMIT 1
) cold ON true
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test test --test-show-details=direct --test-option='--pattern=threshold_map'` then `psql "$PG_CONN_STR" -v ON_ERROR_STOP=1 -f sql/checks/030_votes.sql`
Expected: PASS both

- [ ] **Step 5: Commit**

```bash
git add sql/010_gov_views.sql sql/checks/030_votes.sql src/test/UnitTests/ThresholdMap.hs palmares.cabal
git commit -m "feat(sql): vote with hot+cold CC credentials and the static threshold_map"
```

---

### Task T-SQL-METRICS-6: delegation, voting power (D-20) and the tally — `vote_delegation`, `live_delegation`, `drep_epoch_delegators`, `drep_epoch_power`, `vote_tally`

**Files:**
- Modify: `sql/010_gov_views.sql` (append), `sql/020_views.sql` (append `drep_epoch_delegators`, then `vote_tally`)
- Test: `sql/checks/040_power.sql`

**Interfaces:**
- Consumes: `palmares.sync_state`, `palmares.drep`, `palmares.vote`, `palmares.action_window`, `palmares.thresholds`, `palmares.threshold_map`.
- Produces: `palmares.vote_delegation (addr_id, stake_address, drep_hash_id, target_kind, tx_hash, slot_no, epoch_no, delegated_at, cert_index)` · `palmares.live_delegation (addr_id, drep_hash_id, from_epoch, from_slot)` · `palmares.drep_epoch_delegators (drep_hash_id, epoch_no, delegator_count)` · `palmares.drep_epoch_power (drep_hash_id, drep_raw, has_script, target_kind, epoch_no, amount, active_until)`, `target_kind ∈ drep | always_abstain | always_no_confidence`, **unfiltered** · `palmares.vote_tally (gov_action_id, voter_role, epoch, yes_power, no_power, abstain_power, yes_count, no_count, abstain_count, ratio, threshold, threshold_param, basis)`

- [ ] **Step 1: Write the failing test**

```sql
-- sql/checks/040_power.sql — D-20. Figures: T-DATA §7.1, T-METRIC §3.7, §3.9, §4.
DO $$
DECLARE d bigint;
BEGIN
  -- one view, no filter: epoch 319 has 195 rows = 193 real DReps + the 2 pseudo-DReps
  ASSERT (SELECT count(*) FROM palmares.drep_epoch_power WHERE epoch_no = 319)                        = 195;
  ASSERT (SELECT count(*) FROM palmares.drep_epoch_power WHERE epoch_no = 319 AND target_kind = 'drep') = 193;
  ASSERT (SELECT amount FROM palmares.drep_epoch_power
          WHERE epoch_no = 319 AND target_kind = 'always_abstain')        = 431863790957959;
  ASSERT (SELECT amount FROM palmares.drep_epoch_power
          WHERE epoch_no = 319 AND target_kind = 'always_no_confidence')  = 325390800383;
  ASSERT (SELECT sum(amount) FROM palmares.drep_epoch_power WHERE epoch_no = 319) = 937803900128796,
         'R4 fails by 46% if the pseudo-DReps are filtered out of the sum';
  -- 101 of 294 registered DReps have no drep_distr row at epoch 319 (and that means zero, not unknown)
  ASSERT (SELECT count(*) FROM palmares.drep dr WHERE dr.registration_status = 'registered'
          AND NOT EXISTS (SELECT 1 FROM palmares.drep_epoch_power p
                          WHERE p.drep_hash_id = dr.drep_hash_id AND p.epoch_no = 319))               = 101;
  -- the fixture DRep: 147 unbroken rows for epochs 174..320, none for its registration epoch 173
  SELECT id INTO d FROM public.drep_hash
    WHERE raw = decode('81378d3ea89b68f0fc09944b1f93f2feb851318f196f98ba4029d8a8','hex');
  ASSERT (SELECT count(*) FROM palmares.drep_epoch_power WHERE drep_hash_id = d)                      = 147;
  ASSERT (SELECT min(epoch_no) FROM palmares.drep_epoch_power WHERE drep_hash_id = d)                 = 174;
  ASSERT (SELECT amount FROM palmares.drep_epoch_power WHERE drep_hash_id = d AND epoch_no = 320)     = 3923709956713;
  ASSERT (SELECT active_until FROM palmares.drep_epoch_power WHERE drep_hash_id = d AND epoch_no = 320) = 333;
  -- delegator counts: live = 13, and the per-epoch view agrees with the live view at the tip epoch
  ASSERT (SELECT count(*) FROM palmares.live_delegation WHERE drep_hash_id = d)                       = 13;
  ASSERT (SELECT delegator_count FROM palmares.drep_epoch_delegators
          WHERE drep_hash_id = d AND epoch_no = (SELECT epoch_no FROM palmares.sync_state))          = 13;
  -- a de-registered stake credential must leave live_delegation even though delegation_vote keeps the row
  ASSERT (SELECT count(*) FROM palmares.live_delegation l
          WHERE EXISTS (SELECT 1 FROM public.stake_deregistration sd
                        WHERE sd.addr_id = l.addr_id AND sd.epoch_no >= l.from_epoch))               = 0;
  ASSERT (SELECT count(DISTINCT target_kind) FROM palmares.vote_delegation)                           = 3;
  -- the tally is over final votes only, and carries one basis
  ASSERT (SELECT sum(yes_count + no_count + abstain_count) FROM palmares.vote_tally WHERE voter_role = 'DRep')
         = (SELECT count(*) FROM palmares.vote WHERE voter_role = 'DRep' AND seq_last = 1);
  ASSERT (SELECT count(DISTINCT basis) FROM palmares.vote_tally) = 1;
  ASSERT (SELECT min(basis) FROM palmares.vote_tally) = 'cast_votes_only';
  -- D-25: every non-null threshold_param is a real column of palmares.thresholds — this is what
  -- catches the 'pvt_pp_security_group' misspelling, without depending on which actions got SPO votes
  ASSERT (SELECT count(*) FROM palmares.vote_tally t WHERE t.threshold_param IS NOT NULL
          AND NOT EXISTS (SELECT 1 FROM information_schema.columns c
                          WHERE c.table_schema = 'palmares' AND c.table_name = 'thresholds'
                            AND c.column_name = t.threshold_param))                                  = 0;
END $$;
```

- [ ] **Step 2: Run test to verify it fails**

Run: `psql "$PG_CONN_STR" -v ON_ERROR_STOP=1 -f sql/checks/040_power.sql`
Expected: FAIL with `ERROR: relation "palmares.drep_epoch_power" does not exist`

- [ ] **Step 3: Implement the four views, then `vote_tally`**

- `drep_epoch_power` = `drep_distr ⋈ drep_hash`, **no `WHERE`**, with
  `target_kind = CASE WHEN dh.raw IS NOT NULL THEN 'drep' WHEN dh.view = 'drep_always_abstain' THEN 'always_abstain' ELSE 'always_no_confidence' END`. Consumers filter; the dashboard reads the two pseudo rows off this same view.
- `vote_delegation` = `delegation_vote ⋈ tx ⋈ block ⋈ stake_address`, same `target_kind`.
- `live_delegation` = T-METRIC §5.4's `vd` + `live` CTEs restricted to the current epoch: latest `delegation_vote`
  per `addr_id`, minus any `stake_deregistration` at or after that delegation's epoch.
- `drep_epoch_delegators` = the same `live` intervals, cross-joined with
  `generate_series((SELECT min(epoch_no) FROM public.drep_distr), (SELECT epoch_no FROM palmares.sync_state))`
  and counted per `(drep_hash_id, epoch_no)`. `ponytail:` comment naming the ceiling from the synthesis cut list:
  promote to a matview only if `/drep/{id}` or `/drep/{id}/power` p95 exceeds 500 ms on preprod.

Then, last in `020_views.sql`: `vote_tally`, **one row per `(gov_action_id, voter_role)`** over
`palmares.vote WHERE seq_last = 1` — the six counters are `count(*) FILTER (WHERE vote = 'Yes'|'No'|'Abstain')`
and the matching `sum(power) FILTER (…)`, which is what T-API's `VoteTally` shape requires (T-DATA's
`(…, vote)` grouping is an alias to delete) — `ratio = yes_power / NULLIF(yes_power + no_power, 0)` (abstain excluded),
`threshold` / `threshold_param` joined through `threshold_map ⋈ thresholds` at the action's
`last_votable_epoch`, and the literal `basis = 'cast_votes_only'`. The three power columns per role:
**DRep** = `palmares.drep_epoch_power WHERE target_kind = 'drep'` at `last_votable_epoch`;
**CC** = `0` (T-API §2.6: "lovelace; 0 for the CC"); **SPO** = `0`
**(OPEN: T-API §2.6 names `pool_stat.voting_power` for SPO power, but no note verifies that `pool_stat` is
populated under `preset: only_governance` + `shelley: enable: true`. Until the caught-up sync answers it
(`SELECT count(*) FROM pool_stat`), SPO rows publish counts with `yes_power = no_power = abstain_power = 0`;
if the table is populated, the API block decides whether to fill them.)**
`ParameterChange`/DRep's `threshold` stays `NULL` in F1: it is a max over the touched parameter groups, which
needs a second static mapping nobody has asked for yet.

- [ ] **Step 4: Run test to verify it passes**

Run: `psql "$PG_CONN_STR" -v ON_ERROR_STOP=1 -f sql/checks/040_power.sql`
Expected: PASS — exit status 0

Run (the CI half): `cabal test test --test-show-details=direct --test-option='--pattern=palmares schema bootstrap'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add sql/010_gov_views.sql sql/020_views.sql sql/checks/040_power.sql
git commit -m "feat(sql): unfiltered drep_epoch_power with target_kind, delegator views and the cast-votes-only tally"
```

---

### Task T-SQL-METRICS-7: `palmares.gov_action`, `palmares.committee_member`, `palmares.treasury_withdrawal`

**Files:**
- Modify: `sql/020_views.sql` (append)
- Test: `sql/checks/050_actions_committee_treasury.sql`

**Interfaces:**
- Consumes: `palmares.action_window`, `palmares.anchor`, `palmares.vote`.
- Produces: `palmares.gov_action (gov_action_id, tx_hash, index, action_type, title, abstract, proposed_epoch, proposed_slot, proposed_at, expiration, last_votable_epoch, state, ratified_epoch, enacted_epoch, expired_epoch, dropped_epoch, deposit, return_address, prev_gov_action_proposal, withdrawal_total, votes_cast, voting_anchor_id)` · `palmares.committee_member (committee_hash_id, cold_hash, cold_has_script, hot_hash, hot_has_script, status, expiration_epoch, committee_id, is_enacted, quorum_numerator, quorum_denominator)` · `palmares.treasury_withdrawal (gov_action_id, stake_address_id, stake_address, amount, enacted_epoch, paid_heuristic)`

- [ ] **Step 1: Write the failing test**

```sql
-- sql/checks/050_actions_committee_treasury.sql — preprod @ epoch 320.
-- Figures: T-DATA §7.1 (CC 2-of-3, members expiring 372, all script creds), T-METRIC §4 lifecycle row.
DO $$ BEGIN
  ASSERT (SELECT count(*) FROM palmares.gov_action)                                      = 131;
  ASSERT (SELECT count(*) FROM palmares.gov_action WHERE state = 'enacted')              = 11;
  ASSERT (SELECT count(*) FROM palmares.gov_action WHERE ratified_epoch IS NOT NULL)     = 11;
  ASSERT (SELECT count(*) FROM palmares.gov_action WHERE expired_epoch IS NOT NULL)      = 103;
  ASSERT (SELECT count(*) FROM palmares.gov_action WHERE dropped_epoch IS NOT NULL)      = 108;
  ASSERT (SELECT count(*) FROM palmares.gov_action
          WHERE dropped_epoch IS NOT NULL AND expired_epoch IS NULL AND ratified_epoch IS NULL) = 6, 'dropped-only';
  -- 131 - 11 enacted - 103 expired - 6 dropped-only = 11, NOT the 12 "still open" of T-METRIC §4:
  -- one action carries expired_epoch = 320, so its state is 'expired' while its window is still open.
  ASSERT (SELECT count(*) FROM palmares.gov_action WHERE state = 'proposed')             = 11;
  ASSERT (SELECT count(*) FROM palmares.gov_action WHERE last_votable_epoch >= 320)      = 12, 'still votable';
  ASSERT (SELECT count(*) FROM palmares.gov_action WHERE state = 'ratified')             = 0, 'all 11 ratified are enacted';
  -- votes_cast is final votes, so it can never exceed the raw vote rows
  ASSERT (SELECT count(*) FROM palmares.gov_action g
          WHERE g.votes_cast <> (SELECT count(*) FROM palmares.vote v
                                 WHERE v.gov_action_id = g.gov_action_id AND v.seq_last = 1)) = 0;
  -- committee: 2-of-3 quorum, 3 members, all script credentials, expiring epoch 372, no duplicate rows (#2155)
  ASSERT (SELECT count(*) FROM palmares.committee_member WHERE is_enacted)               = 3;
  ASSERT (SELECT count(DISTINCT cold_hash) FROM palmares.committee_member WHERE is_enacted) = 3;
  ASSERT (SELECT count(*) FROM palmares.committee_member WHERE is_enacted AND NOT cold_has_script) = 0;
  ASSERT (SELECT count(*) FROM palmares.committee_member WHERE is_enacted AND expiration_epoch <> 372) = 0;
  ASSERT (SELECT DISTINCT quorum_numerator || '/' || quorum_denominator
          FROM palmares.committee_member WHERE is_enacted)                               = '2/3';
  -- every CC voter resolves to a member (this is what makes the hot->cold join in palmares.vote checkable)
  ASSERT (SELECT count(*) FROM palmares.vote v WHERE v.voter_role = 'ConstitutionalCommittee'
          AND NOT EXISTS (SELECT 1 FROM palmares.committee_member m WHERE m.cold_hash = v.voter_cold_hash)) = 0;
  -- treasury: the per-action total matches gov_action.withdrawal_total
  ASSERT (SELECT count(*) FROM palmares.gov_action g WHERE g.action_type = 'TreasuryWithdrawals'
          AND coalesce(g.withdrawal_total, 0) <> (SELECT coalesce(sum(amount), 0) FROM palmares.treasury_withdrawal t
                                                  WHERE t.gov_action_id = g.gov_action_id)) = 0;
END $$;
```

- [ ] **Step 2: Run test to verify it fails**

Run: `psql "$PG_CONN_STR" -v ON_ERROR_STOP=1 -f sql/checks/050_actions_committee_treasury.sql`
Expected: FAIL with `ERROR: relation "palmares.gov_action" does not exist`

- [ ] **Step 3: Implement the three views**

- `gov_action` = `action_window ⋈ gov_action_proposal ⋈ tx ⋈ block ⋈ palmares.anchor ⋈ off_chain_vote_gov_action_data`, with `state` exactly the published `gov_action_state_v1` chain
  (`enacted_epoch → enacted; else expired_epoch → expired; else dropped_epoch → dropped; else ratified_epoch → ratified; else proposed`), `withdrawal_total` summed over **`public.treasury_withdrawal`** grouped by
  `gov_action_proposal_id` — *not* over `palmares.treasury_withdrawal`, which reads `palmares.gov_action` and
  would make the pair a view cycle PostgreSQL rejects — and `votes_cast` counted over
  `palmares.vote WHERE seq_last = 1`.
- `committee_member` = `committee_member ⋈ committee_hash ⋈ committee`, `is_enacted` from the **max-`id`** row per
  `epoch_state.epoch_no` (`DISTINCT`/max picks one row: issue #2155 can leave duplicates on 13.7.1.0), hot key
  resolved through `committee_registration` and `status` demoted to `resigned` by `committee_de_registration`. One
  `ponytail:` comment stating that this view subsumes spec §6's `committee_vote_auth` — **no extra view**.
- `treasury_withdrawal` = `treasury_withdrawal ⋈ stake_address ⋈ gov_action`, LEFT JOIN `reward_rest`
  (`type = 'treasury'`) on `(stake_address_id, spendable_epoch)`; that column is named **`paid_heuristic`** because
  `reward_rest` has no gov-action key and the match is ambiguous when one address receives two withdrawals in one
  epoch.

- [ ] **Step 4: Run test to verify it passes**

Run: `psql "$PG_CONN_STR" -v ON_ERROR_STOP=1 -f sql/checks/050_actions_committee_treasury.sql`
Expected: PASS — exit status 0

Run (the CI half): `cabal test test --test-show-details=direct --test-option='--pattern=palmares schema bootstrap'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add sql/020_views.sql sql/checks/050_actions_committee_treasury.sql
git commit -m "feat(sql): gov_action lifecycle, committee hot-cold resolution and treasury withdrawals"
```

---

### Task T-SQL-METRICS-8: `palmares.activity` — eleven arms, thirteen kinds, a total cursor (D-19)

**Files:**
- Modify: `sql/020_views.sql` (append)
- Test: `src/test/UnitTests/Activity.hs`, `sql/checks/060_activity.sql`
- Modify: `palmares.cabal` (test-suite `other-modules: UnitTests.Activity`)

**Interfaces:**
- Consumes: `palmares.drep_cert`, `palmares.vote`, `palmares.vote_delegation`, `palmares.gov_action`, `palmares.treasury_withdrawal`.
- Produces: `palmares.activity (event_type, source, sort_slot, tx_index, item_index, slot_no, epoch_no, block_time, tx_hash, actor_id, target_id)`; `event_type` is the thirteen governance values of `ActivityEventType`; `source = 'governance'`.

- [ ] **Step 1: Write the failing test**

```haskell
-- src/test/UnitTests/Activity.hs — D-19. Seeded, because the property under test is the cursor's
-- totality, and a seeded set can contain two events in one slot and two epoch-boundary events in one epoch.
tests :: TestTree
tests = testGroup "activity feed"
  [ testCase "all thirteen governance kinds are produced" $ withScratchDb $ \pool -> do
      applyPalmaresSchema pool
      runPg pool $ rawExecute seedActivity []
      ks <- runPg pool $ rawSql "SELECT DISTINCT event_type FROM palmares.activity ORDER BY 1" []
      map unSingle ks @?= [ "committee_authorized", "committee_resigned", "delegation_changed"
                          , "drep_registered", "drep_retired", "drep_updated"
                          , "gov_action_dropped", "gov_action_enacted", "gov_action_expired"
                          , "gov_action_proposed", "gov_action_ratified"
                          , "treasury_withdrawal_enacted", "vote_cast" ]
  , testCase "the cursor triple is unique over the whole feed" $ withScratchDb $ \pool -> do
      applyPalmaresSchema pool
      runPg pool $ rawExecute seedActivity []
      [Single (dups :: Int)] <- runPg pool $ rawSql
        "SELECT count(*)::int FROM (SELECT 1 FROM palmares.activity \
        \GROUP BY sort_slot, tx_index, item_index HAVING count(*) > 1) x" []
      dups @?= 0
  , testCase "epoch-boundary events carry a negative tx_index, a null slot and a null tx_hash" $ withScratchDb $ \pool -> do
      applyPalmaresSchema pool
      runPg pool $ rawExecute seedActivity []
      [Single (bad :: Int)] <- runPg pool $ rawSql
        "SELECT count(*)::int FROM palmares.activity WHERE event_type IN \
        \('gov_action_ratified','gov_action_enacted','gov_action_expired','gov_action_dropped','treasury_withdrawal_enacted') \
        \AND (tx_index NOT IN (-1, -2) OR slot_no IS NOT NULL OR tx_hash IS NOT NULL)" []
      bad @?= 0
  , testCase "an enacted treasury withdrawal and its action's enactment do not share a cursor" $ withScratchDb $ \pool -> do
      applyPalmaresSchema pool
      runPg pool $ rawExecute seedActivity []   -- seeds one enacted TreasuryWithdrawals action
      [Single (ix :: Int)] <- runPg pool $ rawSql
        "SELECT tx_index FROM palmares.activity WHERE event_type = 'treasury_withdrawal_enacted'" []
      ix @?= (-2)
  , testCase "an epoch-boundary event's time is the first block of its epoch" $ withScratchDb $ \pool -> do
      applyPalmaresSchema pool
      runPg pool $ rawExecute seedActivity []
      [Single (t :: UTCTime)] <- runPg pool $ rawSql
        "SELECT block_time FROM palmares.activity WHERE event_type = 'gov_action_ratified'" []
      t @?= read "2026-10-18 00:00:00 UTC"   -- MIN(block.id) WHERE epoch_no = 320 in seedActivity
  ]
```

```sql
-- sql/checks/060_activity.sql — the same invariants against the real feed.
DO $$ BEGIN
  ASSERT (SELECT count(*) FROM (SELECT 1 FROM palmares.activity
          GROUP BY sort_slot, tx_index, item_index HAVING count(*) > 1) x) = 0, 'cursor must be total';
  -- the thirteen kinds are asserted on the seeded set, not here: nothing measures whether preprod has a
  -- committee_de_registration row, so a live DISTINCT = 13 would be an unfalsifiable gate.
  ASSERT (SELECT count(*) FROM palmares.activity WHERE event_type = 'gov_action_proposed')  = 131;
  ASSERT (SELECT count(*) FROM palmares.activity WHERE event_type = 'gov_action_ratified')  = 11;
  ASSERT (SELECT count(*) FROM palmares.activity WHERE event_type = 'gov_action_enacted')   = 11;
  ASSERT (SELECT count(*) FROM palmares.activity WHERE event_type = 'gov_action_expired')   = 103;
  -- ASSEMBLY CORRECTION 1 (2026-10-19, f1-yaci-store-evaluation.md §4.1 C1): 6, NOT 108.
  -- `dropped_epoch` is not an independent outcome: on expiry db-sync sets it to `expired_epoch + 1`,
  -- so 102 of the 108 rows carrying it are expired actions that already emit `gov_action_expired`.
  -- Emitting `gov_action_dropped` for them duplicates the expiry event one epoch later. Only the
  -- pruned-sibling case is a genuine drop.
  ASSERT (SELECT count(*) FROM palmares.activity WHERE event_type = 'gov_action_dropped')   = 6;
  ASSERT (SELECT count(*) FROM palmares.activity WHERE event_type = 'vote_cast')
         = (SELECT count(*) FROM palmares.vote);
  ASSERT (SELECT count(*) FROM palmares.activity WHERE source <> 'governance') = 0;
END $$;
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test test --test-show-details=direct --test-option='--pattern=activity feed'`
Expected: FAIL with `relation "palmares.activity" does not exist`

- [ ] **Step 3: Implement `palmares.activity` as a `UNION ALL` of eleven arms**

Six tx-borne arms — `drep_cert` (one arm, three kinds off `cert_kind`), `vote` → `vote_cast`,
`gov_action` → `gov_action_proposed`, `vote_delegation` → `delegation_changed`, `committee_registration` →
`committee_authorized`, `committee_de_registration` → `committee_resigned` — all with
`sort_slot = block.slot_no`, `tx_index = tx.block_index`, and `item_index` = that arm's own per-tx index:
`voting_procedure.index` for `vote_cast`, `gov_action_proposal.index` for `gov_action_proposed`, `cert_index`
for the four certificate arms.

Five epoch-boundary arms (`gov_action_ratified`, `gov_action_enacted`, `gov_action_expired`,
`gov_action_dropped`, `treasury_withdrawal_enacted`) that db-sync records only as `*_epoch` columns, so:

> **ASSEMBLY CORRECTION 1** (2026-10-19, `f1-yaci-store-evaluation.md` §4.1 C1 — supersedes synthesis
> D-19's unqualified event list). The `gov_action_dropped` arm **must carry the restricted predicate**
> `WHERE dropped_epoch IS NOT NULL AND ratified_epoch IS NULL AND enacted_epoch IS NULL AND expired_epoch IS NULL`
> (6 rows on preprod today, not 108). Reason: db-sync's `doc/schema.md` line 910 is stale per its own
> maintainer (issue #1932); CHANGELOG 13.6.0.1 — *"Enacted gov_action_proposal are no longer marked as
> dropped"* — and on **expiry** `dropped_epoch` is set to `expired_epoch + 1`. The other four arms are
> unchanged, `close_epoch = LEAST(ratified, expired, dropped)` is unchanged (`LEAST` still picks the
> smaller `expired_epoch`), and the arm count stays five.

```sql
-- one arm; the other three lifecycle arms are identical bar the column and the event_type string,
-- and treasury_withdrawal_enacted differs only in tx_index = -2 (see below)
SELECT 'gov_action_ratified'            AS event_type,
       'governance'                     AS source,
       eb.first_slot                    AS sort_slot,
       -1                               AS tx_index,       -- sorts before every tx in that epoch
       g.gov_action_id                  AS item_index,     -- gov_action_proposal.id
       NULL::bigint                     AS slot_no,        -- ChainPoint.slot is null for these
       g.ratified_epoch                 AS epoch_no,
       eb.first_time                    AS block_time,     -- block.time of MIN(block.id) in that epoch
       NULL::text                       AS tx_hash,
       ...
FROM palmares.gov_action g
JOIN LATERAL (SELECT b.slot_no AS first_slot, b.time AS first_time FROM public.block b
              WHERE b.epoch_no = g.ratified_epoch ORDER BY b.id LIMIT 1) eb ON true
WHERE g.ratified_epoch IS NOT NULL
```

`item_index = gov_action_proposal.id` is what keeps the triple unique when two actions ratify in one epoch, and
the four lifecycle arms cannot collide with each other because an action reaches at most one of
ratified/enacted/expired/dropped per epoch (preprod: `enacted = ratified + 1`, `dropped = expired + 1`).
`treasury_withdrawal_enacted` **does** collide — it fires in the *same* epoch as `gov_action_enacted` for the same
`gov_action_proposal.id` — so that one arm alone uses **`tx_index = -2`**, which still sorts before every tx in
the epoch and restores totality. This is the one place the plan diverges from T-API §2.1's flat `-1`; the API
block's cursor parser must accept both.

**(OPEN: `voting_procedure.index`, `gov_action_proposal.index` and `cert_index` are three independent per-tx
counters, so a single tx carrying both a vote and a certificate yields the same `(sort_slot, tx_index, item_index)`
triple. Nothing in the notes measures whether preprod contains such a tx; the uniqueness ASSERT in
`sql/checks/060_activity.sql` is what detects it. If it fires, the cursor needs a fourth component (an arm
ordinal) — that is T-API's call, because the API block parses and emits the cursor string.)**

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test test --test-show-details=direct --test-option='--pattern=activity feed'` then `psql "$PG_CONN_STR" -v ON_ERROR_STOP=1 -f sql/checks/060_activity.sql`
Expected: PASS both

- [ ] **Step 5: Commit**

```bash
git add sql/020_views.sql sql/checks/060_activity.sql src/test/UnitTests/Activity.hs palmares.cabal
git commit -m "feat(sql): activity feed unions the five epoch-boundary kinds with a total cursor"
```

---

### Task T-SQL-METRICS-9: the metric catalog — `resources/metric-formulas.json`, `resources/metrics/*.sql`, `Palmares.Metrics.Catalog`

**Files:**
- Create: `resources/metric-formulas.json`
- Create: `resources/metrics/participation_rate_v1.sql`, `rationale_rate_v1.sql`, `age_days_v1.sql`, `delegator_retention_rate_v1.sql`, `vote_latency_median_hours_v1.sql`, `votes_by_type_v1.sql`
- Create: `src/lib/offchain-lib/Palmares/Metrics/Catalog.hs`
- Test: `src/test/UnitTests/MetricCatalog.hs`
- Modify: `palmares.cabal` (`extra-source-files: resources/metric-formulas.json`, `resources/metrics/*.sql`; `offchain-lib` `exposed-modules: Palmares.Metrics.Catalog` + `build-depends: file-embed` — DELTA, it was `mcp-server-lib`'s in the Belt System; test-suite `other-modules: UnitTests.MetricCatalog`)

**Interfaces:**
- Consumes: `palmares.eligible_actions`, `palmares.vote`, `palmares.action_window`, `palmares.drep_interval`, `palmares.live_delegation`.
- Produces: `catalogJson :: ByteString` · `metricSql :: [(FilePath, ByteString)]` · `MetricFormula` (fields `formula_id, name, field, expression, denominator, null_when, inputs, edge_cases, sql_file, sql_object, params, spec, version`; accessors `mfFormulaId`, `mfSqlFile`, `mfParams`, …) · `metricCatalog :: Either String [MetricFormula]` · `lookupMetricSql :: Text -> Maybe ByteString` · `retentionWindowEpochs :: Int` (= 10) · the eight `formula_id`s

- [ ] **Step 1: Write the failing test**

```haskell
-- src/test/UnitTests/MetricCatalog.hs — the anti-drift gate (D-9). Three cheap tests.
tests :: TestTree
tests = testGroup "metric catalog"
  -- eight = T-API §2.5's seven published ids + votes_by_type_v1, which T-METRIC §7 ships as a .sql file
  -- and T-API §2.5 omits from its table while DRepMetrics carries the field.
  [ testCase "catalog parses and holds exactly the eight published formula ids" $ do
      fs <- either assertFailure' pure metricCatalog
      map mfFormulaId fs @?= [ "participation_rate_v1", "rationale_rate_v1", "age_days_v1"
                           , "delegator_retention_rate_v1", "vote_latency_median_hours_v1"
                           , "votes_by_type_v1", "gov_action_state_v1", "tally_cast_votes_v1" ]
      retentionWindowEpochs @?= 10
  , testCase "every sql_file resolves to an embedded file, and every embedded file is in the catalog" $ do
      fs <- either assertFailure' pure metricCatalog
      let referenced = [f | Just f <- map mfSqlFile fs]
      sort referenced @?= sort (map fst metricSql)
      mapM_ (\f -> assertBool (f <> " is embedded") (isJust (lookupMetricSql (pack f)))) referenced
  , testCase "every embedded statement parses and resolves against the real view set" $ withScratchDb $ \pool -> do
      applyPalmaresSchema pool
      fs <- either assertFailure' pure metricCatalog
      -- rawExecute, not PREPARE: the files carry persistent's `?` placeholders, which postgresql-simple
      -- interpolates client-side, and PostgreSQL's PREPARE only accepts $n. Executing against the empty
      -- skeleton with dummy params is what proves every table, column and function name resolves.
      forM_ [(f, s) | f <- fs, Just n <- [mfSqlFile f], Just s <- [lookupMetricSql (pack n)]] $ \(f, s) ->
        runPg pool $ rawExecute (decodeUtf8 s) (dummyParams (mfParams f))
  ]

-- one dummy per declared param: 0 for the ids and epochs, retentionWindowEpochs for the window
dummyParams :: [Text] -> [PersistValue]
dummyParams = map (\p -> if p == "retention_window_epochs"
                           then toPersistValue retentionWindowEpochs
                           else PersistInt64 0)
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test test --test-show-details=direct --test-option='--pattern=metric catalog'`
Expected: FAIL with `Could not find module 'Palmares.Metrics.Catalog'`

- [ ] **Step 3: Implement the catalog**

Six `.sql` files, each T-METRIC §5.1/§5.2/§5.3/§5.4/§5.5/§5.6 **with `_common.sql` replaced by the real objects**
(D-15): the `elig` CTE becomes `FROM palmares.eligible_actions(?, ?)`, `drep_vote` becomes
`palmares.vote WHERE voter_role = 'DRep'`, `action_window` and `drep_interval` are read as views. `rationale_rate_v1`
grades **five** ways (D-3) — `verified | inline | unverified | pending | absent`, numerator `verified + inline`,
`inline` being `voting_anchor.url LIKE 'data:%'` — and the five counters sum to `votes_cast` by construction
because `final` joins `eligible_actions`. `age_days_v1` keeps §5.3's `b2.slot_no > i.from_slot` retire bound.

Named parameters are positional `?` in the files, in the order the catalog's `params` array names them
(`["drep_hash_id", "as_of_epoch"]`, plus `"retention_window_epochs"` for `delegator_retention_rate_v1`).
**(OPEN: the notes write `:drep` / `:as_of_epoch` / `:w` but never fix a binding mechanism; persistent's `rawSql`
is positional, so the plan uses `?` + a `params` array. Confirm before `Query/Metrics.hs` is written.)**

`Palmares/Metrics/Catalog.hs`: `catalogJson` and each entry of `metricSql` is its own
`$(makeRelativeToProject "resources/…" >>= embedFile)` — **one `embedFile` per file, never `embedDir`**, because
`embedDir` only depends on the files it found, so a newly added `.sql` would pass CI against a stale embed.
`metricCatalog` is `eitherDecodeStrict' catalogJson` over a `deriving-aeson` record with
`StripPrefix "mf" + CamelToSnake` over fields named `mfFormulaId`/`mfName`/`mfField`/`mfSqlFile`/`mfSqlObject`/`mfParams`/… (stripping `"formula"` would emit `id`, not `formula_id`). `(decision)` the module lives in `offchain-lib`, not in an exe, because
both `admin write-metrics-doc` and query-api read it; synthesis §1.3 names the file but not its library.

Catalog entries carry T-METRIC §7's exact field set. The two view-backed formulas (`gov_action_state_v1`,
`tally_cast_votes_v1`) have `"sql_file": null` and a `"sql_object"` naming `palmares.gov_action` /
`palmares.vote_tally` **(OPEN: field not in the notes; needed because every DTO `formula_id` must resolve)**.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test test --test-show-details=direct --test-option='--pattern=metric catalog'`
Expected: PASS (3 tests)

- [ ] **Step 5: Commit**

```bash
git add resources/metric-formulas.json resources/metrics src/lib/offchain-lib/Palmares/Metrics/Catalog.hs src/test/UnitTests/MetricCatalog.hs palmares.cabal
git commit -m "feat(metrics): file-embedded formula catalog with the executed SQL as its source of truth"
```

---

### Task T-SQL-METRICS-10: `admin write-metrics-doc` generates `docs/metrics.md`, and CI fails on drift

**Files:**
- Modify: `src/exe/admin/Main.hs` (a `WriteMetricsDoc FilePath` constructor on `Command`, a `command` entry in `commandParser`, the pre-provider `case cmd of` branch in `main`, and the two `pure () -- Handled in main before provider loading` cases — exactly `WriteBlueprint`'s four sites)
- Create: `src/lib/offchain-lib/Palmares/Metrics/Doc.hs`
- Create: `docs/metrics.md` (generated, committed)
- Test: `src/test/UnitTests/MetricDoc.hs`
- Modify: `palmares.cabal` (`offchain-lib` `exposed-modules: Palmares.Metrics.Doc`; test-suite `other-modules: UnitTests.MetricDoc`), `.github/workflows/ci.yml` (drift step in the `haskell` job, after `cabal test all`)

**Interfaces:**
- Consumes: `metricCatalog`, `lookupMetricSql`, `MetricFormula` and its `mf*` accessors.
- Produces: `Palmares.Metrics.Doc.renderMetricsDoc :: [MetricFormula] -> Text` · `admin write-metrics-doc --output docs/metrics.md` · the committed `docs/metrics.md` · CI gate `git diff --exit-code docs/metrics.md`

- [ ] **Step 1: Write the failing test**

```haskell
-- src/test/UnitTests/MetricDoc.hs
tests :: TestTree
tests = testGroup "metrics doc"
  [ testCase "the committed docs/metrics.md is exactly what the catalog renders" $ do
      fs <- either assertFailure' pure metricCatalog
      committed <- T.readFile "docs/metrics.md"
      renderMetricsDoc fs @?= committed
  , testCase "every formula's SQL is inlined in the doc, so prose cannot drift from code" $ do
      fs <- either assertFailure' pure metricCatalog
      let doc = renderMetricsDoc fs
      forM_ [s | f <- fs, Just n <- [mfSqlFile f], Just s <- [lookupMetricSql (pack n)]] $ \s ->
        assertBool "sql inlined" (decodeUtf8 s `T.isInfixOf` doc)
  ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test test --test-show-details=direct --test-option='--pattern=metrics doc'`
Expected: FAIL with `docs/metrics.md: openFile: does not exist`

- [ ] **Step 3: Implement**

`renderMetricsDoc` emits one `##` section per catalog entry in catalog order — name, `field`, `expression`,
`denominator`, `null_when`, `inputs`, `edge_cases`, then the SQL in a fenced block — preceded by a generated-file
banner naming `admin write-metrics-doc`. The admin command mirrors `write-blueprint` exactly: `WriteMetricsDoc <$> strOption (long "output" <> short 'o' <> value "docs/metrics.md" <> showDefault)`, handled in `main`'s
pre-provider branch — **no provider, no signing key, no `DEPLOYED_VALIDATORS_CONFIG`**, which is what lets CI run
it. `ci.yml` gains, after `cabal test all`:
`cabal run exe:admin -- write-metrics-doc && git diff --exit-code docs/metrics.md`.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal run exe:admin -- write-metrics-doc && cabal test test --test-show-details=direct --test-option='--pattern=metrics doc' && git diff --exit-code docs/metrics.md`
Expected: PASS (2 tests), and `git diff` exits 0

- [ ] **Step 5: Commit**

```bash
git add src/exe/admin/Main.hs src/lib/offchain-lib/Palmares/Metrics/Doc.hs docs/metrics.md src/test/UnitTests/MetricDoc.hs palmares.cabal .github/workflows/ci.yml
git commit -m "feat(admin): write-metrics-doc generates docs/metrics.md, CI fails on drift"
```

---

### Task T-SQL-METRICS-11: the epoch-320 fixture test, with the cursor block pinned by hash + slot + `block.time`

**Files:**
- Create: `test/fixtures/preprod-epoch-320-metrics.json`
- Test: `src/test/UnitTests/MetricFixture.hs`
- Modify: `palmares.cabal` (test-suite `other-modules: UnitTests.MetricFixture`)

**Interfaces:**
- Consumes: `metricCatalog`, `lookupMetricSql`, `retentionWindowEpochs`, every view of T-2…T-8, `Test.Pg.runPg`.
- Produces: `test/fixtures/preprod-epoch-320-metrics.json` — the regression gate on S2 · `withFixtureDb :: (ConnectionPool -> Fixture -> IO a) -> IO a` and `runMetric :: ConnectionPool -> Text -> [PersistValue] -> IO [row]` (both local to `UnitTests/MetricFixture.hs`) · `PALMARES_FIXTURE_PG_CONN_STR` **(OPEN: name is mine; the notes do not name the connection used for the parity run. Owner decides; it points at the caught-up preprod db-sync, so this test never runs in CI.)**

- [ ] **Step 1: Write the failing test**

```json
// test/fixtures/preprod-epoch-320-metrics.json  (values: T-METRIC §4, post-fact-check)
{
  "cursor": {
    "epoch_no": 320,
    "block_hash": "(OPEN: read the epoch-320 cursor block's hash from our own db-sync on the first caught-up run)",
    "slot_no": "(OPEN: same)",
    "block_time": "(OPEN: same — age_days moves inside an epoch, which is how the old 1463 went unnoticed)"
  },
  "drep": {
    "cip129": "drep1y2qn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qmwj3xg",
    "hex": "81378d3ea89b68f0fc09944b1f93f2feb851318f196f98ba4029d8a8",
    "has_script": false,
    "registered_at": "2024-10-16T20:38:10Z",
    "registered_epoch": 173,
    "deposit": 500000000
  },
  "network": { "proposals": 131, "settled": 119, "settled_drep_votable": 115, "open": 12 },
  "metrics": {
    "as_of_epoch": 320,
    "age_days": 732,
    "age_days_tolerance": 1,
    "eligible_actions": 114,
    "votes_cast": 16,
    "participation_rate": 0.140351,
    "rationale_verified": 2,
    "rationale_inline": 0,
    "_comment_rationale": "(OPEN: only rationale_verified = 2 and rationale_rate = 0.125 are measured (T-METRIC §4). inline = 0 is inferred from 'no data: URI on this DRep's votes' and is not measured; pin inline/unverified/pending/absent from the first caught-up run, in the same commit as the cursor values.)",
    "rationale_rate": 0.125,
    "vote_latency_median_hours": 158.04,
    "vote_latency_p90_hours": 543.16,
    "vote_latency_sample": 16,
    "open_actions_unvoted": 12,
    "voting_power": 3923709956713,
    "voting_power_epoch": 320,
    "delegator_count": 13,
    "is_active": true,
    "active_until_epoch": 333,
    "final_votes": { "yes": 7, "no": 8, "abstain": 1 }
  },
  "latency_sample_hours": [1.29, 1.34, 50.27, 53.07, 75.32, 76.01, 151.30, 155.68,
                           160.41, 242.83, 304.53, 385.73, 459.36, 512.35, 573.98, 630.08],
  "votes_by_type": [
    { "action_type": "parameter_change",     "eligible_actions": 11, "votes_cast": 4, "yes": 2, "no": 1, "abstain": 1, "participation_rate": 0.3636 },
    { "action_type": "hard_fork_initiation", "eligible_actions":  2, "votes_cast": 1, "yes": 1, "no": 0, "abstain": 0, "participation_rate": 0.5000 },
    { "action_type": "treasury_withdrawals", "eligible_actions": 79, "votes_cast": 6, "yes": 1, "no": 5, "abstain": 0, "participation_rate": 0.0759 },
    { "action_type": "no_confidence",        "eligible_actions":  1, "votes_cast": 0, "yes": 0, "no": 0, "abstain": 0, "participation_rate": 0.0000 },
    { "action_type": "new_committee",        "eligible_actions":  7, "votes_cast": 0, "yes": 0, "no": 0, "abstain": 0, "participation_rate": 0.0000 },
    { "action_type": "new_constitution",     "eligible_actions":  1, "votes_cast": 0, "yes": 0, "no": 0, "abstain": 0, "participation_rate": 0.0000 },
    { "action_type": "info_action",          "eligible_actions": 13, "votes_cast": 5, "yes": 3, "no": 2, "abstain": 0, "participation_rate": 0.3846 }
  ],
  "pseudo_dreps": {
    "epoch_no": 319,
    "always_abstain_lovelace": 431863790957959,
    "always_no_confidence_lovelace": 325390800383,
    "total_lovelace": 937803900128796,
    "absent_from_dreps": true
  }
}
```

```haskell
-- src/test/UnitTests/MetricFixture.hs — runs the CATALOG SQL, not a second copy of the arithmetic.
tests :: TestTree
tests = testGroup "preprod epoch-320 fixture"
  [ testCase "the cursor block is still the block the fixture pinned" $ withFixtureDb $ \pool fx -> do
      -- the whole reason the cursor is pinned: age_days moves inside an epoch
      [(Single h, Single s, Single t)] <- runPg pool $ rawSql
        "SELECT encode(b.hash,'hex'), b.slot_no, b.time FROM public.block b \
        \WHERE b.hash = decode(?,'hex')" [toPersistValue (cursorBlockHash fx)]
      (h, s, t) @?= (cursorBlockHash fx, cursorSlotNo fx, cursorBlockTime fx)
  , testCase "participation_rate_v1 reproduces 16 / 114 = 0.140351" $ withFixtureDb $ \pool fx -> do
      [(Single elig, Single cast, Single rate, Single openUnvoted)] <-
        runMetric pool "participation_rate_v1" [drepId fx, 320]
      (elig, cast, openUnvoted) @?= (114 :: Int, 16 :: Int, 12 :: Int)
      fmap (roundTo 6) rate @?= Just (0.140351 :: Double)
  , testCase "rationale_rate_v1's five grades sum to votes_cast and the rate is 0.125" $ withFixtureDb $ \pool fx -> do
      [(Single v, Single inl, Single unv, Single pend, Single abs_, Single rate)] <-
        runMetric pool "rationale_rate_v1" [drepId fx, 320]
      (v, inl) @?= (2 :: Int, 0 :: Int)
      v + inl + unv + pend + abs_ @?= (16 :: Int)
      fmap (roundTo 3) rate @?= Just (0.125 :: Double)
  , testCase "age_days_v1 is 732 +/- 1 against the pinned cursor" $ withFixtureDb $ \pool fx -> do
      [Single (d :: Double)] <- runMetric pool "age_days_v1" [drepId fx, 320]
      assertBool (show d <> " outside 732 +/- 1") (abs (d - 732) <= 1)
  , testCase "vote_latency_median_hours_v1 = 158.04 and p90 = 543.16 (percentile_cont, not _disc)" $
      withFixtureDb $ \pool fx -> do
        [(Single med, Single p90, Single n)] <- runMetric pool "vote_latency_median_hours_v1" [drepId fx, 320]
        (roundTo 2 med, roundTo 2 p90, n) @?= (158.04 :: Double, 543.16 :: Double, 16 :: Int)
  , testCase "votes_by_type_v1 emits all 7 types and the columns sum to the headline figures" $
      withFixtureDb $ \pool fx -> do
        rows <- runMetric pool "votes_by_type_v1" [drepId fx, 320]
        length rows             @?= 7
        sum (map eligible rows) @?= (114 :: Int)
        sum (map cast rows)     @?= (16 :: Int)
        sortOn actionType rows  @?= sortOn actionType (votesByType fx)
  , testCase "delegator_retention_rate_v1 binds retention_window_epochs = 10, not a literal" $
      withFixtureDb $ \pool fx -> do
        retentionWindowEpochs @?= 10
        [(Single now_, Single _prev, Single _retained)] <-
          runMetric pool "delegator_retention_rate_v1" [drepId fx, 320, toPersistValue retentionWindowEpochs]
        now_ @?= (13 :: Int)
        -- W is bound, not inlined: with the window set to 0, "previous" must equal today's count
        [(Single now0, Single prev0, Single _)] <-
          runMetric pool "delegator_retention_rate_v1" [drepId fx, 320, toPersistValue (0 :: Int)]
        (now0, prev0) @?= (13 :: Int, 13 :: Int)
  , testCase "the pseudo-DReps carry the dashboard figures and are absent from palmares.drep" $
      withFixtureDb $ \pool _ -> do
        [(Single abstain, Single noConf)] <- runPg pool $ rawSql
          "SELECT sum(amount) FILTER (WHERE target_kind = 'always_abstain'), \
          \       sum(amount) FILTER (WHERE target_kind = 'always_no_confidence') \
          \FROM palmares.drep_epoch_power WHERE epoch_no = 319" []
        (abstain, noConf) @?= (431863790957959 :: Integer, 325390800383 :: Integer)
        [Single (listed :: Int)] <- runPg pool $ rawSql
          "SELECT count(*)::int FROM palmares.drep WHERE drep_raw IS NULL" []
        listed @?= 0
  ]
```

`roundTo`, `eligible`, `cast`, `actionType` and the `Fixture` accessors (`cursorBlockHash`, `cursorSlotNo`,
`cursorBlockTime`, `drepId`, `votesByType`) are this module's own one-line helpers over the parsed fixture.

- [ ] **Step 2: Run test to verify it fails**

Run: `PALMARES_FIXTURE_PG_CONN_STR="$PG_CONN_STR" cabal test test --test-show-details=direct --test-option='--pattern=epoch-320'`
Expected: FAIL with `test/fixtures/preprod-epoch-320-metrics.json: openFile: does not exist`

- [ ] **Step 3: Fill the three `(OPEN: …)` cursor values from the caught-up db-sync and implement the loader**

One query writes the cursor block into the fixture — `SELECT encode(hash,'hex'), slot_no, time FROM public.block
WHERE epoch_no = 320 ORDER BY id DESC LIMIT 1` — and `age_days` is then recomputed against that exact
`block.time`; if it lands outside `732 ± 1`, the cursor value, not the formula, is what moves. `runMetric` looks
the statement up by `formula_id` through `lookupMetricSql` and binds the catalog's `params` in order: there is no
second copy of any formula in the test. `withFixtureDb` opens a pool on `PALMARES_FIXTURE_PG_CONN_STR` (`die`ing
with the variable's name when it is unset, so a skipped run can never read as a pass), decodes
`test/fixtures/preprod-epoch-320-metrics.json` into `Fixture`, and hands both to the body — it never creates or
drops anything, because the fixture DB is the live preprod db-sync.

- [ ] **Step 4: Run test to verify it passes**

Run: `PALMARES_FIXTURE_PG_CONN_STR="$PG_CONN_STR" cabal test test --test-show-details=direct --test-option='--pattern=epoch-320'`
Expected: PASS (8 tests)

- [ ] **Step 5: Commit**

```bash
git add test/fixtures/preprod-epoch-320-metrics.json src/test/UnitTests/MetricFixture.hs palmares.cabal
git commit -m "test(metrics): epoch-320 fixture pinned by cursor hash, slot and block time"
```

---

### Task T-SQL-METRICS-12: the one materialized view `palmares.drep_metrics`, and the one refresh thread

**Files:**
- Create: `sql/030_metrics.sql`
- Create: `src/exe/query-api/Query/Refresh.hs`
- Test: `src/test/UnitTests/Refresh.hs`, `sql/checks/070_drep_metrics.sql`
- Modify: `palmares.cabal` (query-api `other-modules: Query.Refresh`; test-suite `other-modules: Query.Refresh, UnitTests.Refresh` — it compiles out of the `src/exe/query-api` source dir T-1 added), `.env.example`

**Interfaces:**
- Consumes: `palmares.eligible_actions`, `palmares.vote`, `palmares.anchor`, `palmares.live_delegation`, `palmares.drep_epoch_power`, `palmares.drep_interval`, `palmares.sync_state`, `palmaresSqlFiles`.
- Produces: `palmares.drep_metrics` (columns: `drep_hash_id, drep_raw, has_script, eligible_actions, votes_cast, participation_rate, rationale_verified, rationale_inline, rationale_unverified, rationale_pending, rationale_absent, rationale_rate, vote_latency_median_hours, vote_latency_p90_hours, vote_latency_sample, age_days, voting_power, voting_power_epoch, delegator_count, open_actions_unvoted, is_active, active_until_epoch, last_vote_at, last_vote_epoch, as_of_epoch`) · indexes `palmares_idx_drep_metrics_{pk,part,power,raw}` · `RefreshMark` · `shouldRefresh :: Maybe RefreshMark -> RefreshMark -> Bool` · `refreshDRepMetrics :: ConnectionPool -> IO (Either SomeException ())` · `runRefreshThread :: ConnectionPool -> IO ThreadId` · `PALMARES_VIEW_REFRESH_SECONDS` (default 300)
- Column names are synthesis §1.4's canonical set; T-DATA's `eligible_closed_actions` / `voted_closed_actions` / `votes_with_anchor` / `avg_vote_latency_seconds` / `latest_power_*` / `live_delegator_count` / `epochs_with_power` are aliases to delete on sight.

- [ ] **Step 1: Write the failing test**

```haskell
-- src/test/UnitTests/Refresh.hs
tests :: TestTree
tests = testGroup "drep_metrics refresh"
  [ testCase "refreshes on the first tick, then only when the tip mark changes" $ do
      shouldRefresh Nothing            (RefreshMark 100 320) @?= True   -- never refreshed
      shouldRefresh (Just (RefreshMark 100 320)) (RefreshMark 100 320) @?= False  -- tip stood still
      shouldRefresh (Just (RefreshMark 100 320)) (RefreshMark 101 320) @?= True   -- tip advanced
      shouldRefresh (Just (RefreshMark 100 320)) (RefreshMark 100 321) @?= True   -- epoch boundary: the
      -- lifecycle *_epoch columns and drep_distr only move here, and nothing new is inserted
      shouldRefresh (Just (RefreshMark 100 320)) (RefreshMark  97 320) @?= True   -- rollback: mark went back
  , testCase "the matview exists, is populated at boot, and REFRESH CONCURRENTLY works on it" $
      withScratchDb $ \pool -> do
        applyPalmaresSchema pool          -- sql/030 creates it WITH DATA, inside the boot transaction
        Right () <- refreshDRepMetrics pool
        [Single (n :: Int)] <- runPg pool $ rawSql
          "SELECT count(*)::int FROM pg_matviews WHERE schemaname='palmares' AND matviewname='drep_metrics'" []
        n @?= 1
  , testCase "a failed refresh is returned, never thrown past the loop" $ withScratchDb $ \pool -> do
      applyPalmaresSchema pool
      runPg pool $ rawExecute "DROP MATERIALIZED VIEW palmares.drep_metrics" []   -- what a db-sync migration CASCADE does
      r <- refreshDRepMetrics pool
      assertBool "must come back as Left, so the thread keeps looping" (isLeft r)
  ]
```

```sql
-- sql/checks/070_drep_metrics.sql — the matview agrees with the catalog SQL it is built from.
DO $$
DECLARE d bigint;
BEGIN
  SELECT id INTO d FROM public.drep_hash
    WHERE raw = decode('81378d3ea89b68f0fc09944b1f93f2feb851318f196f98ba4029d8a8','hex');
  ASSERT (SELECT count(*) FROM palmares.drep_metrics) = (SELECT count(*) FROM public.drep_hash WHERE raw IS NOT NULL);
  ASSERT (SELECT eligible_actions FROM palmares.drep_metrics WHERE drep_hash_id = d) = 114;
  ASSERT (SELECT votes_cast       FROM palmares.drep_metrics WHERE drep_hash_id = d) = 16;
  ASSERT (SELECT round(participation_rate, 6) FROM palmares.drep_metrics WHERE drep_hash_id = d) = 0.140351;
  ASSERT (SELECT delegator_count  FROM palmares.drep_metrics WHERE drep_hash_id = d) = 13;
  ASSERT (SELECT voting_power     FROM palmares.drep_metrics WHERE drep_hash_id = d) = 3923709956713;
  -- D-3: the five grades sum to votes_cast, for every DRep
  ASSERT (SELECT count(*) FROM palmares.drep_metrics
          WHERE rationale_verified + rationale_inline + rationale_unverified
              + rationale_pending  + rationale_absent <> votes_cast) = 0;
  -- no rate is ever 0 where it should be null (3.8)
  ASSERT (SELECT count(*) FROM palmares.drep_metrics
          WHERE eligible_actions = 0 AND participation_rate IS NOT NULL) = 0;
  -- CONCURRENTLY needs the unique index; assert it rather than discovering it at runtime
  ASSERT (SELECT count(*) FROM pg_indexes
          WHERE schemaname = 'palmares' AND indexname = 'palmares_idx_drep_metrics_pk') = 1;
END $$;

-- outside the DO block, and outside any transaction: PreventInTransactionBlock rejects CONCURRENTLY
-- "from a function or multi-command string", which a DO body is.
REFRESH MATERIALIZED VIEW CONCURRENTLY palmares.drep_metrics;
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test test --test-show-details=direct --test-option='--pattern=drep_metrics refresh'`
Expected: FAIL with `Could not find module 'Query.Refresh'`

- [ ] **Step 3: Implement `sql/030_metrics.sql` and `Query/Refresh.hs`**

`sql/030_metrics.sql` is T-DATA §3.5's statement rebuilt on the canonical objects and the canonical column names
of synthesis §1.4: the `eligible`/`participation` CTEs become a `LATERAL palmares.eligible_actions(dh.id, s.epoch_no)`
per DRep, `count(DISTINCT …)` stays mandatory (re-votes are real), `rationale` becomes the five-grade `FILTER` set,
latency becomes `percentile_cont(0.5|0.9)`, `power` reads `palmares.drep_epoch_power WHERE target_kind = 'drep'`,
`deleg` reads `palmares.live_delegation`, `age_days` is §1.3's interval sum against `sync_state.tip_time`, and
`epochs_with_power` is dropped (nothing reads it). It ends with the four indexes above, `_pk` UNIQUE on
`drep_hash_id`. It is created **WITH DATA** — a never-populated matview rejects `REFRESH … CONCURRENTLY` — and
since `sql/000` drops the schema, the matview is dropped and rebuilt at every query-api boot, which is what
dissolves T-DATA §3.2's matview-dependency hazard.

`Query/Refresh.hs`: `RefreshMark = RefreshMark { markBlockNo :: Int64, markEpochNo :: Int } deriving (Eq, Show)` read from
`palmares.sync_state`; `shouldRefresh prev now = prev /= Just now` (one expression — it covers first tick, tip
advance, epoch boundary and rollback, so no `palmares.cursor` table is needed and none is in synthesis §1.2);
`refreshDRepMetrics` is
`try (runSqlPoolNoTransaction (rawExecute "REFRESH MATERIALIZED VIEW CONCURRENTLY palmares.drep_metrics" []) pool Nothing)`
— **`runSqlPoolNoTransaction`, not `runSqlPool`**: PostgreSQL's `ExecRefreshMatView` calls
`PreventInTransactionBlock` for `CONCURRENTLY`, so the plain pool runner's `BEGIN` turns every refresh into
`ERROR: REFRESH MATERIALIZED VIEW CONCURRENTLY cannot be executed from a function or multi-command string`.
This is the same hazard D-15 found for `CREATE INDEX CONCURRENTLY`, one layer up;
`runRefreshThread` is `forkIO . forever` of read-mark → `shouldRefresh` → refresh → `threadDelay` on
`PALMARES_VIEW_REFRESH_SECONDS` (default 300), keeping the mark in an `IORef`. Failure behaviour: log the
exception and keep looping — the refresh is idempotent and self-healing, and boot is the only fail-closed point.
A `42P01` means a db-sync migration `CASCADE`-dropped the matview; the log line names the runbook step ("restart
query-api after any db-sync upgrade"). **(OPEN: T-DATA §3.5 says a 60 s tick; T-EXTRACT's `.env.example` ships
`PALMARES_VIEW_REFRESH_SECONDS=300`, which is also the only name the synthesis carries. The plan uses 300 and
keeps the variable, so the owner changes it in `.env.example` without a rebuild; decide before the preprod
deploy: at preprod's ~22.7 s mean block interval a 60 s tick rebuilds the matview every ~2.6 blocks, a 300 s
tick every ~13.)**

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test test --test-show-details=direct --test-option='--pattern=drep_metrics refresh'` then `psql "$PG_CONN_STR" -v ON_ERROR_STOP=1 -f sql/checks/070_drep_metrics.sql`
Expected: PASS (3 tests), and the check file exits 0

- [ ] **Step 5: Commit**

```bash
git add sql/030_metrics.sql src/exe/query-api/Query/Refresh.hs src/test/UnitTests/Refresh.hs sql/checks/070_drep_metrics.sql palmares.cabal .env.example
git commit -m "feat(query-api): palmares.drep_metrics matview and its single CONCURRENTLY refresh thread"
```

---

### Block interfaces — produces

**SQL — schema `palmares` (applied by query-api at boot, in one transaction, `die` on failure):**
`palmares.sync_state` · `palmares.drep` · `palmares.drep_cert` · `palmares.drep_interval` ·
`palmares.action_window` · `palmares.gov_action` · `palmares.vote` · `palmares.vote_tally` ·
`palmares.anchor` · `palmares.vote_delegation` · `palmares.live_delegation` ·
`palmares.drep_epoch_delegators` · `palmares.drep_epoch_power` · `palmares.committee_member` ·
`palmares.treasury_withdrawal` · `palmares.thresholds` · `palmares.threshold_map` (the 18th view: §1.2's
seventeen plus D-25's static table) · `palmares.activity` ·
`palmares.drep_metrics` (the only MATERIALIZED view) · `palmares.eligible_actions(drep_hash_id bigint, as_of_epoch int)` (the only function) · indexes `palmares_idx_drep_metrics_{pk,part,power,raw}`

**Enumerations fixed in SQL:** `palmares.anchor.verdict ∈ valid | invalid_schema | hash_mismatch | oversized | unreachable | inline | not_fetched` (+ `cip100_valid`) · `target_kind ∈ drep | always_abstain | always_no_confidence` · `palmares.gov_action.state ∈ proposed | ratified | enacted | expired | dropped` · `palmares.vote_tally.basis = 'cast_votes_only'` · the thirteen governance `palmares.activity.event_type` values · `palmares.activity.source = 'governance'` · `palmares.activity.tx_index ∈ {-1 (gov-action lifecycle), -2 (treasury_withdrawal_enacted), tx.block_index}` — the API block's cursor parser must accept both negatives

**Haskell:** `Query.Views.{applyPalmaresSchema, applySchemaSql, palmaresSqlFiles}` ·
`Query.Refresh.{RefreshMark, shouldRefresh, refreshDRepMetrics, runRefreshThread}` ·
`Palmares.Metrics.Catalog.{catalogJson, metricSql, MetricFormula, metricCatalog, lookupMetricSql, retentionWindowEpochs}` ·
`Palmares.Metrics.Doc.renderMetricsDoc` · `Test.Pg.{withScratchDb, runPg}`

**Files other blocks read:** `sql/000_schema.sql`, `sql/010_gov_views.sql`, `sql/020_views.sql`, `sql/030_metrics.sql` · `sql/checks/*.sql` + `scripts/verify_views.sh` · `resources/metric-formulas.json` + `resources/metrics/*.sql` · `docs/metrics.md` (generated) · `test/fixtures/dbsync-skeleton.sql` · `test/fixtures/preprod-epoch-320-metrics.json`

**`formula_id`s:** `participation_rate_v1` · `rationale_rate_v1` · `age_days_v1` ·
`delegator_retention_rate_v1` · `vote_latency_median_hours_v1` · `votes_by_type_v1` ·
`gov_action_state_v1` · `tally_cast_votes_v1`

**Env vars:** `PALMARES_VIEW_REFRESH_SECONDS` (default 300) · `PALMARES_TEST_PG_CONN_STR` · `PALMARES_FIXTURE_PG_CONN_STR`; consumes `PG_CONN_STR`

**Consumes from other blocks:** `PG_CONN_STR` and the pool shape of `src/exe/query-api/Main.hs`
(`createPostgresqlPool`) · roles `palmares_owner` / `palmares_ro` and their grants (step 14) ·
`sql/040_indexes.sql` applied by the `admin apply-indexes` Job (step 12, `CONCURRENTLY`, `pg_trgm`) ·
a caught-up from-genesis preprod db-sync with `shelley: enable: true` (step 11) ·
`Palmares.DomainTypes.Core.Ids` for CIP-129 encoding of `(drep_raw, has_script)` and the CC hot/cold hashes ·
query-api's `Main.hs` must call `applyPalmaresSchema` before serving and `runRefreshThread` after ·
`Query/Metrics.hs` (API block) is the only consumer of `lookupMetricSql`

### Block open items

- **(OPEN: `W = 10` epochs for retention)** — T-METRIC §8.2 asks the owner; recommended as specified. It is one
  `retention_window_epochs` field in `resources/metric-formulas.json` and is echoed on every response.
- **(OPEN: `InfoAction` in the participation denominator)** — T-METRIC §8.3 asks the owner; recommended kept as
  specified (excluding an action type is the opaque judgment the spec forbids; `votes_by_type_v1` carries the
  nuance). Changing it changes `eligible_actions` 114 → 101 on the fixture.
- **(OPEN: the `:w` / `:drep` / `:as_of_epoch` binding mechanism)** — the notes write named parameters but fix no
  mechanism; persistent's `rawSql` is positional. The plan uses `?` plus a `params` array per catalog entry.
  Confirm before `Query/Metrics.hs`.
- **(OPEN: the `epoch_stake ↔ drep_distr` reconciliation tolerance)** — T-METRIC §5.4/§9 and synthesis §6 leave it
  unmeasured; Experiment A answers it. Until then `sql/checks/040_power.sql` asserts no tolerance and the
  reconciliation line stays in `admin reconcile-koios` (Phase 5), not here.
- **(OPEN: synthesis §1.2 says "fourteen views" but its own table names seventeen plain views)** — the table
  wins, since it names them; all seventeen are built here, plus `palmares.threshold_map` (D-25) for eighteen.
- **(OPEN: `palmares.threshold_map` is my name)** — D-25 requires the static (role, action-type) → threshold table
  as a named deliverable with its own test, but no note names the object and §1.2 does not enumerate it.
- **(OPEN: `sql_object` catalog field)** — needed so `gov_action_state_v1` and `tally_cast_votes_v1`, whose SQL
  lives in a view rather than a `.sql` file, still resolve for every DTO `formula_id`. Not in the notes.
- **(OPEN: the fixture's five-grade rationale split)** — T-METRIC §4 gives only `rationale_verified = 2` and
  `rationale_rate = 0.125`; how the other 14 votes split across `inline | unverified | pending | absent` is not
  measured. Pin it from the first caught-up run, in the same commit as the fixture's cursor values.
- **(OPEN: the fixture's cursor block hash, slot and `block.time`)** — must be read off our own db-sync at epoch
  320 (T-SQL-METRICS-11 step 3). `age_days` moves inside an epoch; this is how the old `1463` went unnoticed.
- **(OPEN: `PALMARES_TEST_PG_CONN_STR` / `PALMARES_FIXTURE_PG_CONN_STR`)** — no note names a test-database env
  var, and `ci.yml` as written in T-INFRA §5.1 has no Postgres service; T-SQL-METRICS-1 adds one.
- **(OPEN: refresh interval 60 s vs 300 s)** — T-DATA §3.5 vs T-EXTRACT `.env.example`; the synthesis does not
  resolve it. `PALMARES_VIEW_REFRESH_SECONDS=300`.
- **(OPEN: `Palmares.Metrics.Catalog`'s library)** — synthesis §1.3 names `Metrics/Catalog.hs` without a library;
  placed in `offchain-lib` because both `admin` and `query-api` read it, which adds `file-embed` to that library.
- **(OPEN: CC vote volume)** — preprod has 3,243,802 CC vote rows (test spam). Do **not** fixture that figure from
  Koios; re-measure on our own db-sync, and check `/votes` p95 before adding latency/power columns to the matview.
- **(OPEN: the activity cursor's `item_index` across sibling per-tx lists)** — `voting_procedure.index`,
  `gov_action_proposal.index` and `cert_index` are independent counters, so one tx carrying both a vote and a
  certificate produces a duplicate triple. The uniqueness ASSERT in `sql/checks/060_activity.sql` is the detector;
  the fix (a fourth cursor component) is T-API's call, since the API block emits and parses the cursor string.
  The treasury-vs-enactment collision inside one epoch is already resolved here, with `tx_index = -2`.
- **(OPEN: SPO power in `palmares.vote_tally`)** — T-API §2.6 names `pool_stat.voting_power`; no note verifies
  `pool_stat` is populated under `preset: only_governance` + `shelley: enable: true`. Measure
  `SELECT count(*) FROM pool_stat` on the caught-up sync; until then SPO rows ship counts with zero power.
- **(OPEN: `treasury_withdrawal.paid_heuristic`)** — `reward_rest` has no gov-action key, so the
  `(stake_address_id, spendable_epoch)` match is ambiguous when one address receives two withdrawals in one epoch.
  The column name carries the warning; the API block decides whether to publish it at all.

## Block 5 — The public Query API

# Plan A — block: the public Query API (synthesis §3 Phase 3, items 18–19)

This block delivers the read surface of F1 check 12 and the API half of check 16: the governance DTOs, the
`palmares` schema bootstrap and matview refresh thread inside `query-api`, the ten governance route families over
the `palmares.*` views, the four page composites a named screen renders, `/health` · `/ready` · `/status` with the
freshness contract, `/metrics/formulas`, CSV content negotiation, and `query-api --write-swagger-only` with its CI
parity guard. It implements D-1 (no projection tables — every row comes from a view or the one matview), D-7
(`PALMARES_READY_MAX_LAG_SECONDS=300` + `PALMARES_AMBER_LAG_SECONDS=120`, `/ready` keeps the `drep_distr_epoch` and
`network_name` checks), D-9 (`/metrics/formulas` from the embedded catalog), D-10 (the anchor verdict is consumed
from `palmares.anchor`, never reclassified in Haskell), D-11 (`cassava` + `Palmares.WebAPI.CSV` with the
formula-injection guard, `Header "Accept"` on the list endpoints), D-17 (`age_days :: Maybe Int` everywhere),
D-18 (`pageTotal :: Maybe Int`, `null` on `/activity`), D-24 (both `voter_hot_hash` and `voter_cold_hash` on CC
votes, `voter_id` = the cold CIP-129 form), D-25 (the static (role, type) → threshold-column table as a named
deliverable with its own test), and §1.4's seven-value anchor enum plus `cip100_valid :: Maybe Bool`.
**Assumptions.** (a) Tasks 14–17 of synthesis §3 are done: roles + grants exist, `sql/000`–`sql/030` are written,
the metric catalog and the epoch-320 fixture exist, and db-sync has completed a from-genesis preprod sync — every
endpoint test here asserts against that real preprod data at **epoch 320**. (b) `Palmares.DomainTypes.Core.Ids`
(Phase 1 step 7) exists. (c) Attestation-sourced fields are Plan B's: `DRepDetailResponse` gets `profile` /
`attestations` / `disputes`, `StatusResponse` gets `attestations` / `protocol`, `ProposerSummary` gets
`profile_id`, `/ready` gets its Kupo-lag check, `SearchResults` gets its `profiles` / `attestations` groups, and
CSV gets its `/attestations` + `/disputes` instances when Plan B lands — each is a new optional field or route, i.e.
a **minor** Swagger bump by the §5 SemVer rule, so nothing here is rewritten. In Plan A those groups serve empty
and those fields are absent. (d) Two disambiguations against the binding synthesis: `q` is cut **only on `/spos`**
(cut list, C-10) and stays on `/dreps`, `/votes`, `/gov-actions` and `/search`, without which the task's own
"activity & search" family cannot exist; and `Query/Refresh.hs` is listed in §3 step 18, so it is here (T-API-5) —
if another block also claims it, drop it there, not here.

---

### Task T-API-1: Governance ids, chain time, and the wire enums

**Files:**
- Modify: `src/lib/offchain-lib/Palmares/DomainTypes/Core/Types.hs` (append a `-- * Governance ids and enums` section)
- Create: `src/test/UnitTests/GovDto.hs`
- Modify: `src/test/UnitTests.hs` (add `govDtoTests` to the tree)
- Modify: `palmares.cabal` (`other-modules`: `UnitTests.GovDto`)

**Interfaces:**
- Consumes: `Palmares.DomainTypes.Core.Ids.{drepIdCip129, drepIdCip105, govActionIdCip129, parseDrepIdAny}`; `Utils.mkStripPrefixSchemaOptions`
- Produces: `DRepId`, `GovActionId`, `ChainPoint(..)`, `DRepStatus`, `DRepUpdateKind`, `GovActionType`, `GovActionState`, `VoterRole`, `Vote`, `TallyBasis`, `AnchorStatus`, `ActivitySource`, `ActivityEventType`, `SearchKind`, `CommitteeMemberStatus`, `instance FromHttpApiData DRepId`, `instance FromHttpApiData GovActionId`, `govDtoTests`

- [ ] **Step 1: Write the failing test**

```haskell
-- src/test/UnitTests/GovDto.hs
module UnitTests.GovDto (govDtoTests) where

import Data.Aeson (decode, encode, toJSON)
import Data.Time (UTCTime)
import Palmares.DomainTypes.Core.Types
import Servant (parseUrlPiece)
import Test.Tasty
import Test.Tasty.HUnit

-- Bitcows, the §4 fixture DRep: CIP-129 id, CIP-105 view, raw 28-byte hex.
bitcowsCip129, bitcowsCip105, bitcowsHex :: Text
bitcowsCip129 = "drep1y2qn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qmwj3xg"
bitcowsCip105 = "drep1sym3575f3k68c0qfj39e7flthhpf3367t9he3wjqzwc4qcdx550"
bitcowsHex    = "81378d3ea89b68f0fc09944b1f93f2feb851318f196f98ba4029d8a8"

govDtoTests :: TestTree
govDtoTests =
  testGroup
    "governance DTO wire format"
    [ testCase "tx-borne ChainPoint" $
        encode (ChainPoint (Just 84233900) 215 (read "2026-10-19 08:14:02 UTC" :: UTCTime) (Just "a1b2"))
          @?= "{\"slot\":84233900,\"epoch\":215,\"time\":\"2026-10-19T08:14:02Z\",\"tx_hash\":\"a1b2\"}",
      testCase "epoch-boundary ChainPoint has null slot and null tx_hash" $
        encode (ChainPoint Nothing 215 (read "2026-10-19 00:00:00 UTC" :: UTCTime) Nothing)
          @?= "{\"slot\":null,\"epoch\":215,\"time\":\"2026-10-19T00:00:00Z\",\"tx_hash\":null}",
      -- VoterRole is the one enum where CamelToSnake is WRONG (d_rep / s_p_o): §8.4.
      testCase "VoterRole wire values are hand-written" $
        map toJSON [DRep, SPO, ConstitutionalCommittee]
          @?= ["drep", "spo", "constitutional_committee"],
      testCase "VoterRole round-trips" $
        mapM (decode . encode) [DRep, SPO, ConstitutionalCommittee]
          @?= Just [DRep, SPO, ConstitutionalCommittee],
      testCase "GovActionType uses db-sync's NewCommittee, snake_cased" $
        map toJSON [ParameterChange, HardForkInitiation, TreasuryWithdrawals, NoConfidence, NewCommittee, NewConstitution, InfoAction]
          @?= [ "parameter_change", "hard_fork_initiation", "treasury_withdrawals"
              , "no_confidence", "new_committee", "new_constitution", "info_action" ],
      testCase "AnchorStatus has seven values" $
        map toJSON [AnchorValid, AnchorInvalidSchema, AnchorHashMismatch, AnchorOversized, AnchorUnreachable, AnchorInline, AnchorNotFetched]
          @?= [ "valid", "invalid_schema", "hash_mismatch", "oversized"
              , "unreachable", "inline", "not_fetched" ],
      testCase "ActivityEventType is snake_case, not the Belt System's PascalCase" $
        map toJSON [EvtDrepRegistered, EvtVoteCast, EvtGovActionRatified, EvtTreasuryWithdrawalEnacted]
          @?= ["drep_registered", "vote_cast", "gov_action_ratified", "treasury_withdrawal_enacted"],
      testCase "DRepStatus / DRepUpdateKind / GovActionState / Vote / TallyBasis" $
        ( map toJSON [DRepActive, DRepInactive, DRepRetired, DRepReserved]
        , map toJSON [UpdateKindRegistered, UpdateKindUpdated, UpdateKindRetired]
        , map toJSON [Proposed, Ratified, Enacted, Expired, Dropped]
        , map toJSON [Yes, No, Abstain]
        , toJSON CastVotesOnly )
          @?= ( ["active", "inactive", "retired", "reserved"]
              , ["registered", "updated", "retired"]
              , ["proposed", "ratified", "enacted", "expired", "dropped"]
              , ["yes", "no", "abstain"]
              , "cast_votes_only" ),
      testCase "capture accepts CIP-129, CIP-105, raw hex and the two reserved views" $
        traverse (parseUrlPiece @DRepId)
          [bitcowsCip129, bitcowsCip105, bitcowsHex, "drep_always_abstain", "drep_always_no_confidence"]
          @?= Right (map DRepId [bitcowsCip129, bitcowsCip129, bitcowsCip129, "drep_always_abstain", "drep_always_no_confidence"]),
      testCase "capture normalises to CIP-129 and rejects junk" $
        parseUrlPiece @DRepId "pool1abc" @?= Left "invalid_drep_id",
      testCase "GovActionId capture accepts both forms, canonical is tx#index" $
        traverse (parseUrlPiece @GovActionId)
          [ "04f2a1b3c4d5e6f708192a3b4c5d6e7f8091a2b3c4d5e6f708192a3b4c5d6e7f#0"
          , "gov_action1qnergwfc" ]
          @?= Right [ GovActionId "04f2a1b3c4d5e6f708192a3b4c5d6e7f8091a2b3c4d5e6f708192a3b4c5d6e7f#0"
                    , GovActionId "04f2a1b3c4d5e6f708192a3b4c5d6e7f8091a2b3c4d5e6f708192a3b4c5d6e7f#0" ]
    ]
```

**(OPEN: `bitcowsCip105` above is a placeholder — the fixture gives the CIP-129 id and the raw hex but not
`drep_hash.view`; read the real CIP-105 string out of `drep_hash.view` for that credential once db-sync is synced,
and the `gov_action1qnergwfc` vector is truncated in f1-dbsync §1.3 — take the full bech32 from the same query.)**

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "governance DTO wire format"'`
Expected: FAIL with `Module 'Palmares.DomainTypes.Core.Types' does not export 'ChainPoint'`

- [ ] **Step 3: Implement the ids, `ChainPoint` and the enums in `src/lib/offchain-lib/Palmares/DomainTypes/Core/Types.hs`**

Newtypes (`DRepId`, `GovActionId`) derive `ToJSON`/`FromJSON` newtype-style; `ChainPoint` and every enum
use the house `CustomJSON` + `mkStripPrefixSchemaOptions` pair. `VoterRole` alone takes
`ConstructorTagModifier '[Rename "DRep" "drep", Rename "SPO" "spo", Rename "ConstitutionalCommittee" "constitutional_committee"]`,
and the same three strings are spelled again in its `FromHttpApiData` instance and its `ToParamSchema` `enum_`.
`FromHttpApiData DRepId` is `parseDrepIdAny` + `drepIdCip129` for the three credential forms, a pass-through for the
two reserved views, and `Left "invalid_drep_id"` otherwise; `FromHttpApiData GovActionId` splits on `#` or decodes
the CIP-129 bech32 and always renders `<tx_hash_hex>#<index>`.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-options='-p "governance DTO wire format"'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/lib/offchain-lib/Palmares/DomainTypes/Core/Types.hs src/test/UnitTests/GovDto.hs src/test/UnitTests.hs palmares.cabal
git commit -m "feat(api): governance ids, ChainPoint and wire enums with golden JSON"
```

---

### Task T-API-2: Page envelope, `ApiError`, and the JSON error body

**Files:**
- Create: `src/lib/webapi-lib/Palmares/WebAPI/Envelope.hs` (`Cursor`, `Page`, `ApiError`, `ErrorCode`, `errorCodeToHttpStatus`)
- Modify: `src/lib/webapi-lib/Palmares/WebAPI/Errors.hs` (rewrite `mkServantErr`, make `genericErrorMessage` total over `ErrorCode`)
- Modify: `palmares.cabal` (`webapi-lib`: `exposed-modules += Palmares.WebAPI.Envelope`, `build-depends += deriving-aeson`)
- Modify: `src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs` (re-export the five names above)
- Modify: `src/test/UnitTests/GovDto.hs`

**Interfaces:**
- Consumes: nothing
- Produces: `Cursor`, `Page(..)`, `ApiError(..)`, `ErrorCode(..)`, `errorCodeToHttpStatus :: ErrorCode -> Int`, `mkServantErr :: Int -> ErrorCode -> ServerError`, `genericErrorMessage :: ErrorCode -> Text`

Why these four types are in `webapi-lib` and not in `QueryResponses`, where §2.2/§2.3 drew them: `webapi-lib` has
**no project-library dependency** (house layering, restated by D-11) and `offchain-lib` does not depend on
`webapi-lib` either, so a type used by both `mkServantErr` (`ApiError`, `ErrorCode`) and
`MimeRender CSV (Page a)` (`Page`, and `Cursor` through `pageNextCursor`) can only live in `webapi-lib` —
otherwise T-API-4's instance is an orphan declared in neither type's nor the class's library. `QueryResponses`
re-exports them so every DTO consumer still imports one module. `deriving-aeson` is new to `webapi-lib`'s
`build-depends` (it already carries `aeson`, `swagger2`, `text`); nothing else moves.

- [ ] **Step 1: Write the failing test**

```haskell
-- appended to src/test/UnitTests/GovDto.hs's list
      testCase "Page echoes applied limit/offset; total is nullable (D-18)" $
        encode (Page [1 :: Int, 2] (Just 3) 10 0 Nothing)
          @?= "{\"items\":[1,2],\"total\":3,\"limit\":10,\"offset\":0,\"next_cursor\":null}",
      testCase "Page on /activity carries total = null, never -1" $
        encode (Page [1 :: Int] Nothing 20 0 (Just (Cursor "84233900:12:3")))
          @?= "{\"items\":[1],\"total\":null,\"limit\":20,\"offset\":0,\"next_cursor\":\"84233900:12:3\"}",
      testCase "ApiError is {code,message}" $
        encode (ApiError DrepNotFound "No DRep with that identifier")
          @?= "{\"code\":\"drep_not_found\",\"message\":\"No DRep with that identifier\"}",
      testCase "every ErrorCode maps to its documented status" $
        map errorCodeToHttpStatus
          [ InvalidParameter, QueryTooShort, QueryTooLong, OffsetTooLarge, LimitTooLarge, InvalidCursor
          , BothCursorsGiven, InvalidDrepId, InvalidGovActionId, InvalidAssetClass, InvalidTimeRange
          , InvalidEpochRange, DrepNotFound, GovActionNotFound, SpoNotFound, CommitteeNotFound
          , ProfileNotFound, AttestationNotFound, DisputeNotFound, RateLimited
          , NotReady, IndexerBehind, DbUnavailable, InternalError ]
          @?= replicate 12 400 <> replicate 7 404 <> [429] <> replicate 3 503 <> [500],
      testCase "mkServantErr sets a JSON content type and an ApiError body" $
        let e = mkServantErr 503 IndexerBehind
         in (errHTTPCode e, lookup hContentType (errHeaders e), errBody e)
              @?= (503, Just "application/json", encode (ApiError IndexerBehind (genericErrorMessage IndexerBehind))),
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "governance DTO wire format"'`
Expected: FAIL with `Variable not in scope: errorCodeToHttpStatus`

- [ ] **Step 3: Implement `Cursor`, `Page`, `ApiError`, `ErrorCode`, `errorCodeToHttpStatus`, `mkServantErr`**

`mkServantErr status code` keeps the existing "non-4xx is not collapsed to 400" base-selection `case`, replaces
`errBody` with `encode (ApiError code (genericErrorMessage code))`, and sets
`errHeaders = [(hContentType, "application/json")]`. `instance ToSchema (Page a)` needs
`(ToSchema a, Typeable a)`; because `mkStripPrefixSchemaOptions` lives in `offchain-lib` and is unreachable from
here, `Envelope.hs` writes the same four lines inline:

```haskell
pageSchemaOptions :: SchemaOptions   -- the mkStripPrefixSchemaOptions "page" body, inlined (unreachable library)
pageSchemaOptions =
  fromAesonOptions defaultOptions {fieldLabelModifier = camelTo2 '_' . drop (length "page")}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-options='-p "governance DTO wire format"'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/lib/webapi-lib/Palmares/WebAPI/Envelope.hs src/lib/webapi-lib/Palmares/WebAPI/Errors.hs src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs src/test/UnitTests/GovDto.hs palmares.cabal
git commit -m "feat(api): Page envelope and {code,message} error body with an ErrorCode status map"
```

---

### Task T-API-3: `Query.Common` — bounds, stable ordering, LIKE patterns

**Files:**
- Modify: `src/exe/query-api/Query/Common.hs`
- Create: `src/test/UnitTests/QueryHardening.hs`
- Modify: `src/test/UnitTests.hs`, `palmares.cabal`

**Interfaces:**
- Consumes: `ErrorCode` (T-API-2), `SortOrder` (`Palmares.DomainTypes.Transfer.OrderBy`), `Palmares.WebAPI.Utils.escapeLikePattern`
- Produces: `Limit`, `Offset`, `normalizeLimitOffset :: Int -> Maybe Int -> Maybe Int -> Either ErrorCode (Limit, Offset)`, `maxLimitFor :: Maybe Text -> Int`, `activityMaxLimit`, `likePat :: Text -> Text`, `SortSpec(..)`, `orderClause :: SortSpec -> SortOrder -> Text`, `normalizeQ :: Maybe Text -> Either ErrorCode (Maybe Text)`, `checkRanges`

The Belt System's `applyLimits` / `applyFilterOrderLimit` are **not ported** (D-1/§4.1 rule 2: a handler that slices
a Haskell list is the F-17 bug; the helper that enables it is absent so it cannot be reached).

- [ ] **Step 1: Write the failing test**

```haskell
-- src/test/UnitTests/QueryHardening.hs
module UnitTests.QueryHardening (queryHardeningTests) where

import Data.Text qualified as T
import Palmares.DomainTypes.Transfer.OrderBy (SortOrder (..))
import Palmares.WebAPI.Utils (escapeLikePattern)
import Query.Common
import Test.Tasty
import Test.Tasty.HUnit

queryHardeningTests :: TestTree
queryHardeningTests =
  testGroup
    "query hardening"
    [ testCase "limit clamps silently to [1, maxLimit] and offset to >= 0" $
        map (\(m, l, o) -> normalizeLimitOffset m l o)
          [(200, Just 999, Just (-5)), (200, Just 0, Nothing), (200, Nothing, Just 10)]
          @?= [Right (200, 0), Right (1, 0), Right (50, 10)],
      testCase "offset past the hard ceiling is rejected, never clamped" $
        normalizeLimitOffset 200 (Just 50) (Just 10001) @?= Left OffsetTooLarge,
      testCase "maxLimitFor reads the Accept header: CSV 10000, otherwise 200" $
        map maxLimitFor [Just "text/csv", Just "text/csv;charset=utf-8", Just "application/json", Nothing]
          @?= [10000, 10000, 200, 200],
      testCase "/activity ceiling is 100 with default 20" $
        (activityMaxLimit, normalizeLimitOffset activityMaxLimit Nothing Nothing)
          @?= (100, Right (20, 0)),
      -- §4.4: equal keys otherwise reshuffle between pages and rows get skipped.
      testCase "orderClause always appends the ASC tiebreaker and NULLS LAST" $
        ( orderClause (SortSpec "m.voting_power" True "d.drep_hash_id") Desc
        , orderClause (SortSpec "d.given_name" False "d.drep_hash_id") Asc )
          @?= ( "ORDER BY m.voting_power DESC NULLS LAST, d.drep_hash_id ASC"
              , "ORDER BY d.given_name ASC, d.drep_hash_id ASC" ),
      testCase "likePat lowercases, escapes, and wraps in %" $
        likePat "100%_x\\" @?= "%100\\%\\_x\\\\%",
      -- the composition order in escapeLikePattern is load-bearing (right-to-left)
      testCase "a literal backslash survives as exactly one escaped backslash" $
        escapeLikePattern "\\%" @?= "\\\\\\%",
      testCase "q is trimmed then bounded to [2,100]" $
        map normalizeQ [Just "  ab  ", Just " a ", Just (T.replicate 101 "x"), Nothing]
          @?= [Right (Just "ab"), Left QueryTooShort, Left QueryTooLong, Right Nothing],
      testCase "inverted ranges are 400s, not empty pages" $
        ( checkRanges (Just t2) (Just t1) Nothing Nothing
        , checkRanges Nothing Nothing (Just 320) (Just 300) )
          @?= (Left InvalidTimeRange, Left InvalidEpochRange)
    ]
  where
    t1 = read "2026-01-01 00:00:00 UTC"; t2 = read "2026-02-01 00:00:00 UTC"
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "query hardening"'`
Expected: FAIL with `Variable not in scope: maxLimitFor`

- [ ] **Step 3: Implement the helpers in `src/exe/query-api/Query/Common.hs`**

`normalizeLimitOffset maxLimit` takes the ceiling as its first argument so one function serves JSON (200), CSV
(10000) and `/activity` (100); the default is 50 except on `/activity`, whose handler passes `activityMaxLimit` and
its own default 20. `type Limit = Int` and `type Offset = Int` — synonyms, not newtypes, so the literals in the
test above are the values. `orderClause` takes `SortSpec { sortColumn, sortNullsLast, sortTiebreaker }` where
`sortColumn`/`sortTiebreaker` come only from a closed per-endpoint `*OrderBy` → `SortSpec` total function, so no
user text ever reaches the clause.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-options='-p "query hardening"'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Common.hs src/test/UnitTests/QueryHardening.hs src/test/UnitTests.hs palmares.cabal
git commit -m "feat(api): bounded pagination, stable ordering and escaped LIKE patterns in Query.Common"
```

---

### Task T-API-4: `Palmares.WebAPI.CSV` with the formula-injection guard

**Files:**
- Create: `src/lib/webapi-lib/Palmares/WebAPI/CSV.hs`
- Modify: `palmares.cabal` (`webapi-lib`: `exposed-modules += Palmares.WebAPI.CSV`, `build-depends += cassava`)
- Modify: `src/test/UnitTests/QueryHardening.hs`

**Interfaces:**
- Consumes: `Palmares.WebAPI.Envelope.Page(..)` (T-API-2) — same library, so the `MimeRender` instance below is not an orphan
- Produces: `data CSV`, `instance Accept CSV`, `instance (ToNamedRecord a, DefaultOrdered a) => MimeRender CSV (Page a)`, `safeCell :: Text -> Text`

- [ ] **Step 1: Write the failing test**

```haskell
-- appended to src/test/UnitTests/QueryHardening.hs
csvTests :: TestTree
csvTests =
  testGroup
    "CSV export"
    [ testCase "content type is text/csv;charset=utf-8" $
        contentType (Proxy @CSV) @?= "text/csv;charset=utf-8",
      -- exported governance data lands in spreadsheets: this is a trust boundary, not a nicety
      testCase "formula-injection guard prefixes the five dangerous leads" $
        map safeCell ["=1+1", "+1", "-1", "@SUM(A1)", "\t=x", "\r=x", "plain", "-", ""]
          @?= ["'=1+1", "'+1", "'-1", "'@SUM(A1)", "'\t=x", "'\r=x", "plain", "'-", ""],
      testCase "a cell containing a comma, a quote and a newline survives a round trip" $
        decodeByName (mimeRender (Proxy @CSV) (Page [Row "a,b\"c\nd"] (Just 1) 1 0 Nothing))
          @?= Right (header ["cell"], V.fromList [Row "a,b\"c\nd"]),
      testCase "the envelope is dropped: CSV is rows only, with one header line" $
        mimeRender (Proxy @CSV) (Page [Row "x", Row "y"] (Just 2) 2 0 Nothing)
          @?= "cell\r\nx\r\ny\r\n"
    ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "CSV export"'`
Expected: FAIL with `Could not find module 'Palmares.WebAPI.CSV'`

- [ ] **Step 3: Implement `Palmares.WebAPI.CSV` in `src/lib/webapi-lib/Palmares/WebAPI/CSV.hs`**

`MimeRender CSV (Page a) = encodeDefaultOrderedByName . pageItems` — the envelope has no CSV meaning and the
applied limit is already echoed in the JSON path. `safeCell` prefixes a single `'` when the first character is one
of `= + - @ \t \r`, and the per-DTO `ToNamedRecord` instances pass **every** `Text` cell through it.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-options='-p "CSV export"'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/lib/webapi-lib/Palmares/WebAPI/CSV.hs src/test/UnitTests/QueryHardening.hs palmares.cabal
git commit -m "feat(webapi): CSV content type via cassava with a formula-injection guard"
```

---

### Task T-API-5: `Query.Views` boot DDL (fail-closed) and `Query.Refresh`

**Files:**
- Create: `src/exe/query-api/Query/Views.hs`, `src/exe/query-api/Query/Refresh.hs`
- Modify: `src/exe/query-api/Main.hs`, `palmares.cabal` (`other-modules += Query.Refresh`; `Query.Views` is already in the extraction manifest's `other-modules`; add `extra-source-files: sql/*.sql`)
- Modify: `.env.example`
- Test: `scripts/smoke_views.sh`

**Interfaces:**
- Consumes: `sql/000_schema.sql` … `sql/030_metrics.sql`; roles `palmares_owner` / `palmares_ro`; `palmares.drep_metrics`
- Produces: `applyViewDdl :: ConnectionPool -> IO ()`, `startMetricsRefresh :: ConnectionPool -> Int -> IO ThreadId`, env vars `PALMARES_PG_OWNER_CONN_STR`, `PALMARES_VIEW_REFRESH_SECONDS` (default 300)

**(OPEN: query-api's runtime role is `palmares_ro` (USAGE+SELECT only) but D-15 has query-api apply the DDL and
refresh the matview as `palmares_owner`, so it needs a second pool on a second DSN. Decided by the owner, who took
the D-15 role-separation decision: either (a) `PALMARES_PG_OWNER_CONN_STR` as planned here — two pools, the `_ro`
one serving every request — or (b) `GRANT palmares_owner TO palmares_ro` and one DSN, which hands the public read
path DDL rights and is the reason (a) is written. Build (a) unless told otherwise; switching to (b) deletes one env
var and one pool.)**
**(OPEN: `REFRESH MATERIALIZED VIEW CONCURRENTLY` requires a UNIQUE index on the matview and no note specifies one.
`sql/030_metrics.sql` (synthesis §3 step 16) must create `palmares_drep_metrics_pk UNIQUE` on `drep_hash_id`, or
this task's refresh thread fails at runtime, not at boot. Owner: the step-16 task; this task only consumes it.)**

- [ ] **Step 1: Write the failing test** — a true unit test is impossible (the DDL only means anything against a
synced db-sync), so the check is an explicit script with exact observable output. It needs only the binary to
boot, not any route, so it runs before T-API-6 rewrites `RestAPI.hs`.

```bash
#!/usr/bin/env bash
# scripts/smoke_views.sh — query-api owns the `palmares` schema lifecycle. Run against the preprod stack.
set -euo pipefail
PSQL="psql ${PALMARES_PG_OWNER_CONN_STR} -At -c"

# 1. a boot recreates the whole schema: 17 views + 1 matview + 1 function (synthesis §1.2's table)
$PSQL "SELECT count(*) FROM pg_views WHERE schemaname='palmares'"            | grep -qx 17
$PSQL "SELECT count(*) FROM pg_matviews WHERE schemaname='palmares'"         | grep -qx 1
$PSQL "SELECT count(*) FROM pg_proc p JOIN pg_namespace n ON n.oid=p.pronamespace
       WHERE n.nspname='palmares' AND p.proname='eligible_actions'"          | grep -qx 1

# 2. the matview is really refreshed on a tick. Measured: REFRESH rewrites every row, so
#    pg_stat_all_tables.n_tup_ins for drep_metrics strictly increases across one interval.
ins() { $PSQL "SELECT n_tup_ins FROM pg_stat_all_tables WHERE schemaname='palmares' AND relname='drep_metrics'"; }
before=$(ins)
$PSQL "SELECT pg_sleep(${PALMARES_VIEW_REFRESH_SECONDS:-300} + 30)" >/dev/null
after=$(ins)
[ "$after" -gt "$before" ] || { echo "FAIL: no refresh in one interval ($before -> $after)"; exit 1; }

# 3. fail-closed: a broken DDL file must stop the boot, not serve a half-built schema
sed -i.bak '1i\
SELECT 1/0;' sql/020_anchors.sql
# the boot must die; `timeout` is what distinguishes "died" from "served forever"
timeout 120 cabal run query-api 2>&1 | tail -5 | grep -q "palmares schema DDL failed" \
  || { mv sql/020_anchors.sql.bak sql/020_anchors.sql; echo "FAIL: boot did not die"; exit 1; }
mv sql/020_anchors.sql.bak sql/020_anchors.sql
echo "views OK ($before -> $after rows inserted)"
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bash scripts/smoke_views.sh`
Expected: FAIL at check 1 — `pg_views` count is `0` (not `17`), because nothing has ever created schema `palmares`

- [ ] **Step 3: Implement `applyViewDdl` and `startMetricsRefresh`**

`applyViewDdl` embeds the four files one `embedFile` at a time (`makeRelativeToProject "sql/000_schema.sql" >>= embedFile`,
never `embedDir` — D-9's reason applies identically: `embedDir` would not invalidate the module when a file is
added), applies them in lexical order inside **one** `runSqlPool` transaction on the owner pool, and on
`SomeException` calls `die "palmares schema DDL failed: …"` (Stream C fail-closed doctrine). `startMetricsRefresh`
forks a thread that loops `runSqlPoolNoTransaction` + `rawExecute "REFRESH MATERIALIZED VIEW CONCURRENTLY palmares.drep_metrics" []`
every `PALMARES_VIEW_REFRESH_SECONDS` — `CONCURRENTLY` cannot run inside a transaction block, which is why it is
not `runSqlPool`; a failed refresh logs and retries on the next tick rather than killing the server, because stale
metrics are a lag condition `/status` already reports.

- [ ] **Step 4: Run test to verify it passes**

Run: `bash scripts/smoke_views.sh`
Expected: PASS — `views OK (294 -> 588 rows inserted)` (one 294-row rewrite per refresh)

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Views.hs src/exe/query-api/Query/Refresh.hs src/exe/query-api/Main.hs scripts/smoke_views.sh .env.example palmares.cabal
git commit -m "feat(query-api): apply sql/*.sql at boot in one transaction and refresh drep_metrics on a tick"
```

---

### Task T-API-6: `Query.Gov` plumbing, the probes, `/status` and the freshness contract

**Files:**
- Create: `src/exe/query-api/Query/Gov.hs`, `scripts/smoke_query_api.sh`
- Modify: `src/exe/query-api/RestAPI.hs` (the extraction's **rewrite** target, not a new file — it already exists and compiles after the green gate), `src/exe/query-api/QueryAppMonad.hs`, `src/exe/query-api/Query/ServiceStatus.hs`, `src/lib/webapi-lib/Palmares/WebAPI/CORS.hs`, `src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs`, `src/exe/query-api/Main.hs`, `.env.example`

**Interfaces:**
- Consumes: `palmares.sync_state` (`block_no`, `slot_no`, `epoch_no`, `tip_hash`, `tip_time`, `lag_seconds`, `drep_distr_epoch`, `network_name`), `palmares.thresholds`, `Palmares.WebAPI.ServiceProbe.{alwaysHealthy, ServiceProbe}`, `verifyProjectionDbConnection`
- Produces: `queryJson :: FromJSON a => Text -> [PersistValue] -> SqlPersistT IO [a]`, `queryJson1`, `runGovRead` (one `RepeatableRead` transaction per request), `SyncStatus(..)`, `StatusResponse(..)`, `EpochParamsSummary(..)`, `Freshness(..)`, `freshnessMiddleware :: IO Freshness -> Middleware`, `getStatusQuery`, `getReadyProbe`, `FullAPI`, `mkApp`, env vars `PALMARES_READY_MAX_LAG_SECONDS` (300), `PALMARES_AMBER_LAG_SECONDS` (120), `PALMARES_FRESHNESS_POLL_MS` (5000), `PALMARES_NETWORK_NAME` (`preprod`)

**(OPEN: T-API §6 spells the poll interval `FRESHNESS_POLL_MS`; T-EXTRACT §5.1 requires the `PALMARES_` prefix on
every new env var, so it is `PALMARES_FRESHNESS_POLL_MS` here. Same for `PALMARES_NETWORK_NAME`, which no note
names — `/ready` check 4 needs an expected value to compare `meta.network_name` against, and `StatusResponse.network`
echoes it.)**

Two plumbing rules every later family inherits, both pinned by this task's tests. (1) Every governance read goes
through `runGovRead`, which is `runSqlPoolWithIsolation … RepeatableRead` — db-sync declares `-- noreference` on the
governance FKs, so a composite read across several statements can observe dangling ids while a rollback commits.
(2) Every `SELECT` projects exactly one JSON column built with `row_to_json` / `json_build_object`, whose keys are
the DTO's snake_case wire names, and `queryJson` decodes it with the DTO's own `FromJSON`; a renamed or dropped
view column then fails loudly at the boundary naming the field instead of silently shifting a positional decode.
Every timestamp inside those objects is wrapped `AT TIME ZONE 'UTC'` so it renders with a `+00:00` offset that
`FromJSON UTCTime` accepts — `block.time` is `timestamp` without a zone. (3) That cast is owned **here**, in
`Query/Gov.hs`'s projection list, never in the views: `palmares.sync_state` ships `tip_time` as the bare
`timestamp` T-DATA §5.1 wrote, and only `lag_seconds` is pre-cast there.

- [ ] **Step 1: Write the failing test**

```haskell
-- appended to src/test/UnitTests/GovDto.hs: the decode boundary, pure and cheap
      testCase "queryJson's row shape decodes into SyncStatus" $
        eitherDecode "{\"epoch\":320,\"slot\":84233900,\"block_time\":\"2026-10-19T08:14:02+00:00\",\
                      \\"lag_seconds\":42,\"is_ready\":true,\"source\":\"cardano-db-sync\"}"
          @?= Right (SyncStatus 320 84233900 (read "2026-10-19 08:14:02 UTC") 42 True "cardano-db-sync"),
      testCase "readiness verdict: all four checks, 300 s red" $
        map readyVerdict
          [ ReadyInputs True  42  320 320 "preprod" "preprod"
          , ReadyInputs True 301  320 320 "preprod" "preprod"
          , ReadyInputs True  42  320 318 "preprod" "preprod"   -- drep_distr_epoch < epoch_no - 1
          , ReadyInputs True  42  320 319 "preprod" "preprod"   -- epoch_no - 1 is still ready
          , ReadyInputs True  42  320 320 "mainnet" "preprod"
          , ReadyInputs False 42  320 320 "preprod" "preprod" ]
          @?= [Nothing, Just IndexerBehind, Just IndexerBehind, Nothing, Just NotReady, Just DbUnavailable],
      testCase "amber is 120 s and is a UI band, never the probe" $
        (readyMaxLagDefault, amberLagDefault) @?= (300, 120),
```

```bash
# scripts/smoke_query_api.sh — F1 check 12's smoke half; every later family appends a section.
set -euo pipefail
API="${PALMARES_API:-http://localhost:8083}"
fail=0
j() { curl -fsS -H 'Accept: application/json' "$API$1" | jq -c "$2"; }          # path, filter; 2xx only
# `|| true`: without it `set -e` aborts the whole run on the first failing assertion instead of recording it
assert() { got=$(j "$1" "$2" || true); [ "$got" = "$3" ] || { echo "FAIL $1 $2 => $got (want $3)"; fail=1; }; }
code() { curl -s -o /dev/null -w '%{http_code}' "$API$1"; }
# error bodies need a NON-failing curl: `-f` discards the body of a 4xx, so `assert` can never see `.code`.
jerr() { curl -sS -H 'Accept: application/json' "$API$1" | jq -c "$2"; }
assert_err() { got=$(jerr "$1" "$2" || true); [ "$got" = "$3" ] || { echo "FAIL $1 $2 => $got (want $3)"; fail=1; }; }

# --- probes, status, freshness (T-API-6) ---
assert /health '.status' '"healthy"'
assert /health '.service' '"query-api"'
[ "$(code /ready)" = 200 ] || { echo "FAIL /ready not 200"; fail=1; }
assert /status '.network' '"preprod"'
assert /status '.governance.source' '"cardano-db-sync"'
assert /status '.governance.is_ready' 'true'
assert /status '.governance.epoch >= 320' 'true'
assert /status '.governance.lag_seconds < 120' 'true'          # spec check A1, from /status not the probe
assert /status '.epoch_params.gov_action_lifetime' '7'          # preprod; mainnet is 6
assert /status '.epoch_params.pool_thresholds | map(.[0]) | sort' \
  '["pvt_committee_no_confidence","pvt_committee_normal","pvt_hard_fork_initiation","pvt_motion_no_confidence","pvtpp_security_group"]'
assert /status '.epoch_params.drep_thresholds | length' '10'
curl -fsS -D- -o /dev/null "$API/status" | grep -qi '^x-chain-epoch:'          || { echo "FAIL no X-Chain-Epoch"; fail=1; }
curl -fsS -D- -o /dev/null "$API/status" | grep -qi '^x-data-lag-seconds:'     || { echo "FAIL no X-Data-Lag-Seconds"; fail=1; }
curl -fsS -D- -o /dev/null -H 'Origin: https://gov.cardano.vip' "$API/status" \
  | grep -i '^access-control-expose-headers:' | grep -qi 'x-chain-epoch'       || { echo "FAIL header not exposed to the browser"; fail=1; }
exit $fail
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "governance DTO"' && bash scripts/smoke_query_api.sh`
Expected: FAIL — `Variable not in scope: readyVerdict`, then `FAIL /health` (connection refused: there is no `RestAPI.hs`)

- [ ] **Step 3: Implement the plumbing, the three routes and the freshness middleware**

`RestAPI.hs` is created with
`type FullAPI = SwaggerSchemaUI "swagger-ui" "swagger-api.json" :<|> ServiceProbeAPI :<|> CoreFunctionAPI`, no
`BasicAuth` layer and no `WebAPI.Auth` import (query-api is public; rate limiting is the BFF's job), and
`CoreFunctionAPI = StatusAPI` for now — each later family appends one alternative to the type and one to the
server. `/health` is `alwaysHealthy (pack appVersion) "query-api"` and touches no state. `/ready` reads the single
`palmares.sync_state` row and applies `readyVerdict`, returning `mkServantErr 503` on `Just`. One background
thread polls `palmares.sync_state` every `PALMARES_FRESHNESS_POLL_MS` into a `TVar Freshness`; the same `TVar`
feeds `/ready`, `/status` and `freshnessMiddleware`, so the service has exactly one definition of "how fresh are
we" and the hot path adds no query. `mkCorsMiddleware` gains
`corsExposedHeaders = Just ["X-Chain-Epoch","X-Chain-Slot","X-Chain-Block-Time","X-Data-Lag-Seconds","X-Data-Source"]`
— without it `fetch()` cannot read any of them and the freshness chip silently shows nothing.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-options='-p "governance DTO"' && bash scripts/smoke_query_api.sh`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Gov.hs src/exe/query-api/RestAPI.hs src/exe/query-api/QueryAppMonad.hs src/exe/query-api/Query/ServiceStatus.hs src/exe/query-api/Main.hs src/lib/webapi-lib/Palmares/WebAPI/CORS.hs src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs src/test/UnitTests/GovDto.hs scripts/smoke_query_api.sh .env.example
git commit -m "feat(query-api): /health, /ready and /status over palmares.sync_state with freshness headers"
```

---

### Task T-API-7: `/metrics/formulas` from the embedded catalog

**Files:**
- Create: `src/exe/query-api/Metrics/Catalog.hs`
- Modify: `src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs` (`MetricFormula`), `src/exe/query-api/RestAPI.hs`, `palmares.cabal`
- Test: `src/test/UnitTests/MetricsCatalog.hs`

**Files (cont.):**
- Modify: `palmares.cabal` (`query-api`: `other-modules += Metrics.Catalog`; `extra-source-files: resources/metrics/*.sql`, `resources/metric-formulas.json`)

**Interfaces:**
- Consumes: `resources/metric-formulas.json`, `resources/metrics/*.sql` (step 16)
- Produces: `MetricFormula(..)`, `catalog :: [MetricFormula]`, `metricSql :: [(FilePath, ByteString)]`, `GET /metrics/formulas`

- [ ] **Step 1: Write the failing test**

```haskell
-- src/test/UnitTests/MetricsCatalog.hs
metricsCatalogTests :: TestTree
metricsCatalogTests =
  testGroup
    "metric catalog"
    [ -- the six SQL-backed formulas are T-METRIC §7's file list; the two below them are read off
      -- columns and carry no sql_file, but a DTO publishes their formula_id so they MUST be in the catalog
      testCase "every published formula is present, by id" $
        sort (map formulaFormulaId catalog)
          @?= sort [ "participation_rate_v1", "rationale_rate_v1", "age_days_v1"
                   , "delegator_retention_rate_v1", "vote_latency_median_hours_v1", "votes_by_type_v1"
                   , "gov_action_state_v1", "tally_cast_votes_v1" ],
      -- D-9: the SQL the code runs IS the SQL the endpoint publishes
      testCase "exactly six entries carry a sql_file, and each resolves and is inlined" $
        ( length [f | f <- catalog, formulaSqlFile f /= Nothing]
        , all (\f -> maybe True (\s -> lookup s metricSql /= Nothing && formulaSql f /= Just "") (formulaSqlFile f)) catalog )
          @?= (6, True),
      testCase "participation_rate_v1 publishes its denominator and null_when" $
        ( formulaField p, formulaExpression p, formulaNullWhen p, formulaVersion p )
          @?= ( "participation_rate", "|actions_voted| / |actions_eligible|", "eligible_actions = 0", 1 ),
      -- the metric note's own drift rule: every formula_id a DTO publishes exists here
      testCase "no sql_file names a file missing from the embedded set" $
        [s | f <- catalog, Just s <- [formulaSqlFile f], lookup s metricSql == Nothing] @?= []
    ]
  where p = head [f | f <- catalog, formulaFormulaId f == "participation_rate_v1"]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "metric catalog"'`
Expected: FAIL with `Could not find module 'Metrics.Catalog'`

- [ ] **Step 3: Implement `Metrics.Catalog` and the handler**

`catalogJson = $(makeRelativeToProject "resources/metric-formulas.json" >>= embedFile)` and one `embedFile` line
per `.sql` (never `embedDir`: it would not invalidate the module when a `.sql` is added, which defeats the
anti-drift point). `catalog` decodes the JSON once and resolves each entry's `sql` from `metricSql` by `sql_file`,
`error`ing at startup on an unresolvable reference. `MetricFormula`'s fields are the catalog entry's keys verbatim
(`formula_id`, `name`, `field`, `expression`, `denominator`, `null_when`, `inputs`, `edge_cases`, `sql_file`,
`spec`, `version`) plus `sql`, with `sql_file :: Maybe FilePath` and `sql :: Maybe Text` — null together on the
two column-read formulas.

**(OPEN: `resources/metric-formulas.json` is written by synthesis §3 step 16 and T-METRIC §7 enumerates only the
six SQL-backed entries, but `GovActionLifecycle.formula_id = "gov_action_state_v1"` (T-API-11) and
`VoteTally.formula_id = "tally_cast_votes_v1"` (T-API-12) are published to clients, and T-METRIC §7's drift rule
requires every DTO-referenced id to exist in the catalog. Those two entries — with `sql_file: null` — must be
added by the step-16 task; the owner of the metrics block decides whether they instead get their own `.sql` and
become eight files. Either way the id list above is the contract.)**

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-options='-p "metric catalog"'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Metrics/Catalog.hs src/test/UnitTests/MetricsCatalog.hs src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs src/exe/query-api/RestAPI.hs palmares.cabal
git commit -m "feat(api): GET /metrics/formulas served from the embedded catalog with SQL inlined"
```

---

### Task T-API-8: `GET /dreps` — the delegation-discovery list

**Files:**
- Modify: `src/exe/query-api/Query/Gov.hs`, `src/exe/query-api/RestAPI.hs`, `src/lib/offchain-lib/Palmares/DomainTypes/Transfer/{QueryResponses,Filters,OrderBy}.hs`, `scripts/smoke_query_api.sh`
- Test: `src/test/UnitTests/QueryHardening.hs` (the `DRepsOrderBy` → `SortSpec` table)

**Interfaces:**
- Consumes: `palmares.drep`, `palmares.drep_metrics`, `palmares.anchor`, `palmares.drep_epoch_power`, `palmares_idx_ocvdd_given_name_trgm`
- Produces: `DRepSummary(..)`, `DRepFilter(..)`, `drepFilterFromParams`, `DRepsOrderBy(..)`, `drepSortSpec`, `listDReps :: DRepFilter -> (DRepsOrderBy, SortOrder) -> (Limit, Offset) -> GovRead (Page DRepSummary)`, `instance ToNamedRecord DRepSummary`, `GET /dreps`

Query parameters, bounds and defaults are §1.2's table verbatim: `limit` 50 / [1,200] (CSV [1,10000]), `offset` 0 /
[0,10000] then 400 `offset_too_large`, repeatable `status` · `anchor_status`, `has_script`,
`min_voting_power` / `max_voting_power` (lovelace), `min_delegators`, `min_participation_rate` /
`min_rationale_rate` ([0,1]), `registered_before` / `registered_after`, `min_age_days`, `voted_on` /
`not_voted_on`, `language` (≤16), `active_in_epoch`, `q` ([2,100]), `order_by` default `voting_power`,
`sort_order` default **`desc`** (every DRep list is a leaderboard). `order_by=voting_power` sorts `NULLS LAST` in
both directions — a DRep registered this epoch has no `drep_distr` row and its power is unknown, not `0`.
Repeatable parameters are OR within a parameter and AND across parameters; the empty list is no filter, never
"match nothing", documented once in the Swagger top-level description. `q` matches
`off_chain_vote_drep_data.given_name` through `likePat` only (D-1 put the trigram GIN indexes on db-sync's columns,
so there is no projection table to copy text into).

- [ ] **Step 1: Write the failing test**

```bash
# --- /dreps (T-API-8) --- preprod, epoch 320
assert '/dreps?limit=1' '.total'  '294'                      # registered DReps
assert '/dreps?limit=1' '.limit'  '1'
assert '/dreps?limit=999' '.limit' '200'                     # clamped silently, applied value echoed
[ "$(code '/dreps?offset=10001')" = 400 ] || { echo "FAIL offset ceiling"; fail=1; }
assert_err '/dreps?offset=10001' '.code' '"offset_too_large"'
# the two pseudo-DReps are never in the list (D-20): they have no drep_registration row
assert '/dreps?limit=200' '[.items[].id] | map(select(startswith("drep_always"))) | length' '0'
# default sort is voting_power desc NULLS LAST
assert '/dreps?limit=3' '[.items[].voting_power] | . == (. | sort | reverse)' 'true'
assert '/dreps?order_by=voting_power&sort_order=asc&limit=200' '[.items[-1].voting_power] | .[0] == null' 'true'
# the fixture DRep, by filter
assert '/dreps?q=Bitcows' '.items[0].id' '"drep1y2qn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qmwj3xg"'
assert '/dreps?q=Bitcows' '.items[0] | [.votes_cast,.eligible_actions,.delegator_count,.voting_power_epoch,.active_until_epoch]' \
  '[16,114,13,320,333]'
assert '/dreps?q=Bitcows' '.items[0] | [.participation_rate,.rationale_rate] | map(.*1000000|round)' '[140351,125000]'
assert '/dreps?q=Bitcows' '.items[0].voting_power' '3923709956713'
assert '/dreps?q=Bitcows' '.items[0].age_days | . >= 731 and . <= 733' 'true'
# a filter that matches few rows: total must come from the byte-identical WHERE (F-17)
assert '/dreps?status=retired&limit=5' '(.items | length) <= 5 and (.total >= (.items|length))' 'true'
assert '/dreps?min_participation_rate=1.1' '[.total, (.items|length)]' '[0,0]'
# age_days is null exactly when registered_at is null (D-17) — vacuously true only if no such row is listed
assert '/dreps?limit=200' '[.items[] | select(.registered_at == null) | .age_days] | unique | . == [] or . == [null]' 'true'
assert '/dreps?limit=200' '[.items[] | select(.registered_at != null and .age_days == null)] | length' '0'
# CSV: same filters, its own ceiling, injection-guarded
curl -fsS -H 'Accept: text/csv' "$API/dreps?limit=10000" | head -1 | grep -q '^id,hex,has_script' || { echo "FAIL csv header"; fail=1; }
[ "$(curl -fsS -H 'Accept: text/csv' "$API/dreps?limit=10000" | wc -l)" -ge 295 ] || { echo "FAIL csv rows"; fail=1; }
```

```haskell
-- src/test/UnitTests/QueryHardening.hs: the sort table is a closed total function, not user text
      testCase "every DRepsOrderBy maps to a whitelisted column with a tiebreaker" $
        map (sortColumn . drepSortSpec)
          [ DRepsOrderById, DRepsOrderByVotingPower, DRepsOrderByDelegatorCount
          , DRepsOrderByParticipationRate, DRepsOrderByRationaleRate, DRepsOrderByRegisteredAt
          , DRepsOrderByVotesCast, DRepsOrderByName ]
          @?= [ "d.drep_hash_id", "m.voting_power", "m.delegator_count", "m.participation_rate"
              , "m.rationale_rate", "d.registered_slot", "m.votes_cast", "a.given_name" ],
      testCase "voting_power sorts NULLS LAST in both directions" $
        map (\o -> orderClause (drepSortSpec DRepsOrderByVotingPower) o)
          [Asc, Desc]
          @?= [ "ORDER BY m.voting_power ASC NULLS LAST, d.drep_hash_id ASC"
              , "ORDER BY m.voting_power DESC NULLS LAST, d.drep_hash_id ASC" ],
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bash scripts/smoke_query_api.sh`
Expected: FAIL with `FAIL /dreps?limit=1 .total => (curl: HTTP 404)`

- [ ] **Step 3: Implement `listDReps` and wire `GET /dreps`**

One predicate function builds the `WHERE` fragment and its `[PersistValue]`, and both the page `SELECT` and the
`COUNT(*)` take it — the page and the total cannot drift (F-17's root-cause fix). The route type carries
`Header "Accept" Text` before `Get '[JSON, CSV] (Page DRepSummary)`, and the handler passes that header to
`maxLimitFor`: servant resolves content negotiation inside the `Verb` server instance and never hands the
negotiated type to the handler, so this is the only way CSV gets its own ceiling.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-options='-p "query hardening"' && bash scripts/smoke_query_api.sh`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Gov.hs src/exe/query-api/RestAPI.hs src/lib/offchain-lib/Palmares/DomainTypes/Transfer/ src/test/UnitTests/QueryHardening.hs scripts/smoke_query_api.sh
git commit -m "feat(api): GET /dreps with the full filter set, CSV and a drift-proof total"
```

---

### Task T-API-9: the DRep profile — `/drep/{id}` ≡ `/pages/drep/{id}`, plus power, delegators and updates

**Files:**
- Modify: `src/exe/query-api/Query/Gov.hs`, `src/exe/query-api/Query/Pages.hs`, `src/exe/query-api/RestAPI.hs`, `src/lib/offchain-lib/Palmares/DomainTypes/Transfer/{QueryResponses,OrderBy}.hs`, `scripts/smoke_query_api.sh`

**Interfaces:**
- Consumes: `palmares.drep_metrics`, `palmares.drep_epoch_power`, `palmares.drep_epoch_delegators`, `palmares.live_delegation`, `palmares.drep_cert`, `palmares.vote`, `palmares.activity`, `off_chain_vote_drep_data`, `off_chain_vote_reference`
- Produces: `DRepMetrics(..)`, `VoteCounts(..)`, `EpochPowerPoint(..)`, `DelegatorStats(..)`, `DelegatorRow(..)`, `DRepUpdateRow(..)`, `AnchorContent(..)`, `AnchorReference(..)`, `DRepDetailResponse(..)`, `getDRepDetail`, `listEpochPower`, `listDelegators`, `listDRepUpdates`, `GET /drep/{drep-id}`, `GET /pages/drep/{drep-id}`, `GET /drep/{drep-id}/power`, `GET /drep/{drep-id}/delegators`, `GET /drep/{drep-id}/updates`

`/drep/{id}` and `/pages/drep/{id}` are **one handler on two routes** returning the same `DRepDetailResponse` —
`/pages/…` is the documented canonical path, the bare path is the alias for API users who do not want the page
framing. No second DTO. Composite params: `votes_limit` 25 [1,100], `delegators_limit` 10 [1,50],
`timeline_limit` 50 [1,200], `epochs` 200 [1,200]. Sub-route params: `/power` — `limit` 200 / max 200,
`from_epoch`, `to_epoch`, `order_by` fixed `epoch`, `sort_order` default `asc`, **no CSV** (200 epoch rows is one
JSON page) and **no `rank`** (a window function over all of `drep_distr` per epoch, on a Pi, that no F1 check
asks for); `/delegators` — `limit` 50/200, `offset`, `is_current` default `true`, `min_lovelace`, `order_by` ∈
`lovelace | delegated_at | stake_address` default `lovelace`, `sort_order` default `desc`, CSV enabled;
`/updates` — `limit` 50/200, `offset`, repeatable `kind` ∈ `registered | updated | retired`, `order_by` fixed
`at`, `sort_order` default `desc`. `kind` comes from `drep_registration.deposit`: `> 0` → `registered`,
`IS NULL` → `updated`, `< 0` → `retired`. `anchor_content` is omitted whenever `anchor.status /= valid`.

- [ ] **Step 1: Write the failing test**

```bash
# --- DRep profile (T-API-9) --- the §4 fixture DRep at epoch 320
D=drep1y2qn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qmwj3xg
assert "/drep/$D" '.drep.id' "\"$D\""
assert "/drep/$D" '.drep.hex' '"81378d3ea89b68f0fc09944b1f93f2feb851318f196f98ba4029d8a8"'
assert "/drep/$D" '.drep.has_script' 'false'
assert "/drep/$D" '.drep.status' '"active"'
# both routes, one handler, byte-identical body
diff <(j "/drep/$D" '.') <(j "/pages/drep/$D" '.') >/dev/null || { echo "FAIL alias diverges"; fail=1; }
assert "/drep/$D" '.metrics | [.vote_latency_median_hours, .delegator_retention_rate != null]' '[158.04,true]'
assert "/drep/$D" '.metrics.formula_ids | sort' \
  '["age_days_v1","delegator_retention_rate_v1","participation_rate_v1","rationale_rate_v1","vote_latency_median_hours_v1","votes_by_type_v1"]'
# per-type eligible_actions and votes_cast must sum to the headline figures (the Σ row IS the test)
assert "/drep/$D" '[.metrics.votes_by_type[] | .[1] | (.yes+.no+.abstain)] | add' '16'
assert "/drep/$D" '[.metrics.votes_by_type[] | select(.[0]=="treasury_withdrawals") | .[1]] | .[0] | [.yes,.no,.abstain]' '[1,5,0]'
assert "/drep/$D" '.power_by_epoch | length' '147'                              # drep_distr epochs 174..320, no gaps
assert "/drep/$D" '[.power_by_epoch[] | has("rank")] | unique' '[false]'       # rank is cut
assert "/drep/$D" '.power_by_epoch | map(.epoch) | . == (. | sort)' 'true'     # ascending epoch series
assert "/drep/$D" '.delegator_stats | [.count, .retention_rate != null]' '[13,true]'
assert "/drep/$D" '.top_delegators | length <= 10' 'true'
assert "/drep/$D" '.votes.total' '16'
assert "/drep/$D" '.votes.items[0].cast_at.slot != null' 'true'
assert "/drep/$D" '.anchor_content.given_name != null' 'true'                  # CIP-119 anchor added epoch 208
assert "/drep/$D" '.drep.anchor.status' '"valid"'
assert "/drep/$D?timeline_limit=200" '.timeline | map(.at.time) | . == (. | sort | reverse)' 'true'
# registration history: register at 173 (deposit 500000000) + update at 208 (deposit null)
assert "/drep/$D/updates" '[.items[] | [.kind, .deposit]] | sort' '[["registered",500000000],["updated",null]]'
assert "/drep/$D/updates" '.items[0].at.epoch' '208'
assert "/drep/$D/power?from_epoch=320&to_epoch=320" '.items' '[{"epoch":320,"lovelace":3923709956713,"delegator_count":13}]'
assert "/drep/$D/delegators?limit=50" '[.total, ([.items[] | select(.is_current)] | length)]' '[13,13]'
# the zero-denominator case (f1-metric §4): every DRep with no eligible action serves nulls, not zeros.
# The list-level form is the gate, because it does not depend on which DRep happens to be newest.
assert '/dreps?limit=200' '[.items[] | select(.eligible_actions == 0) | .participation_rate] | unique | . == [] or . == [null]' 'true'
Z=$(j '/dreps?order_by=registered_at&sort_order=desc&limit=1' '.items[0]')
if [ "$(echo "$Z" | jq -c '.eligible_actions')" = 0 ]; then
  assert "/drep/$(echo "$Z" | jq -r '.id')" \
    '[.metrics.participation_rate, .metrics.rationale_rate, .metrics.vote_latency_median_hours]' '[null,null,null]'
fi
[ "$(code /drep/drep1ynotarealdrepidatall000000000000000000000000000000)" = 404 ] || { echo "FAIL 404"; fail=1; }
assert_err /drep/drep1ynotarealdrepidatall000000000000000000000000000000 '.code' '"drep_not_found"'
[ "$(code /drep/pool1abc)" = 400 ] || { echo "FAIL capture"; fail=1; }
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bash scripts/smoke_query_api.sh`
Expected: FAIL with `FAIL /drep/$D .drep.id => (curl: HTTP 404)`

- [ ] **Step 3: Implement `getDRepDetail` and the three sub-routes in `Query/Gov.hs` (composite assembly in `Query/Pages.hs`)**

`getDRepDetail` issues one `runGovRead` with the seven sub-selects, so the whole page is one `RepeatableRead`
snapshot and one round trip.

- [ ] **Step 4: Run test to verify it passes**

Run: `bash scripts/smoke_query_api.sh`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Gov.hs src/exe/query-api/Query/Pages.hs src/exe/query-api/RestAPI.hs src/lib/offchain-lib/Palmares/DomainTypes/Transfer/ scripts/smoke_query_api.sh
git commit -m "feat(api): DRep profile composite plus power, delegators and update history"
```

---

### Task T-API-10: the (role, gov-action-type) → threshold-column table

**Files:**
- Create: `src/exe/query-api/Query/Thresholds.hs`, `src/test/UnitTests/GovThresholds.hs`
- Modify: `src/test/UnitTests.hs`, `palmares.cabal`

**Interfaces:**
- Consumes: `GovActionType`, `VoterRole` (T-API-1); `palmares.thresholds`; `param_proposal`
- Produces: `thresholdColumnFor :: VoterRole -> GovActionType -> Maybe Text`, `paramGroupColumns :: [(Text, Text)]`, `securityGroupColumns :: [Text]`, `spoParamChangeApplies :: [Text] -> Bool`, `drepParamChangeThreshold :: [Text] -> Maybe Text`, `govThresholdsTests`

D-25: this is a named deliverable with its own test because "`dvt_*`/`pvt_*` for the action type" is not a
function. F1 always reports the `_normal` committee column — `epoch_state.no_confidence_id` is documented
"TODO: This remains NULL", so the `_no_confidence` variant cannot be chosen from data, and predicting
ratification is cut anyway (§0.7).

- [ ] **Step 1: Write the failing test**

```haskell
-- src/test/UnitTests/GovThresholds.hs
govThresholdsTests :: TestTree
govThresholdsTests =
  testGroup
    "threshold columns"
    [ testCase "DRep column per action type" $
        map (thresholdColumnFor DRep)
          [NoConfidence, NewCommittee, NewConstitution, HardForkInitiation, TreasuryWithdrawals, InfoAction]
          @?= [ Just "dvt_motion_no_confidence", Just "dvt_committee_normal"
              , Just "dvt_update_to_constitution", Just "dvt_hard_fork_initiation"
              , Just "dvt_treasury_withdrawal", Nothing ],
      testCase "SPO column per action type; two types have none" $
        map (thresholdColumnFor SPO)
          [NoConfidence, NewCommittee, NewConstitution, HardForkInitiation, TreasuryWithdrawals, InfoAction]
          @?= [ Just "pvt_motion_no_confidence", Just "pvt_committee_normal", Nothing
              , Just "pvt_hard_fork_initiation", Nothing, Nothing ],
      -- the one column a pvt_% prefix scan silently misses. §2.6: the SPO column APPLIES only when the
      -- proposal touches a security-group parameter; the table names the column, `spoParamChangeApplies`
      -- answers whether it applies to this proposal.
      testCase "the SPO security-group column is pvtpp_security_group" $
        thresholdColumnFor SPO ParameterChange @?= Just "pvtpp_security_group",
      -- driven by the table itself, so it cannot encode a guessed membership
      testCase "the SPO security-group threshold applies iff a securityGroupColumns column is touched" $
        ( spoParamChangeApplies []
        , all (spoParamChangeApplies . (: [])) securityGroupColumns
        , spoParamChangeApplies ["drep_deposit"] )          -- governance group, never security group
          @?= (False, True, False),
      testCase "the CC never has a dvt_/pvt_ threshold: it has a quorum" $
        map (thresholdColumnFor ConstitutionalCommittee) [NoConfidence, NewCommittee, InfoAction, ParameterChange]
          @?= [Nothing, Nothing, Nothing, Nothing],
      -- parameter_change: max over the groups the proposal touches
      testCase "touched param_proposal columns map to CIP-1694 groups" $
        map (`lookup` paramGroupColumns) ["max_block_size", "min_fee_a", "max_tx_ex_mem", "drep_deposit"]
          @?= [ Just "dvt_p_p_network_group", Just "dvt_p_p_economic_group"
              , Just "dvt_p_p_technical_group", Just "dvt_p_p_gov_group" ],
      testCase "the DRep parameter_change threshold is the max over touched groups" $
        ( drepParamChangeThreshold ["max_block_size", "min_fee_a"]
        , drepParamChangeThreshold [] )
          @?= (Just "greatest(dvt_p_p_network_group, dvt_p_p_economic_group)", Nothing)
    ]
```

**(OPEN: two tables are not enumerated anywhere. (a) `paramGroupColumns`: the full `param_proposal` column →
CIP-1694 group mapping; §2.6 names the four `dvt_p_p_*_group` targets and calls it "a static table we own" but no
note lists all ~30 columns. (b) `securityGroupColumns`: the CIP-1694 SecurityGroup subset, which is what makes
`pvtpp_security_group` apply at all. Both are enumerated from `param_proposal` at 13.7.1.0 against CIP-1694
§Protocol-parameter-groups, by whoever implements this task, and the `paramGroupColumns` test is made exhaustive
over `param_proposal`'s non-bookkeeping columns so an unmapped column fails CI. No owner decision is needed — CIP-1694
is the authority — but the enumeration must exist before the test above can be written.)**

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "threshold columns"'`
Expected: FAIL with `Could not find module 'Query.Thresholds'`

- [ ] **Step 3: Implement the five total functions in `src/exe/query-api/Query/Thresholds.hs`**

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-options='-p "threshold columns"'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Thresholds.hs src/test/UnitTests/GovThresholds.hs src/test/UnitTests.hs palmares.cabal
git commit -m "feat(api): static (role, action type) to epoch_param threshold column table"
```

---

### Task T-API-11: `GET /gov-actions`

**Files:**
- Modify: `src/exe/query-api/Query/Gov.hs`, `src/exe/query-api/RestAPI.hs`, `src/lib/offchain-lib/Palmares/DomainTypes/Transfer/{QueryResponses,Filters,OrderBy}.hs`, `scripts/smoke_query_api.sh`

**Interfaces:**
- Consumes: `palmares.gov_action`, `palmares.action_window`, `palmares.anchor`, `palmares.treasury_withdrawal`, `palmares_idx_ocvgad_title_trgm`
- Produces: `GovActionSummary(..)`, `GovActionLifecycle(..)`, `GovActionFilter(..)`, `govActionFilterFromParams`, `GovActionsOrderBy(..)`, `govActionSortSpec`, `listGovActions`, `instance ToNamedRecord GovActionSummary`, `GET /gov-actions`

Params: `limit` 50/200 (CSV 10000), `offset`, repeatable `type` · `state` · `anchor_status`,
`proposed_from` / `proposed_to`, `expires_before_epoch` / `expires_after_epoch`, `proposer_stake_address`,
`min_treasury_amount` / `max_treasury_amount`, `q` ([2,100], matching `off_chain_vote_gov_action_data.title` and
`.abstract`), `order_by` ∈ `proposed_at | expires_epoch | type | deposit | treasury_amount | votes_cast` default
`proposed_at`, `sort_order` default `desc`. `state` is `gov_action_state_v1` read off the four `*_epoch` columns —
`enacted → enacted; else expired → expired; else dropped → dropped; else ratified → ratified; else proposed`.
`id` is `<tx_hash_hex>#<index>`, with the CIP-129 form alongside as `bech32`.

- [ ] **Step 1: Write the failing test**

```bash
# --- /gov-actions (T-API-11) --- 131 proposals on preprod at epoch 320
assert '/gov-actions?limit=1' '.total' '131'
assert '/gov-actions?limit=1' '.items[0].id | test("^[0-9a-f]{64}#[0-9]+$")' 'true'
assert '/gov-actions?limit=1' '.items[0].bech32 | startswith("gov_action1")' 'true'
# `state` is SINGLE-VALUED (gov_action_state_v1 checks enacted, then expired, then dropped, then ratified),
# so these are NOT the per-column counts: all 11 ratified actions were enacted the next epoch, and 102 of the
# 108 rows carrying dropped_epoch also carry expired_epoch and therefore report `expired`.
assert '/gov-actions?state=enacted&limit=1'  '.total' '11'
assert '/gov-actions?state=ratified&limit=1' '.total' '0'
assert '/gov-actions?state=expired&limit=1'  '.total' '103'
assert '/gov-actions?state=dropped&limit=1'  '.total' '6'
assert '/gov-actions?state=proposed&limit=1' '.total' '12'
# the five are a partition of all 131 rows — this is the assertion that catches a mis-ordered CASE
assert '/gov-actions?limit=200' '[.items[].lifecycle.state] | length' '131'
assert '/gov-actions?limit=200' '[.items[].lifecycle.state] | group_by(.) | map(length) | add' '131'
# every type the db enum has, and the db string is NewCommittee -> new_committee
assert '/gov-actions?limit=200' '[.items[].type] | unique | sort' \
  '["hard_fork_initiation","info_action","new_committee","new_constitution","no_confidence","parameter_change","treasury_withdrawals"]'
# govActionLifetime is 7 on preprod: expiration - proposed epoch, on all 131
assert '/gov-actions?limit=200' '[.items[] | select(.expires_epoch != null) | .expires_epoch - .proposed_at.epoch] | unique' '[7]'
assert '/gov-actions?type=treasury_withdrawals&limit=1' '.items[0].treasury_amount != null' 'true'
assert '/gov-actions?order_by=deposit&sort_order=desc&limit=5' '[.items[].deposit] | . == (. | sort | reverse)' 'true'
# an enacted action carries enacted_epoch = ratified_epoch + 1 and NULL expired/dropped
assert '/gov-actions?state=enacted&limit=1' '.items[0].lifecycle | [.enacted_epoch - .ratified_epoch, .expired_epoch, .dropped_epoch]' '[1,null,null]'
assert '/gov-actions?state=enacted&limit=1' '.items[0].lifecycle.formula_id' '"gov_action_state_v1"'
# stable paging over a tied key: 103 expired rows all share state
assert '/gov-actions?order_by=type&limit=200' '[.items[].id] | (length) == (unique | length)' 'true'
curl -fsS -H 'Accept: text/csv' "$API/gov-actions?limit=10000" | head -1 | grep -q '^id,bech32,type' || { echo "FAIL csv"; fail=1; }
```

**(OPEN: the five single-valued counts above sum to 132, one more than the 131 rows that must partition. They are
derived from f1-metric-formulas §4's per-column figures (103 rows with `expired_epoch`, 11 enacted, 6 dropped-only,
12 open), so exactly one of those four is off by one — most likely `proposed` is 11, or one enacted action also
carries `expired_epoch`. Read the five counts off the synced db-sync with
`SELECT state, count(*) FROM palmares.gov_action GROUP BY 1` before this task's test is written, keep Σ = 131, and
correct f1-metric-formulas §4 in the same commit. The implementer of this task decides nothing here — the number
comes from the database.)**

- [ ] **Step 2: Run test to verify it fails**

Run: `bash scripts/smoke_query_api.sh`
Expected: FAIL with `FAIL /gov-actions?limit=1 .total => (curl: HTTP 404)`

- [ ] **Step 3: Implement `listGovActions` and wire `GET /gov-actions`**

- [ ] **Step 4: Run test to verify it passes**

Run: `bash scripts/smoke_query_api.sh`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Gov.hs src/exe/query-api/RestAPI.hs src/lib/offchain-lib/Palmares/DomainTypes/Transfer/ scripts/smoke_query_api.sh
git commit -m "feat(api): GET /gov-actions with lifecycle state, filters and CSV"
```

---

### Task T-API-12: the gov-action page — `/gov-action/{id}` ≡ `/pages/gov-action/{id}` with tallies

**Files:**
- Modify: `src/exe/query-api/Query/Gov.hs`, `src/exe/query-api/Query/Pages.hs`, `src/exe/query-api/RestAPI.hs`, `src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs`, `scripts/smoke_query_api.sh`

**Interfaces:**
- Consumes: `palmares.vote_tally`, `palmares.vote`, `palmares.thresholds`, `palmares.treasury_withdrawal`, `palmares.gov_action`, `palmares.activity`, `param_proposal`; `Query.Thresholds` (T-API-10)
- Produces: `VoteTally(..)`, `GovActionAnchorContent(..)`, `ProposerSummary(..)`, `GovActionDetailResponse(..)`, `getGovActionDetail`, `GET /gov-action/{gov-action-id}`, `GET /pages/gov-action/{gov-action-id}`

One handler, two routes, as in T-API-9. Params: `votes_limit` 50 [1,200], `votes_order_by`, `votes_sort_order`,
`timeline_limit` 50. `tallies` carries one `VoteTally` per role **present** on the action; `basis` is always
`cast_votes_only` and its Swagger description is §2.6's verbatim sentence, because the ratio must never read as a
prediction — the authoritative outcome is `lifecycle`. Timeline entries for ratified / enacted / expired / dropped
emit an epoch-only `ChainPoint`: `epoch` = the recorded epoch, `time` = `block.time` of `MIN(block.id)` in that
epoch, `slot` and `tx_hash` `null`.

- [ ] **Step 1: Write the failing test**

```bash
# --- gov-action page (T-API-12) ---
G=$(j '/gov-actions?type=treasury_withdrawals&state=enacted&limit=1' '.items[0].id' | tr -d '"')
diff <(j "/gov-action/$G" '.') <(j "/pages/gov-action/$G" '.') >/dev/null || { echo "FAIL alias diverges"; fail=1; }
assert "/gov-action/$G" '.action.id' "\"$G\""
assert "/gov-action/$G" '[.tallies[].basis] | unique' '["cast_votes_only"]'
assert "/gov-action/$G" '[.tallies[].formula_id] | unique' '["tally_cast_votes_v1"]'
# treasury_withdrawals: DRep has a threshold column, SPO has none, CC votes by quorum
assert "/gov-action/$G" '[.tallies[] | select(.role=="drep") | .threshold_param]' '["dvt_treasury_withdrawal"]'
assert "/gov-action/$G" '[.tallies[] | select(.role=="spo") | [.threshold, .threshold_param]] | .[0] // [null,null]' '[null,null]'
assert "/gov-action/$G" '[.tallies[] | select(.role=="constitutional_committee") | .yes_power] | .[0] // 0' '0'
# ratio is yes/(yes+no) and null when the denominator is zero
assert "/gov-action/$G" '[.tallies[] | select(.yes_power + .no_power == 0) | .ratio] | unique | . == [] or . == [null]' 'true'
assert "/gov-action/$G" '.withdrawals | length >= 1' 'true'
assert "/gov-action/$G" '.withdrawals[0] | [.beneficiary | startswith("stake"), .amount > 0]' '[true,true]'
# enacted is an epoch-boundary event: no slot, no tx hash, but a real chain time
assert "/gov-action/$G" '.withdrawals[0].enacted_at | [.slot, .tx_hash, (.time != null)]' '[null,null,true]'
assert "/gov-action/$G" '[.timeline[] | select(.event_type=="gov_action_enacted") | .at.slot]' '[null]'
assert "/gov-action/$G" '.proposer | [.stake_address | startswith("stake"), .actions_count >= 1]' '[true,true]'
assert "/gov-action/$G" '.votes.total >= 0' 'true'
# a parameter_change action publishes its touched params and the max-over-groups threshold
P=$(j '/gov-actions?type=parameter_change&limit=1' '.items[0].id' | tr -d '"')
assert "/gov-action/$P" '.param_changes | length >= 1' 'true'
assert "/gov-action/$P" '[.tallies[] | select(.role=="drep") | .threshold_param] | .[0] | startswith("dvt_p_p_")' 'true'
# new_committee always reports the _normal column (epoch_state.no_confidence_id remains NULL)
C=$(j '/gov-actions?type=new_committee&limit=1' '.items[0].id' | tr -d '"')
assert "/gov-action/$C" '[.tallies[] | select(.role=="drep") | .threshold_param]' '["dvt_committee_normal"]'
[ "$(code '/gov-action/0000000000000000000000000000000000000000000000000000000000000000#0')" = 404 ] || { echo "FAIL 404"; fail=1; }
assert_err '/gov-action/0000000000000000000000000000000000000000000000000000000000000000#0' '.code' '"gov_action_not_found"'
[ "$(code /gov-action/nope)" = 400 ] || { echo "FAIL capture"; fail=1; }
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bash scripts/smoke_query_api.sh`
Expected: FAIL with `FAIL /gov-action/$G .action.id => (curl: HTTP 404)`

- [ ] **Step 3: Implement `getGovActionDetail` in `Query/Gov.hs` (composite assembly in `Query/Pages.hs`)**

`VoteTally.threshold` is read from `palmares.thresholds` at the column `thresholdColumnFor` names for the
action's epoch, and `threshold_param` echoes that column name so the number is traceable; `parameter_change`
resolves the column set through `drepParamChangeThreshold` over the non-null `param_proposal` columns.

- [ ] **Step 4: Run test to verify it passes**

Run: `bash scripts/smoke_query_api.sh`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Gov.hs src/exe/query-api/Query/Pages.hs src/exe/query-api/RestAPI.hs src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs scripts/smoke_query_api.sh
git commit -m "feat(api): gov-action page with cast-votes-only tallies, thresholds and withdrawals"
```

---

### Task T-API-13: `GET /votes` — one endpoint, four screens

**Files:**
- Modify: `src/exe/query-api/Query/Gov.hs`, `src/exe/query-api/RestAPI.hs`, `src/lib/offchain-lib/Palmares/DomainTypes/Transfer/{QueryResponses,Filters,OrderBy}.hs`, `scripts/smoke_query_api.sh`

**Interfaces:**
- Consumes: `palmares.vote` (`seq_first`, `seq_last`, `vote_rows`, `voter_hot_hash`, `voter_cold_hash`), `palmares.anchor`, `palmares.drep_epoch_power`, `pool_stat`; `Palmares.DomainTypes.Core.Ids.ccColdIdCip129`
- Produces: `VoteRecord(..)`, `VoteFilter(..)`, `voteFilterFromParams`, `VotesOrderBy(..)`, `voteSortSpec`, `listVotes`, `instance ToNamedRecord VoteRecord`, `GET /votes`

This single endpoint replaces the four per-actor vote routes (cut list). Params: `limit` 50/200 (CSV 10000),
`offset` [0,10000], repeatable `voter_role` · `voter_id` · `gov_action_id` · `verdict` · `gov_action_type` ·
`anchor_status`, `from` / `to` (chain time, inclusive), `from_epoch` / `to_epoch`, `q` ([2,100], matching rationale
title/abstract and voter name), `order_by` ∈ `cast_at | voting_power | latency | voter_id | gov_action_id` default
`cast_at`, `sort_order` default `desc`. `has_rationale` is spelled `anchor_status=valid`, not its own parameter.
D-24: for `voter_role=constitutional_committee`, `voter_id` is the **cold** CIP-129 `cc_cold1…` — the stable
identity a user can follow across hot-key rotations — with the hot hash exposed as `voter_hot_hash`, and
`voter_id=<cc_cold1…>` must match. Re-votes are real on preprod (one pair has 20 rows): the list shows every row
with `revote_count = vote_rows`, the verdict shown on a page composite is the `seq_last = 1` row, and
`latency_hours` is measured from the `seq_first = 1` row.

- [ ] **Step 1: Write the failing test**

```bash
# --- /votes (T-API-13) --- preprod totals at epoch 320
assert '/votes?voter_role=drep&limit=1' '.total' '816'
assert '/votes?voter_role=spo&limit=1'  '.total' '90'
assert '/votes?voter_role=drep&anchor_status=valid&limit=1' '.total' '65'
assert '/votes?voter_role=spo&anchor_status=valid&limit=1'  '.total' '0'
assert '/votes?voter_role=drep&limit=200' '[.items[].voter_role] | unique' '["drep"]'
assert "/votes?voter_id=$D&limit=100" '[.total, ([.items[] | select(.revote_count > 1)] | length)]' '[16,0]'
assert "/votes?voter_id=$D&limit=100" '[.items[].verdict] | group_by(.) | map([.[0], length]) | sort' \
  '[["abstain",1],["no",8],["yes",7]]'
assert "/votes?voter_id=$D&limit=100" '[.items[] | select(.anchor != null)] | length' '2'
assert "/votes?voter_id=$D&order_by=latency&sort_order=asc&limit=1" '.items[0].latency_hours | .*100|round' '129'
assert "/votes?voter_id=$D&order_by=latency&sort_order=desc&limit=1" '.items[0].latency_hours | .*100|round' '63008'
# the worst re-voted pair has 20 rows and they are all returned
assert '/votes?voter_role=drep&limit=200' '[.items[].revote_count] | max >= 1' 'true'
# CC votes are cast by hot keys but addressed by the cold id (D-24)
CC=$(j '/committee' '.members[0].cold_bech32' | tr -d '"')
assert "/votes?voter_id=$CC&limit=1" '.items[0].voter_id' "\"$CC\""
assert "/votes?voter_id=$CC&limit=1" '.items[0] | [.voter_role, (.voter_hot_hash != null)]' '["constitutional_committee",true]'
assert "/votes?voter_id=$CC&limit=1" '.total > 0' 'true'
# power is always labelled with its epoch, and both are null together
assert '/votes?voter_role=drep&limit=200' '[.items[] | select(.voting_power == null) | .voting_power_epoch] | unique | . == [] or . == [null]' 'true'
assert_err '/votes?from_epoch=321&to_epoch=320' '.code' '"invalid_epoch_range"'
curl -fsS -H 'Accept: text/csv' "$API/votes?limit=10000" | head -1 | grep -q '^gov_action_id,gov_action_type' || { echo "FAIL csv"; fail=1; }
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bash scripts/smoke_query_api.sh`
Expected: FAIL with `FAIL /votes?voter_role=drep&limit=1 .total => (curl: HTTP 404)`

- [ ] **Step 3: Implement `listVotes` and wire `GET /votes`**

`voter_id` is matched against `voter_cold_hash` for CC rows and the credential hash otherwise, so one parameter
serves all three roles.

- [ ] **Step 4: Run test to verify it passes**

Run: `bash scripts/smoke_query_api.sh`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Gov.hs src/exe/query-api/RestAPI.hs src/lib/offchain-lib/Palmares/DomainTypes/Transfer/ scripts/smoke_query_api.sh
git commit -m "feat(api): GET /votes across all three roles with cold-key CC identity"
```

---

### Task T-API-14: delegations, committee, SPOs, treasury withdrawals

**Files:**
- Modify: `src/exe/query-api/Query/Gov.hs`, `src/exe/query-api/RestAPI.hs`, `src/lib/offchain-lib/Palmares/DomainTypes/Transfer/{QueryResponses,Filters,OrderBy}.hs`, `scripts/smoke_query_api.sh`

**Interfaces:**
- Consumes: `palmares.vote_delegation`, `palmares.live_delegation`, `palmares.committee_member`, `palmares.treasury_withdrawal` (incl. `paid_heuristic`), `pool_stat`, `pool_hash`; `ccColdIdCip129` / `ccHotIdCip129`
- Produces: `DelegationRecord(..)`, `SPOSummary(..)`, `CommitteeMemberSummary(..)`, `CommitteeResponse(..)`, `TreasuryWithdrawalRecord(..)`, `listDelegations`, `getCommittee`, `listSPOs`, `listTreasuryWithdrawals`, four `ToNamedRecord` instances, `GET /delegations`, `GET /committee`, `GET /spos`, `GET /treasury-withdrawals`

Params, §1.5 verbatim. `/delegations`: `limit` `offset` `drep_id[]` `stake_address[]` `is_current` (default
`true`) `from` `to` `min_lovelace`, `order_by` ∈ `at | lovelace | stake_address` default `at`, `sort_order` `desc`.
`/committee`: no parameters, no pagination — it returns the **current** committee (`epoch_state.committee_id` for
the latest epoch, `DISTINCT`/max-`id` because a rollback across an epoch boundary can leave duplicate
`epoch_state` rows at 13.7.1.0) plus its members inline; it *is* its own composite, and there is no `/committees`
list (cut). `/spos`: `limit` `offset` `min_voting_power` `min_votes_cast` `active_in_epoch`, `order_by` ∈
`voting_power | votes_cast | pool_id`, `sort_order` `desc`, and **no `q`** (cut list, C-10: SPO rows are a direct
`pool_stat`/`pool_hash` read and `ticker` is `null` in F1, so there is nothing to match — exact id lookup is what
`/search` is for). `/treasury-withdrawals`: `limit` `offset` `gov_action_id[]` `beneficiary[]`
`enacted_from_epoch` `enacted_to_epoch` `min_amount` `max_amount`, `order_by` ∈ `amount | enacted_epoch` default
`enacted_epoch`, `sort_order` `desc`; no `/treasury-withdrawal/{id}` (cut). All four serve CSV except `/committee`.

- [ ] **Step 1: Write the failing test**

```bash
# --- delegations, committee, SPOs, treasury (T-API-14) ---
assert '/delegations?limit=1' '.total > 0' 'true'
assert '/delegations?limit=200' '[.items[].is_current] | unique' '[true]'          # is_current defaults to true
assert '/delegations?is_current=false&limit=200' '[.items[].is_current] | unique' '[false]'   # the flag filters, both ways
assert "/delegations?drep_id=$D&limit=50" '[.items[] | select(.is_current)] | length' '13'
assert '/delegations?limit=1' '.items[0].target | startswith("drep")' 'true'
assert '/delegations?limit=1' '.items[0].stake_address | startswith("stake")' 'true'
# the two reserved targets are ordinary delegation rows pointing at raw IS NULL drep_hash rows
assert '/delegations?drep_id=drep_always_abstain&limit=1' '.total > 0' 'true'
assert '/committee' '.members | length >= 1' 'true'
assert '/committee' '[.quorum_numerator, .quorum_denominator] | map(. > 0)' '[true,true]'
assert '/committee' '.members[0].cold_bech32 | startswith("cc_cold1")' 'true'
assert '/committee' '[.members[].status] | unique | map(IN("authorized","unauthorized","resigned","expired")) | unique' '[true]'
assert '/committee' '.epoch >= 320' 'true'
assert '/spos?limit=1' '.total >= 45' 'true'
assert '/spos?limit=200' '[.items[].ticker] | unique' '[null]'                     # offchain_pool_data stays disabled
assert '/spos?limit=1' '.items[0].pool_id | startswith("pool1")' 'true'
assert '/spos?limit=1' '.items[0] | [.voting_power > 0, .voting_power_epoch >= 320]' '[true,true]'
# /spos has no `q` (cut list C-10). Servant IGNORES an undeclared query param, so the observable is
# "it does not filter", not a 400 — the Swagger parameter list is checked in T-API-18.
[ "$(code '/spos?q=abc')" = 200 ] || { echo "FAIL /spos?q must be ignored, not rejected"; fail=1; }
[ "$(j '/spos?q=abc&limit=1' '.total')" = "$(j '/spos?limit=1' '.total')" ] || { echo "FAIL q filtered /spos"; fail=1; }
assert '/treasury-withdrawals?limit=1' '.total > 0' 'true'
assert '/treasury-withdrawals?limit=1' '.items[0] | [.amount > 0, (.gov_action_title != null or .gov_action_title == null)]' '[true,true]'
assert '/treasury-withdrawals?limit=200' '[.items[] | select(.enacted_at != null) | .enacted_at.slot] | unique' '[null]'
assert '/treasury-withdrawals?order_by=amount&sort_order=desc&limit=5' '[.items[].amount] | . == (. | sort | reverse)' 'true'
for p in /delegations /spos /treasury-withdrawals; do
  curl -fsS -H 'Accept: text/csv' "$API$p?limit=10000" | head -1 | grep -q ',' || { echo "FAIL csv $p"; fail=1; }
done
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bash scripts/smoke_query_api.sh`
Expected: FAIL with `FAIL /delegations?limit=1 .total > 0 => (curl: HTTP 404)`

- [ ] **Step 3: Implement the four readers and wire their routes**

`CommitteeMemberSummary.votes_cast` counts through `palmares.vote.voter_cold_hash`, the same resolved column
`/votes` filters on, so one member's history does not split across hot-key rotations.

- [ ] **Step 4: Run test to verify it passes**

Run: `bash scripts/smoke_query_api.sh`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Gov.hs src/exe/query-api/RestAPI.hs src/lib/offchain-lib/Palmares/DomainTypes/Transfer/ scripts/smoke_query_api.sh
git commit -m "feat(api): delegations, current committee, SPO voters and treasury withdrawals"
```

---

### Task T-API-15: `GET /activity` — the cursor feed

**Files:**
- Modify: `src/exe/query-api/Query/Gov.hs`, `src/exe/query-api/RestAPI.hs`, `src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs`, `scripts/smoke_query_api.sh`
- Modify: `src/test/UnitTests/GovDto.hs` (cursor parse/render)

**Interfaces:**
- Consumes: `palmares.activity` (`sort_slot`, `tx_index`, `item_index`, `kind`, `source`, `actor_id`, `target_id`, `time`, `epoch`, `tx_hash`) — including the five epoch-boundary kinds (D-19)
- Produces: `ActivityEvent(..)`, `renderCursor`, `parseCursor`, `listActivity`, `GET /activity`

**(decision, inherited) `offset` is not accepted on `/activity`** — offset paging over a growing feed
double-counts and skips. Params: `limit` 20 [1,100], `since` (exclusive lower bound, result ordered **ascending**,
for catch-up polling), `before` (exclusive upper bound, result ordered **descending**, for infinite scroll),
repeatable `event_type` · `actor` · `source`. Neither cursor → newest-first, i.e. `before` at the tip. Both → 400
`both_cursors_given`. Cursor wire format is plain text `"<sort_slot>:<tx_index>:<item_index>"`, not base64 — it is
not secret and a debuggable cursor is worth more than an opaque one; unparseable → 400 `invalid_cursor`. For
tx-borne events the triple is `block.slot_no`, `tx.block_index` and the per-kind item index; for the five
epoch-boundary kinds it is the first slot of the recorded epoch, `-1` (sorting before every tx in that epoch) and
`gov_action_proposal.id`. `pageTotal` is `null` (D-18): a `COUNT(*)` over the merged feed on every poll is waste
and no screen shows it. `next_cursor` is the last item's cursor in the direction of travel, `null` on a short page.

- [ ] **Step 1: Write the failing test**

```haskell
-- appended to src/test/UnitTests/GovDto.hs
      testCase "cursor is plain <sort_slot>:<tx_index>:<item_index>, monotone and total" $
        ( renderCursor (84233900, 12, 3), parseCursor "84233900:12:3"
        , parseCursor "84000000:-1:4711"            -- an epoch-boundary event
        , parseCursor "nope" )
          @?= ( Cursor "84233900:12:3", Right (84233900, 12, 3)
              , Right (84000000, -1, 4711), Left InvalidCursor ),
```

```bash
# --- /activity (T-API-15) ---
assert '/activity?limit=5' '.total' 'null'                        # D-18: null, never -1
assert '/activity?limit=5' '.limit' '5'
assert '/activity?limit=200' '.limit' '100'                       # ceiling
assert '/activity?limit=5' '[.items[].cursor] | map(test("^[0-9]+:-?[0-9]+:[0-9]+$")) | unique' '[true]'
assert '/activity?limit=5' '.items | map(.at.time) | . == (. | sort | reverse)' 'true'   # newest-first by default
assert '/activity?limit=5' '.next_cursor' "$(j '/activity?limit=5' '.items[-1].cursor')"
# the five epoch-boundary kinds exist in the feed (D-19) and carry slot = null, tx_index = -1
assert '/activity?limit=100&event_type=gov_action_ratified&event_type=gov_action_enacted&event_type=gov_action_expired&event_type=gov_action_dropped&event_type=treasury_withdrawal_enacted' \
  '[.items[] | [.at.slot, (.cursor | split(":") | .[1])]] | unique' '[[null,"-1"]]'
assert '/activity?limit=100' '[.items[].source] | unique | map(IN("governance","attestations")) | unique' '[true]'
assert '/activity?limit=100' '[.items[] | select(.title == "" or .title == null)] | length' '0'  # pre-rendered server-side
# since is exclusive and ascending; the two directions tile the feed with no gap and no repeat
C1=$(j '/activity?limit=10' '.items[-1].cursor' | tr -d '"')
assert "/activity?since=$C1&limit=10" '.items | map(.at.time) | . == (. | sort)' 'true'
assert "/activity?since=$C1&limit=10" "[.items[].cursor] | map(. == \"$C1\") | any" 'false'
assert "/activity?before=$C1&limit=10" "[.items[].cursor] | map(. == \"$C1\") | any" 'false'
assert_err "/activity?since=$C1&before=$C1" '.code' '"both_cursors_given"'
assert_err '/activity?since=garbage' '.code' '"invalid_cursor"'
# `offset` is not declared on /activity, and Servant IGNORES an undeclared query param: the observable is
# "it does not page", not a 400. Its absence from the parameter list is checked in T-API-18.
[ "$(code '/activity?offset=10')" = 200 ] || { echo "FAIL /activity?offset must be ignored, not rejected"; fail=1; }
[ "$(j '/activity?offset=10&limit=5' '[.items[].cursor]')" = "$(j '/activity?limit=5' '[.items[].cursor]')" ] \
  || { echo "FAIL offset paged /activity"; fail=1; }
# a three-page walk must visit each cursor exactly once — the cursor ADVANCES each time, or this proves nothing
B=$C1; A=""
for _ in 1 2 3; do
  page=$(j "/activity?limit=10&before=$B" '[.items[].cursor]')
  A="$A$(echo "$page" | jq -r '.[]')
"
  B=$(echo "$page" | jq -r '.[-1]')
done
[ "$(printf '%s' "$A" | sort | uniq -d | wc -l)" -eq 0 ] || { echo "FAIL feed repeats across pages"; fail=1; }
[ "$(printf '%s' "$A" | grep -c . )" -eq 30 ] || { echo "FAIL feed skipped a page"; fail=1; }
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "governance DTO"' && bash scripts/smoke_query_api.sh`
Expected: FAIL with `Variable not in scope: renderCursor`

- [ ] **Step 3: Implement `listActivity`, `parseCursor` and `renderCursor`**

The `WHERE` is a lexicographic comparison on the stored triple
(`(sort_slot, tx_index, item_index) > (?, ?, ?)` for `since`, `<` for `before`), and `ORDER BY` is that same
triple in the direction of travel — which is why the cursor must be total: the comparison is the paging key, not
an approximation of the timestamp.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-options='-p "governance DTO"' && bash scripts/smoke_query_api.sh`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Gov.hs src/exe/query-api/RestAPI.hs src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs src/test/UnitTests/GovDto.hs scripts/smoke_query_api.sh
git commit -m "feat(api): GET /activity with a total cursor over tx and epoch-boundary events"
```

---

### Task T-API-16: `GET /search`

**Files:**
- Modify: `src/exe/query-api/Query/Gov.hs`, `src/exe/query-api/RestAPI.hs`, `src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs`, `scripts/smoke_query_api.sh`

**Interfaces:**
- Consumes: `palmares.drep`, `palmares.gov_action`, `palmares.committee_member`, `pool_hash`, `palmares_idx_ocvdd_given_name_trgm`, `palmares_idx_ocvgad_title_trgm`; `Query.Common.likePat`
- Produces: `SearchResults(..)`, `SearchGroup(..)`, `SearchHit(..)`, `SearchTarget(..)`, `searchQuery`, `GET /search`

`q` is **required**, trimmed length [2,100] → 400 `query_too_short` / `query_too_long`. `kinds` (default all) and
`limit_per_group` 5 [1,20] — the ⌘K palette shows five per group. `SearchHit` keeps `{id, title, subtitle, badges,
status, target}` and `SearchTarget` keeps `{kind, id}` so the FE router maps `kind` to a route unchanged. In Plan A
the `profiles` and `attestations` groups serve empty (Plan B fills them). Every `%…%` match goes through `likePat`
and lands on a column covered by one of the two trigram GIN indexes; a `pool1…` or `drep1…` paste is matched by
**exact prefix**, not trigram, because an id is not text search.

- [ ] **Step 1: Write the failing test**

```bash
# --- /search (T-API-16) ---
assert '/search?q=Bitcows' '.query' '"Bitcows"'
assert '/search?q=Bitcows' '.dreps.items[0].id' "\"$D\""
assert '/search?q=Bitcows' '.dreps.items[0].target' "{\"kind\":\"drep\",\"id\":\"$D\"}"
assert '/search?q=Bitcows' '.total >= 1' 'true'
assert '/search?q=Bitcows&limit_per_group=20' '[.dreps,.gov_actions,.spos,.committee_members,.profiles,.attestations] | map(.items | length <= 20) | unique' '[true]'
assert '/search?q=Bitcows' '[.profiles.items, .attestations.items] | flatten | length' '0'   # Plan B fills these
assert "/search?q=$D" '.dreps.items[0].id' "\"$D\""                                           # exact id paste
assert_err '/search?q=a' '.code' '"query_too_short"'
assert_err "/search?q=$(printf 'x%.0s' {1..101})" '.code' '"query_too_long"'
# a LIKE metacharacter must be matched literally, not as a wildcard
assert '/search?q=100%25' '.total >= 0' 'true'
assert '/search?q=_a' '.total >= 0' 'true'
[ "$(code /search)" = 400 ] || { echo "FAIL q is required"; fail=1; }
assert '/search?q=Bitcows&kinds=drep' '[.gov_actions.items, .spos.items] | flatten | length' '0'
```

**(OPEN: `kinds` lists `stake_address` (§1.7) but `SearchResults` (§2.10) has no `stake_addresses` group — either
add the group or drop the kind. The six-group record is the one the FE's ⌘K palette renders, so dropping the kind
is the smaller change.)**

- [ ] **Step 2: Run test to verify it fails**

Run: `bash scripts/smoke_query_api.sh`
Expected: FAIL with `FAIL /search?q=Bitcows .query => (curl: HTTP 404)`

- [ ] **Step 3: Implement `searchQuery` and wire `GET /search`**

One `runGovRead` with one `SELECT` per requested kind, each `LIMIT limit_per_group`, so the palette costs one round
trip and one snapshot.

- [ ] **Step 4: Run test to verify it passes**

Run: `bash scripts/smoke_query_api.sh`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Gov.hs src/exe/query-api/RestAPI.hs src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs scripts/smoke_query_api.sh
git commit -m "feat(api): GET /search over DReps, gov actions, SPOs and committee members"
```

---

### Task T-API-17: the two remaining page composites — `/pages/drep-explorer` and `/pages/dashboard`

**Files:**
- Modify: `src/exe/query-api/Query/Pages.hs`, `src/exe/query-api/Query/Aggregates.hs`, `src/exe/query-api/RestAPI.hs`, `src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs`, `scripts/smoke_query_api.sh`

**Interfaces:**
- Consumes: `listDReps` + `drepFilterFromParams` (T-API-8), `listGovActions` (T-API-11), `listActivity` (T-API-15), `palmares.drep_epoch_power` (incl. `target_kind`), `palmares.treasury_withdrawal`, `palmares.drep_metrics`
- Produces: `DRepExplorerPageResponse(..)`, `PowerBucket(..)`, `DashboardPageResponse(..)`, `EpochParticipationPoint(..)`, `getDRepExplorerPage`, `getDashboardPage`, `GET /pages/drep-explorer`, `GET /pages/dashboard`

Only these two are left: the DRep profile and gov-action composites are their detail endpoints (T-API-9, T-API-12),
and §3.5's six candidates stay unbuilt — `/pages/gov-actions-explorer` (`/gov-actions` + `Page.total` *is* the
screen; there is no facet sidebar in the F1 design), `/pages/committee`, `/pages/treasury`, `/pages/profile/{id}`,
`/pages/activity` and `/pages/graph`. `/pages/drep-explorer` takes `GET /dreps`'s filter and sort set through the
**same** `drepFilterFromParams` (one source of truth) and adds the four facet lists the sidebar needs; facets are
computed over the filter set **with that facet's own predicate removed**, so the sidebar can show "Retired (41)"
while `status=active` is applied. `/pages/dashboard` params: `top_limit` 10 [1,50], `closing_soon_limit` 5 [1,20],
`activity_limit` 10 [1,50], `epochs` 20 [1,100]; `closing_soon` is `state = proposed` expiring within 5 epochs.
The always-abstain / always-no-confidence split reads straight off `palmares.drep_epoch_power` by `target_kind` —
the one view with no filter (D-20), which is also why `/dreps` and every rate filter must say
`target_kind = 'drep'`. Neither composite serves CSV (nobody exports a page composite).

- [ ] **Step 1: Write the failing test**

```bash
# --- composites (T-API-17) ---
assert '/pages/drep-explorer?limit=10' '[.items | length, .limit, .offset]' '[10,10,0]'
assert '/pages/drep-explorer?limit=1' '.total' '294'
# the facet keys are a subset of DRepStatus and sum to the unfiltered list total; `reserved` is 0 or absent,
# because the two pseudo-DReps have no drep_registration row and never enter /dreps (D-20)
assert '/pages/drep-explorer?limit=1' '[.status_facets[] | .[0]] | inside(["active","inactive","retired","reserved"])' 'true'
assert '/pages/drep-explorer?limit=1' '[.status_facets[] | .[1]] | add' '294'
assert '/pages/drep-explorer?limit=1' '[.status_facets[] | select(.[0]=="reserved") | .[1]] | . == [] or . == [0]' 'true'
# facet semantics: the facet's own predicate is removed, so retired is still counted under status=active
assert '/pages/drep-explorer?status=active&limit=1' '[.status_facets[] | select(.[0]=="retired") | .[1]] | .[0] > 0' 'true'
assert '/pages/drep-explorer?status=active&limit=1' '[.items[].status] | unique' '["active"]'
assert '/pages/drep-explorer?limit=1' '[.anchor_facets[] | .[0]] | map(IN("valid","invalid_schema","hash_mismatch","oversized","unreachable","inline","not_fetched")) | unique' '[true]'
assert '/pages/drep-explorer?limit=1' '.power_buckets | length >= 1' 'true'
assert '/pages/drep-explorer?limit=1' '[.power_buckets[] | .count] | add <= 294' 'true'
assert '/pages/drep-explorer?limit=1' '.epoch >= 320' 'true'
assert '/pages/dashboard' '.total_dreps' '294'
assert '/pages/dashboard' '[.active_dreps + .retired_dreps <= .total_dreps]' '[true]'
# the spec's headline fact, read off drep_epoch_power by target_kind. Epoch-319 fixture:
# always_abstain = 431863790957959, always_no_confidence = 325390800383 — bounds are within 1 % of those,
# because the dashboard reads the current epoch and the parked stake drifts between snapshots.
assert '/pages/dashboard' '.always_abstain_lovelace > 427000000000000' 'true'
assert '/pages/dashboard' '.always_no_confidence_lovelace > 322000000000' 'true'
assert '/pages/dashboard' '.actions_in_flight' '12'
assert '/pages/dashboard' '[.actions_by_type[] | .[1]] | add' '131'
assert '/pages/dashboard?epochs=20' '.participation_by_epoch | length' '20'
assert '/pages/dashboard?epochs=20' '.participation_by_epoch | map(.epoch) | . == (. | sort)' 'true'
assert '/pages/dashboard?top_limit=10' '.top_dreps_by_power | length' '10'
assert '/pages/dashboard?top_limit=10' '[.top_dreps_by_power[].id] | map(startswith("drep_always")) | any' 'false'
assert '/pages/dashboard?closing_soon_limit=5' '[.closing_soon[] | .lifecycle.state] | unique | . == [] or . == ["proposed"]' 'true'
assert '/pages/dashboard?activity_limit=10' '.recent_activity | length' '10'
assert '/pages/dashboard' '[.attestation_count, .claimed_profile_count]' '[0,0]'   # zero until Plan B
assert '/pages/dashboard' '.treasury_withdrawn_lovelace > 0' 'true'
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bash scripts/smoke_query_api.sh`
Expected: FAIL with `FAIL /pages/drep-explorer?limit=10 [.items | length, .limit, .offset] => (curl: HTTP 404)`

- [ ] **Step 3: Implement `getDRepExplorerPage` and `getDashboardPage`**

Each composite is one `runGovRead`: the explorer reuses `listDReps` plus four facet `SELECT`s built from the same
predicate with one clause dropped; the dashboard's sixteen figures are one `SELECT` per group, not per row, and
`participation_by_epoch` comes from `palmares.drep_epoch_power` joined to the per-epoch vote counts.

- [ ] **Step 4: Run test to verify it passes**

Run: `bash scripts/smoke_query_api.sh`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Pages.hs src/exe/query-api/Query/Aggregates.hs src/exe/query-api/RestAPI.hs src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs scripts/smoke_query_api.sh
git commit -m "feat(api): DRep explorer facets and the dashboard composite"
```

---

### Task T-API-18: `query-api --write-swagger-only` and the Swagger parity guard

**Files:**
- Modify: `src/exe/query-api/Main.hs`, `src/exe/query-api/RestAPI.hs`, `.github/workflows/ci.yml`, `.gitignore`
- Create: `docs/generated/swagger/query-swagger-api.json` (generated, committed)

**Interfaces:**
- Consumes: `apiSwagger` (every route family above)
- Produces: the `--write-swagger-only` flag, the committed `docs/generated/swagger/query-swagger-api.json`, CI job `swagger-parity`

This is a real new task, not a house reuse: the Belt System writes the file **unconditionally at startup**
(`src/exe/query-api/Main.hs:36`) *after* config and DB wiring and then serves forever, which CI cannot use. The
flag must be parsed before `decodeConfigEnvOrFile`, write the file from the pure `apiSwagger` value, touch no
config and no database, and `exitSuccess`; the unconditional startup write is **removed** in the same change, so
the file has exactly one producer. Two consequences the plan must carry: `.gitignore` ignores `docs/generated/*`
(extraction §1.9), so the guard needs `!docs/generated/swagger/query-swagger-api.json` un-ignored or there is
nothing to diff; and the Swagger version is `"1.0.0"`, set in `RestAPI.hs` as
`apiSwagger & info . Data.Swagger.version .~ "1.0.0"`, independent of the app version — `cz bump` must not touch
it. Bump it by the §5 SemVer rule: removing or renaming a field or endpoint, or tightening a type → **major**; new
endpoint, new optional field, new enum value → **minor**; description or example only → **patch**. Swagger
metadata also lands here: title `"Cardano Governance Registry Query API"`, license `"Apache-2.0"`, spec at
`GET /swagger-api.json`, UI at `GET /swagger-ui`, the `ApiError` schema on `allOperations . responses . at`
400/404/429/503 with the `ErrorCode` enum in `definitions`, `addSharedSwaggerDescriptions` extended with `DRepId`,
`GovActionId`, `ChainPoint`, `Cursor`, `AnchorStatus`, `VoterRole`, `GovActionType` and `TallyBasis`, the
repeatable-parameter convention documented once in the top-level description, and the `TallyBasis` sentence from
§2.6 verbatim.

- [ ] **Step 1: Write the failing test**

```yaml
# .github/workflows/ci.yml — a new job; the guard IS the test
  swagger-parity:
    runs-on: ubuntu-24.04-arm
    steps:
      - uses: actions/checkout@v4
      - uses: haskell-actions/setup@v2
        with: { ghc-version: '9.6.6' }
      - run: cabal build exe:query-api
      # must not need Postgres, Atlas config or a port
      - run: cabal run query-api -- --write-swagger-only
      - run: git diff --exit-code docs/generated/swagger/
```

```bash
# local reproduction of the same guard
cabal run query-api -- --write-swagger-only
git diff --exit-code docs/generated/swagger/
jq -e '.info.version == "1.0.0"'                                   docs/generated/swagger/query-swagger-api.json
jq -e '.info.title == "Cardano Governance Registry Query API"'     docs/generated/swagger/query-swagger-api.json
jq -e '.info.license.name == "Apache-2.0"'                         docs/generated/swagger/query-swagger-api.json
jq -e '.definitions.ErrorCode.enum | length == 24'                 docs/generated/swagger/query-swagger-api.json
# 22 documented paths: the 18 governance routes + /health /ready /status /metrics/formulas. The two
# /pages/… aliases are paths of their own; `/swagger-api.json` and `/swagger-ui` are the UI, not CoreFunctionAPI.
jq -e '[.paths | keys[]] | length == 22'                           docs/generated/swagger/query-swagger-api.json
jq -e '.paths["/dreps"].get.parameters | map(select(.name == "Accept")) | .[0].required == false' \
                                                                   docs/generated/swagger/query-swagger-api.json
# q is cut on /spos only (cut list C-10) and kept on the other four
jq -e '[.paths["/spos"].get.parameters[].name] | index("q") == null' docs/generated/swagger/query-swagger-api.json
jq -e '[.paths["/dreps"].get.parameters[].name] | index("q") != null' docs/generated/swagger/query-swagger-api.json
# /activity takes cursors, never offset (T-API-15)
jq -e '[.paths["/activity"].get.parameters[].name] | index("offset") == null' docs/generated/swagger/query-swagger-api.json
jq -e '.definitions.VoterRole.enum == ["drep","spo","constitutional_committee"]' \
                                                                   docs/generated/swagger/query-swagger-api.json
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal run query-api -- --write-swagger-only`
Expected: FAIL — the flag is unrecognised, so the binary falls through to normal startup and dies with
`Atlas configuration failed` without writing anything

- [ ] **Step 3: Implement the flag in `src/exe/query-api/Main.hs` and the metadata in `RestAPI.hs`**

`main = getArgs >>= \case ["--write-swagger-only"] -> writeSwagger >> exitSuccess; _ -> serve` — the flag branch
comes first and runs no `withCfgProviders`, no pool creation and no `getPortFromEnvOrDefault`, which is precisely
what makes it usable in CI.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal run query-api -- --write-swagger-only && git diff --exit-code docs/generated/swagger/`
Expected: PASS (exit 0, no diff)

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Main.hs src/exe/query-api/RestAPI.hs docs/generated/swagger/query-swagger-api.json .github/workflows/ci.yml .gitignore
git commit -m "feat(query-api): --write-swagger-only flag and a CI Swagger parity guard"
```

---

### Block interfaces — produces

**Domain types** (`Palmares.DomainTypes.Core.Types`): `DRepId`, `GovActionId`, `ChainPoint(..)`,
`DRepStatus`, `DRepUpdateKind`, `GovActionType`, `GovActionState`, `VoterRole`, `Vote`, `TallyBasis`,
`AnchorStatus` (7 values), `ActivitySource`, `ActivityEventType`, `SearchKind`, `CommitteeMemberStatus`,
`FromHttpApiData` instances for `DRepId` and `GovActionId`.

**Envelope** (`Palmares.WebAPI.Envelope`, in `webapi-lib`, re-exported from `QueryResponses`): `Cursor`,
`Page(..)` (`pageTotal :: Maybe Int`), `ApiError(..)`, `ErrorCode(..)` (24 values), `errorCodeToHttpStatus`.

**DTOs** (`Palmares.DomainTypes.Transfer.QueryResponses`): `AnchorRef(..)` (+ `cip100_valid`), `AnchorContent(..)`,
`AnchorReference(..)`, `DRepSummary(..)`, `DRepMetrics(..)`, `VoteCounts(..)`, `EpochPowerPoint(..)` (no `rank`),
`DelegatorStats(..)`, `DelegatorRow(..)`, `DRepUpdateRow(..)`, `DRepDetailResponse(..)`, `GovActionSummary(..)`,
`GovActionLifecycle(..)`, `GovActionAnchorContent(..)`, `VoteTally(..)`, `ProposerSummary(..)`,
`GovActionDetailResponse(..)`, `VoteRecord(..)`, `DelegationRecord(..)`, `SPOSummary(..)`,
`CommitteeMemberSummary(..)`, `CommitteeResponse(..)`, `TreasuryWithdrawalRecord(..)`, `ActivityEvent(..)`,
`SearchResults(..)`, `SearchGroup(..)`, `SearchHit(..)`, `SearchTarget(..)`, `SyncStatus(..)`,
`StatusResponse(..)`, `EpochParamsSummary(..)`, `MetricFormula(..)`, `DRepExplorerPageResponse(..)`,
`PowerBucket(..)`, `DashboardPageResponse(..)`, `EpochParticipationPoint(..)`.

**Filters and sorts** (`Transfer.Filters`, `Transfer.OrderBy`): `DRepFilter` / `drepFilterFromParams` /
`DRepsOrderBy`, `GovActionFilter` / `govActionFilterFromParams` / `GovActionsOrderBy`, `VoteFilter` /
`voteFilterFromParams` / `VotesOrderBy`, plus the delegation, SPO, treasury, delegator and update sort enums.

**webapi-lib**: `Palmares.WebAPI.Envelope` (above; new `deriving-aeson` dependency),
`Palmares.WebAPI.CSV` (`CSV`, `Accept CSV`, `MimeRender CSV (Page a)`, `safeCell`),
`mkServantErr :: Int -> ErrorCode -> ServerError`, `genericErrorMessage :: ErrorCode -> Text`,
`corsExposedHeaders` carrying the five `X-*` freshness headers, `Freshness(..)`, `freshnessMiddleware`.

**query-api**: `Query.Common` (`normalizeLimitOffset`, `maxLimitFor`, `activityMaxLimit`, `likePat`, `SortSpec`,
`orderClause`, `normalizeQ`, `checkRanges`), `Query.Views.applyViewDdl`, `Query.Refresh.startMetricsRefresh`,
`Query.Gov` (`queryJson`, `queryJson1`, `runGovRead`, `listDReps`, `getDRepDetail`, `listEpochPower`,
`listDelegators`, `listDRepUpdates`, `listGovActions`, `getGovActionDetail`, `listVotes`, `listDelegations`,
`getCommittee`, `listSPOs`, `listTreasuryWithdrawals`, `listActivity`, `searchQuery`, `getStatusQuery`,
`parseCursor`, `renderCursor`), `Query.Thresholds` (`thresholdColumnFor`, `paramGroupColumns`, `securityGroupColumns`,
`spoParamChangeApplies`, `drepParamChangeThreshold`), `Metrics.Catalog` (`catalog`, `metricSql`), `Query.Pages`
(`getDRepExplorerPage`, `getDashboardPage`), `RestAPI` (`FullAPI`, `CoreFunctionAPI`, `apiSwagger`, `mkApp`),
the `--write-swagger-only` flag.

**Routes** (22 documented paths, plus the Swagger UI pair): `/health`, `/ready`, `/status`, `/metrics/formulas`, `/dreps`, `/drep/{drep-id}`,
`/drep/{drep-id}/power`, `/drep/{drep-id}/delegators`, `/drep/{drep-id}/updates`, `/gov-actions`,
`/gov-action/{gov-action-id}`, `/votes`, `/delegations`, `/committee`, `/spos`, `/treasury-withdrawals`,
`/activity`, `/search`, `/pages/dashboard`, `/pages/drep-explorer`, `/pages/drep/{drep-id}`,
`/pages/gov-action/{gov-action-id}`, `/swagger-api.json` (+ `/swagger-ui`).

**Artifacts**: `docs/generated/swagger/query-swagger-api.json` (committed, version `1.0.0`, the FE's `gen:api`
input), `scripts/smoke_query_api.sh`, `scripts/smoke_views.sh`, CI job `swagger-parity`.

**Env vars**: `PALMARES_READY_MAX_LAG_SECONDS` (300), `PALMARES_AMBER_LAG_SECONDS` (120),
`PALMARES_VIEW_REFRESH_SECONDS` (300), `PALMARES_FRESHNESS_POLL_MS` (5000), `PALMARES_NETWORK_NAME` (`preprod`),
`PALMARES_PG_OWNER_CONN_STR`; unchanged: `PORT` (8083), `PG_CONN_STR`, `CORS_ALLOWED_ORIGINS`.

**Consumes from other blocks**: `Palmares.DomainTypes.Core.Ids.{drepIdCip129, drepIdCip105, ccHotIdCip129,
ccColdIdCip129, govActionIdCip129, parseDrepIdAny}`; `sql/000`–`sql/030` and the **17** views
`palmares.{sync_state, drep, drep_cert, drep_interval, action_window, gov_action, vote, vote_tally, anchor,
vote_delegation, live_delegation, drep_epoch_delegators, drep_epoch_power, committee_member,
treasury_withdrawal, thresholds, activity}`, the one matview `palmares.drep_metrics` (with the UNIQUE index
`CONCURRENTLY` needs) and the function `palmares.eligible_actions(...)`;
`sql/040_indexes.sql`'s `palmares_idx_*` (notably the two trigram GIN indexes); roles `palmares_owner` /
`palmares_ro`; `resources/metric-formulas.json` + `resources/metrics/*.sql`;
`test/fixtures/preprod-epoch-320-metrics.json`; `Palmares.WebAPI.{Utils, CORS, ServiceProbe}`;
`Utils.mkStripPrefixSchemaOptions`; `Constants.appVersion`; `QueryAppMonad.verifyProjectionDbConnection`.

**Left to Plan B** (each a minor Swagger bump, nothing here is rewritten): `DRepDetailResponse.{profile,
attestations, disputes}`, `ProposerSummary.profile_id`, `StatusResponse.{attestations, protocol}`, `/ready`'s
Kupo-lag check, `SearchResults.{profiles, attestations}` content, `palmares.activity`'s `source=attestations`
arm, the `/profiles` · `/attestations` · `/disputes` route families and their two CSV instances.

### Block open items

- **(OPEN: `bitcowsCip105` in T-API-1 is a placeholder — the fixture gives the CIP-129 id and the raw hex but not
  `drep_hash.view`; read the real CIP-105 string out of `drep_hash.view` for that credential once db-sync is
  synced, and the `gov_action1qnergwfc` vector is truncated in f1-dbsync §1.3 — take the full bech32 from the same
  query.)**
- **(OPEN: query-api needs a second pool on `PALMARES_PG_OWNER_CONN_STR` to apply the DDL and refresh the matview
  as `palmares_owner` while serving reads as `palmares_ro`. Owner decides: two DSNs as planned, or
  `GRANT palmares_owner TO palmares_ro` and one DSN — which gives the public read path DDL rights. Build two.)**
- **(OPEN: `REFRESH MATERIALIZED VIEW CONCURRENTLY` requires a UNIQUE index on the matview and no note specifies
  one. `sql/030_metrics.sql` (step 16) must create `palmares_drep_metrics_pk UNIQUE` on `drep_hash_id`, or the
  refresh thread fails at runtime, not at boot.)**
- **(OPEN: T-API §6 spells the poll interval `FRESHNESS_POLL_MS`; T-EXTRACT §5.1 requires the `PALMARES_` prefix on
  every new env var, so it is `PALMARES_FRESHNESS_POLL_MS` here. Same for `PALMARES_NETWORK_NAME`, which no note
  names — `/ready` check 4 needs an expected value to compare `meta.network_name` against, and
  `StatusResponse.network` echoes it.)**
- **(OPEN: `paramGroupColumns` and `securityGroupColumns` need the full `param_proposal` column → CIP-1694 group
  mapping and its SecurityGroup subset; §2.6 names the four `dvt_p_p_*_group` targets and calls the mapping "a
  static table we own" but no note enumerates all ~30 columns. Enumerated by the T-API-10 implementer from
  `param_proposal` at 13.7.1.0 against CIP-1694 §Protocol-parameter-groups; the `paramGroupColumns` test is
  exhaustive over `param_proposal`'s non-bookkeeping columns so an unmapped column fails CI.)**
- **(OPEN: the five single-valued `/gov-actions?state=` counts derived from f1-metric-formulas §4 sum to 132, not
  131 — one of (103 expired, 11 enacted, 6 dropped-only, 12 open) is off by one. Read
  `SELECT state, count(*) FROM palmares.gov_action GROUP BY 1` off the synced db-sync, pin the five with Σ = 131,
  and correct f1-metric-formulas §4 in the same commit. Decided by the database, not by the implementer.)**
- **(OPEN: `resources/metric-formulas.json` must also carry `gov_action_state_v1` and `tally_cast_votes_v1`
  (`sql_file: null`), because T-API-11 and T-API-12 publish those ids and T-METRIC §7's drift rule requires every
  DTO-referenced id to exist in the catalog. Owner: the synthesis §3 step-16 task.)**
- **(OPEN: `kinds` lists `stake_address` (§1.7) but `SearchResults` (§2.10) has no `stake_addresses` group —
  either add the group or drop the kind. The six-group record is the one the FE's ⌘K palette renders, so dropping
  the kind is the smaller change.)**
- **(OPEN: `GET /graph` (§1.8, `root` · `kind` · `depth` [1,3] · `min_lovelace` · `max_nodes`) is not on the cut
  list and is not in F1 check 12's endpoint list, but check 13's Cytoscape screen needs it. No block in Plan A
  owns it — assign the delegation half to a Plan A addendum or to Plan C alongside the screen.)**
- **(OPEN: `palmares.vote` must expose `revote_count` (T-METRIC §0.3's `vote_rows`) under that name for
  `VoteRecord.revote_count`; §2.7's DTO listing omits the field although §0.3 decides it is surfaced. Confirm the
  column name when `sql/010` is written.)**
*(Closed while reviewing: the `tip_time` cast is owned by `Query/Gov.hs`'s projection list — T-DATA §5.1 pre-casts
only `lag_seconds`, and this block's plumbing rule (3) in T-API-6 already casts every timestamp it projects.)*

## Block 6 — External parity against Koios

# Plan A — block: external parity against Koios (F1 check 6)

This block delivers F1 check 6 — "on ≥ 20 DReps and ≥ 10 gov actions the votes/delegations agree with Koios
preprod, 0 unexplained divergences" — as `admin reconcile-koios`, a pure comparison library in
`offchain-lib`, a pinned (never random) sample, a committed waiver file, a thin shell wrapper and a nightly
warn-only workflow. It implements synthesis §3 Phase 5 item 25 and the canonical names of §1.3
(`scripts/reconcile_koios.sh` over `admin reconcile-koios`, `GET /status`, `PALMARES_READY_MAX_LAG_SECONDS`,
GHCR + hosted arm64 runners, no self-hosted runner); **D-20** (the pseudo-DRep totals: R3 compares the
matched real-DRep set and R4 compares `target_kind IN (…)` totals against Koios's *unfiltered* sum, because
`drep_always_abstain` alone is 46 % of the epoch-319 total); **D-7** (300 s as the freshness bound);
**D-6/D-8** (CI shape); and T-DATA §8.2's R1–R10 numbering, with its R3/R4/R6/R7 corrections. Assumptions:
the preprod stack is live and caught up (synthesis §3 steps 11, 12, 15, 16, 18 done); the comparison runs
against **our own public API, never raw SQL**, so it covers the whole read path including CIP-129 encoding;
**Koios is a check, never a source** — it runs on cardano-db-sync too, so a divergence says "one of us is
wrong", never "we are wrong", and nothing in this block may fall back to it when our sync is behind.

---

### Task T-PARITY-1: Koios preprod wire decoders

**Files:**
- Create: `src/lib/offchain-lib/Palmares/Reconcile/Koios.hs`
- Create: `src/test/UnitTests/Reconcile.hs`
- Create: `test/fixtures/koios/{tip,drep_list,drep_info,drep_voting_power_history,proposal_list,proposal_votes,drep_votes,committee_info}.json`
  — **single-id, hand-recorded wire fixtures; not the replay snapshot pair, which lives under
  `test/fixtures/snapshot/` (T-PARITY-5) so the dumper can never overwrite these pinned counts.**
- Modify: `src/test/UnitTests.hs` (import + one entry in the `unitTests` list)
- Modify: `palmares.cabal` (`library offchain-lib` exposed-modules; `test-suite test` other-modules; `extra-source-files: test/fixtures/koios/*.json`)
- Test: `src/test/UnitTests/Reconcile.hs`

**Interfaces:**
- Consumes: `Palmares.DomainTypes.Transfer.QueryResponses` (`GovActionType`, `Vote`, `VoterRole`,
  `DRepStatus`) — synthesis §3 step 18.
- Produces: `KoiosTip`, `KoiosDRepListRow`, `KoiosDRepInfo`, `KoiosPowerRow`, `KoiosProposal`,
  `KoiosWithdrawal`, `KoiosVoteRow`, `KoiosDRepVoteRow`, `KoiosCommittee`, `KoiosCommitteeMember`,
  `koiosActionType`, `koiosDRepStatus`, `koiosVote`, `koiosVoterRole`; `reconcileTests :: TestTree`;
  the `test/fixtures/koios/` layout.

- [ ] **Step 1: Write the failing test**

Record the eight fixtures first (`K=https://preprod.koios.rest/api/v1`; these exact lines go in the test
module header, they are the only way a later contributor can refresh them):

```bash
K=https://preprod.koios.rest/api/v1
D=drep1y2qn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qmwj3xg
F=test/fixtures/koios
# $A = an action some DRep voted on twice, so the "latest-only" assertion has a re-vote to catch.
# T-METRIC §3.2 measured 69 such pairs on preprod, so this cannot come back empty (OPEN item 3).
A=$(curl -s "$K/vote_list?limit=1000" \
      | jq -r 'group_by(.voter_id + .proposal_id)[] | select(length > 1) | .[0].proposal_id' | head -1)
curl -s "$K/tip"                                              > $F/tip.json
curl -s "$K/drep_list?registered=eq.true&limit=1000"          > $F/drep_list.json
curl -s -H 'content-type: application/json' -d "{\"_drep_ids\":[\"$D\"]}" "$K/drep_info" > $F/drep_info.json
curl -s "$K/drep_voting_power_history?_epoch_no=319&limit=1000" > $F/drep_voting_power_history.json
curl -s "$K/proposal_list?limit=1000"                         > $F/proposal_list.json
curl -s "$K/committee_info"                                   > $F/committee_info.json
jq -n --argjson v "$(curl -s "$K/proposal_votes?_proposal_id=$A")" --arg k "$A" '{($k): $v}' > $F/proposal_votes.json
jq -n --argjson v "$(curl -s "$K/drep_votes?_drep_id=$D")"       --arg k "$D" '{($k): $v}' > $F/drep_votes.json

# the three numbers the notes disagree on, read off the recording before step 4 (OPEN items 2 and 5)
jq -r '.members[].status' $F/committee_info.json | sort -u
jq '[.[]|select(.expired_epoch!=null)]|length,
    [.[]|select(.ratified_epoch==null and .enacted_epoch==null
            and .expired_epoch==null and .dropped_epoch==null)]|length' $F/proposal_list.json
```

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- | Parity against Koios preprod (F1 check 6). Koios is a CHECK, never a source.
-- test/fixtures/koios/ holds the SINGLE-ID wire fixtures recorded by the curl block above;
-- refresh them with those commands. The multi-id replay snapshot pair is a different set,
-- written by `admin reconcile-koios --dump-snapshots test/fixtures/snapshot` (T-PARITY-5);
-- never point the dumper at this directory, it would flatten the pinned single-id counts.
module UnitTests.Reconcile (reconcileTests) where

import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Palmares.DomainTypes.Transfer.QueryResponses
  (DRepStatus (..), GovActionType (..), Vote (..), VoterRole (..))
import Palmares.Reconcile.Koios
import Test.Tasty
import Test.Tasty.HUnit

load :: (FromJSON a) => FilePath -> IO a
load p = eitherDecodeFileStrict ("test/fixtures/" <> p) >>= either assertFailure pure

koiosWireTests :: TestTree
koiosWireTests =
  testGroup
    "Koios wire"
    [ testCase "tip decodes to the pinned preprod point" $ do
        t <- load "koios/tip.json"
        (koiosTipEpoch t, koiosTipAbsSlot t, koiosTipBlockNo t) @?= (320, 136754769, 5311567),
      testCase "drep_list?registered=eq.true has 294 rows, all CIP-129" $ do
        rs <- load "koios/drep_list.json"
        length rs @?= 294
        assertBool "all drep1…" (all (T.isPrefixOf "drep1" . koiosDrepListDrepId) rs),
      testCase "epoch-319 power: 195 rows, and the two pseudo-DReps carry 46% of the total" $ do
        rs <- load "koios/drep_voting_power_history.json"
        length rs @?= 195
        sum (map koiosPowerAmount rs) @?= 937803900128796
        let byId = Map.fromList [(koiosPowerDrepId r, koiosPowerAmount r) | r <- rs]
        Map.lookup "drep_always_abstain" byId @?= Just 431863790957959
        Map.lookup "drep_always_no_confidence" byId @?= Just 325390800383,
      testCase "proposal_list: 131 rows, lifecycle shape, 7-epoch lifetime" $ do
        ps <- load "koios/proposal_list.json"
        length ps @?= 131
        length (filter ((/= Nothing) . koiosProposalRatifiedEpoch) ps) @?= 11
        length (filter ((/= Nothing) . koiosProposalEnactedEpoch) ps) @?= 11
        length [p | p <- ps, koiosProposalDroppedEpoch p /= Nothing,
                    koiosProposalExpiredEpoch p == Nothing, koiosProposalEnactedEpoch p == Nothing] @?= 6
        length (filter ((/= Nothing) . koiosProposalExpiredEpoch) ps) @?= 102
        length [p | p <- ps, all (== Nothing) [koiosProposalRatifiedEpoch p, koiosProposalEnactedEpoch p,
                                               koiosProposalExpiredEpoch p, koiosProposalDroppedEpoch p]] @?= 12
        assertBool "govActionLifetime = 7 on every preprod row"
          (all (\p -> koiosProposalExpiration p == Just (koiosProposalProposedEpoch p + 7)) ps)
        assertBool "all gov_action1…" (all (T.isPrefixOf "gov_action1" . koiosProposalProposalId) ps),
      testCase "drep_info for the fixture DRep" $ do
        is <- load "koios/drep_info.json"
        case is of
          [i] -> do
            koiosDrepInfoRegistered i @?= True
            koiosDrepInfoActive i @?= True
            koiosDrepInfoHasScript i @?= False
            koiosDrepInfoExpiresEpochNo i @?= Just 333
            koiosDrepInfoAmount i @?= Just 3923709956713
            koiosDrepInfoLiveDelegatorCount i @?= Just 13
          _ -> assertFailure "expected exactly one drep_info row",
      testCase "drep_votes is FULL history: 16 rows, 0 re-votes, yes 7 / no 8 / abstain 1" $ do
        m <- load "koios/drep_votes.json"
        let vs = concat (Map.elems (m :: Map.Map Text [KoiosDRepVoteRow]))
        length vs @?= 16
        length (nub (map koiosDrepVoteProposalId vs)) @?= 16
        map (\v -> length (filter ((== v) . koiosDrepVoteVote) vs)) ["Yes", "No", "Abstain"] @?= [7, 8, 1],
      testCase "proposal_votes is LATEST-only: one row per voter" $ do
        m <- load "koios/proposal_votes.json"
        let vs = concat (Map.elems (m :: Map.Map Text [KoiosVoteRow]))
        length (nub (map koiosVoteVoterId vs)) @?= length vs,
      testCase "committee_info: preprod is 2-of-3, all script creds, expiring 372" $ do
        c <- load "koios/committee_info.json"
        (koiosCommitteeQuorumNumerator c, koiosCommitteeQuorumDenominator c) @?= (2, 3)
        length (koiosCommitteeMembers c) @?= 3
        assertBool "all cold creds are scripts"
          (all koiosMemberCcColdHasScript (koiosCommitteeMembers c))
        assertBool "all expire at 372"
          (all ((== Just 372) . koiosMemberExpirationEpoch) (koiosCommitteeMembers c)),
      testCase "proposal_type is db-sync's enum: NewCommittee, never UpdateCommittee" $ do
        koiosActionType "NewCommittee" @?= Just NewCommittee
        koiosActionType "TreasuryWithdrawals" @?= Just TreasuryWithdrawals
        koiosActionType "UpdateCommittee" @?= Nothing,
      testCase "vote and voterrole are db-sync's enum spellings, not our wire values" $ do
        map koiosVote ["Yes", "No", "Abstain", "yes"] @?= [Just Yes, Just No, Just Abstain, Nothing]
        map koiosVoterRole ["DRep", "SPO", "ConstitutionalCommittee", "drep"]
          @?= [Just DRep, Just SPO, Just ConstitutionalCommittee, Nothing],
      testCase "(registered, active) maps onto the three registration states DRepStatus has" $ do
        koiosDRepStatus True True @?= Just DRepActive
        koiosDRepStatus True False @?= Just DRepInactive
        koiosDRepStatus False False @?= Just DRepRetired
        koiosDRepStatus False True @?= Nothing
    ]

reconcileTests :: TestTree
reconcileTests = testGroup "Reconcile" [koiosWireTests]
```

(`DRepReserved` is unreachable from `(registered, active)` — the two pseudo-DReps are identified by
their literal `drep_always_*` id, not by this pair, so `koiosDRepStatus` is deliberately 3-valued.)

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-show-details=direct --test-options='-p "/Koios wire/"'`
Expected: FAIL with `error: [GHC-87110] Could not find module ‘Palmares.Reconcile.Koios’`

- [ ] **Step 3: Implement the ten wire records and four mappings in `src/lib/offchain-lib/Palmares/Reconcile/Koios.hs`**

`FromJSON` only (we never send Koios anything but `_drep_ids`), via the house mechanism — the field prefix is
the type's own word, so `CamelToSnake` lands exactly on Koios's names:

```haskell
data KoiosDRepInfo = KoiosDRepInfo
  { koiosDrepInfoDrepId :: Text, koiosDrepInfoRegistered :: Bool, koiosDrepInfoActive :: Bool
  , koiosDrepInfoHasScript :: Bool, koiosDrepInfoExpiresEpochNo :: Maybe Int
  , koiosDrepInfoAmount :: Maybe Integer, koiosDrepInfoLiveDelegatorCount :: Maybe Int }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "koiosDrepInfo", CamelToSnake]] KoiosDRepInfo
```

Prefixes, one rule: **the type's own name, lower-camel, `koios` included** — `koiosTip`,
`koiosDrepList`, `koiosDrepInfo`, `koiosPower`, `koiosProposal`, `koiosWithdrawal`, `koiosVote`,
`koiosDrepVote`, `koiosCommittee`, `koiosMember`. The `koios` is load-bearing, not decoration:
`palmares.cabal` sets no `DuplicateRecordFields`, and the shorter prefixes collide inside `offchain-lib`
with `VoteRecord.voteVoterId`, `TreasuryWithdrawalRecord.withdrawalAmount` and
`CommitteeResponse.committeeQuorumNumerator` / `.committeeMembers`, all of which `Palmares.Reconcile`
imports in T-PARITY-3. Every lifecycle epoch and every `meta_*` is `Maybe`;
`proposal_list[].withdrawal` is `[KoiosWithdrawal]` defaulting to `[]`.
`koiosActionType`/`koiosVote`/`koiosVoterRole`/`koiosDRepStatus` are total `Maybe`-returning lookups —
never a partial `read`.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-show-details=direct --test-options='-p "/Koios wire/"'`
Expected: PASS (11 cases)

- [ ] **Step 5: Commit**

```bash
git add src/lib/offchain-lib/Palmares/Reconcile/Koios.hs src/test/UnitTests/Reconcile.hs \
        src/test/UnitTests.hs test/fixtures/koios palmares.cabal
git commit -m "feat(reconcile): Koios preprod wire decoders + recorded fixtures"
```

---

### Task T-PARITY-2: comparison primitive, waiver file, freshness guard, report

**Files:**
- Create: `src/lib/offchain-lib/Palmares/Reconcile.hs`
- Create: `config/reconcile-exceptions.yaml`
- Modify: `src/test/UnitTests/Reconcile.hs` (add `primitiveTests` to the group)
- Modify: `palmares.cabal` (`library offchain-lib`: exposed-modules + `build-depends: yaml, filepath`)
- Test: `src/test/UnitTests/Reconcile.hs`

**Interfaces:**
- Consumes: nothing outside `base`/`aeson`/`containers`/`yaml`.
- Produces: `RowId (R1…R10)`, `Divergence`, `RowStatus`, `isSkipped`, `RowResult`, `ExceptionEntry`,
  `Freshness`, `ReportStatus`, `Report`, `diffKeyed`, `applyExceptions`, `staleExceptions`, `mkRow`,
  `skippedRow`, `checkFreshness`, `mkReport`, `reportExitCode`, `loadExceptions`;
  `config/reconcile-exceptions.yaml` and its schema.

- [ ] **Step 1: Write the failing test**

```haskell
primitiveTests :: TestTree
primitiveTests =
  testGroup
    "Reconcile primitives"
    [ testCase "diffKeyed reports one-sided keys as a membership divergence" $ do
        let k = Map.fromList [("a", 1 :: Int), ("b", 2)]
            p = Map.fromList [("b", 2 :: Int), ("c", 3)]
            ds = diffKeyed id (\x y -> [("amount", tshow x, tshow y) | x /= y]) k p
        map (\d -> (divergenceKey d, divergenceField d, divergenceKoios d, divergencePalmares d)) ds
          @?= [ ("a", "membership", "present", "absent"),
                ("c", "membership", "absent", "present") ],
      testCase "diffKeyed reports only the fields the comparator returns" $ do
        let k = Map.fromList [("a", 10 :: Int)]
            p = Map.fromList [("a", 11 :: Int)]
            ds = diffKeyed id (\x y -> [("amount", tshow x, tshow y) | x /= y]) k p
        map (\d -> (divergenceField d, divergenceKoios d, divergencePalmares d)) ds
          @?= [("amount", "10", "11")],
      testCase "an exception on (row, key, field) explains that divergence and nothing else" $ do
        let d1 = Divergence "a" "amount" "10" "11"
            d2 = Divergence "b" "amount" "10" "11"
            e = ExceptionEntry R3 "a" (Just "amount") "known" (fromGregorian 2026 10 20) "it@blazarlabs.io"
        applyExceptions [e] R3 [d1, d2] @?= ([d2], [d1])
        applyExceptions [e] R5 [d1, d2] @?= ([d1, d2], []),
      testCase "an exception with no field explains every field of that key" $ do
        let e = ExceptionEntry R2 "a" Nothing "known" (fromGregorian 2026 10 20) "it@blazarlabs.io"
        applyExceptions [e] R2 [Divergence "a" "delegator_count" "13" "12"]
          @?= ([], [Divergence "a" "delegator_count" "13" "12"]),
      testCase "an exception that matched nothing is reported stale" $ do
        let e = ExceptionEntry R9 "gone" Nothing "stale" (fromGregorian 2026 10 20) "it@blazarlabs.io"
        staleExceptions [e] [mkRow [] R9 7 []] @?= [e],
      testCase "R10 aborts on epoch skew > 1 or lag > the bound, and only then" $ do
        checkFreshness 320 320 41 300 @?= Fresh
        checkFreshness 320 321 41 300 @?= Fresh
        checkFreshness 318 320 41 300 @?= Aborted "epoch skew 2 > 1 (ours 318, koios 320)"
        checkFreshness 320 320 412 300 @?= Aborted "lag 412s > 300s",
      testCase "exit code: 0 clean or explained, 1 unexplained, 3 aborted" $ do
        let t = UTCTime (fromGregorian 2026 10 20) 0
            ok = mkReport "preprod" t 320 41 320 [] [mkRow [] R1 294 []]
            bad = mkReport "preprod" t 320 41 320 [] [mkRow [] R1 294 [Divergence "a" "membership" "present" "absent"]]
            skew = "epoch skew 2 > 1 (ours 318, koios 320)"
            aborted = mkReport "preprod" t 318 41 320 []
              [mkRow [] R10 1 [Divergence "freshness" "epoch_skew" "320" "318"], skippedRow R1 skew]
        (reportStatus ok, reportUnexplained ok, reportExitCode ok) @?= (ReportOk, 0, 0)
        (reportStatus bad, reportUnexplained bad, reportExitCode bad) @?= (ReportDivergent, 1, 1)
        (reportStatus aborted, reportExitCode aborted) @?= (ReportAborted, 3),
      testCase "the committed waiver file parses and every entry carries reason/opened/owner" $ do
        es <- loadExceptions "config/reconcile-exceptions.yaml" >>= either assertFailure pure
        assertBool "no empty reason" (all (not . T.null . exceptionReason) es)
        assertBool "no empty owner" (all (not . T.null . exceptionOwner) es)
    ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-show-details=direct --test-options='-p "/Reconcile primitives/"'`
Expected: FAIL with `error: [GHC-87110] Could not find module ‘Palmares.Reconcile’`

- [ ] **Step 3: Implement the types, `diffKeyed`, the waiver file and the report in `src/lib/offchain-lib/Palmares/Reconcile.hs`**

One comparison primitive, because all ten rows are keyed comparisons:

```haskell
-- | The only comparison primitive. A key on one side only becomes a @membership@ divergence; a key on
-- both sides is handed to the field comparator, which returns ONLY the fields that differ, as
-- (field, koios, palmares). R9's multiset equality is this with k = (address, amount) and v = count.
diffKeyed :: (Ord k) => (k -> Text) -> (a -> b -> [(Text, Text, Text)]) -> Map k a -> Map k b -> [Divergence]
```

The wire names are `koios` and `palmares`, **not** `expected`/`actual` (T-DATA §8.2 wrote the latter): the
report must not read as "Koios is right". `reportExitCode`: 0 ok, 1 unexplained, 3 aborted — **2 is reserved
for transport failure and is produced by the exe, never here**. `ExceptionEntry` is `FromJSON` over
`Data.Yaml.decodeFileEither`, with `exceptionRow :: RowId` decoding `"R1"…"R10"`.

`RowStatus = RowOk | RowDivergent | RowSkipped Text`; `isSkipped` is its predicate and `skippedRow` its
constructor. `mkReport` returns `ReportAborted` **whenever any row is `RowSkipped`** — that is the only
path to exit 3, so the exe needs no abort branch of its own and a skipped row can never be counted as a
pass. (This is why there is no separate `abortedReport`: T-PARITY-3's `reconcileAll` already emits the
skipped rows, and a second report constructor would be a second place for the two to drift.)

`config/reconcile-exceptions.yaml`, seeded empty but with the schema in comments:

```yaml
# Documented divergences against Koios preprod (F1 check 6: "explained divergences are documented").
# An entry here turns one divergence from `unexplained` into `explained`. Nothing else suppresses a row.
# An entry that matches no divergence in a run is reported as `stale_exceptions` — delete it.
# row:    R1 … R10
# key:    the row's comparison key verbatim as the report prints it (a CIP-129 id, "<id>:<epoch>", …)
# field:  one field name; omit to explain every field of that key
# reason / opened / owner: all three required.
exceptions: []
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-show-details=direct --test-options='-p "/Reconcile primitives/"'`
Expected: PASS (8 cases)

- [ ] **Step 5: Commit**

```bash
git add src/lib/offchain-lib/Palmares/Reconcile.hs config/reconcile-exceptions.yaml \
        src/test/UnitTests/Reconcile.hs palmares.cabal
git commit -m "feat(reconcile): comparison primitive, waiver file, freshness guard, report"
```

---

### Task T-PARITY-3: the ten rows — `reconcileAll`

**Files:**
- Modify: `src/lib/offchain-lib/Palmares/Reconcile.hs` (add the snapshots + `reconcileAll`)
- Modify: `src/test/UnitTests/Reconcile.hs` (add `rowTests`)
- Test: `src/test/UnitTests/Reconcile.hs`

**Interfaces:**
- Consumes: `Palmares.Reconcile.Koios` (T-PARITY-1); `diffKeyed`, `mkRow`, `skippedRow`, `isSkipped`,
  `applyExceptions`, `checkFreshness` (T-PARITY-2);
  the DTOs of synthesis §3 step 18 — `StatusResponse` (`governance.epoch`, `governance.lag_seconds`),
  `DRepSummary` (`id`, `has_script`, `status`, `active_until_epoch`, `delegator_count`), `EpochPowerPoint`
  (`epoch`, `lovelace`), `GovActionSummary` (`bech32`, `type`, `proposed_at.epoch`, `expires_epoch`,
  `lifecycle.*_epoch`), `GovActionDetailResponse` (`tallies[].{role,counts}`,
  `withdrawals[].{beneficiary,amount}`), `VoteRecord` (`gov_action_id`, `verdict`, `cast_at.tx_hash`),
  `CommitteeResponse` (`quorum_*`, `members[].{cold_bech32,hot_hash,expiration_epoch}`);
  **`GET /votes` must return every `palmares.vote` row (full history, not `seq_last = 1`)** — R7 is the gate
  on that; `palmares.drep_epoch_power` carrying `target_kind` with no filter (D-20);
  `Palmares.DomainTypes.Core.Ids` (CIP-129) for the ids both sides join on (synthesis §3 step 7).
- Produces: `KoiosSnapshot`, `PalmaresSnapshot`, `reconcileAll`, `loadKoiosSnapshot`,
  `loadPalmaresSnapshot`, and the `test/fixtures/snapshot/{koios,palmares}/` layout the dumper must write.

- [ ] **Step 1: Write the failing test**

Hand-built minimal snapshots (two DReps `drep1a…`/`drep1b…` + the two pseudo-DReps, one action, one
committee member, one withdrawal) — no recording, fully deterministic. `kOk`/`pOk` are the consistent
pair; each case applies one `set*`/`drop*`/`dup*` mutation to it, and those mutators are plain record
updates in the test module, not library code.

```haskell
rowTests :: TestTree
rowTests =
  testGroup
    "Reconcile rows"
    [ testCase "a consistent snapshot pair yields all ten rows ok" $ do
        let rs = reconcileAll kOk pOk []   -- kOk / pOk: the consistent hand-built pair
        map rowId rs @?= [R1, R2, R3, R4, R5, R6, R7, R8, R9, R10]
        map rowStatus rs @?= replicate 10 RowOk
        sum (map (length . rowUnexplained) rs) @?= 0,
      testCase "R1: a DRep Koios has and we do not is one membership divergence" $ do
        let rs = reconcileAll kOk (dropDRep "drep1b…" pOk) []
            r1 = row R1 rs
        rowStatus r1 @?= RowDivergent
        rowUnexplained r1 @?= [Divergence "drep1b…" "membership" "present" "absent"],
      testCase "R2: expires_epoch_no is compared against active_until_epoch" $ do
        let r2 = row R2 (reconcileAll kOk (setActiveUntil "drep1a…" 334 pOk) [])
        rowUnexplained r2 @?= [Divergence "drep1a…" "active_until_epoch" "333" "334"],
      testCase "R3: voting power is exact lovelace, keyed by (drep_id, epoch)" $ do
        let r3 = row R3 (reconcileAll kOk (setPower "drep1a…" 319 3923709956712 pOk) [])
        rowUnexplained r3
          @?= [Divergence "drep1a…:319" "lovelace" "3923709956713" "3923709956712"],
      testCase "R4: the per-epoch total includes BOTH pseudo-DReps (D-20)" $ do
        let r4 = row R4 (reconcileAll kOk (dropDRep "drep_always_abstain" pOk) [])
        rowStatus r4 @?= RowDivergent
        rowUnexplained r4
          @?= [Divergence "319" "total_lovelace" "937803900128796" "505940109170837"],
      testCase "R5: lifecycle epochs are exact per gov_action1… id" $ do
        let r5 = row R5 (reconcileAll kOk (setRatified "gov_action1…" (Just 229) pOk) [])
        rowUnexplained r5 @?= [Divergence "gov_action1…" "ratified_epoch" "228" "229"],
      testCase "R6: the tally map is (role, verdict) -> count, latest vote only" $ do
        let r6 = row R6 (reconcileAll kOk (setTally "gov_action1…" DRep Yes 6 pOk) [])
        rowUnexplained r6 @?= [Divergence "gov_action1…:drep:yes" "count" "7" "6"],
      testCase "R7: the vote-history set keeps superseded votes" $ do
        let r7 = row R7 (reconcileAll kOk (dropVote "drep1a…" pOk) [])
        rowUnexplained r7
          @?= [Divergence "gov_action1…:yes:0bb1…" "membership" "present" "absent"],
      testCase "R8: quorum fraction and hot hex per cold id" $ do
        let r8 = row R8 (reconcileAll kOk (setQuorum 3 3 pOk) [])
        rowUnexplained r8 @?= [Divergence "committee" "quorum" "2/3" "3/3"],
      testCase "R9: withdrawals are a (beneficiary, amount) multiset" $ do
        let r9 = row R9 (reconcileAll kOk (dupWithdrawal "gov_action1…" pOk) [])
        rowUnexplained r9 @?= [Divergence "stake_test1…:120000000000" "count" "1" "2"],
      testCase "R10 aborting skips every other row rather than faking a pass" $ do
        let rs = reconcileAll (setKoiosTipEpoch 322 kOk) pOk []
        rowStatus (row R10 rs) @?= RowDivergent
        assertBool "all others skipped"
          (all (\r -> rowId r == R10 || isSkipped (rowStatus r)) rs),
      testCase "a waived divergence becomes explained and the run exits 0" $ do
        let e = ExceptionEntry R2 "drep1a…" (Just "delegator_count") "Koios counts at its own tip"
                  (fromGregorian 2026 10 20) "it@blazarlabs.io"
            rs = reconcileAll kOk (setDelegators "drep1a…" 12 pOk) [e]
            rp = mkReport "preprod" t0 320 41 320 [e] rs
        rowStatus (row R2 rs) @?= RowOk
        (reportUnexplained rp, reportExplained rp, reportExitCode rp) @?= (0, 1, 0)
    ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-show-details=direct --test-options='-p "/Reconcile rows/"'`
Expected: FAIL with `Variable not in scope: reconcileAll`

- [ ] **Step 3: Implement `reconcileAll :: KoiosSnapshot -> PalmaresSnapshot -> [ExceptionEntry] -> [RowResult]`**

`reconcileAll` evaluates R10 first and, on `Aborted msg`, returns `R10` divergent plus
`RowSkipped msg` for R1–R9 — comparing across an epoch boundary manufactures divergences, so a skipped row
must never read as a pass. Each row is one `diffKeyed` call; the table below is the full specification and
is the one thing the signature and tests do not determine. Koios base `https://preprod.koios.rest/api/v1`;
`E` ranges over the **five completed** epochs `[tip−5 … tip−1]` (315–319 at tip 320).

| Row | Koios endpoint → fields | Palmares endpoint → fields | Key | Compared | Tolerance |
|---|---|---|---|---|---|
| **R1** | `GET /drep_list?registered=eq.true` → `drep_id` | `GET /dreps?status=active&status=inactive&limit=200` → `items[].id` | CIP-129 `drep1…` | set membership (294 at epoch 320) | **exact**, zero symmetric difference |
| **R2** | `POST /drep_info` `{"_drep_ids":[≤100]}` → `has_script`, `expires_epoch_no`, `registered`, `active`, `live_delegator_count` | the same `/dreps` items → `has_script`, `active_until_epoch`, `status`, `delegator_count` | CIP-129 `drep1…` | `has_script`; `expires_epoch_no == active_until_epoch`; `koiosDRepStatus registered active == status`; `live_delegator_count == delegator_count` | **exact** on the first three; see OPEN for `delegator_count` |
| **R3** | `GET /drep_voting_power_history?_epoch_no=E` → `drep_id`, `epoch_no`, `amount` | `GET /drep/{id}/power?from_epoch=315&to_epoch=319` → `items[].{epoch,lovelace}`, for every R1 id **and** `drep_always_abstain` / `drep_always_no_confidence` | `"<drep_id>:<epoch>"` | `amount == lovelace` | **exact lovelace** — both sides read `drep_distr`, so a tolerance would hide a bug |
| **R4** | the same rows, **unfiltered** sum per epoch | Σ over the same R3 rows, i.e. `target_kind IN ('drep','always_abstain','always_no_confidence')` (**D-20**) | `"<epoch>"` | Σ `amount`. Epoch 319 reference: 937 803 900 128 796, of which always_abstain 431 863 790 957 959 (46 %) and always_no_confidence 325 390 800 383 | **exact (0)** — this *is* spec check 5's "documented tolerance", and it is zero; the 46 % it used to need was the pseudo-DRep filter |
| **R5** | `GET /proposal_list` → `proposal_id`, `proposal_type`, `proposed_epoch`, `expiration`, `ratified/enacted/dropped/expired_epoch` | `GET /gov-actions?limit=200` → `items[].{bech32,type,proposed_at.epoch,expires_epoch,lifecycle.*}` | CIP-129 `gov_action1…` | `koiosActionType proposal_type == type`, plus the five epochs. Population-wide (all 131), no sample | **exact** |
| **R6** | `GET /proposal_votes?_proposal_id=` (**latest per voter**) | `GET /gov-action/{id}` → `tallies[].{role,counts.{yes,no,abstain}}` (computed over `palmares.vote WHERE seq_last = 1`) | `"<gov_action1…>:<role>:<verdict>"` | the count map | **exact** |
| **R7** | `GET /drep_votes?_drep_id=` (**full history, superseded votes included**) | `GET /votes?voter_id=<id>&limit=200` → `items[].{gov_action_id,verdict,cast_at.tx_hash}` | `"<gov_action1…>:<verdict>:<vote_tx_hash>"` | set membership | **exact**. DRep role only: preprod carries 3 243 802 CC vote rows of test spam, so no row compares CC history |
| **R8** | `GET /committee_info` → `quorum_numerator/denominator`, `members[].{cc_cold_id,cc_hot_hex,expiration_epoch}` | `GET /committee` → `quorum_*`, `members[].{cold_bech32,hot_hash,expiration_epoch}` | `"committee"` for the quorum, CIP-129 `cc_cold1…` per member | `"<num>/<den>"`; `cc_hot_hex == hot_hash`; `expiration_epoch`. Preprod: 2/3, 3 members, all 372, all script creds | **exact**; member `status` excluded, see OPEN |
| **R9** | `proposal_list[].withdrawal[]` → `stake_address`, `amount` | `GET /gov-action/{id}` → `withdrawals[].{beneficiary,amount}` | `"<stake_address>:<amount>"` → count | multiset equality, for every action with a non-empty `withdrawal[]` | **exact** |
| **R10** | `GET /tip` → `epoch_no` | `GET /status` → `governance.{epoch,lag_seconds}` | `"freshness"` | guard, not a comparison | abort when `abs(Δepoch) > 1` or `lag_seconds > PALMARES_READY_MAX_LAG_SECONDS` (**300**, D-7) |

`deposit` is deliberately **not** in R2 (T-DATA §8.2 listed it): `DRepSummary` carries no `deposit`, it is the
protocol constant `epoch_param.drep_deposit`, and fetching it would cost 294 extra `/drep/{id}/updates`
calls to compare a constant.

`loadKoiosSnapshot`/`loadPalmaresSnapshot :: FilePath -> IO (Either String a)` read one file per field, so
the recorded layout lives in exactly one place:

```
test/fixtures/snapshot/koios/     tip.json drep_list.json drep_info.json
                                  drep_voting_power_history.json proposal_list.json
                                  proposal_votes.json drep_votes.json committee_info.json
test/fixtures/snapshot/palmares/  status.json dreps.json drep_power.json gov_actions.json
                                  gov_action_detail.json votes.json committee.json
```
Under `snapshot/`, **not** `test/fixtures/koios/` — that directory holds T-PARITY-1's single-id wire
fixtures, whose pinned counts (16 vote rows, one `drep_info` row) the multi-id dump would destroy.
The five keyed files (`proposal_votes`, `drep_votes`, `drep_power`, `gov_action_detail`, `votes`) are one
JSON object keyed by the id; the flat ones are the concatenated rows / `Page.items`.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-show-details=direct --test-options='-p "/Reconcile rows/"'`
Expected: PASS (12 cases)

- [ ] **Step 5: Commit**

```bash
git add src/lib/offchain-lib/Palmares/Reconcile.hs src/test/UnitTests/Reconcile.hs
git commit -m "feat(reconcile): the ten R1-R10 parity rows over both snapshots"
```

---

### Task T-PARITY-4: the pinned sample

**Files:**
- Create: `config/reconcile-sample.preprod.json`
- Modify: `src/lib/offchain-lib/Palmares/Reconcile.hs` (add `ReconcileSample` + `loadSample`)
- Modify: `src/test/UnitTests/Reconcile.hs` (add `sampleTests`)
- Modify: `palmares.cabal` (`extra-source-files: config/reconcile-sample.preprod.json`)
- Test: `src/test/UnitTests/Reconcile.hs`

**Interfaces:**
- Consumes: a caught-up preprod stack (synthesis §3 step 11) and the `/dreps` + `/gov-actions`
  `order_by` values `votes_cast`, `registered_at`, `proposed_at`, `treasury_amount` (synthesis §3
  step 18) to draw the ids once.
- Produces: `ReconcileSample` (`sampleNetwork`, `sampleRecordedAtEpoch`, `sampleVoteHistoryDreps`,
  `sampleTallyActions`), `loadSample`, `config/reconcile-sample.preprod.json`.

- [ ] **Step 1: Write the failing test**

CI gates the spec floor on the shipped file, so the floor can never silently regress:

```haskell
sampleTests :: TestTree
sampleTests =
  testGroup
    "Reconcile sample"
    [ testCase "the shipped sample meets spec F1 check 6's floor and is pinned, not drawn" $ do
        s <- loadSample "config/reconcile-sample.preprod.json" >>= either assertFailure pure
        sampleNetwork s @?= "preprod"
        sampleRecordedAtEpoch s @?= 320
        assertBool ">= 20 DReps (spec floor)" (length (sampleVoteHistoryDreps s) >= 20)
        assertBool ">= 10 gov actions (spec floor)" (length (sampleTallyActions s) >= 10)
        assertBool "all CIP-129 drep ids" (all (T.isPrefixOf "drep1") (sampleVoteHistoryDreps s))
        assertBool "all CIP-129 gov action ids" (all (T.isPrefixOf "gov_action1") (sampleTallyActions s))
        assertBool "no duplicate drep id" (nub (sampleVoteHistoryDreps s) == sampleVoteHistoryDreps s)
        assertBool "no duplicate action id" (nub (sampleTallyActions s) == sampleTallyActions s)
        assertBool "the worked-example DRep is in the sample"
          ("drep1y2qn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qmwj3xg" `elem` sampleVoteHistoryDreps s)
    ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-show-details=direct --test-options='-p "/Reconcile sample/"'`
Expected: FAIL with `config/reconcile-sample.preprod.json: openBinaryFile: does not exist`

- [ ] **Step 3: Add `ReconcileSample` + `loadSample`, then draw the sample once and commit it**

`ReconcileSample` is `FromJSON` via the house `StripPrefix "sample" + CamelToSnake`, so its four fields
land on `network`, `recorded_at_epoch`, `vote_history_dreps`, `tally_actions`;
`loadSample :: FilePath -> IO (Either String ReconcileSample)` is `eitherDecodeFileStrict`.

No randomness anywhere — a shell script cannot draw a reproducible random sample, and T-DATA §7.2's
"fixed-seed random 10" is replaced by a second deterministic selector over a different population. Both
lists are sorted by their key so the file is stable under regeneration:

- `vote_history_dreps` (R7) = the **30 DReps with the most vote rows**, ordered by `(vote_rows DESC,
  drep_id ASC)` — the interesting tail — **union** the **10 most recently registered DReps with ≥ 1 vote**,
  ordered by `(registered_at DESC, drep_id ASC)` — the newest cohort, which the tail never covers —
  **union the worked-example DRep** `drep1y2qn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qmwj3xg`
  (T-METRIC §4), explicitly, because its 16 votes are not guaranteed to reach the top 30 and the test
  below asserts it is present.
- `tally_actions` (R6, R9) = the union of: all actions with `ratified_epoch IS NOT NULL` (11), all
  `dropped_epoch`-only actions (6), the 5 treasury withdrawals with the largest `withdrawal[]` total, and
  the 2 oldest bootstrap-era actions — every lifecycle shape and the whole withdrawal path, ≈ 24 ids.

Drawn from our own API, with **one** Koios call — the re-voting-DRep line, which only picks an *id* that
both sides are then compared on, never a value. The last command is what makes the draw reproducible:
`sort -u` gives both lists one canonical order, so a redraw either matches the committed file
byte-for-byte or shows a real population change in `git diff`.

```bash
P=https://gov.cardano.vip/api
curl -s "$P/dreps?status=active&status=inactive&order_by=votes_cast&sort_order=desc&limit=30" \
  | jq -r '.items[].id' > /tmp/tail.txt
curl -s "$P/dreps?status=active&status=inactive&order_by=registered_at&sort_order=desc&limit=200" \
  | jq -r '.items[] | select(.votes_cast > 0) | .id' | head -10 > /tmp/new.txt
echo drep1y2qn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qmwj3xg >> /tmp/new.txt
# plus one DRep that actually re-voted, so R7's superseded-vote path is exercised live (OPEN item 4)
curl -s "https://preprod.koios.rest/api/v1/vote_list?limit=1000" \
  | jq -r 'group_by(.voter_id + .proposal_id)[] | select(length > 1)
           | select(.[0].voter_role == "DRep") | .[0].voter_id' | head -1 >> /tmp/new.txt
curl -s "$P/gov-actions?limit=200" | jq -r '[.items[]
  | select(.lifecycle.ratified_epoch != null
        or (.lifecycle.dropped_epoch != null and .lifecycle.expired_epoch == null
            and .lifecycle.enacted_epoch == null))] | .[].bech32' > /tmp/lifecycle.txt
curl -s "$P/gov-actions?type=treasury_withdrawals&order_by=treasury_amount&sort_order=desc&limit=5" \
  | jq -r '.items[].bech32' >> /tmp/lifecycle.txt
curl -s "$P/gov-actions?order_by=proposed_at&sort_order=asc&limit=2" | jq -r '.items[].bech32' >> /tmp/lifecycle.txt

jq -n --arg epoch "$(curl -s "$P/status" | jq -r '.governance.epoch')" \
  --rawfile d <(sort -u /tmp/tail.txt /tmp/new.txt) \
  --rawfile a <(sort -u /tmp/lifecycle.txt) \
  '{ network: "preprod", recorded_at_epoch: ($epoch | tonumber),
     vote_history_dreps: ($d | split("\n") | map(select(length > 0))),
     tally_actions:      ($a | split("\n") | map(select(length > 0))) }' \
  > config/reconcile-sample.preprod.json
```

The shape it writes:

```json
{
  "network": "preprod",
  "recorded_at_epoch": 320,
  "vote_history_dreps": ["drep1…", "…≥ 20 ids, sorted"],
  "tally_actions": ["gov_action1…", "…≈ 24 ids, sorted"]
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-show-details=direct --test-options='-p "/Reconcile sample/"'`
Expected: PASS (1 case, 9 assertions)

- [ ] **Step 5: Commit**

```bash
git add config/reconcile-sample.preprod.json src/lib/offchain-lib/Palmares/Reconcile.hs \
        src/test/UnitTests/Reconcile.hs palmares.cabal
git commit -m "feat(reconcile): pinned preprod parity sample, CI-gated against the spec floor"
```

---

### Task T-PARITY-5: `admin reconcile-koios`

**Files:**
- Create: `src/exe/admin/Reconcile.hs`
- Modify: `src/exe/admin/Main.hs` (`Command` sum, `commandParser`, the no-provider branch of `main`)
- Modify: `palmares.cabal` (`executable admin`: `other-modules: Reconcile`; `build-depends:` + `aeson,
  containers, directory, filepath, http-client, http-client-tls, http-types, time`)
- Modify: `.gitignore` (`out/`)
- Modify: `src/test/UnitTests/Reconcile.hs` (add `replayTests`)
- Create: `test/fixtures/snapshot/koios/{tip,drep_list,drep_info,drep_voting_power_history,proposal_list,proposal_votes,drep_votes,committee_info}.json`
- Create: `test/fixtures/snapshot/palmares/{status,dreps,drep_power,gov_actions,gov_action_detail,votes,committee}.json`
- Test: `src/test/UnitTests/Reconcile.hs` + the live run below

**Interfaces:**
- Consumes: **a caught-up from-genesis preprod sync** (synthesis §3 step 11 — R10 aborts otherwise);
  `sql/040_indexes.sql` applied (step 12, or every query is measuring the wrong plan); the `palmares.*`
  views including `drep_epoch_power` with `target_kind` (step 15); `sql/030_metrics.sql` +
  `Metrics/Catalog.hs` (step 16, for the `participation_rate`/`delegator_count` fields R2 reads);
  `Query/Gov.hs` + `RestAPI.hs` + the DTOs, which are where the CIP-129 ids both sides join on are rendered
  (steps 7 and 18); `reconcileAll`, `loadExceptions`, `loadSample`, `mkReport`,
  `reportExitCode`, `loadKoiosSnapshot`, `loadPalmaresSnapshot` (T-PARITY-2/3/4).
- Produces: the `reconcile-koios` subcommand and its flags; `runReconcileKoios`; env vars `PALMARES_API`,
  `KOIOS_API`, `KOIOS_API_TOKEN`; reads `PALMARES_READY_MAX_LAG_SECONDS` (produced by step 18, D-7);
  `out/divergences.json` and its schema; exit codes 0/1/2/3; the recorded snapshot pair under
  `test/fixtures/snapshot/`.

- [ ] **Step 1: Write the failing test**

The recorded-fixture mechanism, stated: `--dump-snapshots DIR` writes the **raw** response bodies into
`DIR/{koios,palmares}/` in the T-PARITY-3 layout, and CI replays that committed pair through
`reconcileAll` with **no network at all**, so the nightly outage of either service can never fail a PR.
It is also the "capture the evidence" tool when the nightly does diverge.

```haskell
replayTests :: TestTree
replayTests =
  testGroup
    "Reconcile replay"
    [ testCase "the recorded preprod snapshot pair reconciles with 0 unexplained divergences" $ do
        k <- loadKoiosSnapshot "test/fixtures/snapshot/koios" >>= either assertFailure pure
        p <- loadPalmaresSnapshot "test/fixtures/snapshot/palmares" >>= either assertFailure pure
        es <- loadExceptions "config/reconcile-exceptions.yaml" >>= either assertFailure pure
        let rs = reconcileAll k p es
            rp = mkReport "preprod" t0 320 41 320 es rs
        map rowId rs @?= [R1, R2, R3, R4, R5, R6, R7, R8, R9, R10]
        map rowStatus rs @?= replicate 10 RowOk
        (reportStatus rp, reportUnexplained rp, reportExitCode rp) @?= (ReportOk, 0, 0)
        reportStaleExceptions rp @?= []
        rowCompared (row R1 rs) @?= 294
        rowCompared (row R5 rs) @?= 131
    ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-show-details=direct --test-options='-p "/Reconcile replay/"'`
Expected: FAIL with `test/fixtures/snapshot/palmares/status.json: openBinaryFile: does not exist`

- [ ] **Step 3: Implement the subcommand in `src/exe/admin/Reconcile.hs` and wire it into `src/exe/admin/Main.hs`**

```haskell
data ReconcileKoiosArgs = ReconcileKoiosArgs
  { rkaPalmaresApi    :: String    -- --palmares-api    default $PALMARES_API
  , rkaKoiosApi       :: String    -- --koios-api       default $KOIOS_API, else https://preprod.koios.rest/api/v1
  , rkaSample         :: FilePath  -- --sample          default config/reconcile-sample.preprod.json
  , rkaExceptions     :: FilePath  -- --exceptions      default config/reconcile-exceptions.yaml
  , rkaOut            :: FilePath  -- --out             default out/divergences.json
  , rkaMaxLagSeconds  :: Int       -- --max-lag-seconds default $PALMARES_READY_MAX_LAG_SECONDS, else 300
  , rkaDumpSnapshots  :: Maybe FilePath  -- --dump-snapshots DIR
  }

-- | Fetch both sides, reconcile, write out/divergences.json, return the process exit code.
-- No markdown sibling (T-DATA §8.2 wanted one): the wrapper's jq already prints the per-row table.
runReconcileKoios :: ReconcileKoiosArgs -> IO Int
```

`main` dispatches `ReconcileKoios` in the **no-provider** branch beside `WriteBlueprint`: it must never read
`operation.prv` or `ATLAS_CORE_CONFIG`, because CI runs it with no keys at all. No `--sample-dreps` /
`--sample-actions` counts (T-INFRA §5.3 passed them): a count without a pinned order is a non-reproducible
draw, and the sample file *is* the sample.

Fetch order and cost — R10 first, then everything else; each list fetch pages `limit=1000&offset=N`
(Koios) / `limit=200&offset=N` (ours) until a short page, and **never** uses a reported total as the loop
bound. Koios calls only: 250 ms between requests, honour `Retry-After`, and send
`Authorization: Bearer $KOIOS_API_TOKEN` when the variable is set (CI secret store, never the repo).
Our own API needs no delay. The table below totals **76 Koios requests** (≈ 19 s of pacing) and
**365 local ones**.

| Call | Count at epoch 320 |
|---|---|
| `/tip`, `/status` | 1 + 1 |
| `/drep_list`, `/dreps` | 1 + 2 |
| `POST /drep_info` (≤ 100 ids) | 3 |
| `/drep_voting_power_history?_epoch_no=E` (**`_epoch_no` is mandatory — a bare GET 504s**) | 5 |
| `/drep/{id}/power` for the 294 ids + the 2 reserved | 296 |
| `/proposal_list`, `/gov-actions` | 1 + 1 |
| `/proposal_votes`, `/gov-action/{id}` over `tally_actions` | 24 + 24 |
| `/drep_votes`, `/votes?voter_id=` over `vote_history_dreps` | 40 + 40 |
| `/committee_info`, `/committee` | 1 + 1 |

Exit codes: **0** every row ok or explained · **1** ≥ 1 unexplained divergence · **2** transport or decode
failure on either side (the only code produced here rather than by `reportExitCode`) · **3** R10 abort,
which is neither a pass nor a fail and which `mkReport` derives from the `RowSkipped` rows
`reconcileAll` already emitted, so there is no abort branch here. `out/divergences.json`:

```json
{ "network": "preprod", "ran_at": "2026-10-20T03:00:11Z",
  "palmares_epoch": 320, "palmares_lag_seconds": 41, "koios_epoch": 320,
  "status": "ok", "unexplained": 0, "explained": 0, "stale_exceptions": [],
  "rows": [ { "row": "R1", "compared": 294, "status": "ok", "unexplained": [], "explained": [] } ] }
```

Header comment, verbatim, because a future contributor *will* be tempted: *"Koios runs on cardano-db-sync
too. It is a completeness check, never a source of truth, and never a fallback when our own sync is
behind."*

- [ ] **Step 4: Run test to verify it passes, then verify the live run**

Record the pair and run the replay test:

```
PALMARES_API=https://gov.cardano.vip/api cabal run -v0 admin -- reconcile-koios \
  --dump-snapshots test/fixtures/snapshot --out out/divergences.json
cabal test --test-show-details=direct --test-options='-p "/Reconcile replay/"'
```
Expected: PASS

Live verification — this is F1 check 6, and no unit test can stand in for it:

```
PALMARES_API=https://gov.cardano.vip/api cabal run -v0 admin -- reconcile-koios; echo "exit=$?"
jq -e '.status=="ok" and .unexplained==0
       and ([.rows[].row]|sort)==["R1","R10","R2","R3","R4","R5","R6","R7","R8","R9"]
       and ([.rows[].compared]|min)>0' out/divergences.json
jq -r '[.rows[]|select(.row=="R1" or .row=="R5")|{(.row):.compared}]|add' out/divergences.json
```
Expected (this is the observable "0 unexplained divergences"): `exit=0`, both `jq`s exit 0, and the
second prints `{"R1":294,"R5":131}` — the two rows whose compared counts are pinned population figures
(T-DATA §7.1). R3's count is whatever `drep_distr` holds over epochs 315–319 (~190/epoch, 195 at 319),
so it is gated by `min > 0` and not by a pinned number.

Then the guard, without having to stall db-sync:

```
PALMARES_API=https://gov.cardano.vip/api cabal run -v0 admin -- reconcile-koios --max-lag-seconds 0; echo "exit=$?"
```
Expected: `exit=3`, and `jq -r '.status' out/divergences.json` prints `aborted` with a `lag … > 0s` reason.

- [ ] **Step 5: Commit**

```bash
git add src/exe/admin/Reconcile.hs src/exe/admin/Main.hs palmares.cabal .gitignore \
        test/fixtures/snapshot src/test/UnitTests/Reconcile.hs
git commit -m "feat(admin): reconcile-koios subcommand + recorded snapshot replay test"
```

---

### Task T-PARITY-6: wrapper script and the nightly warn-only workflow

**Files:**
- Create: `scripts/reconcile_koios.sh`
- Create: `.github/workflows/reconcile.yml`
- Create: `docs/deployment/koios-parity-gate.md` (the release gate and what each exit code means)
- Test: the exit-code check below + the `workflow_dispatch` run of `.github/workflows/reconcile.yml`

**Interfaces:**
- Consumes: `admin reconcile-koios` (T-PARITY-5); `images.yml` publishing `ghcr.io/<owner>/palmares-admin`
  for `linux/arm64` with the `admin` entry in its matrix (synthesis §1.3); **`Dockerfile.admin` must set
  `ENTRYPOINT ["/usr/local/bin/admin"]`** so the wrapper can append the subcommand; the public hostname
  `gov.cardano.vip`.
- Produces: `scripts/reconcile_koios.sh`, `PALMARES_ADMIN_CMD`, the `reconcile` workflow, the
  `koios-divergences` artifact, `docs/deployment/koios-parity-gate.md`, and the release-gate `jq`
  one-liner.

- [ ] **Step 1: Write the failing test**

Two checks, neither of which is a unit test. The first is runnable locally and is what fails in Step 2:

```bash
# the wrapper must PROPAGATE the exe's exit code, not swallow it — `set -e` would abort before `case`
KOIOS_API=https://preprod.koios.rest/api/v1 PALMARES_ADMIN_CMD=/bin/false \
  ./scripts/reconcile_koios.sh; echo "exit=$?"
```

The second is the measurement that replaces a unit test for the workflow: a `workflow_dispatch` run on
`ubuntu-24.04-arm` whose observables are in Step 4.

- [ ] **Step 2: Run test to verify it fails**

Run the wrapper check above.
Expected: FAIL with `./scripts/reconcile_koios.sh: No such file or directory` and `exit=127`.

- [ ] **Step 3: Write `scripts/reconcile_koios.sh`, `.github/workflows/reconcile.yml` and the runbook section**

`chmod +x scripts/reconcile_koios.sh`. Warn-only is deliberate: the job needs a live, caught-up stack, so as
a merge gate it would block every PR whenever the Pi is re-syncing or Koios is down. The **release gate** is
read from the artifact instead, and `docs/deployment/koios-parity-gate.md` carries it verbatim together
with the four exit codes:

```bash
jq -e '.status=="ok" and .unexplained==0
       and ([.rows[].row]|sort)==["R1","R10","R2","R3","R4","R5","R6","R7","R8","R9"]' out/divergences.json
```

```yaml
# .github/workflows/reconcile.yml
name: reconcile
on:
  schedule: [{ cron: "0 3 * * *" }]
  workflow_dispatch:
jobs:
  koios:
    # ubuntu-24.04-arm, NOT ubuntu-latest (T-INFRA §5.3): images.yml publishes linux/arm64 only,
    # so an x86 runner cannot exec the admin image.
    runs-on: ubuntu-24.04-arm
    continue-on-error: true          # an upstream outage must never look like our bug
    steps:
      - uses: actions/checkout@v4
      - name: Koios preprod parity
        env:
          PALMARES_API: https://gov.cardano.vip/api
          KOIOS_API: https://preprod.koios.rest/api/v1
          KOIOS_API_TOKEN: ${{ secrets.KOIOS_API_TOKEN }}
          PALMARES_ADMIN_CMD: >-
            docker run --rm -e PALMARES_API -e KOIOS_API -e KOIOS_API_TOKEN
            -v ${{ github.workspace }}:/w -w /w
            ghcr.io/${{ github.repository }}-admin:latest
        run: ./scripts/reconcile_koios.sh
      - uses: actions/upload-artifact@v4
        if: always()
        with: { name: koios-divergences, path: out/ }
```

```bash
#!/usr/bin/env bash
# F1 check 6 — external parity against Koios preprod. Thin wrapper over `admin reconcile-koios`.
# Koios runs on cardano-db-sync too: it is a completeness check, never a source of truth, and never
# a fallback when our own sync is behind.
set -euo pipefail
: "${PALMARES_API:?set PALMARES_API, e.g. https://gov.cardano.vip/api}"
: "${KOIOS_API:=https://preprod.koios.rest/api/v1}"
: "${PALMARES_ADMIN_CMD:=cabal run -v0 admin --}"
export KOIOS_API
mkdir -p out
rc=0
${PALMARES_ADMIN_CMD} reconcile-koios --out out/divergences.json "$@" || rc=$?
case $rc in
  0) echo "parity: 0 unexplained divergences" ;;
  1) echo "parity: UNEXPLAINED DIVERGENCES — out/divergences.json" ;;
  2) echo "parity: transport failure — our API or Koios unreachable" ;;
  3) echo "parity: ABORTED (R10) — db-sync behind, or epoch skew > 1" ;;
  *) echo "parity: unexpected exit $rc" ;;
esac
[ -f out/divergences.json ] &&
  jq -r '.rows[] | "\(.row)\t\(.status)\tcompared=\(.compared)\tunexplained=\(.unexplained|length)"' \
     out/divergences.json
exit $rc
```

- [ ] **Step 4: Run both checks to verify they pass**

Run: `KOIOS_API=https://preprod.koios.rest/api/v1 PALMARES_ADMIN_CMD=/bin/false ./scripts/reconcile_koios.sh; echo "exit=$?"`
Expected: PASS — `exit=1` and no `out/divergences.json`, i.e. the wrapper reached the `case` and
propagated `/bin/false`'s code rather than dying on `set -e`.

Run: `gh workflow run reconcile.yml && sleep 180 && gh run list --workflow=reconcile.yml --limit 1`
Expected: PASS — the run is `completed/success` (job-level `continue-on-error` is what keeps the
*workflow* green on an upstream outage), `gh run download --name koios-divergences` yields
`out/divergences.json`, the log's last lines show ten `R*  ok  compared=…` rows, and the release-gate `jq`
above exits 0 against the downloaded file.

- [ ] **Step 5: Commit**

```bash
git add scripts/reconcile_koios.sh .github/workflows/reconcile.yml docs/deployment/koios-parity-gate.md
git commit -m "ci(reconcile): nightly warn-only Koios parity + release-gate runbook"
```

---

### Block interfaces — produces

Names later blocks (and the F1 sign-off) may consume:

- **Haskell, `offchain-lib`** — `Palmares.Reconcile.Koios`: `KoiosTip`, `KoiosDRepListRow`, `KoiosDRepInfo`,
  `KoiosPowerRow`, `KoiosProposal`, `KoiosWithdrawal`, `KoiosVoteRow`, `KoiosDRepVoteRow`, `KoiosCommittee`,
  `KoiosCommitteeMember`, `koiosActionType`, `koiosDRepStatus`, `koiosVote`, `koiosVoterRole`.
- **Haskell, `offchain-lib`** — `Palmares.Reconcile`: `RowId (R1…R10)`, `Divergence`
  (`key`/`field`/`koios`/`palmares`), `RowStatus (RowOk | RowDivergent | RowSkipped Text)`, `isSkipped`,
  `RowResult`, `ExceptionEntry`, `ReconcileSample`, `Freshness (Fresh | Aborted)`,
  `ReportStatus (ReportOk | ReportDivergent | ReportAborted)`, `Report`, `KoiosSnapshot`,
  `PalmaresSnapshot`, `diffKeyed`, `applyExceptions`, `staleExceptions`, `mkRow`, `skippedRow`,
  `checkFreshness`, `mkReport`, `reportExitCode`, `reconcileAll`, `loadExceptions`, `loadSample`,
  `loadKoiosSnapshot`, `loadPalmaresSnapshot`.
- **CLI** — `admin reconcile-koios` with `--palmares-api`, `--koios-api`, `--sample`, `--exceptions`,
  `--out`, `--max-lag-seconds`, `--dump-snapshots`; dispatched in the no-provider branch (no
  `operation.prv`, no `ATLAS_CORE_CONFIG`).
- **Exit codes** — 0 clean/explained · 1 unexplained · 2 transport · 3 R10 abort.
- **Files** — `scripts/reconcile_koios.sh`, `.github/workflows/reconcile.yml`,
  `docs/deployment/koios-parity-gate.md`, `config/reconcile-exceptions.yaml`,
  `config/reconcile-sample.preprod.json`, `out/divergences.json`, `test/fixtures/koios/*.json`
  (single-id wire fixtures), `test/fixtures/snapshot/{koios,palmares}/*.json` (the replay pair).
- **Env vars** — `PALMARES_API`, `KOIOS_API`, `KOIOS_API_TOKEN` (CI secret only),
  `PALMARES_ADMIN_CMD`; reads `PALMARES_READY_MAX_LAG_SECONDS` (default 300).
- **Tests** — `reconcileTests :: TestTree` with the groups `Koios wire`, `Reconcile primitives`,
  `Reconcile rows`, `Reconcile sample`, `Reconcile replay`.
- **Cabal deltas** — `offchain-lib` + `yaml`, `filepath`; `admin` + `aeson`, `containers`, `directory`,
  `filepath`, `http-client`, `http-client-tls`, `http-types`, `time` and `other-modules: Reconcile`;
  `extra-source-files` + `test/fixtures/koios/*.json`, `test/fixtures/snapshot/*/*.json`,
  `config/reconcile-sample.preprod.json`.
- **Contracts this block pins on other blocks** — `GET /votes` returns the full vote history, not
  `seq_last = 1` (R7); `palmares.drep_epoch_power` carries `target_kind` with no filter (R4, D-20);
  `GET /drep/{id}/power` answers for `drep_always_abstain` / `drep_always_no_confidence` (R3/R4);
  `GET /gov-action/{id}` carries `tallies[]` and `withdrawals[]` (R6/R9); `images.yml`'s matrix includes
  `admin`; `Dockerfile.admin` sets `ENTRYPOINT ["/usr/local/bin/admin"]`.

### Block open items

- **(OPEN: tolerance for `live_delegator_count` in R2.** Ours is as-of `as_of_epoch`, Koios's is live at its
  own tip, and R10 still allows 1 epoch of skew. T-METRIC §9 leaves the `epoch_stake ↔ drep_distr`
  reconciliation tolerance unmeasured, so **no number exists to write down yet**. T-PARITY-3's table ships
  it as **exact**; the implementer measures the residue on the first caught-up run (synthesis §3 step 11)
  and either keeps exact or waives the residue in `config/reconcile-exceptions.yaml` with the measured
  count as the `reason`. **Decided by: whoever runs T-PARITY-5's live verification** — it is a measurement,
  not a judgement, and it needs no owner sign-off.**)**
- **(OPEN: Koios `committee_info` member `status` value set.** Not captured in any design note and not
  readable from the repo. Until it is, R8 compares only the quorum fraction, `cc_hot_hex` and
  `expiration_epoch`, and `KoiosCommitteeMember` has no `status` field. **Closed by: reading the recorded
  `test/fixtures/koios/committee_info.json` in T-PARITY-1 step 1** — one `jq -r '.members[].status'` — then
  adding the mapping to `CommitteeMemberStatus` (`authorized | unauthorized | resigned | expired`) and one
  comparator line to R8. No owner decision involved.**)**
- **(OPEN: the full CIP-129 id of the re-voted action.** T-DATA §8.2 R6 abbreviates it as
  `gov_action197dpq8x…` — 3 rows from `/proposal_votes` where `/vote_list` shows 4 from one DRep, and the
  remaining characters exist nowhere in the design notes. Needed as `$A` in T-PARITY-1's recording recipe,
  or the "latest-only" assertion has no re-vote to catch. **Closed by: `curl -s "$K/vote_list?limit=1000" |
  jq -r 'group_by(.voter_id + .proposal_id)[] | select(length > 1) | .[0].proposal_id' | head -1` in
  T-PARITY-1 step 1** — T-METRIC §3.2 proves 69 such pairs exist, so the query cannot come back empty.**)**
- **(OPEN: a pinned re-voting DRep id for R7's superseded-vote case.** T-DATA §8.2 R7 has one with 11 vote
  rows, 5 of them on a single action, but does not name it, and the worked-example DRep has 0 re-votes.
  T-PARITY-4 now pins the worked-example DRep explicitly, which covers the *membership* comparison but not
  the superseded-vote path, so that path is covered only by T-PARITY-3's hand-built snapshot until this id
  lands. **Closed by: the same `vote_list` group-by as above, taking `.[0].voter_id`**, appended to
  `/tmp/new.txt` in T-PARITY-4's draw beside the worked-example id.**)**
- **(OPEN: the preprod expired-action count — the one place two fact-checked notes still disagree.**
  T-DATA §7.1 says 102 expired (all also carrying `dropped_epoch`) and 11 open; T-METRIC §4 line 688 says
  103 expired and 12 open. Only `11 enacted + 102 expired + 6 dropped-only + 12 open = 131` closes, and
  T-METRIC's own `119 settled` (= 11 + 102 + 6) and `eligible_actions = 114` chain both need the 12, so
  T-PARITY-1 asserts **102** and **12** and T-DATA's "11 open" is the arithmetic that fails. **Decided by:
  the first recording** — `jq` the counts straight out of `test/fixtures/koios/proposal_list.json` in
  T-PARITY-1 step 1 and correct the two literals before step 4 if live preprod disagrees.**)**

---

## Self-review (writing-plans checklist, run 2026-10-20)

**1. Spec coverage.** F1 checks in Plan A's scope map to tasks as follows: check 1 → T-SUBSTRATE-5 ·
check 2 → T-SQL-METRICS-3…8 · **check 3 → T-SQL-METRICS-13 (GAP FOUND: no task existed; added below)** ·
check 4 → db-sync's own rollback plus the attestation replay test, which lives in **Plan B** (recorded
here so nobody looks for it in Plan A) · check 5 → T-SQL-METRICS-6 · check 6 → T-PARITY-1…6 ·
check 7 → T-SQL-METRICS-2 · check 12 → T-API-8…17 · check 16 (API half) → T-API-18 + T-BOOTSTRAP-4.

**2. Step scan.** 275 checkbox steps. The six blocks were each reviewed by an independent pass whose
explicit defect kinds included *decides-nothing* and *transcript*; 1 of ~90 findings could not be settled
without pulling an image, and that one — the db-sync `NodeConfigFile` mount — **has since been settled
empirically** (`/opt/cardano/config/preprod/config.json` exists in `cardano-node:11.1.2`; `/data` does
not), and the Experiment A bundle already mounts a pinned host config dir into both containers.

**3. Type consistency.** Task ids are block-prefixed and stable precisely so `Consumes` references survive
reordering. One cross-block naming drift was found and corrected during assembly: the substrate block
consumed `experiments/dbsync-genesis/RESULTS.md` while the bundle on disk is
`experiments/dbsync-preprod/measurements.md`.

**4. Review Focus.** All five failure modes have tests in the owning tasks (verified by grep over the
assembled plan): `/ready` lag fail-closed and `lag_seconds` (28 references), `target_kind` filtering
(25), zero-denominator → `null`/`n/a` (13), the anchor `unreachable` default arm (14), activity cursor
totality (asserted both as a seeded unit test and as a live `sql/checks` invariant).

**5. Proportion — the honest finding.** 7.7k plan lines against a 270-line spec is 28×, which the skill
flags as a transcript risk. Examined: the volume is **~50 SQL objects whose bodies ARE the deliverable**
(a view is its SELECT), ~20 route families with their DTO field names and `jq` assertions, and pinned
infra manifests — all of it values an implementer cannot derive. 37 Haskell code blocks remain, which the
block reviewers already pruned to signatures plus test assertions. Verdict: **justified, not trimmed**,
because the spec is terse Romanian prose while the plan must pin exact names against a 100+ table
upstream schema. The one place proportion still bites is the 75 `(OPEN: …)` lines, consolidated below so
they are decided once rather than rediscovered per task.

### Task T-SQL-METRICS-13: the determinism gate — F1 check 3

Added by self-review: spec §9 F1 check 3 (restated) requires that running the metric catalog's SQL twice
against the same db-sync produces identical output. No drafted task covered it.

**Files:**
- Create: `/Users/mg/Projects/CardanoProjects/palmares/scripts/check_determinism.sh`
- Modify: `/Users/mg/Projects/CardanoProjects/palmares/.github/workflows/ci.yml`

**Interfaces:**
- Consumes: T-SQL-METRICS-9's `resources/metrics/*.sql` and `Palmares.Metrics.Catalog`;
  T-SQL-METRICS-1's `applyPalmaresSchema`; `scripts/check_dbsync_tip.sh` (freshness precondition).
- Produces: `scripts/check_determinism.sh` (exit 0 = identical, 1 = drift, 2 = db-sync moved mid-run);
  CI job `determinism`.

- [ ] **Step 1: Write the failing test**

`scripts/check_determinism.sh`: read the tip `(block_no, hash)` before and after; for every metric id in
`resources/metric-formulas.json`, execute its `.sql` twice via `psql -At` over the pinned epoch-320
cursor, `shasum -a 256` each result set, and assert the two digests are equal per metric. Exit 2 (not 1)
when the before/after tip differs, so a moving chain is reported as *inconclusive* rather than as drift.

- [ ] **Step 2: Run it to verify it fails**

Run: `scripts/check_determinism.sh` with `resources/metrics/` absent.
Expected: FAIL — `no metric catalog found`.

- [ ] **Step 3: Implement the script and wire the CI job**

Add a `determinism` job to `ci.yml` that runs it against the seeded db-sync skeleton of T-SQL-METRICS-1
(so it blocks merges without needing the real synced database), and document in
`docs/deployment/preprod-dbsync-sync.md` that the against-preprod run is the one that signs check 3.

- [ ] **Step 4: Run it to verify it passes**

Run: `scripts/check_determinism.sh`
Expected: PASS — `N/N metrics deterministic`, exit 0.

- [ ] **Step 5: Commit**

```bash
git add scripts/check_determinism.sh .github/workflows/ci.yml docs/deployment/preprod-dbsync-sync.md
git commit -m "test: determinism gate over the metric catalog (F1 check 3)"
```

## Open decisions (consolidated)

75 `(OPEN: …)` lines survive in the blocks. They are reproduced below verbatim, in file order, so they
are decided once. Most are *implementer* calls settled by a single command; the few that are the owner's
are marked **[OWNER]** in the blocks themselves. Resolve them in order; each is quoted with its block.


### bootstrap (11 open)

- `L162`:   **(OPEN: publishing `palmares-builder:9.6.6` to GHCR from the laptop needs `write:packages`; the
- `L1122`: **(OPEN: the `node-db` → `NodeConfigFile` pairing is unverified and it is what the step-4 gate turns
- `L1276`: - **(OPEN: `libblst-dev` may not exist in the Ubuntu 24.04 arm64 archive.** T-BOOTSTRAP-1's step-2 log
- `L1279`: - **(OPEN: whether `pkg-config` resolves `libblst-any` on the hosted runner.** `cardano-crypto-class`
- `L1283`: - **(OPEN: publishing `palmares-builder:9.6.6` to GHCR from the laptop needs `write:packages`;** the
- `L1286`: - **(OPEN: whether `gh repo create en7angled/palmares` succeeds with the current token.** `repo` +
- `L1289`: - **(OPEN: the first `ci.yml` cold-run wall clock.** The two notes disagree — T-INFRA §5.1 says
- `L1292`: - **(OPEN: whether Experiment A's drafter left files at
- `L1296`: - **(OPEN: where blinklabs' `cardano-node:11.1.2` keeps `config.json` + the genesis files**, and
- `L1301`: - **(OPEN: `reconcile.yml` now belongs to the Phase-5 parity block**, not to T-BOOTSTRAP-4. The
- `L1304`: - **(OPEN: `CI_SHAPE` and T-BOOTSTRAP-1's run URL** are appended here once the probe has run, as

### extraction (5 open)

- `L489`: **(OPEN: only the `drep1y2qn0rf…` vector is a value read off Koios. T-DATA §1.3's live `cc_hot1qddl5lv…` /
- `L790`: - **(OPEN: of the seven `UnitTests/GovIds.hs` id literals, only `drep1y2qn0rf…` is a value read off Koios
- `L796`: - **(OPEN: blocker B2 — the script-DRep profile claim is still unspecified (T-EXTRACT §7.5, synthesis §3 step 24).
- `L801`: - **(OPEN: T-EXTRACT §2.5 and §4.5 place fix #5's read-side conversion in `Query/Aggregates.hs`; measured, the
- `L807`: - **(OPEN: `parseDrepIdAny` deviates from T-DATA §1.3's `Either Text (Bool, ByteString)` to

### substrate (6 open)

- `L852`: `ghcr.io/<owner>/palmares-admin:<sha>` **(OPEN: the GHCR owner handle — T-INFRA's compose defaults to
- `L1278`: - **(OPEN: the GHCR owner handle.)** `ghcr.io/<owner>/palmares-admin` — T-INFRA's compose defaults to
- `L1281`: - **(OPEN: `drep_distr` coverage of the earliest Conway epochs.)** F1 check 1 is written as
- `L1287`: - **(OPEN: whether db-sync 13.7.1.0 tolerates the node config's `LedgerDB: V2LSM` block.)** Answered for
- `L1291`: - **(OPEN: `minirack-m4`'s live taint.)** T-INFRA §1.2 records that
- `L1296`: - **(OPEN: re-check `ghcr.io/blinklabs-io/cardano-db-sync` tags before T-SUBSTRATE-4.)** Upstream aarch64

### sql-metrics (29 open)

- `L47`: - Produces: `applyPalmaresSchema :: ConnectionPool -> IO ()` · `applySchemaSql :: [(FilePath, ByteString)] -> SqlPersistT IO ()` · `palmaresSqlFiles :: [(FilePath, ByteString)]` · `Test.Pg.withScratchDb :: (ConnectionPool -> IO a) -> IO a` · `Test.Pg.runPg :: ConnectionPool -> SqlPersistT IO a -> IO a` · `test/fixtures/dbsync-skeleton.sql` · `PALMARES_TEST_PG_CONN_STR` **(OPEN: the 
- `L401`: - Produces: `palmares.vote (gov_action_id, voter_role, voter_id_hash, voter_hot_hash, voter_cold_hash, has_script, vote, voting_anchor_id, vote_epoch, vote_slot, cast_at, vote_tx_hash, tx_index, vote_index, seq_first, seq_last, vote_rows)` · `palmares.thresholds (epoch_no, dvt_*, pvt_*, pvtpp_security_group, committee_min_size, committee_max_term_length, gov_action_lifetime, gov_action_deposit, drep_d
- `L595`: **(OPEN: T-API §2.6 names `pool_stat.voting_power` for SPO power, but no note verifies that `pool_stat` is
- `L839`: **(OPEN: `voting_procedure.index`, `gov_action_proposal.index` and `cert_index` are three independent per-tx
- `L924`: **(OPEN: the notes write `:drep` / `:as_of_epoch` / `:w` but never fix a binding mechanism; persistent's `rawSql`
- `L936`: `palmares.vote_tally` **(OPEN: field not in the notes; needed because every DTO `formula_id` must resolve)**.
- `L1020`: - Produces: `test/fixtures/preprod-epoch-320-metrics.json` — the regression gate on S2 · `withFixtureDb :: (ConnectionPool -> Fixture -> IO a) -> IO a` and `runMetric :: ConnectionPool -> Text -> [PersistValue] -> IO [row]` (both local to `UnitTests/MetricFixture.hs`) · `PALMARES_FIXTURE_PG_CONN_STR` **(OPEN: name is mine; the notes do not name the connection used for the parity run
- `L1029`:     "block_hash": "(OPEN: read the epoch-320 cursor block's hash from our own db-sync on the first caught-up run)",
- `L1030`:     "slot_no": "(OPEN: same)",
- `L1031`:     "block_time": "(OPEN: same — age_days moves inside an epoch, which is how the old 1463 went unnoticed)"
- `L1051`:     "_comment_rationale": "(OPEN: only rationale_verified = 2 and rationale_rate = 0.125 are measured (T-METRIC §4). inline = 0 is inferred from 'no data: URI on this DRep's votes' and is not measured; pin inline/unverified/pending/absent from the first caught-up run, in the same commit as the cursor values.)",
- `L1151`: - [ ] **Step 3: Fill the three `(OPEN: …)` cursor values from the caught-up db-sync and implement the loader**
- `L1277`: query-api after any db-sync upgrade"). **(OPEN: T-DATA §3.5 says a 60 s tick; T-EXTRACT's `.env.example` ships
- `L1333`: - **(OPEN: `W = 10` epochs for retention)** — T-METRIC §8.2 asks the owner; recommended as specified. It is one
- `L1335`: - **(OPEN: `InfoAction` in the participation denominator)** — T-METRIC §8.3 asks the owner; recommended kept as
- `L1338`: - **(OPEN: the `:w` / `:drep` / `:as_of_epoch` binding mechanism)** — the notes write named parameters but fix no
- `L1341`: - **(OPEN: the `epoch_stake ↔ drep_distr` reconciliation tolerance)** — T-METRIC §5.4/§9 and synthesis §6 leave it
- `L1344`: - **(OPEN: synthesis §1.2 says "fourteen views" but its own table names seventeen plain views)** — the table
- `L1346`: - **(OPEN: `palmares.threshold_map` is my name)** — D-25 requires the static (role, action-type) → threshold table
- `L1348`: - **(OPEN: `sql_object` catalog field)** — needed so `gov_action_state_v1` and `tally_cast_votes_v1`, whose SQL
- `L1350`: - **(OPEN: the fixture's five-grade rationale split)** — T-METRIC §4 gives only `rationale_verified = 2` and
- `L1353`: - **(OPEN: the fixture's cursor block hash, slot and `block.time`)** — must be read off our own db-sync at epoch
- `L1355`: - **(OPEN: `PALMARES_TEST_PG_CONN_STR` / `PALMARES_FIXTURE_PG_CONN_STR`)** — no note names a test-database env
- `L1357`: - **(OPEN: refresh interval 60 s vs 300 s)** — T-DATA §3.5 vs T-EXTRACT `.env.example`; the synthesis does not
- `L1359`: - **(OPEN: `Palmares.Metrics.Catalog`'s library)** — synthesis §1.3 names `Metrics/Catalog.hs` without a library;
- `L1361`: - **(OPEN: CC vote volume)** — preprod has 3,243,802 CC vote rows (test spam). Do **not** fixture that figure from
- `L1363`: - **(OPEN: the activity cursor's `item_index` across sibling per-tx lists)** — `voting_procedure.index`,
- `L1368`: - **(OPEN: SPO power in `palmares.vote_tally`)** — T-API §2.6 names `pool_stat.voting_power`; no note verifies
- `L1371`: - **(OPEN: `treasury_withdrawal.paid_heuristic`)** — `reward_rest` has no gov-action key, so the

### api (18 open)

- `L115`: **(OPEN: `bitcowsCip105` above is a placeholder — the fixture gives the CIP-129 id and the raw hex but not
- `L394`: **(OPEN: query-api's runtime role is `palmares_ro` (USAGE+SELECT only) but D-15 has query-api apply the DDL and
- `L400`: **(OPEN: `REFRESH MATERIALIZED VIEW CONCURRENTLY` requires a UNIQUE index on the matview and no note specifies one.
- `L478`: **(OPEN: T-API §6 spells the poll interval `FRESHNESS_POLL_MS`; T-EXTRACT §5.1 requires the `PALMARES_` prefix on
- `L640`: **(OPEN: `resources/metric-formulas.json` is written by synthesis §3 step 16 and T-METRIC §7 enumerates only the
- `L908`: **(OPEN: two tables are not enumerated anywhere. (a) `paramGroupColumns`: the full `param_proposal` column →
- `L987`: **(OPEN: the five single-valued counts above sum to 132, one more than the 131 rows that must partition. They are
- `L1374`: **(OPEN: `kinds` lists `stake_address` (§1.7) but `SearchResults` (§2.10) has no `stake_addresses` group — either
- `L1646`: - **(OPEN: `bitcowsCip105` in T-API-1 is a placeholder — the fixture gives the CIP-129 id and the raw hex but not
- `L1650`: - **(OPEN: query-api needs a second pool on `PALMARES_PG_OWNER_CONN_STR` to apply the DDL and refresh the matview
- `L1653`: - **(OPEN: `REFRESH MATERIALIZED VIEW CONCURRENTLY` requires a UNIQUE index on the matview and no note specifies
- `L1656`: - **(OPEN: T-API §6 spells the poll interval `FRESHNESS_POLL_MS`; T-EXTRACT §5.1 requires the `PALMARES_` prefix on
- `L1660`: - **(OPEN: `paramGroupColumns` and `securityGroupColumns` need the full `param_proposal` column → CIP-1694 group
- `L1665`: - **(OPEN: the five single-valued `/gov-actions?state=` counts derived from f1-metric-formulas §4 sum to 132, not
- `L1669`: - **(OPEN: `resources/metric-formulas.json` must also carry `gov_action_state_v1` and `tally_cast_votes_v1`
- `L1672`: - **(OPEN: `kinds` lists `stake_address` (§1.7) but `SearchResults` (§2.10) has no `stake_addresses` group —
- `L1675`: - **(OPEN: `GET /graph` (§1.8, `root` · `kind` · `depth` [1,3] · `min_lovelace` · `max_nodes`) is not on the cut
- `L1678`: - **(OPEN: `palmares.vote` must expose `revote_count` (T-METRIC §0.3's `vote_rows`) under that name for

### parity (5 open)

- `L935`: - **(OPEN: tolerance for `live_delegator_count` in R2.** Ours is as-of `as_of_epoch`, Koios's is live at its
- `L942`: - **(OPEN: Koios `committee_info` member `status` value set.** Not captured in any design note and not
- `L948`: - **(OPEN: the full CIP-129 id of the re-voted action.** T-DATA §8.2 R6 abbreviates it as
- `L954`: - **(OPEN: a pinned re-voting DRep id for R7's superseded-vote case.** T-DATA §8.2 R7 has one with 11 vote
- `L960`: - **(OPEN: the preprod expired-action count — the one place two fact-checked notes still disagree.**

_Note: line numbers refer to `docs/superpowers/plans/drafts/plan-a-<block>.md`, which remain the editable source of each block._

### Resolutions recorded during execution

- **`CI_SHAPE=hosted-setup`** + the blst-from-source step. Probe run 1
  (https://github.com/mariusgeorgescu/solana-haskell-sdk/actions/runs/39324907758) proved
  `ubuntu-24.04-arm` is a real ARM64 runner but **`libblst-dev` does not exist in the Ubuntu 24.04 arm64
  archive** (`E: Unable to locate package libblst-dev`), which is the branch T-BOOTSTRAP-1 step 3
  anticipated. Run 2 (https://github.com/mariusgeorgescu/solana-haskell-sdk/actions/runs/39325302759)
  re-measures with `libblst-dev` dropped from apt and blst v0.3.15 built from source
  (`./build.sh`, headers to `/usr/include`, `libblst.a` to `/usr/lib`, a hand-written `libblst.pc`), and
  also settles the `libblst-any` alias question `cardano-crypto-class` depends on.
  ⇒ `ci.yml` is `runs-on: ubuntu-24.04-arm` + `haskell-actions/setup@v2` + `actions/cache@v4`, with the
  blst build and the `libblst-any` alias as two extra steps. The `ghcr-builder` fallback is NOT needed.
- **GitHub Actions availability, clarified.** `apps-k8s-configs/CLAUDE.md` says this account's Actions are
  "blocked on billing" — that applies to **private** repos. Public personal/org repos get free hosted
  runners: `mariusgeorgescu/solana-haskell-sdk` ran successfully on 2026-10-19, and both probe runs above
  executed. **Consequence: `palmares` being public from commit #1 is load-bearing for CI**, not only for
  the open-source story. If it were private, `ci.yml` would have to move to a local gate.
- **`.dockerignore` must not be copied from the Belt System** (its four lines omit `.env`, which is leak
  #1 itself) — written fresh, and both leak classes are now covered by a passing hygiene check plus the
  `.githooks/pre-push` gate.

- **Probe run 2 concluded: `success`** (run 39325302759). Measurements: [36;1m  echo "$p=$(pkg-config --modversion "$p")"[0m [36;1m  | sudo tee /usr/lib/pkgconfig/libblst-any.pc >/dev/null[0m [36;1mdone[0m [36;1mecho "cabal-store=/home/runner/.cabal/store"[0m [36;1mecho "libblst-any-after-alias=$(pkg-config --modversion libblst-any)"[0m [36;1mecho "libblst-any=$(pkg-config --modversion libblst-any 2>/dev/null || echo MISSING)"[0m [36;1mfor p in libsodium libsecp256k1 libblst; do[0m [36;1msed 's/^Name: libblst/Name: libblst-any/' /usr/lib/pkgconfig/libblst.pc \[0m [36;1mset -euo pipefail[0m ##[endgroup] ##[group]Run set -euo pipefail cabal-store=/home/runner/.cabal/store libblst-any-after-alias=0.3.15 libblst-any=MISSING libblst=0.3.15 libsecp256k1=0.2.0 libsodium=1.0.18 shell: /usr/bin/bash -e {0} 
  ⇒ every premise of `CI_SHAPE=hosted-setup` is now measured, not assumed: ARM64 runner, distro packages minus `libblst-dev`, blst v0.3.15 from source, GHC 9.6.6 aarch64 bindist, and the `libblst-any` alias generated from `libblst.pc`.
