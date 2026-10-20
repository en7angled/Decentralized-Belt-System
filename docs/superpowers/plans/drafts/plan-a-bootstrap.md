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

## Decision
cardano-db-sync 13.7.1.0 (blinklabs arm64, pinned by digest) with
`insert_options: {preset: only_governance, offchain_vote_data: enable, shelley: {enable: true}}`
and the default in-memory ledger backend, in db-sync's own `cexplorer` database. Our governance layer
is SQL views in schema `palmares` plus one materialized view — not a projection service, and not an
Ogmios follower. Our own metadata fetcher is cut: db-sync fetches and hash-verifies anchors itself.

## Measurements
<!-- filled by the Phase-2 measurement task from Experiment A + the preprod sync:
     wall clock to tip, pg_total_relation_size per table (top 20), peak db-sync RSS,
     count(*) on stake_deregistration / pool_update / drep_distr / off_chain_vote_data /
     off_chain_vote_fetch_error -->

## Consequences
<!-- filled by the same task; must include the two documented holes in "db-sync already does it":
     constitution anchors are never fetched, and duplicate off_chain_vote_* child rows (PR #2137) -->
```

`docs/adr/0002-write-path-provider.md`:
```markdown
# 0002 — Write-path provider

Status: accepted for F1

## Decision
Maestro, via Atlas, with the key injected from the environment and never committed. The read path has
no provider dependency at all. `maestro-sdk` is on neither Hackage nor CHaP and upstream is archived,
so the `sourabhxyz/haskell-sdk` fork pin in `cabal.project` is load-bearing.

## Spike
<!-- filled by the Plan B write-path task: does Atlas's local-node provider
     (src/GeniusYield/Providers/Node.hs) carry Conway governance certificates?
     Atlas has NO db-sync provider — that option does not exist. -->

## Consequences
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

## Produces

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

## Open items

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
