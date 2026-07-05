# Preprod coordinated redeploy — runbook

**Network:** preprod (testnet). **Decision:** abandon old on-chain data, re-populate fresh.
Design reference: `docs/superpowers/specs/2026-07-05-preprod-coordinated-redeploy-design.md`.

## 1. Purpose + scope

This is the single runbook for cutting over the preprod deployment to pick up
everything that is merged to `main` but not yet deployed: Stream E's on-chain
hardening (F-21/F-22a/F-22b, four changed validator hashes), the R1 chain-sync
schema bump (v4, tx_hash/slot/output_index backfill), Stream C's fail-closed
frontend change, and the leaked Maestro token rotation.

For the exact validator-hash change set and the deploy-reference-scripts
mechanics, see `docs/deployment/redeploy-onchain-hardening.md` — that document
is the E-specific detail; this runbook does not duplicate its contents, only
links it.

**Decision:** old on-chain preprod state (profiles, ranks, membership
histories, achievements) is test/demo data only. It is **abandoned**, not
migrated. The validator-hash change makes the redeploy a fresh protocol
instance; old-hash state is simply left unspent and re-seeded from scratch via
`scripts/populate_testnet.sh`.

Out of scope: mainnet cutover (separate, later, after preprod sign-off), data
migration, the FE consumption track (verification panel / org-explorer
wiring), R6/R7 product decisions.

## 2. Pre-flight gate

Confirmed (Task 1, `admin write-blueprint` regeneration diffed against the
committed blueprint and against `redeploy-onchain-hardening.md` §2 — result:
**MATCH**, no drift):

| Validator | Deployed (old) hash | Regenerated (new) hash | Changed? |
| --- | --- | --- | --- |
| Minting Policy | `e6353af3c3555a14c1f232c8f0b985cbd581fd24e14e60221cf295fc` | `091a10e63e6048ce2157560547032216ffaf6c227a35d6ff8a0d0c38` | **YES** (F-21) |
| Memberships Validator | `79ac62cfa3dc2da82a83921a12301e09a46e07be09c3f46a321a5668` | `9cf4f22e58a70eef8f6c2214ac64cb50803a2955611ad31fe38cbf71` | **YES** (F-21 + F-22b) |
| Ranks Validator | `91f4d324fcb63323b93bfbdd22952524854e52abe2e37c0e6541d675` | `8c777a5ab3f806819d9c32231415027ae6e869e655a6a0120771375e` | **YES** (F-22b) |
| Achievements Validator | `03c7daf20c54a9caeaaf8794b8e959bcb74ac148871dc44000ced729` | `f43bec2cdebd560c43a5e1f041ffe785b8298a24a3a6dbf25d547240` | **YES** (F-22b) |

Profiles Validator and Oracle Validator are unchanged (F-18 was doc-only;
Oracle untouched). `OracleNFTPolicy` is excluded from the blueprint by design
(deployment-time `TxOutRef` parameterization) — it will get a **new currency
symbol** as a side effect of `deploy-reference-scripts` regardless; see
`redeploy-onchain-hardening.md` §2 for details. Full comparison and
methodology: `.superpowers/sdd/task-1-report.md`.

Before starting the ops steps below, confirm:

- [ ] `operation.prv` (admin key) is present at repo root and **not** committed.
- [ ] A funded preprod wallet backs `operation.prv` (sufficient ADA for
      5× reference-script deployment + oracle NFT mint + minting-policy
      deployment).
- [ ] `config/config_atlas.json` has `"networkId": "preprod"`.

## 3. Step 1 — Rotate Maestro token `[ops]`

The Maestro token committed in `config/config_atlas.json` (exposed in git
history — `coreProvider.maestroToken`, introduced in commit `e9514de`) must
be rotated before anything else in this sequence.

```bash
grep -n 'maestroToken\|MaestroToken\|apiKey' config/config_atlas.json
```

**How the token actually reaches a running server** (verified:
`decodeConfigEnvOrFile` in `src/lib/offchain-lib/Utils.hs`, called as
`decodeConfigEnvOrFile "ATLAS_CORE_CONFIG" defaultAtlasCoreConfig` from
`interaction-api`/`query-api`/`chainsync-service` `Main.hs`): at startup each
server checks the `ATLAS_CORE_CONFIG` env var first. If it's set, its value
is parsed **directly as inline JSON** and `config/config_atlas.json` is never
read at all; only when the env var is *unset* does the server fall back to
the baked-in file. Every `Dockerfile.{chainsync,interaction-api,query-api}`
does `COPY ./config /app/config`, so the committed (leaked) token is baked
into every image as that fallback — but it's only *used* if
`ATLAS_CORE_CONFIG` is unset in the deploying `.env`.

Both this repo's dev/audit `docker-compose.yml` and the `bjj-frontend`
unified `docker-compose.yml` pass `ATLAS_CORE_CONFIG=${ATLAS_CORE_CONFIG}`
straight through from their own `.env`. Confirmed: the `.env` that actually
drives the deployed preprod stack (`bjj-frontend/.env`) already sets
`ATLAS_CORE_CONFIG` to an inline JSON blob — so the running containers are
**not** using the baked, leaked token; they're using whatever token is in
that blob. Rotation therefore has two independent parts:

1. Issue a new Maestro API key from the Maestro dashboard.
2. Rotate the **live** credential: update the `maestroToken` field inside the
   `ATLAS_CORE_CONFIG` JSON blob in `bjj-frontend/.env` to the new key. This
   takes effect on the next container recreate — step 3 below already runs
   `docker compose up -d`, so no separate restart is needed if this edit
   lands first. No image rebuild is required for the token itself.
3. Revoke the old key at the Maestro dashboard once the new one is verified
   working (see rollback table, row 1) — this is what actually neutralizes
   the git-history exposure, since the committed value can't be scrubbed out
   of prior commits.
4. Scrub `maestroToken` out of **this repo's** `config/config_atlas.json`
   (replace with an empty/placeholder value) and commit. This doesn't rotate
   anything live (step 2 already did) — it only stops future images from
   baking in a live-looking fallback secret.

The **old committed token is exposed in this repo's git history** regardless
of step 4 — scrubbing the working tree does not remove it from prior
commits. Revoking it (step 3) is what neutralizes the exposure. Purging it
from git history (`git filter-repo` / BFG) is **optional** and not required
for this cutover.

## 4. Step 2 — Redeploy on-chain `[ops]`

Needs `config/config_atlas.json` (rotated token in place) and `operation.prv`
at repo root.

```bash
direnv exec . cabal run exe:admin -- deploy-reference-scripts
```

This deploys fresh reference scripts for all five validators, mints a new
oracle NFT (giving `OracleNFTPolicy` a new currency symbol), deploys the new
`MintingPolicy`, and **unconditionally overwrites**
`config/config_bjj_validators.json` with the new hashes/refs/
`oracleNFTAssetClass`. This resolves both Stream E (hash change) and Stream G
(stale committed config) in one step — see `redeploy-onchain-hardening.md` §3
for the full breakdown of what the command does.

After it completes, commit the regenerated `config/config_bjj_validators.json`.

## 5. Step 3 — Cutover: publish images, bump tag, redeploy the stack `[ops]` — CUTOVER POINT

**This is the cutover point.** The old deployment stays fully live through
steps 1–2 above (non-destructive rehearsal); this step is where traffic
moves to the new on-chain state and the new servers.

The BE servers are **not** run via `cabal run` in any deployed environment —
`interaction-api`, `query-api`, `chainsync-service`, and `mcp-server` ship as
published Docker Hub images (`mariusgeorgescu/bjj-{chainsync,interaction-api,
query-api,mcp-server}`), pinned by a tag, pulled by the `bjj-frontend` repo's
unified `docker-compose.yml`. That stack co-locates the frontend, BFF, and
`agent-service` alongside these protocol images, all reading **one shared
`.env`** — so there's no separate "BE deploy" and "FE deploy" step; bumping
the tag and running `docker compose pull && docker compose up -d` deploys
both sides together, in one operation.

### 3a. Commit → CI publishes the new images

The regenerated `config/config_bjj_validators.json` from step 2 hasn't been
committed yet — do that now:

```bash
git add config/config_bjj_validators.json
git commit -m "chore: redeploy reference scripts + minting policy (preprod cutover)"
git push
```

`.github/workflows/{chainsync,interaction-api,query-api,mcp-server}.yml` all
trigger on push (to `develop`/`main`) with `paths: - 'config/**'` (each also
matches its own `Dockerfile.*`, which does `COPY config ./config`) — so this
commit is what triggers CI to rebuild and publish new images, with the new
validator config baked in on top of the Stream E/F/G code already on `main`.

**Wait for CI to publish** before continuing — each workflow runs `cabal
build all && cabal test` (self-hosted) ahead of `docker build`/`docker push`,
so there is real latency between the push landing and a pullable image
existing. Confirm all four `build-and-push` jobs have finished (Actions tab,
or `gh run list --workflow=<name>.yml`) for this commit's SHA before moving
to 3b.

The published tag is the pushed commit's **short SHA** (`${GITHUB_SHA::7}`,
7 characters) — the same scheme this repo's own dev-stack `docker-compose.yml`
already pins against (e.g. `mariusgeorgescu/bjj-chainsync:e36ddc2`). Note
that short SHA; it's what gets set in 3b.

### 3b. Bump the tag and redeploy the unified stack (in `bjj-frontend`)

In the `bjj-frontend` repo, branch `val1-prerelease-blockers`:

1. In `bjj-frontend/.env` (the single `.env` shared by the whole stack —
   protocol + FE + BFF + agent-service), set `UPSTREAM_TAG` to the short SHA
   from 3a. (`bjj-frontend/docs/DEPLOYMENT.md`'s prose calls this
   `PROTOCOL_TAG`; the variable actually consumed by its `docker-compose.yml`
   and `.env.example` is `UPSTREAM_TAG` — use that name.)
2. Confirm the FE image (`DOWNSTREAM_TAG`, `mariusgeorgescu/bjj-frontend:dev`
   or `:prod`) was built from branch `val1-prerelease-blockers` (carries
   Stream C's fail-closed change). If the currently-pushed image predates
   that branch, rebuild and push it first, from that branch:

   ```bash
   npm run deploy-dev    # or deploy-prod — matches the tier this stack targets
   ```

   (`package.json`: both scripts run `docker buildx build ... --push` to
   `mariusgeorgescu/bjj-frontend:dev`/`:prod`.) Then set `DOWNSTREAM_TAG` in
   `.env` to match.
3. Confirm `bjj-frontend/.env` has `BASIC_USER`, `BASIC_PASS`, and
   `CORS_ALLOWED_ORIGINS` set to real values — Stream C's fail-closed change
   means the Haskell servers die at startup if `BASIC_USER`/`BASIC_PASS` are
   unset (no `cardano`/`lovelace` fallback), and the FE/BFF reads the same
   variables from the same file, so there's no separate "match FE to BE"
   step the way a two-repo deploy would need.
4. Pull and restart:

   ```bash
   docker compose pull
   docker compose up -d
   ```

   Healthchecks gate ordering: `postgres` → `chainsync` →
   (`interaction-api` | `query-api` | `ipfs`) → `mcp-server` →
   `agent-service` → `bjj-frontend` → `nginx`. Watch with `docker compose ps`.

`chainsync-service` is on schema **v4** (R1: `tx_hash`/`slot`/`output_index`
backfill), and this deploy also lands Stream E's new `MintingPolicy` hash. At
startup, chain-sync's `readSchemaProbe` reads the stored schema version and
minting-policy hex from `chain_sync_config`; if the stored schema version is
behind current, **or** the stored policy hex differs from the current one,
it wipes chain-sync's tables (`wipeChainSyncTablesRaw`) and re-syncs from
origin before running migrations (see `docs/architecture/chain-sync.md`).
Both conditions are independently true on this deploy — the v4 schema bump
and the new minting-policy hash — so the wipe fires either way. This is
expected, not a failure mode.

## 6. Step 4 — Repopulate + validate `[ops]`

Re-seed sample data against the new deployment:

```bash
direnv exec . scripts/populate_testnet.sh
```

(`populate_testnet.sh` skips deployment if `config/config_bjj_validators.json`
already exists — since step 2 already deployed and wrote that file, this run
populates data only, no `--force-redeploy` needed.)

Then run the validation checklist:

- `scripts/test_black_promotes_white_to_blue.sh` green against the new deploy
- chain-sync `/ready` returns 200 (re-sync caught up to tip)
- a promotion query response carries non-null `tx_hash`/`slot`/`output_index`
  → **proves R1 end-to-end** (no unit test covers this)
- one negative-path spot check per hardening (burn-fails,
  extra-oracle-mint-fails, spend-without-NFT-fails) — now partly covered by
  Stream F's automated negative tests
- fail-closed check: an unauthenticated request to a protected BE route
  returns 401, not data

## 7. Rollback table

Because old data is abandoned, rollback is cheap — no state to recover.

| Step fails | Blast radius | Recovery |
| --- | --- | --- |
| 1 Token | none on-chain | old key valid until revoked; revoke only after new key verified |
| 2 Redeploy | new refs half-published | re-run (`deploy-reference-scripts` always deploys fresh); old deployment untouched |
| 3 Cutover | CI publish fails, or re-sync fail/crashloop, or FE can't reach authed BE | CI failure: fix and re-push — no images changed yet, old deployment untouched. Post-pull failure: roll back `UPSTREAM_TAG`/`DOWNSTREAM_TAG` to their previous values in `bjj-frontend/.env` and `docker compose up -d`; previous `config_bjj_validators.json` is still in git history if the on-chain side also needs reverting. `BASIC_*` mismatch between FE and BE is structurally less likely now (one shared `.env`), but still verify parity if it happens |
| 4 Repopulate | partial sample data | wipe DB + re-run populate |

**Safety property:** the old deployment stays live until step 3 (§5 above), so
steps 1–2 are a non-destructive rehearsal. Step 3 is the single commit point,
gated by the pre-flight hash confirmation in §2.
