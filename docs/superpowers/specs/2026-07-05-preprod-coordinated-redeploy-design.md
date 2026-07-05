# Preprod coordinated redeploy — design

**Date:** 2026-07-05
**Status:** approved (brainstorm)
**Scope:** one design → one runbook. Sequences all merged-but-undeployed backend
work into a single preprod cutover. Does **not** cover mainnet, the frontend
consumption track (verification panel / org-explorer wiring), or the R6/R7
product decisions — those are separate specs.

## 1. Problem

Several streams are merged to `main` but not deployed, and some cannot deploy
independently without hash/config mismatches:

- **R1** (this session) bumped chain-sync schema to **v4** → deploy triggers a
  full re-sync to backfill `tx_hash`/`slot`/`output_index`.
- **Stream E** (on-chain hardening, F-21/F-22a/F-22b) changed four validator
  script hashes → requires a real redeploy, not just a merge
  (see `docs/deployment/redeploy-onchain-hardening.md`).
- **Stream G**: committed `config/config_bjj_validators.json` is stale
  (minting-policy hash mismatch). This is **not a separate bug** — the config
  stores hashes *and* deployed reference-script refs, which only exist after a
  real deploy. It is stale precisely because E changed the hashes but has not
  been redeployed. The E redeploy regenerates the config and resolves G.
- **Stream C**: the `bjj-frontend` fail-closed change (`93bff37` on
  `val1-prerelease-blockers`) must deploy in lockstep with the fail-closed BE
  servers (which now require `BASIC_USER`/`BASIC_PASS`/`CORS_ALLOWED_ORIGINS`).
- **Security**: the Maestro token committed in `config/config_atlas.json` must
  be rotated.

## 2. Decisions (approved)

- **Target:** preprod (testnet) only. Mainnet is a later, separate cutover.
- **Old on-chain data:** abandon + re-populate. The validator-hash change makes
  this a fresh protocol instance; existing preprod profiles/ranks/achievements
  are stranded under the old policy and are accepted as lost (test/demo data
  only). Re-seed with `scripts/populate_testnet.sh`.
- **Role split:** I write the runbook and do the safe, reversible, local steps
  (pre-flight hash verification, drafting exact commands, committing the
  regenerated config). Ops runs everything requiring secrets/keys/network:
  Maestro rotation, `deploy-reference-scripts`, server + FE deploy, repopulate.
- **Cutover point:** the old deployment stays fully live until step 3 (repoint
  servers). Steps 1–2 are non-destructive.
- **Ordering:** token-first (kill the leak early), on-chain before servers
  (servers need the new config/oracle), BE before FE (FE fail-closed needs the
  authed BE live), repopulate last (needs the new policy).

## 3. Coordinated deploy sequence

```
0. PRE-FLIGHT (me — safe, local, reversible)
   • admin write-blueprint → diff regenerated hashes vs committed
     config_bjj_validators.json → confirm the 4 changed hashes match
     redeploy-onchain-hardening.md (report the exact change set)
   • confirm operation.prv + funded preprod wallet present (report only)
   • draft the exact command block per ops step

1. ROTATE MAESTRO TOKEN                         [ops — Maestro account]
   • issue new key → move to .env (uncommitted); scrub from config_atlas.json
   • old committed token dies when the new key is issued
   • (optional) purge old token from git history — marked optional

2. REDEPLOY ON-CHAIN                            [ops — operation.prv + preprod]
   • cabal run exe:admin -- deploy-reference-scripts
       → new reference scripts, new oracle NFT, new MintingPolicy,
         rewrites config_bjj_validators.json (resolves E + G)
   • I commit the regenerated config

3. DEPLOY BE SERVERS (fail-closed)              [ops]  <-- CUTOVER POINT
   • interaction / query / chainsync / mcp with
     BASIC_USER/BASIC_PASS/CORS_ALLOWED_ORIGINS + new config
   • chain-sync sees schema v4 → wipes + re-syncs against the NEW validators
     from origin (R1 tx_hash backfill happens here)

4. DEPLOY FE IN LOCKSTEP (C)                    [ops]
   • bjj-frontend val1-prerelease-blockers with matching BASIC_* env

5. RE-POPULATE + VALIDATE                       [ops runs; I provide checks]
   • scripts/populate_testnet.sh → fresh sample data
   • validation checklist (§5)
```

## 4. Rollback & failure handling

Because old data is abandoned, rollback is cheap — no state to recover.

| Step fails | Blast radius | Recovery |
| --- | --- | --- |
| 1 Token | none on-chain | old key valid until revoked; revoke only after new key verified |
| 2 Redeploy | new refs half-published | re-run (`deploy-reference-scripts` always deploys fresh); old deployment untouched |
| 3 BE servers | re-sync fail / crashloop | roll back image + previous `config_bjj_validators.json` (git history); re-sync from origin either way |
| 4 FE lockstep | FE can't reach authed BE | roll back FE image; usual cause is `BASIC_*` env mismatch — verify parity |
| 5 Repopulate | partial sample data | wipe DB + re-run populate |

**Safety property:** the old deployment stays live until step 3, so steps 1–2
are a non-destructive rehearsal. Step 3 is the single commit point, gated by
the step-0 pre-flight hash confirmation.

## 5. Validation checklist (step 5)

- `scripts/test_black_promotes_white_to_blue.sh` green against the new deploy
- chain-sync `/ready` returns 200 (re-sync caught up to tip)
- a promotion query response carries non-null `tx_hash`/`slot`/`output_index`
  → **proves R1 end-to-end** (no unit test covers this)
- one negative-path spot check per hardening (burn-fails,
  extra-oracle-mint-fails, spend-without-NFT-fails) — now partly covered by
  Stream F's automated negative tests
- fail-closed check: an unauthenticated request to a protected BE route
  returns 401, not data

## 6. Deliverables

- `docs/deployment/preprod-coordinated-redeploy.md` — the runbook (ordered
  sequence, exact commands, pre-flight gate, rollback, validation). Links the
  existing `redeploy-onchain-hardening.md` as the E-specific detail.
- Pre-flight hash-diff report (run before handing off).
- Maestro-rotation note (token location, move-to-.env, git-history exposure).

## 7. Out of scope

- Mainnet cutover (separate, after preprod sign-off).
- Data migration (explicitly rejected — abandon + repopulate).
- FE consumption track (verification panel, org-explorer wiring, error-code
  migration) — separate spec, deploys later.
- R6 / R7 product decisions.
- Git-history purge of the leaked token (optional; rotation alone kills it).
