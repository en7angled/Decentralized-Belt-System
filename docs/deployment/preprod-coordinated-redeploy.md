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

The Maestro API token is committed in cleartext at `config/config_atlas.json`
(`coreProvider.maestroToken`, currently `DB7AVocLdKo22TtFxvuMlHKD7aofoJYQ`,
introduced in commit `e9514de`). It must be rotated and moved out of version
control before anything else in this sequence.

```bash
grep -n 'maestroToken\|MaestroToken\|apiKey' config/config_atlas.json
```

1. Issue a new Maestro API key from the Maestro dashboard (kills the old key
   as soon as it's issued/the old one is revoked).
2. Move the new token to `.env` (uncommitted, sourced by direnv per
   `CLAUDE.md` §Environment) instead of `config/config_atlas.json`. Use
   `decodeConfigEnvOrFile` conventions already in use elsewhere in this repo
   so the token is read from an env var, not the checked-in file.
3. Scrub `maestroToken` out of `config/config_atlas.json` (or replace with an
   empty/placeholder value that is overridden by the env var at load time).
4. Commit the scrubbed `config_atlas.json`.

The **old committed token is exposed in this repo's git history** regardless
of step 3 — scrubbing the working tree does not remove it from prior commits.
Rotation (step 1) neutralizes the exposure by making the leaked value useless.
Purging it from git history (`git filter-repo` / BFG) is **optional** and not
required for this cutover.

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

## 5. Step 3 — Deploy BE servers (fail-closed) `[ops]` — CUTOVER POINT

**This is the cutover point.** The old deployment stays fully live through
steps 1–2 above (non-destructive rehearsal); this step is where traffic moves
to the new on-chain state and new servers.

Deploy `interaction-api`, `query-api`, `chainsync-service`, and `mcp-server`
against the new `config/config_bjj_validators.json`, with the fail-closed env
vars set (Stream C — see `stream_c_api_hardening` memory note):

```bash
BASIC_USER=<value> BASIC_PASS=<value> CORS_ALLOWED_ORIGINS=<value> \
  direnv exec . cabal run exe:interaction-api
# repeat with the equivalent config for exe:query-api, exe:chainsync-service, exe:mcp-server
```

`chainsync-service` is on schema **v4** (R1: `tx_hash`/`slot`/`output_index`
backfill). On this deploy it detects the new validator hashes and **wipes +
re-syncs from origin** against the new deployment — this is expected, not a
failure mode.

## 6. Step 4 — Deploy FE in lockstep `[ops]`

The frontend's fail-closed change must deploy in lockstep with the fail-closed
BE servers above — mismatched `BASIC_*` values between FE and BE is the
primary failure mode here.

```bash
# in the bjj-frontend repo, branch val1-prerelease-blockers
BASIC_USER=<same value as BE> BASIC_PASS=<same value as BE> \
  <frontend deploy command>
```

Verify the FE's `BASIC_USER`/`BASIC_PASS` match the values set on the BE
servers in step 3 exactly.

## 7. Step 5 — Repopulate + validate `[ops]`

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

## 8. Rollback table

Because old data is abandoned, rollback is cheap — no state to recover.

| Step fails | Blast radius | Recovery |
| --- | --- | --- |
| 1 Token | none on-chain | old key valid until revoked; revoke only after new key verified |
| 2 Redeploy | new refs half-published | re-run (`deploy-reference-scripts` always deploys fresh); old deployment untouched |
| 3 BE servers | re-sync fail / crashloop | roll back image + previous `config_bjj_validators.json` (git history); re-sync from origin either way |
| 4 FE lockstep | FE can't reach authed BE | roll back FE image; usual cause is `BASIC_*` env mismatch — verify parity |
| 5 Repopulate | partial sample data | wipe DB + re-run populate |

**Safety property:** the old deployment stays live until step 3 (§5 above), so
steps 1–2 are a non-destructive rehearsal. Step 3 is the single commit point,
gated by the pre-flight hash confirmation in §2.
