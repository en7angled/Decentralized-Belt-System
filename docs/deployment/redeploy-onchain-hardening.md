# Redeploy handoff: on-chain hardening (F-21, F-22a, F-22b)

Stream E (branch `feat/onchain-hardening`) changed on-chain validator/policy
code. This is a **breaking change at the script-hash level** — it requires a
real redeploy, not just a merge. This document is the handoff for whoever
owns that redeploy.

## 1. What changed and why

| ID | Change | File(s) |
| -- | ------ | ------- |
| F-21 | Constrain the **burn side** of every mint redeemer to `mempty` (defensive: closes off future burn-side abuse of `txInfoMint`; no legitimate off-chain builder burns protocol tokens today) | `Onchain/Validators/MintingPolicy.hs`, `Onchain/Validators/MembershipsValidator.hs` |
| F-22a | `OracleNFTPolicy` now does an **exact-mint check** (binds its own `CurrencySymbol`, requires `mintValueMinted == {policyCS: {"": 1}}`) — previously only the seed-spend was constrained, so a tx could mint the oracle NFT plus arbitrary extra tokens under the same policy id | `Onchain/Validators/OracleNFTPolicy.hs` |
| F-21/F-22a consistency | Also forbid burns in `OracleNFTPolicy`'s exact-mint check (N2), matching F-21 | `Onchain/Validators/OracleNFTPolicy.hs` |
| F-22b | Defensive NFT-ownership checks on state-spend handlers (`RanksValidator` `PromotionAcceptance`, `MembershipsValidator` `handleUpdateNode`, `AchievementsValidator` `AcceptAchievement`) — each now rejects spending a state UTxO that lacks its own authenticating NFT, instead of relying solely on forced co-execution with the minting policy | `Onchain/Validators/RanksValidator.hs`, `Onchain/Validators/MembershipsValidator.hs`, `Onchain/Validators/AchievementsValidator.hs` |

F-18 (trust-model/pause/rank/interval documentation notes) was doc-only and
does not affect any script hash.

Design/plan references: `docs/superpowers/specs/2026-07-05-onchain-hardening-design.md`,
`docs/superpowers/plans/2026-07-05-onchain-hardening.md`.

## 2. Validator hashes that changed

The blueprint (`docs/generated/bjj-belt-system-blueprint.json`) was
regenerated on this branch via:

```bash
cabal run exe:admin -- write-blueprint
```

Comparing the regenerated blueprint's validator hashes against the last
known-deployed reference (`config/config_bjj_validators.json`, committed on
`main`):

| Validator | Deployed (old) hash | Regenerated (new) hash | Changed? |
| --- | --- | --- | --- |
| Minting Policy | `e6353af3c3555a14c1f232c8f0b985cbd581fd24e14e60221cf295fc` | `091a10e63e6048ce2157560547032216ffaf6c227a35d6ff8a0d0c38` | **YES** (F-21) |
| Memberships Validator | `79ac62cfa3dc2da82a83921a12301e09a46e07be09c3f46a321a5668` | `9cf4f22e58a70eef8f6c2214ac64cb50803a2955611ad31fe38cbf71` | **YES** (F-21 + F-22b) |
| Ranks Validator | `91f4d324fcb63323b93bfbdd22952524854e52abe2e37c0e6541d675` | `8c777a5ab3f806819d9c32231415027ae6e869e655a6a0120771375e` | **YES** (F-22b) |
| Achievements Validator | `03c7daf20c54a9caeaaf8794b8e959bcb74ac148871dc44000ced729` | `f43bec2cdebd560c43a5e1f041ffe785b8298a24a3a6dbf25d547240` | **YES** (F-22b) |
| Profiles Validator | `001bec676b189dc1da726e5f5ad78edfeba58e843881175be629dae1` | `001bec676b189dc1da726e5f5ad78edfeba58e843881175be629dae1` | no (F-18 was doc-only) |
| Oracle Validator | `6ae4cfd1e52a199068cb73c83840bd70e079446723fd6b143309493d` | `6ae4cfd1e52a199068cb73c83840bd70e079446723fd6b143309493d` | no |
| **OracleNFTPolicy** | `e3a3495b0ebdb98140b5009772a6782237ff479b0bd84f622c7c295b` (currency symbol in `oracleNFTAssetClass`) | not in blueprint (see below) | **YES** (F-22a + burn consistency) — hash changes on every deployment regardless (one-shot policy), but the *logic* embedded in it has genuinely changed this stream |

**Important:** `OracleNFTPolicy` is deliberately **excluded from the
blueprint** (see `Onchain/Blueprint.hs` around the "one-shot minting policy"
comment) because it is parameterized by a `TxOutRef` known only at
deployment time — every deployment produces a unique policy id, so a static
blueprint entry is not meaningful. Its F-22a hardening cannot be verified via
a blueprint diff; it is verified by the code diff in commits `de605a5`
(exact-mint check) and `7fecbee` (burn consistency). Whoever runs the
redeploy should expect a **new `oracleNFTAssetClass`** (new currency symbol)
in the regenerated `config/config_bjj_validators.json`, in addition to the
four changed validator hashes above.

## 3. What the owner must do: redeploy reference scripts

The deployed reference-script config, `config/config_bjj_validators.json`,
was **already stale relative to `main` before this stream** (it reflects an
older Profiles/Ranks/Memberships/Achievements build — the committed blueprint
at the start of this stream didn't even have Achievements/Oracle entries).
This redeploy reconciles that pre-existing staleness at the same time as
publishing the F-21/F-22 hardening — there is no separate "just fix the
stale config" step needed.

To publish new reference scripts and regenerate the config:

```bash
# from repo root, with config/config_atlas.json and operation.prv in place
cabal run exe:admin -- deploy-reference-scripts
```

This is the same command `scripts/populate_testnet.sh` calls under the hood.
Per its own description, it performs the full flow in one shot:
1. Deploys `OracleValidator`, `ProfilesValidator`, `RanksValidator`,
   `MembershipsValidator`, `AchievementsValidator` as reference scripts.
2. Mints the oracle NFT and locks the initial `OracleParams` at the oracle
   validator (this mint is what makes `OracleNFTPolicy`'s hash/currency
   symbol deployment-specific).
3. Compiles `MintingPolicy` (parameterized by the oracle NFT's asset class)
   and deploys it as a reference script.
4. Overwrites `config/config_bjj_validators.json` unconditionally with the
   new hashes/refs/`oracleNFTAssetClass`.

Note: `deploy-reference-scripts` always deploys fresh and overwrites the
config; it does not check for an existing file. (`populate_testnet.sh` has
its own convenience guard that skips deployment if
`config/config_bjj_validators.json` already exists — pass
`--force-redeploy`, or remove/relocate the existing file first, if using
that script rather than the admin command directly.)

## 4. Validate on TESTNET before mainnet

Do not deploy directly to mainnet. Before any mainnet cutover:

1. Run `deploy-reference-scripts` against **preview/preprod** testnet
   (`config/config_atlas.json` → `"networkId": "preview"` or equivalent).
2. Run `scripts/populate_testnet.sh` (or `test_black_promotes_white_to_blue.sh`)
   against the new deployment to exercise init-profile, promote/accept,
   membership history/interval, and achievement flows end-to-end with the
   hardened validators.
3. Confirm the negative-path hardenings actually reject what they should:
   this stream added the *positive-path* constraints only; the *negative*
   tests that pin them (burn-fails, extra-oracle-mint-fails,
   spend-without-NFT-fails) land in stream F and are not yet part of the
   automated test suite. Until stream F lands, manually verify at least one
   rejection case per hardening on testnet (e.g. attempt a mint tx with a
   burn component, attempt to mint an extra token alongside the oracle NFT,
   attempt to spend a rank/membership/achievement UTxO via a path that
   bypasses minting-policy co-execution) before trusting the mainnet
   deployment.
4. Only after testnet validation should the same command be run against
   mainnet config.

## 5. Existing on-chain state under OLD hashes — migration/cutover

Reference scripts and their hashes are immutable once deployed. Redeploying
does **not** migrate existing UTxOs:

- All profiles, ranks, membership histories/intervals, and achievements
  currently locked at the **old** validator addresses remain spendable only
  by the **old** validator logic (pre-F-21/F-22). The old reference scripts
  must stay available (do not delete/garbage-collect them) for as long as
  any old-hash state remains unspent.
- New protocol activity (new profiles, promotions, etc.) after redeploy will
  use the **new** hashes/addresses and the new `MintingPolicy`/
  `OracleNFTPolicy` currency symbol. Old-hash and new-hash state are
  **not interchangeable** — they are different addresses with different
  currency symbols; the chain-sync/query layer will need to track both if
  historical old-hash data must remain queryable.
- Practical cutover options for the owner to decide between (out of scope
  for this task to choose):
  - **Clean cutover**: if current on-chain state is only test/demo data
    (this repo's testnet population scripts fall in this category), simply
    redeploy and repopulate; old state can be abandoned.
  - **Migration**: if there is real user state under the old hashes, plan an
    explicit migration transaction path (spend old-hash UTxOs under the old
    validator's rules, mint/re-create equivalent new-hash state under the
    new validator) before decommissioning old references.
  - **Dual-track**: keep both old and new reference scripts + chain-sync
    projections live simultaneously during a transition window, directing
    new activity to the new hashes while old-hash state winds down
    naturally.
- After redeploy, `config/config_bjj_validators.json` reflects only the
  **new** deployment. If old-hash reference scripts/config values are still
  needed operationally during a migration window, keep a copy of the
  pre-redeploy `config/config_bjj_validators.json` (or retrieve it from git
  history) separately — this repo's committed config always reflects the
  current/latest deployment only.

## 6. Summary checklist for the owner

- [ ] Review changed validator source: commits `dcaeac8`, `de605a5`,
      `7fecbee`, `1f4cb2d` on `feat/onchain-hardening`.
- [ ] Run `cabal run exe:admin -- deploy-reference-scripts` against
      **testnet** first.
- [ ] Validate hardened behavior on testnet (see §4), including manual
      negative-path checks until stream F's automated tests land.
- [ ] Decide and execute a migration/cutover plan for any real state under
      old hashes (see §5).
- [ ] Re-run `deploy-reference-scripts` against **mainnet** only after
      testnet sign-off.
- [ ] Confirm `config/config_bjj_validators.json` is updated and committed
      as part of the mainnet deploy step (reconciles the pre-existing
      staleness noted above).
- [ ] Track stream F (negative tests pinning F-21/F-22a/F-22b) to close the
      test-coverage gap called out in §4.
