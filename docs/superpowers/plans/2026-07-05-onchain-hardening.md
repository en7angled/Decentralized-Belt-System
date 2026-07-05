# On-chain Hardening Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add defensive on-chain constraints (F-21 burn, F-22a oracle exact-mint, F-22b NFT auth) + trust-model docs + the F-25 offchain wrapper, then regenerate the blueprint and hand off the redeploy.

**Architecture:** 5 tasks. Tasks 1-3 change validator hashes (defensive, no-op on the happy path — verified). Task 4 is hash-neutral (docs + offchain refactor). Task 5 regenerates the blueprint and documents the redeploy. Every hash-changing task's proof of safety is `cabal test` staying green (the happy-path emulator tests exercise every hardened validator).

**Tech Stack:** Haskell/GHC 9.6, PlutusTx, Cabal, Tasty/HUnit (CLB emulator).

**Reference spec:** `docs/superpowers/specs/2026-07-05-onchain-hardening-design.md`.

## Global Constraints

- **Every hash-changing check must be a no-op on the happy path** — `cabal test` MUST stay green after each. A failing emulator test means a hardening broke a legitimate flow — STOP and report.
- **Do NOT change any happy-path logic** — only ADD conjuncts to existing `and`/`&&` checks.
- **F-22b-3 (AcceptInterval NFT-auth) is EXCLUDED** — do not attempt it (needs a redeemer + offchain change; out of scope).
- **Negative tests pinning these hardenings are delivered in stream F**, not here — do not add them here.
- **Style:** max 120 chars/line, 2-space indent, match the surrounding validator style.
- **After each task:** `cabal build all` then `cabal test` must pass.

---

### Task 1: F-21 — constrain the burn side of every mint redeemer

**Files:** `src/lib/onchain-lib/Onchain/Validators/MintingPolicy.hs`, `src/lib/onchain-lib/Onchain/Validators/MembershipsValidator.hs`.

**Safety:** no offchain builder burns any protocol token (`txMustBurnCIP68UserAndRef` is dead code, never called; every live `mustMint` uses positive amounts), so `mintValueBurned txInfoMint == mempty` is already true on every legitimate tx. Verified.

- [ ] **Step 1: Add the burn conjunct to each MintingPolicy mint check.** In `src/lib/onchain-lib/Onchain/Validators/MintingPolicy.hs`, at each of the 6 mint-exactness checks, add `&& mintValueBurned txInfoMint == mempty` to the existing conjunction:
  - `:206` (M5, Practitioner CreateProfile): the `traceIfFalse "M5" (mintValueMinted txInfoMint == profileRefNFT + profileUserNFT + rankNFT)` → append `&& mintValueBurned txInfoMint == mempty` inside the `traceIfFalse`'s boolean.
  - `:223` (M6, Organization CreateProfile): same, on the `== profileRefNFT + profileUserNFT + membershipHistoriesRootNFT` check.
  - `:279` (Ma, Promote): `traceIfFalse "Ma" (mintValueMinted txInfoMint == pendingRankNFT)` → `... == pendingRankNFT && mintValueBurned txInfoMint == mempty`.
  - `:323` (Md, NewMembershipHistory): on the `== (membershipHistoryNFT + fstIntervalNFT)` check.
  - `:371` (Me, NewMembershipInterval): on the `== newIntervalNFT` check.
  - `:414` (Mj, NewAchievement): `traceIfFalse "Mj" (mintValueMinted txInfoMint == achievementNFT)` → `... && mintValueBurned txInfoMint == mempty`.

  Read each exact line first (locations are current but confirm). `mintValueMinted`/`mintValueBurned` come from `PlutusLedgerApi.V3` (imported wholesale — no new import); `mempty :: Value`.

- [ ] **Step 2: Add the burn conjunct to the 2 MembershipsValidator mint checks.** In `src/lib/onchain-lib/Onchain/Validators/MembershipsValidator.hs`:
  - `:196` (V5, InsertNodeToMHList): the `and [...]` element `mintValueMinted txInfoMint == (insertedNodeNFT + newIntervalNFT)` → `mintValueMinted txInfoMint == (insertedNodeNFT + newIntervalNFT) && mintValueBurned txInfoMint == mempty`.
  - `:229` (V6): the analogous mint-exactness check → append the same conjunct.

  (`PlutusLedgerApi.V3` already imported wholesale here too.)

- [ ] **Step 3: Build + test (safety proof).** Run: `cabal build all 2>&1 | tail -8 && cabal test 2>&1 | tail -6`. Expected: clean build, ALL emulator tests green (green = the 8 hardened mint redeemers still accept every legitimate mint tx — the no-op proof).

- [ ] **Step 4: Commit.**

```bash
git add src/lib/onchain-lib/Onchain/Validators/MintingPolicy.hs src/lib/onchain-lib/Onchain/Validators/MembershipsValidator.hs
git commit -m "feat(onchain): constrain burn side of every mint redeemer to mempty (F-21)"
```

---

### Task 2: F-22a — OracleNFTPolicy exact-mint

**Files:** `src/lib/onchain-lib/Onchain/Validators/OracleNFTPolicy.hs`.

**Safety:** the only builder (`mintOracleNFTAndLockDatum`) mints exactly `{policyCS: {"": 1}}` and nothing else; the exact-mint check holds. Verified.

- [ ] **Step 1: Add the exact-mint check.** In `src/lib/onchain-lib/Onchain/Validators/OracleNFTPolicy.hs`, `oracleNFTPolicyLambda` (~30-37), change the `MintingScript _ ->` branch to bind the currency symbol and add the exact-mint conjunct (mirroring `MintingPolicy.hs:128` for the `MintingScript cs` bind and `:279/:323/:414` for the `mintValueMinted == theNFT` pattern):

```haskell
    MintingScript mintingPolicyCurrencySymbol ->
      let theOracleNFT = V1.assetClassValue (V1.AssetClass (mintingPolicyCurrencySymbol, V1.TokenName emptyByteString)) 1
       in traceIfFalse "N0" (any ((== seedRef) . txInInfoOutRef) txInfoInputs)      -- Must spend seed UTxO (N0)
            && traceIfFalse "N2" (mintValueMinted txInfoMint == theOracleNFT)        -- Tx must mint JUST the oracle NFT (N2)
```

  Add `import PlutusLedgerApi.V1 qualified as V1` (for `assetClassValue`/`AssetClass`/`TokenName`); `mintValueMinted` is from the already-available `PlutusLedgerApi.V3`. `emptyByteString` — use the project's empty-`BuiltinByteString` (check how MintingPolicy/CIP68 express an empty token name; the offchain side uses `oracleNFTTN = ""`). Read the current lambda + a sibling policy's exact-mint check to match the exact accessor names before writing.

- [ ] **Step 2: Build + test.** Run: `cabal build all 2>&1 | tail -8 && cabal test 2>&1 | tail -6`. Expected: clean, green — the oracle deploy happens in every test's `deployBJJValidators`/`mintTestOracleNFT` setup, so green proves the exact-mint check accepts the real oracle mint.

- [ ] **Step 3: Commit.**

```bash
git add src/lib/onchain-lib/Onchain/Validators/OracleNFTPolicy.hs
git commit -m "feat(onchain): OracleNFTPolicy exact-mint check (F-22a)"
```

---

### Task 3: F-22b — defensive NFT authentication on state-spend handlers

**Files:** `src/lib/onchain-lib/Onchain/Validators/RanksValidator.hs`, `MembershipsValidator.hs`, `AchievementsValidator.hs`.

**Safety:** each happy-path spend resolves its UTxO by exactly that NFT (`getUTxOWithNFTOrThrow`, single match) and each NFT is minted once, so `ownValue` carries exactly 1; the check only rejects an attack spend of a state UTxO lacking its NFT. Cleanup is a separate handler (unaffected). Verified.

- [ ] **Step 1: RanksValidator PromotionAcceptance.** In `src/lib/onchain-lib/Onchain/Validators/RanksValidator.hs` (~83-108), add a conjunct to the `PromotionAcceptance` handler's `and [...]` list (`ownValue` and `promotionRankDatum` are already in scope):

```haskell
      traceIfFalse "R2b" (V1.assetClassValueOf ownValue (promotionId promotionRankDatum) == 1)
```

  Add `import PlutusLedgerApi.V1 qualified as V1` if not present (for `assetClassValueOf`). Read the handler to confirm `ownValue`/`promotionId promotionRankDatum` names and place the conjunct in the `and` list.

- [ ] **Step 2: MembershipsValidator handleUpdateNode.** In `MembershipsValidator.hs` `handleUpdateNode` (~200-230), add to its `and [...]` list (`historyId` computed at ~220, `ownValue` a handler param):

```haskell
      V1.assetClassValueOf ownValue historyId == 1
```

- [ ] **Step 3: AchievementsValidator AcceptAchievement.** In `AchievementsValidator.hs` (~71-95), add to the `AcceptAchievement` `and [...]` list (`achievement = extra achievementDatum`, `ownValue` in scope; `V1.assetClassValueOf` already used in this file):

```haskell
      V1.assetClassValueOf ownValue (achievementId achievement) == 1
```

- [ ] **Step 4: Build + test (safety proof).** Run: `cabal build all 2>&1 | tail -8 && cabal test 2>&1 | tail -6`. Expected: clean, green — the accept/update happy paths (`blackPromotesWhiteToBlue`, `orgThreeHistoriesOneUpdatesIntervalTest`, `practitionerAcceptsAchievementTest`, `fullAchievementLifecycleTest`) exercise all three handlers; green proves the NFT-auth accepts every legitimate spend.

- [ ] **Step 5: Commit.**

```bash
git add src/lib/onchain-lib/Onchain/Validators/RanksValidator.hs src/lib/onchain-lib/Onchain/Validators/MembershipsValidator.hs src/lib/onchain-lib/Onchain/Validators/AchievementsValidator.hs
git commit -m "feat(onchain): defensive NFT auth on PromotionAcceptance/handleUpdateNode/AcceptAchievement (F-22b)"
```

---

### Task 4: Docs (F-18, pause, rank, interval) + F-25 offchain wrapper (hash-neutral)

**Files:** `MintingPolicy.hs`, `docs/onchain-architecture.md`, `RanksValidator.hs`, `Onchain/Protocol/Core.hs`, `TxBuilding/SafeOnchainLogic.hs`, `TxBuilding/Operations.hs`.

- [ ] **Step 1: F-18 trust-model doc.** Add a comment in `MintingPolicy.hs` `handleCreateProfile` (~183) noting the initial rank (0-14) is self-attested (`rankAwardedByProfileId == profileId`), intentional per the onchain-minimal/mirror-and-pin philosophy, and a matching paragraph in `docs/onchain-architecture.md` (~328). Comment/doc only.

- [ ] **Step 2: Pause-gate scope doc.** Add a module comment near `MintingPolicy.hs:144` (the `opPaused` M0 gate) documenting that `opPaused` intentionally gates only NEW MINTS, not acceptances of already-minted state (which live in other validators, ungated) — expanding pause to accepts would be a design change (out of scope). Comment only.

- [ ] **Step 3: Rank-UTxO lineage note.** Add a module-level Haddock note in `RanksValidator.hs` (~top) about the intentional permanent-lineage / min-ADA tradeoff (rule #14): every historical rank stays locked at the validator, one min-ADA deposit per rank-up, borne by the fee-payer — intentional (belt history is permanent). Comment only.

- [ ] **Step 4: updateMembershipIntervalEndDate doc alignment.** In `Onchain/Protocol/Core.hs` (~213-216), edit ONLY the Haddock so the Organization line reflects that it is still gated by the tx-validity-range check (`isInValidRange`) — not "any future" date, but any date within the tx validity interval. Do NOT touch the logic (~217-235).

- [ ] **Step 5: F-25 offchain safe-wrapper.** In `src/lib/offchain-lib/TxBuilding/SafeOnchainLogic.hs`, add `safeAddMembershipIntervalToHistory` mirroring the existing `safe*` wrappers — it performs the SAME four checks currently inline in `Operations.hs:625-635` (HeadNumberMismatch, LastIntervalNotClosed [both branches], LastIntervalNotAccepted, InvalidNewIntervalEndDate — same conditions, order, exception constructors) then returns `Onchain.addMembershipIntervalToHistory ...`. Read the current inline checks and copy them verbatim into the wrapper. In `Operations.hs` `addMembershipIntervalTX` (~592), delete the 4 inline check blocks (~625-635) and replace the direct `let (updatedHistory, newInterval) = Onchain.addMembershipIntervalToHistory ...` (~638) with `(updatedHistory, newInterval) <- safeAddMembershipIntervalToHistory ...`. Behavior-preserving (verbatim checks).

- [ ] **Step 6: Build + test.** Run: `cabal build all 2>&1 | tail -8 && cabal test 2>&1 | tail -6`. Expected: clean, green. The F-25 refactor is covered by the existing happy-path (`orgThreeHistoriesOneUpdatesIntervalTest`) + 4 negative tests (`addMembershipIntervalFails*`) — all must stay green (proves the wrapper preserved the checks).

- [ ] **Step 7: Confirm hash-neutral.** The doc changes are comment-only and F-25 is offchain — validator hashes are unaffected. (The blueprint regen in Task 5 confirms; Tasks 1-3 are the intended hash changes.)

- [ ] **Step 8: Commit.**

```bash
git add src/lib/onchain-lib/Onchain/Validators/MintingPolicy.hs docs/onchain-architecture.md src/lib/onchain-lib/Onchain/Validators/RanksValidator.hs src/lib/onchain-lib/Onchain/Protocol/Core.hs src/lib/offchain-lib/TxBuilding/SafeOnchainLogic.hs src/lib/offchain-lib/TxBuilding/Operations.hs
git commit -m "docs(onchain): trust-model/pause/rank/interval notes; refactor(offchain): safeAddMembershipIntervalToHistory (F-18, F-25)"
```

---

### Task 5: Regenerate blueprint + record new hashes + deploy handoff

**Files:** the generated blueprint (`docs/generated/…` — locate it), `docs/onchain-architecture.md` or a new `docs/deployment/redeploy-onchain-hardening.md`.

- [ ] **Step 1: Regenerate the blueprint.** Run the project's blueprint command (`cabal run exe:admin -- write-blueprint`, or per CLAUDE.md/scripts). This writes the blueprint reflecting the NEW (hardened) validator code.

- [ ] **Step 2: Record which validator hashes changed.** Compare the regenerated blueprint's validator hashes against the committed blueprint (and note the divergence from the deployed `config/config_bjj_validators.json`, which is stale and will only update on redeploy). List every changed hash (expect: minting policy, ranks, memberships, achievements, oracle-NFT — whichever the hardenings touched).

- [ ] **Step 3: Commit the regenerated blueprint.** Stage and commit ONLY the regenerated blueprint artifact (it's a tracked generated file; do not hand-edit it). This makes `main`'s blueprint match `main`'s code.

```bash
git add <the regenerated blueprint path>
git commit -m "chore(onchain): regenerate blueprint for hardened validators (F-21/F-22)"
```

- [ ] **Step 4: Write the redeploy handoff doc.** Create `docs/deployment/redeploy-onchain-hardening.md` documenting: (a) this stream changed validator hashes (list them); (b) the owner must run `deploy-reference-scripts` to publish new reference scripts and regenerate `config/config_bjj_validators.json`; (c) validate on TESTNET before mainnet; (d) existing on-chain state under the OLD hashes remains spendable only by the OLD scripts — plan the migration/cutover; (e) this redeploy also reconciles the pre-existing stale config. Commit it.

```bash
git add docs/deployment/redeploy-onchain-hardening.md
git commit -m "docs(deploy): redeploy handoff for on-chain hardening (hash changes + migration)"
```

---

## Completion notes

- Tasks 1-3 intentionally change validator hashes → **redeploy required** (Task 5 handoff). Do NOT merge-and-forget: the deployed protocol runs the OLD validators until the owner redeploys and validates on testnet.
- Negative tests pinning these hardenings (burn-fails, extra-oracle-mint-fails, spend-without-NFT-fails) are stream F.
- The stale `config_bjj_validators.json` is reconciled by the redeploy, not by this stream.
