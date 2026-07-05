# Low-tier Cleanup & Correctness Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Clear the Low-tier backlog (dead code, debug leftovers, cabal/script hygiene, behavior-preserving dedup, small correctness fixes, on-chain cosmetics) grouped by risk, with on-chain changes blueprint-hash-gated.

**Architecture:** 7 tasks, one per risk cluster (mechanical deletions → debug/raw-errors → cabal/scripts → dedup → correctness → on-chain-safe → INLINEABLE-hash-gated). Every location below was verified against the CURRENT code (the review was stale post-B/C/D).

**Tech Stack:** Haskell/GHC 9.6, Cabal, PlutusTx, esqueleto/persistent, Tasty/HUnit, bash.

**Reference spec:** `docs/superpowers/specs/2026-07-05-low-tier-cleanup-design.md`.

## Global Constraints

- **Risk isolation.** Mechanical deletions/cosmetics must not change behavior. Behavior changes (cluster 5, the F-26 dedup) are intentional and, where pure, tested. On-chain INLINEABLE is reverted if it changes any validator hash.
- **On-chain hash gate.** After ANY onchain-lib change that could affect compiled code, run `admin write-blueprint` and diff validator hashes against `config/config_bjj_validators.json`. Name/comment/export-list changes are hash-neutral; INLINEABLE is NOT — revert it if a hash changed.
- **All cabal dependency edits live in Task 3** (avoid cross-task cabal churn); Task 1 touches cabal only for the `other-modules` module removal.
- **Style:** max 120 chars/line, 2-space indent, `-- |` Haddock on new exports.
- **After each task:** `cabal build all` then `cabal test` must pass (green + no new warnings).

---

### Task 1: Cluster 1 — dead code deletion

**Files:** `Lookups.hs`, `TxBuilding/Utils.hs`, `Skeletons.hs`, `Query/Projected.hs`, `Exceptions.hs`, `interaction-api/Types.hs` (delete), `mcp-server-lib/MCPServer/Tools/Common.hs`, `Decentralized-Belt-System.cabal` (other-modules only).

- [ ] **Step 1: Delete the unused functions.** Remove each definition (signature + body) entirely:
  - `getAllProfilesCount` — `src/lib/offchain-lib/TxBuilding/Lookups.hs` (~146-147)
  - `pPOSIXTimeFromSlotInteger` — `src/lib/offchain-lib/TxBuilding/Utils.hs` (~55-56)
  - `oracleParamsFromGYOutDatum` — `src/lib/offchain-lib/TxBuilding/Utils.hs` (~162-164)
  - `getRefScriptAtTxOutRef` — `src/lib/offchain-lib/TxBuilding/Skeletons.hs` (~21-…; read the full body first, remove the whole binding)
  - `getMembershipHistoriesAsHistory` — `src/exe/query-api/Query/Projected.hs` (~756-757)
  - `getMembershipIntervalsAsInterval` — `src/exe/query-api/Query/Projected.hs` (~843-844)

  Each is confirmed unused repo-wide. If any is listed in a module export list, remove that export entry too. Grep each name after deleting to confirm zero remaining references.

- [ ] **Step 2: Remove the two dead exception constructors.** In `src/lib/offchain-lib/TxBuilding/Exceptions.hs`, delete `PromotionNotFound` and `MembershipHistoryNotFound` from the `TxBuildingException` data declaration (~40, 42) AND their `displayException` clauses (~83-84) AND their `txBuildingExceptionToHttpStatus` clauses (~136-137). `displayException` is an exhaustive match, so all three must go together. Grep the repo for both names afterward — expect zero hits (they are never thrown).

- [ ] **Step 3: Delete the dead `Types` module.** Delete `src/exe/interaction-api/Types.hs` (whole file — unimported). Remove the `Types` line from the `executable interaction-api` `other-modules` list in `Decentralized-Belt-System.cabal` (~line 300).

- [ ] **Step 4: Remove `_unusedBL8`.** In `src/lib/mcp-server-lib/MCPServer/Tools/Common.hs`, delete the `import … as BL8` line (~29), the explanatory comment (~170), and the `_unusedBL8` signature+binding (~171-172) — all four lines, so the import doesn't become unused-then-warned.

- [ ] **Step 5: Build + verify.** Run: `cabal build all 2>&1 | tail -15`. Expected: clean, zero warnings. `grep -rn "getAllProfilesCount\|pPOSIXTimeFromSlotInteger\|oracleParamsFromGYOutDatum\|getRefScriptAtTxOutRef\|getMembershipHistoriesAsHistory\|getMembershipIntervalsAsInterval\|PromotionNotFound\|MembershipHistoryNotFound\|_unusedBL8" src` → no hits. `cabal test 2>&1 | tail -5` green.

  (Note: `webapi-lib`'s unused `deriving-aeson` dependency is removed in Task 3 with the other cabal dep cleanup — do NOT touch build-depends here.)

- [ ] **Step 6: Commit.**

```bash
# Explicit paths only — the pre-existing untracked files (.claude/, .cursor/, .mcp.json,
# .psc-ide-port, FableReview-0407.md) must NEVER be staged. `git add <path>` stages the
# Types.hs deletion for a tracked file.
git add src/lib/offchain-lib/TxBuilding/Lookups.hs src/lib/offchain-lib/TxBuilding/Utils.hs src/lib/offchain-lib/TxBuilding/Skeletons.hs src/exe/query-api/Query/Projected.hs src/lib/offchain-lib/TxBuilding/Exceptions.hs src/exe/interaction-api/Types.hs src/lib/mcp-server-lib/MCPServer/Tools/Common.hs Decentralized-Belt-System.cabal
git commit -m "chore(cleanup): remove dead code — unused functions, exception ctors, Types module, _unusedBL8"
```

---

### Task 2: Cluster 2 — debug leftovers & the chain-sync-crashing raw error

**Files:** `TxBuilding/Transactions.hs`, `TxBuilding/Operations.hs`, `chainsync-lib/KupoAtlas.hs`, and (lower priority) `TxBuilding/Utils.hs`, `Utils.hs`.

- [ ] **Step 1: Remove debug prints.**
  - `src/lib/offchain-lib/TxBuilding/Transactions.hs:86` — delete the `print interaction` line (dumps addresses on every admin tx).
  - `src/lib/offchain-lib/TxBuilding/Operations.hs:438-439` — delete the two `gyLogInfo'` datum-dump lines in `acceptPromotionTX` (they log full datums on every accept, INFO level, with a label-concat bug).

- [ ] **Step 2: Fix `decodeGYScriptHash` (F-38, the one that can crash chain-sync).** In `src/lib/chainsync-lib/KupoAtlas.hs` (~41-42), the current `decodeGYScriptHash t = fromString (T.unpack t)` uses a partial `IsString` instance that throws at runtime on invalid hex, called at ~:100 inside `kupoMatchToAtlasMatch`'s `Either String AtlasMatch` builder — a malformed Kupo `script_hash` crashes live ingestion. Change it to return `Either String GYScriptHash` using a safe parse, mirroring the sibling `decodeGYDatumHash`/`decodeGYDatum` in the same module (read them for the exact safe-decode idiom, e.g. a hex-parse returning `Maybe`/`Either` + `maybeToEither`/`Left` on failure). Thread it through the `<$>`/do-block at the call site (~:100) — it is already inside the `Either` monad, so `gyScriptHash <- decodeGYScriptHash …` composes with no happy-path behavior change; a malformed hash now yields `Left` (skipped/logged) instead of crashing.

- [ ] **Step 3: (Lower priority) raw `error` → `die`.** In `src/lib/offchain-lib/TxBuilding/Utils.hs:86,93` (`readMnemonicFile`, CLI-only) and `src/lib/offchain-lib/Utils.hs:57` (`decodeConfigEnvOrFile`, startup-only across executables), replace `error msg` with `System.Exit.die msg` (import `System.Exit (die)` where needed) so these are clean process exits, not partial-function crashes. Keep it minimal — same messages.

- [ ] **Step 4: Build + test.** Run: `cabal build all 2>&1 | tail -10 && cabal test 2>&1 | tail -5`. Expected: clean, green. Manually confirm `decodeGYScriptHash`'s call site compiles inside the `Either` block and the happy path is unchanged.

- [ ] **Step 5: Commit.**

```bash
git add src/lib/offchain-lib/TxBuilding/Transactions.hs src/lib/offchain-lib/TxBuilding/Operations.hs src/lib/chainsync-lib/KupoAtlas.hs src/lib/offchain-lib/TxBuilding/Utils.hs src/lib/offchain-lib/Utils.hs
git commit -m "fix(chainsync): decodeGYScriptHash returns Either (no live crash); drop debug prints; raw error->die (F-38, F-39)"
```

---

### Task 3: Cluster 3 — cabal + scripts hygiene

**Files:** `Decentralized-Belt-System.cabal`, `src/lib/chainsync-lib/KupoClient.hs`, `src/lib/chainsync-lib/KupoAtlas.hs`, `scripts/populate_testnet.sh`, `scripts/populate_recent_activity.sh`, `scripts/test_black_promotes_white_to_blue.sh`.

- [ ] **Step 1: Add `-Wunused-packages`.** Add `-Wunused-packages` to the `ghc-options` in the `common common-options` stanza (`Decentralized-Belt-System.cabal` ~27-30). Run `cabal build all 2>&1 | grep -i "unused-packages\|not used"` to see the warnings (build stays green — no `-Werror`).

- [ ] **Step 2: Remove unused dependencies, one component at a time.** For each component, delete the listed unused deps from its `build-depends`, then `cabal build <component>` to confirm it still compiles before moving on (a genuinely-needed transitive re-export would break — if so, keep that dep and note it):
  - `library webapi-lib`: `deriving-aeson`, `servant`
  - `library mcp-server-lib`: `onchain-lib`, `mtl`, `servant`, `time`
  - `executable interaction-api`: `onchain-lib`, `esqueleto`, `monad-logger`, `multiset`, `persistent`, `persistent-postgresql`
  - `executable query-api`: `deriving-aeson`, `http-types`
  - `executable chainsync-service`: `onchain-lib`, `aeson`, `mtl`, `plutus-ledger-api`, `plutus-tx`, `wai`
  - `executable admin`: `plutus-tx`
  - `test-suite mcp-integration`: `aeson`
  - **Keep** `onchain-lib`'s `plutus-tx-plugin` (used via `-fplugin`, a false positive).

- [ ] **Step 3: Consolidate bounds + pragmas.** After Step 2, the duplicated bounded deps (`bytestring`/`http-types`/`lens`) are simpler (`http-types` gone from query-api). Leave the remaining bounds as-is unless trivially consolidatable. In `src/lib/chainsync-lib/KupoClient.hs`, delete the 5 redundant `LANGUAGE` pragmas (lines 1-5: `DataKinds`, `DeriveGeneric`, `OverloadedStrings`, `TemplateHaskell`, `TypeOperators` — all in the library's `default-extensions`), keep `InstanceSigs` (line 6). Delete the lone `{-# LANGUAGE OverloadedStrings #-}` at `src/lib/chainsync-lib/KupoAtlas.hs:1`.

- [ ] **Step 4: Remove hardcoded `extra-lib-dirs`.** Delete `extra-lib-dirs: /usr/lib /usr/local/lib` from the `interaction-api` (~293) and `query-api` (~345) stanzas (verified the Nix build succeeds without them).

- [ ] **Step 5: Scripts.** Add `set -u` after the existing `set -e`/`set -o pipefail` in `scripts/populate_testnet.sh` (~37), `scripts/populate_recent_activity.sh` (~47), `scripts/test_black_promotes_white_to_blue.sh` (~7). Replace the hardcoded `--fee-address "addr_test1qz2fxv2..."` at `populate_testnet.sh:274` and `test_black_promotes_white_to_blue.sh:353` with `--fee-address "${FEE_ADDRESS:-addr_test1qz2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzer3jcu5d8ps7zex2k2xt3uqxgjqnnj83ws8lhrn648jjxtwq2ytjqp}"` (env override with the current value as fallback — no behavior change for existing runs).

- [ ] **Step 6: (Last, lower priority) unify `default-language`.** Change the 4 core libs (`onchain-lib` ~61, `chainsync-lib` ~130, `offchain-lib` ~163, `webapi-lib` ~233) from `Haskell2010` to `GHC2021`. Then `cabal build all` — if ANY component regresses (an extension-set change breaks compilation), revert this step and note it; it is the lowest-value item.

- [ ] **Step 7: Build + test.** Run: `cabal build all 2>&1 | tail -15 && cabal test 2>&1 | tail -5`. Expected: clean (no unused-packages warnings remain for the cleaned components), green.

- [ ] **Step 8: Commit.**

```bash
git add Decentralized-Belt-System.cabal src/lib/chainsync-lib/KupoClient.hs src/lib/chainsync-lib/KupoAtlas.hs scripts/populate_testnet.sh scripts/populate_recent_activity.sh scripts/test_black_promotes_white_to_blue.sh
git commit -m "chore(build): -Wunused-packages + drop unused deps; redundant pragmas; extra-lib-dirs; script set -u + fee env (F-44, F-45)"
```

---

### Task 4: Cluster 4 — behavior-preserving dedup (+ F-26 fix + F-24 consistency)

**Files:** `Query/Projected.hs`, `DomainTypes/Transfer/Filters.hs` (+ call sites), `TxBuilding/Lookups.hs`, `Query/Aggregates.hs`, `DomainTypes/Rules/Promotion.hs`.

- [ ] **Step 1: Dedup `toIntervalInfo` (zero-risk).** In `src/exe/query-api/Query/Projected.hs`, the two byte-identical `toIntervalInfo` local `let`s (~802-814 in `getMembershipHistories`, ~877-889 in `getMembershipIntervals`) → extract one top-level helper (name it `toIntervalInfo`, same body, taking the projection row) and call it from both sites. Do NOT unify with `Lookups.hs`'s `intervalToInformation` (different Maybe-org signature).

- [ ] **Step 2: Drop vestigial `Maybe` from `Filters.*FromParams`.** In `src/lib/offchain-lib/DomainTypes/Transfer/Filters.hs`, the 5 functions `profileFilterFromParams`, `promotionsFilterFromParams`, `membershipHistoryFilterFromParams`, `membershipIntervalFilterFromParams`, `achievementFilterFromParams` always return `Just` — change their return type from `Maybe XFilter` to `XFilter` (drop the `Just`). Leave `activityFilterFromParams` (genuinely `Maybe`). `grep -rn "FilterFromParams" src` and update every call site to consume the non-`Maybe` value (they currently thread the always-`Just` through; adjust to pass the filter directly, wrapping in `Just` only where a downstream signature still needs `Maybe`). Build-verify.

- [ ] **Step 3: `get*DatumAndValue` dedup that FIXES F-26.** In `src/lib/offchain-lib/TxBuilding/Lookups.hs`, `getProfileStateDatumAndValue` (~75-80), `getRankStateDatumAndValue` (~82-87), `getAchievementDatumAndValue` (~448-453) share the shape `getUTxOWithNFTOrThrow gyAC <notFound>` then parse. Currently rank/achievement throw the same not-found constructor on a PARSE failure (F-26). Add a shared helper that takes distinct not-found and parse-fail exceptions:

```haskell
-- | Look up the single UTxO holding an NFT and parse its state datum+value.
-- Throws @onNotFound@ if the UTxO is absent, @onParseFail@ if its datum does not
-- parse — so a genuine parse failure is distinguishable from a missing UTxO (F-26).
getStateOrThrow ::
  (GYTxQueryMonad m) =>
  GYAssetClass ->
  (GYUTxO -> Maybe (a, Value)) ->
  TxBuildingException ->
  TxBuildingException ->
  m (a, Value)
getStateOrThrow gyAC parse onNotFound onParseFail = do
  utxo <- getUTxOWithNFTOrThrow gyAC onNotFound
  case parse utxo of
    Just r -> return r
    Nothing -> throwError (GYApplicationException onParseFail)
```

  Rewrite the three functions to call `getStateOrThrow` with their own `*FromUTxO` parser, their existing not-found exception, and `DatumParseError` as the parse-fail exception (this is the F-26 fix — rank/achievement parse failures now surface `DatumParseError` instead of a misleading not-found). Leave `getMembershipDatumAndValue` (~246-254) as-is (different shape — extra inline-datum branch). Verify the exact parser/exception names against the current code before wiring.

- [ ] **Step 4: Make `loadPractitionerOrOrg` consistent with F-24.** In `src/exe/query-api/Query/Aggregates.hs`, `loadPractitionerOrOrg` (~235-257) still 404s for a rankless profile, whereas `resolveProfileForPromotionSide` (~106-145, updated by stream D) returns a `placeholderPractitioner`. Add the same rankless-placeholder fallback to `loadPractitionerOrOrg`'s final failure branch (mirror the datum-fetch → `placeholderPractitioner` logic that `resolveProfileForPromotionSide` already uses), so awarder maps (practitioner/organization detail) never 404 a rankless awarder — consistent with F-24. Reuse `placeholderPractitioner` (already exported/available in this module). This is an intentional, small behavior improvement.

- [ ] **Step 5: Extract shared helpers in `checkPromotion`/`checkPromotionWithOptionalMaster`.** In `src/lib/offchain-lib/DomainTypes/Rules/Promotion.hs`, extract the byte-identical `rungViolation` (~136-139 / ~174-177) and `timeInGradeViolation` (~143-149 / ~179-185) into shared top-level helpers, called from both functions. Leave each function's unique parts (master-authority / date-ordering logic) untouched, preserving list ordering. This is pinned by `prop_checkPromotionMirrorsValidate` (property test on `checkPromotion`).

- [ ] **Step 6: Build + test (property tests are the gate).** Run: `cabal build all 2>&1 | tail -10 && cabal test 2>&1 | tail -8`. Expected: clean, ALL green — especially `prop_checkPromotionMirrorsValidate` and any Promotion-rules / query tests must pass identically (proves the dedup preserved behavior). The F-26 and F-24-consistency changes are intentional behavior changes with no test currently pinning the old behavior; confirm no existing test asserted the old (wrong) not-found/404.

- [ ] **Step 7: Commit.**

```bash
git add src/exe/query-api/Query/Projected.hs src/lib/offchain-lib/DomainTypes/Transfer/Filters.hs src/lib/offchain-lib/TxBuilding/Lookups.hs src/exe/query-api/Query/Aggregates.hs src/lib/offchain-lib/DomainTypes/Rules/Promotion.hs
# plus any RestAPI call sites touched by Step 2
git commit -m "refactor(query): dedup (toIntervalInfo, Filters Maybe, get*DatumAndValue->F-26 fix, promotion rules); loadPractitionerOrOrg F-24 consistency"
```

---

### Task 5: Cluster 5 — correctness fixes (with F-37 pure test)

**Files:** `DomainTypes/Core/Types.hs`, `TxBuilding/Exceptions.hs`, `TxBuilding/Operations.hs`, `Query/Live.hs`, `Storage.hs`, and a test file.

- [ ] **Step 1: Write the failing F-37 test.** In the test-suite (a new `UnitTests/PromotionState.hs` or add to an existing pure-domain test module — wire into `UnitTests.hs` + cabal `other-modules` if new), add:

```haskell
testCase "same current and proposed belt is Superseded (F-37)" $
  [ promotionStateFromBelts (Just b) b | b <- [minBound .. maxBound] ]
    @?= replicate (length [minBound .. maxBound :: BJJBelt]) PromotionSuperseded
```

  Import `promotionStateFromBelts`, `PromotionState (..)`, `BJJBelt` from their modules. Run `cabal test` — expect FAIL (currently returns `PromotionPending` for `current == proposed`).

- [ ] **Step 2: Fix F-37.** In `src/lib/offchain-lib/DomainTypes/Core/Types.hs` (~156-160), change the guard in `promotionStateFromBelts` from `| current > proposed = PromotionSuperseded` to `| current >= proposed = PromotionSuperseded`. Run the test → PASS.

- [ ] **Step 3: Fix F-36 (status mapping).** In `src/lib/offchain-lib/TxBuilding/Exceptions.hs`, add explicit clauses BEFORE the `_ = 400` catch-all (~155): `txBuildingExceptionToHttpStatus ScriptNotFound = 503`, `txBuildingExceptionToHttpStatus OracleDatumInvalid = 500`, `txBuildingExceptionToHttpStatus DatumParseError = 500`. (Optional: a small mapping unit test asserting these three.)

- [ ] **Step 4: Narrow the F-35 swallows.**
  - `src/lib/offchain-lib/TxBuilding/Operations.hs:805-806` (`updateEndDateTX`): the `catchError (True <$ …ProfileNotFound…) (const (return False))` collapses ALL errors to `False`. Narrow it to only treat a `ProfileNotFound` (or the specific not-found) as `False` and rethrow anything else — inspect the caught exception and `throwError e` unless it is the expected not-found. (Match the pattern used elsewhere for `GYApplicationException`/`cast` to `TxBuildingException`.)
  - `src/exe/query-api/Query/Live.hs:275` (`assignPromotionState`): the `try @SomeException … fromRight Nothing` maps any error to `Nothing`→`PromotionPending`. Narrow to catch only the expected not-found; on an unexpected exception, log and rethrow (or at least don't silently report `PromotionPending`).
  - `src/lib/offchain-lib/Storage.hs:290,296` (projection errors only `putStrLn`): upgrade to a clearly-labeled error log (e.g. `putStrLn ("PROJECTION ERROR (dropped, raw match retained): " <> …)`). **Do NOT** add a dead-letter table (schema change, out of scope) — leave a `-- TODO(stream ?): dead-letter table for replayable failed projections` comment.

- [ ] **Step 5: F-40 (narrowed) — `getPromotionsCount` SQL COUNT.** In `src/exe/query-api/Query/Projected.hs` (~621-622), `getPromotionsCount = length <$> getPromotions Nothing filter Nothing`. Convert to a SQL `COUNT(*)` matching the promotion filter, mirroring the sibling `getProfilesCount`/`getAchievementsCount` (which already use `selectOne { … pure countRows }`) — **only if** the promotion filter is fully SQL-expressible. If any part of the filter requires the loaded promotion rows (e.g. a state predicate that needs `promotionStateFromBelts`), leave it as `length <$> getPromotions …` and add a comment explaining why. Implementer decides after reading the filter application.

- [ ] **Step 6: Build + test.** Run: `cabal build all 2>&1 | tail -10 && cabal test 2>&1 | tail -8`. Expected: clean, green, F-37 test passes.

- [ ] **Step 7: Commit.**

```bash
# Explicit paths only (never stage the pre-existing untracked files). Include the new
# test file + UnitTests.hs + cabal if a new test module was added in Step 1.
git add src/lib/offchain-lib/DomainTypes/Core/Types.hs src/lib/offchain-lib/TxBuilding/Exceptions.hs src/lib/offchain-lib/TxBuilding/Operations.hs src/exe/query-api/Query/Live.hs src/lib/offchain-lib/Storage.hs src/exe/query-api/Query/Projected.hs src/test/UnitTests.hs src/test/UnitTests/PromotionState.hs Decentralized-Belt-System.cabal
git commit -m "fix(correctness): promotionState Superseded (F-37) + status mapping (F-36) + narrow swallowed errors (F-35) + getPromotionsCount COUNT (F-40)"
```

---

### Task 6: Cluster 6a — on-chain SAFE cosmetics (hash-neutral)

**Files:** `Onchain/CIP68.hs`, `Onchain/Validators/MintingPolicy.hs`, `Onchain/Validators/AchievementsValidator.hs`, `Onchain/Validators/ProfilesValidator.hs`, `Onchain/Protocol/Core.hs`.

- [ ] **Step 1: Fix the typo.** Rename `otherMetdataEncoded` → `otherMetadataEncoded` at all 4 sites: `CIP68.hs:108` (binder), `:116` (use), `MintingPolicy.hs:389` (binder), `:408` (arg). Pure local-name change (erased in Plutus Core).

- [ ] **Step 2: Fix AchievementsValidator docs.** In `src/lib/onchain-lib/Onchain/Validators/AchievementsValidator.hs`: correct the copy-pasted module Haddock (~12, currently "Ranks validator…") to describe the Achievements validator; add the missing explicit export list to the module declaration (~13) matching the sibling validators' style (export the same surface they do — the typed/untyped/compile bindings used via TH stay in-module, so export what an importer needs, e.g. the compiled script accessor); fix the stale "Ranks Redeemer" section comment (~29) to "Achievements Redeemer".

- [ ] **Step 3: Fix dangling doc references.** The `onchain-security-audit.md` doc was deleted (commit dc74b1f). Rewrite the comments at `ProfilesValidator.hs:57`, `:178`, and `Core.hs:290` to state the rationale inline (the "R2 redundancy"/"R4 optimization" reasoning) without the dead filename.

- [ ] **Step 4: Build + hash check.** Run: `cabal build all 2>&1 | tail -8`. Then regenerate the blueprint and confirm hashes are UNCHANGED (these changes are name/comment/export-list only, which are hash-neutral, but verify): `cabal run exe:admin -- write-blueprint` (or the project's blueprint command per CLAUDE.md), then compare the validator hashes in the regenerated blueprint / `config/config_bjj_validators.json` against the committed ones — expect NO change. `cabal test 2>&1 | tail -5` green.

- [ ] **Step 5: Commit.**

```bash
git add src/lib/onchain-lib/Onchain/CIP68.hs src/lib/onchain-lib/Onchain/Validators/MintingPolicy.hs src/lib/onchain-lib/Onchain/Validators/AchievementsValidator.hs src/lib/onchain-lib/Onchain/Validators/ProfilesValidator.hs src/lib/onchain-lib/Onchain/Protocol/Core.hs
git commit -m "docs(onchain): fix otherMetadataEncoded typo, AchievementsValidator haddock/exports, dangling audit-doc refs (hash-neutral)"
```

---

### Task 7: Cluster 6b — INLINEABLE additions (HASH-GATED: revert if hash changes)

**Files:** `ProfilesValidator.hs`, `RanksValidator.hs`, `MembershipsValidator.hs`, `AchievementsValidator.hs`, `CIP68.hs`.

- [ ] **Step 1: Record current validator hashes.** Before changing anything, capture the current validator hashes from `config/config_bjj_validators.json` (and/or run the blueprint command and record the hashes) so you can diff after.

- [ ] **Step 2: Add the INLINEABLE pragmas.** Add `{-# INLINEABLE #-}` above: `profilesUntyped` (`ProfilesValidator.hs:197`), `ranksUntyped` (`RanksValidator.hs:118`), `membershipsUntyped` (`MembershipsValidator.hs:294`), `achievementsUntyped` (`AchievementsValidator.hs:95`), `metadataVersion` (`CIP68.hs:50`).

- [ ] **Step 3: Regenerate the blueprint and DIFF hashes.** Run `cabal build all`, then the blueprint command (`cabal run exe:admin -- write-blueprint` or per CLAUDE.md), then compare EVERY validator hash against the values recorded in Step 1 / `config/config_bjj_validators.json`.

- [ ] **Step 4: Decide based on the hash diff.**
  - **If NO hash changed:** keep the pragmas. Run `cabal test`. Commit.
  - **If ANY hash changed:** `git checkout -- <the validator files>` to REVERT the INLINEABLE additions (a hash change cannot be merged without a coordinated validator redeploy). Record the reverted items as a **stream-E (redeploy) follow-up** in the task report. Do NOT commit the pragma changes. (The task still "succeeds" — the correct outcome of the hash gate is either keep-if-neutral or revert-if-not.)

- [ ] **Step 5: Commit (only if hashes unchanged).**

```bash
git add src/lib/onchain-lib/Onchain/Validators/ProfilesValidator.hs src/lib/onchain-lib/Onchain/Validators/RanksValidator.hs src/lib/onchain-lib/Onchain/Validators/MembershipsValidator.hs src/lib/onchain-lib/Onchain/Validators/AchievementsValidator.hs src/lib/onchain-lib/Onchain/CIP68.hs
git commit -m "perf(onchain): add INLINEABLE to untyped validator wrappers + metadataVersion (blueprint hashes verified unchanged)"
```

  If reverted, skip this commit and note the deferral in the report.

---

## Completion notes

- The **only** redeploy-risk item is Task 7 (INLINEABLE), gated by the blueprint-hash diff — if it changed hashes it is reverted, so this stream forces no redeploy.
- F-36 changes some HTTP statuses (400 → 500/503 for infra errors) — an intentional API-contract improvement.
- F-35c (projection dead-letter table) is explicitly deferred (schema change, out of scope) — left as a TODO comment.
- Grab-bag F-34/F-41/F-43/F-46 are omitted from tasks as low-value/higher-churn; F-46 (name the 1200 constant) may be folded into Task 3 if trivial.
