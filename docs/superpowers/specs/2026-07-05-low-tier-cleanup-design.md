# Low-tier Cleanup & Correctness — Design Spec

**Date:** 2026-07-05
**Sub-project:** G (from `FableReview-0407.md`, action-plan row 11 + the Low tier F-34…F-46)
**Status:** design approved; item-verified against current code (see Verification record); pending plan

## Goal

Clear the Low-tier backlog: dead code, debug leftovers, cabal/script hygiene,
behavior-preserving dedup, small correctness fixes, and on-chain cosmetics. The
review is from 2026-07-04 and streams B/C/D have since moved much of it — every
item below was re-verified against the **current** code (locations here are
current, not the review's).

## Principle

Mechanical cleanup and small correctness fixes, grouped by risk. Zero-risk
mechanical changes are separated from behavior changes (which get tests) and from
on-chain changes (which are blueprint-hash-checked — a hash change means the item
is reverted and deferred to a redeploy, stream E).

## Clusters (execution order)

### Cluster 1 — Dead code (zero-risk deletion)

All verified unused repo-wide (incl. tests):
- `getAllProfilesCount` — `Lookups.hs:146-147`
- `pPOSIXTimeFromSlotInteger` — `Utils.hs:55-56` (TxBuilding/Utils.hs)
- `oracleParamsFromGYOutDatum` — `Utils.hs:162-164` (TxBuilding/Utils.hs)
- `getRefScriptAtTxOutRef` — `Skeletons.hs:21-22` (read full body before deleting)
- `getMembershipHistoriesAsHistory` — `Query/Projected.hs:756-757`
- `getMembershipIntervalsAsInterval` — `Query/Projected.hs:843-844`
- `TxBuildingException` constructors `PromotionNotFound`, `MembershipHistoryNotFound`
  — never thrown. **Coupling:** `displayException` is an exhaustive match — delete
  the constructor (Exceptions.hs:40,42) **and** its `displayException` clauses
  (83-84) **and** its `txBuildingExceptionToHttpStatus` clauses (136-137) together.
- `interaction-api/Types.hs` — whole module, unimported. **Coupling:** also remove
  the `Types` line from cabal `other-modules` (line ~300).
- `webapi-lib` `deriving-aeson` dependency — cabal (~line 252), unused in webapi-lib.
- `mcp-server-lib/MCPServer/Tools/Common.hs` `_unusedBL8` + its `BL8` import
  (lines 29, 170-172) — delete all four lines together.

Verify: `cabal build all` clean (build proves the exception-constructor deletion
compiled and nothing references the removed names).

### Cluster 2 — Debug leftovers & raw errors off-chain

- `print interaction` — `Transactions.hs:86` (dumps the full Interaction incl.
  addresses on every admin tx) → delete.
- Two `gyLogInfo'` datum dumps — `Operations.hs:438-439` (fire on every real
  `acceptPromotionTX`, INFO level, also have a label-concat cosmetic bug) →
  remove (or downgrade to `gyLogDebug'`).
- **F-38 raw `error`:** the one that matters is `decodeGYScriptHash` —
  `chainsync-lib/KupoAtlas.hs:41-42`: it uses partial `fromString`, called at
  `:100` inside `kupoMatchToAtlasMatch`'s `Either String AtlasMatch` builder, so a
  malformed Kupo `script_hash` **crashes live chain-sync** (unlike its `Either`
  siblings). Change to `Text -> Either String GYScriptHash` and thread through the
  existing `<$>` (composes trivially inside the `Either` do-block). The other F-38
  sites (`TxBuilding/Utils.hs:86,93` readMnemonicFile; `Utils.hs:57`
  decodeConfigEnvOrFile) are CLI/startup-only — lower priority; convert to `die`
  for consistency (small).

### Cluster 3 — cabal + scripts hygiene (F-44, F-45)

- **`-Wunused-packages`:** add to `common-options` (cabal:~27-30). Build stays green
  (no `-Werror`). Then remove the genuinely-unused deps the flag surfaces, **one
  component at a time with a rebuild+test after each**: `webapi-lib` → drop
  `servant` (and `deriving-aeson`, cluster 1); `mcp-server-lib` → `onchain-lib`,
  `mtl`, `servant`, `time`; `interaction-api` → `onchain-lib`, `esqueleto`,
  `monad-logger`, `multiset`, `persistent`, `persistent-postgresql`; `query-api` →
  `deriving-aeson`, `http-types`; `chainsync-service` → `onchain-lib`, `aeson`,
  `mtl`, `plutus-ledger-api`, `plutus-tx`, `wai`; `admin` → `plutus-tx`;
  `test:mcp-integration` → `aeson`. **Keep** `onchain-lib`'s `plutus-tx-plugin`
  (used via `-fplugin`, false positive). Each removal is compile-verified.
- **Bounds dedup (F-45a):** hoist the 3 bounded deps (`bytestring`/`http-types`/
  `lens`, duplicated at cabal:311,317-318 and 368,373-374) — but note `http-types`
  is unused in query-api (remove it there per above). Consolidate the rest.
- **KupoClient.hs pragmas (F-45e):** delete the 5 redundant `LANGUAGE` pragmas
  (lines 1-5), keep `InstanceSigs` (line 6); also the lone `OverloadedStrings` in
  `KupoAtlas.hs:1`.
- **extra-lib-dirs (F-45d):** remove `extra-lib-dirs: /usr/lib /usr/local/lib`
  (cabal:293,345) — verified the Nix build succeeds without them.
- **default-language (F-45c):** unify to `GHC2021` in the 4 core libs
  (cabal:61,130,163,233) — build+test after (GHC2021 is a superset; confirm no
  regression). *Lower priority; do last, revert if any build issue.*
- **Scripts (F-44):** add `set -u` to `populate_testnet.sh` (36-37),
  `populate_recent_activity.sh` (46-47), `test_black_promotes_white_to_blue.sh`
  (6-7); replace the hardcoded fee address (populate_testnet.sh:274,
  test_black:353) with `${FEE_ADDRESS:-<current default>}`.

### Cluster 4 — Dedup (behavior-preserving, with two flagged nuances)

- **`toIntervalInfo` (Projected.hs:802-814, 877-889):** the two copies are
  byte-identical → extract one top-level helper, reuse twice. **Zero risk.** (Skip
  unifying with `Lookups.hs:398-408` `intervalToInformation` — different Maybe-org
  signature, not worth it.)
- **`Filters.*FromParams` Maybe vestigial (Filters.hs:86-155):** the 5 non-activity
  functions always return `Just`. Drop the `Maybe` from their return types and
  update call sites (RestAPI). Behavior-preserving (compile-enforced).
- **`get*DatumAndValue` dedup (Lookups.hs:75-80/82-87/246-254/448-453) — fixes
  F-26:** profile/rank/achievement share a `getUTxOWithNFTOrThrow` + parse shape,
  but rank/achievement currently throw the SAME *not-found* constructor on parse
  failure (the F-26 bug). A shared `getStateOrThrow` helper takes **distinct**
  not-found and parse-fail exceptions, which **fixes F-26** (parse failures →
  `DatumParseError`). This is a deliberate behavior change (desirable per review).
  Leave `getMembershipDatumAndValue` (different shape: extra inline-datum branch)
  as-is.
- **`loadPractitionerOrOrg` consistency (Aggregates.hs:235-257) — flagged:** stream
  D added a rankless-placeholder fallback to `resolveProfileForPromotionSide`
  (:106-145) but **not** to `loadPractitionerOrOrg`, so the two diverged and the
  latter still 404s for a rankless awarder. Rather than a blind dedup, **apply the
  same placeholder fallback to `loadPractitionerOrOrg`** so F-24's "never 404 a
  rankless profile" is consistent across both call sites (awarder maps in
  practitioner/organization detail). This is a small, intentional behavior
  improvement (consistency with F-24), not a silent refactor.
- **`checkPromotion`/`checkPromotionWithOptionalMaster` (Rules/Promotion.hs:102-149/
  156-185):** extract the byte-identical `rungViolation`/`timeInGradeViolation`
  into shared helpers; leave each function's unique parts untouched. Pinned by
  `prop_checkPromotionMirrorsValidate` (property test on `checkPromotion` only) —
  re-run `cabal test` to confirm it still passes (the test checks `null`, so
  ordering isn't pinned, but preserve it anyway).

### Cluster 5 — Correctness (behavior changes, tested where pure)

- **F-37 (pure, TESTED):** `promotionStateFromBelts` (Core/Types.hs:156-160) — the
  `otherwise` branch maps `current == proposed` to `PromotionPending`, but a
  promotion to a currently-held belt can't be accepted on-chain → change the guard
  `current > proposed` to `current >= proposed` so `current == proposed` →
  `PromotionSuperseded`. **Add a pure unit test:**
  `promotionStateFromBelts (Just b) b == PromotionSuperseded` for all belts.
- **F-36:** `txBuildingExceptionToHttpStatus` (Exceptions.hs:155 `_ = 400`) — add
  `ScriptNotFound = 503`, `OracleDatumInvalid = 500`, `DatumParseError = 500`
  (infra/server errors currently masquerading as 400). Pin with a small mapping
  test if cheap.
- **F-35 (narrow the swallow):**
  - `Operations.hs:805-806` (`updateEndDateTX` `catchError … (const False)`): catch
    only `ProfileNotFound`, rethrow other errors (don't mask network/`MultipleUtxosFound`).
  - `Live.hs:275` (`assignPromotionState` `try @SomeException → Nothing`): narrow to
    the expected not-found case; don't turn a backend failure into `PromotionPending`.
  - `Storage.hs:290,296` (projection errors only `putStrLn`): upgrade to a clear
    error log; **do NOT** add a dead-letter table (schema change, out of scope) —
    note it as a follow-up.
- **F-40 (narrowed):** only `getPromotionsCount` in `Projected.hs:621-622` remains
  `length <$> getPromotions` (the other projected counts already use SQL
  `COUNT(*)`; live counts are intentional — no SQL backend). Convert it to a SQL
  `COUNT(*)` matching the promotion filter, **only if** the filter is fully
  SQL-expressible; if promotion-state filtering needs the loaded rows, leave it and
  document why. Implementer verifies.

### Cluster 6 — On-chain (SAFE items + INLINEABLE with hash gate)

**Safe (name/comment — erased in Plutus Core, no hash change):**
- Typo `otherMetdataEncoded` → `otherMetadataEncoded` at all 4 sites (CIP68.hs:108,116;
  MintingPolicy.hs:389,408) — parameter binder name only.
- `AchievementsValidator.hs`: fix the copy-pasted Haddock (:12 "Ranks validator…"),
  add the missing export list (:13), fix the stale "Ranks Redeemer" comment (:29).
- Dangling `onchain-security-audit.md` refs (ProfilesValidator.hs:57,178;
  Core.hs:290 — doc deleted in dc74b1f) → rewrite the comments to state the
  rationale inline (no filename).

**INLINEABLE — hash-gated (do LAST, in its own task):**
- Add `{-# INLINEABLE #-}` to `profilesUntyped` (ProfilesValidator.hs:197),
  `ranksUntyped` (RanksValidator.hs:118), `membershipsUntyped`
  (MembershipsValidator.hs:294), `achievementsUntyped` (AchievementsValidator.hs:95),
  `metadataVersion` (CIP68.hs:50).
- **These are NOT provably hash-neutral** (INLINEABLE can affect the GHC Core the
  PlutusTx plugin compiles). After adding: run `admin write-blueprint` and **diff
  the validator hashes against `config/config_bjj_validators.json`**. If **any**
  hash changed, **revert those pragmas** and record them as a redeploy item for
  stream E (we cannot merge a hash change without a coordinated redeploy). If
  hashes are unchanged, keep them.

### Grab-bag (opportunistic, low-value — include if cheap)

- F-34 (Skeletons.hs monad-constraint downgrades), F-41 (cache minting policy in
  context), F-43 (admin `.prv` lazy load for read-only), F-46 (name the magic 1200
  "way behind" constant). These are optional; include the trivial ones (F-46
  constant name), defer the rest if they add risk/churn.

## Deploy / operational note

- The **on-chain INLINEABLE task is the only redeploy risk.** If the blueprint diff
  shows changed hashes, those pragmas are reverted (not merged) — no redeploy is
  forced by this stream. All other changes are off-chain / cosmetic / build-config.
- F-36 changes some HTTP status codes (400 → 500/503 for infra errors) — an
  observable API contract improvement; note in the changelog.

## Testing strategy

- **Zero-risk clusters (1, 2, 3, 6-safe):** `cabal build all` + `cabal test` green;
  `-Wunused` clean; blueprint hashes unchanged for on-chain-safe items.
- **Dedup (4):** existing tests + `prop_checkPromotionMirrorsValidate` pass identically.
- **Correctness (5):** F-37 gets a pure unit test; F-36 a mapping test if cheap;
  F-35 verified by build + reasoning (narrowed catches).
- **INLINEABLE (6):** blueprint hash diff is the test (unchanged → keep, changed → revert).

## Verification record

Every item was verified against current code by a 6-cluster verification workflow
(6 sonnet agents, 255 tool calls). Key findings folded in above:

- **Line numbers were stale** (streams B/C/D shifted files); all locations here are
  current. All dead-code items still unused.
- **F-40 mostly already fixed** by the C/D query-api rewrite (only projected
  `getPromotionsCount` remains; live counts are intentional) — scope narrowed.
- **`get*DatumAndValue` dedup is entangled with F-26** — the dedup fixes F-26 (a
  behavior change), which is included deliberately.
- **`loadPractitionerOrOrg` diverged from `resolveProfileForPromotionSide`** (stream
  D's F-24 placeholder was applied to only one) — resolved by extending the
  placeholder to both for consistency, not a blind dedup.
- **INLINEABLE is not provably hash-neutral** — gated behind a blueprint-hash diff,
  revert-if-changed. The typo/Haddock/comment on-chain items ARE hash-neutral
  (names/comments erased).
- **`-Wunused-packages` build-verified** to warn-not-fail; the exact per-component
  unused-dep list is captured in Cluster 3.
- `checkPromotion` property test checks `null` only (order not pinned), so the
  shared-helper extraction is safe.
