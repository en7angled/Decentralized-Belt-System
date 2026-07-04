# Offchain Correctness — Design Spec

**Date:** 2026-07-05
**Sub-project:** D (from `FableReview-0407.md`, action-plan row 8)
**Status:** design approved; adversarially verified (F-24 fix revised — see Verification record); pending plan

## Goal

Fix three offchain read-path correctness bugs: a UTF-8 decoder that corrupts
non-ASCII text and can crash chain-sync (F-09, an active data-corruption bug),
a live-backend pagination that applies `LIMIT` before its post-filters and
returns wrong pages (F-17), and a partial `Map.!` that 500s the promotions
dashboard when a referenced profile is unresolved (F-24). No behavior change
beyond correctness.

## Principle

Correctness on the read path. The on-chain data is already correct; these are
all bugs in how it is decoded, filtered, or assembled on the way out.

## Scope

**In scope** (3 findings, narrow — action-plan row 8):

| Finding | One-line |
| ------- | -------- |
| F-09 | `fromBuiltinByteStringUtf8` corrupts non-ASCII (`show`/strip hack) and throws on invalid UTF-8 → lenient UTF-8 decode |
| F-17 | live `getProfiles` applies limit/offset before the post-filters → apply all filters, then order, then limit |
| F-24 | partial `Map.!` in `promotionsToInformationBatch` → `Map.lookup` + skip unresolved |

**Prep (done, commit 9f85012):** committed the pre-existing `Lookups.hs` WIP
(hoist `getUTxOWithNFT` above its wrapper) with the review §6 cleanups (Haddock,
trailing whitespace, double blank line). Semantically neutral.

**Out of scope** (deferred): F-23 (membership-history duplicate → raw
`traceError`; needs a new exception constructor), F-25 (direct on-chain call
with inline pre-validation — structural, not an active bug), F-26 (datum parse
errors reported as "not found"), and the Low offchain items (F-35/36/37/40/41).
These form a separate "offchain error-mapping" cluster.

## F-09 — UTF-8 decode

**File:** `src/lib/offchain-lib/TxBuilding/Conversions.hs:42-47`.

Current: `fromBuiltinByteStringUtf8 bs = let shown = show (decodeUtf8 bs) in T.pack $ case shown of ('"':rest) -> init rest; other -> other`. `decodeUtf8` (PlutusTx) yields a `BuiltinString`; `show` renders it as a quoted, backslash-escaped Haskell literal, so `"José"` becomes the literal `Jos\233` (corruption), and `decodeUtf8` throws impurely on invalid UTF-8 (attacker-controllable via redeemer metadata) — which can kill the chain-sync loop.

**The encode side is correct** (`textToBuiltinByteString = stringToBuiltinByteStringUtf8 . T.unpack` writes proper UTF-8 bytes), so on-chain data is fine. This is purely a decode/display bug.

**Fix:**
```haskell
fromBuiltinByteStringUtf8 :: BuiltinByteString -> T.Text
fromBuiltinByteStringUtf8 = TE.decodeUtf8With TEE.lenientDecode . fromBuiltin
```
- Remove the `import PlutusTx.Builtins (decodeUtf8)` (if unused elsewhere — check the two usages at :179-180 are the fixed function, not the PlutusTx `decodeUtf8`); add `import qualified Data.Text.Encoding as TE`, `import qualified Data.Text.Encoding.Error as TEE`, and `fromBuiltin` (from `PlutusTx.Builtins`).
- `lenientDecode` replaces invalid bytes with U+FFFD instead of throwing — chain-sync survives malformed metadata.

**Existing corrupt projections:** the ingestion path stored the corrupted text,
so existing projection rows are wrong. Bump `currentSchemaVersion` in
`src/lib/offchain-lib/Storage.hs` (2 → 3) so chain-sync's schema-version-gated
startup wipes the chain-sync tables and re-syncs from Kupo, re-projecting names/
descriptions/URIs correctly. Operator action: redeploy chain-sync → automatic
full re-sync (same wipe+resync mechanism as stream B).

**Test (pure, strong — Conversions is a library):** new `UnitTests/Conversions.hs`:
- round-trip: `fromBuiltinByteStringUtf8 (textToBuiltinByteString "José") == "José"`; also an emoji / multi-byte string; plain ASCII unchanged.
- invalid UTF-8 bytes → no exception (returns a Text with U+FFFD), evaluated to WHNF/forced to prove no impure throw.

## F-17 — limit-after-filter in live `getProfiles`

**File:** `src/exe/query-api/Query/Live.hs:97-183`.

Current: `base = applyFilterOrderLimit maybeLimitOffset filterPass1 maybeOrder …`
(line 113) bakes the `LIMIT` into `base` **before** the four post-filters run
(text-with-org-affiliation ~116-135, membership-org ~137-147, active-membership
~149-166, belt ~168-183). So `GET /profiles?belt=Black&limit=10&liveprojection`
slices the first 10 unfiltered profiles, then belt-filters those 10 → arbitrarily
empty. The projected backend filters in SQL before `LIMIT`; the two diverge.

**Fix:** apply all filters (pass-1 + all four post-filters) and the ordering to
the **unbounded** set, then `applyLimits maybeLimitOffset` at the very end.
Concretely: compute the base/post-filter chain with `Nothing` as the limit (so
nothing is truncated mid-pipeline), keep the ordering, and add a final
`applyLimits maybeLimitOffset` to the fully-filtered, ordered list before
returning. Preserve the existing filter/order semantics exactly — only move where
the limit is applied.

**Test:** live path is IO-heavy (fetches profiles/memberships/ranks via provider),
not cleanly unit-testable in the suite. Verify by build + reasoning, and state
the projected/live parity intent. If the filter→order→limit tail factors cleanly
into a pure function over the already-fetched lists, extract and unit-test that
core; otherwise document the deferral (consistent with stream B/C IO-path
handling).

## F-24 — partial `Map.!` in `promotionsToInformationBatch`

**File:** `src/exe/query-api/Query/Aggregates.hs:185-186`.

Current: the list comprehension binds `let achieved = profileMap M.! promotionAchievedByProfileId p` and `let awarded = profileMap M.! …`. `profileMap` comes from `resolveProfilesBatch`, whose projected branch is `M.union practMap orgShims`; `getPractitionerProfilesBatch` **omits** profiles with no rank rows (`Projected.hs:380`, `buildInfo` returns `Nothing` when `domainRanks == []`). So a promotion referencing a profile whose rank projection hasn't arrived (or was lost) → key absent → `Map.!` throws `KeyNotFound` → uncaught 500 on the dashboard.

**Fix — make the resolver total, do NOT drop (revised after adversarial
verification).** A drop-based skip is **wrong**: `promotionsToInformationBatch`
is consumed positionally by `getPractitionerDetail` (`Aggregates.hs:271-273`),
which does `splitAt (length promotionsGiven) allInfos` on the combined
given++received list — a length-changing skip misaligns the given/received
boundary (blocker), and it also makes `getPromotionsPage`'s independent `total`
(a plain `COUNT`) diverge from `length items` (silent pagination under-report,
major). Both are confirmed against the code.

Instead, make **`resolveProfilesBatch` total** so `profileMap` contains an entry
for every requested id and `promotionsToInformationBatch` stays a
**length- and order-preserving** total map (no `splitAt`/count breakage):

- The current projected branch is `M.union practMap orgShims`, where
  `getPractitionerProfilesBatch` (`Projected.hs:376-393`, `buildInfo` returns
  `Nothing` for empty `domainRanks`) **omits** practitioners with no rank
  projection — the common case being a pending *first* promotion (recipient not
  yet ranked). Those ids are absent from both maps → `M.!` crash.
- Fix: after the union, for any still-missing requested id, add a **placeholder**
  `PractitionerProfileInformation` built from the profile's real
  name/description/image (fetched from `ProfileProjection`) plus a placeholder
  rank — exactly the existing precedent `organizationToShimPractitioner`
  (`Aggregates.hs:53-69`, already used for organization edges in promotion
  responses with a `White` placeholder rank and the comment "use a placeholder
  rank … so the object decodes on the frontend"). For a first-promotion
  recipient the placeholder is usually accurate (they were `White`).
- Keep `promotionsToInformationBatch` length-preserving; `M.!` becomes safe (map
  total) — optionally use `M.findWithDefault <minimal-unknown-placeholder>` so a
  truly non-existent id (invalid data, should not occur for a real on-chain
  promotion) still never drops a row. The `idTimeMap M.!` at `:148`/`:162` stay
  (safe by construction).

**Test:** the assembly is `QueryAppMonad` (exe-resident, not importable). If the
"union-then-fill-missing-with-placeholder" step factors into a pure function
(`Map ProfileRefAC PractitionerProfileInformation -> [(ProfileRefAC, profileRow)]
-> Map …` producing a total map), extract and unit-test that it is total and
preserves existing entries; otherwise verify by build + reasoning (the invariant
is "output map ⊇ requested id set", trivially checkable).

## Deploy / operational note

- **F-09 schema bump forces a full chain-sync re-sync** on next chainsync-service
  start (wipe + re-ingest from Kupo). Expected and intended — it is how existing
  corrupted projection rows get corrected. Operators should expect a resync
  window after deploying this change, same as any schema-version bump.
- query-api / interaction-api need no migration; the F-09 fix corrects live reads
  immediately and projected reads after the resync.

## Testing strategy

Mirror stream B/C: TDD the pure cores, build + reasoning for IO/exe paths.

- **F-09:** strong pure tests (round-trip + invalid-bytes no-throw) in a new
  `UnitTests/Conversions.hs`, wired into `UnitTests.hs` and the cabal test-suite.
- **F-17:** build + reasoning + projected/live parity note; extract a pure
  filter→order→limit core if clean.
- **F-24:** build + reasoning; extract a pure skip-assembly core if clean.

## Verification record

Adversarial design-verification workflow (5 dimensions × independent skeptic).
Two review dimensions (`f09-decode-correctness`, `f17-filter-limit`) failed on a
schema retry cap and produced no output — both were **verified manually instead**
(findings below). Confirmed findings, folded in:

- **Blocker — F-24 drop-based skip breaks positional `splitAt`.**
  `getPractitionerDetail` (`Aggregates.hs:271-273`) recovers the given/received
  split by `splitAt (length promotionsGiven)`; a length-changing skip misaligns
  the boundary → promotions mislabeled/lost. → F-24 fix rewritten to a
  **length-preserving total-map/placeholder** approach (no drop).
- **Major — F-24 skip makes `total` diverge from `items`.**
  `getPromotionsPage` sets `promotionsPageTotal` from an independent `COUNT`
  (`getPromotionsCount = length . getPromotions`); dropping rows in `items`
  silently under-reports. → same total-map fix keeps `total == length items`.

**Manually verified (workflow dimensions errored):**

- **F-09 sound.** `metadataFieldsToProfileData`/`profileDatumToProfileData`
  (`Conversions.hs:100`) and the achievement path (`Conversions.hs:179`) both go
  through the single `fromBuiltinByteStringUtf8` — the fix covers profile AND
  achievement text. `fromBuiltin` is not yet imported (add
  `PlutusTx.Builtins (fromBuiltin)`); `decodeUtf8`'s only use is the function
  being replaced (safe to drop). No other on-chain-text decode site exists
  (mcp-server-lib's `TE.decodeUtf8` is on JSON, not chain data).
- **F-17 scope correct.** `getProfiles` is the **only** live handler that applies
  the limit inside `base` and then runs further post-filters; `getAchievements`
  (:343), `getMembershipHistories` (:387), `getMembershipIntervals` (:418),
  `getPromotions` (:269), `getActivityFeed` (:627) all apply the limit last or
  have no post-filters — no sibling handler has the bug.

**Refuted / not actioned:** one `test-extractability` finding did not survive
verification.
