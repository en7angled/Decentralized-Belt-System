# Offchain Correctness Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Fix three offchain read-path correctness bugs — F-09 (UTF-8 decoder corrupts non-ASCII and can crash chain-sync), F-17 (live `getProfiles` limits before post-filtering), F-24 (partial `Map.!` / rankless-profile crash on the promotions dashboard, both backends).

**Architecture:** F-09 lives in `offchain-lib` (library) with strong pure tests. F-17 and F-24 live in the `query-api` executable (IO/DB paths) — verified by build + reasoning, with a pure core extracted and unit-tested where clean. No behavior change beyond correctness.

**Tech Stack:** Haskell/GHC 9.6, Cabal, PlutusTx builtins, `text` (`decodeUtf8With`/`lenientDecode`), esqueleto/persistent, Tasty/HUnit.

**Reference spec:** `docs/superpowers/specs/2026-07-05-offchain-correctness-design.md`.

## Global Constraints

- **Read-path correctness only.** On-chain data is already correct; fix decode/filter/assembly. No behavior change beyond fixing the bug.
- **F-24 must stay length- and order-preserving.** `promotionsToInformationBatch` is consumed positionally (`getPractitionerDetail` `splitAt`) and its output length is compared to an independent `COUNT` (`getPromotionsPage`). Do NOT drop rows — fill unresolved profiles with placeholders (precedent: `organizationToShimPractitioner`).
- **Do not change `getPractitionerProfilesBatch`'s contract** — it has five callers; the placeholder fill goes in `resolveProfilesBatch`/`resolveProfileForPromotionSide` only.
- **Style:** max 120 chars/line, 2-space indent, one blank line between top-level decls, `-- |` Haddock on new exports.
- **After each task:** `cabal build all` then `cabal test` must pass.

---

### Task 1: F-09 — lenient UTF-8 decode + schema-version bump + tests

**Files:**
- Modify: `src/lib/offchain-lib/TxBuilding/Conversions.hs`
- Modify: `src/lib/offchain-lib/Storage.hs`
- Create: `src/test/UnitTests/Conversions.hs`
- Modify: `src/test/UnitTests.hs`, `Decentralized-Belt-System.cabal`

**Interfaces:**
- Produces: `fromBuiltinByteStringUtf8 :: BuiltinByteString -> T.Text` (now lenient UTF-8 decode); `conversionsTests :: TestTree`.

- [ ] **Step 1: Write the failing test.** Create `src/test/UnitTests/Conversions.hs`:

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- | Pure tests for on-chain text conversions (F-09): the encode/decode round-trip
-- must preserve non-ASCII, and decoding invalid UTF-8 must not throw.
module UnitTests.Conversions (conversionsTests) where

import Control.Exception (evaluate, try, SomeException)
import qualified Data.Text as T
import PlutusTx.Builtins (toBuiltin)
import Test.Tasty
import Test.Tasty.HUnit
import TxBuilding.Conversions (fromBuiltinByteStringUtf8, textToBuiltinByteString)

roundTrip :: T.Text -> T.Text
roundTrip = fromBuiltinByteStringUtf8 . textToBuiltinByteString

conversionsTests :: TestTree
conversionsTests =
  testGroup
    "Conversions (F-09 UTF-8)"
    [ testCase "ASCII round-trips" $ roundTrip "Hello World" @?= "Hello World",
      testCase "accented text round-trips" $ roundTrip "José Gração" @?= "José Gração",
      testCase "emoji round-trips" $ roundTrip "belt \128081 test" @?= "belt \128081 test",
      testCase "empty round-trips" $ roundTrip "" @?= "",
      testCase "invalid UTF-8 bytes do not throw (lenient)" $ do
        -- 0xFF 0xFE are not valid UTF-8; decoding must yield a Text without raising.
        let bad = toBuiltin (Data.ByteString.pack [0x41, 0xFF, 0xFE, 0x42])
        r <- try (evaluate (T.length (fromBuiltinByteStringUtf8 bad)))
        case r of
          Right _ -> pure ()
          Left (e :: SomeException) -> assertFailure ("decode threw: " <> show e)
    ]
```

  Add `import qualified Data.ByteString` to the test imports (used for `pack`). Wire into `src/test/UnitTests.hs`: add `import UnitTests.Conversions (conversionsTests)` (alphabetically after `UnitTests.ChainSyncReplay`) and add `conversionsTests` to the `testGroup "BJJ Unit Tests" [...]` list. In `Decentralized-Belt-System.cabal` `test-suite test` `other-modules`, add `UnitTests.Conversions` (alphabetically after `UnitTests.ChainSyncReplay`). The test-suite already depends on `offchain-lib` and `bytestring`.

- [ ] **Step 2: Run the test to verify it fails.** Run: `cabal test 2>&1 | tail -30`. Expected: the accented/emoji round-trip cases FAIL (current `show`/strip corrupts non-ASCII), and/or the invalid-bytes case fails (current `decodeUtf8` throws).

- [ ] **Step 3: Implement the decoder.** In `src/lib/offchain-lib/TxBuilding/Conversions.hs`:
  - Change the import `import PlutusTx.Builtins (decodeUtf8)` (line 17) to `import PlutusTx.Builtins (fromBuiltin)`.
  - Add `import qualified Data.Text.Encoding as TE` and `import qualified Data.Text.Encoding.Error as TEE` (near the other imports).
  - Replace `fromBuiltinByteStringUtf8` (lines 42-47) with:

```haskell
-- | Decode an on-chain UTF-8 'BuiltinByteString' to 'Text', replacing any
-- invalid byte sequence with U+FFFD (never throws — keeps chain-sync alive on
-- malformed metadata).
fromBuiltinByteStringUtf8 :: BuiltinByteString -> T.Text
fromBuiltinByteStringUtf8 = TE.decodeUtf8With TEE.lenientDecode . fromBuiltin
```

  This also fixes the achievement `otherMetadata` path (`cip68DatumOtherMetadataPairs`, :178-180) and the profile path (`metadataFieldsToProfileData`, :34-36) — both route through this single function.

- [ ] **Step 4: Bump the schema version (re-project existing corrupt rows).** In `src/lib/offchain-lib/Storage.hs`, change `currentSchemaVersion = 2` (line 200) to `currentSchemaVersion = 3`. On next chainsync-service start this triggers the existing schema-version-gated `wipeChainSyncTablesRaw` + re-migrate + full re-sync from Kupo, re-projecting names/descriptions/URIs with the fixed decoder. (No other code change — the wipe/resync mechanism from stream B is reused.)

- [ ] **Step 5: Run the tests to verify they pass.** Run: `cabal build all 2>&1 | tail -5 && cabal test 2>&1 | tail -20`. Expected: build clean, `Conversions (F-09 UTF-8)` group passes, full suite green.

- [ ] **Step 6: Commit.**

```bash
git add src/lib/offchain-lib/TxBuilding/Conversions.hs src/lib/offchain-lib/Storage.hs src/test/UnitTests/Conversions.hs src/test/UnitTests.hs Decentralized-Belt-System.cabal
git commit -m "fix(conversions): lenient UTF-8 decode + schema bump for reproject (F-09)"
```

---

### Task 2: F-17 — apply limit after all filters in live `getProfiles`

**Files:**
- Modify: `src/exe/query-api/Query/Live.hs`

**Interfaces:**
- Consumes: `applyFilterOrderLimit`, `applyLimits` (`Query.Common`).

- [ ] **Step 1: Restructure so the limit is applied last.** In `src/exe/query-api/Query/Live.hs` `getProfiles` (lines 97-183):
  - Change the `base` binding (line 113) to pass **`Nothing`** as the limit, so `base` is filtered + ordered but NOT truncated:

```haskell
      base = applyFilterOrderLimit Nothing filterPass1 maybeOrder applyProfileFilter (applyOrdering regTsMap) allProfiles
```

  - The four post-filters (`afterQ` ~116-135, `afterMembershipOrg` ~137-147, `afterActiveOrg` ~149-166, belt ~168-183) run on the unbounded, ordered `base` as before (they are order-preserving `filter`s). Change ONLY the final `case … of` block (belt filter, ~168-183) so its result is wrapped in `applyLimits maybeLimitOffset` at the very end:

```haskell
  -- Post-filter: belt (current rank matches given belts), then apply the page limit LAST
  afterBelt <- case maybeProfileFilter >>= profileFilterBelt of
    Nothing -> return afterActiveOrg
    Just belts -> do
      allRanks <- liftIO $ runQuery providerCtx (getAllRanks nid)
      let beltSet = S.fromList belts
          currentBeltMap =
            M.fromListWith
              (\(b1, d1) (b2, d2) -> if d1 >= d2 then (b1, d1) else (b2, d2))
              [ (rankAchievedByProfileId r, (rankBelt r, rankAchievementDate r))
                | r <- allRanks
              ]
          hasBelt p = case M.lookup (profileId p) currentBeltMap of
            Just (b, _) -> b `S.member` beltSet
            Nothing -> False
      return $ Prelude.filter hasBelt afterActiveOrg
  return $ applyLimits maybeLimitOffset afterBelt
```

  (The only functional change is: `base` no longer carries the limit, and `applyLimits maybeLimitOffset` is applied once at the end to the fully-filtered, ordered list — matching the projected backend, which filters in SQL before `LIMIT`. All filter/order semantics are otherwise unchanged.)

- [ ] **Step 2: Build.** Run: `cabal build all 2>&1 | tail -10`. Expected: clean.

- [ ] **Step 3: Reasoning check.** Confirm: `applyFilterOrderLimit Nothing …` applies filter + order but no limit (`applyLimits Nothing xs = xs`); the post-filters preserve order; `applyLimits maybeLimitOffset` at the end bounds the final page. A request like `GET /profiles?belt=Black&limit=10&liveprojection` now belt-filters the whole set, then takes 10 — instead of taking 10 then belt-filtering. Note the projected/live parity intent in the commit.

- [ ] **Step 4: Run tests.** Run: `cabal test 2>&1 | tail -8`. Expected: green (no unit test added; F-17 is a live-IO reordering — build + reasoning per the plan's testing strategy).

- [ ] **Step 5: Commit.**

```bash
git add src/exe/query-api/Query/Live.hs
git commit -m "fix(query-live): apply pagination limit after all profile post-filters (F-17)"
```

---

### Task 3: F-24 — total-map profile resolver (both backends), no dropped rows

**Files:**
- Modify: `src/exe/query-api/Query/Projected.hs` (add `getProfileProjectionsBatch`)
- Modify: `src/exe/query-api/Query/Aggregates.hs` (placeholder builder; fill both branches of `resolveProfilesBatch` / `resolveProfileForPromotionSide`)

**Interfaces:**
- Consumes: `organizationToShimPractitioner` pattern (`Aggregates.hs:53-69`), `getProfileStateDatumAndValue`/`profileDatumToProfileData` for the live rankless fetch.
- Produces: `getProfileProjectionsBatch :: [ProfileRefAC] -> …(M.Map ProfileRefAC (Text, Text, Text))`; `placeholderPractitioner :: ProfileRefAC -> Text -> Text -> Text -> GYTime -> PractitionerProfileInformation`.

- [ ] **Step 1: Add a raw profile-projection batch getter.** In `src/exe/query-api/Query/Projected.hs`, add (near `getPractitionerProfilesBatch`):

```haskell
-- | Batch-load raw profile fields (name, description, imageURI) by id, for any
-- profile regardless of rank. Used to build placeholder practitioner info for
-- promotion edges whose profile has no rank projection yet. Skips missing ids.
getProfileProjectionsBatch ::
  (MonadIO m, MonadReader QueryAppContext m) =>
  [ProfileRefAC] ->
  m (M.Map ProfileRefAC (Text, Text, Text))
getProfileProjectionsBatch [] = return M.empty
getProfileProjectionsBatch pids = do
  pool <- asks pgPool
  let uniqPids = nub pids
  liftIO $
    runSqlPool
      ( do
          rows <- select $ do
            pp <- from $ table @ProfileProjection
            where_ (pp ^. ProfileProjectionProfileId `in_` valList uniqPids)
            pure pp
          return $
            M.fromList
              [ ( profileProjectionProfileId r,
                  ( profileProjectionProfileName r,
                    profileProjectionProfileDescription r,
                    profileProjectionProfileImageURI r
                  )
                )
              | Entity _ r <- rows
              ]
      )
      pool
```

  Export it if `Query.Projected` uses an explicit export list (match the existing style — `getPractitionerProfilesBatch` is exported the same way). `Text` is already imported in this module.

- [ ] **Step 2: Add a placeholder practitioner builder.** In `src/exe/query-api/Query/Aggregates.hs`, add next to `organizationToShimPractitioner`:

```haskell
-- | Placeholder @PractitionerProfileInformation@ for a practitioner whose profile
-- exists but has no rank projection yet (e.g. a pending first promotion). Uses the
-- profile's real name/description/image plus a placeholder 'White' rank at @t@,
-- mirroring 'organizationToShimPractitioner' so promotion edges stay renderable.
placeholderPractitioner :: ProfileRefAC -> Text -> Text -> Text -> GYTime -> PractitionerProfileInformation
placeholderPractitioner pid name desc img t =
  PractitionerProfileInformation
    { practitionerId = pid,
      practitionerName = name,
      practitionerDescription = desc,
      practitionerImageURI = img,
      practitionerCurrentRank =
        Rank
          { rankId = pid,
            rankBelt = White,
            rankAchievedByProfileId = pid,
            rankAwardedByProfileId = pid,
            rankAchievementDate = t
          },
      practitionerPreviousRanks = []
    }
```

  (`Text`, `Rank`, `White`, `GYTime`, `ProfileRefAC` are already in scope in this module — `organizationToShimPractitioner` uses all of them.)

- [ ] **Step 3: Make the projected branch of `resolveProfilesBatch` total.** In `resolveProfilesBatch` (`Aggregates.hs:137-166`), the projected branch currently returns `M.union practMap orgShims`. Extend it to fill any still-missing id with a placeholder built from its raw profile row:

```haskell
    ( do
        practMap <- P.getPractitionerProfilesBatch distinctIds
        orgMap <- P.getOrganizationProfilesBatch distinctIds
        let resolved =
              M.union
                practMap
                ( M.mapWithKey
                    (\pid org -> organizationToShimPractitioner org (idTimeMap M.! pid))
                    orgMap
                )
            missing = [pid | pid <- distinctIds, not (M.member pid resolved)]
        rawProfiles <- P.getProfileProjectionsBatch missing
        let placeholders =
              M.fromList
                [ (pid, placeholderPractitioner pid nm ds im (idTimeMap M.! pid))
                | (pid, (nm, ds, im)) <- M.toList rawProfiles
                ]
        return (M.union resolved placeholders)
    )
```

- [ ] **Step 4: Make the live branch total too.** The live branch calls `resolveProfileForPromotionSide` per id, which throws `ProfileNotFound` for a rankless practitioner (`getPractitionerInformation` throws `RankListEmpty`, then the org fallback fails). In `resolveProfileForPromotionSide` (`Aggregates.hs:82-107`), change the live branch's final `Left _` fallback (after the org attempt fails) to try the profile's raw datum and build a placeholder before giving up:

```haskell
      e <- liftIO $ try @SomeException $ runQuery ctx (getPractitionerInformation pid)
      case e of
        Right p -> return p
        Left _ -> do
          eOrg <- liftIO $ try @SomeException $ runQuery ctx (getOrganizationInformation pid)
          case eOrg of
            Right org -> return $ organizationToShimPractitioner org t
            Left _ -> do
              -- Rankless practitioner (e.g. pending first promotion): build a placeholder
              -- from the profile datum rather than 404ing the whole promotion.
              eData <- liftIO $ try @SomeException $ runQuery ctx (getProfileStateDatumAndValue pid)
              case eData of
                Right (datum, _val) ->
                  let pd = profileDatumToProfileData datum
                   in return $
                        placeholderPractitioner
                          pid
                          (profileDataName pd)
                          (profileDataDescription pd)
                          (profileDataImageURI pd)
                          t
                Left _ -> runWithQueryErrorHandling $ throwIO ProfileNotFound
```

  Add imports to `Aggregates.hs` as needed: `getProfileStateDatumAndValue` (from `TxBuilding.Lookups`), `profileDatumToProfileData` (from `TxBuilding.Conversions`), and `ProfileData (..)` accessors (`profileDataName`/`profileDataDescription`/`profileDataImageURI`, from `DomainTypes.Core.*`). Verify `getProfileStateDatumAndValue :: GYAssetClass -> m (CIP68Datum OnchainProfile, Value)` matches how `pid :: ProfileRefAC` is passed (it is the profile's ref asset class — same value the other live lookups take); if the type does not line up, STOP and report.

- [ ] **Step 5: Make the final assembly defensively total.** In `promotionsToInformationBatch` (`Aggregates.hs:169-187`), replace the two `profileMap M.! …` with `M.findWithDefault` against a minimal unknown placeholder, so a truly non-existent id (should not occur for a valid on-chain promotion) still never crashes or drops a row — keeping the output length equal to the input length:

```haskell
  profileMap <- resolveProfilesBatch idTimePairs
  let resolve pid t = M.findWithDefault (placeholderPractitioner pid "" "" "" t) pid profileMap
  return
    [ promotionInformationToResponse achieved awarded p
      | p <- ps,
        let achieved = resolve (promotionAchievedByProfileId p) (promotionAchievementDate p),
        let awarded = resolve (promotionAwardedByProfileId p) (promotionAchievementDate p)
    ]
```

  Leave the `idTimeMap M.!` at `:148`/`:162` unchanged (safe by construction: they iterate over keys of `idTimeMap`).

- [ ] **Step 6: Build.** Run: `cabal build all 2>&1 | tail -15`. Expected: clean. If any lookup/type in Step 4 doesn't line up, resolve it or report.

- [ ] **Step 7: Reasoning check.** Confirm: `promotionsToInformationBatch` now returns exactly `length ps` responses (no drops) — so `getPractitionerDetail`'s `splitAt (length promotionsGiven)` stays aligned and `getPromotionsPage`'s `total`/`items` stay consistent. A promotion whose recipient has a profile but no rank now renders with the profile's real name/image and a `White` placeholder rank (both backends), instead of a 500 (projected) or 404 (live).

- [ ] **Step 8: Run tests.** Run: `cabal test 2>&1 | tail -8`. Expected: green.

- [ ] **Step 9: Commit.**

```bash
git add src/exe/query-api/Query/Projected.hs src/exe/query-api/Query/Aggregates.hs
git commit -m "fix(query): total-map profile resolver — placeholder for rankless, no dropped rows (F-24)"
```

---

## Completion notes / deploy

- **F-09 forces a full chain-sync re-sync** on next chainsync-service start (schema-version bump → wipe + re-ingest from Kupo). Intended — this corrects existing corrupted projection rows. Operators expect a resync window after deploy.
- F-24 fix is length-preserving in both backends; the placeholder-`White` rank for rankless promotion edges is consistent with the pre-existing `organizationToShimPractitioner` behavior.
