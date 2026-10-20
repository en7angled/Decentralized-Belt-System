# Plan A — block: external parity against Koios (F1 check 6)

This block delivers F1 check 6 — "on ≥ 20 DReps and ≥ 10 gov actions the votes/delegations agree with Koios
preprod, 0 unexplained divergences" — as `admin reconcile-koios`, a pure comparison library in
`offchain-lib`, a pinned (never random) sample, a committed waiver file, a thin shell wrapper and a nightly
warn-only workflow. It implements synthesis §3 Phase 5 item 25 and the canonical names of §1.3
(`scripts/reconcile_koios.sh` over `admin reconcile-koios`, `GET /status`, `PALMARES_READY_MAX_LAG_SECONDS`,
GHCR + hosted arm64 runners, no self-hosted runner); **D-20** (the pseudo-DRep totals: R3 compares the
matched real-DRep set and R4 compares `target_kind IN (…)` totals against Koios's *unfiltered* sum, because
`drep_always_abstain` alone is 46 % of the epoch-319 total); **D-7** (300 s as the freshness bound);
**D-6/D-8** (CI shape); and T-DATA §8.2's R1–R10 numbering, with its R3/R4/R6/R7 corrections. Assumptions:
the preprod stack is live and caught up (synthesis §3 steps 11, 12, 15, 16, 18 done); the comparison runs
against **our own public API, never raw SQL**, so it covers the whole read path including CIP-129 encoding;
**Koios is a check, never a source** — it runs on cardano-db-sync too, so a divergence says "one of us is
wrong", never "we are wrong", and nothing in this block may fall back to it when our sync is behind.

---

### Task T-PARITY-1: Koios preprod wire decoders

**Files:**
- Create: `src/lib/offchain-lib/Palmares/Reconcile/Koios.hs`
- Create: `src/test/UnitTests/Reconcile.hs`
- Create: `test/fixtures/koios/{tip,drep_list,drep_info,drep_voting_power_history,proposal_list,proposal_votes,drep_votes,committee_info}.json`
  — **single-id, hand-recorded wire fixtures; not the replay snapshot pair, which lives under
  `test/fixtures/snapshot/` (T-PARITY-5) so the dumper can never overwrite these pinned counts.**
- Modify: `src/test/UnitTests.hs` (import + one entry in the `unitTests` list)
- Modify: `palmares.cabal` (`library offchain-lib` exposed-modules; `test-suite test` other-modules; `extra-source-files: test/fixtures/koios/*.json`)
- Test: `src/test/UnitTests/Reconcile.hs`

**Interfaces:**
- Consumes: `Palmares.DomainTypes.Transfer.QueryResponses` (`GovActionType`, `Vote`, `VoterRole`,
  `DRepStatus`) — synthesis §3 step 18.
- Produces: `KoiosTip`, `KoiosDRepListRow`, `KoiosDRepInfo`, `KoiosPowerRow`, `KoiosProposal`,
  `KoiosWithdrawal`, `KoiosVoteRow`, `KoiosDRepVoteRow`, `KoiosCommittee`, `KoiosCommitteeMember`,
  `koiosActionType`, `koiosDRepStatus`, `koiosVote`, `koiosVoterRole`; `reconcileTests :: TestTree`;
  the `test/fixtures/koios/` layout.

- [ ] **Step 1: Write the failing test**

Record the eight fixtures first (`K=https://preprod.koios.rest/api/v1`; these exact lines go in the test
module header, they are the only way a later contributor can refresh them):

```bash
K=https://preprod.koios.rest/api/v1
D=drep1y2qn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qmwj3xg
F=test/fixtures/koios
# $A = an action some DRep voted on twice, so the "latest-only" assertion has a re-vote to catch.
# T-METRIC §3.2 measured 69 such pairs on preprod, so this cannot come back empty (OPEN item 3).
A=$(curl -s "$K/vote_list?limit=1000" \
      | jq -r 'group_by(.voter_id + .proposal_id)[] | select(length > 1) | .[0].proposal_id' | head -1)
curl -s "$K/tip"                                              > $F/tip.json
curl -s "$K/drep_list?registered=eq.true&limit=1000"          > $F/drep_list.json
curl -s -H 'content-type: application/json' -d "{\"_drep_ids\":[\"$D\"]}" "$K/drep_info" > $F/drep_info.json
curl -s "$K/drep_voting_power_history?_epoch_no=319&limit=1000" > $F/drep_voting_power_history.json
curl -s "$K/proposal_list?limit=1000"                         > $F/proposal_list.json
curl -s "$K/committee_info"                                   > $F/committee_info.json
jq -n --argjson v "$(curl -s "$K/proposal_votes?_proposal_id=$A")" --arg k "$A" '{($k): $v}' > $F/proposal_votes.json
jq -n --argjson v "$(curl -s "$K/drep_votes?_drep_id=$D")"       --arg k "$D" '{($k): $v}' > $F/drep_votes.json

# the three numbers the notes disagree on, read off the recording before step 4 (OPEN items 2 and 5)
jq -r '.members[].status' $F/committee_info.json | sort -u
jq '[.[]|select(.expired_epoch!=null)]|length,
    [.[]|select(.ratified_epoch==null and .enacted_epoch==null
            and .expired_epoch==null and .dropped_epoch==null)]|length' $F/proposal_list.json
```

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- | Parity against Koios preprod (F1 check 6). Koios is a CHECK, never a source.
-- test/fixtures/koios/ holds the SINGLE-ID wire fixtures recorded by the curl block above;
-- refresh them with those commands. The multi-id replay snapshot pair is a different set,
-- written by `admin reconcile-koios --dump-snapshots test/fixtures/snapshot` (T-PARITY-5);
-- never point the dumper at this directory, it would flatten the pinned single-id counts.
module UnitTests.Reconcile (reconcileTests) where

import Data.Aeson (FromJSON, eitherDecodeFileStrict)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Palmares.DomainTypes.Transfer.QueryResponses
  (DRepStatus (..), GovActionType (..), Vote (..), VoterRole (..))
import Palmares.Reconcile.Koios
import Test.Tasty
import Test.Tasty.HUnit

load :: (FromJSON a) => FilePath -> IO a
load p = eitherDecodeFileStrict ("test/fixtures/" <> p) >>= either assertFailure pure

koiosWireTests :: TestTree
koiosWireTests =
  testGroup
    "Koios wire"
    [ testCase "tip decodes to the pinned preprod point" $ do
        t <- load "koios/tip.json"
        (koiosTipEpoch t, koiosTipAbsSlot t, koiosTipBlockNo t) @?= (320, 136754769, 5311567),
      testCase "drep_list?registered=eq.true has 294 rows, all CIP-129" $ do
        rs <- load "koios/drep_list.json"
        length rs @?= 294
        assertBool "all drep1…" (all (T.isPrefixOf "drep1" . koiosDrepListDrepId) rs),
      testCase "epoch-319 power: 195 rows, and the two pseudo-DReps carry 46% of the total" $ do
        rs <- load "koios/drep_voting_power_history.json"
        length rs @?= 195
        sum (map koiosPowerAmount rs) @?= 937803900128796
        let byId = Map.fromList [(koiosPowerDrepId r, koiosPowerAmount r) | r <- rs]
        Map.lookup "drep_always_abstain" byId @?= Just 431863790957959
        Map.lookup "drep_always_no_confidence" byId @?= Just 325390800383,
      testCase "proposal_list: 131 rows, lifecycle shape, 7-epoch lifetime" $ do
        ps <- load "koios/proposal_list.json"
        length ps @?= 131
        length (filter ((/= Nothing) . koiosProposalRatifiedEpoch) ps) @?= 11
        length (filter ((/= Nothing) . koiosProposalEnactedEpoch) ps) @?= 11
        length [p | p <- ps, koiosProposalDroppedEpoch p /= Nothing,
                    koiosProposalExpiredEpoch p == Nothing, koiosProposalEnactedEpoch p == Nothing] @?= 6
        length (filter ((/= Nothing) . koiosProposalExpiredEpoch) ps) @?= 102
        length [p | p <- ps, all (== Nothing) [koiosProposalRatifiedEpoch p, koiosProposalEnactedEpoch p,
                                               koiosProposalExpiredEpoch p, koiosProposalDroppedEpoch p]] @?= 12
        assertBool "govActionLifetime = 7 on every preprod row"
          (all (\p -> koiosProposalExpiration p == Just (koiosProposalProposedEpoch p + 7)) ps)
        assertBool "all gov_action1…" (all (T.isPrefixOf "gov_action1" . koiosProposalProposalId) ps),
      testCase "drep_info for the fixture DRep" $ do
        is <- load "koios/drep_info.json"
        case is of
          [i] -> do
            koiosDrepInfoRegistered i @?= True
            koiosDrepInfoActive i @?= True
            koiosDrepInfoHasScript i @?= False
            koiosDrepInfoExpiresEpochNo i @?= Just 333
            koiosDrepInfoAmount i @?= Just 3923709956713
            koiosDrepInfoLiveDelegatorCount i @?= Just 13
          _ -> assertFailure "expected exactly one drep_info row",
      testCase "drep_votes is FULL history: 16 rows, 0 re-votes, yes 7 / no 8 / abstain 1" $ do
        m <- load "koios/drep_votes.json"
        let vs = concat (Map.elems (m :: Map.Map Text [KoiosDRepVoteRow]))
        length vs @?= 16
        length (nub (map koiosDrepVoteProposalId vs)) @?= 16
        map (\v -> length (filter ((== v) . koiosDrepVoteVote) vs)) ["Yes", "No", "Abstain"] @?= [7, 8, 1],
      testCase "proposal_votes is LATEST-only: one row per voter" $ do
        m <- load "koios/proposal_votes.json"
        let vs = concat (Map.elems (m :: Map.Map Text [KoiosVoteRow]))
        length (nub (map koiosVoteVoterId vs)) @?= length vs,
      testCase "committee_info: preprod is 2-of-3, all script creds, expiring 372" $ do
        c <- load "koios/committee_info.json"
        (koiosCommitteeQuorumNumerator c, koiosCommitteeQuorumDenominator c) @?= (2, 3)
        length (koiosCommitteeMembers c) @?= 3
        assertBool "all cold creds are scripts"
          (all koiosMemberCcColdHasScript (koiosCommitteeMembers c))
        assertBool "all expire at 372"
          (all ((== Just 372) . koiosMemberExpirationEpoch) (koiosCommitteeMembers c)),
      testCase "proposal_type is db-sync's enum: NewCommittee, never UpdateCommittee" $ do
        koiosActionType "NewCommittee" @?= Just NewCommittee
        koiosActionType "TreasuryWithdrawals" @?= Just TreasuryWithdrawals
        koiosActionType "UpdateCommittee" @?= Nothing,
      testCase "vote and voterrole are db-sync's enum spellings, not our wire values" $ do
        map koiosVote ["Yes", "No", "Abstain", "yes"] @?= [Just Yes, Just No, Just Abstain, Nothing]
        map koiosVoterRole ["DRep", "SPO", "ConstitutionalCommittee", "drep"]
          @?= [Just DRep, Just SPO, Just ConstitutionalCommittee, Nothing],
      testCase "(registered, active) maps onto the three registration states DRepStatus has" $ do
        koiosDRepStatus True True @?= Just DRepActive
        koiosDRepStatus True False @?= Just DRepInactive
        koiosDRepStatus False False @?= Just DRepRetired
        koiosDRepStatus False True @?= Nothing
    ]

reconcileTests :: TestTree
reconcileTests = testGroup "Reconcile" [koiosWireTests]
```

(`DRepReserved` is unreachable from `(registered, active)` — the two pseudo-DReps are identified by
their literal `drep_always_*` id, not by this pair, so `koiosDRepStatus` is deliberately 3-valued.)

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-show-details=direct --test-options='-p "/Koios wire/"'`
Expected: FAIL with `error: [GHC-87110] Could not find module ‘Palmares.Reconcile.Koios’`

- [ ] **Step 3: Implement the ten wire records and four mappings in `src/lib/offchain-lib/Palmares/Reconcile/Koios.hs`**

`FromJSON` only (we never send Koios anything but `_drep_ids`), via the house mechanism — the field prefix is
the type's own word, so `CamelToSnake` lands exactly on Koios's names:

```haskell
data KoiosDRepInfo = KoiosDRepInfo
  { koiosDrepInfoDrepId :: Text, koiosDrepInfoRegistered :: Bool, koiosDrepInfoActive :: Bool
  , koiosDrepInfoHasScript :: Bool, koiosDrepInfoExpiresEpochNo :: Maybe Int
  , koiosDrepInfoAmount :: Maybe Integer, koiosDrepInfoLiveDelegatorCount :: Maybe Int }
  deriving stock (Eq, Show, Generic)
  deriving (FromJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "koiosDrepInfo", CamelToSnake]] KoiosDRepInfo
```

Prefixes, one rule: **the type's own name, lower-camel, `koios` included** — `koiosTip`,
`koiosDrepList`, `koiosDrepInfo`, `koiosPower`, `koiosProposal`, `koiosWithdrawal`, `koiosVote`,
`koiosDrepVote`, `koiosCommittee`, `koiosMember`. The `koios` is load-bearing, not decoration:
`palmares.cabal` sets no `DuplicateRecordFields`, and the shorter prefixes collide inside `offchain-lib`
with `VoteRecord.voteVoterId`, `TreasuryWithdrawalRecord.withdrawalAmount` and
`CommitteeResponse.committeeQuorumNumerator` / `.committeeMembers`, all of which `Palmares.Reconcile`
imports in T-PARITY-3. Every lifecycle epoch and every `meta_*` is `Maybe`;
`proposal_list[].withdrawal` is `[KoiosWithdrawal]` defaulting to `[]`.
`koiosActionType`/`koiosVote`/`koiosVoterRole`/`koiosDRepStatus` are total `Maybe`-returning lookups —
never a partial `read`.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-show-details=direct --test-options='-p "/Koios wire/"'`
Expected: PASS (11 cases)

- [ ] **Step 5: Commit**

```bash
git add src/lib/offchain-lib/Palmares/Reconcile/Koios.hs src/test/UnitTests/Reconcile.hs \
        src/test/UnitTests.hs test/fixtures/koios palmares.cabal
git commit -m "feat(reconcile): Koios preprod wire decoders + recorded fixtures"
```

---

### Task T-PARITY-2: comparison primitive, waiver file, freshness guard, report

**Files:**
- Create: `src/lib/offchain-lib/Palmares/Reconcile.hs`
- Create: `config/reconcile-exceptions.yaml`
- Modify: `src/test/UnitTests/Reconcile.hs` (add `primitiveTests` to the group)
- Modify: `palmares.cabal` (`library offchain-lib`: exposed-modules + `build-depends: yaml, filepath`)
- Test: `src/test/UnitTests/Reconcile.hs`

**Interfaces:**
- Consumes: nothing outside `base`/`aeson`/`containers`/`yaml`.
- Produces: `RowId (R1…R10)`, `Divergence`, `RowStatus`, `isSkipped`, `RowResult`, `ExceptionEntry`,
  `Freshness`, `ReportStatus`, `Report`, `diffKeyed`, `applyExceptions`, `staleExceptions`, `mkRow`,
  `skippedRow`, `checkFreshness`, `mkReport`, `reportExitCode`, `loadExceptions`;
  `config/reconcile-exceptions.yaml` and its schema.

- [ ] **Step 1: Write the failing test**

```haskell
primitiveTests :: TestTree
primitiveTests =
  testGroup
    "Reconcile primitives"
    [ testCase "diffKeyed reports one-sided keys as a membership divergence" $ do
        let k = Map.fromList [("a", 1 :: Int), ("b", 2)]
            p = Map.fromList [("b", 2 :: Int), ("c", 3)]
            ds = diffKeyed id (\x y -> [("amount", tshow x, tshow y) | x /= y]) k p
        map (\d -> (divergenceKey d, divergenceField d, divergenceKoios d, divergencePalmares d)) ds
          @?= [ ("a", "membership", "present", "absent"),
                ("c", "membership", "absent", "present") ],
      testCase "diffKeyed reports only the fields the comparator returns" $ do
        let k = Map.fromList [("a", 10 :: Int)]
            p = Map.fromList [("a", 11 :: Int)]
            ds = diffKeyed id (\x y -> [("amount", tshow x, tshow y) | x /= y]) k p
        map (\d -> (divergenceField d, divergenceKoios d, divergencePalmares d)) ds
          @?= [("amount", "10", "11")],
      testCase "an exception on (row, key, field) explains that divergence and nothing else" $ do
        let d1 = Divergence "a" "amount" "10" "11"
            d2 = Divergence "b" "amount" "10" "11"
            e = ExceptionEntry R3 "a" (Just "amount") "known" (fromGregorian 2026 10 20) "it@blazarlabs.io"
        applyExceptions [e] R3 [d1, d2] @?= ([d2], [d1])
        applyExceptions [e] R5 [d1, d2] @?= ([d1, d2], []),
      testCase "an exception with no field explains every field of that key" $ do
        let e = ExceptionEntry R2 "a" Nothing "known" (fromGregorian 2026 10 20) "it@blazarlabs.io"
        applyExceptions [e] R2 [Divergence "a" "delegator_count" "13" "12"]
          @?= ([], [Divergence "a" "delegator_count" "13" "12"]),
      testCase "an exception that matched nothing is reported stale" $ do
        let e = ExceptionEntry R9 "gone" Nothing "stale" (fromGregorian 2026 10 20) "it@blazarlabs.io"
        staleExceptions [e] [mkRow [] R9 7 []] @?= [e],
      testCase "R10 aborts on epoch skew > 1 or lag > the bound, and only then" $ do
        checkFreshness 320 320 41 300 @?= Fresh
        checkFreshness 320 321 41 300 @?= Fresh
        checkFreshness 318 320 41 300 @?= Aborted "epoch skew 2 > 1 (ours 318, koios 320)"
        checkFreshness 320 320 412 300 @?= Aborted "lag 412s > 300s",
      testCase "exit code: 0 clean or explained, 1 unexplained, 3 aborted" $ do
        let t = UTCTime (fromGregorian 2026 10 20) 0
            ok = mkReport "preprod" t 320 41 320 [] [mkRow [] R1 294 []]
            bad = mkReport "preprod" t 320 41 320 [] [mkRow [] R1 294 [Divergence "a" "membership" "present" "absent"]]
            skew = "epoch skew 2 > 1 (ours 318, koios 320)"
            aborted = mkReport "preprod" t 318 41 320 []
              [mkRow [] R10 1 [Divergence "freshness" "epoch_skew" "320" "318"], skippedRow R1 skew]
        (reportStatus ok, reportUnexplained ok, reportExitCode ok) @?= (ReportOk, 0, 0)
        (reportStatus bad, reportUnexplained bad, reportExitCode bad) @?= (ReportDivergent, 1, 1)
        (reportStatus aborted, reportExitCode aborted) @?= (ReportAborted, 3),
      testCase "the committed waiver file parses and every entry carries reason/opened/owner" $ do
        es <- loadExceptions "config/reconcile-exceptions.yaml" >>= either assertFailure pure
        assertBool "no empty reason" (all (not . T.null . exceptionReason) es)
        assertBool "no empty owner" (all (not . T.null . exceptionOwner) es)
    ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-show-details=direct --test-options='-p "/Reconcile primitives/"'`
Expected: FAIL with `error: [GHC-87110] Could not find module ‘Palmares.Reconcile’`

- [ ] **Step 3: Implement the types, `diffKeyed`, the waiver file and the report in `src/lib/offchain-lib/Palmares/Reconcile.hs`**

One comparison primitive, because all ten rows are keyed comparisons:

```haskell
-- | The only comparison primitive. A key on one side only becomes a @membership@ divergence; a key on
-- both sides is handed to the field comparator, which returns ONLY the fields that differ, as
-- (field, koios, palmares). R9's multiset equality is this with k = (address, amount) and v = count.
diffKeyed :: (Ord k) => (k -> Text) -> (a -> b -> [(Text, Text, Text)]) -> Map k a -> Map k b -> [Divergence]
```

The wire names are `koios` and `palmares`, **not** `expected`/`actual` (T-DATA §8.2 wrote the latter): the
report must not read as "Koios is right". `reportExitCode`: 0 ok, 1 unexplained, 3 aborted — **2 is reserved
for transport failure and is produced by the exe, never here**. `ExceptionEntry` is `FromJSON` over
`Data.Yaml.decodeFileEither`, with `exceptionRow :: RowId` decoding `"R1"…"R10"`.

`RowStatus = RowOk | RowDivergent | RowSkipped Text`; `isSkipped` is its predicate and `skippedRow` its
constructor. `mkReport` returns `ReportAborted` **whenever any row is `RowSkipped`** — that is the only
path to exit 3, so the exe needs no abort branch of its own and a skipped row can never be counted as a
pass. (This is why there is no separate `abortedReport`: T-PARITY-3's `reconcileAll` already emits the
skipped rows, and a second report constructor would be a second place for the two to drift.)

`config/reconcile-exceptions.yaml`, seeded empty but with the schema in comments:

```yaml
# Documented divergences against Koios preprod (F1 check 6: "explained divergences are documented").
# An entry here turns one divergence from `unexplained` into `explained`. Nothing else suppresses a row.
# An entry that matches no divergence in a run is reported as `stale_exceptions` — delete it.
# row:    R1 … R10
# key:    the row's comparison key verbatim as the report prints it (a CIP-129 id, "<id>:<epoch>", …)
# field:  one field name; omit to explain every field of that key
# reason / opened / owner: all three required.
exceptions: []
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-show-details=direct --test-options='-p "/Reconcile primitives/"'`
Expected: PASS (8 cases)

- [ ] **Step 5: Commit**

```bash
git add src/lib/offchain-lib/Palmares/Reconcile.hs config/reconcile-exceptions.yaml \
        src/test/UnitTests/Reconcile.hs palmares.cabal
git commit -m "feat(reconcile): comparison primitive, waiver file, freshness guard, report"
```

---

### Task T-PARITY-3: the ten rows — `reconcileAll`

**Files:**
- Modify: `src/lib/offchain-lib/Palmares/Reconcile.hs` (add the snapshots + `reconcileAll`)
- Modify: `src/test/UnitTests/Reconcile.hs` (add `rowTests`)
- Test: `src/test/UnitTests/Reconcile.hs`

**Interfaces:**
- Consumes: `Palmares.Reconcile.Koios` (T-PARITY-1); `diffKeyed`, `mkRow`, `skippedRow`, `isSkipped`,
  `applyExceptions`, `checkFreshness` (T-PARITY-2);
  the DTOs of synthesis §3 step 18 — `StatusResponse` (`governance.epoch`, `governance.lag_seconds`),
  `DRepSummary` (`id`, `has_script`, `status`, `active_until_epoch`, `delegator_count`), `EpochPowerPoint`
  (`epoch`, `lovelace`), `GovActionSummary` (`bech32`, `type`, `proposed_at.epoch`, `expires_epoch`,
  `lifecycle.*_epoch`), `GovActionDetailResponse` (`tallies[].{role,counts}`,
  `withdrawals[].{beneficiary,amount}`), `VoteRecord` (`gov_action_id`, `verdict`, `cast_at.tx_hash`),
  `CommitteeResponse` (`quorum_*`, `members[].{cold_bech32,hot_hash,expiration_epoch}`);
  **`GET /votes` must return every `palmares.vote` row (full history, not `seq_last = 1`)** — R7 is the gate
  on that; `palmares.drep_epoch_power` carrying `target_kind` with no filter (D-20);
  `Palmares.DomainTypes.Core.Ids` (CIP-129) for the ids both sides join on (synthesis §3 step 7).
- Produces: `KoiosSnapshot`, `PalmaresSnapshot`, `reconcileAll`, `loadKoiosSnapshot`,
  `loadPalmaresSnapshot`, and the `test/fixtures/snapshot/{koios,palmares}/` layout the dumper must write.

- [ ] **Step 1: Write the failing test**

Hand-built minimal snapshots (two DReps `drep1a…`/`drep1b…` + the two pseudo-DReps, one action, one
committee member, one withdrawal) — no recording, fully deterministic. `kOk`/`pOk` are the consistent
pair; each case applies one `set*`/`drop*`/`dup*` mutation to it, and those mutators are plain record
updates in the test module, not library code.

```haskell
rowTests :: TestTree
rowTests =
  testGroup
    "Reconcile rows"
    [ testCase "a consistent snapshot pair yields all ten rows ok" $ do
        let rs = reconcileAll kOk pOk []   -- kOk / pOk: the consistent hand-built pair
        map rowId rs @?= [R1, R2, R3, R4, R5, R6, R7, R8, R9, R10]
        map rowStatus rs @?= replicate 10 RowOk
        sum (map (length . rowUnexplained) rs) @?= 0,
      testCase "R1: a DRep Koios has and we do not is one membership divergence" $ do
        let rs = reconcileAll kOk (dropDRep "drep1b…" pOk) []
            r1 = row R1 rs
        rowStatus r1 @?= RowDivergent
        rowUnexplained r1 @?= [Divergence "drep1b…" "membership" "present" "absent"],
      testCase "R2: expires_epoch_no is compared against active_until_epoch" $ do
        let r2 = row R2 (reconcileAll kOk (setActiveUntil "drep1a…" 334 pOk) [])
        rowUnexplained r2 @?= [Divergence "drep1a…" "active_until_epoch" "333" "334"],
      testCase "R3: voting power is exact lovelace, keyed by (drep_id, epoch)" $ do
        let r3 = row R3 (reconcileAll kOk (setPower "drep1a…" 319 3923709956712 pOk) [])
        rowUnexplained r3
          @?= [Divergence "drep1a…:319" "lovelace" "3923709956713" "3923709956712"],
      testCase "R4: the per-epoch total includes BOTH pseudo-DReps (D-20)" $ do
        let r4 = row R4 (reconcileAll kOk (dropDRep "drep_always_abstain" pOk) [])
        rowStatus r4 @?= RowDivergent
        rowUnexplained r4
          @?= [Divergence "319" "total_lovelace" "937803900128796" "505940109170837"],
      testCase "R5: lifecycle epochs are exact per gov_action1… id" $ do
        let r5 = row R5 (reconcileAll kOk (setRatified "gov_action1…" (Just 229) pOk) [])
        rowUnexplained r5 @?= [Divergence "gov_action1…" "ratified_epoch" "228" "229"],
      testCase "R6: the tally map is (role, verdict) -> count, latest vote only" $ do
        let r6 = row R6 (reconcileAll kOk (setTally "gov_action1…" DRep Yes 6 pOk) [])
        rowUnexplained r6 @?= [Divergence "gov_action1…:drep:yes" "count" "7" "6"],
      testCase "R7: the vote-history set keeps superseded votes" $ do
        let r7 = row R7 (reconcileAll kOk (dropVote "drep1a…" pOk) [])
        rowUnexplained r7
          @?= [Divergence "gov_action1…:yes:0bb1…" "membership" "present" "absent"],
      testCase "R8: quorum fraction and hot hex per cold id" $ do
        let r8 = row R8 (reconcileAll kOk (setQuorum 3 3 pOk) [])
        rowUnexplained r8 @?= [Divergence "committee" "quorum" "2/3" "3/3"],
      testCase "R9: withdrawals are a (beneficiary, amount) multiset" $ do
        let r9 = row R9 (reconcileAll kOk (dupWithdrawal "gov_action1…" pOk) [])
        rowUnexplained r9 @?= [Divergence "stake_test1…:120000000000" "count" "1" "2"],
      testCase "R10 aborting skips every other row rather than faking a pass" $ do
        let rs = reconcileAll (setKoiosTipEpoch 322 kOk) pOk []
        rowStatus (row R10 rs) @?= RowDivergent
        assertBool "all others skipped"
          (all (\r -> rowId r == R10 || isSkipped (rowStatus r)) rs),
      testCase "a waived divergence becomes explained and the run exits 0" $ do
        let e = ExceptionEntry R2 "drep1a…" (Just "delegator_count") "Koios counts at its own tip"
                  (fromGregorian 2026 10 20) "it@blazarlabs.io"
            rs = reconcileAll kOk (setDelegators "drep1a…" 12 pOk) [e]
            rp = mkReport "preprod" t0 320 41 320 [e] rs
        rowStatus (row R2 rs) @?= RowOk
        (reportUnexplained rp, reportExplained rp, reportExitCode rp) @?= (0, 1, 0)
    ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-show-details=direct --test-options='-p "/Reconcile rows/"'`
Expected: FAIL with `Variable not in scope: reconcileAll`

- [ ] **Step 3: Implement `reconcileAll :: KoiosSnapshot -> PalmaresSnapshot -> [ExceptionEntry] -> [RowResult]`**

`reconcileAll` evaluates R10 first and, on `Aborted msg`, returns `R10` divergent plus
`RowSkipped msg` for R1–R9 — comparing across an epoch boundary manufactures divergences, so a skipped row
must never read as a pass. Each row is one `diffKeyed` call; the table below is the full specification and
is the one thing the signature and tests do not determine. Koios base `https://preprod.koios.rest/api/v1`;
`E` ranges over the **five completed** epochs `[tip−5 … tip−1]` (315–319 at tip 320).

| Row | Koios endpoint → fields | Palmares endpoint → fields | Key | Compared | Tolerance |
|---|---|---|---|---|---|
| **R1** | `GET /drep_list?registered=eq.true` → `drep_id` | `GET /dreps?status=active&status=inactive&limit=200` → `items[].id` | CIP-129 `drep1…` | set membership (294 at epoch 320) | **exact**, zero symmetric difference |
| **R2** | `POST /drep_info` `{"_drep_ids":[≤100]}` → `has_script`, `expires_epoch_no`, `registered`, `active`, `live_delegator_count` | the same `/dreps` items → `has_script`, `active_until_epoch`, `status`, `delegator_count` | CIP-129 `drep1…` | `has_script`; `expires_epoch_no == active_until_epoch`; `koiosDRepStatus registered active == status`; `live_delegator_count == delegator_count` | **exact** on the first three; see OPEN for `delegator_count` |
| **R3** | `GET /drep_voting_power_history?_epoch_no=E` → `drep_id`, `epoch_no`, `amount` | `GET /drep/{id}/power?from_epoch=315&to_epoch=319` → `items[].{epoch,lovelace}`, for every R1 id **and** `drep_always_abstain` / `drep_always_no_confidence` | `"<drep_id>:<epoch>"` | `amount == lovelace` | **exact lovelace** — both sides read `drep_distr`, so a tolerance would hide a bug |
| **R4** | the same rows, **unfiltered** sum per epoch | Σ over the same R3 rows, i.e. `target_kind IN ('drep','always_abstain','always_no_confidence')` (**D-20**) | `"<epoch>"` | Σ `amount`. Epoch 319 reference: 937 803 900 128 796, of which always_abstain 431 863 790 957 959 (46 %) and always_no_confidence 325 390 800 383 | **exact (0)** — this *is* spec check 5's "documented tolerance", and it is zero; the 46 % it used to need was the pseudo-DRep filter |
| **R5** | `GET /proposal_list` → `proposal_id`, `proposal_type`, `proposed_epoch`, `expiration`, `ratified/enacted/dropped/expired_epoch` | `GET /gov-actions?limit=200` → `items[].{bech32,type,proposed_at.epoch,expires_epoch,lifecycle.*}` | CIP-129 `gov_action1…` | `koiosActionType proposal_type == type`, plus the five epochs. Population-wide (all 131), no sample | **exact** |
| **R6** | `GET /proposal_votes?_proposal_id=` (**latest per voter**) | `GET /gov-action/{id}` → `tallies[].{role,counts.{yes,no,abstain}}` (computed over `palmares.vote WHERE seq_last = 1`) | `"<gov_action1…>:<role>:<verdict>"` | the count map | **exact** |
| **R7** | `GET /drep_votes?_drep_id=` (**full history, superseded votes included**) | `GET /votes?voter_id=<id>&limit=200` → `items[].{gov_action_id,verdict,cast_at.tx_hash}` | `"<gov_action1…>:<verdict>:<vote_tx_hash>"` | set membership | **exact**. DRep role only: preprod carries 3 243 802 CC vote rows of test spam, so no row compares CC history |
| **R8** | `GET /committee_info` → `quorum_numerator/denominator`, `members[].{cc_cold_id,cc_hot_hex,expiration_epoch}` | `GET /committee` → `quorum_*`, `members[].{cold_bech32,hot_hash,expiration_epoch}` | `"committee"` for the quorum, CIP-129 `cc_cold1…` per member | `"<num>/<den>"`; `cc_hot_hex == hot_hash`; `expiration_epoch`. Preprod: 2/3, 3 members, all 372, all script creds | **exact**; member `status` excluded, see OPEN |
| **R9** | `proposal_list[].withdrawal[]` → `stake_address`, `amount` | `GET /gov-action/{id}` → `withdrawals[].{beneficiary,amount}` | `"<stake_address>:<amount>"` → count | multiset equality, for every action with a non-empty `withdrawal[]` | **exact** |
| **R10** | `GET /tip` → `epoch_no` | `GET /status` → `governance.{epoch,lag_seconds}` | `"freshness"` | guard, not a comparison | abort when `abs(Δepoch) > 1` or `lag_seconds > PALMARES_READY_MAX_LAG_SECONDS` (**300**, D-7) |

`deposit` is deliberately **not** in R2 (T-DATA §8.2 listed it): `DRepSummary` carries no `deposit`, it is the
protocol constant `epoch_param.drep_deposit`, and fetching it would cost 294 extra `/drep/{id}/updates`
calls to compare a constant.

`loadKoiosSnapshot`/`loadPalmaresSnapshot :: FilePath -> IO (Either String a)` read one file per field, so
the recorded layout lives in exactly one place:

```
test/fixtures/snapshot/koios/     tip.json drep_list.json drep_info.json
                                  drep_voting_power_history.json proposal_list.json
                                  proposal_votes.json drep_votes.json committee_info.json
test/fixtures/snapshot/palmares/  status.json dreps.json drep_power.json gov_actions.json
                                  gov_action_detail.json votes.json committee.json
```
Under `snapshot/`, **not** `test/fixtures/koios/` — that directory holds T-PARITY-1's single-id wire
fixtures, whose pinned counts (16 vote rows, one `drep_info` row) the multi-id dump would destroy.
The five keyed files (`proposal_votes`, `drep_votes`, `drep_power`, `gov_action_detail`, `votes`) are one
JSON object keyed by the id; the flat ones are the concatenated rows / `Page.items`.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-show-details=direct --test-options='-p "/Reconcile rows/"'`
Expected: PASS (12 cases)

- [ ] **Step 5: Commit**

```bash
git add src/lib/offchain-lib/Palmares/Reconcile.hs src/test/UnitTests/Reconcile.hs
git commit -m "feat(reconcile): the ten R1-R10 parity rows over both snapshots"
```

---

### Task T-PARITY-4: the pinned sample

**Files:**
- Create: `config/reconcile-sample.preprod.json`
- Modify: `src/lib/offchain-lib/Palmares/Reconcile.hs` (add `ReconcileSample` + `loadSample`)
- Modify: `src/test/UnitTests/Reconcile.hs` (add `sampleTests`)
- Modify: `palmares.cabal` (`extra-source-files: config/reconcile-sample.preprod.json`)
- Test: `src/test/UnitTests/Reconcile.hs`

**Interfaces:**
- Consumes: a caught-up preprod stack (synthesis §3 step 11) and the `/dreps` + `/gov-actions`
  `order_by` values `votes_cast`, `registered_at`, `proposed_at`, `treasury_amount` (synthesis §3
  step 18) to draw the ids once.
- Produces: `ReconcileSample` (`sampleNetwork`, `sampleRecordedAtEpoch`, `sampleVoteHistoryDreps`,
  `sampleTallyActions`), `loadSample`, `config/reconcile-sample.preprod.json`.

- [ ] **Step 1: Write the failing test**

CI gates the spec floor on the shipped file, so the floor can never silently regress:

```haskell
sampleTests :: TestTree
sampleTests =
  testGroup
    "Reconcile sample"
    [ testCase "the shipped sample meets spec F1 check 6's floor and is pinned, not drawn" $ do
        s <- loadSample "config/reconcile-sample.preprod.json" >>= either assertFailure pure
        sampleNetwork s @?= "preprod"
        sampleRecordedAtEpoch s @?= 320
        assertBool ">= 20 DReps (spec floor)" (length (sampleVoteHistoryDreps s) >= 20)
        assertBool ">= 10 gov actions (spec floor)" (length (sampleTallyActions s) >= 10)
        assertBool "all CIP-129 drep ids" (all (T.isPrefixOf "drep1") (sampleVoteHistoryDreps s))
        assertBool "all CIP-129 gov action ids" (all (T.isPrefixOf "gov_action1") (sampleTallyActions s))
        assertBool "no duplicate drep id" (nub (sampleVoteHistoryDreps s) == sampleVoteHistoryDreps s)
        assertBool "no duplicate action id" (nub (sampleTallyActions s) == sampleTallyActions s)
        assertBool "the worked-example DRep is in the sample"
          ("drep1y2qn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qmwj3xg" `elem` sampleVoteHistoryDreps s)
    ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-show-details=direct --test-options='-p "/Reconcile sample/"'`
Expected: FAIL with `config/reconcile-sample.preprod.json: openBinaryFile: does not exist`

- [ ] **Step 3: Add `ReconcileSample` + `loadSample`, then draw the sample once and commit it**

`ReconcileSample` is `FromJSON` via the house `StripPrefix "sample" + CamelToSnake`, so its four fields
land on `network`, `recorded_at_epoch`, `vote_history_dreps`, `tally_actions`;
`loadSample :: FilePath -> IO (Either String ReconcileSample)` is `eitherDecodeFileStrict`.

No randomness anywhere — a shell script cannot draw a reproducible random sample, and T-DATA §7.2's
"fixed-seed random 10" is replaced by a second deterministic selector over a different population. Both
lists are sorted by their key so the file is stable under regeneration:

- `vote_history_dreps` (R7) = the **30 DReps with the most vote rows**, ordered by `(vote_rows DESC,
  drep_id ASC)` — the interesting tail — **union** the **10 most recently registered DReps with ≥ 1 vote**,
  ordered by `(registered_at DESC, drep_id ASC)` — the newest cohort, which the tail never covers —
  **union the worked-example DRep** `drep1y2qn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qmwj3xg`
  (T-METRIC §4), explicitly, because its 16 votes are not guaranteed to reach the top 30 and the test
  below asserts it is present.
- `tally_actions` (R6, R9) = the union of: all actions with `ratified_epoch IS NOT NULL` (11), all
  `dropped_epoch`-only actions (6), the 5 treasury withdrawals with the largest `withdrawal[]` total, and
  the 2 oldest bootstrap-era actions — every lifecycle shape and the whole withdrawal path, ≈ 24 ids.

Drawn from our own API, with **one** Koios call — the re-voting-DRep line, which only picks an *id* that
both sides are then compared on, never a value. The last command is what makes the draw reproducible:
`sort -u` gives both lists one canonical order, so a redraw either matches the committed file
byte-for-byte or shows a real population change in `git diff`.

```bash
P=https://gov.cardano.vip/api
curl -s "$P/dreps?status=active&status=inactive&order_by=votes_cast&sort_order=desc&limit=30" \
  | jq -r '.items[].id' > /tmp/tail.txt
curl -s "$P/dreps?status=active&status=inactive&order_by=registered_at&sort_order=desc&limit=200" \
  | jq -r '.items[] | select(.votes_cast > 0) | .id' | head -10 > /tmp/new.txt
echo drep1y2qn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qmwj3xg >> /tmp/new.txt
# plus one DRep that actually re-voted, so R7's superseded-vote path is exercised live (OPEN item 4)
curl -s "https://preprod.koios.rest/api/v1/vote_list?limit=1000" \
  | jq -r 'group_by(.voter_id + .proposal_id)[] | select(length > 1)
           | select(.[0].voter_role == "DRep") | .[0].voter_id' | head -1 >> /tmp/new.txt
curl -s "$P/gov-actions?limit=200" | jq -r '[.items[]
  | select(.lifecycle.ratified_epoch != null
        or (.lifecycle.dropped_epoch != null and .lifecycle.expired_epoch == null
            and .lifecycle.enacted_epoch == null))] | .[].bech32' > /tmp/lifecycle.txt
curl -s "$P/gov-actions?type=treasury_withdrawals&order_by=treasury_amount&sort_order=desc&limit=5" \
  | jq -r '.items[].bech32' >> /tmp/lifecycle.txt
curl -s "$P/gov-actions?order_by=proposed_at&sort_order=asc&limit=2" | jq -r '.items[].bech32' >> /tmp/lifecycle.txt

jq -n --arg epoch "$(curl -s "$P/status" | jq -r '.governance.epoch')" \
  --rawfile d <(sort -u /tmp/tail.txt /tmp/new.txt) \
  --rawfile a <(sort -u /tmp/lifecycle.txt) \
  '{ network: "preprod", recorded_at_epoch: ($epoch | tonumber),
     vote_history_dreps: ($d | split("\n") | map(select(length > 0))),
     tally_actions:      ($a | split("\n") | map(select(length > 0))) }' \
  > config/reconcile-sample.preprod.json
```

The shape it writes:

```json
{
  "network": "preprod",
  "recorded_at_epoch": 320,
  "vote_history_dreps": ["drep1…", "…≥ 20 ids, sorted"],
  "tally_actions": ["gov_action1…", "…≈ 24 ids, sorted"]
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-show-details=direct --test-options='-p "/Reconcile sample/"'`
Expected: PASS (1 case, 9 assertions)

- [ ] **Step 5: Commit**

```bash
git add config/reconcile-sample.preprod.json src/lib/offchain-lib/Palmares/Reconcile.hs \
        src/test/UnitTests/Reconcile.hs palmares.cabal
git commit -m "feat(reconcile): pinned preprod parity sample, CI-gated against the spec floor"
```

---

### Task T-PARITY-5: `admin reconcile-koios`

**Files:**
- Create: `src/exe/admin/Reconcile.hs`
- Modify: `src/exe/admin/Main.hs` (`Command` sum, `commandParser`, the no-provider branch of `main`)
- Modify: `palmares.cabal` (`executable admin`: `other-modules: Reconcile`; `build-depends:` + `aeson,
  containers, directory, filepath, http-client, http-client-tls, http-types, time`)
- Modify: `.gitignore` (`out/`)
- Modify: `src/test/UnitTests/Reconcile.hs` (add `replayTests`)
- Create: `test/fixtures/snapshot/koios/{tip,drep_list,drep_info,drep_voting_power_history,proposal_list,proposal_votes,drep_votes,committee_info}.json`
- Create: `test/fixtures/snapshot/palmares/{status,dreps,drep_power,gov_actions,gov_action_detail,votes,committee}.json`
- Test: `src/test/UnitTests/Reconcile.hs` + the live run below

**Interfaces:**
- Consumes: **a caught-up from-genesis preprod sync** (synthesis §3 step 11 — R10 aborts otherwise);
  `sql/040_indexes.sql` applied (step 12, or every query is measuring the wrong plan); the `palmares.*`
  views including `drep_epoch_power` with `target_kind` (step 15); `sql/030_metrics.sql` +
  `Metrics/Catalog.hs` (step 16, for the `participation_rate`/`delegator_count` fields R2 reads);
  `Query/Gov.hs` + `RestAPI.hs` + the DTOs, which are where the CIP-129 ids both sides join on are rendered
  (steps 7 and 18); `reconcileAll`, `loadExceptions`, `loadSample`, `mkReport`,
  `reportExitCode`, `loadKoiosSnapshot`, `loadPalmaresSnapshot` (T-PARITY-2/3/4).
- Produces: the `reconcile-koios` subcommand and its flags; `runReconcileKoios`; env vars `PALMARES_API`,
  `KOIOS_API`, `KOIOS_API_TOKEN`; reads `PALMARES_READY_MAX_LAG_SECONDS` (produced by step 18, D-7);
  `out/divergences.json` and its schema; exit codes 0/1/2/3; the recorded snapshot pair under
  `test/fixtures/snapshot/`.

- [ ] **Step 1: Write the failing test**

The recorded-fixture mechanism, stated: `--dump-snapshots DIR` writes the **raw** response bodies into
`DIR/{koios,palmares}/` in the T-PARITY-3 layout, and CI replays that committed pair through
`reconcileAll` with **no network at all**, so the nightly outage of either service can never fail a PR.
It is also the "capture the evidence" tool when the nightly does diverge.

```haskell
replayTests :: TestTree
replayTests =
  testGroup
    "Reconcile replay"
    [ testCase "the recorded preprod snapshot pair reconciles with 0 unexplained divergences" $ do
        k <- loadKoiosSnapshot "test/fixtures/snapshot/koios" >>= either assertFailure pure
        p <- loadPalmaresSnapshot "test/fixtures/snapshot/palmares" >>= either assertFailure pure
        es <- loadExceptions "config/reconcile-exceptions.yaml" >>= either assertFailure pure
        let rs = reconcileAll k p es
            rp = mkReport "preprod" t0 320 41 320 es rs
        map rowId rs @?= [R1, R2, R3, R4, R5, R6, R7, R8, R9, R10]
        map rowStatus rs @?= replicate 10 RowOk
        (reportStatus rp, reportUnexplained rp, reportExitCode rp) @?= (ReportOk, 0, 0)
        reportStaleExceptions rp @?= []
        rowCompared (row R1 rs) @?= 294
        rowCompared (row R5 rs) @?= 131
    ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-show-details=direct --test-options='-p "/Reconcile replay/"'`
Expected: FAIL with `test/fixtures/snapshot/palmares/status.json: openBinaryFile: does not exist`

- [ ] **Step 3: Implement the subcommand in `src/exe/admin/Reconcile.hs` and wire it into `src/exe/admin/Main.hs`**

```haskell
data ReconcileKoiosArgs = ReconcileKoiosArgs
  { rkaPalmaresApi    :: String    -- --palmares-api    default $PALMARES_API
  , rkaKoiosApi       :: String    -- --koios-api       default $KOIOS_API, else https://preprod.koios.rest/api/v1
  , rkaSample         :: FilePath  -- --sample          default config/reconcile-sample.preprod.json
  , rkaExceptions     :: FilePath  -- --exceptions      default config/reconcile-exceptions.yaml
  , rkaOut            :: FilePath  -- --out             default out/divergences.json
  , rkaMaxLagSeconds  :: Int       -- --max-lag-seconds default $PALMARES_READY_MAX_LAG_SECONDS, else 300
  , rkaDumpSnapshots  :: Maybe FilePath  -- --dump-snapshots DIR
  }

-- | Fetch both sides, reconcile, write out/divergences.json, return the process exit code.
-- No markdown sibling (T-DATA §8.2 wanted one): the wrapper's jq already prints the per-row table.
runReconcileKoios :: ReconcileKoiosArgs -> IO Int
```

`main` dispatches `ReconcileKoios` in the **no-provider** branch beside `WriteBlueprint`: it must never read
`operation.prv` or `ATLAS_CORE_CONFIG`, because CI runs it with no keys at all. No `--sample-dreps` /
`--sample-actions` counts (T-INFRA §5.3 passed them): a count without a pinned order is a non-reproducible
draw, and the sample file *is* the sample.

Fetch order and cost — R10 first, then everything else; each list fetch pages `limit=1000&offset=N`
(Koios) / `limit=200&offset=N` (ours) until a short page, and **never** uses a reported total as the loop
bound. Koios calls only: 250 ms between requests, honour `Retry-After`, and send
`Authorization: Bearer $KOIOS_API_TOKEN` when the variable is set (CI secret store, never the repo).
Our own API needs no delay. The table below totals **76 Koios requests** (≈ 19 s of pacing) and
**365 local ones**.

| Call | Count at epoch 320 |
|---|---|
| `/tip`, `/status` | 1 + 1 |
| `/drep_list`, `/dreps` | 1 + 2 |
| `POST /drep_info` (≤ 100 ids) | 3 |
| `/drep_voting_power_history?_epoch_no=E` (**`_epoch_no` is mandatory — a bare GET 504s**) | 5 |
| `/drep/{id}/power` for the 294 ids + the 2 reserved | 296 |
| `/proposal_list`, `/gov-actions` | 1 + 1 |
| `/proposal_votes`, `/gov-action/{id}` over `tally_actions` | 24 + 24 |
| `/drep_votes`, `/votes?voter_id=` over `vote_history_dreps` | 40 + 40 |
| `/committee_info`, `/committee` | 1 + 1 |

Exit codes: **0** every row ok or explained · **1** ≥ 1 unexplained divergence · **2** transport or decode
failure on either side (the only code produced here rather than by `reportExitCode`) · **3** R10 abort,
which is neither a pass nor a fail and which `mkReport` derives from the `RowSkipped` rows
`reconcileAll` already emitted, so there is no abort branch here. `out/divergences.json`:

```json
{ "network": "preprod", "ran_at": "2026-10-20T03:00:11Z",
  "palmares_epoch": 320, "palmares_lag_seconds": 41, "koios_epoch": 320,
  "status": "ok", "unexplained": 0, "explained": 0, "stale_exceptions": [],
  "rows": [ { "row": "R1", "compared": 294, "status": "ok", "unexplained": [], "explained": [] } ] }
```

Header comment, verbatim, because a future contributor *will* be tempted: *"Koios runs on cardano-db-sync
too. It is a completeness check, never a source of truth, and never a fallback when our own sync is
behind."*

- [ ] **Step 4: Run test to verify it passes, then verify the live run**

Record the pair and run the replay test:

```
PALMARES_API=https://gov.cardano.vip/api cabal run -v0 admin -- reconcile-koios \
  --dump-snapshots test/fixtures/snapshot --out out/divergences.json
cabal test --test-show-details=direct --test-options='-p "/Reconcile replay/"'
```
Expected: PASS

Live verification — this is F1 check 6, and no unit test can stand in for it:

```
PALMARES_API=https://gov.cardano.vip/api cabal run -v0 admin -- reconcile-koios; echo "exit=$?"
jq -e '.status=="ok" and .unexplained==0
       and ([.rows[].row]|sort)==["R1","R10","R2","R3","R4","R5","R6","R7","R8","R9"]
       and ([.rows[].compared]|min)>0' out/divergences.json
jq -r '[.rows[]|select(.row=="R1" or .row=="R5")|{(.row):.compared}]|add' out/divergences.json
```
Expected (this is the observable "0 unexplained divergences"): `exit=0`, both `jq`s exit 0, and the
second prints `{"R1":294,"R5":131}` — the two rows whose compared counts are pinned population figures
(T-DATA §7.1). R3's count is whatever `drep_distr` holds over epochs 315–319 (~190/epoch, 195 at 319),
so it is gated by `min > 0` and not by a pinned number.

Then the guard, without having to stall db-sync:

```
PALMARES_API=https://gov.cardano.vip/api cabal run -v0 admin -- reconcile-koios --max-lag-seconds 0; echo "exit=$?"
```
Expected: `exit=3`, and `jq -r '.status' out/divergences.json` prints `aborted` with a `lag … > 0s` reason.

- [ ] **Step 5: Commit**

```bash
git add src/exe/admin/Reconcile.hs src/exe/admin/Main.hs palmares.cabal .gitignore \
        test/fixtures/snapshot src/test/UnitTests/Reconcile.hs
git commit -m "feat(admin): reconcile-koios subcommand + recorded snapshot replay test"
```

---

### Task T-PARITY-6: wrapper script and the nightly warn-only workflow

**Files:**
- Create: `scripts/reconcile_koios.sh`
- Create: `.github/workflows/reconcile.yml`
- Create: `docs/deployment/koios-parity-gate.md` (the release gate and what each exit code means)
- Test: the exit-code check below + the `workflow_dispatch` run of `.github/workflows/reconcile.yml`

**Interfaces:**
- Consumes: `admin reconcile-koios` (T-PARITY-5); `images.yml` publishing `ghcr.io/<owner>/palmares-admin`
  for `linux/arm64` with the `admin` entry in its matrix (synthesis §1.3); **`Dockerfile.admin` must set
  `ENTRYPOINT ["/usr/local/bin/admin"]`** so the wrapper can append the subcommand; the public hostname
  `gov.cardano.vip`.
- Produces: `scripts/reconcile_koios.sh`, `PALMARES_ADMIN_CMD`, the `reconcile` workflow, the
  `koios-divergences` artifact, `docs/deployment/koios-parity-gate.md`, and the release-gate `jq`
  one-liner.

- [ ] **Step 1: Write the failing test**

Two checks, neither of which is a unit test. The first is runnable locally and is what fails in Step 2:

```bash
# the wrapper must PROPAGATE the exe's exit code, not swallow it — `set -e` would abort before `case`
KOIOS_API=https://preprod.koios.rest/api/v1 PALMARES_ADMIN_CMD=/bin/false \
  ./scripts/reconcile_koios.sh; echo "exit=$?"
```

The second is the measurement that replaces a unit test for the workflow: a `workflow_dispatch` run on
`ubuntu-24.04-arm` whose observables are in Step 4.

- [ ] **Step 2: Run test to verify it fails**

Run the wrapper check above.
Expected: FAIL with `./scripts/reconcile_koios.sh: No such file or directory` and `exit=127`.

- [ ] **Step 3: Write `scripts/reconcile_koios.sh`, `.github/workflows/reconcile.yml` and the runbook section**

`chmod +x scripts/reconcile_koios.sh`. Warn-only is deliberate: the job needs a live, caught-up stack, so as
a merge gate it would block every PR whenever the Pi is re-syncing or Koios is down. The **release gate** is
read from the artifact instead, and `docs/deployment/koios-parity-gate.md` carries it verbatim together
with the four exit codes:

```bash
jq -e '.status=="ok" and .unexplained==0
       and ([.rows[].row]|sort)==["R1","R10","R2","R3","R4","R5","R6","R7","R8","R9"]' out/divergences.json
```

```yaml
# .github/workflows/reconcile.yml
name: reconcile
on:
  schedule: [{ cron: "0 3 * * *" }]
  workflow_dispatch:
jobs:
  koios:
    # ubuntu-24.04-arm, NOT ubuntu-latest (T-INFRA §5.3): images.yml publishes linux/arm64 only,
    # so an x86 runner cannot exec the admin image.
    runs-on: ubuntu-24.04-arm
    continue-on-error: true          # an upstream outage must never look like our bug
    steps:
      - uses: actions/checkout@v4
      - name: Koios preprod parity
        env:
          PALMARES_API: https://gov.cardano.vip/api
          KOIOS_API: https://preprod.koios.rest/api/v1
          KOIOS_API_TOKEN: ${{ secrets.KOIOS_API_TOKEN }}
          PALMARES_ADMIN_CMD: >-
            docker run --rm -e PALMARES_API -e KOIOS_API -e KOIOS_API_TOKEN
            -v ${{ github.workspace }}:/w -w /w
            ghcr.io/${{ github.repository }}-admin:latest
        run: ./scripts/reconcile_koios.sh
      - uses: actions/upload-artifact@v4
        if: always()
        with: { name: koios-divergences, path: out/ }
```

```bash
#!/usr/bin/env bash
# F1 check 6 — external parity against Koios preprod. Thin wrapper over `admin reconcile-koios`.
# Koios runs on cardano-db-sync too: it is a completeness check, never a source of truth, and never
# a fallback when our own sync is behind.
set -euo pipefail
: "${PALMARES_API:?set PALMARES_API, e.g. https://gov.cardano.vip/api}"
: "${KOIOS_API:=https://preprod.koios.rest/api/v1}"
: "${PALMARES_ADMIN_CMD:=cabal run -v0 admin --}"
export KOIOS_API
mkdir -p out
rc=0
${PALMARES_ADMIN_CMD} reconcile-koios --out out/divergences.json "$@" || rc=$?
case $rc in
  0) echo "parity: 0 unexplained divergences" ;;
  1) echo "parity: UNEXPLAINED DIVERGENCES — out/divergences.json" ;;
  2) echo "parity: transport failure — our API or Koios unreachable" ;;
  3) echo "parity: ABORTED (R10) — db-sync behind, or epoch skew > 1" ;;
  *) echo "parity: unexpected exit $rc" ;;
esac
[ -f out/divergences.json ] &&
  jq -r '.rows[] | "\(.row)\t\(.status)\tcompared=\(.compared)\tunexplained=\(.unexplained|length)"' \
     out/divergences.json
exit $rc
```

- [ ] **Step 4: Run both checks to verify they pass**

Run: `KOIOS_API=https://preprod.koios.rest/api/v1 PALMARES_ADMIN_CMD=/bin/false ./scripts/reconcile_koios.sh; echo "exit=$?"`
Expected: PASS — `exit=1` and no `out/divergences.json`, i.e. the wrapper reached the `case` and
propagated `/bin/false`'s code rather than dying on `set -e`.

Run: `gh workflow run reconcile.yml && sleep 180 && gh run list --workflow=reconcile.yml --limit 1`
Expected: PASS — the run is `completed/success` (job-level `continue-on-error` is what keeps the
*workflow* green on an upstream outage), `gh run download --name koios-divergences` yields
`out/divergences.json`, the log's last lines show ten `R*  ok  compared=…` rows, and the release-gate `jq`
above exits 0 against the downloaded file.

- [ ] **Step 5: Commit**

```bash
git add scripts/reconcile_koios.sh .github/workflows/reconcile.yml docs/deployment/koios-parity-gate.md
git commit -m "ci(reconcile): nightly warn-only Koios parity + release-gate runbook"
```

---

## Produces

Names later blocks (and the F1 sign-off) may consume:

- **Haskell, `offchain-lib`** — `Palmares.Reconcile.Koios`: `KoiosTip`, `KoiosDRepListRow`, `KoiosDRepInfo`,
  `KoiosPowerRow`, `KoiosProposal`, `KoiosWithdrawal`, `KoiosVoteRow`, `KoiosDRepVoteRow`, `KoiosCommittee`,
  `KoiosCommitteeMember`, `koiosActionType`, `koiosDRepStatus`, `koiosVote`, `koiosVoterRole`.
- **Haskell, `offchain-lib`** — `Palmares.Reconcile`: `RowId (R1…R10)`, `Divergence`
  (`key`/`field`/`koios`/`palmares`), `RowStatus (RowOk | RowDivergent | RowSkipped Text)`, `isSkipped`,
  `RowResult`, `ExceptionEntry`, `ReconcileSample`, `Freshness (Fresh | Aborted)`,
  `ReportStatus (ReportOk | ReportDivergent | ReportAborted)`, `Report`, `KoiosSnapshot`,
  `PalmaresSnapshot`, `diffKeyed`, `applyExceptions`, `staleExceptions`, `mkRow`, `skippedRow`,
  `checkFreshness`, `mkReport`, `reportExitCode`, `reconcileAll`, `loadExceptions`, `loadSample`,
  `loadKoiosSnapshot`, `loadPalmaresSnapshot`.
- **CLI** — `admin reconcile-koios` with `--palmares-api`, `--koios-api`, `--sample`, `--exceptions`,
  `--out`, `--max-lag-seconds`, `--dump-snapshots`; dispatched in the no-provider branch (no
  `operation.prv`, no `ATLAS_CORE_CONFIG`).
- **Exit codes** — 0 clean/explained · 1 unexplained · 2 transport · 3 R10 abort.
- **Files** — `scripts/reconcile_koios.sh`, `.github/workflows/reconcile.yml`,
  `docs/deployment/koios-parity-gate.md`, `config/reconcile-exceptions.yaml`,
  `config/reconcile-sample.preprod.json`, `out/divergences.json`, `test/fixtures/koios/*.json`
  (single-id wire fixtures), `test/fixtures/snapshot/{koios,palmares}/*.json` (the replay pair).
- **Env vars** — `PALMARES_API`, `KOIOS_API`, `KOIOS_API_TOKEN` (CI secret only),
  `PALMARES_ADMIN_CMD`; reads `PALMARES_READY_MAX_LAG_SECONDS` (default 300).
- **Tests** — `reconcileTests :: TestTree` with the groups `Koios wire`, `Reconcile primitives`,
  `Reconcile rows`, `Reconcile sample`, `Reconcile replay`.
- **Cabal deltas** — `offchain-lib` + `yaml`, `filepath`; `admin` + `aeson`, `containers`, `directory`,
  `filepath`, `http-client`, `http-client-tls`, `http-types`, `time` and `other-modules: Reconcile`;
  `extra-source-files` + `test/fixtures/koios/*.json`, `test/fixtures/snapshot/*/*.json`,
  `config/reconcile-sample.preprod.json`.
- **Contracts this block pins on other blocks** — `GET /votes` returns the full vote history, not
  `seq_last = 1` (R7); `palmares.drep_epoch_power` carries `target_kind` with no filter (R4, D-20);
  `GET /drep/{id}/power` answers for `drep_always_abstain` / `drep_always_no_confidence` (R3/R4);
  `GET /gov-action/{id}` carries `tallies[]` and `withdrawals[]` (R6/R9); `images.yml`'s matrix includes
  `admin`; `Dockerfile.admin` sets `ENTRYPOINT ["/usr/local/bin/admin"]`.

## Open items

- **(OPEN: tolerance for `live_delegator_count` in R2.** Ours is as-of `as_of_epoch`, Koios's is live at its
  own tip, and R10 still allows 1 epoch of skew. T-METRIC §9 leaves the `epoch_stake ↔ drep_distr`
  reconciliation tolerance unmeasured, so **no number exists to write down yet**. T-PARITY-3's table ships
  it as **exact**; the implementer measures the residue on the first caught-up run (synthesis §3 step 11)
  and either keeps exact or waives the residue in `config/reconcile-exceptions.yaml` with the measured
  count as the `reason`. **Decided by: whoever runs T-PARITY-5's live verification** — it is a measurement,
  not a judgement, and it needs no owner sign-off.**)**
- **(OPEN: Koios `committee_info` member `status` value set.** Not captured in any design note and not
  readable from the repo. Until it is, R8 compares only the quorum fraction, `cc_hot_hex` and
  `expiration_epoch`, and `KoiosCommitteeMember` has no `status` field. **Closed by: reading the recorded
  `test/fixtures/koios/committee_info.json` in T-PARITY-1 step 1** — one `jq -r '.members[].status'` — then
  adding the mapping to `CommitteeMemberStatus` (`authorized | unauthorized | resigned | expired`) and one
  comparator line to R8. No owner decision involved.**)**
- **(OPEN: the full CIP-129 id of the re-voted action.** T-DATA §8.2 R6 abbreviates it as
  `gov_action197dpq8x…` — 3 rows from `/proposal_votes` where `/vote_list` shows 4 from one DRep, and the
  remaining characters exist nowhere in the design notes. Needed as `$A` in T-PARITY-1's recording recipe,
  or the "latest-only" assertion has no re-vote to catch. **Closed by: `curl -s "$K/vote_list?limit=1000" |
  jq -r 'group_by(.voter_id + .proposal_id)[] | select(length > 1) | .[0].proposal_id' | head -1` in
  T-PARITY-1 step 1** — T-METRIC §3.2 proves 69 such pairs exist, so the query cannot come back empty.**)**
- **(OPEN: a pinned re-voting DRep id for R7's superseded-vote case.** T-DATA §8.2 R7 has one with 11 vote
  rows, 5 of them on a single action, but does not name it, and the worked-example DRep has 0 re-votes.
  T-PARITY-4 now pins the worked-example DRep explicitly, which covers the *membership* comparison but not
  the superseded-vote path, so that path is covered only by T-PARITY-3's hand-built snapshot until this id
  lands. **Closed by: the same `vote_list` group-by as above, taking `.[0].voter_id`**, appended to
  `/tmp/new.txt` in T-PARITY-4's draw beside the worked-example id.**)**
- **(OPEN: the preprod expired-action count — the one place two fact-checked notes still disagree.**
  T-DATA §7.1 says 102 expired (all also carrying `dropped_epoch`) and 11 open; T-METRIC §4 line 688 says
  103 expired and 12 open. Only `11 enacted + 102 expired + 6 dropped-only + 12 open = 131` closes, and
  T-METRIC's own `119 settled` (= 11 + 102 + 6) and `eligible_actions = 114` chain both need the 12, so
  T-PARITY-1 asserts **102** and **12** and T-DATA's "11 open" is the arithmetic that fails. **Decided by:
  the first recording** — `jq` the counts straight out of `test/fixtures/koios/proposal_list.json` in
  T-PARITY-1 step 1 and correct the two literals before step 4 if live preprod disagrees.**)**
