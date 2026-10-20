# Plan A — extraction from the Belt System + the CIP-129 id module

This block delivers synthesis §3 Phase 1 items 6–7: the Belt System engine, extracted into
`/Users/mg/Projects/CardanoProjects/palmares` as `Palmares.*` / package `palmares`, through the prescribed commit
sequence **as-is → rename → strip → GREEN GATE → rewrites** (the gate after *strip*, per T-EXTRACT §3.2 rule 3 as
corrected: 29 kept files import skipped modules); the `Palmares.DomainTypes.Core.Ids` CIP-129/CIP-105 encoder with its
golden test (synthesis §1.3, T-DATA §1.3); the Plan-A half of the test harness (green `cabal test`); and the two
inherited fixes that live in extracted off-chain code — #3 `authenticateStateUTxO` (one site after D-1/§4.3 cut
`Query/Live.hs`) and #5 chain-time-on-read. It implements D-14 (four libraries, no `govdata-lib`), D-1/§0.4 (no
`Query.Live`, no `RestAPI.Common`, no `multiset`), D-11 (`cassava` + `Palmares.WebAPI.CSV` in `webapi-lib`), D-9
(`Query/Rules.hs` stays skipped), and the §1.3 canonical names (`attest-sync`, `Query/Gov.hs`, `Query/Views.hs`).
**Assumptions:** the bootstrap task (synthesis §3 item 5) has already created the repo with `LICENSE`, `NOTICE`,
`licenses/`, `cabal.project` (T-EXTRACT §1.4 verbatim, no pin bumps), `palmares.cabal` (T-EXTRACT §1.3), the five
`CLAUDE.md` files, `Dockerfile.base`, `.envrc`, `.gitignore`, `hie.yaml`, `libblst.pc`, `scripts/test_exunits.sh`;
`direnv allow` + `cabal update` have run once. **Paths below are relative to that repo root**; `$SRC` =
`/Users/mg/Projects/CardanoProjects/Decentralized-Belt-System` (READ-ONLY). Fixes #1, #2 and #4 touch validators and are
**Plan B's**; the attestation payload of the 25 rewrite files is **Plan B's**; the frontend is Plan C's.
**On fixes #3 and #5 (Tasks T-EXTRACTION-5 and -6):** synthesis §3 lists both under Phase 4 step 22, because their
*wiring* (`Ingestion.projectChainEvent`, `getActivityFeed`) is attest-sync behaviour. What lands here is only the part
that is extraction hygiene — a pure helper, a deletion the §4 cut list already assigns to the extraction, and the
removal of a wall clock from a file being extracted anyway. Both wirings and both behavioural gates stay Plan B's, and
are named as such. If the assembler would rather not carry them in Plan A at all, these two tasks move intact to
Plan B's Phase 4 with no effect on Tasks 1–4.

---

### Task T-EXTRACTION-1: As-is extraction commit (byte-identical copy)

**Files:**
- Create: `extraction-keep.txt` (**69** lines — every manifest row whose verdict is not `skip`: as-is 19 + rename/strip **25** + rewrite 25. T-EXTRACT §2.8 says `rename/strip 24`; counted row by row across §2.1–§2.7 there are 25 strip rows, so the non-skip total is 69, not 68. All 69 verified to exist in `$SRC`.)
- Create: the 69 copied files, at their **Belt System paths** (`src/lib/onchain-lib/Onchain/…`, `src/exe/chain-sync/…`, `src/test/…`) — the list is in Step 3
- Test: `/tmp/verify-extraction.sh` (scratch, never committed)

**Interfaces:**
- Consumes: the bootstrapped repo; `$SRC` at a recorded `BELT_SHA`.
- Produces: `extraction-keep.txt`; `BELT_SHA` in the commit message (provenance for `NOTICE`).

- [ ] **Step 1: Write the failing test**

Write it to `/tmp/verify-extraction.sh` (scratch, not in the repo):

```bash
# every kept path must exist in the new repo and be byte-identical to the Belt System
SRC=/Users/mg/Projects/CardanoProjects/Decentralized-Belt-System
n=0; bad=0
while read -r rel; do
  n=$((n+1))
  diff -q "$SRC/$rel" "$rel" >/dev/null 2>&1 || { echo "DIFFERS/MISSING: $rel"; bad=$((bad+1)); }
done < extraction-keep.txt
echo "checked=$n differing=$bad"
test "$n" -eq 69 && test "$bad" -eq 0
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cd /Users/mg/Projects/CardanoProjects/palmares && bash /tmp/verify-extraction.sh`
Expected: FAIL — `extraction-keep.txt: No such file or directory`, `checked=0 differing=0`, exit 1.

- [ ] **Step 3: Write `extraction-keep.txt` (these 69 lines, in this order) and copy the files**

```
src/lib/webapi-lib/WebAPI/Auth.hs
src/lib/webapi-lib/WebAPI/CORS.hs
src/lib/webapi-lib/WebAPI/Errors.hs
src/lib/webapi-lib/WebAPI/ServiceProbe.hs
src/lib/webapi-lib/WebAPI/Utils.hs
src/lib/onchain-lib/Onchain/CIP68.hs
src/lib/onchain-lib/Onchain/Utils.hs
src/lib/onchain-lib/Onchain/Blueprint.hs
src/lib/onchain-lib/Onchain/Protocol.hs
src/lib/onchain-lib/Onchain/Protocol/Types.hs
src/lib/onchain-lib/Onchain/Protocol/Lookup.hs
src/lib/onchain-lib/Onchain/Protocol/Id.hs
src/lib/onchain-lib/Onchain/Validators/OracleNFTPolicy.hs
src/lib/onchain-lib/Onchain/Validators/OracleValidator.hs
src/lib/onchain-lib/Onchain/Validators/AchievementsValidator.hs
src/lib/onchain-lib/Onchain/Validators/ProfilesValidator.hs
src/lib/onchain-lib/Onchain/Validators/MintingPolicy.hs
src/lib/chainsync-lib/KupoClient.hs
src/lib/chainsync-lib/KupoAtlas.hs
src/lib/offchain-lib/Constants.hs
src/lib/offchain-lib/Utils.hs
src/lib/offchain-lib/Storage.hs
src/lib/offchain-lib/Ingestion.hs
src/lib/offchain-lib/DomainTypes/Core/Types.hs
src/lib/offchain-lib/DomainTypes/Core/Actions.hs
src/lib/offchain-lib/DomainTypes/Transfer/Types.hs
src/lib/offchain-lib/DomainTypes/Transfer/Filters.hs
src/lib/offchain-lib/DomainTypes/Transfer/OrderBy.hs
src/lib/offchain-lib/DomainTypes/Transfer/QueryResponses.hs
src/lib/offchain-lib/TxBuilding/Skeletons.hs
src/lib/offchain-lib/TxBuilding/Transactions.hs
src/lib/offchain-lib/TxBuilding/Context.hs
src/lib/offchain-lib/TxBuilding/Utils.hs
src/lib/offchain-lib/TxBuilding/Exceptions.hs
src/lib/offchain-lib/TxBuilding/Validators.hs
src/lib/offchain-lib/TxBuilding/Interactions.hs
src/lib/offchain-lib/TxBuilding/Lookups.hs
src/lib/offchain-lib/TxBuilding/Operations.hs
src/lib/offchain-lib/TxBuilding/Conversions.hs
src/exe/admin/Main.hs
src/exe/query-api/Main.hs
src/exe/query-api/QueryAppMonad.hs
src/exe/query-api/RestAPI.hs
src/exe/query-api/Query/Common.hs
src/exe/query-api/Query/Projected.hs
src/exe/query-api/Query/Aggregates.hs
src/exe/query-api/Query/Pages.hs
src/exe/query-api/Query/ServiceStatus.hs
src/exe/interaction-api/Main.hs
src/exe/interaction-api/InteractionAppMonad.hs
src/exe/interaction-api/RestAPI.hs
src/exe/interaction-api/ServiceHandlers.hs
src/exe/interaction-api/ServiceRequests.hs
src/exe/chain-sync/Main.hs
src/exe/chain-sync/ChainSyncLogic.hs
src/exe/chain-sync/ChainSyncServer.hs
src/exe/chain-sync/ChainsyncAPI.hs
src/test/Main.hs
src/test/UnitTests.hs
src/test/TestRuns.hs
src/test/Test/Helpers.hs
src/test/Test/Fixtures.hs
src/test/UnitTests/ApiHardening.hs
src/test/UnitTests/StorageRoundtrip.hs
src/test/UnitTests/ChainSyncReplay.hs
src/test/UnitTests/Conversions.hs
src/test/UnitTests/Achievement.hs
src/test/UnitTests/Oracle.hs
src/test/UnitTests/Cleanup.hs
```

```bash
SRC=/Users/mg/Projects/CardanoProjects/Decentralized-Belt-System
while read -r rel; do mkdir -p "$(dirname "$rel")" && cp "$SRC/$rel" "$rel"; done < extraction-keep.txt
```
Never `cp -r src` — the explicit list is what makes the `skip` column real (20 files / 5,590 lines of BJJ domain and
18 `mcp-server-lib` modules / 2,831 lines stay behind; `BJJPropertyTests.hs`, `UnitTests/{Membership,Promotion,MCPTools}.hs`,
`Query/{Live,Rules,ExplorerMonthly}.hs`, `RestAPI/Common.hs`, `IPFS.hs`, `WebAPI/ImageType.hs`, `Onchain/{LinkedList,BJJ}.hs`,
`Onchain/Protocol/Core.hs`, `Onchain/Validators/{Ranks,Memberships}Validator.hs`, `TxBuilding/SafeOnchainLogic.hs`,
`DomainTypes/Core/BJJ.hs`, `DomainTypes/Rules/Promotion.hs` are the absences a reviewer checks for).
Do not touch a single byte of the copies in this commit: module headers, `BJJ` identifiers and imports of skipped
modules all stay, so a reviewer can `diff` the commit to zero.

- [ ] **Step 4: Run test to verify it passes**

Run: `bash /tmp/verify-extraction.sh`
Expected: PASS — `checked=69 differing=0`, exit 0

- [ ] **Step 5: Commit**

```bash
BELT_SHA=$(git -C /Users/mg/Projects/CardanoProjects/Decentralized-Belt-System rev-parse HEAD)
git add extraction-keep.txt src/
git commit -m "feat: extract reusable libraries from Decentralized-Belt-System

Source: Decentralized-Belt-System @ ${BELT_SHA} (MIT, Copyright (c) 2025 Marius Georgescu).
Relicensed to Apache-2.0 by the copyright holder; the MIT notice is retained in
licenses/LICENSE.decentralized-belt-system.MIT per NOTICE.
File-by-file provenance and verdicts: docs/superpowers/design/f1-library-extraction.md §2.
Byte-identical copy; renames, strips and rewrites follow as separate commits."
```

---

### Task T-EXTRACTION-2: Rename commit (namespace, paths, binary names, project-named identifiers)

**Files:**
- Modify: all 69 copied `.hs` files (module header + import lines only)
- Modify: `palmares.cabal` (component names + `hs-source-dirs` + module lists to the `Palmares.*` spellings of T-EXTRACT §1.3)
- Rename: `src/lib/{onchain,offchain,chainsync,webapi}-lib/*` → `…/Palmares/*`; `src/lib/chainsync-lib/Palmares/Kupo*.hs` → `src/lib/chainsync-lib/Palmares/ChainSync/`; `src/exe/chain-sync/` → `src/exe/attest-sync/`; `src/lib/onchain-lib/Palmares/Onchain/Validators/AchievementsValidator.hs` → `AttestationsValidator.hs`; `src/test/UnitTests/Achievement.hs` → `src/test/UnitTests/Attestation.hs`; `src/test/BJJPropertyTests.hs` → not copied (skip-the-file, port-the-pattern — Plan B)
- Test: inline `grep` assertions (no committed script)

**Interfaces:**
- Consumes: Task T-EXTRACTION-1's tree.
- Produces: the `Palmares.*` namespace; executables `admin`, `query-api` (8083), `interaction-api` (8082), `attest-sync` (8084); identifiers `mkWebApp`, `runActionWithPK`, `deployValidators`, `profileInteraction`, `webApp`; log namespace `"Palmares"`.

- [ ] **Step 1: Write the failing test**

```bash
# 1. no library/exe module may still live in an old top-level namespace
grep -rn -E '^module (Onchain|WebAPI|TxBuilding|DomainTypes)\.|^module (Storage|Ingestion|Constants|Utils|KupoClient|KupoAtlas)\b' src/ --include='*.hs'
# 2. no import of an old top-level namespace
grep -rn -E '^import +(qualified +)?(Onchain|WebAPI|TxBuilding|DomainTypes)\.|^import +(qualified +)?(Storage|Ingestion|Constants|Utils|KupoClient|KupoAtlas)\b' src/ --include='*.hs'
# 3. no project-named identifier survives (T-EXTRACT §5.2 rule 1)
grep -rn -E '\b(mkBJJApp|runBJJActionWithPK|deployBJJValidators|bjjInteraction|bjjDApp|BJJDApp)\b' src/ --include='*.hs'
# 4. the old chain-sync exe dir is gone
test ! -d src/exe/chain-sync
# all four must print nothing / succeed
```

- [ ] **Step 2: Run test to verify it fails**

Run: the four checks above
Expected: FAIL — grep 1 prints **39 lines**, one per copied library module (`module Onchain.CIP68 where`,
`module Storage where`, …: webapi 5 + onchain 12 + chainsync 2 + offchain 20); grep 3 prints **99 lines**
(measured over the 69 kept files: `mkBJJApp` 8 lines, `runBJJActionWithPK` 18, `deployBJJValidators` 27,
`bjjInteraction` 44, `bjjDApp` 4, `BJJDApp` 2, minus the 4 lines matching two patterns at once —
`bjjDApp`/`BJJDApp` sit at `src/exe/interaction-api/Main.hs:31,43,50` and `src/exe/query-api/Main.hs:40,53,60`);
check 4 exits 1.

- [ ] **Step 3: Run the mechanical rename**

```bash
for lib in onchain-lib offchain-lib chainsync-lib webapi-lib; do
  mkdir -p "src/lib/$lib/Palmares"
  # CLAUDE.md must NOT move: onchain-lib/ and offchain-lib/ each carry one (bootstrap task, §3 item 5).
  (cd "src/lib/$lib" && for e in *; do
     case "$e" in Palmares|CLAUDE.md) continue;; esac
     git mv "$e" Palmares/
   done)
done
mkdir -p src/lib/chainsync-lib/Palmares/ChainSync
git mv src/lib/chainsync-lib/Palmares/KupoClient.hs src/lib/chainsync-lib/Palmares/ChainSync/KupoClient.hs
git mv src/lib/chainsync-lib/Palmares/KupoAtlas.hs  src/lib/chainsync-lib/Palmares/ChainSync/KupoAtlas.hs
git mv src/exe/chain-sync src/exe/attest-sync
git mv src/lib/onchain-lib/Palmares/Onchain/Validators/AchievementsValidator.hs \
       src/lib/onchain-lib/Palmares/Onchain/Validators/AttestationsValidator.hs
git mv src/test/UnitTests/Achievement.hs src/test/UnitTests/Attestation.hs

grep -rl . --include='*.hs' src | xargs sed -i '' -E \
  -e 's/^module (Onchain|WebAPI|TxBuilding|DomainTypes)\./module Palmares.\1./' \
  -e 's/^(import +(qualified +)?)(Onchain|WebAPI|TxBuilding|DomainTypes)\./\1Palmares.\3./' \
  -e 's/^module (Storage|Ingestion|Constants|Utils)\b/module Palmares.\1/' \
  -e 's/^(import +(qualified +)?)(Storage|Ingestion|Constants|Utils)\b/\1Palmares.\3/' \
  -e 's/\b(KupoClient|KupoAtlas)\b/Palmares.ChainSync.\1/g' \
  -e 's/\bAchievementsValidator\b/AttestationsValidator/g' \
  -e 's/\bmkBJJApp\b/mkWebApp/g' \
  -e 's/\brunBJJActionWithPK\b/runActionWithPK/g' \
  -e 's/\bdeployBJJValidators\b/deployValidators/g' \
  -e 's/\bbjjInteraction\b/profileInteraction/g' \
  -e 's/\bbjjDApp\b/webApp/g' \
  -e 's/"BJJDApp"/"Palmares"/g'
```
`BJJBelt` and the other BJJ *type* names are deliberately **not** renamed — their uses are deleted in Task T-EXTRACTION-3, not
carried forward. `sed -i ''` is the BSD/macOS form.

- [ ] **Step 4: Run test to verify it passes**

Run: the four Step-1 checks
Expected: PASS — all four print nothing and exit 0. (`cabal build all` is **not** expected to pass yet.)

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "refactor(extract): move to the Palmares.* namespace and rename components

Mechanical: module headers + imports, src/exe/chain-sync -> src/exe/attest-sync,
Achievements -> Attestations, mkBJJApp -> mkWebApp, runBJJActionWithPK -> runActionWithPK,
deployBJJValidators -> deployValidators, bjjInteraction -> profileInteraction.
No behaviour change; git show -M shows renames plus header lines only."
```

---

### Task T-EXTRACTION-3: Strip commit — GREEN GATE (`cabal build all` + `cabal test`)

**Files (29 kept files whose imports must change — measured, complete; this is every kept file that imports a
skipped module, and the line numbers are the Belt System's. Module names below are the pre-rename spellings;
after Task T-EXTRACTION-2 the library ones read `Palmares.<name>`):**
- Modify: `src/exe/admin/Main.hs:15` (`DomainTypes.Core.BJJ (BJJBelt (..), parseBelt)`)
- Modify: `src/exe/interaction-api/InteractionAppMonad.hs:31` (`IPFS (IPFSConfig)`) · `Main.hs:12` (`IPFS (initIPFSConfig)`) · `ServiceHandlers.hs:19,27` (`IPFS`, `WebAPI.ImageType`) · `ServiceRequests.hs:21` (`BJJBelt`)
- Modify: `src/exe/query-api/Query/Aggregates.hs:24,42,45` (`BJJBelt`, `Query.Live`, `RestAPI.Common`) · `Query/Common.hs:6` (`BJJBelt`) · `Query/Pages.hs:24,29,49,58` (`BJJBelt`, `Query.ExplorerMonthly`, `Query.Live`, `RestAPI.Common`) · `Query/Projected.hs:30,51` (`BJJBelt`, `Query.ExplorerMonthly`) · `RestAPI.hs:15,24,27,36` (`BJJBelt`, `Query.Live`, `Query.Rules`, `RestAPI.Common`)
- Modify: `src/lib/offchain-lib/Palmares/DomainTypes/Core/Actions.hs:15` · `Core/Types.hs:18` · `Transfer/Filters.hs:22` · `Transfer/QueryResponses.hs:60,62` (`BJJBelt`, `DomainTypes.Rules.Promotion (PromotionViolation)`)
- Modify: `src/lib/offchain-lib/Palmares/Ingestion.hs:13` (`Onchain.LinkedList (NodeDatum (..))`) · `Storage.hs:34` (`BJJBelt`) · `TxBuilding/Conversions.hs:21` (`SafeOnchainLogic (safeIntToBelt)`) · `TxBuilding/Lookups.hs:9,18,28` · `TxBuilding/Operations.hs:12,14,19,24,25,27,31` (`Onchain.BJJ`, `Onchain.LinkedList`, `MembershipsValidator`, `RanksValidator` ×2, the ranks/memberships accessors of `TxBuilding.Context`, `SafeOnchainLogic`) · `TxBuilding/Validators.hs:10,15` (`MembershipsValidator.membershipsCompile`, `RanksValidator.ranksCompile`)
- Modify: `src/lib/onchain-lib/Palmares/Onchain/Blueprint.hs:15,19` · `Onchain/Protocol.hs:14` (`Onchain.Protocol.Core`) · `Onchain/Protocol/Types.hs:8` (`Onchain.LinkedList (NodeDatum (..))`) · `Onchain/Validators/MintingPolicy.hs:25` (`Onchain.BJJ (BeltSnapshot (..), intToBelt, validatePromotion)`)
- Modify: `src/test/Main.hs:1` (`BJJPropertyTests`) · `TestRuns.hs:38` (`RanksValidator`) · `UnitTests.hs:12,13,15` (`UnitTests.MCPTools`, `.Membership`, `.Promotion`) · `UnitTests/Attestation.hs:17` (`Onchain.BJJ`) · `UnitTests/ApiHardening.hs:21` (`WebAPI.ImageType`)
- Modify: `src/lib/offchain-lib/Palmares/TxBuilding/Context.hs` (strip `DeployedScriptsContext` to minting policy / profiles / attestations / disputes / oracle + `oracleNFTAssetClass`, per T-EXTRACT §2.4) · `Palmares/Onchain/Protocol/Lookup.hs` (drop `unsafeGetRank`, `unsafeGetListNodeDatumAndValue`, `unsafeGetMembershipInterval`) · `Palmares/Constants.hs` · `Palmares/WebAPI/Utils.hs` · `src/exe/query-api/QueryAppMonad.hs` (drop `liveProjection`) · `Query/ServiceStatus.hs` · `src/test/UnitTests/Conversions.hs` (the one stripped test module Plan A builds; `Oracle.hs` and `Cleanup.hs` are **not** stripped here — they are out of `other-modules`, so nothing compiles them, and their strip is Plan B's when it puts them back)
- Modify: `palmares.cabal` — module lists to T-EXTRACT §1.3 exactly (`webapi-lib` gains `Palmares.WebAPI.CSV` + `cassava` per D-11; `query-api` has no `Query.Live`/`Query.Rules`/`Query.ExplorerMonthly`/`RestAPI.Common` and no `multiset`; test-suite `other-modules` = `Test.Helpers, UnitTests, UnitTests.ApiHardening, UnitTests.ChainSyncReplay, UnitTests.Conversions, UnitTests.StorageRoundtrip` — `Main` is `main-is`, not an other-module, and §1.3's `UnitTests.Profile` names no file in the manifest), test-suite `build-depends` loses `hedgehog`/`tasty-hedgehog` until Plan B's `PropertyTests` (`-Wunused-packages` fails the build otherwise)
- Test: `cabal build all`, `cabal test --test-show-details=direct`

**Interfaces:**
- Consumes: Task T-EXTRACTION-2's tree.
- Produces: a compiling four-library/four-executable package; `Palmares.WebAPI.{Auth,CORS,Errors,ServiceProbe,Utils,CSV}`; `Palmares.ChainSync.{KupoClient,KupoAtlas}`; `Palmares.Storage` (`OnchainMatchEvent`, `putKupoMatch`, `putMatchAndProjections`, `rollbackTo`, `replayOrder`, `runMigrations`, `readSchemaProbe`, `wipeChainSyncTablesRaw`, `chainSyncTableNames`, `upsertByUnique`, `currentSchemaVersion = 1`); `Palmares.TxBuilding.{Skeletons,Transactions,Context,Utils,Exceptions,Validators,Interactions}`; `Query/Common.hs`'s `normalizeLimitOffset` (default 100, clamp [1,500]), `normalizeOrder`, `applyLimits`, `applyFilterOrderLimit`; `Test.Helpers.{assert,queryOracle}`.

**Harness disposition** — Plan A builds: `Test/Helpers.hs` (as-is), `UnitTests/StorageRoundtrip.hs` (as-is, in-memory
SQLite `runSqlite ":memory:"`), `UnitTests/ApiHardening.hs` (minus the 4-case `imageTests` group + its import + its
list entry; 22 → 18 cases), `UnitTests/Conversions.hs`, `UnitTests/ChainSyncReplay.hs` (keep the 2 bare
`replayOrder` cases in `chainSyncReplayTests` and the 3-case `valueConversionTests`; drop `classifyTests` (2 cases)
and `deletionSequencingTests` (1 case) with their `promotionMatch`/`rankMatch` fixtures — they drive
`projectChainEvent` on `OnchainRank` datums at `ranksValidatorHashGY`; 8 → 5 cases).
**Plan B's**, in two groups. Copied by Task T-EXTRACTION-1 and tracked, but *not* in `other-modules` until rewritten
(no stub payloads typed twice): `TestRuns.hs` (the CLB `asUser … buildTxBody >>= signAndSubmitConfirmed` runners,
`deployReferenceScriptRun`, `mintTestOracleNFT`, `sendDustToValidator`), `Test/Fixtures.hs`,
`UnitTests/Attestation.hs`, `UnitTests/Oracle.hs`, `UnitTests/Cleanup.hs`. Created from nothing by Plan B (no Belt
System source, so not in `extraction-keep.txt`): `UnitTests/Dispute.hs` and `PropertyTests.hs` (the ~60-line
mirror-and-pin port:
one hedgehog generator over the new datum + one `property` asserting off-chain predicate ≡ on-chain predicate). The
`mkTestFor` emulator pattern itself is `atlas-cardano`'s and needs no extraction — only a `deployValidators` that
compiles, which is Plan B's.

**Rewrite set left stubbed by this commit (25 files / 10,614 source lines, T-EXTRACT §2.8) — one line each:**

| File (under `src/`) | The rewrite | Owner |
|---|---|---|
| `lib/onchain-lib/Palmares/Onchain/Protocol/Types.hs` | keep `ProtocolParams`/`FeeConfig`/`OracleParams`/`OracleAdminAction`/`applyOracleAdminAction` + the `makeIsDataSchemaIndexed` stable-index discipline; new attestation/profile/dispute payload types; fixes #1+#2 | Plan B |
| `lib/onchain-lib/Palmares/Onchain/Protocol/Id.hs` | keep the `blake2b_224` 28-byte-name derivation; new `deriveAttestationId`, `deriveDisputeId` | Plan B |
| `lib/onchain-lib/Palmares/Onchain/Protocol.hs` | re-export barrel; new list | Plan B |
| `lib/onchain-lib/Palmares/Onchain/Validators/MintingPolicy.hs` | keep `opPaused` gate + per-redeemer `checkFee` + exact-mint + seed-TxOutRef one-shot names; redeemers `ClaimProfile`/`IssueAttestation`/`OpenDispute`; fixes #1+#4 | Plan B |
| `lib/offchain-lib/Palmares/Storage.hs` | keep the machinery (`PersistField Integer`/`GYTime`, `upsertByUnique`, cursor, raw log, `replayOrder`, `rollbackTo`); 6 projection entities → 3; `currentSchemaVersion = 1`; fix #5 as a removal (Task T-EXTRACTION-6) | Plan B payload, Task T-EXTRACTION-6 removal |
| `lib/offchain-lib/Palmares/Ingestion.hs` | same address-dispatch shape, 3 branches; fix #3's single call site (Task T-EXTRACTION-5 guard) | Plan B |
| `lib/offchain-lib/Palmares/TxBuilding/Lookups.hs` | keep `getUTxOWithNFT`, `getUTxOWithTokenAtAddresses`, `queryOracleParams`; `getAllParsedDatumsAtValidator` + its 4 wrappers **deleted** (Task T-EXTRACTION-5) | Task T-EXTRACTION-5 + Plan B |
| `lib/offchain-lib/Palmares/TxBuilding/Operations.hs` | keep `getFeeSkeleton`, `getOracleFeeAndValiditySkeleton`, `applyAdminAction`, `adminActionToOracleAdminAction`; 6 BJJ ops → 5; fixes #1+#4 | Plan B |
| `lib/offchain-lib/Palmares/TxBuilding/Conversions.hs` | the onchain↔domain bridge; shape survives, types do not | Plan B |
| `lib/offchain-lib/Palmares/DomainTypes/Core/Types.hs` | new domain types; keep `deriving-aeson` StripPrefix+CamelToSnake + `ToSchema` conventions | Plan B |
| `lib/offchain-lib/Palmares/DomainTypes/Core/Actions.hs` | keep `AdminActionType` + the action-sum shape; add `SetAdminAction` (fix #2) | Plan B |
| `lib/offchain-lib/Palmares/DomainTypes/Transfer/Types.hs` | new transfer DTOs | Plan B |
| `lib/offchain-lib/Palmares/DomainTypes/Transfer/Filters.hs` | governance + attestation filters; keep the `*FromParams` convention | Plan A (API block) |
| `lib/offchain-lib/Palmares/DomainTypes/Transfer/OrderBy.hs` | governance sort enums; keep `SortOrder` + the enum/`FromHttpApiData`/`ToParamSchema` triple | Plan A (API block) |
| `lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs` | all new DTOs (`DRepSummary`, `GovActionSummary`, `VoteRecord`, `Page`, …) | Plan A (API block) |
| `exe/query-api/RestAPI.hs` | keep the `swagger :<|> probe :<|> public` composition + `Summary`/`Description` discipline; **no `BasicAuth` on read routes**; `Header "Accept"` CSV on the nine endpoints (D-11) | Plan A (API block) |
| `exe/query-api/Query/Projected.hs` | esqueleto over `palmares_attest` (profiles/attestations/disputes); `getActivityFeed` orders on `created_at_slot` (Task T-EXTRACTION-6 contract) | Plan B |
| `exe/query-api/Query/Aggregates.hs` | network dashboard aggregates | Plan A (API block) |
| `exe/query-api/Query/Pages.hs` | one-screen-one-request composites; keep the pattern | Plan A (API block) |
| `exe/interaction-api/ServiceRequests.hs` | keep the used-addresses + change-address + optional-collateral request shape exactly (the CIP-30 contract) | Plan B |
| `exe/admin/Main.hs` | keep the `Either ProviderCtx TxBuildingContext` split, `Command` + `hsubparser` + `executeCommand`, the shared parsers, `deploy-reference-scripts` + `write-blueprint`; new `rotate-admin`, `resolve-dispute`, `apply-indexes`, `reconcile-koios`, `write-metrics-doc` | Plan A (infra/parity blocks) + Plan B |
| `test/TestRuns.hs` | `deployValidators` over the new validator set; keep the runner + `malicious*` conventions | Plan B |
| `test/UnitTests/Attestation.hs` | two-phase pending→accept coverage incl. the double-accept negative; fix #4's cases | Plan B |
| `test/UnitTests/ChainSyncReplay.hs` | keep replay-order + idempotence + rollback shape; fixes #3+#5 cases | Tasks T-EXTRACTION-5–6 + Plan B |
| `test/Test/Fixtures.hs` | new fixtures | Plan B |

- [ ] **Step 1: Write the failing test**

Run: `cabal build all` (it is the test: the gate is "compiles with every skipped module gone").

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal build all`
Expected: FAIL with `Could not find module ‘Palmares.Onchain.LinkedList’` / `‘Palmares.DomainTypes.Core.BJJ’` /
`‘IPFS’` / `‘Query.Live’` — the library-namespace ones carry the `Palmares.` prefix Task T-EXTRACTION-2's `sed` put on
them, the executable-local ones (`IPFS`, `Query.Live`, `Query.Rules`, `Query.ExplorerMonthly`, `RestAPI.Common`) do
not, because exe modules are not namespaced. One error per row of the 29-file list above.

- [ ] **Step 3: Remove each skipped import and every declaration that uses it**

Rule for the pass: delete the import, then delete the smallest enclosing declaration that no longer type-checks;
never re-type a payload here — a rewrite-class file may end this commit as a module that exports less. Named
survivors (do not delete): `escapeLikePattern` + `getPortFromEnvOrDefault` (`WebAPI/Utils.hs`),
`normalizeLimitOffset` (`Query/Common.hs`), both error-handling shapes in `QueryAppMonad.hs`,
`txBuildingExceptionToHttpStatus` with its insufficient-funds→400 mapping (`TxBuilding/Exceptions.hs`),
`safeEraTime = 25920` + `txIsValidForSafeEra` (`TxBuilding/Skeletons.hs`), `defaultRollbackMargin = 2160`
(`Constants.hs`), the startup order in `src/exe/attest-sync/Main.hs` (schema probe → wipe → migrate → cursor → fork
probe → loop). In `WebAPI/Utils.hs`'s `addSharedSwaggerDescriptions`, replace the `BJJBelt`/`ProfileType` entries with
`GYPubKeyHash`, `DRepId`, `GovActionId`, `AttestationType`. In `src/test/UnitTests.hs` the tree becomes
`[apiHardeningTests, chainSyncReplayTests, conversionsTests, storageRoundtripTests]` and the group name drops "BJJ";
`src/test/Main.hs` becomes `defaultMain (testGroup "Tests" [unitTests])`.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal build all && cabal test --test-show-details=direct`
Expected: PASS — build exits 0; `cabal test` reports **31 cases in 4 groups** (ApiHardening 18, ChainSyncReplay 5,
Conversions 5, StorageRoundtrip 3), `All 31 tests passed`.

- [ ] **Step 5: Commit**

```bash
git add -A
git commit -m "refactor(extract): strip BJJ-specific members; green gate

Removes every import of a skipped module (29 files) and the declarations that used them,
reduces palmares.cabal to the F1 module set, and reduces the test tree to the 31 cases that
survive extraction. cabal build all + cabal test are green from here; the 25 rewrite files
carry no payload yet."
```

---

### Task T-EXTRACTION-4: `Palmares.DomainTypes.Core.Ids` — CIP-129 / CIP-105 identifiers

**Files:**
- Create: `src/lib/offchain-lib/Palmares/DomainTypes/Core/Ids.hs`
- Modify: `palmares.cabal` — `offchain-lib`: `exposed-modules` += `Palmares.DomainTypes.Core.Ids`, `build-depends` += `bech32`; test-suite: `other-modules` += `UnitTests.GovIds`, `build-depends` += `bech32, base16-bytestring` (the golden test decodes our own output and hex-decodes the fixture; both are already in `cabal.project.freeze` — `any.bech32 ==1.1.8`, `any.base16-bytestring ==1.0.2.0` — so neither is a new pin)
- Create: `src/test/UnitTests/GovIds.hs`
- Modify: `src/test/UnitTests.hs` (add `govIdsTests` to the tree)

**Interfaces:**
- Consumes: nothing from earlier tasks but Task T-EXTRACTION-3's green gate.
- Produces: `drepIdCip129 :: Bool -> ByteString -> Text` · `drepIdCip105 :: ByteString -> Text` · `ccHotIdCip129 :: Bool -> ByteString -> Text` · `ccColdIdCip129 :: Bool -> ByteString -> Text` · `govActionIdCip129 :: ByteString -> Word8 -> Text` · `parseDrepIdAny :: Text -> Either Text (Maybe Bool, ByteString)`.

We compute **every** governance bech32 ourselves: db-sync's `drep_hash.view` is **CIP-105** (`serialiseToBech32
"drep"` over the raw 28 bytes, same HRP for key and script, so it cannot be compared against Koios/GovTool/wallets),
`committee_hash` has **no `view` column at all** (only `id`, `raw`, `has_script`) so all four `cc_hot`/`cc_cold` forms
are ours, and `gov_action_proposal` has no bech32 id. Header byte = `(key-type << 4) | credential-type`:

| Identifier | HRP | header (key hash) | header (script hash) | payload |
|---|---|---|---|---|
| CC hot | `cc_hot` | `0x02` | `0x03` | 1 + 28 bytes |
| CC cold | `cc_cold` | `0x12` | `0x13` | 1 + 28 bytes |
| DRep (CIP-129) | `drep` | `0x22` | `0x23` | 1 + 28 bytes |
| DRep (CIP-105) | `drep` | *(none)* | *(none)* | 28 bytes |
| gov action | `gov_action` | *(none)* | *(none)* | 32-byte tx id ‖ 1-byte index = 33 bytes |

**(DEVIATION from T-DATA §1.3)** `parseDrepIdAny` returns `Maybe Bool`, not `Bool`: the CIP-105 and raw-hex forms do
**not** carry the script flag (HRP `drep` is used for both), so `Nothing` means "resolve `has_script` from
`drep_hash` by `raw`". Returning `False` there would silently mis-identify every script DRep at the id boundary that
the Koios reconciliation joins on. The two reserved views `drep_always_abstain` / `drep_always_no_confidence` are
**not** this module's business — they are literals handled in the DTO layer (T-API §7.2).

- [ ] **Step 1: Write the failing test**

```haskell
{-# LANGUAGE OverloadedStrings #-}

-- | Golden vectors for the CIP-129/CIP-105 identifier codec. One real preprod id read from Koios on
-- 2026-10-19 (T-METRIC §4); the rest are the same 28 bytes re-encoded per T-DATA §1.3's header table.
module UnitTests.GovIds (govIdsTests) where

import Codec.Binary.Bech32 qualified as Bech32
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as B16
import Data.Text (Text)
import Data.Text qualified as T
import Palmares.DomainTypes.Core.Ids
import Test.Tasty
import Test.Tasty.HUnit

-- | "Bitcows" preprod DRep, key credential (T-METRIC §4).
bitcowsRaw :: ByteString
bitcowsRaw = B16.decodeLenient "81378d3ea89b68f0fc09944b1f93f2feb851318f196f98ba4029d8a8"

bitcowsCip129 :: Text
bitcowsCip129 = "drep1y2qn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qmwj3xg"

-- | The other six renderings of the SAME 28 bytes. Derived from T-DATA §1.3's header table with an
-- independent BIP-173 reference implementation, so they pin header byte, HRP and 5-bit regrouping.
bitcowsCip129Script, bitcowsCip105, bitcowsCcHot, bitcowsCcHotScript, bitcowsCcCold, bitcowsCcColdScript :: Text
bitcowsCip129Script = "drep1ywqn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qmuzqx0"
bitcowsCip105 = "drep1symc604gnd50plqfj393lyljl6u9zvv0r9he3wjq98v2sz3vtkp"
bitcowsCcHot = "cc_hot1q2qn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qf6qm49"
bitcowsCcHotScript = "cc_hot1qwqn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qfgs24z"
bitcowsCcCold = "cc_cold1z2qn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qlx580v"
bitcowsCcColdScript = "cc_cold1zwqn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32ql5yk0t"

-- | 32-byte tx id placeholder for the gov-action shape test.
txid32 :: ByteString
txid32 = BS.replicate 32 0xab

-- | Decode one of our own ids back to (hrp, payload bytes).
dec :: Text -> (Text, ByteString)
dec t = case Bech32.decode t of
  Right (hrp, dp) | Just bs <- Bech32.dataPartToBytes dp -> (Bech32.humanReadablePartToText hrp, bs)
  _ -> error ("undecodable: " <> T.unpack t)

govIdsTests :: TestTree
govIdsTests =
  testGroup
    "CIP-129 / CIP-105 identifiers"
    [ testCase "Bitcows DRep encodes to the Koios CIP-129 id" $
        drepIdCip129 False bitcowsRaw @?= bitcowsCip129,
      testCase "DRep key credential: hrp drep, 29 bytes, header 0x22" $
        let (hrp, bs) = dec (drepIdCip129 False bitcowsRaw)
         in (hrp, BS.length bs, BS.head bs, BS.tail bs) @?= ("drep", 29, 0x22, bitcowsRaw),
      testCase "DRep script credential: header 0x23" $
        (drepIdCip129 True bitcowsRaw, BS.head (snd (dec (drepIdCip129 True bitcowsRaw))))
          @?= (bitcowsCip129Script, 0x23),
      testCase "CIP-105 DRep: hrp drep, 28 bytes, no header" $
        (drepIdCip105 bitcowsRaw, dec (drepIdCip105 bitcowsRaw)) @?= (bitcowsCip105, ("drep", bitcowsRaw)),
      testCase "cc_hot key credential: header 0x02" $
        let (hrp, bs) = dec (ccHotIdCip129 False bitcowsRaw)
         in (hrp, BS.head bs, ccHotIdCip129 False bitcowsRaw) @?= ("cc_hot", 0x02, bitcowsCcHot),
      testCase "cc_hot script credential: header 0x03" $
        (ccHotIdCip129 True bitcowsRaw, BS.head (snd (dec (ccHotIdCip129 True bitcowsRaw))))
          @?= (bitcowsCcHotScript, 0x03),
      testCase "cc_cold key credential: header 0x12" $
        let (hrp, bs) = dec (ccColdIdCip129 False bitcowsRaw)
         in (hrp, BS.head bs, ccColdIdCip129 False bitcowsRaw) @?= ("cc_cold", 0x12, bitcowsCcCold),
      testCase "cc_cold script credential: header 0x13" $
        (ccColdIdCip129 True bitcowsRaw, BS.head (snd (dec (ccColdIdCip129 True bitcowsRaw))))
          @?= (bitcowsCcColdScript, 0x13),
      testCase "gov action: hrp gov_action, 33 bytes = txid || index, no header" $
        dec (govActionIdCip129 txid32 0) @?= ("gov_action", txid32 <> BS.singleton 0x00),
      testCase "parseDrepIdAny round-trips all three forms and rejects garbage" $ do
        parseDrepIdAny (drepIdCip129 True bitcowsRaw) @?= Right (Just True, bitcowsRaw)
        parseDrepIdAny (drepIdCip129 False bitcowsRaw) @?= Right (Just False, bitcowsRaw)
        parseDrepIdAny (drepIdCip105 bitcowsRaw) @?= Right (Nothing, bitcowsRaw)
        parseDrepIdAny "81378d3ea89b68f0fc09944b1f93f2feb851318f196f98ba4029d8a8"
          @?= Right (Nothing, bitcowsRaw)
        parseDrepIdAny "drep_always_abstain" @?= Left "invalid_drep_id"
    ]
```
The first case is the one that catches a wrong header byte, a wrong HRP **and** a wrong 5-bit regrouping at once:
`drep1y2qn0rf…` is the live Koios rendering of exactly `0x22 ‖ 81378d3e…29d8a8` (T-METRIC §4). The other six
renderings of the same hash are exact-equality vectors computed from T-DATA §1.3's header table with an independent
BIP-173 implementation; `0x02`/`0x03`, `0x12`/`0x13` and `0x22`/`0x23` differ only below the top 5 bits, so each
key/script pair shares its first data char (`q`, `z`, `y`) and diverges only at the second (`2` vs `w`) — which is
exactly where a hand-rolled 5-bit regrouping goes wrong.
**(OPEN: only the `drep1y2qn0rf…` vector is a value read off Koios. T-DATA §1.3's live `cc_hot1qddl5lv…` /
`cc_cold1zds4k4q…` / `gov_action1qnergwfc…` observations are truncated AND belong to other credentials whose raw
hex the notes do not record, so they cannot be turned into vectors. To cross-check the committee forms against a
third party, the **owner or the data block** must supply one real preprod `committee_hash.raw` + its Koios
`cc_hot_id`/`cc_cold_id`; until then the six derived vectors stand as a self-consistency pin, not independent
confirmation.)**

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-show-details=direct`
Expected: FAIL at compile time with `Could not find module ‘Palmares.DomainTypes.Core.Ids’`.

- [ ] **Step 3: Implement the six exports in `src/lib/offchain-lib/Palmares/DomainTypes/Core/Ids.hs`**

Signatures exactly as in **Interfaces** above, Haddock on each. One helper, over the measured bech32-1.1.8 API
(`encodeLenient :: HumanReadablePart -> DataPart -> Text`,
`humanReadablePartFromText :: Text -> Either HumanReadablePartError HumanReadablePart`):

```haskell
encodeWith :: Text -> ByteString -> Text
encodeWith h bs =
  Bech32.encodeLenient
    (either (error "Palmares.DomainTypes.Core.Ids: literal HRP rejected") id (Bech32.humanReadablePartFromText h))
    (Bech32.dataPartFromBytes bs)
```

`encodeLenient`, not `encode`: the 90-char BIP-173 limit is irrelevant at 29/33 bytes and `encode`'s `Either` would
force a partial function on a total operation. The four HRPs are module-level literals, so the `error` is
unreachable by construction. Every exported encoder is then `encodeWith hrp (BS.cons header raw)`, and
`govActionIdCip129 txid ix = encodeWith "gov_action" (txid <> BS.singleton ix)`. `parseDrepIdAny` dispatches on the
decoded payload length: 29 → `(Just (BS.head == 0x23), BS.tail)` rejecting any other header byte, 28 →
`(Nothing, payload)`, 56-char hex → `(Nothing, decoded)`, anything else → `Left "invalid_drep_id"` (the error code
T-API §1.1 returns as 400).

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-show-details=direct`
Expected: PASS — 41 cases (31 from Task T-EXTRACTION-3 + the 10-case `CIP-129 / CIP-105 identifiers` group).

- [ ] **Step 5: Commit**

```bash
git add src/lib/offchain-lib/Palmares/DomainTypes/Core/Ids.hs src/test/UnitTests/GovIds.hs src/test/UnitTests.hs palmares.cabal
git commit -m "feat(ids): CIP-129 and CIP-105 governance identifiers with golden vectors

db-sync stores only the CIP-105 drep view and no committee bech32 at all, so every id the API
publishes and the Koios reconciliation joins on is computed here. Golden test pins the live
preprod Bitcows DRep id and the four header bytes."
```

---

### Task T-EXTRACTION-5: Inherited fix #3 — `authenticateStateUTxO`, and the unguarded scanner deleted

**Files:**
- Modify: `src/lib/offchain-lib/Palmares/TxBuilding/Utils.hs` (add `authenticateStateUTxO` next to `extractNFTAssetClass`)
- Modify: `src/lib/offchain-lib/Palmares/TxBuilding/Lookups.hs` (delete `getAllParsedDatumsAtValidator` and its four wrappers `getAllOnchainValidRanks`, `getAllOnchainProfiles`, `getAllMembershipDatums`, `getAllAchievements` — Belt System L134-138 and L141/154/353/468. Their five same-file dependents — `getAllPromotions` L143, `getAllRanks` L148, `getAllProfiles` L156, and the two membership-history readers at L366/L379 — are BJJ-typed and are already gone with Task T-EXTRACTION-3's strip; `cabal build all` names any that are not.)
- Test: `src/test/UnitTests/ChainSyncReplay.hs` (new `authenticationTests` group)

**Interfaces:**
- Consumes: `Palmares.ChainSync.KupoAtlas.toGYAssetClass`, `Palmares.TxBuilding.Utils.extractNFTAssetClass`.
- Produces: `authenticateStateUTxO :: GYMintingPolicyId -> GYAssetClass -> GYValue -> Bool`.

Fix #3 has **exactly one implementation site**, `Ingestion.projectChainEvent` (synthesis §4 / T-EXTRACT §4.3): the
second site, `getAllParsedDatumsAtValidator`, is dead code once `Query/Live.hs` is cut (every caller of its four
wrappers lived there), so it is **deleted, not guarded** — and nothing may reintroduce a whole-address-scan helper
without the guard. The call-site wiring is Plan B's, because `projectChainEvent`'s three branches are the attestation
payload: **Plan B's Ingestion rewrite is gated on** passing `amValue` on **every** branch, returning `NoEvent am`
unless `authenticateStateUTxO ourPolicy (idOf datum) amValue`, still writing the raw match to `onchain_match_event`,
and on the two negative cases of T-EXTRACT §4.3 ("a well-formed datum with no NFT in the value is not projected",
"a datum naming id X in a UTxO holding NFT Y is not projected") asserting projection count unchanged **and** raw-log
count +1.

- [ ] **Step 1: Write the failing test**

```haskell
-- in src/test/UnitTests/ChainSyncReplay.hs; add authenticationTests to the module's testGroup list
ac :: Text -> GYAssetClass
ac t = either (error "bad fixture") id (toGYAssetClass t)

-- 56 hex chars = the 28-byte script hash GYMintingPolicyId's IsString instance requires; 58 would `error`.
ourPolicy :: GYMintingPolicyId
ourPolicy = "c0ffee00000000000000000000000000000000000000000000000001"

acX, acY, acForeign :: GYAssetClass
acX = ac "c0ffee00000000000000000000000000000000000000000000000001.5801"  -- id X under our policy
acY = ac "c0ffee00000000000000000000000000000000000000000000000001.5902"  -- id Y under our policy
acForeign = ac "deadbeef000000000000000000000000000000000000000000000002.5801"  -- someone else's policy

authenticationTests :: TestTree
authenticationTests =
  testGroup
    "state-UTxO authentication (inherited fix #3)"
    [ testCase "a datum's own NFT, under our policy, authenticates" $
        authenticateStateUTxO ourPolicy acX (valueSingleton acX 1 <> valueFromLovelace 2_000_000) @?= True,
      testCase "no NFT in the value is not authentic (the free-dust forgery)" $
        authenticateStateUTxO ourPolicy acX (valueFromLovelace 2_000_000) @?= False,
      testCase "a datum naming id X in a UTxO holding NFT Y is not authentic" $
        authenticateStateUTxO ourPolicy acX (valueSingleton acY 1 <> valueFromLovelace 2_000_000) @?= False,
      testCase "the right token name under someone else's policy is not authentic" $
        authenticateStateUTxO ourPolicy acForeign (valueSingleton acForeign 1) @?= False,
      testCase "two copies of the NFT is not authentic" $
        authenticateStateUTxO ourPolicy acX (valueSingleton acX 2) @?= False,
      testCase "GYLovelace as the claimed id is never authentic" $
        authenticateStateUTxO ourPolicy GYLovelace (valueFromLovelace 2_000_000) @?= False
    ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-show-details=direct`
Expected: FAIL at compile time — `Variable not in scope: authenticateStateUTxO`.

- [ ] **Step 3: Implement `authenticateStateUTxO :: GYMintingPolicyId -> GYAssetClass -> GYValue -> Bool` in `Palmares/TxBuilding/Utils.hs`**

```haskell
-- | A protocol state UTxO is authentic iff its value carries exactly one of the NFT named by
-- (or derived from) its own datum, under OUR minting policy. Address + parseable datum is not
-- enough: anyone can pay a forged datum to a script address.
authenticateStateUTxO :: GYMintingPolicyId -> GYAssetClass -> GYValue -> Bool
authenticateStateUTxO ourPolicy ac v =
  case ac of
    GYToken pid _ -> pid == ourPolicy && valueAssetClass v ac == 1
    GYLovelace -> False
```
Then delete `getAllParsedDatumsAtValidator` and its four wrappers from `Lookups.hs`.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal build all && cabal test --test-show-details=direct && ! grep -rn "getAllParsedDatumsAtValidator\|getAllOnchainValidRanks\|getAllOnchainProfiles\|getAllMembershipDatums\|getAllAchievements\|getAllAuthenticStateAtValidator" src/`
Expected: PASS — build exits 0 (which is also what proves no caller of the five deleted names survives), 47 cases,
and the `grep` prints nothing (exit 1, so the `!` succeeds).

- [ ] **Step 5: Commit**

```bash
git add src/lib/offchain-lib/Palmares/TxBuilding/Utils.hs src/lib/offchain-lib/Palmares/TxBuilding/Lookups.hs src/test/UnitTests/ChainSyncReplay.hs
git commit -m "fix(ingestion): authenticate state UTxOs by their own NFT (inherited fix #3)

A parseable datum at a script address proves nothing: 2 ADA plus a hand-crafted inline datum
poisoned the projections for free. One guard in the shared helper; the unguarded
whole-address-scan helper is deleted with Query/Live.hs rather than fixed in four callers.
Wiring into projectChainEvent's branches lands with the Ingestion rewrite."
```

---

### Task T-EXTRACTION-6: Inherited fix #5 — chain time on read, no stored wall clock

**Files:**
- Modify: `src/lib/offchain-lib/Palmares/Storage.hs` (remove `getCurrentTime`; Belt System sites L29 import + L340, L358, L374, L397, L446, L464)
- Modify: `src/lib/offchain-lib/Palmares/TxBuilding/Utils.hs` (the two pure converters, next to the existing `pPOSIXTimeFromGYSlot`/`gySlotFromPOSIXTime`)
- Modify: `src/exe/query-api/QueryAppMonad.hs` (`requestSlotConfig`, next to `providerContext`)
- Test: `src/test/UnitTests/ChainSyncReplay.hs` (add a `chainTimeTests` group to the module's `testGroup` list)

**Interfaces:**
- Consumes: `QueryAppContext`'s `providerContext :: ProviderCtx` (`QueryAppMonad.hs:34`), `Palmares.TxBuilding.Context.runQuery :: ProviderCtx -> GYTxQueryMonadIO a -> IO a` (`Context.hs:90`), Atlas's `slotConfig`, `slotToBeginTimePure`, `enclosingSlotFromTimePure`, `simpleSlotConfig`.
- Produces: `slotToUTC :: GYSlotConfig -> Integer -> UTCTime` and `utcToSlot :: GYSlotConfig -> UTCTime -> Integer` in `Palmares.TxBuilding.Utils` · `requestSlotConfig :: QueryAppMonad GYSlotConfig` in `QueryAppMonad` · the contract "`created_at_slot` is the only time in a projection row".

The converters live in `offchain-lib`, **not** in `src/exe/query-api/Query/Common.hs`: a cabal test-suite cannot
import an executable's `other-modules`, so sited there they would be untestable. `requestSlotConfig` is the only
half that needs `ProviderCtx`, so it is the only half that stays in the executable.

The fix is a **removal plus a read-time conversion**, not a new column: `createdAtSlot` is already the chain's own
clock and is immune to replay, while `insertedAt` is re-stamped by every `rollbackTo` + replay, which silently
renumbers the whole activity feed. So: `insertedAt` is never serialized (it appears in no `QueryResponses` DTO and in
no `SELECT` list of a public endpoint), `slotToBeginTime` is **monadic** (`GYTxQueryMonad f => GYSlot -> f GYTime`,
Atlas `TxBuilder/Class.hs:556`) so it must never be called per row — fetch the slot config once per request
and map rows with the pure `slotToBeginTimePure`; translate a `since :: UTCTime` query param time→slot with
`enclosingSlotFromTimePure` **before** the SQL so the `created_at_slot` index is used.
**Plan B's gate on the Storage/Ingestion rewrite** is the replay-stability case of T-EXTRACT §4.5: project slots
[S1,S2,S3] → snapshot the API-shaped feed → `rollbackTo (S1-1)` → re-project → assert the snapshot is
**byte-identical**. That case needs a projection entity, which is Plan B's payload; step 3 alone would pass on a
half-fix that stamps a chain-derived `createdAt` at projection time, so it must not be dropped.
Atlas's pure API, read off the 0.14.1 interface (`GeniusYield/Types/SlotConfig.hs`): `slotToBeginTimePure ::
GYSlotConfig -> GYSlot -> GYTime`, `enclosingSlotFromTimePure :: GYSlotConfig -> GYTime -> Maybe GYSlot` (`Nothing`
only before system start), `simpleSlotConfig :: UTCTime -> NominalDiffTime -> GYSlotConfig`,
`unsafeSlotFromInteger :: Integer -> GYSlot`, and `GYTime` converts via `timeToPOSIX`/`timeFromPOSIX` only — there is
no `timeToUTC`/`timeFromUTC`.

- [ ] **Step 1: Write the failing test**

```bash
# 1. the wall clock must be gone from the projection writers
grep -c "getCurrentTime" src/lib/offchain-lib/Palmares/Storage.hs
# 2. insertedAt must be confined to Storage.hs's persist block: no DTO and no query module may name it
grep -rn "insertedAt" src/lib/offchain-lib/Palmares/DomainTypes/ src/exe/query-api/
```
```haskell
-- chainTimeTests, in src/test/UnitTests/ChainSyncReplay.hs
-- 1-second slots from the POSIX epoch, so the expected time IS the slot number.
testSlotConfig :: GYSlotConfig
testSlotConfig = simpleSlotConfig (posixSecondsToUTCTime 0) 1

chainTimeTests :: TestTree
chainTimeTests =
  testGroup
    "chain time on read (inherited fix #5)"
    [ testCase "slotToUTC is the slot's begin time, from the slot alone" $
        slotToUTC testSlotConfig 12345 @?= posixSecondsToUTCTime 12345,
      testCase "slotToUTC is strictly monotone in the slot" $
        compare (slotToUTC testSlotConfig 100) (slotToUTC testSlotConfig 101) @?= LT,
      testCase "utcToSlot . slotToUTC is the identity on slot boundaries" $
        utcToSlot testSlotConfig (slotToUTC testSlotConfig 777) @?= 777
    ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `grep -c "getCurrentTime" src/lib/offchain-lib/Palmares/Storage.hs && cabal test --test-show-details=direct`
Expected: FAIL — the `grep -c` prints **`7`** (the `Data.Time` import at Belt System L29 plus the six
`now <- liftIO getCurrentTime` lines in the `put*Projection` writers), and the Haskell cases fail to compile with
`Variable not in scope: slotToUTC`.

- [ ] **Step 3: Delete the wall clock and add the two converters**

In `Storage.hs`: drop `getCurrentTime` from the `Data.Time` import list and the `now <- liftIO getCurrentTime` line
plus the `insertedAt = now` field from each writer; the `insertedAt` **column stays** (ingest telemetry) and is filled
by the DB default — nothing reads it. In `Palmares/TxBuilding/Utils.hs`:

```haskell
-- | Chain time for a stored @created_at_slot@. Pure, so a handler maps a whole page with one slot config.
slotToUTC :: GYSlotConfig -> Integer -> UTCTime
slotToUTC cfg = posixSecondsToUTCTime . timeToPOSIX . slotToBeginTimePure cfg . unsafeSlotFromInteger

-- | A @since@ query param as a slot, so the comparison happens on the indexed column.
-- A time before the system start clamps to slot 0 — i.e. "no lower bound", which is what @since@ means.
utcToSlot :: GYSlotConfig -> UTCTime -> Integer
utcToSlot cfg =
  maybe 0 slotToInteger . enclosingSlotFromTimePure cfg . timeFromPOSIX . utcTimeToPOSIXSeconds
```

In `QueryAppMonad.hs`, one definition: `requestSlotConfig = asks providerContext >>= \ctx -> liftIO (runQuery ctx slotConfig)`.
The only `insertedAt` outside `Storage.hs`'s persist block is a stale comment, Belt System
`Query/Projected.hs:320` (`-- Filter by registration date (insertedAt >= cutoff)`); delete it if Task
T-EXTRACTION-3's strip left it standing, or Step 4's grep gate fails on a comment.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal build all && cabal test --test-show-details=direct && test "$(grep -c getCurrentTime src/lib/offchain-lib/Palmares/Storage.hs)" -eq 0 && ! grep -rqn "insertedAt" src/lib/offchain-lib/Palmares/DomainTypes/ src/exe/query-api/`
Expected: PASS — build exits 0, `All 50 tests passed`, the `grep -c` prints `0`, and the `insertedAt` grep prints
nothing (exit 1, so the `!` succeeds).

- [ ] **Step 5: Commit**

```bash
git add src/lib/offchain-lib/Palmares/Storage.hs src/lib/offchain-lib/Palmares/TxBuilding/Utils.hs src/exe/query-api/QueryAppMonad.hs src/test/UnitTests/ChainSyncReplay.hs
git commit -m "fix(storage): derive timestamps from chain slots, never from ingest time (inherited fix #5)

rollbackTo wipes and replays the projections, so a stored getCurrentTime re-stamped every row
and renumbered the activity feed on every re-sync. created_at_slot is the only time stored;
query-api converts once per request with the pure slot config."
```

---

## Produces

**Repo / build**
- `/Users/mg/Projects/CardanoProjects/palmares` with the `Palmares.*` namespace, package `palmares`, four libraries
  (`onchain-lib`, `chainsync-lib`, `offchain-lib`, `webapi-lib`) and four executables `admin`, `query-api` (8083),
  `interaction-api` (8082), `attest-sync` (8084).
- `extraction-keep.txt` (69 kept paths) — the drift guard between the manifest and the tree.
- Green `cabal build all` + `cabal test` from the strip commit onward (31 → 50 cases across this block).
- Rename footprint held to three commands (T-EXTRACT §5.2): one `sed` over `Palmares|palmares`, one `git mv` per
  `Palmares` namespace dir, one `git mv` of `palmares.cabal`; no identifier, filename, config key or URI path carries
  the project name (`PALMARES_` env prefix is the single exception).

**Modules later blocks consume**
- `Palmares.DomainTypes.Core.Ids`: `drepIdCip129 :: Bool -> ByteString -> Text`, `drepIdCip105 :: ByteString -> Text`,
  `ccHotIdCip129 :: Bool -> ByteString -> Text`, `ccColdIdCip129 :: Bool -> ByteString -> Text`,
  `govActionIdCip129 :: ByteString -> Word8 -> Text`, `parseDrepIdAny :: Text -> Either Text (Maybe Bool, ByteString)`.
- `Palmares.TxBuilding.Utils`: `authenticateStateUTxO :: GYMintingPolicyId -> GYAssetClass -> GYValue -> Bool`,
  `slotToUTC :: GYSlotConfig -> Integer -> UTCTime`, `utcToSlot :: GYSlotConfig -> UTCTime -> Integer`
  (+ `extractNFTAssetClass`, `getInlineDatumAndValue(OrThrow)`, `pPOSIXTimeFromGYSlot`, `gySlotFromPOSIXTime`,
  `txOutRefToV3Plutus`, `tnFromGYAssetClass`).
- `src/exe/query-api/QueryAppMonad.hs`: `requestSlotConfig :: QueryAppMonad GYSlotConfig`.
- `src/exe/query-api/Query/Common.hs`: `normalizeLimitOffset` (100, clamp [1,500]), `normalizeOrder`,
  `applyLimits`, `applyFilterOrderLimit` — unchanged by this block beyond the `beltIsMasterCapable` strip.
- `Palmares.Storage`: `OnchainMatchEvent`, `putKupoMatch`, `putMatchAndProjections`, `rollbackTo`, `replayOrder`,
  `runMigrations`, `readSchemaProbe`, `wipeChainSyncTablesRaw`, `chainSyncTableNames`, `upsertByUnique`,
  `PersistField Integer`/`GYTime`, `currentSchemaVersion = 1`; contract: `created_at_slot` is the only time in a row.
- `Palmares.WebAPI.{Auth,CORS,Errors,ServiceProbe,Utils}` as-is + `Palmares.WebAPI.CSV` slot (D-11, `cassava`).
- `Palmares.ChainSync.{KupoClient,KupoAtlas}` as-is (incl. `toGYAssetClass`'s separatorless fallback).
- `Palmares.TxBuilding.{Skeletons,Transactions,Context,Exceptions,Validators,Interactions}` (incl.
  `safeEraTime = 25920`, `txIsValidForSafeEra`, `txBuildingExceptionToHttpStatus`, `runActionWithPK`).
- Test harness: `Test.Helpers.{assert,queryOracle}`, `UnitTests.StorageRoundtrip` (in-memory SQLite),
  `UnitTests.ApiHardening`, `UnitTests.Conversions`, `UnitTests.ChainSyncReplay`, `UnitTests.GovIds`.
- `bech32` added to `offchain-lib`'s `build-depends`, `bech32` + `base16-bytestring` to the test-suite's
  (`any.bech32 ==1.1.8`, `any.base16-bytestring ==1.0.2.0`, both already in the freeze — no new pin).

**Handoffs**
- **Plan B**: fixes #1 (`feeFor`), #2 (`OracleSetAdmin` index 4), #4 (`checkTimestampInFiniteRange`) — all three touch
  the validators; the `projectChainEvent` wiring of `authenticateStateUTxO` with the two §4.3 negative cases; the
  §4.5 replay-stability case; `TestRuns.hs`, `Test/Fixtures.hs`, `UnitTests/{Attestation,Oracle,Cleanup,Dispute}.hs`,
  `PropertyTests.hs` (mirror-and-pin), and the attestation payload of the rewrite table in Task T-EXTRACTION-3.
- **Plan A API block**: `DomainTypes/Transfer/{Filters,OrderBy,QueryResponses}.hs`, `query-api/RestAPI.hs`,
  `Query/{Aggregates,Pages}.hs`, and the new `Query/{Gov,Views,Refresh,Metrics}.hs` + `Metrics/Catalog.hs`.

## Open items

- **(OPEN: of the seven `UnitTests/GovIds.hs` id literals, only `drep1y2qn0rf…` is a value read off Koios
  (T-METRIC §4); the other six are the same 28 bytes re-encoded from T-DATA §1.3's header table with an independent BIP-173
  implementation. T-DATA's live `cc_hot1qddl5lv…` / `cc_cold1zds4k4q…` / `gov_action1qnergwfc…` observations are
  truncated **and** belong to credentials whose raw hex the notes never record, so they cannot become vectors. To get
  third-party confirmation of the committee forms, the **owner or the data block** must supply one real preprod
  `committee_hash.raw` together with Koios's `cc_hot_id`/`cc_cold_id` for it.)** — Task T-EXTRACTION-4.
- **(OPEN: blocker B2 — the script-DRep profile claim is still unspecified (T-EXTRACT §7.5, synthesis §3 step 24).
  Key-DReps prove control with one extra signatory; script-DReps cannot sign, so `MintClaimProfile` needs a different
  proof (spend a UTxO at the DRep script address? require a script witness?). It is on-chain design, it blocks F1
  check 11, and it belongs to **Plan B** — it must be settled in Plan B's task list, not discovered at implementation
  time. Nothing in Plan A depends on it.)**
- **(OPEN: T-EXTRACT §2.5 and §4.5 place fix #5's read-side conversion in `Query/Aggregates.hs`; measured, the
  activity feed's `since`/`ORDER BY` on `insertedAt` is in `Query/Projected.hs:1331` (`getActivityFeed`), and
  `Aggregates.hs`/`Pages.hs` carry no `insertedAt` at all. The converters therefore live in
  `Palmares.TxBuilding.Utils` (library, so the test-suite can import them) and the per-row conversion lands wherever
  a projection row is serialized — **Plan B's** `getActivityFeed`, since the attestation projections are its payload.
  Nothing in Plan A's governance read path consumes `slotToUTC`: db-sync's own `block.time` is already a timestamp.)**
- **(OPEN: `parseDrepIdAny` deviates from T-DATA §1.3's `Either Text (Bool, ByteString)` to
  `Either Text (Maybe Bool, ByteString)` because CIP-105 and raw-hex ids do not carry the script flag. The DTO layer
  must therefore resolve `has_script` from `drep_hash` by `raw` for those two forms; note that `drep_hash` is
  `UNIQUE(raw, has_script)`, so a raw-only lookup can in principle match two rows. The deviation itself is **decided
  here**; what is open is the tie-break for that two-row case, and the **Plan A API block** owns it when it writes
  T-API §1.1's `{drep-id}` capture.)**
