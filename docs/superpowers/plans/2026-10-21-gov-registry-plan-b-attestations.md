# Plan B — the on-chain attestation protocol Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** On preprod, a governance actor claims a profile by proving control of a Cardano credential, issues and countersigns attestations about other actors and about earlier attestations, and contests one through a dispute — all four flows visible in the public API with tx-proof.

**Architecture:** Five PlutusTx V3 validators (minting policy, profiles, attestations, disputes, oracle) whose authority primitive is CIP-68 user-token possession rather than a signature list, so an organisation or committee acting through a script is a first-class issuer. Off-chain, the existing `Interaction → interactionToTxSkeleton → Operations → GYTxSkeleton → interactionToTxBody` pipeline gains the profile and dispute actions; `interaction-api` returns unsigned CBOR for a wallet to sign. `attest-sync` projects the resulting UTxOs from Kupo into `palmares_attest`, and `query-api` reads them beside the governance views in `palmares`.

**Tech Stack:** GHC 9.6.6 · PlutusTx/Plinth V3 (`target-version=1.1.0`) · Atlas 0.14.1 (pinned at `bf0d2d1`) · clb emulator via `GeniusYield.Test.Clb` · persistent 2.15 over PostgreSQL 17 · Kupo · servant · tasty + tasty-hunit (+ hedgehog, added by T-ATTEST-5)

**Spec:** `docs/superpowers/specs/2026-10-19-gov-registry-spec.md` — §2 (the claim rule), §4.3 (the attestation layer), §9 F1 checks 4 and 8–11. Hard ordering: `docs/superpowers/design/f1-synthesis.md` §3 Phase 4, steps 21–24.

**Plan A:** `docs/superpowers/plans/2026-10-19-gov-registry-plan-a-data-and-api.md` — the data substrate and read API, complete except its cluster tasks. Plan B consumes `palmares.treasury_withdrawal` (anchor resolution), `Palmares.DomainTypes.Core.Ids` (CIP-129), `Palmares.Admin.ApplySql` and the `palmares_attest` schema.

---

## Why this plan is bigger than "wire up the attestation layer"

Four code surveys of `palmares@71fa00f` established that the protocol **cannot run at all today**, and not because of stubs — everything compiles and there is no `undefined` anywhere on the path:

1. **No profile can be created.** `MintingRedeemer` has exactly one constructor, `NewAchievement`
   (`src/lib/onchain-lib/Palmares/Onchain/Validators/MintingPolicy.hs:70-78`). There is no
   `CreateProfileAction` off-chain (`DomainTypes/Core/Actions.hs:49-64`), no `createProfileTX`, and no
   route. All three existing user operations take a `ProfileRefAC` as *input*, and issuing an
   attestation requires the issuer's profile user NFT (`MintingPolicy.hs:160`) — so **all three are
   unreachable**. The removal left tracks: `fcProfileCreationFee` (`Protocol/Types.hs:47`),
   `MintingContext.ctxProfilesAddress` (set at `:108`, never read),
   `ProtocolParams.profilesValidatorScriptHash` (used only to compute that dead field), and the
   uncalled CIP-68 mint/burn pair (`TxBuilding/Skeletons.hs:161-166`).
2. **No identity claim exists, for either credential kind.** `grep -rni "credential|claim"` over
   `src/lib/onchain-lib` returns zero hits. `txInfoSignatories` appears **once** in the whole layer, for
   the oracle admin (`OracleValidator.hs:74`). `Utils.hasTxInAtAddressWithNFT` (`Utils.hs:92-93`) is the
   only helper shaped for a claimed-address check, and nothing calls it.
3. **Disputes never existed** — `git log --all -- '*DisputesValidator*'` is empty. What exists is an
   unreachable HTTP code `DisputeNotFound` (`WebAPI/Envelope.hs:138`) and one design sentence
   (`src/lib/onchain-lib/CLAUDE.md:48`).
4. **The blueprint publishes three defects.** Two are false capability claims — `Blueprint.hs:206`
   ("re-keyed without redeploying a validator") and `Protocol/Types.hs:59` ("can rotate itself") — and
   rotation is not merely unimplemented but *forbidden*: `OracleValidator.hs:75` requires the output
   datum to equal `applyOracleAdminAction action currentParams`, and that function
   (`Types.hs:88-92`) preserves `opAdminPkh` by construction. The third: the Attestations datum schema
   is `definitionRef @()` (`Blueprint.hs:189`) instead of `CIP68Datum OnchainAttestation`, and the type
   is absent from `contractDefinitions`.
5. **Our own off-chain code cannot build a script-issuer transaction**, though the blueprint advertises
   exactly that (`Blueprint.hs:60-62`). `txMustSpendFromAddress` hardcodes
   `gyTxInWitness = GYTxInWitnessKey` (`TxBuilding/Skeletons.hs:117-125`) and all three user operations
   route through it (`Operations.hs:77`, `:148`, `:303`).
6. **Zero emulator tests run.** `TestRuns`, `Test.Fixtures`, `UnitTests.Attestation`,
   `UnitTests.Cleanup`, `UnitTests.Oracle` are comment-prefixed out of the test-suite
   (`palmares.cabal:459-490`); `PropertyTests`, `UnitTests.Dispute`, `UnitTests.Profile` and
   `Query.Metrics` name **no file at all**. The live 263 tests contain no emulator test.
7. **Projection poisoning is still open.** `authenticateStateUTxO` is tested as a pure function
   (`UnitTests/ChainSyncReplay.hs:86-102`) and called by nothing; `projectChainEvent`
   (`Ingestion.hs:25-42`) dispatches on address alone, so a hand-crafted inline datum at a script
   address with 2 ADA projects as protocol state. The test's own comment says the wiring "lands with
   the Ingestion rewrite" (`ChainSyncReplay.hs:70-71`), which is T-ATTEST-8.

One finding from that survey was **fixed before this plan was written**, because it was a data-loss
hazard with a ten-line fix: attest-sync's unqualified `persistent` migrations landed in `palmares`, the
schema `sql/000_schema.sql` drops CASCADE on every query-api boot. Commit `71fa00f`, five tests, one of
which pins the old behaviour. This plan therefore assumes `runAttest` and `palmares_attest` exist.

## Global Constraints

Every task's requirements implicitly include these.

- **`onchain-lib` imports no off-chain dependency.** No Aeson, Servant, Swagger. PlutusTx types only.
- **`{-# INLINEABLE #-}` before the type signature** of every cross-module on-chain function.
- **Trace codes are short and unique per validator**: `traceIfFalse "M0" cond -- Description (M0)`. A new
  code never reuses a letter-number already in that module.
- **Output checks are index-based.** The redeemer carries the expected output index; no O(n) search.
  Continuing and newly created state outputs use the **min-value** variant
  (`checkTxOutAtIndexWithDatumMinValueAndAddress`) because the balancer may add lovelace. The
  exact-value variant stays unused.
- **`makeIsDataSchemaIndexed` indices are append-only.** Never reorder a constructor, never insert.
  Nothing is deployed yet (`MintingPolicy.hs:76-77`), and T-ATTEST-10 is the moment that freezes.
- **Validators are unparameterised except the minting policy.** Addresses come from the datum's
  `protocolParams`; validator hashes are never validator parameters.
- **Authority is token possession, not a signature list** — that is what makes a script a first-class
  issuer. `deriveUserFromRefAC` from `Palmares.Onchain.CIP68`.
- **One naming site per `FeeConfig` field**: `feeFor :: MintAction -> FeeConfig -> Integer` in
  `Protocol/Lookup.hs`, and `checkFee :: OracleParams -> MintAction -> [TxOut] -> Bool` takes the
  action, never a selector (inherited fix #1).
- **Any date carried in a datum is validated against the transaction's validity interval** (fix #4).
- **Every inherited fix gets a test that fails on the old behaviour** (`src/test/CLAUDE.md:34-37`).
- **No ExUnits for non-exploitable defence.** A per-transaction on-chain check whose attack has no
  exploit and whose workaround is free is rejected, and the rejection is documented in the validator's
  haddock so the next reader does not re-derive it.
- **No protocol fees as a business model** (spec §4.3). `FeeConfig` ships with every fee at zero on
  preprod. Anti-spam deposits are returned on both outcomes.
- Max 120 columns, 2-space indent. Module-level haddock required. `mk` smart constructors, `unsafe`
  prefix, `is`/`has`/`check` predicates. No redundant record field prefixes.
- **Errors**: `TxBuildingException`, mapped by `txBuildingExceptionToHttpStatus`. No raw strings.
- **JSON**: `deriving-aeson` with `StripPrefix` + `CamelToSnake`.
- `cabal build all` then `cabal test` before any task is considered done.
  `PALMARES_TEST_PG_CONN_STR` must be set for the DB tests.

## Review Focus

Five input classes the spec implies that no task's happy path exercises. Each line's test is placed in
the task that owns the code, named there in that task's own step style.

1. **A claim whose credential is a script that holds no UTxO at its own address.** The spec's rule is
   "prove you can spend as that credential"; a Plutus script that only votes has nothing to spend. The
   expected behaviour is a *clean refusal* with the workaround in the error, not a confusing failure in
   the balancer. → T-ATTEST-2 (on-chain refusal) and T-ATTEST-6 (`ScriptCredentialHasNoUtxo`).
2. **The same credential claimed twice, by different wallets.** Nothing in the design says a credential
   maps to one profile, and two profiles claiming one DRep id makes every "verified" badge meaningless.
   → T-ATTEST-2 pins the rule and the test.
3. **An attestation whose subject profile does not exist.** `MintingPolicy.hs:161` validates only that
   the subject asset class carries the protocol currency symbol, and nothing burns, so such a record is
   permanently pending and permanently unremovable. → T-ATTEST-2 (off-chain refusal) and T-ATTEST-9
   (the read layer marks an unresolvable subject rather than hiding it).
4. **A dispute opened against an attestation that is later disputed again.** The deposit is returned on
   both verdicts, so repeat disputes are free; a single attestation can carry an unbounded number.
   → T-ATTEST-4 bounds it (one open dispute per attestation at a time) and tests the second.
5. **A replay that must rebuild projections, not just raw rows.** `UnitTests/StorageRoundtrip.hs` calls
   `rollbackTo` but asserts only `OnchainMatchEvent` counts, and its fixture address is the literal
   `"addr"` (`:35`) which fails `addressFromTextMaybe`, so every match takes the conversion-error branch:
   **the projection replay is exercised only in its failure path.** → T-ATTEST-8.

---

## Phase 1 — the rename, and the debris it orphans

### Task T-ATTEST-1: the domain rename, and the three blueprint defects

**This must be first and it must be one commit.** Nothing is deployed, so every renamed constructor is
free; after T-ATTEST-10 each one is a validator-hash change and a coordinated redeploy. The layers
currently disagree on the noun — on-chain says attestation (`AttestationsValidator.hs`,
`ctxAttestationsAddress`, `attestationsValidatorScriptHash`) while off-chain, the DTOs, the routes and
the projection columns all say achievement — and that disagreement is what makes every later task
ambiguous.

**Files:**
- Modify: `src/lib/onchain-lib/Palmares/Onchain/Protocol/Types.hs`
- Modify: `src/lib/onchain-lib/Palmares/Onchain/Validators/AttestationsValidator.hs`
- Modify: `src/lib/onchain-lib/Palmares/Onchain/Validators/MintingPolicy.hs`
- Modify: `src/lib/onchain-lib/Palmares/Onchain/Blueprint.hs`
- Modify: `src/lib/offchain-lib/Palmares/DomainTypes/Core/Types.hs`, `Core/Actions.hs`,
  `Transfer/Types.hs`, `Transfer/QueryResponses.hs`, `Transfer/Filters.hs`, `Transfer/OrderBy.hs`
- Modify: `src/lib/offchain-lib/Palmares/TxBuilding/{Validators,Context,Operations,Lookups,Conversions,Exceptions}.hs`
- Modify: `src/lib/offchain-lib/Palmares/Storage.hs`
- Modify: `src/exe/interaction-api/{RestAPI,ServiceRequests,ServiceHandlers,InteractionAppMonad}.hs`
- Modify: `src/test/UnitTests/{GovDto,ChainSyncReplay,StorageRoundtrip}.hs`, `palmares.cabal`
- Test: `src/test/UnitTests/Rename.hs` (new — the grep guard)

**Interfaces:**
- Produces: `OnchainAttestation {attestationId, attestationIssuer, attestationSubject, attestationDate,
  attestationAccepted}`; `AttestationsRedeemer (AcceptAttestation Integer | Cleanup)`;
  `OnchainProfileKind (PersonDRep | Organization | Committee | Community | Vendor | Auditor)` at indices
  0–5; off-chain `Attestation`, `IssueAttestationAction`, `AcceptAttestationAction`,
  `attestationsValidator{Plutus,GY,HashGY,HashPlutus}`, `AttestationProjection`; the JSON field names
  `issuer_profile_id` / `subject_profile_id`; routes `/issue-attestation` and `/accept-attestation`.

- [ ] **Step 1: Write the failing test** — `src/test/UnitTests/Rename.hs`

A grep guard, because the rename's failure mode is *partial*: the compiler is happy with half of it.
The constants are the embedded sources, read with `file-embed` the way `UnitTests.ApplySql` reads the
SQL, so the test needs no working directory.

```haskell
tests = testGroup "the domain is attestation, everywhere"
  [ testCase "no on-chain or off-chain source says achievement" $
      bannedIn ["achievement", "Achievement"] librarySources @?= []
  , testCase "no source says practitioner, rank, belt, promotion, membership or dojo" $
      bannedIn ["practitioner", "Practitioner", "rank", "Rank", "belt", "Belt",
                "promotion", "Promotion", "membership", "Membership", "dojo"] librarySources @?= []
  , -- The two that reach a user rather than a reader.
    testCase "the interaction API's Swagger title names this product" $ do
      assertBool "no Belt System in the title" (not ("Belt System" `T.isInfixOf` interactionSwaggerTitle))
      assertBool "no Belt System in the query title" (not ("Belt System" `T.isInfixOf` querySwaggerTitle))
  , testCase "no error message mentions a belt number or a network we are not on" $ do
      assertBool "no belt number" (not ("belt number" `T.isInfixOf` allExceptionMessages))
      assertBool "no preview" (not ("preview" `T.isInfixOf` allInteractionErrorStrings))
  ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "the domain is attestation"'`
Expected: FAIL on all four. The first lists ~70 on-chain and ~40 off-chain `achievement` hits; the
second lists `Practitioner` (`Core/Types.hs:45`, `Protocol/Types.hs:111`) and the `practitioner*` locals
at `Operations.hs:300-303`, `AttestationsValidator.hs:81,93`; the third finds
`"Decentralized Belt System Interaction API"` (`RestAPI.hs:178`); the fourth finds
`"Invalid belt number (must be 0–14)"` (`Exceptions.hs:95`) and `"Not enough preview ADA"`
(`InteractionAppMonad.hs:108`, which also says "profile fee" for a path that cannot create profiles).

- [ ] **Step 3: Rename, and delete what the rename orphans**

The rename table is in **Plan B decisions §1** (below, "Decisions"). Three details that are not
mechanical:

- `OnchainProfileType = Practitioner | Organization` becomes `OnchainProfileKind` with **six**
  constructors at indices 0–5 in the spec's order: `PersonDRep`, `Organization`, `Committee`,
  `Community`, `Vendor`, `Auditor`. `Organization` keeps index 1 so the one constructor that survives
  keeps its index. `ProfileType` off-chain gains the same six and its `parseProfileType` strings become
  `person_drep | organization | committee | community | vendor | auditor` (snake_case, matching every
  other wire enum in this repo). **`profile_projection.profile_type` stores this text**
  (`Storage.hs:124` via `derivePersistFieldJSON`), so T-ATTEST-8 bumps `currentSchemaVersion` to 2 and
  the existing wipe-on-mismatch path handles it.
- `Query.Projected`, `Query.Aggregates` and `Query.Pages` are **renamed but stay commented out** of
  `query-api`'s `other-modules` (`palmares.cabal:334-347`). Every identifier they import still exists,
  so leaving them stale would mean a second rename in T-ATTEST-9. Rename now, land them there.
- `src/test/UnitTests/Attestation.hs:4` declares `module UnitTests.Achievement` and exports
  `achievementTests` (`:5`, `:22-23`) — a module-name/path mismatch that blocks un-commenting it. Fix
  the header here; the body is T-ATTEST-5's.

**Deleted, because this rename is what orphans them** — not unrelated cleanup:

| What | Where |
|---|---|
| 16 unreachable `TxBuildingException` constructors + the whole `AddMembershipIntervalReason` type, with their `displayException` arms and HTTP mappings | `TxBuilding/Exceptions.hs:15-20`, `:33-62`, `:77-99`, `:129-152` |
| The orphaned membership haddock block, documenting a deleted `UpdateEndDate` and a `validityOverride` parameter that no longer exists | `Operations.hs:174-182` |
| Three dead lookups: `hasUserNFTAt`, `getOrganizationInformation`, `getProfileRefACsAtAddress` | `Operations.hs:165-172`, `Lookups.hs:100-113`, `:172-194` |
| Two dead `Protocol/Id.hs` derivations: `deriveSeededId` (applies no CIP-67 prefix, so it is not interchangeable with what the minting policy builds) and `deriveIndexedId` (for the per-interval children that no longer exist) | `Protocol/Id.hs:31-39` |
| Dead `Protocol/Lookup.hs`: `unsafeGetProfile`, `unsafeGetProfileDatumAndValue`, `unsafeGetProtocolParamsFromProfileRefInput` | `Lookup.hs:26`, `:33`, `:43` |
| The unused exact-value helpers, since every validator uses the `>=` variant | `Utils.hs:70`, `:75` |
| `CIP68.updateCIP68DatumImage`, `deriveRefFromUserTN`, `deriveRefFromUserAC` | `CIP68.hs:135`, `:173-178` |

**Kept deliberately**, with a one-line haddock saying why: `Utils.hasTxInAtAddressWithNFT` (`Utils.hs:92`)
— T-ATTEST-2 is about to call it — and the CIP-68 mint/burn pair (`Skeletons.hs:150-166`), which
T-ATTEST-2 needs for profile creation.

Also fixed here, because the test demands it: the two user-facing strings, the interaction-api Swagger
title and version (`RestAPI.hs:178-179` → "Palmares Interaction API", `3.0.0`), and the Swagger
**description** at `ServiceRequests.hs:106` ("Must be called by the recipient practitioner's wallet").

And the one inconsistency worth fixing while in `CIP68.hs`: `deriveUserFromRefTN` slices with
`sliceByteString 4 (lengthOfByteString bs) bs` — an over-long count, harmless on the builtin but
inconsistent with its inverse at `:174`, which uses `lengthOfByteString bs - 4`.

- [ ] **Step 4: The three blueprint defects, in this same commit**

Two false capability claims and one wrong schema. They go here rather than in T-ATTEST-3 because
`write-blueprint` output is a published artifact and a blueprint that asserts a capability its
validator forbids is worse than the missing capability.

- `Blueprint.hs:206` — delete "re-keyed without redeploying a validator" and `:63-64`'s "admin key"
  from the oracle's capability list. T-ATTEST-3 puts them back **with** `OracleSetAdmin`.
- `Protocol/Types.hs:59` — `-- | Admin public key hash (can rotate itself)` is false. Replace with the
  truth and a forward reference: rotation arrives with `OracleSetAdmin` (T-ATTEST-3).
- `Blueprint.hs:189` — the Attestations datum is `definitionRef @()`. Make it
  `definitionRef @(CIP68Datum OnchainAttestation)` and add that type to `contractDefinitions`
  (`:39-48`) via `HasBlueprintDefinition`.

- [ ] **Step 5: Run the tests to verify they pass**

Run: `cabal build all && cabal test`
Expected: PASS. `UnitTests.Rename` green, and `admin write-blueprint -o /tmp/bp.json` followed by
`jq -e '.validators[] | select(.title | test("Attestation")) | .datum.schema.$ref' /tmp/bp.json`
returns a `CIP68Datum_OnchainAttestation` reference rather than `Unit`.

- [ ] **Step 6: Commit**

```bash
git add -A src/lib src/exe src/test palmares.cabal
git commit -m "refactor!: the domain is attestation — rename, delete the BJJ orphans, fix the blueprint

BREAKING: wire field names and the profile-kind enum change. Done now because nothing
is deployed; after deploy-reference-scripts each constructor is a hash change."
```

---

## Phase 2 — the on-chain protocol

> **T-ATTEST-1 as executed — three deviations from this task as written, each a builder decision.**
>
> 1. **The guard is `scripts/check_vocabulary.sh`, not `UnitTests/Rename.hs`.** Embedding ~30 Haskell
>    files into the test binary to grep them would mean listing each in `extra-source-files` and
>    re-embedding on every edit, to do what grep does. The repo already has a family of CI check
>    scripts (`check_swagger.sh`, `check_roles.sh`, `check_gitleaks_allowlist.sh`) and this belongs to
>    it. Eight checks, all green.
> 2. **The pre-existing dead code was NOT deleted** — only the BJJ-domain debris was. `CLAUDE.md` §3
>    says to mention unrelated dead code rather than remove it, and `Protocol/Id.hs`'s two
>    derivations, `Protocol/Lookup.hs`'s three `unsafeGet*`, `Utils.hs`'s two exact-value helpers,
>    `CIP68.hs`'s three unused functions and `Operations.hs`'s `hasUserNFTAt` are domain-neutral dead
>    code, not debris this rename orphans. **Mentioned, left in place.** What did go: the 16
>    unreachable `TxBuildingException` constructors and `AddMembershipIntervalReason` (they carried
>    user-facing strings like "Invalid belt number (must be 0–14)"), and the orphaned haddock block
>    documenting a deleted `UpdateEndDate` with a `validityOverride` parameter that no longer exists.
> 3. **`Query.Projected`, `Query.Aggregates` and `Query.Pages` were not renamed.** They are dormant
>    *and already broken against the live DTOs* — `Query/Projected.hs` reads
>    `searchResultsPractitioners`, which `QueryResponses.hs` no longer has. A mechanical rename would
>    not make them compile, so T-ATTEST-9 rebuilds them over `palmares_attest` instead. They are in
>    the guard's `DORMANT` exclusion list with that reason, and dropping a file from that list brings
>    it back under the guard with no other edit.
>
> **One defect the task did not anticipate, found by the rename and now guarded.** `deriving-aeson`'s
> `StripPrefix "aar"` is a *string*, not a reference to the record's fields. Renaming
> `AwardAchievementRequest`'s fields from `aar*` to `iar*` left the option naming a prefix that
> matched nothing — **and that compiles perfectly**. Every JSON key would have shipped as
> `iar_subject_profile_id` instead of `subject_profile_id`, on a live route, with no error anywhere.
> Worse, the prefix is written twice: once for `FieldLabelModifier` and once for
> `mkStripPrefixSchemaOptions`, which drives Swagger, so a half-fix publishes a document describing
> fields the API rejects. `check_vocabulary.sh` now asserts that every `StripPrefix` names a prefix
> at least one identifier in its own file uses — verified to fail when the bug is reintroduced.
>
> Also: `Committee` is our profile kind **and** `PlutusLedgerApi.V3.Committee`, the CIP-1694
> constitutional-committee datum. Three files import both layers and none uses the Plutus one, so the
> import hides it — which leaves a compiler error for anyone who later needs it, rather than a silent
> shadow.

### Task T-ATTEST-2: `MintProfile` and the identity claim — the piece synthesis §3 step 24 calls unspecified

The spec's rule is one sentence (§2, §4.3): **prove you can spend as that credential.** The full
reasoning, the rejected alternatives and the documented limitation are in **Decisions §3** below. The
implementation is a `Credential` in the datum and a two-arm check.

**Files:**
- Modify: `src/lib/onchain-lib/Palmares/Onchain/Protocol/Types.hs`
- Modify: `src/lib/onchain-lib/Palmares/Onchain/Validators/MintingPolicy.hs`
- Modify: `src/lib/onchain-lib/Palmares/Onchain/Validators/ProfilesValidator.hs`
- Modify: `src/lib/onchain-lib/Palmares/Onchain/Utils.hs`
- Test: `src/test/UnitTests/Claim.hs` (new — the pure-function tests)

**Interfaces:**
- Consumes: T-ATTEST-1's `OnchainProfileKind`; `Palmares.Onchain.CIP68.generateRefAndUserTN`;
  `Utils.nameFromTxOutRef`; `Utils.hasTxInAtAddressWithNFT`.
- Produces:
  - `OnchainProfile {profileId, profileKind, profileCredential :: Maybe Credential, protocolParams}` —
    `Maybe`, because a Community or an Auditor need not correspond to any on-chain credential, and
    forcing one would make the registry refuse the actors the spec names.
  - `checkCredentialControl :: Credential -> TxInfo -> Bool` in `Onchain/Utils.hs`, `INLINEABLE`.
  - `MintingRedeemer` gains `MintProfile TxOutRef OnchainProfileKind (Maybe Credential) MetadataFields Integer`
    at **index 1** (`NewAttestation` keeps 0).
  - `MintAction (MintActionProfile | MintActionAttestation | MintActionDispute)` — see T-ATTEST-3.

- [ ] **Step 1: Write the failing test** — `src/test/UnitTests/Claim.hs`

`checkCredentialControl` is a pure function of a `TxInfo`, so it is unit-testable without the emulator.
The emulator tests that drive a real claim transaction are T-ATTEST-5's; these pin the rule.

```haskell
tests = testGroup "credential control"
  [ testCase "a key credential is controlled when its hash is a signatory" $
      checkCredentialControl (PubKeyCredential pkhA) (txInfoWith [] [pkhA]) @?= True
  , testCase "a key credential is not controlled by another signatory" $
      checkCredentialControl (PubKeyCredential pkhA) (txInfoWith [] [pkhB]) @?= False
  , testCase "a script credential is controlled when an input's PAYMENT credential is that script" $
      checkCredentialControl (ScriptCredential shA) (txInfoWith [inputAtPayment shA] []) @?= True
  , testCase "a script credential is controlled when an input's STAKING credential is that script" $
      checkCredentialControl (ScriptCredential shA) (txInfoWith [inputAtStaking shA] []) @?= True
  , -- The limitation, asserted rather than left implicit: a signature does not stand in for a script.
    testCase "a script credential is NOT controlled by a signature" $
      checkCredentialControl (ScriptCredential shA) (txInfoWith [] [pkhA]) @?= False
  , testCase "a script credential is not controlled by an input at a DIFFERENT script" $
      checkCredentialControl (ScriptCredential shA) (txInfoWith [inputAtPayment shB] []) @?= False
  , -- Mixed arms must not leak into each other.
    testCase "a key credential is NOT controlled by an input at a script of the same bytes" $
      checkCredentialControl (PubKeyCredential pkhFromShA) (txInfoWith [inputAtPayment shA] []) @?= False
  , testCase "no inputs and no signatories controls nothing" $
      (checkCredentialControl (PubKeyCredential pkhA) (txInfoWith [] []),
       checkCredentialControl (ScriptCredential shA) (txInfoWith [] [])) @?= (False, False)
  ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "credential control"'`
Expected: FAIL to **compile** — `Variable not in scope: checkCredentialControl`. That is the honest
failure here; there is no prior behaviour to contradict.

- [ ] **Step 3: Implement `checkCredentialControl` and `MintProfile`**

`checkCredentialControl c txInfo` folds `txInfoInputs` once:

- `PubKeyCredential pkh` → `pkh ∈ txInfoSignatories txInfo`.
- `ScriptCredential sh` → some input's resolved `txOutAddress` has `addressCredential ==
  ScriptCredential sh`, **or** `addressStakingCredential == Just (StakingHash (ScriptCredential sh))`.

`handleMintProfile` in `MintingPolicy.hs`, with trace codes continuing the module's sequence
(`Mp1`…`Mp6`, none reusing an existing code):

| Code | Check |
|---|---|
| `Mp1` | the seed `TxOutRef` is spent (`Utils.nameFromTxOutRef` derives the token name from it) |
| `Mp2` | minted value is **exactly** the CIP-68 ref token **and** the CIP-68 user token, and burned is `mempty` |
| `Mp3` | `checkCredentialControl` for the claimed credential, when one is present. **Absent credential is permitted** (Community, Auditor) and is what `Mp3` short-circuits on |
| `Mp4` | metadata field sizes within `CIP68` limits |
| `Mp5` | output at the redeemer's index carries the `CIP68Datum OnchainProfile`, value `>=` ref NFT + oracle `minLv`, at the **Profiles** validator address — which is what finally reads `ctxProfilesAddress` (`MintingPolicy.hs:108`), dead until now |
| `Mp6` | `checkFee oracle MintActionProfile` (T-ATTEST-3) |

**Both CIP-68 tokens are minted here**, unlike `NewAttestation` which mints only the ref token and
discards the user half (`MintingPolicy.hs:144`). That asymmetry is correct and deliberate: a profile's
user token **is** the authority instrument — every other redeemer requires it in `valueSpent` — while an
attestation has no owner who acts on its behalf. Say so in the haddock, because the discarded `_` reads
like an oversight.

**Review Focus item 2 — one credential, one profile.** Nothing above stops two profiles claiming the
same DRep id, and two "verified" badges on one credential makes the badge meaningless. An on-chain
uniqueness check would need a global index UTxO, which is the contention anti-pattern this repo's own
security rules reject (rule 11). **Decision: uniqueness is a read-layer fact, not an on-chain one.** The
chain records every claim; `palmares_attest` has a unique index on the credential and T-ATTEST-9's
response marks a credential with more than one claimant as `contested` — showing all claimants with
their claim slots, oldest first — rather than picking a winner. Documented in the validator's haddock
and in Decisions §3.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cabal build all && cabal test --test-options='-p "credential control"'`
Expected: PASS, 8 tests. Plus `cabal test` green overall and `admin write-blueprint` listing
`MintProfile` at redeemer index 1.

- [ ] **Step 5: Commit**

```bash
git add src/lib/onchain-lib src/test/UnitTests/Claim.hs src/test/UnitTests.hs palmares.cabal
git commit -m "feat(onchain): MintProfile and the identity claim — control of a credential, key or script"
```

### Task T-ATTEST-3: inherited fixes #1, #2 and #4, and the double-accept guard

Four changes, one commit, because three of them touch `OracleParams`/`FeeConfig` and the fourth is the
same file's missing guard. Each gets a test that fails on the old behaviour.

**Files:**
- Modify: `src/lib/onchain-lib/Palmares/Onchain/Protocol/Types.hs`, `Protocol/Lookup.hs`
- Modify: `src/lib/onchain-lib/Palmares/Onchain/Validators/{MintingPolicy,AttestationsValidator,OracleValidator}.hs`
- Modify: `src/lib/onchain-lib/Palmares/Onchain/Blueprint.hs` (put back what T-ATTEST-1 deleted, now true)
- Modify: `src/lib/offchain-lib/Palmares/TxBuilding/Operations.hs`, `DomainTypes/Core/Actions.hs`
- Modify: `src/exe/admin/Main.hs`
- Test: `src/test/UnitTests/Fees.hs` (new), and `src/test/UnitTests/Claim.hs` extended

**Interfaces:**
- Produces: `MintAction`; `feeFor :: MintAction -> FeeConfig -> Integer`;
  `checkFee :: OracleParams -> MintAction -> [TxOut] -> Bool`;
  `OracleAdminAction` gains `OracleSetAdmin PubKeyHash` at **index 4**;
  `OracleParams` gains `opDisputeDeposit :: Integer`;
  off-chain `AdminActionType` gains `RotateAdminAction PubKeyHash`; `admin rotate-admin --pkh HEX`;
  `getFeeSkeleton :: ... => OracleParams -> MintAction -> m (GYTxSkeleton 'PlutusV3)` — **no selector**.

- [ ] **Step 1: Write the failing tests**

```haskell
-- UnitTests/Fees.hs — fix #1. The Belt System charged the wrong field because the selector was a
-- parameter; the property is that there is now exactly one function that names a field.
tests = testGroup "fees name one field in one place"
  [ testProperty "the off-chain fee equals feeFor for every MintAction" $
      property $ do
        act <- forAll genMintAction
        cfg <- forAll genFeeConfig
        offchainFeeLovelace act cfg === feeFor act cfg
  , testCase "feeFor maps each action to its own field, and no two actions share one" $
      map (`feeFor` distinctCfg) [MintActionProfile, MintActionAttestation, MintActionDispute]
        @?= [11, 22, 33]
  , -- The shape that caused the bug, now impossible: a selector cannot be passed.
    testCase "getFeeSkeleton takes a MintAction, not a FeeConfig selector" $
      assertBool "no (FeeConfig -> Integer) in the signature"
        (not ("FeeConfig -> Integer" `isInfixOf` operationsSourceSignatureOf "getFeeSkeleton"))
  ]
```

```haskell
-- UnitTests/Claim.hs, appended — fixes #2 and #4, and the accept guard.
, testCase "applyOracleAdminAction rotates the admin, which it could not before (fix #2)" $
    opAdminPkh (applyOracleAdminAction (OracleSetAdmin pkhB) (paramsWithAdmin pkhA)) @?= pkhB
, testCase "rotation changes nothing else in OracleParams" $
    (applyOracleAdminAction (OracleSetAdmin pkhB) (paramsWithAdmin pkhA)) { opAdminPkh = pkhA }
      @?= paramsWithAdmin pkhA
, testCase "OracleSetAdmin is at index 4 — appended, never inserted (fix #2)" $
    builtinDataIndexOf (OracleSetAdmin pkhA) @?= 4
, -- fix #4: the date must be INSIDE the interval, not merely before its end.
  testCase "a date inside the validity interval is accepted" $
    checkDateInRange (t 100) (interval (t 90) (t 110)) @?= True
, testCase "a back-dated attestation is rejected (the old check allowed any past date)" $
    checkDateInRange (t 10) (interval (t 90) (t 110)) @?= False
, testCase "a future-dated attestation is rejected" $
    checkDateInRange (t 200) (interval (t 90) (t 110)) @?= False
, testCase "an unbounded lower bound is rejected, so NegInf cannot stand in for 'any past'" $
    checkDateInRange (t 100) (from (t 110)) @?= False
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cabal test --test-options='-p "fees name one field" -p "credential control"'`
Expected: FAIL. `feeFor`/`MintAction`/`OracleSetAdmin`/`checkDateInRange` are not in scope; and once
they are stubbed, `"a back-dated attestation is rejected"` fails against today's
`achievementDate \`before\` txInfoValidRange` (`MintingPolicy.hs:158`), which accepts an attestation
dated 2015.

- [ ] **Step 3: Implement all four**

**Fix #1.** `MintAction` with one constructor per minting redeemer. `feeFor` is the only function in the
codebase that names a `FeeConfig` field — enforced by `UnitTests.Fees`' third case, which greps the
signature. `getFeeSkeleton` loses its `(FeeConfig -> Integer)` argument
(`TxBuilding/Operations.hs:42-53`), and `getOracleFeeAndValiditySkeleton` (`:58-67`) stops passing one
through.

**Fix #2.** `OracleAdminAction` gains `OracleSetAdmin PubKeyHash` at index 4, appended after 0–3
(`Types.hs:83`). `applyOracleAdminAction` (`Types.hs:88-92`) gains the arm. **`OracleValidator` needs no
change** — `O1` already requires the *current* admin's signature and `O2` already requires the output to
equal `applyOracleAdminAction`, so rotation becomes possible purely by making that function able to
express it. The cheapest possible fix, and it is cheap because the validator was written correctly.
Off-chain: `AdminActionType` gains `RotateAdminAction PubKeyHash`, `admin rotate-admin` appears, and —
unlike `SetFeesAction` — it stays **out** of the HTTP `FromJSON` (`Core/Actions.hs:116` already rejects
admin actions it does not want over the wire). Blueprint: put back the capability line T-ATTEST-1
deleted, now true.

**Fix #4.** `checkDateInRange :: POSIXTime -> POSIXInterval -> Bool` in `Onchain/Utils.hs`, requiring the
date to be **contained in** the interval and the interval to have a finite lower bound. `MintingPolicy`'s
`Ml` becomes that call. Off-chain, `issueAttestationTX` **derives** the date from `slotOfCurrentBlock`
rather than accepting it: `aa_attestation_date` leaves the request type entirely, which removes the
unvalidated client input the survey found (`ServiceRequests.hs:69` → `Operations.hs:241`, `:256`, with no
bound of any kind). The self-reported event date, if the owner wants one, is Decisions §7's open item and
would live in metadata.

**The accept guard.** `AttestationsValidator`'s `AcceptAttestation` gains
`traceIfFalse "A3" (not (attestationAccepted spentAttestation))`. One boolean read of a datum already
parsed. This is **not** the non-exploitable-defence case: the off-chain guard
(`AttestationAlreadyAccepted`, `Operations.hs:286-341`) is bypassable by anyone building their own
transaction, and the result is a permanent record whose acceptance is whatever the last accepter chose.
`Operations.hs:305-310` already says this half is Plan B's.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cabal build all && cabal test`
Expected: PASS. Then confirm the published artifact agrees:
`admin write-blueprint -o /tmp/bp.json && jq -e '[.definitions | keys[] | select(test("OracleAdminAction"))] | length > 0' /tmp/bp.json`,
and the oracle's capability list in the blueprint now includes the admin key.

- [ ] **Step 5: Commit**

```bash
git add src/lib src/exe/admin src/test palmares.cabal
git commit -m "fix(onchain): inherited fixes #1, #2, #4 and the on-chain double-accept guard"
```

### Task T-ATTEST-4: the disputes validator

Built to `src/lib/onchain-lib/CLAUDE.md:48` — "opening a dispute requires the disputer's User NFT plus
the deposit" — and spec §4.3: verdict from the oracle admin in v1, deposit returned, and the dispute
**never hides** the attestation.

**`ProtocolParams` gains `disputesValidatorScriptHash`, which changes the minting policy's parameter and
therefore its hash.** That is why disputes land before T-ATTEST-10 and not in a later increment.

**Files:**
- Create: `src/lib/onchain-lib/Palmares/Onchain/Validators/DisputesValidator.hs`
- Modify: `src/lib/onchain-lib/Palmares/Onchain/Protocol/Types.hs`, `Protocol.hs`, `Protocol/Lookup.hs`
- Modify: `src/lib/onchain-lib/Palmares/Onchain/Validators/MintingPolicy.hs`, `Blueprint.hs`
- Modify: `src/lib/offchain-lib/Palmares/TxBuilding/{Validators,Context}.hs`
- Modify: `palmares.cabal` (un-comment `Palmares.Onchain.Validators.DisputesValidator`, line 93)
- Test: `src/test/UnitTests/Dispute.hs` (new — the pure parts; emulator cases in T-ATTEST-5)

**Interfaces:**
- Produces: `OnchainDispute {disputeId, disputeTarget :: AttestationId, disputeOpenedBy :: ProfileId,
  disputeOpenedAt :: POSIXTime, disputeVerdict :: Maybe Verdict}`;
  `Verdict (VerdictUpheld | VerdictRejected)` at indices 0–1;
  `DisputesRedeemer (ResolveDispute Verdict Integer | Cleanup)`;
  `MintingRedeemer` gains `OpenDispute AttestationId ProfileId MetadataFields Integer` at **index 2**;
  `ProtocolParams.disputesValidatorScriptHash`; `OracleParams.opDisputeDeposit`.

- [ ] **Step 1: Write the failing test** — `src/test/UnitTests/Dispute.hs`

```haskell
tests = testGroup "disputes"
  [ testCase "a dispute's id is derived from its seed, so two disputes never collide" $
      deriveDisputeId seedA cs /= deriveDisputeId seedB cs @?= True
  , testCase "an unresolved dispute carries no verdict" $
      disputeVerdict (freshDispute targetA openerA) @?= Nothing
  , testCase "resolving writes the verdict and changes nothing else" $
      resolveDispute VerdictUpheld (freshDispute targetA openerA)
        @?= (freshDispute targetA openerA) { disputeVerdict = Just VerdictUpheld }
  , testCase "Verdict indices are 0 and 1, and the enum is closed" $
      map builtinDataIndexOf [VerdictUpheld, VerdictRejected] @?= [0, 1]
  , -- Review Focus item 4: repeat disputes are free, because the deposit comes back.
    testCase "a second dispute against the same attestation is refused while one is open" $
      checkNoOpenDispute targetA [freshDispute targetA openerB] @?= False
  , testCase "a second dispute is allowed once the first is resolved" $
      checkNoOpenDispute targetA [resolveDispute VerdictRejected (freshDispute targetA openerB)] @?= True
  , testCase "the deposit is returned on BOTH verdicts, so losing a dispute costs only fees" $
      map (depositReturnedFor . (`resolveDispute` freshDispute targetA openerA))
          [VerdictUpheld, VerdictRejected] @?= [True, True]
  ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "disputes"'`
Expected: FAIL to compile — the module does not exist.

- [ ] **Step 3: Write the validator and the two redeemers**

`DisputesValidator`, unparameterised, address from the datum's `protocolParams`, trace codes `D0`–`D4`:

- `ResolveDispute Verdict Integer` → `D1` the oracle's `opAdminPkh` signed, read from the oracle
  **reference input** exactly as `MintingPolicy` already does (`:101`, `K1` on absence); `D2` the output
  at the index carries the datum with `disputeVerdict = Just v`, value `>=` own value, same address;
  `D3` the dispute was unresolved (`disputeVerdict == Nothing`) — the same shape as T-ATTEST-3's accept
  guard and for the same reason; `D4` the deposit is paid to the disputer's address.
- `Cleanup` → the house pattern: succeeds if the datum is absent or unparseable, `traceError "D0"` if it
  parses.
- Non-spending purpose → `traceError "D5"`.

`handleOpenDispute` in `MintingPolicy.hs`, codes `Md1`–`Md5`: seed spent; mint exactly the dispute ref
token and burn nothing; the **disputer's** profile user NFT in `valueSpent`; the target attestation's
asset class carries the protocol currency symbol; output at the index at the Disputes validator address
with value `>=` ref NFT + `opMinUTxOValue` + **`opDisputeDeposit`**; `checkFee oracle MintActionDispute`.

**Review Focus item 4, and the one place this plan accepts an on-chain cost.** "One open dispute per
attestation at a time" cannot be checked on-chain without an index UTxO, so it is **not** checked
on-chain. `checkNoOpenDispute` is an off-chain predicate (T-ATTEST-6) over the projections, and the
read layer shows every dispute a target has. The on-chain property is weaker and sufficient: each
dispute is its own UTxO with its own deposit, so N disputes cost N deposits, and the deposit is what
bounds the spam. Say exactly this in the validator's haddock — the off-chain check is a courtesy, not a
guarantee, and T-ATTEST-9's response must not present it as one.

**Nothing in `AttestationsValidator` learns about disputes.** A dispute is a separate UTxO pointing at
an attestation; the attestation path's ExUnits are unchanged, and the spec's "displayed next to the
attestation without hiding it" is a read-layer property.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cabal build all && cabal test --test-options='-p "disputes"'`
Expected: PASS, 7 tests. And `admin write-blueprint` lists five validators with `OpenDispute` at
redeemer index 2; `scripts/test_exunits.sh` reports a script size for `DisputesValidator`.

- [ ] **Step 5: Commit**

```bash
git add src/lib src/test palmares.cabal
git commit -m "feat(onchain): the disputes validator — deposit in, verdict from the oracle, deposit back"
```

### Task T-ATTEST-5: the emulator suite, re-landed and extended — F1 check 8

Today **zero** emulator tests run. Five dormant modules exist on disk and three cabal lines name no
file at all. This task is the single largest test increment in the project and it is what F1 check 8
asks for: "suită de emulator pe pattern-ul `mkTestFor` cu acoperire pe căile negative, inclusiv un claim
de script-credential reușit și unul respins".

**No new dependency is needed for the emulator** — `GeniusYield.Test.Clb` and `GeniusYield.Test.Utils`
are exposed modules of `atlas-cardano`, which is already a test-suite dependency (`palmares.cabal:530`),
and `clb` reaches us transitively. `PropertyTests` needs two: `hedgehog` and `tasty-hedgehog` (both
already pinned in `cabal.project.freeze:299-301,597`, but a freeze entry is not a declared dependency).

**Files:**
- Modify: `src/test/TestRuns.hs` (479 lines; `deployValidators` deploys **five** validators into a
  **seven**-field context against a live five-field one — trim to {profiles, attestations, disputes,
  oracle} + minting policy, which deletes the 115-line promotion/rank block at `:365-479`)
- Modify: `src/test/Test/Fixtures.hs` (136 lines; `ProfileData` is unchanged, so this is pure content —
  keep the three byte-boundary fixtures at `:116-136` verbatim, they are pinned against `Onchain.CIP68`)
- Modify: `src/test/UnitTests/Attestation.hs` (544 lines, 9 tests, 3 negative)
- Modify: `src/test/UnitTests/Oracle.hs` (286 lines, 9 tests, 3 fully negative + 1 mixed)
- Modify: `src/test/UnitTests/Cleanup.hs` (175 lines, 5 tests, **0 negative**)
- Create: `src/test/UnitTests/Profile.hs`, `src/test/PropertyTests.hs`
- Modify: `src/test/UnitTests/Dispute.hs` (emulator cases appended to T-ATTEST-4's pure ones)
- Modify: `src/test/UnitTests.hs`, `palmares.cabal`

**Interfaces:**
- Consumes: `mkTestFor`, `GYTxMonadClb`, `mustFail`, `sendSkeleton'`, `withWalletBalancesCheck`,
  `TestInfo`/`Wallets`/`w1`…`w3`, `waitNSlots_`; `Test.Helpers.assert`/`queryOracle` (live in
  `other-modules` at `palmares.cabal:466` but imported by nothing live — it becomes live again here).
- Produces: `deployValidators` over the five-validator context; `profileInteraction`,
  `attestationInteraction`, `disputeInteraction`, `adminInteraction`; the malicious builders
  `maliciousMintOracleNFTWithExtraToken`, `maliciousMintOracleNFTWrongName`,
  `maliciousMintProfileWithBurn`, `maliciousClaimWithoutControl`, `maliciousDoubleAccept`.

- [ ] **Step 1: Write the failing tests**

Six groups. The **new** cases, named, are what this step adds; the re-landed ones keep their numbering
so a reader can match them to the originals.

`UnitTests.Profile` (new, 8 cases — 5 of them negative, which is where the old suite was thinnest):
```
"Test Case 1.1: a key credential claims a profile with an extra signatory"
"Test Case 1.2: a script credential claims a profile by spending a UTxO at its own address"
"Test Case 1.3: a script credential claims by spending a UTxO whose STAKING credential is the script"
"Test Case 1.4: a profile with no credential (Community) mints without a claim"
"Test Case 1.5: a claim without the signatory FAILS (Mp3)"
"Test Case 1.6: a claim naming a script whose UTxO is not spent FAILS (Mp3)"
"Test Case 1.7: a claim whose minted value carries an extra token FAILS (Mp2)"
"Test Case 1.8: a claim that burns a token alongside the mint FAILS (Mp2)"
"Test Case 1.9: the same credential claimed by two wallets both succeed — uniqueness is a read fact"
```
That last one is **Review Focus item 2**, and it asserts the documented behaviour rather than a wish: it
must pass, and T-ATTEST-9's test asserts the API marks the credential `contested`.

`UnitTests.Attestation` (re-landed, 9 → 13). The three existing negatives stay; the new cases:
```
"Test Case 5.4: Double-accept attestation fails on-chain (A3)"   -- was 'TA', a code that no longer exists
"Test Case 5.10: an attestation whose subject profile does not exist mints, and is unacceptable forever"
"Test Case 5.11: an attestation about another ATTESTATION (lineage) succeeds"
"Test Case 5.12: a back-dated attestation FAILS (Ml, fix #4)"
"Test Case 5.13: an attestation issued with someone else's user NFT FAILS (Mf)"
```
5.4's label previously named on-chain trace `"TA"`; `AttestationsValidator` now traces `A0`–`A3`, and the
guard is T-ATTEST-3's `A3`, so the rewritten test fails at a different layer than the old name claimed.
5.10 is **Review Focus item 3** and documents the permanent-pending record.

`UnitTests.Dispute` (emulator cases, 6):
```
"Test Case 6.1: open a dispute against an accepted attestation, deposit locked"
"Test Case 6.2: the oracle admin resolves it upheld, deposit returned"
"Test Case 6.3: the oracle admin resolves it rejected, deposit returned"
"Test Case 6.4: a non-admin resolution FAILS (D1)"
"Test Case 6.5: resolving an already-resolved dispute FAILS (D3)"
"Test Case 6.6: the attestation is untouched by the dispute's whole lifecycle"
```

`UnitTests.Oracle` (re-landed, 9 → 12). Its only BJJ coupling is four obsolete `FeeConfig` fields
(`fcPromotionFee`, `fcMembershipHistoryFee`, `fcMembershipIntervalFee`, `fcAchievementFee` at
`:122-125`, `:208-211`, `:266-269`) and four `InitProfileAction` call sites. New:
```
"Test Case 0.10: rotate the admin key, and the new admin can act (fix #2)"
"Test Case 0.11: the OLD admin cannot act after rotation FAILS (O1)"
"Test Case 0.12: a non-admin rotation FAILS (O1)"
```

`UnitTests.Cleanup` (re-landed, 5 → 4 and finally with a negative). Cases 3.2 and 3.5 pointed at the
Ranks and Memberships validators, which do not exist; they become one case over the Disputes validator.
And 3.4 — "cleanup does not affect legitimate protocol UTxOs" — currently only *logs* the profile twice
(`:136`, `:152`), so it catches a crash and not silent corruption: it becomes an equality assertion on
the queried datum, plus a `mustFail` that Cleanup cannot spend a UTxO whose datum parses.

`PropertyTests` (new). The upstream `Decentralized-Belt-System/src/test/BJJPropertyTests.hs` has 37
properties of which exactly **one** is the on-chain ≡ off-chain equivalence
(`"checkPromotion mirrors validatePromotion"`, `:68`); the other 36 are intra-layer laws. For this domain:
```
"the off-chain fee equals feeFor for every MintAction"              -- the T-ATTEST-3 property, moved here
"applyOracleAdminAction off-chain mirrors applyAdminAction on-chain"
"checkCredentialControl agrees with the off-chain claim predicate"
"toBuiltinData/fromBuiltinData roundtrips"  -- OracleParams, FeeConfig, OnchainProfileKind,
                                            -- OnchainProfile, OnchainAttestation, OnchainDispute,
                                            -- Verdict, MetadataFields, CIP68Datum
```
Only the equivalence properties are new work; the roundtrips transfer shape-for-shape.

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cabal test --test-options='-p "Profile Tests"'` (and the five other group names)
Expected: FAIL to compile, with the missing names enumerated: from `TestRuns.hs`, the deps
`ranksValidatorGY`, `membershipsValidatorGY`, `ranksValidatorSize`, `membershipsValidatorSize`,
`ranksValidatorHashAndRef`, `membershipsValidatorHashAndRef`, `getRanksValidatorRef`, `OnchainRank`,
`getCurrentRankId`, `promoteProfile`, `promotionAwardedTo`, `rankId`, `getPractitionerInformation`,
`getRankStateDatumAndValue`, and the `ProfilesRedeemer` constructors `AcceptPromotion` /
`PromotionAcceptance`; from `UnitTests/Attestation.hs`, `CreateProfileWithRankAction`,
`InitProfileAction` and the belt enum `White`/`Black`, which **exists nowhere in `src/lib`**.

- [ ] **Step 3: Re-land and extend**

Order matters: `Test/Fixtures.hs` (content only, compiles today), then `TestRuns.hs` (the trim is where
half the BJJ surface disappears), then the five test modules, then `PropertyTests`. Add `hedgehog` and
`tasty-hedgehog` to the test-suite `build-depends`; add all six modules to `other-modules` and wire each
into `src/test/UnitTests.hs` — `Main.hs` runs only `unitTests`, so a module not wired there is a module
that compiles and never runs.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cabal test` and `scripts/test_exunits.sh`
Expected: PASS. 263 + ~45 emulator and property tests. `test_exunits.sh` reports an `EXUNITS` and
`SCRIPTSIZE` line per validator — **record them in the task's commit message**, because T-ATTEST-10's
deployment is where a size regression becomes expensive.

- [ ] **Step 5: Commit**

```bash
git add src/test palmares.cabal
git commit -m "test(onchain): the emulator suite — F1 check 8, from zero to N cases, M negative"
```

---

## Phase 3 — the off-chain write path

### Task T-ATTEST-6: the domain actions, the claim builder, and the script witness

**Files:**
- Modify: `src/lib/offchain-lib/Palmares/DomainTypes/Core/{Types,Actions}.hs`
- Modify: `src/lib/offchain-lib/Palmares/TxBuilding/{Interactions,Operations,Lookups,Skeletons,Conversions,Exceptions,Context,Validators}.hs`
- Test: `src/test/UnitTests/WritePath.hs` (new)

**Interfaces:**
- Consumes: T-ATTEST-2's `MintProfile`; T-ATTEST-3's `MintAction`/`feeFor`; T-ATTEST-4's `OpenDispute`;
  `Palmares.DomainTypes.Core.Ids` (CIP-129 decode, from Plan A).
- Produces:
  - `ProfileActionType` gains `ClaimProfileAction {kind, credential :: Maybe Text, witness :: Maybe ClaimWitness, profileData, outputId}`
    and `DisputeActionType (OpenDisputeAction | ResolveDisputeAction)`.
  - `ClaimWitness (WitnessKey | WitnessSimpleScript GYSimpleScript | WitnessPlutusScript GYScript GYRedeemer)`.
  - `txMustSpendFromAddress :: ... => GYAssetClass -> [GYAddress] -> GYTxInWitness 'PlutusV3 -> m (GYTxSkeleton 'PlutusV3)`
    — the witness becomes a **parameter** (today `GYTxInWitnessKey` is hardcoded at `Skeletons.hs:124`).
  - `claimProfileTX`, `openDisputeTX`, `resolveDisputeTX`; `getDisputeDatumAndValue`,
    `getOpenDisputesForAttestation`.
  - New `TxBuildingException`s: `ScriptCredentialHasNoUtxo`, `CredentialMalformed`,
    `DisputeAlreadyOpen`, `DisputeNotFound`, `DisputeAlreadyResolved`, `SubjectProfileNotFound`.
    `DisputeNotFound` already exists as an unreachable **HTTP code** (`WebAPI/Envelope.hs:138`, →404 at
    `:200`), so this is what finally makes it reachable — and `UnitTests/GovDto.hs:219`'s golden test on
    its wire string already pins the string.

- [ ] **Step 1: Write the failing test**

```haskell
tests = testGroup "the write path"
  [ -- Review Focus item 1: the refusal must be clean and must carry the workaround.
    testCase "a script credential with no UTxO at its own address is refused, with the fix in the message" $ do
      e <- buildClaimFor (scriptCredWithNoUtxo) `catchTxBuilding` pure
      e @?= ScriptCredentialHasNoUtxo scriptHashA
      assertBool "the message says how to fix it"
        ("send a UTxO to that script address" `isInfixOf` displayException e)
  , testCase "a malformed credential is refused before any chain query" $
      parseClaimCredential "drep1notabech32" @?= Left CredentialMalformed
  , -- CIP-129 both ways, because the claim's whole point is that the header byte is not thrown away.
    testCase "a CIP-129 drep id round-trips to a key credential and back" $
      (claimCredentialToText <$> parseClaimCredential drepKeyBech32) @?= Right drepKeyBech32
  , testCase "a CIP-129 SCRIPT drep id parses to a script credential, not a key one" $
      isScriptCredential <$> parseClaimCredential drepScriptBech32 @?= Right True
  , -- Review Focus item 3, off-chain half.
    testCase "issuing an attestation about a subject that does not exist is refused" $
      buildIssueFor missingSubject `shouldThrowTxBuilding` SubjectProfileNotFound
  , -- Review Focus item 4, off-chain half — stated as a courtesy, not a guarantee.
    testCase "a second dispute while one is open is refused off-chain" $
      buildDisputeFor targetWithOpenDispute `shouldThrowTxBuilding` DisputeAlreadyOpen
  , testCase "the witness is a parameter, so a script-held user NFT can be spent" $ do
      k <- skeletonWitnessOf WitnessKey
      s <- skeletonWitnessOf (WitnessSimpleScript nativeScriptA)
      (isKeyWitness k, isKeyWitness s) @?= (True, False)
  , -- Fix #4's off-chain half: the date is no longer client input at all.
    testCase "IssueAttestationRequest carries no date field" $
      assertBool "no date in the request type"
        (not ("attestation_date" `isInfixOf` requestSchemaJson @IssueAttestationRequest))
  ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "the write path"'`
Expected: FAIL to compile — `ClaimProfileAction`, `ClaimWitness`, `ScriptCredentialHasNoUtxo`,
`parseClaimCredential` are not in scope; and `skeletonWitnessOf` cannot be written at all against
today's `txMustSpendFromAddress`, which takes no witness.

- [ ] **Step 3: Implement**

`interactionToTxSkeleton` gains the arms (`Interactions.hs:74-103` is total today and must stay total).
`claimProfileTX` resolves the credential with `Palmares.DomainTypes.Core.Ids`, and for a
`ScriptCredential` finds a spendable UTxO at the script's address **before** building, so the refusal is
`ScriptCredentialHasNoUtxo` and not a balancer error — that is Review Focus item 1's whole point.

`getFeeSkeleton` is already selector-free (T-ATTEST-3). `issueAttestationTX` derives the date from
`slotOfCurrentBlock`, and `aa_attestation_date` disappears from the request type.

`openDisputeTX` queries `getOpenDisputesForAttestation` and refuses when one is open — **a courtesy,
not a guarantee**, because the chain does not enforce it (T-ATTEST-4). The haddock says so.

Also: give `updateProfileTX` and `acceptAttestationTX` the validity window and the oracle reference
input they currently skip. Today **only** `issueAttestationTX` attaches either (`Operations.hs:218`), so
`opPaused` does not stop a profile update or an acceptance — the protocol cannot actually be paused.
That is a correctness gap this task closes, and it earns its own test:
`"a paused protocol refuses a profile update and an acceptance, not only an issuance"`.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cabal build all && cabal test`
Expected: PASS, including the nine `the write path` cases and the pause test.

- [ ] **Step 5: Commit**

```bash
git add src/lib src/test palmares.cabal
git commit -m "feat(offchain): claim, dispute, and a witness that can be a script"
```

### Task T-ATTEST-7: the interaction-api surface

**Files:**
- Modify: `src/exe/interaction-api/{RestAPI,ServiceRequests,ServiceHandlers,InteractionAppMonad,Main}.hs`
- Create: `src/exe/interaction-api/ServiceStatus.hs`
- Test: `src/test/UnitTests/InteractionApi.hs` (new); `scripts/smoke_interaction_api.sh` (new)

**Interfaces:**
- Produces: `POST /claim-profile`, `POST /open-dispute`, `POST /resolve-dispute`; `GET /status`;
  `interaction-api --write-swagger-only`; Swagger `3.0.0` titled for this product.

- [ ] **Step 1: Write the failing test**

```haskell
tests = testGroup "interaction API surface"
  [ testCase "every write action has a typed route" $
      sort (routePaths fullAPI) @?= sort
        [ "/accept-attestation", "/build-tx", "/claim-profile", "/health", "/issue-attestation"
        , "/open-dispute", "/ready", "/resolve-dispute", "/status", "/submit-tx"
        , "/swagger-api.json", "/swagger-ui", "/tx-status/:txId" ]
  , -- /health answers `alwaysHealthy` today (ServiceProbe.hs:58-59) and reports healthy with a dead
    -- provider and unreachable scripts. /status is what an operator actually needs.
    testCase "/status reports the provider, the deployed scripts and the oracle's paused flag" $
      statusFields @?= ["deployed_scripts", "network", "oracle_paused", "provider", "version"]
  , -- The admin surface must not be reachable by a Basic-auth holder.
    testCase "no admin action deserializes from an HTTP body" $
      map (parseAdminFromWire . tagOf)
          [PauseProtocolAction, UnpauseProtocolAction, SetMinUTxOValueAction 1, RotateAdminAction pkhA]
        @?= [Left (), Left (), Left (), Left ()]
  , testCase "the Swagger title and version name this product" $
      (swaggerTitle, swaggerVersion) @?= ("Palmares Interaction API", "3.0.0")
  ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "interaction API surface"'`
Expected: FAIL on all four. The route list is missing four paths; `statusFields` does not exist;
**`parseAdminFromWire` returns `Right` for the first three** — today `AdminActionType`'s `FromJSON`
accepts `PauseProtocolAction`, `UnpauseProtocolAction` and `SetMinUTxOValueAction` over the wire
(`DomainTypes/Core/Actions.hs:109-116`), so any Basic-auth holder can have the server build a
protocol-pause transaction through `/build-tx`; and the title is still
`"Decentralized Belt System Interaction API"` at version `2.1.0`.

- [ ] **Step 3: Implement**

The three routes as typed endpoints beside the existing ones (`RestAPI.hs:127-143`), each a narrow
façade over the same `Interaction` pipeline. `/status` modelled on `query-api`'s
(`src/exe/query-api/Query/ServiceStatus.hs:52`), reporting the redacted provider, whether all five
reference scripts resolve, the oracle's `opPaused`, the network and the version.

**Close the admin hole**: every `AdminActionType` constructor's `FromJSON` fails, the way
`SetFeesAction` already does (`Core/Actions.hs:116`). Admin actions reach the chain through
`admin` only. This is a behaviour change on a route that exists today, so it is called out in the commit
message, not just the diff.

`--write-swagger-only` mirroring `query-api`'s (`src/exe/query-api/Main.hs:73`), so the Swagger CI guard
covers both services. Note `docs/generated/swagger/interaction-swagger-api.json` **does not exist** in
the tree — this service has never been booted here — so the guard's first run creates it.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cabal test` and `scripts/check_swagger.sh`
Expected: PASS, and `check_swagger.sh` green over both generated files.

- [ ] **Step 5: Commit**

```bash
git add src/exe/interaction-api src/lib/offchain-lib src/test scripts palmares.cabal docs/generated
git commit -m "feat(api): claim/dispute routes, /status, and admin actions off the HTTP surface"
```

---

## Phase 4 — projections and the read surface

### Task T-ATTEST-8: Ingestion — fix #3 wired, and a replay that rebuilds projections

Synthesis §3 step 22. Fix #5 (chain time on read) landed in Plan A's T-EXTRACTION-6; fix #3 is tested
and **not wired**, which is the open half.

**Files:**
- Modify: `src/lib/offchain-lib/Palmares/Ingestion.hs`, `Palmares/Storage.hs`
- Modify: `src/test/UnitTests/{ChainSyncReplay,StorageRoundtrip}.hs`
- Test: `src/test/UnitTests/Projections.hs` (new)

**Interfaces:**
- Consumes: `runAttest`, `attestSchema`, `ensureAttestSchema` (commit `71fa00f`);
  `authenticateStateUTxO`.
- Produces: `DisputeProjection`; `ProfileProjection` gains `profileCredential`, `profileKind`, and a
  **unique index on `profileCredential`** dropped in favour of a plain index (a credential may be
  claimed twice — Review Focus item 2); `currentSchemaVersion = 2`; `projectChainEvent` returning
  `NoEvent` for an unauthenticated UTxO.

- [ ] **Step 1: Write the failing test** — `src/test/UnitTests/Projections.hs`

```haskell
tests = testGroup "projections"
  [ -- Fix #3, and the test that fails on the old behaviour: today projectChainEvent dispatches on
    -- address alone (Ingestion.hs:30, :36), so an inline datum at a script address with 2 ADA and no
    -- protocol token projects as state.
    testCase "a UTxO at the profiles address WITHOUT the named token projects nothing" $
      projectChainEvent nid (matchAtProfilesAddressWithoutToken) @?= NoEvent
  , testCase "a UTxO at the profiles address WITH its named token projects a profile" $
      isProfileEvent (projectChainEvent nid matchAtProfilesAddressWithToken) @?= True
  , testCase "a token whose name does not match the datum's own id projects nothing" $
      projectChainEvent nid matchWithMismatchedToken @?= NoEvent
  , -- Review Focus item 5: the replay must rebuild PROJECTIONS, not just raw rows.
    testCase "a rollback and replay rebuilds the projections byte-identically" $
      withScratchDb $ \pool -> do
        before <- applyAll pool fixtureMatches >> projectionSnapshot pool
        runAttest pool (rollbackTo nid rollbackSlot)
        after <- applyAll pool fixtureMatches >> projectionSnapshot pool
        after @?= before
  , testCase "the fixture addresses are real addresses, so the replay takes the success branch" $
      all (isJust . addressFromTextMaybe) fixtureAddresses @?= True
  , testCase "replaying twice is idempotent at the projection level, not only the match level" $
      withScratchDb $ \pool -> do
        applyAll pool fixtureMatches
        one <- projectionSnapshot pool
        applyAll pool fixtureMatches
        two <- projectionSnapshot pool
        two @?= one
  , testCase "a dispute UTxO projects a DisputeProjection with its target and verdict" $
      isDisputeEvent (projectChainEvent nid matchAtDisputesAddress) @?= True
  , testCase "a profile projection carries the claimed credential, and two claims coexist" $
      withScratchDb $ \pool -> do
        applyAll pool [claimOf walletA credX, claimOf walletB credX]
        claimants <- profilesForCredential pool credX
        length claimants @?= 2
  ]
```

- [ ] **Step 2: Run the tests to verify they fail**

Run: `cabal test --test-options='-p "projections"'`
Expected: FAIL. The first case returns a `ProfileEvent` rather than `NoEvent` — that is the poisoning,
reproduced. The replay cases fail to compile (`projectionSnapshot`, `DisputeProjection` absent). And
`"the fixture addresses are real addresses"` fails against `StorageRoundtrip.hs:35`'s literal `"addr"`,
which is exactly why the existing replay test only ever exercised the conversion-error branch
(`Storage.hs:240`).

- [ ] **Step 3: Implement**

Wire `authenticateStateUTxO` into both branches of `projectChainEvent` (`Ingestion.hs:30`, `:36`) and the
new disputes branch — the one site the plan's fix #3 names. Add `DisputeProjection`; extend
`ProfileProjection`; bump `currentSchemaVersion` to 2 so the existing wipe-on-mismatch path
(`Main.hs:83-91`) re-syncs. Add every new table name to `chainSyncTableNames` — the
`UnitTests.AttestSchema` test added in `71fa00f` fails if one is missing.

Fix `StorageRoundtrip.hs`'s fixture addresses (`:35`) so its `rollbackTo` call actually exercises the
projection path, and give it the projection assertions it lacks.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cabal build all && cabal test`
Expected: PASS, 8 new cases plus the repaired `StorageRoundtrip`. F1 check 4 is met by
`"a rollback and replay rebuilds the projections byte-identically"`.

- [ ] **Step 5: Commit**

```bash
git add src/lib/offchain-lib src/test palmares.cabal
git commit -m "fix(ingestion): fix #3 wired — an unauthenticated UTxO is not state; replay rebuilds projections"
```

### Task T-ATTEST-9: the attestation read surface over `palmares_attest`

`Query.Projected` (488 lines), `Query.Aggregates` (56) and `Query.Pages` (120) exist and are commented
out of `query-api`'s `other-modules` (`palmares.cabal:334-347`) by **domain decision**, not because of
broken references — every identifier they import still exists, so they are plausibly compilable today.

**Files:**
- Modify: `src/exe/query-api/{RestAPI,Query/Projected,Query/Aggregates,Query/Pages}.hs`
- Create: `src/exe/query-api/Query/Attest.hs` (the anchor join and the contested-credential rule)
- Modify: `src/lib/offchain-lib/Palmares/DomainTypes/Transfer/{QueryResponses,Filters,OrderBy}.hs`
- Modify: `palmares.cabal`; `scripts/smoke_query_api.sh`
- Test: `src/test/UnitTests/AttestQuery.hs` (new)

**Interfaces:**
- Consumes: T-ATTEST-8's projections; `palmares.treasury_withdrawal` and `palmares.gov_action` from
  Plan A; `Query.Common`'s bounds and ordering helpers.
- Produces: `GET /profiles`, `/profile/{id}`, `/attestations`, `/attestation/{id}`, `/disputes`,
  `/pages/profile/{id}`, `/pages/attestation-explorer`; `anchor_status` on an attestation response;
  `credential_status: verified | contested | unclaimed` on a profile response.

- [ ] **Step 1: Write the failing test**

```haskell
tests = testGroup "the attestation read surface"
  [ -- Review Focus item 2's visible half.
    testCase "a credential with two claimants reads contested, listing both oldest-first" $ do
      r <- getProfileByCredential credX
      (credentialStatus r, map claimSlot (claimants r)) @?= (Contested, [100, 220])
  , testCase "a credential with one claimant reads verified" $
      credentialStatus <$> getProfileByCredential credY >>= (@?= Verified)
  , -- spec §4.3: the pointer is checked at READ time and the API says whether it resolved.
    testCase "an attestation whose treasury anchor resolves shows the withdrawal" $ do
      r <- getAttestation withResolvableAnchor
      (anchorStatus r, anchorWithdrawalLovelace r) @?= (AnchorResolved, Just 150000000000)
  , testCase "an attestation whose anchor does NOT resolve is marked, not hidden" $ do
      r <- getAttestation withDanglingAnchor
      (anchorStatus r, isJust (anchorWithdrawalLovelace r)) @?= (AnchorUnresolved, False)
  , -- Review Focus item 3's visible half.
    testCase "an attestation whose subject profile does not exist is marked, not hidden" $
      subjectStatus <$> getAttestation withMissingSubject >>= (@?= SubjectUnresolved)
  , -- spec §4.3: a dispute is shown BESIDE the attestation and never hides it.
    testCase "a disputed attestation is still returned, with its disputes attached" $ do
      r <- getAttestation disputedOne
      (isJust (attestationId r), length (disputes r)) @?= (True, 1)
  , testCase "an upheld dispute does not remove the attestation from the list" $
      elem disputedOne . map attestationId <$> listAttestations defaultFilter >>= assertBool "still listed"
  , testCase "the attestation routes read palmares_attest and never db-sync's tables" $
      assertBool "no unqualified or public. table in the SQL"
        (all (readsOnly ["palmares_attest.", "palmares."]) attestQuerySql)
  ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "the attestation read surface"'`
Expected: FAIL to compile — the modules are not in `other-modules`, so none of the handlers exists.

- [ ] **Step 3: Implement**

Un-comment the three modules, point them at `palmares_attest` (schema-qualified, like every other query
in this codebase — `Query.Common`'s own haddock notes `legacyNormalizeLimitOffset` survives only because
"the inherited profile/attestation handlers still call" it, so this is where that note is resolved).
`Query/Attest.hs` carries the two read-time rules: the anchor join against
`palmares.treasury_withdrawal` and the contested-credential grouping.

Extend `scripts/smoke_query_api.sh` (330 assertions today) with the new routes.

- [ ] **Step 4: Run the tests to verify they pass**

Run: `cabal test && scripts/smoke_query_api.sh`
Expected: PASS, and the smoke script green over 22 + 7 routes.

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api src/lib/offchain-lib src/test scripts palmares.cabal
git commit -m "feat(api): the attestation read surface — anchors resolved at read, disputes beside not instead"
```

---

## Phase 5 — deploy, and the end-to-end gate

### Task T-ATTEST-10: deploy the validators on preprod — F1 check 10

**This is the task that freezes every `makeIsDataSchemaIndexed` index.** Nothing before it is
deployed; nothing after it can reorder a constructor without a coordinated redeploy.

**Files:**
- Modify: `src/exe/admin/Main.hs` (`deploy-reference-scripts` over five validators)
- Create: `config/config_palmares_validators.preprod.json` (**committed in the same commit as the deploy**)
- Create: `docs/deployment/attestation-deploy.md`
- Modify: `compose.yaml` (add `kupo`, `interaction-api`, `attest-sync` — all three absent today)
- Test: `scripts/check_deployed_validators.sh` (new)

**Interfaces:**
- Consumes: ADR 0002's provider decision (Atlas 0.14.1 + Maestro, token from the environment);
  `operation.prv`.
- Produces: the deployed-validators config; `interaction-api`'s `/ready` green;
  `admin rotate-admin` usable against the live oracle.

- [ ] **Step 1: Write the failing test** — `scripts/check_deployed_validators.sh`

```bash
#!/usr/bin/env bash
# The deployed config is a committed artifact, so it gets a check rather than a trusting read.
set -euo pipefail
: "${PALMARES_INTERACTION_API:=http://localhost:8082}"
CFG="${1:-config/config_palmares_validators.preprod.json}"
fail=0
chk() { [ "$2" = "$3" ] || { echo "FAIL: $1 (got $2, want $3)"; fail=1; }; }
chk "five validators in the config" "$(jq '[.. | objects | select(has("refScript"))] | length' "$CFG")" 5
chk "the minting policy hash is 56 hex chars" "$(jq -r '.mintingPolicy.hash | length' "$CFG")" 56
chk "/ready is green"   "$(curl -fsS "$PALMARES_INTERACTION_API/ready" | jq -r .status)" ready
chk "the oracle is not paused" "$(curl -fsS "$PALMARES_INTERACTION_API/status" | jq -r .oracle_paused)" false
# Every fee zero, per spec §4.3 — no protocol fees as a business model.
chk "no fee is configured" "$(curl -fsS "$PALMARES_INTERACTION_API/status" | jq -r '.fees_total // 0')" 0
exit $fail
```

- [ ] **Step 2: Run test to verify it fails**

Run: `./scripts/check_deployed_validators.sh`
Expected: FAIL — `config/config_palmares_validators.preprod.json` does not exist (today `config/` holds
only `config_atlas.example.json`, `reconcile-exceptions.yaml`, `reconcile-sample.preprod.json`, so
**`Constants.hs:9`'s default resolves to nothing** and both services die unless
`DEPLOYED_VALIDATORS_CONFIG` is supplied as an env blob), and `/ready` is unreachable.

- [ ] **Step 3: Deploy**

`admin deploy-reference-scripts` with `ATLAS_CORE_CONFIG` pointing at a real Maestro token from the
environment. The oracle NFT mints **before** the minting policy compiles (the policy is parameterised by
the oracle asset class), and the oracle NFT policy stays **inlined**, not deployed as a reference script.

Record in `docs/deployment/attestation-deploy.md`: every validator hash, the oracle asset class, the
`EXUNITS`/`SCRIPTSIZE` figures from T-ATTEST-5, and the rotation procedure now that `OracleSetAdmin`
exists. **Also record the ADR 0002 spike outcome** if it has been run by now — the preprod node's IPC
socket exists in `experiments/dbsync-preprod` (`/ipc/node.socket`), so Atlas's local-node provider can be
evaluated off-cluster without the cluster.

- [ ] **Step 4: Run the test to verify it passes**

Run: `./scripts/check_deployed_validators.sh`
Expected: PASS, five validators resolvable and `/ready` green.

- [ ] **Step 5: Commit**

```bash
git add config/config_palmares_validators.preprod.json docs/deployment/attestation-deploy.md \
        scripts/check_deployed_validators.sh compose.yaml src/exe/admin
git commit -m "feat(deploy): five validators on preprod, and the config committed with the deploy"
```

### Task T-ATTEST-11: `scripts/smoke_attest_flow.sh` — F1 check 11

The check in the spec's own words: claim on **both** credential paths → issue (pending), with and
without a treasury anchor → countersign → dispute opened → resolved, with the projections appearing
correctly in query-api **with tx-proof**, the anchored attestation showing its treasury withdrawal, and
one with an unresolvable anchor marked rather than hidden.

**Files:**
- Create: `scripts/smoke_attest_flow.sh`, `scripts/populate_attest_fixtures.sh`
- Modify: `docs/deployment/attestation-deploy.md`
- Modify: `docs/superpowers/F1-PROGRESS.md`

**Interfaces:**
- Consumes: T-ATTEST-10's deployed config; a funded preprod key; `interaction-api` and `query-api`
  running; `attest-sync` following Kupo.
- Produces: the F1 check 11 evidence, and a populate script the UI (Plan C) can run against.

- [ ] **Step 1: Write the failing test**

The script **is** the test. Its assertions, in order, each a `check` in the style of
`scripts/smoke_query_api.sh`:

```
claim a key-credential profile          -> 200, tx submitted, projection appears, credential_status=verified
claim a script-credential profile       -> 200, projection appears, credential_status=verified
claim a script credential with no UTxO  -> 4xx, error code script_credential_has_no_utxo
issue an attestation, no anchor         -> 200, accepted=false, subject_status=resolved
issue an attestation with an anchor     -> 200, anchor_status=resolved, the withdrawal's lovelace matches
issue one with a dangling anchor        -> 200, anchor_status=unresolved, and it is RETURNED not hidden
issue an attestation ABOUT an attestation -> 200, lineage visible on the parent
countersign the first                   -> 200, accepted=true
countersign it again                    -> 4xx, attestation_already_accepted
open a dispute against it               -> 200, deposit locked, attestation STILL listed
open a second dispute                   -> 4xx, dispute_already_open
resolve it upheld (admin)               -> 200, verdict=upheld, deposit returned, attestation still listed
every projection row                    -> carries tx_hash + output_index (tx-proof)
```

- [ ] **Step 2: Run it to verify it fails**

Run: `./scripts/smoke_attest_flow.sh`
Expected: FAIL at the first assertion — `/claim-profile` 404 before T-ATTEST-7, or the projection never
appears before T-ATTEST-8.

- [ ] **Step 3: Write both scripts**

`populate_attest_fixtures.sh` creates the profiles and attestations a UI needs to look alive — the
pattern of the Belt System's `populate_*` scripts, whose two known bugs
(`MATHEUS_PURPLE_TIME`, impossible superseded promotions) are **not** carried over. Note the lesson from
those: `admin` asset-id arguments need JSON quoting.

- [ ] **Step 4: Run it to verify it passes**

Run: `./scripts/smoke_attest_flow.sh`
Expected: PASS, all 13 assertions, `smoke OK`. **This is F1 check 11.** Record the run in
`F1-PROGRESS.md` with the transaction hashes, because the check's evidence is the chain, not the script's
exit code.

- [ ] **Step 5: Commit**

```bash
git add scripts/smoke_attest_flow.sh scripts/populate_attest_fixtures.sh \
        docs/deployment/attestation-deploy.md docs/superpowers/F1-PROGRESS.md
git commit -m "test(preprod): the attestation flow end to end — F1 check 11"
```

---

## Decisions

The builder decisions this plan rests on, with their reasoning, and the three things that are the
owner's. Written before the tasks, per the autonomy mandate; the long form is in
`docs/superpowers/design/` once this plan is approved.

### §1 The rename table

| now | becomes |
|---|---|
| `OnchainAchievement` | `OnchainAttestation` |
| `achievementAwardedBy` / `achievementAwardedTo` | `attestationIssuer` / `attestationSubject` |
| `achievementIsAccepted` | `attestationAccepted` |
| `AchievementsRedeemer` / `AcceptAchievement` | `AttestationsRedeemer` / `AcceptAttestation` |
| `OnchainProfileType = Practitioner \| Organization` | `OnchainProfileKind`, six constructors (§2) |
| off-chain `Achievement`, `AwardAchievementAction`, `AcceptAchievementAction` | `Attestation`, `IssueAttestationAction`, `AcceptAttestationAction` |
| `achievementsValidator*`, `scriptHashesAchievementsValidator` | `attestationsValidator*`, `scriptHashesAttestationsValidator` |
| JSON `awarded_to_profile_id` / `awarded_by_profile_id` | `subject_profile_id` / `issuer_profile_id` |
| `AchievementProjection`, `achievement_projection` | `AttestationProjection`, `attestation_projection` |
| routes `/award-achievement`, `/accept-achievement` | `/issue-attestation`, `/accept-attestation` |

Breaking on the wire, and acceptable exactly once — now, before any client exists.

### §2 Six profile kinds, as constructors

`OnchainProfileKind (PersonDRep | Organization | Committee | Community | Vendor | Auditor)`, indices
0–5, `Organization` keeping index 1.

**Rejected:** keep two constructors and put the kind in CIP-68 metadata. The kind decides nothing
on-chain, so metadata would be cheaper — but the read layer filters and groups by it, metadata is a
`Map BuiltinByteString BuiltinByteString` attest-sync would have to parse, and a future update redeemer
could silently change it. A constructor is one byte and cannot drift. The enum is closed: a seventh
kind is a redeploy, which is the right cost for changing what the registry recognises.

### §3 The claim: control of a credential

`profileCredential :: Maybe Credential` in the datum — `Credential`, so CIP-129's header byte
distinguishing key from script is not thrown away, and `Maybe`, because a Community or an Auditor need
not correspond to any on-chain credential and forcing one would exclude actors the spec names.

- `PubKeyCredential pkh` → `pkh ∈ txInfoSignatories`.
- `ScriptCredential sh` → an input's **payment or staking** credential is `ScriptCredential sh`. If the
  ledger is spending that input, the script's spend condition was already satisfied — by the ledger,
  before our validator ran. We do not re-check it and could not: a native timelock/multisig script has
  no redeemer to inspect, and committees and multisig organisations are the majority case §4.3 calls out.

**Staking as well as payment**, because a stake credential that is a script normally appears in the
staking half of an address. One more field read per input, and it removes a whole class of "I cannot
claim my own stake script". This is `onchain-lib/CLAUDE.md`'s security rule 13 applied usefully.

**Documented limitation:** a Plutus script holding no UTxO at its own address — one that only votes or
withdraws — cannot claim in a single transaction. Its workaround is one transaction: send yourself a
UTxO at your own script address, then claim. We do **not** add a `txInfoRedeemers`-purpose check to
cover it; that would cost ExUnits on every claim to serve a case whose workaround is free, against this
repo's standing rule. It goes in the runbook, the validator haddock, and the error message
(`ScriptCredentialHasNoUtxo`).

**Uniqueness is a read fact.** Nothing on-chain stops two profiles claiming one credential; enforcing it
would need a global index UTxO, the contention anti-pattern rule 11 rejects. The chain records every
claim; the API marks a credential with more than one claimant `contested`, showing all claimants
oldest-first, rather than picking a winner.

**What the claim does not prove**, and the API must not imply: that the credential's holder is who they
say they are. It proves control. Plan C's `VerificationChip` says "controls this credential", never "is".

### §4–§6 The inherited fixes, and disputes

Fix #1 → `MintAction` + `feeFor`, one naming site, selector-free off-chain
(`getFeeSkeleton`'s `(FeeConfig -> Integer)` is the exact mechanism that charged the wrong field in the
Belt System). Fix #2 → `OracleSetAdmin` at index 4; the validator needs no change because `O1`/`O2` were
written correctly. Fix #4 → `checkDateInRange` requiring containment **and** a finite lower bound; today
`before` blocks future-dating and leaves back-dating unbounded, so an attestation can claim 2015. Plus
the on-chain double-accept guard, which is not the non-exploitable case: the off-chain guard is
bypassable by anyone building their own transaction.

Disputes → their own validator and UTxO, `opDisputeDeposit` **outside** `FeeConfig` (a returned deposit
is not a fee, and fees ship at zero), verdict from the oracle admin, deposit returned on both verdicts,
and the attestation never hidden. `ProtocolParams` gains the disputes hash, which changes the minting
policy's hash — hence disputes before T-ATTEST-10, not after.

### §7 Out of Plan B, each with its reason

- **Protocol fees as revenue** — spec §4.3 is explicit (the JPG Store lesson). `FeeConfig` ships at zero.
- **Admin multisig (M-of-N)** — §4.3 marks it desirable, decision deferred to the builder on ExUnits.
  **Not in F1.** `OracleSetAdmin` makes key compromise *recoverable*, which is the actual risk; M-of-N
  costs ExUnits on every admin action to reduce a risk rotation already bounds. Revisit before F2, where
  a soak month exists to test it.
- **Attestation revocation and expiry** — not in the spec. An attestation is a permanent record and a
  dispute is how it is contested. Named here because nothing can be burned (`MintingPolicy.hs:165`), so
  permanence is a conscious property, not an oversight.
- **The oracle validator's missing NFT check.** `OracleValidator` is unparameterised and does not verify
  the oracle NFT, so any UTxO at that address with a parseable `OracleParams` datum is spendable by
  whatever pkh *its own datum* names. Not exploitable: every reader authenticates by NFT
  (`readOracleParams`), so an attacker can only plant and spend their own UTxO. **No check added**;
  documented in the validator haddock so the next reader does not re-derive it.
- **Subject-existence check at mint.** Needs the subject's profile UTxO as a reference input on every
  issuance. The off-chain path already looks the subject up to build the transaction, and the read layer
  marks an unresolvable subject — the same trade, and the same reasoning, as §4.3's anchor pointer.
- **`BATCH_SIZE` / `FETCH_BATCH_SIZE` drift.** `BATCH_SIZE` is set in `.env.example:19` and
  `Dockerfile.attest-sync:51` and **read nowhere**; `FETCH_BATCH_SIZE`'s in-code default is 10,000,000
  slots against 100,000 in both files, so unset, the first sync window is effectively unbounded. An
  operational hazard, not a correctness one. Named for T-ATTEST-10's deployment review.

### OWNER — three decisions, none blocking

1. **Is `Auditor` a profile kind or a role an `Organization` holds?** The spec lists it as a kind, so
   that is what is built. The question is only whether an auditor that is also an organisation should
   need two profiles.
2. **May an attestation record an event date earlier than its issuance?** (A milestone delivered last
   month.) The plan builds the strict version — `attestationDate` is issuance time, bounded by the
   validity interval. If yes, the datum needs a second, explicitly self-reported field and the API must
   never present it as chain-verified. This is a product question, not a technical one.
3. **Carried over from Plan A and still open:** the third `DRepStatus` for "registered, deadline
   unknown" (21 preprod DReps read `inactive` while the ledger calls them active); the GHCR owner handle
   (`en7angled` vs `mariusgeorgescu`), which blocks the two k8s Jobs and the live `reconcile.yml`; and
   ADR 0001's PVC/memory fork.

---

## Self-review

**Spec coverage.** §4.3's clauses map to tasks: CIP-68 profiles with six kinds → T-ATTEST-1/2; the
identity claim on both credential paths → T-ATTEST-2/6; two-phase attestations with a profile *or an
attestation* as subject → T-ATTEST-5 case 5.11; the optional level-1 anchor checked at read →
T-ATTEST-9; disputes with deposit and oracle verdict → T-ATTEST-4; oracle pause/min-UTxO/deposit/rotation
→ T-ATTEST-3; no protocol fees → Decisions §7; PlutusTx with the property test pinning on-chain ≡
off-chain → T-ATTEST-5; preprod first, mainnet after soak → T-ATTEST-10. F1 checks: 4 → T-ATTEST-8,
8 → T-ATTEST-5, 9 → T-ATTEST-3 (fixes #1/#2/#4) and T-ATTEST-8 (#3; #5 landed in Plan A),
10 → T-ATTEST-10, 11 → T-ATTEST-11. **Gap, deliberate:** §4.6's "arbitraj dispute" admin command is
T-ATTEST-4's `resolveDisputeTX` reached through `admin`, not a separate task.

**Review Focus coverage.** Item 1 → T-ATTEST-2 (`Mp3`), T-ATTEST-5 case 1.6, T-ATTEST-6
(`ScriptCredentialHasNoUtxo` with the workaround in the message). Item 2 → T-ATTEST-2's haddock,
T-ATTEST-5 case 1.9, T-ATTEST-8's two-claimant test, T-ATTEST-9's `contested`. Item 3 → T-ATTEST-5 case
5.10, T-ATTEST-6's `SubjectProfileNotFound`, T-ATTEST-9's `SubjectUnresolved`. Item 4 → T-ATTEST-4's
`checkNoOpenDispute` tests, T-ATTEST-6's `DisputeAlreadyOpen`. Item 5 → T-ATTEST-8's replay tests and
the `StorageRoundtrip` fixture repair.

**Type consistency.** `OnchainProfileKind` (not `ProfileKind`) on-chain and `ProfileType` off-chain,
which is the existing split. `AttestationId`/`ProfileId` stay `= AssetClass`. `MintAction` is defined in
T-ATTEST-3 and consumed by T-ATTEST-2's `Mp6` — so **T-ATTEST-2 and T-ATTEST-3 must land in that order
or `Mp6` is a forward reference**; T-ATTEST-2 stubs `checkFee oracle MintActionProfile` against a
`MintAction` it introduces, and T-ATTEST-3 moves `feeFor` to its final home. Called out because the
self-review found it.

**Proportion.** Twelve tasks over a spec section of ~20 clauses. No step carries a function body; the
code blocks are test assertions and tables of checks, which are the decisions an implementer cannot make
alone. The longest single piece of prose is Decisions §3, and it is the piece the synthesis explicitly
says must be settled in the plan rather than discovered at implementation time.

---

## Addendum — 2026-10-21, after the owner's answers and two wiring surveys

The owner answered the three open decisions (ledger: "Cele patru decizii de owner"). Two of the
answers change this plan, and two read-only surveys of the repo found four things the plan asserts
that the code does not. Both are recorded here rather than by rewriting the tasks above, so the
history of what was planned and what was learned stays legible.

### A new task: T-ATTEST-3b — the attestation kind and the claimed period

**Why this task exists at all: the plan had a spec gap, and the self-review missed it.** Spec §3's
glossary (line 40) defines an attestation as carrying one of **five types** — `MilestoneDelivered`,
`AuditPassed`, `Affiliation`, `RoleHeld`, `Endorsement` — and says two of them carry **intervals**:
`Affiliation` (organisation ↔ member, *cu intervale*) and `RoleHeld` (extra-chain mandate, *cu
intervale*). A grep for all five names across this plan returns nothing. The self-review mapped
§4.3, which is the attestation layer's own section, and never read §3's glossary — so the type and
the interval were not deferred, they were **lost**.

That gap is also what settles owner decision #3. An affiliation interval that begins before the
attestation is issued is the normal case, not an edge case: "a member since March 2024, attested
today". So "no, an attestation may not record an earlier date" was never an available answer.

**The answer taken, and the reason it is two fields and not a looser one.**

| field | meaning | validated against the validity interval? |
| --- | --- | --- |
| `attestationDate` | when the attestation was **issued** | **yes** — unchanged, `Ml` / `checkDateInRange` |
| `attestationPeriod :: Maybe (POSIXTime, POSIXTime)` | the period the attestation **claims** | **no, deliberately** |
| `attestationKind :: AttestationKind` | which of the five the attestation is | n/a |

The period is **not** validated and must never be, because validating it would be a lie: nothing
on chain can confirm when a membership began. That also means the validator gains **no new rule**
for it — there is nothing to check — which is why this task is small on chain and almost all of its
work is in presentation.

`attestationKind` is a **constructor, not metadata**, for the same reason `OnchainProfileKind` is: a
byte in the datum cannot drift, and a `Map BuiltinByteString BuiltinByteString` that the projection
has to parse can. The period is meaningful for only two of the five kinds, and the read layer has to
filter by kind anyway.

**Follow `OnchainProfileKind` / `ProfileType` exactly** — it is the same shape, already built, and
the survey confirms nothing in the repo branches on an attestation kind today:
`Protocol/Types.hs:170-181` ↔ `Core/Types.hs:49-79`, bridged at `Conversions.hs:52-67`, persisted
via `derivePersistFieldJSON`.

**The surveyed sites, in the order they must change:**

1. `onchain-lib/Palmares/Onchain/Protocol/Types.hs:219-229` — the two fields on `OnchainAttestation`,
   plus `AttestationKind` with its own `makeIsDataSchemaIndexed` at indices 0–4, appended discipline
   from the first deployment onward.
2. `onchain-lib/…/Validators/MintingPolicy.hs:80-96` — `NewAttestation` gains two arguments;
   `:132-134` the match; `:169-176` the on-chain datum literal. **`:187`'s `Mk` check compares that
   literal to the off-chain one byte for byte and there is no shared constructor between them**, so
   `Operations.hs:262-269` must change in the same edit or every issue transaction fails on chain.
3. `onchain-lib/…/Blueprint.hs:37-51` — register `AttestationKind` in `contractDefinitions`
   (`:192-195` records what a missed registration did last time); `:103-107` and `:186-190` are
   **published interface prose** that enumerates the redeemer args and the datum fields — both must
   list the new ones, and `:103-107` currently states the date "is validated against the transaction
   validity range" with no qualification, which becomes wrong the moment a second date exists.
4. `offchain-lib/…/TxBuilding/Operations.hs:262-269` (the datum), `:246-255` (the redeemer),
   `:191-206` (the signature). **`:226-230` is the date-window check and the period must NOT be
   routed through it** — that is the whole point of the field.
5. `offchain-lib/…/TxBuilding/Exceptions.hs:52-55` — `AttestationDateOutOfRange`'s
   `displayException` currently reads *"an earlier event date is self-reported and belongs in
   other_metadata"*. Adding the period **directly contradicts** it. The new text has to send the
   caller to `claimed_period` instead, or the error tells people to do the thing we just replaced.
6. `offchain-lib/…/DomainTypes/Core/Types.hs:124-185` — the `Attestation` record, and three
   hand-written instances that take positional care: `FromJSON` at `:142-156` (12 positional
   applications — a field inserted in the wrong position mis-binds **silently**), `Show` at
   `:161-175`, and `ToSchema` at `:177-185`, which is the one attestation schema that spells its
   field-label modifier out by hand instead of using `mkStripPrefixSchemaOptions`.
7. `offchain-lib/…/DomainTypes/Core/Actions.hs:55-61` — `IssueAttestationAction`.
8. `offchain-lib/…/DomainTypes/Transfer/QueryResponses.hs:126-208` — **two** response mirrors
   (`AttestationResponse`, `AttestationInformationResponse`) and their two copy functions.
9. `exe/interaction-api/ServiceRequests.hs:62-85` — `IssueAttestationRequest`, its
   `StripPrefix "iar"` **and** its `mkStripPrefixSchemaOptions "iar"`, plus a `describeField` for
   each new field. `check_vocabulary.sh` already guards that the two prefixes agree.
10. `offchain-lib/Palmares/Storage.hs:128-143` — the `AttestationProjection` entity, and
    `:321-338` `putAttestationProjection`, which builds it with **13 positional arguments and no
    field labels**. The survey calls this the most fragile site in the change and it is right.
11. `offchain-lib/Palmares/Storage.hs:187-188` — **bump `currentSchemaVersion` to 2.** A column
    added without the bump leaves a deployed attest-sync migrating onto a schema it then reads with
    the wrong shape. `UnitTests/AttestSchema.hs` does **not** assert this constant today, so nothing
    fails if it is forgotten — so this task also adds the assertion that would have caught it.

**The dangling key the survey found, which is evidence the type was always meant to exist.**
`webapi-lib/Palmares/WebAPI/Utils.hs:49` already attaches a swagger description to a definition
named **`"AttestationType"`** — *"What an attestation asserts about its subject"* — and **no Haskell
type of that name exists anywhere in the repo**. The key is absent from the generated swagger, so
the `at` lens is a silent no-op. It is the only trace of the planned enum, inherited and half-wired.
Name the off-chain enum `AttestationType` (the on-chain one `AttestationKind`, mirroring the
existing `ProfileType` / `OnchainProfileKind` split) and the description attaches itself; the
alternative is to delete a key that was right all along.

**The presentation rule, which is half this task's value.** The period ships as `claimed_period`
beside `issued_at`, and `/attestations` must never sort or filter the record as if the period were
verified. One sentence per field in the swagger description, exactly as `profileCredential =
Nothing` is presented as `unclaimed` rather than as verified. Owner decision #4's residue lands
here too: `kind` on a **profile** is `self_declared` — an `Auditor` profile is not an accreditation,
the accreditation is an attestation someone issued to it.

**Ordering:** before T-ATTEST-4 or in the same batch. Both change validator hashes, and T-ATTEST-4
changes the minting policy's parameter as well, so the two together are one CI run.

### Four corrections the surveys made to this plan's assumptions about T-ATTEST-4

1. **`mkProtocolParams` is point-free and positional.** `TxBuilding/Validators.hs:91-95` is
   `ProtocolParams profilesValidatorHashPlutus attestationsValidatorHashPlutus` with the oracle
   `AssetClass` as the eta-reduced last argument. Appending `disputesValidatorScriptHash` **after**
   `oracleToken` breaks that eta-reduction, so the function has to be rewritten (named fields, or an
   explicit two-argument form). The plan said "gains a field" and implied a one-line edit.
2. **`ProtocolParams` is embedded in every profile datum** (`Protocol/Types.hs:206`), so appending a
   field is a **datum-format change for all existing profiles**, not only a minting-policy parameter
   change. Nothing is deployed yet, so the cost is zero today and infinite after T-ATTEST-10 — which
   is the actual reason disputes must land before the deploy, stated more sharply than the plan
   stated it.
3. **`onchain-lib/CLAUDE.md:28-29` and `Blueprint.hs:127-128` both say the Profiles and Attestations
   validators "get validator addresses from the datum's `protocolParams`". Neither validator reads
   `protocolParams` at all.** Only `MintingPolicy.hs:121-122` resolves sibling addresses, and it
   does so from its lifted compile-time parameter. The only datum-sourced reader,
   `Protocol/Lookup.hs:44-52`, is unused by any validator. So the new disputes validator should
   follow what the code actually does — own address from the spent input via
   `unsafeFindOwnInputByTxOutRef`, oracle from the reference input — and the two documents should be
   corrected rather than copied.
4. **The dispute deposit cannot live on `FeeConfig` even if someone wanted it to.**
   `scripts/check_vocabulary.sh:139-156` is a CI gate that permits a `FeeConfig` field name in only
   four locations. `OracleParams.opDisputeDeposit`, which the plan already specified for the
   semantic reason that a returned deposit is not a fee, is also the only shape that passes the
   lint. Trace prefix **`D` is free**; `MintingPolicy`'s existing codes are an inconsistent mix of
   `M0`/`M1`, letter suffixes on the attestation path and `Mp1`–`Mp5` on the profile path, so the
   dispute path should use `Md1`–`Md5` and the disputes validator `D0`–`D5`.

### Two pre-existing problems the surveys surfaced, mentioned and NOT fixed

`CLAUDE.md` §3 — notice unrelated dead code, do not delete it.

* **`src/test/TestRuns.hs` is stale against the current `Validators.hs`**: it references
  `ranksValidatorGY` / `ranksValidatorSize` / `ranksValidatorHashAndRef` and the `memberships`
  equivalents, none of which exist — Belt System leftovers. `src/test/UnitTests/Cleanup.hs:19` has
  the same stale import. Both are comment-prefixed out of the test suite, so they are dormant rather
  than broken, and `TestRuns.hs:80-101` is the **only** script-size reporting site in the repo
  (namespace `SCRIPTSIZE`). T-ATTEST-5 revives it and will have to repair it then.
* **There is no ExUnits or script-size gate.** `scripts/test_exunits.sh` exists in the Belt System
  and not here; `Validators.hs:121-136` has the size functions and nothing invokes them. Worth a
  task before mainnet, not before preprod.
