# Revision history for Decentralized-Belt-System


## 0.3.1.3 -- 2026-02-15

### Memberships (histories & intervals)

- **On-chain**: `MembershipsValidator`, minting redeemers `NewMembershipHistory` / `NewMembershipInterval`, `fcMembershipFee`, oracle integration; protocol types and IDs in `Protocol/Types.hs`, `Protocol/Id.hs`
- **Off-chain**: Domain types `MembershipHistory` / `MembershipInterval`; actions `CreateMembershipHistoryAction`, `AddMembershipIntervalAction`, `AcceptMembershipIntervalAction`; operations `createMembershipHistoryTX`, `addMembershipIntervalTX`, `acceptMembershipIntervalTX`; lookups, interactions, context `membershipsValidatorHashAndRef`, validators `membershipsValidatorGY`, `deployReferenceScripts`, exceptions, functors, datum parser
- **Storage & ingestion**: `MembershipHistoryProjection` / `MembershipIntervalProjection`, put/rollback; `MembershipHistoryEvent` / `MembershipIntervalEvent`, `projectChainEvent` for memberships validator
- **Query API**: REST endpoints for membership histories and intervals (list/count, filters, ordering) in `RestAPI.hs`, `Query/Projected.hs`
- **Admin CLI**: `CreateMembershipHistory`, `AddMembershipInterval`, `AcceptMembershipInterval`; `verifyDeployedScriptsAreReady` includes memberships validator
- **Export & config**: `exportValidators` writes memberships validator; `defaultMembershipsValidatorFile` / `exportMembershipsValidator` in Constants and Validators; `config_bjj_validators.json` includes all six fields (placeholders for memberships and oracle)
- **Tests**: Unit tests for create history, accept interval, 3-practitioner interval update
- **Security (LOW)**: Permissionless `Cleanup` redeemer added to ProfilesValidator, RanksValidator, MembershipsValidator (dust/griefing mitigation); `cleanupDustTX`, `CleanupDustAction`, `cleanup-dust` admin CLI; see `OnchainSecurityAudit.md`
- **Docs**: Membership implementation status moved from Developer Guide §6.1 to `docs/to-do-tasks.md`; removed `inconsistencies-todos-consolidated.md`


## 0.3.1.2 -- 2026-02-15

### TxBuilding Exception Refactor & HTTP Status Mapping

- **Exceptions**: Renamed `ProfileException` to `TxBuildingException`; expanded to cover all offchain tx-building failures (RankListEmpty, PromotionNotFound, OracleDatumInvalid, DeployedScriptsNotReady, MultipleUtxosFound, DatumParseError)
- **HTTP Status Mapping**: Added `txBuildingExceptionToHttpStatus` to map exceptions to 404 (not found), 503 (service unavailable / protocol paused), 400 (bad request)
- **Interaction API**: `runWithTxErrorHandling` and `checkDeployedScriptsAreReady` now cast `GYApplicationException` to `TxBuildingException` and return appropriate HTTP status codes instead of always 400
- **Operations**: Added `ensureDeployedScriptsAreReady` (throws `DeployedScriptsNotReady`); interaction API probe now uses it for structured error handling
- **Operations**: `getOracleRefInputSkeleton` now checks `opPaused` and throws `ProtocolPaused` before building profile transactions
- **Query.Projected**: Use `throwIO ProfileNotFound` / `throwIO RankListEmpty` instead of `ioError (userError ...)` for structured exceptions
- **Semantic fixes**: Use `DatumParseError` (not `ProfileNotFound`) when UTxO exists but datum parsing fails in Lookups and Skeletons
- **Admin CLI**: Removed redundant parens around `setFeesParser`
- **Documentation**: Added `docs/DeveloperGuide.md` — comprehensive guide for adding new concepts (onchain types, validators, offchain ops, API, chain sync, tests)


## 0.3.1.1 -- 2026-02-15

### Consistency: use existing wrappers and abstractions

- **Lookups**: Use `profileDatumToProfileData` from `TxBuilding.Functors` in `getPractitionerInformation` and `getOrganizationInformation` instead of inlining `metadataFieldsToProfileData . getMetadataFields`
- **Lookups**: Renamed `getPractiotionerInformation` to `getPractitionerInformation`; updated call sites in `Query.Live` and `TestRuns`
- **Lookups**: Added `getAllParsedDatumsAtValidator` and refactored `getAllOnchainValidRanks` and `getAllOnchainProfiles` to use it
- **Operations**: Unified reader usage to `asks getX` in `createProfileWithRankTX`, `promoteProfileTX`, and `updateOracleTX` (removed `ctx <- ask` + `let x = getX ctx`)
- **Operations**: Removed redundant `$` in single-argument `redeemerFromPlutusData` calls
- **chain-sync**: Use imported `fromMaybe` instead of `Data.Maybe.fromMaybe` for config loading
- **Storage**: Added generic `upsertByUnique` and refactored `putCursor`, `putKupoMatch`, `putRankProjection`, `putProfileProjection`, and `putPromotionProjection` to use it


## 0.3.1.0 -- 2026-02-15

### Codebase Consistency & Cleanup

- Relaxed monad constraints on `txMustLockStateWithInlineDatumAndValue`, `txIsPayingValueToAddress`, and `txIsPayingValueToAddressWithInlineDatum` from `GYTxUserQueryMonad` to `GYTxQueryMonad` (they only use `scriptAddress`/`return`/`mustHaveOutput`)
- Extracted `txOutRefToV3Plutus` into `TxBuilding.Utils` and replaced all 5 inline V1-to-V3 TxOutRef conversions across Operations, Skeletons, Transactions, and TestRuns
- Admin CLI now uses `Constants.defaultAtlasCoreConfig` and `Constants.defaultTxBuldingContextFile` instead of hardcoded local paths
- Admin CLI now uses `decodeConfigEnvOrFile` for config loading, supporting `DEPLOYED_VALIDATORS_CONFIG` and `ATLAS_CORE_CONFIG` env vars (consistent with all other executables)
- Chain sync service now reads network ID from `GYCoreConfig` instead of hardcoding `GYTestnetPreview`
- `updateOracleTX` now uses `txMustLockStateWithInlineDatumAndValue` for re-locking oracle datum (consistent with all other operations)
- Added `txMustSpendFromRefScriptWithKnownDatum` skeleton helper for spending UTxOs with pre-resolved refs/datums; used in `updateOracleTX`
- `getFeeSkeleton` now uses `txIsPayingValueToAddress` instead of manual output construction
- `mintOracleNFTAndLockDatum` and `mintTestOracleNFT` now use `txMustLockStateWithInlineDatumAndValue` instead of manual output construction
- `queryOracleParams` now uses `getUtxoWithTokenAtAddresses` and `getInlineDatumAndValue` instead of manual UTxO filtering and datum parsing
- Added `getNetworkId :: ProviderCtx -> GYNetworkId` helper; replaced 7+ occurrences of `cfgNetworkId . ctxCoreCfg`
- Extracted `pkhFromExtendedSkey` into `TxBuilding.Utils`; `addressFromPaymentSigningKey` and `pkhFromSkey` now share the same key derivation logic
- `getProfileRanks` now uses the `extra` accessor instead of pattern-matching `CIP68Datum` internals
- Standardized redeemer construction to use `redeemerFromPlutusData` everywhere (replaced `redeemerFromPlutus' . toBuiltinData`)
- Fixed malicious AcceptPromotion test to use `PromotionAcceptance` redeemer instead of `unitRedeemer` so the test isolates the intended vulnerability
- Replaced local `whenJust` in `Query.Projected` with `for_` from `Data.Foldable`; removed dead `whenJust` from `Query.Common`
- Added `getMintingPolicyHash` getter to `TxBuilding.Context`; chain-sync now uses it instead of destructuring the tuple directly
- Extracted `runWithTxErrorHandling` helper in `InteractionAppMonad` to deduplicate the try/catch/log/throwError pattern
- Added `appVersion` constant to `Constants.hs`; replaced hardcoded `"1.0.0"` in `InteractionAppMonad` and `QueryAppMonad` probe responses
- Removed dead code: `txIsPayingValueToAddressWithInlineDatum`, `isValidBetween`, `safeEraTime`, `txIsValidForSafeEra` from Skeletons; `getAdaBalance`/`getValueBalance` from Utils; annotated `txMustBurnCIP68UserAndRef` as reserved for future use


## 0.3.0.0 -- 2026-02-15

### Oracle Hub for Parameters

Implemented a dynamic protocol parameter system via an on-chain oracle, enabling runtime configuration changes (pause, fees, min lovelace, admin rotation) without redeploying validators.

#### New On-chain Components
- **`OracleValidator`** — Unparameterized spending validator guarding the oracle UTxO; enforces admin signature and value preservation
- **`OracleNFTPolicy`** — One-shot minting policy for the unique oracle NFT identifier
- **`OracleParams` / `FeeConfig`** — New data types for mutable operational parameters (admin PKH, pause flag, fee configuration, min output lovelace)

#### Core Design Decision
BJJ rules, belt hierarchy, and metadata limits remain hardcoded (domain invariants). Only runtime-operational parameters are oracle-ized: pause, fees, min lovelace, and admin identity.

#### MintingPolicy Oracle Integration
- MintingPolicy now reads `OracleParams` from a reference input at runtime
- Enforces global pause gate (`opPaused`), fee payments (`checkFee`), and minimum output lovelace
- `ProtocolParams` extended with `oracleToken :: AssetClass` field

#### Off-chain Architecture: AdminAction Pipeline
- **`AdminActionType`** added to `DomainTypes.Core.Actions` with constructors: `PauseProtocolAction`, `UnpauseProtocolAction`, `SetFeesAction`, `SetMinLovelaceAction`
- **`ActionType`** extended from newtype to sum type: `ProfileAction | AdminAction`
- **`updateOracleTX`** in `Operations.hs` — skeleton-based oracle update with `mustBeSignedBy` for admin signature
- **`interactionToTxSkeleton`** returns `Maybe GYAssetClass` (profile actions return `Just`, admin actions return `Nothing`)
- Admin CLI commands (`pause-protocol`, `unpause-protocol`, `set-fees`, `query-oracle`) now route through the shared `Interaction` pipeline via `runBJJActionWithPK`
- Removed standalone `updateOracleParams` from `Transactions.hs`

#### Multi-step Deployment Flow
```
1. Deploy OracleValidator (reference script)
2. Mint Oracle NFT + lock initial OracleParams
3. Deploy ProfilesValidator, RanksValidator (reference scripts)
4. Compile MintingPolicy with oracle AssetClass, deploy (reference script)
```

#### Tests
- **Oracle admin unit tests**: pause/unpause, set/clear fees, update min lovelace, sequential admin actions interleaved with profile interactions
- **Serialization roundtrip property tests**: Added 18 new tests for all datum types (BJJBelt, BeltSnapshot, OnchainProfile, OnchainRank, membership types, CIP-68 types, NodeDatum)
- **`Eq` instances** added to 10 on-chain types for test assertions
- **Real admin PKH** in test oracle (derived from wallet address via `addressToPlutus`)

#### Documentation & Scripts
- Updated `OnchainArchitecture.md` with Oracle Hub section, deployment order, security model
- Updated `to-do-tasks.md` with oracle completion status
- Updated CIP-57 blueprint with `OracleValidator` and new type definitions
- Updated `populate_testnet.sh`, `test_black_promotes_white_to_blue.sh` with admin action steps
- Updated `test_exunits.sh` with Type/Action columns and ASCII table rendering

#### Cabal Dependencies
- Added `plutus-tx` to test suite, `mtl` to admin executable

---

## 0.2.9.0 -- 2026-02-15

### Memberships Onchain & Code Quality

#### New: Memberships Validator & Protocol Logic (Onchain)

Extended the onchain library with the full memberships system, including new modules and refactored protocol logic.

**New Modules**:
- `Onchain.Protocol.Types` — Core on-chain data types extracted from `Protocol.hs`
- `Onchain.Protocol.Lookup` — Typed datum lookup helpers
- `Onchain.Protocol.Id` — Asset class / ID generation functions

**Protocol.hs Refactor**:
- Split monolithic `Protocol.hs` into `Protocol/Types.hs`, `Protocol/Lookup.hs`, and `Protocol/Id.hs`
- `Protocol.hs` now re-exports the sub-modules and contains business logic only

#### Code Quality: Haskell Style Guide Compliance

Applied the project `HaskellStyleGuide.md` to all `onchain-lib` modules:

**INLINEABLE Pragma Placement**:
- Moved `{-# INLINEABLE #-}` pragmas from after definitions to before type signatures across `CIP68.hs` and other modules

**Haddock Comments**:
- Added module-level Haddock comments to all validators, `BJJ.hs`, `Blueprint.hs`, and `Utils.hs`
- Fixed triple-dash `--- ^` to standard `-- ^` for record field comments in `CIP68.hs`
- Converted line-above `-- |` to inline `-- ^` for record fields in `Protocol/Types.hs`

**Explicit Export Lists**:
- Added explicit export lists with Haddock section headings to: `BJJ.hs`, `Blueprint.hs`, `CIP68.hs`, `Utils.hs`, `MintingPolicy.hs`, `ProfilesValidator.hs`, `RanksValidator.hs`, `MembershipsValidator.hs`

**Blank Line Cleanup**:
- Collapsed multiple consecutive blank lines to a single blank line in `CIP68.hs`

**Error Message Convention**:
- Standardised all `traceError` / `traceIfFalse` messages to the pattern `"Cannot <verb>: <reason>"` or `"Invalid <noun>: <reason>"` across `Protocol.hs`, `Utils.hs`, `LinkedList.hs`, `Protocol/Lookup.hs`

**Refactoring**:
- Introduced `BeltSnapshot` record type in `BJJ.hs` bundling `BJJBelt` + `POSIXTime`, replacing the error-prone 6-argument `validatePromotion` signature with 3 `BeltSnapshot` arguments
- Updated call site in `MintingPolicy.hs` and property tests in `BJJPropertyTests.hs`

**Style Guide Update**:
- Removed the "Trailing commas: use leading-comma style" rule from `HaskellStyleGuide.md` (the project already uses trailing-comma style consistently)

---

## 0.2.8.2 -- 2026-02-10

### Script Size Reporting in Test Output

- **Added** `compiledCodeSize` helper and per-validator size constants (`mintingPolicySize`, `profilesValidatorSize`, `ranksValidatorSize`, `membershipsValidatorSize`) to `Validators.hs` using `SBS.length . serialiseCompiledCode` for flat-encoded byte counts
- **Added** `SCRIPT_SIZES` logging in `TestRuns.hs` `deployBJJValidators` so script sizes appear in `cabal test` output
- **Updated** `scripts/test_exunits.sh` to parse `SCRIPT_SIZES` from test output and display a color-coded table with absolute size (bytes) and percentage of the 16 KB max transaction size limit

## 0.2.8.1 -- 2026-02-10

### Code Quality, Blueprint CLI & Documentation Fixes

#### Admin CLI: `write-blueprint` Command
- **Added** `write-blueprint` command to the admin CLI for generating the CIP-57 contract blueprint JSON
- Outputs to `docs/bjj-belt-system-blueprint.json` by default; customizable via `--output`/`-o` flag
- Does not require blockchain providers or signing keys — runs purely locally
- Refactored `main` to separate offline commands (`write-blueprint`) from provider-dependent commands

#### Blueprint.hs Overhaul
- **Added** `MembershipsValidator` blueprint (was entirely missing from CIP-57 contract blueprint)
- **Added** `RanksRedeemer` and `MembershipsRedeemer` to `contractDefinitions` type list
- **Added** `MembershipDatum` to `contractDefinitions` type list
- **Fixed** `RanksRedeemer` now derives `HasBlueprintDefinition`; blueprint uses `definitionRef @RanksRedeemer` instead of `definitionRef @()`
- **Fixed** stale descriptions: removed references to deleted features (`BurnProfileId`, `DeleteProfile`), updated preamble to mention four validators, corrected all validator descriptions to reflect current architecture

#### Bug Fixes
- **Fixed** `unsafeGetProfileDatumAndValue` in `Protocol.hs`: first parameter type annotation changed from `RankId` to `ProfileId` (semantic type mismatch — both are `AssetClass` so no runtime impact, but the type was misleading)
- **Fixed** typo in `MintingPolicy.hs`: `"Must promotion validation rules "` → `"Must pass promotion validation rules"`
- **Fixed** double-space typo in `MintingPolicy.hs`: `"Tx must mint JUST interval  NFT"` → `"Tx must mint JUST interval NFT"`
- **Fixed** double-space typo in `MembershipsValidator.hs`: same `"interval  NFT"` pattern

#### Documentation Fix
- **Corrected** `OnchainSecurityAudit.md` R5 analysis: MV **does** independently check organization User NFT (derived from on-chain datum, L105-106 and L126-127), contrary to previous claim that it did not. Updated R5, R6 explanations and summary table to accurately describe the mint check's purpose: forcing the correct MP to run for `startDate` validation and token integrity, not org authorization

#### Dead Code Removed
- **Removed** `deriveRefFromUserTN` and `deriveRefFromUserAC` from `CIP68.hs` (unused anywhere in codebase)
- **Removed** `initEmptyNodeDatum` from `LinkedList.hs` (unused anywhere in codebase)

---

## 0.2.8.0 -- 2026-02-09

### Memberships System & Security Audit

#### New: Membership Management (Onchain)

Implemented the complete onchain membership system for tracking practitioner-organization relationships over time.

**New Modules**:
- `Onchain.LinkedList` — Generic sorted linked list for on-chain state
- `Onchain.MembershipsValidator` — Validator for membership histories, intervals, and acceptance

**New Data Types** (in `Protocol.hs`):
- `OnchainMembershipHistory` — Tracks a practitioner's membership at an organization
- `OnchainMembershipInterval` — Individual time-bounded membership period
- `MembershipHistoriesListNode` — Linked list node wrapper with organization scoping
- `MembershipDatum` — Sum type (`ListNodeDatum` | `IntervalDatum`) for the MembershipsValidator

**New MintingPolicy Redeemers**:
- `NewMembershipHistory` — Creates a new membership history (linked list node + first interval)
- `NewMembershipInterval` — Adds a new interval to an existing membership history

**New MembershipsValidator Redeemers**:
- `InsertNodeToMHList` — Inserts a membership history node into the sorted linked list
- `UpdateNodeInMHList` — Updates a membership history node (adds new interval to chain)
- `AcceptInterval` — Practitioner accepts/acknowledges a membership interval

**Transaction Flows**:
| Flow | Authorization | What Happens |
|------|--------------|--------------|
| InitMembershipHistory | Org User NFT | Root created atomically with org profile; new history node inserted into list |
| AddToMembershipHistory | Org User NFT | New interval prepended to existing history |
| AcceptMembershipInterval | Practitioner User NFT | Practitioner acknowledges the interval |

#### Security Audit

Comprehensive security audit documented in `docs/OnchainSecurityAudit.md`.

**Issues Found and Resolved**:

| Severity | Issue | Status |
|----------|-------|--------|
| HIGH | Cross-Organization Membership Manipulation — Org A could modify Org B's membership data | **Fixed** (MV now independently checks Org User NFT from on-chain datum) |
| MEDIUM | Missing CurrencySymbol validation for `practitionerId` in `NewMembershipHistory` | **Fixed** |
| MEDIUM | No `startDate` validation against transaction validity range | **Fixed** |

**Remaining LOW items**: `UpdateEndDate` time validation (not yet implemented), `endDate` validation on creation, dust/griefing mitigation (admin cleanup TODO), MV redeemer data integrity.

#### Cross-Validator Redundancy Analysis

Analyzed 7 cross-validator redundancies. 3 safely removed, 4 essential and retained.

**Removed (execution cost savings)**:
- **R2**: ProfilesValidator rank output check in `AcceptPromotion` — delegated to RanksValidator (only one redeemer, always validates). Also removed `rankOutputIdx` from PV's `AcceptPromotion` redeemer.
- **R3**: RanksValidator profile Ref NFT `== 1` check — already guaranteed by `checkAndGetCurrentStateDatumAndValue` `geq` filter + mint uniqueness
- **R7**: MintingPolicy `isCorrectOrganization` in `NewMembershipInterval` — MV's `orgUserAC` check is strictly stronger

**Essential (must keep)**:
- **R1**: RanksValidator profile output check — prevents "wasted promotion" attack via `UpdateProfileImage` redeemer
- **R4**: MembershipsValidator exact mint checks — prevents rogue MintingPolicy bypass
- **R5**: `addMembershipIntervalToHistory` in both MP and MV — each script needs different parts of the result
- **R6**: `validLastInterval` — prevents validation against arbitrary historical intervals

#### Redeemer Changes (Breaking)

**ProfilesRedeemer**:
- `AcceptPromotion`: Removed `rankOutputIdx` parameter (R2 — rank output delegated to RV)

**New MintingRedeemer constructors**:
- `NewMembershipHistory organizationProfileId practitionerId startDate endDate leftNodeId firstIntervalOutputIdx`
- `NewMembershipInterval organizationProfileId membershipNodeId startDate endDate intervalOutputIdx`

#### R4 Optimization: `promoteProfile` Restructured

Restructured `promoteProfile` into two separate functions to reduce ProfilesValidator script cost:
- **`promoteProfileDatum`** (new) — lightweight function that only updates the profile's `currentRank` pointer without constructing the full 7-field Rank record. Used by ProfilesValidator.
- **`promoteProfile`** (retained) — combined function returning both updated profile datum and accepted rank. Used by RanksValidator and off-chain code.

This avoids unnecessary Rank record construction in PV, which only needs the updated profile datum (rank output validation is handled by RV — see R2).

#### Other Changes
- Updated `OnchainArchitecture.md` with membership system design
- Updated `CIP68.hs` with new derivation functions
- Updated `Utils.hs` with new helper functions (`hasTxInAtAddressWithNFT`, `mintValueMinted`)
- Updated `.cabal` file with new modules

---

## 0.2.7.0 -- 2026-02-01

### Output Index Optimization

#### Performance Improvement

Implemented O(1) output validation by passing output indices in redeemers instead of O(n) search through all transaction outputs.

**Changed Functions**:
- `checkTxOutAtIndex` added to `Onchain.Utils` - validates output at specific index
- All validators now use indexed lookup instead of `hasTxOutWithInlineDatumAndValue`

**Before** (O(n) search):
```haskell
hasTxOutWithInlineDatumAndValue datum value address txInfoOutputs
```

**After** (O(1) indexed lookup):
```haskell
checkTxOutAtIndex outputIdx datum value address txInfoOutputs
```

#### Redeemer Changes (Breaking)

**MintingRedeemer**:
- `CreateProfile`: Added `profileOutputIdx` and `rankOutputIdx` parameters
- `Promote`: Added `pendingRankOutputIdx` parameter

**ProfilesRedeemer**:
- `UpdateProfileImage`: Added `profileOutputIdx` parameter
- `AcceptPromotion`: Added `profileOutputIdx` and `rankOutputIdx` parameters

#### Off-chain Updates

Transaction builders in `Operations.hs` now:
- Track output indices with clear comments
- Include indices in redeemers
- Document expected output order in skeleton construction

**Output Index Conventions**:
| Transaction | Output 0 | Output 1 | Output 2 |
|-------------|----------|----------|----------|
| CreateProfile (Practitioner) | Profile state | User NFT | Rank state |
| CreateProfile (Organization) | Profile state | User NFT | - |
| UpdateProfile | Updated profile | - | - |
| Promote | Pending rank | - | - |
| AcceptPromotion | Updated profile | Updated rank | - |

#### Benefits

| Aspect | Before | After |
|--------|--------|-------|
| Time complexity | O(n) per output check | O(1) per output check |
| Execution units | Higher | Lower |

**Note**: This is a **breaking change** for redeemer structure. Existing on-chain scripts need redeployment to use the new optimized validation.

---

## 0.2.6.0 -- 2026-02-01

### PlutusV3 Optimizations

#### Token ID Generation Optimization

Improved efficiency of token ID generation functions by leveraging PlutusV3-native builtins.

**Changed Functions**:
- `nameFromTxOutRef` in `Onchain.Utils`
- `generateRankId` in `Onchain.Protocol`

**Before** (PlutusV2 workaround):
```haskell
takeByteString 28 $ blake2b_256 (bs <> (serialiseData . toBuiltinData) i)
```

**After** (PlutusV3 native):
```haskell
blake2b_224 (bs <> integerToByteString BigEndian 0 i)
```

**Benefits**:
| Aspect | Old Approach | New Approach |
|--------|-------------|--------------|
| Hash function | `blake2b_256` + truncate | `blake2b_224` (native 28-byte) |
| Integer encoding | CBOR via `serialiseData . toBuiltinData` | Raw bytes via `integerToByteString` |
| Overhead | Two operations + truncation | Single builtin each |
| Execution units | Higher (extra computation discarded) | Lower (direct output) |

**Note**: This is a **breaking change** for token name generation. Existing on-chain tokens are not affected, but new tokens will have different names for the same inputs compared to the old algorithm.

---

### Testnet Scripts & Documentation

#### New Testnet Population Script

Added `scripts/populate_testnet.sh` - a comprehensive script to populate the testnet with realistic sample data for testing and demonstrations.

**Profiles Created**:
| Type | Name | Belt Level |
|------|------|------------|
| Organization | Gracie Barra Academy | N/A |
| Organization | Alliance Jiu-Jitsu | N/A |
| Grand Master | Grand Master Helio | Red (9th degree) |
| Master | Master Ricardo Silva | Black → Black1 |
| Master | Master Ana Santos | Black |
| Student | John Martinez | White → Blue → Purple |
| Student | Maria Garcia | White → Blue |
| Student | Carlos Oliveira | White |
| Student | Emma Thompson | White |

**Promotion Scenarios**:
- John's Journey: White → Blue → Purple (promoted by Master Ricardo)
- Maria's Progress: White → Blue (promoted by Master Ana)
- Master Degree: Black → Black1 (Master Ricardo promoted by Grand Master)

#### Script Improvements

Both test scripts (`populate_testnet.sh` and `test_black_promotes_white_to_blue.sh`) now include:
- **Helper functions** with proper error handling and output suppression
- **Error detection** for common issues (missing reference scripts, deployment failures)
- **Clean output** - verbose CLI messages are suppressed, only relevant info shown
- **Colored progress indicators** with section headers
- **Detailed summaries** with all created profile/promotion IDs

#### Documentation Updates

- **README.md**: Updated CLI help (removed deprecated `delete-profile` command), added testnet scripts section
- **docs/Documentation.md**: Updated backend component descriptions
- **docs/OnchainArchitecture.md**: Already up-to-date from v0.2.5.0

---

## 0.2.5.0 -- 2026-02-01

### Security Review & Architectural Changes

This release addresses issues discovered during a comprehensive security review and implements important architectural decisions.

#### Profile Deletion Removed (Breaking Change)

**Decision**: Profile deletion functionality has been intentionally removed from the protocol.

**Rationale**:
- BJJ belt records are permanent historical facts that should not be erasable
- Lineage integrity requires that promotion relationships remain verifiable
- If a master deletes their profile, all promotions they granted become unverifiable
- Immutability aligns with blockchain's core value proposition

**Changes**:
- Removed `DeleteProfile` redeemer from `ProfilesValidator`
- Removed `BurnProfileId` redeemer from `MintingPolicy`
- Removed `DeleteProfileAction` from offchain API
- Removed `deleteProfileTX` from transaction builders

#### ProfilesValidator Optimization

**AcceptPromotion - Redundant Check Removed**
- **Original Issue**: Validator checked `promotionAwardedTo` (Ref AC) instead of deriving User AC
- **Analysis**: RanksValidator already correctly checks User NFT consent via `deriveUserFromRefAC`
- **Resolution**: Removed redundant check since RanksValidator guarantees user consent
- **Rationale**: AcceptPromotion must spend Promotion UTxO → RanksValidator always runs → consent verified
- **Benefit**: Smaller script size, lower execution costs

#### Metadata Size Limits (CIP68.hs)

**Unbounded Metadata Vulnerability**
- **Bug**: No size limits on CIP68 metadata fields allowed DoS via oversized datums
- **Fix**: Added per-field size validation

| Field | Max Size | Rationale |
|-------|----------|-----------|
| `name` | 128 bytes | Full names with titles, accents, academy |
| `description` | 1024 bytes | Detailed bio with achievements and lineage |
| `imageURI` | 256 bytes | IPFS/Arweave/HTTPS URLs |

- **Applied to**: `MintingPolicy` (CreateProfile) and `ProfilesValidator` (UpdateProfileImage)

#### New Validation Functions
- `validateMetadataFields` - Validates all metadata fields for profile creation
- `validateImageURI` - Validates image URI size for profile updates

#### Tests
- **Malicious AcceptPromotion Test**: Constructs attack transaction without User NFT to verify RanksValidator protection

#### Summary

| Change | Type | Impact |
|--------|------|--------|
| Profile deletion removed | Breaking | Profiles are now permanent (by design) |
| Metadata size limits | Security | Prevents DoS via oversized datums |
| AcceptPromotion optimized | Optimization | Smaller script, lower costs |


## 0.2.4.0 -- 2025-02-01

### Promotion Security Enhancements

**Comprehensive Promotion Validation Overhaul**

This release significantly improves the security and robustness of the promotion system by implementing a two-layer validation architecture.

#### Minting Policy Enhancements
- **Full BJJ Rule Validation at Mint Time**: Promotions are now fully validated when created, not just when accepted
- **Seed TxOutRef for Uniqueness**: Each promotion now uses a consumed TxOutRef as a seed, guaranteeing unique promotion IDs
- **Reference Input Validation**: Minting policy now references student and master profiles/ranks to validate promotion rules
- **New `generatePromotionRankId` Function**: Creates unique promotion IDs from seed TxOutRef

#### ProfilesValidator Enhancements
- **Acceptance-Time State Validation**: Added checks to prevent invalid acceptance scenarios:
  - `nextBelt > currentBelt` - Prevents double-acceptance of same-rank promotions
  - `nextBeltDate > currentBeltDate` - Prevents out-of-order date acceptance
- **Current Rank Reference Input**: Now references the student's current rank to validate state

#### RanksValidator Simplification
- **Consent-Only Validation**: Simplified to only verify student spends their User NFT
- **Reduced Script Size**: Removed redundant validation (now done at mint time)
- **Lower Execution Costs**: Fewer reference inputs and simpler logic

#### Protocol Changes
- **Updated `Promote` Redeemer**: Now includes `TxOutRef` seed parameter for uniqueness
- **Updated `mkPendingRank`**: Now takes seed TxOutRef and CurrencySymbol parameters
- **New `generatePromotionRankId`**: Generates unique promotion IDs from seed

#### Security Improvements
| Attack Vector | Prevention Mechanism |
|--------------|---------------------|
| Invalid promotion creation | Full validation at mint time |
| Token ID collisions | Seed TxOutRef guarantees uniqueness |
| Double-acceptance (same rank) | `nextBelt > currentBelt` check |
| Out-of-order acceptance | `nextBeltDate > currentBeltDate` check |

#### Documentation Updates
- **OnchainArchitecture.md**: Complete rewrite with new security model documentation
- **New Sections**: Promotion Flow, Security Model, Attack Prevention tables

#### Test Updates
- **New Security Tests**: Added tests for multiple master promotion scenarios
- **Sequential Promotion Tests**: Verify correct promotion flow with date ordering


## 0.2.3.0 -- 2025-01-06

### Query API Enhancements

**Extended Profiles and Promotions APIs with Count and Frequency Endpoints**

#### Profiles API Extensions
- **GET /profiles/count**: Get total count of profiles with optional `profile_type` filter
- **GET /profiles/frequency**: Get profile counts grouped by type (`Practitioner`, `Organization`)

#### Promotions API Extensions  
- **GET /promotions/count**: Get total count of promotions with optional filters (`profile`, `belt`, `achieved_by`, `awarded_by`)
- **GET /promotions/frequency**: Get promotion counts grouped by belt type

#### Implementation Details
- Added `getProfileTypeTotals` to `Query/Projected.hs` and `Query/Live.hs`
- Added `getPromotionBeltTotals` to `Query/Projected.hs` and `Query/Live.hs`
- All new endpoints support `liveprojection` query flag for live blockchain vs projected database queries
- Follows the same pattern as existing Belts API Count and Frequency endpoints

## 0.2.2.0 -- 2024-12-21

### API Architecture Split

**Service Separation for Better Scalability**

#### API Split Implementation
- **Interaction API Service**: Dedicated service for transaction building and submission (`build-tx`, `submit-tx`)
- **Query API Service**: Dedicated service for data queries (profiles, promotions, belts)
- **Independent Deployment**: Each service can be deployed and scaled independently
- **Docker Support**: Separate Dockerfiles for each service (`Dockerfile.interaction-api`, `Dockerfile.query-api`)

#### Infrastructure Updates
- **Nginx Configuration**: Updated routing to direct requests to appropriate services
- **Docker Compose**: Updated to orchestrate both services independently
- **Build Scripts**: Updated to build and deploy both services separately

#### Documentation Updates
- **Architecture Documentation**: Updated to reflect split service architecture
- **API Documentation**: Service-specific Swagger documentation for each API

## 0.2.1.0 -- 2024-12-20

### Milestone 2 Enhancements & Feedback Response

**Additional Evidence and Improvements**

#### Testnet Deployment Evidence
- **Testnet Transactions**: `test_evidence_preview.txt` - Successful testnet transaction execution
- **Automated Test Script**: `test_black_promotes_white_to_blue.sh` - Self-service test script for reviewers

#### Documentation Updates
- **Enhanced README**: Complete installation, setup, and usage instructions


#### Admin Command Line Tools
- **Deployment Commands**: admin cli


## 0.2.0.0 -- 2024-12-19

### Milestone 2 - Smart Contract Architecture ✅ COMPLETED

**Project Catalyst F13 - ID: #1300081**

Milestone 2 has been successfully completed with all deliverables delivered and acceptance criteria met. The project has delivered a complete on-chain smart contract architecture for managing BJJ practitioner profiles and rank promotions on the Cardano blockchain.

#### Deliverables Evidence

**1. Complete Smart Contract Architecture** ✅
- **Blueprint Definition**: `src/lib/Onchain/Blueprint.hs` 
- **Contract Specification**: `bjj-belt-system-blueprint.json`
- **Core Validators**: 
  - `src/lib/Onchain/MintingPolicy.hs` 
  - `src/lib/Onchain/ProfilesValidator.hs` 
  - `src/lib/Onchain/RanksValidator.hs` 
- **BJJ Logic**: `src/lib/Onchain/BJJ.hs` 

**2. Transaction Building Infrastructure** ✅
- **Core Operations**: `src/lib/TxBuilding/Operations.hs`  
- **Interaction Layer**: `src/lib/TxBuilding/Interactions.hs`  
- **Supporting Services**: `src/lib/TxBuilding/Lookups.hs`, `src/lib/TxBuilding/Skeletons.hs` 

**3. Comprehensive Testing Suite** ✅
- **Unit Tests**: `src/test/UnitTests.hs`
- **Property Tests**: `src/test/BJJPropertyTests.hs` 
- **Tests Evidence**: `tests_evidence.txt` 

**4. Complete Documentation** ✅
- **Technical Documentation**: `docs/Documentation.md`  
- **Architecture Documentation**: `docs/OnchainArchitecture.md`
- **Visual Documentation**: `puml/` 
- **Project Overview**: `README.md`

#### Acceptance Criteria Evidence

- **AC1: Smart Contract Completeness** ✅ - All three validators implemented with BJJ-specific logic
- **AC2: Transaction Building Completeness** ✅ - Full operation set for profiles, ranks, and promotions  
- **AC3: Testing Completeness** ✅ - Unit and property tests covering all core functionality
- **AC4: Documentation Completeness** ✅ - Complete technical specs and user documentation
- **AC5: Blueprint Integration** ✅ - Contract blueprint with validator specifications

#### Repository Structure Evidence

```
src/
├── lib/               # Core library modules
│   ├── Onchain/      # Smart contract implementations
│   ├── TxBuilding/   # Transaction building infrastructure  
│   └── DomainTypes/  # Domain type definitions
├── exe/              # Executable applications
│   ├── admin/        # Command-line admin tool
│   ├── interaction-api/  # Transaction building and submission API
│   └── query-api/    # Data querying API
└── test/             # Comprehensive testing suite
docs/                 # Complete documentation
puml/                 # Visual documentation diagrams
bjj-belt-system-blueprint.json  # Contract specification
```

## 0.1.0.0 -- YYYY-mm-dd

* First version. Released on an unsuspecting world.
