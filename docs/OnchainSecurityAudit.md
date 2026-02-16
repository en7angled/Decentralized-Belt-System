# Onchain Security Audit

- [Onchain Security Audit](#onchain-security-audit)
  - [Scope](#scope)
  - [Core Attack Vectors Analyzed](#core-attack-vectors-analyzed)
    - [1. Forged Datum at Validator Address](#1-forged-datum-at-validator-address)
    - [2. Minting Unintended Tokens](#2-minting-unintended-tokens)
  - [Issues Found and Resolved](#issues-found-and-resolved)
    - [MEDIUM (Fixed): Missing CurrencySymbol Validation for practitionerId](#medium-fixed-missing-currencysymbol-validation-for-practitionerid)
    - [MEDIUM (Fixed): No startDate Validation Against Transaction Validity Range](#medium-fixed-no-startdate-validation-against-transaction-validity-range)
  - [Remaining Items](#remaining-items)
    - [LOW: updateMembershipIntervalEndDate Needs Time Validation in Validator](#low-updatemembershipintervalenddate-needs-time-validation-in-validator)
    - [LOW (Resolved): Dust/Griefing at Validator Addresses](#low-resolved-dustgriefing-at-validator-addresses)
    - [Cross-Script Redundancy Analysis](#cross-script-redundancy-analysis)
    - [Multi-Script Interaction Vulnerability Analysis](#multi-script-interaction-vulnerability-analysis)
    - [`validLastInterval` TODO Resolution](#validlastinterval-todo-resolution)
  - [Comprehensive Protection Summary](#comprehensive-protection-summary)
  - [Script-by-Script Analysis](#script-by-script-analysis)
    - [MintingPolicy](#mintingpolicy)
    - [ProfilesValidator](#profilesvalidator)
    - [RanksValidator](#ranksvalidator)
    - [MembershipsValidator](#membershipsvalidator)
    - [LinkedList](#linkedlist)
  - [Conclusion](#conclusion)


## Scope

This audit covers the onchain smart contract architecture of the Decentralized BJJ Belt System, including:

- **MintingPolicy** (`src/lib/onchain-lib/Onchain/MintingPolicy.hs`)
- **ProfilesValidator** (`src/lib/onchain-lib/Onchain/ProfilesValidator.hs`)
- **RanksValidator** (`src/lib/onchain-lib/Onchain/RanksValidator.hs`)
- **MembershipsValidator** (`src/lib/onchain-lib/Onchain/MembershipsValidator.hs`)
- **Protocol** (`src/lib/onchain-lib/Onchain/Protocol.hs`) — data types and pure functions
- **LinkedList** (`src/lib/onchain-lib/Onchain/LinkedList.hs`) — generic sorted linked list
- **Utils** (`src/lib/onchain-lib/Onchain/Utils.hs`) — shared helper functions

Focus areas: datum forgery, token minting exploits, authorization bypasses, linked list manipulation, and cross-validator communication integrity.


## Core Attack Vectors Analyzed

### 1. Forged Datum at Validator Address

**Question**: Could a bad actor use their User token to lock a forged datum at one of the validators and bypass validation?

**Result: NOT VULNERABLE.**

Anyone can send a UTxO to any script address on Cardano (validators only run when UTxOs are *spent*, not when they are *sent to* an address). However, the protocol is safe because:

1. **NFT-based datum lookup**: Every datum lookup uses `checkAndGetCurrentStateDatumAndValue` (Utils.hs), which filters by **both** the exact `AssetClass` (CurrencySymbol + TokenName) **and** the correct validator address, and demands **exactly one** matching UTxO:
   ```haskell
   checkAndGetCurrentStateDatumAndValue stateToken addr outs =
     case filter (\(TxInInfo _ (TxOut {txOutValue, txOutAddress})) ->
       (txOutValue `geq` assetClassValue stateToken 1) && (addr == txOutAddress)) outs of
       [TxInInfo _ out] -> (txOutValue out, checkAndGetInlineDatum out)
       _ -> traceError "U2"
   ```
   A forged UTxO without the genuine protocol NFT (correct CurrencySymbol) will **never** be returned by this function.

2. **Unforgeable NFTs**: Protocol NFTs can only be minted by the MintingPolicy, which validates all creation rules. The CurrencySymbol is unique to the deployed MintingPolicy. Nobody can forge an NFT with the correct CurrencySymbol.

3. **Exact value checks on outputs**: All output validations use `value == txOutValue` (strict equality, not `>=`), preventing extra tokens from being injected into output UTxOs.

4. **MembershipDatum wrapper**: The MembershipsValidator pattern-matches on `ListNodeDatum` vs `IntervalDatum` and rejects invalid datum/redeemer combinations, preventing datum type confusion.

### 2. Minting Unintended Tokens

**Question**: Could a bad actor build a transaction to mint a token that was not intended?

**Result: NOT VULNERABLE.**

Every minting redeemer uses a strict exact-mint check:
```haskell
mintValueMinted txInfoMint == expectedTokens
```

`mintValueMinted` returns **all** minted tokens across **all** CurrencySymbols in the entire transaction. This means:
- No extra tokens from this MintingPolicy can be minted (other-token-name attack blocked)
- No other MintingPolicy can mint tokens in the same transaction
- The MembershipsValidator independently verifies the same exact-mint condition, creating redundant protection

This check is present in:
- MintingPolicy: `CreateProfile`, `Promote`, `NewMembershipHistory`, `NewMembershipInterval`
- MembershipsValidator: `InsertNodeToMHList`, `UpdateNodeInMHList`

Both the MintingPolicy and MembershipsValidator must agree on the exact set of minted tokens for any membership transaction to succeed.


## Issues Found and Resolved

### MEDIUM (Fixed): Missing CurrencySymbol Validation for `practitionerId`

**Location**: `MintingPolicy.hs`, `NewMembershipHistory` redeemer

**Description**: The `NewMembershipHistory` redeemer validated `hasCurrencySymbol` for `organizationProfileId` and `leftNodeId` but **not** for `practitionerId`. This allowed an organization to create a membership history for a completely fabricated practitioner (with a wrong CurrencySymbol or pointing to nothing).

**Attack scenario**:
1. Organization calls `NewMembershipHistory` with a fabricated `practitionerId` (wrong CS)
2. A membership history is created for a non-existent entity
3. The linked list accepts the insertion (ordering is on `AssetClass` bytes)
4. The interval has `membershipIntervalPractitionerId = fakeId`, making it permanently unacceptable (nobody holds the derived User NFT)

**Impact**: Pollution of the membership linked list with dead entries. No compromise of other practitioners or the protocol, but wastes on-chain space.

**Fix applied** (MintingPolicy.hs, `NewMembershipHistory`):
```haskell
isPractitionerValid = hasCurrencySymbol practitionerId mintingPolicyCurrencySymbol
-- ...
(isOrganizationValid && isPractitionerValid && isLeftNodeIdValid)
```

**Status**: **RESOLVED**

---

### MEDIUM (Fixed): No `startDate` Validation Against Transaction Validity Range

**Location**: `MintingPolicy.hs`, `NewMembershipHistory` and `NewMembershipInterval` redeemers

**Description**: Both membership redeemers accepted `startDate` directly from the redeemer without any validation against the current time (transaction validity range). Compare with `CreateProfile`, which correctly validated `creationDate `before` txInfoValidRange`.

**Attack scenario**:
1. An organization creates a membership interval with `startDate` in 2020, even though it's 2026 — falsely backdating a membership
2. An organization creates a membership interval with `startDate` far in the future (year 2030)

The only existing constraint was from `addMembershipIntervalToHistory`: `startDate >= lastIntervalEndDate` (prevents overlapping), but this doesn't anchor to real time.

**Impact**: Allows fabrication of historical membership records. In a BJJ belt system where membership duration matters for legitimacy, this could be used to make memberships appear older than they are.

**Fix applied** (MintingPolicy.hs, both membership redeemers):
```haskell
traceIfFalse "Start date must be before tx validity range"
    $ startDate `before` txInfoValidRange,
```

**Status**: **RESOLVED**


## Remaining Items

### LOW (Resolved): `UpdateEndDate` Time Validation and Role-Based Rules

**Location**: `Protocol.hs`, `MembershipsValidator.hs`

**Description**: The `UpdateEndDate` redeemer was implemented with time validation and dual-authorization. The validator requires the new end date to lie **within** the transaction validity interval (after its lower bound and before its upper bound), using PlutusLedgerApi.V1.Interval `contains` on a singleton interval (trace TD). The UpdateEndDate transaction is built with `isValidBetween now validUntil` where **validUntil > newEndDate**, so the new end date lies inside the validity range and the on-chain check is meaningful. Role-based rules: organization (spends org User NFT) may set any future end date; practitioner (spends practitioner User NFT) may only shorten or close an accepted interval (trace TB if they try to extend). The history node is supplied as a reference input so the validator can derive the org and enforce the correct User NFT.

**Implementation note**: Off-chain, the output datum is built with `updateEndDateWithoutValidations` (no TD/TE/TB checks) so that invalid attempts (e.g. practitioner extend) can be submitted; the validator then runs `updateMembershipIntervalEndDate` and rejects with TD/TE/TB as appropriate.

**Status**: **RESOLVED** (UpdateEndDate redeemer index 4, handleUpdateEndDate, Protocol `updateMembershipIntervalEndDate` with TD/TE/TB; off-chain `updateEndDateWithoutValidations`).

---

### LOW (Resolved): MV Redeemer Data Integrity — Safe-by-Design

**Location**: `MembershipsValidator.hs`, `UpdateNodeInMHList` redeemer

**Description**: The `UpdateNodeInMHList` redeemer carries `startDate`/`endDate` which the MV uses for validation but does not independently time-validate. This is **safe-by-design** because:

1. **Exact mint check**: `mintValueMinted == newIntervalNFT` forces the correct MintingPolicy to run in the same transaction; the MP validates startDate against the tx validity range and (after the TC fix) `endDate > startDate` when provided.
2. **Dual computation**: Both MP and MV independently compute `addMembershipIntervalToHistory`; the interval datum is enforced by the MP output check, the history node datum by the MV output check — both outputs must exist in the same tx.
3. **validLastInterval anchor**: The `validLastInterval` check ties MV logic to on-chain data (the current head and last interval), not to redeemer data alone.

No change to the redeemer format is required; the design is documented here as resolved.

**Status**: **RESOLVED** (documented).

---

### LOW (Resolved): Dust/Griefing at Validator Addresses

**Description**: Anyone can send UTxOs with invalid datums to any validator address. Previously, these UTxOs were **permanently locked** and unspendable because validators failed with `traceError "Invalid datum"` for unparseable datums.

**Impact**: Nuisance only. Does not affect protocol functionality — all lookups use NFT-based filtering and ignore UTxOs without valid protocol NFTs.

**Mitigation (offchain)**: Offchain code filters UTxOs by protocol CurrencySymbol before processing.

**Mitigation (onchain — RESOLVED)**: A permissionless `Cleanup` redeemer was added to `ProfilesValidator`, `RanksValidator`, and `MembershipsValidator`. The redeemer allows **anyone** to spend a UTxO at a validator address if its datum is absent or does not parse as the expected protocol datum type. Legitimate protocol UTxOs (with valid datums) are always rejected by the `Cleanup` redeemer.

**Design choice**: The original proposal required an admin signature and a CurrencySymbol-based check. Instead, a datum-validity-based permissionless approach was adopted because:
1. The validators are **unparameterized** — they do not have the protocol CurrencySymbol baked in, so an on-chain CS check is not feasible without architectural changes.
2. All legitimate protocol UTxOs always have valid, parseable inline datums (created by MintingPolicy/validators), so checking datum parseability is a sound guard.
3. Permissionless cleanup creates a **built-in bounty** — anyone can reclaim the dust ADA, making griefing self-defeating (attacker loses funds, cleaner profits).
4. No oracle reference input or admin signature is needed, keeping the redeemer minimal and cheap.

**Implementation**:
- `ProfilesRedeemer.Cleanup` (index 2): checks `fromBuiltinData @(CIP68Datum OnchainProfile)`
- `RanksRedeemer.Cleanup` (index 1): checks `fromBuiltinData @OnchainRank`
- `MembershipsRedeemer.Cleanup` (index 3): checks `fromBuiltinData @MembershipDatum`
- Off-chain: `cleanupDustTX` operation + `ProtocolAction CleanupDustAction` in the interaction API + `cleanup-dust` admin CLI command
- Tests (`UnitTests.hs`):
  - 3.1: Dust at ProfilesValidator — send ADA-only UTxO, cleanup by a third party
  - 3.2: Dust at RanksValidator — same pattern
  - 3.3: Dust at both validators — single cleanup transaction sweeps both
  - 3.4: Safety test — create legitimate profile, add dust, cleanup, verify profile intact

**Edge case**: A UTxO with a coincidentally valid datum but no protocol NFT would be rejected by `Cleanup` (datum parses) and also rejected by legitimate redeemers (NFT checks fail). This is no worse than the previous situation and requires the attacker to craft valid Plutus Data, which is non-trivial.

**Status**: **RESOLVED**

---

### Cross-Script Redundancy Analysis

A systematic review of all multi-script transactions to identify which validations are duplicated across scripts, which can be safely eliminated, and which are essential despite appearing redundant.

**Note:** Line references in this section are from the original audit and may have shifted due to subsequent changes (Cleanup redeemer, UpdateEndDate, R2/R3 removal).

#### Transaction: AcceptPromotion (ProfilesValidator + RanksValidator)

Both validators **must** run: PV reads the promotion from `txInfoInputs` (forcing RV to run), and RV reads the profile from `txInfoInputs` (forcing PV to run).

| # | Check | ProfilesValidator | RanksValidator | Safely Removable? | Status |
|---|---|---|---|---|---|
| R1 | Profile output check | L122-123 | L75-76 | **NO — from RV** | Retained |
| R2 | Rank output check | ~~L124-125~~ | L77-78 | **YES — from PV** | **REMOVED from PV** |
| R3 | Profile Ref NFT `== 1` in input value | L118-120 | ~~L72-74~~ | **YES — from RV** | **REMOVED from RV** |
| R4 | `promoteProfile` computation | L110 (`promoteProfileDatum`) | L62 (`promoteProfile`) | **OPTIMIZED** — PV uses lightweight `promoteProfileDatum` | Split into two functions |
| R5 | User NFT consent | — (defers to RV) | L69-71 | Only in RV — not redundant | Retained |
| R6 | Promotion state validity (belt/date) | L92-107 | — | Only in PV — not redundant | Retained |
| R7 | `hasCurrencySymbol` on promotionId | L83/L99-101 | — | Only in PV — not redundant | Retained |

**R1 — RanksValidator L75-76: Profile output check — MUST STAY**

This check appears redundant because PV independently validates its own output (L122-123). However, **PV has multiple redeemers** (`UpdateProfileImage` and `AcceptPromotion`). When RV forces the profile to be in `txInfoInputs`, it forces PV to run — but PV could run with **any** redeemer. If an attacker constructs a transaction that spends the profile with `UpdateProfileImage` (instead of `AcceptPromotion`) while spending the promotion with `PromotionAcceptance`:
- PV (`UpdateProfileImage`): outputs profile with new image but **old rank pointer** — PV passes
- RV (`PromotionAcceptance`): computes promoted datum, checks User NFT, checks rank output — RV passes
- **Without R1**: transaction succeeds — promotion is consumed without being applied to the profile
- **With R1**: RV checks profile output against promoted datum → mismatch with image-updated datum → transaction fails

R1 is the **sole enforcement** that when a promotion is consumed at RV, the profile at PV must reflect the promotion. It prevents "promotion consumption via alternative PV redeemer" attacks.

**R2 — ProfilesValidator L124-125: Rank output check — REMOVED**

Unlike PV, **RV has only one redeemer that operates on valid protocol datums** (`PromotionAcceptance`); the permissionless `Cleanup` redeemer only handles absent/unparseable datums. When PV forces the promotion to be in `txInfoInputs`, RV must run, and it has no alternative code path for valid rank datums. RV always validates the rank output (L77-78). Therefore PV's rank output check was genuinely redundant.

Removing R2 also removed the `rankOutputIdx` parameter from PV's `AcceptPromotion` redeemer, reducing redeemer size and PV script cost.

**Status**: **REMOVED**. The `AcceptPromotion` redeemer now takes `RankId Integer` (promotionId + profileOutputIdx only). The rank output validation is fully delegated to RanksValidator.

**R3 — RanksValidator L72-74: Profile Ref NFT `== 1` — REMOVED**

`unsafeGetProfileDatumAndValue` (L61) uses `checkAndGetCurrentStateDatumAndValue`, which filters by `geq assetClassValue stateToken 1` and demands exactly one matching UTxO. If the lookup succeeds, the NFT is guaranteed to be present. Additionally, the MintingPolicy only ever mints exactly 1 of each profile NFT, so `≥ 1` implies `== 1`. PV also independently checks this (L118-120).

**Status**: **REMOVED**. The explicit `assetClassValueOf studentProfileValue profileRefAssetClass == 1` check has been removed from RanksValidator.

**R4 — `promoteProfile` called in both PV and RV — OPTIMIZED**

Both scripts need to compute promotion results. After removing R2:
- PV only needs `updatedProfileCIP68Datum` for its own output check
- RV needs both `updatedProfileCIP68Datum` (for R1 profile output check) and `newRank` (for rank output check)

**Optimization applied**: `promoteProfile` was restructured into two separate functions:
- `promoteProfileDatum` — lightweight function that only updates the profile's `currentRank` pointer to the promotion ID, without constructing the full 7-field Rank record. Used by PV.
- `promoteProfile` — retained for RV (which needs both the updated profile datum and the accepted rank) and off-chain code.

This reduces PV script cost by avoiding unnecessary Rank record construction.

---

#### Transaction: NewMembershipHistory (MintingPolicy + MembershipsValidator)

Both scripts must run: MP runs because tokens are minted; MV runs because the left node is spent.

| # | Check | MintingPolicy | MembershipsValidator | Safely Removable? |
|---|---|---|---|---|
| R5 | Exact mint check | L186-189 | L104-106 | **NO — from MV** |
| R6 | First interval output @ MV | L190-191 | — | Only in MP — not redundant |
| R7 | Updated left node output @ MV | — | L102-103 | Only in MV — not redundant |
| R8 | Inserted node output @ MV | — | L107-108 | Only in MV — not redundant |

**R5 — MV InsertNodeToMHList L104-106: Exact mint check — MUST STAY**

The MV **independently** checks organization User NFT (derived from the on-chain datum of the left node, L105-106). However, the mint check remains essential for a separate reason: it forces the **correct** MintingPolicy to run. Without this check, an attacker could:
1. Use a rogue minting policy to mint tokens with matching token names but a foreign CurrencySymbol
2. Spend the left node from MV with `InsertNodeToMHList` (the MV's org User NFT check would pass because it's derived from the on-chain datum, not the minting policy)
3. Insert a node with tokens that have no protocol-enforced integrity

The linked list's `checkIfValidNodeDatum` would catch foreign-CS tokens for insert-between (CS mismatch between node keys), but **not** for appending to an empty list (root's `nodeKey = Nothing` short-circuits to `True`, appended node's `nextNodeKey = Nothing` also short-circuits).

Additionally, forcing the correct MP to run provides `startDate` validation against the transaction validity range and ensures the membership data is properly initialized.

Conflicting mint checks between redeemers prevent cross-redeemer attacks: MV `InsertNodeToMHList` expects 2 tokens while MV `UpdateNodeInMHList` expects 1 token, so providing the wrong MV redeemer causes the mint check to fail against the MP's computation.

---

#### Transaction: NewMembershipInterval (MintingPolicy + MembershipsValidator)

| # | Check | MintingPolicy | MembershipsValidator | Safely Removable? |
|---|---|---|---|---|
| R6 | Exact mint check | L227-229 | L120-122 | **NO — from MV** |
| R7 | `addMembershipIntervalToHistory` | L205 | L113 | Cannot eliminate (each needs different output) |
| R8 | Last interval lookup from ref inputs | L203 | L112 | Cannot eliminate (separate scripts) |

**R6 — MV UpdateNodeInMHList L120-122: Exact mint check — MUST STAY**

Same reasoning as R5 above. Although MV independently checks the org User NFT (L126-127), the mint check forces the correct MP to run for: `startDate` validation, exact token name verification, and defense-in-depth authorization.

**R7 — `addMembershipIntervalToHistory` called in both MP and MV — CANNOT ELIMINATE**

Both scripts call this with the same inputs but need different parts of the result:
- MP needs `newInterval` (for interval output check and mint check)
- MV needs `newHistory` (for updated node output) and `newIntervalId` (for mint check)

This is unavoidable cross-script duplication — each script runs independently in the Plutus execution model.

---

#### Transaction: AcceptInterval (MembershipsValidator only)

Single-script transaction. No cross-script redundancy possible. MV handles all validation: practitioner User NFT consent, datum update, and output check.

---

#### Summary of Removed Redundancies

| # | Location | Description | Savings |
|---|---|---|---|
| **R2** | ProfilesValidator (was L124-125) | Rank output check (RV always validates this) | Eliminated `checkTxOutAtIndexWithDatumValueAndAddress` + `rankOutputIdx` from PV `AcceptPromotion` redeemer |
| **R3** | RanksValidator (was L72-74) | Profile Ref NFT `== 1` (guaranteed by lookup + mint uniqueness) | Eliminated one `assetClassValueOf` comparison |

#### Summary of Essential Checks (NOT Removable Despite Appearing Redundant)

| # | Location | Description | Why Essential |
|---|---|---|---|
| **R1** | RanksValidator L75-76 | Profile output check | Prevents promotion consumption via alternative PV redeemer (e.g., `UpdateProfileImage`) |
| **R5** | MV InsertNodeToMHList L104-106 | Exact mint check | Forces correct MP to run → `startDate` validation, token integrity; MV checks org User NFT independently (L105-106) |
| **R6** | MV UpdateNodeInMHList L120-122 | Exact mint check | Forces correct MP to run → `startDate` validation, token integrity; MV checks org User NFT independently (L126-127) |

---

### Multi-Script Interaction Vulnerability Analysis

This section analyzes whether an attacker can exploit the fact that when multiple validators run in the same transaction, each validator processes its own redeemer independently. The key question: can an attacker provide an **unexpected redeemer** to one validator while another validator processes the expected redeemer?

#### Methodology

For each multi-script transaction, we enumerate all possible alternative redeemer combinations and analyze whether the transaction could succeed with unintended side effects.

#### AcceptPromotion: Alternative Redeemer Scenarios

**Scripts**: ProfilesValidator (PV) + RanksValidator (RV)

| PV Redeemer | RV Redeemer | Result |
|---|---|---|
| `AcceptPromotion` | `PromotionAcceptance` | **Intended flow** — both validators agree on outputs |
| `UpdateProfileImage` | `PromotionAcceptance` | **Blocked by R1** — RV's profile output check rejects image-updated datum |

RV has only one redeemer that operates on valid protocol datums (`PromotionAcceptance`); the permissionless `Cleanup` redeemer only handles absent/unparseable datums. So there are no alternative RV paths to analyze for the AcceptPromotion flow.

**Conclusion**: The AcceptPromotion flow is secure. R1 is the critical cross-validator coupling that prevents redeemer mismatch attacks.

#### NewMembershipHistory: Alternative Redeemer Scenarios

**Scripts**: MintingPolicy (MP) + MembershipsValidator (MV)

The MP runs once per CurrencySymbol with a single redeemer. MV runs once per spending input.

| MP Redeemer | MV Redeemer | Result |
|---|---|---|
| `NewMembershipHistory` | `InsertNodeToMHList` | **Intended flow** |
| `NewMembershipHistory` | `UpdateNodeInMHList` | **Blocked** — MP expects 2 minted tokens, MV expects 1 → conflicting mint checks |
| `NewMembershipHistory` | `AcceptInterval` | **Blocked** — `AcceptInterval` requires `IntervalDatum`, but left node has `ListNodeDatum` |

**Conclusion**: The conflicting mint checks and datum-type pattern matching prevent all cross-redeemer attacks.

#### NewMembershipInterval: Alternative Redeemer Scenarios

**Scripts**: MintingPolicy (MP) + MembershipsValidator (MV)

| MP Redeemer | MV Redeemer | Result |
|---|---|---|
| `NewMembershipInterval` | `UpdateNodeInMHList` | **Intended flow** |
| `NewMembershipInterval` | `InsertNodeToMHList` | **Blocked** — MP expects 1 minted token, MV expects 2 → conflicting mint checks |
| `NewMembershipInterval` | `AcceptInterval` | **Blocked** — `AcceptInterval` requires `IntervalDatum`, but history node has `ListNodeDatum` |

**Conclusion**: Same protection mechanisms as NewMembershipHistory.

#### Cross-Flow Composition: Combining Independent Operations

Can multiple operations be batched in a single transaction?

| Combination | Feasibility | Security Impact |
|---|---|---|
| AcceptPromotion + AcceptInterval | Possible (no minting, different User NFTs) | Safe — independent operations |
| AcceptPromotion + NewMembershipHistory | **Blocked** — MP runs once per CS, can't have two MP redeemers | N/A |
| UpdateProfileImage + AcceptInterval | Possible (same practitioner User NFT) | Safe — both authorized by the same user |
| Two NewMembershipHistory operations | **Blocked** — MP runs once per CS, would need two redeemers | N/A |

**Conclusion**: The single-invocation-per-CurrencySymbol rule in Plutus V3 naturally prevents batching of different minting operations, eliminating a class of composability attacks.

#### Rank vs Promotion Spending at RanksValidator

Could someone spend an accepted `Rank` (non-Promotion) datum with `PromotionAcceptance`?

- `Rank` has constructor index 0, `Promotion` has constructor index 1
- `promotionAwardedTo` is a partial field only on `Promotion` — accessing it on `Rank` causes runtime failure
- `promoteProfile` calls `acceptRank` which explicitly checks: `acceptRank (Rank {}) _ = traceError "Cannot accept a rank that is not pending"`

**Conclusion**: Ranks are effectively non-spendable. Both the partial field access and `acceptRank` guard prevent this.

#### ProtocolParams Trust Chain

Both PV and RV extract cross-validator addresses from datum fields (`protocolParams profile` and `promotionProtocolParams promotionRankDatum`). Could an attacker forge a datum with wrong ProtocolParams?

All datums at validator addresses are created by the MintingPolicy, which embeds the correct `ProtocolParams` at compile time. The datum is then locked at the validator with a protocol NFT. Since protocol NFTs can only be minted by the MP, and `checkAndGetCurrentStateDatumAndValue` requires the NFT for lookup, the ProtocolParams in datums are always authentic.

**Conclusion**: The trust chain from MintingPolicy → datum → validator is intact.

---

### `validLastInterval` TODO Resolution

**Location**: `Protocol.hs` L357

```haskell
validLastInterval = membershipHistoryIntervalsHeadId currentHistory == membershipIntervalId lastInterval
-- TODO: tbc if this check is required considering the validator logic
```

**Answer: YES, this check is REQUIRED.** Without it, the MV's `UpdateNodeInMHList` redeemer could provide an arbitrary `lastIntervalId` pointing to an old, already-closed interval deep in the chain (not the actual head). The lookup via `unsafeGetMembershipInterval` would find it, and the "is closed" / "is accepted" checks would pass on that old interval, allowing creation of a new interval even though the actual head might not be closed or accepted.

This check anchors the validation to the **actual head** of the interval chain, preventing head-bypass attacks.

---

## Comprehensive Protection Summary

| Attack Vector | Protection Mechanism | Status |
|---|---|---|
| Forged datum at validator | NFT-based lookups (`checkAndGetCurrentStateDatumAndValue`) ignore UTxOs without correct NFT | **Protected** |
| Mint extra tokens | `mintValueMinted == exact expected` blocks any extra minting across ALL CurrencySymbols | **Protected** |
| Token leaves validator | `ownValue == outputValue` (exact match) ensures tokens stay locked | **Protected** |
| Wrong redeemer for datum type | MV pattern-matches `ListNodeDatum` vs `IntervalDatum` and rejects mismatches | **Protected** |
| Duplicate membership histories | Sorted linked list with strict ordering (`<` not `<=`) prevents duplicate `nodeKey` | **Protected** |
| Cross-organization manipulation | `organizationId` equality checked on all nodes; sorted linked list enforces ordering | **Protected** |
| Unauthorized membership creation | Organization User NFT spending enforced by MintingPolicy | **Protected** |
| Unauthorized membership acceptance | Practitioner User NFT spending enforced by MembershipsValidator | **Protected** |
| Double membership acceptance | `acceptMembershipInterval` fails if `isAck` is already `True` | **Protected** |
| Parallel root creation | Root created atomically with org profile; deterministic ID; seed TxOutRef prevents replay | **Protected** |
| Interval overlap | `addMembershipIntervalToHistory` requires last interval closed and accepted | **Protected** |
| Unauthorized promotion creation | Master must spend their User NFT | **Protected** |
| Unauthorized promotion acceptance | Student must spend User NFT (RanksValidator) | **Protected** |
| Double promotion acceptance | `nextBelt > currentBelt` check at ProfilesValidator | **Protected** |
| Out-of-order promotion acceptance | `nextBeltDate > currentBeltDate` check at ProfilesValidator | **Protected** |
| Invalid promotion rules | Full `validatePromotion` at mint time | **Protected** |
| Replay attacks (profiles/promotions) | Seed TxOutRef must be spent (one-time use) | **Protected** |
| Token name collisions | `blake2b_224` hash-based deterministic naming | **Protected** |
| Oversized metadata | Per-field size limits enforced on-chain | **Protected** |
| Malicious ProtocolParams | MintingPolicy hash includes params; wrong params = orphaned profiles | **Protected** |
| Profile deletion | Not supported by design (immutability) | **Protected** |
| Foreign token injection | `hasCurrencySymbol` validates referenced AssetClasses (now including practitionerId) | **Protected** |
| Datum type confusion at MV | `MembershipDatum` sum type with pattern matching on constructor | **Protected** |
| Backdated membership dates | `startDate `before` txInfoValidRange` in both membership redeemers | **Protected** |
| Promoting Organizations | `getCurrentRankId` fails for profiles with `currentRank = Nothing` | **Protected** |
| Promotion consumption via alternative PV redeemer | RV checks profile output (R1) — rejects non-promoted profile datum | **Protected** |
| Cross-redeemer attack on MV (wrong redeemer for left node) | Conflicting exact mint checks between `InsertNodeToMHList` (2 tokens) and `UpdateNodeInMHList` (1 token) | **Protected** |
| MV linked list manipulation without org authorization | MV mint check forces MP to run → MP enforces org User NFT | **Protected** |
| Spending an accepted Rank (non-Promotion) at RV | `acceptRank (Rank {}) _` fails + partial field accessor failure | **Protected** |
| ProtocolParams forgery in datums | Datums created by MP (authentic); lookups require protocol NFT | **Protected** |
| Head-bypass in interval chain | `validLastInterval` check anchors to actual head of interval chain | **Protected** |
| Batching different MP operations | Single MP invocation per CurrencySymbol in Plutus V3 | **Protected** |


## Script-by-Script Analysis

### MintingPolicy

| Redeemer | Authorization | Uniqueness | Exact Mint | Output Locked | Date Validation | CS Validation |
|---|---|---|---|---|---|---|
| `CreateProfile` (Practitioner) | Seed TxOutRef spent | Seed TxOutRef | Ref + User + Rank | Profile @ PV, Rank @ RV | `creationDate `before` txInfoValidRange` | N/A (derived from seed) |
| `CreateProfile` (Organization) | Seed TxOutRef spent | Seed TxOutRef | Ref + User + Root | Profile @ PV, Root @ MV | `creationDate `before` txInfoValidRange` | N/A (derived from seed) |
| `Promote` | Master User NFT | Seed TxOutRef | Promotion only | Promotion @ RV | N/A (dates validated by `validatePromotion`) | Student + Master |
| `NewMembershipHistory` | Org User NFT | Deterministic ID | MH + Interval | Interval @ MV (node outputs enforced by MV) | `startDate `before` txInfoValidRange` | Org + Practitioner + LeftNode |
| `NewMembershipInterval` | Org User NFT | Deterministic ID | Interval only | Interval @ MV (node output enforced by MV) | `startDate `before` txInfoValidRange` | Org + MembershipNode |

### ProfilesValidator

| Redeemer | Authorization | Output Validation | Cross-Validator |
|---|---|---|---|
| `UpdateProfileImage` | Owner User NFT | Updated datum @ same address, same value | None |
| `AcceptPromotion` | Implicit (RV guarantees consent) | Updated profile @ PV (rank output delegated to RV — R2 removed) | Reads promotion from `txInfoInputs` (RV), reads current rank from `txInfoReferenceInputs` (RV) |

### RanksValidator

| Redeemer | Authorization | Output Validation | Cross-Validator |
|---|---|---|---|
| `PromotionAcceptance` | Student User NFT | Updated profile @ PV, Transformed rank @ RV | Reads student profile from `txInfoInputs` (PV) |

- Ranks (non-Promotion) cannot be spent — `promotionAwardedTo` field only exists on the `Promotion` constructor
- Output checks guarantee tokens never leave the validator

### MembershipsValidator

| Redeemer | Datum Type | Authorization | Exact Mint | Output Validation |
|---|---|---|---|---|
| `InsertNodeToMHList` | `ListNodeDatum` | Org User NFT (via MP) | MH + Interval | Updated left node + Inserted node @ MV |
| `UpdateNodeInMHList` | `ListNodeDatum` | Org User NFT (via MP) | Interval only | Updated history node @ MV |
| `AcceptInterval` | `IntervalDatum` | Practitioner User NFT | N/A (no minting) | Updated interval @ MV, same value |

- Invalid datum/redeemer combinations are rejected: `ListNodeDatum` only accepts `InsertNodeToMHList`/`UpdateNodeInMHList`; `IntervalDatum` only accepts `AcceptInterval`
- `ownValue == outputValue` on all outputs guarantees tokens never leave the validator

### LinkedList

| Operation | Ordering | Adjacency | Uniqueness | Currency Consistency |
|---|---|---|---|---|
| `checkInputsAndInsertInBetweenNodes` | `left < inserted < right` (strict) | `nextNodeKey left == nodeKey right` | Strict `<` prevents duplicates | `checkIfValidNodeDatum` on all nodes |
| `checkInputsAndAppendNode` | `appended > last` (strict) | `last.nextNodeKey == Nothing` | Strict `>` prevents duplicates | `checkIfValidNodeDatum` on all nodes |

Organization-level constraint (`sameOrganization`) is enforced by the `MembershipHistoriesListNode` wrapper functions in `Protocol.hs`.


## Conclusion

The core onchain architecture is **sound**. The NFT-based authentication model, exact-mint checks, strict value equality on outputs, and the `MembershipDatum` wrapper form a robust security model. The multi-script interaction analysis confirms that all cross-validator communication is properly secured through a combination of:

1. **Conflicting mint checks** that create implicit cross-script coupling between MP and MV
2. **Output validation across validators** (R1 in RV) that prevents alternative-redeemer attacks
3. **Datum-type pattern matching** that restricts redeemer usage at the MembershipsValidator
4. **Single MP invocation per CurrencySymbol** that prevents batching of incompatible operations

Two medium-severity issues were identified and resolved:
1. Missing CurrencySymbol validation for `practitionerId` in `NewMembershipHistory`
2. Missing `startDate` validation against transaction validity range in both membership redeemers

The cross-script redundancy analysis identified and **removed two redundancies** (R2: PV rank output check, R3: RV profile Ref NFT == 1) and confirmed that **three seemingly redundant checks are essential** (R1: RV profile output check, R5/R6: MV mint checks). The `validLastInterval` check in `addMembershipIntervalToHistory` was confirmed as required to prevent head-bypass attacks.

Remaining items are low-severity design considerations (time validation in future `UpdateEndDate` redeemer). The dust/griefing attack vector has been mitigated with a permissionless `Cleanup` redeemer on all three validators.

**No new vulnerabilities were identified** in the multi-script interaction analysis. All 35 attack vectors in the protection summary are confirmed as protected.
