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
    - [LOW: Dust/Griefing at Validator Addresses](#low-dustgriefing-at-validator-addresses)
    - [Cross-Validator Redundancy Analysis](#cross-validator-redundancy-analysis)
    - [HIGH (Fixed): Cross-Organization Membership Manipulation](#high-fixed-cross-organization-membership-manipulation)
    - [LOW: Missing endDate Validation on Interval Creation](#low-missing-enddate-validation-on-interval-creation)
    - [LOW: MV Redeemer Data Integrity (Subset of Cross-Org Issue)](#low-mv-redeemer-data-integrity-subset-of-cross-org-issue)
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
       _ -> traceError "state nft not found"
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

### LOW: `updateMembershipIntervalEndDate` Needs Time Validation in Validator

**Location**: `Protocol.hs`, `updateMembershipIntervalEndDate` pure function

**Description**: The `idei` specification states: *"Când este setat (Close), valoarea este doar în viitor (>= current time)."* The pure function only validates `newEndDate >= currentEndDate`, not `newEndDate >= currentTime`. A pure function cannot access the transaction context.

**Impact**: Not currently exploitable because the `UpdateEndDate` redeemer is not yet implemented.

**Recommendation**: When implementing the `UpdateEndDate` redeemer in `MembershipsValidator.hs`, add a check against `txInfoValidRange`:
```haskell
traceIfFalse "End date must be in the future"
    $ newEndDate `after` lowerBoundOfValidRange
```

---

### LOW: Dust/Griefing at Validator Addresses

**Description**: Anyone can send UTxOs with invalid datums to any validator address. The MembershipsValidator fails with `traceError "Invalid datum"` for unparseable datums, making these UTxOs **permanently locked** and unspendable.

**Impact**: Nuisance only. Does not affect protocol functionality — all lookups use NFT-based filtering and ignore UTxOs without valid protocol NFTs. Creates unspendable UTxOs at the script address that accumulate over time and can never be reclaimed.

**Mitigation (offchain)**: Offchain code should filter UTxOs by protocol CurrencySymbol before processing.

**Mitigation (onchain — TODO)**: Add an admin `PubKeyHash` to `ProtocolParams`. Each validator should accept a dedicated `AdminCleanup` redeemer that allows the admin key to spend any UTxO that does not contain a valid protocol NFT. This enables the protocol administrator to reclaim ADA locked in dust/griefing UTxOs without affecting legitimate protocol state. Implementation steps:
1. Extend `ProtocolParams` with an `adminPubKeyHash :: PubKeyHash` field
2. Add an `AdminCleanup` redeemer to `ProfilesValidator`, `RanksValidator`, and `MembershipsValidator`
3. The `AdminCleanup` validation logic should:
   - Verify the transaction is signed by `adminPubKeyHash` (via `txInfoSignatories`)
   - Verify that the spent UTxO does **not** contain any token with the protocol's CurrencySymbol (to prevent the admin from spending legitimate protocol UTxOs)
4. Update deployment order and documentation to reflect the new `ProtocolParams` structure

---

### Cross-Validator Redundancy Analysis

When multiple scripts run in the same transaction, they can independently validate the same conditions. Some of these redundancies are safely removable (reducing execution cost), while others are **essential** for security and removing them would introduce vulnerabilities.

#### R1: RanksValidator L75-76 — Profile output check ❌ NOT REMOVABLE

```haskell
-- RanksValidator.hs L75-76
traceIfFalse "Must lock profile Ref NFT with inline updated datum at profilesValidator address (output idx)"
    $ Utils.checkTxOutAtIndexWithDatumValueAndAddress profileOutputIdx updatedProfileCIP68Datum studentProfileValue profilesValidatorAddress txInfoOutputs
```

**Appears redundant with**: ProfilesValidator L122-123 (same profile output check in `AcceptPromotion`).

**Why it CANNOT be removed**: ProfilesValidator has **multiple redeemers** (`UpdateProfileImage` and `AcceptPromotion`). When RV forces the profile to be in `txInfoInputs`, PV must run, but PV could run with **any** redeemer. Without this check, an attacker could construct a transaction that spends a Promotion UTxO (triggering RV with `PromotionAcceptance`) while the Profile UTxO is spent with `UpdateProfileImage` instead of `AcceptPromotion`. The promotion would be consumed (converted to a Rank), but the profile's `currentRank` pointer would NOT be updated — the promotion is "wasted" and the profile is never promoted. RV's profile output check is the only thing that forces PV to process `AcceptPromotion` specifically.

---

#### R2: ProfilesValidator — Rank output check in `AcceptPromotion` ✅ REMOVED

Previously at ProfilesValidator L124-125:
```haskell
traceIfFalse "Must lock rank NFT with inline datum at ranksValidator address (output idx)"
    $ Utils.checkTxOutAtIndexWithDatumValueAndAddress rankOutputIdx newRankDatum promotionValue ranksValidatorAddress txInfoOutputs
```

**Was redundant with**: RanksValidator L77-78 (same rank output check in `PromotionAcceptance`).

**Why it was safe to remove**: RV has only **one redeemer** (`PromotionAcceptance`). PV's `AcceptPromotion` forces the Promotion UTxO to be in `txInfoInputs`, so the Promotion UTxO must be spent, which forces RV to run. RV's only redeemer always validates the rank output. There is no alternative code path in RV.

**Changes applied**:
- Removed the rank output `checkTxOutAtIndexWithDatumValueAndAddress` check from PV's `AcceptPromotion`
- Removed `rankOutputIdx` from the `AcceptPromotion` redeemer (reduced from `AcceptPromotion RankId Integer Integer` to `AcceptPromotion RankId Integer`)
- `newRankDatum` from `promoteProfile` is now unused (`_newRankDatum`)
- `promotionValue` from `unsafeGetRankDatumAndValue` is now unused (`_promotionValue`)

**Status**: **RESOLVED**

---

#### R3: RanksValidator — Profile Ref NFT `== 1` check in `PromotionAcceptance` ✅ REMOVED

Previously at RanksValidator L72-74:
```haskell
traceIfFalse "Student Profile value must contain profile Ref NFT"
    $ V1.assetClassValueOf studentProfileValue profileRefAssetClass == 1
```

**Was redundant with**: ProfilesValidator L118-120 (same check on `ownValue`) and `checkAndGetCurrentStateDatumAndValue`'s `geq` filter.

**Why it was safe to remove**: `unsafeGetProfileDatumAndValue` (L61) uses `checkAndGetCurrentStateDatumAndValue` which filters by `geq assetClassValue stateToken 1`, guaranteeing the NFT exists (≥1). The MintingPolicy only ever mints exactly 1 of each profile NFT, so `≥1` implies `==1`. ProfilesValidator also independently checks this.

**Changes applied**:
- Removed the `assetClassValueOf studentProfileValue profileRefAssetClass == 1` check from RV's `PromotionAcceptance`
- Removed the now-unused `profileRefAssetClass` binding

**Status**: **RESOLVED**

---

#### R4: MembershipsValidator L110-112, L131-133 — Exact mint checks ❌ NOT REMOVABLE

```haskell
-- MembershipsValidator.hs L110-112 (InsertNodeToMHList)
traceIfFalse "Tx must mint JUST inserted node NFT and interval NFT"
    $ mintValueMinted txInfoMint == (insertedNodeNFT + newIntervalNFT)

-- MembershipsValidator.hs L131-133 (UpdateNodeInMHList)
traceIfFalse "Tx must mint JUST interval  NFT"
    $ mintValueMinted txInfoMint == newIntervalNFT
```

**Appears redundant with**: MintingPolicy L185-189 / L231-233 (identical exact mint checks in `NewMembershipHistory` / `NewMembershipInterval`).

**Why they CANNOT be removed**: Although the MembershipsValidator now independently checks the Organization User NFT (derived from the on-chain datum), the mint checks remain essential as a **separate protection layer**. The Org User NFT check prevents cross-organization attacks (wrong org), but the mint checks prevent a completely **different attack**: using a rogue MintingPolicy to mint tokens and manipulate the linked list. Without the mint checks, an attacker could bypass the protocol's MintingPolicy entirely (which enforces CS validation, date validation, deterministic IDs, etc.) and use any minting script. The `checkIfValidNodeDatum` CS check in `LinkedList.hs` does not catch all cases (specifically: appending to an empty list, where both root `nodeKey = Nothing` and appended `nextNodeKey = Nothing` short-circuit to `True`).

---

#### R5: `addMembershipIntervalToHistory` called in both MP and MV ❌ NOT REMOVABLE

**Appears redundant**: Both MintingPolicy (`NewMembershipInterval`, L207) and MembershipsValidator (`UpdateNodeInMHList`, L121) call this function with the same inputs.

**Why it CANNOT be removed**: Each script needs a different part of the result. MP needs `newInterval` (to check the interval output and compute exact mint). MV needs `newHistory` (to compute the updated node datum) and `newIntervalId` (for its mint check). Since Plutus scripts run independently, there is no mechanism to share computation results across validators in the same transaction.

---

#### R6: `validLastInterval` in Protocol.hs L357 ❌ NOT REMOVABLE

```haskell
-- Protocol.hs L357
validLastInterval = membershipHistoryIntervalsHeadId currentHistory == membershipIntervalId lastInterval
```

**Why it CANNOT be removed** (resolving the `TODO: tbc` comment): Without this check, the MV's `UpdateNodeInMHList` redeemer could provide an arbitrary `lastIntervalId` pointing to an old, already-closed interval deep in the chain. The `unsafeGetMembershipInterval` would find it successfully, and the "is closed" / "is accepted" checks would pass on that old interval. This would allow creating a new interval even though the actual head interval might still be open or unaccepted. The `validLastInterval` check ensures validation runs against the actual head, not an arbitrary historical interval.


#### R7: MintingPolicy — `isCorrectOrganization` in `NewMembershipInterval` ✅ REMOVED

Previously at MintingPolicy L201-202/L224-225:
```haskell
isCorrectOrganization = membershipHistoryOrganizationId oldHistory == organizationProfileId
-- ...
traceIfFalse "Membership history must belong to the authorized organization"
    isCorrectOrganization
```

**Was redundant with**: MembershipsValidator L117-118/L126-128 (`orgUserAC = deriveUserFromRefAC (organizationId spendingNode)` + Org User NFT spending check).

**Why it was safe to remove**: The MV's `orgUserAC` check is strictly stronger — it derives the expected Organization User NFT from the **on-chain** datum's `organizationId` and requires it to be **spent**. This guarantees that only the organization that owns the membership data can modify it. The MP's `isCorrectOrganization` performed a weaker version of the same assertion (checks the org ID matches, but the MV already ensures the right org's NFT is spent). For `NewMembershipInterval`, the MV runs `UpdateNodeInMHList` which includes the `orgUserAC` check, making the MP check fully redundant.

**Changes applied**:
- Removed the `isCorrectOrganization` binding and its `traceIfFalse` check from MP's `NewMembershipInterval`
- Added a comment block referencing the MV's `UpdateNodeInMHList` check as the authoritative cross-org protection

**Note**: No equivalent check existed in `NewMembershipHistory` (MP side) because it would require parsing the left node's datum in the parameterized script, adding non-trivial cost. For that flow, the MV's `orgUserAC` check in `InsertNodeToMHList` is the sole cross-org protection.

**Status**: **RESOLVED**

---

### HIGH (Fixed): Cross-Organization Membership Manipulation

**Location**: `MembershipsValidator.hs` (`InsertNodeToMHList`, `UpdateNodeInMHList`) and `MintingPolicy.hs` (`NewMembershipHistory`, `NewMembershipInterval`)

**Description**: Organization A could insert membership history nodes into Organization B's linked list, and could add intervals to membership histories belonging to Organization B. This was possible because authorization (Organization User NFT) was only enforced by the MintingPolicy, and the MintingPolicy did NOT verify that the data being modified belonged to the authorized organization.

**Root cause — trust chain gap (pre-fix)**:

1. The **MintingPolicy** validated: (a) `organizationProfileId` has correct CurrencySymbol, (b) Organization's User NFT is spent, (c) `leftNodeId` / `membershipNodeId` has correct CurrencySymbol, (d) left node is spent from MembershipsValidator address. However, it did **NOT** verify that `leftNodeId` or `membershipNodeId` belonged to the same organization as `organizationProfileId`.

2. The **MembershipsValidator** validated: (a) `sameOrganization` between existing nodes and the inserted node, (b) linked list ordering, (c) exact minting. However, the `insertedMembershipHistory` (including its `organizationId`) came from the **MV redeemer** — which is entirely attacker-controlled. The attacker set the inserted node's `organizationId` to match the victim's left node, satisfying `sameOrganization`. The MV did **not** independently check Organization User NFT.

3. **Result**: The MP checked Org A's User NFT ✓, the MV checked `sameOrganization` using spoofed data ✓, and the cross-organizational attack succeeded.

**Attack scenario (NewMembershipHistory — inserting into another org's list)**:

1. Org A crafts MP redeemer: `NewMembershipHistory orgA practX startDate endDate orgB_leftNodeId outputIdx`
2. Org A crafts MV redeemer: `InsertNodeToMHList { insertedMembershipHistory = (history with organizationId = orgB) ... }`
3. MP validates Org A's User NFT ✓ (Org A holds their own)
4. MP validates `leftNodeId` has correct CS ✓ (it's a real protocol NFT from Org B's list)
5. MP computes and validates exact mint ✓
6. MV validates `sameOrganization` ✓ (attacker set inserted.organizationId = orgB, matching the left node)
7. MV validates linked list ordering ✓
8. **Result**: A foreign node (with Org A's NFT namespace) is inserted into Org B's membership list

**Attack scenario (NewMembershipInterval — adding intervals to another org's history)**:

1. Org A crafts MP redeemer: `NewMembershipInterval orgA orgB_membershipNodeId startDate endDate outputIdx`
2. MP reads Org B's membership history datum (via `unsafeGetListNodeDatumAndValue`)
3. MP validates Org A's User NFT ✓ (Org A holds their own)
4. MP does **not** check that `membershipHistoryOrganizationId` of the read datum matches `orgA`
5. MV validates the interval chain update ✓
6. **Result**: An unauthorized interval is added to Org B's member's history. The membership history head now points to this unauthorized interval, **blocking** Org B from adding legitimate intervals (the head must be accepted and closed first, but the practitioner can refuse the unauthorized interval, deadlocking the history)

**Impact**:
- Organization A can corrupt Organization B's membership linked list
- Organization A can block legitimate interval additions at Organization B (deadlock attack via unaccepted unauthorized interval at head)
- Deterministic NFTs derived from Org A's namespace are placed in Org B's list, creating permanent data inconsistency
- The practitioner cannot fix this — they can only refuse to accept the unauthorized interval, which leaves it blocking the head

**Fix applied** — Organization User NFT authorization added to MembershipsValidator:

The MV now independently verifies that the correct organization authorized the operation, using the **existing node datum** (not the attacker-controlled redeemer):

```haskell
-- MembershipsValidator.hs, InsertNodeToMHList (L88-90, L105-107):
let oldLeftNode = spendingNode
    -- Derive org User NFT from the EXISTING on-chain datum (not redeemer) to prevent cross-org attacks
    orgUserAC = deriveUserFromRefAC (organizationId oldLeftNode)
-- ...
traceIfFalse "Must spend organization User NFT to modify membership list"
    $ V1.assetClassValueOf (valueSpent txInfo) orgUserAC == 1

-- MembershipsValidator.hs, UpdateNodeInMHList (L117-118, L126-128):
let -- Derive org User NFT from the EXISTING on-chain datum (not redeemer) to prevent cross-org attacks
    orgUserAC = deriveUserFromRefAC (organizationId spendingNode)
-- ...
traceIfFalse "Must spend organization User NFT to modify membership history"
    $ V1.assetClassValueOf (valueSpent txInfo) orgUserAC == 1
```

This derives the expected Organization User NFT from the **spending node's `organizationId`** (which is anchored to the legitimate organization via the root created atomically with the org profile). The attacker cannot spoof this because it comes from the existing on-chain datum, not the redeemer.

**Note on defense-in-depth**: A defense-in-depth `isCorrectOrganization` check was initially added to MP's `NewMembershipInterval`, but was subsequently removed as Redundancy R7 — the MV's `orgUserAC` check is strictly stronger and fully sufficient. For `NewMembershipHistory`, no equivalent MP check exists (would require parsing the left node's datum, adding cost to the parameterized script) — the MV's `orgUserAC` check in `InsertNodeToMHList` is the sole cross-org protection there.

**Status**: **RESOLVED**

---

### LOW: Missing `endDate` Validation on Interval Creation

**Location**: `Protocol.hs` (`initMembershipHistory` L314, `addMembershipIntervalToHistory` L340)

**Description**: When creating a membership interval (both during `initMembershipHistory` and `addMembershipIntervalToHistory`), the `endDate` parameter from the redeemer is stored directly without validation. The specification states: *"Când este setat (Close), valoarea este doar în viitor (>= current time)"*, but this is not enforced for `endDate` at creation time. An organization could create an interval with `endDate = Just (pastDate)`, making it appear already closed.

**Impact**: Low — the organization can create pre-closed intervals. The practitioner can refuse to accept them. The `startDate `before` txInfoValidRange` check ensures the start date is anchored to real time, but no corresponding check exists for `endDate`.

**Recommendation**: When `endDate` is `Just date`, validate `date `after` lowerBound(txInfoValidRange)` and `date > startDate` in both the MP redeemers (`NewMembershipHistory`, `NewMembershipInterval`).

---

### LOW: MV Redeemer Data Integrity (Subset of Cross-Org Issue)

**Location**: `MembershipsValidator.hs`, `InsertNodeToMHList` redeemer

**Description**: The `InsertNodeToMHList` redeemer includes `insertedMembershipHistory :: OnchainMembershipHistory` as attacker-controlled data. While the `membershipHistoryId` field is indirectly validated (must match minted NFT), the `membershipHistoryPractitionerId` could differ from the practitioner used to derive the `membershipHistoryId` in the MintingPolicy. This creates a mismatch between the NFT identity (derived from `orgId + practA`) and the linked list key (using `practB`).

**Impact**: Even after fixing the cross-organization vulnerability, a malicious organization can still create nodes with mismatched NFT-vs-key data **in their own list**. This is self-harm (corrupts only their own linked list) and mainly affects offchain query consistency.

**Mitigation**: Resolving the HIGH-severity cross-organization issue (via MV authorization check) limits this to self-harm only. Full prevention would require the MV to independently re-derive the `membershipHistoryId` from the redeemer's `organizationId + practitionerId` and compare, but this adds significant execution cost.

---


## Comprehensive Protection Summary

| Attack Vector | Protection Mechanism | Status |
|---|---|---|
| Forged datum at validator | NFT-based lookups (`checkAndGetCurrentStateDatumAndValue`) ignore UTxOs without correct NFT | **Protected** |
| Mint extra tokens | `mintValueMinted == exact expected` blocks any extra minting across ALL CurrencySymbols | **Protected** |
| Token leaves validator | `ownValue == outputValue` (exact match) ensures tokens stay locked | **Protected** |
| Wrong redeemer for datum type | MV pattern-matches `ListNodeDatum` vs `IntervalDatum` and rejects mismatches | **Protected** |
| Duplicate membership histories | Sorted linked list with strict ordering (`<` not `<=`) prevents duplicate `nodeKey` | **Protected** |
| Cross-organization manipulation | MV independently checks Org User NFT derived from on-chain datum; MP validates org ownership of data | **Protected** (fixed) |
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
| `InsertNodeToMHList` | `ListNodeDatum` | Org User NFT (MV: from on-chain datum; MP: CS check + spending) | MH + Interval | Updated left node + Inserted node @ MV |
| `UpdateNodeInMHList` | `ListNodeDatum` | Org User NFT (MV: from on-chain datum; MP: CS check + spending) | Interval only | Updated history node @ MV |
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

The core onchain architecture for profiles, ranks, and promotions is **sound**. The NFT-based authentication model, exact-mint checks, strict value equality on outputs, and the `MembershipDatum` wrapper form a robust security model that prevents the two primary attack vectors (forged datums and unauthorized minting).

Two medium-severity issues were identified and **resolved**:
1. Missing CurrencySymbol validation for `practitionerId` in `NewMembershipHistory`
2. Missing `startDate` validation against transaction validity range in both membership redeemers

**One high-severity vulnerability was identified and resolved** in the membership system:
- **Cross-Organization Membership Manipulation**: Organization A could modify Organization B's membership data because the MembershipsValidator did not independently verify organization authorization. **Fixed** by adding Org User NFT authorization to MV (derived from on-chain datum, not redeemer) for both `InsertNodeToMHList` and `UpdateNodeInMHList`. A defense-in-depth `isCorrectOrganization` check was also added to MP's `NewMembershipInterval` (redundant with MV check, retained for layered security).

A detailed cross-validator redundancy analysis was performed:
- **3 redundancies removed**: R2 (PV rank output check — delegated to RV), R3 (RV profile Ref NFT `== 1` check — guaranteed by `geq` filter and single-mint invariant), R7 (MP `isCorrectOrganization` — MV's `orgUserAC` check is strictly stronger)
- **4 apparent redundancies are essential** and must be kept (R1: RV profile output check, R4: MV mint checks, R5: addMembershipIntervalToHistory duplication, R6: validLastInterval)

Additional low-severity items remain: time validation for `UpdateEndDate` redeemer, missing `endDate` validation on creation, dust/griefing mitigation, and MV redeemer data integrity.
