# On-chain Hardening — Design Spec

**Date:** 2026-07-05
**Sub-project:** E (from `FableReview-0407.md`, action-plan row 9 — F-18, F-21, F-22, F-25)
**Status:** design approved; item-verified against current code + offchain break-risk assessed (see Verification record); pending plan

## Goal

Add defensive on-chain constraints that harden the validators against future edits
and convention drift, plus documentation of intentional trust-model choices and an
offchain safe-wrapper. Every hash-changing check is verified to be a **no-op on the
happy path** (never breaks a legitimate transaction).

## Principle

Defense in depth: add checks that are already implied by the happy path (so they
never break a real tx) but that fail an attacker/edit that violates a protocol
invariant. **This stream changes validator hashes → it REQUIRES a redeploy** (see
Deploy handoff). Doc-only and offchain items are hash-neutral.

## Scope

**Hash-changing (validator) hardenings — all no-op on the happy path:**

- **F-21 — constrain the burn side of `txInfoMint`.** All mint checks compare only
  `mintValueMinted` (net-positive); add `&& mintValueBurned txInfoMint == mempty` to
  each. Sites: `MintingPolicy.hs:206` (M5), `:223` (M6), `:279` (Ma), `:323` (Md),
  `:371` (Me), `:414` (Mj); `MembershipsValidator.hs:196` (V5), `:229` (V6).
  `mintValueMinted`/`mintValueBurned`/`mempty::Value` are already in scope (no new
  imports). **Safe:** no offchain builder burns any protocol token — every builder
  uses `mintOrBurn = True` / positive amounts, so `mintValueBurned` is already
  `mempty` on every legitimate tx.
- **F-22a — OracleNFTPolicy exact-mint.** `oracleNFTPolicyLambda`
  (`OracleNFTPolicy.hs:30-37`) constrains only the seed spend, not the minted value.
  Add `&& traceIfFalse "N2" (mintValueMinted txInfoMint == theOracleNFT)` where
  `theOracleNFT = assetClassValue (AssetClass (mintingPolicyCurrencySymbol, TokenName "")) 1`
  (bind the CS via `MintingScript cs` in `scriptInfo`, mirroring `MintingPolicy.hs:128`;
  empty token name matches offchain `oracleNFTTN = ""`). Add `import PlutusLedgerApi.V1 qualified as V1`.
  **Safe:** the only builder (`mintOracleNFTAndLockDatum`, `Transactions.hs:154-199`)
  mints exactly `{policyCS: {"": 1}}` and nothing else.
- **F-22b — defensive NFT authentication** on state-spend handlers that today rely
  on forced co-execution. Add `V1.assetClassValueOf ownValue <ownNFT> == 1`:
  - **F-22b-1** RanksValidator `PromotionAcceptance` (`RanksValidator.hs:83-108`):
    `assetClassValueOf ownValue (promotionId promotionRankDatum) == 1` — `ownValue`
    and `promotionRankDatum` already in scope.
  - **F-22b-2** MembershipsValidator `handleUpdateNode` (`:200-230`):
    `assetClassValueOf ownValue historyId == 1` — `historyId` and `ownValue` already
    in scope.
  - **F-22b-4** AchievementsValidator `AcceptAchievement` (`AchievementsValidator.hs:71-95`):
    `assetClassValueOf ownValue (achievementId achievement) == 1` — `achievement` and
    `ownValue` already in scope.
  **Safe:** every happy-path spend looks up its UTxO by exactly that NFT
  (`getUTxOWithNFTOrThrow`) and each NFT is minted exactly once, so the spent UTxO
  genuinely carries `nft -> 1`; the check only rejects a spend of a state UTxO that
  doesn't carry its own NFT (an attack, not a legitimate tx). Needs `import PlutusLedgerApi.V1 qualified as V1` where absent.

**Excluded (deliberate):**

- **F-22b-3** AcceptInterval NFT-auth — NOT a drop-in. `OnchainMembershipInterval`
  doesn't store its own NFT id; deriving it needs the parent history id, which isn't
  in `handleAcceptInterval`'s scope. Adding it requires a NEW redeemer field
  (`membershipHistoryNodeId`) + a reference input + a coordinated offchain change to
  `acceptMembershipIntervalTX`/`AcceptMembershipIntervalAction`. The review rates this
  gap "harmless today (only own dust), inconsistent with convention" — LOW value. The
  redeemer-change blast radius is disproportionate; **excluded**, documented as a
  follow-up should that redeemer change for another reason.
- **F-19, F-20** — retrograded to Low/optional in the review; excluded (added
  complexity + hash churn for optional value).

**Doc-only (hash-neutral):**

- **F-18** — document the self-attested initial-belt trust model: a note in
  `MintingPolicy.hs` `handleCreateProfile` (~183) + a paragraph in
  `docs/onchain-architecture.md` (~328). Intentional per prior owner decision.
- **Pause-gate scope** — a module comment at `MintingPolicy.hs:144` documenting that
  `opPaused` intentionally gates only new mints, not acceptances of already-minted
  state. (Do NOT expand pause to accepts — a design change, out of scope.)
- **Rank-UTxO lineage** — a module note in `RanksValidator.hs` about the intentional
  permanent-lineage / min-ADA tradeoff (rule #14).
- **updateMembershipIntervalEndDate** — align the Haddock (`Core.hs:213-216`, "any
  future") to match the actual `isInValidRange` gate (org branch IS validity-range
  gated). Doc only — do NOT change the logic.

**Offchain (hash-neutral, behavior-preserving):**

- **F-25** — extract the 4 inline pre-validation checks before the direct
  `Onchain.addMembershipIntervalToHistory` call (`Operations.hs:625-638`) into a
  `safeAddMembershipIntervalToHistory` wrapper in `SafeOnchainLogic.hs`, mirroring
  the existing `safe*` wrappers. Verbatim-same checks (HeadNumberMismatch,
  LastIntervalNotClosed, LastIntervalNotAccepted, InvalidNewIntervalEndDate) — pure
  refactor. Covered by existing happy-path + 4 negative tests.

## Deploy handoff (REQUIRED — this stream changes validator hashes)

- The F-21/F-22a/F-22b hardenings **change every affected validator's compiled hash.**
  This stream produces the code + a **regenerated blueprint** (`admin write-blueprint`),
  but does NOT deploy. Deploying is the owner's action: run `deploy-reference-scripts`
  to publish the new reference scripts and regenerate `config/config_bjj_validators.json`,
  then validate on **testnet** before mainnet. All existing on-chain state locked under
  the OLD validator hashes remains spendable only by the OLD scripts — a validator-hash
  change is a migration event; plan the cutover.
- **This also reconciles the pre-existing stale `config_bjj_validators.json`** (its
  Minting Policy hash already diverged from `main` before this stream) — the redeploy
  regenerates it from current code.

## Testing strategy

- **E delivers the hardenings + happy-path verification.** Every hardened
  validator/redeemer is already exercised by an existing emulator/property test
  (verified per-item), so `cabal test` staying green proves no hardening breaks a
  legitimate flow. Blueprint regen confirms the (intended) hash changes.
- **Negative tests that PIN the new hardenings** (a burn attempt fails, an
  extra-mint-on-oracle fails, a spend-without-NFT fails) are delivered in **stream F**
  (extended tests / F-33), immediately after E. E notes this so the pinning isn't lost.

## Verification record

A 4-cluster verification workflow (sonnet, high effort) located every item against
current code and traced the OFFCHAIN tx builders for each hash-changing hardening to
assess break-risk. Findings folded in:

- **F-21 safe** — no offchain builder burns any protocol token (all `mintOrBurn=True`);
  `mintValueBurned == mempty` is a happy-path no-op at all 8 sites.
- **F-22a safe** — `mintOracleNFTAndLockDatum` mints exactly the oracle NFT (empty
  token name), nothing else; exact-mint holds.
- **F-22b-1/2/4 safe** — each happy-path spend resolves its UTxO by the exact NFT and
  each NFT is minted once, so `assetClassValueOf ownValue nft == 1` holds; only an
  attack spend fails.
- **F-22b-3 excluded** — requires a redeemer-field + ref-input + offchain change
  (interval datum lacks its own NFT id); low value, disproportionate blast radius.
- **All doc + F-25 items hash-neutral**; F-25 is a verbatim check-preserving refactor
  with existing happy-path + 4 negative tests.
- Every hardened validator/redeemer has an identified existing happy-path test; no
  existing test pins the new checks (negatives → stream F).
