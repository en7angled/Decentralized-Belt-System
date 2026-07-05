# Extended Security & Negative Tests — Design Spec

**Date:** 2026-07-05
**Sub-project:** F (from `FableReview-0407.md`, action-plan row — F-33 "extended tests"; also delivers the negative pins promised by stream E)
**Status:** design (autonomous execution authorized); item-verified against current code via four reconnaissance passes; pending plan

## Goal

Close the negative/security test gaps the review flagged (F-33) and add the
negative tests that **pin** the stream-E on-chain hardenings. The suite today
(139 tests) is happy-path-heavy: ~15 negative tests, several real
authorization boundaries untested, one security helper built but never called,
and one negative test skipped behind a stale justification. This stream adds
targeted negative tests that assert the protocol **rejects** malicious/invalid
transactions.

## Principle

Every added test asserts a **failure** (`mustFail`) or a **value delivery**
(`withWalletBalancesCheck`). A `mustFail` test that unexpectedly PASSES means a
transaction that should have been rejected succeeded — that is a potential
**real vulnerability**, not a test bug: the implementer must STOP and escalate,
never weaken or silence the assertion to make it green. No production code
changes except test files (and test-only helpers in `TestRuns.hs`); if a test
surfaces a genuine offchain/onchain defect, surface it — do not fix protocol
logic inside a test stream.

## Harness facts (verified)

- Framework: Tasty + tasty-hunit + tasty-hedgehog over the **Atlas CLB
  emulator**. Emulator entry: `mkTestFor "Case N.X: desc" testFn`.
- Failure assertion: `mustFail :: GYTxMonadClbT m a -> GYTxMonadClbT m ()`
  (from `GeniusYield.Test.Clb`). **Verified** it catches BOTH build-time
  failures (e.g. reserved-metadata reject, `Achievement.hs:170-184`) AND
  phase-2 script-validation failures (e.g. double-accept, `Achievement.hs:448-456`).
- Balance assertion: `withWalletBalancesCheck [w := valueDelta] $ do ...` /
  `withWalletBalancesCheckSimple` (framework nets out the acting user's tx fee).
- Wallets: `w1..w10 testWallets`, pre-funded. Deploy: `deployBJJValidators (w1 …)`.
- Normal path: `bjjInteraction ctx user actionType mrecipient` (profile
  actions) / `adminInteraction ctx user adminAction` (oracle admin) /
  `protocolInteraction ctx user protocolAction` (cleanup).
- Raw-skeleton path (for custom malicious txs): `mustMint (GYMintScript @'PlutusV3 policy) redeemer tokenName amount`,
  `mustHaveInput`, `mustHaveRefInput`, `txMustLockStateWithInlineDatumAndValue`,
  `txMustSpendStateFromRefScriptWithRedeemer`, `mconcat`, `sendSkeleton'`. The
  existing `maliciousAcceptPromotionTX` (`TestRuns.hs:304-371`) is the reference
  pattern; the protocol minting policy is reachable via
  `compileMintingPolicy oracleNFT` and the oracle-NFT policy via
  `compileOracleNFTPolicy seedPlutus` (see `mintTestOracleNFT`, `TestRuns.hs:120-161`).

## Scope — committed (6 tasks)

**Task 1 — Activate the dead malicious-promotion security test (gap #2).**
`maliciousAcceptPromotionTX` / `maliciousBjjAcceptPromotion` (`TestRuns.hs:304-387`)
are exported but called by no suite (only referenced in a `Cleanup.hs:154`
comment). Add a test: set up master + student, master promotes student, then
`mustFail $ maliciousBjjAcceptPromotion ctx attacker promotionId` — the attacker
accepts WITHOUT the student's User NFT; RanksValidator (via `deriveUserFromRefAC`)
must reject. Register in `UnitTests/Promotion.hs`. Pins the RanksValidator
user-NFT consent boundary.

**Task 2 — Promotion double-accept negative (gap #1).** Achievements have a
double-accept test (`Achievement.hs:448-456`); promotions do not. Add: promote +
accept, then `mustFail $` accept the same promotion again (RanksValidator rejects
re-accept of an already-accepted/at-rank promotion). Register in `Promotion.hs`.

**Task 3 — Authorization negatives (gaps #3, #6, #7).** Three `mustFail` tests
where the WRONG wallet attempts a privileged action:
- **Non-owner UpdateProfile:** w2 creates a profile; `mustFail $ bjjInteraction
  ctx (w3…) (UpdateProfileAction …)` — w3 does not hold the profile's User NFT.
- **Wrong-user AcceptMembershipInterval:** set up a membership interval for w2;
  `mustFail $ bjjInteraction ctx (w3…) (AcceptMembershipIntervalAction …)`.
- **Non-admin oracle update:** `mustFail $ adminInteraction ctx (w2…)
  PauseProtocolAction` (and/or `SetFeesAction`) — w2 is not `opAdminPkh`;
  OracleValidator must reject. Register alongside the relevant existing groups
  (`Promotion.hs`, `Membership.hs`, `Oracle.hs`).

**Task 4 — Pause-gate real test + fee-delivery positive (gaps #4, #5a).**
- **Pause-gate:** replace the stale skip at `Oracle.hs:185-186` ("CLB limitation
  for expected-failure tests") with a real test. The claim is **false** —
  minting while paused throws `ProtocolPaused` at build time in
  `getOracleRefInputSkeleton` (`Operations.hs:48-55`), and `mustFail` catches
  build-time failures. Pause the protocol, then
  `mustFail $ bjjInteraction ctx (w1…) (InitProfileAction …)`. Remove the
  misleading skip note.
- **Fee delivery:** configure fees via `SetFeesAction (Just fc)` with
  `fcFeeAddress = addressToPlutus (userChangeAddress (w2…))` and
  `fcProfileCreationFee = 2_000_000`; create a profile as w1;
  `withWalletBalancesCheck [w2 := <+2 ADA fee, minus nothing (w2 is not the
  actor)>]` asserts the fee value reaches `fcFeeAddress`. Pins that `checkFee`
  (trace "K2", `Lookup.hs:119-132`) is satisfied by a real output to the fee
  address on the happy path.

**Task 5 — OracleNFTPolicy exact-mint negatives (gap #10, pins F-22a).** Clone
`mintTestOracleNFT` into test-only malicious variants that reuse the one-shot
oracle policy but violate the new N2 exact-mint check
(`OracleNFTPolicy.hs:36-42`, `mintValueMinted == theOracleNFT`):
- mint the oracle NFT **plus an extra token name** under the same policy →
  `mustFail`.
- mint the oracle NFT with a **non-empty token name** → `mustFail`.
Add the helper(s) to `TestRuns.hs`, register tests in `UnitTests/Oracle.hs`.

**Task 6 — F-21 burn-along-mint pin (gap #9, pins F-21).** Build the normal
create-profile skeleton via the offchain builder, then `mconcat` an extra
`mustMint protocolMP <CreateProfile redeemer> <held-user-NFT-TN> (-1)` so
`txInfoMint` carries a burn alongside the mint under one redeemer invocation;
`mustFail`. Pre-E this passed (`mintValueMinted` matches the positive side);
post-E the `mintValueBurned txInfoMint == mempty` conjunct rejects it. This is
the highest-complexity task — it needs the protocol MP handle and the matching
`CreateProfile` redeemer in the test. **Feasibility gate:** if GY's mint-merging
or balancing cannot express a mint+burn combo under one redeemer, document the
exact obstruction (with the code evidence, not a vague note) and fall back to
asserting the check via the existing happy-path no-op argument from stream E;
escalate rather than ship a contrived or passing-should-fail test.

## Scope — deferred (documented follow-ups)

These require contrived hand-built attack skeletons of disproportionate
complexity relative to their value; the underlying checks are **defense-in-depth
no-ops already verified as happy-path-safe during stream E** (offchain-trace +
adversarial-skeptic workflows). Listed so the gap is explicit, not silently
dropped:

- **Fee-underpayment negative (gap #5b).** The offchain builder always pays the
  exact fee; forcing an underpayment needs a full hand-rebuilt create-profile
  skeleton with a reduced fee output. Task 4's fee-delivery positive already
  exercises the `checkFee`/K2 path end-to-end.
- **F-22b spend-without-own-NFT pin.** Requires fabricating an NFT-less state
  UTxO at a validator and routing it through the accept path — the most
  contrived construction, validator-specific. The check only fires on a manually
  manufactured state that no offchain path produces. (Mirrors stream E's
  deferral of F-22b-3 for the same disproportionate-blast-radius reason.)
- **On-chain M0 pause pin.** Task 4 pins the offchain `ProtocolPaused` guard
  (the layer a real user hits). Pinning the on-chain M0 gate itself requires a
  custom mint skeleton that bypasses the offchain guard while paused — same
  custom-mint family as Task 6, low marginal value once Task 6 exists.

## Observations (not fixed here — surface to owner)

- `addMembershipIntervalTX` (`Operations.hs:610`) computes its fee with
  `fcMembershipHistoryFee`, not `fcMembershipIntervalFee`. May be intentional or
  a latent bug; out of scope for a test stream — flagged for owner review. A
  fee-delivery test for interval creation would encode whichever is current
  behavior; Task 4 targets profile-creation fees to avoid baking in a possibly
  wrong selector.

## Testing strategy / success criteria

- New tests follow the existing `mkTestFor "Test Case N.X: …"` naming and are
  registered in the matching `UnitTests/<Topic>.hs` group so `cabal test` runs
  them.
- **Definition of done per task:** the new negative test(s) FAIL the malicious
  tx (go green under `mustFail`) / the balance test asserts the exact fee
  delivery, AND the full suite (`cabal build all` + `cabal test`) stays green
  with the new count > 139. No happy-path test regresses.
- **Escalation rule (all tasks):** if a `mustFail` test does not fail (tx
  succeeded), treat it as a candidate real vulnerability — stop, report, do not
  adjust the assertion to pass.

## Verification record

Four reconnaissance passes (Explore agents) located, against current code:
the test harness + assertion/balance helpers; the malicious-builder inventory
(confirmed `maliciousAcceptPromotionTX` exported but never called; no other
malicious builders); a per-validator coverage matrix with the 10 flagged gaps
anchored to file:line; and fee mechanics (`FeeConfig.fcFeeAddress`, `checkFee`
K2, `getFeeSkeleton`) + pause feasibility (build-time `ProtocolPaused`; `mustFail`
proven to catch build-time and phase-2 failures — the skip note is stale). Each
committed task maps to a verified gap; each deferral maps to a verified
complexity/value assessment.
