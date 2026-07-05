# Extended Security & Negative Tests Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Add negative/security emulator tests closing the F-33 gaps and pinning the stream-E on-chain hardenings.

**Architecture:** Pure test additions (Atlas CLB emulator, Tasty). Most tests use `mustFail $ bjjInteraction/adminInteraction …` by the wrong wallet or in a forbidden state; one uses `withWalletBalancesCheck` for fee delivery; two build custom malicious skeletons (`mustMint`/`mconcat`). No production code changes except test files and test-only helpers in `TestRuns.hs`.

**Tech Stack:** Haskell, GeniusYield/Atlas CLB (`GYTxMonadClb`), `mkTestFor`, `mustFail`, `withWalletBalancesCheck`, `mustMint`.

## Global Constraints

- **No production/protocol logic changes.** Only `src/test/**` and (Task 5/6) test-only helpers in `src/test/TestRuns.hs`. If a test surfaces a real offchain/onchain defect, STOP and report — do not fix protocol code in this stream.
- **Escalation rule:** a `mustFail` test that does NOT fail (the malicious tx succeeded) is a candidate real vulnerability — report it, never weaken/relax the assertion to go green.
- **Naming/registration:** follow the existing `mkTestFor "Test Case N.X: …"` convention; register each new test in the matching `UnitTests/<Topic>.hs` group so `cabal test` runs it.
- **Haskell style:** max 120 cols, 2-space indent, one blank line between top-level decls (per repo CLAUDE.md).
- **Definition of done (every task):** `cabal build all` clean, `cabal test` green, total test count strictly greater than before the task, no happy-path regression.
- **Verify command:** `cabal test 2>&1 | tail -3` (suite passes as `1 of 1 test suites … passed`). To see the new test ran, grep the log path it prints for the new "Test Case" label.

---

### Task 1: Activate the malicious-promotion security test (gap #2)

**Files:**
- Modify: `src/test/UnitTests/Promotion.hs` (import list ~line 21; `promotionSecurityTests` group list ~line 251-253; add a local test fn in its `where` ~after line 253)

**Interfaces:**
- Consumes: `maliciousBjjAcceptPromotion :: DeployedScriptsContext -> User -> GYAssetClass -> m (GYTxId, GYAssetClass)` (from `TestRuns`, `TestRuns.hs:374-387`) — attacker accepts a promotion WITHOUT the student's User NFT; RanksValidator must reject.
- Consumes: `bjjInteraction`, `deployBJJValidators`, `CreateProfileWithRankAction`, `InitProfileAction`, `PromoteProfileAction{..}`, `mustFail`.

- [ ] **Step 1: Extend the TestRuns import** — add `maliciousBjjAcceptPromotion` to the existing `import TestRuns (…)` at `Promotion.hs:21`:

```haskell
import TestRuns (bjjInteraction, deployBJJValidators, logPractitionerProfileInformation, maliciousBjjAcceptPromotion)
```

- [ ] **Step 2: Add the test function** inside the `where` block of `promotionSecurityTests` (after `sequentialPromotionsWork`, before the closing of the `where`). Mirror the setup of `sequentialPromotionsWork` (`Promotion.hs:335-377`) to obtain `blueBeltPromotionAC`, then assert the malicious accept fails. Do NOT accept it normally first.

```haskell
    maliciousAcceptWithoutUserNftFails :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    maliciousAcceptWithoutUserNftFails TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000
      txBuildingContext <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000
      (_gyTxId, masterAC) <-
        bjjInteraction
          txBuildingContext
          (w1 testWallets)
          (CreateProfileWithRankAction masterProfileData Practitioner creationDate Black)
          Nothing
      (_gyTxId, studentAC) <-
        bjjInteraction
          txBuildingContext
          (w2 testWallets)
          (InitProfileAction studentProfileData Practitioner creationDate)
          Nothing
      waitNSlots_ 2
      s' <- slotOfCurrentBlock
      blueBeltDate <- slotToBeginTime s'
      (_gyTxId, blueBeltPromotionAC) <-
        bjjInteraction
          txBuildingContext
          (w1 testWallets)
          ( PromoteProfileAction
              { promoted_profile_id = studentAC,
                promoted_by_profile_id = masterAC,
                achievement_date = blueBeltDate,
                promoted_belt = Blue
              }
          )
          Nothing
      -- Attacker w3 tries to accept the promotion WITHOUT spending the student's User NFT.
      -- RanksValidator (via deriveUserFromRefAC) must reject this.
      mustFail $
        void $
          maliciousBjjAcceptPromotion txBuildingContext (w3 testWallets) blueBeltPromotionAC
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Malicious AcceptPromotion without User NFT correctly rejected!"
      return ()
```

- [ ] **Step 3: Register the test** in the `promotionSecurityTests` list (`Promotion.hs:251-253`):

```haskell
    [ mkTestFor "Test Case 2.1: Multiple masters can create promotions for same student" multipleMastersCanPromote,
      mkTestFor "Test Case 2.2: Sequential promotions from same master work correctly" sequentialPromotionsWork,
      mkTestFor "Test Case 2.3: Accepting a promotion without the student's User NFT fails (RanksValidator)" maliciousAcceptWithoutUserNftFails
    ]
```

- [ ] **Step 4: Build + test.** Run `cabal build all` then `cabal test 2>&1 | tail -3`. Expected: green, and the "Test Case 2.3" test runs. If the malicious tx SUCCEEDS (mustFail fails), STOP and escalate (real vuln).

- [ ] **Step 5: Commit.**

```bash
git add src/test/UnitTests/Promotion.hs
git commit -m "test: activate malicious-promotion security test (F-33 gap #2)"
```

---

### Task 2: Promotion double-accept negative (gap #1)

**Files:**
- Modify: `src/test/UnitTests/Promotion.hs` (`promotionSecurityTests` group + `where`)

**Interfaces:**
- Consumes: `AcceptPromotionAction :: GYAssetClass -> ProfileActionType`, `mustFail`, same setup builders as Task 1.

- [ ] **Step 1: Add the test function** in the `promotionSecurityTests` `where` block. Setup mirrors `sequentialPromotionsWork` through the first accept, then a second accept must fail (RanksValidator rejects re-accepting an already-accepted/at-rank promotion).

```haskell
    doubleAcceptPromotionFails :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    doubleAcceptPromotionFails TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000
      txBuildingContext <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000
      (_gyTxId, masterAC) <-
        bjjInteraction
          txBuildingContext
          (w1 testWallets)
          (CreateProfileWithRankAction masterProfileData Practitioner creationDate Black)
          Nothing
      (_gyTxId, studentAC) <-
        bjjInteraction
          txBuildingContext
          (w2 testWallets)
          (InitProfileAction studentProfileData Practitioner creationDate)
          Nothing
      waitNSlots_ 2
      s' <- slotOfCurrentBlock
      blueBeltDate <- slotToBeginTime s'
      (_gyTxId, blueBeltPromotionAC) <-
        bjjInteraction
          txBuildingContext
          (w1 testWallets)
          ( PromoteProfileAction
              { promoted_profile_id = studentAC,
                promoted_by_profile_id = masterAC,
                achievement_date = blueBeltDate,
                promoted_belt = Blue
              }
          )
          Nothing
      void $
        bjjInteraction
          txBuildingContext
          (w2 testWallets)
          (AcceptPromotionAction blueBeltPromotionAC)
          Nothing
      waitNSlots_ 1
      -- Second acceptance of the same promotion must fail.
      mustFail $
        void $
          bjjInteraction
            txBuildingContext
            (w2 testWallets)
            (AcceptPromotionAction blueBeltPromotionAC)
            Nothing
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Double-accept promotion correctly rejected!"
      return ()
```

- [ ] **Step 2: Register** in the `promotionSecurityTests` list:

```haskell
      mkTestFor "Test Case 2.4: Accepting the same promotion twice fails" doubleAcceptPromotionFails
```

- [ ] **Step 3: Build + test.** `cabal build all` then `cabal test 2>&1 | tail -3`. Green; "Test Case 2.4" runs. If the second accept SUCCEEDS, STOP and escalate.

- [ ] **Step 4: Commit.**

```bash
git add src/test/UnitTests/Promotion.hs
git commit -m "test: promotion double-accept negative (F-33 gap #1)"
```

---

### Task 3: Authorization negatives — non-owner update, wrong-user interval accept, non-admin oracle (gaps #3, #6, #7)

**Files:**
- Modify: `src/test/UnitTests/Promotion.hs` (non-owner UpdateProfile)
- Modify: `src/test/UnitTests/Membership.hs` (wrong-user AcceptMembershipInterval)
- Modify: `src/test/UnitTests/Oracle.hs` (non-admin oracle update)

**Interfaces:**
- Consumes: `UpdateProfileAction :: GYAssetClass -> Maybe BuiltinByteString -> ImageURI -> ProfileActionType` (usage: `UpdateProfileAction profileRefAC Nothing "ipfs://…"`, `Promotion.hs:158`).
- Consumes: `AcceptMembershipIntervalAction` (see the existing happy-path accept test in `Membership.hs`; mirror its setup, then swap the accepting wallet).
- Consumes: `adminInteraction :: DeployedScriptsContext -> User -> AdminActionType -> m …` and `PauseProtocolAction :: AdminActionType` (usage in `Oracle.hs`).

- [ ] **Step 1 (Promotion.hs): non-owner UpdateProfile.** Add to `promotionSecurityTests` `where` + register. w1 creates a profile; w2 (no User NFT) attempts to update it.

```haskell
    nonOwnerUpdateProfileFails :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    nonOwnerUpdateProfileFails TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000
      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000
      (_txId, profileRefAC) <-
        bjjInteraction
          ctx
          (w1 testWallets)
          (InitProfileAction studentProfileData Practitioner creationDate)
          Nothing
      waitNSlots_ 1
      -- w2 does not hold the profile's User NFT and must not be able to update it.
      mustFail $
        void $
          bjjInteraction
            ctx
            (w2 testWallets)
            (UpdateProfileAction profileRefAC Nothing "ipfs://QmAttackerImage")
            Nothing
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Non-owner UpdateProfile correctly rejected!"
      return ()
```

Register: `mkTestFor "Test Case 2.5: UpdateProfile by a non-owner fails" nonOwnerUpdateProfileFails`.

- [ ] **Step 2 (Membership.hs): wrong-user AcceptMembershipInterval.** Locate the existing happy-path accept-interval test (the report identified `acceptMembershipIntervalTest`, ~`Membership.hs:122-188`). Copy its setup verbatim into a new local test function, but change the accepting wallet to a wallet that does NOT own the practitioner profile, and wrap the accept in `mustFail`. Use the same action constructor and arguments the happy-path test uses; only the acting `User` changes and the accept is wrapped. Register: `mkTestFor "Test Case 4.13: AcceptMembershipInterval by the wrong user fails" wrongUserAcceptIntervalFails` in the group where the other membership `mkTestFor` entries live. (Read the file to confirm the exact setup fixtures and the membership group's registration list before writing — do not invent field names.)

- [ ] **Step 3 (Oracle.hs): non-admin oracle update.** Add a test mirroring the admin setup in `sequentialAdminWithProfiles` but where a non-admin wallet (e.g. `w2`, while `w1` is admin) attempts `PauseProtocolAction`:

```haskell
    nonAdminOracleUpdateFails :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    nonAdminOracleUpdateFails TestInfo {..} = do
      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1
      -- w2 is not opAdminPkh; OracleValidator must reject its admin action.
      mustFail $
        void $
          adminInteraction ctx (w2 testWallets) PauseProtocolAction
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Non-admin oracle update correctly rejected!"
      return ()
```

Register in the Oracle test group list: `mkTestFor "Test Case 0.6: Non-admin oracle update fails" nonAdminOracleUpdateFails`. (Confirm `adminInteraction` is imported in `Oracle.hs`; the report shows it is used there.)

- [ ] **Step 4: Build + test.** `cabal build all` then `cabal test 2>&1 | tail -3`. Green; the three new tests run. Any that unexpectedly SUCCEEDS → STOP and escalate.

- [ ] **Step 5: Commit.**

```bash
git add src/test/UnitTests/Promotion.hs src/test/UnitTests/Membership.hs src/test/UnitTests/Oracle.hs
git commit -m "test: authorization negatives — non-owner update, wrong-user interval, non-admin oracle (F-33 gaps #3/#6/#7)"
```

---

### Task 4: Real pause-gate test (replace stale skip) + fee-delivery positive (gaps #4, #5a)

**Files:**
- Modify: `src/test/UnitTests/Oracle.hs` (un-skip the pause step in `sequentialAdminWithProfiles` ~lines 177-187; add a fee-delivery test + register it)

**Interfaces:**
- Consumes: `adminInteraction`, `PauseProtocolAction`, `SetFeesAction :: Maybe FeeConfig -> AdminActionType`, `FeeConfig{ fcFeeAddress, fcProfileCreationFee, … }` (`Onchain.Protocol.Types`), `queryOracle`, `withWalletBalancesCheck`, `valueFromLovelace`, `addressToPlutus`, `User.userChangeAddress`, `bjjInteraction`, `InitProfileAction`.

- [ ] **Step 1: Un-skip the pause test.** In `sequentialAdminWithProfiles` (`Oracle.hs`), replace the two stale skip log lines (~185-186: "Skipping paused profile creation test (CLB limitation…)" and the "Note:" line) with a real assertion that minting while paused fails. The protocol is already paused at that point (the code asserts `opPaused params1` just above). Insert:

```haskell
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Step 3: Attempting profile creation while paused (must fail)."
      mustFail $
        void $
          bjjInteraction ctx (w1 testWallets) (InitProfileAction studentProfileData Practitioner creationDate) Nothing
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Paused profile creation correctly rejected (ProtocolPaused)."
```

Use whatever `ctx`/`creationDate` binding names already exist in that test's scope (read the surrounding code; if `creationDate` isn't in scope, derive it as the other tests do via `slotOfCurrentBlock`/`slotToBeginTime`, and import `studentProfileData` from `Test.Fixtures` if not already imported).

- [ ] **Step 2: Add the fee-delivery test.** Configure a profile-creation fee routed to `w2`, create a profile as `w1`, and assert `w2` receives exactly the fee.

```haskell
    feeReachesFeeAddress :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    feeReachesFeeAddress TestInfo {..} = do
      waitNSlots_ 1000
      s <- slotOfCurrentBlock
      t <- slotToBeginTime s
      let creationDate = timeFromPOSIX $ timeToPOSIX t - 100000
      ctx <- deployBJJValidators (w1 testWallets)
      waitNSlots_ 1000
      let feeAddr = addressToPlutus (User.userChangeAddress (w2 testWallets))
      let feeConfig =
            FeeConfig
              { fcFeeAddress = feeAddr,
                fcProfileCreationFee = 2000000,
                fcPromotionFee = 3000000,
                fcMembershipHistoryFee = 1500000,
                fcMembershipIntervalFee = 1500000,
                fcAchievementFee = 1500000
              }
      _ <- adminInteraction ctx (w1 testWallets) (SetFeesAction (Just feeConfig))
      waitNSlots_ 1
      -- w2 is the fee recipient (not the actor), so its delta is exactly the fee.
      withWalletBalancesCheck [w2 testWallets := valueFromLovelace 2000000] $
        void $
          bjjInteraction ctx (w1 testWallets) (InitProfileAction studentProfileData Practitioner creationDate) Nothing
      gyLogInfo' ("TESTLOG" :: GYLogNamespace) "Profile-creation fee delivered to fee address!"
      return ()
```

Confirm the imports: `FeeConfig(..)` from `Onchain.Protocol.Types`, `withWalletBalancesCheck` from `GeniusYield.Test.Utils`, `addressToPlutus`, `(:=)`, `valueFromLovelace` from `GeniusYield.Types`, `User` qualified as used elsewhere in the file. Match the file's existing import style (the Oracle admin tests already build a `FeeConfig` this way — reuse those imports).

- [ ] **Step 3: Register** the fee-delivery test in the Oracle group list: `mkTestFor "Test Case 0.7: Profile-creation fee is delivered to the fee address" feeReachesFeeAddress`.

- [ ] **Step 4: Build + test.** `cabal build all` then `cabal test 2>&1 | tail -3`. Green; the un-skipped pause assertion and the fee test run. If paused creation SUCCEEDS, or the fee balance delta mismatches, STOP and escalate (the mismatch may reveal the fee is not actually delivered / a wrong selector).

- [ ] **Step 5: Commit.**

```bash
git add src/test/UnitTests/Oracle.hs
git commit -m "test: real pause-gate test + fee-delivery positive (F-33 gaps #4/#5a)"
```

---

### Task 5: OracleNFTPolicy exact-mint negatives (gap #10 — pins F-22a)

**Files:**
- Modify: `src/test/TestRuns.hs` (add test-only malicious mint helpers next to `mintTestOracleNFT`, ~line 161; export them)
- Modify: `src/test/UnitTests/Oracle.hs` (register two tests)

**Interfaces:**
- Consumes/pattern: `mintTestOracleNFT` (`TestRuns.hs:120-161`): `someUTxOWithoutRefScript`, `txOutRefToV3Plutus`, `compileOracleNFTPolicy`, `mintingPolicyId`, `GYMintScript @'PlutusV3`, `redeemerFromPlutusData ()`, `mustMint mp redeemer tn amount`, `mustHaveInput (GYTxIn seed GYTxInWitnessKey)`, `txMustLockStateWithInlineDatumAndValue`, `sendSkeleton'`, `valueSingleton`, `valueFromLovelace`.
- The new N2 check (`OracleNFTPolicy.hs:36-42`) requires `mintValueMinted txInfoMint == theOracleNFT` where `theOracleNFT` has the **empty** token name. Any deviation (extra token, non-empty name) must fail N2.

- [ ] **Step 1: Add two malicious mint helpers** to `TestRuns.hs` and export them (add both names to the module export list at the top of `TestRuns.hs`). Each mirrors `mintTestOracleNFT`'s seed-spend + policy compile, but mints a value that violates exact-mint. Lock the oracle datum + minted value at the oracle validator so the tx is balanced/spendable (the policy does not constrain outputs, only the mint + seed).

```haskell
-- | MALICIOUS: attempt to mint the oracle NFT PLUS an extra token under the one-shot policy.
-- Must fail OracleNFTPolicy N2 (exact-mint: mintValueMinted == theOracleNFT).
maliciousMintOracleNFTWithExtraToken :: (GYTxGameMonad m, HasCallStack) => User -> m GYAssetClass
maliciousMintOracleNFTWithExtraToken w = asUser w $ do
  seedGYRef <- someUTxOWithoutRefScript
  let seedPlutus = txOutRefToV3Plutus seedGYRef
  let oracleNFTPolicyGY = compileOracleNFTPolicy seedPlutus
  let oracleNFTMPId = mintingPolicyId oracleNFTPolicyGY
  let theOracleNFTAC = GYToken oracleNFTMPId ""
  let adminPkh = userPlutusPkh w
  let initialOracleParams = OracleParams {opAdminPkh = adminPkh, opPaused = False, opFeeConfig = Nothing, opMinUTxOValue = 1000000}
  let spendSeed = mustHaveInput (GYTxIn seedGYRef GYTxInWitnessKey)
  let mp = GYMintScript @'PlutusV3 oracleNFTPolicyGY
  let gyRedeemer = redeemerFromPlutusData ()
  -- MALICIOUS: mint the NFT AND an extra token name under the same policy.
  let mintNFT = mustMint mp gyRedeemer "" 1 <> mustMint mp gyRedeemer "EXTRA" 1
  lockOutput <-
    txMustLockStateWithInlineDatumAndValue
      oracleValidatorGY
      initialOracleParams
      (valueSingleton theOracleNFTAC 1 <> valueSingleton (GYToken oracleNFTMPId "EXTRA") 1 <> valueFromLovelace 3500000)
  void $ sendSkeleton' $ mconcat [spendSeed, mintNFT, lockOutput]
  return theOracleNFTAC

-- | MALICIOUS: attempt to mint the oracle NFT with a NON-EMPTY token name.
-- Must fail OracleNFTPolicy N2 (theOracleNFT uses the empty token name).
maliciousMintOracleNFTWrongName :: (GYTxGameMonad m, HasCallStack) => User -> m GYAssetClass
maliciousMintOracleNFTWrongName w = asUser w $ do
  seedGYRef <- someUTxOWithoutRefScript
  let seedPlutus = txOutRefToV3Plutus seedGYRef
  let oracleNFTPolicyGY = compileOracleNFTPolicy seedPlutus
  let oracleNFTMPId = mintingPolicyId oracleNFTPolicyGY
  let wrongAC = GYToken oracleNFTMPId "ORACLE"
  let adminPkh = userPlutusPkh w
  let initialOracleParams = OracleParams {opAdminPkh = adminPkh, opPaused = False, opFeeConfig = Nothing, opMinUTxOValue = 1000000}
  let spendSeed = mustHaveInput (GYTxIn seedGYRef GYTxInWitnessKey)
  let mp = GYMintScript @'PlutusV3 oracleNFTPolicyGY
  let gyRedeemer = redeemerFromPlutusData ()
  -- MALICIOUS: non-empty token name.
  let mintNFT = mustMint mp gyRedeemer "ORACLE" 1
  lockOutput <-
    txMustLockStateWithInlineDatumAndValue
      oracleValidatorGY
      initialOracleParams
      (valueSingleton wrongAC 1 <> valueFromLovelace 3500000)
  void $ sendSkeleton' $ mconcat [spendSeed, mintNFT, lockOutput]
  return wrongAC
```

If a helper (`oracleValidatorGY`, `compileOracleNFTPolicy`, `userPlutusPkh`, `valueSingleton`) is not already in scope in `TestRuns.hs`, it is — `mintTestOracleNFT` uses all of them; reuse the same references and imports.

- [ ] **Step 2: Register two tests** in `Oracle.hs`, each wrapping a helper in `mustFail`:

```haskell
    oracleExtraTokenMintFails :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    oracleExtraTokenMintFails _ =
      mustFail $ void $ maliciousMintOracleNFTWithExtraToken (w1 testWallets)

    oracleWrongNameMintFails :: (HasCallStack) => TestInfo -> GYTxMonadClb ()
    oracleWrongNameMintFails _ =
      mustFail $ void $ maliciousMintOracleNFTWrongName (w1 testWallets)
```

Wait — `TestInfo {..}` is needed to bind `testWallets`. Use `oracleExtraTokenMintFails TestInfo {..} = mustFail $ void $ maliciousMintOracleNFTWithExtraToken (w1 testWallets)` (bind the record). Add the two helper names to the `TestRuns` import in `Oracle.hs`. Register:

```haskell
      mkTestFor "Test Case 0.8: Minting the oracle NFT with an extra token fails (F-22a)" oracleExtraTokenMintFails,
      mkTestFor "Test Case 0.9: Minting the oracle NFT with a non-empty token name fails (F-22a)" oracleWrongNameMintFails
```

- [ ] **Step 3: Build + test.** `cabal build all` then `cabal test 2>&1 | tail -3`. Green; both tests run and fail-as-expected. If either malicious mint SUCCEEDS, STOP and escalate (F-22a hole).

- [ ] **Step 4: Commit.**

```bash
git add src/test/TestRuns.hs src/test/UnitTests/Oracle.hs
git commit -m "test: OracleNFTPolicy exact-mint negatives — pins F-22a (F-33 gap #10)"
```

---

### Task 6: F-21 burn-along-mint pin (gap #9 — pins F-21) — feasibility-gated

**Files:**
- Modify: `src/test/TestRuns.hs` (add a test-only malicious builder; export it)
- Modify: `src/test/UnitTests/Promotion.hs` or `Oracle.hs` (register one test)

**Goal:** Prove the stream-E `mintValueBurned txInfoMint == mempty` conjunct rejects a tx that runs a mint redeemer while ALSO burning a protocol token under the same policy/redeemer.

**Approach:** Create a profile normally (w1 now holds that profile's User NFT). Build the offchain create-profile skeleton for a SECOND profile via the normal builder, then `mconcat` an extra `mustMint protocolMP <CreateProfile redeemer> <heldUserTokenName> (-1)` so `txInfoMint` carries a burn under the same `CreateProfile` redeemer invocation. Submit; `mustFail`.

**Interfaces:**
- The protocol minting policy handle: `compileMintingPolicy oracleNFT` (`TxBuilding/Validators.hs:151`). The oracle AC is available from `deployBJJValidators`/context.
- The `CreateProfile` minting redeemer and its exact data: read `MintingPolicy.hs` + the offchain `createProfileWithRankTX` (`Operations.hs:145-190`) to reproduce the redeemer with matching fields.
- `mustMint`, `mconcat`, `sendSkeleton'`, `redeemerFromPlutusData`.

- [ ] **Step 1: Attempt the malicious builder.** Add `maliciousCreateProfileWithBurn` to `TestRuns.hs`. It must reuse the SAME `CreateProfile` redeemer value the real builder uses so GY merges the extra burn into the one mint entry (`{newRef+1, newUser+1, victimUser-1}`). If reproducing the exact redeemer/skeleton is impractical, an acceptable alternative is to obtain the real create-profile skeleton from `interactionToTxSkeleton (InitProfileAction …)` and `mconcat` the burn onto it, relying on GY to merge same-policy same-redeemer mints. Wrap execution so a test can call it.

- [ ] **Step 2: Register one test** wrapping it in `mustFail`:

```haskell
      mkTestFor "Test Case: Minting while burning a protocol token fails (F-21)" burnAlongMintFails
```

- [ ] **Step 3: Build + test.** `cabal build all` then `cabal test 2>&1 | tail -3`.
  - If the test builds and fails-as-expected (green under `mustFail`): DONE — F-21 is pinned. Commit.
  - **Feasibility gate:** if the malicious skeleton cannot be constructed (GY refuses to merge the mints, or balancing rejects the burn before validation, or the redeemer cannot be reproduced) after a genuine attempt, DO NOT ship a contrived or always-passing test. Instead: remove the broken test, and add a short note to the stream-F spec's "deferred" section documenting the exact obstruction with code evidence (which call failed and why), citing that stream E already verified F-21 is a happy-path no-op. Report this outcome in the task report. If instead the burn tx SUCCEEDS at validation, STOP and escalate (F-21 hole — a real finding).

- [ ] **Step 4: Commit** (whichever outcome):

```bash
git add src/test/TestRuns.hs src/test/UnitTests/Promotion.hs docs/superpowers/specs/2026-07-05-extended-tests-design.md
git commit -m "test: F-21 burn-along-mint pin (F-33 gap #9)"   # or: "docs: document F-21 burn-pin infeasibility with evidence"
```

---

## Self-Review

- **Spec coverage:** Task 1↔gap#2, Task 2↔gap#1, Task 3↔gaps#3/#6/#7, Task 4↔gaps#4/#5a, Task 5↔gap#10 (F-22a), Task 6↔gap#9 (F-21). Deferred gaps (#5b fee-underpay, F-22b spend-without-NFT, on-chain M0) documented in the spec — no task, by design.
- **Type consistency:** action constructors match verified usages — `CreateProfileWithRankAction data type date belt`, `InitProfileAction data type date`, `PromoteProfileAction{promoted_profile_id, promoted_by_profile_id, achievement_date, promoted_belt}`, `AcceptPromotionAction ac`, `UpdateProfileAction ac (Maybe desc) image`, `SetFeesAction (Maybe FeeConfig)`, `PauseProtocolAction`. `mustFail`, `withWalletBalancesCheck [w := v]`, `mustMint mp r tn n` per harness.
- **Placeholder scan:** Tasks 1, 2, 3(step1), 4, 5 carry complete code. Task 3 steps 2-3 and Task 6 intentionally instruct the implementer to READ the exact existing setup/redeemer before writing (field names must not be invented) — this is a deliberate guard, not a placeholder. Task 6 is explicitly feasibility-gated with a defined fallback.
