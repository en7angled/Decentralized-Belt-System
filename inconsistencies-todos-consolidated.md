# Codebase Inconsistencies & TODOs — Consolidated

All findings from rounds 1–4, deduplicated and organized by priority.

---

## HIGH PRIORITY

### 1. `txOutRefToV3Plutus` duplicated 5 times

The V1-to-V3 `TxOutRef` conversion is defined as a helper in two places and inlined in three more:

| Location | Form |
|----------|------|
| `TxBuilding/Transactions.hs:215–218` | Defines `txOutRefToV3Plutus` |
| `test/TestRuns.hs:92–95` | Copy-pasted duplicate (comment says "same as in Transactions.hs") |
| `TxBuilding/Skeletons.hs:39–40` | Inlined in `gyGenerateRefAndUserAC` |
| `TxBuilding/Operations.hs:134–135` | Inlined in `createProfileWithRankTX` |
| `TxBuilding/Operations.hs:282–283` | Inlined in `promoteProfileTX` |

All five sites do:
```haskell
let (V1.TxOutRef (V1.TxId bs) i) = txOutRefToPlutus ref
let v3Ref = V3.TxOutRef (V3.TxId bs) i
```

**Fix:** Move `txOutRefToV3Plutus` to `TxBuilding.Utils` and replace all 5 occurrences.

---

### 2. Admin CLI hardcodes config paths instead of using `Constants`

`admin/Main.hs` (lines 32–39) declares its own local constants:
```haskell
atlasCoreConfig = "config/config_atlas.json"
txBuldingContextFile = "config/config_bjj_validators.json"
```
These are identical to `Constants.defaultAtlasCoreConfig` and `Constants.defaultTxBuldingContextFile`, which all other executables (`interaction-api`, `query-api`, `chain-sync`) already import from `Constants`. The admin tool even imports `Constants qualified` but only uses `Constants.defaultBlueprintFile`.

**Fix:** Remove the local declarations and use the `Constants` module values directly.

---

### 3. Admin CLI uses `decodeConfigFile` instead of `decodeConfigEnvOrFile`

`admin/Main.hs` (lines 510, 521) is the only executable that uses `decodeConfigFile` instead of `decodeConfigEnvOrFile`. All three services support `ATLAS_CORE_CONFIG` and `DEPLOYED_VALIDATORS_CONFIG` env vars for Docker/CI. The admin CLI cannot be configured via env vars.

**Fix:**
```haskell
-- Replace:
mTxBuildingContext <- decodeConfigFile @DeployedScriptsContext txBuldingContextFile
atlasConfig <- ... decodeConfigFile @GYCoreConfig atlasCoreConfig
-- With:
mTxBuildingContext <- decodeConfigEnvOrFile "DEPLOYED_VALIDATORS_CONFIG" defaultTxBuldingContextFile
atlasConfig <- ... decodeConfigEnvOrFile "ATLAS_CORE_CONFIG" defaultAtlasCoreConfig
```

---

### 4. Hardcoded `GYTestnetPreview` in chain sync

In `ChainSyncLogic.hs` line 115:
```haskell
applyMatches GYTestnetPreview pool matches
```
The network ID is hardcoded instead of being read from configuration. The `chain-sync/Main.hs` already loads `DeployedScriptsContext` from config — the network ID should also be configurable.

**Fix:** Load `GYCoreConfig` (or an env var) in `chain-sync/Main.hs` and thread the network ID through to `fetchingMatches`/`applyMatches`.

---

## MEDIUM PRIORITY

### 5. `updateOracleTX` bypasses `txMustLockStateWithInlineDatumAndValue`

In `Operations.hs` (lines 455–462), the oracle re-lock output is built by hand with raw `mustHaveOutput GYTxOut{..}`, while every other operation in the same file uses the `txMustLockStateWithInlineDatumAndValue` wrapper from `Skeletons.hs`.

**Fix:**
```haskell
lockOracle <- txMustLockStateWithInlineDatumAndValue oracleValidatorGY newParams oracleValue
```
**Note:** May require adjusting the monad constraint from `GYTxQueryMonad` to `GYTxUserQueryMonad`, or generalizing the skeleton helper (see note at bottom).

---

### 6. `updateOracleTX` bypasses `txMustSpendStateFromRefScriptWithRedeemer`

In `Operations.hs` (lines 447–452), the oracle UTxO spend is manually assembled with `mustHaveInput` + `GYTxInWitnessScript (GYInReference ...)`, while all other validator spends use `txMustSpendStateFromRefScriptWithRedeemer`.

The difference is that `updateOracleTX` already has the `oracleRef` from `queryOracleParams` instead of looking it up by NFT. **Fix options:**
- Make the existing wrapper accept a pre-resolved UTxO ref and datum, or
- Extract the spend-construction part into a lower-level helper.

---

### 7. `getFeeSkeleton` bypasses `txIsPayingValueToAddress`

In `Operations.hs` (lines 49–62), the fee output is manually constructed with `mustHaveOutput GYTxOut{..}` (address + value, no datum). `Skeletons.hs` provides `txIsPayingValueToAddress` for exactly this pattern.

**Fix:**
```haskell
txIsPayingValueToAddress gyFeeAddr (valueFromLovelace feeAmount)
```
**Note:** `getFeeSkeleton` has `GYTxQueryMonad` while `txIsPayingValueToAddress` requires `GYTxUserQueryMonad`. Either relax the skeleton helper's constraint (it only wraps `mustHaveOutput`) or widen `getFeeSkeleton`'s constraint.

---

### 8. `mintOracleNFTAndLockDatum` / `mintTestOracleNFT` bypass skeleton wrappers

`Transactions.hs` `mintOracleNFTAndLockDatum` (lines 196–203) and `TestRuns.hs` `mintTestOracleNFT` (lines 140–147) contain nearly identical logic for minting the oracle NFT, both using raw `mustHaveOutput GYTxOut{..}` instead of `txMustLockStateWithInlineDatumAndValue`.

**Fix:**
```haskell
lockOutput <- txMustLockStateWithInlineDatumAndValue oracleValidatorGY initialOracleParams (valueSingleton theOracleNFTAC 1 <> valueFromLovelace 3500000)
```
Consider extracting a shared oracle-minting skeleton builder that both production deployment and test harness call.

---

### 9. `queryOracleParams` manually filters UTxOs instead of using `getUtxoWithTokenAtAddresses`

In `Lookups.hs` (lines 172–182), `queryOracleParams` manually does `utxosAtAddresses -> filterUTxOs -> utxosToList -> pattern match singleton` — the exact same lookup-and-match-single pattern as `getUtxoWithTokenAtAddresses` defined in the same file (line 32).

The only reason for not using it is the error case: throws `OracleNotFound` vs `ProfileNotFound`.

**Fix:**
```haskell
utxo <- getUtxoWithTokenAtAddresses oracleAC [oracleAddr]
```
Then parse the datum from the UTxO. Either parameterize the error in `getUtxoWithTokenAtAddresses`, or catch/rethrow for the oracle case.

---

### 10. `queryOracleParams` reinvents inline datum parsing instead of using `getInlineDatumAndValue`

In `Lookups.hs` (lines 184–187), `queryOracleParams` manually pattern-matches `GYOutDatumInline d -> fromBuiltinData (datumToPlutus' d)`, while `TxBuilding.Utils` provides `getInlineDatumAndValue` for extracting inline datum + value — consistent with profile/rank datum parsing.

**Fix:** Use `getInlineDatumAndValue utxo` followed by `fromBuiltinData . datumToPlutus'` on the datum.

---

### 11. No `getNetworkId` wrapper for repeated `cfgNetworkId . ctxCoreCfg` pattern

The expression `cfgNetworkId $ ctxCoreCfg providerCtx` (or variations) appears 7+ times:
- `Context.hs` lines 56, 72, 101
- `Transactions.hs` lines 79, 159
- `Query/Live.hs` lines 33, 38, 72, 116

**Fix:** Add to `TxBuilding.Context`:
```haskell
getNetworkId :: ProviderCtx -> GYNetworkId
getNetworkId = cfgNetworkId . ctxCoreCfg
```

---

## LOW PRIORITY

### 12. `pkhFromSkey` duplicates key derivation from `addressFromPaymentSigningKey`

`Transactions.hs` (lines 83–85) `pkhFromSkey` performs the same `getVerificationKey -> castVerificationKey -> paymentVerificationKeyFromApi -> paymentKeyHash` chain that `Utils.hs` `addressFromPaymentSigningKey` (lines 37–43) does internally.

**Fix:** Extract `pkhFromExtendedSkey :: GYExtendedPaymentSigningKey -> GYPaymentKeyHash` into `TxBuilding.Utils`:
```haskell
addressFromPaymentSigningKey nid skey = addressFromPaymentKeyHash nid (pkhFromExtendedSkey skey)
```

---

### 13. `getProfileRanks` uses pattern matching instead of `extra` accessor

In `Lookups.hs` line 68, `getProfileRanks` destructures the CIP68Datum via pattern matching:
```haskell
(CIP68Datum _metadata _version profile, _profileValue) <- getProfileStateDataAndValue profileRef
```
Everywhere else in the codebase, the `extra` accessor from `Onchain.CIP68` is used:
```haskell
let plutusStudentProfile = extra plutusStudentProfileDatum
```

**Fix:** Use `extra` instead for consistency and resilience to `CIP68Datum` internal changes.

---

### 14. Inconsistent redeemer construction style

Most operations use `redeemerFromPlutus' . toBuiltinData $ redeemer` (two-step composition), while `updateOracleTX` and `mintOracleNFTAndLockDatum` use `redeemerFromPlutusData ()` (one-step wrapper).

Both work identically — `redeemerFromPlutusData` calls `toBuiltinData` internally.

**Fix:** Standardize on `redeemerFromPlutusData` everywhere, or document the convention.

---

### 15. Malicious test uses `unitRedeemer` instead of typed redeemer

In `TestRuns.hs` (line 280), the malicious `AcceptPromotion` test uses `unitRedeemer` when spending the promotion rank UTxO. The real `acceptPromotionTX` in `Operations.hs` (line 398) uses `PromotionAcceptance profileOutputIdx rankOutputIdx`.

This means the malicious test may fail for the wrong reason (bad redeemer deserialization) rather than the intended reason (missing User NFT consent).

**Fix:** Use `PromotionAcceptance profileOutputIdx rankOutputIdx` as the redeemer so the test isolates the exact vulnerability it probes.

---

### 16. Two different `whenJust` definitions with different signatures

`Query.Common` (line 42):
```haskell
whenJust :: Maybe a -> (a -> b) -> b -> b
```
`Query.Projected` (line 22):
```haskell
whenJust :: Maybe a -> (a -> SqlQuery ()) -> SqlQuery ()
```
Different functions, same name. `Query.Projected` doesn't import `Query.Common.whenJust`.

**Fix:** Rename one or unify. The `Query.Projected` version can be expressed as `maybe (pure ()) f ma` — consider using `Data.Foldable.for_` or `traverse_` from base.

---

### 17. `chain-sync/Main.hs` accesses `mintingPolicyHashAndRef` directly

Line 77:
```haskell
let (mpHash, _mpRef) = mintingPolicyHashAndRef deployedScriptsContext
```
`Context.hs` provides getter functions (`getMintingPolicyRef`, etc.) but only for the `snd` (ref), not the `fst` (hash).

**Fix:** Add `getMintingPolicyHash :: DeployedScriptsContext -> GYScriptHash` to `Context.hs`.

---

### 18. Duplicated error handling in `InteractionAppMonad`

`buildInteractionApp` and `submitTxApp` in `InteractionAppMonad.hs` follow an identical try/catch/log/throwError pattern:
```haskell
res <- liftIO $ try (runReaderT someAction txBuildingContext)
case res of
  Left (e :: GYTxMonadException) -> do
    liftIO $ putStrLn $ "GYTxMonadException: \n" <> show e
    throwError err400 {errBody = BL8.pack (show e)}
  Right ok -> pure ok
```

**Fix:** Extract a helper:
```haskell
runWithTxContext :: ReaderT TxBuildingContext IO a -> InteractionAppMonad a
```

---

### 19. Hardcoded version string `"1.0.0"` in probe responses

The version `"1.0.0"` is hardcoded in both `InteractionAppMonad.hs` and `QueryAppMonad.hs` inside probe responses.

**Fix:** Define a shared constant in `Constants.hs` or `WebAPI.ServiceProbe`.

---

### 20. Unused abstractions (dead code)

| Function | Location | Notes |
|----------|----------|-------|
| `txIsPayingValueToAddressWithInlineDatum` | `Skeletons.hs:74` | Never used |
| `txMustBurnCIP68UserAndRef` | `Skeletons.hs:182` | Expected (no delete), but could be annotated |
| `getValueBalance` / `getAdaBalance` | `TxBuilding/Utils.hs:31–35` | Appear unused |
| `txIsValidForSafeEra` / `isValidBetween` | `Skeletons.hs:85–109` | Appear unused |
| `whenJust` | `Query/Common.hs:42` | Defined but unused |

**Fix:** Remove dead code or annotate with comments explaining future intent.

---

## Summary Table

| # | Severity | Effort  | Description |
|---|----------|---------|-------------|
| 1 | High     | Low     | `txOutRefToV3Plutus` duplicated 5 times |
| 2 | High     | Trivial | Admin CLI hardcodes paths from `Constants` |
| 3 | High     | Low     | Admin CLI missing env-var config support |
| 4 | High     | Medium  | Hardcoded `GYTestnetPreview` in chain sync |
| 5 | Medium   | Low     | `updateOracleTX` bypasses lock skeleton |
| 6 | Medium   | Low     | `updateOracleTX` bypasses spend skeleton |
| 7 | Medium   | Low     | `getFeeSkeleton` bypasses payment skeleton |
| 8 | Medium   | Low     | Oracle mint functions bypass skeleton wrappers |
| 9 | Medium   | Low     | `queryOracleParams` doesn't use `getUtxoWithTokenAtAddresses` |
| 10 | Medium  | Low     | `queryOracleParams` doesn't use `getInlineDatumAndValue` |
| 11 | Medium  | Trivial | No `getNetworkId` wrapper for `ProviderCtx` |
| 12 | Low     | Low     | `pkhFromSkey` duplicates key derivation |
| 13 | Low     | Trivial | `getProfileRanks` uses pattern match instead of `extra` |
| 14 | Low     | Trivial | Inconsistent redeemer construction style |
| 15 | Low     | Trivial | Malicious test uses wrong redeemer structure |
| 16 | Low     | Trivial | Two different `whenJust` with same name |
| 17 | Low     | Trivial | `chain-sync` accesses tuple directly, no getter |
| 18 | Low     | Low     | Duplicated error handling in `InteractionAppMonad` |
| 19 | Low     | Trivial | Hardcoded version string in probes |
| 20 | Low     | Low     | Unused abstractions / dead code |

---

## Cross-cutting note: monad constraints

Items 5, 6, 7, and 8 share a common blocker: several skeleton helpers (`txMustLockStateWithInlineDatumAndValue`, `txIsPayingValueToAddress`) require `GYTxUserQueryMonad` while some callers only have `GYTxQueryMonad`. These helpers only call `scriptAddress` and `return`, both available in `GYTxQueryMonad`. **Generalizing them to `GYTxQueryMonad` would unblock all four items at once.**
