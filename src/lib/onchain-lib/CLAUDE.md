# Onchain Library Rules

PlutusTx on-chain code. No off-chain imports (no Aeson, Servant, Swagger). PlutusTx types and instances only.

## Plutus Coding Style

- **INLINEABLE**: `{-# INLINEABLE fn #-}` **before** type signature for every cross-module function.
- **Trace messages**: Short unique codes: `traceIfFalse "M0" condition -- Description (M0)`. Same for `traceError`.
- **Validators**: Keep lambdas under ~80 lines. Extract per-redeemer logic into named helpers.
- **Output checks**: Use output-index checks only (no O(n) search). Redeemers carry expected output index.
  - **Min-value** (`checkTxOutAtIndexWithDatumMinValueAndAddress`): For continuing outputs (value ≥ spent value) and newly created state outputs (value ≥ minLv + NFT). Balancer may add lovelace.
  - **Exact-value** (`checkTxOutAtIndexWithDatumValueAndAddress`): Only when value is not modified by balancer (rare).
- **Validator structure**: `mkUntypedLambda` wraps typed lambda. Pattern-match `ScriptContext txInfo (Redeemer bredeemer) scriptInfo`; `SpendingScript spendingTxOutRef mdatum` for spending.
- **Constants**: Named constants from `Onchain.Utils`. No magic numbers.
- **Plutus version**: Plutus V3 with `ghc-options: -fplugin-opt PlutusTx.Plugin:target-version=1.1.0`.

## Project Onchain Concepts

- **Types**: New types in `Protocol/Types.hs`. Use `makeIsDataSchemaIndexed` with **stable** indices. Never reorder constructors. Add `HasBlueprintDefinition` for CIP-57.
- **ID derivation**: All deterministic token names in `Protocol/Id.hs`. `blake2b_224` for 28-byte names. CIP-68 ref/user tokens via `generateRefAndUserTN`.
- **Datum optimization**: Derive IDs from datum fields, don't store derivable IDs. Reduces datum size and min-ADA.
- **ProtocolParams**: Carried in datums for unparameterized validators. Only MintingPolicy is parameterized.
- **Address resolution**: ProfilesValidator and RanksValidator are **unparameterized**; they get validator addresses from the datum's `protocolParams` / `promotionProtocolParams`. Do not add validator hashes as validator parameters.
- **Multiple datum kinds**: Use wrapper sum type. List which redeemers each kind accepts; reject incompatible with `traceError`.
- **Output layout**: The order of outputs in the transaction is determined by the off-chain skeleton `mconcat`. Redeemer parameters (e.g. `profileOutputIdx`) must refer to that order. Document the output layout in a comment when adding or changing a redeemer.
- **Linked lists**: Generic list logic in `Onchain.LinkedList` (`NodeDatum` with `nodeKey`, `nextNodeKey`, `nodeData`). Protocol.Core uses it for membership histories; follow the same pattern for new list-like structures.

### Minting Policy

- New token kind → new redeemer in `MintingPolicy.hs`.
- Validate **exact mint**: `mintValueMinted == expectedValue`. Never allow extra token names.
- Oracle UTxO as **reference input** in every minting tx. Check `opPaused` and `checkFee`.
- Min lovelace from oracle (`opMinUTxOValue`) only, not fixed constants.
- One-off tokens: require spending a **seed TxOutRef**, derive token name from it.
- Validate referenced AssetClasses share protocol CurrencySymbol via `hasCurrencySymbol`.
- **Authorization**: Promotion requires master User NFT; membership ops require organization User NFT; achievement award requires awarder User NFT. Use `deriveUserFromRefAC` from `Onchain.CIP68`.
- **CurrencySymbol**: In the minting policy, pattern-match on `MintingScript mintingPolicyCurrencySymbol` and pass to handlers.
- **Fee check**: `checkFee` (in `Onchain.Protocol.Lookup`) takes `OracleParams`, a selector `(FeeConfig -> Integer)` (e.g. `fcProfileCreationFee`, `fcPromotionFee`), and outputs.

### Validators

- **Cleanup redeemer**: All state validators have it. Anyone can spend if datum absent/unparseable; must fail if datum valid.
- **Business logic**: In `Onchain.BJJ` and `Protocol.Core`. Validators call these; no complex logic in validator lambda.
- **CIP68Datum**: State with metadata uses `CIP68Datum OnchainProfile` etc. Use `extra` for inner datum.
- **Oracle validator**: Unparameterized; admin must sign; output preserves address and value ≥ spent. `applyOracleAdminAction` computes expected new datum on-chain. Use `Utils.checkTxOutAtIndexWithDatumMinValueAndAddress` with the spent input's value (balancer may add lovelace).
- **Expected datum on state change**: When the output datum may differ (e.g. oracle admin update), redeemer carries the action; validator computes the expected new datum on-chain. Keep off-chain and on-chain update logic in sync.
- **Token flow**: Tokens locked at a validator must remain at that validator (same address and value on output) unless the design explicitly burns or moves them. Continuing outputs must allow value ≥ spent value (use min-value check).
- **Lookups**: Use `Onchain.Protocol.Lookup` for reading state: `readOracleParams`, `unsafeGetRank`, `unsafeGetProfile`, `unsafeGetListNodeDatumAndValue`, `unsafeGetMembershipInterval`.

### Deployment

- Oracle NFT must mint before MintingPolicy compilation (parameterized by oracle AssetClass).
- Oracle NFT Policy is **inlined** (not deployed as reference script).

## Security Rules

Based on Cardano Developer Portal and MLabs vulnerability patterns.

1. **Other redeemer**: Explicitly require expected redeemer; new easy redeemers can bypass intended checks.
2. **Other token name**: Constrain **full** minted value, not just one token name. `assetClassValueOf` alone is insufficient.
3. **Arbitrary datum**: Validate datum type on every legit UTxO, even if not "used" by current path.
4. **Unbounded datum**: Upper-bound datum size; no ever-growing lists/maps.
5. **Unbounded value**: Disallow extra/foreign tokens in locked outputs.
6. **Unbounded inputs**: Ensure bounded input count per protocol tx.
7. **Double satisfaction**: Reason about **all** inputs, mints, and withdrawals, not just "own" input.
8. **Missing UTxO authentication**: Use unique NFT to authenticate protocol UTxOs (e.g., oracle).
9. **Time handling**: Use correct validity interval bound (lower for "past deadline", upper for "before start"). Never use interval midpoint.
10. **Token security**: Limit token types in protocol UTxOs. Test with enlarged values.
11. **UTxO contention**: Prefer parallelizable/distributed state over single global UTxO.
12. **Cheap spam**: Ensure legit actions complete despite attacker spam.
13. **Staking control**: Reason about full addresses (payment + staking credential).
14. **Locked value**: Document economic tradeoff when value is permanently locked.
