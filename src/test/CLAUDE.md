# Testing Rules

## Test Layout

- **Unit tests**: `UnitTests.hs` + `UnitTests/<Topic>.hs`
- **Integration**: `TestRuns.hs`
- **Property tests**: `BJJPropertyTests.hs`
- **Run**: `cabal test` from repo root. ExUnits/script-size reporting: `scripts/test_exunits.sh`.

## Atlas Unified Testing

- **Backends**: Same operations run on **CLB emulator** (fresh ledger, fast) or **privnet** (shared network).
  - Emulator: `mkTestFor`
  - Privnet: `mkPrivnetTestFor` inside `withPrivnet`
- **Test environment**: `TestInfo` provides `testWallets :: Wallets`. Each `User` has `userPaymentSKey`, `userAddresses`, `userChangeAddress`, `userCollateral`. Wallets are pre-funded.

## Test Runners

Use `GYTxGameMonad`:
```haskell
asUser user $ do
  skeleton <- myOperation ...
  buildTxBody skeleton >>= signAndSubmitConfirmed
```
Add a runner for every new operation that should be tested end-to-end.

## Assertions

- **Balance checks**: `withWalletBalancesCheckSimple [wallet := valueDelta] $ do ...` (framework accounts for fees). `withWalletBalancesCheck` for finer control.
- **Negative tests**: Emulator: `mustFail`. Privnet: `handleError` and match expected exception (e.g. `GYBuildTxException GYBuildTxBodyErrorAutoBalance {}`).

## Scripts

- `scripts/test_black_promotes_white_to_blue.sh`: End-to-end smoke test mirroring `blackPromotesWhiteToBlue` flow.
- `scripts/populate_testnet.sh`: Populate testnet with sample data.
- `scripts/test_exunits.sh`: Parse exUnits, script sizes, balance changes from test output.
- Update scripts when changing admin CLI, config, or test flows.
