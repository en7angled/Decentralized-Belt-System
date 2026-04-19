# Add Error Handling

## Overview

Add robust error handling to Haskell code in the Decentralized Belt System, using the project's `TxBuildingException` type and idiomatic Haskell error patterns. Errors must map cleanly to HTTP status codes via `txBuildingExceptionToHttpStatus`.

## Steps

1. **Identify Failure Points**
    - Find partial functions (`head`, `tail`, `fromJust`, `read`) and replace with total alternatives
    - Locate uncaught exceptions in `GYTxMonad` and `GYTxQueryMonad` operations
    - Check UTxO lookups, datum parsing, and script execution paths for missing error handling
    - Review external calls: Atlas provider queries, IPFS uploads, config file loading

2. **Use TxBuildingException**
    - Define new error constructors in `TxBuilding/Exceptions.hs` for each failure mode
    - Use `throwError` / `throwTxBuildingException` rather than raw `error` strings
    - Ensure every exception constructor maps to the correct HTTP status via `txBuildingExceptionToHttpStatus`
    - Include context in error values (practitioner ID, action type, expected vs actual)

3. **Haskell Error Patterns**
    - Use `Either` / `ExceptT` for recoverable errors in pure or monadic code
    - Use `Maybe` with explicit handling (`maybe`, `fromMaybe`, pattern match) — never `fromJust`
    - Use `catch` / `try` from `Control.Exception` only at IO boundaries (API handlers)
    - Prefer `note` or custom helpers to convert `Maybe` → `Either SomeError`

4. **Validator & On-Chain Errors**
    - Use `traceError` with descriptive trace codes (see `docs/onchain-trace-codes.md`)
    - Ensure redeemer validation covers all pattern match branches
    - Guard against impossible states with `traceError` rather than silent pass-through

5. **API Layer Error Mapping**
    - Confirm `ServiceHandlers` catch `TxBuildingException` and return appropriate HTTP status + JSON body
    - Return structured error responses: `{ "error": "...", "code": "..." }`
    - Log errors with enough context for debugging without leaking sensitive data

## Add Error Handling Checklist

- [ ] Replaced all partial functions with total alternatives
- [ ] Defined new `TxBuildingException` constructors for each failure mode
- [ ] Mapped exceptions to HTTP status codes via `txBuildingExceptionToHttpStatus`
- [ ] Used `Either` / `ExceptT` for recoverable errors
- [ ] Added `traceError` with trace codes for on-chain validation failures
- [ ] API handlers return structured error JSON with correct status codes
- [ ] `cabal build all` succeeds
- [ ] `cabal test` passes
