# Debug Issue

## Overview

Systematically debug issues in the Decentralized Belt System using GHC tooling, Cardano-specific debugging techniques, and the project's error infrastructure.

## Steps

1. **Reproduce & Identify**
    - Reproduce the issue: compile error, test failure, transaction rejection, or runtime exception
    - For compile errors: run `cabal build all` and read the full GHC error (file, line, expected vs actual type)
    - For test failures: run `cabal test` and read the Tasty output (test name, assertion, counterexample)
    - For transaction failures: check the API response for `TxBuildingException` details and trace codes

2. **Trace Through the Layers**
    - Identify which layer the error originates from:
      - **On-chain**: trace codes from `docs/onchain-trace-codes.md`, script evaluation failures
      - **Tx building**: `TxBuildingException` from `TxBuilding/Exceptions.hs`
      - **API**: HTTP status code + error body from `ServiceHandlers`
      - **Chain-sync**: projection errors in `Ingestion.hs` or `Storage.hs`
    - Follow the type/data flow: Transfer → Domain → Onchain (or reverse for queries)

3. **GHC & Cabal Debugging**
    - Use `cabal repl` to interactively test functions and inspect types
    - Add temporary `Debug.Trace.trace` calls to inspect values at runtime (remove before committing)
    - Use typed holes (`_`) to let GHC tell you what type is expected
    - Check for type ambiguity — add explicit type annotations or `TypeApplications`
    - For performance issues: build with `-prof` and run with `+RTS -p` for profiling

4. **Cardano-Specific Debugging**
    - For script failures: check `traceError` codes against `docs/onchain-trace-codes.md`
    - Use `admin` CLI to inspect current on-chain state (UTxOs, datums, reference scripts)
    - Check if the issue is a stale UTxO (someone else consumed it between query and submit)
    - Verify datum format matches what the validator expects (check `Protocol/Types.hs`)
    - Check Cardano node sync status — stale queries can cause false failures

5. **Fix & Prevent**
    - Apply the fix in the correct layer (don't patch symptoms in the API if the bug is on-chain)
    - Add a test case that reproduces the bug before fixing (regression test)
    - If the error was confusing, improve the `TxBuildingException` constructor or trace code
    - Run `cabal build all && cabal test` to confirm the fix

## Debug Issue Checklist

- [ ] Issue reproduced reliably
- [ ] Error layer identified (on-chain, tx-building, API, chain-sync)
- [ ] Root cause traced through the type/data flow
- [ ] Fix applied in the correct layer
- [ ] Regression test added
- [ ] Error messages improved if they were unclear
- [ ] `cabal build all` succeeds
- [ ] `cabal test` passes
