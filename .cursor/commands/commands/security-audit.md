# Security Audit

## Overview

Comprehensive security review of the Decentralized Belt System, covering Cardano on-chain validator security, off-chain transaction building, API security, and key management.

## Steps

1. **On-Chain Validator Security**
    - Review all validators for double-satisfaction vulnerabilities (ensure each validator checks its own inputs/outputs independently)
    - Check for datum hijacking — verify datum hashes are validated, not just assumed
    - Ensure minting policy enforces correct token names and quantities
    - Verify redeemer validation covers all constructors — no wildcard `_` catch-alls that silently pass
    - Check that `traceError` codes are unique and documented in `docs/onchain-trace-codes.md`
    - Review CIP-68 metadata handling for injection or overflow risks
    - Confirm reference script UTxOs cannot be spent or manipulated

2. **Transaction Building Security**
    - Verify `interactionToTxSkeleton` enforces all business rules before building
    - Check that fee collection cannot be bypassed or manipulated
    - Ensure UTxO lookups validate datum types before use
    - Review for time-based attacks (slot ranges, validity intervals)
    - Confirm admin-only operations check the admin signing key

3. **API & Infrastructure Security**
    - Review Servant endpoints for input validation (request body size, field constraints)
    - Check CORS configuration in `webapi-lib`
    - Ensure `.env`, `operation.prv`, and config files with secrets are in `.gitignore`
    - Verify no secrets are logged or returned in API error responses
    - Review authentication middleware (wallet signature verification)

4. **Key & Credential Management**
    - Confirm `operation.prv` (admin key) is never committed to git
    - Check that wallet private keys are never handled server-side
    - Review config loading (`decodeConfigEnvOrFile`) for path traversal risks
    - Ensure environment variable fallbacks don't leak sensitive defaults

5. **Dependency Audit**
    - Review Nix flake inputs for pinned versions
    - Check Haskell dependencies in `.cabal` for known vulnerabilities
    - Audit Atlas/GeniusYield SDK usage for security advisories

## Security Audit Checklist

- [ ] Validators reviewed for double-satisfaction and datum hijacking
- [ ] Minting policy enforces correct token names/quantities
- [ ] No wildcard catch-alls in redeemer validation
- [ ] Transaction building enforces all business rules
- [ ] Fee collection cannot be bypassed
- [ ] API inputs validated and CORS configured
- [ ] No secrets in git, logs, or error responses
- [ ] Admin key (`operation.prv`) protected
- [ ] Nix and Haskell dependencies reviewed
- [ ] Trace codes documented in `docs/onchain-trace-codes.md`
