# On-Chain Validation Audit

## Overview

Audit on-chain validators and minting policies in the Decentralized Belt System for correctness, completeness, and resistance to common Cardano smart contract vulnerabilities.

## Steps

1. **Redeemer Coverage**
    - Verify every redeemer constructor has an explicit validation branch — no wildcard `_` catch-alls
    - Ensure new redeemers are added to both the validator and the minting policy where applicable
    - Check that redeemer data is fully deconstructed and validated, not just pattern-matched

2. **Datum Integrity**
    - Confirm validators check datum content, not just datum hash presence
    - Verify CIP-68 metadata fields (name, description, image) are validated for format and length
    - Ensure datum transformations (e.g., `updateCIP68DatumMetadata`) preserve immutable fields
    - Check that continuing outputs carry the correct updated datum

3. **Value & Token Checks**
    - Minting policy enforces correct token name derivation (ID functions in `Protocol/Id.hs`)
    - Minting quantities are exactly 1 for NFTs, correct amounts for fungible tokens
    - Validators verify value is preserved or correctly split in continuing outputs
    - Fee tokens are minted/collected per `FeeConfig` rules

4. **Authorization & Access Control**
    - Admin-only operations require the admin signing key
    - Practitioner actions validate the correct wallet signature
    - Academy operations check academy ownership via reference tokens
    - Promotion/rank changes validate the proper authority chain

5. **Common Vulnerability Checks**
    - **Double satisfaction**: Each validator independently checks its own inputs/outputs
    - **Unbounded loops**: No recursive or list operations that could blow the execution budget
    - **Token name confusion**: Token names are derived deterministically, not user-supplied
    - **Missing deadline checks**: Validity intervals are enforced where time-sensitive
    - **Reference input manipulation**: Reference inputs are read-only and cannot be substituted

6. **Trace Codes & Debugging**
    - Every `traceError` uses a unique, documented code from `docs/onchain-trace-codes.md`
    - Error messages are descriptive enough to diagnose failures from transaction traces
    - Add `{-# INLINEABLE #-}` pragmas on all validator helper functions

7. **ExUnit Budget**
    - Run `scripts/test_exunits.sh` to verify execution units are within protocol limits
    - Profile validators that are close to budget limits
    - Optimize hot paths: reduce datum size, minimize list traversals, use `INLINEABLE`

## On-Chain Validation Audit Checklist

- [ ] All redeemer constructors have explicit validation branches
- [ ] Datum content validated (not just hash presence)
- [ ] Immutable fields preserved in datum updates
- [ ] Token names derived deterministically
- [ ] Minting quantities enforced
- [ ] Authorization checks for admin, practitioner, and academy operations
- [ ] No double-satisfaction vulnerabilities
- [ ] No unbounded loops or budget-blowing operations
- [ ] Trace codes unique and documented
- [ ] `{-# INLINEABLE #-}` on validator helpers
- [ ] `scripts/test_exunits.sh` passes within budget
