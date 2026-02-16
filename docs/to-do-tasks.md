# To-Do Tasks


## Security Audit Remaining Items (LOW)

From `docs/OnchainSecurityAudit.md`:

- [x] **Permissionless Cleanup redeemer (dust/griefing mitigation)**
  - Added `Cleanup` redeemer to ProfilesValidator (index 2), RanksValidator (index 1), and MembershipsValidator (index 3)
  - Datum-validity guard: allows spending only when datum is absent or unparseable as the expected protocol type
  - Off-chain: `cleanupDustTX` operation, `ProtocolAction CleanupDustAction` in interaction API, `cleanup-dust` admin CLI command
  - Tests: dust at PV only, dust at RV only, dust at both validators, cleanup safety with legitimate protocol state
  - See `docs/OnchainSecurityAudit.md` "LOW (Resolved): Dust/Griefing" for full design rationale
- [x] **`UpdateEndDate` time validation** — implemented: `UpdateEndDate` redeemer (MV index 4) with dual-authorization (org/practitioner), `newEndDate` validated against `txInfoValidRange` (must be in future); Protocol `updateMembershipIntervalEndDate` is role-based (TB for practitioner extend)
- [x] **`endDate` validation on creation** — implemented: `initMembershipHistory` and `addMembershipIntervalToHistory` require `endDate > startDate` when provided (trace TC)
- [x] **MV redeemer data integrity** — documented as safe-by-design in `OnchainSecurityAudit.md` (exact mint, MP time validation, validLastInterval anchor)

## Memberships Integration

### Membership implementation status

Membership (histories and intervals per organization) against the Developer Guide checklist:

**Done:** On-chain types and IDs (`Protocol/Types.hs`, `Protocol/Id.hs`), `MembershipsValidator.hs`, minting policy redeemers `NewMembershipHistory` / `NewMembershipInterval`, `fcMembershipFee`, oracle integration. Off-chain: domain types `MembershipHistory` / `MembershipInterval`, actions `CreateMembershipHistoryAction` / `AddMembershipIntervalAction` / `AcceptMembershipIntervalAction`, operations (`createMembershipHistoryTX`, `addMembershipIntervalTX`, `acceptMembershipIntervalTX`), lookups, interactions, context (`membershipsValidatorHashAndRef`), validators (`membershipsValidatorGY`), `deployReferenceScripts`, exceptions, functors, datum parser. Storage: `MembershipHistoryProjection` / `MembershipIntervalProjection`, put functions, `putMatchAndProjections`, `rollbackTo`. Ingestion: `MembershipHistoryEvent` / `MembershipIntervalEvent`, `projectChainEvent` for memberships validator. Admin CLI: `CreateMembershipHistory`, `AddMembershipInterval`, `AcceptMembershipInterval`. `verifyDeployedScriptsAreReady` includes the memberships validator. Unit tests cover create history, accept interval, and "3 practitioners, one updates interval".

**Gaps addressed:**

- **Query API:** REST endpoints for membership histories and intervals (list and count, with filters and ordering) in `query-api/RestAPI.hs` and `Query/Projected.hs`.
- **exportValidators:** `defaultMembershipsValidatorFile` and `exportMembershipsValidator` in `Constants.hs` and `TxBuilding/Validators.hs`; `exportValidators` writes all four validators (profiles, ranks, memberships, oracle).
- **config_bjj_validators.json:** Sample config includes all six fields (five script refs plus `oracleNFTAssetClass`); placeholders for memberships and oracle can be replaced after deployment.

### Remaining / follow-up

- [ ] **Deploy MembershipsValidator** (if not yet in target env) — ensure `DeployedScriptsContext`, `deployBJJValidators`, admin deploy flow include memberships (`Context.hs`, `TestRuns.hs`, `Transactions.hs`, `Main.hs`)
- [ ] **Membership test runs** — add test runs for InsertNode, UpdateNode, AcceptInterval interactions (beyond existing unit tests)
- [ ] **Optional: live projection for membership** — add `Query.Live` and `?liveprojection` support for membership endpoints



## New Features

- [ ] **Full onchain + offchain for Achievements** — design and implement on-chain validator(s), data types, minting logic, off-chain transaction building, API endpoints, and chain-sync support for the achievements system
