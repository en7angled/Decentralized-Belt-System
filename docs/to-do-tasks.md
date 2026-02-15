# To-Do Tasks


## Security Audit Remaining Items (LOW)

From `docs/OnchainSecurityAudit.md`:

- [ ] **Admin Cleanup redeemer (dust/griefing mitigation)**
  - The `adminPubKeyHash` is now available via `OracleParams.opAdminPkh` (read from the oracle datum as a reference input)
  - Add `AdminCleanup` redeemer to ProfilesValidator, RanksValidator, and MembershipsValidator
  - Validation: admin signs tx + spent UTxO must NOT contain protocol CurrencySymbol tokens
  - Update deployment order and documentation
- [ ] **`UpdateEndDate` time validation** — not yet implemented
- [ ] **`endDate` validation on creation** — not yet implemented
- [ ] **MV redeemer data integrity** — remaining LOW item from audit

## Memberships Integration

- [ ] **Deploy MembershipsValidator** — add to `DeployedScriptsContext`, `deployBJJValidators`, admin deploy flow (`Context.hs`, `TestRuns.hs`, `Transactions.hs`, `Main.hs`)
- [ ] **Membership test runs** — add test runs for InsertNode, UpdateNode, AcceptInterval interactions
- [ ] **Operations + API + ChainSync for Memberships** — extend offchain operations, REST API endpoints, and chain-sync indexer to support membership interactions (create history, add interval, accept interval, query membership data)

## Code Quality

- [ ] **Haskell style cleanup** — consistent formatting, import ordering, naming conventions, pragma usage across the codebase
- [ ] **`validLastInterval` TODO comment cleanup** — replace `-- TODO: tbc if this check is required` in `Protocol.hs` L357 with a definitive comment (audit confirmed: YES, required — prevents head-bypass attacks)

## New Features

- [ ] **Full onchain + offchain for Achievements** — design and implement on-chain validator(s), data types, minting logic, off-chain transaction building, API endpoints, and chain-sync support for the achievements system
- [x] **Oracle Hub for Parameters** — implemented oracle-based parameter hub with `OracleValidator`, `OracleNFTPolicy`, `OracleParams`/`FeeConfig` types, pause gate, fee enforcement, and dynamic `minOutputLovelace`. Admin CLI commands: `pause-protocol`, `unpause-protocol`, `set-fees`, `query-oracle`. The `adminPkh` is now sourced from the oracle datum instead of `ProtocolParams`.
