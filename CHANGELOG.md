# Revision history for Decentralized-Belt-System

Major changes only, latest first.

---

## 0.3.1.8 -- 2026-02-21

- Unit tests split into modules (Achievement, Cleanup, Membership, Oracle, Promotion); Test.Fixtures and Test.Helpers added.
- Off-chain membership interval validation: structured exceptions (`AddMembershipIntervalReason`, `CannotAddMembershipInterval`); pre-validation in `addMembershipIntervalTX`.
- `protocolMinLovelace` increased to 10_000_000.

## 0.3.1.7 -- 2026-02-20

- `GET /profiles/count` supports same filters as `GET /profiles`; RestAPI refactor (Common.hs, shared filter helpers).
- `TxBuilding.Functors` → `TxBuilding.Conversions`; `getUtxoWithTokenAtAddresses` → `getUTxOWithTokenAtAddresses`.

## 0.3.1.6 -- 2026-02-17

- **Achievements**: AchievementsValidator (award/accept/cleanup); MintingPolicy `NewAchievement`; full off-chain ops, storage, ingestion; REST `GET /achievements` and `GET /achievements/count` with filters and liveprojection.
- Validators moved under `Onchain/Validators/`; protocol logic extracted to `Onchain/Protocol/Core.hs`. Membership endpoints support liveprojection.

## 0.3.1.5 -- 2026-02-16

- **UpdateEndDate security**: TD/TE/TB enforced on-chain (no longer dead code); off-chain uses `updateEndDateWithoutValidations` so invalid attempts fail on-chain.
- Trace codes made globally unique and consolidated; single `onchain-trace-codes.md` (61 codes).

## 0.3.1.4 -- 2026-02-15

- Scripts: `populate_testnet.sh` and `test_black_promotes_white_to_blue.sh` extended for memberships; `test_exunits.sh` parses membership actions.

## 0.3.1.3 -- 2026-02-15

- **Memberships (off-chain)**: Full flow for histories and intervals (create, add interval, accept); storage, ingestion, REST API, admin CLI. Permissionless `Cleanup` redeemer for dust on Profiles/Ranks/Memberships validators.

## 0.3.1.2 -- 2026-02-15

- **TxBuilding exceptions**: `ProfileException` → `TxBuildingException` (RankListEmpty, PromotionNotFound, OracleDatumInvalid, etc.); HTTP mapping 404/503/400. Added `docs/DeveloperGuide.md`.

## 0.3.1.1 -- 2026-02-15

- Consistency: shared lookups (`getAllParsedDatumsAtValidator`, `profileDatumToProfileData`), `upsertByUnique` in storage; typo fix `getPractitionerInformation`.

## 0.3.1.0 -- 2026-02-15

- Codebase consistency: config via env vars, shared helpers (`txOutRefToV3Plutus`, `pkhFromExtendedSkey`), `runWithTxErrorHandling`, `appVersion`; dead code removed.

## 0.3.0.0 -- 2026-02-15

- **Oracle Hub**: Dynamic protocol parameters (pause, fees, min lovelace, admin) via on-chain oracle; OracleValidator, OracleNFTPolicy, OracleParams/FeeConfig. MintingPolicy reads oracle from reference input. Admin actions (pause/unpause, set-fees, query-oracle) via shared Interaction pipeline.

## 0.2.9.0 -- 2026-02-15

- Protocol split into `Protocol/Types.hs`, `Protocol/Lookup.hs`, `Protocol/Id.hs`. Haskell style: Haddock, export lists, INLINEABLE; `BeltSnapshot` refactor in BJJ.

## 0.2.8.2 -- 2026-02-10

- Script size reporting in test output (bytes and % of 16 KB limit).

## 0.2.8.1 -- 2026-02-10

- Admin CLI `write-blueprint`; Blueprint includes MembershipsValidator and fixes. Bug fix: `unsafeGetProfileDatumAndValue` type (ProfileId not RankId); OnchainSecurityAudit R5 corrected.

## 0.2.8.0 -- 2026-02-09

- **Memberships (on-chain)**: LinkedList, MembershipsValidator; membership history/interval datums and redeemers; init history, add interval, accept flows.
- **Security audit**: Cross-org membership fix (MV checks org User NFT); CurrencySymbol and startDate validation. Redeemer **breaking**: AcceptPromotion no longer has `rankOutputIdx`; new mint redeemers for membership. `promoteProfileDatum` split for PV cost reduction.

## 0.2.7.0 -- 2026-02-01

- **Output index optimization**: O(n) → O(1) output validation via `checkTxOutAtIndex`. **Breaking**: Redeemers now include output indices (CreateProfile, Promote, UpdateProfileImage, AcceptPromotion).

## 0.2.6.0 -- 2026-02-01

- **PlutusV3**: Token ID generation via `blake2b_224` + `integerToByteString`. **Breaking** for new token names. Added `populate_testnet.sh`; script improvements and docs.

## 0.2.5.0 -- 2026-02-01

- **Profile deletion removed** (**breaking**): DeleteProfile, BurnProfileId, deleteProfileTX removed; profiles permanent by design.
- Metadata size limits (name 128, description 1024, imageURI 256 bytes) to prevent DoS. AcceptPromotion redundant check removed.

## 0.2.4.0 -- 2025-02-01

- **Promotion security**: Full BJJ validation at mint; seed TxOutRef for unique promotion IDs; acceptance-time checks (nextBelt > currentBelt, date ordering). RanksValidator simplified to consent-only.

## 0.2.3.0 -- 2025-01-06

- Query API: `GET /profiles/count`, `/profiles/frequency`, `GET /promotions/count`, `/promotions/frequency`; all support liveprojection.

## 0.2.2.0 -- 2024-12-21

- **API split**: Interaction API (build/submit tx) and Query API (profiles, promotions, belts) as separate services; separate Dockerfiles and deployment.

## 0.2.1.0 -- 2024-12-20

- Testnet evidence, automated test script, admin CLI deployment commands; README enhancements.

## 0.2.0.0 -- 2024-12-19

- **Milestone 2**: Smart contract architecture — MintingPolicy, ProfilesValidator, RanksValidator, BJJ.hs; TxBuilding (Operations, Interactions, Lookups, Skeletons); unit and property tests; blueprint and documentation.

## 0.1.0.0 -- YYYY-mm-dd

- First version. Released on an unsuspecting world.
