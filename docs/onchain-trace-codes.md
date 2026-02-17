# On-chain trace codes reference

Trace codes are **two-character** and **globally unique**. The first character identifies the module, the second is sequential within that module. When a transaction fails, the reported code identifies the exact failure point without needing the script hash.

## Module prefixes

| Prefix | Module |
|--------|--------|
| `A` | AchievementsValidator |
| `M` | MintingPolicy |
| `U` | Utils |
| `O` | OracleValidator |
| `R` | RanksValidator |
| `P` | ProfilesValidator |
| `T` | Protocol |
| `V` | MembershipsValidator |
| `L` | LinkedList |
| `B` | BJJ |
| `N` | OracleNFTPolicy |
| `K` | Protocol.Lookup |

## Global map (by code)

| Code | Script | Description |
|------|--------|-------------|
| M0 | MintingPolicy | Protocol is paused |
| M1 | MintingPolicy | Invalid purpose |
| M2 | MintingPolicy | Creation date must be before tx validity range |
| M3 | MintingPolicy | Must spend seed TxOutRef |
| M4 | MintingPolicy | Metadata fields validation failed |
| M5 | MintingPolicy | Practitioner mint/lock check failed |
| M6 | MintingPolicy | Organization mint/lock check failed |
| M7 | MintingPolicy | Profiles must have correct currency symbol |
| M8 | MintingPolicy | Must spend seed TxOutRef for uniqueness |
| M9 | MintingPolicy | Must spend master user NFT |
| Ma | MintingPolicy | Tx must mint JUST pending rank NFT |
| Mb | MintingPolicy | Must lock pending rank NFT at RV |
| Mc | MintingPolicy | Must pass promotion validation rules |
| Md | MintingPolicy | Membership history mint check failed |
| Me | MintingPolicy | Membership interval mint check failed |
| Mf | MintingPolicy | Must spend awarded-by user NFT |
| Mg | MintingPolicy | Profiles must have correct currency symbol (awardedTo/awardedBy) |
| Mh | MintingPolicy | Must spend seed for uniqueness (achievement) |
| Mj | MintingPolicy | Tx must mint JUST achievement NFT |
| Mk | MintingPolicy | Lock achievement at AchievementsValidator |
| A0 | AchievementsValidator | Cannot cleanup valid datum / no datum / invalid datum |
| A1 | AchievementsValidator | Accept achievement check failed (lock updated achievement; must spend practitioner user NFT) |
| A2 | AchievementsValidator | Invalid script info |
| U0 | Utils | Invalid context (script context parse failed) |
| U1 | Utils | Cannot find own input by TxOutRef |
| U2 | Utils | Cannot find state NFT at expected address |
| U3 | Utils | Invalid output: expected inline datum |
| O0 | OracleValidator | Invalid oracle datum |
| O1 | OracleValidator | Must be signed by admin |
| O2 | OracleValidator | Must return oracle UTxO |
| O3 | OracleValidator | Invalid script purpose |
| R0 | RanksValidator | Datum missing or invalid (cleanup / no datum / invalid) |
| R1 | RanksValidator | Invalid script info |
| R2 | RanksValidator | Must spend User NFT |
| R3 | RanksValidator | Lock profile at PV |
| R4 | RanksValidator | Lock rank at RV |
| P0 | ProfilesValidator | Datum missing or invalid (cleanup / no datum / invalid) |
| P1 | ProfilesValidator | Invalid purpose |
| P2 | ProfilesValidator | Own value has Ref NFT |
| P3 | ProfilesValidator | Must spend User NFT |
| P4 | ProfilesValidator | Image URI validation failed |
| P5 | ProfilesValidator | Lock updated profile |
| P6 | ProfilesValidator | Promotion ID CS check |
| P7 | ProfilesValidator | Already at or past this rank |
| P8 | ProfilesValidator | Date must be after current |
| P9 | ProfilesValidator | Own value has Ref NFT (accept) |
| Pa | ProfilesValidator | Lock updated profile (accept) |
| T0 | Protocol | Root node has no history |
| T1 | Protocol | Cannot insert: different orgs |
| T2 | Protocol | Cannot append: different orgs |
| T3 | Protocol | Cannot add interval: validation failed |
| T4 | Protocol | RETIRED (superseded by TB) — no longer used in code |
| TB | Protocol | Practitioner cannot extend end date (UpdateEndDate) |
| TC | Protocol | End date must be after start date (interval creation) |
| TD | Protocol | New end date must be within tx validity range, after lower bound and before upper bound (UpdateEndDate) |
| TE | Protocol | Practitioner can only update accepted intervals (UpdateEndDate) |
| T5 | Protocol | Cannot accept interval: already accepted |
| T6 | Protocol | Cannot accept a rank that is not pending |
| T7 | Protocol | OnchainProfile has no rank (promoteProfileDatum / promoteProfile) |
| T8 | Protocol | Cannot accept: not pending |
| T9 | Protocol | getCurrentRankId: OnchainProfile has no rank |
| TA | Protocol | Cannot accept achievement: already accepted |
| V0 | MembershipsValidator | Cannot cleanup UTxO with valid protocol datum |
| V1 | MembershipsValidator | Datum missing or invalid (no datum / invalid) |
| V2 | MembershipsValidator | Invalid redeemer for ListNodeDatum |
| V3 | MembershipsValidator | Invalid redeemer for IntervalDatum |
| V4 | MembershipsValidator | Invalid purpose |
| V5 | MembershipsValidator | Insert node handler failed |
| V6 | MembershipsValidator | Update node handler failed |
| V7 | MembershipsValidator | Accept interval handler failed |
| V8 | MembershipsValidator | UpdateEndDate handler failed (interval/history mismatch, wrong auth, or output check; time/accepted/extends enforced in Protocol TD/TE/TB) |
| L0 | LinkedList | Insert invariant violated |
| L1 | LinkedList | Append invariant violated |
| B0 | BJJ | Belt invariant (invalid belt / cannot succ red10 / cannot pred white) |
| N0 | OracleNFTPolicy | Must spend seed UTxO |
| N1 | OracleNFTPolicy | Invalid script purpose |
| K0 | Protocol.Lookup | Invalid datum type (expected ListNodeDatum or IntervalDatum) |
| K1 | Protocol.Lookup | Oracle read failed (decode / inline datum / not found) |
| K2 | Protocol.Lookup | Must pay required fee |
