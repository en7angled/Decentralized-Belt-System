# Explorer Read-Side Data Model

## Purpose

This document summarizes what data is currently available in this repository (on-chain + off-chain),
what can be derived from it for a read-side explorer, and what future fields should be reserved.

Scope: explorer views for practitioners, organizations, ranks (belts), promotions, achievements,
memberships, timeline/provenance, search/filter/sort, and verify views.

---

## 1. Datatype Inventory (source of truth by layer)

### On-chain protocol datums and params

- `Onchain.Protocol.Types`
  - `OnchainProfile` (`profileId`, `profileType`, `currentRank`, `protocolParams`)
  - `OnchainRank = Rank | Promotion`
  - `OnchainMembershipHistory`
  - `OnchainMembershipInterval`
  - `MembershipDatum = ListNodeDatum | IntervalDatum`
  - `OnchainAchievement`
  - `ProtocolParams`
  - `OracleParams` + `FeeConfig`

### Off-chain domain/read types

- `DomainTypes.Core.Types`
  - `Profile`
  - `Rank`
  - `Promotion`
  - `MembershipHistory`
  - `MembershipInterval`
  - `Achievement`

### Read API transfer types

- `DomainTypes.Transfer.Types`
  - `PractitionerProfileInformation`
  - `OrganizationProfileInformation`
  - `MembershipHistoryInformation`
  - `MembershipIntervalInformation`

### Projected DB entities (chainsync read model)

- `Storage`
  - `ProfileProjection`
  - `RankProjection`
  - `PromotionProjection`
  - `MembershipHistoryProjection`
  - `MembershipIntervalProjection`
  - `AchievementProjection`
  - `OnchainMatchEvent` (raw Kupo match envelope)

---

## 2. Core Concept: Profile

## 2.1 Explicit fields

- `profile_id` (AssetClass / reference NFT ID)
- `profile_type` (`Practitioner` or `Organization`)
- display metadata:
  - `name`
  - `description`
  - `image_uri`

## 2.2 Lifecycle and mutability

- Profiles are effectively permanent:
  - profile deletion is intentionally unsupported
  - profile state is updated in place
- Current implemented update action is image update (`UpdateProfileImage`).
  - Name/description updates are not currently implemented as a dedicated action.

## 2.3 Derived read-side views

- Practitioner view:
  - `current_rank`
  - `previous_ranks`
- Organization view:
  - profile metadata

## 2.4 Relationship edges to expose

- Profile -> ranks/promotions
- Profile -> achievements (awarded to / awarded by)
- Profile -> memberships (member side and org side)

---

## 3. Practitioner

## 3.1 Explicit/available

- Current rank + rank history already available in `PractitionerProfileInformation`.
- Achievements and memberships are queryable via dedicated endpoints filtered by profile IDs.

## 3.2 Derived fields for explorer

- `is_master_capable` (derived):
  - from current rank + protocol promotion rules.
  - BJJ logic currently requires promoter rank >= black-level thresholds, with additional rule:
    `Black1` cannot promote to `Black` or above.
- Promotion lineage trail:
  - from `rank_awarded_by_profile_id` and chronological rank chain.

---

## 4. Organization

## 4.1 Explicit/available

- Organization profile metadata is available.
- Membership histories/intervals can be queried by organization.
- Achievements can be queried by `awarded_by`.

## 4.2 Notes

- Achievement issuer/recipient are validated as protocol profile IDs (same currency symbol),
  not strictly type-locked to practitioner-only recipients in minting policy checks.
- Membership history naming uses "practitioner" in types, but protocol checks are ID-based;
  read-side should treat membership member as a profile ID.

---

## 5. Rank and Promotion

## 5.1 Rank fields to expose

- `rank_id`
- `belt` (full belt enum, including degree belts)
- `achieved_by_profile_id`
- `awarded_by_profile_id`
- `achievement_date`

## 5.2 Promotion fields to expose

- `promotion_id`
- `belt` (target)
- `achieved_by_profile_id` (target practitioner/member profile)
- `awarded_by_profile_id` (promoter)
- `achievement_date`

## 5.3 Lifecycle

- Promotion is pending state (`OnchainRank.Promotion` / `PromotionProjection`).
- Acceptance transitions to rank (`OnchainRank.Rank` / `RankProjection`).
- Projection ingestion deletes pending promotion projection when accepted rank appears.

## 5.4 Constraints context to expose in "verify"

- Promotion validity uses BJJ rule checks:
  - minimum time at current rank
  - authority rank constraints
  - monotonic promotion direction and date ordering

---

## 6. Achievement

## 6.1 Explicit fields

- `achievement_id`
- `awarded_to_profile_id`
- `awarded_by_profile_id`
- `achievement_date`
- `is_accepted`
- metadata:
  - `name`
  - `description`
  - `image_uri`

## 6.2 Lifecycle

- Awarded (pending acceptance): `is_accepted = false`
- Accepted: `is_accepted = true`

## 6.3 Metadata/evidence

- CIP-68 supports extra metadata map (`otherMetadata`) in addition to name/description/image.
- Current domain/query shape exposes core metadata fields, not arbitrary extra metadata keys.
- Explorer should reserve UI space for optional evidence links/tags/categories.

---

## 7. Membership (History vs Interval)

## 7.1 Membership interval (single record)

Available fields:

- `membership_interval_id`
- `start_date`
- `end_date` (nullable)
- `is_accepted`
- `practitioner_id` (member profile ID)
- `interval_number`
- `organization_id` (in `MembershipIntervalInformation`)

Derived status:

- `offered_pending_acceptance`: `is_accepted = false`
- `active`: `is_accepted = true` and (`end_date` is null or in future)
- `ended`: `end_date` is not null and end date reached/closed

## 7.2 Membership history (collection)

Available fields:

- `membership_history_id`
- `practitioner_id` (member profile ID)
- `organization_id`
- `intervals[]` (timeline entries)

## 7.3 Lifecycle

- Organization grants membership history + first interval
- Member accepts interval
- Later intervals may be appended
- End date can be updated (org or member rules)

## 7.4 Initiation variants

- Implemented: org grant + member acceptance
- Mentioned in product docs but not implemented in action model: member-initiated request flow

---

## 8. Protocol Parameters and Oracle

### Explicitly implemented dynamic params (`OracleParams`)

- `op_admin_pkh`
- `op_paused`
- `op_fee_config`:
  - `fc_fee_address`
  - `fc_profile_creation_fee`
  - `fc_promotion_fee`
  - `fc_membership_history_fee`
  - `fc_membership_interval_fee`
  - `fc_achievement_fee`
- `op_min_utxo_value`

### Explorer exposure recommendation

- Current effective values
- Change timeline/history of oracle updates
- Pause/maintenance status banner

### Not currently encoded as first-class on-chain params

- Genesis authority public key (as a dedicated phase/governance concept)
- Genesis phase end date
- protocol phase enum (Genesis/Legacy/DAO)

These should be treated as future/reserved explorer fields unless added to protocol state.

---

## 9. Provenance and Verify Views

## 9.1 Provenance fields to expose consistently

Recommended explorer envelope for any entity state/event:

- `tx_hash`
- `tx_output_index` (and input index when spent)
- `slot_no`
- `block_header_hash`
- `created/spent references`
- `validator_script_hash`
- `datum_hash` / datum type

## 9.2 What is available now in storage

- Projection tables contain:
  - `created_at_slot`
  - `created_at_hash`
  - `inserted_at` (DB ingestion timestamp)
- Raw `OnchainMatchEvent.kupoMatch` stores:
  - transaction id/index/output index
  - address/script hash
  - datum hash/type
  - created/spent points

## 9.3 Gap

- Query API currently returns domain objects without full provenance envelope.
- A dedicated "verify" response shape should include provenance by default.

---

## 10. Search / Filtering / Sorting (current Query API)

Implemented read routes support limit/offset/count and typed ordering.

- Profiles:
  - filter: `profile[]`, `profile_type`
  - order: id, name, description, type
  - extras: type frequency endpoint
- Promotions:
  - filter: `promotion[]`, `belt[]`, `achieved_by[]`, `awarded_by[]`
  - order: id, belt, achieved_by, awarded_by, date
  - extras: belt frequency endpoint
- Belts (accepted ranks):
  - filter: `rank[]`, `belt[]`, `achieved_by[]`, `awarded_by[]`, `from`, `to`
  - order: id, belt, achieved_by, awarded_by, date
  - extras: belt frequency endpoint
- Membership histories:
  - filter: `organization[]`, `practitioner[]`
  - order: id, created_at, practitioner, organization
- Membership intervals:
  - filter: `practitioner[]`
  - order: id, start_date, interval_number, practitioner
- Achievements:
  - filter: `achievement[]`, `awarded_to[]`, `awarded_by[]`, `is_accepted`, `from`, `to`
  - order: id, date, awarded_to, awarded_by, name

---

## 11. Available vs Derivable vs Future (explorer checklist)

### Available now

- Profile metadata and type
- Practitioner current+previous ranks
- Pending promotions
- Accepted ranks
- Achievements with acceptance status
- Membership histories and intervals
- Counts/frequencies/filtering/sorting listed above
- Oracle params queryable in off-chain/admin path

### Derivable now (should be exposed)

- Membership lifecycle label (offered/active/ended)
- Promotion/rank lifecycle label (pending/accepted)
- Master capability flag
- Awarding lineage chain
- Timeline events by ordering and joining projections/raw match events

### Future/reserved

- Governance phase (Genesis/Legacy/DAO)
- Proposals and vote records
- Rank-weighted governance power snapshots
- Treasury spending/governance traces

---

## 12. Practical Read-Side Output Shapes to prioritize

1. `EntityDetail` views (profile/rank/promotion/achievement/membership) with provenance envelope.
2. `EntityTimeline` views per profile and per organization.
3. `LineageView` for practitioner rank chain and awarding authority chain.
4. `VerificationView` with deterministic ID derivation checks (profile/rank/membership/achievement).
5. `ProtocolStatusView` for oracle params + fee schedule + pause flag + historical changes.

