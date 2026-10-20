# F1 — Public Query API contract (Track 4 design note)

Target: the Cardano governance reputation & track-record registry (working name *Palmares*), phase F1 on
**preprod**. Scope of this note: the public read API and the UI's data needs — endpoint list, response DTOs,
page composites, pagination/sorting hardening, OpenAPI, freshness.

Template read before designing (house patterns are followed, not reinvented):
`src/exe/query-api/RestAPI.hs`, `Query/Common.hs`, `Query/Pages.hs`, `Query/Projected.hs`, `Query/ServiceStatus.hs`,
`src/lib/offchain-lib/DomainTypes/Transfer/{QueryResponses,Filters,OrderBy}.hs`, `src/lib/webapi-lib/WebAPI/{ServiceProbe,Errors,Utils,CORS}.hs`.

Every external fact below carries its source URL inline. **(decision)** marks a choice I made; **(BLOCKER)**
marks something that must be answered before the task can be implemented; **(cut)** marks something in the
spec that I am recommending we do *not* build.

---

## 0. Posture: what we are NOT building (read this before section 1)

The laziness rule applied to the spec. Each line saves real implementation work.

| # | Spec item | Verdict | One-line reason |
|---|---|---|---|
| 0.1 | Our own metadata fetcher for governance anchors (§4.1 "Metadata fetcher") | **(cut)** | db-sync already fetches, hash-checks, parses and retries CIP-100/108/119 anchors into `off_chain_vote_data` / `off_chain_vote_drep_data` / `off_chain_vote_gov_action_data` / `off_chain_vote_fetch_error` — we only *classify* its rows. See 0.6 for the config change this needs and 7.3 for the verdict mapping. |
| 0.2 | Dual live/projected backend (`QueryFlag "liveprojection"`, `withBackend`, `Query.Live`) | **(cut)** | For governance there is no "live" alternative — db-sync *is* the source. Deleting the second backend deletes the F-17 bug class structurally (see §4.1) and halves the query modules. Attestation reads also go projected-only; `/ready` gating covers staleness. |
| 0.3 | Separate `/export/*` endpoints (§4.2 "Export") | **(cut)** | Content negotiation on the existing list endpoints: `Get '[JSON, CSV]`. Zero new routes, zero new handlers. See §1.9. |
| 0.4 | `GET /protocol-status` as its own endpoint (Belt System has one) | **(cut)** | Folded into `GET /status` together with the two sync positions and the epoch thresholds. One endpoint the status chip and the oracle panel both read. |
| 0.5 | Per-actor vote endpoints (`/drep/{id}/votes`, `/gov-action/{id}/votes`, `/committee/{id}/votes`, `/spo/{id}/votes`) | **(cut)** | One `GET /votes` with `voter_role` / `voter_id` / `gov_action_id` filters is all four. The DRep page and gov-action page get their votes from their page composite anyway. |
| 0.6 | `preset: only_governance` verbatim | **refined, not replaced** | The preset is an aggregate that "overrides all other properties" ([configuration.md](https://github.com/IntersectMBO/cardano-db-sync/blob/master/doc/configuration.md)) and it leaves `offchain_vote_data` **disabled** and `shelley` **disabled**. We need both on. See §7.1 — this is a config refinement in service of 0.1 and of correct delegator counts, not a change of substrate. |
| 0.7 | A ratification predictor / "will it pass" tally | **(cut)** | The SPO default-vote rule (reward-account delegated to always-abstain / always-no-confidence, bootstrap-phase exceptions, HFI exception) lives in `cardano-ledger` `Conway/Rules/Ratify.hs` and has already produced two bugs in yaci-store ([#1017](https://github.com/bloxbean/yaci-store/issues/1017), [#1148](https://github.com/bloxbean/yaci-store/issues/1148)). v1 publishes **cast-votes-only** tallies with an explicit `basis` field, and reads the authoritative outcome from db-sync's `ratified_epoch`/`enacted_epoch`/`expired_epoch`/`dropped_epoch`. See §2.6. |
| 0.8 | "Live" voting power | **(cut)** | db-sync tracks **active** voting power only (`drep_distr`, per epoch); a maintainer states live power is not tracked ([db-sync #1951](https://github.com/IntersectMBO/cardano-db-sync/issues/1951)). Every power number in the API is labelled with the epoch it belongs to (`voting_power_epoch`). Never ship an unlabelled "voting power". |
| 0.9 | Composite reputation score | **(cut)** — already a spec non-goal (S5, §10) | Enforced in the contract: there is no `score` field anywhere in §2. |
| 0.10 | Migration to `openapi3` / `servant-openapi3` | **(cut)** | Would force re-deriving every `ToSchema` instance. We keep `servant-swagger` + `swagger2` and let the TS side read Swagger 2.0 directly. See §5. |

---

## 1. The complete endpoint list for F1

Conventions, all inherited from the Belt System query-api:

- Base port **8083** via `getPortFromEnvOrDefault 8083`; mount paths unchanged from the house layout.
- **Public — no `BasicAuth`.** `type FullAPI = SwaggerSchemaUI "swagger-ui" "swagger-api.json" :<|> ServiceProbeAPI :<|> CoreFunctionAPI`.
  The `PrivateRestAPI`/`basicAuthServerContext` layer in `RestAPI.hs` is dropped; `WebAPI.Auth` is not linked into this exe.
  Rate limiting is the BFF's job (not in Haskell).
- Singular path = one entity (`/drep/{drep-id}`), plural = list (`/dreps`) — matches `/practitioner/{id}` vs `/profiles`.
- Every list endpoint: `limit`, `offset`, `order_by`, `sort_order`, plus its own filters; returns `Page<T>` (§2.2).
- Every list endpoint also serves `text/csv` (§1.9).
- **Source** column: `db-sync` = served from db-sync-backed projections · `attest` = from the Kupo attestation
  pipeline · `both` = joins the two.

### 1.1 Probes and status

| Method | Path | Params | Response | Source |
|---|---|---|---|---|
| GET | `/health` | — | `ServiceProbeStatus Text` | — |
| GET | `/ready` | — | `ServiceProbeStatus Text` | both |
| GET | `/status` | — | `StatusResponse` | both |
| GET | `/metrics/formulas` | — | `[MetricFormula]` | static |

`/health` = `alwaysHealthy (pack appVersion) "query-api"`, never touches state (house rule).

`/ready` **(decision)** returns 503 `{"code":"indexer_behind"}` when **any** of:
`governance.lag_seconds > READY_MAX_LAG_SECONDS` (default **180**; preprod db-sync at tip must hold < 120 s per
spec check A.1, so 180 gives 60 s of headroom) · `attestations.lag_seconds > READY_MAX_LAG_SECONDS` ·
either Postgres connection check fails. Reuses `verifyProjectionDbConnection`.

`/metrics/formulas` **(decision)** serves one `file-embed`ed JSON (`resources/metric-formulas.json`) so S2
("every displayed metric is reproducible, formula published") costs one file, and F3 reuses the same bytes as an
MCP resource (the `bjj://rules/*` pattern). Fields: `id`, `name`, `expression`, `inputs` (list of db-sync tables/columns),
`caveats`.

### 1.2 DRep — delegation discovery and profile

| Method | Path | Response | Source |
|---|---|---|---|
| GET | `/dreps` | `Page DRepSummary` | db-sync |
| GET | `/drep/{drep-id}` | `DRepDetailResponse` | both |
| GET | `/drep/{drep-id}/power` | `Page EpochPowerPoint` | db-sync |
| GET | `/drep/{drep-id}/delegators` | `Page DelegatorRow` | db-sync |
| GET | `/drep/{drep-id}/updates` | `Page DRepUpdateRow` | db-sync |

`{drep-id}` capture **(decision, corrected by fact-check)**: accepts **CIP-105 bech32 (`drep1…`, 28-byte payload —
this is exactly what db-sync stores in `drep_hash.view`)**, **CIP-129 bech32 (`drep1…`, 29-byte payload = 1 header
byte `0x22` key / `0x23` script + the hash)**, raw 28-byte hex, or the two
reserved views `drep_always_abstain` / `drep_always_no_confidence`. There is **no `drep_script1…` form in db-sync**:
`serialiseDrepToBech32 = serialiseToBech32 "drep"` over the raw credential hash, HRP `drep` for key *and* script
DReps ([Util/Bech32.hs @13.7.2.1](https://github.com/IntersectMBO/cardano-db-sync/blob/13.7.2.1/cardano-db-sync/src/Cardano/DbSync/Util/Bech32.hs),
used at `GovAction.hs:373`). **Canonical output `id` is the CIP-129 form, which we must compute ourselves** (db-sync
has no CIP-129 column); `drep_hash.view` is emitted alongside as `id_cip105` so the Koios/db-sync cross-check in
§7.5 can join on it. These two literal strings are db-sync's own
(`cardano-db/src/Cardano/Db/Types.hs`: `hardcodedAlwaysAbstain = "drep_always_abstain"`,
`hardcodedAlwaysNoConfidence = "drep_always_no_confidence"` —
[source](https://github.com/IntersectMBO/cardano-db-sync/blob/master/cardano-db/src/Cardano/Db/Types.hs)).
Canonical output is always the bech32 form; reserved DReps output the literal view. `FromHttpApiData DRepId`
rejects anything else with 400 `invalid_drep_id`.

`GET /dreps` query parameters — this is the delegation-discovery surface, so it carries every filter §4.2 names:

| Param | Type | Default | Bounds / values | Notes |
|---|---|---|---|---|
| `limit` | Int | 50 | clamped to [1, 200]; CSV [1, 10000] | §4.2 |
| `offset` | Int | 0 | [0, 10000]; above → 400 `offset_too_large` | §4.2 |
| `status` | `[DRepStatus]` (repeatable) | all | `active` `inactive` `retired` `reserved` | repeatable = OR, `QueryParams "status"` |
| `has_script` | Bool | — | | key-DRep vs script-DRep |
| `min_voting_power` | Integer | — | ≥ 0, lovelace | on latest `drep_distr.amount` |
| `max_voting_power` | Integer | — | ≥ 0, lovelace | |
| `min_delegators` | Int | — | ≥ 0 | |
| `min_participation_rate` | Double | — | [0, 1] | `participation_rate_v1` |
| `min_rationale_rate` | Double | — | [0, 1] | `rationale_rate_v1` |
| `registered_before` | UTCTime | — | ISO 8601 | chain time |
| `registered_after` | UTCTime | — | ISO 8601 | chain time |
| `min_age_days` | Int | — | ≥ 0 | the "seniority" filter |
| `voted_on` | `GovActionId` | — | | "show me who voted on this" |
| `not_voted_on` | `GovActionId` | — | | triage: who still owes a vote |
| `anchor_status` | `[AnchorStatus]` | all | §2.4 | surfaces `hash_mismatch` DReps (F1 check 7) |
| `language` | Text | — | BCP-47, ≤ 16 chars | `off_chain_vote_data.language` |
| `active_in_epoch` | Int | — | ≥ 0 | had `drep_distr` row for that epoch |
| `q` | Text | — | trimmed length [2, 100] | §4.3 |
| `order_by` | `DRepsOrderBy` | `voting_power` | see below | |
| `sort_order` | `SortOrder` | `desc` | `asc` `desc` | **(decision)** default `desc`, not the Belt System's `asc` — every DRep list is a leaderboard |

```haskell
data DRepsOrderBy
  = DRepsOrderById
  | DRepsOrderByVotingPower
  | DRepsOrderByDelegatorCount
  | DRepsOrderByParticipationRate
  | DRepsOrderByRationaleRate
  | DRepsOrderByRegisteredAt
  | DRepsOrderByVotesCast
  | DRepsOrderByName
  deriving (Show, Generic, Eq)
  deriving (FromJSON, ToJSON)
    via CustomJSON '[ConstructorTagModifier '[StripPrefix "DRepsOrderBy", CamelToSnake]] DRepsOrderBy
-- wire: id | voting_power | delegator_count | participation_rate | rationale_rate
--     | registered_at | votes_cast | name
```
`FromHttpApiData` + `ToParamSchema` + `ToSchema` instances exactly in the shape of `ProfilesOrderBy` in
`DomainTypes/Transfer/OrderBy.hs` (case-insensitive `T.toLower` match, `Left` message listing the valid values).

`/drep/{drep-id}/power`: `limit` default 200 / max 200 (preprod has ~100 Conway epochs, so one page covers the whole
history), `from_epoch`, `to_epoch`, `order_by` fixed to `epoch`, `sort_order` default `asc`.

`/drep/{drep-id}/delegators`: `limit` 50/200, `offset`, `is_current` (Bool, default `true`),
`min_lovelace`, `order_by` ∈ `lovelace | delegated_at | stake_address` (default `lovelace`), `sort_order` default `desc`.

`/drep/{drep-id}/updates`: `limit` 50/200, `offset`, `kind` ∈ `registered | updated | retired` (repeatable),
`order_by` fixed `at`, `sort_order` default `desc`. Kind derivation **(decision)**, from db-sync's own encoding
(`cardano-db-sync/src/Cardano/DbSync/Era/Universal/Insert/Certificate.hs`:
`ConwayRegDRep → Just coin`, `ConwayUpdateDRep → Nothing`, `ConwayUnRegDRep → Just (-coin)` —
[source](https://github.com/IntersectMBO/cardano-db-sync/blob/master/cardano-db-sync/src/Cardano/DbSync/Era/Universal/Insert/Certificate.hs)):
`drep_registration.deposit > 0` → `registered`; `IS NULL` → `updated`; `< 0` → `retired`.

### 1.3 Votes (one endpoint, four screens)

| Method | Path | Response | Source |
|---|---|---|---|
| GET | `/votes` | `Page VoteRecord` | db-sync |

| Param | Type | Default | Bounds / values |
|---|---|---|---|
| `limit` | Int | 50 | [1, 200] / CSV [1, 10000] |
| `offset` | Int | 0 | [0, 10000] |
| `voter_role` | `[VoterRole]` | all | `drep` `spo` `constitutional_committee` |
| `voter_id` | `[Text]` | — | repeatable; DRep CIP-129/CIP-105 bech32 or hex, pool bech32 `pool1…`, CC **cold** hash (resolved to hot keys — see below) |
| `gov_action_id` | `[GovActionId]` | — | repeatable |
| `verdict` | `[Vote]` | all | `yes` `no` `abstain` |
| `gov_action_type` | `[GovActionType]` | all | §2.6 |
| `anchor_status` | `[AnchorStatus]` | all | `has_rationale` is `anchor_status=valid` |
| `from` | UTCTime | — | chain time, inclusive |
| `to` | UTCTime | — | chain time, inclusive |
| `from_epoch` / `to_epoch` | Int | — | ≥ 0 |
| `q` | Text | — | [2, 100] — matches rationale title/abstract and voter name |
| `order_by` | `VotesOrderBy` | `cast_at` | `cast_at` `voting_power` `latency` `voter_id` `gov_action_id` |
| `sort_order` | `SortOrder` | `desc` | |

**Committee votes are cast by HOT keys — mandatory cold↔hot resolution (fact-check).**
`voting_procedure.committee_voter` is "a reference to the **hot key** committee hash entry that voted"
(schema.md @13.7.2.1), while `committee_member.committee_hash_id` is the member's **cold** credential
(`insertNewMember` inserts the ledger committee member credential, `GovAction.hs:170-179`). A member may hold
several hot keys over time. Therefore, and this is projection work, not a join a handler can improvise:

- `vote_projection` stores, for every CC vote, **both** `voter_hot_hash` and the resolved `voter_cold_hash`,
  resolved through `committee_registration (cold_key_id, hot_key_id)` as of the vote's tx.
- `VoteRecord.voter_id` for `voter_role = constitutional_committee` is the **cold** CIP-129 `cc_cold1…` form —
  the stable identity a user can follow — with the hot hash exposed as `voter_hot_hash`.
- `CommitteeMemberSummary.votes_cast` counts via the same resolved column.

Doing this by hot key only (as §7.2 originally mapped it) would split one member's history across key rotations and
would make `/votes?voter_id=<cc_cold1…>` return nothing.

### 1.4 Governance actions

| Method | Path | Response | Source |
|---|---|---|---|
| GET | `/gov-actions` | `Page GovActionSummary` | db-sync |
| GET | `/gov-action/{gov-action-id}` | `GovActionDetailResponse` | both |

`{gov-action-id}` capture **(decision)**: accepts `<tx_hash_hex>#<index>` (the GovTool URL form) **and** CIP-129
bech32 `gov_action1…`. Canonical `id` in every response is `<tx_hash_hex>#<index>`; the bech32 is emitted alongside as
`bech32`. Invalid → 400 `invalid_gov_action_id`.

`GET /gov-actions` params: `limit` (50/200), `offset`, `type` (`[GovActionType]`, repeatable),
`state` (`[GovActionState]`: `proposed` `ratified` `enacted` `expired` `dropped`),
`proposed_from` / `proposed_to` (UTCTime), `expires_before_epoch` / `expires_after_epoch` (Int),
`proposer_stake_address` (Text), `min_treasury_amount` / `max_treasury_amount` (Integer, lovelace),
`anchor_status`, `q` ([2,100]),
`order_by` ∈ `proposed_at | expires_epoch | type | deposit | treasury_amount | votes_cast` (default `proposed_at`),
`sort_order` default `desc`.

### 1.5 Delegations, committee, SPO voters, treasury

| Method | Path | Key params | Response | Source |
|---|---|---|---|---|
| GET | `/delegations` | `limit` `offset` `drep_id[]` `stake_address[]` `is_current` (default `true`) `from` `to` `min_lovelace` `order_by`(`at`\|`lovelace`\|`stake_address`, default `at`) `sort_order`(`desc`) | `Page DelegationRecord` | db-sync |
| GET | `/committee` | — | `CommitteeResponse` | db-sync |
| GET | `/spos` | `limit` `offset` `min_voting_power` `min_votes_cast` `active_in_epoch` `q` `order_by`(`voting_power`\|`votes_cast`\|`pool_id`) `sort_order`(`desc`) | `Page SPOSummary` | db-sync |
| GET | `/treasury-withdrawals` | `limit` `offset` `gov_action_id[]` `beneficiary[]` `enacted_from_epoch` `enacted_to_epoch` `min_amount` `max_amount` `order_by`(`amount`\|`enacted_epoch`, default `enacted_epoch`) `sort_order`(`desc`) | `Page TreasuryWithdrawalRecord` | both |

`/committee` returns the **current** committee (from `epoch_state.committee_id` for the latest epoch) plus its
members; no pagination — committee size is small by construction (the member list of the enacted `NewCommittee`
action). **There is no `epoch_param.committee_max_size`**: Conway's only committee params are
`epoch_param.committee_min_size` and `epoch_param.committee_max_term_length` (schema.md @13.7.2.1). Historical committees are
reachable through the `new_committee` gov actions (db string `NewCommittee`, §2.6), so there is no `/committees`
list **(cut)**.

`/spos` is deliberately minimal per §4.3 — see §8.2 for the pool-ticker limitation. **Correction:** `/spos` must
**not** take `q`. SPO rows are a direct `pool_stat`/`pool_hash` read (§7.4, not projected) and `ticker` is `null` in
F1 (§8.2), so there is no trigram-indexed text to match and §4.3's "only our own projection tables" rule forbids
`%…%` on db-sync's. `q` is dropped from `/spos`; exact-prefix lookup of a `pool1…` id is what `/search` is for.

No `/treasury-withdrawal/{id}` **(cut)**: the row plus its parent gov-action link is the whole screen, and
`GET /gov-action/{id}` already carries the withdrawals of that action.

### 1.6 Activity feed (cursor semantics)

| Method | Path | Response | Source |
|---|---|---|---|
| GET | `/activity` | `Page ActivityEvent` | both |

**(decision) `offset` is not accepted on `/activity`.** Offset paging over a growing feed double-counts and skips;
that is the whole reason the Belt System's `since` is weak. Cursor rules:

| Param | Type | Default | Semantics |
|---|---|---|---|
| `limit` | Int | 20 | [1, 100] |
| `since` | `Cursor` | — | **exclusive** lower bound; result ordered **ascending** (catch-up / polling) |
| `before` | `Cursor` | — | **exclusive** upper bound; result ordered **descending** (infinite scroll) |
| `event_type` | `[ActivityEventType]` | all | §2.9 |
| `actor` | `[Text]` | — | DRep id, pool id, CC cold hash, or profile asset class |
| `source` | `[ActivitySource]` | all | `governance` \| `attestations` |

- Neither given → newest-first, same as `before` at the tip.
- Both given → 400 `both_cursors_given`.
- `Cursor` wire format **(decision, corrected by fact-check)**: `"<sort_slot>:<tx_index>:<item_index>"`, plain text,
  not base64 — it is not secret and a debuggable cursor is worth more than an opaque one. For tx-borne events
  `sort_slot` = `block.slot_no`, `tx_index` = `tx.block_index`, `item_index` = `voting_procedure.index` /
  `cert_index` / UTxO `output_index` depending on event type. **For epoch-boundary events (ratified / enacted /
  expired / dropped / treasury_withdrawal_enacted) db-sync has no slot and no tx at all** (§2.1), so
  `sort_slot` = first slot of the recorded epoch, `tx_index` = `-1`, `item_index` = `gov_action_proposal.id`.
  With that, the triple is monotone and total, so the feed can never skip or repeat an event across pages, even
  within one slot. The three components are **stored as columns on `activity_projection`**, not recomputed at read
  time. Unparseable → 400 `invalid_cursor`.
- Response `next_cursor` = the cursor of the **last** item in the page in the direction of travel, or `null` when
  the page is short. `total` on `/activity` is `-1` **(decision)**: a `COUNT(*)` over the whole merged feed on every
  poll is pure waste and no screen shows it; documented in the Swagger description for that field.

### 1.7 Unified search

| Method | Path | Response | Source |
|---|---|---|---|
| GET | `/search` | `SearchResults` | both |

`q` **required**, trimmed length [2, 100] (400 `query_too_short` / `query_too_long`).
`kinds` (`[SearchKind]`, default all): `drep` `gov_action` `spo` `committee_member` `profile` `attestation` `stake_address`.
`limit_per_group` Int, default 5, [1, 20] — the ⌘K palette shows five per group.
Shape reuses `SearchResults`/`SearchGroup`/`SearchHit`/`SearchTarget` from
`DomainTypes/Transfer/QueryResponses.hs` verbatim, with the group set swapped (§2.10).

### 1.8 Graph

| Method | Path | Response | Source |
|---|---|---|---|
| GET | `/graph` | `GraphData` | both |

Direct port of `/lineage`: `root` (Text, **required**), `kind` (`GraphKind`: `delegation` \| `attestation`, required),
`depth` (Int, required, [1, 3] — above 3 the Cytoscape canvas is unreadable and the query is unbounded;
out of range → 400 `invalid_parameter`), `min_lovelace` (Integer, optional, prunes dust delegation edges),
`max_nodes` (Int, default 300, [10, 1000]).
Node/edge shape mirrors `LineageGraphData` / `LineageNode` / `LineageEdge`.

### 1.9 Export (no new endpoints)

**(decision)** CSV is a second content type on the existing list endpoints, not a route:

```haskell
:> Get '[JSON, CSV] (Page DRepSummary)
```

- `CSV` is a new content type in `webapi-lib` (`WebAPI.CSV`): `data CSV`, `Accept CSV` → `text/csv;charset=utf-8`,
  `MimeRender CSV (Page a)` for any `a` with a `ToNamedRecord` + `DefaultOrdered` instance. Encoding via **`cassava`**
  (new dep, ~1 module of use) rather than hand-rolled string joining — quoting and embedded newlines are a
  correctness path, and the laziness rule does not apply to correctness on edge cases.
- **CSV formula-injection guard (not optional):** every cell whose first character is one of `= + - @ 0x09 0x0D`
  is prefixed with a single `'`. One function `WebAPI.CSV.safeCell :: Text -> Text`, applied by the `ToNamedRecord`
  instances. Exported governance data is public text that lands in spreadsheets; this is a trust boundary.
- CSV bounds **(decision, corrected by fact-check)**: `limit` default **10000**, max **10000** (vs JSON 50/200),
  because researchers and journalists are the stated audience and 200-row exports are useless. Same filters, same
  ordering, same tiebreaker. **This needs a mechanism `Get '[JSON, CSV]` alone does not give.** Servant resolves
  content negotiation *inside* the `Verb` server instance and never hands the negotiated type to the handler, so a
  handler cannot clamp `limit` differently per `Accept`. The endpoint type therefore carries the header explicitly:
  ```haskell
  :> Header "Accept" Text :> Get '[JSON, CSV] (Page DRepSummary)
  ```
  and `normalizeLimitOffset` takes its `maxLimit` from one helper
  `maxLimitFor :: Maybe Text -> Int` (`Just h | "text/csv" `T.isInfixOf` h = 10000; otherwise 200`). The header is
  marked `required: false` in Swagger. The alternative (`?format=csv`) is still rejected — one mechanism, and the
  header is the one the `Accept`-based download already uses.
- Enabled on: `/dreps`, `/votes`, `/gov-actions`, `/delegations`, `/treasury-withdrawals`, `/spos`, `/attestations`,
  `/disputes`, `/drep/{id}/delegators`. **Not** on composites, `/activity`, `/search`, `/graph`,
  `/status` — nobody exports a page composite. **Not** on `/drep/{id}/power` either (fact-check: §1.2 fixes that
  endpoint at max 200, which contradicted the 10000 CSV ceiling; 200 epoch rows is one page of JSON, so a CSV
  variant buys nothing).
- FE download: `fetch(url, {headers:{Accept:'text/csv'}})` → `Blob` → `URL.createObjectURL` → `<a download>`.
  One mechanism; no `?format=` parameter **(decision)** — two mechanisms for one feature is the thing to avoid.

### 1.10 Attestation side (the Kupo pipeline)

| Method | Path | Key params | Response | Source |
|---|---|---|---|---|
| GET | `/profiles` | `limit` `offset` `profile[]` `kind` `claimed_drep_id` `claim_verified` `q` `order_by`(`created_at`\|`name`\|`id`) `sort_order` | `Page ProfileSummary` | attest |
| GET | `/profile/{profile-id}` | — | `ProfileDetailResponse` | both |
| GET | `/attestations` | `limit` `offset` `attestation[]` `issuer[]` `subject[]` `kind[]` `state[]` `from` `to` `has_dispute` `q` `order_by`(`issued_at`\|`stated_at`\|`kind`) `sort_order` | `Page AttestationRecord` | attest |
| GET | `/attestation/{attestation-id}` | — | `AttestationDetailResponse` | attest |
| GET | `/disputes` | `limit` `offset` `dispute[]` `target_attestation[]` `target_profile[]` `opener[]` `state[]` `from` `to` `order_by`(`opened_at`\|`deposit`) `sort_order` | `Page DisputeRecord` | attest |
| GET | `/dispute/{dispute-id}` | — | `DisputeRecord` | attest |

`{profile-id}` / `{attestation-id}` / `{dispute-id}` captures are asset classes, parsed exactly as `ProfileRefAC` /
`AchievementAC` are today (`maybeReader` + JSON decode; `GYAssetClass` form `<56-hex policy>.<hex name>`).
Invalid → 400 `invalid_asset_class`.

**Inherited-fix #3 (projection poisoning) is a contract requirement here, not just a storage one:** every row
these endpoints return must come from a projection that verified the UTxO's value actually holds the NFT named in
its datum. The API-visible consequence: `ProfileSummary.claim_verified` and `AttestationRecord.state` are
meaningless without it, so the projection-level token check is listed as acceptance for these endpoints.

### 1.11 Page composites

See §3. Paths: `/pages/dashboard`, `/pages/drep-explorer`, `/pages/drep/{drep-id}`, `/pages/gov-action/{gov-action-id}`.

---

## 2. Response DTOs

All of them follow the house JSON rule verbatim:

```haskell
deriving stock (Generic)
deriving (FromJSON, ToJSON)
  via CustomJSON '[FieldLabelModifier '[StripPrefix "<prefix>", CamelToSnake]] <Type>

instance ToSchema <Type> where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "<prefix>")
```

House naming applied: the field prefix *is* the `StripPrefix` token and nothing is repeated after it
(`drepId`, never `drepDrepId`; `withdrawalAmount`, never `treasuryWithdrawalAmount`).

Module layout, mirroring the Belt System:
`src/lib/offchain-lib/DomainTypes/Transfer/QueryResponses.hs` (DTOs) ·
`.../Transfer/Filters.hs` (filter records + `*FromParams`) · `.../Transfer/OrderBy.hs` (sort enums + `FromHttpApiData`) ·
`.../Core/Types.hs` (domain types: `DRepId`, `GovActionId`, `ChainPoint`, enums).

### 2.1 Shared primitives

```haskell
-- | DRep identifier. Canonical rendering is the CIP-129 bech32 form (@drep1…@ over
-- header-byte 0x22/0x23 + hash), which we COMPUTE — db-sync only stores the CIP-105
-- form in @drep_hash.view@ (HRP @drep@ for key and script alike; no @drep_script1…@).
-- The two reserved ledger options render as db-sync's own literal views.
newtype DRepId = DRepId Text
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)
-- JSON: "drep1abc…" | "drep_always_abstain" | "drep_always_no_confidence"

-- | Governance action identifier, canonically @<tx_hash_hex>#<index>@.
newtype GovActionId = GovActionId Text
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

-- | Opaque-but-readable feed cursor: @\<slot\>:\<tx_index\>:\<item_index\>@.
newtype Cursor = Cursor Text
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

-- | Where and *when on chain* something happened. The single carrier of inherited
-- fix #5: @time@ is always @block.time@ (chain time), never a DB @insertedAt@.
data ChainPoint = ChainPoint
  { chainPointSlot   :: Maybe Integer -- slot      null for epoch-boundary events
  , chainPointEpoch  :: Int           -- epoch
  , chainPointTime   :: UTCTime       -- time
  , chainPointTxHash :: Maybe Text    -- tx_hash   null for epoch-boundary events
  }
-- tx event:    {"slot":84233900,"epoch":215,"time":"2026-10-19T08:14:02Z","tx_hash":"a1b2…"}
-- epoch event: {"slot":null,"epoch":215,"time":"2026-10-19T00:00:00Z","tx_hash":null}
```

**Corrected by fact-check — `slot` and `tx_hash` must be nullable.** Ratification, enactment, expiry and drop are
**epoch-boundary ledger events with no transaction and no slot**: db-sync records them only as
`gov_action_proposal.{ratified,enacted,expired,dropped}_epoch` (`word31type`, schema.md @13.7.2.1). There is no
column anywhere in db-sync from which a slot, a block time or a tx hash for those events can be derived. The
consequences, all of which the plan must carry:

- `TreasuryWithdrawalRecord.enacted_at`, `GovActionLifecycle`-driven timeline entries and the activity event types
  `gov_action_ratified` / `gov_action_enacted` / `gov_action_expired` / `gov_action_dropped` /
  `treasury_withdrawal_enacted` all emit an epoch-only `ChainPoint`: `epoch` = the recorded epoch, `time` = the
  `block.time` of the **first block of that epoch** (`MIN(block.id) WHERE epoch_no = e`, documented as such),
  `slot` and `tx_hash` `null`.
- **The §1.6 activity cursor cannot be `<slot>:<tx_index>:<item_index>` for these events.** Corrected cursor:
  `"<sort_slot>:<tx_index>:<item_index>"` where an epoch-boundary event uses the **first slot of its epoch** as
  `sort_slot`, `tx_index = -1` (sorts before every tx in that epoch) and `item_index` =
  `gov_action_proposal.id`, which makes the triple total again. This is a projection-time decision, so
  `activity_projection` stores the three cursor components as columns, never derives them at read time.

**(decision)** `ChainPoint` is the only way a timestamp enters a response. There is no bare `UTCTime` field named
`*_at` anywhere except `anchorFailedAt` (an off-chain fetch-failure time, correctly not chain time) and
`attestationStatedAt` (a datum-carried date, bounded by the tx validity range — inherited fix #4). Making
chain time a *type* rather than a convention is what stops fix #5 regressing.

### 2.2 Envelope

```haskell
-- | Uniform list envelope. Replaces the Belt System's bare-array list responses,
-- which carried no total and forced the per-screen @*PageResponse@ workaround.
data Page a = Page
  { pageItems      :: [a]         -- items
  , pageTotal      :: Int         -- total       (-1 on /activity; see §1.6)
  , pageLimit      :: Int         -- limit       the APPLIED limit after clamping
  , pageOffset     :: Int         -- offset      the APPLIED offset
  , pageNextCursor :: Maybe Cursor-- next_cursor null on non-cursor endpoints
  }
  deriving stock (Generic)
  deriving (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "page", CamelToSnake]] (Page a)

instance (ToSchema a, Typeable a) => ToSchema (Page a) where
  declareNamedSchema = genericDeclareNamedSchema (mkStripPrefixSchemaOptions "page")
```

`limit`/`offset` echo the **applied** values so a client can detect clamping without guessing (§4.2).

### 2.3 Error

```haskell
data ApiError = ApiError
  { errorCode    :: ErrorCode  -- code
  , errorMessage :: Text       -- message
  }
  deriving (FromJSON, ToJSON)
    via CustomJSON '[FieldLabelModifier '[StripPrefix "error", CamelToSnake]] ApiError
-- {"code":"drep_not_found","message":"No DRep with that identifier"}
```

**Extraction-time change to `webapi-lib`:** `WebAPI.Errors.mkServantErr` currently writes a bare string body
(`errBody = BL8.pack msg`) with no content type. It becomes
`mkServantErr :: Int -> ErrorCode -> ServerError` which encodes `ApiError` and sets
`errHeaders = [("Content-Type","application/json")]`. `genericErrorMessage` becomes a total function from
`ErrorCode` to the client-facing message (internal detail stays in the log, as today).

```haskell
data ErrorCode
  -- 400
  = InvalidParameter | QueryTooShort | QueryTooLong | OffsetTooLarge | LimitTooLarge
  | InvalidCursor | BothCursorsGiven | InvalidDrepId | InvalidGovActionId
  | InvalidAssetClass | InvalidTimeRange | InvalidEpochRange
  -- 404
  | DrepNotFound | GovActionNotFound | SpoNotFound | CommitteeNotFound
  | ProfileNotFound | AttestationNotFound | DisputeNotFound
  -- 429 (emitted by the BFF; declared here so it appears in Swagger)
  | RateLimited
  -- 503
  | NotReady | IndexerBehind | DbUnavailable
  -- 500
  | InternalError
  deriving (Show, Generic, Eq)
  deriving (FromJSON, ToJSON)
    via CustomJSON '[ConstructorTagModifier '[CamelToSnake]] ErrorCode
-- wire: invalid_parameter | query_too_short | … | internal_error
```

Status mapping lives in one place, `txBuildingExceptionToHttpStatus`'s sibling
`errorCodeToHttpStatus :: ErrorCode -> Int`. No raw strings anywhere (house rule).

### 2.4 Metadata anchors

```haskell
data AnchorRef = AnchorRef
  { anchorUrl       :: Text            -- url
  , anchorHash      :: Text            -- hash        hex, the ON-CHAIN data_hash
  , anchorStatus    :: AnchorStatus    -- status
  , anchorFailedAt  :: Maybe UTCTime   -- failed_at   off_chain_vote_fetch_error.fetch_time (FAILURES ONLY)
  , anchorError     :: Maybe Text      -- error       db-sync's fetch_error, truncated to 200 chars
  , anchorRetries   :: Int             -- retries     off_chain_vote_fetch_error.retry_count
  }

data AnchorStatus
  = AnchorValid          -- valid          off_chain_vote_data row + is_valid = TRUE
  | AnchorInvalidSchema  -- invalid_schema off_chain_vote_data row + is_valid = FALSE or NULL
  | AnchorHashMismatch   -- hash_mismatch  fetch_error row matching the mismatch rule (§7.3)
  | AnchorOversized      -- oversized      fetch_error row matching the size rule (§7.3)
  | AnchorUnreachable    -- unreachable    fetch_error row, any other reason
  | AnchorNotFetched     -- not_fetched    anchor present on chain, no row either side yet
  deriving (FromJSON, ToJSON)
    via CustomJSON '[ConstructorTagModifier '[StripPrefix "Anchor", CamelToSnake]] AnchorStatus
```

**Corrected by fact-check — there is no `fetched_at` for a successful fetch.** `off_chain_vote_data` has no
timestamp column at all (its columns are `id, voting_anchor_id, hash, language, comment, json, bytes, warning,
is_valid` — schema.md @13.7.2.1); only `off_chain_vote_fetch_error.fetch_time` exists. A `fetched_at` field would
therefore be non-null exactly for the anchors that *failed*, which is the opposite of what the name promises.
Renamed to `failed_at` and documented as failure-only. If a fetch time for valid anchors is wanted later it has to
be our own column, written when our projection first observes the `off_chain_vote_data` row.

**`oversized` is restored (the §4.1 spec verdict was cut for a false reason).** It *is* distinguishable:
`OCFErrDataTooLong` renders as `"… Size error, fetching metadata from <url> exceeded 512 bytes."`
([`Cardano/DbSync/Types.hs` @13.7.2.1, `instance Show OffChainFetchError`](https://github.com/IntersectMBO/cardano-db-sync/blob/13.7.2.1/cardano-db-sync/src/Cardano/DbSync/Types.hs)),
so it is one more `ILIKE` arm in the §7.3 `CASE`, not an invented verdict.

### 2.5 DRep

```haskell
data DRepStatus
  = DRepActive    -- active    registered, and drep_distr.active_until >= current epoch
  | DRepInactive  -- inactive  registered, but active_until < current epoch (CIP-1694 drep_activity lapse)
  | DRepRetired   -- retired   latest drep_registration row has deposit < 0
  | DRepReserved  -- reserved  drep_always_abstain / drep_always_no_confidence
  deriving (FromJSON, ToJSON)
    via CustomJSON '[ConstructorTagModifier '[StripPrefix "DRep", CamelToSnake]] DRepStatus

data DRepSummary = DRepSummary
  { drepId                :: DRepId        -- id
  , drepHex               :: Maybe Text    -- hex                 drep_hash.raw; null for reserved
  , drepHasScript         :: Bool          -- has_script
  , drepStatus            :: DRepStatus    -- status
  , drepGivenName         :: Maybe Text    -- given_name          off_chain_vote_drep_data.given_name
  , drepImageUrl          :: Maybe Text    -- image_url
  , drepLanguage          :: Maybe Text    -- language            off_chain_vote_data.language
  , drepAnchor            :: Maybe AnchorRef -- anchor
  , drepVotingPower       :: Maybe Integer -- voting_power        lovelace, latest drep_distr.amount; null = no snapshot yet
  , drepVotingPowerEpoch  :: Maybe Int     -- voting_power_epoch  ALWAYS paired with the number above; both null together
  , drepDelegatorCount    :: Int           -- delegator_count
  , drepActiveUntilEpoch  :: Maybe Int     -- active_until_epoch  NULL in db-sync for the reserved DReps
  , drepRegisteredAt      :: Maybe ChainPoint -- registered_at     null for the reserved DReps (no drep_registration row)
  , drepRetiredAt         :: Maybe ChainPoint -- retired_at
  , drepAgeDays           :: Maybe Int     -- age_days            null whenever registered_at is null
  , drepVotesCast         :: Int           -- votes_cast
  , drepEligibleActions   :: Int           -- eligible_actions
  , drepParticipationRate :: Maybe Double  -- participation_rate  null when eligible_actions = 0
  , drepRationaleRate     :: Maybe Double  -- rationale_rate      null when votes_cast = 0
  }
```

No `score`, no `rank_overall`, no composite of any kind (S5).

**Why four fields became `Maybe` (fact-check):** `drep_hash` rows exist for credentials that were never registered
and for the two reserved options, which have **no `drep_registration` row ever**
(`insertDrep: DRepAlwaysAbstain -> DB.insertDrepHashAlwaysAbstain`, `GovAction.hs:361-365`), so
`registered_at` / `age_days` cannot be filled for them. `drep_distr` is written once per epoch boundary
(`insertDrepDistr`, `GovAction.hs:379-405`), so a DRep that registered inside the current epoch has no power row
yet and `voting_power` is genuinely unknown rather than `0`. Serving `0` for "unknown" would be the same class of
lie the `basis` field in §2.6 exists to prevent. `order_by=voting_power` sorts `NULLS LAST` in both directions.

```haskell
data DRepMetrics = DRepMetrics
  { metricsParticipationRate  :: Maybe Double -- participation_rate
  , metricsRationaleRate      :: Maybe Double -- rationale_rate
  , metricsAgeDays            :: Int          -- age_days
  , metricsVoteLatencyMedianHours :: Maybe Double -- vote_latency_median_hours
  , metricsDelegatorRetentionRate :: Maybe Double -- delegator_retention_rate
  , metricsVotesByType        :: [(GovActionType, VoteCounts)] -- votes_by_type
  , metricsFormulaIds         :: [Text]       -- formula_ids   keys into /metrics/formulas
  }

data VoteCounts = VoteCounts
  { countsYes :: Int, countsNo :: Int, countsAbstain :: Int }  -- yes | no | abstain

data EpochPowerPoint = EpochPowerPoint
  { epochPowerEpoch          :: Int       -- epoch
  , epochPowerLovelace       :: Integer   -- lovelace
  , epochPowerDelegatorCount :: Int       -- delegator_count
  , epochPowerRank           :: Maybe Int -- rank          position among DReps that epoch
  }

data DelegatorStats = DelegatorStats
  { delegatorStatsCount          :: Int          -- count
  , delegatorStatsLovelace       :: Integer      -- lovelace
  , delegatorStatsNewLastEpoch   :: Int          -- new_last_epoch
  , delegatorStatsLostLastEpoch  :: Int          -- lost_last_epoch
  , delegatorStatsRetentionRate  :: Maybe Double -- retention_rate
  }

data DelegatorRow = DelegatorRow
  { delegatorStakeAddress :: Text       -- stake_address
  , delegatorLovelace     :: Maybe Integer -- lovelace    null when epoch_stake is unavailable
  , delegatorDelegatedAt  :: ChainPoint -- delegated_at
  , delegatorIsCurrent    :: Bool       -- is_current
  }

data DRepUpdateRow = DRepUpdateRow
  { updateKind   :: DRepUpdateKind  -- kind     registered | updated | retired
  , updateAt     :: ChainPoint      -- at
  , updateDeposit :: Maybe Integer  -- deposit
  , updateAnchor :: Maybe AnchorRef -- anchor
  }
```

Published metric formulas (served verbatim by `/metrics/formulas`):

| `formula_id` | Expression | Inputs |
|---|---|---|
| `participation_rate_v1` | `votes_cast_on_eligible / eligible_actions` | `voting_procedure`, `gov_action_proposal`, `drep_registration`; eligible = action whose `[proposed_at, resolution]` window intersects the DRep's registered-and-not-retired interval, restricted to types that role may vote on |
| `rationale_rate_v1` | `votes_with_anchor_status_valid / votes_cast` | `voting_procedure.voting_anchor_id`, `off_chain_vote_data.is_valid` |
| `age_days_v1` | `(chain_tip_time - registered_at.time) / 86400` | `block.time` |
| `vote_latency_median_hours_v1` | `median(cast_at.time - proposed_at.time)` over the DRep's votes | `block.time` via `tx` |
| `delegator_retention_rate_v1` | `|D(e) ∩ D(e-1)| / |D(e-1)|` where `D(e)` = stake addresses delegating at the end of epoch `e` | `delegation_vote`, `stake_deregistration` |
| `gov_action_state_v1` | `enacted_epoch → enacted; else expired_epoch → expired; else dropped_epoch → dropped; else ratified_epoch → ratified; else proposed` | `gov_action_proposal` |
| `tally_cast_votes_v1` | per role: `yes_power / (yes_power + no_power)`; abstain excluded from the denominator; **no default votes applied** | `voting_procedure`, `drep_distr`, `pool_stat.voting_power`, `committee_member` |

### 2.6 Governance actions

```haskell
-- db-sync's govactiontype enum, verbatim. Note NewCommittee, NOT UpdateCommittee:
-- the DB string is "NewCommittee" (Cardano/Db/Types.hs govActionTypeEncoder).
data GovActionType
  = ParameterChange | HardForkInitiation | TreasuryWithdrawals
  | NoConfidence | NewCommittee | NewConstitution | InfoAction
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[CamelToSnake]] GovActionType
-- wire: parameter_change | hard_fork_initiation | treasury_withdrawals
--     | no_confidence | new_committee | new_constitution | info_action

data GovActionState
  = Proposed | Ratified | Enacted | Expired | Dropped
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[CamelToSnake]] GovActionState

data VoterRole
  = DRep | SPO | ConstitutionalCommittee
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[CamelToSnake]] VoterRole
-- wire: d_rep?  NO — see note below

data Vote = Yes | No | Abstain
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[CamelToSnake]] Vote
-- wire: yes | no | abstain
```

**(decision)** `VoterRole`'s wire values are hand-written, not derived, because `CamelToSnake` would turn `DRep`
into `d_rep` and `SPO` into `s_p_o`. Use
`ConstructorTagModifier '[Rename "DRep" "drep", Rename "SPO" "spo", Rename "ConstitutionalCommittee" "constitutional_committee"]`
(`deriving-aeson`'s `Rename`), and spell the same three strings in the `FromHttpApiData` instance and the
`ToParamSchema` `enum_`. This is exactly the trap `ActivityEventType` works around in the Belt System with
`StripPrefix "Evt"`; it must be checked by a round-trip test, because a silent `d_rep` in the public API is a
breaking change later.

```haskell
data GovActionLifecycle = GovActionLifecycle
  { lifecycleState         :: GovActionState -- state
  , lifecycleRatifiedEpoch :: Maybe Int      -- ratified_epoch
  , lifecycleEnactedEpoch  :: Maybe Int      -- enacted_epoch
  , lifecycleExpiredEpoch  :: Maybe Int      -- expired_epoch
  , lifecycleDroppedEpoch  :: Maybe Int      -- dropped_epoch
  , lifecycleFormulaId     :: Text           -- formula_id  "gov_action_state_v1"
  }

data GovActionSummary = GovActionSummary
  { govActionId           :: GovActionId          -- id      "<tx_hash>#<index>"
  , govActionBech32       :: Text                 -- bech32  CIP-129 gov_action1…
  , govActionType         :: GovActionType        -- type
  , govActionTitle        :: Maybe Text           -- title   off_chain_vote_gov_action_data.title
  , govActionAnchor       :: Maybe AnchorRef      -- anchor
  , govActionProposedAt   :: ChainPoint           -- proposed_at
  , govActionExpiresEpoch :: Maybe Int            -- expires_epoch   gov_action_proposal.expiration
  , govActionLifecycle    :: GovActionLifecycle   -- lifecycle
  , govActionDeposit      :: Integer              -- deposit
  , govActionProposerStakeAddress :: Text         -- proposer_stake_address  from return_address
  , govActionTreasuryAmount :: Maybe Integer      -- treasury_amount  sum of treasury_withdrawal
  , govActionVotesCast    :: Int                  -- votes_cast
  , govActionPrevious     :: Maybe GovActionId     -- previous   prev_gov_action_proposal
  }

data VoteTally = VoteTally
  { tallyRole         :: VoterRole  -- role
  , tallyEpoch        :: Int        -- epoch       epoch of the power snapshot used
  , tallyYesPower     :: Integer    -- yes_power   lovelace; 0 for the CC
  , tallyNoPower      :: Integer    -- no_power
  , tallyAbstainPower :: Integer    -- abstain_power
  , tallyCounts       :: VoteCounts -- counts
  , tallyRatio        :: Maybe Double -- ratio      yes/(yes+no); null when yes+no = 0
  , tallyThreshold    :: Maybe Double -- threshold  see the (role, type) table below; null where none applies
  , tallyThresholdParam :: Maybe Text -- threshold_param  the epoch_param column name used, e.g. "dvt_treasury_withdrawal"
  , tallyBasis        :: TallyBasis -- basis
  , tallyFormulaId    :: Text       -- formula_id  "tally_cast_votes_v1"
  }

data TallyBasis = CastVotesOnly
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[CamelToSnake]] TallyBasis
-- wire: "cast_votes_only"
```

**`threshold` per (role, gov action type) — spelled out, because "`dvt_*`/`pvt_*` for the action type" is not a
function (fact-check against `epoch_param` in schema.md @13.7.2.1):**

| Gov action type | DRep threshold column | SPO threshold column | CC |
|---|---|---|---|
| `no_confidence` | `dvt_motion_no_confidence` | `pvt_motion_no_confidence` | does not vote → `null` |
| `new_committee` | `dvt_committee_normal` **or** `dvt_committee_no_confidence` | `pvt_committee_normal` **or** `pvt_committee_no_confidence` | does not vote → `null` |
| `new_constitution` | `dvt_update_to_constitution` | **none** → `null` | quorum |
| `hard_fork_initiation` | `dvt_hard_fork_initiation` | `pvt_hard_fork_initiation` | quorum |
| `treasury_withdrawals` | `dvt_treasury_withdrawal` | **none** → `null` | quorum |
| `parameter_change` | **max** over the groups the proposal touches: `dvt_p_p_network_group`, `dvt_p_p_economic_group`, `dvt_p_p_technical_group`, `dvt_p_p_gov_group` | `pvtpp_security_group` **only if** the proposal touches a security-group parameter, else `null` | quorum |
| `info_action` | **none** (never ratifiable) → `null` | **none** → `null` | `null` |

Three gotchas the plan must carry:

1. The SPO security-group column is spelled **`pvtpp_security_group`**, not `pvt_pp_security_group` or
   `pvt_security_group`. A `pvt_%`-prefix lookup silently misses it — see also §2.11.
2. `parameter_change` needs the *set of touched groups*, which comes from which columns of
   `param_proposal` (via `gov_action_proposal.param_proposal`) are non-null, mapped to the CIP-1694 groups. That
   mapping is a static table we own; it is the one piece of ledger knowledge in the tally path.
3. The `committee_normal` vs `committee_no_confidence` choice depends on whether the system is currently in a state
   of no-confidence. db-sync's `epoch_state.no_confidence_id` is documented as **"TODO: This remains NULL"**, so it
   cannot be read from there. **(decision)** F1 always reports the `_normal` column and says so in
   `threshold_param`; the `_no_confidence` variant is out of scope, which is consistent with §0.7 (we publish
   facts, not ratification predictions).

The `basis` field exists so the UI and the API docs can never present the ratio as a prediction. Swagger
description, verbatim: *"Tally over cast votes only. Default votes (non-voting SPOs, reward accounts delegated to
always-abstain / always-no-confidence, bootstrap-phase rules) are NOT applied, so `ratio` crossing `threshold`
does not imply ratification. The authoritative outcome is `lifecycle`."*

### 2.7 Votes, delegations, SPOs, committee, treasury

```haskell
data VoteRecord = VoteRecord
  { voteGovActionId    :: GovActionId     -- gov_action_id
  , voteGovActionType  :: GovActionType   -- gov_action_type
  , voteGovActionTitle :: Maybe Text      -- gov_action_title
  , voteVoterRole      :: VoterRole       -- voter_role
  , voteVoterId        :: Text            -- voter_id
  , voteVoterName      :: Maybe Text      -- voter_name
  , voteVerdict        :: Vote            -- verdict
  , voteAnchor         :: Maybe AnchorRef  -- anchor
  , voteRationaleTitle :: Maybe Text      -- rationale_title
  , voteCastAt         :: ChainPoint      -- cast_at
  , voteLatencyHours   :: Maybe Double    -- latency_hours
  , voteVotingPower    :: Maybe Integer   -- voting_power        at the epoch of cast_at
  , voteVotingPowerEpoch :: Maybe Int     -- voting_power_epoch
  }

data DelegationRecord = DelegationRecord
  { delegationStakeAddress :: Text          -- stake_address
  , delegationTarget       :: DRepId        -- target
  , delegationTargetName   :: Maybe Text    -- target_name
  , delegationLovelace     :: Maybe Integer -- lovelace
  , delegationAt           :: ChainPoint    -- at
  , delegationIsCurrent    :: Bool          -- is_current
  }

data SPOSummary = SPOSummary
  { spoPoolId           :: Text       -- pool_id      bech32 pool1…
  , spoTicker           :: Maybe Text -- ticker       null in F1; see §8.2
  , spoVotingPower      :: Integer    -- voting_power pool_stat.voting_power
  , spoVotingPowerEpoch :: Int        -- voting_power_epoch
  , spoDelegatorCount   :: Int        -- delegator_count  pool_stat.number_of_delegators
  , spoVotesCast        :: Int        -- votes_cast
  , spoParticipationRate :: Maybe Double -- participation_rate
  }

data CommitteeMemberStatus
  = CommitteeAuthorized | CommitteeUnauthorized | CommitteeResigned | CommitteeExpired
  deriving (FromJSON, ToJSON)
    via CustomJSON '[ConstructorTagModifier '[StripPrefix "Committee", CamelToSnake]] CommitteeMemberStatus

data CommitteeMemberSummary = CommitteeMemberSummary
  { committeeMemberColdHash        :: Text   -- cold_hash
  , committeeMemberColdBech32      :: Text   -- cold_bech32   CIP-129 cc_cold1…
  , committeeMemberHotHash         :: Maybe Text -- hot_hash
  , committeeMemberExpirationEpoch :: Int    -- expiration_epoch
  , committeeMemberStatus          :: CommitteeMemberStatus -- status
  , committeeMemberVotesCast       :: Int    -- votes_cast
  , committeeMemberAuthorizedAt    :: Maybe ChainPoint -- authorized_at
  , committeeMemberResignedAt      :: Maybe ChainPoint -- resigned_at
  }

data CommitteeResponse = CommitteeResponse
  { committeeQuorumNumerator   :: Int     -- quorum_numerator
  , committeeQuorumDenominator :: Int     -- quorum_denominator
  , committeeEstablishedBy     :: Maybe GovActionId -- established_by
  , committeeEpoch             :: Int     -- epoch
  , committeeMembers           :: [CommitteeMemberSummary] -- members
  }

data TreasuryWithdrawalRecord = TreasuryWithdrawalRecord
  { withdrawalGovActionId    :: GovActionId -- gov_action_id
  , withdrawalGovActionTitle :: Maybe Text  -- gov_action_title
  , withdrawalBeneficiary    :: Text        -- beneficiary    stake address bech32
  , withdrawalAmount         :: Integer     -- amount
  , withdrawalEnactedEpoch   :: Maybe Int   -- enacted_epoch   the ONLY authoritative value db-sync has
  , withdrawalEnactedAt      :: Maybe ChainPoint -- enacted_at   epoch-only ChainPoint: slot/tx_hash null (§2.1)
  , withdrawalProfileId      :: Maybe Text  -- profile_id     claimed profile, if any (both)
  , withdrawalAttestationCount :: Int       -- attestation_count  milestone attestations on that profile
  }
```

`withdrawalProfileId` / `withdrawalAttestationCount` are the one place where the two pipelines join on the
governance side — the vendor/treasury story in §4.2. Zero in F1 until profiles are claimed, which is fine.

### 2.8 Attestation side

```haskell
data ProfileKind = Person | Organization | Vendor
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[CamelToSnake]] ProfileKind

data ProfileSummary = ProfileSummary
  { profileId            :: ProfileRefAC  -- id
  , profileKind          :: ProfileKind   -- kind
  , profileName          :: Text          -- name
  , profileDescription   :: Text          -- description
  , profileImageUri      :: Text          -- image_uri
  , profileClaimedDrepId :: Maybe DRepId  -- claimed_drep_id
  , profileClaimVerified :: Bool          -- claim_verified   DRep credential signed the claim tx
  , profileCreatedAt     :: ChainPoint    -- created_at
  , profileOutputIndex   :: Int           -- output_index
  }

data AttestationKind = MilestoneDelivered | AuditPassed | Affiliation | Endorsement
  deriving (FromJSON, ToJSON) via CustomJSON '[ConstructorTagModifier '[CamelToSnake]] AttestationKind

data AttestationState = AttestationPending | AttestationAccepted
  deriving (FromJSON, ToJSON)
    via CustomJSON '[ConstructorTagModifier '[StripPrefix "Attestation", CamelToSnake]] AttestationState

data AttestationRecord = AttestationRecord
  { attestationId           :: AttestationAC  -- id
  , attestationKind         :: AttestationKind-- kind
  , attestationIssuerId     :: ProfileRefAC   -- issuer_id
  , attestationSubjectId    :: ProfileRefAC   -- subject_id
  , attestationState        :: AttestationState -- state
  , attestationStatedAt     :: UTCTime        -- stated_at   datum date, bounded by validity range (fix #4)
  , attestationPreviousId   :: Maybe AttestationAC -- previous_id  lineage
  , attestationAnchor       :: Maybe AnchorRef -- anchor      IPFS
  , attestationMetadata     :: [(Text, Text)] -- metadata
  , attestationIssuedAt     :: ChainPoint     -- issued_at
  , attestationAcceptedAt   :: Maybe ChainPoint -- accepted_at
  , attestationOutputIndex  :: Int            -- output_index
  , attestationDisputeCount :: Int            -- dispute_count
  }

data DisputeState = DisputeOpen | DisputeUpheld | DisputeRejected | DisputeWithdrawn
  deriving (FromJSON, ToJSON)
    via CustomJSON '[ConstructorTagModifier '[StripPrefix "Dispute", CamelToSnake]] DisputeState

data DisputeRecord = DisputeRecord
  { disputeId              :: DisputeAC   -- id
  , disputeTargetAttestationId :: Maybe AttestationAC -- target_attestation_id
  , disputeTargetProfileId :: Maybe ProfileRefAC -- target_profile_id
  , disputeOpenerId        :: ProfileRefAC -- opener_id
  , disputeState           :: DisputeState -- state
  , disputeDeposit         :: Integer      -- deposit
  , disputeAnchor          :: Maybe AnchorRef -- anchor
  , disputeOpenedAt        :: ChainPoint   -- opened_at
  , disputeResolvedAt      :: Maybe ChainPoint -- resolved_at
  , disputeOutputIndex     :: Int          -- output_index
  }
```

### 2.9 Activity

```haskell
data ActivitySource = SourceGovernance | SourceAttestations
  deriving (FromJSON, ToJSON)
    via CustomJSON '[ConstructorTagModifier '[StripPrefix "Source", CamelToSnake]] ActivitySource

data ActivityEventType
  = EvtDrepRegistered | EvtDrepUpdated | EvtDrepRetired
  | EvtVoteCast
  | EvtGovActionProposed | EvtGovActionRatified | EvtGovActionEnacted
  | EvtGovActionExpired  | EvtGovActionDropped
  | EvtDelegationChanged
  | EvtCommitteeAuthorized | EvtCommitteeResigned
  | EvtTreasuryWithdrawalEnacted
  | EvtProfileClaimed
  | EvtAttestationIssued | EvtAttestationAccepted
  | EvtDisputeOpened | EvtDisputeResolved
  deriving (Show, Generic, Eq)
  deriving (FromJSON, ToJSON)
    via CustomJSON '[ConstructorTagModifier '[StripPrefix "Evt", CamelToSnake]] ActivityEventType
-- wire: drep_registered | vote_cast | gov_action_proposed | … | dispute_resolved
```
**(decision)** snake_case wire values, unlike the Belt System's PascalCase `ActivityEventType` — the rest of this
API is snake_case and one inconsistent enum is a permanent papercut. The `Evt` prefix is kept for the same reason
it exists there (constructor clashes with `GovActionState`, `DisputeState`).

```haskell
data ActivityEvent = ActivityEvent
  { activityEventType    :: ActivityEventType -- event_type
  , activityEventSource  :: ActivitySource    -- source
  , activityEventCursor  :: Cursor            -- cursor      this event's own cursor
  , activityEventAt      :: ChainPoint        -- at
  , activityEventActorId :: Maybe Text        -- actor_id
  , activityEventActorName :: Maybe Text      -- actor_name
  , activityEventTargetId :: Maybe Text       -- target_id
  , activityEventTargetName :: Maybe Text     -- target_name
  , activityEventTitle   :: Text              -- title       human-readable, pre-rendered
  , activityEventBody    :: Maybe Text        -- body
  }
```
Pre-rendering `title`/`body` server-side is the Belt System's `ActivityEventDetails` pattern — the feed must not
force the FE to own 17 copy strings.

### 2.10 Search

Reuses `SearchResults` / `SearchGroup` / `SearchHit` / `SearchTarget` from `QueryResponses.hs` **unchanged in
shape**, with the group set replaced:

```haskell
data SearchResults = SearchResults
  { searchResultsQuery            :: Text        -- query
  , searchResultsTotal            :: Int         -- total
  , searchResultsDreps            :: SearchGroup -- dreps
  , searchResultsGovActions       :: SearchGroup -- gov_actions
  , searchResultsSpos             :: SearchGroup -- spos
  , searchResultsCommitteeMembers :: SearchGroup -- committee_members
  , searchResultsProfiles         :: SearchGroup -- profiles
  , searchResultsAttestations     :: SearchGroup -- attestations
  }
```
`SearchHit` keeps `{id, title, subtitle, badges, status, target}` and `SearchTarget` keeps `{kind, id}` — the FE
router maps `kind` to a route exactly as today.

### 2.11 Status

```haskell
data SyncStatus = SyncStatus
  { syncEpoch      :: Int      -- epoch
  , syncSlot       :: Integer  -- slot
  , syncBlockTime  :: UTCTime  -- block_time   chain time of the tip we have indexed
  , syncLagSeconds :: Int      -- lag_seconds  wall clock now - block_time
  , syncIsReady    :: Bool     -- is_ready
  , syncSource     :: Text     -- source       "cardano-db-sync" | "kupo"
  }

data StatusResponse = StatusResponse
  { statusApiVersion   :: Text       -- api_version
  , statusNetwork      :: Text       -- network        "preprod"
  , statusGovernance   :: SyncStatus -- governance
  , statusAttestations :: SyncStatus -- attestations
  , statusProtocol     :: ProtocolStatus     -- protocol     oracle: paused, min-UTxO, deposits, script hashes
  , statusEpochParams  :: EpochParamsSummary -- epoch_params thresholds in force
  }

data EpochParamsSummary = EpochParamsSummary
  { epochParamsEpoch                 :: Int    -- epoch
  , epochParamsDrepDeposit           :: Integer-- drep_deposit
  , epochParamsDrepActivity          :: Int    -- drep_activity
  , epochParamsGovActionDeposit      :: Integer-- gov_action_deposit
  , epochParamsGovActionLifetime     :: Int    -- gov_action_lifetime
  , epochParamsCommitteeMinSize      :: Int    -- committee_min_size
  , epochParamsCommitteeMaxTermLength :: Int   -- committee_max_term_length
  , epochParamsDrepThresholds        :: [(Text, Double)] -- drep_thresholds  the 10 dvt_* columns, by name
  , epochParamsPoolThresholds        :: [(Text, Double)] -- pool_thresholds  the 4 pvt_* columns PLUS pvtpp_security_group
  }
```
**(fact-check)** `pool_thresholds` is an explicit column list, not a `pvt_%` prefix scan: `epoch_param` holds
`pvt_motion_no_confidence`, `pvt_committee_normal`, `pvt_committee_no_confidence`, `pvt_hard_fork_initiation` and
`pvtpp_security_group` — the last one does **not** share the `pvt_` prefix (schema.md @13.7.2.1). There is no
`epoch_param.committee_max_size` (see §1.5).
`ProtocolStatus` is the Belt System DTO, re-pointed at the new oracle (fee fields → deposit fields; it also gains
`admin_key_hash` so the key-rotation fix #2 is observable from the API).

---

## 3. Page composites

One per first-UI screen, no more. Each is justified by the N+1 it collapses; a composite nobody renders does not
get built. Screen names are the spec's own F1 check 13 list.

### 3.1 `GET /pages/dashboard` → `DashboardPageResponse`

*Screen:* Hub + Dashboard (§4.5 "Hub, Dashboard"). Without it: 9 round trips before first paint.

```haskell
data DashboardPageResponse = DashboardPageResponse
  { dashboardEpoch                 :: Int            -- epoch
  , dashboardTotalDreps            :: Int            -- total_dreps
  , dashboardActiveDreps           :: Int            -- active_dreps
  , dashboardRetiredDreps          :: Int            -- retired_dreps
  , dashboardTotalDelegatedLovelace :: Integer       -- total_delegated_lovelace
  , dashboardAlwaysAbstainLovelace :: Integer        -- always_abstain_lovelace
  , dashboardAlwaysNoConfidenceLovelace :: Integer   -- always_no_confidence_lovelace
  , dashboardDelegatorCount        :: Int            -- delegator_count
  , dashboardActionsInFlight       :: Int            -- actions_in_flight       state = proposed
  , dashboardActionsByType         :: [(GovActionType, Int)] -- actions_by_type
  , dashboardParticipationByEpoch  :: [EpochParticipationPoint] -- participation_by_epoch  last 20 epochs
  , dashboardTopDrepsByPower       :: [DRepSummary]  -- top_dreps_by_power
  , dashboardClosingSoon           :: [GovActionSummary] -- closing_soon   expires within 5 epochs
  , dashboardRecentActivity        :: [ActivityEvent] -- recent_activity
  , dashboardTreasuryWithdrawnLovelace :: Integer    -- treasury_withdrawn_lovelace
  , dashboardAttestationCount      :: Int            -- attestation_count
  , dashboardClaimedProfileCount   :: Int            -- claimed_profile_count
  }

data EpochParticipationPoint = EpochParticipationPoint
  { participationEpoch         :: Int    -- epoch
  , participationVotesCast     :: Int    -- votes_cast
  , participationEligibleDreps :: Int    -- eligible_dreps
  , participationVotingDreps   :: Int    -- voting_dreps
  , participationRate          :: Maybe Double -- rate
  }
```
Params: `top_limit` (Int, default 10, [1, 50]), `closing_soon_limit` (default 5, [1, 20]),
`activity_limit` (default 10, [1, 50]), `epochs` (default 20, [1, 100]).

The always-abstain / always-no-confidence lovelace split is the spec's headline fact ("9.75 billion ADA parked on
always-abstain", §1) — it reads straight out of `drep_distr` joined to `drep_hash.view` on the two reserved
strings, so it costs one SQL line and earns the dashboard its reason to exist.

### 3.2 `GET /pages/drep-explorer` → `DRepExplorerPageResponse`

*Screen:* "DRep list with filters + pagination + URL params". Identical filter/sort set to `GET /dreps` (same
`*FromParams` constructor, one source of truth) plus the facet counts the filter sidebar needs — without them the
sidebar fires 4 extra count queries on every keystroke.

```haskell
data DRepExplorerPageResponse = DRepExplorerPageResponse
  { drepExplorerItems          :: [DRepSummary]      -- items
  , drepExplorerTotal          :: Int                -- total
  , drepExplorerLimit          :: Int                -- limit
  , drepExplorerOffset         :: Int                -- offset
  , drepExplorerStatusFacets   :: [(DRepStatus, Int)]-- status_facets
  , drepExplorerAnchorFacets   :: [(AnchorStatus, Int)] -- anchor_facets
  , drepExplorerLanguageFacets :: [(Text, Int)]      -- language_facets
  , drepExplorerPowerBuckets   :: [PowerBucket]      -- power_buckets
  , drepExplorerEpoch          :: Int                -- epoch
  }

data PowerBucket = PowerBucket
  { bucketLabel :: Text, bucketMinLovelace :: Integer, bucketMaxLovelace :: Maybe Integer, bucketCount :: Int }
  -- label | min_lovelace | max_lovelace | count
```
**(decision)** facets are computed over the filter set **with that facet's own predicate removed** (standard facet
semantics), so the sidebar can show "Retired (41)" while `status=active` is applied.

### 3.3 `GET /pages/drep/{drep-id}` → `DRepDetailResponse`

*Screen:* "full DRep page (unified timeline + metrics + delegators + attestations)". This is the single most
N+1-prone screen in the product: identity, anchor, metrics, per-epoch power, delegator stats, vote history and
attestations = 7 calls collapsed into 1.

**(decision)** `GET /drep/{drep-id}` and `GET /pages/drep/{drep-id}` return the **same** `DRepDetailResponse`, with
`/pages/…` the documented canonical path and the bare path kept as an alias for third-party API users who do not
want the page framing. One handler, two routes — no second DTO.

```haskell
data DRepDetailResponse = DRepDetailResponse
  { drepDetailDrep           :: DRepSummary        -- drep
  , drepDetailMetrics        :: DRepMetrics        -- metrics
  , drepDetailAnchorContent  :: Maybe AnchorContent-- anchor_content
  , drepDetailPowerByEpoch   :: [EpochPowerPoint]  -- power_by_epoch
  , drepDetailDelegatorStats :: DelegatorStats     -- delegator_stats
  , drepDetailTopDelegators  :: [DelegatorRow]     -- top_delegators
  , drepDetailVotes          :: Page VoteRecord    -- votes            first page, paginated after
  , drepDetailTimeline       :: [ActivityEvent]    -- timeline         reg/update/retire + votes + delegation swings
  , drepDetailProfile        :: Maybe ProfileSummary -- profile        claimed CIP-68 profile (attest)
  , drepDetailAttestations   :: [AttestationRecord] -- attestations    accepted, about this profile (attest)
  , drepDetailDisputes       :: [DisputeRecord]    -- disputes
  }

-- | The verified CIP-119 body, served from db-sync's off_chain_vote_* tables.
data AnchorContent = AnchorContent
  { anchorContentGivenName      :: Maybe Text -- given_name
  , anchorContentObjectives     :: Maybe Text -- objectives
  , anchorContentMotivations    :: Maybe Text -- motivations
  , anchorContentQualifications :: Maybe Text -- qualifications
  , anchorContentPaymentAddress :: Maybe Text -- payment_address
  , anchorContentImageUrl       :: Maybe Text -- image_url
  , anchorContentImageHash      :: Maybe Text -- image_hash
  , anchorContentReferences     :: [AnchorReference] -- references
  , anchorContentRaw            :: Maybe Value -- raw   off_chain_vote_data.json
  }

data AnchorReference = AnchorReference
  { referenceLabel :: Maybe Text, referenceUri :: Text }  -- label | uri
```
Params: `votes_limit` (default 25, [1, 100]), `delegators_limit` (default 10, [1, 50]),
`timeline_limit` (default 50, [1, 200]), `epochs` (default 200, [1, 200]).

`anchorContentRaw` is `off_chain_vote_data.json` passed through, so the verification panel can show exactly what
was hashed. Omitted when `anchor.status /= valid`.

### 3.4 `GET /pages/gov-action/{gov-action-id}` → `GovActionDetailResponse`

*Screen:* "gov action pages". Collapses action + metadata + 3 tallies + thresholds + withdrawals + proposer
track record + paginated votes.

```haskell
data GovActionDetailResponse = GovActionDetailResponse
  { govActionDetailAction        :: GovActionSummary -- action
  , govActionDetailAnchorContent :: Maybe GovActionAnchorContent -- anchor_content
  , govActionDetailTallies       :: [VoteTally]      -- tallies      one per role present
  , govActionDetailVotes         :: Page VoteRecord  -- votes
  , govActionDetailWithdrawals   :: [TreasuryWithdrawalRecord] -- withdrawals
  , govActionDetailParamChanges  :: [(Text, Value)]  -- param_changes   from param_proposal
  , govActionDetailProposer      :: ProposerSummary  -- proposer
  , govActionDetailTimeline      :: [ActivityEvent]  -- timeline     proposed → votes → ratified/enacted
  , govActionDetailPrevious      :: Maybe GovActionSummary -- previous
  , govActionDetailSupersededBy  :: [GovActionSummary] -- superseded_by
  }

data GovActionAnchorContent = GovActionAnchorContent
  { govAnchorTitle      :: Maybe Text -- title
  , govAnchorAbstract   :: Maybe Text -- abstract
  , govAnchorMotivation :: Maybe Text -- motivation
  , govAnchorRationale  :: Maybe Text -- rationale
  , govAnchorReferences :: [AnchorReference] -- references
  , govAnchorRaw        :: Maybe Value -- raw
  }

-- | The spec's "cine a propus (+ istoric al proposer-ului)".
data ProposerSummary = ProposerSummary
  { proposerStakeAddress   :: Text          -- stake_address
  , proposerActionsCount   :: Int           -- actions_count
  , proposerEnactedCount   :: Int           -- enacted_count
  , proposerFirstActionAt  :: Maybe ChainPoint -- first_action_at
  , proposerProfileId      :: Maybe ProfileRefAC -- profile_id        (attest)
  , proposerAttestationCount :: Int         -- attestation_count     (attest)
  }
```
Params: `votes_limit` (default 50, [1, 200]), `votes_order_by`, `votes_sort_order`, `timeline_limit` (default 50).

### 3.5 Composites deliberately NOT built

| Candidate | Why not |
|---|---|
| `/pages/gov-actions-explorer` | `GET /gov-actions` + `Page.total` is the whole screen; no facet sidebar in the F1 design. |
| `/pages/committee` | `GET /committee` already returns members inline — it *is* the composite. |
| `/pages/treasury` | `GET /treasury-withdrawals` with `gov_action_title` denormalised inline is the whole table. |
| `/pages/profile/{id}` | `GET /profile/{id}` → `ProfileDetailResponse` already nests attestations + disputes + the claimed DRep. One endpoint, not two. |
| `/pages/activity` | The feed is one endpoint by construction. |
| `/pages/graph` | `GET /graph` already returns nodes + edges in one call. |

---

## 4. Pagination and sorting hardening

These are the rules that stop F-17 and the LIKE-escaping bug from coming back. Each is enforced by a shared
helper in `Query.Common`, not by reviewer vigilance.

### 4.1 F-17: there is exactly one backend, and filtering happens in SQL

F-17 was: `Query/Live.hs` sliced to the page **before** applying belt / membership / text post-filters, so
`?belt=Black&limit=10&liveprojection` filtered the first 10 unfiltered rows
(`FableReview-0407.md` §F-17). Three rules:

1. **(decision) `withBackend`, `QueryFlag "liveprojection"` and `Query.Live` do not exist in this repo.** One
   projected path. The bug needs two backends to exist; deleting one deletes the divergence class. This is the
   root-cause fix, not a patch on the symptom.
2. **`Query.Common.applyLimits` is not ported.** Pagination is `LIMIT`/`OFFSET` inside the Esqueleto query. A
   handler that slices a Haskell list is a bug; the helper that enabled it is simply absent so it cannot be
   reached.
3. **`total` comes from a `COUNT(*)` with the byte-identical `WHERE`.** Mechanically guaranteed by building each
   list endpoint from one shared predicate function:
   ```haskell
   -- One predicate, two queries. The page and the count cannot drift.
   drepWhere :: DRepFilter -> SqlExpr (Entity DrepProjection) -> SqlExpr (Value Bool)
   ```
   and a shared runner `listAndCount :: (e -> SqlExpr (Value Bool)) -> Order -> (Limit, Offset) -> m (Page a)`.
   Acceptance test: for a filter that matches 3 of 50 rows, `limit=10` returns 3 items **and** `total = 3`.

### 4.2 Bounds

| Rule | Value | Behaviour outside |
|---|---|---|
| `limit` default (JSON) | 50 | — |
| `limit` max (JSON) | 200 | clamped silently; applied value echoed in `Page.limit` |
| `limit` min | 1 | clamped to 1 |
| `limit` default/max (CSV) | 10000 / 10000 | clamped |
| `limit` on `/activity` | default 20, max 100 | clamped |
| `offset` min | 0 | clamped to 0 |
| `offset` max | 10000 | **400 `offset_too_large`**, message points at cursor/filters |
| `q` length (trimmed) | [2, 100] | 400 `query_too_short` / `query_too_long` |
| `limit_per_group` (`/search`) | default 5, [1, 20] | clamped |
| `depth` (`/graph`) | [1, 3] | **400 `invalid_parameter`** |
| `from` > `to` | — | **400 `invalid_time_range`** |
| `from_epoch` > `to_epoch` | — | **400 `invalid_epoch_range`** |

**(decision)** clamp silently for `limit` (matches the Belt System's `max 1 (min 500 l)` and never breaks a
client), but **reject** `offset` beyond 10000 rather than clamping: silently returning page 10000's rows for
page 50000 is a correctness lie, and deep `OFFSET` over `COUNT` is the cheapest DoS on this API. Helper:

```haskell
-- | Normalize limit/offset. Returns Left on an offset past the hard ceiling.
-- maxLimit is 200 for JSON, 10000 for CSV, 100 for /activity.
normalizeLimitOffset :: Int -> Maybe Int -> Maybe Int -> Either ErrorCode (Limit, Offset)
```

### 4.3 LIKE escaping

**Reuse `WebAPI.Utils.escapeLikePattern` verbatim.** It is already correct:

```haskell
escapeLikePattern :: Text -> Text
escapeLikePattern =
  T.replace "_" "\\_"
    . T.replace "%" "\\%"
    . T.replace "\\" "\\\\"
```

The **composition order is load-bearing**: `.` applies right-to-left, so backslashes are doubled *first* and the
escapes introduced afterwards are not re-escaped. Reordering silently breaks it. Protections:

- The escape is only ever reached through `Query.Common.likePat`, copied as-is:
  ```haskell
  likePat :: Text -> SqlExpr (Value Text)
  likePat q = val ("%" <> escapeLikePattern (T.toLower q) <> "%")
  ```
  No handler builds a pattern by hand. Postgres `LIKE`'s default escape character is backslash, so no
  `ESCAPE` clause is needed.
- One property test, the smallest thing that fails if the order regresses:
  ```haskell
  -- escapeLikePattern never leaves an unescaped metacharacter,
  -- and a literal backslash survives as exactly one escaped backslash.
  prop_escapeLike :: Text -> Bool
  prop_escapeLike t = noBareMeta (escapeLikePattern t)
                   && escapeLikePattern "\\%" == "\\\\\\%"
  ```
- `q` is matched **only against columns in our own projection tables**, never db-sync's.
  **(decision)** consequence for the plan: any text the UI searches (DRep `given_name`, `objectives`, gov-action
  `title`/`abstract`, profile `name`) must be **copied into our projection rows**, because the `pg_trgm` GIN
  indexes that make `%…%` survivable go on *our* tables — adding indexes to db-sync's tables would fight its
  own migrations. Index DDL lives in our migration, e.g.
  `CREATE INDEX drep_projection_name_trgm ON drep_projection USING gin (lower(given_name) gin_trgm_ops);`

### 4.4 Stable sort tiebreaker

**(decision)** Every list query appends a deterministic, never-user-controlled final sort key, always ascending:

| Table family | Tiebreaker |
|---|---|
| db-sync-backed projections | `<projection>.id ASC` — our surrogate PK, assigned in chain order during projection |
| attestation projections | `slot ASC, tx_hash ASC, output_index ASC` — the triple already stored today |
| per-epoch series | `epoch ASC` (already total) |

Enforced by one helper that every handler must route through, so an `orderBy` without a tiebreaker is not
expressible:

```haskell
-- | Apply the user's sort key and direction, then the mandatory stable tiebreaker.
-- There is no API for ordering without a tiebreaker; equal keys otherwise reshuffle
-- between pages and rows get silently skipped or repeated.
orderByStable ::
  SortOrder ->
  SqlExpr (Value a) ->          -- user-selected key
  [SqlExpr OrderBy] ->          -- tiebreaker, always ASC
  SqlQuery ()
```
Acceptance test: a table where 30 of 50 rows share the sort key — paging `limit=10` three times returns 30
distinct ids. This fails loudly today on `order_by=belt` style keys and would fail on `order_by=status`,
`order_by=voting_power` (ties at 0) or `order_by=type` here.

### 4.5 Repeatable-parameter semantics

`QueryParams "status" DRepStatus` style parameters are **OR within a parameter, AND across parameters** — the
Belt System's `optionalNonEmpty` + `IN (…)` convention. Documented once in the Swagger top-level description
rather than on 40 parameters. Empty list = no filter (never "match nothing").

---

## 5. OpenAPI / Swagger

**(decision) Stay on `servant-swagger` + `swagger2` + `servant-swagger-ui` (Swagger 2.0).** Not
`servant-openapi3`/`openapi3` ([servant-openapi3 on Hackage](https://hackage.haskell.org/package/servant-openapi3)):
migrating would mean re-deriving every `ToSchema` instance plus `mkStripPrefixSchemaOptions` and
`addSharedSwaggerDescriptions`, and buys nothing the TS side cannot get for free (below).

| Item | Decision |
|---|---|
| Version string | `"1.0.0"` |
| Where | `src/exe/query-api/RestAPI.hs`, `apiSwagger & info . Data.Swagger.version .~ "1.0.0"` — one literal, same place as the Belt System's `"3.3.0"` |
| Independence | API version is independent of the app version (house rule); `cz bump` does not touch it |
| SemVer rule | removing/renaming a field or endpoint, or tightening a type → **major**; new endpoint, new optional field, new enum value → **minor**; description/example only → **patch** |
| Title | `"Cardano Governance Registry Query API"` |
| License | `"Apache-2.0"` (spec §7) |
| Spec URL | `GET /swagger-api.json` |
| UI | `GET /swagger-ui` |
| Generated file | `docs/generated/swagger/query-swagger-api.json` (the house filename — Belt System `query-api/Main.hs:36`); **never hand-edited** |
| CI guard | `cabal run query-api -- --write-swagger-only && git diff --exit-code docs/generated/swagger/`. **(fact-check) this flag does not exist in the Belt System** — `Main.hs:36` writes the file unconditionally and then serves forever, which CI cannot use, and it happens *after* config/DB wiring. Implementing `--write-swagger-only` (write the file from the pure `apiSwagger` value, exit 0, touch no config and no database) is a named task, not a free reuse. Without it the guard is aspirational. |
| Error responses | `allOperations . responses . at 400/404/429/503` with the `ApiError` schema inline, plus the `ErrorCode` enum in `definitions` so clients can exhaustively switch |
| Shared descriptions | `addSharedSwaggerDescriptions` reused, extended with `DRepId`, `GovActionId`, `ChainPoint`, `Cursor`, `AnchorStatus`, `VoterRole`, `GovActionType`, `TallyBasis` |

### What the UI generates from it

**(decision) types only, via `swagger-typescript-api`.** It reads Swagger 2.0 natively
([README](https://github.com/acacode/swagger-typescript-api): "Support for OpenAPI 3.0, 2.0, JSON and YAML"),
current version **13.13.0** — so there is no conversion step. `openapi-typescript` v6+ dropped Swagger 2.0
support, which is why it is not the choice.

```jsonc
// bjj-gov-frontend/package.json
"devDependencies": { "swagger-typescript-api": "13.13.0" },
"scripts": {
  "gen:api": "swagger-typescript-api generate -p ../backend/docs/generated/swagger/query-swagger-api.json -o ./src/api/generated -n types.ts --no-client"
}
```
`--no-client` is the real flag, **verified in the published 13.13.0 tarball** (`dist/cli.mjs`: a boolean arg
`client`, description `"do not generate an API class"`, wired to `generateClient: args.client`; `path` has alias
`p`, `output` alias `o`, `name` alias `n`). Note the internal detail: v13 depends on `swagger2openapi@^7`, so the
Swagger 2.0 → OpenAPI 3 conversion happens *inside* the tool — there is no conversion step **for us**, which is the
claim that matters. README line 3 is verbatim "Support for OpenAPI 3.0, 2.0, JSON and YAML".

- Generated `src/api/generated/types.ts` is **committed**, and CI runs `pnpm gen:api && git diff --exit-code` so a
  backend DTO change that the FE has not absorbed fails the FE build. That is the parity test, for free.
- **No generated client.** The FE keeps its own ~40-line `fetch` wrapper so the ported patterns stay ours:
  `RemoteData` states, `ApiError.code` → user-facing copy, BFF base URL from a build-time env var, and the
  freshness headers of §6. A generated Axios class would hide all four.

---

## 6. Freshness contract

**(decision) one mechanism: response headers, set by a single WAI middleware.** Not an envelope field.

```
X-Chain-Epoch: 215
X-Chain-Slot: 84233900
X-Chain-Block-Time: 2026-10-19T08:14:02Z
X-Data-Lag-Seconds: 42
X-Data-Source: db-sync          # or "kupo", or "db-sync+kupo" on composites
Cache-Control: public, max-age=10, stale-while-revalidate=30
```

The explorer's "synced to epoch 215, slot 84233900, 42s ago" chip reads those four headers off any response it
already made. Why headers and not an envelope field:

- An envelope field has to be added to **every** DTO — the single-object detail responses, the composites, and the
  CSV rows, where it does not even fit. Headers are one place.
- The BFF/CDN can route and cache on them without parsing a body.
- Adding freshness to a new endpoint later costs zero lines.

Implementation, `webapi-lib`:

```haskell
-- | Stamps every response with the indexer's chain position.
-- Freshness is read from a TVar refreshed every FRESHNESS_POLL_MS (default 5000)
-- by one background thread, so the hot path adds no query.
data Freshness = Freshness
  { freshEpoch :: Int, freshSlot :: Integer, freshBlockTime :: UTCTime
  , freshLagSeconds :: Int, freshSource :: Text }

freshnessMiddleware :: IO Freshness -> Middleware
```
The refresher runs one cheap query per pipeline —
`SELECT epoch_no, slot_no, time FROM block ORDER BY id DESC LIMIT 1` for governance, the chain-cursor row for
attestations — and **the same `TVar` feeds `/ready` and `/status`**, so there is exactly one definition of "how
fresh are we" in the service.

**The CORS gotcha, which is the whole reason this can fail silently:** a browser cannot read a non-safelisted
response header unless the server lists it. `WebAPI.CORS.mkCorsMiddleware` must set

```haskell
corsExposedHeaders = Just
  [ "X-Chain-Epoch", "X-Chain-Slot", "X-Chain-Block-Time"
  , "X-Data-Lag-Seconds", "X-Data-Source" ]
```
Without it `fetch()` sees none of them and the chip silently shows nothing. Acceptance: a Playwright assertion that
reads `response.headers()['x-chain-epoch']` on the dashboard request.

Not built **(cut)**: `ETag`/`If-None-Match` — `Cache-Control: max-age=10` plus the BFF cache covers the load, and
conditional requests over a 10-second-fresh dataset save nothing worth a hash per response.

---

## 7. Appendix: source map and config consequences

### 7.1 db-sync `insert_options` for this project

**(decision) do not use `preset`.** The docs state "Preset is an aggregate setting that overrides all other
properties" ([configuration.md](https://github.com/IntersectMBO/cardano-db-sync/blob/master/doc/configuration.md)),
and `only_governance` leaves `offchain_vote_data` **disabled** (its default since 13.7.0.1) and `shelley`
**disabled**. We need both on, so we spell out the preset's settings with two changes:

```json
"insert_options": {
  "tx_cbor": "disable",
  "tx_out": { "value": "disable" },
  "ledger": "enable",
  "shelley": { "enable": true },
  "multi_asset": { "enable": false },
  "metadata": { "enable": false },
  "plutus": { "enable": false },
  "governance": "enable",
  "offchain_pool_data": "disable",
  "offchain_vote_data": "enable",
  "pool_stat": "enable",
  "remove_jsonb_from_schema": "disable"
}
```

Why each change, one line each:

- `offchain_vote_data: enable` — this *is* our metadata fetcher (§0.1). It fetches each voting anchor, keeps only
  hash-matching content in `off_chain_vote_data`, records failures in `off_chain_vote_fetch_error` with
  `retry_count`, and parses CIP-119 into `off_chain_vote_drep_data` and CIP-108 into
  `off_chain_vote_gov_action_data`. Building our own would duplicate it *and* double the request load on DRep
  websites.
- `shelley: enable` — **required for correct delegator counts.** With it disabled, db-sync skips
  `insertStakeDeregistration` (`Certificate.hs`: `ConwayUnRegCert cred _dep -> when (ioShelley iopts) $ insertStakeDeregistration …`
  — [source](https://github.com/IntersectMBO/cardano-db-sync/blob/master/cardano-db-sync/src/Cardano/DbSync/Era/Universal/Insert/Certificate.hs)),
  so a stake key that deregisters still looks like an active delegator forever and `delegator_count` drifts
  upward. It also populates `pool_hash`/`pool_update` for SPO-voter identity.
- `ledger: enable` — unchanged from the established decision; disabling it empties `drep_distr`, `epoch_stake`,
  `epoch_param`, `reward` and `ada_pots` (configuration.md), which would take out voting power *and* the
  thresholds in §2.11.
- `remove_jsonb_from_schema: disable` — we read `gov_action_proposal.description` and `off_chain_vote_data.json`
  as JSON (`anchor_content.raw`, `param_changes`).

**Verification step for the plan (do not skip):** after the preprod sync, assert non-zero row counts in
`off_chain_vote_data`, `off_chain_vote_drep_data`, `off_chain_vote_gov_action_data`, `stake_deregistration`,
`pool_hash`, `pool_stat`, `drep_distr`, and record the resulting Postgres size against the ~16 GB minirack budget
in spec §8. **Fallback if the size budget blows:** revert to `preset: only_governance` and accept
`anchor.status = not_fetched` for all anchors in F1 — i.e. drop F1 check 7 to a later phase — rather than writing
a second fetcher.

### 7.2 DTO field → db-sync source

| DTO field | Source |
|---|---|
| `DRepSummary.id` | **computed CIP-129** bech32 from `drep_hash.raw` + `has_script` (header `0x22`/`0x23`, HRP `drep`); `drep_hash.view` is the CIP-105 form and is served as `id_cip105`. For reserved DReps `id` = `drep_hash.view` literal. |
| `DRepSummary.hex` / `.has_script` | `drep_hash.raw` (NULL for reserved — `drepHashRaw = Just bs` only on the credential path, `GovAction.hs:368-375`) / `.has_script` |
| `DRepSummary.status` | latest `drep_registration.deposit` sign (`>0` reg, `NULL` update, `<0` retire) + `drep_distr.active_until` vs current epoch; `drep_hash.view IN ('drep_always_abstain','drep_always_no_confidence')` → `reserved` |
| `DRepSummary.voting_power` / `.voting_power_epoch` | `drep_distr.amount` / `.epoch_no` (max epoch) |
| `DRepSummary.given_name` / `.image_url` | `off_chain_vote_drep_data.given_name` / `.image_url` |
| `DRepSummary.language` | `off_chain_vote_data.language` |
| `DRepSummary.delegator_count` | **ours**: replay `delegation_vote` + `stake_deregistration` → `drep_epoch_delegators` |
| `AnchorRef.url` / `.hash` | `voting_anchor.url` / `.data_hash` |
| `AnchorRef.status` | §7.3 |
| `AnchorContent.*` | `off_chain_vote_drep_data.*`, `off_chain_vote_reference.*`, `off_chain_vote_data.json` |
| `GovActionSummary.id` | `tx.hash` of `gov_action_proposal.tx_id` + `gov_action_proposal.index` |
| `GovActionSummary.type` | `gov_action_proposal.type` (`govactiontype` enum; `NewCommittee`, not `UpdateCommittee`) |
| `GovActionSummary.expires_epoch` / `.deposit` / `.previous` | `gov_action_proposal.expiration` / `.deposit` / `.prev_gov_action_proposal` |
| `GovActionSummary.proposer_stake_address` | `stake_address.view` via `gov_action_proposal.return_address` |
| `GovActionLifecycle.*` | `gov_action_proposal.{ratified,enacted,expired,dropped}_epoch` |
| `GovActionAnchorContent.*` | `off_chain_vote_gov_action_data.{title,abstract,motivation,rationale}` |
| `VoteRecord.voter_role` / `.verdict` | `voting_procedure.voter_role` (`voterrole`) / `.vote` (`vote`) |
| `VoteRecord.voter_id` | DRep: computed CIP-129 from `drep_hash` · SPO: `pool_hash.view` · CC: the **cold** `committee_hash.raw` resolved from the hot `voting_procedure.committee_voter` via `committee_registration` (§1.3), rendered CIP-129 `cc_cold1…`. `committee_hash` has **no `view` column** (only `id`, `raw`, `has_script`), so every committee bech32 is computed by us. |
| `ChainPoint.*` for tx-borne events | `block.{slot_no, epoch_no, time}` + `tx.hash` — join `tx → block`; `delegation_vote`, `drep_registration` and `voting_procedure` carry **no slot column**, so this join is mandatory (and is inherited fix #5) |
| `ChainPoint.*` for epoch-boundary events | `gov_action_proposal.{ratified,enacted,expired,dropped}_epoch` only. **No slot, no tx, no block time exists in db-sync for these.** `time` = `block.time` of `MIN(block.id)` in that epoch; `slot` and `tx_hash` are `null` (§2.1) |
| `VoteTally.threshold` | the explicit (role, type) → column table in §2.6; **not** a `dvt_%`/`pvt_%` prefix scan. CC: `committee.quorum_numerator`/`_denominator` |
| `SPOSummary.voting_power` / `.delegator_count` | `pool_stat.voting_power` / `.number_of_delegators` |
| `CommitteeResponse.*` | `epoch_state.committee_id` → `committee` + `committee_member`, with `committee_registration` / `committee_de_registration` for hot-key status |
| `TreasuryWithdrawalRecord.*` | `treasury_withdrawal` + `stake_address.view` + parent `gov_action_proposal` |
| `EpochParamsSummary.*` | `epoch_param` (latest epoch) |
| `SyncStatus` (governance) | `SELECT epoch_no, slot_no, time FROM block ORDER BY id DESC LIMIT 1` |

Column names verified against
[cardano-db-sync `doc/schema.md`](https://github.com/IntersectMBO/cardano-db-sync/blob/master/doc/schema.md);
enum string values against
[`cardano-db/src/Cardano/Db/Types.hs`](https://github.com/IntersectMBO/cardano-db-sync/blob/master/cardano-db/src/Cardano/Db/Types.hs).

### 7.3 `AnchorStatus` derivation (one SQL `CASE`, one place)

```sql
CASE
  WHEN ovd.id IS NOT NULL AND ovd.is_valid IS TRUE  THEN 'valid'
  WHEN ovd.id IS NOT NULL                           THEN 'invalid_schema'
  WHEN ovfe.id IS NOT NULL
   AND ovfe.fetch_error LIKE 'Hash mismatch%'       THEN 'hash_mismatch'
  WHEN ovfe.id IS NOT NULL
   AND ovfe.fetch_error LIKE '%Size error, fetching metadata from%' THEN 'oversized'
  WHEN ovfe.id IS NOT NULL                          THEN 'unreachable'
  ELSE 'not_fetched'
END
```

**The two match strings are now sourced, not guessed** (closes old open question §8.5/C1). From
[`Cardano/DbSync/Types.hs` @13.7.2.1 `instance Show OffChainFetchError`](https://github.com/IntersectMBO/cardano-db-sync/blob/13.7.2.1/cardano-db-sync/src/Cardano/DbSync/Types.hs):

- `OCFErrHashMismatch` → `"Hash mismatch when fetching metadata from <url>. Expected \"<hex>\" but got \"<hex>\"."`
  — note it is the **only** arm that does *not* get the `"Error Offchain Voting Anchor: "` prefix (it uses
  `showMUrl`, not `fetchUrlToString`), which is why the anchored `LIKE 'Hash mismatch%'` is safe and is preferred
  over `ILIKE '%hash mismatch%'` (no risk of matching a URL or a server-returned body containing that phrase).
- `OCFErrDataTooLong` → `"<prefix>Size error, fetching metadata from <url> exceeded 512 bytes."`

The fixture test still belongs in the plan, but as a regression pin against these exact strings, not as discovery.
Rationale: db-sync stores only hash-matching content in `off_chain_vote_data` and routes mismatches to
`off_chain_vote_fetch_error` (schema.md), and `is_valid` is a documented tri-state — TRUE = valid JSON conforming
to CIP-100, FALSE = hash matched but non-conforming, NULL = hash matched but not valid JSON.

**The two fragile lines are the `LIKE`s** — they match upstream error strings. They live in exactly one SQL
expression, default to `unreachable` when they do not match (never a false accusation against a DRep), and are
pinned by a unit test over fixture rows. If upstream reworks a message we lose that one split and nothing else.
Accepted over writing a second HTTP fetcher.

**Config fact the plan needs (§7.1 did not mention it):** anchors on `ipfs://` URLs are fetched through db-sync's
own `ipfs_gateway` setting — a **top-level config key** (not inside `insert_options`), a list of gateway base URLs,
default `["https://ipfs.io/ipfs"]`
([`Config/Types.hs:456` @13.7.2.1](https://github.com/IntersectMBO/cardano-db-sync/blob/13.7.2.1/cardano-db-sync/src/Cardano/DbSync/Config/Types.hs)).
`OCFErrNoIpfsGateway` ("No ipfs_gateway provided in the db-sync config") only fires if it is set to `[]`. The
single public default gateway rate-limits hard, which is a plausible contributor to the poor anchor coverage in
§8.5 — set two or three gateways. 13.7.x also **requires HTTPS** for anchor URLs and blocks private/loopback IPs
(configuration.md), so `http://` anchors land in `off_chain_vote_fetch_error` permanently; that is correct
behaviour to surface, not a bug to work around.

### 7.4 Our own projection tables (everything else reads db-sync directly)

Only these five exist, because everything else is a view over db-sync and the laziness rule says do not project
what SQL already answers:

| Table | Why it cannot be a db-sync query |
|---|---|
| `drep_projection` | holds the denormalised, **trigram-indexed** search text (`given_name`, `objectives`) and the precomputed metrics; we cannot index db-sync's tables |
| `drep_epoch_delegators` | db-sync has delegation *events*, not a per-epoch delegator snapshot; needed for `delegator_count` and `delegator_retention_rate_v1` |
| `gov_action_projection` | denormalised trigram search text (`title`, `abstract`) + the precomputed `state` and tallies |
| `vote_projection` | precomputed `latency_hours` and the power-at-cast-epoch join, which is the hot path on every vote list |
| `activity_projection` | the merged governance + attestation feed with the `(slot, tx_index, item_index)` cursor key |

Plus the attestation projections (`profile_projection`, `attestation_projection`, `dispute_projection`) and the
existing bookkeeping (`chain_cursor`, `raw_event_log`, `schema_version`), unchanged in mechanism from the Belt
System.

Explicitly **not** projected: committee, treasury withdrawals, epoch params, SPO stats, per-epoch DRep power.
Those are direct reads of `committee*`, `treasury_withdrawal`, `epoch_param`, `pool_stat`, `drep_distr`.

### 7.5 Koios cross-check mapping (CI job, spec §4.1)

| Ours | Koios |
|---|---|
| `GET /dreps` | `/drep_list`, `/drep_info` |
| `GET /drep/{id}/power` | `/drep_voting_power_history` |
| `GET /drep/{id}/delegators` | `/drep_delegators` |
| `GET /votes?voter_id=` | `/vote_list`, `/voter_proposal_list` |
| `GET /gov-actions` | `/proposal_list` |
| `GET /gov-action/{id}` tallies | `/proposal_voting_summary`, `/proposal_votes` |
| `GET /committee` | `/committee_info`, `/committee_votes` |
| `GET /spos` | `/pool_voting_power_history` |
| dashboard aggregates | `/drep_epoch_summary` |
| DRep anchor content | `/drep_metadata`, `/drep_updates` |

Paths from [api.koios.rest/koiosapi.yaml](https://api.koios.rest/koiosapi.yaml). Koios runs on db-sync too, so
this is a free consistency check in CI, never a data source (spec §4.1).

---

## 8. Open questions and limitations

### (BLOCKER) 8.1 Preprod Postgres size under `offchain_vote_data: enable` + `shelley: enable`

The spec budgets ~16 GB of Postgres on the minirack for `only_governance` (§8). Turning on `shelley` (stake and
pool certificates) and `offchain_vote_data` (fetched anchor bodies + `jsonb`) grows that by an amount I cannot
determine from documentation — it depends on preprod's certificate count and anchor payload sizes. **This must be
measured on the real preprod sync before the API contract is locked**, because the fallback (§7.1) deletes
`anchor.status` as a real field and therefore changes four DTOs, the `anchor_status` filter on three endpoints,
and F1 check 7. Nothing else in this note depends on it.

### 8.2 SPO pool tickers (limitation, accepted)

`offchain_pool_data` stays `disable`, so `SPOSummary.ticker` is `null` in F1 and SPO rows show the bech32
`pool1…` id only. Enabling it adds another fetcher workload for data that is cosmetic on a governance-reputation
site. The FE links out to a pool explorer. Revisit only if users ask.

### 8.3 `delegator.lovelace` depends on `epoch_stake`

`DelegatorRow.lovelace` and `DelegatorStats.lovelace` read `epoch_stake`, which `ledger: enable` populates but
which is also the single largest table db-sync writes. If 8.1 forces a size cut, `epoch_stake` is the first thing
to drop — `DelegatorRow.lovelace` is already `Maybe` for exactly this reason, and `drep_distr.amount` still gives
the aggregate voting power. No DTO change needed either way.

### 8.3b Current minirack provisioning (fact-check context for 8.1)

`apps-k8s-configs` today has **no preprod `cardano-node` and no `cardano-db-sync` workload at all**, and the only
Postgres on the bjj-belts stack is a **2 Gi** PVC
(`apps/bjj-belts/base/storage/pvc-bjjbelts-postgres.yaml`, with `512Mi` memory limit on
`deployment-bjjbelts-postgres.yaml`). The ~16 GB figure in spec §8 is therefore a *projection*, not an observed
or provisioned budget: 8.1 cannot be answered by looking at the cluster, and the plan must include provisioning
the PVC and the node/db-sync workloads as new GitOps work.

### 8.4 `VoterRole` wire spelling

Hand-written as `drep` / `spo` / `constitutional_committee` (§2.6) because `CamelToSnake` yields `d_rep` / `s_p_o`.
This is the one enum where the derived value is wrong, and it appears in a query parameter, a filter, a response
field and the CSV header. It needs a round-trip test; a silent `d_rep` shipping to third parties is a breaking
change we would have to live with.

### 8.5 Not determined from documentation (two of three now RESOLVED by fact-check)

- Whether db-sync's off-chain vote fetcher achieves useful anchor coverage on preprod. A recent third-party
  measurement reports `off_chain_vote_fetch_error` holding far more rows than there are anchors, with only a
  small fraction ever validated ([dev.to write-up](https://dev.to/elliotagent/do-cardanos-governance-vote-rationales-still-resolve-measuring-it-with-db-sync-and-a-blake2b-256-1dmg)).
  **Plan step:** measure coverage during F1 check 7; if more than ~20 % of anchors sit at `not_fetched` after
  48 h, add a *supplementary* fetcher that only re-fetches those anchor ids (not a replacement for db-sync's).
- ~~The exact `off_chain_vote_fetch_error.fetch_error` text for a hash mismatch (§7.3).~~ **RESOLVED** — read off
  the source, not the DB: `"Hash mismatch when fetching metadata from <url>. Expected \"<hex>\" but got \"<hex>\"."`
  and `"<prefix>Size error, fetching metadata from <url> exceeded 512 bytes."`, from
  [`Cardano/DbSync/Types.hs` @13.7.2.1 `instance Show OffChainFetchError`](https://github.com/IntersectMBO/cardano-db-sync/blob/13.7.2.1/cardano-db-sync/src/Cardano/DbSync/Types.hs).
  §7.3 now uses anchored `LIKE`s against these. The fixture test stays, as a regression pin.
- ~~Whether `drep_distr` rows exist for the two reserved DReps on preprod.~~ **RESOLVED: yes.**
  `insertDrepDistr` maps **every** key of the ledger's `psDRepDistr` through `insertDrep`, which has explicit
  `DRepAlwaysAbstain -> DB.insertDrepHashAlwaysAbstain` / `DRepAlwaysNoConfidence -> …` arms, and
  `isActiveEpochNo` returns `Nothing` for both — i.e. a `drep_distr` row **with `active_until = NULL`**
  ([`GovAction.hs:361-405` @13.7.2.1](https://github.com/IntersectMBO/cardano-db-sync/blob/13.7.2.1/cardano-db-sync/src/Cardano/DbSync/Era/Universal/Insert/GovAction.hs)).
  §3.1's always-abstain headline reads straight out of `drep_distr`, and no `delegation_vote` fallback is needed.
  Consequence for §2.5: the `active_until`-based `inactive` test must treat `NULL` as "not applicable", not as
  "expired" — the `reserved` branch has to be evaluated first, as it already is.

---

## Fact-check corrections

Adversarial fact-check against primary sources: cardano-db-sync **tag 13.7.2.1** (`doc/schema.md`,
`doc/configuration.md`, `cardano-db/src/Cardano/Db/Types.hs`,
`cardano-db-sync/src/Cardano/DbSync/{Types.hs,Config/Types.hs,Util/Bech32.hs,Era/Universal/Insert/{Certificate,GovAction}.hs}`),
the published `swagger-typescript-api@13.13.0` tarball, GitHub issues, and the owner's three repos.

### Corrected (the design changed)

| # | Was | Now | Source |
|---|---|---|---|
| C-1 | `drep_hash.view` called "CIP-129 bech32", with a `drep_script1…` variant | `drep_hash.view` is **CIP-105** (`serialiseToBech32 "drep"` over the raw 28-byte hash, HRP `drep` for key *and* script). No `drep_script1…` exists. CIP-129 ids are **computed by us**; `id_cip105` added | `Util/Bech32.hs`, `GovAction.hs:368-375` |
| C-2 | `ChainPoint {slot :: Integer, tx_hash :: Text}`, cursor `<slot>:<tx_index>:<item_index>` | `slot`/`tx_hash` are `Maybe`; epoch-boundary events (ratified/enacted/expired/dropped/treasury-enacted) have **no slot and no tx in db-sync** — cursor gets `sort_slot` = first slot of epoch, `tx_index = -1`, `item_index = gov_action_proposal.id`, stored as projection columns | `schema.md` `gov_action_proposal` (epoch columns only) |
| C-3 | `VoteRecord.voter_id` for CC from `committee_hash.raw` per `committee_voter`; filter doc said "CC cold hash" | `committee_voter` is the **hot** key; `committee_member.committee_hash_id` is the **cold** credential. `vote_projection` must store both and resolve via `committee_registration`. `committee_hash` has no `view` column | `schema.md`; `GovAction.hs:170-179` |
| C-4 | `AnchorRef.fetched_at` | renamed `failed_at`; `off_chain_vote_data` has **no timestamp column**, only `off_chain_vote_fetch_error.fetch_time` | `schema.md` `off_chain_vote_data` |
| C-5 | `oversized` cut as "indistinguishable" | restored: `OCFErrDataTooLong` → `"… Size error, fetching metadata from <url> exceeded 512 bytes."` | `Types.hs` `Show OffChainFetchError` |
| C-6 | `voting_power :: Integer`, `voting_power_epoch :: Int`, `registered_at :: ChainPoint`, `age_days :: Int` | all four `Maybe` — reserved DReps have no `drep_registration` row ever; a DRep registered this epoch has no `drep_distr` row yet. `order_by=voting_power` → `NULLS LAST` | `GovAction.hs:361-405` |
| C-7 | `epoch_param.committee_max_size` | **no such column**. Conway has `committee_min_size` + `committee_max_term_length` only | `schema.md` `epoch_param` |
| C-8 | `pool_thresholds = pvt_* by name`; `threshold = dvt_*/pvt_* for the action type` | explicit column list incl. **`pvtpp_security_group`** (no `pvt_` prefix); full (role, type) → column table added, with `threshold_param` echoed in the DTO, `null` where no threshold exists, and `_normal` always chosen because `epoch_state.no_confidence_id` is documented "TODO: This remains NULL" | `schema.md` `epoch_param`, `epoch_state` |
| C-9 | CSV `limit` 10000 via `Get '[JSON, CSV]` | servant never hands the negotiated content type to the handler — endpoints carry `Header "Accept" Text` and `maxLimitFor`. CSV dropped from `/drep/{id}/power` (its own max is 200) | servant `Verb` semantics; internal §1.2 vs §1.9 conflict |
| C-10 | `/spos` takes `q` | `q` dropped: SPO rows are unprojected db-sync reads (§7.4) with `ticker` null (§8.2), so §4.3's rule leaves nothing to match | internal contradiction |
| C-11 | CI guard `cabal run query-api -- --write-swagger-only` presented as house pattern; file `query-api.json` | flag **does not exist** (Belt System `query-api/Main.hs:36` writes unconditionally at startup, then serves); it is a named task. House filename is `query-swagger-api.json` | `src/exe/query-api/Main.hs:36` |
| C-12 | §1.5 "historical committees reachable through the `UpdateCommittee` gov actions" | the db string is `NewCommittee` (as §2.6 itself says) | `Cardano/Db/Types.hs:456-487` |
| C-13 | §7.3 `ILIKE '%hash mismatch%'`, flagged unverifiable | exact strings sourced; anchored `LIKE 'Hash mismatch%'` (the mismatch arm is the only one without the `"Error Offchain Voting Anchor: "` prefix) | `Types.hs` `Show OffChainFetchError` |
| C-14 | `ipfs_gateway` not mentioned | top-level config key, list, default `["https://ipfs.io/ipfs"]`; HTTPS required and private IPs blocked in 13.7.x — both bear directly on §8.5 anchor coverage | `Config/Types.hs:456`; `doc/configuration.md` |
| C-15 | open question: `drep_distr` rows for reserved DReps | **resolved: yes**, with `active_until = NULL` | `GovAction.hs:379-405` |

### Verified correct (no change; checked because they are load-bearing)

- `preset` values are exactly `full` / `only_utxo` / `only_governance` / `disable_all`; "Preset is an aggregate
  setting that overrides all other properties" is a verbatim quote; `only_governance` keeps ledger + governance +
  pool_stat and **disables shelley**; `offchain_vote_data` defaults to `disable` since 13.7.0.1. The §7.1
  spelled-out `insert_options` block uses valid keys and valid value shapes throughout. (`doc/configuration.md`)
- `shelley: enable` really is required for correct delegator counts:
  `ConwayUnRegCert cred _dep -> when (ioShelley iopts) $ insertStakeDeregistration …` (`Certificate.hs:136-138`),
  while `insertDelegationVote` is gated on `ioGov` (`Certificate.hs:145-155`). The note's cut of our own metadata
  fetcher rests on a real db-sync capability, and schema.md states verbatim that only hash-matching content is
  stored in `off_chain_vote_data` with mismatches routed to `off_chain_vote_fetch_error`, and documents the
  `is_valid` tri-state exactly as §2.4/§7.3 describe it.
- Every other db-sync column named in §7.2 exists at 13.7.2.1: `pool_stat.{voting_power,number_of_delegators}`,
  `drep_distr.{hash_id,amount,epoch_no,active_until}`, `gov_action_proposal.{index,expiration,deposit,return_address,
  prev_gov_action_proposal,param_proposal,ratified/enacted/dropped/expired_epoch,type,description}`,
  `voting_procedure.{index,voter_role,drep_voter,pool_voter,committee_voter,vote,voting_anchor_id}`,
  `epoch_state.committee_id`, `committee.quorum_{numerator,denominator}`, `committee_member.{committee_id,
  committee_hash_id,expiration_epoch}`, `committee_{registration,de_registration}`, `treasury_withdrawal.*`,
  `voting_anchor.{url,data_hash,type}`, `off_chain_vote_drep_data.*`, `off_chain_vote_gov_action_data.*`,
  `off_chain_vote_reference.{label,uri}`, `off_chain_vote_fetch_error.{fetch_error,fetch_time,retry_count}`,
  `pool_hash.view`, `stake_address.view`, `block.{slot_no,epoch_no,time}`, `tx.{hash,block_index}`,
  `stake_deregistration.*`. The `govactiontype` DB string **is** `NewCommittee` (Haskell constructor
  `NewCommitteeType`), `voterrole` is `ConstitutionalCommittee | DRep | SPO`, and
  `hardcodedAlwaysAbstain = "drep_always_abstain"` / `hardcodedAlwaysNoConfidence = "drep_always_no_confidence"`
  are at `Cardano/Db/Types.hs:569-573`.
- db-sync #1951: the live-vs-active statement is a **comment by kderme (2025-03-13)** — "Currently dbsync doesn't
  track the live voting power, only the active, which we receive from the upstream ledger code" — not the issue
  body; the issue is closed `not_planned`. The conclusion in §0.8 stands. yaci-store #1017 is indeed
  "[Governance] SPO AlwaysNoConfidence non-voters counted as No/Yes instead of Abstain in bootstrap phase",
  closed completed, so §0.7's cut of a ratification predictor is well founded.
- `swagger-typescript-api@13.13.0` is the current npm version, README line 3 is "Support for OpenAPI 3.0, 2.0, JSON
  and YAML", and `--no-client` / `-p` / `-o` / `-n` are all real in the shipped `dist/cli.mjs`.
  `openapi-typescript` did drop Swagger 2.0 in v6.
- Every Belt System path and identifier §2–§6 reuses exists: `WebAPI/{Utils,Errors,CORS,ServiceProbe}.hs`,
  `Query/{Common,Projected,Pages,Live,ServiceStatus}.hs`, `DomainTypes/Transfer/{QueryResponses,Filters,OrderBy}.hs`,
  `escapeLikePattern` (verbatim as quoted, right-to-left order as described, and the property test's
  `escapeLikePattern "\\%" == "\\\\\\%"` is arithmetically right), `likePat` (`Query/Projected.hs:92`),
  `applyLimits` (`Query/Common.hs:33`), `normalizeLimitOffset` with `max 1 (min 500 l)` (`Query/Common.hs:23`),
  `mkServantErr :: Int -> String -> ServerError` writing a bare `BL8.pack msg` body with no content type
  (`WebAPI/Errors.hs:17-18`), `corsExposedHeaders` already set but to `simpleHeaders <> [hAuthorization]` only
  (`WebAPI/CORS.hs:58` — the §6 gotcha is real), `alwaysHealthy version service` argument order
  (`ServiceProbe.hs:46-59`), `info . version .~ "3.3.0"` (`RestAPI.hs:658`), the `/protocol-status`, `/lineage`,
  `/search`, `/activity` and `/pages` routes, `ActivityEventType` with `ConstructorTagModifier '[StripPrefix "Evt"]`
  (`OrderBy.hs:262-272`, PascalCase wire as §2.9 claims), and F-17 as described in `FableReview-0407.md:122-124`.
  `file-embed` is already a dependency (`.cabal:511`); `cassava` is genuinely new.

### Still open after the fact-check

- §8.1 (Postgres size) remains the one blocker, now with 8.3b: the cluster has no db-sync workload and a 2 Gi
  Postgres PVC today, so the 16 GB budget is unprovisioned as well as unmeasured.
- §8.4 (`VoterRole` wire spelling) stands as written; `deriving-aeson`'s `Rename` is the right tool.
- §8.5's anchor-coverage measurement stands, now with the `ipfs_gateway` / HTTPS-only facts (C-14) as the first
  things to check before concluding db-sync's fetcher is inadequate.
