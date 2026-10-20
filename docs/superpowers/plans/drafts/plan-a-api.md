# Plan A — block: the public Query API (synthesis §3 Phase 3, items 18–19)

This block delivers the read surface of F1 check 12 and the API half of check 16: the governance DTOs, the
`palmares` schema bootstrap and matview refresh thread inside `query-api`, the ten governance route families over
the `palmares.*` views, the four page composites a named screen renders, `/health` · `/ready` · `/status` with the
freshness contract, `/metrics/formulas`, CSV content negotiation, and `query-api --write-swagger-only` with its CI
parity guard. It implements D-1 (no projection tables — every row comes from a view or the one matview), D-7
(`PALMARES_READY_MAX_LAG_SECONDS=300` + `PALMARES_AMBER_LAG_SECONDS=120`, `/ready` keeps the `drep_distr_epoch` and
`network_name` checks), D-9 (`/metrics/formulas` from the embedded catalog), D-10 (the anchor verdict is consumed
from `palmares.anchor`, never reclassified in Haskell), D-11 (`cassava` + `Palmares.WebAPI.CSV` with the
formula-injection guard, `Header "Accept"` on the list endpoints), D-17 (`age_days :: Maybe Int` everywhere),
D-18 (`pageTotal :: Maybe Int`, `null` on `/activity`), D-24 (both `voter_hot_hash` and `voter_cold_hash` on CC
votes, `voter_id` = the cold CIP-129 form), D-25 (the static (role, type) → threshold-column table as a named
deliverable with its own test), and §1.4's seven-value anchor enum plus `cip100_valid :: Maybe Bool`.
**Assumptions.** (a) Tasks 14–17 of synthesis §3 are done: roles + grants exist, `sql/000`–`sql/030` are written,
the metric catalog and the epoch-320 fixture exist, and db-sync has completed a from-genesis preprod sync — every
endpoint test here asserts against that real preprod data at **epoch 320**. (b) `Palmares.DomainTypes.Core.Ids`
(Phase 1 step 7) exists. (c) Attestation-sourced fields are Plan B's: `DRepDetailResponse` gets `profile` /
`attestations` / `disputes`, `StatusResponse` gets `attestations` / `protocol`, `ProposerSummary` gets
`profile_id`, `/ready` gets its Kupo-lag check, `SearchResults` gets its `profiles` / `attestations` groups, and
CSV gets its `/attestations` + `/disputes` instances when Plan B lands — each is a new optional field or route, i.e.
a **minor** Swagger bump by the §5 SemVer rule, so nothing here is rewritten. In Plan A those groups serve empty
and those fields are absent. (d) Two disambiguations against the binding synthesis: `q` is cut **only on `/spos`**
(cut list, C-10) and stays on `/dreps`, `/votes`, `/gov-actions` and `/search`, without which the task's own
"activity & search" family cannot exist; and `Query/Refresh.hs` is listed in §3 step 18, so it is here (T-API-5) —
if another block also claims it, drop it there, not here.

---

### Task T-API-1: Governance ids, chain time, and the wire enums

**Files:**
- Modify: `src/lib/offchain-lib/Palmares/DomainTypes/Core/Types.hs` (append a `-- * Governance ids and enums` section)
- Create: `src/test/UnitTests/GovDto.hs`
- Modify: `src/test/UnitTests.hs` (add `govDtoTests` to the tree)
- Modify: `palmares.cabal` (`other-modules`: `UnitTests.GovDto`)

**Interfaces:**
- Consumes: `Palmares.DomainTypes.Core.Ids.{drepIdCip129, drepIdCip105, govActionIdCip129, parseDrepIdAny}`; `Utils.mkStripPrefixSchemaOptions`
- Produces: `DRepId`, `GovActionId`, `ChainPoint(..)`, `DRepStatus`, `DRepUpdateKind`, `GovActionType`, `GovActionState`, `VoterRole`, `Vote`, `TallyBasis`, `AnchorStatus`, `ActivitySource`, `ActivityEventType`, `SearchKind`, `CommitteeMemberStatus`, `instance FromHttpApiData DRepId`, `instance FromHttpApiData GovActionId`, `govDtoTests`

- [ ] **Step 1: Write the failing test**

```haskell
-- src/test/UnitTests/GovDto.hs
module UnitTests.GovDto (govDtoTests) where

import Data.Aeson (decode, encode, toJSON)
import Data.Time (UTCTime)
import Palmares.DomainTypes.Core.Types
import Servant (parseUrlPiece)
import Test.Tasty
import Test.Tasty.HUnit

-- Bitcows, the §4 fixture DRep: CIP-129 id, CIP-105 view, raw 28-byte hex.
bitcowsCip129, bitcowsCip105, bitcowsHex :: Text
bitcowsCip129 = "drep1y2qn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qmwj3xg"
bitcowsCip105 = "drep1sym3575f3k68c0qfj39e7flthhpf3367t9he3wjqzwc4qcdx550"
bitcowsHex    = "81378d3ea89b68f0fc09944b1f93f2feb851318f196f98ba4029d8a8"

govDtoTests :: TestTree
govDtoTests =
  testGroup
    "governance DTO wire format"
    [ testCase "tx-borne ChainPoint" $
        encode (ChainPoint (Just 84233900) 215 (read "2026-10-19 08:14:02 UTC" :: UTCTime) (Just "a1b2"))
          @?= "{\"slot\":84233900,\"epoch\":215,\"time\":\"2026-10-19T08:14:02Z\",\"tx_hash\":\"a1b2\"}",
      testCase "epoch-boundary ChainPoint has null slot and null tx_hash" $
        encode (ChainPoint Nothing 215 (read "2026-10-19 00:00:00 UTC" :: UTCTime) Nothing)
          @?= "{\"slot\":null,\"epoch\":215,\"time\":\"2026-10-19T00:00:00Z\",\"tx_hash\":null}",
      -- VoterRole is the one enum where CamelToSnake is WRONG (d_rep / s_p_o): §8.4.
      testCase "VoterRole wire values are hand-written" $
        map toJSON [DRep, SPO, ConstitutionalCommittee]
          @?= ["drep", "spo", "constitutional_committee"],
      testCase "VoterRole round-trips" $
        mapM (decode . encode) [DRep, SPO, ConstitutionalCommittee]
          @?= Just [DRep, SPO, ConstitutionalCommittee],
      testCase "GovActionType uses db-sync's NewCommittee, snake_cased" $
        map toJSON [ParameterChange, HardForkInitiation, TreasuryWithdrawals, NoConfidence, NewCommittee, NewConstitution, InfoAction]
          @?= [ "parameter_change", "hard_fork_initiation", "treasury_withdrawals"
              , "no_confidence", "new_committee", "new_constitution", "info_action" ],
      testCase "AnchorStatus has seven values" $
        map toJSON [AnchorValid, AnchorInvalidSchema, AnchorHashMismatch, AnchorOversized, AnchorUnreachable, AnchorInline, AnchorNotFetched]
          @?= [ "valid", "invalid_schema", "hash_mismatch", "oversized"
              , "unreachable", "inline", "not_fetched" ],
      testCase "ActivityEventType is snake_case, not the Belt System's PascalCase" $
        map toJSON [EvtDrepRegistered, EvtVoteCast, EvtGovActionRatified, EvtTreasuryWithdrawalEnacted]
          @?= ["drep_registered", "vote_cast", "gov_action_ratified", "treasury_withdrawal_enacted"],
      testCase "DRepStatus / DRepUpdateKind / GovActionState / Vote / TallyBasis" $
        ( map toJSON [DRepActive, DRepInactive, DRepRetired, DRepReserved]
        , map toJSON [UpdateKindRegistered, UpdateKindUpdated, UpdateKindRetired]
        , map toJSON [Proposed, Ratified, Enacted, Expired, Dropped]
        , map toJSON [Yes, No, Abstain]
        , toJSON CastVotesOnly )
          @?= ( ["active", "inactive", "retired", "reserved"]
              , ["registered", "updated", "retired"]
              , ["proposed", "ratified", "enacted", "expired", "dropped"]
              , ["yes", "no", "abstain"]
              , "cast_votes_only" ),
      testCase "capture accepts CIP-129, CIP-105, raw hex and the two reserved views" $
        traverse (parseUrlPiece @DRepId)
          [bitcowsCip129, bitcowsCip105, bitcowsHex, "drep_always_abstain", "drep_always_no_confidence"]
          @?= Right (map DRepId [bitcowsCip129, bitcowsCip129, bitcowsCip129, "drep_always_abstain", "drep_always_no_confidence"]),
      testCase "capture normalises to CIP-129 and rejects junk" $
        parseUrlPiece @DRepId "pool1abc" @?= Left "invalid_drep_id",
      testCase "GovActionId capture accepts both forms, canonical is tx#index" $
        traverse (parseUrlPiece @GovActionId)
          [ "04f2a1b3c4d5e6f708192a3b4c5d6e7f8091a2b3c4d5e6f708192a3b4c5d6e7f#0"
          , "gov_action1qnergwfc" ]
          @?= Right [ GovActionId "04f2a1b3c4d5e6f708192a3b4c5d6e7f8091a2b3c4d5e6f708192a3b4c5d6e7f#0"
                    , GovActionId "04f2a1b3c4d5e6f708192a3b4c5d6e7f8091a2b3c4d5e6f708192a3b4c5d6e7f#0" ]
    ]
```

**(OPEN: `bitcowsCip105` above is a placeholder — the fixture gives the CIP-129 id and the raw hex but not
`drep_hash.view`; read the real CIP-105 string out of `drep_hash.view` for that credential once db-sync is synced,
and the `gov_action1qnergwfc` vector is truncated in f1-dbsync §1.3 — take the full bech32 from the same query.)**

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "governance DTO wire format"'`
Expected: FAIL with `Module 'Palmares.DomainTypes.Core.Types' does not export 'ChainPoint'`

- [ ] **Step 3: Implement the ids, `ChainPoint` and the enums in `src/lib/offchain-lib/Palmares/DomainTypes/Core/Types.hs`**

Newtypes (`DRepId`, `GovActionId`) derive `ToJSON`/`FromJSON` newtype-style; `ChainPoint` and every enum
use the house `CustomJSON` + `mkStripPrefixSchemaOptions` pair. `VoterRole` alone takes
`ConstructorTagModifier '[Rename "DRep" "drep", Rename "SPO" "spo", Rename "ConstitutionalCommittee" "constitutional_committee"]`,
and the same three strings are spelled again in its `FromHttpApiData` instance and its `ToParamSchema` `enum_`.
`FromHttpApiData DRepId` is `parseDrepIdAny` + `drepIdCip129` for the three credential forms, a pass-through for the
two reserved views, and `Left "invalid_drep_id"` otherwise; `FromHttpApiData GovActionId` splits on `#` or decodes
the CIP-129 bech32 and always renders `<tx_hash_hex>#<index>`.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-options='-p "governance DTO wire format"'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/lib/offchain-lib/Palmares/DomainTypes/Core/Types.hs src/test/UnitTests/GovDto.hs src/test/UnitTests.hs palmares.cabal
git commit -m "feat(api): governance ids, ChainPoint and wire enums with golden JSON"
```

---

### Task T-API-2: Page envelope, `ApiError`, and the JSON error body

**Files:**
- Create: `src/lib/webapi-lib/Palmares/WebAPI/Envelope.hs` (`Cursor`, `Page`, `ApiError`, `ErrorCode`, `errorCodeToHttpStatus`)
- Modify: `src/lib/webapi-lib/Palmares/WebAPI/Errors.hs` (rewrite `mkServantErr`, make `genericErrorMessage` total over `ErrorCode`)
- Modify: `palmares.cabal` (`webapi-lib`: `exposed-modules += Palmares.WebAPI.Envelope`, `build-depends += deriving-aeson`)
- Modify: `src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs` (re-export the five names above)
- Modify: `src/test/UnitTests/GovDto.hs`

**Interfaces:**
- Consumes: nothing
- Produces: `Cursor`, `Page(..)`, `ApiError(..)`, `ErrorCode(..)`, `errorCodeToHttpStatus :: ErrorCode -> Int`, `mkServantErr :: Int -> ErrorCode -> ServerError`, `genericErrorMessage :: ErrorCode -> Text`

Why these four types are in `webapi-lib` and not in `QueryResponses`, where §2.2/§2.3 drew them: `webapi-lib` has
**no project-library dependency** (house layering, restated by D-11) and `offchain-lib` does not depend on
`webapi-lib` either, so a type used by both `mkServantErr` (`ApiError`, `ErrorCode`) and
`MimeRender CSV (Page a)` (`Page`, and `Cursor` through `pageNextCursor`) can only live in `webapi-lib` —
otherwise T-API-4's instance is an orphan declared in neither type's nor the class's library. `QueryResponses`
re-exports them so every DTO consumer still imports one module. `deriving-aeson` is new to `webapi-lib`'s
`build-depends` (it already carries `aeson`, `swagger2`, `text`); nothing else moves.

- [ ] **Step 1: Write the failing test**

```haskell
-- appended to src/test/UnitTests/GovDto.hs's list
      testCase "Page echoes applied limit/offset; total is nullable (D-18)" $
        encode (Page [1 :: Int, 2] (Just 3) 10 0 Nothing)
          @?= "{\"items\":[1,2],\"total\":3,\"limit\":10,\"offset\":0,\"next_cursor\":null}",
      testCase "Page on /activity carries total = null, never -1" $
        encode (Page [1 :: Int] Nothing 20 0 (Just (Cursor "84233900:12:3")))
          @?= "{\"items\":[1],\"total\":null,\"limit\":20,\"offset\":0,\"next_cursor\":\"84233900:12:3\"}",
      testCase "ApiError is {code,message}" $
        encode (ApiError DrepNotFound "No DRep with that identifier")
          @?= "{\"code\":\"drep_not_found\",\"message\":\"No DRep with that identifier\"}",
      testCase "every ErrorCode maps to its documented status" $
        map errorCodeToHttpStatus
          [ InvalidParameter, QueryTooShort, QueryTooLong, OffsetTooLarge, LimitTooLarge, InvalidCursor
          , BothCursorsGiven, InvalidDrepId, InvalidGovActionId, InvalidAssetClass, InvalidTimeRange
          , InvalidEpochRange, DrepNotFound, GovActionNotFound, SpoNotFound, CommitteeNotFound
          , ProfileNotFound, AttestationNotFound, DisputeNotFound, RateLimited
          , NotReady, IndexerBehind, DbUnavailable, InternalError ]
          @?= replicate 12 400 <> replicate 7 404 <> [429] <> replicate 3 503 <> [500],
      testCase "mkServantErr sets a JSON content type and an ApiError body" $
        let e = mkServantErr 503 IndexerBehind
         in (errHTTPCode e, lookup hContentType (errHeaders e), errBody e)
              @?= (503, Just "application/json", encode (ApiError IndexerBehind (genericErrorMessage IndexerBehind))),
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "governance DTO wire format"'`
Expected: FAIL with `Variable not in scope: errorCodeToHttpStatus`

- [ ] **Step 3: Implement `Cursor`, `Page`, `ApiError`, `ErrorCode`, `errorCodeToHttpStatus`, `mkServantErr`**

`mkServantErr status code` keeps the existing "non-4xx is not collapsed to 400" base-selection `case`, replaces
`errBody` with `encode (ApiError code (genericErrorMessage code))`, and sets
`errHeaders = [(hContentType, "application/json")]`. `instance ToSchema (Page a)` needs
`(ToSchema a, Typeable a)`; because `mkStripPrefixSchemaOptions` lives in `offchain-lib` and is unreachable from
here, `Envelope.hs` writes the same four lines inline:

```haskell
pageSchemaOptions :: SchemaOptions   -- the mkStripPrefixSchemaOptions "page" body, inlined (unreachable library)
pageSchemaOptions =
  fromAesonOptions defaultOptions {fieldLabelModifier = camelTo2 '_' . drop (length "page")}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-options='-p "governance DTO wire format"'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/lib/webapi-lib/Palmares/WebAPI/Envelope.hs src/lib/webapi-lib/Palmares/WebAPI/Errors.hs src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs src/test/UnitTests/GovDto.hs palmares.cabal
git commit -m "feat(api): Page envelope and {code,message} error body with an ErrorCode status map"
```

---

### Task T-API-3: `Query.Common` — bounds, stable ordering, LIKE patterns

**Files:**
- Modify: `src/exe/query-api/Query/Common.hs`
- Create: `src/test/UnitTests/QueryHardening.hs`
- Modify: `src/test/UnitTests.hs`, `palmares.cabal`

**Interfaces:**
- Consumes: `ErrorCode` (T-API-2), `SortOrder` (`Palmares.DomainTypes.Transfer.OrderBy`), `Palmares.WebAPI.Utils.escapeLikePattern`
- Produces: `Limit`, `Offset`, `normalizeLimitOffset :: Int -> Maybe Int -> Maybe Int -> Either ErrorCode (Limit, Offset)`, `maxLimitFor :: Maybe Text -> Int`, `activityMaxLimit`, `likePat :: Text -> Text`, `SortSpec(..)`, `orderClause :: SortSpec -> SortOrder -> Text`, `normalizeQ :: Maybe Text -> Either ErrorCode (Maybe Text)`, `checkRanges`

The Belt System's `applyLimits` / `applyFilterOrderLimit` are **not ported** (D-1/§4.1 rule 2: a handler that slices
a Haskell list is the F-17 bug; the helper that enables it is absent so it cannot be reached).

- [ ] **Step 1: Write the failing test**

```haskell
-- src/test/UnitTests/QueryHardening.hs
module UnitTests.QueryHardening (queryHardeningTests) where

import Data.Text qualified as T
import Palmares.DomainTypes.Transfer.OrderBy (SortOrder (..))
import Palmares.WebAPI.Utils (escapeLikePattern)
import Query.Common
import Test.Tasty
import Test.Tasty.HUnit

queryHardeningTests :: TestTree
queryHardeningTests =
  testGroup
    "query hardening"
    [ testCase "limit clamps silently to [1, maxLimit] and offset to >= 0" $
        map (\(m, l, o) -> normalizeLimitOffset m l o)
          [(200, Just 999, Just (-5)), (200, Just 0, Nothing), (200, Nothing, Just 10)]
          @?= [Right (200, 0), Right (1, 0), Right (50, 10)],
      testCase "offset past the hard ceiling is rejected, never clamped" $
        normalizeLimitOffset 200 (Just 50) (Just 10001) @?= Left OffsetTooLarge,
      testCase "maxLimitFor reads the Accept header: CSV 10000, otherwise 200" $
        map maxLimitFor [Just "text/csv", Just "text/csv;charset=utf-8", Just "application/json", Nothing]
          @?= [10000, 10000, 200, 200],
      testCase "/activity ceiling is 100 with default 20" $
        (activityMaxLimit, normalizeLimitOffset activityMaxLimit Nothing Nothing)
          @?= (100, Right (20, 0)),
      -- §4.4: equal keys otherwise reshuffle between pages and rows get skipped.
      testCase "orderClause always appends the ASC tiebreaker and NULLS LAST" $
        ( orderClause (SortSpec "m.voting_power" True "d.drep_hash_id") Desc
        , orderClause (SortSpec "d.given_name" False "d.drep_hash_id") Asc )
          @?= ( "ORDER BY m.voting_power DESC NULLS LAST, d.drep_hash_id ASC"
              , "ORDER BY d.given_name ASC, d.drep_hash_id ASC" ),
      testCase "likePat lowercases, escapes, and wraps in %" $
        likePat "100%_x\\" @?= "%100\\%\\_x\\\\%",
      -- the composition order in escapeLikePattern is load-bearing (right-to-left)
      testCase "a literal backslash survives as exactly one escaped backslash" $
        escapeLikePattern "\\%" @?= "\\\\\\%",
      testCase "q is trimmed then bounded to [2,100]" $
        map normalizeQ [Just "  ab  ", Just " a ", Just (T.replicate 101 "x"), Nothing]
          @?= [Right (Just "ab"), Left QueryTooShort, Left QueryTooLong, Right Nothing],
      testCase "inverted ranges are 400s, not empty pages" $
        ( checkRanges (Just t2) (Just t1) Nothing Nothing
        , checkRanges Nothing Nothing (Just 320) (Just 300) )
          @?= (Left InvalidTimeRange, Left InvalidEpochRange)
    ]
  where
    t1 = read "2026-01-01 00:00:00 UTC"; t2 = read "2026-02-01 00:00:00 UTC"
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "query hardening"'`
Expected: FAIL with `Variable not in scope: maxLimitFor`

- [ ] **Step 3: Implement the helpers in `src/exe/query-api/Query/Common.hs`**

`normalizeLimitOffset maxLimit` takes the ceiling as its first argument so one function serves JSON (200), CSV
(10000) and `/activity` (100); the default is 50 except on `/activity`, whose handler passes `activityMaxLimit` and
its own default 20. `type Limit = Int` and `type Offset = Int` — synonyms, not newtypes, so the literals in the
test above are the values. `orderClause` takes `SortSpec { sortColumn, sortNullsLast, sortTiebreaker }` where
`sortColumn`/`sortTiebreaker` come only from a closed per-endpoint `*OrderBy` → `SortSpec` total function, so no
user text ever reaches the clause.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-options='-p "query hardening"'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Common.hs src/test/UnitTests/QueryHardening.hs src/test/UnitTests.hs palmares.cabal
git commit -m "feat(api): bounded pagination, stable ordering and escaped LIKE patterns in Query.Common"
```

---

### Task T-API-4: `Palmares.WebAPI.CSV` with the formula-injection guard

**Files:**
- Create: `src/lib/webapi-lib/Palmares/WebAPI/CSV.hs`
- Modify: `palmares.cabal` (`webapi-lib`: `exposed-modules += Palmares.WebAPI.CSV`, `build-depends += cassava`)
- Modify: `src/test/UnitTests/QueryHardening.hs`

**Interfaces:**
- Consumes: `Palmares.WebAPI.Envelope.Page(..)` (T-API-2) — same library, so the `MimeRender` instance below is not an orphan
- Produces: `data CSV`, `instance Accept CSV`, `instance (ToNamedRecord a, DefaultOrdered a) => MimeRender CSV (Page a)`, `safeCell :: Text -> Text`

- [ ] **Step 1: Write the failing test**

```haskell
-- appended to src/test/UnitTests/QueryHardening.hs
csvTests :: TestTree
csvTests =
  testGroup
    "CSV export"
    [ testCase "content type is text/csv;charset=utf-8" $
        contentType (Proxy @CSV) @?= "text/csv;charset=utf-8",
      -- exported governance data lands in spreadsheets: this is a trust boundary, not a nicety
      testCase "formula-injection guard prefixes the five dangerous leads" $
        map safeCell ["=1+1", "+1", "-1", "@SUM(A1)", "\t=x", "\r=x", "plain", "-", ""]
          @?= ["'=1+1", "'+1", "'-1", "'@SUM(A1)", "'\t=x", "'\r=x", "plain", "'-", ""],
      testCase "a cell containing a comma, a quote and a newline survives a round trip" $
        decodeByName (mimeRender (Proxy @CSV) (Page [Row "a,b\"c\nd"] (Just 1) 1 0 Nothing))
          @?= Right (header ["cell"], V.fromList [Row "a,b\"c\nd"]),
      testCase "the envelope is dropped: CSV is rows only, with one header line" $
        mimeRender (Proxy @CSV) (Page [Row "x", Row "y"] (Just 2) 2 0 Nothing)
          @?= "cell\r\nx\r\ny\r\n"
    ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "CSV export"'`
Expected: FAIL with `Could not find module 'Palmares.WebAPI.CSV'`

- [ ] **Step 3: Implement `Palmares.WebAPI.CSV` in `src/lib/webapi-lib/Palmares/WebAPI/CSV.hs`**

`MimeRender CSV (Page a) = encodeDefaultOrderedByName . pageItems` — the envelope has no CSV meaning and the
applied limit is already echoed in the JSON path. `safeCell` prefixes a single `'` when the first character is one
of `= + - @ \t \r`, and the per-DTO `ToNamedRecord` instances pass **every** `Text` cell through it.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-options='-p "CSV export"'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/lib/webapi-lib/Palmares/WebAPI/CSV.hs src/test/UnitTests/QueryHardening.hs palmares.cabal
git commit -m "feat(webapi): CSV content type via cassava with a formula-injection guard"
```

---

### Task T-API-5: `Query.Views` boot DDL (fail-closed) and `Query.Refresh`

**Files:**
- Create: `src/exe/query-api/Query/Views.hs`, `src/exe/query-api/Query/Refresh.hs`
- Modify: `src/exe/query-api/Main.hs`, `palmares.cabal` (`other-modules += Query.Refresh`; `Query.Views` is already in the extraction manifest's `other-modules`; add `extra-source-files: sql/*.sql`)
- Modify: `.env.example`
- Test: `scripts/smoke_views.sh`

**Interfaces:**
- Consumes: `sql/000_schema.sql` … `sql/030_metrics.sql`; roles `palmares_owner` / `palmares_ro`; `palmares.drep_metrics`
- Produces: `applyViewDdl :: ConnectionPool -> IO ()`, `startMetricsRefresh :: ConnectionPool -> Int -> IO ThreadId`, env vars `PALMARES_PG_OWNER_CONN_STR`, `PALMARES_VIEW_REFRESH_SECONDS` (default 300)

**(OPEN: query-api's runtime role is `palmares_ro` (USAGE+SELECT only) but D-15 has query-api apply the DDL and
refresh the matview as `palmares_owner`, so it needs a second pool on a second DSN. Decided by the owner, who took
the D-15 role-separation decision: either (a) `PALMARES_PG_OWNER_CONN_STR` as planned here — two pools, the `_ro`
one serving every request — or (b) `GRANT palmares_owner TO palmares_ro` and one DSN, which hands the public read
path DDL rights and is the reason (a) is written. Build (a) unless told otherwise; switching to (b) deletes one env
var and one pool.)**
**(OPEN: `REFRESH MATERIALIZED VIEW CONCURRENTLY` requires a UNIQUE index on the matview and no note specifies one.
`sql/030_metrics.sql` (synthesis §3 step 16) must create `palmares_drep_metrics_pk UNIQUE` on `drep_hash_id`, or
this task's refresh thread fails at runtime, not at boot. Owner: the step-16 task; this task only consumes it.)**

- [ ] **Step 1: Write the failing test** — a true unit test is impossible (the DDL only means anything against a
synced db-sync), so the check is an explicit script with exact observable output. It needs only the binary to
boot, not any route, so it runs before T-API-6 rewrites `RestAPI.hs`.

```bash
#!/usr/bin/env bash
# scripts/smoke_views.sh — query-api owns the `palmares` schema lifecycle. Run against the preprod stack.
set -euo pipefail
PSQL="psql ${PALMARES_PG_OWNER_CONN_STR} -At -c"

# 1. a boot recreates the whole schema: 17 views + 1 matview + 1 function (synthesis §1.2's table)
$PSQL "SELECT count(*) FROM pg_views WHERE schemaname='palmares'"            | grep -qx 17
$PSQL "SELECT count(*) FROM pg_matviews WHERE schemaname='palmares'"         | grep -qx 1
$PSQL "SELECT count(*) FROM pg_proc p JOIN pg_namespace n ON n.oid=p.pronamespace
       WHERE n.nspname='palmares' AND p.proname='eligible_actions'"          | grep -qx 1

# 2. the matview is really refreshed on a tick. Measured: REFRESH rewrites every row, so
#    pg_stat_all_tables.n_tup_ins for drep_metrics strictly increases across one interval.
ins() { $PSQL "SELECT n_tup_ins FROM pg_stat_all_tables WHERE schemaname='palmares' AND relname='drep_metrics'"; }
before=$(ins)
$PSQL "SELECT pg_sleep(${PALMARES_VIEW_REFRESH_SECONDS:-300} + 30)" >/dev/null
after=$(ins)
[ "$after" -gt "$before" ] || { echo "FAIL: no refresh in one interval ($before -> $after)"; exit 1; }

# 3. fail-closed: a broken DDL file must stop the boot, not serve a half-built schema
sed -i.bak '1i\
SELECT 1/0;' sql/020_anchors.sql
# the boot must die; `timeout` is what distinguishes "died" from "served forever"
timeout 120 cabal run query-api 2>&1 | tail -5 | grep -q "palmares schema DDL failed" \
  || { mv sql/020_anchors.sql.bak sql/020_anchors.sql; echo "FAIL: boot did not die"; exit 1; }
mv sql/020_anchors.sql.bak sql/020_anchors.sql
echo "views OK ($before -> $after rows inserted)"
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bash scripts/smoke_views.sh`
Expected: FAIL at check 1 — `pg_views` count is `0` (not `17`), because nothing has ever created schema `palmares`

- [ ] **Step 3: Implement `applyViewDdl` and `startMetricsRefresh`**

`applyViewDdl` embeds the four files one `embedFile` at a time (`makeRelativeToProject "sql/000_schema.sql" >>= embedFile`,
never `embedDir` — D-9's reason applies identically: `embedDir` would not invalidate the module when a file is
added), applies them in lexical order inside **one** `runSqlPool` transaction on the owner pool, and on
`SomeException` calls `die "palmares schema DDL failed: …"` (Stream C fail-closed doctrine). `startMetricsRefresh`
forks a thread that loops `runSqlPoolNoTransaction` + `rawExecute "REFRESH MATERIALIZED VIEW CONCURRENTLY palmares.drep_metrics" []`
every `PALMARES_VIEW_REFRESH_SECONDS` — `CONCURRENTLY` cannot run inside a transaction block, which is why it is
not `runSqlPool`; a failed refresh logs and retries on the next tick rather than killing the server, because stale
metrics are a lag condition `/status` already reports.

- [ ] **Step 4: Run test to verify it passes**

Run: `bash scripts/smoke_views.sh`
Expected: PASS — `views OK (294 -> 588 rows inserted)` (one 294-row rewrite per refresh)

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Views.hs src/exe/query-api/Query/Refresh.hs src/exe/query-api/Main.hs scripts/smoke_views.sh .env.example palmares.cabal
git commit -m "feat(query-api): apply sql/*.sql at boot in one transaction and refresh drep_metrics on a tick"
```

---

### Task T-API-6: `Query.Gov` plumbing, the probes, `/status` and the freshness contract

**Files:**
- Create: `src/exe/query-api/Query/Gov.hs`, `scripts/smoke_query_api.sh`
- Modify: `src/exe/query-api/RestAPI.hs` (the extraction's **rewrite** target, not a new file — it already exists and compiles after the green gate), `src/exe/query-api/QueryAppMonad.hs`, `src/exe/query-api/Query/ServiceStatus.hs`, `src/lib/webapi-lib/Palmares/WebAPI/CORS.hs`, `src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs`, `src/exe/query-api/Main.hs`, `.env.example`

**Interfaces:**
- Consumes: `palmares.sync_state` (`block_no`, `slot_no`, `epoch_no`, `tip_hash`, `tip_time`, `lag_seconds`, `drep_distr_epoch`, `network_name`), `palmares.thresholds`, `Palmares.WebAPI.ServiceProbe.{alwaysHealthy, ServiceProbe}`, `verifyProjectionDbConnection`
- Produces: `queryJson :: FromJSON a => Text -> [PersistValue] -> SqlPersistT IO [a]`, `queryJson1`, `runGovRead` (one `RepeatableRead` transaction per request), `SyncStatus(..)`, `StatusResponse(..)`, `EpochParamsSummary(..)`, `Freshness(..)`, `freshnessMiddleware :: IO Freshness -> Middleware`, `getStatusQuery`, `getReadyProbe`, `FullAPI`, `mkApp`, env vars `PALMARES_READY_MAX_LAG_SECONDS` (300), `PALMARES_AMBER_LAG_SECONDS` (120), `PALMARES_FRESHNESS_POLL_MS` (5000), `PALMARES_NETWORK_NAME` (`preprod`)

**(OPEN: T-API §6 spells the poll interval `FRESHNESS_POLL_MS`; T-EXTRACT §5.1 requires the `PALMARES_` prefix on
every new env var, so it is `PALMARES_FRESHNESS_POLL_MS` here. Same for `PALMARES_NETWORK_NAME`, which no note
names — `/ready` check 4 needs an expected value to compare `meta.network_name` against, and `StatusResponse.network`
echoes it.)**

Two plumbing rules every later family inherits, both pinned by this task's tests. (1) Every governance read goes
through `runGovRead`, which is `runSqlPoolWithIsolation … RepeatableRead` — db-sync declares `-- noreference` on the
governance FKs, so a composite read across several statements can observe dangling ids while a rollback commits.
(2) Every `SELECT` projects exactly one JSON column built with `row_to_json` / `json_build_object`, whose keys are
the DTO's snake_case wire names, and `queryJson` decodes it with the DTO's own `FromJSON`; a renamed or dropped
view column then fails loudly at the boundary naming the field instead of silently shifting a positional decode.
Every timestamp inside those objects is wrapped `AT TIME ZONE 'UTC'` so it renders with a `+00:00` offset that
`FromJSON UTCTime` accepts — `block.time` is `timestamp` without a zone. (3) That cast is owned **here**, in
`Query/Gov.hs`'s projection list, never in the views: `palmares.sync_state` ships `tip_time` as the bare
`timestamp` T-DATA §5.1 wrote, and only `lag_seconds` is pre-cast there.

- [ ] **Step 1: Write the failing test**

```haskell
-- appended to src/test/UnitTests/GovDto.hs: the decode boundary, pure and cheap
      testCase "queryJson's row shape decodes into SyncStatus" $
        eitherDecode "{\"epoch\":320,\"slot\":84233900,\"block_time\":\"2026-10-19T08:14:02+00:00\",\
                      \\"lag_seconds\":42,\"is_ready\":true,\"source\":\"cardano-db-sync\"}"
          @?= Right (SyncStatus 320 84233900 (read "2026-10-19 08:14:02 UTC") 42 True "cardano-db-sync"),
      testCase "readiness verdict: all four checks, 300 s red" $
        map readyVerdict
          [ ReadyInputs True  42  320 320 "preprod" "preprod"
          , ReadyInputs True 301  320 320 "preprod" "preprod"
          , ReadyInputs True  42  320 318 "preprod" "preprod"   -- drep_distr_epoch < epoch_no - 1
          , ReadyInputs True  42  320 319 "preprod" "preprod"   -- epoch_no - 1 is still ready
          , ReadyInputs True  42  320 320 "mainnet" "preprod"
          , ReadyInputs False 42  320 320 "preprod" "preprod" ]
          @?= [Nothing, Just IndexerBehind, Just IndexerBehind, Nothing, Just NotReady, Just DbUnavailable],
      testCase "amber is 120 s and is a UI band, never the probe" $
        (readyMaxLagDefault, amberLagDefault) @?= (300, 120),
```

```bash
# scripts/smoke_query_api.sh — F1 check 12's smoke half; every later family appends a section.
set -euo pipefail
API="${PALMARES_API:-http://localhost:8083}"
fail=0
j() { curl -fsS -H 'Accept: application/json' "$API$1" | jq -c "$2"; }          # path, filter; 2xx only
# `|| true`: without it `set -e` aborts the whole run on the first failing assertion instead of recording it
assert() { got=$(j "$1" "$2" || true); [ "$got" = "$3" ] || { echo "FAIL $1 $2 => $got (want $3)"; fail=1; }; }
code() { curl -s -o /dev/null -w '%{http_code}' "$API$1"; }
# error bodies need a NON-failing curl: `-f` discards the body of a 4xx, so `assert` can never see `.code`.
jerr() { curl -sS -H 'Accept: application/json' "$API$1" | jq -c "$2"; }
assert_err() { got=$(jerr "$1" "$2" || true); [ "$got" = "$3" ] || { echo "FAIL $1 $2 => $got (want $3)"; fail=1; }; }

# --- probes, status, freshness (T-API-6) ---
assert /health '.status' '"healthy"'
assert /health '.service' '"query-api"'
[ "$(code /ready)" = 200 ] || { echo "FAIL /ready not 200"; fail=1; }
assert /status '.network' '"preprod"'
assert /status '.governance.source' '"cardano-db-sync"'
assert /status '.governance.is_ready' 'true'
assert /status '.governance.epoch >= 320' 'true'
assert /status '.governance.lag_seconds < 120' 'true'          # spec check A1, from /status not the probe
assert /status '.epoch_params.gov_action_lifetime' '7'          # preprod; mainnet is 6
assert /status '.epoch_params.pool_thresholds | map(.[0]) | sort' \
  '["pvt_committee_no_confidence","pvt_committee_normal","pvt_hard_fork_initiation","pvt_motion_no_confidence","pvtpp_security_group"]'
assert /status '.epoch_params.drep_thresholds | length' '10'
curl -fsS -D- -o /dev/null "$API/status" | grep -qi '^x-chain-epoch:'          || { echo "FAIL no X-Chain-Epoch"; fail=1; }
curl -fsS -D- -o /dev/null "$API/status" | grep -qi '^x-data-lag-seconds:'     || { echo "FAIL no X-Data-Lag-Seconds"; fail=1; }
curl -fsS -D- -o /dev/null -H 'Origin: https://gov.cardano.vip' "$API/status" \
  | grep -i '^access-control-expose-headers:' | grep -qi 'x-chain-epoch'       || { echo "FAIL header not exposed to the browser"; fail=1; }
exit $fail
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "governance DTO"' && bash scripts/smoke_query_api.sh`
Expected: FAIL — `Variable not in scope: readyVerdict`, then `FAIL /health` (connection refused: there is no `RestAPI.hs`)

- [ ] **Step 3: Implement the plumbing, the three routes and the freshness middleware**

`RestAPI.hs` is created with
`type FullAPI = SwaggerSchemaUI "swagger-ui" "swagger-api.json" :<|> ServiceProbeAPI :<|> CoreFunctionAPI`, no
`BasicAuth` layer and no `WebAPI.Auth` import (query-api is public; rate limiting is the BFF's job), and
`CoreFunctionAPI = StatusAPI` for now — each later family appends one alternative to the type and one to the
server. `/health` is `alwaysHealthy (pack appVersion) "query-api"` and touches no state. `/ready` reads the single
`palmares.sync_state` row and applies `readyVerdict`, returning `mkServantErr 503` on `Just`. One background
thread polls `palmares.sync_state` every `PALMARES_FRESHNESS_POLL_MS` into a `TVar Freshness`; the same `TVar`
feeds `/ready`, `/status` and `freshnessMiddleware`, so the service has exactly one definition of "how fresh are
we" and the hot path adds no query. `mkCorsMiddleware` gains
`corsExposedHeaders = Just ["X-Chain-Epoch","X-Chain-Slot","X-Chain-Block-Time","X-Data-Lag-Seconds","X-Data-Source"]`
— without it `fetch()` cannot read any of them and the freshness chip silently shows nothing.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-options='-p "governance DTO"' && bash scripts/smoke_query_api.sh`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Gov.hs src/exe/query-api/RestAPI.hs src/exe/query-api/QueryAppMonad.hs src/exe/query-api/Query/ServiceStatus.hs src/exe/query-api/Main.hs src/lib/webapi-lib/Palmares/WebAPI/CORS.hs src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs src/test/UnitTests/GovDto.hs scripts/smoke_query_api.sh .env.example
git commit -m "feat(query-api): /health, /ready and /status over palmares.sync_state with freshness headers"
```

---

### Task T-API-7: `/metrics/formulas` from the embedded catalog

**Files:**
- Create: `src/exe/query-api/Metrics/Catalog.hs`
- Modify: `src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs` (`MetricFormula`), `src/exe/query-api/RestAPI.hs`, `palmares.cabal`
- Test: `src/test/UnitTests/MetricsCatalog.hs`

**Files (cont.):**
- Modify: `palmares.cabal` (`query-api`: `other-modules += Metrics.Catalog`; `extra-source-files: resources/metrics/*.sql`, `resources/metric-formulas.json`)

**Interfaces:**
- Consumes: `resources/metric-formulas.json`, `resources/metrics/*.sql` (step 16)
- Produces: `MetricFormula(..)`, `catalog :: [MetricFormula]`, `metricSql :: [(FilePath, ByteString)]`, `GET /metrics/formulas`

- [ ] **Step 1: Write the failing test**

```haskell
-- src/test/UnitTests/MetricsCatalog.hs
metricsCatalogTests :: TestTree
metricsCatalogTests =
  testGroup
    "metric catalog"
    [ -- the six SQL-backed formulas are T-METRIC §7's file list; the two below them are read off
      -- columns and carry no sql_file, but a DTO publishes their formula_id so they MUST be in the catalog
      testCase "every published formula is present, by id" $
        sort (map formulaFormulaId catalog)
          @?= sort [ "participation_rate_v1", "rationale_rate_v1", "age_days_v1"
                   , "delegator_retention_rate_v1", "vote_latency_median_hours_v1", "votes_by_type_v1"
                   , "gov_action_state_v1", "tally_cast_votes_v1" ],
      -- D-9: the SQL the code runs IS the SQL the endpoint publishes
      testCase "exactly six entries carry a sql_file, and each resolves and is inlined" $
        ( length [f | f <- catalog, formulaSqlFile f /= Nothing]
        , all (\f -> maybe True (\s -> lookup s metricSql /= Nothing && formulaSql f /= Just "") (formulaSqlFile f)) catalog )
          @?= (6, True),
      testCase "participation_rate_v1 publishes its denominator and null_when" $
        ( formulaField p, formulaExpression p, formulaNullWhen p, formulaVersion p )
          @?= ( "participation_rate", "|actions_voted| / |actions_eligible|", "eligible_actions = 0", 1 ),
      -- the metric note's own drift rule: every formula_id a DTO publishes exists here
      testCase "no sql_file names a file missing from the embedded set" $
        [s | f <- catalog, Just s <- [formulaSqlFile f], lookup s metricSql == Nothing] @?= []
    ]
  where p = head [f | f <- catalog, formulaFormulaId f == "participation_rate_v1"]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "metric catalog"'`
Expected: FAIL with `Could not find module 'Metrics.Catalog'`

- [ ] **Step 3: Implement `Metrics.Catalog` and the handler**

`catalogJson = $(makeRelativeToProject "resources/metric-formulas.json" >>= embedFile)` and one `embedFile` line
per `.sql` (never `embedDir`: it would not invalidate the module when a `.sql` is added, which defeats the
anti-drift point). `catalog` decodes the JSON once and resolves each entry's `sql` from `metricSql` by `sql_file`,
`error`ing at startup on an unresolvable reference. `MetricFormula`'s fields are the catalog entry's keys verbatim
(`formula_id`, `name`, `field`, `expression`, `denominator`, `null_when`, `inputs`, `edge_cases`, `sql_file`,
`spec`, `version`) plus `sql`, with `sql_file :: Maybe FilePath` and `sql :: Maybe Text` — null together on the
two column-read formulas.

**(OPEN: `resources/metric-formulas.json` is written by synthesis §3 step 16 and T-METRIC §7 enumerates only the
six SQL-backed entries, but `GovActionLifecycle.formula_id = "gov_action_state_v1"` (T-API-11) and
`VoteTally.formula_id = "tally_cast_votes_v1"` (T-API-12) are published to clients, and T-METRIC §7's drift rule
requires every DTO-referenced id to exist in the catalog. Those two entries — with `sql_file: null` — must be
added by the step-16 task; the owner of the metrics block decides whether they instead get their own `.sql` and
become eight files. Either way the id list above is the contract.)**

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-options='-p "metric catalog"'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Metrics/Catalog.hs src/test/UnitTests/MetricsCatalog.hs src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs src/exe/query-api/RestAPI.hs palmares.cabal
git commit -m "feat(api): GET /metrics/formulas served from the embedded catalog with SQL inlined"
```

---

### Task T-API-8: `GET /dreps` — the delegation-discovery list

**Files:**
- Modify: `src/exe/query-api/Query/Gov.hs`, `src/exe/query-api/RestAPI.hs`, `src/lib/offchain-lib/Palmares/DomainTypes/Transfer/{QueryResponses,Filters,OrderBy}.hs`, `scripts/smoke_query_api.sh`
- Test: `src/test/UnitTests/QueryHardening.hs` (the `DRepsOrderBy` → `SortSpec` table)

**Interfaces:**
- Consumes: `palmares.drep`, `palmares.drep_metrics`, `palmares.anchor`, `palmares.drep_epoch_power`, `palmares_idx_ocvdd_given_name_trgm`
- Produces: `DRepSummary(..)`, `DRepFilter(..)`, `drepFilterFromParams`, `DRepsOrderBy(..)`, `drepSortSpec`, `listDReps :: DRepFilter -> (DRepsOrderBy, SortOrder) -> (Limit, Offset) -> GovRead (Page DRepSummary)`, `instance ToNamedRecord DRepSummary`, `GET /dreps`

Query parameters, bounds and defaults are §1.2's table verbatim: `limit` 50 / [1,200] (CSV [1,10000]), `offset` 0 /
[0,10000] then 400 `offset_too_large`, repeatable `status` · `anchor_status`, `has_script`,
`min_voting_power` / `max_voting_power` (lovelace), `min_delegators`, `min_participation_rate` /
`min_rationale_rate` ([0,1]), `registered_before` / `registered_after`, `min_age_days`, `voted_on` /
`not_voted_on`, `language` (≤16), `active_in_epoch`, `q` ([2,100]), `order_by` default `voting_power`,
`sort_order` default **`desc`** (every DRep list is a leaderboard). `order_by=voting_power` sorts `NULLS LAST` in
both directions — a DRep registered this epoch has no `drep_distr` row and its power is unknown, not `0`.
Repeatable parameters are OR within a parameter and AND across parameters; the empty list is no filter, never
"match nothing", documented once in the Swagger top-level description. `q` matches
`off_chain_vote_drep_data.given_name` through `likePat` only (D-1 put the trigram GIN indexes on db-sync's columns,
so there is no projection table to copy text into).

- [ ] **Step 1: Write the failing test**

```bash
# --- /dreps (T-API-8) --- preprod, epoch 320
assert '/dreps?limit=1' '.total'  '294'                      # registered DReps
assert '/dreps?limit=1' '.limit'  '1'
assert '/dreps?limit=999' '.limit' '200'                     # clamped silently, applied value echoed
[ "$(code '/dreps?offset=10001')" = 400 ] || { echo "FAIL offset ceiling"; fail=1; }
assert_err '/dreps?offset=10001' '.code' '"offset_too_large"'
# the two pseudo-DReps are never in the list (D-20): they have no drep_registration row
assert '/dreps?limit=200' '[.items[].id] | map(select(startswith("drep_always"))) | length' '0'
# default sort is voting_power desc NULLS LAST
assert '/dreps?limit=3' '[.items[].voting_power] | . == (. | sort | reverse)' 'true'
assert '/dreps?order_by=voting_power&sort_order=asc&limit=200' '[.items[-1].voting_power] | .[0] == null' 'true'
# the fixture DRep, by filter
assert '/dreps?q=Bitcows' '.items[0].id' '"drep1y2qn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qmwj3xg"'
assert '/dreps?q=Bitcows' '.items[0] | [.votes_cast,.eligible_actions,.delegator_count,.voting_power_epoch,.active_until_epoch]' \
  '[16,114,13,320,333]'
assert '/dreps?q=Bitcows' '.items[0] | [.participation_rate,.rationale_rate] | map(.*1000000|round)' '[140351,125000]'
assert '/dreps?q=Bitcows' '.items[0].voting_power' '3923709956713'
assert '/dreps?q=Bitcows' '.items[0].age_days | . >= 731 and . <= 733' 'true'
# a filter that matches few rows: total must come from the byte-identical WHERE (F-17)
assert '/dreps?status=retired&limit=5' '(.items | length) <= 5 and (.total >= (.items|length))' 'true'
assert '/dreps?min_participation_rate=1.1' '[.total, (.items|length)]' '[0,0]'
# age_days is null exactly when registered_at is null (D-17) — vacuously true only if no such row is listed
assert '/dreps?limit=200' '[.items[] | select(.registered_at == null) | .age_days] | unique | . == [] or . == [null]' 'true'
assert '/dreps?limit=200' '[.items[] | select(.registered_at != null and .age_days == null)] | length' '0'
# CSV: same filters, its own ceiling, injection-guarded
curl -fsS -H 'Accept: text/csv' "$API/dreps?limit=10000" | head -1 | grep -q '^id,hex,has_script' || { echo "FAIL csv header"; fail=1; }
[ "$(curl -fsS -H 'Accept: text/csv' "$API/dreps?limit=10000" | wc -l)" -ge 295 ] || { echo "FAIL csv rows"; fail=1; }
```

```haskell
-- src/test/UnitTests/QueryHardening.hs: the sort table is a closed total function, not user text
      testCase "every DRepsOrderBy maps to a whitelisted column with a tiebreaker" $
        map (sortColumn . drepSortSpec)
          [ DRepsOrderById, DRepsOrderByVotingPower, DRepsOrderByDelegatorCount
          , DRepsOrderByParticipationRate, DRepsOrderByRationaleRate, DRepsOrderByRegisteredAt
          , DRepsOrderByVotesCast, DRepsOrderByName ]
          @?= [ "d.drep_hash_id", "m.voting_power", "m.delegator_count", "m.participation_rate"
              , "m.rationale_rate", "d.registered_slot", "m.votes_cast", "a.given_name" ],
      testCase "voting_power sorts NULLS LAST in both directions" $
        map (\o -> orderClause (drepSortSpec DRepsOrderByVotingPower) o)
          [Asc, Desc]
          @?= [ "ORDER BY m.voting_power ASC NULLS LAST, d.drep_hash_id ASC"
              , "ORDER BY m.voting_power DESC NULLS LAST, d.drep_hash_id ASC" ],
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bash scripts/smoke_query_api.sh`
Expected: FAIL with `FAIL /dreps?limit=1 .total => (curl: HTTP 404)`

- [ ] **Step 3: Implement `listDReps` and wire `GET /dreps`**

One predicate function builds the `WHERE` fragment and its `[PersistValue]`, and both the page `SELECT` and the
`COUNT(*)` take it — the page and the total cannot drift (F-17's root-cause fix). The route type carries
`Header "Accept" Text` before `Get '[JSON, CSV] (Page DRepSummary)`, and the handler passes that header to
`maxLimitFor`: servant resolves content negotiation inside the `Verb` server instance and never hands the
negotiated type to the handler, so this is the only way CSV gets its own ceiling.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-options='-p "query hardening"' && bash scripts/smoke_query_api.sh`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Gov.hs src/exe/query-api/RestAPI.hs src/lib/offchain-lib/Palmares/DomainTypes/Transfer/ src/test/UnitTests/QueryHardening.hs scripts/smoke_query_api.sh
git commit -m "feat(api): GET /dreps with the full filter set, CSV and a drift-proof total"
```

---

### Task T-API-9: the DRep profile — `/drep/{id}` ≡ `/pages/drep/{id}`, plus power, delegators and updates

**Files:**
- Modify: `src/exe/query-api/Query/Gov.hs`, `src/exe/query-api/Query/Pages.hs`, `src/exe/query-api/RestAPI.hs`, `src/lib/offchain-lib/Palmares/DomainTypes/Transfer/{QueryResponses,OrderBy}.hs`, `scripts/smoke_query_api.sh`

**Interfaces:**
- Consumes: `palmares.drep_metrics`, `palmares.drep_epoch_power`, `palmares.drep_epoch_delegators`, `palmares.live_delegation`, `palmares.drep_cert`, `palmares.vote`, `palmares.activity`, `off_chain_vote_drep_data`, `off_chain_vote_reference`
- Produces: `DRepMetrics(..)`, `VoteCounts(..)`, `EpochPowerPoint(..)`, `DelegatorStats(..)`, `DelegatorRow(..)`, `DRepUpdateRow(..)`, `AnchorContent(..)`, `AnchorReference(..)`, `DRepDetailResponse(..)`, `getDRepDetail`, `listEpochPower`, `listDelegators`, `listDRepUpdates`, `GET /drep/{drep-id}`, `GET /pages/drep/{drep-id}`, `GET /drep/{drep-id}/power`, `GET /drep/{drep-id}/delegators`, `GET /drep/{drep-id}/updates`

`/drep/{id}` and `/pages/drep/{id}` are **one handler on two routes** returning the same `DRepDetailResponse` —
`/pages/…` is the documented canonical path, the bare path is the alias for API users who do not want the page
framing. No second DTO. Composite params: `votes_limit` 25 [1,100], `delegators_limit` 10 [1,50],
`timeline_limit` 50 [1,200], `epochs` 200 [1,200]. Sub-route params: `/power` — `limit` 200 / max 200,
`from_epoch`, `to_epoch`, `order_by` fixed `epoch`, `sort_order` default `asc`, **no CSV** (200 epoch rows is one
JSON page) and **no `rank`** (a window function over all of `drep_distr` per epoch, on a Pi, that no F1 check
asks for); `/delegators` — `limit` 50/200, `offset`, `is_current` default `true`, `min_lovelace`, `order_by` ∈
`lovelace | delegated_at | stake_address` default `lovelace`, `sort_order` default `desc`, CSV enabled;
`/updates` — `limit` 50/200, `offset`, repeatable `kind` ∈ `registered | updated | retired`, `order_by` fixed
`at`, `sort_order` default `desc`. `kind` comes from `drep_registration.deposit`: `> 0` → `registered`,
`IS NULL` → `updated`, `< 0` → `retired`. `anchor_content` is omitted whenever `anchor.status /= valid`.

- [ ] **Step 1: Write the failing test**

```bash
# --- DRep profile (T-API-9) --- the §4 fixture DRep at epoch 320
D=drep1y2qn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qmwj3xg
assert "/drep/$D" '.drep.id' "\"$D\""
assert "/drep/$D" '.drep.hex' '"81378d3ea89b68f0fc09944b1f93f2feb851318f196f98ba4029d8a8"'
assert "/drep/$D" '.drep.has_script' 'false'
assert "/drep/$D" '.drep.status' '"active"'
# both routes, one handler, byte-identical body
diff <(j "/drep/$D" '.') <(j "/pages/drep/$D" '.') >/dev/null || { echo "FAIL alias diverges"; fail=1; }
assert "/drep/$D" '.metrics | [.vote_latency_median_hours, .delegator_retention_rate != null]' '[158.04,true]'
assert "/drep/$D" '.metrics.formula_ids | sort' \
  '["age_days_v1","delegator_retention_rate_v1","participation_rate_v1","rationale_rate_v1","vote_latency_median_hours_v1","votes_by_type_v1"]'
# per-type eligible_actions and votes_cast must sum to the headline figures (the Σ row IS the test)
assert "/drep/$D" '[.metrics.votes_by_type[] | .[1] | (.yes+.no+.abstain)] | add' '16'
assert "/drep/$D" '[.metrics.votes_by_type[] | select(.[0]=="treasury_withdrawals") | .[1]] | .[0] | [.yes,.no,.abstain]' '[1,5,0]'
assert "/drep/$D" '.power_by_epoch | length' '147'                              # drep_distr epochs 174..320, no gaps
assert "/drep/$D" '[.power_by_epoch[] | has("rank")] | unique' '[false]'       # rank is cut
assert "/drep/$D" '.power_by_epoch | map(.epoch) | . == (. | sort)' 'true'     # ascending epoch series
assert "/drep/$D" '.delegator_stats | [.count, .retention_rate != null]' '[13,true]'
assert "/drep/$D" '.top_delegators | length <= 10' 'true'
assert "/drep/$D" '.votes.total' '16'
assert "/drep/$D" '.votes.items[0].cast_at.slot != null' 'true'
assert "/drep/$D" '.anchor_content.given_name != null' 'true'                  # CIP-119 anchor added epoch 208
assert "/drep/$D" '.drep.anchor.status' '"valid"'
assert "/drep/$D?timeline_limit=200" '.timeline | map(.at.time) | . == (. | sort | reverse)' 'true'
# registration history: register at 173 (deposit 500000000) + update at 208 (deposit null)
assert "/drep/$D/updates" '[.items[] | [.kind, .deposit]] | sort' '[["registered",500000000],["updated",null]]'
assert "/drep/$D/updates" '.items[0].at.epoch' '208'
assert "/drep/$D/power?from_epoch=320&to_epoch=320" '.items' '[{"epoch":320,"lovelace":3923709956713,"delegator_count":13}]'
assert "/drep/$D/delegators?limit=50" '[.total, ([.items[] | select(.is_current)] | length)]' '[13,13]'
# the zero-denominator case (f1-metric §4): every DRep with no eligible action serves nulls, not zeros.
# The list-level form is the gate, because it does not depend on which DRep happens to be newest.
assert '/dreps?limit=200' '[.items[] | select(.eligible_actions == 0) | .participation_rate] | unique | . == [] or . == [null]' 'true'
Z=$(j '/dreps?order_by=registered_at&sort_order=desc&limit=1' '.items[0]')
if [ "$(echo "$Z" | jq -c '.eligible_actions')" = 0 ]; then
  assert "/drep/$(echo "$Z" | jq -r '.id')" \
    '[.metrics.participation_rate, .metrics.rationale_rate, .metrics.vote_latency_median_hours]' '[null,null,null]'
fi
[ "$(code /drep/drep1ynotarealdrepidatall000000000000000000000000000000)" = 404 ] || { echo "FAIL 404"; fail=1; }
assert_err /drep/drep1ynotarealdrepidatall000000000000000000000000000000 '.code' '"drep_not_found"'
[ "$(code /drep/pool1abc)" = 400 ] || { echo "FAIL capture"; fail=1; }
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bash scripts/smoke_query_api.sh`
Expected: FAIL with `FAIL /drep/$D .drep.id => (curl: HTTP 404)`

- [ ] **Step 3: Implement `getDRepDetail` and the three sub-routes in `Query/Gov.hs` (composite assembly in `Query/Pages.hs`)**

`getDRepDetail` issues one `runGovRead` with the seven sub-selects, so the whole page is one `RepeatableRead`
snapshot and one round trip.

- [ ] **Step 4: Run test to verify it passes**

Run: `bash scripts/smoke_query_api.sh`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Gov.hs src/exe/query-api/Query/Pages.hs src/exe/query-api/RestAPI.hs src/lib/offchain-lib/Palmares/DomainTypes/Transfer/ scripts/smoke_query_api.sh
git commit -m "feat(api): DRep profile composite plus power, delegators and update history"
```

---

### Task T-API-10: the (role, gov-action-type) → threshold-column table

**Files:**
- Create: `src/exe/query-api/Query/Thresholds.hs`, `src/test/UnitTests/GovThresholds.hs`
- Modify: `src/test/UnitTests.hs`, `palmares.cabal`

**Interfaces:**
- Consumes: `GovActionType`, `VoterRole` (T-API-1); `palmares.thresholds`; `param_proposal`
- Produces: `thresholdColumnFor :: VoterRole -> GovActionType -> Maybe Text`, `paramGroupColumns :: [(Text, Text)]`, `securityGroupColumns :: [Text]`, `spoParamChangeApplies :: [Text] -> Bool`, `drepParamChangeThreshold :: [Text] -> Maybe Text`, `govThresholdsTests`

D-25: this is a named deliverable with its own test because "`dvt_*`/`pvt_*` for the action type" is not a
function. F1 always reports the `_normal` committee column — `epoch_state.no_confidence_id` is documented
"TODO: This remains NULL", so the `_no_confidence` variant cannot be chosen from data, and predicting
ratification is cut anyway (§0.7).

- [ ] **Step 1: Write the failing test**

```haskell
-- src/test/UnitTests/GovThresholds.hs
govThresholdsTests :: TestTree
govThresholdsTests =
  testGroup
    "threshold columns"
    [ testCase "DRep column per action type" $
        map (thresholdColumnFor DRep)
          [NoConfidence, NewCommittee, NewConstitution, HardForkInitiation, TreasuryWithdrawals, InfoAction]
          @?= [ Just "dvt_motion_no_confidence", Just "dvt_committee_normal"
              , Just "dvt_update_to_constitution", Just "dvt_hard_fork_initiation"
              , Just "dvt_treasury_withdrawal", Nothing ],
      testCase "SPO column per action type; two types have none" $
        map (thresholdColumnFor SPO)
          [NoConfidence, NewCommittee, NewConstitution, HardForkInitiation, TreasuryWithdrawals, InfoAction]
          @?= [ Just "pvt_motion_no_confidence", Just "pvt_committee_normal", Nothing
              , Just "pvt_hard_fork_initiation", Nothing, Nothing ],
      -- the one column a pvt_% prefix scan silently misses. §2.6: the SPO column APPLIES only when the
      -- proposal touches a security-group parameter; the table names the column, `spoParamChangeApplies`
      -- answers whether it applies to this proposal.
      testCase "the SPO security-group column is pvtpp_security_group" $
        thresholdColumnFor SPO ParameterChange @?= Just "pvtpp_security_group",
      -- driven by the table itself, so it cannot encode a guessed membership
      testCase "the SPO security-group threshold applies iff a securityGroupColumns column is touched" $
        ( spoParamChangeApplies []
        , all (spoParamChangeApplies . (: [])) securityGroupColumns
        , spoParamChangeApplies ["drep_deposit"] )          -- governance group, never security group
          @?= (False, True, False),
      testCase "the CC never has a dvt_/pvt_ threshold: it has a quorum" $
        map (thresholdColumnFor ConstitutionalCommittee) [NoConfidence, NewCommittee, InfoAction, ParameterChange]
          @?= [Nothing, Nothing, Nothing, Nothing],
      -- parameter_change: max over the groups the proposal touches
      testCase "touched param_proposal columns map to CIP-1694 groups" $
        map (`lookup` paramGroupColumns) ["max_block_size", "min_fee_a", "max_tx_ex_mem", "drep_deposit"]
          @?= [ Just "dvt_p_p_network_group", Just "dvt_p_p_economic_group"
              , Just "dvt_p_p_technical_group", Just "dvt_p_p_gov_group" ],
      testCase "the DRep parameter_change threshold is the max over touched groups" $
        ( drepParamChangeThreshold ["max_block_size", "min_fee_a"]
        , drepParamChangeThreshold [] )
          @?= (Just "greatest(dvt_p_p_network_group, dvt_p_p_economic_group)", Nothing)
    ]
```

**(OPEN: two tables are not enumerated anywhere. (a) `paramGroupColumns`: the full `param_proposal` column →
CIP-1694 group mapping; §2.6 names the four `dvt_p_p_*_group` targets and calls it "a static table we own" but no
note lists all ~30 columns. (b) `securityGroupColumns`: the CIP-1694 SecurityGroup subset, which is what makes
`pvtpp_security_group` apply at all. Both are enumerated from `param_proposal` at 13.7.1.0 against CIP-1694
§Protocol-parameter-groups, by whoever implements this task, and the `paramGroupColumns` test is made exhaustive
over `param_proposal`'s non-bookkeeping columns so an unmapped column fails CI. No owner decision is needed — CIP-1694
is the authority — but the enumeration must exist before the test above can be written.)**

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "threshold columns"'`
Expected: FAIL with `Could not find module 'Query.Thresholds'`

- [ ] **Step 3: Implement the five total functions in `src/exe/query-api/Query/Thresholds.hs`**

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-options='-p "threshold columns"'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Thresholds.hs src/test/UnitTests/GovThresholds.hs src/test/UnitTests.hs palmares.cabal
git commit -m "feat(api): static (role, action type) to epoch_param threshold column table"
```

---

### Task T-API-11: `GET /gov-actions`

**Files:**
- Modify: `src/exe/query-api/Query/Gov.hs`, `src/exe/query-api/RestAPI.hs`, `src/lib/offchain-lib/Palmares/DomainTypes/Transfer/{QueryResponses,Filters,OrderBy}.hs`, `scripts/smoke_query_api.sh`

**Interfaces:**
- Consumes: `palmares.gov_action`, `palmares.action_window`, `palmares.anchor`, `palmares.treasury_withdrawal`, `palmares_idx_ocvgad_title_trgm`
- Produces: `GovActionSummary(..)`, `GovActionLifecycle(..)`, `GovActionFilter(..)`, `govActionFilterFromParams`, `GovActionsOrderBy(..)`, `govActionSortSpec`, `listGovActions`, `instance ToNamedRecord GovActionSummary`, `GET /gov-actions`

Params: `limit` 50/200 (CSV 10000), `offset`, repeatable `type` · `state` · `anchor_status`,
`proposed_from` / `proposed_to`, `expires_before_epoch` / `expires_after_epoch`, `proposer_stake_address`,
`min_treasury_amount` / `max_treasury_amount`, `q` ([2,100], matching `off_chain_vote_gov_action_data.title` and
`.abstract`), `order_by` ∈ `proposed_at | expires_epoch | type | deposit | treasury_amount | votes_cast` default
`proposed_at`, `sort_order` default `desc`. `state` is `gov_action_state_v1` read off the four `*_epoch` columns —
`enacted → enacted; else expired → expired; else dropped → dropped; else ratified → ratified; else proposed`.
`id` is `<tx_hash_hex>#<index>`, with the CIP-129 form alongside as `bech32`.

- [ ] **Step 1: Write the failing test**

```bash
# --- /gov-actions (T-API-11) --- 131 proposals on preprod at epoch 320
assert '/gov-actions?limit=1' '.total' '131'
assert '/gov-actions?limit=1' '.items[0].id | test("^[0-9a-f]{64}#[0-9]+$")' 'true'
assert '/gov-actions?limit=1' '.items[0].bech32 | startswith("gov_action1")' 'true'
# `state` is SINGLE-VALUED (gov_action_state_v1 checks enacted, then expired, then dropped, then ratified),
# so these are NOT the per-column counts: all 11 ratified actions were enacted the next epoch, and 102 of the
# 108 rows carrying dropped_epoch also carry expired_epoch and therefore report `expired`.
assert '/gov-actions?state=enacted&limit=1'  '.total' '11'
assert '/gov-actions?state=ratified&limit=1' '.total' '0'
assert '/gov-actions?state=expired&limit=1'  '.total' '103'
assert '/gov-actions?state=dropped&limit=1'  '.total' '6'
assert '/gov-actions?state=proposed&limit=1' '.total' '12'
# the five are a partition of all 131 rows — this is the assertion that catches a mis-ordered CASE
assert '/gov-actions?limit=200' '[.items[].lifecycle.state] | length' '131'
assert '/gov-actions?limit=200' '[.items[].lifecycle.state] | group_by(.) | map(length) | add' '131'
# every type the db enum has, and the db string is NewCommittee -> new_committee
assert '/gov-actions?limit=200' '[.items[].type] | unique | sort' \
  '["hard_fork_initiation","info_action","new_committee","new_constitution","no_confidence","parameter_change","treasury_withdrawals"]'
# govActionLifetime is 7 on preprod: expiration - proposed epoch, on all 131
assert '/gov-actions?limit=200' '[.items[] | select(.expires_epoch != null) | .expires_epoch - .proposed_at.epoch] | unique' '[7]'
assert '/gov-actions?type=treasury_withdrawals&limit=1' '.items[0].treasury_amount != null' 'true'
assert '/gov-actions?order_by=deposit&sort_order=desc&limit=5' '[.items[].deposit] | . == (. | sort | reverse)' 'true'
# an enacted action carries enacted_epoch = ratified_epoch + 1 and NULL expired/dropped
assert '/gov-actions?state=enacted&limit=1' '.items[0].lifecycle | [.enacted_epoch - .ratified_epoch, .expired_epoch, .dropped_epoch]' '[1,null,null]'
assert '/gov-actions?state=enacted&limit=1' '.items[0].lifecycle.formula_id' '"gov_action_state_v1"'
# stable paging over a tied key: 103 expired rows all share state
assert '/gov-actions?order_by=type&limit=200' '[.items[].id] | (length) == (unique | length)' 'true'
curl -fsS -H 'Accept: text/csv' "$API/gov-actions?limit=10000" | head -1 | grep -q '^id,bech32,type' || { echo "FAIL csv"; fail=1; }
```

**(OPEN: the five single-valued counts above sum to 132, one more than the 131 rows that must partition. They are
derived from f1-metric-formulas §4's per-column figures (103 rows with `expired_epoch`, 11 enacted, 6 dropped-only,
12 open), so exactly one of those four is off by one — most likely `proposed` is 11, or one enacted action also
carries `expired_epoch`. Read the five counts off the synced db-sync with
`SELECT state, count(*) FROM palmares.gov_action GROUP BY 1` before this task's test is written, keep Σ = 131, and
correct f1-metric-formulas §4 in the same commit. The implementer of this task decides nothing here — the number
comes from the database.)**

- [ ] **Step 2: Run test to verify it fails**

Run: `bash scripts/smoke_query_api.sh`
Expected: FAIL with `FAIL /gov-actions?limit=1 .total => (curl: HTTP 404)`

- [ ] **Step 3: Implement `listGovActions` and wire `GET /gov-actions`**

- [ ] **Step 4: Run test to verify it passes**

Run: `bash scripts/smoke_query_api.sh`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Gov.hs src/exe/query-api/RestAPI.hs src/lib/offchain-lib/Palmares/DomainTypes/Transfer/ scripts/smoke_query_api.sh
git commit -m "feat(api): GET /gov-actions with lifecycle state, filters and CSV"
```

---

### Task T-API-12: the gov-action page — `/gov-action/{id}` ≡ `/pages/gov-action/{id}` with tallies

**Files:**
- Modify: `src/exe/query-api/Query/Gov.hs`, `src/exe/query-api/Query/Pages.hs`, `src/exe/query-api/RestAPI.hs`, `src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs`, `scripts/smoke_query_api.sh`

**Interfaces:**
- Consumes: `palmares.vote_tally`, `palmares.vote`, `palmares.thresholds`, `palmares.treasury_withdrawal`, `palmares.gov_action`, `palmares.activity`, `param_proposal`; `Query.Thresholds` (T-API-10)
- Produces: `VoteTally(..)`, `GovActionAnchorContent(..)`, `ProposerSummary(..)`, `GovActionDetailResponse(..)`, `getGovActionDetail`, `GET /gov-action/{gov-action-id}`, `GET /pages/gov-action/{gov-action-id}`

One handler, two routes, as in T-API-9. Params: `votes_limit` 50 [1,200], `votes_order_by`, `votes_sort_order`,
`timeline_limit` 50. `tallies` carries one `VoteTally` per role **present** on the action; `basis` is always
`cast_votes_only` and its Swagger description is §2.6's verbatim sentence, because the ratio must never read as a
prediction — the authoritative outcome is `lifecycle`. Timeline entries for ratified / enacted / expired / dropped
emit an epoch-only `ChainPoint`: `epoch` = the recorded epoch, `time` = `block.time` of `MIN(block.id)` in that
epoch, `slot` and `tx_hash` `null`.

- [ ] **Step 1: Write the failing test**

```bash
# --- gov-action page (T-API-12) ---
G=$(j '/gov-actions?type=treasury_withdrawals&state=enacted&limit=1' '.items[0].id' | tr -d '"')
diff <(j "/gov-action/$G" '.') <(j "/pages/gov-action/$G" '.') >/dev/null || { echo "FAIL alias diverges"; fail=1; }
assert "/gov-action/$G" '.action.id' "\"$G\""
assert "/gov-action/$G" '[.tallies[].basis] | unique' '["cast_votes_only"]'
assert "/gov-action/$G" '[.tallies[].formula_id] | unique' '["tally_cast_votes_v1"]'
# treasury_withdrawals: DRep has a threshold column, SPO has none, CC votes by quorum
assert "/gov-action/$G" '[.tallies[] | select(.role=="drep") | .threshold_param]' '["dvt_treasury_withdrawal"]'
assert "/gov-action/$G" '[.tallies[] | select(.role=="spo") | [.threshold, .threshold_param]] | .[0] // [null,null]' '[null,null]'
assert "/gov-action/$G" '[.tallies[] | select(.role=="constitutional_committee") | .yes_power] | .[0] // 0' '0'
# ratio is yes/(yes+no) and null when the denominator is zero
assert "/gov-action/$G" '[.tallies[] | select(.yes_power + .no_power == 0) | .ratio] | unique | . == [] or . == [null]' 'true'
assert "/gov-action/$G" '.withdrawals | length >= 1' 'true'
assert "/gov-action/$G" '.withdrawals[0] | [.beneficiary | startswith("stake"), .amount > 0]' '[true,true]'
# enacted is an epoch-boundary event: no slot, no tx hash, but a real chain time
assert "/gov-action/$G" '.withdrawals[0].enacted_at | [.slot, .tx_hash, (.time != null)]' '[null,null,true]'
assert "/gov-action/$G" '[.timeline[] | select(.event_type=="gov_action_enacted") | .at.slot]' '[null]'
assert "/gov-action/$G" '.proposer | [.stake_address | startswith("stake"), .actions_count >= 1]' '[true,true]'
assert "/gov-action/$G" '.votes.total >= 0' 'true'
# a parameter_change action publishes its touched params and the max-over-groups threshold
P=$(j '/gov-actions?type=parameter_change&limit=1' '.items[0].id' | tr -d '"')
assert "/gov-action/$P" '.param_changes | length >= 1' 'true'
assert "/gov-action/$P" '[.tallies[] | select(.role=="drep") | .threshold_param] | .[0] | startswith("dvt_p_p_")' 'true'
# new_committee always reports the _normal column (epoch_state.no_confidence_id remains NULL)
C=$(j '/gov-actions?type=new_committee&limit=1' '.items[0].id' | tr -d '"')
assert "/gov-action/$C" '[.tallies[] | select(.role=="drep") | .threshold_param]' '["dvt_committee_normal"]'
[ "$(code '/gov-action/0000000000000000000000000000000000000000000000000000000000000000#0')" = 404 ] || { echo "FAIL 404"; fail=1; }
assert_err '/gov-action/0000000000000000000000000000000000000000000000000000000000000000#0' '.code' '"gov_action_not_found"'
[ "$(code /gov-action/nope)" = 400 ] || { echo "FAIL capture"; fail=1; }
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bash scripts/smoke_query_api.sh`
Expected: FAIL with `FAIL /gov-action/$G .action.id => (curl: HTTP 404)`

- [ ] **Step 3: Implement `getGovActionDetail` in `Query/Gov.hs` (composite assembly in `Query/Pages.hs`)**

`VoteTally.threshold` is read from `palmares.thresholds` at the column `thresholdColumnFor` names for the
action's epoch, and `threshold_param` echoes that column name so the number is traceable; `parameter_change`
resolves the column set through `drepParamChangeThreshold` over the non-null `param_proposal` columns.

- [ ] **Step 4: Run test to verify it passes**

Run: `bash scripts/smoke_query_api.sh`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Gov.hs src/exe/query-api/Query/Pages.hs src/exe/query-api/RestAPI.hs src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs scripts/smoke_query_api.sh
git commit -m "feat(api): gov-action page with cast-votes-only tallies, thresholds and withdrawals"
```

---

### Task T-API-13: `GET /votes` — one endpoint, four screens

**Files:**
- Modify: `src/exe/query-api/Query/Gov.hs`, `src/exe/query-api/RestAPI.hs`, `src/lib/offchain-lib/Palmares/DomainTypes/Transfer/{QueryResponses,Filters,OrderBy}.hs`, `scripts/smoke_query_api.sh`

**Interfaces:**
- Consumes: `palmares.vote` (`seq_first`, `seq_last`, `vote_rows`, `voter_hot_hash`, `voter_cold_hash`), `palmares.anchor`, `palmares.drep_epoch_power`, `pool_stat`; `Palmares.DomainTypes.Core.Ids.ccColdIdCip129`
- Produces: `VoteRecord(..)`, `VoteFilter(..)`, `voteFilterFromParams`, `VotesOrderBy(..)`, `voteSortSpec`, `listVotes`, `instance ToNamedRecord VoteRecord`, `GET /votes`

This single endpoint replaces the four per-actor vote routes (cut list). Params: `limit` 50/200 (CSV 10000),
`offset` [0,10000], repeatable `voter_role` · `voter_id` · `gov_action_id` · `verdict` · `gov_action_type` ·
`anchor_status`, `from` / `to` (chain time, inclusive), `from_epoch` / `to_epoch`, `q` ([2,100], matching rationale
title/abstract and voter name), `order_by` ∈ `cast_at | voting_power | latency | voter_id | gov_action_id` default
`cast_at`, `sort_order` default `desc`. `has_rationale` is spelled `anchor_status=valid`, not its own parameter.
D-24: for `voter_role=constitutional_committee`, `voter_id` is the **cold** CIP-129 `cc_cold1…` — the stable
identity a user can follow across hot-key rotations — with the hot hash exposed as `voter_hot_hash`, and
`voter_id=<cc_cold1…>` must match. Re-votes are real on preprod (one pair has 20 rows): the list shows every row
with `revote_count = vote_rows`, the verdict shown on a page composite is the `seq_last = 1` row, and
`latency_hours` is measured from the `seq_first = 1` row.

- [ ] **Step 1: Write the failing test**

```bash
# --- /votes (T-API-13) --- preprod totals at epoch 320
assert '/votes?voter_role=drep&limit=1' '.total' '816'
assert '/votes?voter_role=spo&limit=1'  '.total' '90'
assert '/votes?voter_role=drep&anchor_status=valid&limit=1' '.total' '65'
assert '/votes?voter_role=spo&anchor_status=valid&limit=1'  '.total' '0'
assert '/votes?voter_role=drep&limit=200' '[.items[].voter_role] | unique' '["drep"]'
assert "/votes?voter_id=$D&limit=100" '[.total, ([.items[] | select(.revote_count > 1)] | length)]' '[16,0]'
assert "/votes?voter_id=$D&limit=100" '[.items[].verdict] | group_by(.) | map([.[0], length]) | sort' \
  '[["abstain",1],["no",8],["yes",7]]'
assert "/votes?voter_id=$D&limit=100" '[.items[] | select(.anchor != null)] | length' '2'
assert "/votes?voter_id=$D&order_by=latency&sort_order=asc&limit=1" '.items[0].latency_hours | .*100|round' '129'
assert "/votes?voter_id=$D&order_by=latency&sort_order=desc&limit=1" '.items[0].latency_hours | .*100|round' '63008'
# the worst re-voted pair has 20 rows and they are all returned
assert '/votes?voter_role=drep&limit=200' '[.items[].revote_count] | max >= 1' 'true'
# CC votes are cast by hot keys but addressed by the cold id (D-24)
CC=$(j '/committee' '.members[0].cold_bech32' | tr -d '"')
assert "/votes?voter_id=$CC&limit=1" '.items[0].voter_id' "\"$CC\""
assert "/votes?voter_id=$CC&limit=1" '.items[0] | [.voter_role, (.voter_hot_hash != null)]' '["constitutional_committee",true]'
assert "/votes?voter_id=$CC&limit=1" '.total > 0' 'true'
# power is always labelled with its epoch, and both are null together
assert '/votes?voter_role=drep&limit=200' '[.items[] | select(.voting_power == null) | .voting_power_epoch] | unique | . == [] or . == [null]' 'true'
assert_err '/votes?from_epoch=321&to_epoch=320' '.code' '"invalid_epoch_range"'
curl -fsS -H 'Accept: text/csv' "$API/votes?limit=10000" | head -1 | grep -q '^gov_action_id,gov_action_type' || { echo "FAIL csv"; fail=1; }
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bash scripts/smoke_query_api.sh`
Expected: FAIL with `FAIL /votes?voter_role=drep&limit=1 .total => (curl: HTTP 404)`

- [ ] **Step 3: Implement `listVotes` and wire `GET /votes`**

`voter_id` is matched against `voter_cold_hash` for CC rows and the credential hash otherwise, so one parameter
serves all three roles.

- [ ] **Step 4: Run test to verify it passes**

Run: `bash scripts/smoke_query_api.sh`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Gov.hs src/exe/query-api/RestAPI.hs src/lib/offchain-lib/Palmares/DomainTypes/Transfer/ scripts/smoke_query_api.sh
git commit -m "feat(api): GET /votes across all three roles with cold-key CC identity"
```

---

### Task T-API-14: delegations, committee, SPOs, treasury withdrawals

**Files:**
- Modify: `src/exe/query-api/Query/Gov.hs`, `src/exe/query-api/RestAPI.hs`, `src/lib/offchain-lib/Palmares/DomainTypes/Transfer/{QueryResponses,Filters,OrderBy}.hs`, `scripts/smoke_query_api.sh`

**Interfaces:**
- Consumes: `palmares.vote_delegation`, `palmares.live_delegation`, `palmares.committee_member`, `palmares.treasury_withdrawal` (incl. `paid_heuristic`), `pool_stat`, `pool_hash`; `ccColdIdCip129` / `ccHotIdCip129`
- Produces: `DelegationRecord(..)`, `SPOSummary(..)`, `CommitteeMemberSummary(..)`, `CommitteeResponse(..)`, `TreasuryWithdrawalRecord(..)`, `listDelegations`, `getCommittee`, `listSPOs`, `listTreasuryWithdrawals`, four `ToNamedRecord` instances, `GET /delegations`, `GET /committee`, `GET /spos`, `GET /treasury-withdrawals`

Params, §1.5 verbatim. `/delegations`: `limit` `offset` `drep_id[]` `stake_address[]` `is_current` (default
`true`) `from` `to` `min_lovelace`, `order_by` ∈ `at | lovelace | stake_address` default `at`, `sort_order` `desc`.
`/committee`: no parameters, no pagination — it returns the **current** committee (`epoch_state.committee_id` for
the latest epoch, `DISTINCT`/max-`id` because a rollback across an epoch boundary can leave duplicate
`epoch_state` rows at 13.7.1.0) plus its members inline; it *is* its own composite, and there is no `/committees`
list (cut). `/spos`: `limit` `offset` `min_voting_power` `min_votes_cast` `active_in_epoch`, `order_by` ∈
`voting_power | votes_cast | pool_id`, `sort_order` `desc`, and **no `q`** (cut list, C-10: SPO rows are a direct
`pool_stat`/`pool_hash` read and `ticker` is `null` in F1, so there is nothing to match — exact id lookup is what
`/search` is for). `/treasury-withdrawals`: `limit` `offset` `gov_action_id[]` `beneficiary[]`
`enacted_from_epoch` `enacted_to_epoch` `min_amount` `max_amount`, `order_by` ∈ `amount | enacted_epoch` default
`enacted_epoch`, `sort_order` `desc`; no `/treasury-withdrawal/{id}` (cut). All four serve CSV except `/committee`.

- [ ] **Step 1: Write the failing test**

```bash
# --- delegations, committee, SPOs, treasury (T-API-14) ---
assert '/delegations?limit=1' '.total > 0' 'true'
assert '/delegations?limit=200' '[.items[].is_current] | unique' '[true]'          # is_current defaults to true
assert '/delegations?is_current=false&limit=200' '[.items[].is_current] | unique' '[false]'   # the flag filters, both ways
assert "/delegations?drep_id=$D&limit=50" '[.items[] | select(.is_current)] | length' '13'
assert '/delegations?limit=1' '.items[0].target | startswith("drep")' 'true'
assert '/delegations?limit=1' '.items[0].stake_address | startswith("stake")' 'true'
# the two reserved targets are ordinary delegation rows pointing at raw IS NULL drep_hash rows
assert '/delegations?drep_id=drep_always_abstain&limit=1' '.total > 0' 'true'
assert '/committee' '.members | length >= 1' 'true'
assert '/committee' '[.quorum_numerator, .quorum_denominator] | map(. > 0)' '[true,true]'
assert '/committee' '.members[0].cold_bech32 | startswith("cc_cold1")' 'true'
assert '/committee' '[.members[].status] | unique | map(IN("authorized","unauthorized","resigned","expired")) | unique' '[true]'
assert '/committee' '.epoch >= 320' 'true'
assert '/spos?limit=1' '.total >= 45' 'true'
assert '/spos?limit=200' '[.items[].ticker] | unique' '[null]'                     # offchain_pool_data stays disabled
assert '/spos?limit=1' '.items[0].pool_id | startswith("pool1")' 'true'
assert '/spos?limit=1' '.items[0] | [.voting_power > 0, .voting_power_epoch >= 320]' '[true,true]'
# /spos has no `q` (cut list C-10). Servant IGNORES an undeclared query param, so the observable is
# "it does not filter", not a 400 — the Swagger parameter list is checked in T-API-18.
[ "$(code '/spos?q=abc')" = 200 ] || { echo "FAIL /spos?q must be ignored, not rejected"; fail=1; }
[ "$(j '/spos?q=abc&limit=1' '.total')" = "$(j '/spos?limit=1' '.total')" ] || { echo "FAIL q filtered /spos"; fail=1; }
assert '/treasury-withdrawals?limit=1' '.total > 0' 'true'
assert '/treasury-withdrawals?limit=1' '.items[0] | [.amount > 0, (.gov_action_title != null or .gov_action_title == null)]' '[true,true]'
assert '/treasury-withdrawals?limit=200' '[.items[] | select(.enacted_at != null) | .enacted_at.slot] | unique' '[null]'
assert '/treasury-withdrawals?order_by=amount&sort_order=desc&limit=5' '[.items[].amount] | . == (. | sort | reverse)' 'true'
for p in /delegations /spos /treasury-withdrawals; do
  curl -fsS -H 'Accept: text/csv' "$API$p?limit=10000" | head -1 | grep -q ',' || { echo "FAIL csv $p"; fail=1; }
done
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bash scripts/smoke_query_api.sh`
Expected: FAIL with `FAIL /delegations?limit=1 .total > 0 => (curl: HTTP 404)`

- [ ] **Step 3: Implement the four readers and wire their routes**

`CommitteeMemberSummary.votes_cast` counts through `palmares.vote.voter_cold_hash`, the same resolved column
`/votes` filters on, so one member's history does not split across hot-key rotations.

- [ ] **Step 4: Run test to verify it passes**

Run: `bash scripts/smoke_query_api.sh`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Gov.hs src/exe/query-api/RestAPI.hs src/lib/offchain-lib/Palmares/DomainTypes/Transfer/ scripts/smoke_query_api.sh
git commit -m "feat(api): delegations, current committee, SPO voters and treasury withdrawals"
```

---

### Task T-API-15: `GET /activity` — the cursor feed

**Files:**
- Modify: `src/exe/query-api/Query/Gov.hs`, `src/exe/query-api/RestAPI.hs`, `src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs`, `scripts/smoke_query_api.sh`
- Modify: `src/test/UnitTests/GovDto.hs` (cursor parse/render)

**Interfaces:**
- Consumes: `palmares.activity` (`sort_slot`, `tx_index`, `item_index`, `kind`, `source`, `actor_id`, `target_id`, `time`, `epoch`, `tx_hash`) — including the five epoch-boundary kinds (D-19)
- Produces: `ActivityEvent(..)`, `renderCursor`, `parseCursor`, `listActivity`, `GET /activity`

**(decision, inherited) `offset` is not accepted on `/activity`** — offset paging over a growing feed
double-counts and skips. Params: `limit` 20 [1,100], `since` (exclusive lower bound, result ordered **ascending**,
for catch-up polling), `before` (exclusive upper bound, result ordered **descending**, for infinite scroll),
repeatable `event_type` · `actor` · `source`. Neither cursor → newest-first, i.e. `before` at the tip. Both → 400
`both_cursors_given`. Cursor wire format is plain text `"<sort_slot>:<tx_index>:<item_index>"`, not base64 — it is
not secret and a debuggable cursor is worth more than an opaque one; unparseable → 400 `invalid_cursor`. For
tx-borne events the triple is `block.slot_no`, `tx.block_index` and the per-kind item index; for the five
epoch-boundary kinds it is the first slot of the recorded epoch, `-1` (sorting before every tx in that epoch) and
`gov_action_proposal.id`. `pageTotal` is `null` (D-18): a `COUNT(*)` over the merged feed on every poll is waste
and no screen shows it. `next_cursor` is the last item's cursor in the direction of travel, `null` on a short page.

- [ ] **Step 1: Write the failing test**

```haskell
-- appended to src/test/UnitTests/GovDto.hs
      testCase "cursor is plain <sort_slot>:<tx_index>:<item_index>, monotone and total" $
        ( renderCursor (84233900, 12, 3), parseCursor "84233900:12:3"
        , parseCursor "84000000:-1:4711"            -- an epoch-boundary event
        , parseCursor "nope" )
          @?= ( Cursor "84233900:12:3", Right (84233900, 12, 3)
              , Right (84000000, -1, 4711), Left InvalidCursor ),
```

```bash
# --- /activity (T-API-15) ---
assert '/activity?limit=5' '.total' 'null'                        # D-18: null, never -1
assert '/activity?limit=5' '.limit' '5'
assert '/activity?limit=200' '.limit' '100'                       # ceiling
assert '/activity?limit=5' '[.items[].cursor] | map(test("^[0-9]+:-?[0-9]+:[0-9]+$")) | unique' '[true]'
assert '/activity?limit=5' '.items | map(.at.time) | . == (. | sort | reverse)' 'true'   # newest-first by default
assert '/activity?limit=5' '.next_cursor' "$(j '/activity?limit=5' '.items[-1].cursor')"
# the five epoch-boundary kinds exist in the feed (D-19) and carry slot = null, tx_index = -1
assert '/activity?limit=100&event_type=gov_action_ratified&event_type=gov_action_enacted&event_type=gov_action_expired&event_type=gov_action_dropped&event_type=treasury_withdrawal_enacted' \
  '[.items[] | [.at.slot, (.cursor | split(":") | .[1])]] | unique' '[[null,"-1"]]'
assert '/activity?limit=100' '[.items[].source] | unique | map(IN("governance","attestations")) | unique' '[true]'
assert '/activity?limit=100' '[.items[] | select(.title == "" or .title == null)] | length' '0'  # pre-rendered server-side
# since is exclusive and ascending; the two directions tile the feed with no gap and no repeat
C1=$(j '/activity?limit=10' '.items[-1].cursor' | tr -d '"')
assert "/activity?since=$C1&limit=10" '.items | map(.at.time) | . == (. | sort)' 'true'
assert "/activity?since=$C1&limit=10" "[.items[].cursor] | map(. == \"$C1\") | any" 'false'
assert "/activity?before=$C1&limit=10" "[.items[].cursor] | map(. == \"$C1\") | any" 'false'
assert_err "/activity?since=$C1&before=$C1" '.code' '"both_cursors_given"'
assert_err '/activity?since=garbage' '.code' '"invalid_cursor"'
# `offset` is not declared on /activity, and Servant IGNORES an undeclared query param: the observable is
# "it does not page", not a 400. Its absence from the parameter list is checked in T-API-18.
[ "$(code '/activity?offset=10')" = 200 ] || { echo "FAIL /activity?offset must be ignored, not rejected"; fail=1; }
[ "$(j '/activity?offset=10&limit=5' '[.items[].cursor]')" = "$(j '/activity?limit=5' '[.items[].cursor]')" ] \
  || { echo "FAIL offset paged /activity"; fail=1; }
# a three-page walk must visit each cursor exactly once — the cursor ADVANCES each time, or this proves nothing
B=$C1; A=""
for _ in 1 2 3; do
  page=$(j "/activity?limit=10&before=$B" '[.items[].cursor]')
  A="$A$(echo "$page" | jq -r '.[]')
"
  B=$(echo "$page" | jq -r '.[-1]')
done
[ "$(printf '%s' "$A" | sort | uniq -d | wc -l)" -eq 0 ] || { echo "FAIL feed repeats across pages"; fail=1; }
[ "$(printf '%s' "$A" | grep -c . )" -eq 30 ] || { echo "FAIL feed skipped a page"; fail=1; }
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test --test-options='-p "governance DTO"' && bash scripts/smoke_query_api.sh`
Expected: FAIL with `Variable not in scope: renderCursor`

- [ ] **Step 3: Implement `listActivity`, `parseCursor` and `renderCursor`**

The `WHERE` is a lexicographic comparison on the stored triple
(`(sort_slot, tx_index, item_index) > (?, ?, ?)` for `since`, `<` for `before`), and `ORDER BY` is that same
triple in the direction of travel — which is why the cursor must be total: the comparison is the paging key, not
an approximation of the timestamp.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test --test-options='-p "governance DTO"' && bash scripts/smoke_query_api.sh`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Gov.hs src/exe/query-api/RestAPI.hs src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs src/test/UnitTests/GovDto.hs scripts/smoke_query_api.sh
git commit -m "feat(api): GET /activity with a total cursor over tx and epoch-boundary events"
```

---

### Task T-API-16: `GET /search`

**Files:**
- Modify: `src/exe/query-api/Query/Gov.hs`, `src/exe/query-api/RestAPI.hs`, `src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs`, `scripts/smoke_query_api.sh`

**Interfaces:**
- Consumes: `palmares.drep`, `palmares.gov_action`, `palmares.committee_member`, `pool_hash`, `palmares_idx_ocvdd_given_name_trgm`, `palmares_idx_ocvgad_title_trgm`; `Query.Common.likePat`
- Produces: `SearchResults(..)`, `SearchGroup(..)`, `SearchHit(..)`, `SearchTarget(..)`, `searchQuery`, `GET /search`

`q` is **required**, trimmed length [2,100] → 400 `query_too_short` / `query_too_long`. `kinds` (default all) and
`limit_per_group` 5 [1,20] — the ⌘K palette shows five per group. `SearchHit` keeps `{id, title, subtitle, badges,
status, target}` and `SearchTarget` keeps `{kind, id}` so the FE router maps `kind` to a route unchanged. In Plan A
the `profiles` and `attestations` groups serve empty (Plan B fills them). Every `%…%` match goes through `likePat`
and lands on a column covered by one of the two trigram GIN indexes; a `pool1…` or `drep1…` paste is matched by
**exact prefix**, not trigram, because an id is not text search.

- [ ] **Step 1: Write the failing test**

```bash
# --- /search (T-API-16) ---
assert '/search?q=Bitcows' '.query' '"Bitcows"'
assert '/search?q=Bitcows' '.dreps.items[0].id' "\"$D\""
assert '/search?q=Bitcows' '.dreps.items[0].target' "{\"kind\":\"drep\",\"id\":\"$D\"}"
assert '/search?q=Bitcows' '.total >= 1' 'true'
assert '/search?q=Bitcows&limit_per_group=20' '[.dreps,.gov_actions,.spos,.committee_members,.profiles,.attestations] | map(.items | length <= 20) | unique' '[true]'
assert '/search?q=Bitcows' '[.profiles.items, .attestations.items] | flatten | length' '0'   # Plan B fills these
assert "/search?q=$D" '.dreps.items[0].id' "\"$D\""                                           # exact id paste
assert_err '/search?q=a' '.code' '"query_too_short"'
assert_err "/search?q=$(printf 'x%.0s' {1..101})" '.code' '"query_too_long"'
# a LIKE metacharacter must be matched literally, not as a wildcard
assert '/search?q=100%25' '.total >= 0' 'true'
assert '/search?q=_a' '.total >= 0' 'true'
[ "$(code /search)" = 400 ] || { echo "FAIL q is required"; fail=1; }
assert '/search?q=Bitcows&kinds=drep' '[.gov_actions.items, .spos.items] | flatten | length' '0'
```

**(OPEN: `kinds` lists `stake_address` (§1.7) but `SearchResults` (§2.10) has no `stake_addresses` group — either
add the group or drop the kind. The six-group record is the one the FE's ⌘K palette renders, so dropping the kind
is the smaller change.)**

- [ ] **Step 2: Run test to verify it fails**

Run: `bash scripts/smoke_query_api.sh`
Expected: FAIL with `FAIL /search?q=Bitcows .query => (curl: HTTP 404)`

- [ ] **Step 3: Implement `searchQuery` and wire `GET /search`**

One `runGovRead` with one `SELECT` per requested kind, each `LIMIT limit_per_group`, so the palette costs one round
trip and one snapshot.

- [ ] **Step 4: Run test to verify it passes**

Run: `bash scripts/smoke_query_api.sh`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Gov.hs src/exe/query-api/RestAPI.hs src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs scripts/smoke_query_api.sh
git commit -m "feat(api): GET /search over DReps, gov actions, SPOs and committee members"
```

---

### Task T-API-17: the two remaining page composites — `/pages/drep-explorer` and `/pages/dashboard`

**Files:**
- Modify: `src/exe/query-api/Query/Pages.hs`, `src/exe/query-api/Query/Aggregates.hs`, `src/exe/query-api/RestAPI.hs`, `src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs`, `scripts/smoke_query_api.sh`

**Interfaces:**
- Consumes: `listDReps` + `drepFilterFromParams` (T-API-8), `listGovActions` (T-API-11), `listActivity` (T-API-15), `palmares.drep_epoch_power` (incl. `target_kind`), `palmares.treasury_withdrawal`, `palmares.drep_metrics`
- Produces: `DRepExplorerPageResponse(..)`, `PowerBucket(..)`, `DashboardPageResponse(..)`, `EpochParticipationPoint(..)`, `getDRepExplorerPage`, `getDashboardPage`, `GET /pages/drep-explorer`, `GET /pages/dashboard`

Only these two are left: the DRep profile and gov-action composites are their detail endpoints (T-API-9, T-API-12),
and §3.5's six candidates stay unbuilt — `/pages/gov-actions-explorer` (`/gov-actions` + `Page.total` *is* the
screen; there is no facet sidebar in the F1 design), `/pages/committee`, `/pages/treasury`, `/pages/profile/{id}`,
`/pages/activity` and `/pages/graph`. `/pages/drep-explorer` takes `GET /dreps`'s filter and sort set through the
**same** `drepFilterFromParams` (one source of truth) and adds the four facet lists the sidebar needs; facets are
computed over the filter set **with that facet's own predicate removed**, so the sidebar can show "Retired (41)"
while `status=active` is applied. `/pages/dashboard` params: `top_limit` 10 [1,50], `closing_soon_limit` 5 [1,20],
`activity_limit` 10 [1,50], `epochs` 20 [1,100]; `closing_soon` is `state = proposed` expiring within 5 epochs.
The always-abstain / always-no-confidence split reads straight off `palmares.drep_epoch_power` by `target_kind` —
the one view with no filter (D-20), which is also why `/dreps` and every rate filter must say
`target_kind = 'drep'`. Neither composite serves CSV (nobody exports a page composite).

- [ ] **Step 1: Write the failing test**

```bash
# --- composites (T-API-17) ---
assert '/pages/drep-explorer?limit=10' '[.items | length, .limit, .offset]' '[10,10,0]'
assert '/pages/drep-explorer?limit=1' '.total' '294'
# the facet keys are a subset of DRepStatus and sum to the unfiltered list total; `reserved` is 0 or absent,
# because the two pseudo-DReps have no drep_registration row and never enter /dreps (D-20)
assert '/pages/drep-explorer?limit=1' '[.status_facets[] | .[0]] | inside(["active","inactive","retired","reserved"])' 'true'
assert '/pages/drep-explorer?limit=1' '[.status_facets[] | .[1]] | add' '294'
assert '/pages/drep-explorer?limit=1' '[.status_facets[] | select(.[0]=="reserved") | .[1]] | . == [] or . == [0]' 'true'
# facet semantics: the facet's own predicate is removed, so retired is still counted under status=active
assert '/pages/drep-explorer?status=active&limit=1' '[.status_facets[] | select(.[0]=="retired") | .[1]] | .[0] > 0' 'true'
assert '/pages/drep-explorer?status=active&limit=1' '[.items[].status] | unique' '["active"]'
assert '/pages/drep-explorer?limit=1' '[.anchor_facets[] | .[0]] | map(IN("valid","invalid_schema","hash_mismatch","oversized","unreachable","inline","not_fetched")) | unique' '[true]'
assert '/pages/drep-explorer?limit=1' '.power_buckets | length >= 1' 'true'
assert '/pages/drep-explorer?limit=1' '[.power_buckets[] | .count] | add <= 294' 'true'
assert '/pages/drep-explorer?limit=1' '.epoch >= 320' 'true'
assert '/pages/dashboard' '.total_dreps' '294'
assert '/pages/dashboard' '[.active_dreps + .retired_dreps <= .total_dreps]' '[true]'
# the spec's headline fact, read off drep_epoch_power by target_kind. Epoch-319 fixture:
# always_abstain = 431863790957959, always_no_confidence = 325390800383 — bounds are within 1 % of those,
# because the dashboard reads the current epoch and the parked stake drifts between snapshots.
assert '/pages/dashboard' '.always_abstain_lovelace > 427000000000000' 'true'
assert '/pages/dashboard' '.always_no_confidence_lovelace > 322000000000' 'true'
assert '/pages/dashboard' '.actions_in_flight' '12'
assert '/pages/dashboard' '[.actions_by_type[] | .[1]] | add' '131'
assert '/pages/dashboard?epochs=20' '.participation_by_epoch | length' '20'
assert '/pages/dashboard?epochs=20' '.participation_by_epoch | map(.epoch) | . == (. | sort)' 'true'
assert '/pages/dashboard?top_limit=10' '.top_dreps_by_power | length' '10'
assert '/pages/dashboard?top_limit=10' '[.top_dreps_by_power[].id] | map(startswith("drep_always")) | any' 'false'
assert '/pages/dashboard?closing_soon_limit=5' '[.closing_soon[] | .lifecycle.state] | unique | . == [] or . == ["proposed"]' 'true'
assert '/pages/dashboard?activity_limit=10' '.recent_activity | length' '10'
assert '/pages/dashboard' '[.attestation_count, .claimed_profile_count]' '[0,0]'   # zero until Plan B
assert '/pages/dashboard' '.treasury_withdrawn_lovelace > 0' 'true'
```

- [ ] **Step 2: Run test to verify it fails**

Run: `bash scripts/smoke_query_api.sh`
Expected: FAIL with `FAIL /pages/drep-explorer?limit=10 [.items | length, .limit, .offset] => (curl: HTTP 404)`

- [ ] **Step 3: Implement `getDRepExplorerPage` and `getDashboardPage`**

Each composite is one `runGovRead`: the explorer reuses `listDReps` plus four facet `SELECT`s built from the same
predicate with one clause dropped; the dashboard's sixteen figures are one `SELECT` per group, not per row, and
`participation_by_epoch` comes from `palmares.drep_epoch_power` joined to the per-epoch vote counts.

- [ ] **Step 4: Run test to verify it passes**

Run: `bash scripts/smoke_query_api.sh`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Query/Pages.hs src/exe/query-api/Query/Aggregates.hs src/exe/query-api/RestAPI.hs src/lib/offchain-lib/Palmares/DomainTypes/Transfer/QueryResponses.hs scripts/smoke_query_api.sh
git commit -m "feat(api): DRep explorer facets and the dashboard composite"
```

---

### Task T-API-18: `query-api --write-swagger-only` and the Swagger parity guard

**Files:**
- Modify: `src/exe/query-api/Main.hs`, `src/exe/query-api/RestAPI.hs`, `.github/workflows/ci.yml`, `.gitignore`
- Create: `docs/generated/swagger/query-swagger-api.json` (generated, committed)

**Interfaces:**
- Consumes: `apiSwagger` (every route family above)
- Produces: the `--write-swagger-only` flag, the committed `docs/generated/swagger/query-swagger-api.json`, CI job `swagger-parity`

This is a real new task, not a house reuse: the Belt System writes the file **unconditionally at startup**
(`src/exe/query-api/Main.hs:36`) *after* config and DB wiring and then serves forever, which CI cannot use. The
flag must be parsed before `decodeConfigEnvOrFile`, write the file from the pure `apiSwagger` value, touch no
config and no database, and `exitSuccess`; the unconditional startup write is **removed** in the same change, so
the file has exactly one producer. Two consequences the plan must carry: `.gitignore` ignores `docs/generated/*`
(extraction §1.9), so the guard needs `!docs/generated/swagger/query-swagger-api.json` un-ignored or there is
nothing to diff; and the Swagger version is `"1.0.0"`, set in `RestAPI.hs` as
`apiSwagger & info . Data.Swagger.version .~ "1.0.0"`, independent of the app version — `cz bump` must not touch
it. Bump it by the §5 SemVer rule: removing or renaming a field or endpoint, or tightening a type → **major**; new
endpoint, new optional field, new enum value → **minor**; description or example only → **patch**. Swagger
metadata also lands here: title `"Cardano Governance Registry Query API"`, license `"Apache-2.0"`, spec at
`GET /swagger-api.json`, UI at `GET /swagger-ui`, the `ApiError` schema on `allOperations . responses . at`
400/404/429/503 with the `ErrorCode` enum in `definitions`, `addSharedSwaggerDescriptions` extended with `DRepId`,
`GovActionId`, `ChainPoint`, `Cursor`, `AnchorStatus`, `VoterRole`, `GovActionType` and `TallyBasis`, the
repeatable-parameter convention documented once in the top-level description, and the `TallyBasis` sentence from
§2.6 verbatim.

- [ ] **Step 1: Write the failing test**

```yaml
# .github/workflows/ci.yml — a new job; the guard IS the test
  swagger-parity:
    runs-on: ubuntu-24.04-arm
    steps:
      - uses: actions/checkout@v4
      - uses: haskell-actions/setup@v2
        with: { ghc-version: '9.6.6' }
      - run: cabal build exe:query-api
      # must not need Postgres, Atlas config or a port
      - run: cabal run query-api -- --write-swagger-only
      - run: git diff --exit-code docs/generated/swagger/
```

```bash
# local reproduction of the same guard
cabal run query-api -- --write-swagger-only
git diff --exit-code docs/generated/swagger/
jq -e '.info.version == "1.0.0"'                                   docs/generated/swagger/query-swagger-api.json
jq -e '.info.title == "Cardano Governance Registry Query API"'     docs/generated/swagger/query-swagger-api.json
jq -e '.info.license.name == "Apache-2.0"'                         docs/generated/swagger/query-swagger-api.json
jq -e '.definitions.ErrorCode.enum | length == 24'                 docs/generated/swagger/query-swagger-api.json
# 22 documented paths: the 18 governance routes + /health /ready /status /metrics/formulas. The two
# /pages/… aliases are paths of their own; `/swagger-api.json` and `/swagger-ui` are the UI, not CoreFunctionAPI.
jq -e '[.paths | keys[]] | length == 22'                           docs/generated/swagger/query-swagger-api.json
jq -e '.paths["/dreps"].get.parameters | map(select(.name == "Accept")) | .[0].required == false' \
                                                                   docs/generated/swagger/query-swagger-api.json
# q is cut on /spos only (cut list C-10) and kept on the other four
jq -e '[.paths["/spos"].get.parameters[].name] | index("q") == null' docs/generated/swagger/query-swagger-api.json
jq -e '[.paths["/dreps"].get.parameters[].name] | index("q") != null' docs/generated/swagger/query-swagger-api.json
# /activity takes cursors, never offset (T-API-15)
jq -e '[.paths["/activity"].get.parameters[].name] | index("offset") == null' docs/generated/swagger/query-swagger-api.json
jq -e '.definitions.VoterRole.enum == ["drep","spo","constitutional_committee"]' \
                                                                   docs/generated/swagger/query-swagger-api.json
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal run query-api -- --write-swagger-only`
Expected: FAIL — the flag is unrecognised, so the binary falls through to normal startup and dies with
`Atlas configuration failed` without writing anything

- [ ] **Step 3: Implement the flag in `src/exe/query-api/Main.hs` and the metadata in `RestAPI.hs`**

`main = getArgs >>= \case ["--write-swagger-only"] -> writeSwagger >> exitSuccess; _ -> serve` — the flag branch
comes first and runs no `withCfgProviders`, no pool creation and no `getPortFromEnvOrDefault`, which is precisely
what makes it usable in CI.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal run query-api -- --write-swagger-only && git diff --exit-code docs/generated/swagger/`
Expected: PASS (exit 0, no diff)

- [ ] **Step 5: Commit**

```bash
git add src/exe/query-api/Main.hs src/exe/query-api/RestAPI.hs docs/generated/swagger/query-swagger-api.json .github/workflows/ci.yml .gitignore
git commit -m "feat(query-api): --write-swagger-only flag and a CI Swagger parity guard"
```

---

## Produces

**Domain types** (`Palmares.DomainTypes.Core.Types`): `DRepId`, `GovActionId`, `ChainPoint(..)`,
`DRepStatus`, `DRepUpdateKind`, `GovActionType`, `GovActionState`, `VoterRole`, `Vote`, `TallyBasis`,
`AnchorStatus` (7 values), `ActivitySource`, `ActivityEventType`, `SearchKind`, `CommitteeMemberStatus`,
`FromHttpApiData` instances for `DRepId` and `GovActionId`.

**Envelope** (`Palmares.WebAPI.Envelope`, in `webapi-lib`, re-exported from `QueryResponses`): `Cursor`,
`Page(..)` (`pageTotal :: Maybe Int`), `ApiError(..)`, `ErrorCode(..)` (24 values), `errorCodeToHttpStatus`.

**DTOs** (`Palmares.DomainTypes.Transfer.QueryResponses`): `AnchorRef(..)` (+ `cip100_valid`), `AnchorContent(..)`,
`AnchorReference(..)`, `DRepSummary(..)`, `DRepMetrics(..)`, `VoteCounts(..)`, `EpochPowerPoint(..)` (no `rank`),
`DelegatorStats(..)`, `DelegatorRow(..)`, `DRepUpdateRow(..)`, `DRepDetailResponse(..)`, `GovActionSummary(..)`,
`GovActionLifecycle(..)`, `GovActionAnchorContent(..)`, `VoteTally(..)`, `ProposerSummary(..)`,
`GovActionDetailResponse(..)`, `VoteRecord(..)`, `DelegationRecord(..)`, `SPOSummary(..)`,
`CommitteeMemberSummary(..)`, `CommitteeResponse(..)`, `TreasuryWithdrawalRecord(..)`, `ActivityEvent(..)`,
`SearchResults(..)`, `SearchGroup(..)`, `SearchHit(..)`, `SearchTarget(..)`, `SyncStatus(..)`,
`StatusResponse(..)`, `EpochParamsSummary(..)`, `MetricFormula(..)`, `DRepExplorerPageResponse(..)`,
`PowerBucket(..)`, `DashboardPageResponse(..)`, `EpochParticipationPoint(..)`.

**Filters and sorts** (`Transfer.Filters`, `Transfer.OrderBy`): `DRepFilter` / `drepFilterFromParams` /
`DRepsOrderBy`, `GovActionFilter` / `govActionFilterFromParams` / `GovActionsOrderBy`, `VoteFilter` /
`voteFilterFromParams` / `VotesOrderBy`, plus the delegation, SPO, treasury, delegator and update sort enums.

**webapi-lib**: `Palmares.WebAPI.Envelope` (above; new `deriving-aeson` dependency),
`Palmares.WebAPI.CSV` (`CSV`, `Accept CSV`, `MimeRender CSV (Page a)`, `safeCell`),
`mkServantErr :: Int -> ErrorCode -> ServerError`, `genericErrorMessage :: ErrorCode -> Text`,
`corsExposedHeaders` carrying the five `X-*` freshness headers, `Freshness(..)`, `freshnessMiddleware`.

**query-api**: `Query.Common` (`normalizeLimitOffset`, `maxLimitFor`, `activityMaxLimit`, `likePat`, `SortSpec`,
`orderClause`, `normalizeQ`, `checkRanges`), `Query.Views.applyViewDdl`, `Query.Refresh.startMetricsRefresh`,
`Query.Gov` (`queryJson`, `queryJson1`, `runGovRead`, `listDReps`, `getDRepDetail`, `listEpochPower`,
`listDelegators`, `listDRepUpdates`, `listGovActions`, `getGovActionDetail`, `listVotes`, `listDelegations`,
`getCommittee`, `listSPOs`, `listTreasuryWithdrawals`, `listActivity`, `searchQuery`, `getStatusQuery`,
`parseCursor`, `renderCursor`), `Query.Thresholds` (`thresholdColumnFor`, `paramGroupColumns`, `securityGroupColumns`,
`spoParamChangeApplies`, `drepParamChangeThreshold`), `Metrics.Catalog` (`catalog`, `metricSql`), `Query.Pages`
(`getDRepExplorerPage`, `getDashboardPage`), `RestAPI` (`FullAPI`, `CoreFunctionAPI`, `apiSwagger`, `mkApp`),
the `--write-swagger-only` flag.

**Routes** (22 documented paths, plus the Swagger UI pair): `/health`, `/ready`, `/status`, `/metrics/formulas`, `/dreps`, `/drep/{drep-id}`,
`/drep/{drep-id}/power`, `/drep/{drep-id}/delegators`, `/drep/{drep-id}/updates`, `/gov-actions`,
`/gov-action/{gov-action-id}`, `/votes`, `/delegations`, `/committee`, `/spos`, `/treasury-withdrawals`,
`/activity`, `/search`, `/pages/dashboard`, `/pages/drep-explorer`, `/pages/drep/{drep-id}`,
`/pages/gov-action/{gov-action-id}`, `/swagger-api.json` (+ `/swagger-ui`).

**Artifacts**: `docs/generated/swagger/query-swagger-api.json` (committed, version `1.0.0`, the FE's `gen:api`
input), `scripts/smoke_query_api.sh`, `scripts/smoke_views.sh`, CI job `swagger-parity`.

**Env vars**: `PALMARES_READY_MAX_LAG_SECONDS` (300), `PALMARES_AMBER_LAG_SECONDS` (120),
`PALMARES_VIEW_REFRESH_SECONDS` (300), `PALMARES_FRESHNESS_POLL_MS` (5000), `PALMARES_NETWORK_NAME` (`preprod`),
`PALMARES_PG_OWNER_CONN_STR`; unchanged: `PORT` (8083), `PG_CONN_STR`, `CORS_ALLOWED_ORIGINS`.

**Consumes from other blocks**: `Palmares.DomainTypes.Core.Ids.{drepIdCip129, drepIdCip105, ccHotIdCip129,
ccColdIdCip129, govActionIdCip129, parseDrepIdAny}`; `sql/000`–`sql/030` and the **17** views
`palmares.{sync_state, drep, drep_cert, drep_interval, action_window, gov_action, vote, vote_tally, anchor,
vote_delegation, live_delegation, drep_epoch_delegators, drep_epoch_power, committee_member,
treasury_withdrawal, thresholds, activity}`, the one matview `palmares.drep_metrics` (with the UNIQUE index
`CONCURRENTLY` needs) and the function `palmares.eligible_actions(...)`;
`sql/040_indexes.sql`'s `palmares_idx_*` (notably the two trigram GIN indexes); roles `palmares_owner` /
`palmares_ro`; `resources/metric-formulas.json` + `resources/metrics/*.sql`;
`test/fixtures/preprod-epoch-320-metrics.json`; `Palmares.WebAPI.{Utils, CORS, ServiceProbe}`;
`Utils.mkStripPrefixSchemaOptions`; `Constants.appVersion`; `QueryAppMonad.verifyProjectionDbConnection`.

**Left to Plan B** (each a minor Swagger bump, nothing here is rewritten): `DRepDetailResponse.{profile,
attestations, disputes}`, `ProposerSummary.profile_id`, `StatusResponse.{attestations, protocol}`, `/ready`'s
Kupo-lag check, `SearchResults.{profiles, attestations}` content, `palmares.activity`'s `source=attestations`
arm, the `/profiles` · `/attestations` · `/disputes` route families and their two CSV instances.

## Open items

- **(OPEN: `bitcowsCip105` in T-API-1 is a placeholder — the fixture gives the CIP-129 id and the raw hex but not
  `drep_hash.view`; read the real CIP-105 string out of `drep_hash.view` for that credential once db-sync is
  synced, and the `gov_action1qnergwfc` vector is truncated in f1-dbsync §1.3 — take the full bech32 from the same
  query.)**
- **(OPEN: query-api needs a second pool on `PALMARES_PG_OWNER_CONN_STR` to apply the DDL and refresh the matview
  as `palmares_owner` while serving reads as `palmares_ro`. Owner decides: two DSNs as planned, or
  `GRANT palmares_owner TO palmares_ro` and one DSN — which gives the public read path DDL rights. Build two.)**
- **(OPEN: `REFRESH MATERIALIZED VIEW CONCURRENTLY` requires a UNIQUE index on the matview and no note specifies
  one. `sql/030_metrics.sql` (step 16) must create `palmares_drep_metrics_pk UNIQUE` on `drep_hash_id`, or the
  refresh thread fails at runtime, not at boot.)**
- **(OPEN: T-API §6 spells the poll interval `FRESHNESS_POLL_MS`; T-EXTRACT §5.1 requires the `PALMARES_` prefix on
  every new env var, so it is `PALMARES_FRESHNESS_POLL_MS` here. Same for `PALMARES_NETWORK_NAME`, which no note
  names — `/ready` check 4 needs an expected value to compare `meta.network_name` against, and
  `StatusResponse.network` echoes it.)**
- **(OPEN: `paramGroupColumns` and `securityGroupColumns` need the full `param_proposal` column → CIP-1694 group
  mapping and its SecurityGroup subset; §2.6 names the four `dvt_p_p_*_group` targets and calls the mapping "a
  static table we own" but no note enumerates all ~30 columns. Enumerated by the T-API-10 implementer from
  `param_proposal` at 13.7.1.0 against CIP-1694 §Protocol-parameter-groups; the `paramGroupColumns` test is
  exhaustive over `param_proposal`'s non-bookkeeping columns so an unmapped column fails CI.)**
- **(OPEN: the five single-valued `/gov-actions?state=` counts derived from f1-metric-formulas §4 sum to 132, not
  131 — one of (103 expired, 11 enacted, 6 dropped-only, 12 open) is off by one. Read
  `SELECT state, count(*) FROM palmares.gov_action GROUP BY 1` off the synced db-sync, pin the five with Σ = 131,
  and correct f1-metric-formulas §4 in the same commit. Decided by the database, not by the implementer.)**
- **(OPEN: `resources/metric-formulas.json` must also carry `gov_action_state_v1` and `tally_cast_votes_v1`
  (`sql_file: null`), because T-API-11 and T-API-12 publish those ids and T-METRIC §7's drift rule requires every
  DTO-referenced id to exist in the catalog. Owner: the synthesis §3 step-16 task.)**
- **(OPEN: `kinds` lists `stake_address` (§1.7) but `SearchResults` (§2.10) has no `stake_addresses` group —
  either add the group or drop the kind. The six-group record is the one the FE's ⌘K palette renders, so dropping
  the kind is the smaller change.)**
- **(OPEN: `GET /graph` (§1.8, `root` · `kind` · `depth` [1,3] · `min_lovelace` · `max_nodes`) is not on the cut
  list and is not in F1 check 12's endpoint list, but check 13's Cytoscape screen needs it. No block in Plan A
  owns it — assign the delegation half to a Plan A addendum or to Plan C alongside the screen.)**
- **(OPEN: `palmares.vote` must expose `revote_count` (T-METRIC §0.3's `vote_rows`) under that name for
  `VoteRecord.revote_count`; §2.7's DTO listing omits the field although §0.3 decides it is surfaced. Confirm the
  column name when `sql/010` is written.)**
*(Closed while reviewing: the `tip_time` cast is owned by `Query/Gov.hs`'s projection list — T-DATA §5.1 pre-casts
only `lag_seconds`, and this block's plumbing rule (3) in T-API-6 already casts every timestamp it projects.)*
