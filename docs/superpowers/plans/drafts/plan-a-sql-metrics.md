# Plan A — block: the SQL layer and the metric catalog

This block delivers the whole `palmares` Postgres schema (synthesis §3 Phase 3 steps 15–17, plus `Query/Views.hs`
and `Query/Refresh.hs` out of step 18): `sql/000_schema.sql` → `sql/030_metrics.sql` applied by query-api at boot as
`palmares_owner` in one transaction that begins `DROP SCHEMA IF EXISTS palmares CASCADE; CREATE SCHEMA palmares;`
and **`die`s on failure**; the seventeen plain views of synthesis §1.2 plus `palmares.threshold_map` (D-25, which
§1.2 does not enumerate), the one SQL function and the one materialized view, under the canonical names of §1.1
and the canonical metric column names of §1.4; the `file-embed`ed metric catalog that
is the single source of truth for every published formula; the epoch-320 fixture test; and the single
`REFRESH … CONCURRENTLY` thread. It implements D-2 (eligibility from `drep_interval` + `drep_votable`, never
`drep_distr` membership), D-3 (five rationale grades summing to `votes_cast`, numerator `verified + inline`),
D-10 (one anchor verdict `CASE` in SQL with its mandatory golden test on the exact `fetch_error` strings),
D-15 (`palmares.eligible_actions` is a SQL function; the DDL runs in one transaction; `sql/040_indexes.sql` is
*not* ours), D-19 (`palmares.activity` unions the five epoch-boundary kinds with the corrected cursor), D-20 (one
unfiltered `drep_epoch_power` with `target_kind`), D-24 (`palmares.vote` carries `voter_hot_hash` **and**
`voter_cold_hash`), D-25 (the static (role, action-type) → threshold-column table as a named deliverable with its
own test) and D-9 (the catalog, not hand-written prose, generates `docs/metrics.md`). Assumptions: the repo
bootstrap and extraction (synthesis §3 steps 5–7) have landed, so `palmares.cabal`, `src/exe/query-api/`,
`src/exe/admin/` and `src/test/` exist; roles `palmares_owner` / `palmares_ro` and their grants (step 14) exist
before the first boot against a real db-sync; `sql/040_indexes.sql` and the preprod sync (steps 8–13) are other
blocks' work, and every *measured* assertion here therefore runs against a caught-up preprod db-sync while every
*shape* assertion runs in CI against a seeded skeleton. File split (ours, not in the notes): `000` = schema,
`010_gov_views.sql` = views over db-sync only, `020_views.sql` = views over `palmares` views + the function,
`030_metrics.sql` = the matview. T-EXTRACT's `sql/020_anchors.sql` is an alias to delete on sight — the anchor
view must precede `gov_action` and `vote` in lexical order, so it opens `010_gov_views.sql`. All four files are
created (three of them as comment-only stubs) in T-SQL-METRICS-1, because `palmaresSqlFiles` embeds them by name
at compile time; every later task appends to a file that already exists. `sql/030_metrics.sql` is written last
(T-SQL-METRICS-12) rather than at its §3 step-16 position: nothing in steps 15–17 reads the matview, and its
expected numbers are the step-17 fixture's, so building it after the fixture removes a round of re-derivation.

---

### Task T-SQL-METRICS-1: Schema bootstrap, fail-closed, plus the db-sync skeleton that makes every later view testable in CI

**Files:**
- Create: `sql/000_schema.sql`
- Create: `sql/010_gov_views.sql`, `sql/020_views.sql`, `sql/030_metrics.sql` — comment-only stubs; later tasks append
- Create: `src/exe/query-api/Query/Views.hs`
- Create: `test/fixtures/dbsync-skeleton.sql`
- Create: `src/test/Test/Pg.hs`
- Test: `src/test/UnitTests/Views.hs`
- Modify: `palmares.cabal` (`extra-source-files: sql/*.sql` — already in T-EXTRACT §1.3; `query-api` `other-modules: Query.Views`; test-suite `hs-source-dirs: src/test, src/exe/query-api` **and** `other-modules: Query.Views, Test.Pg, UnitTests.Views` + `build-depends: file-embed, persistent, persistent-postgresql`. The extra source dir is load-bearing: a test-suite cannot import an executable's `other-modules`, and synthesis §1.3 fixes the module at `src/exe/query-api/Query/Views.hs`.)
- Modify: `.github/workflows/ci.yml` (add a `postgres:17-alpine` service to the `haskell` job)

**Interfaces:**
- Consumes: `PG_CONN_STR`; roles `palmares_owner`, `palmares_ro` (synthesis §3 step 14).
- Produces: `applyPalmaresSchema :: ConnectionPool -> IO ()` · `applySchemaSql :: [(FilePath, ByteString)] -> SqlPersistT IO ()` · `palmaresSqlFiles :: [(FilePath, ByteString)]` · `Test.Pg.withScratchDb :: (ConnectionPool -> IO a) -> IO a` · `Test.Pg.runPg :: ConnectionPool -> SqlPersistT IO a -> IO a` · `test/fixtures/dbsync-skeleton.sql` · `PALMARES_TEST_PG_CONN_STR` **(OPEN: the notes name no test-database env var; this name is mine. Owner/CI decides, and `ci.yml` must set it on the `haskell` job's test step.)**

- [ ] **Step 1: Write the failing test**

```haskell
-- src/test/UnitTests/Views.hs
module UnitTests.Views (tests) where

import Data.Text (Text)
import Database.Persist.Sql (Single (..), rawSql)
import Query.Views (applyPalmaresSchema, applySchemaSql, palmaresSqlFiles)
import Test.Pg (withScratchDb, runPg)
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests =
  testGroup
    "palmares schema bootstrap"
    [ testCase "applies, and applies again (DROP SCHEMA CASCADE makes it idempotent)" $
        withScratchDb $ \pool -> do
          applyPalmaresSchema pool
          applyPalmaresSchema pool
          [Single (n :: Int)] <-
            runPg pool $ rawSql "SELECT count(*)::int FROM pg_namespace WHERE nspname = 'palmares'" []
          n @?= 1
    , testCase "every file in palmaresSqlFiles is applied, in lexical order" $
        map fst palmaresSqlFiles
          @?= ["000_schema.sql", "010_gov_views.sql", "020_views.sql", "030_metrics.sql"]
    , testCase "a failing statement rolls the whole set back — the previous schema survives" $
        withScratchDb $ \pool -> do
          applyPalmaresSchema pool
          [Single (before :: Int)] <- runPg pool countPalmaresViews
          r <- try @SomeException $ runPg pool (applySchemaSql [("999_bad.sql", "DROP SCHEMA palmares CASCADE; SELECT 1/0;")])
          assertBool "the bad set must fail" (isLeft r)
          [Single (after :: Int)] <- runPg pool countPalmaresViews
          after @?= before
    ]
  where
    countPalmaresViews :: SqlPersistT IO [Single Int]
    countPalmaresViews =
      rawSql "SELECT count(*)::int FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace WHERE n.nspname = 'palmares'" []
```

- [ ] **Step 2: Run test to verify it fails**

Run: `PALMARES_TEST_PG_CONN_STR="host=localhost user=postgres password=devonly dbname=postgres port=5432" cabal test test --test-show-details=direct --test-option='--pattern=palmares schema bootstrap'`
Expected: FAIL with `Could not find module 'Query.Views'`

- [ ] **Step 3: Implement**

`sql/000_schema.sql` is exactly two statements — `DROP SCHEMA IF EXISTS palmares CASCADE;` then `CREATE SCHEMA palmares;` — and **must not** mention `palmares_attest` (attest-sync creates that one before `runMigrations`, or `persistent` migrates into db-sync's `public`).

`sql/010_gov_views.sql`, `sql/020_views.sql` and `sql/030_metrics.sql` are created here holding one header comment each and no statements (`-- palmares views over db-sync's public schema; objects appended by later tasks.`). They must exist now because `palmaresSqlFiles` embeds all four by name at compile time; an absent file is a build error, not a runtime one.

`Query/Views.hs` — `palmaresSqlFiles` is one `makeRelativeToProject … >>= embedFile` per file, listed explicitly in lexical order (never `embedDir`); `applySchemaSql` issues each file's bytes with `rawExecute` inside the caller's transaction; `applyPalmaresSchema pool = either (\e -> die ("palmares schema DDL failed: " <> show (e :: SomeException))) pure =<< try (runSqlPool (applySchemaSql palmaresSqlFiles) pool)`. One `runSqlPool` call ⇒ one transaction ⇒ the rollback test passes for free.

`test/fixtures/dbsync-skeleton.sql` creates, in `public`, the db-sync enums (`anchortype`, `govactiontype`, `voterrole`, `vote`, `rewardtype`) and empty stand-ins for exactly the tables our views read, with the columns the notes' column map names: `block`, `tx`, `meta`, `schema_version`, `drep_hash`, `drep_registration`, `drep_distr`, `delegation_vote`, `stake_address`, `stake_registration`, `stake_deregistration`, `gov_action_proposal`, `voting_procedure`, `voting_anchor`, `off_chain_vote_data`, `off_chain_vote_gov_action_data`, `off_chain_vote_drep_data`, `off_chain_vote_author`, `off_chain_vote_reference`, `off_chain_vote_external_update`, `off_chain_vote_fetch_error`, `committee`, `committee_hash`, `committee_member`, `committee_registration`, `committee_de_registration`, `epoch_state`, `constitution`, `treasury_withdrawal`, `reward_rest`, `epoch_param`, `param_proposal`, `pool_hash`, `pool_update`, `pool_retire`. `ponytail:` comment on the file naming its ceiling: it is a *shape* fixture, and `sql/checks/*.sql` against the real preprod db-sync is what catches skeleton drift.

`Test/Pg.hs` — `withScratchDb` creates `palmares_test_<pid>`, connects, applies the skeleton, hands over a pool, drops the database in `finally`; `runPg = flip runSqlPool`. The `CREATE DATABASE` / `DROP DATABASE` pair runs through **`runSqlPoolNoTransaction … Nothing`** on a pool against the admin database — PostgreSQL forbids both inside a transaction block, and `runSqlPool` opens one. It `die`s with the env var name when `PALMARES_TEST_PG_CONN_STR` is unset so a skipped DB test can never read as a pass.

`ci.yml`, in the `haskell` job, as a job-level `services:` key (sibling of `steps:`):

```yaml
    services:
      postgres:
        image: postgres:17-alpine
        env: { POSTGRES_PASSWORD: devonly }
        ports: ["5432:5432"]
        options: >-
          --health-cmd "pg_isready -U postgres" --health-interval 10s
          --health-timeout 5s --health-retries 5
```
and `PALMARES_TEST_PG_CONN_STR: host=localhost user=postgres password=devonly dbname=postgres port=5432` on the `cabal test all --test-show-details=direct` step that T-INFRA §5.1 already defines.

- [ ] **Step 4: Run test to verify it passes**

Run: `PALMARES_TEST_PG_CONN_STR="host=localhost user=postgres password=devonly dbname=postgres port=5432" cabal test test --test-show-details=direct --test-option='--pattern=palmares schema bootstrap'`
Expected: PASS (3 tests)

- [ ] **Step 5: Commit**

```bash
git add sql/000_schema.sql sql/010_gov_views.sql sql/020_views.sql sql/030_metrics.sql src/exe/query-api/Query/Views.hs test/fixtures/dbsync-skeleton.sql src/test/Test/Pg.hs src/test/UnitTests/Views.hs palmares.cabal .github/workflows/ci.yml
git commit -m "feat(sql): fail-closed palmares schema bootstrap in one transaction"
```

---

### Task T-SQL-METRICS-2: `palmares.anchor` — the single verdict `CASE` (D-10) and its mandatory golden test

**Files:**
- Modify: `sql/010_gov_views.sql` (the T-1 stub; `palmares.anchor` is its first object)
- Test: `src/test/UnitTests/Anchor.hs`
- Modify: `palmares.cabal` (test-suite `other-modules: UnitTests.Anchor`)

**Interfaces:**
- Consumes: `applyPalmaresSchema`, `Test.Pg.withScratchDb`, `test/fixtures/dbsync-skeleton.sql`.
- Produces: `palmares.anchor (voting_anchor_id, url, data_hash, anchor_type, verdict, cip100_valid, language, comment, warning, json, fetch_error, fetch_time, retry_count)`; `verdict ∈ valid | invalid_schema | hash_mismatch | oversized | unreachable | inline | not_fetched`.

- [ ] **Step 1: Write the failing test**

```haskell
-- src/test/UnitTests/Anchor.hs — D-10: pins the exact upstream `show OffChainFetchError` strings.
tests :: TestTree
tests = testCase "anchor verdicts, seeded rows" $ withScratchDb $ \pool -> do
  applyPalmaresSchema pool
  runPg pool $ rawExecute seedRows []
  verdicts <- runPg pool $ rawSql "SELECT url, verdict, cip100_valid FROM palmares.anchor ORDER BY voting_anchor_id" []
  map (\(Single u, Single v, Single c) -> (u, v, c)) verdicts
    @?= [ ("https://ok.example/a.jsonld",        "valid",          Just True)
        , ("https://schema.example/b.jsonld",    "invalid_schema", Just False)
        , ("https://notjson.example/c.txt",      "invalid_schema", Nothing)     -- is_valid IS NULL
        , ("https://mismatch.example/d.jsonld",  "hash_mismatch",  Nothing)
        , ("https://big.example/e.jsonld",       "oversized",      Nothing)
        , ("ftp://nope.example/f.jsonld",        "unreachable",    Nothing)     -- default arm, never a false accusation
        , ("data:text/plain;utf8,Approve parameter change action on preprod testnet", "inline", Nothing)
        , ("https://queued.example/h.jsonld",    "not_fetched",    Nothing)
        ]

seedRows :: Text
seedRows =
  "INSERT INTO public.voting_anchor (id, block_id, data_hash, url, type) VALUES \
  \ (1,1,'\\x01','https://ok.example/a.jsonld','vote'), \
  \ (2,1,'\\x02','https://schema.example/b.jsonld','vote'), \
  \ (3,1,'\\x03','https://notjson.example/c.txt','vote'), \
  \ (4,1,'\\x04','https://mismatch.example/d.jsonld','vote'), \
  \ (5,1,'\\x05','https://big.example/e.jsonld','vote'), \
  \ (6,1,'\\x06','ftp://nope.example/f.jsonld','vote'), \
  \ (7,1,'\\x07','data:text/plain;utf8,Approve parameter change action on preprod testnet','vote'), \
  \ (8,1,'\\x08','https://queued.example/h.jsonld','vote'); \
  \ INSERT INTO public.off_chain_vote_data (id, voting_anchor_id, hash, language, is_valid) VALUES \
  \ (1,1,'\\x01','en',TRUE), (2,2,'\\x02','en',FALSE), (3,3,'\\x03',NULL,NULL); \
  \ INSERT INTO public.off_chain_vote_fetch_error (id, voting_anchor_id, fetch_error, fetch_time, retry_count) VALUES \
  \ (1,4,'Hash mismatch when fetching metadata from https://mismatch.example/d.jsonld. Expected \"ab\" but got \"cd\".','2026-10-19 00:00:00',0), \
  \ (2,5,'Error Offchain Voting Anchor: Size error, fetching metadata from https://big.example/e.jsonld exceeded 512 bytes.','2026-10-19 00:00:00',0), \
  \ (3,6,'Error Offchain Voting Anchor: URL parse error for ftp://nope.example/f.jsonld','2026-10-19 00:00:00',2), \
  \ (4,7,'Error Offchain Voting Anchor: URL parse error for data:text/plain;utf8,Approve parameter change action on preprod testnet','2026-10-19 00:00:00',1);"
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test test --test-show-details=direct --test-option='--pattern=anchor verdicts'`
Expected: FAIL with `relation "palmares.anchor" does not exist`

- [ ] **Step 3: Implement `palmares.anchor` as the first object in `sql/010_gov_views.sql`**

T-DATA §6.3's `v_anchor` verbatim, with four changes and nothing else: the canonical name; the canonical verdict
strings (`valid_hash_bad_schema` and `valid_hash_not_json` both collapse to `invalid_schema`, `pending` becomes
`not_fetched`); `d.is_valid AS cip100_valid` as its own column (synthesis §1.4); and the arm order below, in which
`inline` is tested **first** because a `data:` anchor also carries a URL-parse `off_chain_vote_fetch_error` row.

```sql
CASE
  WHEN va.url LIKE 'data:%'                                        THEN 'inline'
  WHEN d.id IS NOT NULL AND d.is_valid IS TRUE                     THEN 'valid'
  WHEN d.id IS NOT NULL                                            THEN 'invalid_schema'
  WHEN e.fetch_error LIKE 'Hash mismatch%'                         THEN 'hash_mismatch'
  WHEN e.fetch_error LIKE '%Size error, fetching metadata from%'    THEN 'oversized'
  WHEN e.id IS NOT NULL                                            THEN 'unreachable'
  ELSE                                                                  'not_fetched'
END AS verdict
```

`LEFT JOIN LATERAL (… ORDER BY x.retry_count DESC LIMIT 1) e ON true` picks the newest failure. Every read of
`off_chain_vote_{author,reference,external_update}` in this file carries `DISTINCT` with a `ponytail:` comment
naming PR #2137 as the version that removes it.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test test --test-show-details=direct --test-option='--pattern=anchor verdicts'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add sql/010_gov_views.sql src/test/UnitTests/Anchor.hs palmares.cabal
git commit -m "feat(sql): palmares.anchor single verdict CASE with golden test on upstream fetch_error strings"
```

---

### Task T-SQL-METRICS-3: DRep identity, certificate log, registration intervals, sync state

**Files:**
- Modify: `sql/010_gov_views.sql` (append after `palmares.anchor`)
- Test: `sql/checks/010_drep.sql`
- Create: `scripts/verify_views.sh`

**Interfaces:**
- Consumes: `palmares.anchor`.
- Produces: `palmares.sync_state (block_no, slot_no, epoch_no, tip_hash, tip_time, lag_seconds, drep_distr_epoch, dbsync_schema_stage_two, network_name)` · `palmares.drep_cert (drep_hash_id, cert_kind, tx_hash, slot_no, epoch_no, cert_time, deposit, voting_anchor_id)` · `palmares.drep_interval (drep_hash_id, from_epoch, from_slot, from_time, to_epoch)` · `palmares.drep (drep_hash_id, drep_raw, has_script, registration_status, is_active, active_until_epoch, voting_anchor_id, first_cert_slot, first_cert_time, last_cert_slot, last_cert_time, given_name, image_url, language)`

- [ ] **Step 1: Write the failing test**

A measured assertion against the caught-up preprod db-sync — the row counts are the point, and no seeded fixture
can produce them.

```sql
-- sql/checks/010_drep.sql — preprod, tip epoch >= 320. Figures: T-DATA §7.1, T-METRIC §4.
DO $$ BEGIN
  ASSERT (SELECT count(*) FROM public.drep_hash)                                     = 802,  'drep_hash rows';
  ASSERT (SELECT count(*) FROM palmares.drep WHERE registration_status = 'registered') = 294, 'registered DReps';
  ASSERT (SELECT count(*) FROM palmares.drep WHERE drep_raw IS NULL)                 = 0,    'pseudo-DReps are not DReps (3.7)';
  -- the fixture DRep: registered in epoch 173, one open interval, no retirement
  ASSERT (SELECT count(*) FROM palmares.drep_interval i JOIN public.drep_hash h ON h.id = i.drep_hash_id
          WHERE h.raw = decode('81378d3ea89b68f0fc09944b1f93f2feb851318f196f98ba4029d8a8','hex')) = 1;
  ASSERT (SELECT from_epoch FROM palmares.drep_interval i JOIN public.drep_hash h ON h.id = i.drep_hash_id
          WHERE h.raw = decode('81378d3ea89b68f0fc09944b1f93f2feb851318f196f98ba4029d8a8','hex')) = 173;
  ASSERT (SELECT to_epoch IS NULL FROM palmares.drep_interval i JOIN public.drep_hash h ON h.id = i.drep_hash_id
          WHERE h.raw = decode('81378d3ea89b68f0fc09944b1f93f2feb851318f196f98ba4029d8a8','hex'));
  ASSERT (SELECT from_time FROM palmares.drep_interval i JOIN public.drep_hash h ON h.id = i.drep_hash_id
          WHERE h.raw = decode('81378d3ea89b68f0fc09944b1f93f2feb851318f196f98ba4029d8a8','hex'))
         = '2024-10-16 20:38:10'::timestamp;
  -- drep_cert carries all three kinds; the fixture DRep has register + update
  ASSERT (SELECT count(DISTINCT cert_kind) FROM palmares.drep_cert) = 3, 'register/update/retire';
  -- sync_state is exactly one row and guards the network
  ASSERT (SELECT count(*) FROM palmares.sync_state) = 1;
  ASSERT (SELECT network_name FROM palmares.sync_state) = 'preprod';
  ASSERT (SELECT drep_distr_epoch >= epoch_no - 1 FROM palmares.sync_state), 'ledger: enable took';
END $$;
```

- [ ] **Step 2: Run test to verify it fails**

Run: `psql "$PG_CONN_STR" -v ON_ERROR_STOP=1 -f sql/checks/010_drep.sql`
Expected: FAIL with `ERROR: relation "palmares.drep" does not exist` (exit status 3)

- [ ] **Step 3: Implement the four views in `sql/010_gov_views.sql`, in this order: `sync_state`, `drep_cert`, `drep_interval`, `drep`**

- `sync_state`: T-DATA §5.1 verbatim under the canonical name — `ORDER BY b.id DESC LIMIT 1`, and
  `EXTRACT(EPOCH FROM (now() AT TIME ZONE 'UTC' - b.time))::bigint AS lag_seconds` (`block.time` has no zone).
- `drep_cert`: `drep_registration ⋈ tx ⋈ block`, `cert_kind = CASE WHEN deposit > 0 THEN 'register' WHEN deposit < 0 THEN 'retire' ELSE 'update' END`.
- `drep_interval`: T-METRIC §0.3 (2) verbatim, **including `from_slot`** (T-METRIC §5.3's correction — §5.3's
  retire lookup is bounded by `slot_no`, not `epoch_no`).
- `drep`: one row per `drep_hash.raw IS NOT NULL`; `registration_status` derived from the **latest** `drep_cert`
  row (`retire` ⇒ `retired`, else `registered`), **never** from the existence of a `drep_hash` row; `is_active` =
  `drep_distr.active_until >= (SELECT epoch_no FROM palmares.sync_state)`; the CIP-119 display fields joined
  through `palmares.anchor` → `off_chain_vote_drep_data`. No bech32 in SQL: expose `(drep_raw, has_script)` and
  let `Palmares.DomainTypes.Core.Ids` encode.

`scripts/verify_views.sh` runs every `sql/checks/*.sql` in lexical order with `psql -v ON_ERROR_STOP=1 -f` and
exits non-zero on the first failed assertion. **Never `--single-transaction`**: `070_drep_metrics.sql` ends with a
bare `REFRESH … CONCURRENTLY`, which cannot run inside a transaction block.

- [ ] **Step 4: Run test to verify it passes**

Run: `psql "$PG_CONN_STR" -v ON_ERROR_STOP=1 -f sql/checks/010_drep.sql`
Expected: PASS — `DO` printed once, exit status 0

Run (the CI half — the appended DDL must still apply against the seeded skeleton): `cabal test test --test-show-details=direct --test-option='--pattern=palmares schema bootstrap'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add sql/010_gov_views.sql sql/checks/010_drep.sql scripts/verify_views.sh
git commit -m "feat(sql): drep, drep_cert, drep_interval and sync_state views"
```

---

### Task T-SQL-METRICS-4: `palmares.action_window` and the `palmares.eligible_actions` SQL function (D-2, D-15)

**Files:**
- Modify: `sql/010_gov_views.sql` (append), `sql/020_views.sql` (the T-1 stub; `palmares.eligible_actions` is its first object)
- Test: `sql/checks/020_eligibility.sql`

**Interfaces:**
- Consumes: `palmares.drep_interval`.
- Produces: `palmares.action_window (gov_action_id, action_type, proposed_epoch, proposed_slot, proposed_at, proposed_proto_major, expiration, last_votable_epoch, drep_votable, spo_votable, param_proposal)` · `palmares.eligible_actions(drep_hash_id bigint, as_of_epoch int) RETURNS TABLE (gov_action_id bigint, action_type public.govactiontype)`

- [ ] **Step 1: Write the failing test**

```sql
-- sql/checks/020_eligibility.sql — preprod @ epoch 320. Figures: T-METRIC §4, §3.4, §10.
DO $$
DECLARE d bigint;
BEGIN
  ASSERT (SELECT count(*) FROM palmares.action_window)                                      = 131, 'all proposals';
  ASSERT (SELECT count(*) FROM palmares.action_window WHERE last_votable_epoch < 320)       = 119, 'settled';
  ASSERT (SELECT count(*) FROM palmares.action_window WHERE last_votable_epoch >= 320)      = 12,  'still open';
  ASSERT (SELECT count(*) FROM palmares.action_window WHERE last_votable_epoch < 320 AND drep_votable) = 115,
         'settled and DRep-votable: the 4 excluded are the bootstrap-era 3 ParameterChange + 1 HardForkInitiation';
  -- govActionLifetime is 7 on preprod, on all 131 rows (network-specific: mainnet is 6)
  ASSERT (SELECT count(*) FROM palmares.action_window WHERE expiration - proposed_epoch <> 7) = 0;
  -- the bootstrap window is epochs 163..180 and holds exactly 6 proposals of the 3 permitted types
  ASSERT (SELECT count(*) FROM palmares.action_window WHERE proposed_epoch BETWEEN 163 AND 180) = 6;
  ASSERT (SELECT count(*) FROM palmares.action_window
          WHERE proposed_epoch BETWEEN 163 AND 180 AND action_type = 'ParameterChange')     = 3;
  ASSERT (SELECT count(*) FROM palmares.action_window
          WHERE proposed_epoch BETWEEN 163 AND 180 AND action_type = 'InfoAction')          = 2;
  ASSERT (SELECT count(*) FROM palmares.action_window
          WHERE proposed_epoch BETWEEN 163 AND 180 AND action_type = 'HardForkInitiation')   = 1;
  -- D-15: the function, not a copy-pasted CTE. 114 = 115 settled-and-votable minus 1 closing before epoch 173.
  SELECT id INTO d FROM public.drep_hash
    WHERE raw = decode('81378d3ea89b68f0fc09944b1f93f2feb851318f196f98ba4029d8a8','hex');
  ASSERT (SELECT count(*) FROM palmares.eligible_actions(d, 320)) = 114, 'fixture DRep eligible_actions';
  ASSERT (SELECT count(*) FROM palmares.eligible_actions(d, 320) WHERE action_type = 'TreasuryWithdrawals') = 79;
  ASSERT (SELECT count(*) FROM palmares.eligible_actions(d, 320) WHERE action_type = 'InfoAction')          = 13;
  ASSERT (SELECT count(*) FROM palmares.eligible_actions(d, 320) WHERE action_type = 'ParameterChange')     = 11;
  ASSERT (SELECT count(*) FROM palmares.eligible_actions(d, 320) WHERE action_type = 'NewCommittee')        = 7;
  ASSERT (SELECT count(*) FROM palmares.eligible_actions(d, 320) WHERE action_type = 'HardForkInitiation')  = 2;
  ASSERT (SELECT count(*) FROM palmares.eligible_actions(d, 320) WHERE action_type = 'NoConfidence')        = 1;
  ASSERT (SELECT count(*) FROM palmares.eligible_actions(d, 320) WHERE action_type = 'NewConstitution')     = 1;
  -- the 'n/a' case, asserted so it can never pass vacuously: as of its own registration epoch the
  -- fixture DRep has nothing settled behind it, so its eligibility set is empty (T-METRIC §3.8).
  ASSERT (SELECT count(*) FROM palmares.eligible_actions(d, 173)) = 0, 'empty eligibility set';
END $$;
```

- [ ] **Step 2: Run test to verify it fails**

Run: `psql "$PG_CONN_STR" -v ON_ERROR_STOP=1 -f sql/checks/020_eligibility.sql`
Expected: FAIL with `ERROR: relation "palmares.action_window" does not exist`

- [ ] **Step 3: Implement `palmares.action_window` (in `010_gov_views.sql`) and `palmares.eligible_actions` (opening `020_views.sql`)**

`action_window` is T-METRIC §0.3 (1) verbatim: `last_votable_epoch = LEAST(expiration, COALESCE(ratified_epoch, expiration), COALESCE(expired_epoch, expiration), COALESCE(dropped_epoch - 1, expiration))` — `ratified_epoch`
un-decremented, `dropped_epoch` decremented, both empirically pinned — and `drep_votable` as the **window** test
(`action_type = 'InfoAction' OR EXISTS (SELECT 1 FROM public.epoch_param ep WHERE ep.epoch_no BETWEEN proposed_epoch AND last_votable_epoch AND ep.protocol_major <> 9)`), never `proposed_proto_major`, which stays
for display only. `spo_votable` is T-METRIC §3.3's `CASE` with all eleven `param_proposal` SecurityGroup columns.

`eligible_actions` is §5.1's `elig` CTE promoted to `LANGUAGE sql STABLE`, taking `(drep_hash_id, as_of_epoch)` and
returning `(gov_action_id, action_type)` — `GROUP BY` both columns, because a DRep may have several intervals.
Eligibility is `drep_interval` overlap plus `drep_votable` plus `last_votable_epoch < as_of_epoch` (D-2: never
`drep_distr` membership).

- [ ] **Step 4: Run test to verify it passes**

Run: `psql "$PG_CONN_STR" -v ON_ERROR_STOP=1 -f sql/checks/020_eligibility.sql`
Expected: PASS — exit status 0

Run (the CI half): `cabal test test --test-show-details=direct --test-option='--pattern=palmares schema bootstrap'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add sql/010_gov_views.sql sql/020_views.sql sql/checks/020_eligibility.sql
git commit -m "feat(sql): action_window with window-based drep_votable + eligible_actions SQL function"
```

---

### Task T-SQL-METRICS-5: `palmares.vote` (D-24), `palmares.thresholds`, `palmares.threshold_map` (D-25)

**Files:**
- Modify: `sql/010_gov_views.sql` (append)
- Test: `src/test/UnitTests/ThresholdMap.hs`, `sql/checks/030_votes.sql`
- Modify: `palmares.cabal` (test-suite `other-modules: UnitTests.ThresholdMap`)

**Interfaces:**
- Consumes: `palmares.action_window`, `palmares.anchor`, `applyPalmaresSchema`, `Test.Pg.withScratchDb`.
- Produces: `palmares.vote (gov_action_id, voter_role, voter_id_hash, voter_hot_hash, voter_cold_hash, has_script, vote, voting_anchor_id, vote_epoch, vote_slot, cast_at, vote_tx_hash, tx_index, vote_index, seq_first, seq_last, vote_rows)` · `palmares.thresholds (epoch_no, dvt_*, pvt_*, pvtpp_security_group, committee_min_size, committee_max_term_length, gov_action_lifetime, gov_action_deposit, drep_deposit, drep_activity)` · `palmares.threshold_map (action_type, voter_role, threshold_param)` **(OPEN: D-25 requires this table as a named deliverable but no note names the object; `palmares.threshold_map` is mine, and synthesis §1.2 does not enumerate it. Owner confirms the name before the API block echoes `threshold_param`.)**
- `palmares.vote_tally` consumes `palmares.drep_epoch_power`, so it is built in T-SQL-METRICS-6, not here.

- [ ] **Step 1: Write the failing test**

```haskell
-- src/test/UnitTests/ThresholdMap.hs — D-25. The whole point is `pvtpp_security_group`, which a
-- `pvt_%` prefix scan misses, and the NULLs where no threshold exists.
tests :: TestTree
tests = testCase "threshold_map is the API contract's static table" $ withScratchDb $ \pool -> do
  applyPalmaresSchema pool
  rows <- runPg pool $ rawSql "SELECT action_type::text, voter_role::text, threshold_param FROM palmares.threshold_map ORDER BY action_type::text, voter_role::text" []
  map (\(Single a, Single r, Single p) -> (a, r, p)) rows
    @?= [ ("HardForkInitiation",  "ConstitutionalCommittee", Nothing)                      -- quorum, not a column
        , ("HardForkInitiation",  "DRep", Just "dvt_hard_fork_initiation")
        , ("HardForkInitiation",  "SPO",  Just "pvt_hard_fork_initiation")
        , ("InfoAction",          "ConstitutionalCommittee", Nothing)
        , ("InfoAction",          "DRep", Nothing)                                          -- never ratifiable
        , ("InfoAction",          "SPO",  Nothing)
        , ("NewCommittee",        "ConstitutionalCommittee", Nothing)                       -- does not vote
        , ("NewCommittee",        "DRep", Just "dvt_committee_normal")                      -- _normal always, in F1
        , ("NewCommittee",        "SPO",  Just "pvt_committee_normal")
        , ("NewConstitution",     "ConstitutionalCommittee", Nothing)
        , ("NewConstitution",     "DRep", Just "dvt_update_to_constitution")
        , ("NewConstitution",     "SPO",  Nothing)
        , ("NoConfidence",        "ConstitutionalCommittee", Nothing)                       -- does not vote
        , ("NoConfidence",        "DRep", Just "dvt_motion_no_confidence")
        , ("NoConfidence",        "SPO",  Just "pvt_motion_no_confidence")
        , ("ParameterChange",     "ConstitutionalCommittee", Nothing)
        , ("ParameterChange",     "DRep", Nothing)                                          -- max over touched groups
        , ("ParameterChange",     "SPO",  Just "pvtpp_security_group")                      -- NOT pvt_pp_security_group
        , ("TreasuryWithdrawals", "ConstitutionalCommittee", Nothing)
        , ("TreasuryWithdrawals", "DRep", Just "dvt_treasury_withdrawal")
        , ("TreasuryWithdrawals", "SPO",  Nothing)
        ]
```

```sql
-- sql/checks/030_votes.sql — preprod @ epoch 320. Figures: T-METRIC §3.2, §4 network table.
DO $$ BEGIN
  ASSERT (SELECT count(*) FROM palmares.vote WHERE voter_role = 'DRep')                       = 816, 'DRep vote rows';
  ASSERT (SELECT count(DISTINCT voter_hot_hash) FROM palmares.vote WHERE voter_role = 'DRep')  = 511, 'distinct DRep voters, all time';
  ASSERT (SELECT count(*) FROM (SELECT 1 FROM palmares.vote WHERE voter_role = 'DRep'
          GROUP BY voter_hot_hash, gov_action_id HAVING count(*) > 1) x)                       = 69,  're-voted pairs';
  ASSERT (SELECT max(vote_rows) FROM palmares.vote WHERE voter_role = 'DRep')                  = 20,  'worst re-voted pair';
  ASSERT (SELECT count(*) FROM palmares.vote
          WHERE voter_role = 'DRep' AND voting_anchor_id IS NOT NULL)                          = 65,  'DRep votes carrying an anchor';
  ASSERT (SELECT count(*) FROM palmares.vote WHERE voter_role = 'SPO')                         = 90;
  ASSERT (SELECT count(DISTINCT voter_hot_hash) FROM palmares.vote WHERE voter_role = 'SPO')    = 45;
  ASSERT (SELECT count(*) FROM palmares.vote
          WHERE voter_role = 'SPO' AND voting_anchor_id IS NOT NULL)                           = 0;
  -- exactly one seq_last = 1 and one seq_first = 1 per (voter identity, action) — the tally depends on it
  ASSERT (SELECT count(*) FROM (SELECT 1 FROM palmares.vote GROUP BY voter_role, voter_id_hash, gov_action_id
          HAVING count(*) FILTER (WHERE seq_last = 1) <> 1 OR count(*) FILTER (WHERE seq_first = 1) <> 1) x) = 0;
  -- D-24: CC votes are cast by hot keys; the cold credential must resolve for every CC vote
  ASSERT (SELECT count(*) FROM palmares.vote
          WHERE voter_role = 'ConstitutionalCommittee' AND voter_cold_hash IS NULL)            = 0;
  -- D-27: the CC member identity is the COLD key. Preprod carries 648.766 CC hot keys mapping to only
  -- 43 cold keys (an AuthCommitteeHotKey stress test in epoch 187), so partitioning the windows by
  -- voter_hot_hash reports 3.243.798 final CC votes where the truth is 193.
  ASSERT (SELECT count(*) FROM palmares.vote WHERE voter_role = 'ConstitutionalCommittee')     = 3243802;
  ASSERT (SELECT count(*) FROM palmares.vote
          WHERE voter_role = 'ConstitutionalCommittee' AND seq_last = 1)                       = 193;
  ASSERT (SELECT count(DISTINCT voter_cold_hash) FROM palmares.vote
          WHERE voter_role = 'ConstitutionalCommittee')                                        = 43;
  -- gov_action_lifetime is 6, not 7. The ledger's own `expiration` is proposed_epoch + lifetime + 1
  -- (delta exactly 7 on all 131 preprod actions), which is why `last_votable_epoch` must read
  -- `gov_action_proposal.expiration` and never arithmetic over this parameter.
  ASSERT (SELECT gov_action_lifetime FROM palmares.thresholds WHERE epoch_no = 320) = 6;
END $$;
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test test --test-show-details=direct --test-option='--pattern=threshold_map'` then `psql "$PG_CONN_STR" -v ON_ERROR_STOP=1 -f sql/checks/030_votes.sql`
Expected: FAIL with `relation "palmares.threshold_map" does not exist`, then `relation "palmares.vote" does not exist`

- [ ] **Step 3: Implement the three objects, all in `010_gov_views.sql`**

`thresholds` (`epoch_param` restricted to the governance columns, **explicitly listed** —
`pvtpp_security_group` does not share the `pvt_` prefix); `threshold_map` as a plain view over a 21-row `VALUES`
list, the exact content of the test above; `vote`, which is T-METRIC §0.3 (3)'s `drep_vote` generalised to all
three roles — drop the `voter_role = 'DRep'` filter, keep `seq_first` / `seq_last` / `vote_rows` with the same
window specs partitioned by `(voter_role, voter_id_hash, gov_action_proposal_id)` — `voter_id_hash` is
`coalesce(voter_cold_hash, voter_hot_hash)` computed in an outer SELECT over the one below, i.e. the **cold**
credential for the CC and the signing credential itself for DRep/SPO (D-27: hot keys rotate, and preprod has
648.766 CC hot keys over 43 cold keys, so a hot-key partition reports 3.243.798 final CC votes instead of
193) — and resolve the voter per D-24:

```sql
-- voter_hot_hash: the credential that signed. voter_cold_hash: NULL except for the CC, where it is
-- resolved as of the vote's tx, because a member's history splits across hot-key rotations.
CASE v.voter_role
  WHEN 'DRep' THEN (SELECT dh.raw FROM public.drep_hash dh WHERE dh.id = v.drep_voter)
  WHEN 'SPO'  THEN (SELECT ph.hash_raw FROM public.pool_hash ph WHERE ph.id = v.pool_voter)
  ELSE             (SELECT ch.raw FROM public.committee_hash ch WHERE ch.id = v.committee_voter)
END AS voter_hot_hash,
cold.raw AS voter_cold_hash
...
LEFT JOIN LATERAL (
  SELECT ch.raw
  FROM public.committee_registration cr
  JOIN public.committee_hash ch ON ch.id = cr.cold_key_id
  WHERE v.voter_role = 'ConstitutionalCommittee' AND cr.hot_key_id = v.committee_voter
    AND cr.tx_id <= v.tx_id
  ORDER BY cr.tx_id DESC, cr.cert_index DESC LIMIT 1
) cold ON true
```

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test test --test-show-details=direct --test-option='--pattern=threshold_map'` then `psql "$PG_CONN_STR" -v ON_ERROR_STOP=1 -f sql/checks/030_votes.sql`
Expected: PASS both

- [ ] **Step 5: Commit**

```bash
git add sql/010_gov_views.sql sql/checks/030_votes.sql src/test/UnitTests/ThresholdMap.hs palmares.cabal
git commit -m "feat(sql): vote with hot+cold CC credentials and the static threshold_map"
```

---

### Task T-SQL-METRICS-6: delegation, voting power (D-20) and the tally — `vote_delegation`, `live_delegation`, `drep_epoch_delegators`, `drep_epoch_power`, `vote_tally`

**Files:**
- Modify: `sql/010_gov_views.sql` (append), `sql/020_views.sql` (append `drep_epoch_delegators`, then `vote_tally`)
- Test: `sql/checks/040_power.sql`

**Interfaces:**
- Consumes: `palmares.sync_state`, `palmares.drep`, `palmares.vote`, `palmares.action_window`, `palmares.thresholds`, `palmares.threshold_map`.
- Produces: `palmares.vote_delegation (addr_id, stake_address, drep_hash_id, target_kind, tx_hash, slot_no, epoch_no, delegated_at, cert_index)` · `palmares.live_delegation (addr_id, drep_hash_id, from_epoch, from_slot)` · `palmares.drep_epoch_delegators (drep_hash_id, epoch_no, delegator_count)` · `palmares.drep_epoch_power (drep_hash_id, drep_raw, has_script, target_kind, epoch_no, amount, active_until)`, `target_kind ∈ drep | always_abstain | always_no_confidence`, **unfiltered** · `palmares.vote_tally (gov_action_id, voter_role, epoch, yes_power, no_power, abstain_power, yes_count, no_count, abstain_count, ratio, threshold, threshold_param, basis)`

- [ ] **Step 1: Write the failing test**

```sql
-- sql/checks/040_power.sql — D-20. Figures: T-DATA §7.1, T-METRIC §3.7, §3.9, §4.
DO $$
DECLARE d bigint;
BEGIN
  -- one view, no filter: epoch 319 has 195 rows = 193 real DReps + the 2 pseudo-DReps
  ASSERT (SELECT count(*) FROM palmares.drep_epoch_power WHERE epoch_no = 319)                        = 195;
  ASSERT (SELECT count(*) FROM palmares.drep_epoch_power WHERE epoch_no = 319 AND target_kind = 'drep') = 193;
  ASSERT (SELECT amount FROM palmares.drep_epoch_power
          WHERE epoch_no = 319 AND target_kind = 'always_abstain')        = 431863790957959;
  ASSERT (SELECT amount FROM palmares.drep_epoch_power
          WHERE epoch_no = 319 AND target_kind = 'always_no_confidence')  = 325390800383;
  ASSERT (SELECT sum(amount) FROM palmares.drep_epoch_power WHERE epoch_no = 319) = 937803900128796,
         'R4 fails by 46% if the pseudo-DReps are filtered out of the sum';
  -- 101 of 294 registered DReps have no drep_distr row at epoch 319 (and that means zero, not unknown)
  ASSERT (SELECT count(*) FROM palmares.drep dr WHERE dr.registration_status = 'registered'
          AND NOT EXISTS (SELECT 1 FROM palmares.drep_epoch_power p
                          WHERE p.drep_hash_id = dr.drep_hash_id AND p.epoch_no = 319))               = 101;
  -- the fixture DRep: 147 unbroken rows for epochs 174..320, none for its registration epoch 173
  SELECT id INTO d FROM public.drep_hash
    WHERE raw = decode('81378d3ea89b68f0fc09944b1f93f2feb851318f196f98ba4029d8a8','hex');
  ASSERT (SELECT count(*) FROM palmares.drep_epoch_power WHERE drep_hash_id = d)                      = 147;
  ASSERT (SELECT min(epoch_no) FROM palmares.drep_epoch_power WHERE drep_hash_id = d)                 = 174;
  ASSERT (SELECT amount FROM palmares.drep_epoch_power WHERE drep_hash_id = d AND epoch_no = 320)     = 3923709956713;
  ASSERT (SELECT active_until FROM palmares.drep_epoch_power WHERE drep_hash_id = d AND epoch_no = 320) = 333;
  -- delegator counts: live = 13, and the per-epoch view agrees with the live view at the tip epoch
  ASSERT (SELECT count(*) FROM palmares.live_delegation WHERE drep_hash_id = d)                       = 13;
  ASSERT (SELECT delegator_count FROM palmares.drep_epoch_delegators
          WHERE drep_hash_id = d AND epoch_no = (SELECT epoch_no FROM palmares.sync_state))          = 13;
  -- a de-registered stake credential must leave live_delegation even though delegation_vote keeps the row
  ASSERT (SELECT count(*) FROM palmares.live_delegation l
          WHERE EXISTS (SELECT 1 FROM public.stake_deregistration sd
                        WHERE sd.addr_id = l.addr_id AND sd.epoch_no >= l.from_epoch))               = 0;
  ASSERT (SELECT count(DISTINCT target_kind) FROM palmares.vote_delegation)                           = 3;
  -- the tally is over final votes only, and carries one basis
  ASSERT (SELECT sum(yes_count + no_count + abstain_count) FROM palmares.vote_tally WHERE voter_role = 'DRep')
         = (SELECT count(*) FROM palmares.vote WHERE voter_role = 'DRep' AND seq_last = 1);
  ASSERT (SELECT count(DISTINCT basis) FROM palmares.vote_tally) = 1;
  ASSERT (SELECT min(basis) FROM palmares.vote_tally) = 'cast_votes_only';
  -- D-25: every non-null threshold_param is a real column of palmares.thresholds — this is what
  -- catches the 'pvt_pp_security_group' misspelling, without depending on which actions got SPO votes
  ASSERT (SELECT count(*) FROM palmares.vote_tally t WHERE t.threshold_param IS NOT NULL
          AND NOT EXISTS (SELECT 1 FROM information_schema.columns c
                          WHERE c.table_schema = 'palmares' AND c.table_name = 'thresholds'
                            AND c.column_name = t.threshold_param))                                  = 0;
END $$;
```

- [ ] **Step 2: Run test to verify it fails**

Run: `psql "$PG_CONN_STR" -v ON_ERROR_STOP=1 -f sql/checks/040_power.sql`
Expected: FAIL with `ERROR: relation "palmares.drep_epoch_power" does not exist`

- [ ] **Step 3: Implement the four views, then `vote_tally`**

- `drep_epoch_power` = `drep_distr ⋈ drep_hash`, **no `WHERE`**, with
  `target_kind = CASE WHEN dh.raw IS NOT NULL THEN 'drep' WHEN dh.view = 'drep_always_abstain' THEN 'always_abstain' ELSE 'always_no_confidence' END`. Consumers filter; the dashboard reads the two pseudo rows off this same view.
- `vote_delegation` = `delegation_vote ⋈ tx ⋈ block ⋈ stake_address`, same `target_kind`.
- `live_delegation` = T-METRIC §5.4's `vd` + `live` CTEs restricted to the current epoch: latest `delegation_vote`
  per `addr_id`, minus any `stake_deregistration` at or after that delegation's epoch.
- `drep_epoch_delegators` = the same `live` intervals, cross-joined with
  `generate_series((SELECT min(epoch_no) FROM public.drep_distr), (SELECT epoch_no FROM palmares.sync_state))`
  and counted per `(drep_hash_id, epoch_no)`. `ponytail:` comment naming the ceiling from the synthesis cut list:
  promote to a matview only if `/drep/{id}` or `/drep/{id}/power` p95 exceeds 500 ms on preprod.

Then, last in `020_views.sql`: `vote_tally`, **one row per `(gov_action_id, voter_role)`** over
`palmares.vote WHERE seq_last = 1` — the six counters are `count(*) FILTER (WHERE vote = 'Yes'|'No'|'Abstain')`
and the matching `sum(power) FILTER (…)`, which is what T-API's `VoteTally` shape requires (T-DATA's
`(…, vote)` grouping is an alias to delete) — `ratio = yes_power / NULLIF(yes_power + no_power, 0)` (abstain excluded),
`threshold` / `threshold_param` joined through `threshold_map ⋈ thresholds` at the action's
`last_votable_epoch`, and the literal `basis = 'cast_votes_only'`. The three power columns per role:
**DRep** = `palmares.drep_epoch_power WHERE target_kind = 'drep'` at `last_votable_epoch`;
**CC** = `0` (T-API §2.6: "lovelace; 0 for the CC"); **SPO** = `0`
**(OPEN: T-API §2.6 names `pool_stat.voting_power` for SPO power, but no note verifies that `pool_stat` is
populated under `preset: only_governance` + `shelley: enable: true`. Until the caught-up sync answers it
(`SELECT count(*) FROM pool_stat`), SPO rows publish counts with `yes_power = no_power = abstain_power = 0`;
if the table is populated, the API block decides whether to fill them.)**
`ParameterChange`/DRep's `threshold` stays `NULL` in F1: it is a max over the touched parameter groups, which
needs a second static mapping nobody has asked for yet.

- [ ] **Step 4: Run test to verify it passes**

Run: `psql "$PG_CONN_STR" -v ON_ERROR_STOP=1 -f sql/checks/040_power.sql`
Expected: PASS — exit status 0

Run (the CI half): `cabal test test --test-show-details=direct --test-option='--pattern=palmares schema bootstrap'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add sql/010_gov_views.sql sql/020_views.sql sql/checks/040_power.sql
git commit -m "feat(sql): unfiltered drep_epoch_power with target_kind, delegator views and the cast-votes-only tally"
```

---

### Task T-SQL-METRICS-7: `palmares.gov_action`, `palmares.committee_member`, `palmares.treasury_withdrawal`

**Files:**
- Modify: `sql/020_views.sql` (append)
- Test: `sql/checks/050_actions_committee_treasury.sql`

**Interfaces:**
- Consumes: `palmares.action_window`, `palmares.anchor`, `palmares.vote`.
- Produces: `palmares.gov_action (gov_action_id, tx_hash, index, action_type, title, abstract, proposed_epoch, proposed_slot, proposed_at, expiration, last_votable_epoch, state, ratified_epoch, enacted_epoch, expired_epoch, dropped_epoch, deposit, return_address, prev_gov_action_proposal, withdrawal_total, votes_cast, voting_anchor_id)` · `palmares.committee_member (committee_hash_id, cold_hash, cold_has_script, hot_hash, hot_has_script, status, expiration_epoch, committee_id, is_enacted, quorum_numerator, quorum_denominator)` · `palmares.treasury_withdrawal (gov_action_id, stake_address_id, stake_address, amount, enacted_epoch, paid_heuristic)`

- [ ] **Step 1: Write the failing test**

```sql
-- sql/checks/050_actions_committee_treasury.sql — preprod @ epoch 320.
-- Figures: T-DATA §7.1 (CC 2-of-3, members expiring 372, all script creds), T-METRIC §4 lifecycle row.
DO $$ BEGIN
  ASSERT (SELECT count(*) FROM palmares.gov_action)                                      = 131;
  ASSERT (SELECT count(*) FROM palmares.gov_action WHERE state = 'enacted')              = 11;
  ASSERT (SELECT count(*) FROM palmares.gov_action WHERE ratified_epoch IS NOT NULL)     = 11;
  ASSERT (SELECT count(*) FROM palmares.gov_action WHERE expired_epoch IS NOT NULL)      = 103;
  ASSERT (SELECT count(*) FROM palmares.gov_action WHERE dropped_epoch IS NOT NULL)      = 108;
  ASSERT (SELECT count(*) FROM palmares.gov_action
          WHERE dropped_epoch IS NOT NULL AND expired_epoch IS NULL AND ratified_epoch IS NULL) = 6, 'dropped-only';
  -- 131 - 11 enacted - 103 expired - 6 dropped-only = 11, NOT the 12 "still open" of T-METRIC §4:
  -- one action carries expired_epoch = 320, so its state is 'expired' while its window is still open.
  ASSERT (SELECT count(*) FROM palmares.gov_action WHERE state = 'proposed')             = 11;
  ASSERT (SELECT count(*) FROM palmares.gov_action WHERE last_votable_epoch >= 320)      = 12, 'still votable';
  ASSERT (SELECT count(*) FROM palmares.gov_action WHERE state = 'ratified')             = 0, 'all 11 ratified are enacted';
  -- votes_cast is final votes, so it can never exceed the raw vote rows
  ASSERT (SELECT count(*) FROM palmares.gov_action g
          WHERE g.votes_cast <> (SELECT count(*) FROM palmares.vote v
                                 WHERE v.gov_action_id = g.gov_action_id AND v.seq_last = 1)) = 0;
  -- committee: 2-of-3 quorum, 3 members, all script credentials, expiring epoch 372, no duplicate rows (#2155)
  ASSERT (SELECT count(*) FROM palmares.committee_member WHERE is_enacted)               = 3;
  ASSERT (SELECT count(DISTINCT cold_hash) FROM palmares.committee_member WHERE is_enacted) = 3;
  ASSERT (SELECT count(*) FROM palmares.committee_member WHERE is_enacted AND NOT cold_has_script) = 0;
  ASSERT (SELECT count(*) FROM palmares.committee_member WHERE is_enacted AND expiration_epoch <> 372) = 0;
  ASSERT (SELECT DISTINCT quorum_numerator || '/' || quorum_denominator
          FROM palmares.committee_member WHERE is_enacted)                               = '2/3';
  -- every CC voter resolves to a member (this is what makes the hot->cold join in palmares.vote checkable)
  ASSERT (SELECT count(*) FROM palmares.vote v WHERE v.voter_role = 'ConstitutionalCommittee'
          AND NOT EXISTS (SELECT 1 FROM palmares.committee_member m WHERE m.cold_hash = v.voter_cold_hash)) = 0;
  -- treasury: the per-action total matches gov_action.withdrawal_total
  ASSERT (SELECT count(*) FROM palmares.gov_action g WHERE g.action_type = 'TreasuryWithdrawals'
          AND coalesce(g.withdrawal_total, 0) <> (SELECT coalesce(sum(amount), 0) FROM palmares.treasury_withdrawal t
                                                  WHERE t.gov_action_id = g.gov_action_id)) = 0;
END $$;
```

- [ ] **Step 2: Run test to verify it fails**

Run: `psql "$PG_CONN_STR" -v ON_ERROR_STOP=1 -f sql/checks/050_actions_committee_treasury.sql`
Expected: FAIL with `ERROR: relation "palmares.gov_action" does not exist`

- [ ] **Step 3: Implement the three views**

- `gov_action` = `action_window ⋈ gov_action_proposal ⋈ tx ⋈ block ⋈ palmares.anchor ⋈ off_chain_vote_gov_action_data`, with `state` exactly the published `gov_action_state_v1` chain
  (`enacted_epoch → enacted; else expired_epoch → expired; else dropped_epoch → dropped; else ratified_epoch → ratified; else proposed`), `withdrawal_total` summed over **`public.treasury_withdrawal`** grouped by
  `gov_action_proposal_id` — *not* over `palmares.treasury_withdrawal`, which reads `palmares.gov_action` and
  would make the pair a view cycle PostgreSQL rejects — and `votes_cast` counted over
  `palmares.vote WHERE seq_last = 1`.
- `committee_member` = `committee_member ⋈ committee_hash ⋈ committee`, `is_enacted` from the **max-`id`** row per
  `epoch_state.epoch_no` (`DISTINCT`/max picks one row: issue #2155 can leave duplicates on 13.7.1.0), hot key
  resolved through `committee_registration` and `status` demoted to `resigned` by `committee_de_registration`. One
  `ponytail:` comment stating that this view subsumes spec §6's `committee_vote_auth` — **no extra view**.
- `treasury_withdrawal` = `treasury_withdrawal ⋈ stake_address ⋈ gov_action`, LEFT JOIN `reward_rest`
  (`type = 'treasury'`) on `(stake_address_id, spendable_epoch)`; that column is named **`paid_heuristic`** because
  `reward_rest` has no gov-action key and the match is ambiguous when one address receives two withdrawals in one
  epoch.

- [ ] **Step 4: Run test to verify it passes**

Run: `psql "$PG_CONN_STR" -v ON_ERROR_STOP=1 -f sql/checks/050_actions_committee_treasury.sql`
Expected: PASS — exit status 0

Run (the CI half): `cabal test test --test-show-details=direct --test-option='--pattern=palmares schema bootstrap'`
Expected: PASS

- [ ] **Step 5: Commit**

```bash
git add sql/020_views.sql sql/checks/050_actions_committee_treasury.sql
git commit -m "feat(sql): gov_action lifecycle, committee hot-cold resolution and treasury withdrawals"
```

---

### Task T-SQL-METRICS-8: `palmares.activity` — eleven arms, thirteen kinds, a total cursor (D-19)

**Files:**
- Modify: `sql/020_views.sql` (append)
- Test: `src/test/UnitTests/Activity.hs`, `sql/checks/060_activity.sql`
- Modify: `palmares.cabal` (test-suite `other-modules: UnitTests.Activity`)

**Interfaces:**
- Consumes: `palmares.drep_cert`, `palmares.vote`, `palmares.vote_delegation`, `palmares.gov_action`, `palmares.treasury_withdrawal`.
- Produces: `palmares.activity (event_type, source, sort_slot, tx_index, item_index, slot_no, epoch_no, block_time, tx_hash, actor_id, target_id)`; `event_type` is the thirteen governance values of `ActivityEventType`; `source = 'governance'`.

- [ ] **Step 1: Write the failing test**

```haskell
-- src/test/UnitTests/Activity.hs — D-19. Seeded, because the property under test is the cursor's
-- totality, and a seeded set can contain two events in one slot and two epoch-boundary events in one epoch.
tests :: TestTree
tests = testGroup "activity feed"
  [ testCase "all thirteen governance kinds are produced" $ withScratchDb $ \pool -> do
      applyPalmaresSchema pool
      runPg pool $ rawExecute seedActivity []
      ks <- runPg pool $ rawSql "SELECT DISTINCT event_type FROM palmares.activity ORDER BY 1" []
      map unSingle ks @?= [ "committee_authorized", "committee_resigned", "delegation_changed"
                          , "drep_registered", "drep_retired", "drep_updated"
                          , "gov_action_dropped", "gov_action_enacted", "gov_action_expired"
                          , "gov_action_proposed", "gov_action_ratified"
                          , "treasury_withdrawal_enacted", "vote_cast" ]
  , testCase "the cursor triple is unique over the whole feed" $ withScratchDb $ \pool -> do
      applyPalmaresSchema pool
      runPg pool $ rawExecute seedActivity []
      [Single (dups :: Int)] <- runPg pool $ rawSql
        "SELECT count(*)::int FROM (SELECT 1 FROM palmares.activity \
        \GROUP BY sort_slot, tx_index, item_index HAVING count(*) > 1) x" []
      dups @?= 0
  , testCase "epoch-boundary events carry a negative tx_index, a null slot and a null tx_hash" $ withScratchDb $ \pool -> do
      applyPalmaresSchema pool
      runPg pool $ rawExecute seedActivity []
      [Single (bad :: Int)] <- runPg pool $ rawSql
        "SELECT count(*)::int FROM palmares.activity WHERE event_type IN \
        \('gov_action_ratified','gov_action_enacted','gov_action_expired','gov_action_dropped','treasury_withdrawal_enacted') \
        \AND (tx_index NOT IN (-1, -2) OR slot_no IS NOT NULL OR tx_hash IS NOT NULL)" []
      bad @?= 0
  , testCase "an enacted treasury withdrawal and its action's enactment do not share a cursor" $ withScratchDb $ \pool -> do
      applyPalmaresSchema pool
      runPg pool $ rawExecute seedActivity []   -- seeds one enacted TreasuryWithdrawals action
      [Single (ix :: Int)] <- runPg pool $ rawSql
        "SELECT tx_index FROM palmares.activity WHERE event_type = 'treasury_withdrawal_enacted'" []
      ix @?= (-2)
  , testCase "an epoch-boundary event's time is the first block of its epoch" $ withScratchDb $ \pool -> do
      applyPalmaresSchema pool
      runPg pool $ rawExecute seedActivity []
      [Single (t :: UTCTime)] <- runPg pool $ rawSql
        "SELECT block_time FROM palmares.activity WHERE event_type = 'gov_action_ratified'" []
      t @?= read "2026-10-18 00:00:00 UTC"   -- MIN(block.id) WHERE epoch_no = 320 in seedActivity
  ]
```

```sql
-- sql/checks/060_activity.sql — the same invariants against the real feed.
DO $$ BEGIN
  ASSERT (SELECT count(*) FROM (SELECT 1 FROM palmares.activity
          GROUP BY sort_slot, tx_index, item_index HAVING count(*) > 1) x) = 0, 'cursor must be total';
  -- the thirteen kinds are asserted on the seeded set, not here: nothing measures whether preprod has a
  -- committee_de_registration row, so a live DISTINCT = 13 would be an unfalsifiable gate.
  ASSERT (SELECT count(*) FROM palmares.activity WHERE event_type = 'gov_action_proposed')  = 131;
  ASSERT (SELECT count(*) FROM palmares.activity WHERE event_type = 'gov_action_ratified')  = 11;
  ASSERT (SELECT count(*) FROM palmares.activity WHERE event_type = 'gov_action_enacted')   = 11;
  ASSERT (SELECT count(*) FROM palmares.activity WHERE event_type = 'gov_action_expired')   = 103;
  -- ASSEMBLY CORRECTION 1 (2026-10-19, f1-yaci-store-evaluation.md §4.1 C1): 6, NOT 108.
  -- `dropped_epoch` is not an independent outcome: on expiry db-sync sets it to `expired_epoch + 1`,
  -- so 102 of the 108 rows carrying it are expired actions that already emit `gov_action_expired`.
  -- Emitting `gov_action_dropped` for them duplicates the expiry event one epoch later. Only the
  -- pruned-sibling case is a genuine drop.
  ASSERT (SELECT count(*) FROM palmares.activity WHERE event_type = 'gov_action_dropped')   = 6;
  ASSERT (SELECT count(*) FROM palmares.activity WHERE event_type = 'vote_cast')
         = (SELECT count(*) FROM palmares.vote);
  ASSERT (SELECT count(*) FROM palmares.activity WHERE source <> 'governance') = 0;
END $$;
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test test --test-show-details=direct --test-option='--pattern=activity feed'`
Expected: FAIL with `relation "palmares.activity" does not exist`

- [ ] **Step 3: Implement `palmares.activity` as a `UNION ALL` of eleven arms**

Six tx-borne arms — `drep_cert` (one arm, three kinds off `cert_kind`), `vote` → `vote_cast`,
`gov_action` → `gov_action_proposed`, `vote_delegation` → `delegation_changed`, `committee_registration` →
`committee_authorized`, `committee_de_registration` → `committee_resigned` — all with
`sort_slot = block.slot_no`, `tx_index = tx.block_index`, and `item_index` = that arm's own per-tx index:
`voting_procedure.index` for `vote_cast`, `gov_action_proposal.index` for `gov_action_proposed`, `cert_index`
for the four certificate arms.

Five epoch-boundary arms (`gov_action_ratified`, `gov_action_enacted`, `gov_action_expired`,
`gov_action_dropped`, `treasury_withdrawal_enacted`) that db-sync records only as `*_epoch` columns, so:

> **ASSEMBLY CORRECTION 1** (2026-10-19, `f1-yaci-store-evaluation.md` §4.1 C1 — supersedes synthesis
> D-19's unqualified event list). The `gov_action_dropped` arm **must carry the restricted predicate**
> `WHERE dropped_epoch IS NOT NULL AND ratified_epoch IS NULL AND enacted_epoch IS NULL AND expired_epoch IS NULL`
> (6 rows on preprod today, not 108). Reason: db-sync's `doc/schema.md` line 910 is stale per its own
> maintainer (issue #1932); CHANGELOG 13.6.0.1 — *"Enacted gov_action_proposal are no longer marked as
> dropped"* — and on **expiry** `dropped_epoch` is set to `expired_epoch + 1`. The other four arms are
> unchanged, `close_epoch = LEAST(ratified, expired, dropped)` is unchanged (`LEAST` still picks the
> smaller `expired_epoch`), and the arm count stays five.

```sql
-- one arm; the other three lifecycle arms are identical bar the column and the event_type string,
-- and treasury_withdrawal_enacted differs only in tx_index = -2 (see below)
SELECT 'gov_action_ratified'            AS event_type,
       'governance'                     AS source,
       eb.first_slot                    AS sort_slot,
       -1                               AS tx_index,       -- sorts before every tx in that epoch
       g.gov_action_id                  AS item_index,     -- gov_action_proposal.id
       NULL::bigint                     AS slot_no,        -- ChainPoint.slot is null for these
       g.ratified_epoch                 AS epoch_no,
       eb.first_time                    AS block_time,     -- block.time of MIN(block.id) in that epoch
       NULL::text                       AS tx_hash,
       ...
FROM palmares.gov_action g
JOIN LATERAL (SELECT b.slot_no AS first_slot, b.time AS first_time FROM public.block b
              WHERE b.epoch_no = g.ratified_epoch ORDER BY b.id LIMIT 1) eb ON true
WHERE g.ratified_epoch IS NOT NULL
```

`item_index = gov_action_proposal.id` is what keeps the triple unique when two actions ratify in one epoch, and
the four lifecycle arms cannot collide with each other because an action reaches at most one of
ratified/enacted/expired/dropped per epoch (preprod: `enacted = ratified + 1`, `dropped = expired + 1`).
`treasury_withdrawal_enacted` **does** collide — it fires in the *same* epoch as `gov_action_enacted` for the same
`gov_action_proposal.id` — so that one arm alone uses **`tx_index = -2`**, which still sorts before every tx in
the epoch and restores totality. This is the one place the plan diverges from T-API §2.1's flat `-1`; the API
block's cursor parser must accept both.

**(OPEN: `voting_procedure.index`, `gov_action_proposal.index` and `cert_index` are three independent per-tx
counters, so a single tx carrying both a vote and a certificate yields the same `(sort_slot, tx_index, item_index)`
triple. Nothing in the notes measures whether preprod contains such a tx; the uniqueness ASSERT in
`sql/checks/060_activity.sql` is what detects it. If it fires, the cursor needs a fourth component (an arm
ordinal) — that is T-API's call, because the API block parses and emits the cursor string.)**

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test test --test-show-details=direct --test-option='--pattern=activity feed'` then `psql "$PG_CONN_STR" -v ON_ERROR_STOP=1 -f sql/checks/060_activity.sql`
Expected: PASS both

- [ ] **Step 5: Commit**

```bash
git add sql/020_views.sql sql/checks/060_activity.sql src/test/UnitTests/Activity.hs palmares.cabal
git commit -m "feat(sql): activity feed unions the five epoch-boundary kinds with a total cursor"
```

---

### Task T-SQL-METRICS-9: the metric catalog — `resources/metric-formulas.json`, `resources/metrics/*.sql`, `Palmares.Metrics.Catalog`

**Files:**
- Create: `resources/metric-formulas.json`
- Create: `resources/metrics/participation_rate_v1.sql`, `rationale_rate_v1.sql`, `age_days_v1.sql`, `delegator_retention_rate_v1.sql`, `vote_latency_median_hours_v1.sql`, `votes_by_type_v1.sql`
- Create: `src/lib/offchain-lib/Palmares/Metrics/Catalog.hs`
- Test: `src/test/UnitTests/MetricCatalog.hs`
- Modify: `palmares.cabal` (`extra-source-files: resources/metric-formulas.json`, `resources/metrics/*.sql`; `offchain-lib` `exposed-modules: Palmares.Metrics.Catalog` + `build-depends: file-embed` — DELTA, it was `mcp-server-lib`'s in the Belt System; test-suite `other-modules: UnitTests.MetricCatalog`)

**Interfaces:**
- Consumes: `palmares.eligible_actions`, `palmares.vote`, `palmares.action_window`, `palmares.drep_interval`, `palmares.live_delegation`.
- Produces: `catalogJson :: ByteString` · `metricSql :: [(FilePath, ByteString)]` · `MetricFormula` (fields `formula_id, name, field, expression, denominator, null_when, inputs, edge_cases, sql_file, sql_object, params, spec, version`; accessors `mfFormulaId`, `mfSqlFile`, `mfParams`, …) · `metricCatalog :: Either String [MetricFormula]` · `lookupMetricSql :: Text -> Maybe ByteString` · `retentionWindowEpochs :: Int` (= 10) · the eight `formula_id`s

- [ ] **Step 1: Write the failing test**

```haskell
-- src/test/UnitTests/MetricCatalog.hs — the anti-drift gate (D-9). Three cheap tests.
tests :: TestTree
tests = testGroup "metric catalog"
  -- eight = T-API §2.5's seven published ids + votes_by_type_v1, which T-METRIC §7 ships as a .sql file
  -- and T-API §2.5 omits from its table while DRepMetrics carries the field.
  [ testCase "catalog parses and holds exactly the eight published formula ids" $ do
      fs <- either assertFailure' pure metricCatalog
      map mfFormulaId fs @?= [ "participation_rate_v1", "rationale_rate_v1", "age_days_v1"
                           , "delegator_retention_rate_v1", "vote_latency_median_hours_v1"
                           , "votes_by_type_v1", "gov_action_state_v1", "tally_cast_votes_v1" ]
      retentionWindowEpochs @?= 10
  , testCase "every sql_file resolves to an embedded file, and every embedded file is in the catalog" $ do
      fs <- either assertFailure' pure metricCatalog
      let referenced = [f | Just f <- map mfSqlFile fs]
      sort referenced @?= sort (map fst metricSql)
      mapM_ (\f -> assertBool (f <> " is embedded") (isJust (lookupMetricSql (pack f)))) referenced
  , testCase "every embedded statement parses and resolves against the real view set" $ withScratchDb $ \pool -> do
      applyPalmaresSchema pool
      fs <- either assertFailure' pure metricCatalog
      -- rawExecute, not PREPARE: the files carry persistent's `?` placeholders, which postgresql-simple
      -- interpolates client-side, and PostgreSQL's PREPARE only accepts $n. Executing against the empty
      -- skeleton with dummy params is what proves every table, column and function name resolves.
      forM_ [(f, s) | f <- fs, Just n <- [mfSqlFile f], Just s <- [lookupMetricSql (pack n)]] $ \(f, s) ->
        runPg pool $ rawExecute (decodeUtf8 s) (dummyParams (mfParams f))
  ]

-- one dummy per declared param: 0 for the ids and epochs, retentionWindowEpochs for the window
dummyParams :: [Text] -> [PersistValue]
dummyParams = map (\p -> if p == "retention_window_epochs"
                           then toPersistValue retentionWindowEpochs
                           else PersistInt64 0)
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test test --test-show-details=direct --test-option='--pattern=metric catalog'`
Expected: FAIL with `Could not find module 'Palmares.Metrics.Catalog'`

- [ ] **Step 3: Implement the catalog**

Six `.sql` files, each T-METRIC §5.1/§5.2/§5.3/§5.4/§5.5/§5.6 **with `_common.sql` replaced by the real objects**
(D-15): the `elig` CTE becomes `FROM palmares.eligible_actions(?, ?)`, `drep_vote` becomes
`palmares.vote WHERE voter_role = 'DRep'`, `action_window` and `drep_interval` are read as views. `rationale_rate_v1`
grades **five** ways (D-3) — `verified | inline | unverified | pending | absent`, numerator `verified + inline`,
`inline` being `voting_anchor.url LIKE 'data:%'` — and the five counters sum to `votes_cast` by construction
because `final` joins `eligible_actions`. `age_days_v1` keeps §5.3's `b2.slot_no > i.from_slot` retire bound.

Named parameters are positional `?` in the files, in the order the catalog's `params` array names them
(`["drep_hash_id", "as_of_epoch"]`, plus `"retention_window_epochs"` for `delegator_retention_rate_v1`).
**(OPEN: the notes write `:drep` / `:as_of_epoch` / `:w` but never fix a binding mechanism; persistent's `rawSql`
is positional, so the plan uses `?` + a `params` array. Confirm before `Query/Metrics.hs` is written.)**

`Palmares/Metrics/Catalog.hs`: `catalogJson` and each entry of `metricSql` is its own
`$(makeRelativeToProject "resources/…" >>= embedFile)` — **one `embedFile` per file, never `embedDir`**, because
`embedDir` only depends on the files it found, so a newly added `.sql` would pass CI against a stale embed.
`metricCatalog` is `eitherDecodeStrict' catalogJson` over a `deriving-aeson` record with
`StripPrefix "mf" + CamelToSnake` over fields named `mfFormulaId`/`mfName`/`mfField`/`mfSqlFile`/`mfSqlObject`/`mfParams`/… (stripping `"formula"` would emit `id`, not `formula_id`). `(decision)` the module lives in `offchain-lib`, not in an exe, because
both `admin write-metrics-doc` and query-api read it; synthesis §1.3 names the file but not its library.

Catalog entries carry T-METRIC §7's exact field set. The two view-backed formulas (`gov_action_state_v1`,
`tally_cast_votes_v1`) have `"sql_file": null` and a `"sql_object"` naming `palmares.gov_action` /
`palmares.vote_tally` **(OPEN: field not in the notes; needed because every DTO `formula_id` must resolve)**.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test test --test-show-details=direct --test-option='--pattern=metric catalog'`
Expected: PASS (3 tests)

- [ ] **Step 5: Commit**

```bash
git add resources/metric-formulas.json resources/metrics src/lib/offchain-lib/Palmares/Metrics/Catalog.hs src/test/UnitTests/MetricCatalog.hs palmares.cabal
git commit -m "feat(metrics): file-embedded formula catalog with the executed SQL as its source of truth"
```

---

### Task T-SQL-METRICS-10: `admin write-metrics-doc` generates `docs/metrics.md`, and CI fails on drift

**Files:**
- Modify: `src/exe/admin/Main.hs` (a `WriteMetricsDoc FilePath` constructor on `Command`, a `command` entry in `commandParser`, the pre-provider `case cmd of` branch in `main`, and the two `pure () -- Handled in main before provider loading` cases — exactly `WriteBlueprint`'s four sites)
- Create: `src/lib/offchain-lib/Palmares/Metrics/Doc.hs`
- Create: `docs/metrics.md` (generated, committed)
- Test: `src/test/UnitTests/MetricDoc.hs`
- Modify: `palmares.cabal` (`offchain-lib` `exposed-modules: Palmares.Metrics.Doc`; test-suite `other-modules: UnitTests.MetricDoc`), `.github/workflows/ci.yml` (drift step in the `haskell` job, after `cabal test all`)

**Interfaces:**
- Consumes: `metricCatalog`, `lookupMetricSql`, `MetricFormula` and its `mf*` accessors.
- Produces: `Palmares.Metrics.Doc.renderMetricsDoc :: [MetricFormula] -> Text` · `admin write-metrics-doc --output docs/metrics.md` · the committed `docs/metrics.md` · CI gate `git diff --exit-code docs/metrics.md`

- [ ] **Step 1: Write the failing test**

```haskell
-- src/test/UnitTests/MetricDoc.hs
tests :: TestTree
tests = testGroup "metrics doc"
  [ testCase "the committed docs/metrics.md is exactly what the catalog renders" $ do
      fs <- either assertFailure' pure metricCatalog
      committed <- T.readFile "docs/metrics.md"
      renderMetricsDoc fs @?= committed
  , testCase "every formula's SQL is inlined in the doc, so prose cannot drift from code" $ do
      fs <- either assertFailure' pure metricCatalog
      let doc = renderMetricsDoc fs
      forM_ [s | f <- fs, Just n <- [mfSqlFile f], Just s <- [lookupMetricSql (pack n)]] $ \s ->
        assertBool "sql inlined" (decodeUtf8 s `T.isInfixOf` doc)
  ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test test --test-show-details=direct --test-option='--pattern=metrics doc'`
Expected: FAIL with `docs/metrics.md: openFile: does not exist`

- [ ] **Step 3: Implement**

`renderMetricsDoc` emits one `##` section per catalog entry in catalog order — name, `field`, `expression`,
`denominator`, `null_when`, `inputs`, `edge_cases`, then the SQL in a fenced block — preceded by a generated-file
banner naming `admin write-metrics-doc`. The admin command mirrors `write-blueprint` exactly: `WriteMetricsDoc <$> strOption (long "output" <> short 'o' <> value "docs/metrics.md" <> showDefault)`, handled in `main`'s
pre-provider branch — **no provider, no signing key, no `DEPLOYED_VALIDATORS_CONFIG`**, which is what lets CI run
it. `ci.yml` gains, after `cabal test all`:
`cabal run exe:admin -- write-metrics-doc && git diff --exit-code docs/metrics.md`.

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal run exe:admin -- write-metrics-doc && cabal test test --test-show-details=direct --test-option='--pattern=metrics doc' && git diff --exit-code docs/metrics.md`
Expected: PASS (2 tests), and `git diff` exits 0

- [ ] **Step 5: Commit**

```bash
git add src/exe/admin/Main.hs src/lib/offchain-lib/Palmares/Metrics/Doc.hs docs/metrics.md src/test/UnitTests/MetricDoc.hs palmares.cabal .github/workflows/ci.yml
git commit -m "feat(admin): write-metrics-doc generates docs/metrics.md, CI fails on drift"
```

---

### Task T-SQL-METRICS-11: the epoch-320 fixture test, with the cursor block pinned by hash + slot + `block.time`

**Files:**
- Create: `test/fixtures/preprod-epoch-320-metrics.json`
- Test: `src/test/UnitTests/MetricFixture.hs`
- Modify: `palmares.cabal` (test-suite `other-modules: UnitTests.MetricFixture`)

**Interfaces:**
- Consumes: `metricCatalog`, `lookupMetricSql`, `retentionWindowEpochs`, every view of T-2…T-8, `Test.Pg.runPg`.
- Produces: `test/fixtures/preprod-epoch-320-metrics.json` — the regression gate on S2 · `withFixtureDb :: (ConnectionPool -> Fixture -> IO a) -> IO a` and `runMetric :: ConnectionPool -> Text -> [PersistValue] -> IO [row]` (both local to `UnitTests/MetricFixture.hs`) · `PALMARES_FIXTURE_PG_CONN_STR` **(OPEN: name is mine; the notes do not name the connection used for the parity run. Owner decides; it points at the caught-up preprod db-sync, so this test never runs in CI.)**

- [ ] **Step 1: Write the failing test**

```json
// test/fixtures/preprod-epoch-320-metrics.json  (values: T-METRIC §4, post-fact-check)
{
  "cursor": {
    "epoch_no": 320,
    "block_hash": "(OPEN: read the epoch-320 cursor block's hash from our own db-sync on the first caught-up run)",
    "slot_no": "(OPEN: same)",
    "block_time": "(OPEN: same — age_days moves inside an epoch, which is how the old 1463 went unnoticed)"
  },
  "drep": {
    "cip129": "drep1y2qn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qmwj3xg",
    "hex": "81378d3ea89b68f0fc09944b1f93f2feb851318f196f98ba4029d8a8",
    "has_script": false,
    "registered_at": "2024-10-16T20:38:10Z",
    "registered_epoch": 173,
    "deposit": 500000000
  },
  "network": { "proposals": 131, "settled": 119, "settled_drep_votable": 115, "open": 12 },
  "metrics": {
    "as_of_epoch": 320,
    "age_days": 732,
    "age_days_tolerance": 1,
    "eligible_actions": 114,
    "votes_cast": 16,
    "participation_rate": 0.140351,
    "rationale_verified": 2,
    "rationale_inline": 0,
    "_comment_rationale": "(OPEN: only rationale_verified = 2 and rationale_rate = 0.125 are measured (T-METRIC §4). inline = 0 is inferred from 'no data: URI on this DRep's votes' and is not measured; pin inline/unverified/pending/absent from the first caught-up run, in the same commit as the cursor values.)",
    "rationale_rate": 0.125,
    "vote_latency_median_hours": 158.04,
    "vote_latency_p90_hours": 543.16,
    "vote_latency_sample": 16,
    "open_actions_unvoted": 12,
    "voting_power": 3923709956713,
    "voting_power_epoch": 320,
    "delegator_count": 13,
    "is_active": true,
    "active_until_epoch": 333,
    "final_votes": { "yes": 7, "no": 8, "abstain": 1 }
  },
  "latency_sample_hours": [1.29, 1.34, 50.27, 53.07, 75.32, 76.01, 151.30, 155.68,
                           160.41, 242.83, 304.53, 385.73, 459.36, 512.35, 573.98, 630.08],
  "votes_by_type": [
    { "action_type": "parameter_change",     "eligible_actions": 11, "votes_cast": 4, "yes": 2, "no": 1, "abstain": 1, "participation_rate": 0.3636 },
    { "action_type": "hard_fork_initiation", "eligible_actions":  2, "votes_cast": 1, "yes": 1, "no": 0, "abstain": 0, "participation_rate": 0.5000 },
    { "action_type": "treasury_withdrawals", "eligible_actions": 79, "votes_cast": 6, "yes": 1, "no": 5, "abstain": 0, "participation_rate": 0.0759 },
    { "action_type": "no_confidence",        "eligible_actions":  1, "votes_cast": 0, "yes": 0, "no": 0, "abstain": 0, "participation_rate": 0.0000 },
    { "action_type": "new_committee",        "eligible_actions":  7, "votes_cast": 0, "yes": 0, "no": 0, "abstain": 0, "participation_rate": 0.0000 },
    { "action_type": "new_constitution",     "eligible_actions":  1, "votes_cast": 0, "yes": 0, "no": 0, "abstain": 0, "participation_rate": 0.0000 },
    { "action_type": "info_action",          "eligible_actions": 13, "votes_cast": 5, "yes": 3, "no": 2, "abstain": 0, "participation_rate": 0.3846 }
  ],
  "pseudo_dreps": {
    "epoch_no": 319,
    "always_abstain_lovelace": 431863790957959,
    "always_no_confidence_lovelace": 325390800383,
    "total_lovelace": 937803900128796,
    "absent_from_dreps": true
  }
}
```

```haskell
-- src/test/UnitTests/MetricFixture.hs — runs the CATALOG SQL, not a second copy of the arithmetic.
tests :: TestTree
tests = testGroup "preprod epoch-320 fixture"
  [ testCase "the cursor block is still the block the fixture pinned" $ withFixtureDb $ \pool fx -> do
      -- the whole reason the cursor is pinned: age_days moves inside an epoch
      [(Single h, Single s, Single t)] <- runPg pool $ rawSql
        "SELECT encode(b.hash,'hex'), b.slot_no, b.time FROM public.block b \
        \WHERE b.hash = decode(?,'hex')" [toPersistValue (cursorBlockHash fx)]
      (h, s, t) @?= (cursorBlockHash fx, cursorSlotNo fx, cursorBlockTime fx)
  , testCase "participation_rate_v1 reproduces 16 / 114 = 0.140351" $ withFixtureDb $ \pool fx -> do
      [(Single elig, Single cast, Single rate, Single openUnvoted)] <-
        runMetric pool "participation_rate_v1" [drepId fx, 320]
      (elig, cast, openUnvoted) @?= (114 :: Int, 16 :: Int, 12 :: Int)
      fmap (roundTo 6) rate @?= Just (0.140351 :: Double)
  , testCase "rationale_rate_v1's five grades sum to votes_cast and the rate is 0.125" $ withFixtureDb $ \pool fx -> do
      [(Single v, Single inl, Single unv, Single pend, Single abs_, Single rate)] <-
        runMetric pool "rationale_rate_v1" [drepId fx, 320]
      (v, inl) @?= (2 :: Int, 0 :: Int)
      v + inl + unv + pend + abs_ @?= (16 :: Int)
      fmap (roundTo 3) rate @?= Just (0.125 :: Double)
  , testCase "age_days_v1 is 732 +/- 1 against the pinned cursor" $ withFixtureDb $ \pool fx -> do
      [Single (d :: Double)] <- runMetric pool "age_days_v1" [drepId fx, 320]
      assertBool (show d <> " outside 732 +/- 1") (abs (d - 732) <= 1)
  , testCase "vote_latency_median_hours_v1 = 158.04 and p90 = 543.16 (percentile_cont, not _disc)" $
      withFixtureDb $ \pool fx -> do
        [(Single med, Single p90, Single n)] <- runMetric pool "vote_latency_median_hours_v1" [drepId fx, 320]
        (roundTo 2 med, roundTo 2 p90, n) @?= (158.04 :: Double, 543.16 :: Double, 16 :: Int)
  , testCase "votes_by_type_v1 emits all 7 types and the columns sum to the headline figures" $
      withFixtureDb $ \pool fx -> do
        rows <- runMetric pool "votes_by_type_v1" [drepId fx, 320]
        length rows             @?= 7
        sum (map eligible rows) @?= (114 :: Int)
        sum (map cast rows)     @?= (16 :: Int)
        sortOn actionType rows  @?= sortOn actionType (votesByType fx)
  , testCase "delegator_retention_rate_v1 binds retention_window_epochs = 10, not a literal" $
      withFixtureDb $ \pool fx -> do
        retentionWindowEpochs @?= 10
        [(Single now_, Single _prev, Single _retained)] <-
          runMetric pool "delegator_retention_rate_v1" [drepId fx, 320, toPersistValue retentionWindowEpochs]
        now_ @?= (13 :: Int)
        -- W is bound, not inlined: with the window set to 0, "previous" must equal today's count
        [(Single now0, Single prev0, Single _)] <-
          runMetric pool "delegator_retention_rate_v1" [drepId fx, 320, toPersistValue (0 :: Int)]
        (now0, prev0) @?= (13 :: Int, 13 :: Int)
  , testCase "the pseudo-DReps carry the dashboard figures and are absent from palmares.drep" $
      withFixtureDb $ \pool _ -> do
        [(Single abstain, Single noConf)] <- runPg pool $ rawSql
          "SELECT sum(amount) FILTER (WHERE target_kind = 'always_abstain'), \
          \       sum(amount) FILTER (WHERE target_kind = 'always_no_confidence') \
          \FROM palmares.drep_epoch_power WHERE epoch_no = 319" []
        (abstain, noConf) @?= (431863790957959 :: Integer, 325390800383 :: Integer)
        [Single (listed :: Int)] <- runPg pool $ rawSql
          "SELECT count(*)::int FROM palmares.drep WHERE drep_raw IS NULL" []
        listed @?= 0
  ]
```

`roundTo`, `eligible`, `cast`, `actionType` and the `Fixture` accessors (`cursorBlockHash`, `cursorSlotNo`,
`cursorBlockTime`, `drepId`, `votesByType`) are this module's own one-line helpers over the parsed fixture.

- [ ] **Step 2: Run test to verify it fails**

Run: `PALMARES_FIXTURE_PG_CONN_STR="$PG_CONN_STR" cabal test test --test-show-details=direct --test-option='--pattern=epoch-320'`
Expected: FAIL with `test/fixtures/preprod-epoch-320-metrics.json: openFile: does not exist`

- [ ] **Step 3: Fill the three `(OPEN: …)` cursor values from the caught-up db-sync and implement the loader**

One query writes the cursor block into the fixture — `SELECT encode(hash,'hex'), slot_no, time FROM public.block
WHERE epoch_no = 320 ORDER BY id DESC LIMIT 1` — and `age_days` is then recomputed against that exact
`block.time`; if it lands outside `732 ± 1`, the cursor value, not the formula, is what moves. `runMetric` looks
the statement up by `formula_id` through `lookupMetricSql` and binds the catalog's `params` in order: there is no
second copy of any formula in the test. `withFixtureDb` opens a pool on `PALMARES_FIXTURE_PG_CONN_STR` (`die`ing
with the variable's name when it is unset, so a skipped run can never read as a pass), decodes
`test/fixtures/preprod-epoch-320-metrics.json` into `Fixture`, and hands both to the body — it never creates or
drops anything, because the fixture DB is the live preprod db-sync.

- [ ] **Step 4: Run test to verify it passes**

Run: `PALMARES_FIXTURE_PG_CONN_STR="$PG_CONN_STR" cabal test test --test-show-details=direct --test-option='--pattern=epoch-320'`
Expected: PASS (8 tests)

- [ ] **Step 5: Commit**

```bash
git add test/fixtures/preprod-epoch-320-metrics.json src/test/UnitTests/MetricFixture.hs palmares.cabal
git commit -m "test(metrics): epoch-320 fixture pinned by cursor hash, slot and block time"
```

---

### Task T-SQL-METRICS-12: the one materialized view `palmares.drep_metrics`, and the one refresh thread

**Files:**
- Create: `sql/030_metrics.sql`
- Create: `src/exe/query-api/Query/Refresh.hs`
- Test: `src/test/UnitTests/Refresh.hs`, `sql/checks/070_drep_metrics.sql`
- Modify: `palmares.cabal` (query-api `other-modules: Query.Refresh`; test-suite `other-modules: Query.Refresh, UnitTests.Refresh` — it compiles out of the `src/exe/query-api` source dir T-1 added), `.env.example`

**Interfaces:**
- Consumes: `palmares.eligible_actions`, `palmares.vote`, `palmares.anchor`, `palmares.live_delegation`, `palmares.drep_epoch_power`, `palmares.drep_interval`, `palmares.sync_state`, `palmaresSqlFiles`.
- Produces: `palmares.drep_metrics` (columns: `drep_hash_id, drep_raw, has_script, eligible_actions, votes_cast, participation_rate, rationale_verified, rationale_inline, rationale_unverified, rationale_pending, rationale_absent, rationale_rate, vote_latency_median_hours, vote_latency_p90_hours, vote_latency_sample, age_days, voting_power, voting_power_epoch, delegator_count, open_actions_unvoted, is_active, active_until_epoch, last_vote_at, last_vote_epoch, as_of_epoch`) · indexes `palmares_idx_drep_metrics_{pk,part,power,raw}` · `RefreshMark` · `shouldRefresh :: Maybe RefreshMark -> RefreshMark -> Bool` · `refreshDRepMetrics :: ConnectionPool -> IO (Either SomeException ())` · `runRefreshThread :: ConnectionPool -> IO ThreadId` · `PALMARES_VIEW_REFRESH_SECONDS` (default 300)
- Column names are synthesis §1.4's canonical set; T-DATA's `eligible_closed_actions` / `voted_closed_actions` / `votes_with_anchor` / `avg_vote_latency_seconds` / `latest_power_*` / `live_delegator_count` / `epochs_with_power` are aliases to delete on sight.

- [ ] **Step 1: Write the failing test**

```haskell
-- src/test/UnitTests/Refresh.hs
tests :: TestTree
tests = testGroup "drep_metrics refresh"
  [ testCase "refreshes on the first tick, then only when the tip mark changes" $ do
      shouldRefresh Nothing            (RefreshMark 100 320) @?= True   -- never refreshed
      shouldRefresh (Just (RefreshMark 100 320)) (RefreshMark 100 320) @?= False  -- tip stood still
      shouldRefresh (Just (RefreshMark 100 320)) (RefreshMark 101 320) @?= True   -- tip advanced
      shouldRefresh (Just (RefreshMark 100 320)) (RefreshMark 100 321) @?= True   -- epoch boundary: the
      -- lifecycle *_epoch columns and drep_distr only move here, and nothing new is inserted
      shouldRefresh (Just (RefreshMark 100 320)) (RefreshMark  97 320) @?= True   -- rollback: mark went back
  , testCase "the matview exists, is populated at boot, and REFRESH CONCURRENTLY works on it" $
      withScratchDb $ \pool -> do
        applyPalmaresSchema pool          -- sql/030 creates it WITH DATA, inside the boot transaction
        Right () <- refreshDRepMetrics pool
        [Single (n :: Int)] <- runPg pool $ rawSql
          "SELECT count(*)::int FROM pg_matviews WHERE schemaname='palmares' AND matviewname='drep_metrics'" []
        n @?= 1
  , testCase "a failed refresh is returned, never thrown past the loop" $ withScratchDb $ \pool -> do
      applyPalmaresSchema pool
      runPg pool $ rawExecute "DROP MATERIALIZED VIEW palmares.drep_metrics" []   -- what a db-sync migration CASCADE does
      r <- refreshDRepMetrics pool
      assertBool "must come back as Left, so the thread keeps looping" (isLeft r)
  ]
```

```sql
-- sql/checks/070_drep_metrics.sql — the matview agrees with the catalog SQL it is built from.
DO $$
DECLARE d bigint;
BEGIN
  SELECT id INTO d FROM public.drep_hash
    WHERE raw = decode('81378d3ea89b68f0fc09944b1f93f2feb851318f196f98ba4029d8a8','hex');
  ASSERT (SELECT count(*) FROM palmares.drep_metrics) = (SELECT count(*) FROM public.drep_hash WHERE raw IS NOT NULL);
  ASSERT (SELECT eligible_actions FROM palmares.drep_metrics WHERE drep_hash_id = d) = 114;
  ASSERT (SELECT votes_cast       FROM palmares.drep_metrics WHERE drep_hash_id = d) = 16;
  ASSERT (SELECT round(participation_rate, 6) FROM palmares.drep_metrics WHERE drep_hash_id = d) = 0.140351;
  ASSERT (SELECT delegator_count  FROM palmares.drep_metrics WHERE drep_hash_id = d) = 13;
  ASSERT (SELECT voting_power     FROM palmares.drep_metrics WHERE drep_hash_id = d) = 3923709956713;
  -- D-3: the five grades sum to votes_cast, for every DRep
  ASSERT (SELECT count(*) FROM palmares.drep_metrics
          WHERE rationale_verified + rationale_inline + rationale_unverified
              + rationale_pending  + rationale_absent <> votes_cast) = 0;
  -- no rate is ever 0 where it should be null (3.8)
  ASSERT (SELECT count(*) FROM palmares.drep_metrics
          WHERE eligible_actions = 0 AND participation_rate IS NOT NULL) = 0;
  -- CONCURRENTLY needs the unique index; assert it rather than discovering it at runtime
  ASSERT (SELECT count(*) FROM pg_indexes
          WHERE schemaname = 'palmares' AND indexname = 'palmares_idx_drep_metrics_pk') = 1;
END $$;

-- outside the DO block, and outside any transaction: PreventInTransactionBlock rejects CONCURRENTLY
-- "from a function or multi-command string", which a DO body is.
REFRESH MATERIALIZED VIEW CONCURRENTLY palmares.drep_metrics;
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test test --test-show-details=direct --test-option='--pattern=drep_metrics refresh'`
Expected: FAIL with `Could not find module 'Query.Refresh'`

- [ ] **Step 3: Implement `sql/030_metrics.sql` and `Query/Refresh.hs`**

`sql/030_metrics.sql` is T-DATA §3.5's statement rebuilt on the canonical objects and the canonical column names
of synthesis §1.4: the `eligible`/`participation` CTEs become a `LATERAL palmares.eligible_actions(dh.id, s.epoch_no)`
per DRep, `count(DISTINCT …)` stays mandatory (re-votes are real), `rationale` becomes the five-grade `FILTER` set,
latency becomes `percentile_cont(0.5|0.9)`, `power` reads `palmares.drep_epoch_power WHERE target_kind = 'drep'`,
`deleg` reads `palmares.live_delegation`, `age_days` is §1.3's interval sum against `sync_state.tip_time`, and
`epochs_with_power` is dropped (nothing reads it). It ends with the four indexes above, `_pk` UNIQUE on
`drep_hash_id`. It is created **WITH DATA** — a never-populated matview rejects `REFRESH … CONCURRENTLY` — and
since `sql/000` drops the schema, the matview is dropped and rebuilt at every query-api boot, which is what
dissolves T-DATA §3.2's matview-dependency hazard.

`Query/Refresh.hs`: `RefreshMark = RefreshMark { markBlockNo :: Int64, markEpochNo :: Int } deriving (Eq, Show)` read from
`palmares.sync_state`; `shouldRefresh prev now = prev /= Just now` (one expression — it covers first tick, tip
advance, epoch boundary and rollback, so no `palmares.cursor` table is needed and none is in synthesis §1.2);
`refreshDRepMetrics` is
`try (runSqlPoolNoTransaction (rawExecute "REFRESH MATERIALIZED VIEW CONCURRENTLY palmares.drep_metrics" []) pool Nothing)`
— **`runSqlPoolNoTransaction`, not `runSqlPool`**: PostgreSQL's `ExecRefreshMatView` calls
`PreventInTransactionBlock` for `CONCURRENTLY`, so the plain pool runner's `BEGIN` turns every refresh into
`ERROR: REFRESH MATERIALIZED VIEW CONCURRENTLY cannot be executed from a function or multi-command string`.
This is the same hazard D-15 found for `CREATE INDEX CONCURRENTLY`, one layer up;
`runRefreshThread` is `forkIO . forever` of read-mark → `shouldRefresh` → refresh → `threadDelay` on
`PALMARES_VIEW_REFRESH_SECONDS` (default 300), keeping the mark in an `IORef`. Failure behaviour: log the
exception and keep looping — the refresh is idempotent and self-healing, and boot is the only fail-closed point.
A `42P01` means a db-sync migration `CASCADE`-dropped the matview; the log line names the runbook step ("restart
query-api after any db-sync upgrade"). **(OPEN: T-DATA §3.5 says a 60 s tick; T-EXTRACT's `.env.example` ships
`PALMARES_VIEW_REFRESH_SECONDS=300`, which is also the only name the synthesis carries. The plan uses 300 and
keeps the variable, so the owner changes it in `.env.example` without a rebuild; decide before the preprod
deploy: at preprod's ~22.7 s mean block interval a 60 s tick rebuilds the matview every ~2.6 blocks, a 300 s
tick every ~13.)**

- [ ] **Step 4: Run test to verify it passes**

Run: `cabal test test --test-show-details=direct --test-option='--pattern=drep_metrics refresh'` then `psql "$PG_CONN_STR" -v ON_ERROR_STOP=1 -f sql/checks/070_drep_metrics.sql`
Expected: PASS (3 tests), and the check file exits 0

- [ ] **Step 5: Commit**

```bash
git add sql/030_metrics.sql src/exe/query-api/Query/Refresh.hs src/test/UnitTests/Refresh.hs sql/checks/070_drep_metrics.sql palmares.cabal .env.example
git commit -m "feat(query-api): palmares.drep_metrics matview and its single CONCURRENTLY refresh thread"
```

---

## Produces

**SQL — schema `palmares` (applied by query-api at boot, in one transaction, `die` on failure):**
`palmares.sync_state` · `palmares.drep` · `palmares.drep_cert` · `palmares.drep_interval` ·
`palmares.action_window` · `palmares.gov_action` · `palmares.vote` · `palmares.vote_tally` ·
`palmares.anchor` · `palmares.vote_delegation` · `palmares.live_delegation` ·
`palmares.drep_epoch_delegators` · `palmares.drep_epoch_power` · `palmares.committee_member` ·
`palmares.treasury_withdrawal` · `palmares.thresholds` · `palmares.threshold_map` (the 18th view: §1.2's
seventeen plus D-25's static table) · `palmares.activity` ·
`palmares.drep_metrics` (the only MATERIALIZED view) · `palmares.eligible_actions(drep_hash_id bigint, as_of_epoch int)` (the only function) · indexes `palmares_idx_drep_metrics_{pk,part,power,raw}`

**Enumerations fixed in SQL:** `palmares.anchor.verdict ∈ valid | invalid_schema | hash_mismatch | oversized | unreachable | inline | not_fetched` (+ `cip100_valid`) · `target_kind ∈ drep | always_abstain | always_no_confidence` · `palmares.gov_action.state ∈ proposed | ratified | enacted | expired | dropped` · `palmares.vote_tally.basis = 'cast_votes_only'` · the thirteen governance `palmares.activity.event_type` values · `palmares.activity.source = 'governance'` · `palmares.activity.tx_index ∈ {-1 (gov-action lifecycle), -2 (treasury_withdrawal_enacted), tx.block_index}` — the API block's cursor parser must accept both negatives

**Haskell:** `Query.Views.{applyPalmaresSchema, applySchemaSql, palmaresSqlFiles}` ·
`Query.Refresh.{RefreshMark, shouldRefresh, refreshDRepMetrics, runRefreshThread}` ·
`Palmares.Metrics.Catalog.{catalogJson, metricSql, MetricFormula, metricCatalog, lookupMetricSql, retentionWindowEpochs}` ·
`Palmares.Metrics.Doc.renderMetricsDoc` · `Test.Pg.{withScratchDb, runPg}`

**Files other blocks read:** `sql/000_schema.sql`, `sql/010_gov_views.sql`, `sql/020_views.sql`, `sql/030_metrics.sql` · `sql/checks/*.sql` + `scripts/verify_views.sh` · `resources/metric-formulas.json` + `resources/metrics/*.sql` · `docs/metrics.md` (generated) · `test/fixtures/dbsync-skeleton.sql` · `test/fixtures/preprod-epoch-320-metrics.json`

**`formula_id`s:** `participation_rate_v1` · `rationale_rate_v1` · `age_days_v1` ·
`delegator_retention_rate_v1` · `vote_latency_median_hours_v1` · `votes_by_type_v1` ·
`gov_action_state_v1` · `tally_cast_votes_v1`

**Env vars:** `PALMARES_VIEW_REFRESH_SECONDS` (default 300) · `PALMARES_TEST_PG_CONN_STR` · `PALMARES_FIXTURE_PG_CONN_STR`; consumes `PG_CONN_STR`

**Consumes from other blocks:** `PG_CONN_STR` and the pool shape of `src/exe/query-api/Main.hs`
(`createPostgresqlPool`) · roles `palmares_owner` / `palmares_ro` and their grants (step 14) ·
`sql/040_indexes.sql` applied by the `admin apply-indexes` Job (step 12, `CONCURRENTLY`, `pg_trgm`) ·
a caught-up from-genesis preprod db-sync with `shelley: enable: true` (step 11) ·
`Palmares.DomainTypes.Core.Ids` for CIP-129 encoding of `(drep_raw, has_script)` and the CC hot/cold hashes ·
query-api's `Main.hs` must call `applyPalmaresSchema` before serving and `runRefreshThread` after ·
`Query/Metrics.hs` (API block) is the only consumer of `lookupMetricSql`

## Open items

- **(OPEN: `W = 10` epochs for retention)** — T-METRIC §8.2 asks the owner; recommended as specified. It is one
  `retention_window_epochs` field in `resources/metric-formulas.json` and is echoed on every response.
- **(OPEN: `InfoAction` in the participation denominator)** — T-METRIC §8.3 asks the owner; recommended kept as
  specified (excluding an action type is the opaque judgment the spec forbids; `votes_by_type_v1` carries the
  nuance). Changing it changes `eligible_actions` 114 → 101 on the fixture.
- **(OPEN: the `:w` / `:drep` / `:as_of_epoch` binding mechanism)** — the notes write named parameters but fix no
  mechanism; persistent's `rawSql` is positional. The plan uses `?` plus a `params` array per catalog entry.
  Confirm before `Query/Metrics.hs`.
- **(OPEN: the `epoch_stake ↔ drep_distr` reconciliation tolerance)** — T-METRIC §5.4/§9 and synthesis §6 leave it
  unmeasured; Experiment A answers it. Until then `sql/checks/040_power.sql` asserts no tolerance and the
  reconciliation line stays in `admin reconcile-koios` (Phase 5), not here.
- **(OPEN: synthesis §1.2 says "fourteen views" but its own table names seventeen plain views)** — the table
  wins, since it names them; all seventeen are built here, plus `palmares.threshold_map` (D-25) for eighteen.
- **(OPEN: `palmares.threshold_map` is my name)** — D-25 requires the static (role, action-type) → threshold table
  as a named deliverable with its own test, but no note names the object and §1.2 does not enumerate it.
- **(OPEN: `sql_object` catalog field)** — needed so `gov_action_state_v1` and `tally_cast_votes_v1`, whose SQL
  lives in a view rather than a `.sql` file, still resolve for every DTO `formula_id`. Not in the notes.
- **(OPEN: the fixture's five-grade rationale split)** — T-METRIC §4 gives only `rationale_verified = 2` and
  `rationale_rate = 0.125`; how the other 14 votes split across `inline | unverified | pending | absent` is not
  measured. Pin it from the first caught-up run, in the same commit as the fixture's cursor values.
- **(OPEN: the fixture's cursor block hash, slot and `block.time`)** — must be read off our own db-sync at epoch
  320 (T-SQL-METRICS-11 step 3). `age_days` moves inside an epoch; this is how the old `1463` went unnoticed.
- **(OPEN: `PALMARES_TEST_PG_CONN_STR` / `PALMARES_FIXTURE_PG_CONN_STR`)** — no note names a test-database env
  var, and `ci.yml` as written in T-INFRA §5.1 has no Postgres service; T-SQL-METRICS-1 adds one.
- **(OPEN: refresh interval 60 s vs 300 s)** — T-DATA §3.5 vs T-EXTRACT `.env.example`; the synthesis does not
  resolve it. `PALMARES_VIEW_REFRESH_SECONDS=300`.
- **(OPEN: `Palmares.Metrics.Catalog`'s library)** — synthesis §1.3 names `Metrics/Catalog.hs` without a library;
  placed in `offchain-lib` because both `admin` and `query-api` read it, which adds `file-embed` to that library.
- **(OPEN: CC vote volume)** — preprod has 3,243,802 CC vote rows (test spam). Do **not** fixture that figure from
  Koios; re-measure on our own db-sync, and check `/votes` p95 before adding latency/power columns to the matview.
- **(OPEN: the activity cursor's `item_index` across sibling per-tx lists)** — `voting_procedure.index`,
  `gov_action_proposal.index` and `cert_index` are independent counters, so one tx carrying both a vote and a
  certificate produces a duplicate triple. The uniqueness ASSERT in `sql/checks/060_activity.sql` is the detector;
  the fix (a fourth cursor component) is T-API's call, since the API block emits and parses the cursor string.
  The treasury-vs-enactment collision inside one epoch is already resolved here, with `tx_index = -2`.
- **(OPEN: SPO power in `palmares.vote_tally`)** — T-API §2.6 names `pool_stat.voting_power`; no note verifies
  `pool_stat` is populated under `preset: only_governance` + `shelley: enable: true`. Measure
  `SELECT count(*) FROM pool_stat` on the caught-up sync; until then SPO rows ship counts with zero power.
- **(OPEN: `treasury_withdrawal.paid_heuristic`)** — `reward_rest` has no gov-action key, so the
  `(stake_address_id, spendable_epoch)` match is ambiguous when one address receives two withdrawals in one epoch.
  The column name carries the warning; the API block decides whether to publish it at all.
