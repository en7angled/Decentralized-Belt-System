# Database Migration

## Overview

Create and manage database schema changes for the Decentralized Belt System's chain-sync projection database. The database stores projected views of on-chain state, populated by the `chainsync-service`.

## Steps

1. **Understand the Data Flow**
    - On-chain events are captured by `chainsync-service` via chain-sync protocol
    - Events are processed by `projectChainEvent` in `Ingestion.hs`
    - Projections are written to PostgreSQL by `putMatchAndProjections` in `Storage.hs`
    - The `query-api` reads projected data via `Query/Projected.hs`

2. **Plan the Schema Change**
    - Determine which projection tables are affected
    - Check `Storage.hs` for current table definitions and column types
    - Ensure new columns use correct SQL types (e.g., `timestamptz` for `GYTime`, not `varchar`)
    - Plan for backward compatibility — can the chainsync-service re-index if needed?

3. **Update Storage Layer**
    - Modify table creation / migration SQL in `Storage.hs`
    - Update `putMatchAndProjections` to write new columns
    - Update read queries in `Query/Projected.hs` and `Query/Aggregates.hs`
    - Add indexes for columns used in WHERE clauses or JOINs

4. **Update Ingestion Layer**
    - Modify `projectChainEvent` in `Ingestion.hs` to extract new data from on-chain events
    - Ensure datum parsing handles both old and new datum formats during transition
    - Add error handling for malformed or unexpected datum shapes

5. **Migration Strategy**
    - For additive changes (new nullable columns): ALTER TABLE, no re-index needed
    - For breaking changes (type changes, new NOT NULL columns): plan a re-index from genesis or a specific slot
    - Document the migration steps in the PR description
    - Test migration on a staging database before production

6. **Verification**
    - `cabal build all` succeeds
    - `cabal test` passes
    - Query-api returns correct data from new/modified columns
    - Chainsync-service processes new events without errors

## Database Migration Checklist

- [ ] Schema change planned and documented
- [ ] `Storage.hs` updated (table definitions, write queries)
- [ ] `Ingestion.hs` updated (event projection)
- [ ] `Query/Projected.hs` updated (read queries)
- [ ] Correct SQL types used (timestamptz for time, etc.)
- [ ] Indexes added for query columns
- [ ] Migration strategy documented (additive vs re-index)
- [ ] `cabal build all` succeeds
- [ ] `cabal test` passes
