# Haskell Style Guide

Concise coding rules for this project. Applies to all modules in `onchain-lib`
and `offchain-lib`.

---

## Naming

| Item | Convention | Example |
|------|-----------|---------|
| Types / Data | `PascalCase` | `OnchainProfile`, `BJJBelt` |
| Functions | `camelCase` | `mkPractitionerProfile` |
| Constants | `camelCase` | `minLovelaceValue`, `msPerMonth` |
| Type aliases | `PascalCase` | `type ProfileId = AssetClass` |
| Record fields | `camelCase`, short, no type-name prefix | `profileId`, `startDate` |
| Constructors (smart) | prefix `mk` | `mkPromotion`, `mkCIP68Datum` |
| Unsafe functions | prefix `unsafe` | `unsafeGetRank` |
| Boolean checks | prefix `is`, `has`, `check`, `validate` | `isGivenInlineDatum` |
| Derived ID functions | prefix `derive` or `generate` | `deriveMembershipHistoryId` |

**Avoid redundant field prefixes.** If a field lives inside `OnchainMembershipHistory`,
don't prefix it with `membershipHistory`. The type already provides context.
Use `DuplicateRecordFields` if collisions arise.

## Formatting

- **Line length:** max 120 characters. Break long lines.
- **Indentation:** 2 spaces. No tabs.
- **Blank lines:** one blank line between top-level declarations. Never more
  than one consecutive blank line.
- **Where clauses:** indent 2 spaces from the function body:

```haskell
myFn x =
  result
  where
    result = x + 1
```

## INLINEABLE Pragmas (PlutusTx)

- Place `{-# INLINEABLE fn #-}` **before** the type signature, never after the
  function body.
- Add it to **every** function that is used cross-module in on-chain code.

```haskell
{-# INLINEABLE myValidator #-}
myValidator :: ScriptContext -> Bool
myValidator ctx = ...
```

## Haddock Comments

- Every **exported** function and type must have a `-- |` Haddock comment.
- Use `-- ^` for inline field documentation in records.
- Module-level Haddock is required: place it before the `module` keyword.

```haskell
-- | Membership history for a practitioner-organization pair.
data OnchainMembershipHistory = OnchainMembershipHistory
  { historyId       :: MembershipHistoryId      -- ^ NFT identifier
  , practitionerId  :: ProfileId                 -- ^ Practitioner profile
  , organizationId  :: ProfileId                 -- ^ Organization profile
  , intervalsHeadId :: MembershipIntervalId      -- ^ Head of intervals list
  }
```

## Section Headers

Use Haddock section headers (`-- *`) with a consistent divider width (79 chars):

```haskell
-------------------------------------------------------------------------------
-- * Protocol Parameters
-------------------------------------------------------------------------------
```

## Data Types

- **Records with 4+ fields:** always use record syntax, never positional.
- **Sum types with partial fields:** prefer separate record types over a single
  sum type with overlapping field names, or accept the `-Wno-partial-fields`
  trade-off and prefix fields per constructor.
- **Redeemer constructors with 4+ fields:** use record syntax or introduce type
  aliases (`type OutputIndex = Integer`) to distinguish same-typed parameters.
- **Functions with 4+ arguments of similar types:** introduce a small record or
  newtype to prevent argument-swapping bugs.

## Constants

- No magic numbers in validator logic. Define named constants in a shared
  location (`Onchain.Utils` or `Onchain.Constants`):

```haskell
{-# INLINEABLE minLovelaceValue #-}
minLovelaceValue :: Value
minLovelaceValue = V1.lovelaceValue 3_500_000

-- | Milliseconds per average month (30.4375 days).
msPerMonth :: Integer
msPerMonth = 2_629_800_000
```

## Validator Functions

- Keep each validator lambda **under ~80 lines**. Extract per-redeemer logic
  into named helper functions:

```haskell
myValidatorLambda ctx = case redeemer of
  ActionA{..} -> handleActionA ...
  ActionB{..} -> handleActionB ...
```

- Each helper should be `INLINEABLE` and independently understandable.
- Use `traceIfFalse` with **descriptive** error messages — they are your debug
  log on-chain.

## Error Messages

- Prefix with the context: `"Must ..."`, `"Cannot ..."`, `"Invalid ..."`.
- Be specific enough to identify which check failed without reading source:

```haskell
-- Good:
traceIfFalse "Must spend organization User NFT to modify membership list"

-- Bad:
traceIfFalse "check failed"
```

## Module Boundaries

- **On-chain code** (`onchain-lib`) must not import offchain dependencies
  (`Aeson`, `Servant`, `Swagger`, etc.).
- If a type needs both on-chain and off-chain instances, define the core type
  and PlutusTx instances in `onchain-lib`, and orphan/newtype instances in
  `offchain-lib`.
- Keep modules focused — aim for **one responsibility per module** (types,
  logic, lookups, ID generation, blueprint).
