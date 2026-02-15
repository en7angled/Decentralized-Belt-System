# On-chain trace codes reference

Trace codes are **single-character** (0-9, A-Z, a-z; 62 total) to minimize script size. When debugging failed transactions, identify the **script** that failed (from the tx), then look up the code in the table for that module.

## MintingPolicy (single-char: 0-9, A-R)

| Code | Description |
|------|-------------|
| 0 | Protocol is paused |
| 1 | Invalid purpose |
| 2 | Creation date must be before the tx validity range |
| 3 | Must spend seed TxOutRef |
| 4 | Must lock profile at PV (Practitioner) |
| 5 | Exact mint check (Practitioner) |
| 6 | Must lock rank at RV |
| 7 | Must lock profile at PV (Organization) |
| 8 | Must lock root at MV |
| 9 | Exact mint check (Organization) |
| A | Profiles must have correct currency symbol |
| B | Must spend seed TxOutRef for uniqueness |
| C | Must spend user NFT of the profile who awards the promotion |
| D | Tx must mint JUST the pending rank NFT |
| E | Must lock pending rank NFT with inline datum at ranksValidator address |
| F | Must pass promotion validation rules |
| G | Start date before validity (membership history) |
| H | CS check (membership history) |
| I | Must spend org User NFT (membership history) |
| J | Must spend left node |
| K | Exact mint check (membership history) |
| L | Lock interval at MV (membership history) |
| M | Start date before validity (membership interval) |
| N | CS check (membership interval) |
| O | Must spend org User NFT (membership interval) |
| P | Must spend history node |
| Q | Exact mint check (membership interval) |
| R | Lock interval at MV (membership interval) |

## Utils (single-char: S-Z)

| Code | Description |
|------|-------------|
| S | Invalid context (script context parse failed) |
| T | Cannot find own input by TxOutRef |
| U | Cannot find state NFT at expected address |
| V | Invalid output: expected inline datum |
| W | Cannot decode oracle datum |
| X | Oracle UTxO must have inline datum |
| Y | Oracle UTxO not found in reference inputs |
| Z | Must pay required fee |

## CIP68 (single-char: 0-2)

| Code | Description |
|------|-------------|
| 0 | Name too long (max 128 bytes) |
| 1 | Description too long (max 1024 bytes) |
| 2 | Image URI too long (max 256 bytes) |

## BJJ (single-char: a-j)

| Code | Description |
|------|-------------|
| a | Invalid belt (expected 0-14) |
| b | Cannot succ a red 10 belt |
| c | Cannot pred a white belt |
| d | Belts lower than black are not allowed to promote |
| e | Only 2 degree black belts can promote to black |
| f | Master belt must be greater than the student's next belt |
| g | Master belt date must be before the student's next belt date |
| h | Student's next belt must be greater than the student's current belt |
| i | Student Next belt date must be after the student's current belt date |
| j | Time in the current belt must be greater than the minimum time for the next belt |

## LinkedList (single-char: B-L)

| Code | Description |
|------|-------------|
| B | CS mismatch |
| C | Insert invariant violated |
| D | Adjacent nodes |
| E | Left < inserted |
| F | Right > inserted |
| G | Inserted -> right |
| H | Append invariant violated |
| I | Last has no next |
| J | Appended has key |
| K | Appended has no next |
| L | Appended > last |

## Protocol (single-char: 3-9, A-R)

| Code | Description |
|------|-------------|
| 3 | Root node has no history |
| 4 | Cannot insert: different orgs |
| 5 | Cannot append: different orgs |
| 6 | Cannot add interval: validation failed |
| 7 | Last interval is not the head |
| 8 | Last interval not closed |
| 9 | Last interval not accepted |
| A | Cannot update interval: end date before current |
| M | Cannot accept interval: already accepted |
| N | Cannot accept a rank that is not pending |
| O | OnchainProfile has no rank |
| P | Cannot accept: not pending |
| Q | OnchainProfile has no rank |
| R | OnchainProfile has no rank |

## OracleValidator (single-char: 0-3)

| Code | Description |
|------|-------------|
| 0 | Invalid oracle datum |
| 1 | Must be signed by admin |
| 2 | Must return oracle UTxO |
| 3 | Invalid script purpose |

## RanksValidator (single-char: 3-9)

| Code | Description |
|------|-------------|
| 3 | Cannot cleanup valid datum |
| 4 | No datum |
| 5 | Invalid datum |
| 6 | Invalid script info |
| 7 | Must spend User NFT |
| 8 | Lock profile at PV |
| 9 | Lock rank at RV |

## ProfilesValidator (single-char: 3-E)

| Code | Description |
|------|-------------|
| 3 | Cannot cleanup valid datum |
| 4 | No datum |
| 5 | Invalid datum |
| 6 | Invalid purpose |
| 7 | Own value has Ref NFT |
| 8 | Must spend User NFT |
| 9 | Lock updated profile |
| A | Promotion ID CS check |
| B | Already at or past this rank |
| C | Date must be after current |
| D | Own value has Ref NFT (accept) |
| E | Lock updated profile (accept) |

## MembershipsValidator (single-char: k-y)

| Code | Description |
|------|-------------|
| k | Cannot cleanup UTxO with valid protocol datum |
| l | No datum |
| m | Invalid datum |
| n | Invalid redeemer for ListNodeDatum |
| o | Invalid redeemer for IntervalDatum |
| p | Invalid purpose |
| q | Must spend org User NFT (insert) |
| r | Lock updated left node |
| s | Exact mint check (insert) |
| t | Lock inserted node |
| u | Must spend org User NFT (update) |
| v | Lock updated node |
| w | Exact mint check (update) |
| x | Lock updated interval |
| y | Must spend practitioner User NFT |

## OracleNFTPolicy (single-char: 0-1)

| Code | Description |
|------|-------------|
| 0 | Must spend seed UTxO |
| 1 | Invalid script purpose |

## Protocol.Lookup (single-char: 0-1)

| Code | Description |
|------|-------------|
| 0 | Invalid datum: expected ListNodeDatum |
| 1 | Invalid datum: expected IntervalDatum |
