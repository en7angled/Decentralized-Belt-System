# Annex 3 — Protocol Promotion Rules

## Belt hierarchy

Order from junior to senior, exactly as encoded in `BJJBelt`:

| # | Belt | Notes |
|---|------|-------|
| 0 | White       | Starter rank. |
| 1 | Blue        | |
| 2 | Purple      | |
| 3 | Brown       | |
| 4 | Black       | (0th degree) |
| 5 | Black1      | 1st degree |
| 6 | Black2      | 2nd degree |
| 7 | Black3      | 3rd degree |
| 8 | Black4      | 4th degree |
| 9 | Black5      | 5th degree |
| 10 | Black6     | 6th degree |
| 11 | RedAndBlack | 7th degree (coral) |
| 12 | RedAndWhite | 8th degree (coral) |
| 13 | Red         | 9th degree |
| 14 | Red10       | 10th degree (honorific) |

Belts are strictly ordered; a promotion cannot skip a rung. The full
runtime-generated list is also available as the `bjj://rules/belt-hierarchy`
MCP resource.

---

## Minimum time in grade

Minimum number of calendar months the practitioner must hold a belt before
the next promotion is accepted by the protocol. Values come from
`minMonthsForBelt` in `Onchain.BJJ`. A month is defined as 30.4375 days
(`msPerMonth = 2_629_800_000 ms`).

| From belt | Minimum months before next promotion |
|-----------|--------------------------------------|
| White        | 0   |
| Blue         | 12  |
| Purple       | 18  |
| Brown        | 12  |
| Black (0°)   | 12  |
| Black 1°     | 36  |
| Black 2°     | 36  |
| Black 3°     | 36  |
| Black 4°     | 60  |
| Black 5°     | 60  |
| Black 6°     | 60  |
| RedAndBlack  | 84  |
| RedAndWhite  | 84  |
| Red          | 120 |
| Red10        | 0   |

These are **minimums**. The practitioner's age and additional criteria can
further delay a promotion in real-world BJJ federations; the on-chain
protocol only enforces the minimum-time-in-grade check.

