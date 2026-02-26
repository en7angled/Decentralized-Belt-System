# Explorer Website UI Design (DaisyUI-first)

This document proposes a modern, explorer-first UI for the Decentralized BJJ Belt protocol website, with a split between:

- **Read-side explorer** (public, wallet optional)
- **Write-side actions** (wallet-connected, role/context-aware)

The goal is to make it feel like a premium Web3 explorer + social credential network for BJJ.

---

## 1) Product principles mapped to UI

### Explorer-first
- All browse/search/filter/sort/verify flows work without wallet connection.
- Wallet UI should not block public content.
- Public pages should always display verifiable evidence snippets (IDs, dates, issuer, tx links).

### Action-contextual
- When wallet is connected and authorized, actions appear inline near relevant data:
  - On practitioner page: **Promote**, **Award achievement**, **Grant membership**
  - On pending promotion card: **Accept promotion**
  - On pending membership interval: **Accept membership**
  - On pending achievement: **Accept achievement**

### Trust by design
- Every credential view includes a compact verification panel:
  - asset class ID
  - issuer profile
  - date
  - acceptance state
  - tx hash / explorer deep-link

---

## 2) Information architecture (sitemap)

### Public routes (no wallet required)

1. `/` **Home / Protocol Overview**
2. `/explore` **Explorer Hub**
3. `/practitioners` **Practitioner Directory**
4. `/practitioner/:id` **Practitioner Profile**
5. `/organizations` **Organization Directory**
6. `/organization/:id` **Organization Profile**
7. `/ranks` **Belts & Promotions Explorer**
8. `/achievements` **Achievements Explorer**
9. `/memberships` **Membership Explorer**
10. `/lineage` **Lineage Graph Explorer**
11. `/verify` **Verify Credential**
12. `/activity` **Global Activity Feed**

### Wallet routes (connected users)

13. `/my-dojo` **My Dashboard + Pending Actions**
14. `/actions/new` **Action Composer**
15. `/tx/:txId` **Transaction Result / Receipt**

---

## 3) Reusable UI kit (DaisyUI components)

Use these as canonical building blocks:

| Reusable Component | DaisyUI primitives | Purpose |
|---|---|---|
| App shell | `navbar`, `drawer`, `menu`, `footer` | Responsive layout and navigation |
| Section headers | `breadcrumbs`, `tabs`, `divider` | Page hierarchy and context |
| KPI strip | `stats`, `stat`, `radial-progress` | Counts, acceptance rates, sync confidence |
| Entity cards | `card`, `avatar`, `badge`, `tooltip` | Practitioner/org/credential summaries |
| Belt badge system | `badge`, `mask`, custom classes | Belt color chip + rank visuals |
| Data grids | `table`, `table-zebra`, `join`, `pagination` | Sortable browse views |
| Filters panel | `collapse`, `input`, `select`, `checkbox`, `range` | Dense, skimmable filtering |
| Timeline | `timeline`, `timeline-box` | Promotions + achievements + memberships chronology |
| Verification strip | `alert`, `mockup-code`, `kbd` | Surface on-chain proof details |
| Action launcher | `dropdown`, `button-group`, `modal`, `drawer` | Contextual write actions |
| Form wizard | `steps`, `form-control`, `textarea`, `file-input`, `toggle` | Multi-step tx assembly UX |
| Async states | `skeleton`, `loading`, `toast`, `alert` | Loading, success, error, pending signatures |

---

## 4) Page-by-page UX blueprint

### 4.1 Home (`/`)

### Contains
- Hero with protocol value proposition and quick links
- Live stats (profiles, promotions, achievements, memberships)
- "Recent verified promotions" carousel
- CTA: Explore + Connect Wallet

### DaisyUI
- `hero`, `stats`, `carousel`, `card`, `button`, `badge`

### Cool ideas
- Animated "belt spectrum" gradient bar under hero
- "Latest lineage events" ticker using compact `badge` chips

---

### 4.2 Explorer Hub (`/explore`)

### Contains
- Search-first interface (global query box)
- Category tiles: Practitioners, Organizations, Ranks, Achievements, Memberships
- Top filters pinned at top
- Real-time-ish "Protocol Pulse"

### DaisyUI
- `input input-bordered`, `tabs tabs-boxed`, `card`, `stat`, `skeleton`

### Cool ideas
- Command-palette style global search (`Ctrl/Cmd + K`)
- View toggle chips: Grid / Table / Map / Timeline

---

### 4.3 Practitioner Directory (`/practitioners`)

### Contains
- Full-text search + advanced filters:
  - current belt
  - awarded by organization/master
  - date range
  - has pending actions
- Sort options: newest rank, name, belt
- Result cards/table with compact rank signal

### DaisyUI
- `table`, `card`, `avatar`, `badge`, `dropdown`, `collapse`, `join`

### Cool ideas
- Belt-colored left border on each row/card
- Mini sparkline timeline preview on hover (recent activity)

---

### 4.4 Practitioner Profile (`/practitioner/:id`) [Flagship page]

### Contains
- Identity header (name, image, current rank, verification chip)
- Current rank card + issuer + date
- Previous ranks timeline
- Achievements gallery (accepted vs pending)
- Membership timeline (active/ended/pending intervals)
- Promotion lineage:
  - promoted by whom
  - downstream students promoted by this practitioner (if any)

### DaisyUI
- `hero`, `avatar`, `badge`, `tabs`, `timeline`, `card`, `alert`, `modal`

### Contextual actions (wallet aware)
- If connected wallet owns authorized profile:
  - **Promote this practitioner** (if rules permit)
  - **Award achievement**
  - **Grant membership** (if acting as organization)
- If connected wallet owns this practitioner:
  - **Accept promotion**
  - **Accept pending achievement**
  - **Accept pending membership interval**

### Cool ideas
- "Belt Journey" vertical timeline with belt-colored milestones
- "Trust score" style chip (purely visual; based on accepted credentials count)

---

### 4.5 Organizations Directory (`/organizations`)

### Contains
- Org search + filters (size, activity, recent awards)
- Cards with logo, active members, achievements issued, latest activity

### DaisyUI
- `card`, `avatar`, `stats`, `badge`, `input`, `select`

### Cool ideas
- "Most active this month" spotlight cards
- Hover quick actions (view, verify, explore members)

---

### 4.6 Organization Profile (`/organization/:id`)

### Contains
- Organization identity + verification panel
- Memberships issued (current + historical)
- Achievements issued timeline
- Linked practitioners and affiliated orgs

### DaisyUI
- `card`, `tabs`, `table`, `timeline`, `stat`, `badge`

### Contextual actions
- **Grant membership**
- **Add membership interval**
- **Update membership end date**
- **Award achievement**

### Cool ideas
- "Academy graph" showing practitioner network around org
- Seasonal activity heatmap (memberships/awards by month)

---

### 4.7 Ranks & Promotions Explorer (`/ranks`)

### Contains
- Belt distribution charts
- Pending promotions queue
- Accepted rank history feed
- Filters by belt, actor, recipient, date

### DaisyUI
- `tabs`, `table`, `badge`, `stat`, `alert`, `select`

### Cool ideas
- Belt pyramid visualization (counts by belt)
- "Pending acceptance" urgency cards

---

### 4.8 Achievements Explorer (`/achievements`)

### Contains
- Achievement cards with image, issuer, recipient, accepted state
- Filters by organization, practitioner, accepted/pending, date range
- Drill-down modal with raw verification details

### DaisyUI
- `card`, `badge`, `tooltip`, `modal`, `mockup-code`, `pagination`

### Cool ideas
- Masonry gallery mode for images + compact "proof chips"
- Accepted/pending split view with two synced columns

---

### 4.9 Membership Explorer (`/memberships`)

### Contains
- Histories and intervals view
- Active memberships list
- Pending intervals requiring acceptance
- Ended memberships with duration chips

### DaisyUI
- `timeline`, `table`, `badge`, `collapse`, `steps`

### Cool ideas
- "Membership lifecycle" ribbon per practitioner
- Interval stack card showing renewals/extensions over time

---

### 4.10 Lineage Graph Explorer (`/lineage`)

### Contains
- Node-edge graph:
  - practitioner nodes
  - organization nodes
  - promotion edges
  - award edges (toggle)
- Filters by belt, date, root profile
- Path mode: "Show lineage path from A to B"

### DaisyUI
- `drawer` (graph controls), `badge`, `tooltip`, `toggle`, `range`

### Cool ideas
- Force-directed graph with belt-colored rings
- "Story mode": step through a practitioner's lineage chronologically

---

### 4.11 Verify Credential (`/verify`)

### Contains
- Input: credential ID / profile ID / tx hash
- Output: normalized verification card:
  - type (rank/achievement/membership)
  - status (accepted/pending/invalid/not found)
  - issuer + recipient
  - issued date + on-chain reference
- Deep links to related profile pages

### DaisyUI
- `input-group`, `alert`, `card`, `mockup-code`, `kbd`

### Cool ideas
- "Share verification result" screenshot-ready card mode
- Green/gold animated verified stamp for valid accepted credentials

---

### 4.12 Activity Feed (`/activity`)

### Contains
- Unified chronological events:
  - profile creation
  - promotions (issued/accepted)
  - memberships (created/accepted/ended)
  - achievements (awarded/accepted)
- Filter chips + infinite scroll

### DaisyUI
- `timeline`, `badge`, `skeleton`, `loading`, `join`

### Cool ideas
- Live pulse animation for new events
- Compact "event bundles" for repeated actions in short windows

---

### 4.13 My Dojo (`/my-dojo`) [Connected users]

### Contains
- Wallet + owned profiles summary
- Pending action inbox (accept promotion, accept achievement, accept membership)
- Suggested actions (based on role/permissions)
- Recent tx receipts and statuses

### DaisyUI
- `stats`, `card`, `alert`, `steps`, `toast`, `dropdown`

### Cool ideas
- "Action urgency" ranking
- 1-click continue flow for half-completed tx interactions

---

### 4.14 Action Composer (`/actions/new`)

### Contains
- Multi-step transaction wizard:
  1) Select action
  2) Fill payload
  3) Validate and preview
  4) Sign in wallet
  5) Submit and track
- JSON preview for advanced users

### DaisyUI
- `steps`, `form-control`, `textarea`, `modal`, `mockup-code`, `alert`

### Cool ideas
- Side-by-side "Human summary" + "Raw payload"
- Gas/fee estimate + protocol fee breakdown panel before signing

---

## 5) Contextual action system (critical UX pattern)

Use a shared component: `EntityActionBar`.

### Wallet states
1. **Disconnected**: show read-only + Connect CTA.
2. **Connected (no matching profile)**: show "Create profile" contextual CTA.
3. **Connected (owns practitioner)**: show accept actions where relevant.
4. **Connected (owns master/org authority)**: show grant/promote/award actions.

### Inline action triggers
- On any card with pending state, show primary button:
  - `btn btn-primary btn-sm` + icon
- On immutable facts, only show verify/share actions.

---

## 6) Visual style: modern and "cool"

### Theme
- DaisyUI themes with custom brand tokens:
  - light mode: `cupcake`-derived custom
  - dark mode: `night`/`business`-derived custom
- Belt palette mapped to ranks:
  - white, blue, purple, brown, black, coral-red variants for dan levels

### Motion and microinteractions
- Subtle hover lift on `card`
- Belt-color glow ring on active rank
- Animated transitions for timeline insertions
- Toast feedback for tx lifecycle (build/sign/submit/confirmed)

### Trust aesthetics
- Always show verification chips:
  - `badge badge-success` accepted
  - `badge badge-warning` pending acceptance
  - `badge badge-error` invalid/unverified

---

## 7) API additions required for best UX

Current APIs already support strong filtering for core lists, but a modern explorer needs aggregation and graph/search endpoints.

### 7.1 Recommended new endpoints

| Proposed Endpoint | Purpose | Enables UI |
|---|---|---|
| `GET /search?q=&type=&limit=` | Unified full-text search across profiles, orgs, ranks, achievements, memberships | Global command palette + instant results |
| `GET /events?entity_id=&entity_type=&from=&to=&types=` | Unified event stream | Activity feed and timeline composition |
| `GET /lineage/{profile-id}?depth=&include_orgs=` | Graph nodes/edges for lineage traversal | Lineage graph explorer |
| `GET /practitioner/{id}/full` | Aggregated practitioner payload (profile + ranks + achievements + memberships + pending items) | Fast flagship profile page |
| `GET /organization/{id}/full` | Aggregated org payload (profile + members + issued credentials + pending items) | Rich org detail page |
| `GET /profiles/{id}/pending-actions` | Pending acceptables for a profile | My Dojo inbox + inline pending chips |
| `POST /actions/eligibility-check` | Validate if actor can perform action (promote/award/grant) before build-tx | Disable impossible actions early |
| `POST /tx/preview` | Simulate action impact + fee/protocol fee estimate | Safer signer review UX |
| `GET /verify/{credential-id}` | One-shot normalized verification result | Verify page simplicity |
| `GET /network/sync-status` | Combined query-api + chain-sync freshness and lag | Protocol pulse indicator |

### 7.2 Existing endpoint leverage (already available)
- Query lists/count/frequency: profiles, belts, promotions, achievements, memberships
- Detail pages: `/practitioner/{id}`, `/organization/{id}`
- Write actions via `POST /build-tx` + `POST /submit-tx`

---

## 8) Integration notes

1. **BFF/Gateway strongly recommended**: both APIs use Basic Auth; do not expose service credentials in browser clients.
2. Wallet integration should stay client-side (CIP-30), while API secrets remain server-side.
3. Add feature flags for endpoints not yet implemented (UI can still show placeholder modules).

---

## 9) Suggested delivery phases

### Phase 1 (MVP explorer)
- Home, Explore hub, practitioners/org directories, practitioner detail, verify page
- Basic wallet connect + pending actions panel

### Phase 2 (full action UX)
- Action composer wizard
- Contextual actions on entity pages
- Tx receipts + richer error handling

### Phase 3 (wow factor)
- Lineage graph explorer
- Unified activity feed
- Protocol pulse with sync confidence and event streaming feel

---

This structure keeps the product useful as a public explorer from day one, while progressively unlocking powerful write-side workflows for authorized actors.
