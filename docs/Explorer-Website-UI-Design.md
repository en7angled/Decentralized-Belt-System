# Explorer Website UI Design (DaisyUI 5 · Tailwind CSS 4)

> **Baseline**: DaisyUI **v5** on Tailwind CSS **4**. All component names, class names, and theming APIs in this document target that stack. When DaisyUI ships breaking changes, update this document accordingly.

This document proposes a modern, explorer-first UI for the Decentralized BJJ Belt protocol website, with a split between:

- **Read-side explorer** (public, wallet optional)
- **Write-side actions** (wallet-connected, role/context-aware)

The goal is to make it feel like a premium Web3 explorer + social credential network for BJJ.

---

## Table of contents

1. [Product principles mapped to UI](#1-product-principles-mapped-to-ui)
2. [Information architecture (sitemap)](#2-information-architecture-sitemap)
3. [Reusable UI kit (DaisyUI 5 components)](#3-reusable-ui-kit-daisyui-5-components)
4. [Page-by-page UX blueprint](#4-page-by-page-ux-blueprint)
5. [Contextual action system](#5-contextual-action-system-critical-ux-pattern)
6. [Wallet connection flow](#6-wallet-connection-flow)
7. [Onboarding & first-time experience](#7-onboarding--first-time-experience)
8. [Notification & pending-actions system](#8-notification--pending-actions-system)
9. [Error, loading, & empty states](#9-error-loading--empty-states)
10. [Responsive & mobile design](#10-responsive--mobile-design)
11. [Visual style: modern and "cool"](#11-visual-style-modern-and-cool)
12. [API additions required for best UX](#12-api-additions-required-for-best-ux)
13. [Integration notes](#13-integration-notes)
14. [Suggested delivery phases](#14-suggested-delivery-phases)
15. [Appendix A — key component specifications](#appendix-a--key-component-specifications)

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
- Actions that cannot be performed by the current wallet are hidden—never greyed out—so the UI stays clean.

### Trust by design
- Every credential view includes a compact verification panel:
  - asset class ID (truncated with copy-to-clipboard)
  - issuer profile (avatar + name link)
  - date
  - acceptance state (`badge badge-success` / `badge badge-warning`)
  - tx hash with Cardano explorer deep-link

---

## 2) Information architecture (sitemap)

### Public routes (no wallet required)

| # | Route | Label |
|---|---|---|
| 1 | `/` | Home / Protocol Overview |
| 2 | `/explore` | Explorer Hub |
| 3 | `/practitioners` | Practitioner Directory |
| 4 | `/practitioner/:id` | Practitioner Profile ★ |
| 5 | `/organizations` | Organization Directory |
| 6 | `/organization/:id` | Organization Profile |
| 7 | `/ranks` | Belts & Promotions Explorer |
| 8 | `/achievements` | Achievements Explorer |
| 9 | `/memberships` | Membership Explorer |
| 10 | `/lineage` | Lineage Graph Explorer |
| 11 | `/verify` | Verify Credential |
| 12 | `/activity` | Global Activity Feed |

### Wallet routes (connected users)

| # | Route | Label |
|---|---|---|
| 13 | `/my-dojo` | My Dashboard + Pending Actions |
| 14 | `/actions/new` | Action Composer |
| 15 | `/tx/:txId` | Transaction Result / Receipt |

★ = flagship page

---

## 3) Reusable UI kit (DaisyUI 5 components)

Use these as canonical building blocks. Components marked ★ are new or significantly changed in DaisyUI 5.

| Reusable Component | DaisyUI 5 primitives | Purpose |
|---|---|---|
| **App shell** | `navbar`, `drawer`, `menu`, `footer`, `dock` ★ | Responsive layout; `dock` replaces bottom-tab on mobile |
| **Section headers** | `breadcrumbs`, `tabs`, `divider`, `fieldset` ★ | Page hierarchy, form groupings, context |
| **KPI strip** | `stat`, `radial-progress`, `countdown` | Counts, acceptance rates, sync confidence |
| **Entity cards** | `card`, `avatar`, `badge`, `tooltip`, `popover` ★ | Practitioner/org/credential summaries; `popover` for hover previews |
| **Belt badge system** | `badge`, custom CSS vars | Belt color chip + rank visuals (see §11 Belt palette) |
| **Data grids** | `table`, `join`, `pagination` | Sortable browse views |
| **Filter bar** | `filter` ★, `input`, `select`, `checkbox`, `range`, `calendar` ★ | Tag-style toggles (DaisyUI 5 `filter`), dense filtering, date pickers |
| **Timeline** | `timeline`, `timeline-box` | Promotions + achievements + memberships chronology |
| **Verification strip** | `alert`, `kbd`, `tooltip` | Surface on-chain proof details |
| **Action launcher** | `dropdown`, `modal`, `drawer`, `fab` ★ | Contextual write actions; FAB for mobile quick-action |
| **Form wizard** | `steps`, `fieldset` ★, `input`, `textarea`, `file-input`, `toggle`, `select` | Multi-step tx assembly UX; `fieldset` groups form sections |
| **Async states** | `skeleton`, `loading`, `spinner` ★, `toast`, `alert` | Loading, success, error, pending signatures; `spinner` for lightweight indicators |
| **Expandable panels** | `accordion` ★, `collapse` | FAQ sections, expandable detail panels, advanced filter groups |
| **Hover previews** | `popover` ★, `hover-card` ★ | Quick entity preview on link hover without page navigation |

---

## 4) Page-by-page UX blueprint

### 4.1 Home (`/`)

#### Layout
Full-width hero → 4-col KPI strip → 2-col content (recent promotions + how it works) → CTA footer.

#### Contains
- **Hero** with protocol value proposition, animated belt-spectrum gradient bar, and two CTAs: "Explore the Protocol" (primary) + "Connect Wallet" (outline).
- **KPI strip** (`stat` × 4): Total Profiles, Accepted Promotions, Active Memberships, Achievements Awarded. Each stat shows number + trend arrow (↑ / ↓ vs last 30 days). Use `stat-title`, `stat-value`, `stat-desc`.
- **"How It Works"** 3-step horizontal `steps` component:
  1. "Create your on-chain profile" (icon: person+)
  2. "Earn promotions & credentials" (icon: belt)
  3. "Verify anywhere, forever" (icon: shield-check)
- **"Latest Promotions"** carousel (`carousel carousel-center`) of compact `card` items, each showing: practitioner avatar, name, belt badge, promoter name, date. Auto-scrolls; manual drag.
- **Protocol Pulse** — small `stat` widget in the footer area showing chain-sync status (slot lag, "Synced" / "Syncing…" / "Behind"), sourced from `/network/sync-status`.

#### DaisyUI 5

```
hero, stat, steps, carousel, card, badge, btn, skeleton
```

#### Cool ideas
- Animated belt-spectrum gradient bar (`@keyframes` shifting hue across the 15 belt colors) under the hero.
- "Latest lineage events" scrolling ticker using a row of compact `badge` chips with belt colors and practitioner names.
- Parallax background showing faint BJJ mat pattern.

#### Empty state
On a fresh protocol with zero profiles: replace KPI strip with a single centered `card` inviting the first user to create a profile. Hide carousel.

---

### 4.2 Explorer Hub (`/explore`)

#### Layout
Full-width search bar → category tile grid (3 × 2) → "Protocol Pulse" live stats.

#### Contains
- **Command-palette search** (⌘K / Ctrl+K trigger): opens a `modal` with `input input-bordered input-lg` and live results rendered as a `menu` of entity links (practitioners, orgs, belts, achievements). Fuzzy-search with debounce.
- **Category tiles** — 6 `card card-compact` items in a responsive grid:
  - Practitioners (icon: 🥋, count badge)
  - Organizations (icon: 🏢, count badge)
  - Ranks & Belts (icon: belt icon, count badge)
  - Achievements (icon: 🏆, count badge)
  - Memberships (icon: 🤝, count badge)
  - Lineage Graph (icon: 🌳, no count — action CTA)
- **View-mode toggle** using DaisyUI 5 `filter` component (pill-style toggle chips): Grid | Table | Timeline.
- **Protocol Pulse** — real-time-ish sync confidence bar (`radial-progress`) + slot lag stat.

#### DaisyUI 5

```
modal, input, menu, card, badge, stat, filter, radial-progress, skeleton
```

#### Cool ideas
- Category tiles use `hover-card` ★ effect — subtle 3D lift + shadow on hover.
- Search results grouped by entity type with `divider` separators.
- Keyboard navigation through search results (↑↓ + Enter).

#### Empty state
Category tiles always show even with zero items (count = 0). Search returns "No results — try a different query" with suggested links.

---

### 4.3 Practitioner Directory (`/practitioners`)

#### Layout
Sticky filter bar → results area (card grid or table, toggled by `filter`) → pagination.

#### Contains
- **Filter bar** — horizontal strip:
  - `input input-bordered` for name/ID search
  - `select` for current belt level
  - `select` for awarded-by (organization/master)
  - `calendar` ★ for date range (profile created / last promoted)
  - `checkbox` "Has pending actions" (only meaningful when wallet connected)
  - Reset button
- **Sort dropdown** (`select select-sm`): Newest rank first, Name A→Z, Belt (highest first).
- **Result cards** (`card card-side card-compact` in grid) or **table rows** (`table table-zebra`), toggled via `filter` component (Grid / Table).
  - Each result shows: `avatar` (or placeholder), name, belt `badge` with belt-color background, organization affiliation (if any), last promotion date.
  - Belt-colored left border (`border-l-4`) on each card/row using the belt palette.
- **Pagination** (`join` with page buttons) at bottom.

#### DaisyUI 5

```
input, select, checkbox, calendar, filter, card, avatar, badge, table, join, pagination, skeleton, popover
```

#### Interactive behaviors
- **Hover preview** (`popover`): hovering a practitioner card shows a mini profile preview (belt journey summary, last 2 ranks, active memberships count) without navigating away.
- **Click** → navigates to `/practitioner/:id`.

#### Cool ideas
- Belt-colored left border on each card/row matching their current rank.
- Mini sparkline-style dot strip on each card showing recent activity (last 5 events as colored dots: green = promotion accepted, blue = achievement, purple = membership).
- "Spotlight" section at the top: "Recently Promoted" — 3 highlighted practitioner cards with a subtle gold shimmer border.

#### Empty state
"No practitioners found" card with CTA: "Be the first — Create your profile" (links to action composer if wallet connected, or to wallet connect flow).

---

### 4.4 Practitioner Profile (`/practitioner/:id`) — ★ Flagship page

This is the most important page. It must feel rich, trustworthy, and visually stunning.

#### Layout
Full-width identity header → tabbed content area (Belt Journey | Achievements | Memberships | Promoted Students | On-Chain) → contextual action bar (floating or sticky bottom).

#### Contains

**Identity Header**
- Large `avatar avatar-lg` (or `avatar avatar-placeholder` with initials)
- Name (h1), description excerpt, profile ID (truncated `kbd` with copy)
- Current belt `badge badge-lg` with belt-color background + glow ring (`ring ring-offset-2` with belt color)
- Verification chip: `badge badge-success badge-sm` "On-chain verified" with tooltip showing profile ref token asset class
- If wallet connected and this is the user's profile: `btn btn-outline btn-sm` "Edit Profile"

**Tab: Belt Journey** (default active)
- Vertical `timeline` with `timeline-box` items, one per rank (current + all previous):
  - Belt-colored milestone dot (filled circle in belt color)
  - `timeline-box` content: Belt name + `badge`, promoter name (link to their profile), achievement date, time-in-belt duration label (e.g., "2 years, 3 months at Blue")
  - Deeplink to on-chain tx
- Current rank is the topmost item, highlighted with a `ring` glow and "Current" `badge badge-primary`.
- If there are pending promotions for this practitioner: yellow `alert alert-warning` card above the timeline: "Pending Promotion to [Belt] from [Master]" with "Accept" button (if wallet matches).

**Tab: Achievements**
- Masonry-style grid of achievement `card` items:
  - Achievement image (if URI set), name, description excerpt
  - Awarded by (avatar + name link)
  - Date
  - Acceptance state: `badge badge-success` "Accepted" or `badge badge-warning` "Pending"
  - If pending and wallet matches: "Accept" `btn btn-primary btn-sm`
- Filter sub-bar: `filter` pills for Accepted / Pending / All.
- If zero achievements: friendly empty card "No achievements yet."

**Tab: Memberships**
- `timeline` of membership histories, grouped by organization:
  - Organization avatar + name (link)
  - Intervals shown as stacked `card card-compact` items within each history:
    - Start date → End date (or "Active" `badge badge-info`)
    - Accepted state (`badge badge-success` / `badge badge-warning`)
    - Duration label
  - If pending interval and wallet matches: "Accept" button
- "Membership lifecycle" ribbon: a horizontal bar per organization, colored segments showing active/ended intervals proportionally.

**Tab: Promoted Students** (only shown if this practitioner has promoted others)
- Grid of practitioner `card` items that this master has promoted
- Each card: student avatar, name, belt promoted to, date
- Sort by date (newest first)
- Shows the master's lineage reach. Links to each student's profile.

**Tab: On-Chain**
- Raw credential inventory: all protocol tokens associated with this profile
- `table` of: Token type (Profile Ref, Rank, Achievement, etc.), Asset Class ID (`kbd` with copy), Validator address, Datum summary
- Deep-link each row to a Cardano explorer (e.g., Cexplorer, CardanoScan)

#### DaisyUI 5

```
avatar, badge, tabs, timeline, timeline-box, card, alert, modal, btn, table, kbd, tooltip, popover, filter, skeleton, accordion
```

#### Contextual actions (wallet-aware, sticky bottom bar or floating `fab`)

| Connected wallet is… | Actions shown |
|---|---|
| Not connected | "Connect Wallet" subtle CTA in header |
| Connected, no matching profile | "Create Profile" CTA |
| This practitioner's owner | "Accept Promotion" (if pending), "Accept Achievement" (if pending), "Accept Membership" (if pending), "Edit Profile" |
| A master (Black+) | "Promote this Practitioner" `btn btn-primary` |
| An organization owner | "Award Achievement", "Grant Membership" |

Actions render as a `join` button group at the bottom of the identity header (desktop) or as a `fab` ★ floating action button on mobile.

#### Cool ideas
- **"Belt Journey" glow effect**: each timeline milestone has a soft radial glow in the belt color, with the current rank having a pulsing animation (`animate-pulse`).
- **"Trust Score" radial** (`radial-progress`): a purely visual indicator based on: (accepted ranks count × 30 + accepted achievements count × 10 + accepted memberships count × 10) / max possible, displayed as a ring. Tooltip explains the formula. Not a formal score — just a visual signal of credential density.
- **Promotion comparison card**: when a pending promotion exists, show a side-by-side `diff`-style card: "Current: Blue Belt (since 2023-01-15)" → "Promoted to: Purple Belt (by Master X, 2025-06-01)".
- **Share button**: generates a shareable link + Open Graph preview image for the profile.

#### Empty state
If profile ID not found: 404 page with "Profile not found" + search bar + "Browse all practitioners" link.

---

### 4.5 Organization Directory (`/organizations`)

#### Layout
Filter bar → card grid → pagination.

#### Contains
- **Filter/search bar**: name search `input`, activity level `select` (Most active / All), recent awards toggle.
- **Organization cards** (`card`) in responsive grid:
  - Organization logo/avatar
  - Name, description excerpt
  - `stat` strip: Active Members count, Achievements Issued count, Latest Activity date
  - `badge` chips for quick info (e.g., "12 active members", "47 achievements")
- **"Most Active This Month" spotlight** — top 3 organizations highlighted in a dedicated row with larger cards and a subtle gold border.

#### DaisyUI 5

```
card, avatar, stat, badge, input, select, pagination, popover, skeleton
```

#### Cool ideas
- Hover `popover` on each org card showing a mini member-count breakdown by belt.
- Organization cards have a subtle gradient background matching their "personality" (derived from org ID hash → deterministic pastel hue).

#### Empty state
"No organizations found" + "Create your organization" CTA.

---

### 4.6 Organization Profile (`/organization/:id`)

#### Layout
Identity header → tabbed content (Members | Achievements Issued | Membership Activity | On-Chain) → contextual actions.

#### Contains

**Identity Header**
- Organization avatar, name (h1), description, profile ID (`kbd` with copy)
- Verification chip
- KPI strip (`stat` × 3): Active Members, Total Achievements Issued, Membership Histories

**Tab: Members**
- **Member grid**: `card card-compact` items for each practitioner with an active membership:
  - Practitioner avatar, name, current belt `badge`, membership status (`badge badge-info` "Active" / `badge badge-neutral` "Ended")
  - Click → practitioner profile
- Filter: `filter` pills for Active / Historical / All
- Sort: Name, Belt level, Membership start date

**Tab: Achievements Issued**
- `timeline` of achievements this organization has awarded:
  - Achievement name, image thumbnail, recipient (avatar + name link), date, accepted state badge
- Masonry gallery mode toggle.

**Tab: Membership Activity**
- Horizontal timeline visualization: each practitioner's membership intervals as colored segments on a horizontal bar (green = active, grey = ended, yellow = pending acceptance).
- Below: `table` with raw interval data (practitioner, start, end, status).

**Tab: On-Chain**
- Organization's protocol tokens (Profile Ref, Membership Histories Root) with asset class IDs and explorer deep-links.

#### DaisyUI 5

```
avatar, badge, tabs, stat, card, timeline, table, filter, kbd, tooltip, skeleton
```

#### Contextual actions (wallet-aware)

| Connected wallet is… | Actions shown |
|---|---|
| This organization's owner | "Grant Membership", "Add Membership Interval", "Update Membership End Date", "Award Achievement" |
| A practitioner with pending membership | "Accept Membership" |
| Not connected | None (read-only) |

#### Cool ideas
- **"Academy network" mini-graph**: a small force-directed graph (D3/vis.js) showing the organization at center with connected practitioners as nodes, sized by belt level, colored by belt. Clickable.
- **Activity heatmap**: a GitHub-style contribution heatmap showing membership grants + achievement awards by week, for the last 12 months.

#### Empty state
If org ID not found: 404 + search. If org has zero members: "This organization has no members yet" card.

---

### 4.7 Ranks & Promotions Explorer (`/ranks`)

#### Layout
Tab bar (Accepted Ranks | Pending Promotions) → filter bar → content area → pagination.

#### Contains

**Tab: Accepted Ranks** (default)
- **Belt distribution chart**: horizontal stacked bars, one per belt level, colored with belt palette, showing count. Rendered with a chart library (e.g., Recharts, Chart.js) or pure CSS width-proportional `div` bars.
- **Belt pyramid**: visual pyramid with each belt level as a layer, wider at the bottom (White) and narrower at the top (Red). Shows count labels. Pure CSS or SVG. The pyramid should use exact belt colors from §11.
- **Data table** (`table table-zebra`): filterable, sortable list of all accepted ranks.
  - Columns: Practitioner (avatar + name), Belt (`badge`), Awarded By (name link), Date, Rank ID (`kbd`)
  - Filters: belt level `select`, awarded by `select`, date range `calendar`
  - Sort: belt level, date, practitioner name

**Tab: Pending Promotions**
- **"Pending acceptance" urgency cards**: `card card-bordered` items with `alert alert-warning` styling:
  - Student name + avatar
  - Promoted to: belt `badge`
  - Promoted by: master name + avatar
  - Date issued
  - If wallet matches student: "Accept Promotion" `btn btn-primary`
  - Time since issued (e.g., "3 days ago")

#### DaisyUI 5

```
tabs, table, badge, card, alert, stat, select, calendar, pagination, btn, skeleton
```

#### Cool ideas
- Belt pyramid SVG with hover tooltips showing exact count + percentage per level.
- "Promotion velocity" stat: average time between promotions across the protocol.
- Animated confetti burst when viewing a promotion that was just accepted (within last 24h).

#### Empty state
"No ranks recorded yet — the protocol is just getting started!" with explore CTA.

---

### 4.8 Achievements Explorer (`/achievements`)

#### Layout
Filter bar → view toggle (Gallery | Table) → content → pagination.

#### Contains
- **Filter bar**:
  - `input` name search
  - `select` awarded by (organization/practitioner)
  - `select` awarded to (practitioner)
  - `filter` pills: Accepted / Pending / All
  - `calendar` date range
- **Gallery mode** (default): masonry grid of `card` items:
  - Achievement image (or gradient placeholder derived from name hash)
  - Name, description excerpt
  - Awarded by (avatar + name), Awarded to (avatar + name)
  - Date, acceptance state `badge`
  - "Proof chip" — tiny inline `kbd` with truncated achievement asset class ID
- **Table mode**: `table table-zebra` with columns: Name, Image, Awarded To, Awarded By, Date, Status, ID
- **Drill-down modal**: clicking a card opens `modal` with full details including:
  - Full-size image
  - Complete description
  - Complete verification details (asset class, issuer profile, date, tx hash)
  - Acceptance state with "Accept" button if applicable

#### DaisyUI 5

```
card, badge, tooltip, modal, kbd, filter, input, select, calendar, table, pagination, skeleton
```

#### Cool ideas
- Masonry gallery with subtle hover zoom (CSS `scale(1.02)` on hover).
- "Accepted/Pending split view": two synced columns, accepted on left, pending on right, with `divider` between.
- Achievement cards have a subtle ribbon in the top-right corner showing acceptance state.

#### Empty state
"No achievements found" + conditional CTA: "Award your first achievement" (if org owner connected) or "Check back later."

---

### 4.9 Membership Explorer (`/memberships`)

#### Layout
Sub-tabs (Histories | Intervals) → filter bar → content → pagination.

#### Contains

**Sub-tab: Histories**
- `table` of membership histories:
  - Practitioner (avatar + name link)
  - Organization (avatar + name link)
  - Number of intervals
  - Current status (active / ended / pending)
  - History ID (`kbd`)
- Expandable rows (`accordion` ★): click a row to expand and see all intervals inline.

**Sub-tab: Intervals**
- `timeline` view: intervals as chronological events.
  - Each interval: start → end, practitioner name, organization name, acceptance state.
  - Active intervals: `badge badge-info` "Active" with green left border.
  - Ended intervals: `badge badge-neutral` "Ended" with grey left border.
  - Pending acceptance: `badge badge-warning` "Pending" with yellow left border.
  - If wallet matches and pending: "Accept" `btn btn-sm`.

#### DaisyUI 5

```
tabs, table, accordion, timeline, badge, btn, filter, select, calendar, pagination, skeleton
```

#### Cool ideas
- **"Membership lifecycle" ribbon**: per practitioner × organization, a horizontal bar where colored segments represent each interval proportionally (green = active, grey = ended, yellow = pending).
- **"Interval stack" card**: for a single practitioner-org pair, show all intervals stacked vertically like a timeline ruler, showing renewals and extensions over time.

#### Empty state
"No memberships recorded yet" + CTA.

---

### 4.10 Lineage Graph Explorer (`/lineage`)

#### Layout
Full-screen graph canvas → `drawer` (right-side) for controls/details → breadcrumb path display.

#### Contains
- **Interactive graph** (powered by D3-force, vis.js, or Cytoscape.js):
  - **Practitioner nodes**: circle, sized by number of promotions given, filled with belt-color ring.
  - **Organization nodes**: square, with org avatar.
  - **Promotion edges**: directed arrows (master → student), labeled with target belt.
  - **Achievement edges** (togglable): dashed lines, labeled with achievement name.
- **Controls drawer** (`drawer drawer-end`):
  - Belt filter: checkboxes to show/hide nodes by belt.
  - Date range: `calendar` to limit edges to a time window.
  - Depth slider: `range` to control how many levels of lineage to show from the root.
  - Root profile selector: `input` with autocomplete to pick a starting practitioner.
  - Toggle switches: Show/hide orgs, Show/hide achievements, Labels on/off.
- **"Path Finder"**: two `input` fields (From practitioner, To practitioner) with "Find Path" button. Highlights the shortest promotion-lineage path between two practitioners.
- **Node click**: opens `popover` with mini profile card; double-click navigates to profile page.

#### DaisyUI 5

```
drawer, badge, tooltip, popover, toggle, range, input, select, calendar, btn, card, skeleton
```

#### Cool ideas
- **"Story mode"**: button that auto-animates through a practitioner's lineage chronologically, highlighting each node/edge in sequence with a 2-second delay. Like a presentation mode for lineage.
- Belt-colored rings on nodes with a subtle radial gradient glow.
- Physics-based layout where higher belts "float" higher (y-position bias by belt level).
- Zoom to cluster: double-click an organization node to zoom into its member subgraph.

#### Empty state
"No lineage data yet" + explanation of how lineage forms (promotions between practitioners).

---

### 4.11 Verify Credential (`/verify`)

#### Layout
Centered single-column: input section → result card.

#### Contains
- **Input section** (`fieldset` ★):
  - `input input-bordered input-lg` with placeholder "Enter credential ID, profile ID, or tx hash"
  - "Verify" `btn btn-primary btn-lg`
  - Or: "Scan QR Code" button → opens camera modal for scanning verification QR codes
- **Result card** (`card card-bordered`) — appears after verification:
  - **Status banner**:
    - ✅ `alert alert-success` "Verified & Accepted" — for accepted credentials
    - ⏳ `alert alert-warning` "Valid but Pending Acceptance" — for pending credentials
    - ❌ `alert alert-error` "Not Found or Invalid" — for unknown IDs
  - **Credential details** (if found):
    - Type: Rank / Achievement / Membership (`badge`)
    - Issuer: avatar + name + profile link
    - Recipient: avatar + name + profile link
    - Date issued
    - Acceptance state
    - On-chain reference: asset class ID (`kbd`), tx hash (`kbd`), Cardano explorer link
  - **Deep links**: "View Issuer Profile", "View Recipient Profile"

**"Verification Certificate" mode**: button to render the result as a clean, printable card:
  - White background, centered layout
  - Protocol logo watermark
  - All verification details
  - QR code encoding the verification URL
  - "Verified by [Protocol Name] on Cardano — [date]" footer
  - CSS `@media print` styles for clean printing

**QR Code generation**: every verified credential gets a unique URL (`/verify?id=<assetClass>`) and a QR code rendered inline, shareable via copy-link or download-image.

#### DaisyUI 5

```
fieldset, input, btn, card, alert, kbd, badge, avatar, tooltip, modal, skeleton
```

#### Cool ideas
- Animated green checkmark SVG burst when verification succeeds (CSS animation, 1s duration).
- Gold/bronze "verified" stamp watermark overlay on the result card (purely visual flourish).
- "Share verification" button: copies a formatted text snippet + link to clipboard.
- Social preview: the verification URL generates an Open Graph image showing the credential details.

#### Empty state
Initial state: just the input form with a brief explainer: "Paste any credential ID, profile ID, or transaction hash to verify it on-chain."

---

### 4.12 Activity Feed (`/activity`)

#### Layout
Filter chips (sticky) → infinite-scroll timeline.

#### Contains
- **Filter chips** (`filter` ★ pills, horizontal scrollable row):
  - All | Profiles | Promotions | Achievements | Memberships
  - Additional: Accepted | Issued | All States
- **Unified timeline** (`timeline timeline-vertical`):
  - Each event is a `timeline-box` with:
    - Event icon (color-coded by type):
      - 👤 Profile created (neutral)
      - 🥋 Promotion issued (belt color) / Promotion accepted (belt color + ✓)
      - 🏆 Achievement awarded (gold) / Achievement accepted (gold + ✓)
      - 🤝 Membership created (blue) / Membership accepted (blue + ✓) / Membership ended (grey)
    - Actor(s): avatar + name links (issuer + recipient)
    - Summary text: "Master Carlos promoted João to Purple Belt"
    - Relative timestamp ("3 hours ago")
    - Quick-verify link (shield icon → opens verify modal)
  - Color-coded left border on each event card matching the event type.
- **Infinite scroll**: `loading` spinner at bottom while fetching next page. Uses intersection observer.

#### DaisyUI 5

```
timeline, timeline-box, badge, avatar, filter, loading, spinner, skeleton
```

#### Cool ideas
- **Live pulse animation**: when a new event arrives (via polling or WebSocket), the new item slides in from the top with a subtle green flash border.
- **"Event bundles"**: when the same actor performs multiple actions in a short window (e.g., master promotes 3 students), collapse into a single bundle: "Master X promoted 3 practitioners" with expandable details (`collapse`).
- **Sound effect toggle**: optional subtle "ding" notification sound when new events appear (off by default).

#### Empty state
"No activity yet — the protocol is waiting for its first action!" with CTA to create a profile.

---

### 4.13 My Dojo (`/my-dojo`) — Connected users only

#### Layout
Wallet summary bar → KPI strip → pending-actions inbox → recent tx history.

#### Contains

**Wallet summary bar**
- Connected wallet address (truncated, with copy)
- Wallet identicon/avatar
- Owned protocol profiles listed as `badge` chips (click → jump to profile)
- "Disconnect" `btn btn-ghost btn-sm`

**KPI strip** (`stat` × 4)
- Owned Profiles count
- Pending Actions (accept promotions + accept achievements + accept memberships)
- Credentials Earned (total accepted ranks + achievements)
- Active Memberships

**Pending-actions inbox**
- Sorted by urgency (oldest pending first).
- Each item is a `card card-compact card-bordered` with:
  - Action type icon + label (e.g., "Accept Promotion to Purple Belt")
  - Issuer name + avatar
  - Date issued + "X days ago" relative time
  - "Accept" `btn btn-primary btn-sm` → triggers wallet signing flow inline
  - "View Details" link → navigates to the relevant entity page
- If zero pending: `alert alert-info` "You're all caught up! No pending actions."

**Suggested actions** (role-based)
- If user is a master (Black+): "Promote a student" card
- If user is an organization: "Grant membership" card, "Award achievement" card
- Always: "Update profile image" card

**Recent tx receipts**
- `table` of last 10 transactions:
  - Action type
  - Date/time
  - Status (`badge`): Submitted / Confirmed / Failed
  - Tx hash (`kbd`, clickable → opens `/tx/:txId`)

#### DaisyUI 5

```
stat, card, alert, badge, btn, table, kbd, toast, avatar, steps, skeleton
```

#### Cool ideas
- **"Action urgency" ranking**: pending items have a color-coded time indicator (green < 7d, yellow 7–30d, red > 30d since issued).
- **1-click continue flow**: for half-completed interactions (e.g., built tx but didn't sign), show a "Resume" button that re-opens the signing modal with the same tx.
- **"Daily dojo" summary**: at the top, a one-sentence summary of today's activity: "You accepted 1 promotion and earned 2 achievements today" or "Nothing new today — time to train! 🥋".

#### Empty state
If wallet has no protocol profiles: large centered CTA card: "Welcome to the Protocol — Create your first profile" with wizard link.

---

### 4.14 Action Composer (`/actions/new`)

#### Layout
Full-page multi-step wizard with sidebar progress indicator.

#### Contains
- **Step indicator** (`steps steps-vertical` on desktop sidebar, `steps steps-horizontal` on mobile top bar):
  1. Select Action
  2. Fill Details
  3. Review & Preview
  4. Sign in Wallet
  5. Track Submission

- **Step 1: Select Action**
  - Grid of action `card` items, each with icon, title, description:
    - Create Profile (Practitioner / Organization)
    - Promote Practitioner
    - Award Achievement
    - Grant Membership
    - Add Membership Interval
    - Accept Promotion / Achievement / Membership
    - Update Profile Image
    - Update Membership End Date
  - Cards that are unavailable (e.g., "Promote" when not a Black belt) are hidden or shown with a lock icon + tooltip explaining why.

- **Step 2: Fill Details**
  - `fieldset` ★ groups for each form section:
    - Profile details: name `input`, description `textarea`, image URI `input` (with preview), profile type `select`
    - Promotion details: student profile `input` (with autocomplete/search), belt `select`, date `calendar`
    - Achievement details: recipient `input`, name `input`, description `textarea`, image URI `input`, metadata key-value pairs (dynamic `input` rows), date `calendar`
    - Membership details: organization `select`, practitioner `input`, start date `calendar`, end date `calendar` (optional)
  - Real-time validation with inline error messages.

- **Step 3: Review & Preview**
  - Side-by-side layout:
    - Left: **"Human summary"** — readable card: "You are promoting João Silva from Blue Belt to Purple Belt, authorized by Master Carlos, effective June 1, 2025."
    - Right: **"Raw payload"** — `mockup-code` JSON preview of the `Interaction` body that will be sent to `/build-tx`.
  - Protocol fee breakdown panel (if fees are configured):
    - Action fee (e.g., 2 ADA promotion fee)
    - Estimated transaction fee
    - Total
  - "Edit" button to go back to Step 2.

- **Step 4: Sign in Wallet**
  - `loading` indicator: "Building transaction…"
  - On success: "Transaction built. Please sign in your wallet."
  - Wallet signing prompt (CIP-30 `signTx`).
  - On signing: "Submitting transaction…"
  - Error handling: if build fails, show `alert alert-error` with the error message from the API.

- **Step 5: Track Submission**
  - `steps` component showing tx lifecycle:
    1. ✅ Built
    2. ✅ Signed
    3. ⏳ Submitted (waiting for confirmation)
    4. ✅ Confirmed (with tx hash + explorer link)
  - On confirmation: `toast` "Transaction confirmed!" + confetti animation.
  - "View Transaction" button → `/tx/:txId`
  - "Do Another Action" button → resets wizard.

#### DaisyUI 5

```
steps, fieldset, input, textarea, select, calendar, file-input, toggle, card, modal, alert, btn, loading, spinner, toast, kbd
```

#### Cool ideas
- Side-by-side "Human summary" + "Raw payload" in Step 3 — advanced users can inspect exactly what's being sent.
- **Gas/fee estimate** with a visual bar: "This transaction will cost approximately 0.3 ADA in network fees + 2 ADA protocol fee."
- **"Quick action" shortcuts**: from any entity page, clicking "Promote" pre-fills Step 1 + Step 2 with context (student ID, etc.) and jumps directly to Step 2.
- **Draft saving**: if the user navigates away mid-wizard, save the draft to localStorage and offer to resume.

---

### 4.15 Transaction Receipt (`/tx/:txId`)

#### Layout
Centered single-column card.

#### Contains
- **Status header**: large status badge (Confirmed ✅ / Pending ⏳ / Failed ❌)
- **Transaction details** `card`:
  - Tx hash (`kbd` with copy, explorer deep-link)
  - Action type (`badge`)
  - Actor (avatar + name)
  - Affected entities (links to profiles/credentials)
  - Timestamp
  - Block / slot info (if confirmed)
- **Created assets** (if any): list of newly minted tokens with their asset class IDs

#### DaisyUI 5

```
card, badge, kbd, btn, alert, avatar, tooltip, skeleton
```

---

## 5) Contextual action system (critical UX pattern)

Use a shared component: `EntityActionBar`.

### Wallet states and their UI

| # | State | UI behavior |
|---|---|---|
| 1 | **Disconnected** | Read-only. Show "Connect Wallet" outline button in navbar. No action buttons on entity pages. |
| 2 | **Connected, no matching profile** | Show "Create Profile" contextual CTA card on `/my-dojo` and as a banner on explore pages. |
| 3 | **Connected, owns practitioner profile** | Show "Accept" buttons on own pending promotions/achievements/memberships. Show "Edit Profile". |
| 4 | **Connected, owns master profile (Black+)** | All of #3, plus "Promote" button on other practitioner profiles (only those eligible for promotion by this master). |
| 5 | **Connected, owns organization profile** | Show "Grant Membership", "Add Interval", "Update End Date", "Award Achievement" on relevant pages. |

### Inline action triggers
- On any card with pending state, show primary action button: `btn btn-primary btn-sm` + icon.
- On immutable facts (accepted ranks, accepted achievements), only show verify/share actions: `btn btn-ghost btn-xs`.
- Actions that would fail (e.g., promoting someone who outranks you) should be hidden, not disabled.

### Action flow
1. User clicks inline action button.
2. If action needs additional input: open `modal` with the required form (pre-filled with context).
3. Call `/build-tx` → show `spinner`.
4. If success: prompt wallet signing (CIP-30 `signTx`).
5. Call `/submit-tx` → show progress `steps`.
6. On confirmation: `toast` success + update UI state (refetch data).
7. On error: `alert alert-error` with message + "Try Again" button.

---

## 6) Wallet connection flow

### Wallet selector

Triggered by "Connect Wallet" button in the navbar. Opens a `modal modal-bottom` (mobile) or `modal` (desktop):

- Title: "Connect your Cardano wallet"
- Grid of wallet option `btn` items with wallet icons:
  - Eternl, Lace, Nami, Flint, GeroWallet, Typhon, Yoroi, NuFi
  - Each button detects if the wallet extension is installed (`window.cardano.<name>`).
  - Installed: normal button, icon + name.
  - Not installed: greyed text + "Install →" link to wallet's website.
- Footer: "What is a Cardano wallet?" `accordion` ★ with brief explainer.

### Connected state (navbar)

After connection:
- Wallet icon (tiny, matching the connected wallet) + truncated address (first 8 + last 4 chars) displayed in the navbar.
- Dropdown on click:
  - Full address (copyable `kbd`)
  - Owned protocol profiles (listed as links)
  - Wallet balance (ADA amount)
  - "My Dojo" link
  - "Disconnect" `btn btn-error btn-sm`

### Profile linking

On first connection, the BFF checks which protocol profile(s) the wallet's addresses own (by looking for Profile User tokens in the wallet's UTxO set). The result determines contextual action visibility.

- **Zero profiles**: show "Create Profile" CTA.
- **One profile**: auto-select it as active.
- **Multiple profiles**: show a profile selector dropdown in the navbar (rare but possible if wallet has multiple addresses or manages multiple profiles).

---

## 7) Onboarding & first-time experience

### Zero-state home page
When the protocol has < 10 profiles, the home page shows an "Early Adopter" mode:
- Replace carousel with a large explainer `card`:
  - "You're early! This protocol has [N] profiles."
  - "Be among the first to bring your BJJ credentials on-chain."
  - "Connect Wallet & Create Profile" `btn btn-primary btn-lg`

### "Get Started" wizard
Accessed from the home CTA or "Create Profile" flows. This is the Action Composer (§4.14) with a friendlier wrapper:
- Pre-selects "Create Profile (Practitioner)" as the action.
- Adds contextual help tooltips on each field: "Your name as it appears in competitions", "A photo of you — URI to an image hosted anywhere", etc.
- After successful profile creation: `modal` celebration with confetti + "Welcome to the Protocol!" message + "Explore your profile" link.

### Contextual tooltips
On first visit (tracked via localStorage), show dismissible `tooltip tooltip-open` hints on:
- The search bar: "Search for practitioners, organizations, or credentials."
- The wallet connect button: "Connect your wallet to take actions."
- The lineage graph: "Explore the promotion lineage between practitioners."

### "What is this?" explainer cards
On complex pages (Lineage Graph, Verify, Activity Feed), show a collapsible `accordion` at the top:
- "What is a belt promotion?" — brief explanation of the two-phase flow (issue + accept).
- "What is a membership?" — explanation of org-practitioner intervals.
- "What does 'pending' mean?" — explanation of the acceptance pattern.

---

## 8) Notification & pending-actions system

### Desktop: notification bell
In the navbar (next to wallet indicator), show a bell icon with a `badge badge-error badge-xs` count of pending actions.
- Click opens a `popover` ★ dropdown:
  - List of up to 5 most recent pending items (type + issuer + relative time).
  - Each item is clickable → navigates to the relevant entity or My Dojo.
  - "View all" link → `/my-dojo`.
- If zero pending: "No pending actions" text.

### Mobile: dock badge
The mobile `dock` ★ bottom navigation bar includes a "My Dojo" item with a `badge badge-error badge-xs` count overlay.

### Toast notifications for tx lifecycle
When a user submits a transaction from any page:
1. `toast` (bottom-right, auto-dismiss after 5s): "Transaction submitted ⏳" (`alert alert-info`)
2. When confirmed (via polling): `toast` "Transaction confirmed ✅" (`alert alert-success`) with link to `/tx/:txId`.
3. If failed: `toast` "Transaction failed ❌" (`alert alert-error`) — persists until dismissed.

### "Action required" banners
On pages where the connected wallet has a pending action related to the displayed entity:
- Sticky top banner: `alert alert-warning` — "You have a pending [promotion/achievement/membership] here. [Accept Now →]"

---

## 9) Error, loading, & empty states

### Loading patterns

| Component type | Loading pattern |
|---|---|
| Full page | Centered `spinner spinner-lg` with "Loading…" text |
| Entity card | `skeleton` matching card dimensions (image area + 3 text lines) |
| Table row | `skeleton` row (4 columns of `skeleton h-4 w-full`) |
| Timeline event | `skeleton` with circle (milestone) + rectangle (content box) |
| KPI stat | `skeleton` for `stat-value` (wide), `skeleton` for `stat-desc` (narrow) |
| Profile header | Large `skeleton` circle (avatar) + `skeleton` rectangles (name, description) |
| Graph | Full-canvas `spinner` with "Building graph…" text |

### Error boundary design

- **Inline error** (API call fails for one section): `alert alert-error alert-sm` replacing the failed section content: "Failed to load [section name]. [Retry →]"
- **Full-page error** (page-level API failure): centered `card` with error icon, message, "Retry" button, and "Go Home" link.
- **Network error** (no connectivity): full-width sticky `alert alert-error` at top: "Network error — please check your connection."

### 404 page
- Centered layout.
- Large "404" text with a BJJ-themed illustration (e.g., a broken belt or an empty mat).
- "Page not found" message.
- Global search bar (same as Explorer Hub command palette).
- Quick links: "Browse Practitioners", "Browse Organizations", "Go Home".

### Chain-sync lag indicator
When the query API's sync status is behind:
- Subtle `alert alert-warning alert-sm` banner at the top of every page:
  - "Data may be up to [N] blocks behind. Last synced: [timestamp]."
  - Auto-dismisses when sync catches up.

### Empty states per entity type

| Page | Empty state message | CTA |
|---|---|---|
| Practitioner Directory | "No practitioners have joined yet." | "Create the first profile" |
| Organization Directory | "No organizations registered yet." | "Register your academy" |
| Achievements Explorer | "No achievements have been awarded yet." | "Award the first achievement" |
| Membership Explorer | "No memberships created yet." | "Grant a membership" |
| Activity Feed | "The protocol is quiet — no activity yet." | "Create a profile to get started" |
| Lineage Graph | "No promotion lineage exists yet." | "Be the first to promote" |
| My Dojo (no profiles) | "You don't have any protocol profiles." | "Create your profile" |
| My Dojo (no pending) | "You're all caught up!" | "Explore the protocol" |

---

## 10) Responsive & mobile design

### Breakpoint strategy (Tailwind CSS 4 defaults)

| Breakpoint | Width | Layout behavior |
|---|---|---|
| `sm` | ≥ 640px | Single column → 2 columns for card grids |
| `md` | ≥ 768px | Sidebar appears (drawer); tables become visible |
| `lg` | ≥ 1024px | Full 3-column grid; graphs at full width |
| `xl` | ≥ 1280px | Max-width container; larger cards, more padding |

### Mobile navigation (`dock` ★)
Replace the sidebar `drawer` on mobile with a fixed-bottom `dock`:
- 5 items: Home 🏠 | Explore 🔍 | Lineage 🌳 | My Dojo 🥋 | More ⋯
- "More" opens a `drawer drawer-bottom` with remaining navigation links.
- "My Dojo" item has `badge badge-error badge-xs` for pending action count.
- Active item highlighted with `dock-active`.

### Component transformations on mobile

| Desktop | Mobile |
|---|---|
| `table table-zebra` | Stacked `card` list (each row becomes a compact card) |
| Side-by-side profile header | Stacked: avatar on top, info below |
| Horizontal `steps` | Vertical `steps` |
| `drawer drawer-side` (sidebar) | `dock` bottom nav + `drawer drawer-bottom` for overflow |
| Masonry grid (3+ cols) | Single-column card stack |
| Graph full-screen | Graph with pinch-zoom + swipe pan; controls in `drawer drawer-bottom` |
| Horizontal filter bar | Scrollable horizontal row or `collapse` panel |
| `popover` hover previews | Disabled (touch devices don't hover); tap → navigate instead |

### Touch considerations
- All interactive elements: minimum 44×44px touch target.
- Swipe gestures: swipe left on pending-action cards to reveal "Accept" action (like mobile email).
- Pull-to-refresh on list/feed pages.
- Bottom-sheet modals (`modal modal-bottom`) for actions on mobile instead of centered modals.

---

## 11) Visual style: modern and "cool"

### Theme

DaisyUI 5 themes with custom brand tokens:

| Mode | Base theme | Customization |
|---|---|---|
| Light | `silk` | Custom primary (BJJ brand blue), belt palette vars |
| Dark | `abyss` | Custom primary, belt palette vars, high-contrast text |
| Alternative light | `cupcake` | Softer, warmer palette for accessibility |
| Alternative dark | `dim` | Less dramatic than `abyss`, easier on eyes |

Theme toggle: `toggle` switch in navbar (or system-preference auto-detect).

Custom theme tokens set via DaisyUI 5's `@plugin "daisyui"` CSS configuration:

```css
@plugin "daisyui" {
  themes: light --default, dark, silk, abyss, dim, cupcake;
}
```

### Belt color palette

Every BJJ belt maps to a CSS custom property and Tailwind utility. Use these consistently for borders, badge backgrounds, glow rings, timeline dots, and chart segments.

| Belt | Enum value | Hex color | CSS variable | Usage |
|---|---|---|---|---|
| White | `White` | `#F5F5F5` | `--belt-white` | `bg-[--belt-white]`, light grey (visible on white bg via border) |
| Blue | `Blue` | `#1E40AF` | `--belt-blue` | Deep blue |
| Purple | `Purple` | `#7C3AED` | `--belt-purple` | Vivid purple |
| Brown | `Brown` | `#92400E` | `--belt-brown` | Rich brown |
| Black | `Black` | `#1C1917` | `--belt-black` | Near-black |
| Black 1st Degree | `Black1` | `#292524` | `--belt-black1` | Black + 1 gold stripe marker |
| Black 2nd Degree | `Black2` | `#44403C` | `--belt-black2` | Black + 2 gold stripe markers |
| Black 3rd Degree | `Black3` | `#57534E` | `--belt-black3` | Black + 3 gold stripe markers |
| Black 4th Degree | `Black4` | `#78716C` | `--belt-black4` | Black + subtle sheen |
| Black 5th Degree | `Black5` | `#A8A29E` | `--belt-black5` | Black + prominent sheen |
| Black 6th Degree | `Black6` | `#D6D3D1` | `--belt-black6` | Black transitioning to master tones |
| Red & Black | `RedAndBlack` | `#DC2626` | `--belt-red-black` | Coral-red, alternating red/black stripe indicator |
| Red & White | `RedAndWhite` | `#EF4444` | `--belt-red-white` | Bright red, alternating red/white stripe indicator |
| Red | `Red` | `#B91C1C` | `--belt-red` | Deep red |
| Red 10th Degree | `Red10` | `#991B1B` | `--belt-red10` | Darkest red, gold border treatment |

**Black belt degree markers**: Since Black1–Black6 are the same physical color (black), differentiate them in the UI with:
- A small gold stripe count indicator (1–6 tiny gold dots or bars) next to the badge.
- A tooltip showing the full degree name.
- In tables/grids: "Black 3rd°" text label with degree superscript.

**Belt color usage guide**:
- **Card left border**: `border-l-4` with belt color.
- **Badge background**: `badge` with `style="background-color: var(--belt-<name>); color: <contrast>"`.
- **Timeline milestone dot**: filled circle in belt color.
- **Glow ring** (current rank): `ring ring-2 ring-offset-2` with belt color, `animate-pulse` on flagship profile.
- **Chart segments**: stacked/bar charts use belt colors directly.

### Motion and microinteractions

| Element | Interaction | Animation |
|---|---|---|
| `card` | Hover | `transform: translateY(-2px); box-shadow: +2px` over 150ms ease |
| Belt `badge` on profile | Idle | Subtle `animate-pulse` glow ring in belt color (2s cycle) |
| Timeline events | Page load | Staggered fade-in from top (each item delayed by 50ms) |
| `toast` notifications | Appear/dismiss | Slide in from bottom-right (300ms ease-out), fade out (200ms) |
| Tx lifecycle `steps` | Step completion | Current step gets a ✓ icon with a 200ms scale-in animation |
| Search results | Typing | Results fade-in with 100ms stagger as they arrive |
| Graph nodes | Hover | Scale 1.2× with belt-color highlight ring (200ms) |
| Verification result | Success | Green checkmark SVG draws in (stroke-dashoffset animation, 800ms) |

### Trust aesthetics

Always show verification chips on credential displays:

| State | Component | Styling |
|---|---|---|
| Accepted | `badge badge-success badge-sm` | "✓ Accepted" — green |
| Pending acceptance | `badge badge-warning badge-sm` | "⏳ Pending" — yellow/amber |
| Invalid / Not found | `badge badge-error badge-sm` | "✗ Invalid" — red |
| On-chain verified | `badge badge-info badge-sm` | "⛓ On-chain" — blue, with Cardano explorer link |

---

## 12) API additions required for best UX

Current APIs already support strong filtering for core lists, but a modern explorer needs aggregation, search, and graph endpoints.

### 12.1 Existing endpoint leverage (already available)

| Endpoint | Supports | Used by |
|---|---|---|
| `GET /profiles` | list, count, frequency by type; filter by ID, type; sort; pagination | Practitioner/Org directories, KPI stats |
| `GET /practitioner/:id` | full practitioner info: name, description, image, current_rank, previous_ranks | Practitioner profile page |
| `GET /organization/:id` | full org info: name, description, image | Organization profile page |
| `GET /belts` | list, count, frequency by belt; filter by rank ID, belt, achieved_by, awarded_by, date range | Ranks explorer, belt distribution charts |
| `GET /promotions` | list, count, frequency by belt; filter by ID, belt, achieved_by, awarded_by | Pending promotions, action inbox |
| `GET /achievements` | list, count; filter by ID, awarded_to, awarded_by, is_accepted, date range | Achievements explorer, profile achievements tab |
| `GET /membership-histories` | list, count; filter by organization, practitioner | Membership explorer, profile memberships tab |
| `GET /membership-intervals` | list, count; filter by practitioner | Membership explorer, interval details |
| `POST /build-tx` | builds unsigned tx from `Interaction` (action + userAddresses) | All write actions |
| `POST /submit-tx` | submits signed tx (unsigned body + witness) | All write actions |
| `GET /health`, `GET /ready` | service health and readiness status | Protocol Pulse indicator |

### 12.2 Recommended new endpoints

Each endpoint is tagged with a priority:
- **P0** = required for MVP explorer (Phase 1)
- **P1** = required for full action UX (Phase 2)
- **P2** = nice-to-have for wow factor (Phase 3)

---

#### `GET /search?q=&type=&limit=` — **P0**

Unified full-text search across profiles, organizations, ranks, achievements, memberships.

**Enables**: Global command palette (⌘K), Explorer Hub search, header search bar.

**Request**:
```
GET /search?q=Carlos&type=practitioner&limit=10
```

**Response shape**:
```json
{
  "results": [
    {
      "type": "practitioner",
      "id": "<asset-class>",
      "name": "Carlos Gracie Jr.",
      "description": "...",
      "image_uri": "...",
      "current_belt": "Red",
      "score": 0.95
    },
    {
      "type": "organization",
      "id": "<asset-class>",
      "name": "Gracie Barra",
      "score": 0.87
    }
  ],
  "total": 42
}
```

**Client-side fallback**: Can be approximated with `GET /profiles?profile_type=Practitioner` + client-side name filtering, but this is slow and doesn't search across entity types.

---

#### `GET /stats/overview` — **P0**

Aggregated protocol-wide statistics in a single call.

**Enables**: Home page KPI strip, Protocol Pulse.

**Response shape**:
```json
{
  "profiles": { "total": 1234, "practitioners": 1100, "organizations": 134 },
  "belts": { "total": 1100, "frequency": [["White", 400], ["Blue", 300], ...] },
  "promotions_pending": 23,
  "achievements": { "total": 567, "accepted": 540, "pending": 27 },
  "memberships": { "active_intervals": 890, "total_histories": 450 },
  "sync_status": { "local_slot": 123456, "chain_slot": 123460, "state": "Behind", "last_synced": "2025-06-01T12:00:00Z" }
}
```

**Client-side fallback**: Multiple parallel calls to `/profiles/count`, `/profiles/frequency`, `/belts/count`, `/belts/frequency`, `/promotions/count`, `/achievements/count`, `/membership-histories/count`. Works but requires 7+ API calls.

---

#### `GET /events?entity_id=&entity_type=&from=&to=&types=&limit=&offset=` — **P1**

Unified event stream for the activity feed.

**Enables**: Activity feed page, profile activity timelines.

**Response shape**:
```json
[
  {
    "event_type": "promotion_accepted",
    "timestamp": "2025-06-01T12:00:00Z",
    "slot": 123456,
    "block_hash": "abc...",
    "actor": { "id": "<asset-class>", "name": "Master Carlos", "type": "practitioner" },
    "subject": { "id": "<asset-class>", "name": "João Silva", "type": "practitioner" },
    "details": { "belt": "Purple", "promotion_id": "<asset-class>" },
    "tx_hash": "abc..."
  }
]
```

**Client-side fallback**: Not feasible — no unified event stream exists; would need to poll all list endpoints and merge/sort client-side.

---

#### `GET /lineage/{profile-id}?depth=&include_orgs=&include_achievements=` — **P2**

Graph nodes and edges for lineage traversal.

**Enables**: Lineage Graph Explorer.

**Response shape**:
```json
{
  "nodes": [
    { "id": "<asset-class>", "name": "Master Carlos", "type": "practitioner", "current_belt": "Red", "image_uri": "..." },
    { "id": "<asset-class>", "name": "Gracie Barra", "type": "organization" }
  ],
  "edges": [
    { "from": "<master-id>", "to": "<student-id>", "type": "promotion", "belt": "Purple", "date": "2025-01-15T00:00:00Z" },
    { "from": "<org-id>", "to": "<practitioner-id>", "type": "membership" }
  ]
}
```

**Client-side fallback**: Possible with iterative calls to `/belts?awarded_by=<id>` for each level, but performance degrades with depth and requires N+1 queries per level. Not viable for a smooth interactive graph.

---

#### `GET /practitioner/{id}/full` — **P0**

Aggregated practitioner payload: profile + all ranks + all achievements + all memberships + pending items.

**Enables**: Fast flagship profile page load (single API call instead of 5+).

**Response shape**:
```json
{
  "profile": { "id": "...", "name": "...", "description": "...", "image_uri": "...", "type": "Practitioner" },
  "current_rank": { "id": "...", "belt": "Purple", "achieved_by": "...", "awarded_by": "...", "date": "..." },
  "previous_ranks": [ ... ],
  "pending_promotions": [ ... ],
  "achievements": [ ... ],
  "membership_histories": [ { "organization": "...", "intervals": [ ... ] } ],
  "promotions_given": [ ... ]
}
```

**Client-side fallback**: Parallel calls to `/practitioner/:id`, `/achievements?awarded_to=:id`, `/membership-histories?practitioner=:id`, `/promotions?achieved_by=:id`, `/belts?awarded_by=:id`. Works but requires 5 calls.

---

#### `GET /organization/{id}/full` — **P0**

Aggregated organization payload: profile + members + issued credentials + pending items.

**Enables**: Rich organization detail page.

**Response shape**:
```json
{
  "profile": { "id": "...", "name": "...", "description": "...", "image_uri": "..." },
  "members": [
    { "practitioner_id": "...", "practitioner_name": "...", "current_belt": "Blue", "membership_status": "active", "latest_interval": { ... } }
  ],
  "achievements_issued": [ ... ],
  "membership_histories_count": 45,
  "active_members_count": 38
}
```

**Client-side fallback**: `/organization/:id` + `/membership-histories?organization=:id` + `/achievements?awarded_by=:id`. Requires 3 calls and manual member enrichment.

---

#### `GET /profiles/{id}/pending-actions` — **P1**

Pending acceptables for a profile (promotions, achievements, membership intervals).

**Enables**: My Dojo inbox, notification bell count, inline pending chips.

**Response shape**:
```json
{
  "pending_promotions": [ { "id": "...", "belt": "Purple", "awarded_by": "...", "date": "..." } ],
  "pending_achievements": [ { "id": "...", "name": "...", "awarded_by": "...", "date": "..." } ],
  "pending_memberships": [ { "id": "...", "organization": "...", "start_date": "...", "end_date": "..." } ],
  "total_pending": 5
}
```

**Client-side fallback**: `/promotions?achieved_by=:id` + `/achievements?awarded_to=:id&is_accepted=false` + `/membership-intervals?practitioner=:id` (then filter by `is_accepted=false` client-side, since intervals don't have `is_accepted` filter yet). Workable but requires 3 calls + client filtering.

---

#### `POST /actions/eligibility-check` — **P1**

Validate if an actor can perform an action before building the tx.

**Enables**: Hide impossible actions early, show informative error messages.

**Request**:
```json
{
  "action": "promote",
  "actor_profile_id": "<master-id>",
  "target_profile_id": "<student-id>",
  "parameters": { "target_belt": "Purple" }
}
```

**Response shape**:
```json
{
  "eligible": false,
  "reasons": [
    "Master's rank (Black) is not sufficient to promote to Purple — requires at least Black 2nd Degree",
    "Student has not held Blue belt for the required 18 months (current: 14 months)"
  ]
}
```

**Client-side fallback**: The client can replicate some rules (belt hierarchy checks), but time-in-belt and other on-chain validations require server-side data.

---

#### `GET /verify/{credential-id}` — **P1**

One-shot normalized verification result.

**Enables**: Verify page simplicity, QR-code verification flow.

**Response shape**:
```json
{
  "status": "accepted",
  "type": "rank",
  "credential": {
    "id": "<asset-class>",
    "belt": "Purple",
    "recipient": { "id": "...", "name": "João Silva" },
    "issuer": { "id": "...", "name": "Master Carlos" },
    "date": "2025-06-01T00:00:00Z",
    "tx_hash": "abc..."
  }
}
```

**Client-side fallback**: Client must determine credential type from the ID, then call the appropriate endpoint (`/belts?rank=<id>`, `/achievements?achievement=<id>`, `/promotions?promotion=<id>`) and normalize. Fragile and complex.

---

#### `GET /belts?awarded_by={id}` — **P0** (enhancement to existing)

The existing `/belts` endpoint already supports `awarded_by` filter. This confirms it's usable for "Promoted Students" view on the practitioner profile.

**Enables**: "Promoted Students" tab on practitioner profiles.

---

#### `GET /membership-intervals?organization={id}` — **P1** (enhancement to existing)

Add `organization` filter to the existing `/membership-intervals` endpoint (currently only supports `practitioner` filter).

**Enables**: Organization profile "Membership Activity" tab filtered to that org's intervals only.

---

#### `GET /achievements/frequency` — **P1** (new, matches existing pattern)

Frequency of achievements by organization (or by accepted/pending state).

**Enables**: Achievements explorer charts, organization profile activity stats.

**Response shape** (matches existing `/belts/frequency` pattern):
```json
[["<org-name-or-id>", 42], ["<org-name-or-id>", 31], ...]
```

---

#### `GET /network/sync-status` — **P0**

Combined query-api + chain-sync freshness and lag.

**Enables**: Protocol Pulse indicator, chain-sync lag banner.

**Response shape**:
```json
{
  "query_api": { "status": "healthy", "version": "1.0.0" },
  "chain_sync": {
    "state": "UpToDate",
    "local_slot": 123456,
    "chain_slot": 123456,
    "slot_lag": 0,
    "last_synced": "2025-06-01T12:00:00Z"
  }
}
```

**Client-side fallback**: `/health` and `/ready` endpoints exist but provide less detail. Chain-sync exposes `/health` on port 8084 but that's a separate service not exposed to browsers.

---

## 13) Integration notes

1. **BFF/Gateway strongly recommended**: both APIs use Basic Auth; do not expose service credentials in browser clients. The BFF proxies requests, adds auth headers, and can aggregate multiple backend calls into single frontend calls.

2. **Wallet integration stays client-side** (CIP-30): the browser directly calls `window.cardano.<wallet>.enable()`, `getUsedAddresses()`, `getChangeAddress()`, `signTx()`, `getCollateral()`. The BFF never sees the signing key.

3. **Transaction flow** (browser → BFF → APIs):
   ```
   Browser                    BFF                     Interaction API
   ──────                    ───                     ───────────────
   1. Collect wallet info    →
   2. Send action + addrs   → Forward to /build-tx  → Build unsigned tx
   3. Receive unsigned tx    ←                       ← Return CBOR hex
   4. signTx() in wallet
   5. Send tx + witness      → Forward to /submit-tx → Submit to Cardano
   6. Receive txId           ←                       ← Return txId
   7. Poll for confirmation  → Forward to /ready     → Check sync
   ```

4. **Feature flags**: use feature flags for endpoints not yet implemented. The UI should show placeholder modules with "Coming Soon" labels rather than hiding entire pages.

5. **Caching**: The BFF should cache read-side responses with short TTLs (30–60s for lists, 5–10s for counts/frequencies, no cache for live-projection queries). Profile detail pages can cache for 60s since profile data changes infrequently.

6. **CORS**: The query API already has CORS middleware (`WebAPI.CORS.setupCors`). The BFF should be the single origin for the frontend, avoiding direct browser-to-API CORS issues.

---

## 14) Suggested delivery phases

### Phase 1 — MVP Explorer (read-side core)

**Goal**: public browsing works without a wallet. 

**Pages**: Home, Explorer Hub, Practitioner Directory, Practitioner Profile, Organization Directory, Organization Profile, Verify Credential.

**Components**: App shell (navbar, footer, dock), entity cards, belt badges, data tables, filter bars, search, KPI stats, skeletons.

**API requirements**: `/search`, `/stats/overview`, `/practitioner/:id/full`, `/organization/:id/full`, `/network/sync-status`, existing list/count/frequency endpoints.

**Wallet**: Basic connect/disconnect. "Create Profile" CTA. No write actions yet.

---

### Phase 2 — Full Action UX (write-side)

**Goal**: authorized actors can perform all protocol actions through the browser.

**Pages**: My Dojo, Action Composer, Transaction Receipt.

**Components**: Action launcher, form wizard, tx lifecycle steps, toast notifications, contextual action bars on all entity pages.

**API requirements**: `/profiles/:id/pending-actions`, `/actions/eligibility-check`, `/verify/:id`, `/membership-intervals?organization=`, `/achievements/frequency`.

**Wallet**: Full CIP-30 integration. Profile linking. Inline accept/promote/award/grant actions.

---

### Phase 3 — Wow Factor (rich visualization + community)

**Goal**: differentiated, visually stunning features that make the explorer a destination.

**Pages**: Lineage Graph Explorer, Activity Feed, Ranks & Promotions Explorer, Achievements Explorer, Membership Explorer.

**Components**: Force-directed graph, unified timeline, belt pyramid, activity heatmaps, event bundles, story mode.

**API requirements**: `/lineage/:id`, `/events`.

**Features**: Share/embed verification cards, QR codes, Open Graph previews, "Story Mode" lineage animation.

---

## Appendix A — Key component specifications

These pseudo-HTML snippets show the structure and DaisyUI 5 classes for the 5 most important reusable components. Use as direct implementation reference.

### A.1 `BeltBadge`

Displays a practitioner's belt as a colored chip. Accepts `belt` (BJJBelt enum value) and optional `size` (sm/md/lg).

```html
<!-- BeltBadge: belt="Purple", size="md" -->
<span
  class="badge badge-md font-semibold text-white"
  style="background-color: var(--belt-purple);"
  title="Purple Belt"
>
  Purple Belt
</span>

<!-- BeltBadge for Black degrees: belt="Black3" -->
<span class="inline-flex items-center gap-1">
  <span
    class="badge badge-md font-semibold text-white"
    style="background-color: var(--belt-black3);"
    title="Black Belt 3rd Degree"
  >
    Black 3rd°
  </span>
  <!-- Gold degree dots -->
  <span class="flex gap-0.5">
    <span class="w-1.5 h-1.5 rounded-full bg-yellow-500"></span>
    <span class="w-1.5 h-1.5 rounded-full bg-yellow-500"></span>
    <span class="w-1.5 h-1.5 rounded-full bg-yellow-500"></span>
  </span>
</span>
```

### A.2 `EntityCard`

Summary card for a practitioner or organization. Used in directories and search results.

```html
<!-- EntityCard: practitioner -->
<div class="card card-side card-compact bg-base-100 shadow-sm hover:-translate-y-0.5 hover:shadow-md transition-all duration-150 border-l-4" style="border-left-color: var(--belt-purple);">
  <figure class="p-4">
    <div class="avatar">
      <div class="w-16 rounded-full ring ring-offset-2" style="--tw-ring-color: var(--belt-purple);">
        <img src="practitioner-image.jpg" alt="João Silva" />
      </div>
    </div>
  </figure>
  <div class="card-body">
    <h3 class="card-title text-base">
      João Silva
      <!-- BeltBadge -->
      <span class="badge badge-sm text-white" style="background-color: var(--belt-purple);">Purple</span>
    </h3>
    <p class="text-sm text-base-content/70 line-clamp-2">Practitioner at Gracie Barra São Paulo...</p>
    <div class="card-actions justify-end">
      <span class="badge badge-ghost badge-sm">3 achievements</span>
      <span class="badge badge-ghost badge-sm">2 memberships</span>
    </div>
  </div>
</div>

<!-- EntityCard: organization -->
<div class="card card-compact bg-base-100 shadow-sm hover:-translate-y-0.5 hover:shadow-md transition-all duration-150">
  <div class="card-body">
    <div class="flex items-center gap-3">
      <div class="avatar">
        <div class="w-12 rounded-lg">
          <img src="org-logo.jpg" alt="Gracie Barra" />
        </div>
      </div>
      <div>
        <h3 class="card-title text-base">Gracie Barra</h3>
        <p class="text-xs text-base-content/60">São Paulo, Brazil</p>
      </div>
    </div>
    <div class="stats stats-horizontal shadow-none mt-2">
      <div class="stat py-1 px-2">
        <div class="stat-title text-xs">Members</div>
        <div class="stat-value text-lg">127</div>
      </div>
      <div class="stat py-1 px-2">
        <div class="stat-title text-xs">Achievements</div>
        <div class="stat-value text-lg">342</div>
      </div>
    </div>
  </div>
</div>
```

### A.3 `VerificationChip`

Inline widget showing on-chain proof for any credential. Compact by default; expands on click.

```html
<!-- VerificationChip: compact mode -->
<div class="inline-flex items-center gap-1.5 px-2 py-1 rounded-lg bg-base-200 text-xs">
  <span class="badge badge-success badge-xs">✓</span>
  <span class="font-mono text-base-content/70 truncate max-w-[120px]" title="ff80aaaf03a2...474f4c44">
    ff80aa…4c44
  </span>
  <button class="btn btn-ghost btn-xs p-0" title="Copy asset class ID">
    📋
  </button>
  <a href="https://cexplorer.io/asset/..." target="_blank" class="btn btn-ghost btn-xs p-0" title="View on Cardano explorer">
    ↗
  </a>
</div>

<!-- VerificationChip: expanded (in a popover or modal) -->
<div class="card card-compact bg-base-200 shadow-sm w-80">
  <div class="card-body text-xs">
    <div class="flex items-center gap-2 mb-2">
      <span class="badge badge-success">✓ Accepted</span>
      <span class="badge badge-info">⛓ On-chain</span>
    </div>
    <div class="space-y-1">
      <div class="flex justify-between">
        <span class="text-base-content/60">Asset Class</span>
        <kbd class="kbd kbd-xs font-mono">ff80aaaf...474f4c44</kbd>
      </div>
      <div class="flex justify-between">
        <span class="text-base-content/60">Issuer</span>
        <a href="/practitioner/..." class="link link-primary">Master Carlos</a>
      </div>
      <div class="flex justify-between">
        <span class="text-base-content/60">Date</span>
        <span>2025-06-01</span>
      </div>
      <div class="flex justify-between">
        <span class="text-base-content/60">Tx Hash</span>
        <a href="https://cexplorer.io/tx/..." target="_blank" class="link link-primary font-mono">a8d75b...8935 ↗</a>
      </div>
    </div>
  </div>
</div>
```

### A.4 `ActionButton`

Contextual, wallet-aware action trigger. Renders differently based on wallet state.

```html
<!-- ActionButton: wallet disconnected → hidden or "Connect" CTA -->
<!-- (nothing rendered, or:) -->
<button class="btn btn-outline btn-sm">Connect Wallet to Act</button>

<!-- ActionButton: wallet connected, action available -->
<button class="btn btn-primary btn-sm gap-1">
  <svg class="w-4 h-4"><!-- promote icon --></svg>
  Promote to Purple
</button>

<!-- ActionButton: action in progress (after click, building tx) -->
<button class="btn btn-primary btn-sm gap-1" disabled>
  <span class="spinner spinner-sm"></span>
  Building…
</button>

<!-- ActionButton: waiting for wallet signature -->
<button class="btn btn-warning btn-sm gap-1" disabled>
  <span class="spinner spinner-sm"></span>
  Sign in wallet…
</button>

<!-- ActionButton: success (brief flash, then resets) -->
<button class="btn btn-success btn-sm gap-1" disabled>
  <svg class="w-4 h-4"><!-- checkmark --></svg>
  Done!
</button>
```

### A.5 `TimelineEvent`

Single event item in a timeline (used in Belt Journey, Activity Feed, Membership timeline).

```html
<!-- TimelineEvent: promotion accepted -->
<li>
  <div class="timeline-start timeline-box text-sm">
    <time class="text-xs text-base-content/60">2025-06-01</time>
  </div>
  <div class="timeline-middle">
    <!-- Belt-colored milestone dot -->
    <div class="w-4 h-4 rounded-full ring-2 ring-offset-2" style="background-color: var(--belt-purple); --tw-ring-color: var(--belt-purple);"></div>
  </div>
  <div class="timeline-end timeline-box bg-base-200 shadow-sm">
    <div class="flex items-center gap-2 mb-1">
      <span class="badge badge-sm text-white" style="background-color: var(--belt-purple);">Purple Belt</span>
      <span class="badge badge-success badge-xs">✓ Accepted</span>
    </div>
    <p class="text-sm">
      Promoted by
      <a href="/practitioner/..." class="link link-primary">Master Carlos</a>
    </p>
    <p class="text-xs text-base-content/60 mt-1">
      Time at Blue: 2 years, 3 months
    </p>
    <!-- VerificationChip (compact) -->
    <div class="mt-2">
      <div class="inline-flex items-center gap-1 px-1.5 py-0.5 rounded bg-base-300 text-xs font-mono">
        ⛓ ff80aa…4c44
        <a href="https://cexplorer.io/..." target="_blank" class="text-primary">↗</a>
      </div>
    </div>
  </div>
  <hr />
</li>

<!-- TimelineEvent: pending promotion (with action button) -->
<li>
  <div class="timeline-start timeline-box text-sm">
    <time class="text-xs text-base-content/60">2025-07-15</time>
  </div>
  <div class="timeline-middle">
    <div class="w-4 h-4 rounded-full ring-2 ring-offset-2 animate-pulse" style="background-color: var(--belt-brown); --tw-ring-color: var(--belt-brown);"></div>
  </div>
  <div class="timeline-end timeline-box border-warning border-2 bg-warning/10 shadow-sm">
    <div class="flex items-center gap-2 mb-1">
      <span class="badge badge-sm text-white" style="background-color: var(--belt-brown);">Brown Belt</span>
      <span class="badge badge-warning badge-xs">⏳ Pending</span>
    </div>
    <p class="text-sm">
      Promoted by
      <a href="/practitioner/..." class="link link-primary">Master Roberto</a>
    </p>
    <!-- ActionButton: only shown if wallet matches this practitioner -->
    <div class="mt-2">
      <button class="btn btn-primary btn-sm">Accept Promotion</button>
    </div>
  </div>
  <hr />
</li>
```

---

This structure keeps the product useful as a public explorer from day one, while progressively unlocking powerful write-side workflows for authorized actors—and looking awesome while doing it.
