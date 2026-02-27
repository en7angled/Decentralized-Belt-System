# Explorer Website UI Design (DaisyUI 5 · Tailwind CSS 4)

> **Baseline**: DaisyUI **v5** on Tailwind CSS **4**. All component names, class names, and theming APIs in this document target that stack. When DaisyUI ships breaking changes, update this document accordingly.

This document defines the **UI requirements and design** for the Decentralized BJJ Belt protocol explorer website. It covers:

- **Read-side explorer** (public, wallet optional)
- **Write-side actions** (wallet-connected, role/context-aware)

The goal is to make it feel like a premium Web3 explorer + social credential network for BJJ.

---

## Table of contents

1. [Product principles](#1-product-principles)
2. [Information architecture (sitemap)](#2-information-architecture-sitemap)
3. [Reusable UI kit (DaisyUI 5 components)](#3-reusable-ui-kit-daisyui-5-components)
4. [Page-by-page UX blueprint](#4-page-by-page-ux-blueprint)
5. [Contextual action system](#5-contextual-action-system)
6. [Wallet connection flow](#6-wallet-connection-flow)
7. [Onboarding & first-time experience](#7-onboarding--first-time-experience)
8. [Notification & pending-actions system](#8-notification--pending-actions-system)
9. [Error, loading, & empty states](#9-error-loading--empty-states)
10. [Responsive & mobile design](#10-responsive--mobile-design)
11. [Visual style: modern and "cool"](#11-visual-style-modern-and-cool)
12. [Appendix A — key component specifications](#appendix-a--key-component-specifications)

---

## 1) Product principles

### Explorer-first

- All browse/search/filter/sort flows work **without wallet connection**.
- Wallet UI should not block public content.
- Public pages should always display **verifiable evidence snippets** (on-chain tx links for events, asset links for entities).

### Action-contextual

- When wallet is connected and authorized, actions must be available **inline near relevant data**:
  - On practitioner page: **Promote**, **Award achievement**, **Grant membership**
  - On pending promotion card: **Accept promotion**
  - On pending membership interval: **Accept membership**
  - On pending achievement: **Accept achievement**
- Actions that cannot be performed by the current wallet are **hidden—never greyed out**—so the UI stays clean.

### Trust by design

- Every credential view includes a compact **verification panel**:
  - Asset class ID (truncated with copy-to-clipboard)
  - Issuer profile (avatar + name link)
  - Date
  - Acceptance state (`badge badge-success` / `badge badge-warning`)
  - Tx hash with Cardano explorer deep-link
- On-chain proof is never more than one click away from any credential display.

---

## 2) Information architecture (sitemap)

### Public routes (no wallet required)

| # | Route | Label | Child Components |
|---|---|---|---|
| 1 | `/` | Home / Explorer Hub | `IntroHero`, `UnifiedSearch`, `CommunityDashboard`, `GlobalActivityFeed` |
| 2 | `/practitioners` | Practitioner Explorer | `PractitionersStats`, `PractitionersSearch` |
| 3 | `/practitioner/:id` | Practitioner Profile ★ | `PractitionerInfoCard`, `PractitionerTimeline`, `RankLineageGraph` |
| 4 | `/organizations` | Organization Explorer | `OrganizationsStats`, `OrganizationsSearch` |
| 5 | `/organization/:id` | Organization Profile | `OrganizationInfoCard` |
| 6 | `/ranks` | Belt Promotions Explorer | `BeltPromotionsStats`, `BeltPromotionsSearch` |
| 7 | `/achievements` | Achievements Explorer | `AchievementStats`, `AchievementsSearch` |
| 8 | `/memberships` | Membership Explorer | `MembershipStats`, `MembershipsSearch` |
| 9 | `/lineage` | Lineage Graph Explorer | |
| 10 | `/about` | About the Protocol | |

### Wallet routes (connected users only)

| # | Route | Label | Child Components |
|---|---|---|---|
| 11 | `/my-dojo` | My Dojo | `WalletSummary`, `PendingActions` (per profile), `AllowedActions` (per profile) |

### Global overlay (public)

| Component | Label | Description |
|---|---|---|
| `ActionComposer` | Action Wizard | Full-page multi-step wizard with sidebar progress indicator |

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

### 4.1 Home / Explorer Hub (`/`)

The single entry point combining the hero introduction, global search, community statistics, and live activity.

#### Layout
Full-width hero → unified search → community dashboard → global activity feed.

#### Child components

**`IntroHero`**
- Hero section with protocol value proposition, animated belt-spectrum gradient bar, animated lineage graph visualization, and CTAs to create a profile for practitioners and for organizations.
- Two CTAs: "Explore the Protocol" (primary) + "Connect Wallet" (outline).
- **"How It Works"** 3-step horizontal `steps` component:
  1. "Create your on-chain profile" (icon: person+)
  2. "Earn promotions & credentials" (icon: belt)
  3. "Verify anywhere, forever" (icon: shield-check)

**`UnifiedSearch`**
- Unified full-text search across practitioners, organizations, ranks, achievements, and memberships.
- Command-palette trigger (⌘K / Ctrl+K): opens a `modal` with `input input-bordered input-lg` and live results rendered as a `menu` of entity links.
- Search results grouped by entity type with `divider` separators.
- Keyboard navigation through search results (↑↓ + Enter).
- When a record is clicked, the user is redirected to the respective route.

**`CommunityDashboard`**
- **Stats grid** — Four summary numbers: total achievements, total practitioners, total academies (organizations), and pending promotions. Uses `stat` × 4 with `stat-title`, `stat-value`, `stat-desc`.
- **RecentlyRegisteredCard** — Bar chart showing how many practitioners are at each belt level (recently registered).
- **BeltJourneyPyramid** — Pyramid chart of belt distribution across all ranks. Each belt level is a layer, wider at the bottom (White) and narrower at the top (Red). Uses exact belt colors from §11.
- **Promotions TTM** — Stacked bar chart of belt promotions per month over the last twelve months, colored by belt.
- **Latest Academies** — List of the most recently registered academies (name, description, short ID).
- **Latest Practitioners** — List of the most recently registered practitioners (name, belt, short ID).
- **Recent Belt Promotions** — List of recent promotions: who got which belt, who awarded it, with a link to view on the explorer.

**`GlobalActivityFeed`**
- Feed with "natural language representation" of each event type, e.g.:
  - "Andrei Ionescu promoted Marius Georgescu to Blue Belt"
  - "Marius Georgescu accepted Blue Belt promotion of Andrei Ionescu"
  - "Ionut Popescu received achievement 'name' from Atos JiuJitsu"
- Each event shows: actor(s) avatar + name links, summary text, relative timestamp ("3 hours ago"), on-chain tx link.
- Color-coded left border on each event card matching the event type.
- **Filter chips** (`filter` ★ pills): All | Profiles | Promotions | Achievements | Memberships.

#### DaisyUI 5

```
hero, stat, steps, card, badge, btn, modal, input, menu, filter, timeline, timeline-box, avatar, skeleton, divider, popover
```

#### Cool ideas
- Animated belt-spectrum gradient bar (`@keyframes` shifting hue across the 15 belt colors) under the hero.
- Parallax background showing faint BJJ mat pattern.
- **Live pulse animation**: when a new event arrives, the new item slides in from the top with a subtle green flash border.
- **"Event bundles"**: when the same actor performs multiple actions in a short window (e.g., master promotes 3 students), collapse into a single bundle with expandable details (`collapse`).

#### Empty state
On a fresh protocol with zero profiles: replace CommunityDashboard stats with a single centered `card` inviting the first user to create a profile. Hide activity feed.

---

### 4.2 Practitioner Explorer (`/practitioners`)

#### Layout
Stats summary → sticky filter bar → results area (card grid or table) → pagination.

#### Child components

**`PractitionersStats`**
- Summary strip at the top showing key practitioner metrics: total count, distribution by belt (compact bar or pills), recent activity count.

**`PractitionersSearch`**
- **Filter bar** — horizontal strip:
  - `input input-bordered` for name/ID search
  - `select` for current belt level
  - `select` for awarded-by (organization/master)
  - `calendar` ★ for date range (profile created / last promoted)
  - `checkbox` "Has pending actions" (only meaningful when wallet connected)
  - Reset button
- **Sort dropdown** (`select select-sm`): Newest rank first, Name A→Z, Belt (highest first).
- **View toggle**: `filter` component (Grid / Table).
- **Result cards** (`card card-side card-compact` in grid) or **table rows** (`table table-zebra`):
  - Each result shows: `avatar` (or placeholder), name, belt `badge` with belt-color background, organization affiliation (if any), last promotion date.
  - Belt-colored left border (`border-l-4`) on each card/row using the belt palette.
- **Pagination** (`join` with page buttons) at bottom.

#### DaisyUI 5

```
input, select, checkbox, calendar, filter, card, avatar, badge, table, join, pagination, skeleton, popover, stat
```

#### Interactive behaviors
- **Hover preview** (`popover`): hovering a practitioner card shows a mini profile preview (belt journey summary, last 2 ranks, active memberships count) without navigating away.
- **Click** → navigates to `/practitioner/:id`.

#### Cool ideas
- Belt-colored left border on each card/row matching their current rank.
- Mini sparkline-style dot strip on each card showing recent activity (last 5 events as colored dots: green = promotion accepted, blue = achievement, purple = membership).

#### Empty state
"No practitioners found" card with CTA: "Be the first — Create your profile" (links to action composer if wallet connected, or to wallet connect flow).

---

### 4.3 Practitioner Profile (`/practitioner/:id`) — ★ Flagship page

This is the most important page. It must feel rich, trustworthy, and visually stunning.

#### Layout
Full-width identity card → tabbed content area (Belt Journey | Achievements | Memberships | Promoted Students) → rank lineage graph → contextual action bar (floating or sticky bottom).

#### Child components

**`PractitionerInfoCard`**
- Large `avatar avatar-lg` (or `avatar avatar-placeholder` with initials)
- Name (h1), description excerpt, profile ID (truncated `kbd` with copy)
- Current belt `badge badge-lg` with belt-color background + glow ring (`ring ring-offset-2` with belt color)
- Verification chip: `badge badge-success badge-sm` "On-chain verified" with tooltip showing profile ref token asset class and Cardano explorer deep-link
- If wallet connected and this is the user's profile: `btn btn-outline btn-sm` "Edit Profile"

**`PractitionerTimeline`**
- Tabbed content area with the following tabs:

  **Tab: Belt Journey** (default active)
  - Vertical `timeline` with `timeline-box` items, one per rank (current + all previous):
    - Belt-colored milestone dot (filled circle in belt color)
    - `timeline-box` content: Belt name + `badge`, promoter name (link to their profile), date, time-in-belt duration label (e.g., "2 years, 3 months at Blue")
    - On-chain tx deep-link
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

**`RankLineageGraph`**
- Interactive graph showing this practitioner's promotion lineage:
  - **Practitioner nodes**: circle, sized by number of promotions given, filled with belt-color ring.
  - **Promotion edges**: directed arrows (master → student), colored with target belt and labeled with date.
- **Controls drawer** (`drawer drawer-end`):
  - Belt filter: checkboxes to show/hide nodes by belt.
  - Date range: `calendar` to limit edges to a time window.
  - Depth slider: `range` to control how many levels of lineage to show from the root.
  - Root profile selector: `input` with autocomplete to pick a starting practitioner.
- **Node click**: opens `popover` with mini profile card; double-click navigates to profile page.

#### DaisyUI 5

```
avatar, badge, tabs, timeline, timeline-box, card, alert, modal, btn, table, kbd, tooltip, popover, filter, skeleton, accordion, drawer, range, toggle
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

### 4.4 Organization Explorer (`/organizations`)

#### Layout
Stats summary → filter bar → card grid → pagination.

#### Child components

**`OrganizationsStats`**
- Summary strip showing key organization metrics: total count, most active this month, total members across all orgs.

**`OrganizationsSearch`**
- **Filter/search bar**: name search `input`, activity level `select` (Most active / All), recent awards toggle.
- **Organization cards** (`card`) in responsive grid:
  - Organization logo/avatar
  - Name, description excerpt
  - `stat` strip: Active Members count, Achievements Issued count, Latest Activity date
  - `badge` chips for quick info (e.g., "12 active members", "47 achievements")
- **Pagination** at bottom.

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

### 4.5 Organization Profile (`/organization/:id`)

#### Layout
Identity header → tabbed content (Members | Achievements Issued | Membership Activity) → contextual actions.

#### Child components

**`OrganizationInfoCard`**
- Organization avatar, name (h1), description, profile ID (`kbd` with copy)
- Verification chip with Cardano explorer deep-link
- KPI strip (`stat` × 3): Active Members, Total Achievements Issued, Membership Histories

**Tabs**

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
- **"Academy network" mini-graph**: a small force-directed graph showing the organization at center with connected practitioners as nodes, sized by belt level, colored by belt. Clickable.
- **Activity heatmap**: a GitHub-style contribution heatmap showing membership grants + achievement awards by week, for the last 12 months.

#### Empty state
If org ID not found: 404 + search. If org has zero members: "This organization has no members yet" card.

---

### 4.6 Belt Promotions Explorer (`/ranks`)

#### Layout
Stats summary → tab bar (Accepted Ranks | Pending Promotions) → filter bar → content area → pagination.

#### Child components

**`BeltPromotionsStats`**
- **Belt distribution chart**: horizontal stacked bars, one per belt level, colored with belt palette, showing count.
- **Belt pyramid**: visual pyramid with each belt level as a layer, wider at the bottom (White) and narrower at the top (Red). Shows count labels. Uses exact belt colors from §11.
- Hover tooltips on pyramid showing exact count + percentage per level.
- "Promotion velocity" stat: average time between promotions across the protocol.

**`BeltPromotionsSearch`**
- **Tab: Accepted Ranks** (default)
  - **Data table** (`table table-zebra`): filterable, sortable list of all accepted ranks.
    - Columns: Practitioner (avatar + name), Belt (`badge`), Awarded By (name link), Date, Rank ID (`kbd`)
    - Filters: belt level `select`, awarded by `select`, date range `calendar`
    - Sort: belt level, date, practitioner name

- **Tab: Pending Promotions**
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
- Animated confetti burst when viewing a promotion that was just accepted (within last 24h).

#### Empty state
"No ranks recorded yet — the protocol is just getting started!" with explore CTA.

---

### 4.7 Achievements Explorer (`/achievements`)

#### Layout
Stats summary → filter bar → view toggle (Gallery | Table) → content → pagination.

#### Child components

**`AchievementStats`**
- Summary strip: total achievements, accepted count, pending count, most active awarding organization.

**`AchievementsSearch`**
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
  - Complete verification details (asset class, issuer profile, date, tx hash with Cardano explorer link)
  - Acceptance state with "Accept" button if applicable

#### DaisyUI 5

```
card, badge, tooltip, modal, kbd, filter, input, select, calendar, table, pagination, skeleton, stat
```

#### Cool ideas
- Masonry gallery with subtle hover zoom (CSS `scale(1.02)` on hover).
- Achievement cards have a subtle ribbon in the top-right corner showing acceptance state.

#### Empty state
"No achievements found" + conditional CTA: "Award your first achievement" (if org owner connected) or "Check back later."

---

### 4.8 Membership Explorer (`/memberships`)

#### Layout
Stats summary → sub-tabs (Histories | Intervals) → filter bar → content → pagination.

#### Child components

**`MembershipStats`**
- Summary strip: total membership histories, active intervals, ended intervals, pending acceptance count.

**`MembershipsSearch`**
- **Sub-tab: Histories**
  - `table` of membership histories:
    - Practitioner (avatar + name link)
    - Organization (avatar + name link)
    - Number of intervals
    - Current status (active / ended / pending)
    - History ID (`kbd`)
  - Expandable rows (`accordion` ★): click a row to expand and see all intervals inline.

- **Sub-tab: Intervals**
  - `timeline` view: intervals as chronological events.
    - Each interval: start → end, practitioner name, organization name, acceptance state.
    - Active intervals: `badge badge-info` "Active" with green left border.
    - Ended intervals: `badge badge-neutral` "Ended" with grey left border.
    - Pending acceptance: `badge badge-warning` "Pending" with yellow left border.
    - If wallet matches and pending: "Accept" `btn btn-sm`.

#### DaisyUI 5

```
tabs, table, accordion, timeline, badge, btn, filter, select, calendar, pagination, skeleton, stat
```

#### Cool ideas
- **"Membership lifecycle" ribbon**: per practitioner × organization, a horizontal bar where colored segments represent each interval proportionally (green = active, grey = ended, yellow = pending).
- **"Interval stack" card**: for a single practitioner-org pair, show all intervals stacked vertically like a timeline ruler, showing renewals and extensions over time.

#### Empty state
"No memberships recorded yet" + CTA.

---

### 4.9 Lineage Graph Explorer (`/lineage`)

#### Layout
Full-screen graph canvas → `drawer` (right-side) for controls/details → breadcrumb path display.

#### Contains
- **Interactive graph** (powered by D3-force, vis.js, or Cytoscape.js):
  - **Practitioner nodes**: circle, sized by number of promotions given, filled with belt-color ring.
  - **Organization nodes**: square, with org avatar.
  - **Promotion edges**: directed arrows (master → student), colored with target belt and labeled with date.
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

### 4.10 About the Protocol (`/about`)

#### Layout
Centered single-column content page.

#### Contains
- **Protocol overview**: brief explanation of what the Decentralized BJJ Belt System is and its mission (transparency, trust, verifiability for BJJ credentials).
- **How it works**: visual explainer of the core concepts:
  - Profiles (Practitioner & Organization)
  - Belt promotions (two-phase: issue + accept)
  - Achievements (awarded + accepted)
  - Memberships (grant + accept intervals)
- **Belt hierarchy**: visual chart showing all belt levels from White to Red 10th Degree, using belt color palette from §11.
- **Key principles**: Explorer-first, Action-contextual, Trust by design (referencing §1).
- **Links**: to the project's documentation, source code repository, and Cardano Project Catalyst proposal.
- **FAQ section** using `accordion` ★:
  - "What is a belt promotion?"
  - "What does 'pending acceptance' mean?"
  - "How do I create a profile?"
  - "What wallet do I need?"

#### DaisyUI 5

```
card, accordion, steps, badge, btn, divider, kbd, tooltip
```

#### Empty state
N/A — this is a static content page.

---

### 4.11 My Dojo (`/my-dojo`) — Connected users only

#### Layout
Wallet summary bar → pending-actions inbox (per profile) → allowed actions (per profile).

#### Child components

**`WalletSummary`**
- Connected wallet address (truncated, with copy)
- Wallet identicon/avatar
- Owned protocol profiles listed as `badge` chips (click → jump to profile)
- "Disconnect" `btn btn-ghost btn-sm`

**`PendingActions`** (per profile)
- Sorted by urgency (oldest pending first).
- Each item is a `card card-compact card-bordered` with:
  - Action type icon + label (e.g., "Accept Promotion to Purple Belt")
  - Issuer name + avatar
  - Date issued + "X days ago" relative time
  - "Accept" `btn btn-primary btn-sm` → triggers wallet signing flow inline
  - "View Details" → show all details in modal
- Includes: accept promotions + accept achievements + accept memberships.
- If zero pending: `alert alert-info` "You're all caught up! No pending actions."

**`AllowedActions`** (per profile)
- Role-based suggested actions for each owned profile:
  - If user is a master (Black+): "Promote a student" card
  - If user is an organization: "Grant membership" card, "Award achievement" card
  - Always: "Update profile image" card
- Each card links to the Action Composer (§4.12) pre-filled with the relevant action type.

#### DaisyUI 5

```
stat, card, alert, badge, btn, table, kbd, toast, avatar, skeleton, modal
```

#### Cool ideas
- **"Action urgency" ranking**: pending items have a color-coded time indicator (green < 7d, yellow 7–30d, red > 30d since issued).
- **"Daily dojo" summary**: at the top, a one-sentence summary of today's activity: "You accepted 1 promotion and earned 2 achievements today" or "Nothing new today — time to train! 🥋".

#### Empty state
If wallet has no protocol profiles: large centered CTA card: "Welcome to the Protocol — Create your first profile" with wizard link.

---

### 4.12 Action Composer (`ActionComposer` / Action Wizard)

Full-page multi-step wizard with sidebar progress indicator. Available as a global overlay accessible from any page.

#### Layout
Sidebar progress indicator (desktop) or top bar (mobile) → step content area.

#### Step indicator

`steps steps-vertical` on desktop sidebar, `steps steps-horizontal` on mobile top bar:
1. Select Action (available depending if a profile is selected or not, and if yes depending on the profile type)
2. Fill Details
3. Review & Preview
4. Sign in Wallet
5. Track Submission

#### Action table

| Accessibility | Action | Wizard Description | Result |
|---|---|---|---|
| Public | Create profile | Create practitioner/org | `InitProfileAction` |
| Connected users only | Update Profile Image | Update image URI | `UpdateProfileImageAction` |
| Connected users only | Belt Issuance | Master promotes practitioner | `PromoteProfileAction` |
| Connected users only | Accept Promotion | Practitioner accepts promotion | `AcceptPromotionAction` |
| Connected users only | Award Achievement | Org/master awards achievement | `AwardAchievementAction` |
| Connected users only | Accept Achievement | Practitioner accepts | `AcceptAchievementAction` |
| Connected users only | Grant Membership | Create membership history | `CreateMembershipHistoryAction` |
| Connected users only | Renew Membership | Add new interval to existing history | `AddMembershipIntervalAction` |
| Connected users only | Accept Membership | Practitioner accepts interval | `AcceptMembershipIntervalAction` |
| Connected users only | End Membership | Set end date on interval | `UpdateEndDateAction` |

#### Step details

**Step 1: Select Action**
- Grid of action `card` items, each with icon, title, description.
- Cards for actions that require a connected wallet are hidden when no wallet is connected.
- Cards for actions that require a specific profile type are hidden when no matching profile is available.

**Step 2: Fill Details**
- `fieldset` ★ groups for each form section:
  - Profile details: name `input`, description `textarea`, image URI `input` (with preview), profile type `select`
  - Promotion details: student profile `input` (with autocomplete/search), belt `select`, date `calendar`
  - Achievement details: recipient `input`, name `input`, description `textarea`, image URI `input`, metadata key-value pairs (dynamic `input` rows), date `calendar`
  - Membership details: organization `select`, practitioner `input`, start date `calendar`, end date `calendar` (optional)
- Real-time validation with inline error messages.

**Step 3: Review & Preview**
- Side-by-side layout:
  - Left: **"Human summary"** — readable card: "You are promoting João Silva from Blue Belt to Purple Belt, authorized by Master Carlos, effective June 1, 2025."
  - Right: **"Raw payload"** — `mockup-code` JSON preview of the interaction payload.
- Protocol fee breakdown panel (if fees are configured):
  - Action fee (e.g., 2 ADA promotion fee)
  - Estimated transaction fee
  - Total
- "Edit" button to go back to Step 2.

**Step 4: Sign in Wallet**
- `loading` indicator: "Building transaction…"
- On success: "Transaction built. Please sign in your wallet."
- Wallet signing prompt.
- On signing: "Submitting transaction…"
- Error handling: if build fails, show `alert alert-error` with the error message.

**Step 5: Track Submission**
- `steps` component showing tx lifecycle:
  1. ✅ Built
  2. ✅ Signed
  3. ⏳ Submitted (waiting for confirmation)
  4. ✅ Confirmed (with tx hash + Cardano explorer link)
- On confirmation: `toast` "Transaction confirmed!" + confetti animation.
- "Do Another Action" button → resets wizard.

#### DaisyUI 5

```
steps, fieldset, input, textarea, select, calendar, file-input, toggle, card, modal, alert, btn, loading, spinner, toast, kbd
```

#### Cool ideas
- Side-by-side "Human summary" + "Raw payload" in Step 3 — advanced users can inspect exactly what's being sent.
- **Fee estimate** with a visual bar: "This transaction will cost approximately 0.3 ADA in network fees + 2 ADA protocol fee."
- **"Quick action" shortcuts**: from any entity page, clicking an action button pre-fills the wizard with context (student ID, etc.) and jumps directly to Step 2.
- **Draft saving**: if the user navigates away mid-wizard, save the draft to localStorage and offer to resume.

---

## 5) Contextual action system

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
- Actions that would fail (e.g., promoting someone who outranks you) are **hidden, not disabled**.

### Action flow (UX)
1. User clicks inline action button.
2. If action needs additional input: open `modal` with the required form (pre-filled with context).
3. Show `spinner` while the transaction is being built.
4. Prompt wallet signing.
5. Show progress `steps` while the transaction is being submitted.
6. On confirmation: `toast` success + update UI state (refetch data).
7. On error: `alert alert-error` with message + "Try Again" button.

---

## 6) Wallet connection flow

### Wallet selector

Triggered by "Connect Wallet" button in the navbar. Opens a `modal modal-bottom` (mobile) or `modal` (desktop):

- Title: "Connect your Cardano wallet"
- Grid of wallet option `btn` items with wallet icons:
  - Eternl, Lace, Nami, Flint, GeroWallet, Typhon, Yoroi, NuFi
  - Each button detects if the wallet browser extension is installed.
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

After wallet connection, the system identifies which protocol profile(s) the wallet owns. The result determines contextual action visibility:

- **Zero profiles**: show "Create Profile" CTA.
- **One profile**: auto-select it as active.
- **Multiple profiles**: show a profile selector dropdown in the navbar (rare but possible if wallet manages multiple profiles).

---

## 7) Onboarding & first-time experience

### Zero-state home page
When the protocol has < 10 profiles, the home page shows an "Early Adopter" mode:
- Replace CommunityDashboard with a large explainer `card`:
  - "You're early! This protocol has [N] profiles."
  - "Be among the first to bring your BJJ credentials on-chain."
  - "Connect Wallet & Create Profile" `btn btn-primary btn-lg`

### "Get Started" wizard
Accessed from the home CTA or "Create Profile" flows. This is the Action Composer (§4.12) with a friendlier wrapper:
- Pre-selects "Create Profile (Practitioner)" as the action.
- Adds contextual help tooltips on each field: "Your name as it appears in competitions", "A photo of you — URI to an image hosted anywhere", etc.
- After successful profile creation: `modal` celebration with confetti + "Welcome to the Protocol!" message + "Explore your profile" link.

### Contextual tooltips
On first visit (tracked via localStorage), show dismissible `tooltip tooltip-open` hints on:
- The search bar: "Search for practitioners, organizations, or credentials."
- The wallet connect button: "Connect your wallet to take actions."
- The lineage graph: "Explore the promotion lineage between practitioners."

### "What is this?" explainer cards
On complex pages (Lineage Graph, About, Activity Feed), show a collapsible `accordion` at the top:
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
2. When confirmed: `toast` "Transaction confirmed ✅" (`alert alert-success`) with link to view the transaction on a Cardano explorer.
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

- **Inline error** (data load fails for one section): `alert alert-error alert-sm` replacing the failed section content: "Failed to load [section name]. [Retry →]"
- **Full-page error** (page-level data failure): centered `card` with error icon, message, "Retry" button, and "Go Home" link.
- **Network error** (no connectivity): full-width sticky `alert alert-error` at top: "Network error — please check your connection."

### 404 page
- Centered layout.
- Large "404" text with a BJJ-themed illustration (e.g., a broken belt or an empty mat).
- "Page not found" message.
- Global search bar (same as UnifiedSearch command palette).
- Quick links: "Browse Practitioners", "Browse Organizations", "Go Home".

### Chain-sync lag indicator
When the data source is behind the chain tip:
- Subtle `alert alert-warning alert-sm` banner at the top of every page:
  - "Data may be up to [N] blocks behind. Last synced: [timestamp]."
  - Auto-dismisses when sync catches up.

### Empty states per entity type

| Page | Empty state message | CTA |
|---|---|---|
| Practitioner Explorer | "No practitioners have joined yet." | "Create the first profile" |
| Organization Explorer | "No organizations registered yet." | "Register your academy" |
| Achievements Explorer | "No achievements have been awarded yet." | "Award the first achievement" |
| Membership Explorer | "No memberships created yet." | "Grant a membership" |
| GlobalActivityFeed | "The protocol is quiet — no activity yet." | "Create a profile to get started" |
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
- 5 items: Home 🏠 | Search 🔍 | Lineage 🌳 | My Dojo 🥋 | More ⋯
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

### Trust aesthetics

Always show verification chips on credential displays:

| State | Component | Styling |
|---|---|---|
| Accepted | `badge badge-success badge-sm` | "✓ Accepted" — green |
| Pending acceptance | `badge badge-warning badge-sm` | "⏳ Pending" — yellow/amber |
| Invalid / Not found | `badge badge-error badge-sm` | "✗ Invalid" — red |
| On-chain verified | `badge badge-info badge-sm` | "⛓ On-chain" — blue, with Cardano explorer link |

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

Summary card for a practitioner or organization. Used in explorer pages and search results.

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

<!-- ActionButton: action in progress (building tx) -->
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
