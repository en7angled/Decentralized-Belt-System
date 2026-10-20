# F1 — Preprod infrastructure on the minirack, and CI

**Track 5 design note.** Date: 2026-10-19. Target: spec §8, §9 F1 check 1 and check 16.
Placeholder app identifier: **`palmares`** (no dashes, per `apps-k8s-configs/CLAUDE.md` naming
convention). When the owner picks the final name it is a single find/replace across this note.

Every cluster number below is **measured live** on `minirack-lan` on 2026-10-19, not inferred
from the repo. Every external fact carries its source URL.

---

## 0. Headline findings that change the plan

| # | Finding | Consequence |
|---|---|---|
| 0.1 | **A preprod `cardano-node` 11.1.2 already runs in the cluster**, synced, on `minirack-pi5-1` (`cardano-preprod/statefulset-cardano-preprod-0`, 100Gi PVC, measured **21 GB** on disk). A *second* full preprod node also runs in `hydra-m5` on `minirack-pi5-2`. | **(decision)** Build **no new preprod node**. F1 check 1 reduces to "add db-sync beside the existing node". Deliverable 3 (Mithril bootstrap) is **not on the F1 critical path** — it is only needed to rebuild the node or for contributors' local stacks. |
| 0.2 | **IntersectMBO's db-sync images are amd64-only** (`ghcr.io/intersectmbo/cardano-db-sync:13.7.3.0` config blob: `os=linux arch=amd64`). arm64 comes from **blinklabs-io**, whose newest tag is **13.7.1.0** — *not* 13.7.2.1 as the spec assumes. Upstream aarch64 is listed under **"Unreleased"** in the CHANGELOG ("Add static binaries and docker images for aarch64-linux"). | **(decision)** Pin `ghcr.io/blinklabs-io/cardano-db-sync:13.7.1.0`. Spec §8 version table needs this correction. **Not a blocker** — a real multi-arch index exists, verified below. |
| 0.3 | **No preprod db-sync Postgres snapshot exists.** `update-cardano-preprod.iohk.io` and `update-cardano-testnet.iohk.io` do not resolve (curl: `000`); only `update-cardano-mainnet.iohk.io` answers `200`. | A full from-genesis db-sync pass on preprod is **mandatory and unavoidable**. Mithril accelerates the **node only**, never db-sync. Budget it as a one-off multi-day step, and **publish our own snapshot** afterwards (§4.3). |
| 0.4 | **CORRECTED 2026-10-19 (fact-check).** `configuration.md` prose says a preset "overrides all other properties", but **the code says otherwise**: `FromJSON SyncInsertConfig` loads the preset as `baseOptions` and then runs `parseOverrides obj baseOptions`, where every explicit key wins (`obj .:? "offchain_vote_data" .!= sioOffchainVoteData baseOptions`) — [`Cardano/DbSync/Config/Types.hs#L465-L495` @13.7.1.0](https://github.com/IntersectMBO/cardano-db-sync/blob/13.7.1.0/cardano-db-sync/src/Cardano/DbSync/Config/Types.hs#L465). So `preset: only_governance` + `offchain_vote_data: "enable"` **works exactly as intended**. | **(decision, revised)** **Use the preset**, with `offchain_vote_data: "enable"` as the one override (§1.3). The hand-written option set is not just unnecessary, it is *wrong*: with no `preset` the base is `def`, which has `sioMetadata = MetadataEnable` and `sioRewards = RewardsConfig True` ([`Types.hs#L798-L816`](https://github.com/IntersectMBO/cardano-db-sync/blob/13.7.1.0/cardano-db-sync/src/Cardano/DbSync/Config/Types.hs#L798)) — i.e. it silently turns `tx_metadata` and `reward` back **on**, the two biggest tables §1.2 claims to have dropped. And `sioRewards` is `pure (sioRewards baseOptions)` — **not settable from JSON at all**, so only a preset can disable rewards. |
| 0.5 | With `offchain_vote_data: "enable"`, db-sync itself fetches every CIP-100/108/119 anchor (including `ipfs://`, via the top-level `ipfs_gateway` option, default `["https://ipfs.io/ipfs"]`), verifies the blake2b hash, stores the content, and records failures in `off_chain_vote_fetch_error`. | **(decision — delete scope, with one carve-out)** The spec's §4.1 "Metadata fetcher" **should not be built** — no HTTP client, no retry loop, no cache. But F1 check 7's four verdicts are **not** a pure read: see the corrected §1.4.1 — `off_chain_vote_data.is_valid` means *CIP-100 schema conformance*, not hash validity, and `off_chain_vote_fetch_error` has only a free-text `fetch_error` column, so a small classifier is needed. |
| 0.6 | PostgreSQL cannot join across databases without `postgres_fdw`. | **(decision)** Our projections live in a **`registry` schema inside the db-sync database** (`cexplorer`), not in a separate database. The projection step becomes `INSERT INTO registry.x SELECT … FROM public.voting_procedure …` — plain SQL, no FDW, no extra connection pool. |
| 0.7 | GitHub-hosted **`ubuntu-24.04-arm` runners are free and unlimited on public repositories** (4 vCPU / 16 GB) — [GA announcement, 2025-08-07](https://github.blog/changelog/2025-08-07-arm64-hosted-runners-for-public-repositories-are-now-generally-available/). And **this account's public-repo Actions demonstrably work today**: `mariusgeorgescu/solana-haskell-sdk` (public, Haskell, GHC 9.6) ran `CI` to `success` on **2026-10-19T18:19→18:31Z**, 12 minutes. | **(decision)** No self-hosted runner. The `apps-k8s-configs` billing block (`CLAUDE.md` line 49) affects **private** repos only; the new repo is public from commit 1 (spec §7), so it is a non-issue. Retire the `runs-on: self-hosted` pattern inherited from the Belt System. |
| 0.8 | db-sync's LSM backend **explicitly crashes on Raspberry Pi**: *"When using the LSM storage backend, `db-sync` could crash due to how `ghc` handles system threads. This only happens on resource-constrained devices such as the Raspberry Pi"* — [13.7.2.1 release notes, Known issues](https://github.com/IntersectMBO/cardano-db-sync/releases/tag/13.7.2.1). Same section: LSM cannot read blocks with >4096 items (`blockio-uring` bug). | **(decision)** Leave `ledger_backend` **unset** (default `"inmemory"`). Never set `"lsm"` on this cluster. Note: this is a *db-sync* setting and is **independent** of the *node's* `LedgerDB.Backend: V2LSM`, which is already in use and fine. |

---

## 1. Resource plan

### 1.1 Measured cluster state (2026-10-19, `kubectl --context minirack-lan`)

| Node | arch | CPU | alloc. mem | **mem requested** | **mem really used / available** | disk free | notes |
|---|---|---|---|---|---|---|---|
| `minirack-pi5-1` | arm64 | 4 (3500m alloc.) | 13.6 Gi | 6304Mi (45%) | 6.09 Gi used / **9.51 Gi avail** | **767 GB** | control-plane+etcd. **Hosts the preprod node** (2.88 Gi). Honcho, Grafana, some DISCO volumes. |
| `minirack-pi5-2` | arm64 | 4 (3500m) | 13.6 Gi | 6348Mi (45%) | — | **270 GB** | jellyfin 700Gi PVC eats the disk; hydra-m5 preprod node; kupo-preview. |
| `minirack-pi5-3` | arm64 | 4 (3500m) | 13.6 Gi | 4480Mi (32%) | 4.75 Gi used / 10.85 Gi avail | **824 GB** | most free node. defibot mainnet scanner+DB, preview node. |
| `minirack-pi5-4` | arm64 | 4 (3750m) | 14.6 Gi | 13862Mi (**92%**) | — | 638 GB | mainnet BP. **Excluded** — no noisy neighbours (CLAUDE.md). |
| `minirack-m4` | arm64 | 8 | 13.6 Gi | 1060Mi (13%) | — | 275 GB | public mainnet relay `cardano-mainnet-m4` + mempool-feed + submit-api, chain on hostPath/external SSD. Strongest node but reserved. ⚠️ the taint named in the first draft (`dedicated=m4:NoSchedule`) is **not** what `apps-k8s-configs/CLAUDE.md` records: the documented taint is `dedicated=m4-keys`, on the *other* M4 VM (`minirack-m4-keys`, the mainnet signer). Re-read the live taint before relying on either. |
| `rockpi-4a` | arm64 | 6 | 3.26 Gi | 2336Mi (69%) | — | 188 GB | 4 GB box. **Excluded** — too small. |

`minirack-m4-keys` is in `CLAUDE.md` but **not in the live node list** (6 nodes Ready, not 7).
Flag for the owner; irrelevant to this plan.

**Storage class:** `local-path` (`rancher.io/local-path`), default, `reclaimPolicy: Delete`,
`WaitForFirstConsumer`, **`ALLOWVOLUMEEXPANSION: false`**.
**(decision)** Every PVC must be sized correctly at creation — there is no growing it later.
Hence the generous headroom below; disk is the one resource this cluster has in abundance.

**Verdict: the cluster CAN host this.** No workload needs retiring. The Belt System preview
stack stays frozen and untouched.

### 1.2 Placement and sizing

The node socket is a UNIX socket, so **db-sync must run on the node that runs cardano-node**:
`minirack-pi5-1`. pi5-1 has 9.51 Gi *really* available and 7.4 Gi of unrequested memory — db-sync's
documented 3.5 GB RSS fits with room.

| Workload | Node | CPU req / limit | Mem req / limit | PVC | Image |
|---|---|---|---|---|---|
| `deployment-palmares-dbsync` | `minirack-pi5-1` (pinned, hostPath socket) | `750m` / `2` | `4Gi` / `6Gi` | **10Gi** (`/var/lib/cexplorer`, ledger state) | `ghcr.io/blinklabs-io/cardano-db-sync:13.7.1.0@sha256:2a341eb32f15a3e65435f4123c3186f467bf2ed9d3ea2eac449592b5a54eb950` |
| `deployment-palmares-postgres` | `minirack-pi5-1` (pinned, same node as db-sync) | `500m` / `2` | `1Gi` / `3Gi` | **50Gi** | `postgres:17-alpine` (verified `linux/arm64/v8` in the manifest index) |
| `deployment-palmares-projector` | NotIn pi5-4, rockpi-4a | `100m` / `1` | `256Mi` / `768Mi` | — | `ghcr.io/<owner>/palmares-projector` |
| `deployment-palmares-query-api` | NotIn pi5-4, rockpi-4a | `100m` / `500m` | `256Mi` / `512Mi` | — | `ghcr.io/<owner>/palmares-query-api` |
| `deployment-palmares-interaction-api` | NotIn pi5-4, rockpi-4a | `100m` / `500m` | `256Mi` / `512Mi` | — | `ghcr.io/<owner>/palmares-interaction-api` |
| `deployment-palmares-bff` | NotIn pi5-4, rockpi-4a | `50m` / `300m` | `128Mi` / `256Mi` | — | `ghcr.io/<owner>/palmares-bff` |
| `deployment-palmares-frontend` | NotIn pi5-4, rockpi-4a | `50m` / `200m` | `64Mi` / `128Mi` | — | `ghcr.io/<owner>/palmares-frontend` (nginx + static) |

New memory **requests** landing on pi5-1: **5 Gi** → pi5-1 goes from 45% to ~82% requested, with
~4.5 Gi of real headroom left. CPU on pi5-1 is 0.43 cores *actually* used out of 4 — no contention.

**PVC sizing rationale.** Official per-network figures ([db-sync README](https://github.com/IntersectMBO/cardano-db-sync),
"System Requirements", last updated May 2025, measured on `preprod_13.6.0.5`) for the **full**
preset on preprod: node DB 12 GB, ledger state ~2 GB, Postgres **16 GB**, db-sync RSS 3.5 GB.
The README's node figure is stale — measured today it is **21 GB**. The `only_governance` preset
drops `tx_in`/`tx_out`/`ma_tx_out`/`datum`/`script`/`redeemer`/`tx_metadata`/`reward`/`offchain_pool_data`,
which is the bulk of those 16 GB; `epoch_stake` (~320 preprod epochs — Koios preprod `/tip`
reports epoch 320 on 2026-10-19) is the largest survivor. **Estimate 6–10 GB.** 50Gi is 5×
headroom, and headroom is free here but un-addable later.
**This estimate is only valid with `preset: only_governance` actually set** — the hand-written
option set the first draft of this note proposed would have left `tx_metadata` and `reward`
enabled and landed much closer to the full 16 GB (finding 0.4).

### 1.3 The db-sync config file — exact content

`chains/cardano/preprod-dbsync/config/configmap-palmares-dbsync-config.yaml` carries this as
`db-sync-config.yaml`. Derived from the upstream
[`config/preprod-config.yaml`](https://github.com/IntersectMBO/cardano-db-sync/blob/13.7.1.0/config/preprod-config.yaml)
with two edits: `NodeConfigFile` repointed, and `insert_options` added.

```yaml
NetworkName: preprod
EnableLogMetrics: False
EnableLogging: True
# PrometheusPort defaults to 8080 — we scrape it; do not change it.

# The preprod node publishes its config dir on hostPath (see §2.2). Genesis files are
# referenced by BARE FILENAME inside config.json (verified: "ByronGenesisFile":
# "byron-genesis.json", etc.) so they must sit beside it — which they do.
NodeConfigFile: /node-configs/config.json

# ledger_backend is DELIBERATELY ABSENT. Default is "inmemory". "lsm" is documented as
# crashing on Raspberry Pi — see §0.8. Do not add it.

# ipfs_gateway is a TOP-LEVEL option (undocumented in configuration.md; see
# Config/Types.hs#L441: `o .:? "ipfs_gateway" .!= ["https://ipfs.io/ipfs"]`).
# CIP-119/CIP-108 anchors are frequently ipfs://; with an EMPTY list they fail
# `OCFErrNoIpfsGateway`. Pin two gateways so F1 check 7 is not hostage to one host.
ipfs_gateway:
  - "https://ipfs.io/ipfs"
  - "https://cloudflare-ipfs.com/ipfs"

insert_options:
  # USE THE PRESET. Verified in source (Config/Types.hs#L465-L495, tag 13.7.1.0): the preset
  # is only the BASE, and every explicit key below overrides it. configuration.md's prose
  # ("overrides all other properties") is misleading — the code disagrees with it.
  #
  # Do NOT hand-roll the equivalent option set. Without `preset` the base is `def`, which has
  # metadata ENABLED and rewards ENABLED, and `sioRewards` cannot be set from JSON at all
  # (`pure (sioRewards baseOptions)`), so `tx_metadata` and `reward` — the two biggest tables
  # — would silently come back. See finding 0.4.
  preset: "only_governance"
  offchain_vote_data: "enable"   # <-- THE ONLY DELTA. db-sync becomes our metadata fetcher.
  # ledger stays "enable" (from the preset): with "disable", drep_distr and
  # gov_action_proposal.{ratified,enacted,expired,dropped}_epoch are left empty/null.
  # remove_jsonb_from_schema is already false by default — do not restate it.

minSeverity: Info
setupBackends: [KatipBK]
defaultBackends: [KatipBK]
setupScribes:
  - scKind: StdoutSK
    scName: stdout
    scFormat: ScText
    scRotation: null
defaultScribes:
  - [StdoutSK, stdout]
options:
  mapSeverity:
    db-sync-node.Subscription: Error
    db-sync-node.Mux: Error
    db-sync-node: Info
```

**Tables this yields** (from `configuration.md`'s governance list, all populated because
`governance: enable` + `ledger: enable`): `committee`, `committee_member`,
`committee_registration`, `committee_de_registration`, `constitution`, `delegation_vote`,
`drep_distr`, `drep_registration`, `epoch_stake`, `gov_action_proposal`, `treasury_withdrawal`,
`voting_anchor`, `voting_procedure`, `off_chain_vote_data`, `off_chain_vote_drep_data`,
`off_chain_vote_gov_action_data`, `off_chain_vote_reference`, `off_chain_vote_external_update`,
`off_chain_vote_fetch_error` — plus `block`, `tx`, `drep_hash`, `pool_hash`, `stake_address`,
`epoch_param`, `ada_pots`, `pool_stat`.

**Known gap from `shelley.enable: false`:** it disables "all certificates, withdrawals and param
proposals" — so stake *delegation* (`delegation`), `stake_registration`, `pool_update`,
`pool_retire`, `withdrawal`, `param_proposal` are **empty**. Vote delegation (`delegation_vote`)
is a governance table and is unaffected. The upstream wording is exact: *"Enables or disables data
related to shelley: all certificates, withdrawals, and param proposals. … This governs
transaction-era certificates only; genesis-era stake and pool registrations are always inserted
regardless of this setting."*
**(decision)** Accept the gap for F1 — with **one named consequence**, corrected 2026-10-19:
`stake_registration` / `stake_deregistration` are certificates, so they are gone too, and that
makes the per-DRep **delegator count / churn / retention** metrics of spec §4.2 and F1 check 12
an upper bound rather than exact (§1.4.3). Document the bound in the published formula. If an SPO
page later needs pool ticker/metadata, or exact delegator counts are wanted, flip
`shelley.enable: true` (+ `offchain_pool_data: "enable"` for pool metadata) and re-sync — record
it as a known re-sync trigger, like the Belt System's F-09.

### 1.4 What NOT to build (laziness callouts)

1. **No metadata fetcher — but one small classifier.** §0.5. db-sync does the fetch, the
   blake2b verification, the storage and the retry loop. What it does **not** do is hand us the
   spec's four verdicts, and the original version of this note got that wrong. Verified against
   [`doc/schema.md` @13.7.1.0](https://github.com/IntersectMBO/cardano-db-sync/blob/13.7.1.0/doc/schema.md):
   - `off_chain_vote_data.is_valid` is **CIP-100 schema conformance, not hash validity**:
     `TRUE` = valid JSON conforming to CIP-100; `FALSE` = valid JSON that does not conform;
     `NULL` = not valid JSON at all. The table's own note: *"Only data with hash matches are
     stored here; hash mismatches are stored in `off_chain_vote_fetch_error` for retry."*
   - `off_chain_vote_fetch_error` columns are exactly `id`, `voting_anchor_id`, `fetch_error`
     (free-text string), `fetch_time`, `retry_count`. **There is no category column.**
   So `hash_mismatch | unreachable | oversized` must be derived by matching `fetch_error`
   against db-sync's `OffChainFetchError` constructors
   (`Cardano/DbSync/OffChain/Types.hs`). **(decision)** Build a ~30-line pure classifier
   `Palmares.Anchor.classifyFetchError :: Text -> AnchorVerdict`, with a golden test over the
   constructor list, and treat it as a **db-sync-version-pinned** function: it is the one place
   a db-sync upgrade can silently degrade F1 check 7. Presence of a row in
   `off_chain_vote_data` = hash matched; `is_valid` then only refines *conformance*, which the
   API should expose as a separate field, not folded into the anchor verdict.
   (Note: `--allow-private-offchain-urls` exists but defaults off — leave it off.)
   (Note: 13.7.1.0 and 13.7.2.1 both carry the open known issue *"duplicate off-chain vote
   metadata child rows"* ([PR #2137](https://github.com/IntersectMBO/cardano-db-sync/pull/2137)),
   so every read of `off_chain_vote_author` / `off_chain_vote_reference` /
   `off_chain_vote_external_update` must `DISTINCT`/dedupe.)
2. **No new preprod node.** §0.1.
3. **No projection for anything db-sync already answers.** `gov_action_proposal` already carries
   `expiration`, `ratified_epoch`, `enacted_epoch`, `expired_epoch`, `dropped_epoch` (verified in
   `schema.md` @13.7.1.0). Project only the **derived metrics** (participation rate, rationale
   rate, vote latency, delegator retention) and the **composite page rows**. Everything else is a
   view or a direct query.
   **CORRECTED 2026-10-19:** `drep_distr` is **not** the spec's `drep_epoch_power`. Its columns
   are `id`, `hash_id`, `amount`, `epoch_no`, `active_until` — there is **no delegator count**,
   and the schema says so explicitly: *"Currently this has a single entry per DRep and doesn't
   show every delegator."* Spec §6 defines `drep_epoch_power` as *(drep, epoch, lovelace,
   **nr. delegatori**)*, and spec §4.2 / F1 check 12 require delegator count + churn + retention.
   Those must be derived from `delegation_vote` — and that collides with the §1.3 gap:
   with `shelley.enable: false` the **stake de-registration certificates are not inserted**
   ("all certificates"), so a delegator count built from `delegation_vote` alone **over-counts**
   (a stake key that de-registered still looks delegated).
   **(decision, needs the owner's eye)** For F1, publish delegator count/churn as
   *delegation-certificate-derived and documented as an upper bound*, and put the exact caveat in
   the published formula (spec S2 demands the formula, so an honest caveat satisfies it). The
   alternative — `shelley.enable: true` — is a **full re-sync** and pulls in the whole
   certificate/withdrawal/param-proposal volume. Do not make that call silently at
   implementation time.
4. **No epoch-boundary snapshot scheme, no custom Ogmios follower.** Already decided in the spec;
   restated because it is the single biggest avoided cost.
5. **No `postgres_fdw`, no second Postgres instance.** §0.6.
6. **No Mithril job for F1.** §0.1 / §3.

### 1.5 Sync time — node vs db-sync (they differ, and only one is on the critical path)

| Step | Duration | Basis |
|---|---|---|
| **Preprod node** (existing) | **0 h** | Already at tip. Measured 21 GB `/data`, 18990 immutable chunks. |
| Preprod node **rebuilt from Mithril + ancillary** | ~20–40 min download + either ~30 s catch-up (matching `LedgerDB` backend) or **~4–5 h** full replay (`MetadataBackendMismatch`) | The rack's own measurements: preview replayed **3.13 h from genesis on a 15 GB chain**; preprod, converted instead, caught up in **~26 s** (`apps-k8s-configs/CLAUDE.md`). Preprod is 21 GB, so scale 3.13 h → ~4.4 h. |
| Preprod node **from genesis, no Mithril** | ~6–10 h | Same measurement, plus block download. |
| **db-sync from genesis, preprod, governance options, `inmemory` ledger, on a Pi 5** | **24–48 h (estimate; floor ~8 h)** — **must be measured, this is an F1 deliverable** | **(BLOCKER-adjacent: no published figure exists.)** Derivation: [Cardano developer docs](https://developers.cardano.org/docs/developers/curriculum/production/infrastructure/) give 2–3 days for *mainnet full* on x86. Preprod's chain is ~10× smaller (21 GB vs 203 GB); a Pi 5 is ~3–5× slower on this single-core-bound work; the governance options remove most of the INSERT volume but **none** of the ledger replay. Band: 0.5–2 days. |
| db-sync **from our published snapshot** (§4.3) | ~20 min | `RESTORE_SNAPSHOT` in the blinklabs entrypoint: downloads, verifies `.sha256sum`, runs `postgresql-setup.sh --restore-snapshot`. |

**Does Mithril help db-sync?** **No.** Mithril restores the *node's* immutable chain DB. db-sync
then still streams every block over the local chain-sync protocol from genesis and maintains its
**own** ledger state. db-sync's only shortcut is a Postgres dump — and none is published for
preprod (§0.3). **Say this plainly in the repo README**; it is the single most likely thing for a
contributor to get wrong.

### 1.6 ARM caveats, in one place

- **db-sync LSM backend: forbidden here.** §0.8, verbatim upstream wording. Classic
  (`inmemory`) only.
- **arm64 images exist and are verified.** `ghcr.io/blinklabs-io/cardano-db-sync:13.7.1.0` is a
  real multi-arch index: `[(linux, amd64), (linux, arm64)]`, index digest
  `sha256:2a341eb32f15a3e65435f4123c3186f467bf2ed9d3ea2eac449592b5a54eb950`.
  **Not a blocker.** IntersectMBO's own `13.7.2.1` / `13.7.3.0` are single-manifest amd64.
- **Version-pin consequence.** 13.7.1.0 predates the 13.7.2.1 `epoch`-table redesign and its
  rollup-corruption fix, and predates the 13.7.2.1 off-chain DRep `data:` image-URI fix. We do
  **not** read the `epoch` table (we read `epoch_stake`, `drep_distr`, `epoch_param`), so the
  first is immaterial. The second is cosmetic (DRep avatar URLs). **(decision)** Ship 13.7.1.0.
  If a governance-relevant bug bites, build our own arm64 13.7.2.1 — the team already publishes
  custom arm64 images (`mariusgeorgescu/hydra-node:2.2.0-arm64`), so the escape hatch is proven.
  Re-check `ghcr.io/blinklabs-io/cardano-db-sync` tags before F1 starts; upstream aarch64 is in
  "Unreleased" and may land first.
- **Node's own `V2LSM` is unrelated and stays.** Cluster-wide decision D7.

### 1.7 Mainnet (F2) — restated, unchanged

mainnet db-sync: node DB **203 GB**, ledger state ~10 GB, Postgres **438 GB**, RSS **21 GB**
(official README figures). Does not fit any minirack node (max 13.6 Gi allocatable, 824 GB free).
F2 needs the dedicated x86_64 box the spec already budgets. Governance options will cut the 438 GB
substantially, but 21 GB RSS is a ledger-replay cost the options do not touch.

---

## 2. Manifests

Four Flux roots. Cluster runs **k3s v1.36.3+k3s1**, **Flux v2.9.3** — `kustomize.toolkit.fluxcd.io/v1`,
`gateway.networking.k8s.io/v1`, `cert-manager.io/v1`, `monitoring.coreos.com/v1` are all live and
in use in this repo today; every manifest below is shaped after an existing, reconciling file, which
is a stronger schema guarantee than a catalog lookup.

```
chains/cardano/preprod/workloads/statefulset-cardano-preprod.yaml   ← 2-line edit (§2.2)
chains/cardano/preprod-dbsync/                                      ← NEW root
  namespace.yaml                        (none — reuses ns cardano-preprod)
  config/configmap-palmares-dbsync-config.yaml
  config/configmap-palmares-dbsync-env.yaml
  secrets/secret-palmares-dbsync.yaml   (SOPS)
  storage/pvc-palmares-dbsync-state.yaml
  storage/pvc-palmares-dbsync-pg.yaml
  services/service-palmares-postgres.yaml
  services/service-palmares-dbsync-metrics.yaml
  workloads/deployment-palmares-postgres.yaml
  workloads/deployment-palmares-dbsync.yaml
  policies/networkpolicy-palmares-postgres.yaml
  monitoring/servicemonitor-palmares-dbsync.yaml
  kustomization.yaml
apps/palmares/base/{namespace,config,services,workloads,policies}/…
apps/palmares/preprod/{kustomization.yaml,networking/…,patches/…,secrets/…}
clusters/minirack/palmares/{kustomization.yaml,palmares-dbsync.yaml,palmares-preprod.yaml}
```

**(decision) Why db-sync gets its own Flux root, not a place in `chains/cardano/preprod`:**
that root carries `wait: true, timeout: 10m` and health-checks everything it reconciles. A
component whose first sync takes ~48 h, and whose config will churn during F1, must be
independently suspendable without touching a live node that Kupo-preprod and hydra-pay depend on.

**(decision) Why db-sync is a separate pod, not a sidecar in the node StatefulSet:** a sidecar is
a smaller diff, but pod readiness is the AND of all containers. A db-sync `CrashLoopBackOff` would
make the pod NotReady, withdraw the **Ogmios** endpoint from its Service, and break Kupo-preprod +
hydra-pay. Also every db-sync config edit would restart `hydra-node`. The one-time cost of the
separate pod is a ~26 s node restart to switch the volumes to hostPath.

### 2.1 Flux wiring — `clusters/minirack/palmares/palmares-dbsync.yaml`

```yaml
# Flux Kustomization "palmares-dbsync": cardano-db-sync (governance options) + its Postgres 17,
# in namespace cardano-preprod, pinned to the node that runs the preprod cardano-node.
# Separate root from `cardano-preprod` on purpose: the first from-genesis sync takes ~48h and this
# must be suspendable without reconciling a live node that Kupo and hydra-pay read.
apiVersion: kustomize.toolkit.fluxcd.io/v1
kind: Kustomization
metadata:
  name: palmares-dbsync
  namespace: flux-system
spec:
  interval: 10m0s
  retryInterval: 2m0s
  path: ./chains/cardano/preprod-dbsync
  prune: true
  sourceRef:
    kind: GitRepository
    name: flux-system
  decryption:
    provider: sops
    secretRef:
      name: sops-age
  # wait with a SHORT timeout: readiness is reported honestly but a Flux worker is never held
  # for the duration of a sync (the 2026-09-23 audit lesson — 4 default workers, all blocked).
  wait: true
  timeout: 1m0s
  dependsOn:
    - name: infrastructure
    - name: cardano-preprod
```

```yaml
# clusters/minirack/palmares/palmares-preprod.yaml
apiVersion: kustomize.toolkit.fluxcd.io/v1
kind: Kustomization
metadata:
  name: palmares-preprod
  namespace: flux-system
spec:
  interval: 10m0s
  retryInterval: 2m0s
  path: ./apps/palmares/preprod
  prune: true
  sourceRef:
    kind: GitRepository
    name: flux-system
  decryption:
    provider: sops
    secretRef:
      name: sops-age
  wait: true
  timeout: 1m0s
  dependsOn:
    - name: infrastructure
    - name: cert-manager-configs
    - name: palmares-dbsync
```

Add both to `clusters/minirack/palmares/kustomization.yaml` and that directory to
`clusters/minirack/kustomization.yaml`. (Verified: that is exactly the repo's shape — one
directory per app under `clusters/minirack/`, each with a `kustomization.yaml` listing its Flux
Kustomization files, and the three `dependsOn` names used above — `infrastructure`,
`cert-manager-configs`, `cardano-preprod` — are the real `metadata.name`s.)

⚠️ **Two consequences of `wait: true` + `dependsOn` worth knowing before F1 starts.**
(1) `palmares-preprod` depending on `palmares-dbsync` means a db-sync readiness blip blocks the
*app* root, even though the app pods only need Postgres, not a caught-up db-sync. If that bites,
drop the `dependsOn` and let `/ready`'s lag gate do the job it was designed for.
(2) The readinessProbe must therefore pass **during** the sync, not after it — which is exactly
why the `path: /` correction in §2.3 is a blocker and not a nit.

### 2.2 The only edit to an existing live file

`chains/cardano/preprod/workloads/statefulset-cardano-preprod.yaml` — publish the node's IPC
socket and config dir on hostPath, exactly as the mainnet BP already does
(`/var/lib/cardano-mainnet/{ipc,configs}`, per `apps-k8s-configs/CLAUDE.md`).

```yaml
    spec:
      # hostPath volumes are node-local. The data PVC already pins this pod to pi5-1
      # (local-path + RWO), but make it explicit so the socket path can never be orphaned.
      nodeSelector:
        kubernetes.io/hostname: minirack-pi5-1
      …
      volumes:
        - name: configs
          hostPath:                                  # was: emptyDir: {}
            path: /var/lib/cardano-preprod/configs
            type: DirectoryOrCreate
        - name: ipc
          hostPath:                                  # was: emptyDir: {}
            path: /var/lib/cardano-preprod/ipc
            type: DirectoryOrCreate
```

Also bump `cardano.io/node-config-stamp` in the same commit (repo rule). Cost: one pod restart.
(The "~26 s" figure in `apps-k8s-configs/CLAUDE.md` is the catch-up **after a V2LSM snapshot
conversion**, not a measurement of a plain pod restart — expect seconds-to-minutes, and budget a
`V2LSM` snapshot reload, not 26 s exactly.) Note also that the StatefulSet's own in-file comment
still says the preprod node "rămâne V2InMemory"; that comment is **stale** — the committed
`configmap-cardano-preprod-node-config.yaml` has `"LedgerDB": {"Backend": "V2LSM", …}` and
`CLAUDE.md` records all three networks on `V2LSM` (decision D7). This note's V2LSM premise is
correct; the StatefulSet comment is the thing that is wrong, and fixing it is not our commit.
Both volumes are published because db-sync needs **both** the socket
*and* `config.json` + the four genesis files — and the existing `fetch-configs` initContainer
already assembles exactly that directory, so nothing is duplicated.

### 2.3 db-sync Deployment

The blinklabs entrypoint (`bin/entry-point`) has a non-obvious contract, read from source:

- It runs `set -euo pipefail` and then tests `if [[ -z ${NETWORK} ]]`. With **`NETWORK` unset
  this is an unbound-variable error**. It must be **present and empty** — `value: ""`.
- With `NETWORK` empty it takes the "Connecting to network specified in configuration.yaml" branch
  and execs `cardano-db-sync --schema-dir /opt/cardano/schema --state-dir /var/lib/cexplorer "$@"`,
  so container `args` are appended verbatim. We supply `--config` and `--socket-path`.
- `gen-pgpass` writes `/configuration/pgpass` from `POSTGRES_{HOST,PORT,DB,USER,PASSWORD}` env if
  set, else from files under `/run/secrets`. Setting all five env vars avoids `/run/secrets`
  entirely. `/configuration` must be **writable** → `emptyDir`.
- `RESTORE_SNAPSHOT` accepts an `https://` URL, fetches `<url>.sha256sum`, verifies it, and runs
  `postgresql-setup.sh --restore-snapshot "$snap" /var/lib/cexplorer`. Guarded by a
  `<basename>.restored` marker. This is the F2/contributor accelerator (§4.3) — leave it unset for
  the first preprod sync. **Three gotchas, read from source 2026-10-19 and not obvious:**
  (a) the download and the marker go to the **CWD**, and the image sets no `WORKDIR` — hence the
  `workingDir: /var/lib/cexplorer` added below, or every restart re-downloads the whole dump;
  (b) the checksum is fetched from the **same host** as the dump, so it detects corruption, not
  tampering — a `.asc` is deleted but never verified; if we publish the snapshot (§4.3) the
  trust statement in the README must say exactly that;
  (c) `restore_snapshot` **refuses a non-empty ledger-state directory**, and the entrypoint only
  clears `*.lstate`, not the newer `[0-9]*` consensus-snapshot directories — so a restore onto a
  volume that has already synced fails. Restore onto a fresh PVC only.
  `postgresql-setup.sh` also runs `check_psql_superuser`, which our `postgres` role satisfies, and
  honours `RESTORE_RECREATE_DB` with a **default of `Y`** (drop + create the database).

```yaml
# chains/cardano/preprod-dbsync/workloads/deployment-palmares-dbsync.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: deployment-palmares-dbsync
  namespace: cardano-preprod
spec:
  replicas: 1
  strategy:
    type: Recreate          # single writer on one PVC and one Postgres; never two.
  selector:
    matchLabels:
      app: palmares-dbsync-pod-label
  template:
    metadata:
      labels:
        app: palmares-dbsync-pod-label
      annotations:
        # configmap-palmares-dbsync-config does not restart this pod on its own.
        # Bump this in the SAME commit as any edit to it.
        palmares.io/dbsync-config-stamp: "2026-10-19-1"
    spec:
      # Pinned: reads the preprod node's UNIX socket from hostPath. Must be its node.
      nodeSelector:
        kubernetes.io/hostname: minirack-pi5-1
      terminationGracePeriodSeconds: 120   # db-sync needs a graceful stop to close Postgres cleanly
      containers:
        - name: container-palmares-dbsync
          # workingDir on the PVC. CORRECTED 2026-10-19: the blinklabs entrypoint does
          # `curl -LOC - "$RESTORE_SNAPSHOT"` and `touch "${__base}.restored"` in the CWD, and
          # the image sets NO WORKDIR, so with the default CWD=/ the multi-GB dump and its
          # idempotency marker land on the container's writable layer — lost on every restart
          # (full re-download) and charged against ephemeral storage, not the PVC.
          workingDir: /var/lib/cexplorer
          image: ghcr.io/blinklabs-io/cardano-db-sync:13.7.1.0@sha256:2a341eb32f15a3e65435f4123c3186f467bf2ed9d3ea2eac449592b5a54eb950
          imagePullPolicy: IfNotPresent
          args:
            - --config
            - /dbsync-config/db-sync-config.yaml
            - --socket-path
            - /node-ipc/node.socket
          env:
            # MUST be present and empty: the entrypoint runs `set -u` and reads ${NETWORK}
            # with no default. Empty selects the "use my --config" branch.
            - name: NETWORK
              value: ""
          envFrom:
            - configMapRef:
                name: configmap-palmares-dbsync-env     # POSTGRES_HOST/PORT/DB/USER
            - secretRef:
                name: secret-palmares-dbsync            # POSTGRES_PASSWORD (SOPS)
          ports:
            - name: metrics
              containerPort: 8080
          volumeMounts:
            - name: dbsync-state
              mountPath: /var/lib/cexplorer
            - name: node-ipc
              mountPath: /node-ipc
            - name: node-configs
              mountPath: /node-configs
              readOnly: true
            - name: dbsync-config
              mountPath: /dbsync-config
              readOnly: true
            - name: configuration
              mountPath: /configuration      # entrypoint writes pgpass here
          # NO startup/liveness probe: the first sync runs for ~48h and nothing may kill it.
          # Readiness only, so the Service/metrics endpoint reflects reality.
          #
          # path: "/" — NOT "/metrics". CORRECTED 2026-10-19 (fact-check). db-sync calls
          # `serveMetricsT port []` (Cardano/DbSync/Metrics.hs#L73 @13.7.1.0), and the
          # prometheus library's `prometheusApp` matches `pathInfo request == path`, so with
          # `[]` only "/" answers 200 and "/metrics" returns a hard 404
          # (bitnomial/prometheus, System/Metrics/Prometheus/Http/Scrape.hs — its own haddock:
          # "http://localhost/ should use a path of []").
          # Getting this wrong is a cascade, not a cosmetic bug: the pod would never be Ready,
          # so the `palmares-dbsync` Flux root (wait: true) would stay Ready=False and
          # `palmares-preprod` (dependsOn it) would never reconcile at all.
          #
          # initialDelaySeconds 10, not 60: the Flux root's timeout is 1m0s (§2.1), and the
          # metrics server is up within seconds — it does not wait for the sync.
          readinessProbe:
            httpGet:
              path: /
              port: metrics
            initialDelaySeconds: 10
            periodSeconds: 30
            timeoutSeconds: 10
            failureThreshold: 6
          resources:
            requests:
              cpu: "750m"
              memory: "4Gi"
            limits:
              cpu: "2"
              memory: "6Gi"
      volumes:
        - name: dbsync-state
          persistentVolumeClaim:
            claimName: pvc-palmares-dbsync-state
        - name: node-ipc
          hostPath:
            path: /var/lib/cardano-preprod/ipc
            type: Directory
        - name: node-configs
          hostPath:
            path: /var/lib/cardano-preprod/configs
            type: Directory
        - name: dbsync-config
          configMap:
            name: configmap-palmares-dbsync-config
        - name: configuration
          emptyDir: {}
```

```yaml
# chains/cardano/preprod-dbsync/config/configmap-palmares-dbsync-env.yaml
apiVersion: v1
kind: ConfigMap
metadata:
  name: configmap-palmares-dbsync-env
  namespace: cardano-preprod
data:
  # db-sync's own DB. Our projections live in schema `registry` of this SAME database,
  # because PostgreSQL cannot join across databases without postgres_fdw.
  POSTGRES_HOST: "service-palmares-postgres.cardano-preprod.svc.cluster.local"
  POSTGRES_PORT: "5432"
  POSTGRES_DB: "cexplorer"
  POSTGRES_USER: "postgres"
  PGDATA: "/var/lib/postgresql/data/pgdata"
```

### 2.4 Postgres 17

```yaml
# chains/cardano/preprod-dbsync/workloads/deployment-palmares-postgres.yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: deployment-palmares-postgres
  namespace: cardano-preprod
spec:
  replicas: 1
  strategy:
    type: Recreate
  selector:
    matchLabels:
      app: palmares-postgres-pod-label
  template:
    metadata:
      labels:
        app: palmares-postgres-pod-label
    spec:
      nodeSelector:
        kubernetes.io/hostname: minirack-pi5-1     # same node as db-sync: hottest link in the stack
      containers:
        - name: container-palmares-postgres
          image: postgres:17-alpine
          imagePullPolicy: IfNotPresent
          args:
            # Tuned for one bulk writer on a 4-core Pi with a local NVMe.
            - -c
            - shared_buffers=768MB
            - -c
            - work_mem=32MB
            - -c
            - maintenance_work_mem=512MB
            - -c
            - max_parallel_maintenance_workers=2
            - -c
            - max_wal_size=4GB
            - -c
            - checkpoint_timeout=30min
            - -c
            - wal_compression=on
            # ponytail: synchronous_commit=off trades the last ~200ms of commits for throughput.
            # Safe here and only here: on crash db-sync rolls back to its last stored block and
            # re-reads from the node. Never copy this to a ledger of record.
            - -c
            - synchronous_commit=off
          ports:
            - name: postgres
              containerPort: 5432
          envFrom:
            - configMapRef:
                name: configmap-palmares-dbsync-env
            - secretRef:
                name: secret-palmares-dbsync
          readinessProbe:
            exec:
              command: ["pg_isready", "-U", "postgres", "-d", "cexplorer"]
            initialDelaySeconds: 10
            periodSeconds: 5
            timeoutSeconds: 3
            failureThreshold: 5
          livenessProbe:
            exec:
              command: ["pg_isready", "-U", "postgres", "-d", "cexplorer"]
            initialDelaySeconds: 30
            periodSeconds: 10
            timeoutSeconds: 5
            failureThreshold: 3
          resources:
            requests:
              cpu: "500m"
              memory: "1Gi"
            limits:
              cpu: "2"
              memory: "3Gi"
          volumeMounts:
            - name: pgdata
              mountPath: /var/lib/postgresql/data
      volumes:
        - name: pgdata
          persistentVolumeClaim:
            claimName: pvc-palmares-dbsync-pg
```

```yaml
# chains/cardano/preprod-dbsync/storage/pvc-palmares-dbsync-pg.yaml
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: pvc-palmares-dbsync-pg
  namespace: cardano-preprod
  annotations:
    # local-path reclaim = Delete and ALLOWVOLUMEEXPANSION = false. Deleting this file from Git
    # would destroy a ~48h sync, and it can never be grown. 50Gi for a ~6-10Gi database.
    kustomize.toolkit.fluxcd.io/prune: disabled
spec:
  accessModes: ["ReadWriteOnce"]
  storageClassName: local-path
  resources:
    requests:
      storage: 50Gi
---
# storage/pvc-palmares-dbsync-state.yaml — db-sync ledger snapshots (~2GB official, preprod)
apiVersion: v1
kind: PersistentVolumeClaim
metadata:
  name: pvc-palmares-dbsync-state
  namespace: cardano-preprod
  annotations:
    kustomize.toolkit.fluxcd.io/prune: disabled
spec:
  accessModes: ["ReadWriteOnce"]
  storageClassName: local-path
  resources:
    requests:
      storage: 10Gi
```

```yaml
# chains/cardano/preprod-dbsync/services/service-palmares-postgres.yaml
apiVersion: v1
kind: Service
metadata:
  name: service-palmares-postgres
  namespace: cardano-preprod
spec:
  type: ClusterIP
  selector:
    app: palmares-postgres-pod-label
  ports:
    - name: postgres
      port: 5432
      targetPort: postgres
---
# services/service-palmares-dbsync-metrics.yaml
apiVersion: v1
kind: Service
metadata:
  name: service-palmares-dbsync-metrics
  namespace: cardano-preprod
spec:
  type: ClusterIP
  selector:
    app: palmares-dbsync-pod-label
  ports:
    - name: metrics
      port: 8080
      targetPort: metrics
```

### 2.5 NetworkPolicy — the one that matters

The database holds nothing secret, but it is the *only* writer path into the registry and it is
reachable from every pod in the cluster by default (the exact finding of the 2026-10-18 bjj audit).

```yaml
# chains/cardano/preprod-dbsync/policies/networkpolicy-palmares-postgres.yaml
apiVersion: networking.k8s.io/v1
kind: NetworkPolicy
metadata:
  name: networkpolicy-palmares-postgres
  namespace: cardano-preprod
spec:
  podSelector:
    matchLabels:
      app: palmares-postgres-pod-label
  policyTypes: [Ingress]
  ingress:
    - from:
        - podSelector: {}                       # db-sync, same namespace
        - namespaceSelector:                    # our projector + query API
            matchLabels:
              kubernetes.io/metadata.name: palmares
      ports:
        - protocol: TCP
          port: 5432
```

### 2.6 Monitoring

```yaml
# chains/cardano/preprod-dbsync/monitoring/servicemonitor-palmares-dbsync.yaml
# kube-prometheus-stack runs serviceMonitorSelector: {} on this cluster, so no extra label
# is needed — it is picked up automatically (same as podmonitor-cardano-preprod).
apiVersion: monitoring.coreos.com/v1
kind: ServiceMonitor
metadata:
  name: servicemonitor-palmares-dbsync
  namespace: cardano-preprod
spec:
  selector:
    matchLabels:
      app: palmares-dbsync-pod-label
  endpoints:
    - port: metrics
      # path: "/" — see the readinessProbe comment in §2.3. "/metrics" is a 404 on db-sync.
      path: /
      interval: 30s
  namespaceSelector:
    matchNames: [cardano-preprod]
```

**(decision)** The single alert worth writing for F1: db-sync's reported height falling behind the
node's. **CORRECTED 2026-10-19:** db-sync exports exactly six gauges, read from
[`Metrics.hs` `makeMetrics` @13.7.1.0](https://github.com/IntersectMBO/cardano-db-sync/blob/13.7.1.0/cardano-db-sync/src/Cardano/DbSync/Metrics.hs):
`cardano_db_sync_node_block_height`, `cardano_db_sync_db_queue_length`,
`cardano_db_sync_db_block_height`, `cardano_db_sync_db_slot_height`,
`cardano_db_sync_db_epoch_sync_duration_seconds`, `cardano_db_sync_db_epoch_sync_number`.
There is **no `cardano_db_sync_node_slot_height`** — an alert on it would produce an empty series
and never fire. The lag expression is
`cardano_db_sync_node_block_height - cardano_db_sync_db_block_height`
(both served on `:8080/`). Put it in
`monitoring/prometheusrule-palmares-dbsync.yaml` **after** the first sync completes — during the
48 h sync it would fire continuously and teach everyone to ignore it.

### 2.7 Our services — probes and the lag gate

The extracted `webapi-lib/WebAPI/ServiceProbe.hs` already serves **both** `"health"` and
`"ready"`. Use them as the spec intends and as the Belt System does not:

- `livenessProbe` → `/health` (process alive; never gated on a dependency)
- `readinessProbe` + `startupProbe` → `/ready` (gated on Postgres reachable **and**
  db-sync slot lag ≤ N; spec §7 "indexer may lag at most N slots before `/ready` fails")
- `startupProbe` `failureThreshold: 30, periodSeconds: 10` — the Belt System's shape, which
  survives a slow cold start without a liveness kill.

```yaml
# apps/palmares/base/workloads/deployment-palmares-query-api.yaml (abridged — full shape mirrors
# apps/bjj-belts/base/workloads/deployment-bjjbelts-query-api.yaml, including the dnsConfig block)
          ports:
            - name: http
              containerPort: 8083
          readinessProbe:
            httpGet: { path: /ready, port: http }
            initialDelaySeconds: 10
            periodSeconds: 5
            timeoutSeconds: 3
            failureThreshold: 5
          livenessProbe:
            httpGet: { path: /health, port: http }
            initialDelaySeconds: 30
            periodSeconds: 10
            timeoutSeconds: 5
            failureThreshold: 3
          startupProbe:
            httpGet: { path: /ready, port: http }
            periodSeconds: 10
            timeoutSeconds: 5
            failureThreshold: 30
```

Placement patch, applied to every stateless Deployment in the overlay (copy of
`apps/bjj-belts/dev/patches/deployment-bjjbelts-stateless-placement.yaml`, extended for the 4 GB box):

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: not-used
spec:
  template:
    spec:
      affinity:
        nodeAffinity:
          requiredDuringSchedulingIgnoredDuringExecution:
            nodeSelectorTerms:
              - matchExpressions:
                  - key: kubernetes.io/hostname
                    operator: NotIn
                    values:
                      - minirack-pi5-4     # mainnet block producer, 92% memory committed
                      - rockpi-4a          # 3.26Gi allocatable
```

### 2.8 Gateway API route for the public explorer

**(decision)** Hostname **`gov.cardano.vip`**. `*.cardano.vip` already resolves publicly
(CNAME → `cardano-vip.go.ro`) and in-cluster via `k8s_gateway`, so **no DNS and no CoreDNS
`hosts{}` entry is needed** — `apps-k8s-configs/CLAUDE.md` calls that old rule obsolete as of
2026-10-19.

**(decision)** Issuer **`letsencrypt-http01-prod`** (ClusterIssuer, `READY=True`, 289 d old),
per the current documented rule: "A new public host needs a `Certificate` on
`letsencrypt-http01-prod` — and **nothing else**". Fallback if HTTP-01 misbehaves on this zone:
`letsencrypt-godaddy-prod` (DNS-01), which is what `bjj.cardano.vip` uses today and is also
`READY=True`. GatewayClass **`traefik`** (external, `.240`); `traefik-internal` is the LAN one.

```yaml
# apps/palmares/preprod/networking/certificates/certificate-palmares-cardano-vip.yaml
apiVersion: cert-manager.io/v1
kind: Certificate
metadata:
  name: certificate-palmares-cardano-vip
  namespace: palmares
spec:
  secretName: secret-palmares-cardano-vip-tls
  dnsNames:
    - gov.cardano.vip
  issuerRef:
    name: letsencrypt-http01-prod
    kind: ClusterIssuer
    group: cert-manager.io
---
# networking/gateways/gateway-palmares.yaml
apiVersion: gateway.networking.k8s.io/v1
kind: Gateway
metadata:
  name: gateway-palmares
  namespace: palmares
spec:
  gatewayClassName: traefik
  listeners:
    - name: http
      hostname: gov.cardano.vip
      port: 80
      protocol: HTTP
      allowedRoutes:
        namespaces: { from: All }
    - name: https
      hostname: gov.cardano.vip
      port: 443
      protocol: HTTPS
      tls:
        mode: Terminate
        certificateRefs:
          - kind: Secret
            name: secret-palmares-cardano-vip-tls
      allowedRoutes:
        namespaces: { from: All }
---
# networking/routes/httproute-palmares-redirect.yaml
apiVersion: gateway.networking.k8s.io/v1
kind: HTTPRoute
metadata:
  name: httproute-palmares-redirect
  namespace: palmares
spec:
  parentRefs:
    - name: preprod-gateway-palmares     # namePrefix: preprod- from the overlay
      sectionName: http
  hostnames: [gov.cardano.vip]
  rules:
    - filters:
        - type: RequestRedirect
          requestRedirect: { scheme: https, statusCode: 301 }
---
# networking/routes/httproute-palmares.yaml
# The BFF owns /api and rate-limits it (spec §4.2: public API, no Basic auth). Everything else
# is the static explorer. Most specific match wins in Gateway API, so order is irrelevant.
apiVersion: gateway.networking.k8s.io/v1
kind: HTTPRoute
metadata:
  name: httproute-palmares
  namespace: palmares
spec:
  parentRefs:
    - name: preprod-gateway-palmares
      sectionName: https
  hostnames: [gov.cardano.vip]
  rules:
    - matches:
        - path: { type: PathPrefix, value: /api }
      backendRefs:
        - name: preprod-service-palmares-bff
          port: 8080
    - matches:
        - path: { type: PathPrefix, value: / }
      backendRefs:
        - name: preprod-service-palmares-frontend
          port: 80
```

**Note the `namePrefix` trap**, inherited from `apps/bjj-belts/dev`: the overlay sets
`namePrefix: preprod-`, so `parentRefs.name` and `backendRefs.name` must carry the prefix
(kustomize rewrites Deployment/Service *names* but not these cross-references inside Gateway API
CRs). This is exactly how `httproute-bjjbelts-dev.yaml` is written — copy it, don't re-derive it.

### 2.9 SOPS

`.sops.yaml`'s second rule already matches `^chains/.*/secrets/.*\.ya?ml$` and
`^apps/.*/secrets/.*\.ya?ml$` with age key
`age13hn00phdudrnrvgqd0e9x8ndkk95jtnrqfp8n0g5d86cnhrj99csyc08c6`, `encrypted_regex: '^(data|stringData)$'`.
Both new secret paths fall under it — **no `.sops.yaml` change needed.**

```bash
# Create the one secret F1's read path needs.
cat > chains/cardano/preprod-dbsync/secrets/secret-palmares-dbsync.yaml <<'EOF'
apiVersion: v1
kind: Secret
metadata:
  name: secret-palmares-dbsync
  namespace: cardano-preprod
type: Opaque
stringData:
  POSTGRES_PASSWORD: "<generate: openssl rand -base64 24>"
EOF
sops -e -i chains/cardano/preprod-dbsync/secrets/secret-palmares-dbsync.yaml
```

**The read path needs no provider token.** Spec §7's "self-hostable" promise holds literally:
node + db-sync + Postgres + our projector + query API + UI require **zero** third-party
credentials. Only the write path (tx building, F1 §B check 11) needs a Maestro/Blockfrost key.

### 2.10 Validation before pushing

```bash
cd /Users/mg/Projects/apps-k8s-configs
git config core.hooksPath .githooks          # once per clone; git never propagates hooks
for p in ./chains/cardano/preprod ./chains/cardano/preprod-dbsync ./apps/palmares/preprod; do
  kustomize build --load-restrictor LoadRestrictionsNone "$p" \
    | kubeconform -ignore-missing-schemas -summary -
done
grep -rlE '^kind: Secret' chains/cardano/preprod-dbsync apps/palmares \
  | xargs -r grep -L 'ENC\[AES256_GCM'       # must print nothing
```

---

## 3. Mithril bootstrap

**(decision) Not needed for F1.** §0.1: the preprod node is already synced. This section exists so
a rebuild and the local dev stack (§4) have a procedure, and so the owner can decommission the
redundant second preprod node in `hydra-m5` if they want the 100 GB back.

**The pattern already exists and is proven on arm64 for preprod**, dated 2026-10-12:
`apps-k8s-configs/apps/hydra-m5/workloads/statefulset-hydram5-chain.yaml` lines 58–106. Copy it
verbatim. Do not re-derive it. The non-obvious parts, which the file's own comments record:

| Detail | Value | Why |
|---|---|---|
| Image | `ghcr.io/blinklabs-io/mithril-client:0.13.9@sha256:b063c3f35700c15422f31f0b94a721836aa37c25f9d6778b7c22b3713a2cd27d` | **(decision)** Keep 0.13.9, not the newest 0.13.20. 0.13.9 is the exact digest proven on aarch64 against the preprod aggregator in this cluster. 0.13.x speaks the aggregator's `cardano-database` (v2) API; **0.10.5 would hang in `Init` forever** because `/artifact/snapshots` (v1) is 404. |
| `AGGREGATOR_ENDPOINT` | `https://aggregator.release-preprod.api.mithril.network/aggregator` | |
| `GENESIS_VERIFICATION_KEY` | `5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d` | from `input-output-hk/mithril`, `mithril-infra/configuration/release-preprod`, recorded 2026-10-12 |
| `ANCILLARY_VERIFICATION_KEY` | `5b3138392c3139322c3231362c3135302c3131342c3231362c3233372c3231302c34352c31382c32312c3139362c3230382c3234362c3134362c322c3235322c3234332c3235312c3139372c32382c3135372c3230342c3134352c33302c31342c3232382c3136382c3132392c38332c3133362c33365d` | same source |
| Flag | `mithril-client cardano-db download latest --download-dir "$STAGING" --include-ancillary` | `--include-ancillary` pulls the ledger state too and can skip the hours-long replay. It is signed with **IOG-held** keys, not by Mithril — acceptable on a testnet, deliberately **not** used for the mainnet block producer. |
| Idempotency | marker file `/data/.mithril-restore-complete` containing client version, image, chunk count, last chunk, ledger snapshot | initContainers re-run on **every** pod start; without the marker this re-downloads ~18 GB each time. The marker is written **only after** the final `mv`, and staging is wiped first, because `mithril-client` has `--allow-override` off by default and a retry over partial files fails. |
| Verification before commit | find the `immutable` dir, assert `.chunk` count > 0, **then** `rm -rf /data/db && mv "$SRC" /data/db` | never clobber a live DB on a failed download |

**initContainer, not Job.** It runs inside the node's own pod with the PVC already mounted and
gates startup for free. A Job would need a second mount of an RWO volume and its own ordering.

**If ancillary is rejected** (`MetadataBackendMismatch`, because the node runs `V2LSM` and the
snapshot is `V2InMemory`): the node replays from genesis anyway and reaches tip — ~4–5 h for
preprod, extrapolated from the rack's measured 3.13 h on preview's 15 GB chain. This is a
slowdown, not a failure. `snapshot-converter` conversion is explicitly **out of scope** (the
`Mem→LSM` direction needs `io_uring`, which `seccompProfile: RuntimeDefault` blocks — see
`apps-k8s-configs/CLAUDE.md`).

---

## 4. Local dev stack (`compose.yaml`)

Spec §7's credibility claim is "anyone can re-derive this". The honest shape of that claim has two
doors, and the README must state both.

### 4.1 The compose file

One file at the repo root. Profiles so a contributor can bring up the data substrate without
building Haskell, or the app without a node.

```yaml
# compose.yaml — preprod governance stack, re-derivable from scratch.
# Minimum host: 4 cores, 12 GB RAM, 120 GB free SSD. arm64 and amd64 both work.
#
#   docker compose --profile chain up -d        # node + db-sync + postgres
#   docker compose --profile app   up -d        # + projector, query API, BFF, frontend
#
# TIME UNTIL USEFUL — read §4.3 before you start:
#   from a published snapshot : ~25 min   (set RESTORE_SNAPSHOT)
#   from genesis              : ~1-2 DAYS (db-sync, not the node, is the cost)

name: palmares

services:
  cardano-node:
    profiles: [chain, app]
    image: ghcr.io/blinklabs-io/cardano-node:11.1.2
    environment:
      NETWORK: preprod
      # Mithril bootstrap: ~20 min instead of 6-10 h from genesis. Same aggregator and keys
      # as the cluster (§3). Comment out for a pure from-genesis run.
      MITHRIL_DOWNLOAD: "true"
      MITHRIL_AGGREGATOR_ENDPOINT: https://aggregator.release-preprod.api.mithril.network/aggregator
      MITHRIL_GENESIS_VERIFICATION_KEY: "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d"
    volumes:
      - node-db:/data/db
      - node-ipc:/ipc
    healthcheck:
      test: ["CMD-SHELL", "test -S /ipc/node.socket"]
      interval: 30s
      timeout: 5s
      retries: 20
    restart: on-failure

  postgres:
    profiles: [chain, app]
    image: postgres:17-alpine
    environment:
      POSTGRES_DB: cexplorer
      POSTGRES_USER: postgres
      POSTGRES_PASSWORD: ${POSTGRES_PASSWORD:-devonly}
    command: >
      -c shared_buffers=768MB -c work_mem=32MB -c maintenance_work_mem=512MB
      -c max_wal_size=4GB -c checkpoint_timeout=30min -c synchronous_commit=off
    ports: ["${POSTGRES_PORT:-5432}:5432"]
    volumes: ["postgres:/var/lib/postgresql/data"]
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U postgres -d cexplorer"]
      interval: 10s
      timeout: 5s
      retries: 10
    restart: on-failure

  cardano-db-sync:
    profiles: [chain, app]
    image: ghcr.io/blinklabs-io/cardano-db-sync:13.7.1.0
    environment:
      NETWORK: ""                      # MUST be present and empty — see §2.3
      POSTGRES_HOST: postgres
      POSTGRES_PORT: "5432"
      POSTGRES_DB: cexplorer
      POSTGRES_USER: postgres
      POSTGRES_PASSWORD: ${POSTGRES_PASSWORD:-devonly}
      # Skip the ~48h sync. Verified against its own .sha256sum by the entrypoint (corruption
      # only — same-host checksum, see §2.3(b)). Requires a FRESH postgres+state volume pair.
      RESTORE_SNAPSHOT: ${RESTORE_SNAPSHOT:-}
      # Left at the upstream default Y (drop + create cexplorer) — the first draft set "N",
      # which skips the recreate and makes pg_restore collide with an existing db-sync schema.
    working_dir: /var/lib/cexplorer   # marker + download must land on the volume, not layer /
    command:
      ["--config", "/dbsync-config/db-sync-config.yaml", "--socket-path", "/node-ipc/node.socket"]
    volumes:
      - db-sync-state:/var/lib/cexplorer
      - node-ipc:/node-ipc
      - node-db:/node-db:ro            # the node image keeps config.json + genesis under /data
      - ./deploy/dbsync/:/dbsync-config/:ro
    depends_on:
      cardano-node: { condition: service_healthy }
      postgres:     { condition: service_healthy }
    restart: on-failure

  projector:
    profiles: [app]
    image: ghcr.io/${GH_OWNER:-en7angled}/palmares-projector:${TAG:-latest}
    environment:
      PG_CONN_STR: postgresql://postgres:${POSTGRES_PASSWORD:-devonly}@postgres:5432/cexplorer
    depends_on:
      postgres: { condition: service_healthy }
    restart: on-failure

  query-api:
    profiles: [app]
    image: ghcr.io/${GH_OWNER:-en7angled}/palmares-query-api:${TAG:-latest}
    environment:
      PORT: "8083"
      PG_CONN_STR: postgresql://postgres:${POSTGRES_PASSWORD:-devonly}@postgres:5432/cexplorer
    ports: ["8083:8083"]
    depends_on:
      postgres: { condition: service_healthy }
    restart: on-failure

  bff:
    profiles: [app]
    image: ghcr.io/${GH_OWNER:-en7angled}/palmares-bff:${TAG:-latest}
    environment:
      PALMARES_QUERY_API_URL: http://query-api:8083
    ports: ["8080:8080"]
    restart: on-failure

  frontend:
    profiles: [app]
    image: ghcr.io/${GH_OWNER:-en7angled}/palmares-frontend:${TAG:-latest}
    ports: ["3000:80"]
    restart: on-failure

volumes:
  node-db:
  node-ipc:
  postgres:
  db-sync-state:
```

`deploy/dbsync/db-sync-config.yaml` is the **same file** as the ConfigMap content in §1.3, with
`NodeConfigFile: /node-db/config.json` (the blinklabs node image's own path) instead of
`/node-configs/config.json`. **(decision)** Keep one source file and generate the two variants
with a one-line `sed` in the kustomization and the compose bind — never two hand-maintained copies.

**Footprint:** node 21 GB + db-sync state ~2 GB + Postgres ~10 GB ≈ **35 GB**, peak RAM ~6 GB.

### 4.2 Should contributors start from a Mithril snapshot? Yes

**(decision)** Yes, and it is the **default** (`MITHRIL_DOWNLOAD: "true"` above). The node sync is
pure waiting with no verification value a contributor would miss: Mithril snapshots are
multi-signature-verified by the client, which is a *stronger* guarantee than trusting a single
peer's block stream. Reserve the from-genesis node sync for the reproducibility audit, as one
documented `MITHRIL_DOWNLOAD=false` run.

### 4.3 The snapshot we must publish — and the honest README wording

**(decision)** After the first preprod sync, publish `pg_dump` output of the `cexplorer` database
as a **GitHub Release asset**, with a `.sha256sum` beside it, every time the db-sync version or the
`insert_options` change. The blinklabs entrypoint's `RESTORE_SNAPSHOT` already downloads, verifies
`<url>.sha256sum` and restores it, guarded by a `.restored` marker — **zero new code**, and it is
the same mechanism IOG uses for mainnet.

README wording, which must not overclaim:

> Two ways to get the data. **Re-derive it** — `MITHRIL_DOWNLOAD=false docker compose --profile
> chain up`, and in **1–2 days** you have the governance database from genesis with no trust in
> us at all. Or **restore our snapshot** — set `RESTORE_SNAPSHOT=<release url>` and you are
> querying in **~25 minutes**. The snapshot is a convenience, not the claim: the claim is that
> the first path exists, is documented, and that `scripts/reconcile-koios.sh` will tell you
> whether either database agrees with an independent source.
>
> **Mithril does not speed up `cardano-db-sync`.** It restores the *node's* chain database.
> `cardano-db-sync` still streams every block from genesis and maintains its own ledger state.
> There is no published db-sync snapshot for preprod — ours is the only one.

---

## 5. CI

**Repo is public from commit 1 (spec §7).** That single fact resolves both inherited CI problems:
the self-hosted runner that has been offline since July 2026, and the Actions billing block
(`apps-k8s-configs/CLAUDE.md` line 49: *"The job was not started because recent account payments
have failed…"*). Public repositories get GitHub-hosted runners **free and unlimited**, including
arm64 — and this account's own public Haskell repo `mariusgeorgescu/solana-haskell-sdk` ran CI to
**success today, 2026-10-19T18:19→18:31Z**. Empirically settled.

**(decision)** Three workflows, no self-hosted runner, no Docker Hub.

| Workflow | Trigger | Runner | Blocks merge? |
|---|---|---|---|
| `ci.yml` — Haskell build+test, TS build+test, gitleaks, bundle scan, kustomize/kubeconform | PR + push to `main` | `ubuntu-24.04-arm` | **Yes, all jobs** |
| `images.yml` — build + push 5 arm64 images to GHCR | push to `main`, tags `v*` | `ubuntu-24.04-arm` | n/a (post-merge) |
| `reconcile.yml` — Koios parity against live preprod | nightly cron + `workflow_dispatch` | `ubuntu-latest` | **No — warns only** |

**(decision) `ubuntu-24.04-arm` for everything, including tests.** The cluster is arm64, so the
images must be arm64, and QEMU cross-building GHC is unusably slow. One architecture means one
toolchain, one system-library install step, one cache — and the tests then run on the same
architecture that production runs on, which is where a `plutus-tx-plugin` or `blst` difference
would actually show up. Public-repo arm64 runners are 4 vCPU / 16 GB, same as `ubuntu-latest`.

### 5.1 `ci.yml`

```yaml
name: CI
on:
  pull_request:
  push:
    branches: [main]
concurrency:
  group: ci-${{ github.ref }}
  cancel-in-progress: true

jobs:
  haskell:
    runs-on: ubuntu-24.04-arm     # arm64: same arch as the target cluster
    steps:
      - uses: actions/checkout@v4
      - name: System libraries for plutus-tx / cardano-crypto
        run: |
          set -euo pipefail
          sudo apt-get update
          sudo apt-get install -y build-essential pkg-config libffi-dev libgmp-dev \
            libssl-dev libtinfo-dev libsystemd-dev zlib1g-dev libpq-dev liblmdb-dev \
            libsodium-dev libsecp256k1-dev libblst-dev
      - uses: haskell-actions/setup@v2
        id: setup
        with:
          ghc-version: "9.6.6"      # forced by plutus-tx-plugin; do not widen
          cabal-version: "3.10"
      - uses: actions/cache@v4
        with:
          # Caching the cabal store AND dist-newstyle is the pattern this account already runs
          # in mariusgeorgescu/solana-haskell-sdk. CAVEAT, corrected 2026-10-19: that repo's
          # ci.yml is `runs-on: ubuntu-latest` (x86_64), so its 12-minute run proves
          # public-repo Actions work — it does NOT prove haskell-actions/setup on arm64.
          # Key on the freeze file: CHaP + Atlas are pinned by commit.
          path: |
            ${{ steps.setup.outputs.cabal-store }}
            dist-newstyle
          key: ${{ runner.os }}-${{ runner.arch }}-ghc9.6.6-${{ hashFiles('cabal.project.freeze', 'cabal.project') }}
          restore-keys: ${{ runner.os }}-${{ runner.arch }}-ghc9.6.6-
      - run: cabal update
      - run: cabal build all --enable-tests
      - run: cabal test all --test-show-details=direct

  typescript:
    runs-on: ubuntu-24.04-arm
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with: { node-version: "22", cache: "npm" }
      - run: npm ci
      - run: npm run typecheck
      - run: npm run lint
      - run: npm run test -- --run          # vitest
      - run: npm run build                  # vite build
      - name: Bundle secret scan (leak #1 guard — see §6)
        run: |
          set -euo pipefail
          # No credential-shaped VITE_* may ever reach the browser bundle. bjj-frontend shipped
          # VITE_MAESTRO_API_KEY and VITE_BLOCKFROST_PROJECT_ID into dist/. Never again.
          if grep -rIlE 'VITE_[A-Z0-9_]*(KEY|TOKEN|SECRET|PASSWORD|PROJECT_ID)' src/ ; then
            echo "::error::credential-shaped VITE_* variable in frontend source"; exit 1
          fi
          # 40+ char base58/hex/base64 blobs in the built bundle are almost always keys.
          if grep -rIoE '[A-Za-z0-9_-]{40,}' dist/assets/*.js | grep -vE 'sourceMappingURL' \
             | head -5 | grep -q . ; then
            echo "::warning::long opaque strings in the bundle — review before release"
          fi

  secrets:
    runs-on: ubuntu-24.04-arm
    steps:
      - uses: actions/checkout@v4
        with: { fetch-depth: 0 }            # full history: leak #2 was a token COMMITTED
      - uses: gitleaks/gitleaks-action@v2
        env: { GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }} }

  manifests:
    runs-on: ubuntu-24.04-arm
    steps:
      - uses: actions/checkout@v4
      - name: kustomize + kubeconform
        run: |
          set -euo pipefail
          # Same two gates as apps-k8s-configs/.github/workflows/validate.yaml, over deploy/.
          curl -sSL "https://github.com/kubernetes-sigs/kustomize/releases/download/kustomize%2Fv5.8.3/kustomize_v5.8.3_linux_arm64.tar.gz" | tar xz
          curl -sSL "https://github.com/yannh/kubeconform/releases/download/v0.6.7/kubeconform-linux-arm64.tar.gz" | tar xz kubeconform
          sudo mv kustomize kubeconform /usr/local/bin/
          for p in deploy/k8s/base deploy/k8s/preprod; do
            kustomize build --load-restrictor LoadRestrictionsNone "$p" \
              | kubeconform -ignore-missing-schemas -summary -
          done
      - name: No plaintext Secret
        run: |
          set -euo pipefail
          for f in $(grep -rlE '^kind: Secret' deploy || true); do
            grep -q 'ENC\[AES256_GCM' "$f" || { echo "::error file=$f::unencrypted Secret"; exit 1; }
          done
```

**Haskell caching strategy, stated plainly:** `actions/cache` on
`${{ steps.setup.outputs.cabal-store }}` **plus** `dist-newstyle`, keyed on
`hashFiles('cabal.project.freeze', 'cabal.project')`. This is the exact pattern already proven on
this account (`solana-haskell-sdk`, 12 min build+test). Cold build with Plutus + Atlas + CHaP will
be substantially longer — budget **60–120 min for the first run on a new freeze file**, ~10–20 min
warm. Keep `cabal.project.freeze` committed; without it the cache key changes on every
Hackage/CHaP index move and the cache never hits. GitHub's 10 GB per-repo cache limit is enough for
one GHC 9.6.6 store; it is not enough for a GHC matrix — **do not add one** (base bounds pin 9.6
anyway).

**The aarch64 GHC 9.6.6 bindist exists** (checked 2026-10-19 in
[`ghcup-metadata/ghcup-0.0.9.yaml`](https://raw.githubusercontent.com/haskell/ghcup-metadata/master/ghcup-0.0.9.yaml),
GHC `9.6.6` → `viArch.A_ARM64.Linux_UnknownLinux` →
`https://downloads.haskell.org/~ghc/9.6.6/ghc-9.6.6-aarch64-deb10-linux.tar.xz`; an
`aarch64-alpine` bindist is there too). `haskell-actions/setup` resolves through ghcup, so the
hosted arm64 path should work — but it is still a 15-minute F0 experiment, because the
*account's* only proven Haskell CI run is x86_64.

**Fallback if `haskell-actions/setup@v2` cannot provide an aarch64 GHC 9.6.6 bindist:** build inside
a container, reusing the Belt System's proven `Dockerfile.base` (`benz0li/ghc-musl:9.6.6` +
libsodium 1.0.19 + secp256k1 v0.3.2 + blst v0.3.15, with `cabal build --only-dependencies` baked
into a layer keyed on the `.cabal`/freeze files). Push that builder to
`ghcr.io/<owner>/palmares-builder:9.6.6` and `docker run` it for build+test. Verify the hosted
path in the first hour of F0; this is a 15-minute experiment, not a design question.

### 5.2 `images.yml`

```yaml
name: images
on:
  push:
    branches: [main]
    tags: ["v*.*.*"]
  workflow_dispatch:

jobs:
  build:
    runs-on: ubuntu-24.04-arm
    permissions:
      contents: read
      packages: write          # GHCR via GITHUB_TOKEN — no DOCKERHUB_* secrets at all
    strategy:
      fail-fast: false
      matrix:
        svc: [projector, query-api, interaction-api, bff, frontend]
    steps:
      - uses: actions/checkout@v4
      - uses: docker/setup-buildx-action@v3
      - uses: docker/login-action@v3
        with:
          registry: ghcr.io
          username: ${{ github.actor }}
          password: ${{ secrets.GITHUB_TOKEN }}
      - uses: docker/build-push-action@v6
        with:
          context: .
          file: Dockerfile.${{ matrix.svc }}
          platforms: linux/arm64
          push: true
          # Short SHA is the tag the GitOps overlay pins, exactly like apps/bjj-belts/dev.
          tags: |
            ghcr.io/${{ github.repository }}-${{ matrix.svc }}:${{ github.sha }}
            ghcr.io/${{ github.repository }}-${{ matrix.svc }}:latest
          cache-from: type=gha,scope=${{ matrix.svc }}
          cache-to: type=gha,mode=max,scope=${{ matrix.svc }}
          build-args: |
            VITE_PUBLIC_API_BASE_URL=${{ vars.VITE_PUBLIC_API_BASE_URL }}
            VITE_PUBLIC_NETWORK=${{ vars.VITE_PUBLIC_NETWORK }}
            VITE_PUBLIC_EXPLORER_BASE_URL=${{ vars.VITE_PUBLIC_EXPLORER_BASE_URL }}
```

**(decision) GHCR, not Docker Hub.** `GITHUB_TOKEN` is automatic, so `DOCKERHUB_USERNAME` and
`DOCKERHUB_TOKEN` disappear from the secret list. Public GHCR packages pull anonymously, exactly
like the current `mariusgeorgescu/*` images do — no `imagePullSecrets` in the cluster.

**(decision) Frontend build-time vars come from GitHub Actions `vars`, never from a gitignored
`.env`.** This is the direct fix for the recorded failure "FE image build needs `.env` — a
worktree/CI build drops chat + Maestro fetcher + onramp". Repository **variables** (not secrets)
are the right home: their values are public anyway once they are in the bundle, and `vars` are
visible in logs, which is a feature here — anything that must *not* be visible must not be a
`VITE_*` at all. See §6.

### 5.3 `reconcile.yml` — Koios parity (warns, never blocks)

```yaml
name: reconcile
on:
  schedule: [{ cron: "0 3 * * *" }]
  workflow_dispatch:
jobs:
  koios:
    runs-on: ubuntu-latest
    continue-on-error: true            # an upstream outage must never look like our bug
    steps:
      - uses: actions/checkout@v4
      - name: Koios preprod parity
        env:
          PALMARES_API: https://gov.cardano.vip/api
          KOIOS_API: https://preprod.koios.rest/api/v1   # verified HTTP 200, 2026-10-19
        run: ./scripts/reconcile-koios.sh --sample-dreps 20 --sample-actions 10
      - uses: actions/upload-artifact@v4
        if: always()
        with: { name: koios-divergences, path: out/divergences.json }
```

Endpoints, per spec §4.1, all probed 2026-10-19 against `https://preprod.koios.rest/api/v1`:
`/drep_voting_power_history` (GET, needs `_epoch_no` — a bare `?limit=1` timed out 504),
`/proposal_list`, `/vote_list`, `/drep_epoch_summary`, `/drep_list` (GET, 200), plus
`/drep_info` for the F1 check-6 sample — **`/drep_info` is POST-only** (`{"_drep_ids":[…]}`);
a GET returns 404, so the reconciliation script must not treat it as a GET.
Koios itself runs on db-sync, so this is a *completeness* cross-check, never a source of truth —
state that in the script's header so nobody later treats a divergence as proof we are wrong.

**(decision) Why it only warns:** it needs a live, caught-up preprod stack. Making it a merge gate
would block every PR whenever the Pi is re-syncing or Koios is down. It uploads
`divergences.json` and the F1 acceptance check (§9 F1 #6, "0 unexplained divergences") is a
**release gate**, read from the artifact, not a PR gate.

### 5.4 Blocks vs warns, explicitly

**Blocks a merge:** `cabal build all` · `cabal test all` · `tsc --noEmit` · `eslint` · `vitest` ·
`vite build` · the credential-shaped-`VITE_*` grep · `gitleaks` over full history ·
`kustomize build` + `kubeconform` on `deploy/` · the plaintext-Secret guard.

**Warns only:** Koios reconciliation · the bundle entropy scan · `hlint` / `stylish-haskell` ·
Playwright e2e against preprod (needs the live stack + a funded test wallet; run it on
`workflow_dispatch` and before a release, never on a PR) · `scripts/test_exunits.sh` script-size
and ExUnits report (post it as a PR comment — a number to watch, not a gate).

---

## 6. Secrets and hygiene

### 6.1 The explicit secret list for F1 — four items, and the read path has none

| Secret | Where it lives | Needed for | F1? |
|---|---|---|---|
| `POSTGRES_PASSWORD` | SOPS, `chains/cardano/preprod-dbsync/secrets/secret-palmares-dbsync.yaml` | db-sync ↔ Postgres ↔ projector ↔ query API | yes |
| SOPS age private key | already in-cluster: `sops-age` Secret in `flux-system` | Flux decryption | already exists |
| `MAESTRO_API_KEY` (or `BLOCKFROST_PROJECT_ID`) | SOPS, `apps/palmares/preprod/secrets/` | **write path only** — tx building in interaction-api (F1 §B check 11) | yes, write path only |
| `operation.prv` (oracle admin key) | **never in Git, never in a container image** — operator laptop + offline backup | `admin` CLI: deploy reference scripts, oracle ops, `rotate-admin` | yes, operator-side |

**Zero** image-registry secrets (GHCR + `GITHUB_TOKEN`). **Zero** secrets for the public read path —
node, db-sync, Postgres, projector, query API and the explorer need no third-party credential at
all. That is what makes spec §7's "the read path is 100% autonomous" literally true, and it is
worth writing on the README.

### 6.2 The two leaks that bit this team, and the exact rules that stop them

**Leak #1 — a provider token baked into a frontend bundle.** The mechanism, verified in
`bjj-frontend`: the source reads `VITE_MAESTRO_API_KEY`, `VITE_BLOCKFROST_PROJECT_ID` and
`VITE_UTXOS_PROJECT_ID`; Vite substitutes every `VITE_*` at **build** time, so those values ship
inside `dist/assets/*.js` to every visitor. `.gitignore` had `.env` — but
**`.dockerignore` did not** (it is four lines: `node_modules`, `dist`, `.git`, `.DS_Store`), so
`docker build` copied the real `.env` into the build context and baked it in.

Rules, each one a CI gate or a committed file:

1. **`.dockerignore` must exclude `.env*`.** This is the rule whose absence caused the leak.
2. **Naming is the contract: only `VITE_PUBLIC_*` may exist.** Anything else fails the
   `typescript` job's grep. A reviewer should never have to reason about whether a `VITE_*` is
   secret — the prefix says it is public, and the regex says a credential-shaped name is a build
   failure.
3. **Every provider token is server-side only**, held by the BFF, injected from a SOPS Secret via
   `envFrom`, and reached by the browser only through a BFF route that rate-limits. The frontend
   never holds a key, so there is nothing to leak.
4. **Post-build bundle scan** (§5.1) greps `dist/` before the image is built.
5. **Network configurable at build, never hardcoded** (spec §4.5): `VITE_PUBLIC_NETWORK` is a
   GitHub repository **variable**.

```
# .dockerignore (repo root) — the four lines at the top are the leak fix
.env
.env.*
!.env.example
**/.env
**/.env.*
.git
.github
node_modules
dist
dist-newstyle
.direnv
result
result-*
.hie
*.hi
*.o
operation.prv
*.skey
*.vkey
config/*.prv
docs
*.md
!README.md
.DS_Store
```

```
# .gitignore (repo root) — relevant lines
.env
.env.*
!.env.example
operation.prv
*.skey
*.vkey
config/config_*.local.json
dist-newstyle/
.direnv/
cabal.project.local
# cabal.project.freeze is COMMITTED on purpose: it is the CI cache key (§5.1).
```

`.env.example` is committed and contains **names with empty values only** — it is the
documentation of what a contributor must set, and the CI greps it with the same credential regex.

**Leak #2 — a token committed in history.** The Maestro token is still live in the Belt System's
git history (project memory: *"Maestro token env-injected but still live-in-history until
revoked"*). The lesson is that `git rm` does not help; only rotation does.

6. **`gitleaks` with `fetch-depth: 0` is a merge blocker from commit 1** (§5.1). On a repo that is
   public from the first commit there is no grace period: a push is a publication.
7. **A `.githooks/pre-push` hook mirrors it locally** — same hook name as `apps-k8s-configs`,
   which ships only `pre-push` (checked: `.githooks/` contains exactly that one file), so the
   `core.hooksPath` instruction and the hook name stay identical across the two repos. With the
   `apps-k8s-configs` lesson attached — git
   never propagates hooks, so the README's first instruction is
   `git config core.hooksPath .githooks`, and CI is the real gate because a hook can be bypassed
   with `--no-verify`.
8. **Rotation, not redaction, is the remediation**, and it is written into the runbook before the
   first incident: revoke at the provider, re-issue, re-encrypt the SOPS file, bump the Deployment
   stamp annotation so `envFrom` is re-read. The history stays dirty and that is fine, because the
   credential is dead.
9. **`operation.prv` and `*.skey` are in both ignore files**, because the on-chain admin key is the
   one secret whose leak cannot be fixed by rotation alone (spec §4.3 fix #2 adds `rotate-admin`
   precisely so that it can).

### 6.3 One more hygiene item worth a line

The spec's own §7 asks for "no secrets in history". Add `scripts/preflight.sh` that a contributor
runs once: enables the hooks path, checks `.env` is ignored by **both** ignore files, and prints
the four-item secret list above. Three commands, prevents the two leaks from being reintroduced by
the next person, and is the kind of file that costs nothing and gets read.

---

## 7. Open questions for the owner (genuine, not rhetorical)

1. **`minirack-m4-keys` is missing from the live node list** (6 nodes Ready, `CLAUDE.md` says 7).
   Irrelevant to this plan, but `apps-k8s-configs/CLAUDE.md` is stale on it.
2. **Two full preprod `cardano-node`s run today** — `cardano-preprod` on pi5-1 and `hydra-m5` on
   pi5-2, 100 GB PVC each. Retiring the `hydra-m5` one would free ~100 GB on the disk-poorest node
   (pi5-2 has only 270 GB free, against 767/824 GB on pi5-1/pi5-3). Not needed for F1. Owner's call.
3. **`input-output-hk/dbsync`** appeared during this research: *"A modular Cardano → PostgreSQL
   indexer. Parallel extraction pipeline, profile-driven schemas."* A governance profile on a
   parallel pipeline is exactly our shape, and it is an IOG repo. It is **not** a Track-5 question
   — flagging it for whoever owns the data-substrate decision, because if it is real and
   maintained it would change §4.1 of the spec. Do not act on it without verifying maturity,
   arm64 support, and schema stability; `only_governance` on db-sync 13.7.1.0 is the decision for F1.
4. **The db-sync arm64 pin (13.7.1.0) trails the newest release (13.7.2.1).** Re-check
   `ghcr.io/blinklabs-io/cardano-db-sync` tags at F0; upstream aarch64 is in CHANGELOG
   "Unreleased" and may make this moot.

## 8. What could not be determined here

- **The real db-sync preprod sync duration.** No published figure exists for any preset, let alone
  governance-only on arm64. §1.5 gives a derived 24–48 h band with its reasoning; the first sync
  **is** the measurement, and F1 check 1 should record it.
- **Whether db-sync parses the node's `LedgerDB: {Backend: V2LSM, LSMDatabasePath: /data/lsm}`
  block in `config.json` without complaint.** db-sync links cardano-node's own config parser, so
  this should be fine, and it is a 5-minute test. Fallback if it is not: vendor a db-sync-only copy
  of `config.json` with the `LedgerDB` block removed into
  `configmap-palmares-dbsync-config`, keeping the genesis hashes — the genesis files still come
  from the shared hostPath, so the fail-closed hash verification is preserved.
- ~~**Whether GitHub's aarch64 `haskell-actions/setup` path carries GHC 9.6.6.**~~ **RESOLVED
  2026-10-19:** ghcup's own metadata publishes an aarch64 Linux bindist for GHC 9.6.6
  (`ghc-9.6.6-aarch64-deb10-linux.tar.xz`, plus an alpine one) — see §5.1. The container fallback
  stays documented, but this is no longer an unknown.

---

## 9. Fact-check corrections

An adversarial fact-check was run against this note on 2026-10-19: every db-sync table, column,
config key and metric name was checked against the real `doc/schema.md`, `doc/configuration.md`
**and the 13.7.1.0 source**; every claimed file in `apps-k8s-configs`, `bjj-frontend` and
`Decentralized-Belt-System` was opened; every image tag was resolved against the real registry;
every GitHub claim was checked with `gh`. What changed:

| # | Section | Was | Now | Primary source |
|---|---|---|---|---|
| C1 | §0.4, §1.3 | "`preset:` overrides everything, so hand-write the option set" | **Use `preset: only_governance` + `offchain_vote_data: "enable"`.** The code loads the preset as the *base* and lets explicit keys win. The hand-written set, having no preset, inherited `def` → `tx_metadata` **and** `reward` enabled, and `sioRewards` is not settable from JSON at all. | `Config/Types.hs` L465–495 (parseOverrides), L798–816 (`def`), L858–884 (`onlyGovInsertOptions`/`disableAllInsertOptions`) @13.7.1.0 |
| C2 | §2.3, §2.6 | probe + ServiceMonitor on `path: /metrics` | **`path: /`.** `serveMetricsT port []` serves only `/`; `/metrics` is a 404. Left wrong, the pod is never Ready → Flux root Ready=False → `palmares-preprod` never reconciles. `initialDelaySeconds` also cut 60→10 to fit the 1m Flux timeout. | `Metrics.hs` L73 @13.7.1.0; bitnomial/prometheus `Http/Scrape.hs` (`pathInfo request == path`) |
| C3 | §2.6 | alert on `cardano_db_sync_node_slot_height` | **No such metric.** Use `cardano_db_sync_node_block_height - cardano_db_sync_db_block_height`. | `Metrics.hs` `makeMetrics` @13.7.1.0 (six gauges, listed in §2.6) |
| C4 | §0.5, §1.4.1 | "`is_valid` + a fetch-error row = the four verdicts, zero new code" | `is_valid` is **CIP-100 conformance**, not hash validity; hash mismatches land in `off_chain_vote_fetch_error`, whose only discriminator is free-text `fetch_error`. A pinned ~30-line classifier is needed. | `doc/schema.md` @13.7.1.0, `off_chain_vote_data` / `off_chain_vote_fetch_error` |
| C5 | §1.4.3 | "`drep_distr` *is* the spec's `drep_epoch_power`" | `drep_distr` = (hash_id, amount, epoch_no, active_until) — **no delegator count**. And `shelley.enable: false` hides stake de-registrations, so delegator count/churn/retention (spec §4.2, F1 check 12) are an **upper bound**. Decision surfaced instead of buried. | `doc/schema.md` @13.7.1.0; `configuration.md` Shelley section |
| C6 | §2.3, §4.1 | `RESTORE_SNAPSHOT` "guarded by a marker" | Marker **and** the multi-GB download go to the CWD, and the image has no `WORKDIR` → `/`, i.e. the container layer. Added `workingDir: /var/lib/cexplorer` (and `working_dir:` in compose); documented the same-host checksum and the empty-state-dir precondition; dropped `RESTORE_RECREATE_DB: "N"`. | `blinklabs-io/docker-cardano-db-sync` `bin/entry-point` + `Dockerfile` (no WORKDIR); `scripts/postgresql-setup.sh` `restore_snapshot`, `RESTORE_RECREATE_DB:-Y` |
| C7 | §5.1 | "~12 minutes measured on this account's own public Haskell repo" attached to the arm64 job | That run is `runs-on: ubuntu-latest` (**x86_64**). It proves public-repo Actions work, not arm64 Haskell. Caveat added; the arm64 GHC question answered separately. | `gh api repos/mariusgeorgescu/solana-haskell-sdk/contents/.github/workflows/ci.yml` |
| C8 | §1.3 | no IPFS config | Added the top-level `ipfs_gateway` key (default `["https://ipfs.io/ipfs"]`, undocumented in `configuration.md`); an empty list makes every `ipfs://` anchor fail `OCFErrNoIpfsGateway`. | `Config/Types.hs` L441; `OffChain/Http.hs` `useIpfsGatewayMaybe` @13.7.1.0 |
| C9 | §1.4.1 | — | Added the open upstream known issue *"duplicate off-chain vote metadata child rows"* (present in 13.7.1.0 **and** 13.7.2.1) — dedupe every read of the `off_chain_vote_*` child tables. | 13.7.2.1 release notes, "Known issues" |
| C10 | §1.2 | `minirack-m4` "tainted `dedicated=m4:NoSchedule`" | `CLAUDE.md` records `dedicated=m4-keys` on `minirack-m4-keys`, not a taint on `minirack-m4`. Flagged for re-reading live. | `apps-k8s-configs/CLAUDE.md` L4, L15 |
| C11 | §2.2 | "~26 s catch-up (measured)" | That number is the post-**conversion** catch-up, not a plain restart. Also recorded that the StatefulSet's "rămâne V2InMemory" comment is stale — the ConfigMap really is `V2LSM`, so the note's premise holds. | `apps-k8s-configs/CLAUDE.md` L11; `chains/cardano/preprod/config/configmap-cardano-preprod-node-config.yaml` L73-81 |
| C12 | §6.2 | "a pre-commit hook" | `apps-k8s-configs/.githooks/` ships exactly one file, `pre-push`. Same name here. | `apps-k8s-configs/.githooks/` |
| C13 | §2.1 | — | Noted that `palmares-preprod` → `dependsOn: palmares-dbsync` couples the app root to db-sync readiness, and that this is what makes C2 a blocker rather than a nit. | Flux `dependsOn` semantics + the roots read in `clusters/minirack/` |

**Re-verified and left unchanged** (each one opened, not taken on trust):
`ghcr.io/blinklabs-io/cardano-db-sync:13.7.1.0` is a genuine `manifest.list` with
`[(linux,amd64),(linux,arm64)]` at digest `sha256:2a341eb3…54eb950`, and it is the newest tag
(the image's own `Dockerfile` pins `DBSYNC_VERSION=13.7.1.0`) · IntersectMBO's `13.7.2.1` and
`13.7.3.0` are single amd64 manifests, and `feature_aarch64-release-bin-arm64` tags show the
aarch64 work still in flight · 13.7.2.1 (2026-06-17) is the newest *release*, 13.7.1.0 is
2026-05-21 · the Raspberry-Pi LSM crash and the `blockio-uring` >4096-item bug are verbatim in the
13.7.2.1 known issues, and `ledger_backend` is a **top-level** option defaulting to `inmemory` ·
only mainnet 13.6/13.7 db-sync snapshots are published — **no preprod snapshot exists** ·
`PrometheusPort` may be omitted: `fromMaybe 8080 <$> (o .:? "PrometheusPort")` and the metrics
server starts unconditionally · README per-network figures (preprod 12 GB / ~2 GB / 16 GB /
3.5 GB RSS; mainnet 203 / ~10 / 438 / 21) · `gov_action_proposal.{expiration, ratified_epoch,
enacted_epoch, expired_epoch, dropped_epoch}` all exist · the entrypoint's `NETWORK`-must-be-
present-and-empty contract, the appended `"${@}"`, writable `/configuration`, `gen-pgpass`'s
env-over-`/run/secrets` precedence, state dir `/var/lib/cexplorer`, `EXPOSE 8080` ·
`.sops.yaml`'s second rule already covers `^chains/.*/secrets/…` and `^apps/.*/secrets/…` with
age key `age13hn00ph…` · `kube-prometheus-stack` really runs `serviceMonitorSelector: {}` ·
`local-path` StorageClass has no `allowVolumeExpansion` (so: false), `reclaimPolicy: Delete`,
`WaitForFirstConsumer` · Flux `v2.9.3` · `letsencrypt-http01-prod` and `letsencrypt-godaddy-prod`
both exist, `gatewayClassName: traefik`, and the `namePrefix` trap is exactly as
`httproute-bjjbelts-dev.yaml` writes it · the preprod StatefulSet really has `configs`/`ipc` as
`emptyDir`, a `fetch-configs` initContainer that assembles `config.json` + the four genesis files,
and `hydra-node` as a sidecar (so the sidecar-vs-pod argument holds), while the mainnet BP really
publishes `/var/lib/cardano-mainnet/{ipc,configs}` on hostPath · `apps/bjj-belts/dev/patches/
deployment-bjjbelts-stateless-placement.yaml` excludes only `pi5-4` today · the Belt System's
`ServiceProbe.hs` serves both `"health"` and `"ready"`, and bjj's query-api wires all three probes
to `/health` · `Dockerfile.base` is `benz0li/ghc-musl:9.6.6` + libsodium 1.0.19 + secp256k1
v0.3.2 + blst v0.3.15 · `bjj-frontend/.dockerignore` is exactly four lines, `.env` exists and is
gitignored, the Dockerfile does `COPY . .`, and `VITE_MAESTRO_API_KEY`,
`VITE_BLOCKFROST_PROJECT_ID`, `VITE_UTXOS_PROJECT_ID` are all read in source — leak #1's
mechanism is confirmed exactly as described · `solana-haskell-sdk` is public and ran CI green
2026-10-19 18:19→18:31Z, `apps-k8s-configs` is private, and GitHub's arm64 announcement
(2025-08-07) gives `ubuntu-24.04-arm`, 4 vCPU, free on public repos and **unavailable on private
ones** · `kustomize_v5.8.3_linux_arm64.tar.gz` and `kubeconform-linux-arm64.tar.gz` both exist ·
`postgres:17-alpine` publishes `linux/arm64/v8` · `ghcr.io/blinklabs-io/mithril-client:0.13.9`
resolves to the digest in the note and in the live manifest, multi-arch · Koios preprod
`https://preprod.koios.rest/api/v1` answers 200 on `/tip` (epoch 320),
`/drep_voting_power_history` (with `_epoch_no`), `/proposal_list`, `/vote_list`,
`/drep_epoch_summary`, `/drep_list`; `/drep_info` is **POST-only**.
