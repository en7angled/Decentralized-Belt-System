# Plan A — block: the governance data substrate

This block delivers the Postgres that everything else in Palmares reads: a measured off-cluster
from-genesis preprod `cardano-db-sync` run (synthesis §5, Experiment A), the cluster-side provisioning
(node IPC on hostPath, Postgres 17 + 50 Gi PVC, the db-sync Deployment), the real from-genesis sync that
**is** F1 check 1, `sql/040_indexes.sql` through the house gate-Job pattern, the sizing measurement that
writes ADR `0001-governance-data-substrate.md`, and the three Postgres roles with their grants and
`search_path`. It implements synthesis §3 Phase 2 steps 8–13 plus Phase 3 step 14, and the decisions
D-4 (`shelley: enable: true`), D-5 (the four-key `insert_options`), D-6 (separate Deployment on a
hostPath socket), D-7 (the two lag thresholds), D-13 (50 Gi, measured, with ordered fallbacks),
D-15 (two DDL mechanisms split by transactionality) and D-26 (manifests only in `apps-k8s-configs`).
Assumptions: the preprod `cardano-node` 11.1.2 on `minirack-pi5-1` stays as it is (D-12 — no new node,
no Mithril on the critical path); the owner has approved the five Phase-0 decisions and the one node
pod restart; `/Users/mg/Projects/CardanoProjects/palmares` has been created by Plan A's bootstrap task
before T-SUBSTRATE-2 (every cluster task here commits its check script to `palmares/scripts/`;
T-SUBSTRATE-1 needs only the `experiments/` directory, and T-SUBSTRATE-6 needs the `admin`
executable); and the GitOps repo is at
`/Users/mg/Projects/apps-k8s-configs` (**not** under `CardanoProjects/` — the design notes' path is
wrong, the one here is verified on disk).

Nothing in this block makes a performance claim before T-SUBSTRATE-6, and nothing in it reads
`palmares.*` — those views are the read-path block's.

---

### Task T-SUBSTRATE-1: Experiment A — off-cluster from-genesis preprod db-sync, measured

The riskiest unknown in F1 (synthesis §5). Runs on the owner's Apple-silicon Mac (Docker 29.8.2,
aarch64, 4 CPU, 16.5 GB RAM, 161 GB free — verified 2026-10-19), **off-cluster, zero cluster churn**, in
parallel with the whole extraction track. Its deliverable is a measurement table, and that table decides
the PVC size, the db-sync memory limit and whether the substrate decision reopens.

**Files:**
- Create: `/Users/mg/Projects/CardanoProjects/palmares/experiments/dbsync-genesis/compose.yaml`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/experiments/dbsync-genesis/db-sync-config.yaml`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/experiments/dbsync-genesis/node-configs/config.json`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/experiments/dbsync-genesis/node-configs/{byron,shelley,alonzo,conway}-genesis.json`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/experiments/dbsync-genesis/measure.sh`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/experiments/dbsync-genesis/rss-sample.sh`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/experiments/dbsync-genesis/RESULTS.md`
- Test: `measure.sh` is the test; `RESULTS.md` is its recorded output

**Interfaces:**
- Consumes: nothing.
- Produces: `experiments/dbsync-genesis/RESULTS.md` carrying `WALLCLOCK_TO_TIP_HOURS`,
  `PG_TOTAL_BYTES`, `PEAK_RSS_BYTES`, `EPOCH_SYNC_TIME` table, and the five `count(*)` probes.
  Also: the verdict on whether db-sync 13.7.1.0 parses the cluster's `LedgerDB: V2LSM` node config.
  The `chain`-profile services here become the root `compose.yaml`'s `chain` profile verbatim in the
  Phase-1 bootstrap task.

- [ ] **Step 1: Write the failing test**

`measure.sh` — the whole measurement, idempotent, runnable mid-sync and at the end. It exits non-zero
until every probe is satisfiable.

```bash
#!/usr/bin/env bash
# Experiment A measurement. Run any time; it reports partial progress and exits 1 until done.
# Usage: ./measure.sh [>> RESULTS.md]
set -euo pipefail
PSQL=(docker compose exec -T postgres psql -U dbsync -d cexplorer -At -F$'\t')

echo "## measured $(date -u +%FT%TZ)"

# --- 1. progress and wall-clock to tip -------------------------------------------------------
"${PSQL[@]}" -c "SELECT 'tip_epoch', max(epoch_no) FROM block;"
"${PSQL[@]}" -c "SELECT 'lag_seconds',
  EXTRACT(EPOCH FROM (now() AT TIME ZONE 'UTC'
    - (SELECT time FROM block ORDER BY id DESC LIMIT 1)))::bigint;"

# --- 2. per-epoch timings: db-sync records them itself, no log parsing -----------------------
echo "### epoch_sync_time"
"${PSQL[@]}" -c "SELECT no, seconds, state FROM epoch_sync_time ORDER BY no;"

# --- 3. THE PVC ANSWER: hardware-independent -------------------------------------------------
echo "### pg_total_relation_size top 20"
"${PSQL[@]}" -c "SELECT relname, pg_total_relation_size(c.oid)
  FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace
  WHERE n.nspname = 'public' AND c.relkind = 'r'
  ORDER BY pg_total_relation_size(c.oid) DESC LIMIT 20;"
"${PSQL[@]}" -c "SELECT 'PG_TOTAL_BYTES', pg_database_size('cexplorer');"

# --- 4. peak RSS: written by rss-sample.sh, which runs for the whole experiment ---------------
# rss.csv column 2 is RAW BYTES, so this sort is numeric. (docker stats' MemUsage is a human
# string — "3.5GiB" sorts below "900MiB" under `sort -n` — which is why we do not sample it.)
echo "### PEAK_RSS_BYTES"
sort -t, -k2 -n rss.csv | tail -1

# --- 5. the five count(*) probes -------------------------------------------------------------
echo "### probes"
"${PSQL[@]}" -c "SELECT 'stake_deregistration', count(*) FROM stake_deregistration;"
"${PSQL[@]}" -c "SELECT 'pool_update',          count(*) FROM pool_update;"
"${PSQL[@]}" -c "SELECT 'drep_distr',           count(*) FROM drep_distr;"
"${PSQL[@]}" -c "SELECT 'off_chain_vote_data',  count(*) FROM off_chain_vote_data;"
"${PSQL[@]}" -c "SELECT 'off_chain_vote_fetch_error', count(*) FROM off_chain_vote_fetch_error;"
# drep_distr epoch coverage: no gap, and current. Preprod's first Conway epoch is 163.
"${PSQL[@]}" -c "SELECT 'drep_distr_epochs',
  min(epoch_no), max(epoch_no), count(DISTINCT epoch_no),
  (max(epoch_no) - min(epoch_no) + 1) = count(DISTINCT epoch_no) AS no_gap
  FROM drep_distr;"

# --- 6. the gate -----------------------------------------------------------------------------
fail=0
chk() { # name, sql returning boolean
  local ok; ok=$("${PSQL[@]}" -c "SELECT $2;")
  [ "$ok" = t ] || { echo "FAIL: $1"; fail=1; }
}
chk "shelley flip took (stake_deregistration non-empty)" "(SELECT count(*) FROM stake_deregistration) > 0"
chk "shelley flip took (pool_update non-empty)"          "(SELECT count(*) FROM pool_update) > 0"
chk "drep_distr current"      "(SELECT max(epoch_no) FROM drep_distr) >= (SELECT max(epoch_no) FROM block) - 1"
chk "drep_distr has no gaps"  "(SELECT (max(epoch_no)-min(epoch_no)+1) = count(DISTINCT epoch_no) FROM drep_distr)"
chk "anchors were fetched"    "(SELECT count(*) FROM off_chain_vote_data) > 0"
chk "at tip (<300s)"          "EXTRACT(EPOCH FROM (now() AT TIME ZONE 'UTC' - (SELECT time FROM block ORDER BY id DESC LIMIT 1))) < 300"
exit $fail
```

`rss-sample.sh` — peak RSS in **bytes**, sampled from the container's own cgroup, addressed by compose
service name so it does not depend on the compose project name:

```bash
#!/usr/bin/env bash
# Append db-sync's memory use (RAW BYTES) every 60s for the life of the experiment. Run with nohup.
set -uo pipefail
while true; do
  bytes=$(docker compose exec -T cardano-db-sync sh -c \
    'cat /sys/fs/cgroup/memory.current 2>/dev/null || cat /sys/fs/cgroup/memory/memory.usage_in_bytes' \
    2>/dev/null | tr -d '\r') || bytes=
  printf '%s,%s\n' "$(date -u +%FT%TZ)" "${bytes:-0}" >> rss.csv
  sleep 60
done
```

- [ ] **Step 2: Run test to verify it fails**

Run:
```bash
cd /Users/mg/Projects/CardanoProjects/palmares/experiments/dbsync-genesis && ./measure.sh
```
Expected: FAIL — `psql: error: connection to server ... failed` (no stack is up yet).

- [ ] **Step 3: Assemble the experiment's configs, pre-check the host, and launch**

`node-configs/` is the **cluster's own** `/configs` directory, reassembled exactly as the preprod
StatefulSet's `fetch-configs` initContainer builds it
(`statefulset-cardano-preprod.yaml:30-71`), so the experiment also answers T-INFRA §8's open question.
Both halves are required: `config.json` references the four genesis files **by bare filename**
(T-INFRA §1.3), so db-sync cannot start if they are not in the same directory — and `config.json`
carries their hashes, so a mismatched download is fail-closed, not silent.

```bash
mkdir -p node-configs
yq '.data."config.json"' -r \
  /Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod/config/configmap-cardano-preprod-node-config.yaml \
  > node-configs/config.json   # carries "LedgerDB": {"Backend": "V2LSM", "LSMDatabasePath": "/data/lsm", ...}
BASE="https://book.play.dev.cardano.org/environments/preprod"
for f in byron-genesis.json shelley-genesis.json alonzo-genesis.json conway-genesis.json; do
  curl -fsSL "$BASE/$f" -o "node-configs/$f"
done
```

`db-sync-config.yaml`: download db-sync's shipped `config/preprod-config.yaml` **at tag 13.7.1.0**
verbatim (it carries the logging `Representation` block that db-sync refuses to start without), then make
exactly three edits — `NodeConfigFile: /node-configs/config.json`, add the top-level `ipfs_gateway` list,
and replace `insert_options` with the four keys of D-5:

```yaml
ipfs_gateway:
  - "https://ipfs.io/ipfs"
  - "https://cloudflare-ipfs.com/ipfs"

insert_options:
  preset: "only_governance"
  offchain_vote_data: "enable"
  shelley:
    enable: true
```

No `ledger_backend` (default `inmemory`; `lsm` is D-13 fallback 2 only). No `disable_epoch` (the key does
not exist at 13.7.1.0). No `RequiresNetworkMagic` (not a db-sync key). No `remove_jsonb_from_schema`
(already the default). No restated preset keys.

`compose.yaml`: T-INFRA §4.1's `chain` profile verbatim, including `name: palmares`, with
`cardano-node` pinned to the cluster's exact digest
`ghcr.io/blinklabs-io/cardano-node:11.1.2@sha256:3620624c4cb9ac4e750b6c5c2d674c478010f4a193fdf7418f4930820562c11d`
(`MITHRIL_DOWNLOAD: "true"`, aggregator
`https://aggregator.release-preprod.api.mithril.network/aggregator`, genesis verification key from
T-INFRA §3) + `postgres:17-alpine` + `cardano-db-sync` pinned to
`ghcr.io/blinklabs-io/cardano-db-sync:13.7.1.0@sha256:2a341eb32f15a3e65435f4123c3186f467bf2ed9d3ea2eac449592b5a54eb950`,
with `./node-configs:/node-configs:ro` replacing T-INFRA's `node-db:/node-db:ro` (we test the
cluster's config, not the image's). Four fields where a wrong value silently voids the run:

| Field | Value | Why |
|---|---|---|
| `environment.NETWORK` | `""` — present **and** empty | the entrypoint runs `set -u` and reads `${NETWORK}` with no default; unset is an unbound-variable abort, empty selects the "use my `--config`" branch |
| `working_dir` | `/var/lib/cexplorer` | the image sets no `WORKDIR`, so with CWD `/` the `.restored` marker and any dump land on the container layer |
| `POSTGRES_USER` | `dbsync` | the bootstrap role is db-sync's own role (synthesis §1.1); it is also the superuser, so a future `RESTORE_SNAPSHOT` still passes `check_psql_superuser` |
| `command` | `["--config","/dbsync-config/db-sync-config.yaml","--socket-path","/node-ipc/node.socket"]` | args are appended verbatim to the entrypoint's `exec` |

Pre-check and launch:
```bash
docker info --format '{{.MemTotal}}'     # must be >= 12884901888 (12 GiB); raise Docker Desktop if not
df -g .                                  # must show >= 120 GB free
docker compose up -d
nohup ./rss-sample.sh >/dev/null 2>&1 &
date -u +%FT%TZ > started_at
docker compose logs -f cardano-db-sync   # the V2LSM parse verdict appears in the first 30 seconds
```

- [ ] **Step 4: Run test to verify it passes**

Run, at t+1 h (sanity), then daily, then once the log reports the tip:
```bash
cd /Users/mg/Projects/CardanoProjects/palmares/experiments/dbsync-genesis && ./measure.sh >> RESULTS.md
```
Expected: PASS — exit 0, all six `chk` lines silent.

Pass criteria, and what each measurement decides:

| Measurement | Pass | Decides |
|---|---|---|
| wall-clock to tip (`started_at` → first passing run) | ≤ 96 h | the duration band T-SUBSTRATE-5 quotes. Expected 24–48 h, floor ~8 h. > 96 h on this host ⇒ D-13 fallback 3 (db-sync stays off-cluster) |
| `PG_TOTAL_BYTES` | ≤ 25 GB | **the PVC size.** ≤ 25 GB ⇒ 50 Gi stands. 25–40 GB ⇒ 50 Gi stands, schedule D-13 fallback 1 (`DELETE FROM epoch_stake WHERE epoch_no < tip-2`, only after the `validateEpochStake` test exists). > 40 GB ⇒ edit the PVC manifest **before** T-SUBSTRATE-5; `local-path` has `allowVolumeExpansion: false` |
| `PEAK_RSS_BYTES` | ≤ 6 GiB | the db-sync Deployment's `limits.memory`. 6–9 GiB ⇒ raise the limit and re-check pi5-1's 9.51 Gi real headroom. > 9 GiB ⇒ D-13 fallback 2 (`ledger_backend: lsm`, accepting the Raspberry-Pi thread crash and the >4096-item `blockio-uring` bug), then fallback 3 |
| `epoch_sync_time` per epoch | recorded | where the time goes; the per-epoch curve is what bounds the Pi by the single-core ratio (~3–5×) instead of a 2× guess |
| `stake_deregistration`, `pool_update` counts | both > 0 | **the `shelley: enable: true` flip took.** Zero on either ⇒ the config is wrong and the run is void; a flip after a cluster sync costs a full re-sync. `param_proposal` is **not** a valid probe — it fills regardless of this flag |
| `drep_distr`: `no_gap` true, `max(epoch_no) >= tip-1` | both | F1 check 1's `drep_distr` assertion, and that `ledger: enable` survived the preset |
| `off_chain_vote_data` / `off_chain_vote_fetch_error` counts | recorded | the anchor-coverage figure T-API §8.5 defers; a `not_fetched` share > ~20 % at 48 h after completion is the trigger for the deferred supplementary re-fetcher (measure in F1, build in F2) |
| db-sync's first 30 s of log against `node-configs/config.json` | no `LedgerDB` parse error | T-INFRA §8's open question. On failure: vendor a db-sync-only `config.json` with the `LedgerDB` block removed, keeping the genesis hashes, and carry it into T-SUBSTRATE-4's ConfigMap |

- [ ] **Step 5: Commit**

```bash
git add experiments/dbsync-genesis
git commit -m "chore(experiment): measured from-genesis preprod db-sync run (Experiment A)"
```

---

### Task T-SUBSTRATE-2: publish the preprod node's IPC socket and config dir on hostPath

⚠️ **This task edits a live cluster workload and restarts the preprod `cardano-node` pod, taking
`ogmios` and the `hydra-node` sidecar with it. It needs the owner's go-ahead and nothing else in this
block depends on being done first — stop here if the approval is not in hand.** Expect
seconds-to-minutes plus a `V2LSM` snapshot reload; the "~26 s" in `apps-k8s-configs/CLAUDE.md` is a
post-conversion catch-up, not a restart measurement.

**Files:**
- Modify: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod/workloads/statefulset-cardano-preprod.yaml:26,27,236-240`
- Test: `/Users/mg/Projects/CardanoProjects/palmares/scripts/check_node_hostpath.sh`

**Interfaces:**
- Consumes: the `palmares` repo skeleton from Plan A's Phase-1 bootstrap task (for `scripts/`).
- Produces: `scripts/check_node_hostpath.sh`; hostPaths `/var/lib/cardano-preprod/ipc` (carrying `node.socket`) and
  `/var/lib/cardano-preprod/configs` (carrying `config.json`, `topology.json`, `peer-snapshot.json`
  and the four genesis files, assembled by the existing `fetch-configs` initContainer) on
  `minirack-pi5-1`.

- [ ] **Step 1: Write the failing test**

```bash
#!/usr/bin/env bash
# scripts/check_node_hostpath.sh — run from any host with the minirack kubeconfig.
set -euo pipefail
kubectl --context minirack-lan -n cardano-preprod debug node/minirack-pi5-1 \
  -it --image=busybox:1.36 -- ls -la /host/var/lib/cardano-preprod/ipc/node.socket \
                                      /host/var/lib/cardano-preprod/configs/config.json \
                                      /host/var/lib/cardano-preprod/configs/conway-genesis.json
```

- [ ] **Step 2: Run test to verify it fails**

Run: `./scripts/check_node_hostpath.sh`
Expected: FAIL with `ls: /host/var/lib/cardano-preprod/ipc/node.socket: No such file or directory`
(today both volumes are `emptyDir: {}`).

- [ ] **Step 3: Convert the two volumes to hostPath and bump the stamp**

At lines 236–240, replace `emptyDir: {}` on `configs` and `ipc` with
`hostPath: {path: /var/lib/cardano-preprod/{configs,ipc}, type: DirectoryOrCreate}`; add
`nodeSelector: {kubernetes.io/hostname: minirack-pi5-1}` at the pod `spec:` (line 27) so the socket path
can never be orphaned; and bump `cardano.io/node-config-stamp` (line 26) from `"2026-09-20-1"` in the
**same commit** — the repo rule. Both volumes, not one: db-sync needs the socket **and** `config.json`
plus the four genesis files, which `config.json` references by bare filename.

Do **not** touch the stale comment at `statefulset-cardano-preprod.yaml:80` ("Rămâne V2InMemory —
trecerea la V2LSM e un task separat, ulterior.") — the committed ConfigMap really is `V2LSM`
(`configmap-cardano-preprod-node-config.yaml:73-74`) and fixing that comment is not this commit.

- [ ] **Step 4: Run test to verify it passes**

```bash
cd /Users/mg/Projects/apps-k8s-configs
kustomize build --load-restrictor LoadRestrictionsNone ./chains/cardano/preprod | kubeconform -ignore-missing-schemas -summary -
git push          # Flux root cardano-preprod, interval 10m0s
flux --context minirack-lan reconcile kustomization cardano-preprod --with-source
kubectl --context minirack-lan -n cardano-preprod rollout status statefulset/statefulset-cardano-preprod --timeout=10m
/Users/mg/Projects/CardanoProjects/palmares/scripts/check_node_hostpath.sh
```
Expected: PASS — `rollout status` reports `statefulset rolling update complete 1 pods at revision …`
and all three paths list. Then confirm the dependants survived:
`kubectl -n cardano-preprod get pods -l app=kupo-preprod` Ready, and ogmios answering on its Service.

- [ ] **Step 5: Commit**

```bash
# in apps-k8s-configs
git add chains/cardano/preprod/workloads/statefulset-cardano-preprod.yaml
git commit -m "feat(cardano-preprod): publish node ipc + configs on hostPath for db-sync"
# in palmares
git add scripts/check_node_hostpath.sh
git commit -m "feat(ops): assert the preprod node's ipc + configs hostPaths"
```

---

### Task T-SUBSTRATE-3: Postgres 17 + 50 Gi PVC, pinned to minirack-pi5-1

⚠️ **This task creates new workloads on the live cluster** (a new Flux root in namespace
`cardano-preprod`). It restarts nothing that exists.

**Files:**
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/kustomization.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/config/configmap-palmares-dbsync-env.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/secrets/secret-palmares-dbsync.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/storage/pvc-palmares-dbsync-pg.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/storage/pvc-palmares-dbsync-state.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/services/service-palmares-postgres.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/workloads/deployment-palmares-postgres.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/policies/networkpolicy-palmares-postgres.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/clusters/minirack/chains/cardano-preprod-dbsync.yaml`
- Modify: `/Users/mg/Projects/apps-k8s-configs/clusters/minirack/chains/kustomization.yaml`
- Test: `/Users/mg/Projects/CardanoProjects/palmares/scripts/check_postgres.sh`

**Interfaces:**
- Consumes: T-SUBSTRATE-2's hostPaths (not yet, but the root is shaped for them).
- Produces: `scripts/check_postgres.sh`; `service-palmares-postgres.cardano-preprod.svc.cluster.local:5432`, database `cexplorer`,
  bootstrap superuser role `dbsync`; ConfigMap `configmap-palmares-dbsync-env` with
  `POSTGRES_HOST`/`POSTGRES_PORT`/`POSTGRES_DB`/`POSTGRES_USER`/`PGDATA`; Secret
  `secret-palmares-dbsync` with `POSTGRES_PASSWORD`; PVCs `pvc-palmares-dbsync-pg` (50 Gi) and
  `pvc-palmares-dbsync-state` (10 Gi); Flux Kustomization `cardano-preprod-dbsync`.

- [ ] **Step 1: Write the failing test**

```bash
#!/usr/bin/env bash
# scripts/check_postgres.sh
set -euo pipefail
K="kubectl --context minirack-lan -n cardano-preprod"
$K get pvc pvc-palmares-dbsync-pg -o jsonpath='{.spec.resources.requests.storage}' | grep -qx 50Gi
$K get pod -l app=palmares-postgres-pod-label -o jsonpath='{.items[0].spec.nodeSelector}' \
  | grep -q minirack-pi5-1
$K exec deploy/deployment-palmares-postgres -- \
  psql -U dbsync -d cexplorer -At -c "SELECT current_user, rolsuper FROM pg_roles WHERE rolname=current_user;" \
  | grep -qx 'dbsync|t'
```

- [ ] **Step 2: Run test to verify it fails**

Run: `./scripts/check_postgres.sh`
Expected: FAIL with `Error from server (NotFound): persistentvolumeclaims "pvc-palmares-dbsync-pg" not found`.

- [ ] **Step 3: Write the manifests**

Copy T-INFRA §2.3's `configmap-palmares-dbsync-env` and §2.4 / §2.5 / §2.9 verbatim, with these
corrections applied:

- **`POSTGRES_USER: "dbsync"`**, not `postgres` (synthesis §1.1 — db-sync's own role owns `public`; the
  bare-`postgres` form is an alias to delete). Consequences, all in this task: both `pg_isready` probes
  become `["pg_isready","-U","dbsync","-d","cexplorer"]`; the image makes `dbsync` the bootstrap
  **superuser** so `postgresql-setup.sh`'s `check_psql_superuser` still passes for a future
  `RESTORE_SNAPSHOT`; and `ALTER DEFAULT PRIVILEGES FOR ROLE dbsync` in T-SUBSTRATE-8 names a real role.
- The ConfigMap's own comment says "our projections live in schema `registry`" — write **`palmares`**
  (synthesis §1.1; `registry` is an alias to delete).
- `nodeSelector: {kubernetes.io/hostname: minirack-pi5-1}` on the Postgres pod — same node as db-sync,
  which is the hottest link in the stack and closes T-DATA's cross-node write-path question.
- Both PVCs carry `kustomize.toolkit.fluxcd.io/prune: disabled`: `local-path` is
  `reclaimPolicy: Delete`, so deleting the file from Git would destroy a multi-day sync.
- **50 Gi, sized once.** `local-path` has `allowVolumeExpansion: false`. Estimate 10–18 GB with D-4's
  flip; T-SUBSTRATE-1 either confirms it or forces an edit here before T-SUBSTRATE-5.
- Postgres args as T-INFRA §2.4 (`shared_buffers=768MB`, `work_mem=32MB`,
  `maintenance_work_mem=512MB`, `max_parallel_maintenance_workers=2`, `max_wal_size=4GB`,
  `checkpoint_timeout=30min`, `wal_compression=on`, `synchronous_commit=off` with its `ponytail:`
  comment naming the ceiling).
- `secret-palmares-dbsync` is SOPS-encrypted. `.sops.yaml`'s second rule already matches
  `^chains/.*/secrets/.*\.ya?ml$` — **no `.sops.yaml` change**.
- The NetworkPolicy's `namespaceSelector` names `kubernetes.io/metadata.name: palmares`; that namespace
  does not exist yet and the selector correctly matches nothing until the app block creates it.

Flux root `clusters/minirack/chains/cardano-preprod-dbsync.yaml` — a sibling of `cardano-preprod.yaml`
(D-26), `path: ./chains/cardano/preprod-dbsync`, `prune: true`, SOPS decryption,
`dependsOn: [infrastructure, cardano-preprod]`, and **`wait: true` with `timeout: 1m0s`** — a short
timeout on purpose: readiness is reported honestly but a Flux worker is never held for the length of a
multi-day sync (the 2026-09-23 audit lesson: four default workers, all blocked). Add the file to
`clusters/minirack/chains/kustomization.yaml`.

- [ ] **Step 4: Run test to verify it passes**

```bash
cd /Users/mg/Projects/apps-k8s-configs
kustomize build --load-restrictor LoadRestrictionsNone ./chains/cardano/preprod-dbsync | kubeconform -ignore-missing-schemas -summary -
grep -rlE '^kind: Secret' chains/cardano/preprod-dbsync | xargs -r grep -L 'ENC\[AES256_GCM'   # must print nothing
git push && flux --context minirack-lan reconcile kustomization cardano-preprod-dbsync --with-source
/Users/mg/Projects/CardanoProjects/palmares/scripts/check_postgres.sh
```
Expected: PASS — `kubeconform` summary with 0 invalid, the plaintext-Secret grep silent, and all three
assertions in `check_postgres.sh` succeeding.

- [ ] **Step 5: Commit**

```bash
# in apps-k8s-configs
git add chains/cardano/preprod-dbsync clusters/minirack/chains/cardano-preprod-dbsync.yaml clusters/minirack/chains/kustomization.yaml
git commit -m "feat(palmares): Postgres 17 + 50Gi PVC for the preprod db-sync substrate"
# in palmares
git add scripts/check_postgres.sh
git commit -m "feat(ops): assert the substrate Postgres PVC, placement and dbsync role"
```

---

### Task T-SUBSTRATE-4: the db-sync Deployment

⚠️ **This task creates a new workload on the live cluster.** It restarts nothing that exists; it starts
the from-genesis sync, which T-SUBSTRATE-5 then measures. The config is frozen by that sync: a `shelley`
or `preset` correction after this point costs a full re-sync.

**Files:**
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/config/configmap-palmares-dbsync-config.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/workloads/deployment-palmares-dbsync.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/services/service-palmares-dbsync-metrics.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/monitoring/servicemonitor-palmares-dbsync.yaml`
- Modify: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/kustomization.yaml`
- Test: `/Users/mg/Projects/CardanoProjects/palmares/scripts/check_dbsync.sh`

**Interfaces:**
- Consumes: T-SUBSTRATE-2's hostPaths; T-SUBSTRATE-3's `configmap-palmares-dbsync-env`,
  `secret-palmares-dbsync`, `pvc-palmares-dbsync-state`, `service-palmares-postgres`;
  T-SUBSTRATE-1's `db-sync-config.yaml` (the same source file).
- Produces: `scripts/check_dbsync.sh`; `deployment-palmares-dbsync` writing `public.*` in `cexplorer`;
  `service-palmares-dbsync-metrics:8080` scraped on `path: /`; the six
  `cardano_db_sync_*` gauges.

- [ ] **Step 1: Write the failing test**

```bash
#!/usr/bin/env bash
# scripts/check_dbsync.sh
set -euo pipefail
K="kubectl --context minirack-lan -n cardano-preprod"
# 1. the four-key insert_options reached the pod, unexpanded and unaugmented
$K exec deploy/deployment-palmares-dbsync -- cat /dbsync-config/db-sync-config.yaml \
  | yq -o=json '.insert_options' | jq -e '
      . == {"preset":"only_governance","offchain_vote_data":"enable","shelley":{"enable":true}}'
# 2. NETWORK present and EMPTY (unset is an unbound-variable abort under set -u)
$K get deploy deployment-palmares-dbsync \
  -o jsonpath='{.spec.template.spec.containers[0].env[?(@.name=="NETWORK")].value}{"\n"}' | grep -qx ''
# 3. workingDir on the PVC, not the container layer
$K get deploy deployment-palmares-dbsync -o jsonpath='{.spec.template.spec.containers[0].workingDir}' \
  | grep -qx /var/lib/cexplorer
# 4. metrics answer on "/" — the probe path that cascades if wrong. If the image ships neither
#    wget nor curl, run the same GET from a throwaway pod:
#    $K run m --rm -i --image=busybox:1.36 --restart=Never -- \
#      wget -qO- http://service-palmares-dbsync-metrics:8080/
$K exec deploy/deployment-palmares-dbsync -- \
  sh -c 'wget -qO- http://127.0.0.1:8080/ | head -1' | grep -q cardano_db_sync_
# 5. the ServiceMonitor and the Service that Prometheus scrapes agree on that path and have an endpoint
$K get servicemonitor servicemonitor-palmares-dbsync -o jsonpath='{.spec.endpoints[0].path}' | grep -qx /
$K get endpoints service-palmares-dbsync-metrics -o jsonpath='{.subsets[0].addresses[0].ip}' | grep -qE '^[0-9]'
# 6. it is actually inserting
$K exec deploy/deployment-palmares-postgres -- psql -U dbsync -d cexplorer -At \
  -c "SELECT count(*) > 0 FROM block;" | grep -qx t
```

- [ ] **Step 2: Run test to verify it fails**

Run: `./scripts/check_dbsync.sh`
Expected: FAIL with `Error from server (NotFound): deployments.apps "deployment-palmares-dbsync" not found`.

- [ ] **Step 3: Write the ConfigMap and the Deployment**

`configmap-palmares-dbsync-config.yaml` carries one key, `db-sync-config.yaml`, whose content is **the
same file T-SUBSTRATE-1 built** — db-sync's shipped `config/preprod-config.yaml` at tag **13.7.1.0**
verbatim, with `NodeConfigFile: /node-configs/config.json`, the top-level two-entry `ipfs_gateway`, and
the four-key `insert_options` of D-5. Keep one source file and generate the compose and ConfigMap
variants from it with a one-line `sed` on `NodeConfigFile` — never two hand-maintained copies. Bump
`palmares.io/dbsync-config-stamp` on the Deployment in the **same commit** as any edit to this
ConfigMap; it does not restart the pod on its own.

Deployment: T-INFRA §2.3 verbatim. The five fields where a wrong value silently breaks the sync, spelled
out because none of them fails loudly:

| Field | Value | What goes wrong otherwise |
|---|---|---|
| `env.NETWORK` | present, `value: ""` | the entrypoint runs `set -euo pipefail` then `[[ -z ${NETWORK} ]]`; **unset** is an unbound-variable abort |
| `workingDir` | `/var/lib/cexplorer` | the image sets no `WORKDIR`; with CWD `/` a `RESTORE_SNAPSHOT` download and its `.restored` marker land on the container's writable layer — re-downloaded on every restart, charged to ephemeral storage |
| `insert_options` | exactly the four keys | no `preset` ⇒ base is `def` ⇒ `tx_metadata` **and** `reward` come back on (and `sioRewards` is `pure (sioRewards baseOptions)`, i.e. unsettable from JSON, so only a preset can disable rewards). A restated preset key that later drifts is itself a silent misconfiguration |
| `insert_options.shelley.enable` | `true` | without it `stake_deregistration`/`delegation`/`pool_update` are empty, F1 check 12's churn and retention become an upper bound, and flipping it later is a **full re-sync** |
| probe + ServiceMonitor `path` | `/`, not `/metrics` | db-sync calls `serveMetricsT port []`, so only `/` answers 200. Wrong ⇒ the pod is never Ready ⇒ the `cardano-preprod-dbsync` Flux root (`wait: true`) stays `Ready=False` ⇒ the app root that `dependsOn` it never reconciles at all |

Also: **no startup and no liveness probe** — the first sync runs for days and nothing may kill it;
readiness only, `initialDelaySeconds: 10` (not 60) to fit the root's `timeout: 1m0s`.
`strategy: Recreate` (one writer, ever). `terminationGracePeriodSeconds: 120`. `/configuration` is an
`emptyDir` because `gen-pgpass` writes `pgpass` there. `RESTORE_SNAPSHOT` stays **unset** for the first
sync. `ledger_backend` stays absent. Requests `750m`/`4Gi`, limits `2`/`6Gi` — revised by
T-SUBSTRATE-1's `PEAK_RSS_BYTES` if that lands above 6 GiB.

If T-SUBSTRATE-1 reported a `LedgerDB` parse failure, add the vendored db-sync-only `config.json` (that
block removed, genesis hashes kept) to this ConfigMap and repoint `NodeConfigFile` at it; the genesis
files still come from the shared hostPath, so fail-closed hash verification is preserved.

The ServiceMonitor needs no extra label — `kube-prometheus-stack` runs `serviceMonitorSelector: {}` on
this cluster.

- [ ] **Step 4: Run test to verify it passes**

```bash
cd /Users/mg/Projects/apps-k8s-configs
kustomize build --load-restrictor LoadRestrictionsNone ./chains/cardano/preprod-dbsync | kubeconform -ignore-missing-schemas -summary -
git push && flux --context minirack-lan reconcile kustomization cardano-preprod-dbsync --with-source
kubectl --context minirack-lan -n cardano-preprod rollout status deploy/deployment-palmares-dbsync --timeout=5m
/Users/mg/Projects/CardanoProjects/palmares/scripts/check_dbsync.sh
```
Expected: PASS — all six assertions, and
`kubectl -n cardano-preprod logs deploy/deployment-palmares-dbsync | head -40` shows no `LedgerDB`
parse error and an `Starting epoch` / `Insert Byron Block` progression.

- [ ] **Step 5: Commit**

```bash
# in apps-k8s-configs
git add chains/cardano/preprod-dbsync
git commit -m "feat(palmares): cardano-db-sync 13.7.1.0 preprod Deployment (only_governance + shelley)"
# in palmares
git add scripts/check_dbsync.sh
git commit -m "feat(ops): assert db-sync's five silent-failure fields and its metrics path"
```

---

### Task T-SUBSTRATE-5: the from-genesis preprod sync — F1 check 1

⚠️ **This task observes the live cluster for days.** It changes nothing until its last step, which adds
one PrometheusRule. Its deliverable is the signed F1 check 1.

> **ASSEMBLY CORRECTION 2** (2026-10-19, `f1-yaci-store-evaluation.md` §4.1 C2). "From genesis" is a
> **correctness** requirement, not merely a completeness one, and the task must say so: db-sync issue
> #1975 (open since 2025-05) records two *identical* mainnet instances disagreeing on one DRep's
> epoch-538 voting power by ~740×, and the 13.6.0.4 Known Issue names **preprod** explicitly —
> pre-upgrade `drep_distribution` values "are not well defined and could be slightly different between
> different executions". The documented remedy is deleting the ledger-state files and letting the ledger
> rules recompute, which is exactly what this sync does. **Consequence: a Postgres snapshot restore or an
> in-place db-sync upgrade is NOT an acceptable shortcut for this task** — it would inherit ill-defined
> values in the one column the whole product rests on. Mithril remains fine: it bootstraps the *node*,
> not db-sync's ledger state. Record this rationale in `docs/deployment/preprod-dbsync-sync.md` so a
> future operator does not "optimize" the sync away.

**Files:**
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/monitoring/prometheusrule-palmares-dbsync.yaml`
- Modify: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/kustomization.yaml`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/docs/deployment/preprod-dbsync-sync.md`
- Test: `/Users/mg/Projects/CardanoProjects/palmares/scripts/check_dbsync_tip.sh`

**Interfaces:**
- Consumes: T-SUBSTRATE-4's running `deployment-palmares-dbsync` and its
  `servicemonitor-palmares-dbsync`; T-SUBSTRATE-1's `WALLCLOCK_TO_TIP_HOURS`.
- Produces: a `public.*` schema at tip; `scripts/check_dbsync_tip.sh` (reused by the runbook, by
  T-SUBSTRATE-6's precondition and by `scripts/measure_substrate.sh`'s precondition); the recorded
  wall-clock number that replaces the 24–48 h band; PrometheusRule `prometheusrule-palmares-dbsync`
  with alert `PalmaresDbSyncLagging`.

- [ ] **Step 1: Write the failing test**

`scripts/check_dbsync_tip.sh` — F1 check 1's four assertions, as raw SQL. `/status` does not exist yet
(it is the read-path block's `GET /status`, Phase 3 step 18); these are the same quantities, read
directly, and the read-path block re-asserts them through the endpoint for check 16's API half.

```bash
#!/usr/bin/env bash
# F1 check 1. Exit 0 only when db-sync is at tip with the governance data F1 needs.
# PSQL may be overridden for compose: PSQL="docker compose exec -T postgres psql -U dbsync -d cexplorer"
set -euo pipefail
: "${PSQL:=kubectl --context minirack-lan -n cardano-preprod exec -i deploy/deployment-palmares-postgres -- psql -U dbsync -d cexplorer}"
q() { $PSQL -At -c "$1"; }

printf 'lag_seconds        %s\n' "$(q "SELECT EXTRACT(EPOCH FROM (now() AT TIME ZONE 'UTC' - (SELECT time FROM block ORDER BY id DESC LIMIT 1)))::bigint;")"
printf 'tip_epoch          %s\n' "$(q 'SELECT max(epoch_no) FROM block;')"
printf 'drep_distr_epochs  %s\n' "$(q 'SELECT min(epoch_no)||".."||max(epoch_no)||" n="||count(DISTINCT epoch_no) FROM drep_distr;')"
printf 'stake_deregistration %s\n' "$(q 'SELECT count(*) FROM stake_deregistration;')"
printf 'pool_update          %s\n' "$(q 'SELECT count(*) FROM pool_update;')"

fail=0
chk() { [ "$(q "SELECT $2;")" = t ] || { echo "FAIL: $1"; fail=1; }; }
# A1: < 120 s sustained. Asserted from this value over a soak window, NOT from the /ready probe
# (which is red at 300 s, amber at 120 s in the UI — D-7).
chk "lag < 120s" "EXTRACT(EPOCH FROM (now() AT TIME ZONE 'UTC' - (SELECT time FROM block ORDER BY id DESC LIMIT 1))) < 120"
chk "drep_distr current"         "(SELECT max(epoch_no) FROM drep_distr) >= (SELECT max(epoch_no) FROM block) - 1"
chk "drep_distr has no gaps"     "(SELECT (max(epoch_no)-min(epoch_no)+1) = count(DISTINCT epoch_no) FROM drep_distr)"
chk "stake_deregistration non-empty" "(SELECT count(*) FROM stake_deregistration) > 0"
chk "pool_update non-empty"          "(SELECT count(*) FROM pool_update) > 0"
chk "network is preprod"             "(SELECT network_name = 'preprod' FROM meta LIMIT 1)"
exit $fail
```

- [ ] **Step 2: Run test to verify it fails**

Run: `./scripts/check_dbsync_tip.sh`
Expected: FAIL — `FAIL: lag < 120s` plus `FAIL: drep_distr current` while the sync is mid-chain
(`lag_seconds` prints a number in the millions on the first day).

- [ ] **Step 3: Observe the sync to completion, then write the lag alert**

Progress is observed from db-sync's own records, not from log tailing:
```bash
# the authoritative progress curve
kubectl --context minirack-lan -n cardano-preprod exec -i deploy/deployment-palmares-postgres -- \
  psql -U dbsync -d cexplorer -c "SELECT no, seconds, state FROM epoch_sync_time ORDER BY no DESC LIMIT 10;"
# the two gauges the alert will use
kubectl --context minirack-lan -n cardano-preprod exec deploy/deployment-palmares-dbsync -- \
  sh -c 'wget -qO- http://127.0.0.1:8080/ | grep -E "cardano_db_sync_(node|db)_block_height"'
```

Expected duration: **24–48 h, floor ~8 h** — a derived band, no published figure exists for any preset on
any architecture. **T-SUBSTRATE-1 replaces this band with a measured number**; record the cluster's own
wall-clock in `docs/deployment/preprod-dbsync-sync.md` together with the `epoch_sync_time` dump, and
state the Pi-vs-Mac ratio that fell out.

Write the one alert worth having, **only now** — during the sync it would fire continuously and teach
everyone to ignore it:

```yaml
# monitoring/prometheusrule-palmares-dbsync.yaml — the one rule, in one group; add the file to
# chains/cardano/preprod-dbsync/kustomization.yaml in the same commit or Flux never applies it.
alert: PalmaresDbSyncLagging
expr: cardano_db_sync_node_block_height - cardano_db_sync_db_block_height > 20
for: 15m
labels:
  severity: warning
annotations:
  summary: >-
    db-sync is {{ $value }} blocks behind the preprod node for 15 min — /ready is red past 300 s
    and every palmares.* read is stale.
```

House conventions, from `infrastructure/configs/monitoring/prometheusrule-node-thermal.yaml`:
`severity` label + a `summary` annotation and **no `description`** (the Telegram receiver renders only
`summary`). `namespace: cardano-preprod` is fine — the chart sets `ruleSelector: {}` with no namespace
restriction (`helmrelease-kube-prometheus-stack.yaml:88-91`).

`cardano_db_sync_node_slot_height` **does not exist** — db-sync exports exactly six gauges and an alert
on that name would produce an empty series and never fire. The `> 20` threshold is the 300 s `/ready`
red line expressed in blocks at preprod's measured ~22.7 s mean block interval (300/22.7 ≈ 13; 20 leaves
margin for a legitimate burst).

The runbook note that belongs in `docs/deployment/preprod-dbsync-sync.md`: **Mithril does not speed up
db-sync.** It restores the *node's* chain database; db-sync still streams every block from genesis and
maintains its own ledger state, and no preprod Postgres snapshot is published
(`update-cardano-preprod.iohk.io` does not resolve). This is the single most likely thing for a
contributor to get wrong.

- [ ] **Step 4: Run test to verify it passes**

```bash
./scripts/check_dbsync_tip.sh                    # must exit 0
# ~1 h soak: 12 samples, 5 min apart
for i in $(seq 1 12); do ./scripts/check_dbsync_tip.sh | grep ^lag_seconds; sleep 300; done
```
Expected: PASS — exit 0, and all 12 `lag_seconds` samples < 120, which is how spec check A1 is signed.
Then the alert, gated on Prometheus actually having the two series (an alert over an empty series never
fires, which is the failure mode this task exists to avoid):
```bash
flux --context minirack-lan reconcile kustomization cardano-preprod-dbsync --with-source
kubectl --context minirack-lan -n cardano-preprod get prometheusrule prometheusrule-palmares-dbsync
kubectl --context minirack-lan -n monitoring port-forward svc/kube-prometheus-stack-prometheus 9090:9090 &
sleep 3
curl -sG --data-urlencode \
  'query=cardano_db_sync_node_block_height - cardano_db_sync_db_block_height' \
  http://127.0.0.1:9090/api/v1/query | jq -e '.data.result | length == 1'
curl -s 'http://127.0.0.1:9090/api/v1/rules' \
  | jq -e '[.data.groups[].rules[] | select(.name=="PalmaresDbSyncLagging")] | length == 1'
kill %1
```
Expected: PASS — the `get prometheusrule` prints the object, the instant query returns exactly one
sample (not an empty `result`), and `PalmaresDbSyncLagging` is loaded in Prometheus's rule set.

- [ ] **Step 5: Commit**

```bash
# in apps-k8s-configs
git add chains/cardano/preprod-dbsync/monitoring/prometheusrule-palmares-dbsync.yaml chains/cardano/preprod-dbsync/kustomization.yaml
git commit -m "feat(palmares): db-sync block-height lag alert (post first sync)"
# in palmares
git add scripts/check_dbsync_tip.sh docs/deployment/preprod-dbsync-sync.md
git commit -m "feat(ops): F1 check 1 tip assertions + measured preprod sync record"
```

---

### Task T-SUBSTRATE-6: `sql/040_indexes.sql` through the one-shot admin Job

**Cannot usefully run before T-SUBSTRATE-5.** db-sync's own `idx_drep_distr_hash_id_epoch_no` and
`idx_block_time` live in `migration-4-*` files, i.e. **stage 4**, which db-sync executes only once it is
within 30 minutes of the tip. Any query benchmarked before that is measuring a plan that will never run
in production, and `CREATE INDEX CONCURRENTLY` against db-sync's long bulk-insert transactions blocks or
fails. This task gates every later performance claim.

**Files:**
- Create: `/Users/mg/Projects/CardanoProjects/palmares/sql/040_indexes.sql`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/src/lib/offchain-lib/Palmares/Admin/ApplySql.hs`
- Modify: `/Users/mg/Projects/CardanoProjects/palmares/src/exe/admin/Main.hs`
- Modify: `/Users/mg/Projects/CardanoProjects/palmares/palmares.cabal`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/sql-jobs/job-palmares-apply-indexes.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/sql-jobs/kustomization.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/clusters/minirack/chains/cardano-preprod-dbsync-sql.yaml`
- Modify: `/Users/mg/Projects/apps-k8s-configs/clusters/minirack/chains/kustomization.yaml`
- Test: `/Users/mg/Projects/CardanoProjects/palmares/src/test/UnitTests/ApplySql.hs`

**Interfaces:**
- Consumes: T-SUBSTRATE-5's synced `public.*` and `scripts/check_dbsync_tip.sh`; T-SUBSTRATE-3's
  `configmap-palmares-dbsync-env` + `secret-palmares-dbsync` (the Job assembles its connection string
  from them).
- Produces: 22 indexes named `palmares_idx_*` on `public.*`; the `pg_trgm` extension;
  `admin apply-indexes`; env var `PALMARES_ADMIN_CONN_STR` (libpq keyword/value form, the `dbsync`
  superuser — **not** `PG_CONN_STR`, which T-SUBSTRATE-8 produces as the `palmares_ro` serving
  string); module `Palmares.Admin.ApplySql` in `offchain-lib` exposing
  `applySqlAutocommit :: ByteString -> ByteString -> IO ()` (connection string, then SQL),
  `indexesSql :: ByteString` and `assertIndexesValid :: Connection -> IO [Text]`; Flux Kustomization
  `cardano-preprod-dbsync-sql`.

- [ ] **Step 1: Write the failing test**

```haskell
-- src/test/UnitTests/ApplySql.hs
module UnitTests.ApplySql (tests) where

import Data.ByteString.Char8 qualified as BS8
import Palmares.Admin.ApplySql (indexesSql)
import Test.Tasty
import Test.Tasty.HUnit

-- isPrefixOf, never isInfixOf: the file's own header comment contains the words
-- "CREATE INDEX CONCURRENTLY", so an infix filter counts 23 statements and then asserts a
-- comment line is an index definition.
tests :: TestTree
tests = testGroup "sql/040_indexes.sql"
  [ testCase "pg_trgm is created before any index" $
      let ls = BS8.lines indexesSql
          ix s = length (takeWhile (not . BS8.isPrefixOf s) ls)
       in assertBool "CREATE EXTENSION precedes the first CREATE INDEX"
            (ix "CREATE EXTENSION IF NOT EXISTS pg_trgm" < ix "CREATE INDEX")
  , testCase "every index is CONCURRENTLY IF NOT EXISTS and palmares_idx_*" $
      let stmts = filter (BS8.isPrefixOf "CREATE INDEX") (BS8.lines indexesSql)
       in do length stmts @?= 22
             mapM_ (\s -> assertBool (BS8.unpack s)
                      (BS8.isInfixOf "CONCURRENTLY IF NOT EXISTS palmares_idx_" s)) stmts
  , testCase "no statement is wrapped in a transaction" $
      mapM_ (\kw -> assertBool (BS8.unpack kw) (not (BS8.isInfixOf kw indexesSql)))
            ["BEGIN", "COMMIT"]
  , testCase "the two trigram indexes that replace the cut projection tables are present" $
      mapM_ (\n -> assertBool (BS8.unpack n) (BS8.isInfixOf n indexesSql))
            [ "palmares_idx_ocvdd_given_name_trgm", "palmares_idx_ocvgad_title_trgm" ]
  ]
```

- [ ] **Step 2: Run test to verify it fails**

Run: `cabal test test --test-options='-p ApplySql'`
Expected: FAIL with `Could not find module 'Palmares.Admin.ApplySql'`. (The component is named `test`,
per T-EXTRACT's `palmares.cabal`.)

- [ ] **Step 3: Write `sql/040_indexes.sql`, then `applySqlAutocommit` + `assertIndexesValid` in `src/lib/offchain-lib/Palmares/Admin/ApplySql.hs`**

The SQL is an exact copy, so it is given in full. Every name is T-DATA §3.4's, re-prefixed `palmares_idx_*`
per synthesis §1.1 (`gov_idx_*` and the bare `idx_delegation_vote_drep`, which collided with upstream's
`idx_*`, are aliases to delete).

```sql
-- sql/040_indexes.sql — applied OUTSIDE any transaction by `admin apply-indexes`, connected as the
-- owner of db-sync's public tables (role `dbsync`). CREATE INDEX CONCURRENTLY cannot run inside a
-- transaction block, which is why this file is NOT part of query-api's boot set (sql/000-030).
-- Additive and migration-surviving (db-sync's migrations only add), but they must be re-applied
-- after any --rollback-to-slot + resync. Do NOT also use --schema-dir.
CREATE EXTENSION IF NOT EXISTS pg_trgm;

CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_voting_procedure_gap    ON public.voting_procedure (gov_action_proposal_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_voting_procedure_drep   ON public.voting_procedure (drep_voter) WHERE drep_voter IS NOT NULL;
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_voting_procedure_pool   ON public.voting_procedure (pool_voter) WHERE pool_voter IS NOT NULL;
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_voting_procedure_cc     ON public.voting_procedure (committee_voter) WHERE committee_voter IS NOT NULL;
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_voting_procedure_tx     ON public.voting_procedure (tx_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_drep_registration_drep  ON public.drep_registration (drep_hash_id, tx_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_delegation_vote_drep    ON public.delegation_vote (drep_hash_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_delegation_vote_addr    ON public.delegation_vote (addr_id, tx_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_gov_action_proposal_tx  ON public.gov_action_proposal (tx_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_gov_action_prev         ON public.gov_action_proposal (prev_gov_action_proposal) WHERE prev_gov_action_proposal IS NOT NULL;
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_ocvd_anchor             ON public.off_chain_vote_data (voting_anchor_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_ocvfe_anchor            ON public.off_chain_vote_fetch_error (voting_anchor_id, retry_count DESC);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_treasury_withdrawal_gap ON public.treasury_withdrawal (gov_action_proposal_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_committee_reg_cold      ON public.committee_registration (cold_key_id);
-- not in T-DATA §3.4: D-24 resolves a CC vote's COLD credential by LATERAL over committee_registration
-- keyed on hot_key_id, which the cold-only index cannot drive.
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_committee_reg_hot       ON public.committee_registration (hot_key_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_committee_dereg_cold    ON public.committee_de_registration (cold_key_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_stake_dereg_addr        ON public.stake_deregistration (addr_id, tx_id);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_reward_rest_type        ON public.reward_rest (type, spendable_epoch);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_epoch_state_epoch       ON public.epoch_state (epoch_no);
-- D-2 killed drep_distr as the eligibility source, but the per-epoch voting-power series still
-- filters on epoch_no ALONE, which upstream's (hash_id, epoch_no) index cannot drive.
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_drep_distr_epoch        ON public.drep_distr (epoch_no, hash_id);
-- D-1: these two replace the five cut projection tables' search text. GIN + gin_trgm_ops.
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_ocvdd_given_name_trgm   ON public.off_chain_vote_drep_data USING gin (given_name gin_trgm_ops);
CREATE INDEX CONCURRENTLY IF NOT EXISTS palmares_idx_ocvgad_title_trgm       ON public.off_chain_vote_gov_action_data USING gin (title gin_trgm_ops);
```

The module lives in **`offchain-lib`**, not in `src/exe/admin/`: GHC ties the module name
`Palmares.Admin.ApplySql` to the path under `hs-source-dirs`, and the `test` component
(`hs-source-dirs: src/test`, depending on the four libraries) cannot import an executable's
`other-modules`. So `palmares.cabal` gains `Palmares.Admin.ApplySql` to `offchain-lib`'s
`exposed-modules`, `file-embed` + `postgresql-simple` to its `build-depends`,
`UnitTests.ApplySql` to the `test` component's `other-modules`, and
`extra-source-files: sql/*.sql` at the top level. `src/exe/admin/Main.hs` only wires the subcommand.

It embeds the file with `makeRelativeToProject "sql/040_indexes.sql" >>= embedFile` (one
`embedFile` per `.sql`, never `embedDir`), splits on
`;` and executes each statement with `postgresql-simple`'s `execute_`, which is autocommit —
`persistent`'s `runSqlPool` would wrap them in a transaction and `CONCURRENTLY` would fail.
`postgresql-simple ==0.7.0.0` and `file-embed ==0.0.16.0` are already in `cabal.project.freeze`
(verified: lines 471 and 251), so these are `build-depends` lines, not pin moves.

The one piece the signature does not determine — a `CREATE INDEX CONCURRENTLY` that fails leaves an
**invalid** index behind, which `IF NOT EXISTS` then silently accepts on every later run, so the Job
would report success forever over an index Postgres will never use:

```haskell
-- | Index names in sql/040_indexes.sql that exist but are not usable. Non-empty ⇒ the caller dies.
assertIndexesValid :: Connection -> IO [Text]
assertIndexesValid conn = map fromOnly <$> query_ conn
  "SELECT c.relname::text FROM pg_class c JOIN pg_index i ON i.indexrelid = c.oid \
  \ WHERE c.relname LIKE 'palmares\\_idx\\_%' AND NOT i.indisvalid"
```

`admin apply-indexes` = read `PALMARES_ADMIN_CONN_STR` → `applySqlAutocommit conn indexesSql` →
`assertIndexesValid` → `die` listing the invalid names, or print the count of valid `palmares_idx_*`.
Re-running it after a failure needs those names dropped first; put that one line in the runbook.

The k8s side is the repo's documented **gate Job** pattern (`apps-k8s-configs/CLAUDE.md`): a `batch/v1`
Job in **its own** Flux Kustomization with `wait: true`, carrying
`kustomize.toolkit.fluxcd.io/force: enabled` (only `enabled`/`Enabled` work, not `true`) because
`spec.template` is immutable, **no `ttlSecondsAfterFinished`** (a git-managed Job deleted by TTL is
recreated and re-run at every reconcile), `backoffLimit: 20` as the wait-for-Postgres mechanism, and an
idempotent command — the completed Job is the "it ran" marker. Its re-run trigger is the image digest,
exactly like `job-defibot-migrate`: new SQL ⇒ new `palmares-admin` image ⇒ new pod template ⇒ `force`
recreates ⇒ it re-runs, and only then. Image
`ghcr.io/<owner>/palmares-admin:<sha>` **(OPEN: the GHCR owner handle — T-INFRA's compose defaults to
`en7angled`, the account that runs public CI is `mariusgeorgescu`)**; `command: [admin, apply-indexes]`;
`PALMARES_ADMIN_CONN_STR` assembled in the pod from `configmap-palmares-dbsync-env` +
`secret-palmares-dbsync`, i.e. as the `dbsync` superuser, which is what ownership of `public.*` and
`CREATE EXTENSION` require.

**Precondition, enforced not assumed:** run `scripts/check_dbsync_tip.sh` and require exit 0 before
pushing the Job — T-SUBSTRATE-5's gate is this task's entry condition, because
`CREATE INDEX CONCURRENTLY` against db-sync's bulk-insert transactions blocks or leaves an invalid
index behind.

Flux root `clusters/minirack/chains/cardano-preprod-dbsync-sql.yaml`:
`path: ./chains/cardano/preprod-dbsync/sql-jobs`, `wait: true`, **`timeout: 30m0s`** (a cold GIN build
over `off_chain_vote_*` is minutes, not seconds), `dependsOn: [cardano-preprod-dbsync]`. The app root
that the read-path block adds then `dependsOn` this one, so the views are never created over an
unindexed `public`. (D-26 names two Flux roots; this third one is what the house gate-Job pattern
requires, and it keeps the long-running Job out of the 1-minute db-sync root.)

- [ ] **Step 4: Run test to verify it passes**

```bash
cd /Users/mg/Projects/CardanoProjects/palmares && cabal build all && cabal test test --test-options='-p ApplySql'
./scripts/check_dbsync_tip.sh                     # precondition: must exit 0 before the Job is pushed
# then against the real, synced database
cd /Users/mg/Projects/apps-k8s-configs && git push \
  && flux --context minirack-lan reconcile kustomization cardano-preprod-dbsync-sql --with-source
kubectl --context minirack-lan -n cardano-preprod wait --for=condition=complete job/job-palmares-apply-indexes --timeout=30m
kubectl --context minirack-lan -n cardano-preprod exec -i deploy/deployment-palmares-postgres -- \
  psql -U dbsync -d cexplorer -At -c \
  "SELECT count(*) FROM pg_class c JOIN pg_index i ON i.indexrelid=c.oid
   WHERE c.relname LIKE 'palmares\_idx\_%' AND i.indisvalid;"
```
Expected: PASS — the four HUnit cases green, the Job `condition=complete`, and the last query printing
exactly `22`.

- [ ] **Step 5: Commit**

```bash
# in palmares
git add sql/040_indexes.sql src/lib/offchain-lib/Palmares/Admin/ApplySql.hs src/exe/admin/Main.hs palmares.cabal src/test/UnitTests/ApplySql.hs
git commit -m "feat(admin): apply-indexes — palmares_idx_* on db-sync's public schema"
# in apps-k8s-configs
git add chains/cardano/preprod-dbsync/sql-jobs clusters/minirack/chains/cardano-preprod-dbsync-sql.yaml clusters/minirack/chains/kustomization.yaml
git commit -m "feat(palmares): gate Job for sql/040_indexes.sql"
```

---

### Task T-SUBSTRATE-7: sizing measurement and ADR `0001-governance-data-substrate.md`

The F0 exit criterion of spec §9: the ADR that fixes the data source, carrying Experiment A's and the
cluster's numbers rather than an estimate.

**Files:**
- Create: `/Users/mg/Projects/CardanoProjects/palmares/docs/adr/0001-governance-data-substrate.md`
- Create: `/Users/mg/Projects/CardanoProjects/palmares/scripts/measure_substrate.sh`
- Modify: `/Users/mg/Projects/CardanoProjects/palmares/docs/deployment/preprod-dbsync-sync.md`

**Interfaces:**
- Consumes: `experiments/dbsync-preprod/measurements.md` (**ASSEMBLY CORRECTION: the Experiment A bundle
  that actually exists on disk is `experiments/dbsync-preprod/` with `measurements.md`, not
  `experiments/dbsync-genesis/RESULTS.md` — the two drafters named it differently; use the real path**);
  the synced cluster database; `scripts/check_dbsync_tip.sh`.

> **ASSEMBLY CORRECTION 3** (2026-10-19). ADR `0001` must also carry the **Yaci-Store rejection record**,
> citing `docs/superpowers/design/f1-yaci-store-evaluation.md`: rejected as substrate because its
> `drep_dist` depends on the UTxO store (account → stake-address-balance → snapshot), so it must index
> the entire UTxO set to produce per-epoch DRep power — heavier than `only_governance`, which gets it
> from the ledger replay with `tx_out` disabled; `GovActionStatus` has only `{ACTIVE, RATIFIED, EXPIRED}`
> (no ENACTED/DROPPED, open issue #1194); it stores `anchor_url`/`anchor_hash` only, with no off-chain
> fetching or hash verification, which would resurrect the fetcher we deleted; and its per-epoch range
> partitioning needs ~3,160 locks for one unbounded `drep_dist` query (open issue #1200) while its
> baseline migration opens with `drop table if exists drep_dist cascade`, which would silently drop our
> views. Record the T1–T7 monitorable triggers from that note's §8, and that Yaci-Store is **not** a
> fallback in the disk-pressure chain (being structurally heavier) — the fallback for "does not fit the
> Pi" is the x86_64 box budgeted for F2.
- Produces: `scripts/measure_substrate.sh`; ADR 0001 with the PVC verdict, the peak-RSS verdict and the
  recorded wall-clock.

- [ ] **Step 1: Write the failing test**

`scripts/measure_substrate.sh` — the measurement, re-runnable, and the ADR's only source of numbers.

```bash
#!/usr/bin/env bash
# Substrate sizing. Everything the ADR asserts must come out of this script, not out of a note.
set -euo pipefail
: "${PSQL:=kubectl --context minirack-lan -n cardano-preprod exec -i deploy/deployment-palmares-postgres -- psql -U dbsync -d cexplorer}"
K="kubectl --context minirack-lan -n cardano-preprod"

# Precondition. Measuring a mid-sync database produces numbers the ADR would then assert as final.
"$(dirname "$0")/check_dbsync_tip.sh" >/dev/null 2>&1 \
  || { echo "FAIL: db-sync is not at tip; ADR 0001 numbers are not final"; exit 1; }

echo '## pg_total_relation_size, public, top 20'
$PSQL -c "SELECT relname, pg_size_pretty(pg_total_relation_size(c.oid)) AS total
  FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace
  WHERE n.nspname = 'public' AND c.relkind = 'r'
  ORDER BY pg_total_relation_size(c.oid) DESC LIMIT 20;"

echo '## database and PVC'
$PSQL -At -c "SELECT 'pg_database_size_bytes', pg_database_size('cexplorer');"
$K exec deploy/deployment-palmares-postgres -- df -h /var/lib/postgresql/data | tail -1

echo '## db-sync peak RSS (cgroup v2; the sampled figure from Experiment A is the cross-check)'
$K exec deploy/deployment-palmares-dbsync -- sh -c \
  'cat /sys/fs/cgroup/memory.peak 2>/dev/null || cat /sys/fs/cgroup/memory/memory.max_usage_in_bytes' \
  || echo 'peak_rss unavailable — use Experiment A rss.csv'
$K top pod -l app=palmares-dbsync-pod-label --no-headers || echo 'metrics-server unavailable'

echo '## wall-clock, from db-sync itself'
$PSQL -At -c "SELECT 'epochs_timed', count(*), 'total_seconds', sum(seconds)::bigint FROM epoch_sync_time;"

# The gate: does the measured size still fit the PVC that cannot be expanded?
used=$($PSQL -At -c "SELECT pg_database_size('cexplorer');")
[ "$used" -lt 26843545600 ] || { echo "FAIL: cexplorer $used B >= 25 GB — see ADR 0001 fallbacks"; exit 1; }
```

- [ ] **Step 2: Run test to verify it fails**

Run: `./scripts/measure_substrate.sh` — before T-SUBSTRATE-5's gate has gone green.
Expected: FAIL with exactly `FAIL: db-sync is not at tip; ADR 0001 numbers are not final`, exit 1.

- [ ] **Step 3: Write the ADR from the script's output**

ADR 0001 records, as measured values with the command that produced each: the substrate decision
(cardano-db-sync 13.7.1.0, `preset: only_governance`, `ledger: enable`, `shelley: enable: true`,
`offchain_vote_data: enable`, the blinklabs arm64 digest, Postgres 17 in the same `cexplorer` database
with two added schemas); the `pg_total_relation_size` table; `pg_database_size`; peak RSS from both the
cluster cgroup and Experiment A's sampled figure; the wall-clock; and the three verdicts:

- **PVC verdict** — 50 Gi, **sized once**: `local-path` has `allowVolumeExpansion: false`, so the number
  in the manifest is final for the life of that volume. State the measured headroom multiple.
- **Peak-RSS verdict** — the `requests: 4Gi` / `limits: 6Gi` pair confirmed, raised, or escalated to
  D-13 fallback 2.
- **Fallbacks, ordered, with their triggers** — (1) `DELETE FROM epoch_stake WHERE epoch_no < tip-2` as
  a CronJob, **only after the `validateEpochStake` test exists** (db-sync reads that table back for
  validation); (2) `ledger_backend: lsm`, accepting the Raspberry-Pi system-thread crash and the
  >4096-item `blockio-uring` bug; (3) db-sync off-cluster.

Also record what was deliberately **not** configured away: `epoch_stake` is inserted unconditionally
whenever the ledger is enabled (`insertStakeSlice` has no option gate), so it is the largest table in the
database and 100 % unused by F1 — that is the fact fallback (1) exists for.

Honest limits to write down: `tx.fee` is `0` under this preset (`tx_out: disable`), `off_chain_pool_data`
is empty so SPO pages show hashes not tickers, and constitution anchors are never fetched
(`queryNewVoteWorkQueueDataStmt` has `AND va.type != 'constitution'`).

- [ ] **Step 4: Run test to verify it passes**

```bash
cd /Users/mg/Projects/CardanoProjects/palmares && ./scripts/measure_substrate.sh | tee /tmp/substrate.txt
# the ADR must quote the measured size, not an estimate
size=$(grep '^pg_database_size_bytes' /tmp/substrate.txt | cut -f2)
grep -q "$size" docs/adr/0001-governance-data-substrate.md
```
Expected: PASS — the script exits 0 (database under 25 GB) and the measured `pg_database_size`
byte count appears verbatim in ADR 0001.

- [ ] **Step 5: Commit**

```bash
git add docs/adr/0001-governance-data-substrate.md scripts/measure_substrate.sh docs/deployment/preprod-dbsync-sync.md
git commit -m "docs(adr): 0001 governance data substrate, with measured sizing"
```

---

### Task T-SUBSTRATE-8: Postgres roles, grants and `search_path`

The gap T-EXTRACT flagged and nobody closed. `palmares_owner` and `palmares_ro` are created here, as the
`dbsync` superuser, by the same gate-Job mechanism as T-SUBSTRATE-6 — not by an `initdb.d` script, which
would duplicate the file across two repos and only ever run on a fresh volume.

Its *cluster* precondition is only that db-sync's stage-1/2 migrations have run (seconds into
T-SUBSTRATE-4), not a completed sync. It is nonetheless placed after T-SUBSTRATE-6 because it consumes
`applySqlAutocommit` and the `sql-jobs/` Flux root from it, and because synthesis §3 puts roles in
Phase 3 (step 14) behind all of Phase 2. If the schedule needs the wait filled, the only safe reordering
is to move the whole `Palmares.Admin.ApplySql` + `sql-jobs/` machinery into this task and leave
T-SUBSTRATE-6 holding `indexesSql` alone.

**Files:**
- Create: `/Users/mg/Projects/CardanoProjects/palmares/sql/050_roles.sql`
- Modify: `/Users/mg/Projects/CardanoProjects/palmares/src/lib/offchain-lib/Palmares/Admin/ApplySql.hs`
- Modify: `/Users/mg/Projects/CardanoProjects/palmares/src/exe/admin/Main.hs`
- Modify: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/secrets/secret-palmares-dbsync.yaml`
- Create: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/sql-jobs/job-palmares-apply-roles.yaml`
- Modify: `/Users/mg/Projects/apps-k8s-configs/chains/cardano/preprod-dbsync/sql-jobs/kustomization.yaml`
- Test: `/Users/mg/Projects/CardanoProjects/palmares/scripts/check_roles.sh`

**Interfaces:**
- Consumes: T-SUBSTRATE-3's `dbsync` superuser and `cexplorer`; `applySqlAutocommit`,
  `PALMARES_ADMIN_CONN_STR`, the `sql-jobs/` directory and the Flux root
  `cardano-preprod-dbsync-sql`, all from T-SUBSTRATE-6.
- Produces: roles `palmares_owner` and `palmares_ro`; schema `palmares_attest` owned by
  `palmares_owner`; `admin apply-roles`; `rolesSql :: ByteString`; secret keys
  `PALMARES_OWNER_PASSWORD` and `PALMARES_RO_PASSWORD`; env vars `PG_CONN_STR` (runtime,
  `palmares_ro`) and `PALMARES_OWNER_CONN_STR` (boot DDL + matview refresh, `palmares_owner`);
  `scripts/check_roles.sh`.

- [ ] **Step 1: Write the failing test**

```bash
#!/usr/bin/env bash
# scripts/check_roles.sh — the privilege separation is the control for co-locating our schemas in
# db-sync's database (T-DATA §10). Assert it, do not assume it.
set -euo pipefail
: "${PSQL:=kubectl --context minirack-lan -n cardano-preprod exec -i deploy/deployment-palmares-postgres -- psql -U dbsync -d cexplorer}"
q() { $PSQL -At -c "$1"; }
fail=0
chk() { [ "$(q "SELECT $2;")" = t ] || { echo "FAIL: $1"; fail=1; }; }

chk "palmares_owner exists, can log in, is not superuser" \
  "(SELECT rolcanlogin AND NOT rolsuper FROM pg_roles WHERE rolname='palmares_owner')"
chk "palmares_ro exists, can log in, is not superuser" \
  "(SELECT rolcanlogin AND NOT rolsuper FROM pg_roles WHERE rolname='palmares_ro')"
chk "palmares_ro may read db-sync's tables" \
  "has_table_privilege('palmares_ro','public.voting_procedure','SELECT')"
chk "palmares_ro may NOT write db-sync's tables" \
  "NOT has_table_privilege('palmares_ro','public.voting_procedure','INSERT')"
chk "palmares_ro may NOT create in public" \
  "NOT has_schema_privilege('palmares_ro','public','CREATE')"
chk "palmares_owner may create the palmares schema" \
  "has_database_privilege('palmares_owner','cexplorer','CREATE')"
chk "palmares_attest is owned by palmares_owner" \
  "(SELECT nspowner = 'palmares_owner'::regrole FROM pg_namespace WHERE nspname='palmares_attest')"
chk "search_path resolves unqualified names for palmares_ro" \
  "(SELECT 'search_path=palmares, palmares_attest, public' = ANY(rolconfig) FROM pg_roles WHERE rolname='palmares_ro')"
# future-proofing: a db-sync migration that adds a table must not need a re-GRANT
chk "default privileges cover tables dbsync creates later in public" \
  "(SELECT count(*) > 0 FROM pg_default_acl d JOIN pg_namespace n ON n.oid = d.defaclnamespace
      WHERE n.nspname='public' AND d.defaclobjtype='r'
        AND array_to_string(d.defaclacl,',') LIKE '%palmares_ro=r%')"
exit $fail
```

- [ ] **Step 2: Run test to verify it fails**

Run: `./scripts/check_roles.sh`
Expected: FAIL with all nine lines, starting `FAIL: palmares_owner exists, can log in, is not superuser`
(`pg_roles` has no such row, so the subselect is NULL, not `t`), and with
`ERROR: role "palmares_ro" does not exist` on stderr from the `has_*_privilege` checks. Exit 1.

- [ ] **Step 3: Write `sql/050_roles.sql` and `admin apply-roles`**

Exact content, because the grant set is the whole deliverable:

```sql
-- sql/050_roles.sql — applied OUTSIDE any transaction by `admin apply-roles`, connected as `dbsync`
-- (the Postgres bootstrap superuser). Idempotent; safe to re-run after any db-sync migration.
-- Schema `palmares` is deliberately NOT created here: see the note at the bottom.

-- 1. The two roles. CREATE ROLE has no IF NOT EXISTS.
DO $$ BEGIN CREATE ROLE palmares_owner LOGIN; EXCEPTION WHEN duplicate_object THEN NULL; END $$;
DO $$ BEGIN CREATE ROLE palmares_ro    LOGIN; EXCEPTION WHEN duplicate_object THEN NULL; END $$;

-- 2. palmares_owner creates and owns our two schemas; palmares_ro only connects.
GRANT CONNECT, CREATE ON DATABASE cexplorer TO palmares_owner;
GRANT CONNECT             ON DATABASE cexplorer TO palmares_ro;
CREATE SCHEMA IF NOT EXISTS palmares_attest AUTHORIZATION palmares_owner;

-- 3. Read-only access to db-sync's schema for both roles. ALL TABLES covers what exists now;
--    ALTER DEFAULT PRIVILEGES covers every table a future db-sync migration adds — without it,
--    each upgrade would silently leave new tables unreadable until someone re-granted by hand.
GRANT USAGE ON SCHEMA public TO palmares_owner, palmares_ro;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO palmares_owner, palmares_ro;
ALTER DEFAULT PRIVILEGES FOR ROLE dbsync IN SCHEMA public
  GRANT SELECT ON TABLES TO palmares_owner, palmares_ro;
-- never granted: INSERT/UPDATE/DELETE on public, and CREATE on public. The public read API must
-- never hold a role that can write the substrate.
REVOKE CREATE ON SCHEMA public FROM PUBLIC;

-- 4. palmares_ro reads the attestation projections that attest-sync creates as palmares_owner.
GRANT USAGE ON SCHEMA palmares_attest TO palmares_ro;
GRANT SELECT ON ALL TABLES IN SCHEMA palmares_attest TO palmares_ro;
ALTER DEFAULT PRIVILEGES FOR ROLE palmares_owner IN SCHEMA palmares_attest
  GRANT SELECT ON TABLES TO palmares_ro;

-- 5. search_path, so an unqualified `drep` resolves to palmares.drep, an unqualified
--    `profile_projection` to palmares_attest.profile_projection, and an unqualified `block` to
--    db-sync's public.block — in every session of either role, including psql and the Job.
ALTER ROLE palmares_owner SET search_path = palmares, palmares_attest, public;
ALTER ROLE palmares_ro    SET search_path = palmares, palmares_attest, public;

-- NOTE, and it is load-bearing: query-api's boot transaction runs
--   DROP SCHEMA IF EXISTS palmares CASCADE; CREATE SCHEMA palmares;
-- (D-15), which destroys every grant on that schema and the pg_default_acl row with it. So
-- ALTER DEFAULT PRIVILEGES cannot carry it, and sql/000_schema.sql MUST itself end with
--   GRANT USAGE ON SCHEMA palmares TO palmares_ro;
-- and the boot set's last statement MUST be
--   GRANT SELECT ON ALL TABLES IN SCHEMA palmares TO palmares_ro;
-- Both inside the same transaction. Omit either and query-api serves 500s on every governance route
-- after its first restart.
```

`admin apply-roles` = `applySqlAutocommit <PALMARES_ADMIN_CONN_STR> rolesSql` (reusing T-SUBSTRATE-6's function and
`makeRelativeToProject "sql/050_roles.sql" >>= embedFile`), then set the two passwords from env so no
credential is ever in Git:

```haskell
-- After the file. postgresql-simple substitutes client-side, so `?` yields a correctly escaped
-- literal in the PASSWORD position; the role name cannot be a parameter and is a constant.
setPw :: Connection -> Query -> Text -> IO ()
setPw conn stmt pw = void (execute conn stmt (Only pw))
-- stmts: "ALTER ROLE palmares_owner PASSWORD ?" and "ALTER ROLE palmares_ro PASSWORD ?"
-- values: PALMARES_OWNER_PASSWORD, PALMARES_RO_PASSWORD; die if either is unset or empty.
```

Passwords are required, not optional: the `postgres:17-alpine` default `pg_hba.conf` uses
`scram-sha-256` for host connections, so a `LOGIN` role with no password cannot connect at all.
Add `PALMARES_OWNER_PASSWORD` and `PALMARES_RO_PASSWORD` to the existing SOPS Secret
`secret-palmares-dbsync` (`openssl rand -base64 24` each) — no new secret file, `.sops.yaml` unchanged.

The two connection strings the read path consumes, both libpq keyword/value form as the Belt System
already uses, both assembled in the consuming pod and **never** sharing a password with `dbsync`:

| Env var | Role | Used by |
|---|---|---|
| `PG_CONN_STR` | `palmares_ro` | query-api's serving pool; attest-sync's reads |
| `PALMARES_OWNER_CONN_STR` | `palmares_owner` | query-api's `sql/000`–`030` boot transaction and `Query/Refresh.hs`; attest-sync's `CREATE SCHEMA` + `runMigrations` |

The Secret lives in namespace `cardano-preprod` and the app pods live in `palmares`; a Secret cannot be
read across namespaces, so the app overlay carries its own SOPS copy of the two passwords. That is the
app-deploy block's file, named here so it is not discovered at deploy time.

The Job is T-SUBSTRATE-6's Job with `command: [admin, apply-roles]` and the same gate-Job rules
(`force: enabled`, no `ttlSecondsAfterFinished`, `backoffLimit: 20`), in the same
`sql-jobs/` directory and therefore the same Flux root.

- [ ] **Step 4: Run test to verify it passes**

```bash
cd /Users/mg/Projects/apps-k8s-configs && git push \
  && flux --context minirack-lan reconcile kustomization cardano-preprod-dbsync-sql --with-source
kubectl --context minirack-lan -n cardano-preprod wait --for=condition=complete job/job-palmares-apply-roles --timeout=5m
cd /Users/mg/Projects/CardanoProjects/palmares && ./scripts/check_roles.sh
# and prove the credential actually works end to end
kubectl --context minirack-lan -n cardano-preprod exec -i deploy/deployment-palmares-postgres -- \
  env PGPASSWORD="$PALMARES_RO_PASSWORD" psql -h 127.0.0.1 -U palmares_ro -d cexplorer -At \
  -c 'SELECT count(*) FROM voting_procedure;'     # unqualified: resolved via search_path
```
Expected: PASS — the Job `condition=complete`, `check_roles.sh` exits 0 with no `FAIL:` line, and the
unqualified `voting_procedure` count returns a number as `palmares_ro`.

- [ ] **Step 5: Commit**

```bash
# in palmares
git add sql/050_roles.sql src/lib/offchain-lib/Palmares/Admin/ApplySql.hs src/exe/admin/Main.hs scripts/check_roles.sh
git commit -m "feat(admin): apply-roles — palmares_owner/palmares_ro grants and search_path"
# in apps-k8s-configs
git add chains/cardano/preprod-dbsync/sql-jobs chains/cardano/preprod-dbsync/secrets/secret-palmares-dbsync.yaml
git commit -m "feat(palmares): gate Job for sql/050_roles.sql + role passwords"
```

---

## Produces

**Postgres / SQL**
- Database `cexplorer` on `service-palmares-postgres.cardano-preprod.svc.cluster.local:5432`, one
  database with two added schemas.
- Roles `dbsync` (bootstrap superuser, owns `public`), `palmares_owner` (owns `palmares` +
  `palmares_attest`, `SELECT` on `public.*`, refreshes the matview), `palmares_ro`
  (`USAGE` + `SELECT` only).
- Schema `palmares_attest`, owned by `palmares_owner` (attest-sync's own
  `CREATE SCHEMA IF NOT EXISTS` stays, and is a no-op).
- `search_path = palmares, palmares_attest, public` on both roles.
- `ALTER DEFAULT PRIVILEGES FOR ROLE dbsync IN SCHEMA public GRANT SELECT ON TABLES TO
  palmares_owner, palmares_ro` — new db-sync tables are readable without a re-grant.
- 22 indexes `palmares_idx_*` on `public.*`, all valid, including
  `palmares_idx_drep_distr_epoch`, `palmares_idx_committee_reg_hot`,
  `palmares_idx_ocvdd_given_name_trgm` and `palmares_idx_ocvgad_title_trgm`; extension `pg_trgm`.
- **Contract on the read-path block:** `sql/000_schema.sql` must end with
  `GRANT USAGE ON SCHEMA palmares TO palmares_ro;` and the boot transaction's last statement must be
  `GRANT SELECT ON ALL TABLES IN SCHEMA palmares TO palmares_ro;` — `DROP SCHEMA … CASCADE` destroys
  every grant on `palmares` and the `pg_default_acl` row with it.

**Haskell / repo files**
- `admin apply-indexes`, `admin apply-roles`.
- `Palmares.Admin.ApplySql` (in **`offchain-lib`**, file
  `src/lib/offchain-lib/Palmares/Admin/ApplySql.hs`):
  `applySqlAutocommit :: ByteString -> ByteString -> IO ()` (connection string, then SQL),
  `indexesSql :: ByteString`, `rolesSql :: ByteString`,
  `assertIndexesValid :: Connection -> IO [Text]`.
- `sql/040_indexes.sql`, `sql/050_roles.sql`.
- `scripts/check_node_hostpath.sh`, `scripts/check_postgres.sh`, `scripts/check_dbsync.sh`,
  `scripts/check_dbsync_tip.sh`, `scripts/measure_substrate.sh`, `scripts/check_roles.sh`.
- `experiments/dbsync-genesis/{compose.yaml,db-sync-config.yaml,node-configs/,measure.sh,rss-sample.sh,RESULTS.md}`
  — the `chain`-profile services become the root `compose.yaml`'s `chain` profile verbatim.
- `docs/adr/0001-governance-data-substrate.md`, `docs/deployment/preprod-dbsync-sync.md`.
- Cabal: `postgresql-simple` + `file-embed` and `Palmares.Admin.ApplySql` on **`offchain-lib`**;
  `UnitTests.ApplySql` on the `test` component; `extra-source-files: sql/*.sql`.

**Env vars and secrets**
- `PALMARES_ADMIN_CONN_STR` — `dbsync` superuser, the two gate Jobs only.
- `PG_CONN_STR` — `palmares_ro`, serving.
- `PALMARES_OWNER_CONN_STR` — `palmares_owner`, boot DDL + matview refresh + attest migrations.
- Secret keys `POSTGRES_PASSWORD`, `PALMARES_OWNER_PASSWORD`, `PALMARES_RO_PASSWORD` in
  `secret-palmares-dbsync` (ns `cardano-preprod`); the app overlay needs its own SOPS copy of the last
  two, since Secrets do not cross namespaces.
- ConfigMap `configmap-palmares-dbsync-env`: `POSTGRES_HOST`, `POSTGRES_PORT`, `POSTGRES_DB=cexplorer`,
  `POSTGRES_USER=dbsync`, `PGDATA`.

**Cluster objects (`apps-k8s-configs` only — D-26)**
- Manifest dirs `chains/cardano/preprod-dbsync/` and `chains/cardano/preprod-dbsync/sql-jobs/`.
- Flux Kustomizations `cardano-preprod-dbsync` (`wait: true`, `timeout: 1m0s`) and
  `cardano-preprod-dbsync-sql` (`wait: true`, `timeout: 30m0s`); the app root must `dependsOn` the
  latter.
- `deployment-palmares-dbsync`, `deployment-palmares-postgres`, `service-palmares-postgres`,
  `service-palmares-dbsync-metrics`, `pvc-palmares-dbsync-pg` (50 Gi),
  `pvc-palmares-dbsync-state` (10 Gi), `networkpolicy-palmares-postgres`,
  `servicemonitor-palmares-dbsync` (`path: /`), `prometheusrule-palmares-dbsync` with alert
  `PalmaresDbSyncLagging`
  (`cardano_db_sync_node_block_height - cardano_db_sync_db_block_height > 20` for `15m`),
  `job-palmares-apply-indexes`, `job-palmares-apply-roles`.
- hostPaths `/var/lib/cardano-preprod/{ipc,configs}` on `minirack-pi5-1`.

**Measurements that later blocks quote**
- `WALLCLOCK_TO_TIP_HOURS`, `PG_TOTAL_BYTES`, `PEAK_RSS_BYTES`, the `epoch_sync_time` curve, and the
  five `count(*)` probes — all in `experiments/dbsync-genesis/RESULTS.md` and ADR 0001.
- The anchor-coverage figure (`off_chain_vote_data` vs `off_chain_vote_fetch_error`) that T-API §8.5
  defers and that triggers the deferred supplementary re-fetcher at > ~20 % `not_fetched`.

## Open items

- **(OPEN: the GHCR owner handle.)** `ghcr.io/<owner>/palmares-admin` — T-INFRA's compose defaults to
  `en7angled`; the account with proven public CI is `mariusgeorgescu`. Blocks the two Job manifests
  (T-SUBSTRATE-6, T-SUBSTRATE-8). Owner's call, one find/replace.
- **(OPEN: `drep_distr` coverage of the earliest Conway epochs.)** F1 check 1 is written as
  "populated for every Conway epoch", and preprod's first Conway epoch is 163 — but zero DReps may have
  been registered in the first epochs after Chang, in which case an empty `drep_distr` for them is
  correct, not a defect. T-SUBSTRATE-1 and T-SUBSTRATE-5 assert the defensible form (no gap between
  `min` and `max`, and `max >= tip-1`) and **measure** `min(epoch_no)`; if that comes back as 163 the
  stronger wording stands, and if not the check's wording is amended with the measured value.
- **(OPEN: whether db-sync 13.7.1.0 tolerates the node config's `LedgerDB: V2LSM` block.)** Answered for
  free by T-SUBSTRATE-1 using the cluster's real `config.json`; the fallback (a vendored db-sync-only
  `config.json` with that block removed, genesis hashes kept) is written into T-SUBSTRATE-4 and costs one
  ConfigMap key.
- **(OPEN: `minirack-m4`'s live taint.)** T-INFRA §1.2 records that
  `apps-k8s-configs/CLAUDE.md` documents `dedicated=m4-keys` on `minirack-m4-keys`, not the
  `dedicated=m4:NoSchedule` the first draft assumed, and that `minirack-m4-keys` is absent from the live
  node list. Irrelevant to this block (everything here pins `minirack-pi5-1`) but it must be re-read live
  before the app overlay's placement patch is written.
- **(OPEN: re-check `ghcr.io/blinklabs-io/cardano-db-sync` tags before T-SUBSTRATE-4.)** Upstream aarch64
  images are in CHANGELOG "Unreleased"; if an official arm64 13.7.2.1+ has landed, the pin decision is
  revisited **before** the one-shot sync, never after.
