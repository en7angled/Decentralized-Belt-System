# Preprod Coordinated Redeploy Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Produce a verified, ordered runbook that sequences all merged-but-undeployed backend work (R1 re-sync, Stream E validator redeploy, Stream G config regen, Stream C FE lockstep, Maestro rotation) into one preprod cutover — and execute the safe/local parts (pre-flight hash verification, the runbook doc, the post-redeploy config commit).

**Architecture:** The design (`docs/superpowers/specs/2026-07-05-preprod-coordinated-redeploy-design.md`) reduces everything to one preprod cutover centered on `admin deploy-reference-scripts` (which redeploys scripts + mints a fresh oracle + rewrites `config_bjj_validators.json`, resolving E and G together). This plan's executable deliverables are documentation + verification + one post-handoff commit; the irreversible on-chain/secret steps are ops handoffs the runbook documents.

**Tech Stack:** GHC 9.6.6 + Cabal (via Nix/direnv), `cabal run exe:admin`, Cardano preprod, Maestro provider, Kupo/chain-sync.

## Global Constraints

- Network: **preprod** only (mainnet is a separate later cutover). Copied from spec §2.
- Old on-chain data: **abandon + re-populate** (`scripts/populate_testnet.sh`). No migration.
- Role split: this plan's tasks are **safe/local** (no secrets, reversible). Maestro rotation, `deploy-reference-scripts`, server/FE deploy, and repopulate are **ops handoffs** — documented, never executed here.
- Cutover point: the **old deployment stays live until step 3** (server repoint). Steps 1–2 are non-destructive.
- Ordering (in the runbook): token-first → on-chain → BE → FE → repopulate.
- All `cabal`/`admin` commands run inside the Nix env: prefix with `direnv exec .` from repo root.

---

### Task 1: Pre-flight — verify the validator-hash change set

**Files:**
- Modify (regenerate, expect no diff): `docs/generated/bjj-belt-system-blueprint.json`
- Read: `config/config_bjj_validators.json`, `docs/deployment/redeploy-onchain-hardening.md`

**Interfaces:**
- Produces: a confirmed change-set — the exact validator hashes that will change on redeploy — consumed by Task 2's pre-flight section. If the regenerated hashes do **not** match the redeploy doc's "new" column, STOP and escalate (source drifted since the doc was written).

- [ ] **Step 1: Regenerate the blueprint from current `main` source**

Run:
```bash
cd /Users/mg/Projects/CardanoProjects/Decentralized-Belt-System
direnv exec . cabal run exe:admin -- write-blueprint
```
Expected: writes `docs/generated/bjj-belt-system-blueprint.json`; exits 0.

- [ ] **Step 2: Confirm the committed blueprint is already current**

Run:
```bash
git diff --stat docs/generated/bjj-belt-system-blueprint.json
```
Expected: **no diff** (Stream E already regenerated + committed the blueprint on `main`). If there IS a diff, the committed blueprint was stale — note it; the regenerated one is authoritative.

- [ ] **Step 3: Extract the current validator hashes and diff against the deployed config**

Run:
```bash
direnv exec . python3 - <<'PY'
import json
bp = json.load(open('docs/generated/bjj-belt-system-blueprint.json'))
cfg = json.load(open('config/config_bjj_validators.json'))
# blueprint validator hashes
bp_hashes = {v.get('title'): v.get('hash') for v in bp.get('validators', [])}
print("BLUEPRINT validator titles+hashes:")
for k,v in bp_hashes.items(): print(f"  {k}: {v}")
print("\nDEPLOYED config *HashAndRef keys:")
for k in cfg:
    if k.endswith('HashAndRef'): print(f"  {k}: {str(cfg[k])[:80]}")
PY
```
Expected: prints blueprint hashes + the deployed config's `*HashAndRef` values so they can be eyeballed. (Blueprint field names may differ — adjust the extraction to the actual schema; the goal is to surface both sets side by side.)

- [ ] **Step 4: Confirm the change set matches the redeploy doc**

Compare the four hashes the redeploy doc (`redeploy-onchain-hardening.md` §2) marks **YES** — Minting Policy, Memberships, Ranks, Achievements — against the regenerated blueprint. Confirm:
- those four differ from the deployed config (the staleness), and
- the regenerated "new" hashes equal the doc's "new" column.

Expected: match. If any differs from the doc, **STOP** — source drifted; re-verify before any deploy.

- [ ] **Step 5: No commit**

Task 1 produces confirmation only; the regenerated blueprint should be unchanged (Step 2). Nothing to commit. Record the confirmed hash set for Task 2.

---

### Task 2: Write the coordinated-redeploy runbook

**Files:**
- Create: `docs/deployment/preprod-coordinated-redeploy.md`
- Read: `docs/deployment/redeploy-onchain-hardening.md`, `config/config_atlas.json`, spec §3–§6

**Interfaces:**
- Consumes: Task 1's confirmed hash change-set (goes in the pre-flight section).
- Produces: the runbook — the single doc ops follows for the cutover.

- [ ] **Step 1: Confirm the Maestro token location (for the rotation section)**

Run:
```bash
grep -n 'maestroToken\|MaestroToken\|apiKey' config/config_atlas.json
git log --oneline -1 -- config/config_atlas.json
```
Expected: confirms the token lives in `config/config_atlas.json` (committed) so the runbook's rotation step is accurate + can flag the git-history exposure.

- [ ] **Step 2: Write the runbook doc**

Create `docs/deployment/preprod-coordinated-redeploy.md` with these sections (content drawn from spec §3–§6 and Task 1's confirmed hashes):
1. **Purpose + scope** — one preprod cutover; links `redeploy-onchain-hardening.md` for E-specific detail; states abandon+repopulate.
2. **Pre-flight gate** — the confirmed hash change-set from Task 1; the check that `operation.prv` + a funded preprod wallet are present.
3. **Step 1 — Rotate Maestro token** `[ops]`: issue new key, move to `.env` (uncommitted), scrub from `config_atlas.json`; note old committed token is exposed in git history (rotation kills it; history purge optional).
4. **Step 2 — Redeploy on-chain** `[ops]`: `direnv exec . cabal run exe:admin -- deploy-reference-scripts` (needs `config_atlas.json` + `operation.prv`); rewrites `config_bjj_validators.json` (resolves E + G).
5. **Step 3 — Deploy BE servers (fail-closed)** `[ops]` — **CUTOVER POINT**: interaction/query/chainsync/mcp with `BASIC_USER`/`BASIC_PASS`/`CORS_ALLOWED_ORIGINS` + new config; chain-sync schema v4 → wipe + re-sync from origin against new validators (R1 backfill).
6. **Step 4 — Deploy FE in lockstep** `[ops]`: `bjj-frontend` `val1-prerelease-blockers` with matching `BASIC_*` env.
7. **Step 5 — Repopulate + validate** `[ops]`: `scripts/populate_testnet.sh`; then the validation checklist (spec §5) verbatim.
8. **Rollback table** (spec §4) with the "old deployment live until step 3" safety property.

Mark every ops-executed step with a `[ops]` tag and every command in a fenced block.

- [ ] **Step 3: Verify the runbook has no unresolved placeholders**

Run:
```bash
grep -niE 'TBD|TODO|FIXME|<placeholder>|xxx' docs/deployment/preprod-coordinated-redeploy.md || echo "clean"
```
Expected: `clean`.

- [ ] **Step 4: Commit**

```bash
git add docs/deployment/preprod-coordinated-redeploy.md
git commit -m "docs(deploy): preprod coordinated redeploy runbook"
```

---

### Task 3: Commit the regenerated validator config (POST-HANDOFF — after ops runs Step 2)

**Files:**
- Modify (regenerated by ops's `deploy-reference-scripts`): `config/config_bjj_validators.json`

**Interfaces:**
- Consumes: the output of the ops handoff (runbook Step 2). **This task cannot run until ops has executed `deploy-reference-scripts` on preprod** and the working tree shows a regenerated `config_bjj_validators.json`.

- [ ] **Step 1: Confirm ops redeployed and the config regenerated**

Run:
```bash
git diff --stat config/config_bjj_validators.json
direnv exec . python3 -c "import json;d=json.load(open('config/config_bjj_validators.json'));print('mintingPolicy:', str(d.get('mintingPolicyHashAndRef'))[:80]);print('oracleNFTAssetClass:', str(d.get('oracleNFTAssetClass'))[:80])"
```
Expected: a diff is present; the minting-policy hash now equals the Task-1 confirmed "new" hash, and `oracleNFTAssetClass` is a fresh currency symbol.

- [ ] **Step 2: Sanity-check against Task 1's confirmed hashes**

Confirm the four changed validator hashes in the regenerated config match Task 1's "new" set. If they don't, the redeploy published something unexpected — STOP and escalate.

- [ ] **Step 3: Commit**

```bash
git add config/config_bjj_validators.json
git commit -m "chore(deploy): regenerated preprod validator config after on-chain redeploy"
```

---

## Self-Review

**1. Spec coverage:**
- §3 sequence → runbook (Task 2 Steps 2.3–2.8). ✓
- §2 decisions (preprod, abandon+repopulate, role split, cutover point, ordering) → Global Constraints + runbook. ✓
- §4 rollback → runbook §rollback (Task 2 Step 2.8). ✓
- §5 validation → runbook §step-5 (Task 2 Step 2.7). ✓
- §6 deliverables: runbook (Task 2), pre-flight hash-diff report (Task 1), Maestro-rotation note (Task 2 Steps 1+2.3). ✓
- Config regen commit (spec §3 step 2 tail) → Task 3. ✓
- §7 out-of-scope (mainnet, migration, FE consumption, R6/R7, history purge) → not planned. ✓

**2. Placeholder scan:** Task 1 Step 3 notes "blueprint field names may differ — adjust to actual schema"; this is a real runtime adjustment, not a plan placeholder (the goal + fallback are explicit). No TBD/TODO in deliverables.

**3. Type consistency:** N/A (no code types). Command/file paths are consistent across tasks; Task 3 depends on Task 1's confirmed hashes and the ops Step-2 handoff, both named explicitly.

**Gap noted:** Task 3 is blocked on an ops handoff and will remain open until the preprod redeploy actually happens — expected, not a defect.
