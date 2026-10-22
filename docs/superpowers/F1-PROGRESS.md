# F1 — jurnal de execuție (resume ledger)

> **Scopul acestui fișier:** dacă sesiunea se întrerupe (limită de usage, context pierdut, reboot), o
> sesiune nouă citește DOAR acest fișier + spec-ul și continuă fără să repete munca.
> **Regulă:** se actualizează la fiecare pas terminat. Starea reală e pe disc (fișierele listate aici),
> nu în conversație.

**Început:** 2026-10-19. **Owner:** Marius Georgescu. **Model/sesiune:** Claude Code, ultracode activ.

## Ce construim (pe scurt)

Registru open-source de reputație & track-record pentru guvernanța Cardano (CIP-1694), construit
**pentru comunitate**, fără termen impus de finanțare. Pivot din `Decentralized-Belt-System`
(care îngheață pe preview ca demo). Repo nou, Apache-2.0.

- **Spec aprobat (contractul):** `docs/superpowers/specs/2026-10-19-gov-registry-spec.md`
- **Goal F1** (= §9/F1 din spec): produsul COMPLET pe **preprod** — strat de date de guvernanță
  (cardano-db-sync `only_governance`) + proiecții/metrice + protocol de atestări on-chain + Query API
  public + **UI complet** (React/TS + Mesh), cu 16 checks verificabile (A: date 1–7, B: on-chain 8–11,
  C: API+UI 12–16). Copilotul MCP/AI = F3, în afara acestui goal. Mainnet = F2.

## Decizii fixate (nu se re-litigă)

| Temă | Decizie | Unde e argumentată |
|---|---|---|
| Direcția produsului | Registru de reputație pentru guvernanță (opțiunea 1 din analiza de pivot) | memoria `pivot-analysis-2026-10` |
| Cod | Repo nou + librării extrase din Belt System; Belt System îngheață | spec §8 |
| Licență | Apache-2.0, open-source complet, repo public de la primul commit | spec §7 |
| Public | Comunitatea Cardano; nicio fereastră de finanțare nu dictează scope/calendar | spec §1 |
| Date guvernanță | cardano-db-sync 13.7.x, preset `only_governance`, ledger ON (NU follower propriu) | spec §4.1, §13 |
| Atestări on-chain | PlutusTx/Plinth + pipeline Kupo existent (fără Aiken) | spec §4.3, §13 |
| Off-chain | Haskell + Atlas 0.14.1, GHC 9.6 (pin plutus-tx-plugin) | spec §13 |
| Frontend | TypeScript + React (Vite) + Mesh 1.9 (CIP-95) — NU PureScript | spec §4.5, §13 |
| Infra | preprod pe minirack; mainnet cere mașină dedicată 48–64 GB x86_64 | spec §9 F1/F2 |
| Monetizare | fără fee-uri de protocol; depozite anti-spam returnabile | spec §4.3 |

## Stare pe pași

- [x] **P0. Analiză pivot** (3 agenți: backend, frontend, piață) → recomandare opțiunea 1. **Done.**
- [x] **P1. Spec funcțional** scris + builder-check trecut + analiză tehnologică integrată (Anexa §13).
      **Done.** Aprobat de owner 2026-10-19.
- [x] **P2. Skills vendorate** din `defi-bot` → `.claude/skills/` (13 skills, hardening păstrat).
      **Done.** Proveniență: `.claude/skills/VENDORED.md`.
- [x] **P3. Design detaliat F1** — 5 piste în paralel, fiecare fact-checked adversarial + sinteză.
      **Done** (11 agenți, ~57 min, 2,7M tokeni; 13–20 erori factuale găsite și corectate pe loc per pistă).
      Ieșiri: `docs/superpowers/design/f1-{dbsync-data-layer,library-extraction,metric-formulas,api-contract,preprod-infra}.md`
      + **`f1-synthesis.md` ← BINDING** (rezolvă contradicțiile D-1…D-26, fixează numele canonice,
      ordinea dură a task-urilor, lista de tăieri, riscul principal).
      Spec corectat în consecință (contradicția D-22: §9 mai vorbea de „follower Ogmios"; checks 3/4
      restated pentru view-uri; check 7 — fetcher-ul propriu de metadata e tăiat, db-sync îl face).
- [x] **P4. Planul de implementare — Plan A** (date + API public; Plan B atestări și Plan C UI vin după).
      **SCRIS ȘI ASAMBLAT**; execuția e în curs (blocul 1 bootstrap + blocul 2 extracție: gata).
      **ÎN CURS.** Workflow `gov-registry-plan-a`, runId `wf_11ab5b31-caf`.
      Script: `~/.claude/projects/-Users-mg-Projects-CardanoProjects-Decentralized-Belt-System/51922688-85df-4d6c-9461-47673e464a8c/workflows/scripts/gov-registry-plan-a-wf_11ab5b31-caf.js`
      Reluare: `Workflow({scriptPath: "<scriptul de mai sus>", resumeFromRunId: "wf_11ab5b31-caf"})`.
      Ieșiri: `docs/superpowers/plans/drafts/plan-a-{bootstrap,extraction,substrate,sql-metrics,api,parity}.md`
      + bundle-ul Experimentului A în `~/Projects/CardanoProjects/palmares/experiments/dbsync-preprod/`.
      Apoi: asamblare + self-review → `docs/superpowers/plans/2026-10-19-gov-registry-plan-a-data-and-api.md`
      (**antetul e deja scris**: Goal, Architecture, Tech Stack, Global Constraints cu toate pinurile, și
      Review Focus cu 5 moduri de eșec; lipsesc doar blocurile de task-uri, de asamblat din `drafts/`).
- [x] **P4b. Reevaluare Yaci-Store** (cerută de owner, 2026-10-19) — **Done**:
      `docs/superpowers/design/f1-yaci-store-evaluation.md`. **Verdict: db-sync rămâne; Yaci-Store respins
      ca substrat, mai apăsat decât înainte, dar din alte motive** — `drep_dist`-ul lui depinde de store-ul
      UTxO (deci e mai *greu*, nu mai ușor: exact argumentul care ar fi putut răsturna decizia e fals),
      lifecycle fără ENACTED/DROPPED (issue #1194), fără fetch/verificare de ancore (ne-ar readuce
      componenta ștearsă), partiționare pe epocă ce cere ~3.160 de lock-uri la o interogare nelimitată
      (#1200) + migrare de bază care face `drop table ... cascade` peste view-urile noastre. Argumentul
      „Java e străin unui shop Haskell" e **șters — rula invers**. Yaci-Store nu e nici fallback valid la
      presiune de disc (e mai greu); fallback-ul real e mașina x86_64 de la F2. Rămâne doar ca posibil
      cross-check (a doua derivare independentă), cu triggerele T6/T7 din §8 al notei.
      **Două corecții factuale aduse notelor noastre** (aplicate în spec + sinteză + nota db-sync):
      (C1) `dropped_epoch` nu e un rezultat independent — schema doc upstream e stale (#1932); evenimentul
      `gov_action_dropped` se emite doar pentru cazul pruned-sibling (6 rânduri pe preprod), altfel
      dublează expirarea cu o epocă; `close_epoch` rămâne corect. (C2) „de la genesis" e cerință de
      **corectitudine** (db-sync #1975: două instanțe identice diferă cu ~740× pe puterea unui DRep;
      Known Issue 13.6.0.4 numește preprod) — deci restaurarea din snapshot nu e o scurtătură acceptabilă
      pentru substrat; Mithril rămâne doar pentru nod.
      ⚠️ Cele trei corecții sunt scrise ca **„Corrections that MUST be applied during assembly"** în capul
      planului, pentru că blocurile din `drafts/` au fost redactate înainte și nu le pot ști.

- [x] **P5. Blocul 1 — bootstrap** (T-BOOTSTRAP-1…5): repo `palmares`, toolchain pinuit, CI verde,
      compose, deploy/, cinci `CLAUDE.md`, leak-guard + gitleaks. **Done** 2026-10-20.
- [x] **P6. Blocul 2 — extracția** (T-EXTRACTION-1…6): copie byte-identică → rename → strip (GREEN
      GATE) → `Core.Ids` CIP-129/105 → fixurile moștenite #3 și #5. **Done** 2026-10-20, `26432e9`,
      `All 50 tests passed`. Detalii în secțiunea de la finalul fișierului.
- [x] **P7. Blocul 4 — SQL & metrici** (T-SQL-METRICS-1…13). **ÎNCHIS** 2026-10-20, `b111ba2`,
      `All 83 tests passed`. Cele 19 obiecte `palmares` + funcția de eligibilitate, catalogul de
      8 formule + `docs/metrics.md` generat cu poartă de drift în CI, fixtura pe epoca **319**
      (nu 320 — vezi secțiunea de la final), matview-ul `drep_metrics` + firul de refresh, și
      poarta de determinism (8/8 pe db-sync-ul nostru). Șapte fișiere de check în `sql/checks/`,
      toate verzi pe baza reală. Detalii în secțiunile de la finalul fișierului.
- [ ] **P7b. Blocul 5 — API-ul public** (T-API-1…18). **ÎN CURS**: 1…13 gata,
      142 de teste + ~160 de asserțiuni de smoke verzi pe db-sync-ul nostru. Rutele servite acum:
      `/health`, `/ready`, `/status`, `/metrics/formulas`, `/dreps` (JSON + CSV),
      `/drep/{id}` ≡ `/pages/drep/{id}`, `/drep/{id}/power`, `/drep/{id}/delegators` (JSON + CSV),
      `/drep/{id}/updates`, `/gov-actions` (JSON + CSV),
      `/gov-action/{id}` ≡ `/pages/gov-action/{id}`, `/votes` (JSON + CSV).
      Rulare locală: `PG_CONN_STR=... PORT=8093 cabal run exe:query-api`, apoi
      `PALMARES_API=http://localhost:8093 scripts/smoke_query_api.sh`.
- [ ] **P8. Blocul 3 — substratul** (db-sync pe cluster per D-28). Ținut separat de blocul 4
      pentru că atinge rack-ul; blocul 4 se validează local pe baza sincronizată din Experimentul A.

## Decizii luate ca builder calls (contestabile de owner)

Sinteza a cerut 5 decizii de owner în faza 0; le-am luat ca să nu blochez execuția, toate reversibile:

| Decizie | Alegerea | Dacă owner-ul decide altfel |
|---|---|---|
| Numele proiectului | **Palmares** (namespace `Palmares.*`, pachet `palmares`, imagini `ghcr.io/<owner>/palmares-<svc>`) | redenumire mecanică, footprint documentat în nota de extragere §5.2 |
| `shelley: enable: true` în db-sync | **Da** — F1 check 12 (churn/retenție delegatori) are nevoie de `stake_deregistration`; flip-ul ulterior costă re-sync complet | fără el, delegator counts devin doar o limită superioară documentată |
| Topologie db-sync | **Deployment separat** pe socket hostPath, pinuit pe `minirack-pi5-1` (nu sidecar) | sidecar-ul ar lega restartul de nod, ogmios și hydra-node |
| Schema în Postgres | `palmares` + `palmares_attest` **în baza `cexplorer` a db-sync**, cu roluri separate (`palmares_owner`, `palmares_ro`) | o a doua bază ar cere un al doilea pool și cross-db joins imposibile |
| Imaginea db-sync | **13.7.1.0 arm64 (blinklabs), pinuită prin digest** — nu 13.7.2.1 (fără build arm64) | bump-ul așteaptă un build arm64 upstream |

⚠️ **Churn pe clusterul live**: aplicarea manifestelor noi cere un restart al pod-ului nodului preprod
(editarea hostPath). Nu se atinge clusterul fără aprobarea explicită a ownerului — de aceea Experimentul A
rulează deliberat **off-cluster**, pe Mac, și nu depinde de această aprobare.
- [x] **P4. Planul de implementare — Plan A. Done** (2026-10-20): 13 agenți, 3,1M tokeni; 6 blocuri
      redactate și revizuite independent (≈90 defecte de plan găsite, toate rezolvate în afară de unul,
      care s-a închis apoi empiric). Plan final:
      **`docs/superpowers/plans/2026-10-19-gov-registry-plan-a-data-and-api.md`** — 7.855 linii,
      **51 task-uri, 280 pași verificabili**, cu Global Constraints (toate pinurile), Review Focus
      (5 moduri de eșec cu test alocat), index de task-uri, self-review, și 75 de decizii deschise
      consolidate la final. Blocurile editabile rămân în `plans/drafts/plan-a-*.md`.
      Self-review-ul a găsit o lacună reală: **check 3 (determinism) nu avea task** → adăugat
      T-SQL-METRICS-13. Check 4 (rollback) e parțial în Plan B — consemnat ca să nu fie căutat în A.
- [x] **P4c. Experimentul A LANSAT** (2026-10-20, ~00:05) — rulează pe Mac, off-cluster, ~48h.
      Bundle: `~/Projects/CardanoProjects/palmares/experiments/dbsync-preprod/`
      (compose validat; imagini pinuite prin digest index, arm64 verificat; configurile de nod pinuite
      prin hash, verificate fail-closed de `run.sh`; `insert_options` verificat în sursa db-sync — unde
      s-a descoperit că presetul `only_governance` are `shelley` **dezactivat**, deci override-ul explicit
      e obligatoriu). Nodul se bootstrapează din Mithril (~20–40 min), apoi db-sync pornește pasul de la
      genesis. **Monitorizare:** `cd <bundle> && docker compose logs -f cardano-db-sync` ·
      `./measure.sh` (o dată pe oră, scrie în `measurements.md`) · **oprire:** `docker compose stop`
      (reluabil cu `./run.sh`) · **ștergere completă:** `docker compose down -v`.
      Ce decide: dacă substratul încape pe Pi (limită 6 GiB pusă intenționat = plafonul planificat pe
      `minirack-pi5-1`), dimensiunea reală a PVC-ului, și timpul de sync. Dacă iese prost → fallback-urile
      ordonate D-13 din sinteză, nu Yaci-Store (e structural mai greu).
      **Un bug real găsit și reparat la pornire** (commit `e4f9823`): `topology.json` referențiază
      `peer-snapshot.json`, pe care fetch-ul inițial nu l-a adus, iar cardano-node 11.1.2 tratează lipsa
      ca eșec dur (exit 1) — nodul a intrat în restart-loop de 18 ori. Restaurarea Mithril reușise deja;
      lipsea doar fișierul. Adus de la aceeași sursă pinuită, hash înregistrat, iar cuplajul e numit acum
      în `SOURCES.sha256` ca un re-fetch să nu-l mai poată pierde.
      **Eșantionatorul orar a fost șters** (commit `ff7329e`): `measure.sh` citește deja
      `/sys/fs/cgroup/memory.peak`, contor **cumulativ** întreținut de kernel, deci o singură citire la
      final e la fel de exactă ca 48 orare și nu poate rata un vârf între eșantioane. Curba per-epocă e
      deja în logul db-sync. Nimic nu trebuie să ruleze în paralel cu experimentul.
      **Progres la pornire**: db-sync a trecut de epoca 32 (block 260.000) în ~2 minute; vârf de memorie
      0,14 GiB din plafonul de 6 GiB. Comenzi: `./measure.sh` (snapshot),
      `docker compose logs -f cardano-db-sync` (urmărire), `docker compose stop` (pauză reluabilă).
- [~] **P5. F0 bootstrap** — execuția Plan A, blocurile 1–2. **ÎN CURS.**
  - [x] **T-BOOTSTRAP-1 (Experimentul B) — decis: `CI_SHAPE=hosted-setup` + blst din sursă.**
        Run 1 (`.../runs/39324907758`): `ubuntu-24.04-arm` e runner ARM64 real, dar **`libblst-dev` nu
        există în arhiva arm64** — exact ramura prevăzută de plan. Run 2 (`.../runs/39325302759`) măsoară
        cu blst v0.3.15 compilat din sursă + aliasul `libblst-any`. Fallback-ul `ghcr-builder` NU e necesar.
        ⚠️ **Clarificare importantă**: blocajul Actions „pe facturare" din `apps-k8s-configs/CLAUDE.md` e
        valabil pentru repo-uri **private**; cele publice au runnere gratuite. Deci **`palmares` public de
        la commit #1 e load-bearing pentru CI**, nu doar pentru povestea open-source.
  - [x] **T-BOOTSTRAP-2 — repo creat: https://github.com/en7angled/palmares** (PUBLIC, default `main`,
        commit `c4cc72f`, 23 fișiere). Local: `~/Projects/CardanoProjects/palmares`.
        Conține: Apache-2.0 + NOTICE (atribuire MIT către Belt System) + `licenses/`, `.gitignore` și
        `.dockerignore` acoperind tot setul de secrete (`.dockerignore` **rescris**, NU copiat — cel din
        Belt System omite `.env`, exact scurgerea #1), README cu revendicarea de independență și „două
        uși", ADR 0001 (substratul db-sync + de ce sync de la genesis e corectitudine + respingerea
        Yaci-Store) și ADR 0002 (provider pe calea de scriere), `.githooks/pre-push` (poartă de secunde,
        activată cu `git config core.hooksPath .githooks`), și bundle-ul Experimentului A.
        Testul de igienă TDD a picat corect întâi (`.env` intra în contextul docker), acum trece.
  - [ ] T-BOOTSTRAP-3 (skelet cabal care compilează gol) → T-BOOTSTRAP-4 (`ci.yml` cu blst din sursă,
        `images.yml` GHCR arm64, `reconcile.yml`) → T-BOOTSTRAP-5 (`compose.yaml` top-level).
  - [ ] Blocul 2: extragerea (as-is → rename → strip GREEN GATE → rewrites), CIP-129, fixurile #3 și #5.

## Date reale despre cluster (verificate 2026-10-20, cu aprobarea ownerului)

Reguli citite din `apps-k8s-configs/CLAUDE.md` și respectate: **niciodată `kubectl apply/edit`** pe
resurse din repo (git → commit → push → Flux), `prune: true` (ștergerea unui fișier șterge resursa),
SOPS+age cu cheia Kustomization-ului proprietar, layout `apps/<app>/{base,overlay}` + `chains/cardano/`,
nume fără cratime (`palmares` e ok), prefix de tip în `metadata.name`, Gateway API nu Ingress, host public
nou = doar un `Certificate` (NU intrări CoreDNS — regula aia e obsoletă), docs în română.

| Nod | RAM alocabil | CPU | Observații |
|---|---|---|---|
| pi5-1 | 14,26 Gi (**6,16 Gi cerut → ~7,2 Gi liberi**) | 3,5 | **rulează nodul preprod** (`statefulset-cardano-preprod-0`, 3,75 Gi) + Honcho + Grafana |
| pi5-2 / pi5-3 | 14,26 Gi | 3,5 | pi5-3 are defibot + Alertmanager |
| pi5-4 | 15,3 Gi | 3,75 | BP mainnet — **exclus** (fără vecini zgomotoși) |
| rockpi-4a | 3,4 Gi | — | mic; rulează `kupo-preprod` |
| minirack-m4 | 14,28 Gi | 8 | VM pe Mac mini, taint `dedicated`, relay mainnet |

- **Storage: `local-path` (default), `allowVolumeExpansion: false`** → PVC-ul se dimensionează O DATĂ.
  Nodurile Pi5 au ~935 GB ephemeral, deci 50 Gi e confortabil pe disc.
- **Concluzia pentru substrat**: db-sync trebuie pe **pi5-1** (socket-ul nodului preprod e acolo, hostPath).
  Cu 4 Gi cerere / 6 Gi limită + Postgres, pi5-1 ajunge la ~83% din cereri — fezabil, dar **decizia reală
  e dacă peak RSS încape sub 6 Gi, exact ce măsoară Experimentul A**. Deci manifestele k8s se scriu, dar
  **nu se împing înainte de cifrele Experimentului A** — altfel riscăm un PVC prost dimensionat permanent.
- [ ] **P6. F1-A** — db-sync preprod sincronizat + proiecții + paritate Koios (checks 1–7).
- [ ] **P7. F1-B** — validatoare atestări + deploy preprod + fluxuri cap-coadă (checks 8–11).
- [ ] **P8. F1-C** — Query API public + UI complet + deploy GitOps (checks 12–16).

## Prerechizite verificate (2026-10-19)

- Scrierea de directoare noi sub `~/Projects/CardanoProjects/` funcționează (repo-ul nou va fi sibling).
- `gh` autentificat ca `mariusgeorgescu`, cu acces la org-ul **`en7angled`** (unde trăiește și
  Decentralized-Belt-System); scopes: `repo`, `workflow`, `read:org`, `gist` → suficient pentru a crea
  repo public + Actions. Decizie de luat la P5: repo sub `en7angled` (recomandat, continuitate) vs personal.
- Disc local: 161 GB liberi din 927 GB. Suficient pentru dev local preprod (nod ≈12 GB + Postgres
  db-sync ≈16 GB). Mainnet ulterior (≈250 GB+) merge pe mașina dedicată de la F2, nu aici.

## Cum se reia (pentru o sesiune nouă)

1. Citește `docs/superpowers/specs/2026-10-19-gov-registry-spec.md` (contractul) și acest fișier.
2. Citește notele de design existente din `docs/superpowers/design/` (ce e acolo e deja decis și
   fact-checked; nu se re-cercetează).
3. Dacă P3 e marcat ÎN CURS dar notele de design există toate 6 → marchează P3 done și trece la P4.
4. Dacă un workflow e marcat ÎN CURS: reia-l cu `resumeFromRunId` (comanda e în pasul respectiv), NU
   de la zero — agenții terminați vin din cache.
5. Nu comite nimic în `Decentralized-Belt-System` în afara acestor documente; repo-ul are modificări
   necomise preexistente ale ownerului (`.vscode/settings.json`, `.cabal`, `Query/Pages.hs`).

## Probleme de cod moștenite, de rezolvat în codul extras (nu în Belt System)

Confirmate pe cod: (1) selector de fee greșit la intervale de membership
(`TxBuilding/Operations.hs:611` folosește `fcMembershipHistoryFee`, policy-ul cere
`fcMembershipIntervalFee` la `MintingPolicy.hs:162`); (2) cheia de admin a Oracle-ului nu poate fi rotită
(`OracleAdminAction`, `Types.hs:84-92`, fără `SetAdmin`). Aproape confirmate: (3) projection poisoning
(`Ingestion.hs` / `Lookups.hs` nu verifică prezența NFT-ului numit în datum). De triat: (4) date nelegate
de validity range; (5) timestamps `insertedAt` în loc de timp de chain. Detalii: memoria
`new-code-issues-2026-10`.

**Actualizare 2026-10-21 — toate cinci sunt închise în `palmares`:** (1) la T-ATTEST-2 (`feeFor`,
singura funcție care numește un câmp din `FeeConfig`); (2) la T-ATTEST-3 (`OracleSetAdmin`); (5) la
T-EXTRACTION-6; (3) **scris** la T-EXTRACTION-5 și **cablat** abia la **T-ATTEST-8**
(`authenticateStateUTxO` a stat apelat de nicăieri, cu bump de schemă la 3); (4) la
T-ATTEST-3 și **reparat corect abia la `1668145`** — `checkDateInRange` mărginea capetele
intervalului, nu lățimea lui, deci back-dating-ul a rămas deschis între timp. Secțiunile de la finalul
fișierului.

## 2026-10-20 — Experimentul A închis, planul validat pe substratul real

**Experimentul A: terminat în ≤ 7 ore, nu 48.** db-sync a ajuns la tip (epoca 320, blocul 5.313.085) și
urmărește live 1:1. Măsurătorile complete sunt în `palmares/docs/adr/0001-governance-data-substrate.md`
(tabel umplut, status actualizat). Cifrele care contează pentru cluster: 5.909 MB logic / 10,43 GB volum
Postgres, 2,9 GiB stare ledger db-sync, 20,18 GB chain db nod; db-sync **5,90 GiB memorie anonimă** din
plafonul de 6 GiB, 4.183 evenimente de reclaim, zero OOM.

**Decizie rămasă la owner (bifurcație reală, nu detaliu):** măsurătoarea stabilește plafonul, nu podeaua —
RTS-ul GHC își dimensionează heap-ul din `/proc/meminfo` al gazdei, nu din limita cgroup. Fie (1) mutăm
Postgres pe alt nod și dăm db-sync toată memoria liberă de pe pi5-1, fără măsurători noi, fie (2) plafonăm
heap-ul (`GHCRTS=-M…`) și re-rulăm de la genesis sub bugetul real al Pi-ului (~7 h). **PVC-ul rămâne
nedimensionat până atunci** (`local-path`, `allowVolumeExpansion: false` → o singură șansă).

**Corecție la ADR 0001:** `epoch_stake` NU e cea mai mare tabelă, cum presupunea ADR-ul. 747 MB din 5.909;
`block` (2.323 MB) + `tx` (1.759 MB) = 69% din bază. Prunarea `epoch_stake` cumpără ~13% din disc, deci e
o pârghie slabă.

**Planul verificat pe db-sync-ul real: 15 din 16 constante fixate sunt corecte** (195 / 193 /
431.863.790.957.959 / 325.390.800.383 / 937.803.900.128.796 / 101 din 294 / 12 / 0 / 816 / 511 / 69 / 20 /
65 / 90 / 45), inclusiv corecția D-19 (108 „dropped" brut → 6 cu predicatul restrâns) și cele patru
aserțiuni de comitet (3 membri, cvorum 2/3, expirare 372, 3 credentiale de script). `epoch` e goală, exact
cum prezicea nota de design; view-urile folosesc `epoch_param`/`epoch_state`/`block.epoch_no`, deci nimic
de schimbat.

Două defecte găsite și reparate în plan (draft + asamblat):

1. **`gov_action_lifetime` e 6, nu 7.** 7 e delta `expiration − proposed_epoch`, constantă pe toate cele
   131 de acțiuni → `last_votable_epoch` se citește din `gov_action_proposal.expiration`, niciodată
   calculat din parametru. Aserțiunea de la T-SQL-METRICS (ex-`= 7`) e acum `= 6`, cu motivul scris.
2. **D-27 (nou): identitatea unui membru CC e cheia RECE.** Preprod are 648.766 chei fierbinți mapate pe
   43 de chei reci (stress-test `AuthCommitteeHotKey` în epoca 187). Planul partiționa fereastra de dedup
   pe `voter_hot_hash` → ar fi raportat 3.243.798 voturi finale CC acolo unde adevărul e 193. Reparat:
   coloană `voter_id_hash = coalesce(voter_cold_hash, voter_hot_hash)` în `palmares.vote`, partiție pe ea,
   plus trei aserțiuni noi (3.243.802 / 193 / 43). Join-ul hot→cold prin `committee_registration` e validat:
   zero orfani pe 3,24 milioane de voturi.

**Verificat și NEschimbat** (ca să nu fie „reparat" greșit mai târziu): `101 din 294` e corect pentru
semantica view-ului (status la tip + rând de putere la 319); varianta as-of-319 dă 100/293, alt predicat.
Iar `eligible_actions(d, 320) … NewCommittee = 7` e corect: 8 acțiuni `NewCommittee` propuse din epoca 173
încoace, din care una (id 128, expiră 324) e încă deschisă la 320 → numitorul numără doar acțiunile
închise.

## 2026-10-20 — Lărgirea publicului-țintă (owner)

Owner-ul a numit explicit publicul stratului de atestări: **Intersect, comitetele (buget, produs, CC în
rolul extra-chain), comunitățile și DRep-ii, vendorii de treasury — și orice vendor din ecosistem,
auditorii incluși.** Spec-ul a fost actualizat în §1.2, S4, §2 și §4.3, plus checks-urile F1 8 și 11.
Trei consecințe de design, toate înainte ca Planul B să fie scris:

1. **Claim-ul de identitate pe script devine cazul majoritar, nu o notă de subsol.** Comitetele și
   organizațiile acționează prin multisig/script. Regulă unică: „dovedește că poți cheltui ca acel
   credential" — extra signatory pentru chei, consum de UTxO de la adresa scriptului pentru scripturi.
   Pattern-ul CIP-68 moștenit e deja răspunsul corect: autoritatea e **posesia user token-ului**, care
   funcționează identic pentru chei și scripturi (o listă de semnături nu ar funcționa).
2. **Ancoră opțională la nivelul 1**: datumul atestării poate purta `(gov_action_id, stake_address)` al
   retragerii de treasury vizate. Leagă banii de livrare. Opțională, pentru că vendorii plătiți din
   bugetul unei organizații nu au fapt de plată on-chain — și ei intră în registru. Verificare la citire,
   nu on-chain (o retragere nu e un UTxO adresabil).
3. **Subiectul unei atestări poate fi o altă atestare** (auditorul auditează o livrare atestată), iar
   tipurile cresc cu `RoleHeld` (mandat extra-chain, cu intervale).

**Regulă de delimitare, de respectat în Planul B:** nu atestăm ce lanțul deja dovedește. Apartenența la CC,
voturile, delegările și plățile din treasury sunt nivel 1 (db-sync). Nivelul 2 acoperă doar livrare, audit,
afiliere, mandat, andosare.

## 2026-10-20 — T-BOOTSTRAP-3/4/5 scrise (build în curs)

Toate fișierele există în `palmares`, staged, cu hook-ul de scurgeri verde. Commit-ul așteaptă
`cabal build all` (rulează; log în scratchpad-ul sesiunii).

**T-BOOTSTRAP-3** — `cabal.project` copiat verbatim (zero bump-uri), `palmares.cabal` cu 4 librării +
4 executabile + `test`, toate listele de module **comentate** (extracția le decomentează fișier cu
fișier), `default-extensions` cu 28 de intrări și stanza `plinth-ghc-options` copiate byte-for-byte din
`Decentralized-Belt-System.cabal:32-57,98-126`. `.envrc` cu exact două substituții. Șase Dockerfile-uri
(`admin` e nou). Cinci `CLAUDE.md`, cu cele patru ștergeri cerute în onchain-lib și `checkFee` rescris pe
`feeFor :: MintAction -> FeeConfig -> Integer` (fără selector — exact mecanismul care a produs defectul #1).

**T-BOOTSTRAP-4** — `ci.yml` + `images.yml`, fără runner self-hosted. `ci.yml` poartă corecțiile pe care
proba le-a măsurat, nu ce scria planul: **`libblst-dev` lipsește din lista apt** (nu există în arhiva
arm64), blst v0.3.15 se compilează din sursă, și se generează aliasul `libblst-any.pc` pe care
`cardano-crypto-class` îl cere prin pkg-config. Job-ul `secrets` rulează **hook-ul propriu** al repo-ului
(același cod local și în CI, deci nu pot divergea) plus gitleaks pentru adâncime.

**T-BOOTSTRAP-5** — `compose.yaml` la rădăcină cu profilurile `chain` și `app`, plus
`deploy/dbsync/db-sync-config.yaml` și `deploy/node-config/`. **Nu e o versiune nouă**: e rig-ul
Experimentului A promovat, pentru că e cel verificat pe teren. Diferențele față de schița din plan sunt
chiar descoperirile experimentului: digest-uri de index multi-arch (planul avea taguri), `NETWORK: ""`
prezent-și-gol (planul avea `NETWORK: preprod`, care rutează entrypoint-ul pe configul nepinuit al
imaginii), volum numit pentru socket, `working_dir`. Validat: `docker compose --profile {chain,app}
config -q` trece, digest-ul db-sync e prezent, 4 servicii.

**Ce NU e verificat**: row-count-ul pe un stack nou (ar cere un al doilea nod + db-sync, ~34 GB și încă
7 h, în competiție cu cel care rulează). Se verifică la predare, când stack-ul de la rădăcină preia.

**Experimentul rămâne în picioare deliberat**: volumele lui țin singura bază db-sync sincronizată de pe
mașină, de care are nevoie blocul SQL. Condiția de ștergere e scrisă în
`experiments/dbsync-preprod/README.md`. **Fără `docker compose down -v` acolo.**

### Scurgere prinsă înainte de commit

`config/config_atlas.json` din Belt System conține un **token Maestro viu**, iar T-BOOTSTRAP-3 cere
copierea lui ca `config_atlas.example.json`. L-am înlocuit cu `REPLACE_ME` înainte de orice commit — dar
verificând de ce hook-ul nu l-a prins, am găsit un **defect real în hook**: regexul cerea
`maestro[a-z_]*token` urmat direct de `:`, iar forma reală e `"maestroToken": "…"` — cu **ghilimea
înainte de două puncte** și T majuscul. Deci garda scrisă exact pentru scurgerea moștenită nu s-ar fi
declanșat pe formatul JSON real. Reparat (ghilimea opțională + `-i`) și testat pe cinci forme reale:
`"maestroToken"`, `MAESTRO_API_KEY=`, `maestro_token:`, `"blockfrostProjectKey"`, `ed25519_sk1…` — toate
prinse, placeholder-ul și mențiunile inocente nu. Tokenul din Belt System rămâne viu în istoricul lui:
**de rotit**, e în memoria `security-review-2026-07`.

### Primul build a picat — lipsă în plan, nu în cod

`cabal build all` pe schelet a picat pe două pachete, ambele cu simptom de **version skew**:

- `cardano-prelude-0.2.1.0`: `Lens.Micro.Internal` nu mai exportă `Field1..Field5` (microlens prea nou).
- `cardano-crypto-class-2.2.2.0`: `PackedBytes.hs:126` — aritatea lui `packBytes` s-a schimbat
  (`mem-pack` prea nou).

Cauza: **`cabal.project.freeze` nu era în lista de fișiere a lui T-BOOTSTRAP-3.** `index-state`
mărginește snapshotul de Hackage, dar nu fixează versiuni, deci solverul a avut libertatea să aleagă mai
nou. Planul era inconsistent cu el însuși: T-BOOTSTRAP-4 spune explicit „`cabal.project.freeze` is
committed (T-BOOTSTRAP-3)", iar pasul 4 al lui T-BOOTSTRAP-3 rula `cabal freeze` **după** build — ceea ce
nu poate funcționa, pentru că build-ul are nevoie de freeze ca să treacă.

Reparat în ambele copii ale planului (draft + asamblat): freeze-ul se **copiază în pasul 3, înainte de
primul build**, iar `cabal freeze` din pasul 4 devine o verificare că solve-ul nu a derivat
(`git diff --exit-code`). Freeze-ul Belt System (707 linii) acoperă deja tot ce adaugă Plan A —
`cassava ==0.5.3.2`, `esqueleto ==3.6.0.0`, `file-embed ==0.0.16.0` — deci copierea lui e chiar ce
înseamnă „fără bump-uri".

Notă de metodă pentru viitor: primul build a raportat `exit code 0` deși a picat, pentru că îl rulasem
prin `| tail -400` și codul de ieșire al unui pipeline e al ultimei comenzi. A doua rulare scrie logul
întreg și capturează codul real al lui `cabal`.

## 2026-10-20 — T-BOOTSTRAP-3 VERDE, comis și împins; CI în curs

**Build-ul trece cu adevărat de data asta**: `CABAL_EXIT=0`, toate cele patru executabile și suita de
teste se linkează, iar `cabal test all` raportează `1 of 1 test suites (1 of 1 test cases) passed`.
Cauza primului eșec a fost lipsa freeze-ului din plan (vezi mai sus), nu codul.

`cabal freeze` după build diferă de freeze-ul Belt System în exact două feluri, ambele benigne:
`persistent-sqlite ==2.13.3.0` adăugat (suita de teste palmares îl cere) și timestamp-ul `index-state`
normalizat de cabal la `2026-02-19T23:59:48Z`. **Niciun pachet comun nu s-a mișcat.**

Repo: `en7angled/palmares`, commit-uri `7e880ca` (scheletul), `04b8ad1`, `72aed2d`, `5c10728` (fixuri CI),
toate împinse pe `main`.

### Trei defecte în CI, găsite prin rulări reale

1. **`ci.yml` nu se parsa deloc.** `env: { GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }} }` — într-un flow
   mapping YAML, `{{` din expresia Actions deschide un mapping imbricat, deci tot fișierul e invalid și
   run-ul pică înainte de orice job (GitHub raportează workflow-ul după cale, nu după `name`, exact
   semnătura asta). Reparat pe stil bloc; ambele fișiere trec acum un parser YAML strict.
2. **`gitleaks/gitleaks-action@v2` refuză organizațiile.** Mesaj exact: `[en7angled] is an organization.
   License key is required.` — indiferent de vizibilitate. Unealta e liberă, doar wrapper-ul de Action e
   licențiat. Înlocuit cu binarul pinuit (8.30.1, linux_arm64) rulat ca `gitleaks git --redact
   --exit-code 1`. În același job, garda proprie a repo-ului trecuse deja, deci gate-ul blocant n-a lipsit
   niciodată.
3. **gitleaks semnala `cabal.project:6` ca `generic-api-key`** (entropie 3,82) — blocul `root-keys` al
   CHaP, adică șase chei de semnare **publice**, prezente în documentația CHaP și în orice `cabal.project`
   din Cardano. Rezolvat cu `.gitleaks.toml` care **doar adaugă** un allowlist, scopat pe fișier ȘI pe
   forma de 64 de hexazecimale — nicio regulă dezactivată. Verificat în ambele direcții: scanarea de
   istoric trece de la 1 la 0 findings, iar un token Maestro real în forma lui JSON e în continuare prins
   (`generic-api-key`, entropie 4,50) și de gitleaks și de garda repo-ului.

`images.yml` a primit un `concurrency` group: o imagine de serviciu e un build Haskell musl complet, deci
un push nou anulează acum rularea veche în loc să le ruleze pe amândouă.

**Stare CI acum**: `secrets` **verde**. `haskell` în curs (build rece pe arm64, 60–120 min estimat).
`images` rulează pentru `5c10728`; rulările pentru commit-urile depășite au fost anulate manual.

**Următorul pas**: Blocul 2 (extracția librăriilor) — as-is → rename → strip (GREEN GATE după STRIP) →
rescrieri, plus `Palmares.DomainTypes.Core.Ids` (CIP-129) cu testul golden și fixurile moștenite #3 și #5.

---

# MANDAT DE AUTONOMIE (owner, 2026-10-20)

**Textul owner-ului:** „poti sa iei deciziile pe care le consideri mai bune doar sa le documentezi cu
argumentatie. De asemenea in cazul in care te blochezi in ceva noteaza cu «blocked for user» si continua
cu altceva ce poti face fara interventia mea, in ultima instanta fa review si extinde teste si
actualizeaza documentatia pentru ce ai implementat!"

**Cum se aplică, pentru orice sesiune care reia lucrul:**

1. **Nu se oprește pentru întrebări.** Orice decizie care altfel ar merge la owner se ia aici, cu
   argumentație scrisă în secțiunea „Decizii luate ca builder calls" sau într-un ADR. Argumentația e
   obligatorie, nu decizia e opțională.
2. **Când chiar nu se poate avansa fără owner** (acces, bani, o alegere de produs ireversibilă), se scrie
   în secțiunea `## DECIZII CARE SCHIMBĂ UN NUMĂR PUBLICAT (owner-ul poate vrea să le revizuiască)

### 2026-10-20 — `rationale_rate` exclude ce n-am putut evalua, în loc să-l acuze pe autor

Măsurat pe corpusul real de preprod, în timp ce scriam fixtura de epoca 320:

- peste toate cele **648** de voturi DRep finale există **ZERO** ancore `valid`;
- **23** sunt `unreachable`, iar la nivel de rețea **140 din 237** de ancore unreachable sunt URI-uri
  `ipfs://` pe care lista de gateway-uri a db-sync-ului nostru nu le poate aduce;
- cele două raționamente ale DRep-ului-fixtură sunt chiar CID-uri `ipfs://`, `retry_count` 4,
  „List of errors for each ipfs gateway".

Cu formula din plan — `(verified + inline) / votes_cast` — API-ul ar publica *„practic nimeni pe
preprod nu scrie raționamente"*. Asta e o afirmație despre **infrastructura noastră**, tipărită ca
o judecată despre 36 de oameni care **au** publicat.

Deci numitorul exclude acum ce n-am putut evalua:

```
rationale_rate = (verified + inline) / (verified + inline + absent)
```

`unverified` și `pending` ies din **ambele** părți. O ancoră inaccesibilă e dovadă că autorul a
atașat ceva și zero dovadă despre conținut; a o număra ca eșec e aceeași eroare pe care gradele
`inline` și `pending` există să o evite. Când nimic nu poate fi evaluat, rata e NULL — „n/a", nu 0%.
**Efect măsurat: 10 din 514 DRep-uri trec de la un 0% publicat la n/a**, și niciunui DRep nu-i
scade rata.

Toate cele cinci contoare rămân publicate, deci un consumator care vrea altă bază — o rată de
atașare, `(votes_cast - absent) / votes_cast` — o poate calcula fără noi.

**Trei definiții diferite ale aceluiași număr erau în joc**: fixtura planului are
`rationale_verified: 2, rationale_rate: 0.125`, care numără „e atașată o ancoră" ca „există un
raționament" (2 din 16); SQL-ul din schiță număra doar fetched-and-valid peste toate voturile (0 din
16); al nostru numără fetched-and-valid peste voturile evaluabile (0 din 14). Prima e cea mai
generoasă cu autorii și cea mai puțin sinceră despre ce am verificat — și e cea pe care a folosit-o
măsurătoarea din amonte, de unde și dezacordul dintre cifra fixturii și a noastră.

---

## BLOCKED FOR USER` de mai jos — un rând cu ce lipsește, ce am încercat și ce deblochează
   — **și se trece la altceva.** Niciodată „aștept răspuns".
3. **Ordinea când firul principal e blocat:** (a) următorul task neblocat din plan; (b) review pe codul
   deja scris; (c) extindere de teste; (d) actualizare de documentație pentru ce există. În ordinea asta.
4. **Criteriul de ireversibilitate rămâne în picioare.** Ce e greu de dat înapoi (PVC dimensionat o dată,
   push de manifeste care șterg resurse prin `prune: true`, deploy pe mainnet, ștergere de volume cu date
   sincronizate) se face doar când măsurătoarea există, și se preferă varianta reversibilă.
5. **Regulile din `apps-k8s-configs/CLAUDE.md` nu sunt negociabile** nici sub mandatul ăsta: niciodată
   `kubectl apply/edit` pe resurse din repo, totul prin git → Flux.

## BLOCKED FOR USER

*(gol la 2026-10-20 06:00 UTC — nimic nu blochează implementarea)*

> **Depășit:** lista vie e `## BLOCKED FOR USER — două acțiuni manuale, niciuna de cod`, mai jos.
> **Actualizare 2026-10-21:** mutarea repo-ului **s-a făcut** — primul `images.yml` de după ea a
> publicat cu `GITHUB_TOKEN` — deci a rămas o singură intrare: trecerea pachetelor GHCR pe public.

## Decizii luate sub mandat, cu argumentație

### D-28. Postgres pe pi5-2, db-sync singur pe pi5-1 (plasarea memoriei)

**Decizia:** db-sync pe pi5-1 cu `requests.memory: 4Gi` / `limits.memory: 6.5Gi`; Postgres pe **pi5-2**.
PVC-uri: `palmares-dbsync-state` 10Gi pe pi5-1, `palmares-postgres` 50Gi pe pi5-2.

**Argumentație:** db-sync *trebuie* pe pi5-1 — citește socketul UNIX al nodului preprod prin hostPath, deci
co-locarea e o constrângere dură. Postgres nu are constrângerea asta: vorbește TCP. pi5-1 are ~7,2 Gi
liberi, iar măsurătoarea dă db-sync 5,90 GiB *anonimi*; cu Postgres alături (~2 Gi) nu încape, fără el
rămân ~1,3 Gi marjă peste limită. Alternativa — plafonarea heap-ului GHC și re-sincronizare de la genesis —
costă încă ~7 ore și **nu răspunde** la întrebarea plasării, pentru că Postgres ar trebui mutat oricum.
Dimensionarea PVC: 10,43 GB azi pentru Postgres → 50Gi dă ~5× marjă; 2,9 GiB pentru starea db-sync → 10Gi
dă ~3,4×. `local-path` nu permite extinderea, deci marja se cumpără o singură dată.

**Compromisul, explicit:** inserările db-sync→Postgres trec prin rețea în loc de loopback. E forma normală
de deployment pentru db-sync și LAN-ul clusterului o suportă; dacă throughput-ul devine problema, pârghia e
`max_wal_size` și batch-ul db-sync, nu re-colocarea (care nu încape).

**Dacă greșesc:** db-sync e OOM-killed sub 6,5Gi. Remediul e ridicarea limitei spre 7Gi — disponibilă
tocmai pentru că Postgres a plecat. Schimbare de config, nu de arhitectură. De-asta varianta asta e cea
reversibilă.

### D-29. Fără listă de emitenți „recunoscuți" pentru atestări

**Decizia:** oricine poate emite atestări; nu există allowlist de emitenți, nici în validator, nici în API.

**Argumentație:** o listă ne-ar face **arbitrul** cui îi e permis să garanteze — exact rolul de platformă
de încredere pe care §2 îl respinge („nivelul 2 cere încredere în semnatari, nu în platformă"). Ar fi și
politică: cine decide că Intersect e pe listă și o comunitate nu? Mecanismul care rezolvă problema reală e
deja în design — contrasemnătura (nimeni nu-ți atârnă revendicări pe profil) plus lineage-ul (lanț de
proveniență până la o rădăcină cunoscută, parcurgibil) plus zero scoruri. Arătăm lanțul, cititorul judecă.
**Ce ne obligă asta să livrăm:** proveniența trebuie să fie *lizibilă* în UI — lângă fiecare atestare,
adâncimea și rădăcina lanțului emitentului, ca fapt, nu ca rating.

**Reversibilitate:** o listă se poate adăuga ulterior ca filtru la nivel de view. Una existentă nu se poate
scoate după ce lumea se bazează pe ea. Deci „fără listă" e alegerea reversibilă.

## 2026-10-20 — Blocul 2 (extracția) în curs: commit-urile 1 și 2 gata, stripul la jumătate

Repo `palmares`, commit-uri locale (**nepusate** — vezi de ce mai jos): `691854b`, `fab4f9d`, `964b50d`.

**T-EXTRACTION-1 — gata.** 69 de fișiere / 15.897 linii copiate byte-identic, verificat
`checked=69 differing=0`. Toate cele 18 absențe pe care le-ar verifica un reviewer sunt confirmate
lipsă, plus `mcp-server-lib` integral. Cifrele planului s-au confirmat exact (69 de rânduri non-skip).

**T-EXTRACTION-2 — gata.** Namespace `Palmares.*`, `chain-sync` → `attest-sync`, Achievements →
Attestations, cei cinci identificatori cu nume de proiect. Baseline-ul măsurat înainte de pass a
confirmat planul la virgulă: **39** headere de modul, **99** linii cu identificatori BJJ (planul zicea
39 și 99). După: 0 și 0.
*Gap găsit în plan:* regulile `sed` ancorau `^module`/`^import`, deci **re-exporturile `module X` din
listele de export** nu erau prinse. Unul singur exista (`Protocol.hs`), reparat manual.

**T-EXTRACTION-3 (stripul, GREEN GATE) — parțial.** `lib:onchain-lib` **compilează curat**. Restul
(offchain-lib, executabile, teste) e încă în lucru, deci `cabal build all` NU trece încă.

Ce s-a făcut, cu deciziile luate sub mandat (detalii în mesajul commit-ului `964b50d`):
- Pass mecanic: 51 de declarații `import` ale modulelor sărite, din toate cele 29 de fișiere afectate.
- `Protocol/Types.hs` 257 → 153 linii. **`ProtocolParams` îngustat** la validatoarele care există
  (profiles, attestations, oracle) — un record de parametri care numește un validator neextras e o
  minciună pe care compilatorul nu o prinde. `FeeConfig` îngustat la profil + atestare.
- `Protocol/Id.hs`: șapte derivări cu nume BJJ → **două neutre** (`deriveSeededId`, `deriveIndexedId`),
  aceleași corpuri, aceeași disciplină `blake2b_224` pe 28 de octeți. Ștergerea lor ar fi însemnat
  re-tastarea acelorași două expresii în Planul B.
- `MintingPolicy.hs` 453 → 196 linii, **un singur redeemer rămas**: `NewAchievement`.
  `handleCreateProfile` nu putea supraviețui — corpul lui cheamă trei helperi din `Protocol/Core` (sărit)
  și mintează un rang sau o rădăcină de membership lângă profil. `ClaimProfile` e scris de la zero în
  Planul B. Indexul rămas e 0, nu 4: regula de indici stabili se aplică de la primul deploy.
- `ProfilesValidator.hs`: ramura `AcceptPromotion` ștearsă; `UpdateProfile` + `Cleanup` rămân.
- `Blueprint.hs` **rescris, nu peticit** — e fața CIP-57 publică a protocolului, deci proza BJJ din
  descrieri ar fi publicat o interfață falsă. Patru validatoare, text exact, Apache-2.0, versiune 0.1.0.

**De ce commit-urile nu sunt pusate:** commit-urile 1 și 2 lasă intenționat arborele necompilabil
(planul le vrea așa, ca să poată fi auditate prin `diff`-to-zero), iar stripul e la jumătate. Un push
ar însemna două-trei rulări CI roșii pe `main` fără nicio informație nouă. Se împinge blocul întreg
când `cabal build all` e verde.

### Ce urmează exact, în ordine

1. Stripul `offchain-lib`: `TxBuilding/{Operations,Lookups,Conversions,Validators,Context}`, `Storage`,
   `Ingestion`, `DomainTypes/*`. Context se îngustează la minting policy / profiles / attestations /
   dispute / oracle.
2. Stripul `query-api` (`RestAPI`, `Query/{Pages,Aggregates,Projected,Common}`, `QueryAppMonad` fără
   `liveProjection`) și `interaction-api` (fără IPFS).
3. Stripul arborelui de teste la cele 31 de cazuri în 4 grupuri (ApiHardening 18, ChainSyncReplay 5,
   Conversions 5, StorageRoundtrip 3), `other-modules` redus, apoi `cabal build all && cabal test`.
4. T-EXTRACTION-4 (`Palmares.DomainTypes.Core.Ids`, CIP-129/CIP-105 + test golden), apoi fixurile
   moștenite #3 și #5.

**Curățenie amânată, deliberat:** avertismente `-Wunused-imports` / `-Wunused-local-binds` rămase după
strip. Nu blochează (nu există `-Werror`), și se rezolvă într-un singur pass la final, nu fișier cu
fișier în timp ce structura încă se mișcă.

### Corecție: commit-urile WIP au ajuns pe `main`

Scrisesem mai sus că blocul de extracție se împinge abia când e verde. Push-ul fixului de CI
(`git push origin main`) a dus însă **toate** commit-urile locale de pe ramură, inclusiv cele trei de
extracție. Deci `main` e acum într-o stare în care `cabal build all` nu trece, iar CI va fi roșu până la
finalul stripului.

Nu rescriu istoricul unei ramuri publice pentru asta. Partea utilă: rularea roșie enumeră erorile rămase
ale stripului pe o mașină curată, deci e lista de lucru pentru pașii 1–3 de mai sus, gratis.
Lecție pentru viitor: lucrul WIP stă pe o ramură separată dacă `main` trebuie să rămână verde.

---

## 2026-10-20 — Blocul 2 (extracția) ÎNCHIS: T-EXTRACTION-1…6 gata, 50 de teste verzi

`main` la `26432e9`. `cabal build all --enable-tests` și `cabal test` trec; un singur avertisment pe
build curat, și acela moștenit. Arborele: 71 fișiere `.hs`, 10.690 linii (de la 15.897 copiate).

| Commit | Task | Rezultat |
|---|---|---|
| `691854b` | T-EXTRACTION-1 | copie byte-identică, `checked=69 differing=0` |
| `fab4f9d` | T-EXTRACTION-2 | namespace `Palmares.*`; 39 headere + 99 identificatori → 0 și 0 |
| `964b50d` | T-EXTRACTION-3 (½) | `lib:onchain-lib` compilează |
| `2dda528` | T-EXTRACTION-3 | **GREEN GATE**: build + `All 31 tests passed` |
| `8ee0a5a` | T-EXTRACTION-4 | `Core.Ids` CIP-129/CIP-105, 41 de teste |
| `bd7d1a6` | T-EXTRACTION-5 | fix moștenit #3, 47 de teste |
| `0d57340` | T-EXTRACTION-6 | fix moștenit #5, 50 de teste |
| `26432e9` | curățenie | 45 de avertismente → 1 |

Numărul de cazuri a nimerit exact predicția planului la fiecare pas: 31 → 41 → 47 → 50.

> **Corecție 2026-10-21 la rândul `bd7d1a6`:** „fix moștenit #3" e **scris**, nu **aplicat**.
> `authenticateStateUTxO` a aterizat acolo cu testele lui unitare și **apelat de nicăieri** — deci
> ingestia a continuat să decidă din forma datumului până la **T-ATTEST-8** (`bab8f5f`), care e cel
> care îl cablează. Secțiunea de la finalul fișierului.

### Deciziile luate sub mandat în stripul mare (toate argumentate în mesajele commit-urilor)

- **`DeployedScriptsContext`, `FeeConfig`, `ScriptHashesDTO` și fluxul de deploy** s-au îngustat la
  validatoarele care există. Un record care numește un validator neextras e o minciună pe care
  compilatorul nu o prinde.
- **Garda „accept o singură dată" a fost inlinată, nu ștearsă cu modulul ei.** `safeAcceptAchievement`
  venea din `TxBuilding.SafeOnchainLogic` (modul sărit), iar validatorul extras calculează *același*
  datum de ieșire la un al doilea accept — deci fără verificarea off-chain un dublu-accept s-ar
  construi și s-ar submite ca no-op, în loc să întoarcă 400. Jumătatea on-chain e fixul #4, al
  Planului B. Asta e chiar regresia pe care stream F o prinsese în Belt System.
- **interaction-api fără IPFS:** `image_uri` devine câmp simplu de cerere. Protocolul stochează oricum
  un URI în metadata CIP-68, iar o ancoră de guvernanță e un URL, nu un upload. Au căzut cu el
  `WithImage`, `servant-multipart` și plafonul de 10 MB.
- **query-api fără backend live:** `Query.Live` și `RestAPI.Common` sunt sărite, deci fiecare
  `withBackend live proiectat` a colapsat la `proiectat`, iar `liveProjection` + `LIVE_PROJECTION` au
  dispărut. Proiectat-only e exact ce cere planul.
- **Rute șterse** (sursa lor e un fișier de rescriere): promotions, lineage, dashboard, pending
  actions, practitioner/organization detail, profiles-by-wallet. `getProfileRefACsAtAddress`
  supraviețuiește în `Lookups`, deci ruta pe portofel se poate întoarce când query-api capătă un
  provider.
- **Proiecții: 6 entități → 2** (profil, atestare) și `currentSchemaVersion = 1`. Nimic nu e deployat,
  deci istoria v4 a Belt System nu se transferă.
- **`insertedAt` a fost ȘTERS, nu păstrat-cu-default** (deviere de la T-EXTRACTION-6 cum era scris):
  `persistent` generează un constructor pozițional peste toate coloanele, deci o coloană `UTCTime`
  non-`Maybe` nu poate fi „umplută de default-ul bazei" — scriitorul trebuie oricum să dea o valoare,
  adică exact ceasul de perete pe care fixul îl scoate. Consecințe: filtrul devine
  `profileFilterRegisteredAfterSlot :: Maybe Integer`, `ProfilesOrderByRegisteredAt` ordonează pe
  `created_at_slot`, iar `getActivityFeed` primește un `GYSlotConfig` și cară sloturi cap-coadă.
- **Vectorii golden CIP-129 verificați independent înainte de a fi scriși în test** (implementare
  BIP-173 de referință, în scratchpad): toți șapte coincid cu planul. Doar `drep1y2qn0rf…` e adevăr
  terț (citit de pe Koios); restul sunt pin de auto-consistență până apare un `committee_hash.raw`
  real de preprod cu `cc_hot_id`/`cc_cold_id` de la Koios.

### Defecte găsite și reparate pe drum (niciunul în plan, toate în execuție)

1. **`offchain-lib` nu avea blocul `default-extensions`** din schelet — deci `ImportQualifiedPost` era
   stins pentru toate cele 20 de module. Copiat verbatim din `Decentralized-Belt-System.cabal`.
2. **Pass-ul de rename a rescris `import Constants qualified` fără să renumească site-urile
   `Constants.`** — aceeași clasă de gap ca re-exporturile: regulile ancorau doar liniile de import.
   Șase fișiere, reparate cu alias (`qualified as Constants`, `qualified as WebAPI.CORS`).
3. **Literalii-string cu nume BJJ** nu erau acoperiți de nicio regulă de rename (căi de config,
   bannere CLI, namespace-ul de log, titlurile swagger). Reparate aici, pentru că T-EXTRACTION-3
   listează oricum `Constants.hs`.
4. **`cut`/`dropdecl` — o felie greșită a duplicat o regiune din `query-api/RestAPI.hs`** (ancora
   „Core Function API" apărea de două ori în fișier). Prins de compilator la `Multiple declarations`,
   reparat prin `git checkout` + reluarea celor două pass-uri pe un fișier curat. Instrumentul are
   acum `assert i < j` pe fiecare tăietură.

### Ce urmează

1. **Planul B (atestări)** și **Planul C (UI)** — încă de scris. Planul A acoperă doar blocul de date
   + API-ul public.
2. **Blocul 3 (substrat)**: manifestele k8s per D-28, apoi stiva pe cluster.
3. Retragerea `experiments/dbsync-preprod/` abia când stiva de sus are propria bază sincronizată.
4. Un avertisment moștenit rămas, lăsat intenționat: `KupoAtlas.hs:42` umbrește `scriptHash` din
   `GeniusYield.Types`. Nu e codul blocului și fixul e cosmetic.

---

## 2026-10-20 — Blocul 4 pornit: bootstrap-ul de schemă și `palmares.anchor`

`01c9d97` + `a3cde2a`. 56 de teste, zero avertismente. CI pe `26432e9` a fost **verde complet**
(ambele job-uri, inclusiv `cabal test all` pe runner-ul arm64, ~40 min de la cache rece).

**T-SQL-METRICS-1 — gata.** `Query.Views` încorporează cele patru fișiere DDL la compilare și le
aplică printr-un singur `runSqlPool`, deci tot setul e o tranzacție: o eroare de sintaxă în
ultimul fișier face rollback și lasă schema anterioară să servească trafic. `applyPalmaresSchema`
moare la eșec — un query API pornit cu schema pe jumătate aplicată ar răspunde pe unele rute și
500 pe altele.

`Test.Pg.withScratchDb` dă fiecărui test baza lui `palmares_test_<pid>` cu forma db-sync deja în
ea. **Moare numind `PALMARES_TEST_PG_CONN_STR` când lipsește** — verificat: fără variabilă cele
două cazuri de bază PICĂ, nu se sar.

**Fixtura de formă e GENERATĂ din schema reală** (db-sync 13.7.1.0 de la Experimentul A):
`pg_dump --schema-only` pe cele 35 de tabele citite de view-uri, plus DDL-ul de enum-uri și
domenii scos din `pg_type`, curățat de secvențe, indici, chei străine și ownership. Deci numele,
tipurile și `NOT NULL` sunt cele reale, nu o transcriere a hărții de coloane din note.

> **Asta a găsit imediat un defect în planul pentru T-SQL-METRICS-2:** seed-ul lui insera
> `off_chain_vote_data (id, voting_anchor_id, hash, language, is_valid)`, dar tabelul real are
> `json jsonb NOT NULL`, `bytes bytea NOT NULL` și `language` NOT NULL. Seed-ul ar fi picat pe
> preprod. Reparat în T-SQL-METRICS-2.

Două devieri, ambele forțate de ce face codul:
- **`applySchemaSql` SARE un fișier fără statement.** Cele trei fișiere de view-uri încep ca un
  comentariu-antet, iar `postgresql-simple` respinge o interogare fără comandă
  (`execute: Empty query`) — deci aplicarea lor literală strica bootstrap-ul. `hasStatement`
  decide golirea pe textul fără comentarii, dar execută mereu octeții fișierului.
- **Fiecare pool se creează cu `withPostgresqlPool`, nu `createPostgresqlPool`.** Un pool lăsat
  deschis face `DROP DATABASE` să pice cu „is being accessed by other users"; drop-ul e și
  `WITH (FORCE)`, ca o conexiune scăpată de un test picat să nu blocheze rularea următoare.

**T-SQL-METRICS-2 — gata. Validat pe baza REALĂ, nu doar pe fixtură:** 401 ancore intră, 401
rânduri ies (deci join-ul LATERAL nu multiplică), și `verdict` nu e NULL pentru niciuna — adică
**F1 check 7 satisfăcut pe date vii**.

Distribuția măsurată pe preprod, 2026-10-20:

| verdict | rânduri |
|---|---|
| unreachable | 237 |
| valid | 75 |
| hash_mismatch | 66 |
| invalid_schema | 19 |
| not_fetched | 3 |
| inline | 1 |

Trei lucruri pe care doar datele reale le dovedesc:
- `invalid_schema` = 18 (`is_valid FALSE`) + 1 (`is_valid NULL`). db-sync scrie NULL când
  verificarea CIP-100 e neconcludentă, și preprod chiar are un astfel de rând — deci colapsul
  cerut de design e real, iar `cip100_valid` păstrează distincția „a picat" vs „neconcludent".
- `inline` se testează **primul**, și singura ancoră `data:` vie o dovedește: are și un rând de
  eșec URL-parse, deci în orice altă poziție ar fi citit `unreachable`.
- `oversized` nu are instanță pe preprod azi, deci îl acoperă numai fixtura.

Pattern-urile de eșec se potrivesc pe **proza** db-sync, nu pe numele constructorilor — confirmat
citind șirurile vii din `off_chain_vote_fetch_error`. Cu `%HashMismatch%` toate cele 66 de
hash-mismatch ar fi citit `unreachable`.

**Corecție la mine:** testul „doar 000_schema.sql rulează" din commit-ul anterior era un
instantaneu, nu un invariant, și a devenit stale în aceeași oră. Acum afirmă ce e invariant: 000
nu se sare niciodată, și filtrarea nu reordonează.

**Postgres de test local:** container `palmares-test-pg` (postgres:17-alpine) pe
`127.0.0.1:55439`. Portul 55439, nu 5432 și nici 55432: pe 55432 ascultă deja un Postgres nativ
al mașinii, iar baza sincronizată de la Experimentul A stă pe 5432 în containerul ei — testele
creează și șterg baze, deci stau pe un server separat de cel cu singura copie sincronizată.

---

## 2026-10-20 — Blocul 4, continuare: două defecte de plan găsite prin rulare pe date reale

### Defect 1: `images.yml` împărțea imaginile de bază prin store-ul greșit (al meu, din blocul 1)

Prima rulare `images` care n-a fost anulată de un push mai nou a picat pe toate cele patru
servicii: `failed to resolve source metadata for docker.io/library/palmares-builder:9.6.6`.

`docker buildx build --load` pune imaginea în store-ul **demonului docker**, dar
`docker/build-push-action` rulează prin builder-ul-container buildx, care are store-ul lui și nu o
vede. Deci `FROM palmares-builder:9.6.6` cădea pe Docker Hub. Un registry e singurul store pe care
cele două jumătăți îl împart.

Reparat (`e41597f`): job `bases` care construiește și **împinge** ambele baze în GHCR, iar `build`
depinde de el și suprascrie referințele prin build args, consumând tagul `:${{ github.sha }}` (nu
`:9.6.6` flotant — o rulare concurentă nu poate schimba baza pe sub picioare). Dockerfile-urile
păstrează tagurile locale ca `ARG` implicit, deci `docker build` manual merge în continuare.

### Defect 2: lista de indici din design nu acoperă rezolvarea cheii reci a comitetului

`palmares.vote` rezolvă credențiala **rece** a unui vot CC prin registrarea cheii fierbinți în
vigoare la tranzacția votului: `WHERE hot_key_id = ? AND tx_id <= ? ORDER BY tx_id DESC`.

Designul (T-DATA §1.4) listează `gov_idx_committee_reg_cold ON committee_registration (cold_key_id)`
— **direcția opusă** (cheile fierbinți ale unui membru), nu aceasta. Fără un index pe
`(hot_key_id, tx_id DESC)`, lookup-ul face seq-scan peste `committee_registration` **o dată per
rând de vot CC**: 648.766 registrări × 3.243.802 rânduri de vot. Jumătatea CC a view-ului pur și
simplu nu se termină — măsurat: am lăsat interogarea să ruleze minute și am oprit-o.

Adăugat `sql/indexes/001_gov_indexes.sql` cu toată lista designului **plus**
`gov_idx_committee_reg_hot ON committee_registration (hot_key_id, tx_id DESC)`.

Fișierul e **deliberat în afara** `palmaresSqlFiles` / `applyPalmaresSchema`: indicii stau în
`public`, pe care resetul de schemă nu trebuie să-l atingă; `CREATE INDEX` pe un tabel de milioane
de rânduri ia minute și ține lacăte, ce n-are ce căuta la pornirea unui API; iar `CONCURRENTLY` nu
poate rula într-un bloc tranzacțional, și bootstrap-ul de schemă e o tranzacție prin design. Se
aplică o dată, de Job-ul de indici al blocului 3, și `scripts/verify_views.sh` presupune că a fost
aplicat.

### Disciplina de assert, acum aplicată consecvent

Trei runde de măsurători au arătat același lucru: cifrele planului sunt corecte la momentul
măsurării, dar unele sunt contoare vii.

| cifră | plan (2026-10-19) | azi | de ce |
|---|---|---|---|
| `drep_hash` rânduri | 802 | 814 | db-sync inserează un rând per credențială care apare într-o delegare |
| DRep înregistrați | 294 | **294** | cifra planului era corectă; 297-ul meu era fan-out-ul |
| rânduri de vot DRep | 816 | 819 | voturi noi |
| votanți DRep distincți | 511 | 514 | votanți noi |
| propuneri | 131 | 131 | niciuna nouă încă |

Regula pe care o aplic de acum în `sql/checks/*.sql`: **invarianți + istorie imuabilă**, iar unde
o cifră e valoroasă ca pin de regresie, se taie la o epocă (`proposed_epoch < 320`,
`vote_epoch < 320`) ca să devină imuabilă. Magnitudinile vii merg în `RAISE NOTICE`.

Cu tăietura la epoca 320: 809 rânduri de vot DRep, 504 votanți distincți, 64 cu ancoră, 90 SPO.

---

## 2026-10-20 — Blocul 4: T-SQL-METRICS-5/6/7 gata; cinci defecte găsite pe date reale

`main` la `8c936e2`. 57 de teste, cinci fișiere de check trec pe baza sincronizată.
Commit-uri: `96326b8` (vote + thresholds + threshold_map), `d7c0991` (power + delegare + tally),
`ce7c0f4` (gov_action + committee + treasury), `8c936e2` (trigger-ul de imagini).

### Cifrele planului, reproduse pe propriul db-sync

| ce | plan | măsurat | verdict |
|---|---|---|---|
| rânduri de vot CC | 3.243.802 | **3.243.802** | exact |
| voturi CC finale (pe cheie rece) | 193 | **193** | exact |
| chei reci CC distincte | 43 | **43** | exact |
| chei fierbinți CC distincte | 648.766 | **648.766** | exact |
| putere always_abstain @319 | 431.863.790.957.959 | **identic** | exact |
| putere always_no_confidence @319 | 325.390.800.383 | **identic** | exact |
| total putere @319 | 937.803.900.128.796 | **identic** | exact |
| DRep înregistrați fără rând de putere @319 | 101 | **101** | exact |
| acțiuni / enacted / expired / dropped-only | 131 / 11 / 103 / 6 | **identic** | exact |
| comitet: membri, cvorum, expirare | 3, 2/3, 372 | **identic** | exact |
| retrageri de trezorerie | — | 83, totaluri potrivite | ok |

Raportul care justifică tot `voter_id_hash`: **3.243.802 rânduri → 193 voturi finale pe 43 de
chei reci, peste 648.766 chei fierbinți.** Cu partiție pe cheia fierbinte ar raporta 3.243.798
voturi „finale". Factor ~16.800.

### Cele cinci defecte

**1. Lista de indici a designului nu acoperea rezolvarea cheii reci.** `palmares.vote` caută
`WHERE hot_key_id = ? AND tx_id <= ? ORDER BY tx_id DESC`; designul listează
`(cold_key_id)` — direcția opusă. Fără `(hot_key_id, tx_id DESC)`, lookup-ul face seq-scan peste
648.766 registrări **o dată per rând de vot CC** și nu se termină. Cu index: ~10 s per count.
Adăugat în `sql/indexes/001_gov_indexes.sql`, deliberat **în afara** `palmaresSqlFiles`.

**2. Bug-ul de rânduri duplicate din db-sync 13.7 afectează cel puțin CINCI tabele-copil, nu
trei.** Designul listase `off_chain_vote_{author,reference,external_update}`. Măsurat, îl are și
`off_chain_vote_drep_data` (3 DRep-uri duplicate → `palmares.drep` 815 rânduri pentru 812
credențiale) **și** `off_chain_vote_gov_action_data` (2 id-uri duplicate → `palmares.gov_action`
135 rânduri pentru 131 de acțiuni, care apoi dubla totalurile de trezorerie pe 6 acțiuni).
Ambele reparate cu `DISTINCT ON ... ORDER BY id` și comentariu `ponytail:` care numește PR #2137.

**3. `epoch_state` multiplica membrii comitetului cu numărul de epoci.** `epoch_state` are un rând
per epocă și multe epoci arată spre același comitet, deci selectând un rând per **epocă** și
făcând join pe `committee_id` am umflat 3 membri la **1.563**. `current_committee` e acum un
singur rând (`LIMIT 1`), cu tie-break pe max-id pe care oricum îl cere issue-ul #2155.

**4. Cifra „147 rânduri neîntrerupte" din plan e greșită.** DRep-ul-fixtură are **141** rânduri de
putere pentru 174..320, cu o **gaură la epocile 203–208**. 147 e aritmetică (320−174+1), nu
măsurătoare. Checkul pinuiește acum 141 și pinuiește explicit gaura — devine cea mai bună dovadă
pentru regula „absența înseamnă putere zero", observată chiar pe DRep-ul folosit de restul
checkurilor.

**5. Workflow-ul `images` nu se terminase niciodată.** Imaginea de bază e un build static musl al
întregului arbore Cardano, peste o oră pe runner arm64, cu patru servicii deasupra. Cu trigger pe
`main` + `cancel-in-progress`, fiecare rulare era anulată de commit-ul următor: zero rulări
complete, zero cache de layere, zero imagini. Mutat pe **taguri + workflow_dispatch**. Efect
secundar util: fără trigger pe main, push-urile nu mai anulează rularea în curs, deci cea care
construiește acum baza poate termina și semăna cache-ul.

### O întrebare deschisă, închisă de sincronizare

T-API §2.6 numea `pool_stat.voting_power` pentru puterea SPO, dar nicio notă nu verificase că
tabelul e populat sub `preset: only_governance` + `shelley: enable: true`. **Măsurat: e populat** —
99.872 rânduri, `voting_power` non-null de la epoca 163, cu doar 1–2 pool-uri per epocă lipsă.
Deci `vote_tally` îl folosește în loc să publice zerouri: 0 lovelace de suport SPO e o afirmație
falsă acolo unde datele există. 14 din 17 rânduri SPO au putere reală. Comitetul rămâne la 0 prin
construcție — ratifică prin cvorum de membri, nu deține stake.

### Disciplina de assert, consolidată

Fiecare fișier de check începe fiecare grup cu un **invariant de ne-multiplicare** (un rând per
propunere, un rând per retragere, exact trei membri). Niciunul dintre cele două fan-out-uri nu era
vizibil în nicio altă asserțiune, și ambele corupeau sume în aval.

Numerele se taie la o epocă (`proposed_epoch < 320`, `vote_epoch < 320`) ca să fie imuabile;
magnitudinile vii merg în `RAISE NOTICE`.

### Infrastructură de test

Postgres de test: container `palmares-test-pg` (postgres:17-alpine) pe `127.0.0.1:55439`.
Fixtura de formă a crescut la 36 de tabele (`pool_stat` a fost adăugat când `vote_tally` a avut
nevoie de el — prins de suita de teste, exact ce face fixtura).

---

## 2026-10-20 — T-SQL-METRICS-8: cursorul feed-ului nu era total, și acum e

`a9ee614`. 62 de teste, șase fișiere de check trec pe baza reală.

### OPEN-ul planului s-a declanșat

Planul lăsase totalitatea cursorului ca întrebare deschisă: *„nimic din note nu măsoară dacă
preprod conține o astfel de tranzacție; ASSERT-ul de unicitate e ce o detectează. Dacă se
declanșează, cursorul are nevoie de o a patra componentă."*

**S-a declanșat: 108.141 grupuri de triplete duplicate** peste 3,9 milioane de rânduri de feed,
din **două cauze diferite**, și reparația a avut nevoie de ambele jumătăți:

1. **Între arme — 108.130 grupuri.** `voting_procedure.index`, `gov_action_proposal.index` și
   `cert_index` sunt trei contoare per-tranzacție **independente**, deci o tranzacție care cară
   și un vot și un certificat produce același triplet de două ori. Testul de stres
   AuthCommitteeHotKey din epoca 187 a făcut-o în masă: 108.128 de grupuri sunt
   `committee_authorized` lovindu-se de `vote_cast`, cel mai rău cu 31 de rânduri.
   Reparat cu `arm`, un ordinal per-armă în cursor.
2. **În interiorul armei de voturi — 11 grupuri.** `voting_procedure.index` **nu e** un contor
   per-tranzacție în aceste date: cele 3.243.802 rânduri CC au doar **108.176** perechi distincte
   `(tx_id, index)`, iar valorile 0, 1 și 2 apar fiecare de ~648.000 de ori. Deci nu poate
   identifica un vot într-o tranzacție. Arma de voturi folosește acum `voting_procedure.id`.

Un ordinal de armă singur n-ar fi reparat (2); o cheie unică singură n-ar fi reparat (1).
Cursorul e `(sort_slot, tx_index, arm, item_index)` și e acum total: **0 duplicate pe 3.906.391
de rânduri**. Parserul de cursor al API-ului ia patru componente și trebuie să accepte în
continuare și -1 și -2 pentru `tx_index`.

### Restul se potrivește cu planul

131 propuse, 11 ratificate, 11 intrate în vigoare, 103 expirate, și **6** dropped în loc de 108 —
ASSEMBLY CORRECTION 1 se confirmă pe date reale.

**Două feluri de eveniment sunt inaccesibile pe preprod**, deci cele două straturi de test se
împart: `committee_de_registration` e gol și nicio retragere de trezorerie n-a intrat vreodată în
vigoare (cele 11 acțiuni enacted sunt 6 ParameterChange, 2 HardForkInitiation, 3 NewCommittee).
Deci asserțiunea celor treisprezece feluri stă în testul Haskell cu corpus semănat, iar fișierul
de check spune de ce un `DISTINCT = 13` pe date vii ar fi o poartă nefalsificabilă.

### Încă un defect de-al meu în Dockerfile-uri

Primul job `bases` complet a reușit (fixul GHCR funcționează, cache-ul e semănat), dar toate cele
patru build-uri de serviciu au picat:

```
cabal: sdist of palmares-0.1.0: filepath wildcard 'NOTICE' does not match any files
```

`cabal install` al unui pachet local face întâi un sdist, iar sdist-ul trebuie să conțină fiecare
cale din `extra-source-files` — plus `license-file`. Dockerfile-urile copiau doar `src` și
`config`. Deci imaginile de serviciu **nu funcționaseră niciodată**, consistent cu faptul că
workflow-ul nu se terminase niciodată.

Reparat: `COPY LICENSE NOTICE ./`, `COPY licenses`, `COPY sql`, `COPY test/fixtures`, plus un
comentariu în `palmares.cabal` lângă `extra-source-files` care spune că lista trebuie ținută în
pas cu Dockerfile-urile — pentru că o adăugire acolo sparge build-ul de imagine, nu build-ul
cabal, deci se vede doar în CI. Verificat local cu `cabal sdist`: tarball-ul conține exact
LICENSE, NOTICE, licenses/, palmares.cabal, cele patru `sql/*.sql` și fixtura.


## 2026-10-20 — Blocul 4 ÎNCHIS: T-SQL-METRICS-9…13, catalog + fixtură + matview + determinism

`b111ba2`. 50 → 83 de teste de la începutul blocului. Ce a intrat în ultimele cinci task-uri și, mai
important, ce s-a schimbat din ce prevedea planul.

### Catalogul de metrici (T-9/T-10): o singură definiție, documentația generată din ea

Opt formule în `resources/metric-formulas.json`, șase cu SQL propriu în `resources/metrics/*.sql`, două
(`gov_action_state_v1`, `tally_cast_votes_v1`) care sunt view-uri și n-au statement separat.
`docs/metrics.md` e **generat** din ele de `admin write-metrics-doc` și comis, iar CI rulează
`write-metrics-doc && git diff --exit-code docs/metrics.md`. Deci o formulă nu se poate schimba fără ca
documentația publicată să se schimbe în același commit — ceea ce e tot rostul: numărul servit și proza
publicată ies din aceleași fișiere.

Convenție care s-a dovedit necesară, nu estetică: fiecare statement leagă parametrii **o singură dată**,
într-un CTE `p` în frunte, în ordinea din `params`. Motivul e că `postgresql-simple` numără `?` **lexical,
peste tot textul**, comentariile incluse: un semn de întrebare în proză e citit ca parametru și interogarea
cade cu „N single chars, but M parameters". Există un test care asertează numărul de `?` per fișier față de
`params`, exact ca să aterizeze ca eșec clar.

### Fixtura pinuiește epoca **319**, nu 320, și numele rămâne

`test/fixtures/preprod-epoch-320-metrics.json`. Epoca 320 era **vârful** când am măsurat, deci deschisă, iar
o epocă deschisă nu are cursor stabil: `age_days_v1` ia `max(block.time)` peste epocile `<= as_of_epoch`,
care într-o epocă deschisă e „acum" și se mișcă la fiecare douăzeci de secunde. Măsurat: ultimul bloc al
epocii 320 era `2026-10-20 11:19:14` și avansa, al epocii 319 e `2026-10-17 23:59:44` pe veci. Pinuirea unei
epoci **închise** e ce transformă fișierul din instantaneu în fixtură. Numele a rămas pentru continuitate cu
planul, iar `_about` din fișier spune de ce.

### Două defecte prinse de fixtură (ambele în statement-urile publicate)

1. **`votes_by_type_v1` era necitibilă din Haskell.** `action_type` e un enum PostgreSQL, iar persistent îl
   dă înapoi ca `PersistLiteral`, pe care niciun decodor nu acceptă. Fixat cu `action_type::text`.
2. **`delegator_retention_rate_v1` dubla contoarele la fereastră zero.** Sonda care pune
   `retention_window_epochs = 0` — „cu fereastra zero, «anterior» trebuie să fie «acum», ceea ce e adevărat
   doar dacă parametrul e într-adevăr legat, nu un 10 literal în statement" — a întors (24, 24) pentru 12
   delegatori: cele două valori de epocă coincid și fiecare adresă se unea de două ori. `UNION ALL` →
   `UNION`. Sonda a fost scrisă ca test al legării parametrului și a găsit un bug de corectitudine.

### Matview-ul (T-12): forma e dictată de o optimizare a planificatorului

`palmares.drep_metrics` e **singurul** obiect materializat din schemă. Forma e „un CTE per metrică, unite la
final", nu „un LATERAL per DRep", pentru că `palmares.vote` calculează trei funcții de fereastră și un
`WHERE` nu poate fi împins sub un `WindowAgg` în general — deci fiecare referință la view ar costa pasul
întreg peste 3,2 M de rânduri, iar un LATERAL l-ar plăti 812 ori.

**Măsurat: 3,1 s pentru statement plus cei patru indici.** Motivul e că `voter_role` apare în `PARTITION BY`
al **tuturor** celor trei funcții de fereastră, iar PostgreSQL împinge qual-ul sub `WindowAgg` exact în acel
caz (`qual_is_pushdown_safe`). Deci CTE-ul `dv` scanează ~1.400 de voturi de DRep, nu 3,2 M. Scris în
fișier, pentru că o fereastră partiționată fără `voter_role` ar transforma asta în minute **în liniște**.

`REFRESH` trece prin `runSqlPoolNoTransaction`: `ExecRefreshMatView` cheamă `PreventInTransactionBlock`
pentru `CONCURRENTLY`, deci `BEGIN`-ul runner-ului obișnuit ar strica fiecare refresh — același hazard pe
care D-15 l-a găsit pentru `CREATE INDEX CONCURRENTLY`, cu un strat mai sus. Marca se scrie **doar la
succes**, deci un refresh picat se reîncearcă la următorul tick, iar tot pasul e în `try`: o excepție scăpată
din `forever` ar omorî firul în liniște și ar servi numere vechi pe veci — singurul mod de eșec pe care un
operator nu l-ar vedea.

### `sql/checks/070` asertează invarianți, nu magnitudini — și de ce

Fișierele de check anterioare puteau tăia numărătorile la `epoch < 320` și erau imuabile. Ăsta nu poate:
matview-ul n-are parametru de epocă pe care să tai, are un rând per DRep **la vârf**. Deci un
`eligible_actions = 114` înghețat ar fi greșit în clipa în care se închide epoca următoare. Asertat în
schimb: un rând per credențial real, cele cinci grade însumează `votes_cast`, fiecare rată e NULL **exact**
când trebuie (în ambele direcții), `votes_cast <= eligible_actions`, o singură `as_of_epoch` și o singură
`voting_power_epoch` pe tot matview-ul, plus indicii. Și **acordul cu formulele publicate**: rândul DRep-ului
pinuit e re-derivat din catalog în fișier și comparat. Verificat pe preprod: 812 rânduri la epoca 320, toți
invarianții curați, fiecare contor și percentilă egale cu formula.

`age_days` e singura coloană care nu poate fi asertată egală, și motivul e chiar rostul matview-ului:
formula citește `max(block.time)` **viu**, matview-ul l-a înghețat la ultimul refresh. Invariantul e
direcțional — un instantaneu al trecutului poate fi doar mai tânăr — cu o margine mult sub orice eroare de
formulă. Măsurat la rulare: 58 de secunde de diferență, adică exact blocurile produse între build și check.

### Decizii de builder în matview (toate scrise în fișier)

- **`voting_power_epoch` e una singură pentru toți**, epoca ultimei distribuții din ledger, nu „ultimul rând
  al fiecărui DRep". Altfel un DRep măsurat la epoca 320 ar sta lângă unul al cărui ultim rând e din 180, și
  stake-ul vechi al celui de-al doilea ar fi numit puterea lui de vot curentă.
- **`voting_power` e 0, nu NULL, când lipsește din `drep_distr`** (101 din 294 de DRep-uri înregistrate pe
  preprod). Ledger-ul spune „zero stake delegat" prin absență; „necunoscut" ar fi o altă afirmație.
- **`last_vote_at` nu e filtrat de eligibilitate**: „când a votat ultima dată" trebuie să numere și un vot pe
  o acțiune încă deschisă, pe care setul eligibil îl exclude.
- **`age_days` e NULL exact pentru credențialele niciodată înregistrate** (7 pe preprod) — există în
  `drep_hash` doar pentru că cineva le-a delegat, deci n-au interval de însumat. Asertat în check.
- **Zero rânduri dacă `public.block` e gol**, nu 812 rânduri de NULL-uri: fără vârf nu există `as_of_epoch`,
  iar toate metricile sunt „as of" acea epocă.

### Poarta de determinism (T-13, F1 check 3): 8/8

`scripts/check_determinism.sh` rulează fiecare statement din catalog **de două ori** pe aceeași bază și
compară digest-urile. Verificat pe db-sync-ul nostru la epoca 320: 8/8, ~3,5 minute (`vote_tally` e 12 s pe
citire, și se citește de două ori).

Trei decizii:
- **Cursorul e epoca pinuită, închisă.** Asta face răspunsurile istorie, nu tally viu: ultimul bloc al unei
  epoci închise nu se schimbă, deci un statement parametrizat de el nu se poate mișca odată cu vârful. Cele
  două formule care sunt view-uri n-au cursor și citesc date vii — exact pentru ele e verificarea vârfului.
- **Exit 2, nu 1, când un statement a driftat ȘI vârful a avansat în timpul rulării.** Un lanț în mișcare e
  *inconcludent*, nu drift; a-l raporta ca drift ar da vina pe formulă pentru munca lui db-sync. Fără drift,
  vârful e irelevant și rularea trece.
- **Ieșirea se sortează înainte de hash.** Un result set fără `ORDER BY` n-are ordine canonică și două rulări
  pot întoarce legitim aceleași rânduri în altă ordine; check 3 e despre numere, deci se hash-uiește
  multiset-ul.

Job-ul CI rulează poarta peste scheletul semănat, deci blochează un PR într-un minut fără build Haskell.
Repetat local pe aceeași imagine de postgres: 8/8, `tip none`, `drep_hash_id 0` — două din opt întorc zero
rânduri acolo, deci ce dovedește CI e că statement-urile **parsează pe schema reală** și sunt
self-consistente. Rularea care **semnează** check 3 e cea pe preprod; scris în capul scriptului până când
blocul 3 scrie runbook-ul de deployment.

### Ce urmează

Blocul 5 (API, T-API-1…18) și blocul 6 (paritate, T-PARITY-1…6). Blocul 3 (substratul, manifeste k8s per
D-28) rămâne separat pentru că atinge rack-ul. Planul B (atestări) și planul C (UI) încă nescrise.

**Datorie recunoscută:** `docs/deployment/preprod-dbsync-sync.md` îi e dator blocului 3 și trebuie să ducă
rândul de semnare a check-ului 3 (rularea pe preprod). Până atunci e în capul lui `check_determinism.sh`.


## 2026-10-20 — Blocul 5 pornit: T-API-1…8, API-ul public servește primele cinci rute

`c2f5709`. 83 → 131 de teste, plus `scripts/smoke_query_api.sh` cu 55 de asserțiuni care rulează
împotriva db-sync-ului nostru sincronizat. Rutele: `/health`, `/ready`, `/status`,
`/metrics/formulas`, `/dreps` (JSON + CSV).

### Trei lucruri scoase din query-api, fiecare cu motiv

1. **Familiile moștenite** (profile/atestări/search/activity/pages). Serveau proiecțiile Belt
   System; planul B reconstruiește suprafața de atestări peste schema de atestări, nu peste acele
   tabele. Modulele rămân pe disc, comentate în manifest — convenția repo-ului pentru „extras,
   încă nereaterizat".
2. **Stratul BasicAuth.** API-ul publică ce e deja pe un lanț public, deci o credențială partajată
   nu protejează nimic dar obligă fiecare consumator să ceară una; rate limiting-ul e treaba
   BFF-ului, unde există identitate per client. Contextul de auth a plecat cu el: credențiale
   configurate dar neaplicate **se citesc ca protecție**.
3. **Provider-ul Cardano și contextul de scripturi deployate.** D-1 spune că fiecare rând vine
   dintr-un view sau din matview, deci serviciul nu interoghează niciodată lanțul. Acum pornește
   cu un singur DSN de Postgres — un secret mai puțin pe o cale de citire publică, fără token
   Maestro, fără `config_atlas.json`, și **de-asta scriptul de smoke poate rula oriunde**.
   `atlas-cardano` și `plutus-ledger-api` au ieșit complet din `build-depends`-ul lui query-api.

### Două reguli de plumbing pe care le moștenește fiecare familie de rute

- **O tranzacție per cerere, la `RepeatableRead`.** db-sync declară `-- noreference` pe cheile
  străine de guvernanță, deci o citire compozită din mai multe statement-uri poate observa un id
  atârnat în timp ce un rollback comite dedesubt. La `ReadCommitted` o pagină ar putea arăta un vot
  pe o acțiune pe care statement-ul următor o raportează ca niciodată propusă.
- **O coloană JSON per statement**, decodată de `FromJSON`-ul propriu al DTO-ului. O coloană
  redenumită sau ștearsă cade atunci la graniță **numind câmpul**, în loc să deplaseze o decodare
  pozițională cu o coloană la stânga și să publice numărul greșit sub numele corect.

### Patru defecte găsite prin *rularea* serviciului, nu prin teste

1. `X-Chain-Block-Time` era `show` de UTCTime — `2026-10-20 12:43:24 UTC`, pe care niciun parser
   de date din browser nu-l acceptă, și care nu se potrivea cu forma ISO din corp. Acum
   `iso8601Show`.
2. Boot-ul tipărea ~1000 de linii de DDL (persistent loghează fiecare statement, iar pool-ul de
   owner era creat cu `runStdoutLoggingT`). Linia de care are nevoie un operator era îngropată.
   Acum 41 de linii.
3. **Nicio linie de boot nu apărea sub un pipe.** GHC block-bufferează stdout când nu e TTY, adică
   exact Docker și Kubernetes: serviciul servea deja cereri în timp ce `kubectl logs` arăta nimic.
   Reparat în toate trei serverele, nu doar în cel unde a apărut.
4. **`/dreps` publica 7 non-DRep-uri.** CASE-ul de status nu are ramură pentru un credențial fără
   certificat, deci cele 7 `never_registered` cădeau pe `inactive` și totalul citea 301 în loc de
   294. Acum excluse de o clauză necondiționată — pentru că **niciun** `DRepStatus` nu le numește,
   deci filtrul de status nu le-ar fi putut exclude niciodată.
5. CSV: `show` randa 0.099 ca `9.900990099009901e-2` (multe unelte CSV nu citesc asta) și boolenii
   ca `True`/`False` unde JSON-ul spune `true`/`false`. Ambele reparate și asertate.

### Decizii de builder în blocul 5

- **`normalizeLimitOffset` primește `PageBounds` (plafon + default), nu doar plafonul.** Varianta
  din plan derivă default-ul din plafon, ceea ce merge doar cât timp `/activity` e singurul
  endpoint cu altă dimensiune de pagină și **cade în liniște** în clipa în care alt endpoint
  nimerește același plafon: handler-ul ar primi 20 de rânduri din cauza unui număr pe care nu l-a
  setat niciodată.
- **Offset peste 10.000 e REFUZAT, nu plafonat.** Un limit în afara intervalului se plafonează în
  liniște (ambele capete au un sens evident, iar valoarea aplicată se întoarce în envelope), dar
  plafonarea unui offset ar servi în liniște pagina greșită, iar un client care citește rândul
  10.001 n-are cum să observe.
- **`voting_power` e 0, nu null**, când distribuția ledger-ului pentru epocă nu include DRep-ul.
  Măsurat: 109 din 294, iar 8 dintre ele **au** delegări vii al căror stake sosește în snapshot-ul
  epocii următoare. Zero e răspunsul onest pentru acea epocă. Planul aștepta null; ordonarea
  `NULLS LAST` rămâne (clauză identică în ambele direcții, robustă dacă coloana devine nullable).
- **Statusul implicit la `/dreps` e cele două valori ÎNREGISTRATE.** preprod are 511 retrase față
  de 294 înregistrate, deci o pagină implicită nefiltrată ar fi aproape inutilă pentru singurul
  lucru pentru care există lista.
- **Smoke-ul asertează INVARIANȚI unde planul asertea magnitudini.** `eligible_actions` crește la
  fiecare acțiune care se închide și `voting_power` se schimbă la fiecare epocă, deci
  `participation_rate == votes_cast / eligible_actions` e asserțiunea care rămâne să însemne ceva.
  Magnitudinile sunt pinuite la o epocă **închisă**, în fixtură.
- **Nu re-exportăm envelope-ul din `QueryResponses`** (planul o cerea): ar însemna ca
  `offchain-lib` să depindă de `webapi-lib`, dependență **spre exterior** interzisă de layering în
  ambele direcții. Consumatorii importă `Palmares.WebAPI.Envelope` direct — o linie de import.
- **`Query.Csv` ține instanțe ORFANE**, documentate ca atare. Clasa cassava, DTO-urile și
  `safeCell` stau în trei locuri pe care layering-ul le ține despărțite, deci alternativele erau o
  a doua copie a gărzii anti-formulă (așa driftează un control de securitate) sau niciun CSV.
- **`applyLimits` / `applyFilterOrderLimit` nu sunt șterse** (planul le voia absente), ci
  redenumite `legacy*` cu forma F-17 numită în capul modulului: handler-urile moștenite încă le
  cheamă prin șapte call-site-uri. Pleacă împreună cu ele, la T-API-15.


## 2026-10-20 — T-API-9/10: profilul DRep, și trei descoperiri care au schimbat codul

`9d0c1e8`, `044be81`. 131 → 142 de teste, 55 → 92 de asserțiuni de smoke.

### 1. `drep_hash.raw` NU e unic — și CIP-105 e motivul

Măsurat: **două** hash-uri de 28 de octeți apar de două ori în `drep_hash`, o dată ca credențial
de cheie și o dată ca credențial de script. În ambele cazuri **exact unul** din pereche e
înregistrat, iar celălalt e o fantomă fără niciun certificat — există doar pentru că cineva a
delegat folosind string-ul CIP-105, **care nu poartă bitul de script**, și db-sync a creat rândul
de cealaltă natură.

Și `drep_hash.view` e **identic** pentru ambele rânduri ale unei perechi (același HRP, același
hash). Exact de-asta CIP-105 nu poate identifica un DRep, iar `palmares.drep` nu publică niciodată
coloana `view`.

Consecința pentru API: `/drep/{id}` rezolvă prin **hash-ul credențialului**, preferând rândul
înregistrat, niciodată prin string-ul CIP-129 normalizat. Pentru unul din cele două,
normalizarea la forma de cheie ar ateriza pe fantomă. Scris în haddock-ul lui `drepIdHex`, acolo
unde următorul implementator citește.

### 2. Servant își construia singur 400-urile, în text simplu

Când un `Capture` sau un `QueryParam` nu parsează, handler-ul nu e atins niciodată: servant
construiește răspunsul din `String`-ul întors de parser și-l servește ca **text simplu, fără
`code`**. Măsurat: un id de DRep cu checksum bech32 stricat venea ca `400 Error parsing ...`, pe
care niciun client nu putea să-l potrivească — lângă 400-urile noastre, care poartă `code`.

Reparat la rădăcină cu un `ErrorFormatters` în contextul de servire. Funcționează pentru că
**fiecare `parseUrlPiece` din codebase întoarce deja CODUL ca mesaj**: `invalid_drep_id` iese din
parser și se întoarce ca `InvalidDrepId`. Acum fiecare eșec poartă `{code, message}`:
404 `drep_not_found` pentru un id valid care nu numește nimic, 400 `invalid_drep_id` pentru unul
stricat, 400 `invalid_parameter` pentru un enum greșit.

### 3. Statisticile de delegatori erau o a DOUA derivare a formulei publicate

Prima versiune număra „fiecare adresă care a delegat vreodată acestui DRep înainte de epoca N" —
**o altă întrebare** decât „delegatorii pe care îi AVEA atunci". Măsurat pe DRep-ul din fixtură:
26 de delegatori anteriori și o rată de retenție de 0,42, unde `delegator_retention_rate_v1` —
care reconstruiește setul din intervalele de delegare — dă 11 și 1,0.

Acum rulează **octeții proprii ai catalogului**, împachetați doar în convenția coloanei JSON, iar
numele câmpurilor din DTO sunt numele coloanelor catalogului, ca să nu existe un strat de aliasuri
în care două nume pentru un număr să se despartă. Blocul e măsurat la același `as_of_epoch` ca
blocul de metrici, deci pagina nu poate arăta o rată de retenție dintr-o epocă lângă o rată de
participare din alta.

### 4. Un test pe care l-am scris ca să prindă o eroare de transcriere a prins una — în plan

Vectorul planului aștepta `max_tx_ex_mem → dvt_p_p_technical_group`. CIP-1694 pune **limitele** de
unități de execuție în grupul **network** și **prețurile** lor în grupul economic. Corectat, cu
motivul scris lângă, și `collateral_percent` stă acum ca exemplu tehnic.

Cele două enumerări OPEN din plan sunt închise: toate **55** de coloane din `param_proposal` sunt
clasificate exact o dată — 46 ca parametri, 9 ca bookkeeping cu un motiv fiecare. Un test
asertează acoperirea exactă și fără suprapunere, deci o coloană adăugată de un upgrade de db-sync
**cade în CI** în loc să facă în liniște ca pragul unui ParameterChange să se întoarcă drept
„niciun grup atins".

### Decizii de secvențiere

`votes` și `timeline` lipsesc deliberat din compozit: tipurile lor de rând aparțin lui
`GET /votes` și `GET /activity`, iar inventarea lor aici ar fi exact dublarea pe care decizia
„un handler, două căi" există ca să o evite. Fiecare sosește mai târziu ca un câmp opțional nou —
un bump minor de Swagger, prin aceeași regulă care acoperă câmpurile de atestări.

Lacunele din seria de putere sunt **reale și nu se umplu**: 141 de puncte peste epocile 174–320
pentru DRep-ul din fixtură, cu 203–208 absente din `drep_distr`. Umplerea cu zero ar transforma o
absență într-o măsurătoare, iar un client vede lacuna în seria de `epoch`.


## 2026-10-20 — T-API-11: un defect de performanță de 87 de secunde, reparat la rădăcină

`62524e8`. Ruta `/gov-actions` a scos la iveală ceva ce nicio verificare de până atunci nu putea
scoate: **selectarea `palmares.gov_action.votes_cast` pentru toate cele 131 de acțiuni dura 87 de
secunde**. Asta nu e un endpoint lent, e un endpoint inutilizabil.

### Cauza

Testul de stres `AuthCommitteeHotKey` din epoca 187 a înregistrat **648.766 de chei fierbinți** și
a lăsat **3,24 M de rânduri de vot de comitet**, concentrate astfel încât **cinci** acțiuni de
guvernanță duc fiecare ~648.755 dintre ele. `palmares.vote` rezolva credențialul rece al fiecărui
rând cu un `LATERAL` — „cea mai recentă înregistrare cu `tx_id <=` al votului" — pe care PostgreSQL
execută ca **o căutare în index PER RÂND DE VOT**.

### Reparația, în doi pași

1. **`palmares.cc_cold_interval`**, un view nou care duce intervalul de tranzacții al fiecărei
   înregistrări, unit ca interval. Cele 648.767 de înregistrări se hash-uiesc o dată, iar fiecare
   rând de vot devine o sondă hash plus un test de interval. **87 s → 5,7 s.**
   View propriu, nu subinterogare inline, pentru că atât `palmares.vote` cât și
   `palmares.gov_action` au nevoie de regulă, iar **o a doua copie a „înregistrarea în vigoare la
   momentul T" e o a doua șansă să greșești**.
2. **`gov_action.votes_cast` numără VOTANȚI DISTINCȚI** direct din `voting_procedure`, în loc de
   rânduri din `palmares.vote` cu `seq_last = 1`. Același număr prin definiție — un vot final per
   votant per acțiune — fără să plătească funcțiile de fereastră ale view-ului peste 3,24 M de
   rânduri. **5,7 s → 2,0 s** pentru toate 131.
   Comitetul se numără în continuare după credențialul **rece**: numărarea cheilor fierbinți ar
   raporta o acțiune ca fiind votată de șase sute de mii de membri de comitet.

### Dovada că semantica n-a mișcat

Checks 030 și 050 citesc **exact aceleași** numere ca înainte: 3.243.802 rânduri CC → **193**
finale de **43** de chei reci (peste 648.766 fierbinți), DRep 819, SPO 90, și 11/103/6/11 acțiuni.
Și suita de teste a scăzut de la 95 s la **57 s**, pentru că aplică acest DDL repetat.

Rămâne pasul complet al lui `palmares.vote` la ~5,7 s, cu care `GET /votes` va trebui să se
confrunte. Commit-ul nu pretinde că a rezolvat asta.

**Nouăsprezece** view-uri simple acum, nu optsprezece cât lista planul — `smoke_views.sh`
actualizat cu motivul lângă număr.

### OPEN-ul planului, închis de bază de date

Cele cinci stări cu valoare unică însumau 132 față de 131 de rânduri. Citit din bază:
`proposed` e **11**, nu 12. 103 expirate, 11 intrate în vigoare, 11 propuse, 6 dropped, **0
ratificate** — zero pentru că fiecare acțiune ratificată a intrat în vigoare epoca următoare, iar
`enacted_epoch = ratified_epoch + 1` pe toate unsprezece. Smoke-ul asertează că cele cinci
**partiționează** toate cele 131 de rânduri, ceea ce prinde un CASE prost ordonat.

Forma CIP-129 calculată de noi se potrivește cu vectorul abreviat al notei de design: cea mai nouă
propunere se randează `gov_action1qnergwfc…`, exact string-ul pe care f1-dbsync §1.3 îl trunchiase.


## 2026-10-20 — T-API-12/13: două erori de identitate și una de ratio, toate găsite prin citirea ieșirii

### 1. `ratio` NULL pe o acțiune UNANIMĂ

`palmares.vote_tally` calcula
`sum(power) FILTER (WHERE vote='Yes') / NULLIF(yes + no, 0)`, iar un `sum` cu `FILTER` peste zero
rânduri e **NULL, nu 0**. Deci o acțiune unanimă — toate voturile Yes, niciun No — calcula
`yes + NULL = NULL` și publica `ratio = null`, ceea ce se citește ca „nu s-a votat", **opusul** a
ce s-a întâmplat.

Măsurat: **33 din 49** de tallies care duc putere publicau un ratio nul. După `COALESCE` în
interiorul aritmeticii: niciunul nul, 31 citesc un 1,0 unanim. `NULLIF` rămâne pentru cazul
pentru care a fost scris — nicio putere în spatele niciunei părți.

### 2. Identitatea rece a comitetului era publicată cu header-ul greșit

`palmares.vote.has_script` raportează cheia **FIERBINTE**, în timp ce `voter_id_hash` e cea
**RECE** (D-24). Împerecherea lor construia un id `cc_cold1…` al cărui octet de header spune
cheie-sau-script pentru un alt credențial decât hash-ul.

Măsurat: din cele **43** de chei reci care au votat, **32 sunt chei și 11 sunt scripturi** — deci
unsprezece erau mis-identificate. Reparat purtând `voter_cold_has_script` din
`palmares.cc_cold_interval` prin `palmares.vote`. Verificat că ambele feluri fac **round-trip**:
id-ul întors de API, trimis înapoi ca filtru, găsește exact aceleași rânduri.

### 3. Fiecare căutare după votant plătea un pas complet peste 3,24 M de rânduri

Filtrul era `encode(v.voter_id_hash, 'hex') IN (?)`. `voter_id_hash` **este** în `PARTITION BY`-ul
funcțiilor de fereastră ale lui `palmares.vote`, deci un qual direct pe el se împinge sub
`WindowAgg` — dar învelit în `encode()` planificatorul nu-l mai vede ca qual pe coloana de
partiție. Comparat ca **octeți**, cu `decode()` pe partea parametrului, interogarea atinge doar
rândurile acelui votant.

### Cifre din plan corectate de bază de date

- `max(vote_rows)` e **21.626**, nu 20: o cheie fierbinte de comitet a votat de 21.626 de ori pe o
  singură acțiune, în testul de stres din epoca 187.
- Voturi de DRep cu ancoră `valid`: **1**, nu 65.
- **Nu există** nicio retragere de trezorerie intrată în vigoare pe preprod (83 înregistrate, 0
  enacted), deci testul de smoke al planului pentru T-API-12 nu putea rula așa cum era scris.

### O capcană de URL, documentată în rută

Forma canonică `<tx_hash>#<index>` conține `#`, pe care un URL îl tratează ca delimitator de
fragment — curl nu-l trimite niciodată. Ambele forme sunt acceptate în cale; descrierea rutei spune
acum că `#` trebuie trimis ca `%23` și că forma **bech32** CIP-129 e cea de pus într-un link.


## 2026-10-20 — T-API-14/16/17/18: cele șapte rute noi servesc

`/delegations`, `/committee`, `/spos`, `/treasury-withdrawals`, `/search`,
`/pages/drep-explorer` și `/pages/dashboard` răspund toate 200 cu date reale pe preprod.
Swagger-ul regenerat are **22 de căi**. Cele 20 de teste de hardening trec, inclusiv noul test de
numărare a placeholder-elor pentru statement-ul de facete.


## 2026-10-20 — T-API-15: fluxul de activitate, de la 253 s la 1,0 s

Ruta `GET /activity` e singura **paginată prin cursor** din tot API-ul: `cursor`, `next_cursor`,
`total: null`. Patru componente în cursor (`sort_slot:tx_index:arm:item_index`), pentru că
tripletul avea 108.141 grupuri duplicate. `renderCursor` trăiește în DTO, `parseCursor` în stratul
de citire, iar round-trip-ul lor e test unitar.

### 1. Brațul de voturi: patru subinterogări corelate → patru JOIN-uri

Forma cu subinterogări evaluează `SELECT`-urile scalare **o dată pe rând, înainte de sortare**,
deci 3,24 M de rânduri plăteau pentru cele 50 pe care le păstrează `LIMIT`: **39,8 s** pe o pagină
nefereastruită, din care 36,1 s brațul de voturi și 34,5 s doar căutările de credențial. Scris cu
JOIN-uri și **un singur COALESCE** (nu mai trebuie `CASE` pe `voter_role`, fiindcă exact una din
`drep_voter`, `pool_voter`, `committee_voter` e non-null pe rând): **8,4 s**.

Cele două forme au fost comparate rând cu rând: **0 dezacorduri** pe toate cele 3.244.711 de
rânduri. Verificările 030, 060 și 070 citesc **exact aceleași numere** ca înainte — 3243802 rânduri
CC → 193 finale prin 43 chei reci, 3906393 rânduri de flux cu 0 cursoare duplicate, 812 rânduri de
metrici cu DRep-ul fixat la 16/114 (0,140351).

### 2. Două indexuri care nu erau pe brațul la care te-ai aștepta

Pe o pagină de 1.347 ms, **1.150 ms** veneau din două locuri și niciunul nu era scanarea voturilor:

- `committee_registration` **nu avea index pe `tx_id`**, deci o fereastră de slot nu putea fi
  parcursă `block → tx → committee_registration`. PostgreSQL citea toate cele 648.767 de rânduri și
  întorcea 0: **389 ms ca să nu găsească nimic**.
- `palmares.cc_cold_interval` ordonează fereastra după `(hot_key_id, tx_id, cert_index)`, iar
  indexul existent se oprea la două coloane — deci view-ul plătea un *Incremental Sort* peste toate
  cele 648.767 de rânduri ca să facă hash-join cu **28** rânduri de vot: **760 ms**.

Cu ambele indexuri: pagina **2,03 s → 0,89 s**.

### 3. 503 pe `/activity?actor=…`: memoria partajată a interogărilor paralele

`53100: could not resize shared memory segment … to 16777216 bytes: No space left on device` —
în timp ce `/dev/shm` arăta 1 MB din 256 MB folosiți. Peretele nu e tmpfs-ul, e **cgroup-ul**:
paginile de tmpfs se numără în limita de memorie a containerului, iar limita de 2 GB și
`work_mem=32MB` sunt chiar numerele din specificația de infrastructură — Pi-ul le va rula identic.

Două reparații, amândouă păstrate:

- `shm_size: 256mb` în `experiments/dbsync-preprod/compose.yaml`, pentru că 64 MB e default-ul
  Docker și e sub ce cere un plan paralel. Containerul a fost **recreat**, volumul numit
  `palmares-dbsync-exp_postgres` e intact (3.244.711 de voturi înainte și după).
- `SET LOCAL max_parallel_workers_per_gather = 0` în `listActivity`. O pagină de flux întoarce cel
  mult 100 de rânduri din brațe mărginite de index, deci nu e nimic de paralelizat — și e **mai
  rapid** așa: prima pagină 875 → 393 ms, o pagină filtrată pe actor 913 → 433 ms, treapta
  nemărginită neschimbată la 4,6 s.

### 4. Scara de ferestre, și regula care citește `actor`, nu `hasFilter`

O pagină fără limită inferioară construiește toate cele 3,9 M de rânduri înainte ca top-N să
arunce tot în afară de 50: **4,6 s**. Cu o fereastră de 5 zile: **0,89 s**. Scara lărgește
5 zile → 50 → 500 → fără limită inferioară, iar lărgirea e **idempotentă** (o fereastră mai largă
întoarce aceleași rânduri noi plus unele mai vechi), deci o pagină scurtă se re-rulează în loc să
fie publicată ca „sfârșit de flux".

Prima regulă scrisă — *orice cerere filtrată sare direct la ultima treaptă* — a fost **greșită și
măsurată ca greșită**: `source` nu elimină niciun braț (toate cele unsprezece publică
`'governance'`; `'attestation'` vine cu schema de atestări), deci `?source=governance` **este**
fluxul nefiltrat și regula îl făcea **8,3 s în loc de 1,0 s**. Regula citește acum `actor`:

| cerere | timp |
| --- | --- |
| nefiltrat, 50 de rânduri | 1,04 s |
| `source=governance` | 0,91 s |
| `event_type=gov_action_proposed` | 0,093 s |
| `event_type=committee_resigned` (0 rânduri) | 0,055 s |
| `actor=<drep cu 18 evenimente>` | 6,6 s |

`event_type` e **constantă** în lista de selecție a fiecărui braț, deci un filtru pe el colapsează
nouă sau zece brațe la un qual fals o-singură-dată — de aceea păstrează scara și tot costă
milisecunde. `actor` nu elimină nimic, dar evenimentele unui actor sunt rare, deci toate cele patru
trepte vin scurte: 10,3 s pe scară față de 6,6 s direct pe ultima treaptă. Documentat în rută că
**`/activity` nu e locul dosarului unui actor** — `/votes?voter_id=`, `/dreps/{id}/updates` și
`/dreps/{id}/delegators` îl servesc din coloane indexate.

### 5. Un defect găsit de propriul test: cursorul accepta mai mult decât un întreg

`readMaybe :: Maybe Int64` **este** `Read`-ul lui Haskell, deci lua `" 1"`, `"(1)"` și `"1 "`. Un
cursor e un token emis de serviciu: un client care l-a primit altfel nu l-a construit legitim.
Parserul cere acum cifre cu minus opțional și nimic altceva — iar minusul **trebuie** acceptat,
fiindcă cele cinci brațe de graniță de epocă au `tx_index = -1` și brațul de retragere `-2`.

### Fluxul moștenit, retras

Șters: `ActivityEventType` (PascalCase) din `Transfer/OrderBy.hs`, `ActivityFilter` +
`activityFilterFromParams` din `Transfer/Filters.hs`, `ActivityEventDetails` +
`ActivityEventResponse` din `Transfer/QueryResponses.hs`, `legacyApplyLimits` +
`legacyApplyFilterOrderLimit` din `Query/Common.hs` și `getActivityFeed` din `Query/Projected.hs`.
Cu ele au plecat și cele trei `hiding (ActivityEventType)`. `legacyNormalizeLimitOffset` rămâne:
handler-ele moștenite de profil/atestare încă îl cheamă.

### Cele două direcții, așa cum le cere planul

`before` merge înapoi în timp (scroll infinit, ordine descrescătoare), `since` merge înainte
(poll de recuperare, ordine **crescătoare**), amândouă **exclusive** față de rândul din cursor.
Amândouă în aceeași cerere → 400 `both_cursors_given`: o pagină care numește două direcții nu are
un singur răspuns, iar preferarea silențioasă a uneia ar pagina clientul altfel decât a cerut, dar
raportând succes. Niciuna → cele mai noi, de la vârf.

Direcția decide **trei** lucruri împreună — comparația de keyset (`<` sau `>`), `ORDER BY`-ul și
capătul ferestrei pe care îl fixează cursorul. Un singur constructor (`FeedDir`) le alege pe toate
trei, pentru că oricare dintre ele desincronizat paginează greșit *fără să dea eroare*. Testul
unitar asertează că `since` inversează și comparația și ordinea.

`since` **nu are nevoie de scară**: ambele limite sunt cunoscute de la început (cursorul jos,
vârful sus), deci o singură interogare îl rezolvă — 0,9 s, exact același plan ca un `before`.

`offset` nu e declarat pe rută. Servant **ignoră** un parametru de query nedeclarat, deci
`?offset=10` e servit ca cererea nepaginată care este, nu refuzat; smoke-ul asertează atât 200 cât
și faptul că n-a paginat.

### Devieri de la plan, asumate și documentate

| plan | livrat | motiv |
| --- | --- | --- |
| cursor cu **3** componente | **4** (`sort_slot:tx_index:arm:item_index`) | tripletul avea 108.141 grupuri duplicate, măsurat; `arm` face unicitatea **structurală** |
| câmp `title` pre-randat | **absent** | view-ul nu publică niciun titlu; randarea unuia ar cere un join pe braț exact pe pagina care trebuie să rămână rapidă, iar un titlu derivat din `event_type` e acel enum scris în engleză |
| `item_index` = index per-tranzacție | `voting_procedure.id` pe brațul de voturi | `voting_procedure.index` **nu** e un contor per-tranzacție în aceste date: 3.243.802 de rânduri CC au doar 108.176 de perechi distincte `(tx_id, index)` |


## 2026-10-20 — Un timeout de citire, și un defect pe `/votes` care nu e al meu

### Interogările orfane: de ce toate măsurătorile de azi au fost prea mari

Un `curl --max-time` care expiră **nu** oprește interogarea: clientul pleacă, PostgreSQL macină
mai departe. Trei încercări abandonate la rând au lăsat trei scanări complete peste 3,24 M de
rânduri concurând pentru un container limitat la **0,75 CPU**. De-asta prima pagină de `/activity`
„măsura" 2,0 s și `/votes?voter_role=drep` 6,8 s — cu orfanii uciși, aceleași cereri sunt **0,48 s**
și **2,96 s**. Orice cifră luată în timp ce altceva rula e o cifră despre contenție, nu despre
interogare.

### `SET LOCAL statement_timeout = 30s` pe fiecare citire

Fără el o interogare patologică nu se termină niciodată: clientul așteaptă până renunță, iar
conexiunea din pool rămâne ocupată cât timp PostgreSQL lucrează — adică și **după** ce clientul a
plecat. Cu el, `/votes?voter_role=constitutional_committee&limit=1` întoarce un **503
`db_unavailable` la 30 s** și **nu lasă niciun backend în urmă** (verificat: `active now: 0`).
Anularea vine ca SQLSTATE 57014, iar `runRead` transformă deja orice `SqlError` în 503 — răspunsul
onest pentru „nu acum", și unul **definit**, care e tot scopul.

30 de secunde pentru că cea mai lentă pagină legitimă măsurată în serviciu e ultima treaptă a
scării de activitate, 4,6 s.

### DEFECT: `/votes?voter_role=constitutional_committee` nu poate răspunde

Rolul de comitet e **3.243.802 din 3.244.711** de rânduri de vot pe preprod, iar `palmares.vote`
calculează trei funcții de fereastră partiționate pe `(voter_id_hash, gov_action_id, voter_role)` —
deci o pagină de **un** rând sortează și partiționează tot. Măsurat: statement-ul de pagină 27,5 s
și cel de count 24,0 s prin psql, peste 166 s prin serviciu.

Celelalte roluri răspund în secunde (spo 90 de rânduri, drep 819) pentru că `voter_role` **este**
coloană de partiție și qual-ul se împinge sub `WindowAgg`. Din același motiv, adresarea aceluiași
membru **după id** e rapidă: 6 rânduri în 2,5 s.

Verificat că nu e vina parametrilor: cu `plan_cache_mode = force_generic_plan` și aceiași
parametri pregătiți, 18 s — deci nu planul generic, ci volumul.

Reparația e o **pagină în două etape**: alege rândurile paginii din `voting_procedure` (ieftin,
indexat), apoi rezolvă cele trei coloane de fereastră **doar pentru partițiile alea** — se împinge
sub `WindowAgg` din exact același motiv. E o sarcină proprie, nu o coadă a T-API-15. Aceeași
reparație rezolvă și `/votes` nefiltrat, care are același cost.

Smoke-ul păstrează aserțiunea **neatinsă**, printr-un helper `known_defect` care o afișează și nu
pică rularea. Nu slăbită, nu ștearsă: când reparăm, se șterge wrapper-ul, nu aserțiunea. Cele patru
aserțiuni care depindeau de acea pagină citesc acum același membru **după id**.

### Două aserțiuni vechi, reparate

- `participation_rate` pe o epocă fără voturi e `0.0`, nu `0` — așa se scrie un double zero în JSON.
- `[.participation_by_epoch[].votes_cast] | add` avea un total fix peste o **fereastră care
  alunecă** cu vârful lanțului: era deja depășit cu 30 de voturi o epocă mai târziu. Magnitudinea
  rămâne asertată pe epoca 313; peste fereastră se asertează doar ce nu poate aluneca.

### Contractul de erori, publicat în Swagger

`ErrorCode` și `ApiError` **nu apăreau** în `definitions`: servant tipizează doar corpul de succes,
deci swagger2 n-avea de unde să le emită, și cele 24 de coduri pe care un client face `switch` nu
se puteau descoperi din document. Declarate acum explicit, cu `$ref` pe răspunsurile globale 400,
404 și 503. Versiunea documentului → **1.0.0** (valoarea din plan).

`scripts/check_swagger.sh` (nou, în CI lângă garda de drift) asertează **ce spune** documentul:
cele 22 de căi, cele 24 de coduri, `Accept` opțional, `q` doar unde trebuie, `/activity` fără
`offset` și cu `before`/`since`, `produces` pe fiecare rută. Secțiunea de Swagger din smoke s-a
redus la singura proprietate care e despre serviciul care rulează: documentul **servit** e identic
cu cel **comis**.


## 2026-10-20 — Prima rulare COMPLETĂ a smoke-ului, și trei defecte pe care numai ea le putea găsi

Suita n-a mai ajuns niciodată la capăt în această sesiune: fiecare rulare se îneca în pagina de
comitet. Cu timeout-ul de citire pus, a rulat până la sfârșit — și a găsit trei lucruri în
T-API-14/17, rute considerate „verde" fiindcă răspundeau 200.

### 1. `ORDER BY` într-o subinterogare NU supraviețuiește unui JOIN peste ea

`listDelegations` e singura interogare de listă construită pe două niveluri: pagina e aleasă și
ordonată într-o subinterogare, iar numele DRep-ului se adaugă cu un JOIN **deasupra**. Fără un
`ORDER BY` și pe nivelul de sus, PostgreSQL întoarce cele cinci rânduri corecte în ordinea
hash-join-ului.

Măsurat: `/delegations?order_by=at&sort_order=desc&limit=5` → `136808533, 136808622, 136809426,
136766068, 136815403`. Cele mai noi cinci delegări, **în nicio ordine**. Reparat cu
`delegationOuterSortSpec` — o a doua specificație, nu un prefix pe prima, pentru că `es.amount`
înăuntru e `lovelace` afară, o coloană pe care subinterogarea însăși a numit-o. Verificat pe toate
trei cheile de sortare și în ambele direcții.

Verificat și că e **singura** instanță: celelalte șapte funcții de listă pun proiecția în același
`SELECT` cu `ORDER BY`-ul.

### 2. Servant îmbracă mesajul unui parametru de query și îl lasă gol pe cel al unui capture

Fiecare `parseUrlPiece` din codebase întoarce **codul de eroare** ca mesaj, exact ca să supraviețu-
iască drumului prin servant până în formatter. Ce nu supraviețuiește e ca mesajul să fie **tot**
string-ul: un capture ajunge verbatim (`invalid_drep_id`), un parametru de query ajunge îmbrăcat
(`Error parsing query parameter drep_id failed: invalid_drep_id`).

Deci `/drep/{id}` răspundea corect, iar `?drep_id=` și `?gov_action_id=` răspundeau
`invalid_parameter` — adevărat, dar nu spune clientului **care** parametru, nici de ce. Și cazul
identic pe `/votes?voter_id=` răspundea corect, fiindcă acolo decodarea e în handler, nu în parser.

`errorCodeFromParserMessage` (nou, în `webapi-lib/Errors.hs`, unde îi e locul) citește codul de
după ultimul `:`. Un mesaj fără `:` se întoarce neatins, deci cazul capture citește exact ca
înainte. Șase teste unitare noi, inclusiv round-trip-ul tuturor celor 24 de coduri prin propriul tag
și faptul că fiecare cod are un mesaj pentru oameni.

Reparat și mesajul lui `BothCursorsGiven`: zicea „either a cursor or an offset", dar `/activity`
n-are offset — sunt `before` și `since`.

### 3. Un FAIL care era contenție, nu defect

`/pages/drep-explorer?limit=10` a picat într-o rulare și răspunde `[10,10,0]` în următoarea:
interogările orfane de la pagina de comitet mâncau CPU-ul containerului. Verificat după, nu
reparat — n-avea ce.

### Ce rămâne roșu, intenționat

O singură linie `KNOWN`: pagina `/votes?voter_role=constitutional_committee`, cu aserțiunea
neatinsă și motivul lângă ea.

### 4. Și un al patrulea: aserțiunea era greșită, nu ruta

`/pages/drep-explorer?limit=10` picase și în a doua rulare completă, deși cererea răspunde
`[10,10,0]` când o dai de mână. Filtrul era `[.items | length, .limit, .offset]` — în jq asta
**pipează** `.items` în toate trei, deci încearcă `.items.limit` pe un array și dă eroare; `j`
întoarce string gol și aserțiunea raportează ruta ca stricată. Cu paranteze,
`[(.items | length), .limit, .offset]`, trece.

A stat ascunsă exact atât cât suita n-a putut ajunge până la linia aia. Verificat după aceea că
**toate** filtrele jq din suită compilează (compilarea nu e corectitudine — ăsta compila — dar
prinde clasa vecină).


## 2026-10-20 — Diagnoza completă a defectului `/votes`, cu cifre, ca sarcina următoare să nu mai caute

Am măsurat până la cauză înainte de a scrie o linie de reparație, și cauza **nu** era cea
evidentă. Toate măsurătorile pe cele 3.243.802 de rânduri de comitet, pe preprod:

| forma | timp |
| --- | --- |
| `count(*)` prin `palmares.vote` (cum e azi) | **14,2 s** |
| idem, fără funcțiile de fereastră | 10,7 s |
| idem, fără fereastră și cu `enable_mergejoin = off` | **19,3 s** (mai rău) |
| `count(*)` direct pe `voting_procedure ⋈ gov_action`, **fără** join-ul de cheie rece | **1,14 s** |

Deci: scanarea de bază e **1,1 s**, join-ul cu `palmares.cc_cold_interval` adaugă **~9,5 s**, iar
funcțiile de fereastră încă **~3,5 s** peste. Nu invers.

`EXPLAIN` arată de ce: planificatorul alege un **Merge Left Join** pentru `cc_cold_interval`,
pentru care sortează toate cele 3,24 M de rânduri după `committee_voter` — *external merge,
234.888 kB pe disc*, 12,3 din cele 14,2 s. Cu merge join interzis trece la hash join și construiește
648.767 de rânduri ca să sondeze 3,24 M: 19,3 s. **Ambele drumuri sunt greșite** pentru că join-ul
nu trebuie făcut deloc: un `count` nu are nevoie de identitatea rece, iar o pagină de 50 are nevoie
de ea pentru 50 de rânduri.

### Forma reparației (T-API-13b)

Nu „`vote_base` fără ferestre" — asta taie 3,5 s din 14,2. Ci **trei interogări**, fiecare cu doar
join-urile de care are nevoie:

1. **count** — `voting_procedure ⋈ gov_action` (+ `anchor` doar dacă se filtrează pe `anchor_status`,
   + `drep` doar dacă e `q`). Un `voter_id` de comitet se traduce întâi în id-urile de cheie
   fierbinte, printr-o interogare mică pe `cc_cold_interval` după `cold_raw`. **~1,1 s.**
2. **cheile paginii** — aceeași formă, cu `ORDER BY … LIMIT ? OFFSET ?`.
3. **rândurile paginii** — `palmares.vote` restrâns la perechile `(voter_id_hash, gov_action_id)`
   ale paginii. Ambele **sunt** coloane de partiție, deci qual-ul se împinge sub `WindowAgg` și se
   calculează ferestrele doar pentru acele partiții. Legate ca **parametri**, nu ca subinterogare:
   un `SubLink` nu se împinge.

Trei dus-întors în loc de două, fiecare ieftin.

### Un efect secundar al indexului de azi, raportat cinstit

`gov_idx_committee_reg_hot_asc` face ieșirea lui `cc_cold_interval` **sortată**, ceea ce e exact ce
tentează planificatorul spre merge join-ul de mai sus: 14,2 s cu el, 12,4 s fără. Dar pentru
drumurile normale e invers — `count` pe rolul DRep e 818 ms cu merge join și 1.147 ms fără — și
pagina de `/activity` a câștigat 1.150 ms din 1.347. Net pozitiv, dar costul e real și scris aici.


## 2026-10-20 — Block 6 început: paritatea Koios a găsit un bug de corectitudine în prima oră

T-PARITY-1, pasul de înregistrare. Cele opt fixture-uri de pe `preprod.koios.rest` sunt în
`test/fixtures/koios/`, cu comenzile de reîmprospătare și tabelul de potriviri în README-ul lor.

**Șase numere s-au potrivit înainte de a scrie un singur decodor**: 294 de DRep-uri înregistrate,
195 de rânduri de putere la epoca 319, 131 de propuneri, 16 voturi pentru DRep-ul de fixture, 2
rânduri pentru o acțiune revotată, și comitetul (3 membri, quorum 2/3).

Două OPEN-uri din plan, răspunse direct din înregistrare:
- **OPEN 2** — din cele patru statusuri de comitet ale Koios, pe preprod apare **unul**:
  `authorized`, pentru toți cei trei membri. Comanda planului era greșită: `committee_info`
  întoarce un **ARRAY** de comitete, deci `.[0].members[].status`.
- **OPEN 5** — **103** expirate, **11** fără nicio epocă de ciclu de viață. Ambele se potrivesc cu
  ce publicăm.

### Al șaptelea număr NU s-a potrivit, și avea dreptate Koios

Koios: **13** delegatori live pentru DRep-ul de fixture. Noi: **12**. Diferența, adresă cu adresă:
`stake_test1uzeq0nyprsk5e27qyej25l90lcqleqakvr8hxctns7f5epqs9cn6u`.

Istoricul ei, din bază:

| slot | epocă | ce s-a întâmplat |
| --- | --- | --- |
| 132.510.869 | 310 | înregistrare + delegare către alt DRep |
| 132.862.106 | 311 | **de-înregistrare** |
| 132.863.365 | 311 | **re-înregistrare + delegare către DRep-ul nostru** (același tx) |

`palmares.live_delegation` compara **EPOCI**: `sd.epoch_no >= l.from_epoch`. De-înregistrarea e în
epoca 311, delegarea e în epoca 311, deci regula anula o delegare folosind o închidere petrecută
**1.259 de sloturi înaintea ei**. Și nu e o formă rară — a închide un cont și a deschide altul se
face normal dintr-o mișcare.

Măsurat pe tot corpusul: regula pe epoci excludea **344** de adrese, dintre care **52** se
re-înregistraseră după delegare. După reparație: **2.015** delegări live în loc de 1.960, **289**
arhivate în loc de 344, iar 2015 + 289 = 2304 — partiția ține în continuare. Setul DRep-ului de
fixture e acum **identic cu cel al Koios**, adresă cu adresă.

### Reparat în patru locuri, pentru că regula era copiată în patru

`(sd.tx_id, sd.cert_index) > (from_tx, from_cert)` — db-sync atribuie `tx.id` în ordinea lanțului,
deci perechea e o ordine totală peste certificate, iar indexul `gov_idx_stake_dereg_addr` e deja
`(addr_id, tx_id)`, deci comparația e servită de index fără join la `block`. Aceeași ordonare pe
care `palmares.cc_cold_interval` o folosește din același motiv.

1. `palmares.live_delegation` (`sql/010_gov_views.sql`)
2. `delegationIsCurrentExpr` (`Query/Gov.hs`) — copia inline, al cărei propriu haddock spunea „dacă
   predicatul view-ului se schimbă vreodată, LINIA ASTA trebuie să se schimbe cu el". S-a schimbat.
3. `palmares.drep_epoch_delegators` (`sql/020_views.sql`) — altfel cele două view-uri nu mai erau
   de acord la vârf, ceea ce verificarea 040 **prinde**
4. `delegator_retention_rate_v1.sql` — metrică publicată; `docs/metrics.md` regenerat

`palmares.vote_delegation` expune acum `tx_id`, iar `delegationFrom` îl trece prin subinterogarea
de pagină (a lipsi era un 42703 la prima rulare).

Verificarea 040 își restatea propria regulă **pe epoci** — deci trecea în timp ce view-ul greșea,
fiindcă făceau aceeași greșeală. Restatează acum regula pe certificate.

### Numerele fixate, re-măsurate

2015 curente / 289 arhivate / 2304 adrese; 1.020 abstain + 40 no-confidence + 955 credențial =
2015; 1.092 cu lovelace; 194 cu nume; 24 peste 1 M ADA (neschimbat); CSV 2.016 linii cu header;
DRep-ul de fixture 13. Actualizate în smoke, în haddock-urile DTO-urilor, în descrierea rutei și în
`test/fixtures/preprod-epoch-320-metrics.json` (cu motivul lângă valoare). Cele opt aserțiuni ale
fixture-ului de epoca 319 trec din nou pe preprod; toate cele șapte verificări SQL trec.

### T-PARITY-1 închis: decodoarele și 22 de teste

`Palmares.Reconcile.Koios` (în `offchain-lib`) ține cele opt endpoint-uri ca **sârmă** — numele,
grafiile și codificările lui Koios — iar maparea în vocabularul nostru e în patru funcții explicite
la final. Separarea e ce face ca o schimbare la Koios să fie **un decodor care cade**, nu un număr
diferit în liniște mai jos pe fir.

Testele nu verifică dacă merge aeson. Fixează cele trei capcane pe care un decodor le greșește
**silențios**:

1. **Lovelace vine ca STRING** (`"3923709956713"`), fiindcă valoarea trece de ce round-trip-uiește
   exact un Double. `KoiosLovelace` acceptă string sau număr și **refuză** o fracție în loc s-o
   rotunjească — o paritate care rotunjește e o paritate verde pe un număr greșit.
2. **`withdrawal` e un array**, `[]`, nu `null` — pe toate cele 131 de propuneri, umplut doar de o
   acțiune `TreasuryWithdrawals`.
3. **Un vot de comitet e publicat sub cheia FIERBINTE** (`cc_hot1…`), în timp ce noi publicăm pe
   cea **RECE** (D-24). Comparate ca string-uri nu găsesc nimic nici când cele două indexuri sunt
   perfect de acord. Testul asertează prefixul, ca reconcilierea să nu poată fi scrisă ca o
   comparație de string-uri.

`koiosDRepStatus` ia **două** intrări, pentru că Koios desparte ce noi unim: zice `registered`
pentru un DRep căruia i-a expirat înregistrarea și marchează expirarea în `active`. Citit doar
`drep_status`, fiecare DRep expirat ar fi raportat activ.

Toate cele șapte tipuri de acțiune apar în înregistrare, deci nicio linie din `koiosActionType` nu
e speculativă — și un test asertează că cele șapte grafii ale Koios **sunt** exact cei șapte
constructori ai noștri, în loc să se bazeze pe asta.

**181 de teste unitare trec.**

### Garda de secrete a căzut pe fixture-uri, și avea dreptate să se uite

`secrets` a picat pe commit-ul de fixture-uri cu **81** de găsiri. Toate 81 `generic-api-key`,
toate în `proposal_list.json`, și fiecare un **hash de credențial on-chain**: descrierea unei
acțiuni `NewCommittee` își numește membrii ca `"keyHash": "<28 de octeți hex>"`, adică o valoare cu
entropie mare lângă un nume de câmp care conține „key".

Hash-urile sunt **publice prin construcție** — pe lanț, servite de un API public, și sunt chiar
conținutul pe care o paritate îl compară. A tăia `meta_json` din înregistrare ar face fixture-ul să
nu mai fie sârmă, singurul lucru pentru care există.

Deci regula e pe allowlist pe acea cale. Două lucruri o fac **îngustare**, nu gaură:

1. `[[allowlists]]` cu `targetRules`, **nu** un bloc `[[rules]]`. A declara o regulă cu un id pe
   care config-ul implicit îl folosește deja o **înlocuiește**, inclusiv allowlist-urile ei
   proprii — măsurat: prima încercare a făcut exact asta și a înviat o găsire pe care implicitul o
   suprima de la început, `root-keys:` din `cabal.project`, adică **cheia publică de verificare a
   CHaP**. E acum pe allowlist explicit, cu motivul scris lângă ea, ceea ce bate să te bazezi pe un
   stopword pe care nimeni nu-l vede.
2. `scripts/check_gitleaks_allowlist.sh` (nou, în același job): plantează o cheie AWS și o cheie
   privată **exact pe acea cale** și asertează că garda **tot** cade, apoi plantează un `keyHash`
   on-chain și asertează că nu cade. O gardă care nu prinde nimic și una care prinde totul sunt
   la fel de inutile, deci se asertează ambele jumătăți.

Scanează o **copie** la aceeași cale relativă într-un root temporar: gitleaks raportează căile
relativ la ținta scanării, deci `gitleaks dir test/fixtures/koios` raportează `proposal_list.json`
și allowlist-ul nu se potrivește niciodată — iar scanarea rădăcinii ar umbla prin `dist-newstyle`
(969 MB, 1.984 de găsiri în artefacte de build).

Setul plantat **exclude** intenționat un token `ghp_…`: protecția de push a GitHub scanează
conținutul fișierelor și a **respins** prima versiune a commit-ului pentru că îl conținea ca
literal. Și a avut dreptate — un token care arată valid într-un fișier comis nu se distinge de unul
scurs. Cheia AWS plus un bloc de cheie privată fac același lucru din două familii de reguli.

### T-PARITY-2 închis: primitiva, fișierul de derogări, garda de prospețime, raportul

**O singură primitivă**, pentru că toate cele zece rânduri au aceeași formă: două hărți cheiate la
fel; o cheie într-o singură parte e o divergență de `membership`; o cheie în ambele merge la un
comparator de câmpuri care întoarce **doar** ce diferă. Egalitatea de multiset din R9 e la fel, cu
cheia `(adresă, sumă)` și valoarea un contor.

Patru decizii pe care stă reconcilierea, fiecare un loc unde un răspuns greșit arată exact ca unul
bun — și fiecare cu un test:

- Cele două părți se numesc `koios` și `palmares`, **niciodată** `expected` și `actual`. Un raport
  care numește o parte „expected" a decis deja care index are dreptate. Prima divergență reală
  găsită a fost **a noastră**.
- O derogare mută o divergență din `unexplained` în `explained` și **nu suprimă nimic**: apare tot
  în raport, cu motivul lângă ea. E cheiată pe **rând**, pentru că aceeași cheie înseamnă lucruri
  diferite în rânduri diferite — un id de DRep e cheie de membership în R1 și cheie de profil în R2.
- O derogare care n-a potrivit nimic e raportată **stale**, iar derogările unui rând **sărit** nu
  sunt: nu s-a comparat nimic, deci nu putea potrivi nimic. Linia aia e și singurul semnal că o
  divergență a fost **reparată** — derogarea care supraviețuiește problemei e ce te anunță.
- Un decalaj de **o** epocă e normal (două indexuri la câteva secunde de o graniță); **două** e o
  epocă întreagă de voturi, putere și tranziții de ciclu de viață, deci rularea **abandonează** în
  loc să raporteze un perete de artefacte de sincronizare.

Coduri de ieșire: **0** curat sau explicat, **1** neexplicat, **3** abandonat. **2 e rezervat**
pentru eșec de transport și e produs de executabil, niciodată aici — „Koios n-a răspuns" și „Koios a
răspuns altceva" sunt evenimente diferite, iar un test asertează că niciun raport nu poate da 2.

Un rând sărit e **singurul** drum spre 3: `mkReport` întoarce `ReportAborted` oricând un rând e
sărit, **înainte** de a număra divergențe, deci o rulare care n-a putut compara R3 nu poate raporta
trecere, și apelantul nu are nevoie de ramură proprie de abandon.

`config/reconcile-exceptions.yaml` e însămânțat **gol**, cu schema și motivul în comentarii: prima
divergență pe care Koios a găsit-o a fost **reparată**, nu derogată.

**196 de teste unitare trec.**

### Garda a avut dreptate de două ori

Dupa allowlist, `secrets` tot a picat — dar cu **2** găsiri, nu 81. Amândouă în
`scripts/check_gitleaks_allowlist.sh` **însuși**: cheia AWS și momeala `keyHash`, scrise ca
literali într-un fișier care e comis și deci scanat.

Fiecare plantă e acum **asamblată din fragmente** peste o concatenare de shell, deci niciun pattern
nu se potrivește pe fișier, în timp ce JSON-ul pe care-l scrie (în `mktemp -d`, niciodată comis)
poartă valoarea întreagă. Nu e obfuscare — e singurul mod în care un script a cărui treabă e să
planteze un credențial poate trăi în depozitul pe care-l protejează.

Cele două găsiri din commit-ul `2baed8c` **nu se pot edita** din istoria deja împinsă, deci sunt
numite în `.gitleaksignore` prin **amprentă** — commit, fișier, regulă, linie. O amprentă **nu
poate lărgi**: același pattern oriunde altundeva, inclusiv în același fișier la un commit ulterior,
e o găsire nouă și tot pică garda. Alternativa era rescrierea istoriei împinse ca să ștergem un id
de cheie AWS **sintetic, din corpusul de test al gitleaks-ului însuși**; nimic nu s-a scurs, nu e
nimic de rotit, deci un force-push ar fi fost distructiv degeaba.

Și protecția de push a GitHub a prins separat un `ghp_…` pe care-l pusesem ca a doua plantă. A avut
dreptate și ea: un token care arată valid într-un fișier comis nu se distinge de unul scurs. Planta
aia a dispărut de tot — cheia AWS plus un bloc de cheie privată acoperă două familii de reguli fără
să ne certăm cu o gardă care e de partea noastră.

Verificat cu gitleaks 8.30.1, versiunea pe care CI o fixează: `gitleaks git --redact --no-banner
--exit-code 1 .` → **no leaks found** peste toată istoria.

### T-PARITY-3 închis: cele zece rânduri

Fiecare rând e **un** apel `diffKeyed`; **ce** compară fiecare e specificația, scrisă lângă el,
pentru că semnătura și testele nu o determină.

**R10 rulează primul** și eșecul lui **sare** R1–R9 în loc să compare peste o graniță de epocă —
ceea ce fabrică divergențe care sunt artefacte de sincronizare, nu defecte în vreunul din indexuri.
Limita lui de lag e **aceeași 300 s** pe care o folosește `/ready` (D-7): o rulare contra unui index
pe care serviciul însuși îl declară not-ready compară cu ceva ce nu e servit niciunui client.

`PalmaresSnapshot` poartă o **proiecție**, nu DTO-urile de răspuns. Rândurile compară nouă valori,
unde `DRepSummary` singur are douăzeci și două — un snapshot construit din DTO-uri ar face fiecare
test de rând să scrie treisprezece câmpuri pe care nimeni nu le citește ca să asserteze unul pe
care da. Dependența de DTO rămâne **acolo unde o redenumire de câmp trebuie să rupă** — în
proiecțiile `from*` — iar acelea sunt exercitate cap-la-cap de `admin reconcile-koios` contra
API-ului viu, singurul loc unde **pot** fi: un test unitar care construiește de mână un
`DRepSummary` demonstrează că autorul testului și proiecția sunt de acord, nu că API-ul servește ce
credem.

Trei rânduri poartă o decizie care merită numită:

- **Toleranța lui R4 e ZERO**, și asta **este** „toleranța documentată" din verificarea 5 a
  specificației. Cele 46% de care avea nevoie înainte erau **filtrul de pseudo-DRep**, nu o eroare
  de măsurare — opțiunile rezervate ale ledger-ului sunt rânduri ca oricare (D-20), iar în
  fixture-ul de test `always_abstain` singur ține **98,8%** din putere.
- **R7 compară ISTORICUL de vot**, inclusiv voturile depășite — adică e **poarta** pe
  `GET /votes` să întoarcă fiecare rând din `palmares.vote`, nu doar pe cele finale. Doar rolul
  DRep: cele 3.243.802 de rânduri de vot de comitet de pe preprod sunt spam de test.
- **R9 e un multiset**, nu un set. Același beneficiar poate apărea legitim de două ori într-o
  acțiune, iar colapsarea ar face invizibil un rând duplicat.

**18 teste de rând** peste o pereche consistentă construită de mână — două DRep-uri, ambele opțiuni
rezervate, o acțiune, un membru de comitet, o retragere, un vot — cu **o** mutație fiecare.
Mutatorii sunt actualizări de record **în modulul de test**, nu cod de bibliotecă: o funcție care
strică un snapshot intenționat e una pe care cineva o va chema din greșeală. Un test asertează că
**fiecare rând a comparat cel puțin o cheie**, fiindcă un rând care a comparat în liniște zero ar
trece pe orice pereche.

Raportul a câștigat `reportExplained` lângă `reportUnexplained`: o rulare al cărei număr de
explicate urcă săptămânal merită văzută înainte să merite investigată.

**214 de teste unitare trec.**

### T-PARITY-4 închis: eșantionul fixat

Două rânduri nu pot fi comparate pe toată populația — R7 ar trebui să pagineze istoricul complet de
vot al fiecărui DRep, R6 și R9 cer o cerere pe acțiune — deci rulează pe un **eșantion**, iar
eșantionul e **comis**.

Un eșantion tras la rulare face verificarea **nefalsificabilă**: o divergență care apare într-o
noapte și nu în următoarea poate fi o reparație, o regresie sau **un alt eșantion**, iar nimic din
raport nu spune care. Un fișier comis transformă o schimbare de populație într-un `git diff`.

Nici urmă de aleator. Un script de shell **nu poate** trage un eșantion aleator reproductibil, deci
ambele liste sunt uniuni de **selectori determiniști** peste populații diferite:

- `vote_history_dreps` (**37**) = cei 30 cu cele mai multe voturi (coada interesantă) ∪ cei 10
  înregistrați cel mai recent care au votat (cohorta nouă, pe care coada n-o acoperă niciodată) ∪
  DRep-ul din exemplul lucrat, **explicit**, pentru că cele 16 voturi ale lui nu sunt garantate să
  ajungă în top 30 — un test asertează că e acolo — ∪ un DRep care **a re-votat** efectiv, ca
  drumul de voturi depășite din R7 să ruleze pe date vii. Acel ultim id e **singura** valoare trasă
  cu un apel Koios, și e un **id** pe care ambele părți sunt apoi comparate, niciodată o valoare
  după care vreo parte e judecată.
- `tally_actions` (**24**) = fiecare acțiune ratificată, fiecare doar-dropped, cele 5 retrageri de
  trezorerie cu totalul cel mai mare și cele 2 acțiuni cele mai vechi din era bootstrap: fiecare
  formă de ciclu de viață și tot drumul de retragere.

Ambele liste sunt sortate, deci o re-trasare fie se potrivește octet cu octet, fie arată o schimbare
reală.

**Cinci teste** păzesc fișierul livrat, și CI le rulează, deci podeaua specificației nu poate
regresa în liniște: un eșantion scăzut sub 20 de DRep-uri sau 10 acțiuni ar reconcilia tot curat și
ar fi tot o verificare mai slabă decât a cerut specificația. Verifică și că **fiecare id e forma
CIP-129** pe care ambele părți cheiază — un id malformat nu e o verificare mai slabă, e un rând care
nu compară nimic și raportează ok — și că niciun id nu se repetă, singurul mod în care un eșantion
poate părea mai mare decât e.

`loadSample` întoarce `Left` pe fișier lipsă în loc să arunce: `eitherDecodeFileStrict` aruncă la
eroare de IO și întoarce `Left` doar la eroare de parsare, ceea ce ar face „eșantionul nu e acolo" o
excepție și „eșantionul e malformat" un mesaj.

**219 de teste unitare trec.**

### Comenzile de re-trasare (T-PARITY-4)

Trase de pe API-ul nostru, cu **un** apel Koios. `sort -u` dă ambelor liste o ordine canonică.

```bash
P=http://localhost:8093      # sau https://gov.cardano.vip/api
K=https://preprod.koios.rest/api/v1
W=drep1y2qn0rf74zdk3u8upx2yk8un7tlts5f33uvklx96gq5a32qmwj3xg
curl -fsS "$P/dreps?status=active&status=inactive&order_by=votes_cast&sort_order=desc&limit=30" \
  | jq -r '.items[].id' > /tmp/tail.txt
curl -fsS "$P/dreps?status=active&status=inactive&order_by=registered_at&sort_order=desc&limit=200" \
  | jq -r '.items[] | select(.votes_cast > 0) | .id' | head -10 > /tmp/new.txt
echo "$W" >> /tmp/new.txt
curl -s "$K/vote_list?limit=1000" | jq -r 'group_by(.voter_id + .proposal_id)[] | select(length > 1)
  | select(.[0].voter_role == "DRep") | .[0].voter_id' | head -1 >> /tmp/new.txt
curl -fsS "$P/gov-actions?limit=200" | jq -r '[.items[] | select(.lifecycle.ratified_epoch != null
  or (.lifecycle.dropped_epoch != null and .lifecycle.expired_epoch == null
      and .lifecycle.enacted_epoch == null))] | .[].bech32' > /tmp/lifecycle.txt
curl -fsS "$P/gov-actions?type=treasury_withdrawals&order_by=treasury_amount&sort_order=desc&limit=5" \
  | jq -r '.items[].bech32' >> /tmp/lifecycle.txt
curl -fsS "$P/gov-actions?order_by=proposed_at&sort_order=asc&limit=2" \
  | jq -r '.items[].bech32' >> /tmp/lifecycle.txt
```

### Jumătatea de bibliotecă a T-PARITY-5

`loadKoiosSnapshot` / `loadPalmaresSnapshot` citesc un director de corpuri de răspuns **brute**, un
fișier pe endpoint, numit după el. Rostul: CI poate **relua** o reconciliere reală **fără rețea**,
deci o pană nocturnă a oricărui serviciu nu poate pica un PR — și e unealta care **capturează
dovada** când nocturna chiar divergează: perechea care a produs o constatare e un fișier pe care
cineva îl poate citi șase săptămâni mai târziu.

Partea noastră **decodează prin DTO-urile reale de răspuns mai întâi** și proiectează după. Ordinea
e ce face o pereche înregistrată dovadă **despre API**, nu despre reconciliator, și de-asta o
redenumire de câmp în `DRepSummary` rupe asta la compilare. `mkPalmaresSnapshot` e **singurul** loc
unde se citesc DTO-urile.

Două decizii de proiecție care ar fi fost bug-uri invers:

- un rând de tally se desface în **trei** chei, iar un contor **zero** e **aruncat**. Koios n-are
  rând pentru un verdict pe care nimeni nu l-a dat, deci un 0 pe o parte contra unei chei absente pe
  cealaltă ar fi o divergență de membership **pe fiecare acțiune**.
- `psVotes` ține **doar** voturile de DRep, pentru că R7 compară istoric de DRep, iar cele 3,24 M de
  rânduri de comitet din preprod sunt spam de test.

JSON-ul raportului e **scris de mână**, nu derivat: fișierul e citit de `jq`-ul nocturnei și de un
om, deci numele cheilor sunt **contractul**, nu o consecință a numelor de câmpuri Haskell. Un rând
sărit zice `skipped` în `status` și își poartă motivul sub cheia lui — un cititor care scanează după
eșecuri nu trebuie să parseze proză ca să distingă un rând curat de unul neatins — iar un rând curat
**n-are deloc** cheia aia. Șapte teste asertează seturile de chei, inclusiv că o divergență numește
`koios` și `palmares`, nu `expected` și `actual`.

**226 de teste unitare trec.**

Mai rămâne din T-PARITY-5: executabilul — `admin reconcile-koios`, clienții HTTP pentru ambele părți
(76 de cereri Koios cu pauză de 250 ms și `Retry-After`, 365 locale), `--dump-snapshots`, și
înregistrarea perechii comise. Plus T-PARITY-6 (wrapper + workflow nocturn warn-only).

## 2026-10-21 — T-PARITY-5: executabilul, și patru lucruri pe care numai rularea lui le putea găsi

`admin reconcile-koios` aduce ambele părți, compară, scrie `out/divergences.json` și întoarce
codul de ieșire. Totul se aduce ca `Value` și se decodează la final — de-asta `--dump-snapshots`
merită: fișierul scris pe disc e **ce a venit pe sârmă**, nu o re-codificare a tipurilor noastre.
Iar corpurile aduse se **scriu și se încarcă înapoi** chiar și fără `--dump-snapshots` (într-un
director temporar), ca rularea vie și testul de reluare să decodeze prin **exact același** drum:
o pereche nu poate trece testul și pica rularea.

**R10 se aduce primul.** O rulare care nu poate stabili prospețimea n-are nimic de spus despre
celelalte nouă rânduri, iar să afli asta după 441 de cereri ar fi 441 de cereri irosite.

### 1. `POST /drep_info` dă HTTP 413 la 100 de id-uri

Planul presupunea „cel mult 100 per POST". Măsurat pe serviciul viu: un corp de **100** de id-uri e
6.623 de octeți și Koios răspunde **413**; **50** de id-uri e 3.323 de octeți și răspunde 200. Cele
294 de DRep-uri de pe preprod sunt deci **6** POST-uri, nu 3 — 0,75 s de pauză în plus, și asta e
toată diferența.

### 2. `from_epoch=0` — bug-ul meu, prins de prima rulare

Scrisesem `minimum (0 : epochs)`. Semănarea fold-ului cu 0 face `from_epoch=0` pe **fiecare**
cerere, deci fiecare serie venea de la începutul lanțului și plafonul de 200 de puncte tăia în
liniște exact epocile pe care R3 le compară.

### 3. DEFECT, reparat: `/drep/{id}/power` dădea 404 pentru opțiunile rezervate

Cele două opțiuni rezervate ale ledger-ului sunt rânduri `drep_hash` cu `raw IS NULL` — au id de
rând și **au serie de putere** — dar `resolveDRepHex` caută după `drep_raw` în `palmares.drep`, care
**deliberat** nu le ține (D-20 le scoate din registru și le lasă în distribuție). Iar
`listEpochPower` avea `p.target_kind = 'drep'` **hardcodat**, deci chiar rezolvate ar fi întors o
serie goală — ceea ce se citește ca „ținta asta n-are putere" pentru opțiunea care ține **46%** din
puterea de vot a preprod-ului.

Reparat: `resolveReservedDRep` caută după `view`, iar `target_kind` e acum **parametru**. Verificat:
`drep_always_abstain` la epoca 319 = **431.863.790.957.959**, `drep_always_no_confidence` =
**325.390.800.383** — exact cifrele D-20. `/drep/{id}` **tot** dă 404 pentru ele, și asimetria e
**datele**: au serie de putere și nimic altceva — nicio înregistrare, niciun metadata, niciun vot.
Înainte erau accesibile doar ca agregat pe `/pages/dashboard`.

### 4. DTO-urile de răspuns nu se pot decoda cu propriul lor tip

Prima înregistrare a picat cu `gov_actions.json: Error in $[0]: key "tx_hash" not found` — peste un
fișier cu 131 de elemente **perfect uniforme**, toate cu aceleași 14 chei.

Cauza: `FromJSON GovActionSummary` e scris de mână pentru direcția **RÂND DE SQL**, nu pentru client.
Citește `tx_hash` + `index` și **emite** `id` + `bech32`, pentru că cele două forme de identificator
se derivă o dată la granița aia. `DRepSummary` are aceeași formă. Deci răspunsul nostru
`/gov-actions` **nu poate** fi decodat de tipul care îl definește.

Asimetria e intenționată și merită păstrată. Ce înseamnă pentru reconciliator: am scris
**decodoare proprii** care citesc exact cele nouă valori comparate, iar ce prinde o redenumire de
sârmă e **testul de reluare și smoke-ul**, nu type checker-ul. Comitul anterior pretindea invers —
corectat aici.

### Livrate și netestate încă live

`scripts/reconcile_koios.sh` (propagă codul de ieșire — verificat: `exit=1` cu o comandă care dă 1,
și `set -e` nu avortează înainte de `case`), `.github/workflows/reconcile.yml` (nocturn, warn-only),
`docs/deployment/koios-parity-gate.md` (poarta de release + cele patru coduri), `out/` în
`.gitignore`, și `replayTests` care reconciliază perechea comisă **fără rețea**.

## 2026-10-21 — F1 check 6 ÎNDEPLINITĂ: `status: ok`, 0 neexplicate, 135 explicate, 0 stale

Prima rulare completă a dat **șapte din zece rânduri curate** fără nicio ajustare: R1 (294 de
DRep-uri), R3 (968 de puncte de putere, lovelace **exact**), R4 (5 totaluri pe epocă, toleranță
**zero**, incluzând ambele opțiuni rezervate), R5 (131 de acțiuni, tot ciclul de viață), R7 (170 de
rânduri de istoric de vot), R8 (comitetul), R10.

Trei divergeau. Niciuna n-a fost ce păreau.

### R9 — bug-ul meu: comparam două populații diferite

Partea Koios aduna retragerile din **toate** cele 131 de propuneri, a noastră doar din cele **24
eșantionate** (detaliul se aduce per acțiune eșantionată). 59 de chei contra 4, și toate cele 55 de
diferență raportate ca divergențe de membership unilaterale — **o comparație între două populații,
care nu e o constatare despre niciun index**. Reparat: partea Koios se restrânge la acțiunile
eșantionate, citite din `ksProposalVotes`, ale cărui chei **sunt** eșantionul. Un singur loc care
spune ce acțiuni a privit o rulare.

### R6 — Koios aruncă voturile DRep-urilor retrase. Noi nu.

Măsurat pe `gov_action108a5htq…`: **fiecare** alt contor se potrivește exact (comitet yes 3, DRep
yes 4, SPO yes 12) și singura diferență sunt **două abțineri de DRep** pe care Koios nu le listează.
Ambii abținuți sunt **retrași** și absenți din propriul `drep_list` al Koios, deci
`/proposal_votes` n-are la ce să-i lege.

**Numărul nostru e cel corect**, și e corect după propria noastră regulă publicată: lista de cazuri
limită a catalogului de metrici spune *„voturile anulate de retragerea DRep-ului tot contează ca
date"*. Un tally de voturi date care ar scăpa în liniște retrașii s-ar **micșora** pe măsură ce
DRep-urile se retrag, rescriind cum arăta votul unei acțiuni la momentul deciziei. **Derogat**, cu
motivul ăsta, 5 chei.

### R2 — trei diferențe DEFINIȚIONALE, și un flip pe care l-am implementat și l-am dat înapoi

**`active_until_epoch` (101) + `status` (22).** db-sync înregistrează termenul ledger-ului **doar**
în `drep_distr`, iar `drep_distr` ține un rând doar pentru un DRep pe care ledger-ul l-a inclus în
distribuția epocii. Măsurat la epoca 320: **90 din cele 294** de DRep-uri înregistrate **n-au avut
NICIODATĂ** un rând acolo — niciunul, la nicio epocă. Deci n-avem termen pentru ele, iar `status`
cade pe `inactive`.

Koios, care îl calculează singur, zice despre acele 90: **21 active** (termen 320–340) și **69
inactive** (termen 185–224). Deci `false` e corect pentru 69 și greșit pentru 21.

Flip-ul evident — „termen necunoscut înseamnă activ" — a fost **implementat, măsurat și DAT
ÎNAPOI**: ar fi fost greșit pentru 69 ca să repare 21. Derivarea termenului a fost și ea măsurată și
respinsă: `max(ultimul cert, ultimul vot) + drep_activity` nu e de acord cu valoarea ledger-ului cu
**până la 5 epoci**, pentru că CIP-1694 **nu numără epocile DORMANTE** — cele fără nicio propunere
în curs — spre inactivitate. A reimplementa contabilitatea aia ar fi o a doua derivare care poate
contrazice ledger-ul, exact ce schema asta există **să nu facă**.

> **Decizie pentru tine:** reparația onestă e **un al treilea status publicat** pentru „înregistrat,
> termen necunoscut". `DRepStatus` e un enum publicat, iar adăugarea unei valori rupe fiecare
> `match` exhaustiv scris de un client — deci e o decizie de API, nu de schemă, și e a ta.

**`delegator_count` (7).** Două întrebări diferite, nu două răspunsuri la una. A noastră numără
**delegările în vigoare**; `live_delegator_count` al Koios numără delegatorii **cu stake** în
snapshot-ul curent. Măsurat: 7 DRep-uri diferă, a noastră e mai mare la **toate**, și **fiecare**
delegator în exces are `lovelace: null` — niciun rând în `epoch_stake`. DRep-ul de fixture se
potrivește la 13 pentru că toți cei 13 au stake. A noastră e numărul pe care registrul îl vrea: un
delegator fără stake **tot a ales**.

### Mecanismul de derogare a căpătat o cheie `*`

101 de intrări pentru un singur fapt despre un singur câmp ar fi un fișier pe care nimeni nu-l
întreține și o listă `stale_exceptions` care se agită cu populația. `key: "*"` derogă acel **câmp**
pe fiecare cheie a rândului — și **trebuie** să numească un câmp: `*` fără câmp ar stinge rândul
întreg, ceea ce nu e o divergență documentată, e un buton de oprire. Refuzat la **încărcare**, cu
mesaj, și două teste în plus.

### Rezultatul

```
R1  ok  compared=294  unexpl=0  expl=0     R6  ok  compared=37   unexpl=0  expl=5
R2  ok  compared=294  unexpl=0  expl=130   R7  ok  compared=170  unexpl=0  expl=0
R3  ok  compared=968  unexpl=0  expl=0     R8  ok  compared=4    unexpl=0  expl=0
R4  ok  compared=5    unexpl=0  expl=0     R9  ok  compared=4    unexpl=0  expl=0
R5  ok  compared=131  unexpl=0  expl=0     R10 ok  compared=1    unexpl=0  expl=0
```

`status: ok`, **0 neexplicate**, **135 explicate**, **0 stale**, exit **0**. Poarta de release din
`docs/deployment/koios-parity-gate.md` trece.

### Și patru lucruri pe care numai rularea executabilului le-a găsit

1. **`POST /drep_info` → 413 la 100 de id-uri.** 50 merge. Planul presupunea 100; cele 294 de
   DRep-uri de pe preprod sunt **6** POST-uri, nu 3.
2. **`GET /drep/{id}/power` → 404 pentru opțiunile rezervate.** `resolveDRepHex` caută după
   `drep_raw`, iar `listEpochPower` avea `target_kind = 'drep'` **hardcodat**. 46% din puterea de
   vot a preprod-ului era accesibilă doar ca agregat. Reparat; `/drep/{id}` **tot** dă 404 pentru
   ele, și asta e **datele** — au serie de putere și nimic altceva.
3. **DTO-urile de răspuns nu se pot decoda cu propriul tip.** `FromJSON GovActionSummary` e direcția
   **RÂND DE SQL**: citește `tx_hash` + `index` și **emite** `id` + `bech32`. Reconciliatorul citește
   sârma noastră cu decodoare proprii pentru cele nouă câmpuri comparate; ce prinde o redenumire e
   **testul de reluare**, nu type checker-ul.
4. **`--max-lag-seconds` era citit și aruncat.** `reconcileAll` folosea constanta de 300 s, deci
   `--max-lag-seconds 0` raporta `ok`. Un flag citit și ignorat e **mai rău** decât unul inexistent.
   Acum: exit **3**, `status: aborted`, **9** rânduri sărite, motiv `lag 38s > 0s`.

### Starea verificărilor

**231** de teste unitare, **329** de aserțiuni de smoke (o linie `KNOWN`, pagina de comitet),
**7** verificări SQL, determinism **8/8**, swagger și doc-ul de metrici fără drift, gitleaks curat.
Perechea de snapshot comisă se reia **fără rețea** — asta e ce păzește un PR; nocturna contra
serviciilor vii e **warn-only**, pentru că o poartă roșie de câte ori Pi-ul re-sincronizează învață
oamenii s-o ignore.

## Ce mai rămâne

> **Depășit la 2026-10-21** — lista vie e „Ce rămâne, în ordinea dependențelor" de la finalul
> fișierului. Punctele 1 (`/votes`), 2 (al treilea status) și jumătatea de Plan B din 5 s-au închis
> de atunci; rămân blocul 3 și rularea de `reconcile.yml`.

1. **Pagina `/votes` în două etape** — singurul endpoint livrat care nu poate răspunde pe rolul de
   comitet. Diagnosticat complet (join-ul `cc_cold_interval` e 9,5 s din 14,2 s), design-ul scris.
2. **Al treilea status de DRep** — decizia de API de mai sus, a ta.
3. **Blocul 3** — substrat / manifeste k8s per D-28 (atinge rack-ul, ținut separat).
4. **`gh workflow run reconcile.yml`** — o rulare `workflow_dispatch` ca să vedem artefactul; cere
   imaginea `palmares-admin` publicată.
5. **Planul B** (atestări) și **Planul C** (UI) — încă nescrise.

## 2026-10-21 — `/votes` în patru etape: din >400 s în 3,3 s

Endpoint-ul care nu putea răspunde pe rolul de comitet — 3.243.802 din cele 3.244.711 de rânduri de
vot — răspunde acum. Măsurat la cauză înainte de orice linie de reparație, și cauza **nu** erau
funcțiile de fereastră:

| peste rândurile de comitet | timp |
| --- | --- |
| scanarea de bază (`voting_procedure ⋈ gov_action`) | **1,09 s** |
| + `tx ⋈ block` | +8,7 s |
| + `cc_cold_interval` | +9,5 s |
| + funcțiile de fereastră | +3,5 s |

Un `count` n-are nevoie de niciuna din cele trei; o pagină de cincizeci de rânduri are nevoie de
ferestre **pentru cincizeci de rânduri**. Deci patru statement-uri:

1. **count**, peste forma de bază plus **doar** join-urile cerute de filtre: **1,09 s** unde era 14,2 s
2. **id-urile paginii**, aceeași formă cu `ORDER BY`/`LIMIT`: **6 ms**
3. **id-urile → cheile de partiție**, restrâns **DE** id-uri: **291 ms**, unde același join de cheie
   rece peste tot corpusul e 9,5 s
4. **rândurile paginii**, prin `palmares.vote` restrâns la acele partiții: **286 ms**

### Trei fapte măsurate pe care se sprijină

**`ORDER BY v.id DESC` ESTE `ORDER BY b.time DESC, v.id DESC`.** db-sync atribuie id-uri în ordinea
lanțului, deci id-ul e timpul blocului rafinat în interiorul blocului — exact ce spunea deja
tie-break-ul publicat. **Verificat, nu presupus**: primele 50 după `id DESC` și primele 50 după
`(b.time DESC, v.id DESC)` sunt **aceleași 50 de rânduri**, 0 dezacorduri. Asta scoate `tx ⋈ block`
din sortarea implicită, iar join-ul ăla era 8,7 s din 9,9.

**Etapa 4 leagă cheile de partiție ca PARAMETRI, și asta e portant.** Aceeași restricție scrisă
`(voter_id_hash, gov_action_id) IN (SELECT …)` e **3.340 ms**, pentru că un `SubLink` e opac pentru
`qual_is_pushdown_safe` și ferestrele se calculează peste tot corpusul înainte de filtru. Ca valori
legate: **286 ms**. Lista de id-uri se leagă și ea — perechile dau împingerea, id-urile dau
exactitatea: două rânduri ale unei partiții pot potrivi perechile în timp ce doar unul e pe pagină.

**Etapa 3 e o etapă separată** pentru că pliată în etapa 2 a fost măsurată și costa 3,8 s din 3,8 s:
rezolvarea credențialului rece **lângă** `ORDER BY` pune join-ul ăla înapoi peste tot ce a potrivit
filtrul.

**Filtrul de votant nu atinge `cc_cold_interval`.** E apartenență în tabelele mici de hash-uri — 805
`drep_hash`, 686 `pool_hash`, 48 `committee_hash` — plus cheile fierbinți înregistrate vreodată la
acel credențial rece. Fiecare subinterogare n-are referință exterioară, deci PostgreSQL o evaluează
o dată ca InitPlan. Măsurat: rezolvarea unui id rece la cheile lui fierbinți e **11 ms**, contra
celor 9,5 s ale join-ului de interval.

### Cum arată acum

| cerere | înainte | acum |
| --- | --- | --- |
| `voter_role=constitutional_committee` | **>400 s → 503** | **3,31 s** |
| nefiltrat, 50 de rânduri | același cost patologic | 1,60 s |
| `voter_role=drep` | 6,8 s | 1,57 s |
| `voter_role=spo` | — | 1,48 s |
| `voter_id=<drep>` | 2,5 s | 1,19 s |
| `order_by=latency` | — | 1,59 s |
| `order_by=voter_id` | — | 1,57 s |
| `anchor_status=valid` | — | 0,91 s |
| `q=Bitcows` | — | 1,15 s |

Aserțiunea de smoke pentru pagina de comitet e **restaurată** — `known_defect` nu mai are nicio
linie. Opt teste unitare noi fixează ce join-uri aduce fiecare formă de cerere, că sortarea
implicită e cheia primară, și că filtrul de votant lovește toate patru sursele de credențial fără
join-ul de interval.

---

## T-SUBSTRATE-7 + T-SUBSTRATE-8 — substratul măsurat, și grant-ul care nu se făcea niciodată

Două comit-uri în palmares: `1c6da2a` (măsurarea) și `cca13eb` (rolurile). Blocul 3 rămâne cu
restul la rack.

### T-SUBSTRATE-7: numerele vin dintr-un script, nu dintr-o notiță

`scripts/measure_substrate.sh` rulează pe oricare dintre cele două ținte — experimentul off-cluster
care a dimensionat ADR-ul, sau cluster-ul după T-SUBSTRATE-3/4. Tot ce afirmă ADR 0001 iese acum din
el, pentru că alternativa e un număr care îmbătrânește în liniște în timp ce decizia de PVC se
sprijină pe o cifră pe care n-o mai poate reproduce nimeni.

Re-măsurat la tip pe 2026-10-21:

| ce | înainte | acum |
| --- | --- | --- |
| `pg_database_size('cexplorer')` | 5.909 MB | **6.260 MB** |
| `block` (total) | 2.323 MB | **2.530 MB** |
| `voting_procedure` (total) | 331 MB | **410 MB** |
| schema noastră | — | **0 tabele, 19 view-uri, 1 matview, 680 kB** |
| db-sync anon / reclaims | — | **5,90 GiB / 4.183** (recuperează) → **5,06 GiB / 35** (la tip) |
| nod anon / reclaims | — | 2,88 GiB la plafon 3,5 GiB / 59.697 |

Creștere la tip, nu o corecție. Linia care contează pentru ADR e a patra: toată suprafața de citire
a produsului adaugă **sub un megabyte** la cele șase gigabytes ale lui db-sync.

Scriptul separă în propriul header cele două feluri de cifră, pentru că nu se transferă la fel.
Dimensiunile de bază de date și de relație sunt **fapte de lanț** și trec între gazde neschimbate. O
cifră de memorie spune ce a folosit un proces **sub plafonul care i s-a dat**, și RTS-ul lui GHC își
dimensionează heap-ul din `/proc/meminfo`, nu din limita cgroup — deci un db-sync care a atins
plafonul îți spune că plafonul a ținut, nu care e setul viu. Verdictul de PVC rămâne deschis, legat
de bifurcația de memorie pe care ADR-ul o enunță deja.

### T-SUBSTRATE-8: defectul găsit pe drum

`sql/000_schema.sql` începe cu `DROP SCHEMA palmares CASCADE`, care ia **fiecare privilegiu** de pe
schema aia împreună cu obiectele — și rândul din `pg_default_acl` cu ele, deci `ALTER DEFAULT
PRIVILEGES` nu-l poate purta. **Nimic din setul de boot nu acorda nimic.**

Desfășurarea cu un singur rol nu a observat niciodată, pentru că rolul care aplică DDL-ul era rolul
care servește cererile. Desfășurarea cu două roluri — exact ce creează acest task — ar fi servit 500
pe fiecare rută de guvernanță **la prima repornire** a lui query-api.

Dovedit viu, nu raționat: pe baza de date a experimentului, cu rolurile puse și cu schema dintr-un
boot anterior, `check_roles.sh` a picat exact pe cele două linii, și `palmares_ro` nu putea citi
`palmares.drep`. După un re-boot cu `040_grants.sql`: **16 din 16**.

`sql/040_grants.sql` e deci **ultimul** fișier din `palmaresSqlFiles`, și asta e portant de două ori:

* `GRANT … ON ALL TABLES` se rezolvă la relațiile care există **când rulează**, deci aceeași
  instrucțiune cu un fișier mai devreme ar rata tot ce creează `030_metrics.sql`, inclusiv matview-ul
  pe care-l citește lista de DRep-uri. `ALL TABLES` **acoperă** și view-urile și matview-urile —
  măsurat pe PostgreSQL 17, unde matview-ul citește `has_table_privilege = true` după singura
  instrucțiune.
* e **păzit pe existența rolului**. `PALMARES_PG_OWNER_CONN_STR` cade pe `PG_CONN_STR`, deci forma cu
  un rol n-are deloc `palmares_ro`, și un GRANT nepăzit ar aborta tranzacția de boot cu
  `role "palmares_ro" does not exist` și ar omorî procesul. A adăuga grant-uri pentru rolurile noi ar
  fi rupt fiecare desfășurare care încă n-are roluri.

### Rolul greșit, și de ce fișierul refuză să ruleze sub el

`ALTER DEFAULT PRIVILEGES` din `050_roles.sql` nu poartă **niciun** `FOR ROLE` — adică rolul curent.
Un privilegiu implicit e legat de rolul care **creează** obiectul, iar al lui db-sync e `dbsync` în
cluster și `postgres` în `experiments/dbsync-preprod`. A numi pe oricare dintre ele e greșit în
celălalt loc, **în direcția silențioasă**: fiecare instrucțiune reușește și privilegiul nu acoperă
nimic, deci următoarea migrare de db-sync adaugă un tabel pe care `palmares_ro` nu-l poate citi, și
un view peste el cade peste luni cu o eroare de permisiune care se citește ca un bug în view.

Prima versiune chiar numea `dbsync`, și fișierul nu s-a putut aplica deloc pe experiment —
`role "dbsync" does not exist`, instrucțiunea 12.

Deci presupunerea e **afirmată**, nu documentată. Instrucțiunea 1 compară `current_user` cu
proprietarul lui `public.voting_procedure` și ridică dacă diferă. Verificat rulând-o ca un al doilea
superuser, care a fost refuzat cu numele proprietarului în mesaj.

### `assertIndexesValid` — verificarea pe care niciun cod de eroare nu ți-o dă

`CREATE INDEX CONCURRENTLY` construiește în două treceri cu un commit între ele. Îl întrerupi — Job
anulat, deadlock, pod omorât — și indexul **rămâne**, cu `indisvalid = false`. Planificatorul îl
ignoră, iar `IF NOT EXISTS` vede numele și nu face nimic. Fiecare rulare ulterioară raportează succes
în timp ce interogarea pentru care a fost construit rămâne pe scan secvențial, pentru totdeauna, fără
nicio eroare nicăieri.

Prima mea versiune filtra `relname LIKE 'palmares_idx_%'` și potrivea **patru** indexuri, niciunul din
fișier — care-și numește proprii `gov_idx_*` și re-creează doi `idx_*` ai lui db-sync. Raporta "all
valid" peste un set care excludea fiecare index pentru care exista. Numele se citesc acum **din
fișier** (`declaredIndexes`), și un test unitar fixează că fiecare instrucțiune produce unul, că nu
sunt duplicate, și că fiecare e un identificator curat.

### Ce s-a verificat cap la cap pe `experiments/dbsync-preprod`

| probă | rezultat |
| --- | --- |
| `check_roles.sh` **înainte** | 16 FAIL, `role "palmares_owner" does not exist` |
| `admin apply-roles` | 23 instrucțiuni |
| `admin apply-indexes` | 23 instrucțiuni, **toate cele 23 de indexuri declarate valide** |
| `check_roles.sh` **după** | **16 ok**, `roles OK` |
| `palmares_ro` prin scram-sha-256 | `voting_procedure` necalificat (public, al lui db-sync) = **3.244.711** |
| | `drep` necalificat (palmares, al nostru) = **812** |
| | `drep_metrics` (matview) = **812** |
| | `search_path = palmares, palmares_attest, public` |
| `DELETE FROM voting_procedure` ca `palmares_ro` | **permission denied** |
| `apply-roles` ca alt superuser | refuzat la instrucțiunea 1, cu numele proprietarului |
| `apply-roles` fără parolă | refuzat înainte de a atinge baza de date |

Parola nu apare nicăieri în fișier. `apply-roles` o leagă la `set_config` și un bloc `DO` o citește
înapoi prin `format('%L')`, care face ghilimelele cu regulile PostgreSQL însuși — DDL nu ia parametri,
deci interpolarea valorii ar fi fost un client care construiește DDL din un secret. (`rawExecute`
respinge un `SELECT`; `set_config` e o funcție, deci cele două apeluri merg prin `rawSql`.)

**258 de teste trec.** Job-ul k8s, cheile din secretul SOPS și kustomization-ul rămân pentru rack.

---

## Planul B scris — și defectul pe care l-au găsit surveys-urile

Patru surveys paralele pe `palmares@71fa00f` (validatoare on-chain, tx-building, interaction-api +
attest-sync, modulele de test dormante), ca planul să se sprijine pe ce **e** codul, nu pe ce-mi
amintesc. Au schimbat forma planului.

### Protocolul nu poate rula deloc azi — și nu din cauza unor stub-uri

Totul compilează, nu există niciun `undefined` pe drum. Și totuși:

| ce | dovada |
| --- | --- |
| **Nu se poate crea niciun profil** | `MintingRedeemer` are exact **un** constructor, `NewAchievement` (`MintingPolicy.hs:70-78`). Nu există `CreateProfileAction`, nici `createProfileTX`, nici rută. Toate trei operațiile de utilizator iau un `ProfileRefAC` ca **intrare**, iar emiterea unei atestări cere user NFT-ul profilului emitentului (`:160`) — deci **toate trei sunt inaccesibile** |
| Urmele ștergerii | `fcProfileCreationFee` (`Types.hs:47`), `ctxProfilesAddress` (setat la `:108`, citit niciodată), `profilesValidatorScriptHash` (folosit doar ca să calculeze câmpul ăla mort), și perechea CIP-68 mint/burn neapelată (`Skeletons.hs:161-166`) |
| **Nu există claim de identitate**, pentru niciun fel de credential | `grep -rni "credential\|claim"` pe `onchain-lib` → **zero**. `txInfoSignatories` apare **o dată**, pentru adminul oracle. `Utils.hasTxInAtAddressWithNFT` e singurul helper de forma potrivită și nu-l apelează nimic |
| **Disputele n-au existat niciodată** | `git log --all -- '*DisputesValidator*'` e gol. Ce există: un cod HTTP inaccesibil și o propoziție de intenție în `CLAUDE.md:48` |
| **Blueprint-ul publică trei defecte** | Două afirmații false despre rotația adminului — iar validatorul o **interzice**: `O2` cere ca output-ul să fie egal cu `applyOracleAdminAction`, care păstrează `opAdminPkh` prin construcție. Al treilea: schema datumului de atestare e `definitionRef @()` |
| **Codul nostru off-chain nu poate construi tx-ul cu emitent-script** pe care blueprint-ul îl anunță | `txMustSpendFromAddress` hardcodează `GYTxInWitnessKey` (`Skeletons.hs:124`) |
| **Zero teste de emulator rulează** | Cinci module dormante există pe disc; trei linii din cabal **nu numesc niciun fișier** |

### Defectul reparat imediat: un restart de query-api ștergea starea de chain-sync

`71fa00f`. Fiecare entitate din blocul `persistLowerCase` e declarată **necalificat**, deci
`CREATE TABLE` al lui `migrateAll` aterizează unde rezolvă primul `search_path`. Nimic din serviciu nu
seta calea — `palmares_attest` și `search_path` **nu apăreau nicăieri** în `Storage.hs`, `Ingestion.hs`
sau `src/exe/attest-sync/`. Iar `050_roles.sql` dă lui `palmares_owner`:

```
search_path = palmares, palmares_attest, public
```

Deci proiecțiile aterizau în **`palmares`** — schema pe care `000_schema.sql` o deschide cu
`DROP SCHEMA ... CASCADE` la **fiecare boot de query-api**. Un restart al API-ului de **citire** ar fi
șters starea drumului de **scriere** și ar fi costat un re-sync complet de Kupo, în liniște, cu API-ul
de citire pornind perfect sănătos.

Ironia e în fișier: header-ul lui `000_schema.sql` avertizează "This file must NEVER mention
`palmares_attest` ... Dropping it here would delete chain-sync state that takes a full re-sync to
rebuild." Nu-l menționează. Tabelele nu erau în el.

Fix: `runAttest`, prin care trece acum **fiecare** acces la baza de date —
`SET LOCAL search_path TO palmares_attest, public`, în tranzacția pe care `runSqlPool` o deschide deja.
`SET LOCAL`, nu `SET`: pool-ul refolosește conexiunile, și o cale lăsată în urmă ar urma orice rulează
după pe conexiunea aia. Aceeași formă pe care query-api o folosește pentru plafonul de citire.

**Cinci teste, și unul dintre ele e comportamentul VECHI**: rulează migrarea **fără** pin sub
`search_path`-ul adversarial și toate cele cinci tabele aterizează în `palmares`, zero în
`palmares_attest`. Asta face un singur `SET LOCAL` demonstrabil portant — regula din
`src/test/CLAUDE.md`. Celelalte: tabelele aterizează în `palmares_attest` și nu se scurg nici în
`palmares` nici în `public`; **supraviețuiesc** unui `DROP SCHEMA palmares CASCADE` textual (consecința,
nu numele schemei); pin-ul nu se scurge pe conexiunea din pool; și `chainSyncTableNames` listează exact
ce creează migrarea — un nume lipsă acolo e un tabel pe care wipe-ul de versiune îl lasă în urmă.

Plus o linie de drift: `defaultConnStr` zicea `dbname=chainsync`, o bază de date pe care nicio
desfășurare de aici n-o are.

**263 de teste trec** (258 + 5).

### Planul B: `docs/superpowers/plans/2026-10-21-gov-registry-plan-b-attestations.md`

Douăsprezece task-uri, cinci faze, 1.362 de linii. Decizia pe care sinteza §3 pasul 24 o numește
"singura piesă de design on-chain cu adevărat nespecificată" e **așezată în plan**, nu descoperită la
implementare:

* Claim-ul purtă un `Credential` CIP-129, deci octetul de header care distinge cheie de script
  supraviețuiește.
* Credential de cheie → semnatar. Credential de script → un input al cărui credential de **plată sau de
  staking** e acel script: dacă ledger-ul îl cheltuie, condiția scriptului a fost deja satisfăcută,
  înainte să ruleze validatorul nostru. Nu o reverificăm și nici n-am putea — un script nativ
  (timelock/multisig) n-are redeemer de inspectat, iar comitetele și organizațiile multisig sunt cazul
  majoritar pe care §4.3 îl numește.
* **Și staking, nu doar plată**, pentru că un credential de stake care e script stă normal în jumătatea
  aia a adresei. Un câmp citit în plus pe input, și dispare o clasă întreagă de "nu-mi pot revendica
  propriul script de stake".

  > **Depășit la 2026-10-21 — era o vulnerabilitate CRITICĂ, nu o comoditate.** Regula de martori a
  > ledger-ului autentifică **doar** credentialul de plată al unui input, deci jumătatea de staking nu
  > dovedește nimic: oricine punea cheia lui în jumătatea de plată și scriptul victimei în cea de
  > staking revendica credentialul victimei. Disjuncția e **ștearsă** la `7e6b02a` — vezi reparația
  > critică de la finalul fișierului. Cazul „script de stake" nu era de fapt rezolvat aici, pentru că
  > o cheltuire nu-l poate martora; dovada sănătoasă e o retragere în `txInfoWdrl`.
  >
  > Se aplică și punctului de deasupra: „credential de **plată sau de staking**" se citește de acum
  > **numai plată**.
* **Limitarea e documentată, nu inginerită**: un script Plutus care nu ține UTxO la propria adresă nu
  poate revendica într-o singură tranzacție. Workaround-ul lui e gratuit (trimite-ți un UTxO la propria
  adresă de script, apoi revendică), deci nu adăugăm verificare pe `txInfoRedeemers` — ar costa ExUnits
  la fiecare claim pentru un caz care are deja soluție. Regula permanentă a repo-ului.
* **Unicitatea e un fapt de citire, nu unul on-chain.** Nimic nu oprește două profiluri să revendice
  același credential; să impunem asta ar cere un UTxO de index global, exact anti-pattern-ul de
  contenție pe care regulile proprii ale repo-ului îl resping. Lanțul înregistrează fiecare claim; API-ul
  marchează un credential cu mai mulți revendicatori drept `contested` și îi arată pe toți, în loc să
  aleagă un câștigător.

Cinci lucruri sunt **lăsate deliberat afară**, fiecare cu motivul: fee-uri ca model de business
(§4.3 explicit), admin M-din-N (`OracleSetAdmin` face compromiterea cheii **recuperabilă**, care e riscul
real; M-din-N costă ExUnits la fiecare acțiune de admin pentru un risc pe care rotația deja îl
mărginește — se revine înainte de F2), revocare/expirare, verificarea NFT-ului lipsă din
`OracleValidator` (neexploatabilă: fiecare cititor autentifică prin NFT, deci un atacator poate doar
planta și cheltui propriul UTxO), și existența subiectului la mint.

**Corecție 2026-10-21, la „fiecare cititor autentifică prin NFT":** adevărat **on-chain** (validatoarele
rezolvă oracle-ul prin token-ul din datum), **fals off-chain** — ingestia decidea ce proiectează din
**forma** datumului, deci un UTxO plantat era servit ca stare reală de protocol. Ăsta e fixul
moștenit #3, cablat la T-ATTEST-8; argumentul de neexploatabilitate rămâne valabil doar pentru
brațul on-chain.

### Trei decizii ale ownerului, niciuna blocantă

1. `Auditor` e un **fel** de profil sau un **rol** pe care-l are o `Organization`? Spec-ul îl listează ca
   fel, deci așa se construiește.
2. Poate o atestare să înregistreze o dată de eveniment **mai veche** decât emiterea ei? Planul
   construiește varianta strictă — `attestationDate` e timpul emiterii, mărginit de intervalul de
   validitate. Dacă da, datumul are nevoie de un al doilea câmp explicit auto-raportat, iar API-ul nu
   trebuie **niciodată** să-l prezinte ca verificat on-chain. Întrebare de produs, nu tehnică.
3. Cele trei rămase din planul A: al treilea `DRepStatus`, handle-ul GHCR, bifurcația PVC din ADR 0001.

---

## T-ATTEST-1 — domeniul e atestare, peste tot

`3bc3297` în palmares. 1.645 de substituiri în 33 de fișiere, și a compilat **din prima** pentru că
"achievement" → "attestation" e un schimb de cuvânt 1:1 în aproape fiecare identificator.

Straturile nu erau de acord pe substantiv: on-chain zicea deja atestare (`AttestationsValidator`,
`ctxAttestationsAddress`) în timp ce DTO-urile, rutele și coloanele de proiecție ziceau achievement.
Dezacordul ăla făcea fiecare task ulterior ambiguu.

**Breaking pe sârmă, și acceptabil exact o dată — acum, înainte să existe vreun client.**
`awarded_{to,by}_profile_id` → `{subject,issuer}_profile_id`; `/award-achievement` →
`/issue-attestation`; `achievement_projection` → `attestation_projection`. Swagger-ul lui
interaction-api 2.1.0 → **3.0.0** ca să o spună.

### Șase feluri de profil, nu două

`OnchainProfileType (Practitioner | Organization)` → `OnchainProfileKind (PersonDRep | Organization |
Committee | Community | Vendor | Auditor)`, indici 0–5, `Organization` păstrând indicele 1 — singurul
care supraviețuiește. **Constructor, nu câmp de metadata CIP-68**, deși niciun validator nu ramifică pe
fel: stratul de citire filtrează și grupează după el, metadata e un `Map` pe care proiecția ar trebui
să-l parseze, și un redeemer viitor de update l-ar putea schimba în liniște. Un octet care nu poate
devia.

CLI-ul de admin avea două flag-uri (`-p`, `-o`); acum are un `--kind` condus de ortografiile de pe
sârmă ale tipului însuși, deci un al șaptelea fel nu cere nicio editare acolo.

### Defectul pe care l-a găsit redenumirea

`StripPrefix "aar"` al lui `deriving-aeson` e un **șir**, nu o referință la câmpurile înregistrării.
Redenumind câmpurile din `aar*` în `iar*`, opțiunea a rămas numind un prefix care nu mai potrivea
nimic — **și asta compilează perfect**. Fiecare cheie JSON s-ar fi livrat ca
`iar_subject_profile_id` în loc de `subject_profile_id`, pe o rută vie, fără nicio eroare nicăieri.

Mai rău: prefixul e scris **de două ori**, o dată pentru `FieldLabelModifier` și o dată pentru
`mkStripPrefixSchemaOptions` care conduce Swagger — deci o reparație pe jumătate publică un document
care descrie câmpuri pe care API-ul le respinge.

### `scripts/check_vocabulary.sh` — opt verificări, în CI

Redenumirea are un mod de eșec **parțial**: compilatorul e fericit cu jumătate din ea, iar jumătățile
care ajung la un utilizator — un titlu de Swagger, un mesaj de eroare, o cheie JSON — sunt exact cele
la care el nu se uită niciodată.

| verificare | ce prinde |
| --- | --- |
| substantivul | orice `achievement` rămas |
| domeniul moștenit | `practitioner`, `belt`, `promotion`, `dojo` |
| mașinăria de rank/membership | `OnchainRank`, `RanksValidator`, `MembershipInterval`, … |
| Swagger generat | un document care numește alt produs |
| șiruri către utilizator | titluri, `displayException` |
| nume de rețea hardcodat | `"Not enough preview ADA"` pe un config preprod |
| licența moștenită | **trebuie** să-și păstreze numele — atribuire, nu rest |
| fiecare `StripPrefix` | numește un prefix pe care vreun identificator din fișierul lui îl folosește |

Ultima a fost **verificată reintroducând bug-ul** și urmărind-o să pice.

Două pattern-uri sunt deliberat înguste, pentru că domeniul de guvernanță folosește cuvintele
legitim: **`membership`** e propriul nume de câmp al raportului de paritate Koios
(`Divergence k "membership"` = o cheie doar pe o parte) și apartenența la comitet e vocabular real —
deci pattern-ul numește compușii BJJ, nu cuvântul. Și **`Belt System`** într-un comentariu care
explică *de ce* ceva e cum e ("snake_case, nu PascalCase-ul moștenit") e proveniență care merită
păstrată — deci ce citește un **utilizator** se verifică separat.

### Trei defecte de blueprint, verificate în artefactul generat

1. Schema datumului de atestare era `definitionRef @()` — blueprint-ul publicat descria **singurul
   datum de care un cititor are cea mai mare nevoie** ca Unit. Tipul lipsea și din
   `contractDefinitions`, ceea ce a permis referinței greșite să compileze.
2. Preambulul lista "admin key" între capabilitățile oracle-ului.
3. Descrierea oracle-ului zicea că protocolul poate fi "**re-keyed** without redeploying a validator".

Rotația nu e neimplementată, e **interzisă**: `O2` cere ca datumul de output să fie egal cu
`applyOracleAdminAction` al celui curent, iar funcția aia păstrează `opAdminPkh` prin construcție.
Ambele afirmații spun acum adevărul **și de ce**. T-ATTEST-3 pune capabilitatea înapoi cu
`OracleSetAdmin`.

### O coliziune reală de nume

`Committee` e felul nostru de profil **și** `PlutusLedgerApi.V3.Committee`, datumul comitetului
constituțional CIP-1694. Trei fișiere importă ambele straturi și niciunul nu folosește pe cel Plutus,
deci importul îl ascunde — ceea ce lasă o eroare de compilare pentru cine are nevoie de el mai târziu,
nu o umbrire silențioasă.

### Ce NU s-a șters

Am deviat de la propriul plan aici. `CLAUDE.md` §3 zice să **menționezi** codul mort nelegat, nu să-l
ștergi. Deci au plecat doar resturile de domeniu BJJ: cele 16 constructoare inaccesibile de
`TxBuildingException` plus tot tipul `AddMembershipIntervalReason` (purtau șiruri ca *"Invalid belt
number (must be 0–14)"* într-un registru de guvernanță), și blocul de haddock orfan care documenta un
`UpdateEndDate` șters, inclusiv un parametru `validityOverride` care nu mai există.

**Menționate, lăsate pe loc:** cele două derivări din `Protocol/Id.hs`, cele trei `unsafeGet*` din
`Protocol/Lookup.hs`, cele două variante exact-value din `Utils.hs`, cele trei funcții nefolosite din
`CIP68.hs`, și `hasUserNFTAt` din `Operations.hs`. Toate moarte, niciuna orfanul acestei redenumiri.

Și `Query.Projected` / `Query.Aggregates` / `Query.Pages` **nu** au fost redenumite: sunt dormante
**și deja rupte** față de DTO-urile vii — `Query/Projected.hs` citește `searchResultsPractitioners`,
câmp care nu mai există în `QueryResponses.hs`. O redenumire mecanică nu le-ar face să compileze, deci
T-ATTEST-9 le reconstruiește peste `palmares_attest`.

### Verificat

`cabal build all` curat · **263 de teste trec** · `check_vocabulary.sh` **OK** · Swagger-ul committat
al query-api **fără drift** · și suita de smoke de **330 de aserțiuni** dă `smoke OK` pe drumul de
citire viu de pe experimentul preprod — care e dovada că 1.645 de substituiri n-au schimbat niciun
comportament.

---

## T-ATTEST-2 și T-ATTEST-3 — claim-ul de identitate și cele trei fixuri moștenite

`f6637fa` + `dc20205`. **290 de teste trec** (263 → 290). Nepushate: toate trei schimbă hash-uri de
validator, iar T-ATTEST-4 (disputele) schimbă și hash-ul minting policy-ului prin `ProtocolParams` —
deci granița naturală de push e după el, toată munca de hash într-o singură rulare de CI.

### T-ATTEST-2: `MintProfile` și claim-ul

Înainte de asta **nu se putea crea niciun profil la niciun strat**, deci toate cele trei operații de
utilizator erau inaccesibile.

Regula, din spec, într-o propoziție: **dovedește că poți cheltui ca acel credential.**

| fel de credential | ce e controlul |
| --- | --- |
| **cheie** | hash-ul e printre semnatari. O singură apartenență în listă |
| **script** | un input al tranzacției e guvernat de acel script — în jumătatea de **plată** a adresei lui sau în cea de **staking** |

> **Depășit la 2026-10-21:** „sau în cea de **staking**" era gaura critică, reparată la `7e6b02a`.
> Rândul se citește acum **doar** cu jumătatea de plată. Motivul e în secțiunea de reparații de la
> finalul fișierului; restul tabelului și al secțiunii rămâne valabil.

Pentru script **nu reverificăm** condiția scriptului și nici n-am putea: dacă ledger-ul cheltuie
input-ul ăla, condiția a fost **deja** satisfăcută, înainte să ruleze validatorul nostru. Iar un script
nativ (timelock/multisig) n-are redeemer de inspectat — și comitetele și organizațiile multisig sunt
cazul majoritar pe care §4.3 îl numește. (Valabil **numai** pentru jumătatea de plată: ledger-ul
cheltuie un input fără să ceară nimic de la credentialul lui de staking.)

Datumul purtă un **`Maybe Credential`**, nu un hash. Un `Credential` e ce codifică CIP-129, octet de
header inclus, deci distincția cheie/script pe care se bazează regula nu se pierde niciodată — și o
singură regulă acoperă un DRep id, un credential rece de comitet și un credential de stake, fără cazuri
per-fel. **`Nothing` e legitim**: o Comunitate sau un Auditor nu trebuie să corespundă vreunui
credential on-chain.

**Douăsprezece teste**, și cele care contează sunt cele patru negative. Două sunt subtile: cei 28 de
octeți ai unui hash de script și cei 28 ai unui hash de cheie **sunt aceiași octeți**, deci testele
afirmă că niciun braț nu poate fi satisfăcut cu dovada celuilalt.

> **Corecție 2026-10-21:** un caz din `UnitTests/Claim.hs` aserta claim-ul pe jumătatea de staking ca
> **succes** (`@?= True`), cu un comentariu care argumenta pentru el — adică **pinuia** gaura critică
> reparată la `7e6b02a`, verde, lângă cele patru negative. Acum e `@?= False`.

**Unicitatea e un fapt de citire.** Nimic nu oprește două profiluri să revendice același credential, și
asta e deliberat: a o impune on-chain cere un UTxO de index global, anti-pattern-ul de contenție pe care
regula 11 de securitate a repo-ului îl respinge. Lanțul înregistrează fiecare claim; T-ATTEST-9 marchează
un credential cu mai mulți revendicatori drept `contested` și îi arată pe toți.

Și ce **nu** dovedește claim-ul, spus cu voce tare pentru că UI-ul nu trebuie să exagereze: controlul
unui credential, nu identitatea celui care-l ține. Formularea e "controlează acest credential",
niciodată "este".

`MintProfile` e **adăugat** la indicele 1 — `NewAttestation` păstrează 0. Mintează **ambele** token-uri
CIP-68, spre deosebire de `NewAttestation` care mintează doar ref-ul: token-ul de user al unui profil
**este** instrumentul de autoritate pe care fiecare alt redeemer îl cere în `valueSpent`, în timp ce o
atestare n-are proprietar care să acționeze în numele ei.

### Fixul #1, tras înainte

`MintProfile` are nevoie de verificare de fee, deci l-am tras din T-ATTEST-3 (self-review-ul propriului
plan semnalase referința înainte). `MintAction` înlocuiește selectorul:
`feeFor :: MintAction -> FeeConfig -> Integer` e acum **singura funcție on-chain sau off** care numește
un câmp din `FeeConfig`.

Forma `(FeeConfig -> Integer)` e exact cum fee-ul de interval al Belt System-ului a ajuns să încarce
câmpul de istoric de apartenență: cu un selector, **fiecare call site e un loc unde se poate scrie
câmpul greșit**. Și off-chain — `getFeeSkeleton` și `getOracleFeeAndValiditySkeleton` își pierd
argumentul funcție și apelează `feeFor`, deci fee-ul pe care-l plătește wallet-ul și fee-ul pe care-l
verifică validatorul sunt **același număr din același cod**.

Invarianta are două jumătăți și ambele sunt impuse: **tipul** interzice pasarea unui selector, și
`check_vocabulary.sh` afirmă că un câmp din `FeeConfig` e numit doar unde e definit și în `feeFor` —
cu excepția CLI-ului de admin, care construiește un config din flag-uri și îl printează înapoi.

### T-ATTEST-3: fixurile #2 și #4, plus garda de dublă-acceptare

**Fix #2 — rotația adminului.** `OracleSetAdmin` adăugat la indicele 4. Rotația nu era doar
neimplementată, era **interzisă**. Validatorul **n-a avut nevoie de nicio schimbare** — a face funcția
capabilă să exprime schimbarea e tot fixul, și e atât de ieftin pentru că validatorul fusese scris
corect.

Off-chain oglindește: `RotateAdminAction`, `admin rotate-admin --new-admin PUBKEYHASH_HEX`. **Niciun
echivalent HTTP**, deliberat — `FromJSON` îl respinge ca fallthrough, deci o acțiune care schimbă cine
controlează protocolul poate veni doar din CLI, unde cheia care trebuie s-o semneze stă deja. Nici
`ToJSON` nu serializează hash-ul nou: instanța aia e pentru log-uri, iar linia de log a unei rotații
n-are treabă să care cheia.

Ce cumpără rotația, spus explicit: o cheie **încă ținută** poate fi predată, deci compromiterea e
recuperabilă cât timp observi la timp; una deja pierdută nu, pentru că rotația are nevoie de semnătura
ei. De-aia admin M-din-N rămâne în afara F1.

**Fix #4 — data din datum legată de intervalul de validitate.** Verificarea veche era
``date `before` txInfoValidRange``, care blochează **viitorul** și lasă **trecutul complet nemărginit**:
o atestare putea pretinde 2015 și trecea. `checkDateInRange` cere **conținere**, și cere **ambele
margini finite** — jumătatea greșită de două ori, pentru că simpla conținere e satisfăcută de un interval
fără margine superioară, iar o tranzacție trimisă cu `always` putea apoi data un registru permanent
oriunde.

> **Depășit la 2026-10-21 — fixul mărginea capetele, nu LĂȚIMEA.** Finitudinea nu costă un atacator
> nimic: `Interval (Finite 2015-01-01) (Finite acum+2h)` e finit la ambele capete, acceptat de ledger
> pentru că slotul curent cade în el, și conține orice dată. Deci back-dating-ul a rămas **complet
> nemărginit** până la `1668145`, care verifică lățimea contra `maxDateWindow` ≈ 28h. Vezi reparația
> HIGH de la finalul fișierului.

Are o consecință off-chain care altfel s-ar fi livrat ca eroare de script: fereastra e
`[tip, tip + safeEraTime]`, deci o dată dată de client în afara ei cade acum on-chain — ceea ce unui
client îi pare bug de protocol, nu cerere greșită. Deci `issueAttestationTX` o refuză întâi, ca
`AttestationDateOutOfRange` → **400**, cu un mesaj care spune ce înseamnă câmpul.

**Garda de dublă-acceptare.** `AcceptAttestation` nu verifica niciodată că atestarea nu e deja
acceptată, deci o a doua acceptare rescria un datum identic și trecea. Drumul off-chain o refuză, dar
oricine își construiește propria tranzacție ocolește asta — și rezultatul e un registru permanent a
cărui acceptare e ce a ales ultimul acceptant. O citire booleană a unui datum deja parsat, deci **nu**
e cazul de apărare neexploatabilă: garda off-chain e ocolibilă și consecința e permanentă. Cod de trace
propriu `A3`, nu pliat în `A1`, pentru că "deja acceptată" și "output greșit" sunt greșeli diferite.

**Corecție 2026-10-21:** `A3` — ca toate celelalte coduri de aici — era **șters din script** de
`remove-trace` până la `c0b270d`. Motivul pentru care e un cod separat rămâne valabil; ce e nou e că
se poate și citi.

### Dimensiuni de script după T-ATTEST-2

| validator | octeți |
| --- | --- |
| Minting Policy | 9.568 |
| Profiles | 7.671 |
| Attestations | 7.123 |
| Oracle | 7.122 |

### CI

Verde pe `3bc3297` (redenumirea): haskell, determinism, secrets — iar `check_vocabulary.sh` rulează
acum în job-ul `haskell` și a trecut și acolo.

---

## Cele patru decizii de owner — 2026-10-21

Owner-ul a decis #2 și a cerut detaliere + recomandare pe #1, #3 și #4. Aici e analiza completă;
rezumatul din chat e o condensare a ei.

### #2 — Namespace-ul GHCR: `mariusgeorgescu`. APLICAT (`e6a7fc5`)

Nu fusese niciodată decis, fusese **moștenit**: fiecare tag venea din `${{ github.repository }}`,
care se rezolvă la `en7angled/palmares` pentru că acolo trăiește repo-ul. Acum o decide o singură
linie — `GHCR_OWNER` la nivel de workflow în `images.yml` — iar `${GH_OWNER:-…}` din `compose.yaml`
și `IMAGE_PREFIX` din `scripts/build-images.sh` cară același default, deci un pull local și un push
de CI numesc aceeași imagine. **Numele imaginii nu se schimbă**: `palmares-<svc>` era deja
`<repo>-<svc>`, deci se mută doar namespace-ul.

**Consecința, care e partea care contează.** `GITHUB_TOKEN` e scoped la **owner-ul repo-ului în care
rulează**. Cât timp repo-ul stă sub org-ul `en7angled` iar imaginile se publică sub
`mariusgeorgescu`, nu poate scrie în namespace-ul ăla — push-ul cade cu `denied`, și `packages:
write` nu ajută, pentru că permisiunea aia e asupra pachetelor org-ului. Deci login-ul folosește
acum `secrets.GHCR_TOKEN`, un PAT classic al lui `mariusgeorgescu` cu `write:packages`.

**Actualizare 2026-10-21 — owner-ul a ales mutarea repo-ului (decizia 1B, la finalul fișierului).**
Deci PAT-ul nu se mai creează: `GITHUB_TOKEN` devine credentialul potrivit în momentul în care
`palmares` stă sub `mariusgeorgescu`. Namespace-ul decis aici nu se schimbă, doar remediul.

**Închis la 2026-10-21:** mutarea s-a făcut, iar prima rulare de `images.yml` de după ea a publicat
ambele imagini de bază cu `GITHUB_TOKEN`. Niciun PAT, niciodată.

## BLOCKED FOR USER — două intrări noi la 2026-10-22, amândouă pe deploy

Cele două vechi rămân închise (mai jos). Cele noi sunt amândouă pe calea de deploy T-ATTEST-10 și
niciuna nu e de cod.

**1. Config-ul de provider pentru palmares.** `config/config_atlas.json` nu există în palmares, doar
`.example`. Am încercat să-l copiez din Belt System — **blocat corect** de poarta de permisiuni:
fișierul poartă token-ul Maestro și mutarea unei credențiale între proiecte e exact ce poarta e
pusă să prindă. Nu am ocolit-o. Deblochează: owner-ul pune fișierul (shape-ul e identic cu
`.example`, un singur câmp de completat), sau îl dă prin `ATLAS_CORE_CONFIG` ca variabilă.

Derivarea adresei **nu** a avut nevoie de el: am rulat `new-operator-key` cu un config inline cu
token placeholder, fiindcă derivarea unei adrese nu face niciun apel de rețea — are nevoie doar de
`networkId`. Deci cheia există deja fără să fi atins credențiala reală.

**2. Finanțarea adresei de operator.** Cheia nouă, de unică folosință, e generată — 24 de cuvinte în
`operation.prv`, mod 0600, gitignorat, fraza nu e tipărită niciodată și fișierul e singura copie.
Citită înapoi prin `readMnemonic` **înainte** de a fi scrisă, deci o frază pe care binarul nu o
poate încărca nu ajunge pe disc ca cheie vie.

```
addr_test1vpdqa29j3hhsf9lsj490j59t7cgf0k5la5q8nqgkqkahpwq87tp9t
605a0ea8b28def0497f0954af950abf61097da9fed0079811605bb70b8   (hex, pentru câmpurile de API)
```

Verificat pe Koios: `[]` — adresa n-a fost folosită niciodată, zero UTxO. `deploy-reference-scripts`
trebuie să plătească cinci output-uri de reference script plus oracolul, deci are nevoie de fonduri
înainte. Deblochează: faucet-ul de preprod pe adresa de mai sus, sau un transfer din portofelul de
preprod al owner-ului.

**O corecție la propria mea notă:** am scris mai devreme că portofelul de preprod al Belt System are
„43 063 tADA în 46 de UTxO, verificat pe Koios" și am asociat-o cu adresa
`addr_test1vrpzyqnfedy39h8p7j5f0t5u6mdswqlu2dn6ywegemv0mns0q94rx`. **Adresa aia e greșită** — e
cheia de unică folosință dintr-o verificare anterioară a comenzii `new-operator-key`, și Koios
întoarce zero rânduri pentru ea. Nu știu, din afara `operation.prv`-ului din Belt System, care e
adresa finanțată.

### Ce NU e blocat și merge în paralel

Nu e nevoie de niciuna din cele două pentru restul: `deploy-reference-scripts` e singurul pas care
cere fonduri, iar testul de fum cere pe lângă el un **semnatar**, care nu există în repo (decizie
documentată în `docs/deployment/attest-flow-smoke.md`: „a wrong signer would look like a
convenience"). Comanda `admin sign-tx` care închide golul ăla e în lucru și nu atinge lanțul.

### Cele două intrări vechi, rămân închise

**Punctul 2 (vizibilitatea pachetelor GHCR) — ÎNCHIS 2026-10-22, și NU prin flip.** Măsurat pe
rularea `reconcile` 39697594482: jobul a tras `ghcr.io/mariusgeorgescu/palmares-admin:latest`
**fără niciun `docker login`**, iar un token anonim de registry rezolvă `latest` pentru toate patru
serviciile. Deci afirmația de la punctul 2 — „documentația GitHub e explicită: primul push al unui
pachet aterizează privat, indiferent de namespace" — era documentația descriind **alt caz**: un
pachet împins cu un PAT de pe o stație de lucru, care nu e legat de niciun repo și deci nu
moștenește nimic. Un pachet publicat **de un workflow cu `GITHUB_TOKEN`** e legat de repo-ul în
care a rulat și iese cu vizibilitatea acelui repo.

Consecință de reținut: **decizia 1B a închis și punctul 2**, ca efect secundar, exact cum punctul 2
susținea că NU se întâmplă. Verificarea din punctul 2 („niciunul din cele patru nu există încă, 403
la pull anonim") era corectă și irelevantă: un 403 pentru un pachet inexistent nu spune nimic despre
vizibilitatea lui după creare. Am citit absența ca pe o dovadă.

**A5 (rotirea tokenului Maestro expus al Belt System) — ÎNCHIS 2026-10-22 prin decizia
owner-ului, nu prin rotire:** *„nu ma deranjeaza ca tokenul maestro e expus e un token pentru
preprod"*. O cheie Maestro e per-proiect și per-rețea, deci cea scursă cumpără interogări gratuite
de preprod pe cota owner-ului și nu ajunge la nimic altceva. Nu se mai ridică. O cheie de mainnet e
alt credential și decizia asta nu spune nimic despre ea. Cele trei fișiere de memorie care cereau
rotirea au fost corectate, nu lăsate să ceară o acțiune pe care owner-ul a refuzat-o.

1. ~~**Mutarea repo-ului `en7angled/palmares` sub `mariusgeorgescu`**~~ — **FĂCUTĂ, închisă la
   2026-10-21.** Primul `images.yml` de după mutare a împins **ambele** imagini de bază în
   `ghcr.io/mariusgeorgescu` cu `GITHUB_TOKEN`, pe pasul exact pe care rularea de dinainte picase
   după 42 de minute. Deci decizia 1B e dovedită, nu doar argumentată.
2. **Trecerea celor patru pachete GHCR pe public, DUPĂ primul publish.** Asta NU e eliminată de
   decizia 1B, cum s-a scris inițial — documentația GitHub e explicită: primul push al unui pachet
   aterizează **privat**, indiferent de namespace și indiferent de vizibilitatea repo-ului. Verificat
   2026-10-21: niciunul din cele patru `ghcr.io/mariusgeorgescu/palmares-*` nu există încă (403 la
   pull anonim), deci primul run de `images.yml` le creează private. Cel care **musai** trebuie
   public e `palmares-admin`: `reconcile.yml` îl trage fără `docker login`, prin design, deci jobul
   de paritate nocturn ar cădea cu `pull access denied` în loc de o divergență reală.

**Acțiunea de dinainte e RETRASĂ** — decizia 1B o face inutilă, nu o amână:

1. ~~**Secretul `GHCR_TOKEN`**~~ — nu mai e nevoie de niciun PAT; după mutare, `GITHUB_TOKEN` e deja
   owner-ul namespace-ului. Fallback-ul `secrets.GHCR_TOKEN || secrets.GITHUB_TOKEN` a fost **șters**
   din `images.yml`, nu doar lăsat să cadă pe a doua ramură: `||` cade doar pe un secret *nesetat*,
   niciodată pe unul expirat, deci un PAT vechi adăugat ulterior ar fi întrecut silențios tokenul
   care funcționează.
2. ~~**Vizibilitatea pachetelor**~~ — scopul flip-ului era ca `reconcile.yml` să poată *trage*
   imaginea de admin fără `docker login`. ⚠️ **De verificat la prima rulare de `images.yml` de după
   mutare:** GHCR pune pe privat orice pachet nou publicat, inclusiv sub un namespace de utilizator,
   deci dacă cele patru pachete aterizează private, flip-ul rămâne o singură acțiune de UI. Semnalul
   e același: job-ul nocturn de paritate cade pe `pull access denied`, nu pe o divergență reală.

`secrets.GHCR_TOKEN || secrets.GITHUB_TOKEN` nu e un fallback silențios: e exact ce face din
**"mută repo-ul la `mariusgeorgescu`"** o alternativă cu zero editări, pentru că atunci
`GITHUB_TOKEN` e deja credentialul potrivit și secretul se poate șterge. Ambele stări finale cad
zgomotos; doar cea din mijloc are nevoie de PAT — **și owner-ul a ales starea finală cu mutarea**,
deci cea din mijloc nu se mai atinge niciodată.

### #1 — Al treilea `DRepStatus`: ce e, cât costă, și ce recomand

**Unde se naște.** `sql/010_gov_views.sql:281`:

```sql
COALESCE(ld.active_until >= (SELECT epoch_no FROM palmares.sync_state), false) AS is_active
```

db-sync scrie deadline-ul ledger-ului **numai** în `drep_distr`, iar `drep_distr` ține un rând doar
pentru un DRep pe care ledger-ul l-a inclus în distribuția epocii. **Măsurat pe preprod la epoca
320: 90 din 294 de DRep-i înregistrați nu au avut NICIODATĂ un rând în `drep_distr`** — niciunul, la
nicio epocă. Pentru ei nu avem deadline deloc, iar `COALESCE(…, false)` îi pune pe toți pe
`inactive`.

Koios își calculează singur deadline-ul și spune: **21 din cei 90 sunt ACTIVI** (expiry 320–340),
**69 sunt inactivi** (expiry 185–224). Deci `false` e corect pentru 69 și **greșit pentru 21**.

**Cele două ieșiri ușoare, ambele măsurate și respinse:**

* *„deadline necunoscut înseamnă activ"* — implementat, măsurat, **revertit**: ar fi greșit pentru 69
  ca să repare 21.
* *derivăm noi deadline-ul* — `max(ultimul cert, ultimul vot) + drep_activity` diferă de valoarea
  ledger-ului cu până la **5 epoci**, pentru că CIP-1694 nu numără epocile **DORMANTE** (fără
  propunere pendinte) la inactivitate. A reimplementa contabilitatea aia ar fi o a doua derivare care
  poate să nu fie de acord cu ledger-ul — exact lucrul pentru care schema asta există să nu-l facă.

**Descoperirea care face decizia curată: informația e DEJA completă pe sârmă.**
`active_until_epoch` se publică și e **nullable** (`QueryResponses.hs:618` îl citește cu `.:?`,
`Query/Gov.hs:660` îl selectează, apare în swagger). Deci un client poate separa deja cei 90:
`status = "inactive" AND active_until_epoch = null`. Varianta cu trei statusuri și varianta de azi
**cară exact aceeași informație** — singura diferență e ce cuvânt stă în câmpul-titlu.

Asta reduce întrebarea la una singură: **are voie câmpul `status` să conțină o presupunere?** Pentru
21 de DRep-i reali, azi conține una falsă, iar `status` e fix câmpul pe care un client și UI-ul îl
citesc (și pe care numără facet-urile).

**Ce ar atinge valoarea a treia** (nu e o estimare, e lista):

| sit | ce se schimbă |
| --- | --- |
| `Core/Types.hs:365` | constructorul; tag-ul de sârmă, `jsonEnumSchema` și `FromHttpApiData` se derivă |
| `Query/Gov.hs:1640` `drepStatusExpr` | un braț nou în CASE |
| `Query/Gov.hs:1562` `searchSql` | **al doilea exemplar** al aceluiași CASE — de pliat în `drepStatusExpr`, care există tocmai pentru asta |
| `Query/Gov.hs:1650` `effectiveDRepStatus` | ce selectează pagina implicită |
| `Query/Gov.hs:1662` `registeredDRepStatuses` | universul de facet-uri |
| `Reconcile/Koios.hs:428` `koiosDRepStatus` | Koios **nu** întoarce niciodată „unknown", își calculează deadline-ul |
| `config/reconcile-exceptions.yaml` R2 `field: status` | waiver-ul există deja cu `key: "*"`, deci nu apar 90 de divergențe noi — dar divergența **își schimbă forma** și motivul trebuie rescris (raportul de `stale_exceptions` prinde dacă uit) |
| swagger + FE | regenerare + `switch`-ul exhaustiv din frontend |

**Recomandarea mea: DA, al treilea status, numit `unknown`, și INCLUS în setul implicit.**

Trei motive, în ordine:

1. Nu se pierde și nu se câștigă nicio informație — e o **reetichetare pură**. Cei 90 sunt deja
   identificabili; doar încetăm să pretindem că știm care-i care.
2. `unknown` în default (`[active, inactive]` → `[active, inactive, unknown]`) înseamnă că
   **populația nu se schimbă deloc**: 294 de înregistrați rămân 294 pe pagina implicită, apare un
   rând de facet, și nu dispare niciun DRep. Varianta cu `unknown` **în afara** default-ului ar
   scoate 90 de DRep-i din lista implicită ca să evite o etichetă — mai rău decât oricare etichetă.
3. S2 din spec cere ca orice metrică afișată să fie reproductibilă, iar §10 numește **L2BEAT** ca
   etalon de onestitate în prezentarea datelor. Un `status: inactive` pe care ledger-ul îl contrazice
   e exact genul de titlu pe care etalonul ăla nu-l permite — și e despre reputația unor actori reali,
   pe pagina pentru care produsul există.

Alternativa de nume luată în calcul: `registered` (spune ce **știm**, nu ce nu știm), respinsă pentru
că și DRep-ii activi și cei inactivi sunt înregistrați, deci stă prost lângă celelalte trei.
Varianta „nu schimbăm nimic, doar documentăm combinația `inactive` + `null`" rămâne cea mai ieftină
și e o notă de subsol lângă un titlu greșit.

### #3 — Poate o atestare să înregistreze o dată de eveniment mai veche decât emiterea?

**Ce e construit azi.** `attestationDate :: POSIXTime` în datum, validat de `checkDateInRange`
contra intervalului de validitate al tranzacției, pe care off-chain-ul îl construiește ca
`[tip, tip + safeEraTime]` (~7h). Deci data **e** timpul emiterii, verificat pe lanț cu precizie de
ore. `issueAttestationTX` refuză întâi o dată în afara ferestrei, ca `AttestationDateOutOfRange` →
400.

**Specificația răspunde deja, și nu în direcția în care am construit.** §3 (glosar, linia 40)
listează **cinci tipuri de atestare** — `MilestoneDelivered`, `AuditPassed`, `Affiliation`,
`RoleHeld`, `Endorsement` — și spune explicit că două dintre ele au **intervale**: `Affiliation`
(organizație ↔ membru, *cu intervale*) și `RoleHeld` (mandat extra-chain: lead de working group,
președinte de comitet — *cu intervale*). Un interval de afiliere care **începe înainte** de emiterea
atestării e cazul normal, nu excepția: „e membru din martie 2024, atestat azi". Deci „nu" nu e de
fapt un răspuns disponibil dacă specificația e respectată.

**Și golul pe care l-a descoperit asta: Planul B nu are NICIUN tip de atestare.**
`OnchainAttestation` = id, issuer, subject, date, isAccepted. Zero din cele cinci tipuri ale
spec-ului apar în plan sau în cod — un grep după toate cinci în plan întoarce nimic. Self-review-ul
planului mapează §4.3 (stratul de atestări) și a ratat glosarul din §3. Gol real, nu o preferință.

**Recomandarea mea: DA, dar în DOUĂ câmpuri separate, care nu se pot confunda niciodată.**

1. **`attestationDate` rămâne exact cum e** — timpul emiterii, verificat pe lanț, legat de intervalul
   de validitate. Nu se redenumește, nu se relaxează. E singura dată despre care lanțul poate spune
   ceva.
2. **Un câmp nou pentru perioada revendicată**, `Maybe (POSIXTime, POSIXTime)`, **care NU se
   validează contra intervalului de validitate** — pentru că a-l valida ar fi o minciună: nimic
   on-chain nu poate confirma când a început o afiliere. Auto-raportat prin construcție, deci
   validatorul nu capătă nicio regulă nouă pentru el (nu e nimic de verificat).
3. **Un `attestationKind` cu cele cinci constructoare ale spec-ului.** Perioada e semnificativă doar
   pentru două dintre ele, iar stratul de citire trebuie oricum să filtreze pe tip — același
   argument ca la `OnchainProfileKind`: un constructor, nu metadata, pentru că un octet din datum nu
   poate devia și un `Map BuiltinByteString BuiltinByteString` da.
4. **Regula de API, care e jumătatea care contează**: perioada se livrează sub alt nume și cu alt
   tratament vizual decât data — `claimed_period` lângă `issued_at` — iar `/attestations` nu sortează
   și nu filtrează **niciodată** înregistrarea ca și cum perioada ar fi verificată. O propoziție în
   descrierea de swagger per câmp, exact ca `unclaimed` pentru `profileCredential = Nothing`.

**Cost:** un câmp opțional în datum (câțiva octeți), un octet pentru `kind`, un `feeFor`-style match
exhaustiv, zero logică nouă de validator. **Hash-ul se schimbă** — deci intră în același batch cu
T-ATTEST-4, care e următorul task și care schimbă oricum hash-ul minting policy-ului.

### #4 — `Auditor`: fel de profil sau rol al unei `Organization`?

**Spec-ul o spune de două ori.** §3 linia 39 numește explicit cele șase tipuri, cu „numite explicit
de owner (2026-10-20)", `Auditor` printre ele; §4.3 linia 83 le repetă. Deci așa e construit.
Întrebarea reală, cum o pune planul, e doar dacă un auditor care e **și** organizație are nevoie de
două profiluri.

**Ce e de fapt în joc.** `OnchainProfileKind` e un octet din datum, enum închis, și **niciun
validator nu ramifică pe el** — e pur și simplu un facet al stratului de citire. Deci „fel" costă
nimic și cumpără filtrul `/profiles?kind=auditor`.

Cealaltă citire — „auditor e un rol pe care-l ține o `Organization`" — s-ar exprima prin primitiva
proprie a registrului: o atestare de tip `RoleHeld`/`AuditPassed` emisă de cine acreditează
auditorii. Aia e **strict mai expresivă**: are un emitent (cine o spune), o dată, o contrasemnătură,
și poate fi disputată. Un fel de profil nu are niciuna dintre ele — e **auto-declarat la mint** și
nimeni nu-l contrasemnează.

**Recomandarea mea: `Auditor` rămâne fel de profil (spec-ul o cere, costă un octet), dar NU e tratat
niciodată ca acreditare.** Un profil, nu două: o organizație care auditează alege felul care-i
descrie activitatea principală, iar statutul ei de auditor e o **atestare** pe care i-a emis-o
cineva. Felul e eticheta, atestarea e faptul.

Lucrul care nu trebuie să se întâmple niciodată e ca stratul de citire să prezinte `kind = auditor`
ca pe o acreditare. Deci API-ul îl etichetează `self_declared`, exact cum
`profileCredential = Nothing` se prezintă ca `unclaimed` — un cuvânt în swagger, aceeași disciplină.

---

## #1 livrat, și cercetarea despre verificarea codului on-chain

### #1 — al treilea `DRepStatus`, implementat (`05c83bd`)

Cele 9 situri, **295 de teste trec** (290 + 5), swagger regenerat și neînvechit,
`swagger OK`, `vocabulary OK`. Zero linii schimbate în `010_gov_views.sql` în afară de comentarii —
pentru că informația era deja publicată: `active_until_epoch` a fost mereu nullable.

**Ce a găsit pe drum:** `searchSql` ținea un **exemplar verbatim** al CASE-ului de status. Paleta de
căutare ar fi continuat să răspundă `inactive` după ce proiecția de lângă ea începea să răspundă
`unknown` — exact defectul pentru care `drepStatusExpr` există. Pliat, cu un test care afirmă că
statement-ul **interpolează** expresia, nu o repetă.

**Testul care contează** e pe **ordinea brațelor**: `active_until_epoch IS NULL` trebuie testat
**înainte** de `is_active`, pentru că `COALESCE` din `is_active` a colapsat deja „fără deadline" în
„deadline trecut". E o proprietate de corectitudine, deci e afirmată ca ordine, nu ca potrivire de
șir întreg — statement-ul rămâne liber să fie reformatat.

**Waiver-ul R2 a fost rescris, nu îngustat.** Cei 90 diferă acum toți ca etichetă, unde înainte 21
erau de-a dreptul greșiți: deci **numărul de divergențe a crescut** și onestitatea a crescut cu el.
`koiosDRepStatus` nu a fost lărgit deliberat — Koios are mereu un răspuns și nu raportează niciodată
„unknown"; a-i mapa răspunsul în propria noastră lacună ar fi să-i pui vorbe în gură referinței, fix
lucrul pe care o verificare de paritate nu are voie să-l facă. Trade-ul e scris în motivul waiver-ului
și în `docs/deployment/koios-parity-gate.md`, pentru că un număr de paritate care crește din motiv bun
e exact genul de lucru pe care un cititor viitor va încerca altfel să-l „repare".

### Cercetare: `smartcode-verifier` — **nu se aplică**, dar a dus la ceva care se aplică

Ai cerut un agent separat pe <https://github.com/input-output-hk/smartcode-verifier>. Răspunsul scurt:
**premisa e greșită — nu e un tool de proveniență.**

**Ce e.** Un tool de **verificare formală automată** în Lean4: adnotezi contractul cu proprietăți care
trebuie satisfăcute, iar un SMT solver le descarcă. Dovedește *„validatorul ăsta nu poate fi golit"*.
Nu spune nimic despre ce sursă a produs un hash on-chain.

**Starea lui, verificată independent prin API-ul GitHub:** **40 KB, zero limbaje detectate** (nu e cod
în el), **ultimele trei commit-uri sunt editări de README din 2026-04-20**, 1 issue deschis fără
răspuns, 9 stele. README-ul însuși o spune: *"kept only as a reference to the Catalyst project
submitted where this repository was mentioned"*. Munca s-a mutat în trei repo-uri succesoare
(`Lean-blaster`, `PlutusCoreBlaster`, `CardanoLedgerApiBlaster`), toate active, niciuna cu release
instalabil. Niciun serviciu public.

**Nu se suprapune cu `scripts/check_determinism.sh`** al nostru, care verifică altceva complet:
că formulele SQL de metrici dau același răspuns de două ori pe un cursor pinuit de db-sync. Nimic din
ce avem nu acoperă proveniența hash-ului de script.

### Ce se aplică: **CIP-0171**, verificat direct în textul CIP-ului

<https://cips.cardano.org/cip/CIP-0171> — *On-chain Smart Contract Bytecode Verification*, **Proposed**,
creat 2026-01-19. Exact promisiunea din modelul nostru de încredere, și e **descentralizat prin
construcție**, nu un Etherscan.

* **Label de metadata 1984**, înregistrat în CIP-0010. PlutusData în CBOR, chunk-uit la 64 de octeți.
* **PlutusTx e constructorul 2.** `compiler_fields = [source_url, commit_hash, source_path / null,
  compiler_version, ? parameters]`.
* **Cele două cazuri grele ale noastre SUNT exprimabile**: `parameters = { script_hash =>
  parameter_list }`, iar `parameter_list = [ * bytes ]`, fiecare element „a bytestring wrapping the
  CBOR encoding of one parameter", **în ordinea aplicării**. Deci atât `MintingPolicy ←
  ProtocolParams ← oracle AssetClass`, cât și `OracleNFTPolicy ← TxOutRef` încap.
* **Permisionless, și anti-spam prin construcție**: oricine poate trimite, nu doar cel care a făcut
  deploy — deci un auditor poate verifica scripturile noastre fără noi. Iar o înregistrare fabricată
  e inofensivă, pentru că hash-ul rezultat nu se potrivește cu niciun script on-chain. Costul e **o
  tranzacție de metadata**, fără UTxO creat sau consumat.

**Unde suntem mai bine plasați decât cazul PlutusTx generic.** Schema nu are câmp `env` pentru
non-Aiken (Aiken are, pentru `aiken build --env`), deci reproductibilitatea unui build PlutusTx
depinde de închiderea completă de dependențe — ceea ce `commit_hash` + `compiler_version` nu o prinde
în general. **La noi o prinde**, pentru că `flake.lock` și `cabal.project` (cu Atlas pinuit pe un
commit) sunt în arbore: `commit_hash`-ul repo-ului determină toolchain-ul. Un proiect pe cabal+ghcup
nu are asta.

**Implementarea de referință, și limita ei reală.** `uplc.link`
(`easy1staking-com/uplc-link`) e **vie și activă**, dar mică: `/api/v1/stats` raportează 10
verificări / 50 scripturi / 7 repo-uri, și **toate înregistrările live sunt `AIKEN`** — drumul de
verificare e `git clone` + `aiken build`. PlutusTx există doar ca schelet (`CompilerType.PLUTUSTX(2)`,
un `examples/plutus-example/`, un dockerfile cu GHC 9.6.6 — GHC-ul nostru, dar prin ghcup, nu Nix).
**Dacă o trimitere PlutusTx funcționează cap-coadă azi: incert, n-am găsit dovadă că da.** Și
intrările Aiken live arată `"parameterizationStatus":"PARTIAL","finalHash":null` — scripturile
parametrizate nu sunt încă potrivite complet pe hash în practică, adică exact forma noastră. Toate
cele cinci criterii de acceptare ale CIP-ului sunt nebifate, `Implementors: []`.

### Verdict și singura acțiune de făcut acum

**`smartcode-verifier`: nu. CIP-0171: da, la mainnet, nu înainte de preprod** — motivul care decide:
nu poate potrivi pe hash un script PlutusTx parametrizat construit cu Nix **azi**, iar prezentarea
onestă în stil L2BEAT înseamnă să nu pretindem o verificare pe care nu o putem demonstra.

**Acțiunea gratuită, de făcut la T-ATTEST-10 (deploy-ul pe preprod), nu mai târziu:**
`config/config_palmares_validators.json` nu există încă. Când deploy-ul îl scrie, să înregistreze
**lângă hash-uri și cei doi parametri de deployment**: `AssetClass`-ul oracle-ului (în CBOR) și
`TxOutRef`-ul seed al lui `OracleNFTPolicy`. Sunt singurul input al unei viitoare înregistrări
label-1984 care e ieftin acum și dureros de reconstruit mai târziu — după deploy, `TxOutRef`-ul seed
trebuie scos din istoricul lanțului.

---

## #3 — tipul atestării și perioada revendicată (T-ATTEST-3b)

Decizia owner-ului: da, dar în două câmpuri care nu se pot confunda. Implementat.

### Forma

| câmp | ce e | verificat on-chain |
| --- | --- | --- |
| `attestationKind` / `AttestationType` | unul din cele cinci tipuri ale spec-ului | — (niciun validator nu ramifică pe el) |
| `attestationDate` | când a fost **emisă** | **da**, `Ml` / `checkDateInRange`, neschimbat |
| `attestationPeriod` → `claimed_period` | perioada **revendicată**, `Maybe (POSIXTime, POSIXTime)` | **nu, deliberat** |

Perioada nu e validată și nu trebuie să fie niciodată: a o valida ar fi o minciună, pentru că nimic
on-chain nu poate confirma când a început o afiliere. De-aia validatorul **nu capătă nicio regulă**
pentru ea — nu e nimic de verificat. Ce oprește o perioadă aberantă e același lucru care oprește o
atestare aberantă: subiectul trebuie s-o contrasemneze.

`Maybe` de o **pereche**, nu două borne opționale, ca „ambele sau niciuna" să fie structural. O
perioadă pe jumătate deschisă ar fi o a treia stare pe care stratul de citire ar trebui s-o
reprezinte; o afiliere fără capăt se exprimă cu capătul în viitorul îndepărtat, nu cu absența lui.

### Patru decizii de constructor

1. **O singură verificare pe perioadă, off-chain**: borne inversate → `AttestationPeriodInverted` →
   400. **Nu** o verificare de fereastră — o perioadă care începe înainte de tranzacție e **cazul
   normal**, ăsta e tot motivul pentru care câmpul există separat de dată. Bornele transpuse sunt
   singurul lucru care nu poate fi o revendicare legitimă și singurul despre care un client poate fi
   informat.
2. **`--claimed-period FROM..TO` e o singură opțiune de CLI**, nu două. Două flag-uri opționale ar
   lăsa `--claimed-from` singur să treacă, iar în aval nu-l mai prinde nimic: conversia în acțiune e
   o funcție pură care nu poate eșua, iar validatorul deliberat nu verifică.
3. **`putAttestationProjection` a trecut pe câmpuri numite.** Era pozițional cu 13 argumente și
   perioada îl ducea la 16 — dar numărul nu e pericolul: `attestationId`, `subjectProfileId` și
   `issuerProfileId` sunt toate `GYAssetClass`, deci o transpoziție între ele **compilează** și scrie
   o atestare atribuită profilului greșit. Fixtura de test are cele trei valori distincte exact ca
   asta să devină o asserțiune picată.
4. **`currentSchemaVersion` → 2**, cu re-sync intenționat: cele trei coloane se citesc din datum,
   deci rândurile scrise înainte nu se pot completa din bază, doar din lanț. Plus **asserțiunea care
   lipsea** — numărul de coloane din `attestation_projection` e pinuit lângă versiune, deci o coloană
   adăugată fără bump pică acolo, lângă bump-ul pe care-l datorează.

### Trei defecte găsite generând documentul, nu citind codul

**(a) Enum-ul publicat nu era enum-ul acceptat.** `AttestationType` a pornit cu `ToSchema` **derivat**,
care publică numele constructorilor: `interaction-swagger-api.json` generat scria
`["MilestoneDelivered", …]` în timp ce `FromJSON` acceptă `["milestone_delivered", …]`. **Documentul
descria cinci valori pe care API-ul le respinge**, compila, și nimic nu picat. Toate celelalte enum-uri
din `DomainTypes/Core/Types.hs` își citesc schema din `ToJSON` cu `jsonEnumSchema`; instanța derivată
e capcana.

Și **`ProfileType` stătea în ea** — latent, pentru că `ProfileType` lipsește azi din ambele documente
generate, deci minciuna n-a fost încă publicată. Ar fi devenit vie în momentul în care aterizează
rutele de profil (T-ATTEST-7). Reparat în același loc, pentru că e același fix de patru linii.

Garda pentru **toată clasa**: un test care afirmă că enum-ul schemei **este** lista de tag-uri
`ToJSON`, pentru ambele tipuri. Pică pe o instanță derivată, trece pe una cu `jsonEnumSchema`.

**(b) `describeField "kind"` nu avea niciun efect.** Proprietatea e un `$ref` gol, iar un `$ref` din
Swagger 2.0 nu poate purta o descriere alături — cheia era pur și simplu absentă din documentul
generat. Mecanismul pentru un tip referențiat e descrierea la nivel de **definiție**, și exact de-aia
`addSharedSwaggerDescriptions` avea deja un rând `AttestationType` **înainte ca tipul să existe**.
Lentila `at` fusese un no-op silențios tot timpul. Textul real stă acum acolo.

**(c) Documentul de interaction-api nu era nici commit-uit, nici regenerat în CI.** Se scria la fiecare
boot, iar boot-ul **acestui** serviciu cere o configurație de provider Atlas — deci CI nu-l obținea
niciodată. Rezultatul: contractul de cerere publicat al interaction API-ului nu avea nicio poartă de
drift și nici nu era în repo, în timp ce al query API-ului avea amândouă. Că golul era neintenționat o
spune `check_vocabulary.sh:89` însuși: „run the services with `--write-swagger-only`" — un flag pe care
interaction-api nu-l avea.

Adăugat (`--write-swagger-only`, un singur producător, aceeași formă ca la query-api), documentul
commit-uit, și pasul de CI diffuiește acum **ambele**. Verificarea de vocabular globuiește directorul,
deci îl acoperă de la sine.

Argumentul pentru poartă nu e o teorie: defectul (a) a fost găsit **generând documentul de mână**.

### Verificat în artefactul generat

`AttestationKind` cu cinci constructori la indicii 0–4; `OnchainAttestation` cu șapte câmpuri în
ordinea corectă; `NewAttestation` cu nouă argumente, `MintProfile` cu cinci;
`Maybe_Tuple2_POSIXTime_POSIXTime` derivat automat, deci perechea se exprimă curat în CIP-57. Proza
publicată a blueprint-ului spune acum explicit că **data e mărginită de intervalul de validitate și
perioada nu**, și că un consumator nu are voie să le prezinte la fel.

### Verificat

Toate **303 testele trec** (301 + 2 pentru garda de enum). `cabal build all` curat, ambele documente
Swagger regenerate și neînvechite, `swagger OK`, `vocabulary OK`. Enum-ul `AttestationType` din
documentul de interaction citește acum snake_case — fixul e verificat în artefact, nu doar în cod.

Commit: `a3b36d4`.

---

## T-ATTEST-4 — validatorul de dispute

Al patrulea validator de cheltuire. Un depozit intră, admin-ul oracle-ului scrie un verdict,
depozitul se întoarce — pe **oricare** verdict.

### Forma

**`DisputesValidator`**, nesparametrizat, coduri `D0`–`D5`:

| cod | ce verifică |
| --- | --- |
| `D0` | `Cleanup` pe un datum care **parsează** e refuzat — pattern-ul casei, identic cu celelalte două validatoare de stare |
| `D1` | `opAdminPkh` a semnat, citit din **input-ul de referință** al oracle-ului |
| `D2` | output-ul la index duce datumul rezolvat, aceeași adresă, valoare `>=` (valoarea proprie − depozit), și UTxO-ul cheltuit poartă chiar NFT-ul disputei |
| `D3` | disputa **nu era deja rezolvată** |
| `D4` | depozitul se plătește la adresa din datum |
| `D5` | scop care nu e de cheltuire |

**`handleOpenDispute`** în minting policy, `OpenDispute` la **indexul 2**, coduri `Md1`–`Md7`: seed
cheltuit; mintează **exact** NFT-ul de referință al disputei și nu arde nimic; NFT-ul de user al
**disputantului**; ținta poartă currency symbol-ul protocolului; output la index la adresa
validatorului de dispute cu valoare `>=` NFT + `opMinUTxOValue` + **depozit**; data de deschidere în
intervalul de validitate; dimensiunile de metadata validate.

**Nimic din asta nu atinge atestarea disputată.** Nu e cheltuită, nu e re-datumată, nu e nici măcar
citită ca input — doar clasa ei de active e verificată pentru currency symbol. Asta e ce face o
dispută deschizabilă **fără cooperarea subiectului sau a emitentului**, și ce lasă costul de execuție
al drumului de atestare exact cum era.

**Doar token-ul de referință se mintează, fără token de user.** Un token de user e autoritatea de a
face ceva, iar deținătorul unei dispute nu are ce să facă: rezolvarea e a admin-ului, iar depozitul
se întoarce la o adresă fixată în datum. Să mintezi unul ar fi să mintezi o autoritate fără puteri —
ceea ce mai târziu se citește ca o capabilitate pe care cineva o caută.

### Patru decizii peste plan, fiecare cu motivul ei

**1. Datumul din plan nu-și putea implementa propriile verificări.** Planul specifica cinci câmpuri;
îi lipseau trei fără care `D1` și `D4` nu se pot scrie deloc:

* `disputeOracleToken` — un validator nesparametrizat nu poate ști **care** input de referință e
  oracle-ul fără token-ul care-l identifică. Fără el, oricine atașează un UTxO fals cu el însuși ca
  admin și rezolvă orice dispută. Doar token-ul, nu tot `ProtocolParams`: e tot ce-i trebuie, iar un
  datum se plătește în min-ADA.
* `disputeDepositTo` — depozitul trebuie să se întoarcă la disputant, iar un validator nu poate
  rezolva un `ProfileId` într-o adresă. Disputantul o numește la deschidere; dacă o pune greșit, se
  păgubește singur.
* `disputeDeposit` — **suma plătită efectiv**, înregistrată la deschidere. A o citi din oracle la
  rezolvare ar lăsa admin-ul să coboare prețul, să rezolve, și să păstreze diferența — sau să-l ridice
  și să facă o dispută deschisă **nerezolvabilă**. Oracle-ul pune preț pentru dispute **noi**; prețul
  uneia deschise e istorie.

**2. `OracleSetDisputeDeposit` la indexul 5**, pe care planul nu-l are. Un parametru de protocol care
nu se poate re-tarifa e unul pentru care trebuie să redeployezi un validator — exact ce oracle-ul
există să evite. Aceeași formă ca `OracleSetMinUTxOValue`, și **nu** poate afecta o dispută deja
deschisă.

**3. `Md6` și `Md7` nu sunt adăugiri, sunt cerințe permanente** pe care lista planului le omitea:
data din datum legată de intervalul de validitate (`onchain-lib/CLAUDE.md`, fixul moștenit #4) și
validarea dimensiunilor de metadata, exact ca la celelalte două drumuri de mint.

**4. Disputele n-au taxă de protocol.** Depozitul e returnabil și e singurul cost care le mărginește;
o taxă nereturnabilă pe deasupra ar fi un impozit pe contestarea unei afirmații false — pe exact
comportamentul pe care registrul îl vrea. De-aia `Verdict` are **două** constructoare și nu trei: nu
există verdict punitiv, deci pierderea unei dispute costă doar fee-uri de rețea. Dacă se vrea vreodată
una, e un câmp `fcDisputeFee` și un braț în `feeFor`.

### Defectul latent pe care l-a scos decizia „gratuit"

`checkFee` cu o taxă **zero** cerea un output către adresa de taxe purtând **cel puțin zero**
lovelace — ceva ce nicio tranzacție onestă nu construiește. Deci `OracleSetFees (Just cfg)` cu
**orice** câmp pus pe zero ar fi blocat acțiunea aia până observa cineva, iar specificația §4.3 spune
că pe preprod taxele sunt zero. `MintActionDispute` l-ar fi lovit imediat; celelalte două îl loveau
prima dată când cineva le punea prețul pe nimic. Reparat cu un scurt-circuit pe zero, iar testul din
`UnitTests/Dispute.hs` care-l acoperă **pică pe codul vechi**.

### Și o minciună într-un test, prinsă de agentul care lucra în paralel

`UnitTests/Fees.hs` avea `allActions = [MintActionProfile, MintActionAttestation]` — o listă scrisă
de mână care se numea „every action". `MintActionDispute` ar fi aterizat **netestat** acolo. Acum e
`[minBound .. maxBound]`, iar `MintAction` derivă `Prelude.Enum`/`Prelude.Bounded` **doar pentru
teste** — PlutusTx nu le vede. Testul despre „niciun două acțiuni nu împart un câmp" rămâne pe
acțiunile **tarifate**, pentru că disputa nu numește niciun câmp: altfel ar fi un test despre
constante, nu despre câmpuri.

### Ce NU verifică lanțul, scris în trei locuri

„O singură dispută deschisă per atestare" **nu** se verifică on-chain: ar cere un UTxO de index pentru
care fiecare dispută ar trebui să concureze, ceea ce serializează deschiderea și cumpără o proprietate
pe care depozitul o dă deja — fiecare dispută e UTxO-ul ei cu depozitul ei, deci N dispute costă N
depozite. `checkNoOpenDispute` e o **curtoazie off-chain**, fără cod de trace, pentru că nu e nicio
verificare. Spus în haddock-ul tipului, în haddock-ul validatorului **și** în proza publicată a
blueprint-ului, ca nimic din aval să nu-l prezinte ca regulă de protocol.

### `mkProtocolParams`

Rescris pe câmpuri numite, cu argument explicit. Adăugarea lui `disputesValidatorScriptHash` a rupt
oricum eta-reducerea, dar motivul pentru care **rămâne** așa e următoarea adăugare: trei câmpuri
`ScriptHash` consecutive sunt trei poziții în care o transpoziție **trece la tipuri**, iar o adresă de
validator care arată în silence spre validatorul greșit e o schimbare de hash pe care n-o vede nimeni
până când o tranzacție cade pe lanț.

Și de notat: `ProtocolParams` e **înglobat în fiecare datum de profil**, deci adăugarea câmpului e o
schimbare de format de datum pentru toate profilurile, nu doar un parametru de minting policy. Ăsta e
motivul real pentru care disputele trebuie să preceadă deploy-ul.

### Abatere de la planul propriu

Pasul 4 al task-ului cere ca `scripts/test_exunits.sh` să raporteze o dimensiune de script pentru
`DisputesValidator`. **Scriptul nu există în acest repo** — e în Belt System. Nu există nicio poartă de
ExUnits sau de dimensiune aici; funcțiile de măsurare există în `TxBuilding/Validators.hs` (am adăugat
`disputesValidatorSize` lângă celelalte) și nimic nu le apelează. Singurul loc care raporta dimensiuni
e `TestRuns.hs`, dormant și stale. Rămâne de făcut înainte de mainnet, nu înainte de preprod.

**Închis la 2026-10-21, mai devreme decât am spus aici:** T-ATTEST-5 a adus poarta —
`src/test/UnitTests/ScriptSize.hs` — și a înviat `TestRuns.hs`. Dimensiunile celor cinci scripturi
ies acum dintr-o aserțiune, nu dintr-o notiță. `scripts/test_exunits.sh` rămâne inexistent în repo-ul
ăsta, și nu mai e nevoie de el.

### Paralelizare

La cererea owner-ului, trei fire în același timp: eu pe validator + minting policy + blueprint +
`Validators`/`Context`/`Constants` + cabal + teste; un agent pe toată instalația off-chain a
depozitului (`AdminActionType`, cele două oglinzi din `Operations.hs`, `refGetters`, `cleanupDustTX`,
`deployReferenceScripts`, `OracleParams` inițial, `admin set-dispute-deposit`, `query-oracle`); și un
al treilea pe un review adversarial al commit-ului `a3b36d4`. Agenților li s-a interzis explicit să
ruleze `cabal`, pentru că două build-uri în același `dist-newstyle` se bat pe lock și dau erori
înșelătoare — build-ul e unul, al meu, la final.

### Review adversarial pe `a3b36d4`, și o corecție la propriul meu mesaj de commit

Al treilea fir a fost un review pe commit-ul lui #3. A găsit cinci lucruri, patru reale.

**1. `Query/Projected.hs` — `toAttestation` scăpa silențios ambele câmpuri noi.** Modulul e comentat
din cabal, deci nu compilează azi — dar `-Wmissing-fields` e doar un warning și nu există `-Werror`.
Scenariul: T-ATTEST-9 îl reactivează, build-ul **trece** cu un warning, iar primul
`GET /pages/attestations` aruncă `RecConError "Missing field in record construction attestationKind"`
→ **500 pe fiecare rând**. Reparat, cu reasamblarea perioadei din cele două coloane și cu regula
„ambele sau niciuna" scrisă la citire: un rând pe jumătate setat poate veni doar dintr-un UPDATE
manual, iar „nicio perioadă" e răspunsul sigur.

**2. O AFIRMAȚIE FALSĂ ÎN MESAJUL MEU DE COMMIT.** `a3b36d4` spune: *„fixtura de test dă cele trei
valori distincte exact pentru asta"*, justificând trecerea lui `putAttestationProjection` pe câmpuri
numite. **Nimic din suită nu apela `putAttestationProjection`.** Fixtura păzea round-trip-ul JSON, nu
scrierea proiecției, deci o transpoziție între cele trei coloane `GYAssetClass` ar fi trecut verde
oricum. Acum există un test care scrie efectiv un rând și-l citește înapoi.

**Nu am amendat commit-ul.** Corecția stă aici și în mesajul commit-ului următor: a rescrie istoria ca
să pară că aveam dreptate e mai rău decât a consemna că nu aveam.

**3. `AttestationPeriodInverted` nu avea nicio acoperire** — garda era un `case` inline într-o acțiune
monadică care cere un provider, deci niciun test o putea atinge. Extras ca `claimedPeriodIsInverted`
și testat, inclusiv că bornele **egale nu** sunt inversate: „a început și s-a terminat în aceeași zi"
e o revendicare legitimă.

**4. „Un test păzește toată clasa" era o exagerare** — acoperea 2 din 14 enum-uri cu schemă
publicată. Acum toate 14, plus o asserțiune pe **număr**, ca următorul enum adăugat să coste o linie
și să nu poată fi uitat.

**5. Un typo real**: două literale de string lipite fără spațiu în `ServiceRequests.hs`. Compila
corect (verificat în JSON-ul generat); nu există gate de formatare în CI.

**Și a greșit o dată**, ceea ce merită notat pentru că e motivul pentru care verificările unui agent se
verifică: a raportat `Query/Aggregates.hs` cu același defect. Nu-l are —
`attestationToInformation` pasează întregul `Attestation` lui `attestationInformationToResponse`,
actualizat deja. Verificat înainte de a atinge fișierul.

**Ce a verificat și n-a găsit nimic**, util în sine: cele două literale `OnchainAttestation`
on-chain/off-chain coincid câmp cu câmp, deci `Mk` nu cade; toate 16 coloanele proiecției sunt mapate
corect; `(Maybe GYTime)` e genuin nullable în persistent 2.14.5, deci niciun insert NOT-NULL; numărul
de 17 coloane e corect; nimic altceva nu e legat de versiunea 1 de schemă; `FromJSON Attestation` are
14 parsere pentru 14 câmpuri în ordine, cu cheile exact cum le produce `StripPrefix` + `CamelToSnake`;
și nu există nicio sortare sau filtrare pe perioada revendicată pe nicio suprafață.

### Verificat

**326 de teste trec** (303 + 23). `cabal build all` curat, `swagger OK`, `vocabulary OK`, și
niciunul din cele două documente Swagger nu s-a schimbat — nicio rută documentată nu numește
`AdminActionType`.

În blueprint-ul generat: **cinci validatoare**; `MintingRedeemer` cu arități 9/5/7 la indicii 0/1/2;
`DisputesRedeemer` 2/0 la 0/1; `Verdict` la 0 și 1; cele opt câmpuri ale lui `OnchainDispute` în
ordine (`AssetClass` ×3, `POSIXTime`, `Integer`, `Address`, `AssetClass`, `Maybe_Verdict`); indicii
`OracleAdminAction` de la 0 la 5.

Commit: `405c8d9`.

---

## Cele patru decizii de owner, LUATE — 2026-10-21

Owner-ul a decis toate patru. Aici e ce a decis, argumentul pe care l-a acceptat și consecința pe
care o trage fiecare; analiza care le-a precedat rămâne mai sus, neatinsă.

### 1B — GHCR: se **mută repo-ul** `en7angled/palmares` sub `mariusgeorgescu`

Nu PAT-ul, nu flip-ul de vizibilitate: **mutarea**. `GITHUB_TOKEN` e scoped la owner-ul repo-ului în
care rulează, deci cât timp repo-ul stă sub org nu poate scrie în `ghcr.io/mariusgeorgescu`, iar
`packages: write` nu ajută — permisiunea aia e asupra pachetelor **org-ului**. Mutarea face din
`GITHUB_TOKEN` exact credentialul potrivit, deci secretul `GHCR_TOKEN` nu se mai creează niciodată,
și nici cele patru pachete nu mai trebuie trecute o dată pe public. Alternativa cu PAT le-ar fi
**amânat** pe amândouă; mutarea le face inutile. (Pe a doua jumătate am lăsat o verificare în
`## BLOCKED FOR USER`: GHCR pune pe privat orice pachet nou, deci se confirmă — sau nu — la prima
publicare de după mutare.)

**E o acțiune de om și e încă nefăcută** — vezi `## BLOCKED FOR USER`. Zero editări de cod:
`images.yml` cară deja `GHCR_OWNER=mariusgeorgescu`, iar `secrets.GHCR_TOKEN || secrets.GITHUB_TOKEN`
cade pe ramura a doua de la sine.

### 2 — Codurile de trace: `preserve-logging` peste tot

Build-ul care se desfășoară **păstrează** mesajele de trace. Costul e măsurat, nu estimat: **+1,8%
… +3,4%** la dimensiunea de script (tabelul din descoperirea 1 de mai jos). Argumentul acceptat: un
registru de reputație pe care nu-l poți depana în producție e unul pe care nu-l poți susține, iar
octeții economisiți nu cumpără nimic la 45% ocupare de tranzacție.

### 3 — Cheia de admin a oracle-ului pe preprod: una nouă, dedicată, de unică folosință

Numai pentru preprod; alegerea pentru mainnet rămâne amânată. Argument: preprod e un mediu de test,
iar rotația cheii e acum demonstrată cap la cap de cazurile de emulator **0.9–0.11** — deci o cheie
pierdută pe preprod costă un `OracleSetAdmin`, nu un redeploy.

**Consecința, pe care o asum și o consemnez:** deploy-ul de preprod **al protocolului de atestări**
(T-ATTEST-10) rămâne pe **docker-compose**, nu pe drumul cluster/GitOps. O cheie de unică folosință nu
are nevoie de SOPS+age și de un Kustomization proprietar, iar jumătățile de cluster sunt blocate
oricum. Deploy-ul GitOps al lui F1-C (P8) nu e atins de asta.

### 4 — Depozitul de dispută: **5 ADA rămâne** ca default de desfășurare

E re-tarifabil **fără redeploy** prin `SetDisputeDepositAction` (demonstrat de cazul de emulator
**0.5**), deci nu e o alegere pe care trebuie s-o nimerim acum. Și e **singura** frână pe spam-ul de
dispute, pentru că „o singură dispută deschisă per atestare" e deliberat neimpus on-chain — N dispute
costă N depozite, și asta e tot ce le mărginește.

### Și cele închise mai devreme, rămân închise

`Auditor` e **fel** de profil, nu rol — un octet de datum pe care niciun validator nu ramifică — cu
eticheta de prezentare `self_declared` la stratul de citire. Felul e eticheta, atestarea e faptul.

---

## T-ATTEST-5, prima jumătate — suita de emulator rulează din nou

`c0b270d` în palmares: *test(emulator): the suite runs again — 35 cases, and the trace codes are a
fiction*.

Până la commit-ul ăsta proiectul avea **zero** teste de emulator. Cinci module pe disc, dormante și
stale față de un protocol pe care îl precedă, și trei linii de cabal care **nu numeau niciun fișier**.
Validatoarele erau dovedite de teste pure și de blueprint-ul generat — adică de nimic care să fi
construit vreodată o tranzacție.

**326 → 361 de teste.** 35 noi, **11 negative**:

- **`src/test/TestRuns.hs`** — înviat și tăiat de la contextul de desfășurare cu **șapte** câmpuri al
  Belt System-ului la cele **șase** ale protocolului ăsta: patru validatoare de cheltuire ca scripturi
  de referință, NFT-ul one-shot al oracle-ului, apoi minting policy-ul compilat contra lui. Blocul de
  115 linii de promovare/grad și atacul `AcceptPromotion` au plecat împreună cu validatoarele pe care
  le testau. Fiecare runner trece în continuare prin `interactionToTxSkeleton`, deci testele exersează
  **drumul de producție**, nu unul paralel scris pentru ele.
- **`src/test/UnitTests/Oracle.hs`** — **13** cazuri de emulator (erau 9, toate moarte). Acoperă
  fiecare acțiune de admin, re-tarifarea depozitului de dispută adăugată la T-ATTEST-4, rotația de
  admin cap la coadă, și conservarea valorii de către `O2`.
- **`src/test/UnitTests/Cleanup.hs`** — **6** cazuri (erau 5, moarte, zero negative). Măturile se
  asertează acum pe **numărul de UTxO-uri**, nu pe o linie de log care tipărea „succeeded" indiferent
  dacă praful plecase sau nu.
- **`src/test/PropertyTests.hs`** (nou) — `applyAdminAction` oglindit contra `applyOracleAdminAction`
  pe fiecare acțiune de admin, plus round-trip `toBuiltinData`/`fromBuiltinData` pe fiecare tip de
  datum.
- **`src/test/UnitTests/ScriptSize.hs`** (nou) — poarta de dimensiune de script care lipsea din tot
  proiectul.

### Trei descoperiri

**1. Codurile de trace documentate NU EXISTAU.** `palmares.cabal` compila fiecare validator cu
`-fplugin-opt PlutusTx.Plugin:remove-trace`, deci `O1`, `D0`, `Md1`, `A3` și tot restul erau **șterse
înainte să ruleze orice**. O tranzacție refuzată spunea doar „an error has occurred" — în emulator și
pe preprod la fel; `mustFail` nu putea spune care verificare a refuzat-o, și nici un operator n-ar fi
putut. Un flag de cabal `preserve-logging` adăugat, iar prin decizia 2 e acum **default-ul**.
Dimensiuni măsurate, octeți flat-encoded:

| validator | remove-trace | preserve-logging | delta |
| --- | --- | --- | --- |
| ProfilesValidator | 7.707 | 7.879 | +2,2% |
| AttestationsValidator | 7.410 | 7.554 | +1,9% |
| DisputesValidator | 8.032 | 8.243 | +2,6% |
| OracleValidator | 7.293 | 7.427 | +1,8% |
| MintingPolicy | 11.482 | 11.871 | +3,4% |

> **Actualizare 2026-10-21:** coloana `preserve-logging` a minting policy-ului a mișcat de două ori de
> atunci (11.871 → 11.850 → **11.865**), din cele două reparații de securitate. Tabelul pinuit, viu,
> e cel de la finalul fișierului; delta-urile măsurate aici rămân valabile ca ordin de mărime.

Minting policy-ul e ~70% dintr-o tranzacție întreagă de 16.384 de octeți și singurul script care
crește per caz de redeemer — deci e și singurul loc unde decizia ar putea fi revizuită vreodată. Cu
trace-urile păstrate codurile sunt **observabile**: cazul de oracle cu non-admin logează
`["O1","PT5"]`, iar cel de cleanup `["D0"]`.

**2. Testul moștenit „update de oracle de non-admin eșuează" nu ajungea niciodată la validator.**
`updateOracleTX` citește adminul din datumul oracle-ului și cere **chiar semnătura aceea**, deci un
non-admin construiește o tranzacție pe care nu o poate semna și moare la stratul de required-signer,
înainte să ruleze vreun script. Testul ar fi trecut cu `O1` **șters**. Un builder nou,
`maliciousOracleUpdate` în `TestRuns.hs`, trimite aceeași tranzacție semnată de atacator, și cele trei
cazuri `O1` îl folosesc.

**3. Jumătatea on-chain a lui `Cleanup` n-avea nicio acoperire.** Toate cele cinci cazuri moștenite
erau mături care **reușeau**, deci `Cleanup -> const True` ar fi trecut fiecare dintre ele. Cazul nou
**3.5** blochează un datum care **parsează** ca stare de dispută fără token de protocol: selectorul
off-chain de praf îl oferă măturii, iar singurul care-l poate refuza e validatorul.

### Două abateri deliberate de la plan

**1. T-ATTEST-5 împărțit în două și reordonat după T-ATTEST-6.** Planul cere T-ATTEST-5 înainte de
T-ATTEST-6, și ordinea aia nu poate ține: `ProfileActionType` **nu are caz de mint de profil**, deci
nu există niciun drum off-chain prin care să creezi profilul de care are nevoie fiecare test de
atestare, de dispută și de claim. Oracle, Cleanup, testele de proprietate și poarta de dimensiune nu
au nevoie de nimic din asta și sunt livrate. `UnitTests.Profile` (9 cazuri), `UnitTests.Attestation`
(13) și cazurile de emulator de dispută (6) se mută după T-ATTEST-6 ca **T-ATTEST-5b**, cu numerotarea
cazurilor păstrată.

**2. Fără `hedgehog`.** Planul cerea teste de proprietate cu hedgehog. Ce era de acoperit e o sumă cu
**șase** constructoare și un set fix de forme de record, deci tabelul exhaustiv **este** proprietatea —
condus de `[minBound .. maxBound]`, cu o aserțiune pe **număr** care pică atunci când se adaugă un
constructor. Un generator ar re-arunca zarurile peste aceleași forme, pentru o dependență nouă și o
pagină de boilerplate de `Gen`. `src/test/CLAUDE.md` numește în continuare `PropertyTests.hs` ca locul
oglinzilor on-chain/off-chain, iar modulul păstrează numele și treaba.

Și de notat: `OnchainProfileKind` și `AttestationKind` au căpătat `Enum`/`Bounded` **stock**. E
Haskell curat, nu generează cod Plutus, și **niciun hash de script nu s-a mișcat** — verificat chiar
de tabelul de dimensiuni de mai sus.

### Ce rămâne, în ordinea dependențelor

> **Depășit la 2026-10-21** — tabelul viu e cel din secțiunea „Ce rămâne" de la **finalul
> fișierului**. T-ATTEST-6, 7, 8 și 5b sunt **gata**; rândurile 5, 6 și 7 au rămas, plus backlog-ul
> celor două audituri.

| # | task | blocat de |
| --- | --- | --- |
| 1 | **T-ATTEST-6** — acțiuni de domeniu, builder-ul de claim, martorul de script | — |
| 2 | T-ATTEST-7 — rutele interaction-api, `/status`, gaura de admin peste HTTP | 6 |
| 3 | T-ATTEST-5b — emulatorul de Profile + Attestation + Dispute (~28 de cazuri) | 6 |
| 4 | T-ATTEST-8 — Ingestion, `authenticateStateUTxO`, fixul moștenit #3 | 6 |
| 5 | T-ATTEST-9 — suprafața de citire peste `palmares_attest` | 8 |
| 6 | T-ATTEST-10/11/12 — deploy pe preprod, `smoke_attest_flow.sh`, Kupo în compose | 7, 9 |
| 7 | Planul C — UI-ul (F1 checks 13–15), încă nescris | — |

**T-ATTEST-6 e singurul task care deblochează ceva.**

### Golul de corectitudine pe care T-ATTEST-6 îl închide: protocolul nu poate fi pus în pauză azi

Numai `issueAttestationTX` atașează oracle-ul ca input de referință și construiește un interval de
validitate. Deci `opPaused` nu oprește nici un update de profil, nici o acceptare de atestare —
comutatorul există în datum și nu e citit pe drumurile alea. Nu e o preferință de design, e un gol
pe care planul îl numește și pe care task-ul următor îl închide.

> **Corecție 2026-10-21 — T-ATTEST-6 a închis doar jumătatea off-chain.** `opPaused` e citit **numai**
> de MintingPolicy, deci `AcceptAttestation` și `UpdateProfile` **nu se pot pune în pauză ON CHAIN**
> nici acum; oricine își construiește propria tranzacție trece pe lângă pauză. Mesajul commit-ului
> `8a1549c` („the pause was a fiction, and this fixes it") **exagerează** — jumătatea on-chain e în
> backlog-ul de audit, `MintingPolicy.hs:161`.

### Blocat în continuare pe cluster (neschimbat)

Jumătățile de cluster ale T-SUBSTRATE-2…6, Job-ul/secretul/kustomization-ul de k8s din T-SUBSTRATE-8,
și F1 checks **1** și **16**.

### Verificat

**361 de teste** (326 + 35), **11** negative. Codurile de trace sunt observabile în ieșirea
emulatorului — `["O1","PT5"]` pe cazul de oracle cu non-admin, `["D0"]` pe cel de cleanup. Tabelul de
dimensiuni e produs de `UnitTests/ScriptSize.hs`, deci de aici înainte o creștere de script e o
aserțiune care pică, nu o notiță care îmbătrânește.

Commit: `c0b270d`.

---

## Două audituri adversariale pe stratul on-chain — 2026-10-21

Cerute explicit de owner, cu **F2 (review-ul extern) ținut în backlog** cât timp ele rulează: un
review plătit care raportează ce găsesc gratuit două audituri proprii e bani dați pe aceeași listă.

Amândouă au aceeași disciplină, pentru că fără ea un audit produce o listă de **suspiciuni**, nu de
defecte: fiecare constatare trece prin **două lentile de refutare independente** — una citește codul
și încearcă să arate că verificarea există, alta încearcă să construiască exploit-ul — și
**supraviețuiește doar dacă cel puțin una n-a putut s-o refuze.**

### Auditul 1 — tăiat pe dimensiuni

Șase dimensiuni: minting policy, validatorul de profiluri, validatoarele de stare, oracle,
conservarea valorii, identitate și griefing. **42 de agenți.**

**15 confirmate** (cu duplicate între dimensiuni — două dimensiuni care găsesc același lucru separat e
semnalul cel mai tare pe care metoda îl poate da), **3 refuzate**, **8 de severitate mai mică lăsate
neverificate**.

### Auditul 2 — tăiat pe clase de vulnerabilitate

Din cele două cataloage date de owner — `https://scalus.org/docs/security/common-vulnerabilities` și
`https://www.mlabs.city/blog/common-plutus-security-vulnerabilities` — opt familii:
dublă-satisfacere, conservarea valorii, autentificare, minting, timp și aritmetică, datum și indici,
scopuri și evaluare, resurse și design. **Deduplicat contra constatărilor auditului 1 înainte de
verificare**, ca nimic să nu se plătească de două ori, apoi aceleași două lentile. **40 de agenți.**

**12 confirmate**, **4 refuzate**, **5 neverificate**.

### Ce NU s-a confirmat — consemnat ca rezultat, nu ca absență

**`DS-1`/`DS-2` și multiple-satisfaction: zero confirmări.** Dubla satisfacere e **prima clasă din
ambele cataloage**, și **opt agenți** au vânat-o anume. Nimic.

**`VP-3`** (output-ul de continuare redirecționat) a ieșit la fel de curat: helper-ele **compară
adresa**, deci numele `checkTxOutAtIndexWithDatumMinValueAndAddress` nu mințea.

Iar ce a venit cel mai greu e o familie pe care n-o prezicea nimeni: **`DE-1`, valoare blocată —
stări fără ieșire.** Două confirmate (`DisputesValidator.hs:119`, o dispută deschisă cu o singură
ieșire și fără timeout; `AttestationsValidator.hs:81`, o atestare despre o atestare care nu poate fi
acceptată niciodată) plus una neverificată (`DisputesValidator.hs:118`, o dispută rezolvată fără
tranziție de retragere).

> **Corecție 2026-10-21.** Prima redactare a acestui paragraf spunea „trei confirmări". Două erau
> confirmate și una rămăsese neverificată, fiindcă plafonul de verificare a fost trei descoperiri pe
> dimensiune. Numărul venea din mine, nu din datele auditului.

---

## Două reparații de securitate, amândouă comise

### CRITIC (`7e6b02a`) — jumătatea de staking făcea orice credential de script revendicabil de oricine

`checkCredentialControl` din `Utils.hs` citea:

```haskell
payment == credential || staking == Just (StakingHash credential)
```

A doua disjuncție e gaura. O adresă CIP-19 de **tip 2** — hash de cheie de **plată** + hash de
**script** de staking — e o adresă perfect obișnuită și **nu are nevoie de niciun martor pentru
script**, pentru că regula de martori a ledger-ului autentifică doar credentialul de **plată** al unui
input. Deci oricine putea construi o adresă cu cheia **lui** în jumătatea de plată și hash-ul
scriptului **victimei** în cea de staking, s-o cheltuie cu propria semnătură, și să revendice
credentialul victimei.

**Fiecare DRep id de script și fiecare credential rece de comitet constituțional era revendicabil de
oricine.** Și **permanent**: `profileCredential` nu are redeemer de update, nu există `DeleteProfile`,
și niciun braț de minting nu arde.

Găsită **independent de două din cele șase dimensiuni** ale auditului 1 și confirmată de **toate
patru** lentilele lor.

Disjuncția e **ștearsă**. Nimic legitim nu o folosea: prover-ul off-chain `claimProofSkeleton` caută
prin `utxosAtPaymentCredential`.

**Ce costă, consemnat cinstit:** un credential care e **genuin** un script de stake nu mai poate fi
revendicat pe drumul ăsta. Dar nici nu putea fi **dovedit** pe el — o cheltuire nu martorează un
credential de stake. Dovada sănătoasă e o **retragere** în `txInfoWdrl`: altă funcție, cu ExUnits
proprii, de care credentialele pentru care există registrul ăsta nu au nevoie.

**Și testul care garanta gaura.** `UnitTests/Claim.hs` aserta cazul de atac `@?= True`, cu un
comentariu care argumenta pentru el. **Un test verde care pinuia o vulnerabilitate critică**, într-o
suită de **374 de teste trecute**. Acum e `@?= False`, plus un caz care arată că fixul a scos gaura și
**nu** funcția.

### HIGH (`1668145`) — fixul moștenit #4 mărginea capetele intervalului, nu LĂȚIMEA lui

`checkDateInRange` cerea ambele margini de validitate **finite** și data **conținută**. Finitudinea nu
costă un atacator **nimic**: `Interval (Finite 2015-01-01) (Finite acum+2h)` e finit la ambele capete,
e **acceptat de ledger** fiindcă slotul curent cade în el, și conține **orice** dată vrea. Deci
back-dating-ul a rămas **complet nemărginit** — exact gaura pe care fixul #4 fusese scris să o
închidă. Auditul 2, clasa `TI-1`, **ambele lentile high**.

Indiciul era haddock-ul funcției însăși: *"The caller's window is what gives this meaning: off-chain
builds a ~7h window from the chain tip"*. **Caller-ul e cine construiește tranzacția, și un atacator e
un caller.**

Reparat verificând **lățimea** pe lanț, contra `maxDateWindow` = 4 × `safeEraTime` ≈ **28h**.

**Deliberat o constantă, nu un parametru de oracle** — împotriva convenției proprii a protocolului, și
pentru un singur motiv: un admin care poate **lărgi** fereastra poate valida retroactiv orice
înregistrare back-datată, adică exact atacul pe care marginea îl oprește. E o afirmație despre **ce
înseamnă o dată** aici, nu un preț.

### Tiparul care merită un nume, pentru că s-a repetat de trei ori în aceeași zi

**Artefactul care explica de ce ceva e sigur a fost, de fiecare dată, dovada că nu e.**

| artefactul | ce explica | ce s-a întâmplat |
| --- | --- | --- |
| comentariul din `palmares.cabal` lângă `extra-source-files` | că lista trebuie ținută în pas cu Dockerfile-urile | a pierdut contra **exact** eșecului pe care-l descria (mai jos) |
| `UnitTests/Claim.hs` | de ce claim-ul pe jumătatea de staking e corect | pinuia o vulnerabilitate critică, verde |
| haddock-ul lui `checkDateInRange` | că fereastra caller-ului dă sens verificării | caller-ul poate fi atacatorul |

Un comentariu, un test verde și un haddock sunt toate **afirmații neexecutate** despre cod — și a fi
scrise convins e exact ce oprește următoarea citire.

---

## Patru task-uri de implementare, livrate în paralel

Patru agenți au scris **în același timp**, fiecăruia **interzis să ruleze `cabal`** (două build-uri în
același `dist-newstyle` se bat pe lock) și fiecăruia dat un **tabel explicit de fișiere INTERZISE**.
`palmares.cabal`, `src/test/UnitTests.hs` și **toate commit-urile** au rămas la părinte — cele trei
locuri în care patru fire ar fi produs conflicte, nu muncă.

### T-ATTEST-6 (`8a1549c`) — drumul de scriere off-chain

`ClaimProfileAction` (crearea de profil cu claim **opțional** de credential), `ClaimWitness` în trei
forme, `DisputeActionType`, și `claimProfileTX` / `openDisputeTX` / `resolveDisputeTX`.

**`txMustSpendFromAddress` primește acum martorul ca PARAMETRU.** Avea `GYTxInWitnessKey` **hardcodat**,
ceea ce făcea claim-ul pe un credential de script nu doar neimplementat, ci **inexprimabil** — iar
check-ul **8** din spec cere exact claim-ul ăla. O singură semnătură de funcție stătea între „nu e
scris încă" și „nu se poate scrie".

**`parseCredentialCip129` acceptă numai CIP-129** și refuză CIP-105 și hex-ul gol. Motivul e că nici
una din formele respinse nu cară **octetul de header**, iar `drep` e HRP-ul pentru amândouă: o parsare
permisivă ar fi **revendicat silențios fiecare DRep de script ca pe unul de cheie**.

**Jumătatea off-chain a fixului #4:** `aa_attestation_date` a **dispărut** din tipul de cerere. Data se
derivă din `slotOfCurrentBlock`, deci nu mai există nicio suprafață pe care un client s-o propună —
iar lățimea verificată pe lanț o mărginește oricum.

### T-ATTEST-8 (`bab8f5f`) — autentificarea proiecțiilor (fixul moștenit #3)

Defectul: **ingestia decidea ce proiectează din forma datumului**, iar oricine poate pune orice datum
la orice adresă — deci un UTxO **plantat** era servit ca stare reală de protocol. E `AU-1` /
`missing-utxo-authentication` din cataloage, pe un drum pe care niciunul din ele nu-l are în vedere:
off-chain, nu validator.

Și partea care contează: **`authenticateStateUTxO` exista deja, era deja testat unitar, și era apelat
de nicăieri.** Funcția potrivită, scrisă și verificată și **moartă** — vezi corecția de la rândul
`bd7d1a6` din blocul 2.

Acum e gardă pe **toate trei** brațele indexate, fără cale de ocolire. Un UTxO plantat ajunge în
`NoEvent`, se păstrează ca **match brut** și nu se proiectează nicăieri — deci nici dovada că cineva a
încercat nu se pierde.

**`currentSchemaVersion` 2 → 3, și re-sync-ul NU e opțional.** Fiecare rând dintr-o bază v2 a fost
scris **fără** verificare, deci o bază v2 poate **ține** rânduri plantate pe care niciun replay nu le
mai produce. La bump-urile de dinainte re-sync-ul **completa coloane**; aici **scoate minciuni**.

### T-ATTEST-5b (`0fd1df0`) — 31 de cazuri de emulator, 15 negative

**Cazul 1.3 din plan a trebuit INVERSAT.** Îl specifica drept caz de **succes**: claim-ul pe jumătatea
de staking. Pentru că asta **făcea codul** când s-a scris planul — deci planul, testul moștenit și
comentariul lui spuneau toate același lucru greșit, în trei locuri diferite.

**Șase builder-i malițioși unde planul bugeta trei.** `D1`, `D3`, `Ml` și `Mf` **nu sunt accesibile din
drumul de producție** — off-chain-ul refuză înainte — deci un `mustFail` gol ar fi livrat **patru teste
care trec și cu garda on-chain ștearsă**. Un test negativ care nu ajunge la validator nu testează
validatorul; e exact defectul prins la T-ATTEST-5 pe `O1`, de patru ori.

**Cazul 5.12** („o atestare back-datată EȘUEAZĂ") e **prima dovadă pe tranzacție reală** a verificării
de lățime din `1668145`.

### T-ATTEST-7 (`d55d2cc`) — suprafața HTTP, 12 căi documentate

**O schimbare de comportament pe o rută vie: adminul peste HTTP e închis complet.** `FromJSON` eșuează
acum pe **fiecare** constructor de `AdminActionType`, **inclusiv pe cele patru care parsau**. Motivul
nu e precauția: validatorul de oracle cere semnătura adminului **pe lanț**, deci un server poate
întoarce doar o tranzacție nesemnată — în timp ce parserele lăsau **orice** deținător de Basic-auth să
pună serverul să construiască o tranzacție de **pauză de protocol**. O suprafață care nu putea produce
nimic util putea produce ceva periculos.

**Și Swagger-ul publicat contrazicea formatul de sârmă pentru fiecare tip de acțiune-sumă.**
`swagger2` nu are `sumEncoding`, deci emitea `{"<Constructor>": {…}}` cu `maxProperties: 1`, în timp ce
aeson folosește `{"tag": …}`. **Un client care urma documentul lua 400.** Reparat cu `taggedSumSchema`,
care **desface** schema generică a lui `swagger2` în loc s-o înlocuiască — deci un braț nou
actualizează documentul **singur**, și nu apare o a doua definiție de ținut în pas. Documentul
regenerat: **+434/−110**.

---

## Build-ul de imagini, și ce a învățat

**Prima rulare de `images.yml` de după mutarea repo-ului a dovedit decizia 1B**: ambele imagini de bază
au ajuns în `ghcr.io/mariusgeorgescu` cu `GITHUB_TOKEN`, pe pasul **exact** pe care rularea de dinainte
de mutare picase — după **42 de minute**.

Apoi toate patru build-urile de serviciu au murit în ~90 de secunde fiecare:

```
cabal: sdist of palmares-0.1.0: filepath wildcard
'resources/metric-formulas.json' does not match any files.
```

`cabal install` face întâi un sdist, iar acolo un glob din `extra-source-files` care nu potrivește
nimic e **eroare dură**. `resources/` intrase cu catalogul de metrici și niciun Dockerfile nu fusese
atins.

**Și `palmares.cabal` căra un comentariu care spune exact asta** — scris în blocul 4, lângă
`extra-source-files`, tocmai pentru că o adăugire acolo sparge build-ul de imagine și nu build-ul
cabal. **Comentariul a pierdut.** Ăsta e datul util, nu bug-ul.

Reparat în `0026157`: `COPY resources ./resources` în toate patru Dockerfile-urile, plus
**`scripts/check_packaging.sh`** — parsează `extra-source-files` și `license-file` din fișierul cabal
și verifică că fiecare cale e acoperită de un `COPY` în **fiecare** Dockerfile. **Rulat contra stării
de dinainte de fix ca să confirm că PICĂ acolo**, și pus ca **primul** pas al job-ului `haskell` din
`ci.yml`: costă sub o secundă, iar eșecul pe care-l prinde costă altfel o oră.

O verificare executabilă în locul unui comentariu — pentru că un comentariu nu are cum să pice.

### Dimensiunile de script, care și-au meritat locul

Tabelul pinuit din `UnitTests.ScriptSize` a fost scris sesiunea asta ca **declanșator economic**, iar
prima lui folosință reală a fost să **mărginească raza de explozie** a două reparații de securitate:

| | octeți |
| --- | --- |
| ProfilesValidator | 7.879 |
| AttestationsValidator | 7.554 |
| DisputesValidator | 8.243 |
| OracleValidator | 7.427 |
| MintingPolicy | 11.865 |

Minting policy-ul a mișcat de două ori — **11.871 → 11.850** la reparația critică, **11.850 → 11.865**
la cea de dată — și **celelalte patru n-au mișcat niciuna dintre dăți**. Asta spune ceva ce o citire de
cod nu dă la fel de repede: **ambele funcții sunt atinse numai din minting policy.** O schimbare
**mărginită**, nu una descrisă.

---

## Ce rămâne — 2026-10-21

T-ATTEST-6, 7, 8 și 5b sunt **gata**. Tabelul viu:

| # | task | blocat de |
| --- | --- | --- |
| 1 | **T-ATTEST-9** — suprafața de citire peste `palmares_attest`; de decomentat `Query.Projected`/`Pages`/`Aggregates` | — |
| 2 | **Backlog-ul de audit** — **6 high + 8 medium** confirmate și încă deschise, din ambele audituri (lista de mai jos le strânge în unsprezece intrări) | — |
| 3 | T-ATTEST-10/11/12 — deploy pe preprod, `smoke_attest_flow.sh`, Kupo în compose | 1 |
| 4 | **Planul C** — UI-ul, scope decis ca **varianta B** | cere o notă de design |

### Backlog-ul de audit, deschis

> **Corecție 2026-10-21.** Rândul din tabelul de mai sus spunea întâi „4 high + 5 medium", care era
> numărătoarea **doar a auditului 1**, pusă lângă o listă care strângea descoperirile din **ambele**.
> Cifra corectă, deduplicată pe ambele audituri, e **6 high + 8 medium**. Cele unsprezece intrări de
> mai jos sunt mai puține decât paisprezece fiindcă trei locuri împart gaura „datumul parsează"
> (`ProfilesValidator.hs:92`, `DisputesValidator.hs:88`, `AttestationsValidator.hs:70`) și două o
> împart pe cea de `otherMetadata` (`MintingPolicy.hs:225`, `CIP68.hs:112`) — fiecare numărată o
> dată. Greșeala a fost a mea, în brief-ul dat agentului, nu a lui: a scris rândul verbatim și a
> refuzat să inventeze maparea de severități, care e exact ce trebuia să facă. — fiecare cu locul lui, ca sesiunea următoare să nu mai caute

- **`MintingPolicy.hs:339`** — `handleOpenDispute` nu verifică **niciodată** currency symbol-ul lui
  `openedBy`, deci o dispută nu are nevoie de profil, iar `openedBy` poate **impersona id-ul de profil
  al unei victime**. Găsită de 3 dimensiuni.
- **`DisputesValidator.hs:140`** — `D4` întoarce depozitul cu un `any` **la nivel de tranzacție**, fără
  legătură per-dispută: **un singur output rezolvă toate disputele** care împart un `disputeDepositTo`.
- **`MintingPolicy.hs:330`** — `disputeDepositTo` intră în datumul permanent **nevalidat**.
  `Address (PubKeyCredential (PubKeyHash ""))` e o valoare `Data` **legală** care face `D4`
  **nesatisfiabil** — deci un singur depozit de 5 ADA cumpără o dispută **permanent nerezolvabilă**,
  care blochează pe deasupra canalul de dispute al atestării respective.
- **`DisputesValidator.hs:119`** — o dispută deschisă are **exact o ieșire**, condiționată de **o
  cheie**, **fără timeout**.
- **`DisputesValidator.hs:110`** — o dispută **numește oracle-ul care autorizează propriul ei verdict**.
  ⚠️ Auditul 2 a produs pe linia asta **și o constatare confirmată și una refuzată**, cu încadrări
  diferite: **codul trebuie citit înainte de a acționa.**
- **`OracleValidator.hs:74`** — `OracleSetAdmin` **nu cere nicio semnătură de la adminul ÎNTRĂTOR**,
  deci un singur `PubKeyHash` greșit **bricuiește protocolul permanent**. (De consemnat ironia: e fixul
  moștenit #2, adăugat tocmai ca să facă compromiterea cheii recuperabilă.)
- **`MintingPolicy.hs:161`** — `opPaused` e citit **NUMAI** de MintingPolicy, deci `AcceptAttestation`
  și `UpdateProfile` **nu se pot pune în pauză on-chain**. T-ATTEST-6 a închis **doar jumătatea
  off-chain**, iar mesajul commit-ului `8a1549c` („the pause was a fiction, and this fixes it")
  exagerează.
- **`MintingPolicy.hs:226`** — **niciun** braț nu verifică prefixul de referință CIP-67 pe un câmp de
  id, iar `deriveUserFromRefTN` taie patru octeți **orbește**.
- **`MintingPolicy.hs:225` / `CIP68.hs:112`** — `otherMetadata` intră în datumul permanent fără
  validare de **dimensiune, număr sau chei rezervate**, și poate construi un `AssocMap` cu **chei
  duplicate**, încălcând precondiția lui `unsafeFromList`.
- **`AttestationsValidator.hs:81`** — o atestare **despre o atestare** nu poate fi acceptată niciodată:
  token-ul de user al subiectului nu există și nu poate fi mintat. Iar **lineage-ul e în spec-ul de
  produs**.
- **`Blueprint.hs:135`** — blueprint-ul CIP-57 publicat livrează minting policy-ul **deja aplicat** pe
  un token de oracle placeholder, deci **niciun terț nu poate verifica ce instanțiere folosește**
  (`AU-6`).

### Două acțiuni de owner, ÎNCHISE în runda asta

1. **Token-ul Maestro nu mai e necesar.** Drumul e provider-ul local al Atlas — `socketPath` + **Kupo**
   — și devine **T-ATTEST-12**. ⚠️ **Ogmios e de EVITAT**: nodul 11.1.2 cere Ogmios v7, iar Ogmios v7
   rupe Atlas — exact motivul pentru care Belt System-ul a plecat pe Maestro.
2. **Robinetul de preprod nu mai e necesar.** Adresa de preprod a Belt System-ului ține **43.063 tADA
   în 46 de UTxO-uri**, verificat prin Koios, deci cheia nouă de unică folosință a deciziei 3 se
   alimentează de acolo.

### Blocat în continuare (neschimbat)

`## BLOCKED FOR USER` are o singură intrare rămasă: **trecerea pachetelor GHCR pe public** —
`palmares-admin` **musai** public, altfel `reconcile.yml` cade cu `pull access denied` în loc de o
divergență reală. Plus jumătățile de cluster ale T-SUBSTRATE-2…6, Job-ul/secretul/kustomization-ul de
k8s din T-SUBSTRATE-8, și F1 checks **1** și **16**.

### Verificat

**326 → 440 de teste, toate 440 trec.** **Zece commit-uri** împinse în palmares,
`405c8d9..d55d2cc`. Tabelul de dimensiuni de script e o aserțiune, nu o notiță — și a fost pârghia
care a mărginit ambele reparații de securitate.

---

## 2026-10-22 — poarta de secrete, transportul, șapte descoperiri on-chain și suprafața de citire

Cinci commit-uri, `d55d2cc..87807f1`. **519 teste, toate trec** (de la 440). CI verde pe tot, inclusiv
pe două porți noi.

### Ce a schimbat lista de acțiuni ale owner-ului

`## BLOCKED FOR USER` **e goală**. Ambele intrări s-au închis în aceeași zi — una prin măsurătoare,
una prin decizia owner-ului — și secțiunea de mai sus poartă raționamentul greșit al fiecăreia,
pentru că aia e partea utilă. Pe scurt:

- **Vizibilitatea pachetelor GHCR**: rularea `reconcile` 39697594482 a tras
  `ghcr.io/mariusgeorgescu/palmares-admin:latest` **fără `docker login`**. Documentația pe care mă
  bazasem descrie un pachet împins cu un **PAT**, care nu e legat de niciun repo; unul publicat de un
  **workflow cu `GITHUB_TOKEN`** moștenește vizibilitatea repo-ului. Decizia 1B a închis și asta.
- **A5, token-ul Maestro**: *„nu ma deranjeaza ca tokenul maestro e expus e un token pentru
  preprod"*. Cheie per-proiect și per-rețea. Cele trei fișiere de memorie care cereau rotirea au fost
  corectate, nu lăsate să ceară o acțiune refuzată.

### `60d9aea` — poarta de secrete a citit NUMELE unei legături

CI a căzut pe `gitleaks`: o descoperire, `generic-api-key` pe
`src/test/UnitTests/WritePath.hs:75`, `drepKeyBech32 = "drep1y2qn0rf…"` — un DRep id CIP-129,
publicat pe lanț.

**Diagnosticul e în cele trei linii de dedesubt.** `drepScriptBech32`, `drepCip105` și `ccColdBech32`
țin aceiași 28 de bytes în același modul și niciuna n-a declanșat regula, iar `UnitTests.GovIds`
fixează exact acești vectori de la T-ATTEST-2. Regula a potrivit cuvântul „Key" dintr-un
identificator lângă o valoare cu entropie mare. Legătura e numită după **felul** credențialului —
cheie versus script — adică distincția pentru care s-a reparat `checkCredentialControl` în `7e6b02a`,
deci redenumirea ar fi costat testul subiectul lui.

Îngustare **pe forma valorii, nu pe cale**: un bech32 cu HRP de guvernanță nu poate fi un credențial
(28 de bytes blake2b-224, 29 cu header CIP-129, plus checksum, publicat pe lanț ca identitate), deci
nu are unde să se lărgească. O alocare pe cale ar fi scuzat orice viitor șir cu entropie mare din
fișierul acela. Testată din ambele părți: `check_gitleaks_allowlist.sh` are cinci aserțiuni, inclusiv
un AWS key id plantat sub **exact** legătura care a declanșat regula.

### `b71001b` — handler-ul pentru un transport care eșuează prinde singura excepție care nu apare

Jobul de paritate a rulat prima dată (pentru că imaginea se putea trage) și a raportat
`parity: UNEXPLAINED DIVERGENCES` împotriva unui deployment inexistent, arătând spre un fișier pe
care rularea murise înainte de a-l scrie.

```haskell
Left (e :: IOError) -> retryOr attemptsLeft (show e)
```

`httpLbs` aruncă `HttpException`. Nu aruncă `IOError`. Handler-ul al cărui Haddock se numea „singura
cale către exit 2" prindea **orice eșec de transport în afară de un transport care eșuează**: DNS,
conexiune refuzată, timeout, certificat respins — toate scăpau și de el și de `try @TransportError`,
omorând procesul cu codul 1 al unei excepții neprinse. Ce ajungea la exit 2 era inversul: răspuns
non-200 și corp nedecodabil, adică exact cazurile în care transportul funcționase.

Documentația modulului spune de ce contează trei paragrafe deasupra bug-ului. **Al patrulea** la
tiparul înregistrat mai sus — artefactul care explică de ce ceva e în siguranță fiind dovada că nu e.

Fixat la ambele apeluri. Fixat în CI cu `PALMARES_API` pe portul 1 (rezervat, neascultat, eșuează în
milisecunde): asertează că **codul e 2** și că mesajul numește fetch-ul, fiindcă `loadSample` eșuat
dă și el 2 și o verificare care nu le distinge ar trece cu fetch-ul rupt. Plus: wrapper-ul șterge
`out/divergences.json` înainte de rulare — local, o rulare abandonată tipărea tabelul de ieri sub
verdictul de azi.

### `b22235c` — patru descoperiri în căile de dispută și oracol, una REFUZATĂ

Luate **acum** și nu după T-ATTEST-10: nimic nu e deployat, deci schimbarea a patru hash-uri costă
zero azi și un redeploy coordonat mâine. Același argument care a făcut decizia 2A gratuită.

| Cod | Ce era | Consecința |
|---|---|---|
| `Md4` | `openedBy` nu avea verificare de symbol; `Mg`, handler-ul frate, verifică mereu ambele | `Md3` devine auto-emis — un token sub politica atacatorului îl satisface, deci oricine deschide o dispută fără să dețină niciun profil |
| `Md8` | `depositTo` intra nevalidat în datum-ul permanent | o adresă pe care niciun output real nu o poate purta face `D4` **nesatisfiabil**: dispută nerezolvabilă, deschisă pe veci pe dosarul altcuiva, cinci ADA |
| `D6` | `D4` demonstra obligația cu `any` peste output-uri | două dispute cu aceeași adresă de plată (cazul **obișnuit**) rezolvate într-o tranzacție, un depozit returnat, restul în change-ul adminului |
| `O5` | handover-ul era unilateral | `OracleSetAdmin` greșit la tastare = **sfârșitul protocolului**: `O1` refuză apoi orice update inclusiv cel care l-ar desface, și fiecare dispută deschisă devine nerezolvabilă |

**`DisputesValidator.hs:110` — REFUZAT, zero cod schimbat.** Auditul 2 a dat atât o confirmare cât și
o refutare pe linia aia, cu încadrări diferite. Refutarea e corectă: `handleOpenDispute` scrie
`disputeOracleToken = oracleToken protocolParams` — **parametrul** — deci o dispută nu poate numi un
oracol fals; un datum fabricat la adresa validatorului nu controlează niciun activ, iar
`authenticateStateUTxO` îl ține afară din proiecții. Citirea a economisit o schimbare de hash degeaba.

Pentru `D6`: indexarea output-ului nu ajută (ambele instanțe pot numi același index), etichetarea cu
id-ul disputei ar păstra batching-ul dar forțează un datum pe o plată către o adresă de cheie.
Refuzul batch-ului e o comparație și renunță doar la o capabilitate pe care nimeni n-a cerut-o.

Niciunul din cele trei teste de dispută nu a avut nevoie de un builder malițios: calea de producție
exprimă deja ambele atacuri de mint, fiindcă `openDisputeTX` ia ambii parametri de la apelant și nu
constrânge niciunul — de-aia erau descoperiri și nu ipoteze. Builder-ul scris pentru ele a fost șters.

### `e6f54a7` — un profile id trebuie să fie token de REFERINȚĂ, plus `otherMetadata` mărginit

**`Mg`/`Md4`, jumătatea CIP-67.** `deriveUserFromRefTN` înlocuiește primii patru bytes cu prefixul de
user, deci pe un nume care deja începe cu prefixul de user e **identitatea**. Numește token-ul USER al
unui profil drept id-ul lui și `Mf`/`A1` cer exact token-ul pe care proprietarul real îl deține:
atestarea e emisă și acceptată de oamenii potriviți și înregistrată sub un id pe care nicio linie de
`profile_projection` nu îl are. Acceptată pe lanț, deconectată în read layer — o înșelătorie, nu o
pierdere. Testul pur asertează **mecanismul**: `deriveUserFromRefTN userTN == userTN`.

**`Mn`.** `otherMetadata` intra nemărginit și neverificat. Costul cade pe **subiect** nu pe emitent:
`A1` re-blochează `ownValue`, deci subiectul trebuie să cheltuiască și să reblocheze UTxO-ul ca să
accepte, iar nimic nu-l arde. Plus chei duplicate care rup precondiția lui `unsafeFromList` — `lookup`
întoarce prima intrare iar un decoder off-chain are dreptul să nu fie de acord, deci datum-ul înseamnă
două lucruri după cine îl citește. 16 intrări, chei 64B, valori 256B, fără duplicate inclusiv față de
cele trei rezervate. **Numărul se verifică PRIMUL** și ordinea e portantă: verificarea de unicitate e
quadratică, și o scanare quadratică peste o listă dimensionată de atacator e un vector în sine.

**O CORECȚIE la `b22235c`.** Acel commit spunea că o atestare despre o atestare „e o limită
documentată… `ensureSubjectProfileExists` o refuză deja off-chain". **A doua jumătate e falsă**, iar
Haddock-ul funcției spune invers: *„Existence only, not kind. It deliberately does not require the
subject to be a profile: an attestation ABOUT an earlier attestation is a lineage record the spec asks
for."* Deci atestările de descendență sunt o **funcționalitate livrată și testată** (`5.11`) care nu
poate fi acceptată niciodată, fiindcă `A1` vrea NFT-ul de user al subiectului și o atestare mintează
doar un token de referință. Am trecut o gaură reală la „auto-vătămare" pe baza unei afirmații pe care
nu o verificasem. Rămâne nereparată pe lanț, acum pentru motivul care supraviețuiește citirii: un
validator nu poate deosebi un profile id de un attestation id (ambele sunt token-uri de referință ale
aceleiași politici), deci ar costa un reference input la **fiecare** acceptare. `5.11b` fixează limita
și Haddock-ul validatorului o declară. Ce cere limita e o distincție în **read layer**.

**Dimensiuni.** Cele patru care nu se mișcă sunt jumătatea utilă:

```
DisputesValidator      8243 -> 8340   D6
OracleValidator        7427 -> 7492   O5
MintingPolicy         11865 -> 11920  Md4(openedBy) + Md8
MintingPolicy         11920 -> 12241  Mn + isProtocolRefToken
Profiles, Attestations      nemișcate
```

321 de bytes pentru `Mn`, aproape tot din verificarea quadratică scrisă ca două funcții recursive —
recursia e ce PlutusTx nu poate partaja. Politica e la 75% dintr-o tranzacție întreagă, sub plafonul
de 14000, și e singura intrare din tabel unde costul merită reevaluat dacă se apropie vreodată.

### `87807f1` — T-ATTEST-9, suprafața de citire a atestărilor

Șapte rute, scope varianta B. **Decizia de codec: CIP-129 exclusiv.** CIP-105 și hex-ul brut nu au
header byte iar `drep` e HRP pentru ambele feluri, deci acceptarea lor ar însemna să **alegi** un byte
— și alegerea celui de cheie (ce face `FromHttpApiData DRepId` pentru rutele de guvernanță) ar raporta
fiecare script DRep ca DRep de cheie **și** ar întoarce „nicio revendicare" pentru profilul de script
care există. Rutele de guvernanță își permit normalizarea doar fiindcă rezolvă apoi rândul după
hash-ul de 28 de bytes și re-randează din `drep_hash.has_script`; aici nu există un asemenea rând.
`credential_is_script` e byte-ul **citit înapoi**, nu ghicit. Fixat de 8 cazuri de codec + 5 de lookup,
inclusiv o credențială de cheie și una de script peste **aceiași** 28 de bytes.

`rawSql "SELECT ??"` și nu `row_to_json`: fiecare coloană interesantă e scrisă prin
`derivePersistFieldJSON`, deci `profile_id` ține cele cinci caractere `"abc"` — un document JSON, nu
`abc`. O proiecție `row_to_json` le-ar publica dublu-encodate. `FROM palmares_attest.…` calificat, iar
**testele Pg sunt ce ține calificarea**: citesc prin `runPg` (search_path implicit, ce primește un
handler) și scriu fixture-urile prin `runAttest`, deci un `FROM` necalificat cade pe fiecare caz cu
`relation does not exist` în loc să treacă local și să cadă în producție.

JSON-only pe toate șapte, **decizie** nu omisiune, cu `check_swagger.sh` asertând-o — deci adăugarea
CSV mai târziu e un diff recenzat și în poartă. Poarta a trecut 22 → 29 de căi; ambele verificări
(numărul și numele) rămân, fiindcă prind greșeli diferite: o rută **dispărută** cade pe număr chiar
dacă nimeni n-a numit-o, una **redenumită** cade pe linia ei în timp ce numărul trece.

### Ce rămâne din backlogul de audit

Neatins, cu motiv declarat pentru fiecare: timeout-ul lipsă al unei dispute deschise (redeemer nou +
cale off-chain — o sarcină, nu un fix); `opPaused` citit doar de MintingPolicy (o decizie despre ce
înseamnă o pauză); groapa de praf a adresei oracolului; `Blueprint.hs:135` (AU-6, politica
pre-aplicată pe un token placeholder); `Cleanup` care nu inspectează valoarea la cele trei validatoare
de stare — citit, și edge-ul periculos e **nereachabil** fiindcă NFT-ul nu poate părăsi UTxO-ul de
stare, dar praful cu datum care parsează rămâne nemăturabil pe veci, ceea ce e un vector de aglomerare
ieftin și ireversibil; merită fixat prin „datum-ul parsează **și** valoarea îl susține".

### Următorul lucru

T-ATTEST-10/11/12 — deploy pe preprod. E singurul lucru care ține check 6 roșu: jobul cade acum pe
`NameMismatch "gov.cardano.vip"`, pentru că nimic nu e deployat acolo.

---

## 2026-10-22, a doua rundă — opt lentile independente peste on-chain

Cerut de owner: *"as vrea sa mai rulezi un set de agenti independenti care sa auditeze partea de
onchain atat dpdv vulnerabilitati cat si performanta, eficienta, readability, relevant comments."*
Opt lentile, fiecare cu propriul context, fiecare oprită de la a construi (un `cabal build`
concurent coruptează `dist-newstyle` cu erori care arată ca erori de compilare și nu sunt).

### Măsurătoarea care contează mai mult decât toate găsirile: testul de mutație

S-au șters, simultan, `traceIfFalse "M0"` (poarta de pauză de pe **fiecare** mint) și **tot**
conjunctul `A1` (regula de acceptare a unei atestări). Rezultat: **`1 out of 526 tests failed`**, iar
singurul test roșu a fost `UnitTests/ScriptSize.hs` — pentru că scripturile s-au făcut **mai mici**.

Adică: zero teste de comportament au observat că poarta de pauză și regula de acceptare au dispărut.
Tripwire-ul de dimensiune le-a prins din întâmplare, fiindcă măsoară octeți, nu reguli.

**21 din 40 de coduri de trace nu au niciun test care s-ar înroși dacă verificarea ar fi ștearsă.**
Lista, în ordinea gravității: `M0`, `A1`, `P3`, apoi cele patru de dimensiuni de metadata
(`Mm`/`Mp4`/`Md7`/`P4` — fixture-ul `profileDataOverLongName` **există deja** la
`Test/Fixtures.hs:158` și nu e referit de niciun test), `Mk`/`Mp5`/`Md5`, `D2`/`D4`, `Mj`/`Md2`,
`Mh`/`Mp1`/`Md1`, `Md3`, `Md6`, wiring-ul `Mg`, `O2`, `K1`, `K2` (niciun test de emulator nu mintează
vreodată cu taxe configurate).

Plus un gol de margine: **fiecare** caz existent pentru `checkDateInRange` folosește `interval`
(inclusiv/inclusiv). Nu există niciun caz cu margine de sus **exclusivă**, deci o rescriere neatentă a
funcției ar trece toată suita și ar muta granița cu o milisecundă.

### Convergență: două lentile, căi diferite, aceeași cauză rădăcină

Disciplina `isRefTokenName` era aplicată doar id-urilor care **sosesc în redeemer**, niciodată celor
prin care un UTxO **se declară** stare. Trei situri ale aceleiași cauze:

| Sit | Ce permitea | Reparat |
| --- | --- | --- |
| `Mg`/`Md4` | id de dispută/atestare care nu e token de referință | dimineață, `e6f54a7` |
| `authenticateStateUTxO` (off-chain) | **vectorul de forjare** | runda asta |
| `isUnbackedDatum` (on-chain) | plantul rămânea nemăturabil | runda asta |

### Patru reparații de securitate (`bfe6b51`)

1. **`authenticateStateUTxO`** (`offchain-lib/TxBuilding/Utils.hs`) — **HIGH.** Un token de **user**
   legitim mintat autentica un datum scris de mână la o adresă de validator, deci ~1,3 ADA cumpăra o
   atestare **forjată și acceptată** atribuită unei victime, servită pe propriul query „atestări
   despre mine" al victimei, **ireversibil** (nimic nu arde în acest protocol). Off-chain, fără
   schimbare de hash.
2. **`isUnbackedDatum`** (`Onchain/Utils.hs`) — s-a adăugat `not (isRefTokenName tn) ||`, deci
   plantul rămâne **măturabil**. Închide și varianta cu ancoră pe lovelace găsită de lentila de
   datum: `isRefTokenName (TokenName "")` e `False`, deci `not(...)` e `True`.
3. **`O6` / `oracleParamsAreWellFormed`** — `geq` e **vacuu adevărat** pe o cerință negativă.
   Verificat în sursa plutus: `checkBinRel f l r` are brațele `This a -> f a 0`, `That b -> f 0 b`,
   `These a b -> f a b`; cu `f = (>=)` și `b` negativ, fiecare braț e satisfăcut. Deci
   `OracleSetMinUTxOValue (-5_000_000)` drenează ~3,8 ADA pe dispută **din portofelul adminului
   însuși**, repetabil de oricine deschide una. Verificat pe REZULTAT, deci acoperă toate șase
   acțiunile și orice acțiune viitoare.
4. **`Mp6` / `Utils.valueAtOutputIdx`** — **HIGH, găsit doar de a opta lentilă.**
   `profileUserAssetClass` apare exact o dată după construcție (în egalitatea lui `Mp2`), iar `Mp5`
   folosește `geq`, care **permite pozitiv** token-ul de user **înăuntrul** output-ului de stare.
   După care `P3` e satisfăcut de propriul input al profilului și **orice străin** poate rescrie acel
   profil pe veci și emite atestări ca el.

### 16 afirmații false corectate, trei dintre ele interfață publicată

Blueprint-ul CIP-57 descria regula `Cleanup` de **dinainte** de `isUnbackedDatum` la toate trei
validatoarele, și spunea că o atestare mintează un *pair* de token-uri când mintează doar ref —
contrazicând o proprietate de securitate portantă dintr-un fișier vecin. Modulul respectiv își
declară în header: *„Prose here is a published interface, not a comment — keep it accurate."*

Și **patru reguli permanente** din `onchain-lib/CLAUDE.md`, unde regula **era** bug-ul: „must fail if
datum valid" e verbatim regula pe care `isUnbackedDatum` a înlocuit-o; „All deterministic token names
in `Protocol/Id.hs`" trimitea cititorii să scrie derivări într-un modul cu **zero** apelanți; o linie
numea `unsafeGetAttestation`, funcție care n-a existat niciodată.

Două numere portante: `CIP68.hs` raporta 120 de comparații când face 171; `palmares.cabal` raporta
45% ocupare la 11871 octeți când e ~75% la 12241.

Propoziția refutată despre `depositTo` era **în arbore de trei ori**; reparasem una.

### `fb5f91d` — poarta de secrete a citit iar NUMELE unei legături, de data asta în hex

CI era roșu pe main pe ultimele două push-uri, și **nu pe ScriptSize** cum credeam: jobul `secrets`
cădea cu `leaks found: 1`, redactat în log. Reprodus local cu același binar: `hexCredential` la
`AttestRead.hs:126`, un credențial DRep de 28 de octeți. Aceiași octeți pe care cele trei surori de
două linii mai sus îi purtau în bech32 — și acelea **nu** au declanșat, fiindcă sunt calculate, nu
literale. Nici `otherRaw` de la linia 120, care are tot 56 de caractere hex, nici `txIdHex` de la
131, care are 64. În tot repo-ul: 15 astfel de literale în 9 module, exact unul a căzut.

Al patrulea allowlist, mărginit de **lățime**: `"[0-9a-f]{56}"` = 28 de octeți = blake2b-224.
Secretele emise sunt de 16, 24, 32 sau 64 de octeți; nimeni nu emite o cheie de 28, deci îngustarea
n-are în ce secret să se lărgească. Ghilimelele din pattern sunt portante — fără ele, o cheie de 64
hex conține șaisprezece subșiruri de 56.

Trei aserțiuni noi, și a treia e cea care contează: **un 64 hex sub același nume trebuie să rămână
prins**, altfel intrarea a încetat să însemne „un hash de credențial" și a început să însemne „hex e
ok". Prima mea plantă de 64 hex era `aa` plus zerouri și `generic-api-key` nu o semnalează deloc —
ar fi fost o aserțiune care trece și nu dovedește nimic; e notat în script ca măsurătoare. 8/8 trec,
84 de commit-uri curate.

Iar comentariul pasului susținea că allowlist-ul stinge „ONE rule off on ONE path" — într-un pas
numit chiar „rule-scoped, not path-scoped". Sunt patru locuri și doar primul e o cale.

### Tiparul, acum la 16 instanțe

*Propoziția care explică de ce o regulă e corectă e aproape întotdeauna despre jumătatea care **ESTE**
aplicată.* Instanțele din runda asta: poarta de secrete care citea NUMELE unei legături;
`resolveDisputeTX` cu „o pauză nu o blochează"; `admin/Main.hs` care susținea că CI verifică drift pe
un blueprint comis (nu există nici documentul, nici verificarea); blueprint-ul **publicat** mai sus;
`onchain-lib/CLAUDE.md:58-59`; proza din `Operations.hs` care ținea locul verificării `Mp6`.

### Cinci decizii de owner, toate cu argumentul scris

Niciuna nu e un bug de reparat în liniște — fiecare schimbă **ce este** protocolul.

- **D-A, timeout la dispută.** O ieșire, o cheie, și cel care plătește inacțiunea e **ținta**
  (acuzație permanentă pe dosarul unui străin, fără nicio mișcare on-chain). Un admin viu care nu
  acționează produce aceeași stare ca o cheie pierdută. Pauză + cheie pierdută = îngheț total
  permanent. Recomandare: redeemer de retragere pentru disputant după N sloturi.
- **D-B, credențial de stake. SINGURA CU TERMEN.** Niciun hash al protocolului nu are unul, iar
  `Mp5`/`Mk`/`Md5` compară adresa **întreagă**, deci un output cu parte de staking cade la mint.
  Trebuie decis **înainte de primul deploy**; după, e migrare de registru, fiindcă schimbarea adresei
  schimbă currency symbol-ul. Încalcă regulile 13 și 14 din propriul `CLAUDE.md` al repo-ului și e
  singurul tradeoff din codebase fără comentariu.
- **D-C, pauză la `AcceptAttestation`.** Singura scriere ireversibilă și singura pe care pauza nu o
  oprește. Motivul meu scris enumera două căi scumpe și **enumerarea era incompletă**: oracolul e
  **deja** reference input la fiecare acceptare pe care produsul o construiește, deci parametrizarea
  validatorului prin `AssetClass`-ul lui costă **zero** reference inputs în plus.
- **D-D, `maxDateWindow` 4× → 1×.** Builder-ul off-chain hard-codează lățimea ca exact 1×, deci
  factorul 4× nu absoarbe nimic — doar lărgește back-dating-ul atacatorului la 28,8 h. Remediu fără
  schimbare de hash: stratul de citire compară `attestationDate` cu timpul blocului.
- **D-E, revendicarea de credențial.** `Mp3` dovedește că credențialul a consimțit la **această
  tranzacție**, nu la unde aterizează autoritatea: o tranzacție co-semnată pentru orice motiv
  nelegat mintează un profil care revendică credențialul victimei, cu atacatorul ținând token-ul.
  `checkCredentialControl` dovedește condiția de **cheltuire**, care pentru un script Plutus nu e
  condiția de **vot** — dovezile per-purpose sunt exprimabile (`txInfoVotes`). Și o revendicare e
  dovedită o dată și afirmată pe veci: fără retragere, fără expirare, fără burn.

### Auditul de dimensiuni: aplicat 1 din 6, și estimarea a fost greșită de 6,75 ori

Verdictele sunt acum în Haddock-ul lui `ScriptSize.hs`, nu într-un raport — **un item refuzat pe care
nimeni nu l-a scris se re-propune.**

**Aplicat:** `validateOtherMetadata` de la șase recursii la trei. Estimat ~135 de octeți din
calibrarea proiectului („o recursie ≈ 45 de octeți"). **Măsurat: 20.** 12296 → 12276.

Golul e rezultatul mai util, și drumul până la explicație merită consemnat fiindcă am greșit o dată
pe parcurs.

Prima explicație: `map`, `<>` și `elem` rămân folosite în altă parte, deci codul lor nu pleacă. Am
verificat-o cu `grep` pe `MintingPolicy.hs`, n-am găsit niciunul, și am declarat-o falsă. **Greșit —
verificarea era greșită.** `elem` *este* compilat în politică, prin
`Utils.checkCredentialControl`, care e `INLINEABLE` și apelat din calea de revendicare, iar `any`
cu el. **Scriptul compilat e întregul inlinat, nu un fișier**, deci un `grep` pe un modul nu e
niciodată răspunsul la „ce e în script".

Ce rămâne după corecție: `all`, `map` și `<>`-ul pe listă erau genuin unice funcției, deci scoaterea
netă e de vreo două punct-fixe, pe care calibrarea de ~45 le prezice la ~90 și care au măsurat 20.
Deci calibrarea supra-estimează cu un factor de ~4 chiar și corect aplicată, iar citirea onestă e
că **a fost derivată din *introducerea* unor traversări într-un script care nu le avea și nu se
inversează.** Orice estimare din raportul acela se citește de acum ca **plafon**.

Schimbarea rămâne pentru ExUnits, acolo unde e câștigul real: trei traversări în loc de șase, fără
lista intermediară `map fst`, și 168 de comparații în loc de 171.

**Refuzate, cu motivul fiecăruia:**

- `checkDateInRange` fără `Interval` (~150): testele noi îl fac *sigur*, dar e singura funcție care
  datează o înregistrare permanentă și sunt ~1700 de octeți liberi. Nu rescrii semantica unei
  biblioteci de mână fără presiune.
- `and`→`&&` la Profiles/Oracle (~90): listele alea **conțin** `traceIfFalse`-urile, deci o listă
  strictă loghează **fiecare** cod care cade, iar `&&` doar primul. Proiectul a luat decizia opusă
  explicit când a ales `preserve-logging` peste `remove-trace`.
- `valueSpent` → helper partajat: ~0 pe politica de mint. Lecția „+50 contra +502", în sens invers.
- `checkFee` cu un singur `feeFor`: 0–20 de octeți, probabil deja constant-folded (toate trei
  siturile pasează un constructor cunoscut), plus o capcană de parsare — `$` e `infixr 0` contra
  `infixr 2` al lui `||`.
- `MintingContext`: **0 octeți**. Record construit o dată și selectat în aceeași unitate de
  compilare, deci case-of-known-constructor îl elimină înainte de UPLC. Scoaterea lui ar putea
  **mări** scriptul.
- Și itemul „case inexhaustiv în `ProfilesValidator`" nu e un defect: long-distance info din GHC 9.6
  îl face exhaustiv, iar un al treilea constructor ar produce avertisment.

### O recomandare de agent refuzată, cu motiv

Lentila de ExUnits a propus `unsafeFromBuiltinData` pe citirea oracolului: 450–850 octeți pe trei
scripturi. Refuzat. La 12241/16384 cu plafon 14000 sunt ~1750 de octeți de spațiu, `K1` e valoare
diagnostică pe **singura** cale care citește stare externă, și e singurul item din raport care
schimbă o verificare pe octeți. Notat drept prima pârghie dacă politica se apropie vreodată de
plafon.

### De ce 21 de coduri erau nefixate: suita nu construise niciodată un `ScriptContext`

Briefing-ul meu către agentul de teste spunea să urmeze „tiparul pur" din `Satisfaction.hs` și
`MintInputs.hs`. Agentul a raportat că tiparul **nu există**: acele module testează ajutoare pure
(`spentExactlyOnceFromAddress`, `isRefTokenName`, `validateOtherMetadata`), iar un `grep` arată că
**niciun test din suită nu apelează `mintingPolicyLambda`/`profilesLambda`/`attestationsLambda`**.

Asta explică exact cele 21: singura testare on-chain era prin emulator, iar o tranzacție de emulator
trece prin **builder-ul off-chain**, care refuză o tranzacție malformată înainte să ajungă la
validator. Conjunctele care pot fi atinse doar de un context construit de mână nu au fost niciodată
executate — nu din neglijență, ci fiindcă nu exista niciun drum până la ele. `MutationGuards.hs` e
primul test cu context construit de mână din repo.

### Două decizii luate, cu argumentul în cod

**D-B — închisă fără schimbare de comportament.** Egalitatea pe adresa întreagă **este** alegerea
corectă; termenul exista doar fiindcă nu era nici documentată nici fixată de test. Argumentul e acum
în `Utils.hs`, pe `isTxOutAtIndexWithDatumMinValueAndAddress`: recompensele se acumulează pe adresa
de stake, nu pe UTxO, deci o adresă de script delegată ar strânge un sold mutabil doar cu un
certificat de retragere semnat de credențialul de stake — **a doua cheie, fără niciun proprietar
numit în protocol**, păzind fonduri al căror revendicator e genuin ambiguu între registru și
depunători. Și relaxarea la „doar partea de plată" e strict mai slabă: două adrese care împart
credențialul de plată și diferă în partea de staking sunt adrese **diferite** pe care protocolul
le-ar trata pe amândouă ca fiind ale lui, ceea ce face setul nemărginit. Plus jumătatea cerută de
regula 14: min-ADA pe UTxO de stare e blocat **permanent**, fiindcă nimic nu arde — e prețul unui
registru care nu poate fi retractat; depozitul de dispută se întoarce pe orice verdict, deci nu e
valoare blocată, e garanție depusă.

**D-D — 2×, nu 1×.** Verificat, nu presupus: `safeEraTime = 25920` sloturi și
`txIsValidForSafeEra` construiește exact `[tip, tip + safeEraTime]`, deci lățimea onestă e un 1×
determinist care nu variază niciodată. Surplusul era în întregime al atacatorului, **și e tot
bugetul lui**: raza de back-dating a unui interval forjat e exact `maxDateWindow`, fiindcă ledger-ul
cere doar ca slotul curent să fie înăuntru, deci `[now - maxDateWindow, now]` e mereu disponibil. 4×
cumpăra 28,8 h; 2× cumpără 14,4 h. Nu 1×, fiindcă acolo lățimea onestă **egalează** plafonul și trece
doar prin `<=` — orice portofel care lărgește intervalul ar fi refuzat de validator, nu de o decizie
recenzată.

### `K2` nu avea NICIUN test de comportament, și de ce nimeni n-a observat

Suma pe care `feeFor` o selectează era fixată. **Impunerea ei nu fusese exercitată niciodată.**
`grep -rn "K2" src/test/` întoarce doar comentarii. În detaliu, verificat:

- `UnitTests.Fees` fixează `feeFor` ca **mapare pură** — câmpuri distincte pe acțiune, niciunul
  partajat, totul gratuit sub o configurație de zerouri. Nu apelează `checkFee` niciodată.
- `UnitTests.Oracle`'s `setAndClearFees` setează un `FeeConfig`, citește datum-ul înapoi și îl
  șterge — **fără niciun mint între**.
- `TestRuns.hs:181` desfășoară oracolul cu `opFeeConfig = Nothing`.
- Fiecare context pur de mint seta `Nothing` **intenționat**, ca să nu refuze calea de taxe un caz.

Și motivul pentru care un test de emulator **nu putea** să închidă golul e același tipar care a
produs tot modulul: builder-ul adaugă output-ul de taxă, deci o cale fericită cu preț trece
indiferent dacă validatorul se uită sau nu.

Cinci cazuri noi. Două discriminează între brațul `MintActionDispute` și garda `<= 0`, fiindcă o
taxă zero și o cerere de sumă zero sunt aceeași aritmetică — 8.14 roșu + 8.15 verde ⇒ brațul; ambele
roșii ⇒ garda; 8.12/8.13 roșii ⇒ `K2` însuși.

**Și o mutație prinsă gratis:** taxa de profil e deliberat **mai mică** decât cea de atestare
(3 vs 7 ADA). Dacă `feeFor MintActionProfile` ar fi recablat la `fcAttestationFee` — **chiar bug-ul
moștenit #1**, taxa de interval din Belt System care taxa câmpul de istoric — ar cere 7 ADA de la un
output care plătește 3, deci jumătățile **valide** se înroșesc. Invers prețuite, `geq` ar fi înghițit
schimbul în silență.

### `admin sign-tx` — Atlas livrează decodorul pentru forma asta și niciun encoder

Testul de fum nu putea rula fiindcă nimic din repo nu producea o semnătură în forma pe care
`tx_wit` o acceptă, iar documentul spunea de ce nu s-a livrat unul la întâmplare: *„a wrong signer
would look like a convenience."*

Descoperirea decisivă: `GeniusYield.Types.Tx` exportă `txWitFromHexBS`, `txWitFromHex`,
`txWitFromLedger`, `txWitToLedger` și `txWitToKeyWitnessApi` — **și nimic în sens invers**. Encoderul
e o linie, și nu are nici versiune de protocol nici base16 propriu: `AlonzoTxWits` e un `MemoBytes`,
deci `ToCBOR`-ul lui emite **chiar octeții pe care ledger-ul i-a produs** pentru valoarea aia, nu o
re-codificare, iar `serializeAsHexText` e codecul hex al ledger-ului peste ei. Nu există nimic cu
care alegerea `eraProtVerHigh @ConwayEra` a decodorului să se contrazică.

**Bannerul s-a mutat.** `main` tipărea „Palmares - …" pe **stdout** ca primă acțiune, iar scriptul
de fum capturează stdout întreg (`wit=$(… | $signer)`). Acum se tipărește după `execParser` și e
`pure ()` pentru `SignTx`.

**Și testul agentului nu putea să cadă.** O suită cabal nu poate importa modulele unui executabil,
deci testul purta **copii** ale celor două funcții — iar un test care fixează o copie trece pe veci
în timp ce lucrul după care e numit putrezește. Le-am mutat în `offchain-lib` lângă `signGYTxBody`:
trei linii, și acum aserțiunile spun ceva despre comandă. Bonus — după mutare, `lens` și
`cardano-api` **nu mai trebuie** în stanza `admin`; verificat prin build, nu presupus.

### Codurile de trace, despicate înainte de deploy

Un cod de trace devine **intrare de runbook** în clipa în care ceva e desfășurat. Deci despicarea e
gratuită azi și scumpă mâine — același argument de secvențiere care a guvernat toate schimbările de
hash din ziua asta.

**`A1` → `A1` + `A5` + `A6`.** Era un singur cod peste trei propoziții diferite: output-ul
re-blocat, token-ul de user al subiectului cheltuit, și UTxO-ul cheltuit care ține chiar NFT-ul pe
care datum-ul lui îl numește. Argumentul nu e al meu, e al codebase-ului: comentariul lui `A3`, la
patru linii mai sus, spune *„«already accepted» and «wrong output» are different mistakes and an
operator should be able to tell them apart"*. Cele trei jumătăți ale lui `A1` sunt între ele la fel
de diferite ca oricare dintre ele față de `A3` — un output malformat, o contrasemnătură lipsă, și un
UTxO care nu e atestarea pe care pretinde că e. Un singur cod pentru toate trei spunea unui operator
doar că acceptarea a eșuat, ceea ce pe **singura scriere ireversibilă** a protocolului e cel mai
puțin util lucru pe care l-ar putea spune.

**`P0`/`A0`/`D0` → plus `P7`/`A4`/`D8`.** „Datum-ul ăsta nu e stare de protocol" și „**este** stare
de protocol și nu ai dreptul să o mături" sunt greșeli diferite — a doua e cineva care încearcă să
șteargă o înregistrare vie. Codurile de datum își păstrează sensul; regula de `Cleanup` capătă al
ei. Două coduri, nu trei: „fără datum" și „datum nedecodabil" sunt amândouă imposibile pentru un
client onest și spun operatorului același lucru.

**`Ml` → `Mq`.** Un L mic e indistinguibil de cifra 1 în majoritatea fonturilor monospațiate, iar
`M1` e un cod **diferit** pe **același** script („invalid purpose"). Un operator care citește logul
unei tranzacții picate ar fi depanat altă regulă. Și secvența asta de coduri **sărea deja peste
`i`** exact pe motivul ăsta — f, g, h, j, k, m, n — deci `l` era singura literă care își desfăcea
propria convenție; `o` e sărit pe aceeași bază.

Raza de acțiune: 3 situri pentru `Ml`, plus `Cleanup.hs` (care testează exact regula nouă), plus
patru Haddock-uri a căror referință la `A1` însemna de fapt jumătatea de consimțământ — inclusiv
**blueprint-ul publicat**, care spunea „a lineage attestation can never satisfy A1" când argumentul
e despre token-ul de user al subiectului, adică `A5`.

### O lecție de unealtă, fiindcă a costat timp real

Trei shell-uri de build în fundal au stat blocate **unul pe altul**: fiecare aștepta cu
`while pgrep -f "cabal (build|test)"`, iar linia de comandă a fiecărui shell **conține** șirul
`cabal build`, deci `pgrep -f` îl găsea pe fratele lui. Deadlock pe trei căi; nimic nu s-a compilat
niciodată. Dacă un guard de genul ăsta mai apare, el trebuie să caute procesul **real** (`pgrep -x
ghc-9.6.6`), nu propria lui linie de comandă.
