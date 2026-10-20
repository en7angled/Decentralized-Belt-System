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
- [ ] **P7b. Blocul 5 — API-ul public** (T-API-1…18). **ÎN CURS**: 1…10 gata (`044be81`),
      142 de teste + 92 de asserțiuni de smoke verzi pe db-sync-ul nostru. Rutele servite acum:
      `/health`, `/ready`, `/status`, `/metrics/formulas`, `/dreps` (JSON + CSV),
      `/drep/{id}` ≡ `/pages/drep/{id}`, `/drep/{id}/power`, `/drep/{id}/delegators` (JSON + CSV),
      `/drep/{id}/updates`.
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
