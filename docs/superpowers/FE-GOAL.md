# GOAL — frontend-ul palmares (F1 checks 13–16)

> **Contractul rulării autonome.** Acest fișier e autoritar peste orice amintire. Se citește întreg
> la fiecare reluare, înainte de orice altceva.

**Textul ownerului (2026-10-25):** „*ok mergem pe halogen !*" și „*ține cont că poți să evoluezi
repo-urile mele atunci când consideri necesar. ! vreau să mă ajuți să scriem un goal pentru partea
de front-end a.î. să nu te oprești până nu e gata !*"

**Documentul de design pe care îl execută:**
`docs/superpowers/specs/2026-10-25-palmares-ui-design.md`. Acolo sunt deciziile; aici e doar ordinea,
autorizarea și definiția lui „gata".

---

## 1. Ce înseamnă „gata" — criterii măsurabile, nu impresii

Două porți. **M1 e cea pe care ownerul a cerut-o explicit** (*„în prima fază aș vrea să văd
aplicația live pe preprod"*). M2 închide checks 13–15.

### Poarta M1 — explorer public, live pe preprod

**M1 se livrează în două felii** (§6 din design): **M1a** = cele zece ecrane de guvernanță, și e
poarta de mai jos; **M1b** = celelalte opt plus graful Cytoscape și widget-ul embed.

| # | Criteriu | Cum se verifică |
| --- | --- | --- |
| M1-1 | Un URL public servește explorer-ul — **condiționat de §9.1 din design** | `curl -sI <url>` → 200, și un om care nu are repo-ul îl deschide. **`palmares` nu există în `apps-k8s-configs`**, deci asta poate cere un bloc de platformă care nu e muncă de frontend. Până la răspunsul ownerului, criteriul care ține e: stiva pornește de la zero cu o comandă, local, peste db-sync-ul sincronizat. |
| M1-2 | Toate ecranele de citire randează date reale de preprod | 294 de DRep-i în listă, 131 de acțiuni, dashboard cu epoca curentă |
| M1-3 | Chip-ul de status arată prospețimea reală, nu una hardcodată | Se oprește attest-sync → chip-ul trece pe ambar/roșu |
| M1-4 | Fiecare metrică afișată are „ⓘ" care rezolvă la formula publicată | Toate cele 8 `formula_id` din `/metrics/formulas` apar în UI |
| M1-5 | Widget-ul de verificare răspunde la o credențială CIP-129 lipită | Cele trei cazuri (`total` 0 / 1 / >1) randate distinct |
| M1-6 | E2E verde pe bundle-ul de mock, fără wallet/backend/lanț | `npm run build && npx playwright test` |
| M1-7 | Zero erori în consolă pe fiecare rută | Playwright asertează `console.error` gol |
| M1-8 | Imaginea se construiește multi-arch și pornește | `docker buildx build --platform linux/amd64,linux/arm64` + `/health` → 200 |

### Poarta M2 — fluxurile de scriere

| # | Criteriu | Cum se verifică |
| --- | --- | --- |
| M2-1 | O atestare emisă **și contrasemnată** din browser, pe preprod | Tx-ul apare pe `/attestations` cu `accepted` |
| M2-2 | Poarta de oracol blochează fluxul când trebuie | `oracle_paused === null` → flux blocat, mesaj explicit |
| M2-3 | Fără magic de rețea confirmat, fluxul nu pornește (R6) | Wallet fără CIP-142 → refuz, nu „merge și vedem" |
| M2-4 | Matricea de compatibilitate de wallet e vizibilă în UI | Lace conectat → spune ce nu poate face, **înainte** de semnătură |
| M2-5 | Cele 9 fluxuri din §2.20 există, inclusiv refuzurile documentate | `/claim-credential` cu două wallet-uri → mesaj, nu eroare |
| M2-6 | Check 16: deploy reproductibil BE+FE+BFF | O comandă, de la zero, documentată |

**Nu e „gata" când:** build-ul trece dar un ecran e gol; un ecran randează dar numărul n-are „ⓘ";
e2e e verde dar doar pe mock și nicio rută n-a fost lovită real; imaginea se construiește dar nimeni
din afară nu poate deschide URL-ul.

---

## 2. Ordinea — șapte blocuri, fiecare cu propriul test de ieșire

Se merge în ordine. Un bloc blocat nu oprește run-ul: se trece la următorul neblocat (§5).

### Bloc 0 — schelă și igienă de licență
Copiem `/Users/mg/Projects/CardanoProjects/cardano-dapp-frontend-template` → `palmares/frontend/` și
aplicăm **literal** cei șase pași din §„Customizing for your dApp" al README-ului lui. `bff/config.js`
capătă două servicii (`query`, `interaction`) pe prefixele `/api/query` și `/api/interaction`, și
trece pe `requireEnv`. Job de CI condiționat pe `frontend/**`.
**Plus, în același bloc** (§7 din design): `LICENSE` Apache-2.0 + câmp `license` în template, în
`frontend/` și în cele șase fork-uri — **niciunul nu are azi** — intrare în `NOTICE`-ul palmares, și
`halogen-router` mutat de pe `ref: main` pe un SHA.
**Ieșire:** `npm run dev` servește bundle-ul demo; `npm run build` verde; `cabal build all` **nu** se
declanșează pe un commit care atinge doar `frontend/**`; `grep -L` pe fork-uri nu găsește niciunul
fără LICENSE.

### Bloc 1 — stratul de transport
Capabilitatea `MonadQueryGov` în pattern-ul de patru fișiere al template-ului
(`MonadQueryApi`/`Types`/`Default`/`Mock`), peste cele 29 de rute `GET`. Contractul de eroare
(`{code,message}`, 24 de coduri, corp citit ca String, JSON doar pe 2xx), headerele de prospețime în
Store, regulile de paginare din §4 al designului.
**Ieșire:** interpretul `Mock` servește forma fiecărui ecran; un test de integrare per familie de
rute; o aserțiune care prinde filtrul cu typo prin `total`.

### Bloc 2 — ecranele M1a
Fiecare ecran prin `.cursor/skills/add-explorer-page`. Ordinea: chip de status → hub → dashboard →
listă DRep-i → detaliu DRep → acțiuni (listă + detaliu) → voturi → căutare → metodologie. Cele trei
grafice SVG se copiază din `bjj-frontend/src/Components/Chart/`.
**Ieșire:** M1-2, M1-4, M1-5, M1-7 pe cele zece ecrane.

### Bloc 3 — deploy M1a ← **aici ownerul vede aplicația live**
Nume de imagine, buildx multi-arch. **Scris pentru varianta (b) din §9.1 al designului** — manifeste
prin `apps-k8s-configs` (git → commit → push → Flux, **niciodată** `kubectl apply`), rute Gateway
API, `Certificate`, secrete SOPS. **Dacă ownerul răspunde (a)**, blocul se restrânge la „imaginea se
construiește multi-arch, pornește, și `/health` răspunde 200", iar deploy-ul public rămâne bloc
separat. Fără `CORS_ALLOWED_ORIGINS`: browserul vorbește same-origin prin BFF.
**Ieșire:** M1-3, M1-8, plus M1-1 în varianta (b). **Run-ul raportează ownerului aici, și continuă.**

### Bloc 3b — ecranele M1b
Restul: SPO, comitet, trezorerie, delegări, activitate, profiluri, atestări, dispute. Plus **graful
de delegări/atestări** (check 13 — Cytoscape prin `LineageGraph/FFI` + `LineageGraphRuntime`, singura
dependență nouă pe care designul o justifică) și **widget-ul embed ca `iframe`** de pe originea
noastră (check 15 — un script tag pe origini terțe e imposibil împotriva allowlist-ului fail-closed).
**Ieșire:** M1-2, M1-4, M1-7 pe tot setul; checks 13 și 15 acoperite.

### Bloc 4 — CIP-95 + CIP-142 în fork-urile ownerului
`git clone` pentru `purescript-cip30`, `purescript-cip30-typesafe`,
`purescript-cardano-serialization-lib` (nu sunt local). FFI pentru namespace-ul `cip95`
(`getPubDRepKey`, `getRegisteredPubStakeKeys`, `getUnregisteredPubStakeKeys`, `signData`, `signTx`),
`getNetworkMagic` (CIP-142), și scoaterea lui `[{cip: 30}]` hardcodat din
`purescript-cardano-capabilities` `MonadCIP30.purs:150`.
**Ieșire:** detecția pe `api.cip95` merge în toate trei formele; refuzul pe magic nedeterminat se
declanșează; `cip30-test-wallet` (Apache-2.0, preprod, Playwright) legat în CI.

### Bloc 5 — fluxurile de scriere M2
Fiecare prin `.cursor/skills/add-interaction`. Ordinea: profil (creare, editare) → atestare (emitere,
acceptare) → dispută (deschidere, expirare) → credențială (revendicare, retragere) → rezolvare.
**Ieșire:** M2-1…M2-5.

### Bloc 6 — regulile de randare și livrarea
Cele șapte reguli R1–R7 din §8 al designului, fiecare cu un test. Accesibilitate de bază. Check 16.
**Ieșire:** M2-6, și fiecare R are un test care pică dacă regula se rupe.

---

## 3. Pre-autorizat — nu se întreabă

- **Commit-uri și push pe `main`** în `palmares` și în `Decentralized-Belt-System`.
- **Evoluarea repo-urilor ownerului** (autorizare permanentă, 2026-10-25): clone, branch, commit și
  push în `purescript-cip30`, `purescript-cip30-typesafe`, `purescript-cardano-serialization-lib`,
  `purescript-cardano-capabilities`, `purescript-halogen-daisyui`,
  `halogen-cardano-wallet-connect-component`. Un commit care schimbă un `ref` de pachet în
  `spago.yaml` spune în mesaj de ce.
- Build-uri, teste, formatare (`purs-tidy`), `npm install` / `spago install`, pornirea serverului de
  dev și a BFF-ului local.
- Decizii de implementare, luate și documentate ca `(call)` în §4 de mai jos sau într-un ADR.
- Editări de documentație în ambele repo-uri.
- `curl` read-only împotriva serviciilor locale (8082, 8083, 5173).

## 4. NU e pre-autorizat — se predă ownerului și se continuă

- **Submiterea oricărei tranzacții.** Cheltuie fonduri. Rămâne manual pe bună dreptate.
- **Push de imagine pe GHCR și push de manifeste care șterg resurse** (`prune: true`). Prima oară se
  cere confirmare; după aprobare explicită, în cadrul aceluiași run, nu se re-cere.
- **Mainnet.** Parcat. Nimic, în nicio formă.
- **Rotația tokenului Maestro.** Decizie închisă de owner (2026-10-22). Nu se re-deschide.
- **Metrici noi** (inclusiv alinierea cu rezultatul ratificat). E backend; ownerul decide.
- **`docker compose down -v` în `experiments/dbsync-preprod/`.** Volumele alea sunt singura bază
  db-sync preprod sincronizată. Niciodată.

---

## 5. Protocolul de blocaj — niciodată „aștept răspuns"

Ce oprește rulările nu e autorizarea lipsă, e **clasificatorul de permisiuni al Claude Code**, care
refuză indiferent ce spune un goal. Documentat pe run-ul precedent: a refuzat pornirea unui
container, un script de releu din stdlib, `nc -z`, `python3 -c` cu un socket — și, după un șir de
refuzuri, chiar un `grep` simplu și un `Edit` pe `README.md`. **Devine mai sensibil după refuzuri
consecutive, deci reîncercarea în altă formă nu e răspunsul și citește ca eludare.**

Când se întâmplă: se scrie în §7 un rând cu *ce lipsește / ce s-a încercat / ce deblochează*, se
predă ownerului comanda exactă ca linie `! …`, și **se trece la următorul lucru**. Ordinea când firul
principal e blocat: (a) următorul bloc neblocat; (b) review pe codul deja scris; (c) extindere de
teste; (d) actualizare de documentație. În ordinea asta.

**Regulile de permisiuni.** **ADĂUGATE 2026-10-25**, cu autorizarea explicită a ownerului
(*„îți dau voie să actualizezi ~/.claude/settings.json cu permisiunile de care ai nevoie"*).
21 de intrări noi în `permissions.allow` (524 → 545; backup la
`~/.claude/settings.json.bak-20261025`):

```json
"Bash(npm run:*)", "Bash(npx vite:*)", "Bash(npx playwright:*)", "Bash(npx purs-tidy:*)",
"Bash(node bff/server.js)", "Bash(node --watch bff/server.js)",
"Bash(git clone https://github.com/mariusgeorgescu/:*)",
"Bash(git -C …/purescript-cip30 push:*)", "Bash(git -C …/purescript-cip30-typesafe push:*)",
"Bash(git -C …/purescript-cardano-capabilities push:*)",
"Bash(curl -s http://{localhost,127.0.0.1}:{8082,8083,5173}/:*)",
"Read(//Users/mg/Projects/CardanoProjects/**)",
"Edit(//Users/mg/Projects/CardanoProjects/palmares/frontend/**)",
"Edit(//Users/mg/Projects/CardanoProjects/purescript-cip30{,-typesafe}/**)",
"Edit(//Users/mg/Projects/CardanoProjects/purescript-cardano-capabilities/**)"
```

Erau deja acolo: `npx spago:*`, `npm install *`, `npm ci *`, `npm test *`, `docker build *`,
`docker buildx *`, `git add *`, `git commit -m ' *`, `git checkout *`, `git branch *`.

Două lucruri lăsate intenționat în afară, amândouă sub §4: **`docker push` pe GHCR** și **`git push`
pe `palmares` / `Decentralized-Belt-System`** — al doilea pentru că fiecare push pornește o rulare
de CI de ~25 de minute și anulează precedenta, deci se face în loturi, nu per commit. Push-ul e
permis doar pe cele trei fork-uri unde blocul 4 schimbă efectiv cod.

**Dacă clasificatorul refuză totuși o comandă acoperită de regulile de mai sus**, regulile n-au fost
reîncărcate: ownerul deschide o dată `/hooks` sau repornește sesiunea. Nu se reîncearcă comanda în
altă formă.

---

## 6. Reguli nenegociabile

1. **Fiecare commit lasă un build curat, zero warning-uri noi și teste verzi.** Nu se comite „merge
   local, repar pe urmă".
2. **Nicio dependență npm nouă** fără un rând scris care spune ce rezolvă și de ce nu merge cu ce e
   deja instalat. Mesh și `@utxos/sdk` sunt interzise prin design (§2 și §3 anti-goals).
3. **Regulile din `apps-k8s-configs/CLAUDE.md`** nu sunt negociabile: tot prin git → Flux, niciodată
   `kubectl apply/edit`; SOPS+age pentru secrete; Gateway API, nu Ingress; docs în română.
4. **Niciun client generat din swagger.** Codecurile se scriu de mână din §3 și §4 ale screen-map.
5. **Niciun fișier gitignored nu ajunge în git:** `.env`, `config/config_atlas.json`, `operation.prv`,
   `*.skey`, `*.vkey`. Doar `*.example`.
6. **Disciplină de bază de date:** doar `SELECT`/`EXPLAIN` read-only, un statement pe rând, niciodată
   ridicat `work_mem` (un query paralel a epuizat odată `/dev/shm` de 256 MB și a căzut postmaster-ul).
7. **Testele palmares au nevoie de** `PALMARES_TEST_PG_CONN_STR="host=127.0.0.1 user=postgres password=devonly dbname=postgres port=55439"`
   pasat explicit — fără el ~88 de teste pică fals în ~17 s și arată ca un build rupt.
8. **Un singur `cabal build` la un moment dat** într-un `dist-newstyle`. Build-urile concurente se
   blochează reciproc.
9. **Română corectă** în tot ce e vizibil pentru owner și în mesajele de commit.

---

## 7. BLOCKED FOR USER

Nimic nu blochează blocurile 0–2. Două întrebări sunt deschise și **niciuna nu oprește run-ul** —
ambele au o presupunere scrisă pe care run-ul o urmează până la un răspuns.

| Ce lipsește | Ce s-a încercat | Ce deblochează |
| --- | --- | --- |
| **Cine face deploymentul public** (design §9.1). `palmares` nu apare nicăieri în `apps-k8s-configs`; `F1-PROGRESS` ține manifestele nepublicate deliberat, condiționat de cifrele de RAM din Experimentul A (PVC-ul se dimensionează o dată, `allowVolumeExpansion: false`). | Grep pe tot `apps-k8s-configs`: zero potriviri. Verificat în ledger de ce. | Blocul 3 — varianta (b) cu platformă, sau (a) doar imaginea. **Presupunere până la răspuns: (a)**, fiindcă e reversibilă. |
| **Ce era „varianta B"** pentru scope-ul UI (design §9.2). `F1-PROGRESS.md:3777` o numește; nu e definită în niciunul din repo-uri. | Grep după „varianta A/B/C" în ambele repo-uri: singurul alt hit e un scope de rute, nealăturat. | Scope-ul M1. **Presupunere până la răspuns:** documentul de design o înlocuiește. |
| **Regulile de permisiuni din §5** — numai ownerul le poate adăuga. | — | Tot blocul 0 înainte, dacă clasificatorul refuză `npm install`. |

---

## 8. Decizii luate ca builder calls (contestabile de owner)

*(gol — se completează pe măsură ce run-ul decide)*

---

## 9. Jurnal de execuție

*(gol — fiecare bloc își scrie aici ce a livrat, ce a găsit și ce a greșit, ca în
`F1-PROGRESS.md`)*
