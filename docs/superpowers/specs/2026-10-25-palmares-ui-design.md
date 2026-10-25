# Design — interfața Palmares (F1, preprod)

*Status: DRAFT spre aprobare. Data: 2026-10-25. Acoperă checks 13–15 din §9 a specificației
(`2026-10-19-gov-registry-spec.md`) — adică tot UI-ul — și pregătește check 16 (deploy reproductibil
BE+FE+BFF). Deciziile marcate „(call)" sunt ale builderului și pot fi contestate de owner.*

**Deciziile ownerului care au produs acest document:**

1. *„În prima fază aș vrea să văd aplicația live pe preprod"* (2026-10-25) — ținta e un deploy
   public funcțional, nu completitudine de ecrane.
2. *„Hai să ne ocupăm strict de UI"* (2026-10-25) — mainnet (F2) e parcat; nu se adaugă rute de
   backend, nu se schimbă validatori, nu se adaugă metrici.
3. *„Ok mergem pe Halogen!"* (2026-10-25) — PureScript + Halogen, nu React. Decizia a fost luată
   după cinci analize independente; §2 rezumă de ce, pentru că justificarea scrisă în
   specificație (§4.5 din `protocol.md`) e factual greșită și nu trebuie să mai fie citată.

---

## 1. Obiectiv și de ce acum

**Problema.** Backendul e complet și rulează pe preprod: 29 de rute `GET` pe query-api, 15 pe
interaction-api, 2 pe attest-sync, 796 de teste verzi, protocolul deployat și verificat pe lanț
(`61e280b`, 2026-10-24). Nu există niciun pixel. Produsul e, pentru oricine din afară, inexistent.

**Ce construim.** Un explorer public de track-record de guvernanță peste API-ul existent, plus
fluxurile de scriere care cer un wallet. Nicio linie de backend nou.

**De ce acum.** Două motive, în ordine:

- `docs/usecases/api-screen-map.md` (1833 de linii) a fost scris măsurând serviciul care rulează, nu
  citind codul. Șapte blocante din nouă s-au închis pe 24 octombrie. Contractul e cunoscut cu o
  precizie care nu durează: la următoarea rescriere de query se pierde.
- Datele de guvernanță sunt derivate din db-sync de la genesis — singura poveste de corectitudine
  care ne diferențiază (issue #1975: două instanțe identice de db-sync au raportat puteri de vot
  care diferă de ~740×, iar Known Issue-ul din 13.6.0.4 numește preprod explicit). O poveste de
  corectitudine care nu se vede nu convinge pe nimeni.

---

## 2. De ce Halogen — și de ce specificația greșește

Specificația spune „React + Mesh/CIP-95". Argumentul scris pentru ea, la `palmares/docs/spec/protocol.md:275-278`,
e: *„Mesh gives CIP-95 for free, including the CIP-95 part we would otherwise write ourselves."*
**Această propoziție e falsă** și nu mai trebuie citată de nimeni:

- `mlabs-haskell/purescript-cip95` v1.0.0 e în `purescript-cardano-package-set` și e numit în lista
  Path-to-Active a CIP-95 însuși. (Nu îl folosim oricum — vezi mai jos. Are și o problemă reală:
  nu are fișier LICENSE.)
- Partea de FFI pe care „am scrie-o noi" are **19 linii**.
- Mesh ar *adăuga* ~9 MB pentru o capabilitate pe care backendul o are deja, și ar lupta cu
  contractul de wire: bech32-ifică adresele pe care API-ul le vrea hex brut (§4.2 din screen-map).
- **Niciun** tool de guvernanță din teren nu folosește un SDK de wallet pentru CIP-95. GovTool
  are ~30 de linii scrise de mână peste `window.cardano`. Cardano Foundation rulează în producție
  un tool de guvernanță cu frontend în **Elm**.

Argumentul real pentru React e cel social, de la §4.5: *„the project is for the community, and the
contributor pool is React/TS."* E un argument valid și singurul care ar fi putut răsturna decizia.
Ownerul l-a cântărit și a decis Halogen.

**Ce câștigăm concret** (nu teorie — cod care există, inventariat în §5): fluxul build→sign→submit
vorbește deja formatul de wire al palmares câmp cu câmp; BFF-ul rezolvă deja problema dialogului
nativ de credențiale; iar `cip30`, `cip30-typesafe`, `cardano-serialization-lib` și
`cardano-capabilities` sunt **fork-uri ale ownerului**, deci CIP-95 și CIP-142 sunt commit-uri în
repo-uri proprii, nu o decizie de dependență.

**Ce pierdem, numit cinstit:** nu există set de componente de guvernanță în PureScript
(`@intersect.mbo/govtool-outcomes-pillar-ui`, ISC, e React-only); Halogen a avut un release în patru
ani; calitatea output-ului de agent scade cu masa corpusului; toate graficele se scriu de mână în
SVG (vezi §6).

---

## 3. Goals, non-goals, anti-goals

### Goals

| # | Goal | Verificabil prin |
| --- | --- | --- |
| G1 | Explorer read-only live pe preprod, accesibil public la un URL | Un străin deschide URL-ul și vede 294 de DRep-i reali |
| G2 | Fiecare metrică afișată are un „ⓘ" care rezolvă la formula publicată | Fiecare `formula_id` din `/metrics/formulas` apare în UI |
| G3 | Prospețimea datelor e vizibilă pe fiecare ecran, și e cinstită | Chip de status verde/ambar/roșu din `/status` + headerele `X-*` |
| G4 | Widget-ul de verificare răspunde la o credențială CIP-129 lipită | `total` 0/1/>1 randat cu cele trei înțelesuri distincte |
| G5 | Fluxurile de scriere funcționează end-to-end cu un wallet real pe preprod | O atestare emisă și contrasemnată din browser |
| G6 | Deploy reproductibil: o comandă, BE+FE+BFF | Check 16 din §9 a specificației |

### Non-goals (explicit în afara scopului, nu uitate)

- **Mainnet.** Parcat prin decizia ownerului. Niciun cod condiționat pe rețea dincolo de refuzul de
  §8-R6.
- **Metrica de aliniere cu rezultatul ratificat.** E o metrică de backend (SQL + `docs/spec/metrics.md`),
  nu un ecran — deci cade direct sub *„strict UI"*. Catalogul rămâne la cele 8 metrici existente.
  **(call)** Întrebarea dacă alinierea devine metrică de prim rang rămâne deschisă și e a ownerului;
  acest document rezervă un slot de coloană în tabelul de DRep-i ca să nu fie nevoie de un redesign.
- **Rute noi de backend.** Cele trei lipsuri din §1.4 din screen-map (profil după adresă de wallet,
  dispută după id, credențială de comitet la citire) s-au închis deja pe 24 octombrie. Ce rămâne
  deschis (`/votes/{id}`, istoric de tranzacții pe profil) nu blochează niciun ecran.
- **Fluxul de semnare în doi pași pentru `/claim-credential`.** §5.8: nimic nu stochează o tranzacție
  parțial semnată. Suportăm cazul „ambele chei în același wallet" și **explicăm** celălalt caz; nu
  construim acumulare de martori.
- **Wallet-uri mobile și Trezor.** Trezor nu poate fi DRep deloc (firmware-ul nu are schemă de rol 3).
  Yoroi mobil nu are `cip95.signTx`. Ambele sunt refuzuri explicite cu mesaj, nu bug-uri.
- **Chatbot / copilot MCP.** E F3.

### Anti-goals (ce ar face proiectul un eșec chiar dacă „merge")

- **Un număr pe care UI-ul nu-l poate explica.** Patru tool-uri din ecosistem publică patru
  procente diferite pentru aceeași acțiune ratificată; documentul de metodologie al Intersect scade
  de două ori stake-ul de no-confidence; GovTool a servit 14 luni un bug de ₳177.2M. Diferențiatorul
  nostru e aritmetica publicată. Un procent fără „ⓘ" e o regresie, nu o scurtătură.
- **Date vechi prezentate ca proaspete.** Chip-ul de status nu e decor.
- **Un client generat din swagger.** Bounds-urile `limit`/`offset` sunt întregul interval Int64,
  descrierea `GYAddress` spune bech32 și e greșită, iar corpurile de scriere au camelCase în
  interiorul unor corpuri snake_case. Un client generat documentează formatul greșit și rupe fiecare
  scriere. Codecurile se scriu de mână din §3 și §4 ale screen-map.
- **Mesh în bundle**, sau CSL folosit pentru ceva ce backendul face deja (construit de tranzacții,
  codare de adrese). **CSL pentru a decoda** o tranzacție înainte de semnare e altceva și e permis —
  vezi §8-R8: backendul nu oferă acel serviciu, iar CSL e deja în `package.json`-ul template-ului,
  marcat `--external` în bundle, și deja folosit în `bjj-frontend/src/App/Utils/WalletBalance.purs`.
- **Un procent calculat dintr-un facet count.** Facetele se numără cu propriul predicat *scos*, deci
  suma lor depășește `total` — un procent de acolo trece de 100%.

---

## 4. Constrângeri

**Din contractul API (măsurate, nu citite):**

| Constrângere | Consecință de design |
| --- | --- |
| API-ul de scriere e sub `BasicAuth "user-realm"`, cu un user/parolă partajate | **BFF-ul e parte din arhitectură**, nu un detaliu de dev. §5.10 cerea o decizie de design: aceasta e. |
| Un 401 trimite `WWW-Authenticate: Basic` → dialogul nativ al browserului | BFF-ul îl șterge pe răspuns. Cel din `bjj-frontend/bff/server.js` o face deja. |
| Credențiale greșite dau **403, nu 401**, cu corp **gol** | Logica de reauth pe `status === 401` nu se declanșează; `.json()` aruncă. Parsăm corpul ca String întâi. |
| `CORS_ALLOWED_ORIGINS` e **gol** pe deploymentul actual | **(call) Nu-l setăm în producție, și nici nu trebuie.** Browserul vorbește **same-origin**, prin BFF — vezi §6. CORS rămâne relevant doar pentru dev-ul local pe `:5173`, și chiar și acolo `npm run bff` e varianta fără CORS. Asta face și audit-finding-ul `DC-06` irelevant pentru noi, ține query-api în afara internetului public, și e singura formă în care headerele de prospețime `X-*` sunt vizibile fără `Access-Control-Expose-Headers`. |
| `order_by` fără `sort_order` înseamnă `asc` | „Sortează după putere de vot" arată cei mai slabi DRep-i primii. Trimitem mereu ambele. |
| Parametrii nedeclarați sunt ignorați silențios | Un filtru cu typo eșuează **deschis**: date nefiltrate sub un UI care arată filtrat. Verificăm împotriva `total`. |
| `db_unavailable` înseamnă de obicei „prea lent", nu „picat" | **Nu se face retry orb.** Query-urile abandonate macină mai departe. |
| `/profiles` și `/activity` dau **406** pe CSV; restul merg | Butonul Export nu se randează pe cele două. |
| CSV nu are `Content-Disposition` | Construim `Blob` + `URL.createObjectURL` în client. |
| `/committee` și `/metrics/formulas` nu au envelope | Un parser generic `Page<T>` aruncă. Două codecuri separate. |
| `/profiles/by-credential/` acceptă **numai CIP-129** | CIP-105 și hexul dau 400. Conversia se face client-side și are nevoie de flagul de script, pe care CIP-105 nu-l poartă — se rezolvă prin `/drep/{id}` (acceptă toate trei formele, întoarce `has_script`). |
| `/drep/{id}` răspunde în 0.65–0.68 s | Peste plafonul de 500 ms p95 din `metrics.md`, dar nu blochează nimic. Ecranul se construiește pe o singură cerere, nu pe fan-out-ul de patru. |
| Dashboard `total_dreps` = 204 ≠ `/dreps` `total` = 294 | Linkul din dashboard spre listă filtrează `?status=active&status=inactive`, sau eticheta spune „înregistrați (excl. unknown)". |
| `/activity` e singura rută cu cursor | Un singur mod de paginare diferit în tot UI-ul. |
| Plafon dur de `offset` la 10000 | Peste el: 400 `offset_too_large`. Pager-ul se oprește înainte. |

**Din teren (wallet-uri):**

| Constrângere | Consecință |
| --- | --- |
| `required_signers`-only pentru revendicarea unei credențiale de DRep (`Operations.hs:317`, `Utils.hs:363`) | Semnează **Yoroi-extensie** (din 2023-10-16) și **Seedelf**. Refuză Lace, Gero, Yoroi mobil, orice cale Trezor — și refuzul e **conform specificației**, deci nereclamabil upstream. |
| Yoroi-extensie are preprod ca **singura** rețea de test | Fluxurile de scriere din M2 sunt demonstrabile azi pe preprod, pe acel wallet. |
| CIP-30 nu distinge preprod de preview (`getNetworkId()` → 0 pentru toate testnet-urile) | Cerem `{cip: 142}` (`getNetworkMagic`) și **refuzăm** să continuăm fără magic confirmat. Implementat de Eternl, Gero, Lace. |
| `getExtensions()` minte | Lace ignoră literal argumentul lui `enable`; Gero a raportat extensii inexistente până în 2026-09-20. Detectăm pe obiectul `api.cip95`. |

**De proces:** HARD-GATE-ul skill-ului de brainstorming — nu se scrie cod până ownerul aprobă acest
document. Următorul pas după aprobare e `superpowers:writing-plans`, nu implementare.

---

## 5. Ce se refolosește — inventar verificat

### Punctul de pornire e `cardano-dapp-frontend-template`, nu `bjj-frontend`

**Corecție față de prima versiune a acestui document.** Ownerul are deja un template propriu la
`/Users/mg/Projects/CardanoProjects/cardano-dapp-frontend-template` — *„Initial template scaffold
derived from bjj-frontend"* (2 commit-uri, fără remote), curățat de domeniul BJJ. Deci nu extragem
scheletul din `bjj-frontend`: **clonăm template-ul**. Ce aduce, verificat pe fișiere:

- Arhitectura ReaderT + StoreT + RouterT (Real World Halogen): `AppM.purs`, `AppBase.purs`,
  `DemoM.purs`, `Store.purs`, `AppEnv.purs`, `Main.purs` + `MainDemo.purs`.
- **Pattern-ul de capabilitate „page-shaped" în patru fișiere**, cu un exemplu funcțional:
  `MonadQueryApi.purs` / `Types.purs` / `Default.purs` (HTTP prin `affjax-web`) / `Mock.purs`. Asta
  e exact forma pe care o cer cele 29 de rute `GET`.
- O pagină de explorer lucrată, cu `RemoteData` + debounce: `Components/Explorer/Items.purs`.
- Wallet connect CIP-30 funcțional prin `halogen-cardano-wallet-connect-component`.
- BFF-ul (`bff/server.js`, `config.js`, `start.sh`), Dockerfile multi-stage, `docker-compose.yml`,
  `vite.config.js` deja „Cardano-flavored" (fix de libsodium, polyfill-uri, wasm), Tailwind v4 +
  daisyUI, `scripts/build-image.sh`.
- `package.json` cu bundle separat demo/prod: `build:spago` bundle-ază `MainDemo` (mock),
  `build:spago:prod` bundle-ază `Main` (HTTP real). **Deci bundle-ul de mock e cel implicit** — ce
  face e2e-ul posibil fără wallet, backend sau lanț.
- **`.cursor/skills/`: `add-explorer-page`, `add-interaction`, `capability-layer`,
  `effect-capability-refactor`, `implementation-workflow`, `post-plan-review`** — procedurile
  ownerului pentru exact munca asta. Fiecare ecran nou trece prin `add-explorer-page`, fiecare flux
  de scriere prin `add-interaction`. Plus `.cursor/rules/halogen-purescript/`.
- `README.md` §„Customizing for your dApp" — șase pași numerotați pentru rebranding (tipuri de
  domeniu, servicii BFF, `VITE_*` prin FFI în `AppEnv.js`, nume de imagine, bundle de producție,
  tema daisyUI). Îi urmăm literal; nu inventăm un proces paralel.

Dependențele npm sunt doar fastify + vite + tailwind + purescript/spago/purs-tidy, plus CSL marcat
`--external` în bundle. **Nici Mesh, nici `@utxos/sdk` nu sunt instalate** — confirmă §2.

Mai există patru produse ale ownerului pe același schelet (`raffleize-frontend-purescript`,
`toa-demo-frontend`, `entangled-frontend`, `bjj-frontend`), deci pattern-ul e rulat în producție de
mai multe ori, nu o dată.

### Din `bjj-frontend` se copiază componente, nu scheletul

`bjj-frontend` are 32.534 de linii `.purs`+`.js` sub `src/`, 501 în `bff/`, 361 în `e2e/`. E locul de
unde luăm bucăți pe care template-ul nu le are.

| Ce | De unde | De ce merge direct |
| --- | --- | --- |
| Capabilități de transport | `Capabilities/Query/BJJ/Default/Http.purs`, `Capabilities/Interaction/BJJ/Default.purs`, `Default/Http.purs` | Citesc corpul ca String și parsează JSON **doar pe 2xx** — exact ce cere contractul `{code,message}` + corpurile goale de 403. Template-ul are forma (`Default.purs`), bjj are disciplina măsurată. |
| Polling de tranzacție | `Capabilities/Interaction/BJJ/TxPolling.purs` | 24 de poll-uri, 10/5/10 s. §4.6 cere același lucru. |
| Orchestrarea build→sign→submit | `Components/ActionComposer.purs:875-906` | Un `H.fork` + `runExceptT`. Vorbește `{tx_unsigned, tx_wit}` ↔ `AddWitAndSubmitParams` câmp cu câmp. |
| Codecuri de query în URL | `Route/Codec.purs`, `Route/ExplorerQueries.purs` (286 linii) | routing-duplex. Cele 19 filtre de pe `/dreps` devin stare în URL fără invenție. |
| Filtre debounced | `Components/Explorer/RenderHelpers{,/Filters}.purs` (631 linii) | `runDebouncedLatest`, `commitAndNavigate`. |
| Paletă de căutare | `Components/UnifiedSearch.purs` (383 linii) | §2.14 (⌘K). |
| Primitive de UI | `Components/Shared/`: `RemoteDataRender`, `SectionCard`, `StatGrid`, `EmptyState`, `Breadcrumb`, `Modal`, `ConfirmDialog`, `ActionButton`, `OnchainExplorerLink` | Tailwind/daisyUI prin `halogen-daisyui` (fork propriu). |
| Chip de status | `Components/Shared/ProtocolStatusCard.purs` | Forma există; sursa devine `/status` + headerele `X-*`. §2.1. |
| Widget de verificare | `Components/Shared/VerificationChip.purs` + `VerificationPanel.purs` | §2.18. Semantica se schimbă (0/1/>1), carcasa nu. |
| Așteptare de tranzacție | `Components/Shared/TxWaitCarousel.purs` | §2.20 pasul 4–5. |
| Capabilități mici | `CopyToClipboard`, `Delay`, `GetTime`, `LocalStorage`, `LogMessages`, `ScrollToTop`, `DomFocus`, `App/FocusTrap`, `App/SpaLinkInterceptor` | — |
| E2E fără lanț | `playwright.config.js` + `e2e/` (361 linii) | Template-ul aduce `MainDemo`/`DemoM` și face bundle-ul de mock pe cel implicit; bjj aduce harness-ul Playwright peste el. |

**Despre BFF-ul moștenit prin template** (`bff/server.js`, 190 de linii, identic în ambele repo-uri,
citit integral): hook global `onSend` care **șterge `WWW-Authenticate`** pe tot `/api/*` (`:41-52`);
`rewriteRequestHeaders` (`:120-129`) **aruncă `authorization`-ul venit din client** și pune pe loc
cel ținut pe server, deci un browser nu poate strecura propriile credențiale prin proxy; CORS
`origin:false` implicit; SPA fallback. **Zero editări în `server.js`.** Dar `config.js` (95 de linii)
e per-produs și folosește `getEnv(name, default)` cu `'cardano'`/`'lovelace'` **hardcodate**.
Palmares e fail-closed peste tot (`${VAR:?}` în `compose.yaml`, `die` în interaction-api); varianta
palmares folosește **`requireEnv`**, care există deja în același fișier și face `process.exit(1)`. Un
placeholder care se întâmplă să fie un default real e exact felul în care `devonly` a ajuns să
autorizeze boot-uri de producție — vezi comentariul din `palmares/.env.example`.

### Se scrie de la zero

Tot ce e domeniu BJJ: `Belt/`, `TTM/`, `Chatbot/`, `MyDojo`, `Promotions`, `Achievements`,
`Organization`, `Practitioner`, `PromotionCard`, `BeltCertificate`, `ActivityHeatmap`. Nimic de acolo
nu se adaptează — se șterge.

**Excepție:** `Components/LineageGraph/FFI.{purs,js}` + `Capabilities/LineageGraphRuntime.purs` **se
păstrează** — e Cytoscape ținut în spatele unei capabilități, și check 13 cere un graf de
delegări/atestări. Se șterge semantica de lineage BJJ, nu componenta. Vezi §6, M1b.

### Se extinde în fork-urile ownerului (nu dependențe noi)

`spago.yaml` arată că `cip30`, `cip30-typesafe`, `cardano-serialization-lib`,
`cardano-capabilities`, `halogen-daisyui` și `halogen-cardano-wallet-connect-component` sunt toate
fork-uri `mariusgeorgescu/*`, iar ownerul a dat (2026-10-25) autorizarea permanentă de a le evolua.
**Clonate local:** `purescript-cardano-capabilities`, `purescript-halogen-daisyui`,
`halogen-cardano-wallet-connect-component`, toate în `/Users/mg/Projects/CardanoProjects/`.
**Nu sunt clonate — primul pas al blocului de fork e `git clone`:** `purescript-cip30`,
`purescript-cip30-typesafe`, `purescript-cardano-serialization-lib`. Ce există în `.spago/p/…` e
cache read-only, nu un checkout de lucru. Deci:

1. `purescript-cip30` — FFI pentru namespace-ul `cip95`: `getPubDRepKey`,
   `getRegisteredPubStakeKeys`, `getUnregisteredPubStakeKeys`, `signData`, `signTx`. ~19 linii de JS
   plus tipuri.
2. `purescript-cip30` — `getNetworkMagic` (CIP-142).
3. `purescript-cardano-capabilities` — `MonadCIP30.purs:150` are `enableWallet wname = enable wname [ { cip: 30 } ]`
   **hardcodat**. Devine parametru, ca să putem cere `[{cip:30},{cip:95},{cip:142}]`.

**Asta elimină complet nevoia de `mlabs-haskell/purescript-cip95`** — și cu ea problema LICENSE.

---

## 6. Ce se construiește — două milestone-uri

Împărțirea nu e estetică: **tot riscul de wallet trăiește în M2**. M1 nu atinge un wallet, deci
*„aplicația live pe preprod"* nu depinde de nicio problemă de semnătură.

### M1 — explorer read-only, live pe preprod

Fără wallet, fără scriere. **Dar cu BFF** — corecție față de prima versiune a acestui document, care
spunea „fără BFF" și în același timp cerea `CORS_ALLOWED_ORIGINS`, două lucruri care nu pot fi
amândouă adevărate. În template, **BFF-ul *este* serverul de static** (`@fastify/static` pe `../dist`
plus `setNotFoundHandler`), iar capabilitățile de transport lovesc căi relative same-origin
(`bjj-frontend/src/AppEnv.purs`: `bjjQueryApiBaseUrl = "/api/bjj-query-api"`). Deci M1 livrează un
server; pur și simplu nu livrează nicio scriere.

**(call) Prefixele BFF: `/api/query` și `/api/interaction`.** Runbook-urile de deploy presupun
`/query` și `/api` pe un singur host, unde `/api` **e** interaction-api — o coliziune cu prefixul
`/api/<service>` al template-ului. Prefixul BFF-ului e intern frontendului, deci câștigă în interiorul
containerului; dacă rutele publice din runbook trebuie păstrate, se rezolvă în `HTTPRoute`, nu în
cod.

**(call) M1 se livrează în două felii, nu una.** Ownerul a cerut *„aplicația live pe preprod … nu
completitudine de ecrane"*, iar cele opt ecrane din „Plus:" de mai jos nu costă design nou, dar costă
codecuri scrise de mână, fixturi de mock și un spec Playwright fiecare:

- **M1a — cele zece ecrane de guvernanță de mai jos.** Asta e „live pe preprod", și poarta M1.
- **M1b — restul.** Același pattern, livrat după ce M1a e public.

| Ecran | Apeluri | Note de design |
| --- | --- | --- |
| Chip global de status | `GET /status`, poll 30–60 s | `attestation` e `null` pe un deploy doar-guvernanță. Nu presupunem obiectul. |
| Hub / landing | `/pages/dashboard` + `/activity?limit=10`, în paralel | Ambele autonome. |
| Dashboard | `/pages/dashboard?top_limit=10&closing_soon_limit=5&epochs=20` — **un apel** | `power_epoch` e câmp separat: cele trei `*_lovelace` sunt din alt moment. Trezoreria e două numere **intenționat**. |
| Listă DRep-i | `/pages/drep-explorer` — **un apel** | 22 de câmpuri pe rând, inclusiv toate metricile → zero N+1. Facetele nu însumează `total`. |
| Detaliu DRep | `/drep/{id}` — **un apel** | Fan-out-ul de patru apeluri din screen-map e sfat mort; nu se construiește. |
| Listă acțiuni de guvernanță | `/gov-actions` + filtru de îngustare | ~0.09 s acum, dar filtrul rămâne. |
| Detaliu acțiune | `/gov-action/{id}` | `#` în id are nevoie de `%23`. |
| Explorer de voturi | `/votes` | **Funcție în trepte pe pagini adânci** (1.1 s la offset 600 → 12.15 s la 800), decis prin ADR 0009 că nu se repară. Pager-ul se oprește la 600 și oferă filtrare, nu adâncime. |
| Căutare / ⌘K | `/search` | — |
| Metodologie | `/metrics/formulas` — array gol de envelope, 8 intrări | Fetch o dată, cache pe sesiune. `null_when` e ce distinge „0%" de „nu se aplică". |

**M1b:** listă SPO, comitet, retrageri de trezorerie, explorer de delegări, feed de activitate,
liste/detalii de profil, atestări, dispute — toate pe același pattern de un-apel-pe-ecran, plus
**graful de delegări/atestări** și **widget-ul embed**, ambele cerute de checks 13 și 15:

- **Graful (Cytoscape) rămâne în scope.** Check 13 îl numește explicit. Componenta există —
  `bjj-frontend/src/Components/LineageGraph/FFI.{purs,js}` plus
  `Capabilities/LineageGraphRuntime.purs`, Cytoscape ținut în spatele unei capabilități — și
  Cytoscape e deja în `package.json`-ul lui bjj. **E singura dependență nouă pe care designul o
  justifică**, fiindcă un graf nu e una din cele trei forme SVG. Ce se șterge e semantica de lineage
  BJJ, nu componenta.
- **Widget-ul embed e un `iframe` servit de pe originea noastră, nu un script tag. (call)** Un embed
  prin script tag pe origini terțe arbitrare e **imposibil** împotriva allowlist-ului fail-closed din
  `webapi-lib/Palmares/WebAPI/CORS.hs` — potrivire pe egalitate, fără wildcard, prin design. Un
  script embed ar cere o schimbare de backend, care e non-goal. Iframe-ul satisface check 15
  („randat într-o pagină externă de test") fără să atingă backendul.

**Grafice.** `Components/Chart/` are `Distribution`, `Pyramid`, `StackedBar` — SVG scris de mână, nicio
librărie. Palmares are nevoie de: serie `participation_by_epoch` (20 de puncte), histogramă
`power_buckets` (6 bucket-uri), serie de putere pe DRep (`/drep/{id}/power`, interval de epoci, fără
`offset`). **(call)** Rămân SVG scris de mână, în același stil. Nu adăugăm o librărie de grafice
pentru trei forme.

**Nicio prerechizită de CORS.** Prima versiune a acestui document cerea `CORS_ALLOWED_ORIGINS` setat
pe query-api; same-origin prin BFF o face inutilă, iar headerele de prospețime `X-*` ajung în browser
fără `Access-Control-Expose-Headers`. Pentru dev local pe `:5173` se folosește `npm run bff`, nu o
excepție de CORS în producție.

### M2 — fluxuri de scriere

BFF + wallet. Cele cinci etape din §2.20 sunt identice pe toate fluxurile; §4 din screen-map dă
numele exacte de câmpuri.

**Poarta, înainte de orice flux:** `GET /status` pe interaction-api. Blocăm dacă
`deployed_scripts !== true` sau `oracle_paused !== false`. `oracle_paused === null` e un fapt
*diferit* de `false` — înseamnă că oracolul nu a putut fi citit — și blochează la fel.

| Flux | Cine semnează | Stare |
| --- | --- | --- |
| Creare profil | utilizatorul | Direct. |
| Editare profil | deținătorul user-tokenului | Profilul propriu se găsește prin `/profiles/by-user-token/{asset-class}`. |
| Revendicare credențială | **ambii**: user-token **și** credențiala | Suportat doar când e același wallet. Altfel: mesaj explicit, nu eroare. §5.8. |
| Retragere credențială | credențiala singură | — |
| Emitere atestare | emitentul | — |
| Acceptare atestare | **doar** wallet-ul subiectului | Tranzacție separată; contrasemnarea funcționează. |
| Deschidere dispută | deschizătorul | Depozitul vine din `/status.oracle` pe interaction-api. Nu cerem semnătură pe un cost nenumit. |
| Rezolvare dispută | admin-ul oracolului | Construit în UI, semnat în altă parte. |
| Expirare dispută | **oricine** plătește fee-ul | Intenționat: o ieșire pe care admin-ul o poate bloca nu e o ieșire. Buton pentru toți, pe orice dispută trecută de deadline. |

**Matrice de compatibilitate de wallet, randată în UI.** Nu ascundem constrângerea din §4: la
conectare detectăm pe `api.cip95` (plat, namespace-at, și `api.experimental?.cip95`) și spunem pe
loc ce poate face wallet-ul conectat. Pentru VESPR în browserul in-app de iOS, `window.cardano` e un
proxy care se întoarce pe sine pentru orice cheie — numărăm cu `key in window.cardano`.

**Follow-up de backend, în afara acestui document:** reforma tranzacției de revendicare, ca să
semneze și Lace/Gero. `mkDRepUpdateCertificate` și `mustHaveCertificate` există în Atlas-ul pe care
îl avem pinned (verificat la `c3adfb4`: `Types/Certificate.hs:55,70,113`, `Types/TxCert.hs:19`,
`TxBuilder/Class.hs:889-890`), deci e o linie în `claimProofSkeleton`, fără schimbare de validator și
fără redeploy. **Prețul, care nu e gratuit:** certificatul de update **rescrie ancora de metadata a
DRep-ului** — cu `Nothing` i-ai șterge metadata CIP-119. Trebuie citită ancora curentă din db-sync și
reemisă identic. Nu e în scopul UI-ului; e nevoie de el înainte de mainnet, nu înainte de demo.

---

## 7. Unde trăiește codul

**(call)** `palmares/frontend/`, în același repo cu backendul — o copie a
`cardano-dapp-frontend-template` (care n-are remote, deci se copiază, nu se clonează ca submodul),
cu `spago.yaml`, `package.json` și `Dockerfile` proprii, plus un job de CI care se declanșează
**numai** pe `frontend/**`.

De ce în același repo, și nu un repo nou ca `bjj-frontend`:

- Swagger-ul se generează la pornirea API-ului în `palmares/docs/generated/swagger/`. În același
  repo, FE-ul îl citește de acolo. În repo-uri separate, cineva îl copiază manual — e o corvoadă
  recurentă documentată în memoria proiectului.
- Istoria proiectului: *„bjj-frontend `deploy/api-fail-closed` branch must deploy in lockstep or the
  stack breaks"*. Un repo elimină clasa asta de problemă.
- Contractul de wire și UI-ul se mișcă împreună. Un singur tag.

Costul, numit: repo-ul palmares capătă un al doilea toolchain (node + PureScript lângă Nix + GHC).
De aceea CI-ul e condiționat pe cale — `cabal build all` nu trebuie să ruleze când se schimbă un
`.purs`. Imaginile de backend sunt `linux/arm64` only; `Dockerfile` din bjj-frontend e deja
multi-arch, deci FE-ul nu moștenește problema.

### Igiena de licență — de rezolvat în blocul 0, nu la open-sourcing

`palmares` e Apache-2.0, cu un `NOTICE` care relicențiază porțiunile MIT din Belt System și păstrează
textul original în `licenses/`. Dar:

- `bjj-frontend` **nu are fișier LICENSE** și nu are câmp `license` în `package.json`.
- **Niciunul** din cele șase fork-uri `mariusgeorgescu/*` nu are LICENSE în checkout.
- `cardano-dapp-frontend-template` nu are nici el.

Adică exact defectul pentru care §2 respinge `mlabs-haskell/purescript-cip95`. A copia ~10.000 de
linii nelicențiate într-un repo public Apache-2.0 și apoi a depinde de șase pachete git nelicențiate
e același eșec de igienă, și e ieftin de reparat fiindcă ownerul e singurul deținător de copyright.

**(call)** Blocul 0 adaugă `LICENSE` (Apache-2.0) și câmp `license` în template, în fork-uri și în
`frontend/`, plus o intrare în `NOTICE`-ul palmares pentru codul venit din template. **(call)**
`halogen-router` e pinuit la `ref: main` — o țintă mobilă, care se bate cu reproductibilitatea cerută
de check 16; trece pe un SHA.

---

## 8. Reguli dure de randare

Nenegociabile, fiecare cu motivul ei măsurat. Ancorele CIP-100/CIP-119 sunt **documente
necontrolate de noi**.

| # | Regulă | De ce |
| --- | --- | --- |
| R1 | Markdown doar GFM, **fără cale pentru HTML brut** | Documentele de ancoră sunt input neîncrezut. Fără cale, fără clasă de XSS. |
| R2 | Lungimile se taie **la randare** | Limitele din CIP nu sunt invarianți on-chain. Un câmp de 2 MB e posibil. |
| R3 | **ID-ul de DRep se afișează lângă numele afișat**, mereu | Amenințarea reală e impersonarea, nu injecția. Două profiluri pot revendica numele „Intersect". |
| R4 | Documentele de ancoră neconforme se randează **brut**, nu ascunse | Ascunderea face registrul mai puțin util decât lanțul. |
| R5 | Credențialele CIP-129 se **decodează după byte-ul de header**, niciodată prin prefix | CIP-105 și CIP-129 împart prefixul `drep1`. GovTool a servit `[]` pentru un DRep cu 77 de voturi înregistrate din cauza asta (decizia lor D25). **Verificat în codul nostru 2026-10-25 — e corect, și mai strict decât regula:** `parseCredentialRoleCip129` (`Ids.hs:149-160`) decodează bech32, verifică lungimea de 29 de bytes și **cere ca byte-ul de header să fie de acord cu HRP-ul**, apoi `canonicalGovCredential` (`Attest.hs:159-162`) re-randează din byte în loc să ecoueze textul cererii. Un `drep1…` cu header de comitet e refuzat, nu potrivit silențios. UI-ul moștenește asta gratis; obligația lui e doar conversia CIP-105 → CIP-129 înainte de apel. |
| R6 | Fluxul de scriere **nu pornește** până rețeaua e stabilită fără echivoc | `getNetworkId()` întoarce 0 pentru preprod *și* preview. Un demo care semnează pe rețeaua greșită e un demo pierdut. **(call) Două căi satisfac regula, nu una:** (a) un `getNetworkMagic` confirmat prin CIP-142; sau (b) un wallet a cărui identitate implică exact o rețea de test — Yoroi-extensie, unde preprod e singura. Orice altceva: refuz. **De ce nu e o slăbire:** prima versiune cerea CIP-142 și numai CIP-142, iar intersecția dintre „implementează CIP-142" (Eternl, Gero, Lace) și „semnează pe `required_signers`" (Yoroi-extensie, Seedelf) e **goală** — deci revendicarea de credențială ar fi fost imposibilă prin construcție. Calea (b) păstrează garanția întreagă: dacă wallet-ul nu poate fi pe preview, ambiguitatea pe care CIP-142 o rezolvă nu există. Pentru Seedelf se **măsoară** înainte de M2 (§9). |
| R8 | Cardul de semnare arată un **sumar decodat** al tranzacției, nu doar CBOR | Check 14 cere explicit „fără blind-signing", iar rutele de build întorc un **string** de CBOR hex și nimic altceva (§4.3 din screen-map). **(call) Decodăm cu CSL**, care e deja în `package.json`-ul template-ului (`@emurgo/cardano-serialization-lib-browser`, marcat `--external`) și deja folosit în `bjj-frontend/src/App/Utils/WalletBalance.purs` — zero dependențe noi, și nu încalcă anti-goal-ul din §3, care interzice CSL pentru ce *face deja backendul*. Un decode pentru afișare nu e oferit de nicio rută. Ecoul câmpurilor introduse de utilizator **nu e suficient**: nu dovedește ce s-a construit, doar ce s-a cerut. |
| R7 | Hash-ul de ancoră se face pe **bytes brut**; nicio formă canonică | CIP-100 se contrazice de trei ori și listează canonicalizarea între alternativele **respinse**. Documentele supradimensionate se refuză — niciodată trunchiate-apoi-hash-uite. |

---

## 9. Întrebări deschise — ale ownerului

Două, și nu le pot decide eu.

**9.1 Cine face deploymentul public, și e el parte din munca asta?**

G1 spune „un străin deschide URL-ul". Dar **`palmares` nu apare nicăieri în `apps-k8s-configs`** —
zero potriviri în tot repo-ul: niciun director de app, niciun `Gateway`/`Certificate`, nicio
`Kustomization`. Iar `F1-PROGRESS.md` spune că manifestele sunt ținute deliberat nepublicate, fiindcă
blocul care „atinge rack-ul" e condiționat de cifrele de RAM din Experimentul A, pe un `local-path`
cu `allowVolumeExpansion: false` — PVC-ul se dimensionează o singură dată.

Deci „live pe preprod" are două citiri, și diferența e un bloc de platformă:

- **(a) frontend-only:** M1 înseamnă stiva completă pornită local peste db-sync-ul sincronizat, cu
  deploy-ul public rămas bloc separat, owner-gated. Atunci M1-1 iese din poarta M1 și G6 se
  restrânge la „o comandă, local".
- **(b) frontend + platformă:** munca asta include plasarea db-sync + Postgres, rute Gateway API, un
  `Certificate`, secrete SOPS, și commit-uri în al doilea repo — unde regula casei e git → Flux,
  niciodată `kubectl apply`, și unde `prune: true` face din ștergerea unui fișier o ștergere de
  resursă.

Presupunerea mea până la un răspuns, scrisă ca atare: **(a)**, pentru că e reversibilă și pentru că
ownerul a parcat explicit ce atinge infrastructura. `FE-GOAL.md` are blocul 3 scris pentru (b); dacă
răspunsul e (a), blocul 3 se restrânge la „imaginea se construiește și pornește local".

**9.2 Ce era „varianta B"?**

`F1-PROGRESS.md:3777` spune *„Planul C — UI-ul, scope decis ca **varianta B**"*, dar variantele A/B/C
pentru UI nu sunt scrise nicăieri în niciunul din cele două repo-uri. Documentul ăsta poate fi
varianta B, sau o rescopare a ei. Dacă există o decizie anterioară despre scope, o respect; dacă era
doar o notă, documentul ăsta o înlocuiește.

---

## 10. Ce rămâne builderului

Nedecis aici, pe bună dreptate — un builder competent alege fără să revină la owner:

- Structura de directoare sub `frontend/src/` (urmează pattern-ul `bjj-frontend`).
- Ce primitive din `Components/Shared/` se copiază versus se extrag într-un pachet comun. **Sfat:**
  se copiază. Un pachet partajat între două produse nelegate e abstracția pe care nimeni n-a cerut-o.
- Granularitatea componentelor Halogen și unde stă starea (store global versus stare locală).
- Paleta de culori și tipografia, în limitele daisyUI.
- Dacă e nevoie de un router cu lazy loading (probabil nu la ~20 de ecrane).
- Forma exactă a mesajelor de eroare, derivate din cele 24 de coduri de eroare din
  `webapi-lib/Palmares/WebAPI/Envelope.hs`.

**De citit înainte de a scrie primul ecran**, în ordine: `docs/usecases/api-screen-map.md` §3 și §4
(contractul de integrare și forma de scriere — §4.7 e secvența exactă, scrisă pentru un hook React
dar valabilă pas cu pas), apoi `scripts/smoke_attest_flow.sh` (singurul flux de scriere end-to-end
din repo), apoi `docs/usecases/governance-read-surface.md` și `attestation-protocol.md`.

**De măsurat înainte de M2**, nu de presupus: auditul propriu al selecției de martori din Yoroi și
Gero (ambele open-source, fără browser), un test manual pe Eternl pe preprod cu
`ATTEST_SMOKE_DREP_ID` existent, și `cip30-test-wallet` (Apache-2.0, preprod, Playwright) cablat în
CI pentru instalație.

---

## 11. Surse

- `docs/usecases/api-screen-map.md` — 1833 de linii, contract măsurat pe serviciul care rulează.
- `docs/usecases/governance-read-surface.md` (1742), `attestation-protocol.md` (1300).
- `docs/spec/metrics.md` — cele 8 metrici și plafonul p95.
- `docs/spec/protocol.md` §4.5 și `:275-278` — argumentul social pentru React (valid) și cel tehnic
  (fals, vezi §2).
- `2026-10-19-gov-registry-spec.md` §9 — checks 13–16.
- ADR 0007 (un redeploy e un registru nou), ADR 0009 (funcția în trepte de pe `/votes` nu se repară).
- `bjj-frontend/` — inventarul din §5, verificat pe fișiere.
- Atlas la `c3adfb4` (palmares pinnează tag-ul `bf0d2d13`) — pentru follow-up-ul din §6.
