# Palmares UI — M1a + deploy preprod: plan de implementare

> **Pentru executanți agentici:** SUB-SKILL OBLIGATORIU: `superpowers:executing-plans` (execuție
> nativă, aleasă de owner prin *„nu te mai oprești până nu e gata"*). Pașii folosesc checkbox
> (`- [ ]`) pentru urmărire.

**Goal:** zece ecrane de guvernanță read-only, în PureScript/Halogen, servite same-origin prin BFF,
deployate public pe preprod la `gov.cardano.vip`.

**Arhitectură:** copie a `cardano-dapp-frontend-template` în `palmares/frontend/` — ReaderT + StoreT
+ RouterT, capabilitate de query în patru fișiere (`MonadQuery*`/`Types`/`Default`/`Mock`), BFF
fastify care servește SPA-ul și proxyează `/api/query` + `/api/interaction` injectând Basic-auth.
Fiecare ecran e **un singur apel** către o rută `/pages/*` sau de detaliu; zero N+1.

**Tech stack:** PureScript 0.15.16, Halogen 7, spago 0.93, halogen-store, halogen-router,
halogen-daisyui (fork propriu), affjax-web, argonaut, Tailwind v4 + daisyUI, Vite 6, Fastify 5,
Playwright. Fără Mesh, fără `@utxos/sdk`. CSL doar pentru decode (M2).

**Spec:** `docs/superpowers/specs/2026-10-25-palmares-ui-design.md` (design), cu
`palmares/docs/usecases/api-screen-map.md` §2–§4 ca contract măsurat. Contractul de execuție:
`docs/superpowers/FE-GOAL.md`.

## Constrângeri globale

Fiecare task le moștenește. Valorile sunt copiate verbatim din screen-map.

- **Niciun client generat din swagger.** Bounds-urile `limit`/`offset` publicate sunt intervalul
  Int64; `GYAddress` e documentat bech32 și e de fapt hex; corpurile de scriere au camelCase în
  interiorul unor corpuri snake_case. Codecurile se scriu de mână.
- **Corpul răspunsului se citește ca String; JSON se parsează DOAR pe 2xx.** Un 403 are corp gol →
  `.json()` aruncă. Credențiale greșite dau **403, nu 401**.
- **`order_by` fără `sort_order` înseamnă `asc`** (`Query/Common.hs:155`). Se trimit mereu ambele.
- **Parametrii nedeclarați sunt ignorați silențios** → un filtru cu typo eșuează deschis. Fiecare
  query asertează împotriva `total`.
- **Plafon dur de `offset` la 10000**; peste → `400 offset_too_large`. Clamp-uri reale: `limit` 50
  implicit / 200 maxim pe liste, 20 pe `/activity`, 100 pe `/search`.
- **`/votes` are funcție în trepte pe pagini adânci** (1,1 s la offset 600 → 12,15 s la 800; ADR 0009
  decide că nu se repară). Pager-ul se oprește la offset 600.
- **Nu se face retry pe 503.** `db_unavailable` înseamnă de obicei „prea lent"; query-urile
  abandonate macină mai departe.
- **Facetele nu însumează `total`** (fiecare e numărată cu propriul predicat scos) → niciun procent
  derivat dintr-un facet. `power_buckets` **da**, însumează.
- **`/committee` și `/metrics/formulas` nu au envelope** (obiect gol / array gol de envelope).
- **Dashboard `total_dreps` = 204 ≠ `/dreps` `total` = 294** (dashboard exclude `unknown`). Linkul
  filtrează `?status=active&status=inactive`.
- **`/profiles` și `/activity` dau 406 pe CSV.** Butonul Export nu se randează acolo. CSV n-are
  `Content-Disposition` → `Blob` + `createObjectURL`.
- **`attestation` din `/status` e `Maybe`** — `null` pe un deploy doar-guvernanță.
- **Reguli de randare R1–R5 din design §8** se aplică oriunde se afișează text din ancore: GFM fără
  cale de HTML brut, clamp la randare, ID-ul de DRep lângă numele afișat, documente neconforme
  randate brut. R6–R8 sunt M2.
- **Stil:** `purs-tidy`, 2 spații, fără trailing whitespace, newline final. Module ≡ cale de fișier.
  Capabilități de intenție, nu `MonadAff` în componente. Commit-uri convenționale în română.
- **Fiecare commit lasă `npx spago build --json-errors` curat și `npm run check-formatting` verde.**

## Review Focus

Cinci clase de input pe care specificația le implică dar nicio rută nu le exercitează. Fiecare are
testul lui, pus în task-ul care deține codul.

1. **Corp non-JSON pe un status de eroare** — 403 cu corp gol, 503 de la attest-sync care e un `show`
   de Haskell. Un parser care presupune JSON aruncă și ecranul rămâne alb în loc să arate eroarea.
   → Task 2.
2. **`attestation: null`** pe un deploy doar-guvernanță. Chip-ul de status și orice ecran de
   atestări trebuie să randeze „indisponibil", nu să cadă. → Task 3.
3. **`offset` peste 10000 introdus din URL** (URL-ul e stare partajabilă). Un pager care trimite
   orbește ia 400 și pierde pagina. → Task 4.
4. **Metrică `null` vs `0`** — `rationale_rate` e NULL când nimic nu s-a putut evalua. „0%" ar fi o
   acuzație la adresa autorului; trebuie „n/a". → Task 11.
5. **Nume de DRep duplicat sau absent** — două profiluri pot revendica „Intersect", iar `given_name`
   poate fi gol. Numele singur nu identifică; ID-ul merge alături, mereu. → Task 6.

---

## Structura de fișiere

Sub `palmares/frontend/`, pornind de la template și păstrându-i layout-ul:

| Fișier | Responsabilitate |
| --- | --- |
| `src/AppEnv.purs` | `apiBaseUrl` (= `/api/query`), `interactionBaseUrl`, `allowedNetworkMagic` |
| `src/Store.purs` | `freshness :: Maybe Freshness`, `formulas :: Maybe (Array Formula)` |
| `src/Route.purs`, `src/Route/Codec.purs` | cele zece rute M1a + `NotFound` |
| `src/Route/GovQueries.purs` | codecuri routing-duplex pentru filtrele de listă |
| `src/Capabilities/Query/Gov/Http.purs` | `getJson`: String→2xx→decode, altfel `{code,message}` |
| `src/Capabilities/Query/Gov/MonadQueryGov.purs` | clasa; o metodă per ecran |
| `src/Capabilities/Query/Gov/Types.purs` | DTO-urile celor zece ecrane |
| `src/Capabilities/Query/Gov/Env.purs` | `HasGovEnv r` |
| `src/Capabilities/Query/Gov/Default.purs` | implementarea HTTP |
| `src/Capabilities/Query/Gov/Mock.purs` | fixturi din măsurători reale |
| `src/Query/Page.purs` | clamp, offset ceiling, `order_by`+`sort_order`, aserțiune pe `total` |
| `src/Components/Shared/StatusChip.purs` | §2.1 |
| `src/Components/Shared/MetricInfo.purs` | „ⓘ" → formulă; `null_when` → „n/a" |
| `src/Components/Shared/DRepIdentity.purs` | nume **plus** ID, R3 |
| `src/Components/Shared/Anchor.purs` | GFM fără HTML brut, clamp, R1/R2/R4 |
| `src/Components/Chart/{Series,Histogram}.purs` | SVG scris de mână |
| `src/Components/Explorer/*.purs` | cele zece ecrane |
| `bff/config.js` | `query` + `interaction`, `requireEnv` |

---

## Task 1: Schelă, licență și CI

**Files:**
- Create: `palmares/frontend/**` (copie a `cardano-dapp-frontend-template`, fără `node_modules`,
  `output`, `dist`, `.spago`, `spago.lock`)
- Create: `palmares/frontend/LICENSE` (Apache-2.0), `palmares/frontend/.env.example`
- Modify: `palmares/frontend/package.json` (`name`, `license: "Apache-2.0"`, nume de imagine),
  `palmares/frontend/spago.yaml` (`package.name: palmares-frontend`, `halogen-router` pe SHA),
  `palmares/frontend/bff/config.js`, `palmares/NOTICE`
- Create: `palmares/.github/workflows/frontend.yml`

**Interfaces:**
- Produces: `npm run dev` / `npm run build` / `npm run bff` funcționale în `palmares/frontend/`.

- [ ] **Pas 1: copiază template-ul, excluzând artefactele**

```bash
rsync -a --exclude node_modules --exclude output --exclude dist --exclude .spago \
      --exclude spago.lock --exclude .git \
      /Users/mg/Projects/CardanoProjects/cardano-dapp-frontend-template/ \
      /Users/mg/Projects/CardanoProjects/palmares/frontend/
```

- [ ] **Pas 2: aplică cei șase pași din README-ul template-ului**, §„Customizing for your dApp":
  (1) `MonadQueryApi`/`Item*` → `MonadQueryGov`/domeniul de guvernanță (Task 2 face conținutul; aici
  doar redenumirea mecanică); (2) `bff/config.js`; (3) `AppEnv.js` FFI pentru `VITE_*`; (4) numele de
  imagine `mariusgeorgescu/palmares-frontend`; (5) `build:prod` rămâne cum e; (6) tema daisyUI.

- [ ] **Pas 3: `bff/config.js` — două servicii și `requireEnv`**

Înlocuiește blocul `envVars` și `services`. **`requireEnv` există deja în fișier** și face
`process.exit(1)`; `getEnv` cu default **nu se folosește pentru credențiale**.

```js
const envVars = {
  PALMARES_QUERY_API_URL: requireEnv('PALMARES_QUERY_API_URL'),
  PALMARES_INTERACTION_API_URL: requireEnv('PALMARES_INTERACTION_API_URL'),
  PALMARES_INTERACTION_API_USER: requireEnv('PALMARES_INTERACTION_API_USER'),
  PALMARES_INTERACTION_API_PASS: requireEnv('PALMARES_INTERACTION_API_PASS'),
}
export const services = {
  query: { target: envVars.PALMARES_QUERY_API_URL, noAuth: true },
  interaction: {
    target: envVars.PALMARES_INTERACTION_API_URL,
    username: envVars.PALMARES_INTERACTION_API_USER,
    password: envVars.PALMARES_INTERACTION_API_PASS,
  },
}
```

`query` e `noAuth: true` pentru că query-api **nu are context de auth** (`QueryAppMonad.hs:14`).
`server.js` nu se atinge.

- [ ] **Pas 4: licență și pinning**

`LICENSE` Apache-2.0; `"license": "Apache-2.0"` în `package.json`; o intrare în `palmares/NOTICE`
pentru codul venit din template; `halogen-router` mutat de pe `ref: main` pe SHA-ul curent
(`git ls-remote https://github.com/katsujukou/purescript-halogen-router main`).

- [ ] **Pas 5: `frontend.yml` — workflow separat, NU în `images.yml`**

`images.yml` e **deliberat** doar pe tag + `workflow_dispatch` (build musl Haskell >1h; un trigger pe
main se anula singur). Un build PureScript+node e minute, deci îi trebuie workflow propriu:

```yaml
name: frontend
on:
  push: { branches: [main], paths: ['frontend/**', '.github/workflows/frontend.yml'] }
  workflow_dispatch:
concurrency: { group: frontend-${{ github.ref }}, cancel-in-progress: true }
env: { GHCR_OWNER: mariusgeorgescu }
```

Un job pe `ubuntu-24.04-arm` care: `npm ci`, `npx spago install`, `npm run check-formatting`,
`npx spago build --json-errors`, `npm test`, `npm run build:prod`, `npx playwright test`, apoi
`docker/build-push-action` cu `platforms: linux/arm64` și tag-urile `${{ github.sha }}` + `latest`.

- [ ] **Pas 6: verifică**

Run: `cd palmares/frontend && npm install && npx spago install && npm run build`
Expected: build verde, `dist/` populat.
Run: `npx spago build --json-errors` → zero erori. `npm run check-formatting` → verde.
Run: `git -C palmares log --oneline -1 --name-only | grep -c '^src/' || echo 0` → `0` (commitul nu
atinge codul Haskell, deci `ci.yml` nu se declanșează).

- [ ] **Pas 7: commit**

```bash
git -C /Users/mg/Projects/CardanoProjects/palmares add frontend .github/workflows/frontend.yml NOTICE
git -C /Users/mg/Projects/CardanoProjects/palmares commit -m "feat(frontend): schela din template, cu BFF fail-closed si CI pe cale separata"
```

---

## Task 2: Transportul care respectă contractul de eroare

**Files:**
- Create: `frontend/src/Capabilities/Query/Gov/Http.purs`
- Create: `frontend/test/Test/Capabilities/Http.purs`
- Modify: `frontend/test/Test/Main.purs`

**Interfaces:**
- Produces:
  ```purescript
  data ApiError = Transport String | Api { code :: String, message :: String } | Decode String
  type Freshness = { attestState :: Maybe String, govSlot :: Maybe Int, lagSeconds :: Maybe Int }
  getJson :: forall m a. MonadAff m => DecodeJson a => String -> m (Either ApiError a)
  getJsonWithFreshness :: forall m a. MonadAff m => DecodeJson a => String -> m (Either ApiError { body :: a, freshness :: Freshness })
  renderApiError :: ApiError -> String
  ```
- Consumes: nimic.

Implementarea folosește `ResponseFormat.string`, nu `ResponseFormat.json`: `Default.purs` din
template parsează JSON indiferent de status, iar un 403 cu corp gol face parse-ul să eșueze cu un
mesaj despre JSON în loc de „credențiale".

- [ ] **Pas 1: scrie testele care pică**

```purescript
describe "getJson" do
  it "decodes a 2xx body" -- status 200, body {"total":3} → Right {total: 3}
  it "surfaces {code,message} from a 4xx instead of a decode error" -- 400 {"code":"invalid_drep_id","message":"Not a DRep identifier"} → Left (Api {code:"invalid_drep_id", ...})
  it "survives an EMPTY body on 403" -- 403, body "" → Left (Api {code:"forbidden", ...}), NU Decode
  it "survives a non-JSON body on 503" -- 503, body "ServiceUnavailable" → Left (Api {code:"db_unavailable", ...})
  it "reports a decode failure on a 200 whose shape is wrong" -- 200 {"nope":1} → Left (Decode _)
describe "freshness" do
  it "reads X-Attest-State and X-Gov-Slot when present"
  it "yields Nothing for every field when the headers are absent" -- CORS poate să le ascundă
```

Testele lovesc un server local din `test/`, nu rețeaua: funcția sub test primește corpul și statusul
prin parametru într-o variantă pură `classifyResponse :: Int -> String -> Either ApiError Json`, iar
`getJson` e învelișul subțire de I/O peste ea. Testele testează `classifyResponse`.

- [ ] **Pas 2: rulează, confirmă că pică**

Run: `cd palmares/frontend && npm test`
Expected: FAIL — `Capabilities.Query.Gov.Http` nu există.

- [ ] **Pas 3: implementează `classifyResponse` și `getJson`**

`classifyResponse status body`: pe 2xx, `jsonParser body` → `Decode` pe eșec; altfel încearcă
`{code,message}` și, dacă și asta eșuează (corp gol sau `show` de Haskell), întoarce
`Api {code: <derivat din status>, message: <corpul trunchiat la 200 de caractere, sau statusul>}`.
Codurile derivate din status: 400 `bad_request`, 403 `forbidden`, 404 `not_found`,
503 `db_unavailable`, orice altceva `http_<status>`.

- [ ] **Pas 4: rulează, confirmă că trec**

Run: `npm test` → PASS, 7 cazuri.

- [ ] **Pas 5: commit**

```bash
git -C .../palmares add frontend/src/Capabilities/Query/Gov/Http.purs frontend/test
git -C .../palmares commit -m "feat(capability): corpul se citeste ca String, JSON doar pe 2xx — si 403 cu corp gol nu mai arunca"
```

---

## Task 3: `/status`, Store-ul de prospețime și chip-ul

**Files:**
- Create: `frontend/src/Capabilities/Query/Gov/{Types,Env,MonadQueryGov,Default,Mock}.purs`
- Create: `frontend/src/Components/Shared/StatusChip.purs`
- Modify: `frontend/src/{Store,AppEnv,AppM,DemoM}.purs`
- Create: `frontend/test/Test/Components/StatusChipSpec.purs`

**Interfaces:**
- Produces:
  ```purescript
  type GovStatus =
    { network :: String
    , governance :: { epoch :: Int, slot :: Int, blockTime :: String, lagSeconds :: Int, isReady :: Boolean, source :: String }
    , attestation :: Maybe { slot :: Int, kupoTipSlot :: Int, lagSeconds :: Int, state :: String, isReady :: Boolean, policyId :: String, lastError :: Maybe String }
    , attestLagSeconds :: Maybe Int
    , drepDistrEpoch :: Int, amberLagSeconds :: Int, readyMaxLagSeconds :: Int, metricsAsOfEpoch :: Int
    }
  data ChipState = Green | Amber | Red | Unknown
  chipState :: Int -> Int -> Int -> ChipState   -- lag, amberThreshold, redThreshold
  getStatus :: m (Either ApiError GovStatus)
  ```
- Consumes: Task 2 (`getJson`, `ApiError`, `Freshness`).

- [ ] **Pas 1: scrie testele care pică**

```purescript
describe "chipState" do
  it "is Green below the amber threshold"              -- chipState 10 120 300 == Green
  it "is Amber at the amber threshold"                 -- chipState 120 120 300 == Amber
  it "is Red at the ready threshold"                   -- chipState 300 120 300 == Red
describe "GovStatus decoding" do
  it "decodes the measured preprod payload"            -- fixtura din screen-map §2.1: epoch 321, lag 43
  it "decodes a governance-only deployment"            -- "attestation": null → Nothing, NU eroare
  it "labels the three lovelace figures with power_epoch, not epoch"  -- Review Focus 2
```

- [ ] **Pas 2: rulează, confirmă că pică.** Expected: FAIL — modulul nu există.

- [ ] **Pas 3: implementează.** `Types.purs` ia fixtura din §2.1 verbatim ca test. `Store` capătă
  `freshness` și `status :: RemoteData ApiError GovStatus`; `StatusChip` randează badge-ul de rețea,
  epoca, lag-ul, starea și „metrics as of epoch N". Un singur apel; `attestation` absent se randează
  „atestări: indisponibil".

- [ ] **Pas 4: rulează.** Expected: PASS, 6 cazuri.

- [ ] **Pas 5: commit** — `feat(status): un singur apel, și attestation lipsă nu mai e o eroare`

---

## Task 4: Paginare, sortare și aserțiunea pe `total`

**Files:**
- Create: `frontend/src/Query/Page.purs`
- Create: `frontend/src/Route/GovQueries.purs`
- Create: `frontend/test/Test/Query/PageSpec.purs`

**Interfaces:**
- Produces:
  ```purescript
  type PageQuery = { limit :: Int, offset :: Int, orderBy :: Maybe String, sortOrder :: SortOrder }
  data SortOrder = Asc | Desc
  clampLimit :: Int -> Int -> Int                 -- max, requested
  nextOffset :: Int -> Int -> Int -> Maybe Int    -- limit, offset, total → Nothing la plafon
  renderPageQuery :: PageQuery -> String
  assertFiltered :: Int -> Int -> Either String Unit  -- totalFiltrat, totalNefiltrat
  offsetCeiling :: Int                            -- 10000
  votesOffsetCeiling :: Int                       -- 600
  ```
- Consumes: nimic.

- [ ] **Pas 1: testele**

```purescript
describe "renderPageQuery" do
  it "always emits sort_order when order_by is present"   -- "order_by=voting_power&sort_order=desc"
  it "emits neither when order_by is absent"
describe "nextOffset" do
  it "stops at the 10000 offset ceiling"       -- nextOffset 50 9980 50000 == Nothing
  it "stops at the end of the result set"      -- nextOffset 50 100 120 == Nothing
  it "advances inside the set"                 -- nextOffset 50 0 120 == Just 50
describe "clampLimit" do
  it "clamps above the maximum"                -- clampLimit 200 100000 == 200
  it "clamps zero and negatives to one"        -- clampLimit 200 0 == 1
describe "assertFiltered" do
  it "fails when a filter changed nothing"     -- assertFiltered 294 294 == Left _
```

- [ ] **Pas 2: rulează, confirmă FAIL.**
- [ ] **Pas 3: implementează.**
- [ ] **Pas 4: rulează, PASS, 8 cazuri.**
- [ ] **Pas 5: commit** — `feat(query): sort_order mereu trimis, și plafonul de offset nu mai ia 400`

---

## Task 5: Dashboard și hub

**Files:** `frontend/src/Components/Explorer/Dashboard.purs`, `.../Hub.purs`; `Types.purs` extins;
`frontend/src/Components/Chart/{Series,Histogram}.purs`; test pe decodare + pe cele două etichete.

**Interfaces:** Consumes Task 2, 3, 4. Produces `getDashboard :: DashboardQuery -> m (Either ApiError DashboardPage)`,
`getActivity :: PageQuery -> m (Either ApiError ActivityPage)`.

- [ ] **Pas 1:** teste — decodarea payload-ului măsurat din §2.3 (epoch 321, `total_dreps` 204,
  `power_epoch` 321, `actions_in_flight` 11, 7 perechi `actions_by_type`, 5 `actions_by_state`, 20 de
  puncte `participation_by_epoch`); un test care asertează că linkul spre listă poartă
  `status=active&status=inactive`; un test că trezoreria randează **două** numere.
- [ ] **Pas 2:** FAIL. — [ ] **Pas 3:** implementează (un apel; `Series` pentru participare,
  `Histogram` pentru bucket-uri). — [ ] **Pas 4:** PASS. — [ ] **Pas 5:** commit.

---

## Task 6: Lista de DRep-i

**Files:** `frontend/src/Components/Explorer/DRepList.purs`,
`frontend/src/Components/Shared/DRepIdentity.purs`, filtre în `Route/GovQueries.purs`, teste.

**Interfaces:** Consumes Task 4. Produces `getDRepExplorer :: DRepQuery -> m (Either ApiError DRepExplorerPage)`,
`renderDRepIdentity :: { givenName :: Maybe String, drepId :: String, hasScript :: Boolean } -> HTML`.

- [ ] **Pas 1:** teste — un apel, 22 de câmpuri pe rând, zero N+1; **`renderDRepIdentity` afișează
  ID-ul chiar când `given_name` e `Just`** (Review Focus 5) și randează ID-ul singur când e `Nothing`;
  un test că niciun procent nu se calculează dintr-un facet (facetele însumează peste `total`);
  `power_buckets` **da** însumează la `total`.
- [ ] **Pas 2:** FAIL. — [ ] **Pas 3:** implementează. — [ ] **Pas 4:** PASS. — [ ] **Pas 5:** commit.

---

## Task 7: Detaliul DRep

**Files:** `frontend/src/Components/Explorer/DRepDetail.purs`,
`frontend/src/Components/Shared/Anchor.purs`, teste.

**Interfaces:** Produces `getDRep :: String -> m (Either ApiError DRepDetail)`,
`renderAnchorDocument :: AnchorDoc -> HTML`.

- [ ] **Pas 1:** teste — **un singur apel** (`/drep/{id}`), nu fan-out de patru; `renderAnchorDocument`
  nu produce HTML brut din markdown (R1), taie la randare un câmp de 100 kB (R2), și randează brut un
  document neconform în loc să-l ascundă (R4).
- [ ] **Pas 2:** FAIL. — [ ] **Pas 3:** implementează. — [ ] **Pas 4:** PASS. — [ ] **Pas 5:** commit.

---

## Task 8: Acțiuni de guvernanță — listă și detaliu

**Files:** `frontend/src/Components/Explorer/{ActionList,ActionDetail}.purs`, teste.

- [ ] **Pas 1:** teste — lista trimite mereu un filtru de îngustare; `#` din id-ul de acțiune e
  encodat `%23` în URL (altfel 400); detaliul randează `ratified_epoch`/`enacted_epoch` când există.
- [ ] **Pas 2:** FAIL. — [ ] **Pas 3:** implementează. — [ ] **Pas 4:** PASS. — [ ] **Pas 5:** commit.

---

## Task 9: Explorer de voturi

**Files:** `frontend/src/Components/Explorer/Votes.purs`, teste.

- [ ] **Pas 1:** teste — pager-ul **refuză** offset peste `votesOffsetCeiling` (600) și oferă
  filtrare în loc de adâncime, cu un mesaj care spune de ce.
- [ ] **Pas 2:** FAIL. — [ ] **Pas 3:** implementează. — [ ] **Pas 4:** PASS. — [ ] **Pas 5:** commit.

---

## Task 10: Căutare și paleta ⌘K

**Files:** `frontend/src/Components/Explorer/Search.purs`, teste.

- [ ] **Pas 1:** teste — debounce prin `MonadDelay` (nu `MonadAff`); CIP-105 lipit se convertește la
  CIP-129 **înainte** de `/profiles/by-credential/`, rezolvând `has_script` prin `/drep/{id}`; o
  credențială nevalidă produce un mesaj, nu un 400 nehandled.
- [ ] **Pas 2:** FAIL. — [ ] **Pas 3:** implementează. — [ ] **Pas 4:** PASS. — [ ] **Pas 5:** commit.

---

## Task 11: Metodologie și afordanța „ⓘ"

**Files:** `frontend/src/Components/Explorer/Methodology.purs`,
`frontend/src/Components/Shared/MetricInfo.purs`, teste.

**Interfaces:** Produces `getFormulas :: m (Either ApiError (Array Formula))` — **array gol de
envelope**, deci codec separat; `renderMetric :: Formula -> Maybe Number -> HTML`.

- [ ] **Pas 1:** teste — cele 8 `formula_id` din §2.19 se decodează; **o metrică `null` randează
  „n/a", nu „0%"** (Review Focus 4), iar tooltipul citează `null_when`; formulele se cer **o dată** pe
  sesiune și se țin în Store.
- [ ] **Pas 2:** FAIL. — [ ] **Pas 3:** implementează. — [ ] **Pas 4:** PASS. — [ ] **Pas 5:** commit.

---

## Task 12: e2e pe bundle-ul de mock

**Files:** `frontend/e2e/*.spec.js`, `frontend/playwright.config.js`.

- [ ] **Pas 1:** un spec per rută care: navighează, asertează că ecranul randează conținut (nu
  spinner), și asertează **`console.error` gol**. Rulează pe `MainDemo`, deci fără wallet, backend
  sau lanț.
- [ ] **Pas 2:** rulează, confirmă FAIL pe rutele neimplementate. — [ ] **Pas 3:** completează.
- [ ] **Pas 4:** `npx playwright test` → PASS. — [ ] **Pas 5:** commit.

---

## Task 13: Imagini

- [ ] **Pas 1:** `gh workflow run images.yml --repo mariusgeorgescu/palmares` (backend; `images.yml`
  e tag + dispatch, **nu** push pe main) și, după Task 1, `frontend.yml` se declanșează singur pe
  `frontend/**`.
- [ ] **Pas 2:** așteaptă și verifică: `gh run list --workflow images.yml --limit 1`.
- [ ] **Pas 3:** confirmă că SHA-ul nou e pe GHCR pentru cele patru imagini de backend plus
  `palmares-frontend`, prin pull anonim (`curl` pe `ghcr.io/v2/.../tags/list`).
- [ ] **Pas 4:** notează SHA-ul în FE-GOAL §9.

---

## Task 14: `apps/palmares/` și Flux

**Files:** în `apps-k8s-configs` — `apps/palmares/base/{namespace,config,policies,storage,workloads,services}`,
`apps/palmares/dev/{kustomization,patches,secrets,networking/{certificates,gateways,routes}}`,
`clusters/minirack/palmares/{palmares-dev.yaml,kustomization.yaml}`.

Șablon: `apps/bjj-belts/`. Reguli: git → commit → push → Flux, **niciodată `kubectl apply/edit`**;
nume fără cratime; prefix de tip în `metadata.name`; Gateway API nu Ingress; SOPS+age; docs în română.

- [ ] **Pas 1:** `base/` — șapte workload-uri. Plasarea e **D-28, nu se re-decide**: `dbsync` pe
  pi5-1 (`nodeSelector`, hostPath la socketul nodului preprod), `requests 4Gi`/`limits 6.5Gi`, PVC
  `pvc-palmares-dbsync-state` **10Gi**; `postgres` pe pi5-2, PVC `pvc-palmares-postgres` **50Gi**.
  `local-path` nu se extinde.
- [ ] **Pas 2:** secrete SOPS — `BASIC_USER`/`BASIC_PASS` și cele trei parole de Postgres.
- [ ] **Pas 3:** rețea — `Certificate` pentru `gov.cardano.vip` (DNS-ul **rezolvă deja**, fără
  CoreDNS), `Gateway`, `HTTPRoute` spre `service-palmares-frontend`. Fără `CORS_ALLOWED_ORIGINS`.
- [ ] **Pas 4:** validează **înainte** de commit:
  `kustomize build apps/palmares/dev | kubeconform -strict -ignore-missing-schemas -summary -`
  Expected: zero erori, zero resurse neașteptate.
- [ ] **Pas 5:** `Kustomization` Flux în `clusters/minirack/palmares/`, cu `prune: true` și
  `decryption` pe cheia proprietarului. Validează la fel.
- [ ] **Pas 6:** commit și push. Apoi `flux get kustomization palmares` → `Ready=True`.
- [ ] **Pas 7:** verifică cap-coadă: `curl -sI https://gov.cardano.vip` → 200;
  `curl -s https://gov.cardano.vip/api/query/status | jq .governance.epoch` → epoca curentă;
  lista de DRep-i randează 294 de rânduri.
- [ ] **Pas 8:** scrie în FE-GOAL §9 ce s-a livrat, și raportează ownerului.
