# Specificația funcțională — Registru de reputație & track-record pentru guvernanța Cardano

*Nume de lucru: **Palmares** (palmares = track record; alternativ: „Mandata", „GovRegistry"). Numele final se alege înainte de crearea repo-ului — decizia aparține ownerului.*

*Status: DRAFT spre aprobare — trecut prin builder-check pe 2026-10-19 (claims de reuse verificate pe ambele repo-uri; constatările integrate, vezi mențiunile „(call)"). Data: 2026-10-19. Derivat din analiza de pivot din aceeași zi (vezi memoria proiectului `pivot-analysis-2026-10`) și din deciziile ownerului: (1) pornim de la specificația funcțională cu surse de inspirație; (2) repo nou + librării extrase din Decentralized-Belt-System; (3) open-source complet; (4) proiectul se face pentru comunitate — nu pentru CF/Catalyst; nicio fereastră de finanțare nu dictează scope-ul sau calendarul. Deciziile marcate „(call)" sunt ale builderului și pot fi contestate de owner.*

---

## 1. Obiectiv și criterii de succes

**Problema.** Guvernanța on-chain Cardano (CIP-1694) mișcă sute de milioane de ADA, dar infrastructura de încredere lipsește: Intersect face manual cercetarea de track-record a proposer-ilor pentru procesul de buget; delegatorii aleg DRep-i aproape orbește (9,75 miliarde ADA parcate pe always-abstain); cele două propuneri Catalyst de „DRep reputation" nu au livrat nimic funcțional; tooling-ul existent (GovTool, Tempo.vote, drep.tools, 1694.io) arată *activitate*, nu *reputație verificabilă*.

**Produsul.** Infrastructură publică, open-source, în trei straturi:
1. **Explorer de track-record** (F1): date de guvernanță derivate independent din chain — istoric complet de voturi, delegări, participare și rationale pentru fiecare DRep, SPO (pe acțiunile unde votează) și membru CC, plus paginile acțiunilor de guvernanță și descoperirea delegării.
2. **Strat de atestări contrasemnate** (pe preprod în F1, pe mainnet în F2): profiluri on-chain revendicabile + atestări în doi pași (emitentul emite pending → subiectul contrasemnează prin acceptare) pentru livrări de milestone-uri, audituri, afilieri, mandate și andosări — pattern-ul central moștenit din Belt System. **Publicul-țintă, numit de owner (2026-10-20): Intersect, comitetele (buget, produs, CC în rolul extra-chain), comunitățile și DRep-ii, vendorii de treasury — și, dincolo de ei, *orice* vendor din ecosistem, auditorii incluși.** Vendorul plătit din bugetul unei organizații contează la fel ca cel plătit direct din treasury; diferă doar dacă există un fapt de plată on-chain de ancorat (§2).
3. **Copilot de guvernanță** (F3): server MCP + asistent AI care răspunde din datele indexate (cu citate), triază propunerile și construiește tranzacții de delegare/vot pe care utilizatorul le semnează în propriul wallet (CIP-95).

**Motivație (owner, 2026-10-19):** proiectul se construiește pentru comunitatea Cardano — public, gratuit, fără dependență de vreo finanțare și fără termene impuse de ferestre de granturi. O eventuală propunere (Catalyst/treasury) rămâne o cale opțională de sustenabilitate, ulterioară lansării — nu un criteriu de succes.

**Criterii de succes (măsurabile):**
- S1. Explorer public pe **mainnet** cu 100% din DRep-ii înregistrați și istoricul complet de voturi/delegări, verificat prin eșantionare încrucișată cu surse independente (vezi §9.1).
- S2. Orice metrică afișată e **reproductibilă**: formula publicată, datele de intrare interogabile prin API public.
- S3. Folosit de comunitate: utilizare organică recurentă a explorer-ului după lansarea publică (ordin de mărime țintit: sute de vizitatori unici în ferestrele de vot) și citări publice (forum, rationale-uri, social) din partea membrilor comunității.
- S4. În 6 luni de la lansarea pe mainnet (F2): ≥ 25 de DRep-i cu profil revendicat și ≥ 1 emitent din clasele-țintă (Intersect, un comitet de buget, o comunitate recunoscută sau un auditor) care emite atestări de livrare contrasemnate — din care ≥ 1 ancorată la o retragere de treasury reală, pentru că legătura bani ↔ livrare e proba că stratul 2 adaugă ceva ce lanțul singur nu spune.
- S5. Zero scoruri opace: niciun „rating" agregat nealgoritmic-transparent în v1 (vezi Non-goals).

## 2. Concepte și domeniu

**Entități de nivel 1 — derivate din chain, zero încredere necesară:**
- **Actor de guvernanță**: DRep (cheie sau script), SPO (în rolul de votant), membru Constitutional Committee (cheie cold/hot), adresă de stake delegatoare.
- **Acțiune de guvernanță**: cele 7 tipuri Conway (ParameterChange, HardForkInitiation, TreasuryWithdrawals, NoConfidence, UpdateCommittee, NewConstitution, InfoAction), cu ciclul de viață: propusă → votată → ratificată/expirată → pusă în aplicare.
- **Vot**: (actor, acțiune, Yes/No/Abstain, ancoră de rationale, tx, slot).
- **Delegare de vot**: (adresă stake → DRep | AlwaysAbstain | AlwaysNoConfidence, tx, slot) + puterea de vot pe epocă.
- **Certificat DRep**: înregistrare / actualizare / retragere, cu ancoră de metadata (CIP-119).
- **Ancoră de metadata**: (URL, hash) conform CIP-100/108/119/136 — se fetch-uiește, se verifică hash-ul, se stochează conținutul + verdictul de validitate.
- **Rezultat**: enacted/expired/dropped per acțiune; retrageri de treasury executate.

**Entități de nivel 2 — atestate on-chain prin protocolul propriu (F2):**
- **Profil** (CIP-68, ref + user token): identitatea revendicată a unui actor. Tipurile, numite explicit de owner (2026-10-20): **Persoană/DRep**; **Organizație** (Intersect și echivalentele ei); **Comitet** (de buget, de produs, și Constitutional Committee în rolul său extra-chain); **Comunitate** (grup recunoscut care deleghează sau andosează); **Vendor** — *orice* furnizor din ecosistem, nu doar cei plătiți direct din treasury; **Auditor**. Revendicarea unei identități on-chain (DRep ID, cheie cold de CC, adresă de stake) cere **dovada că poți cheltui ca acel credential**: extra signatory pentru credentiale de cheie, iar pentru credentiale de script consumarea unui UTxO de la acea adresă în tx-ul de claim. E o regulă unică tocmai pentru că majoritatea comitetelor și organizațiilor acționează prin multisig/script, nu prin cheie simplă — iar autoritatea în protocol e **posesia user token-ului**, care funcționează identic pentru chei și pentru scripturi (o listă de semnături nu ar funcționa).
- **Atestare**: afirmație emisă de un profil despre **alt profil sau despre o altă atestare** — cazul auditorului: „am auditat livrarea atestată aici" — în doi pași (mint pending → acceptare de către subiect), permanentă după acceptare, cu tipuri: `MilestoneDelivered` (administratorul de buget atestă livrarea către un vendor), `AuditPassed`, `Affiliation` (organizație ↔ membru, cu intervale), `RoleHeld` (mandat extra-chain: lead de working group, președinte de comitet — cu intervale), `Endorsement`. Metadata arbitrară (cheie/valoare + ancoră IPFS) ca la Achievements.
- **Ancoră de nivel 1 pentru o atestare (opțională)**: o atestare poate referenția faptul on-chain pe care îl comentează — o retragere de treasury (`gov_action_id` + adresa de stake plătită, ambele deja în `palmares.treasury_withdrawal`) sau o acțiune de guvernanță. Asta leagă **banii de livrare**: lanțul spune „adresa X a primit N ADA sub acțiunea G", atestarea contrasemnată spune „comitetul plătitor confirmă milestone-ul 2, iar vendorul e de acord". Rămâne opțională fix pentru că vendorii plătiți din bugetul unei organizații, nu direct din treasury, nu au un fapt de plată on-chain — și ei intră în registru.

**Regulă de delimitare (owner, 2026-10-20):** nu atestăm ce lanțul deja dovedește. Apartenența la Constitutional Committee, voturile, delegările și plățile din treasury sunt **nivel 1** și se citesc din db-sync. Nivelul 2 acoperă exclusiv ce nu există pe lanț: livrare, audit, afiliere, mandat, andosare.
- **Contestație (Dispute)**: token care referențiază o atestare/un profil, cu motiv ancorat IPFS și depozit anti-spam; rezolvată de un mecanism de arbitraj (v1: adminul oracle-ului; vezi riscurile). Preia ideea `DisputeValidator` din roadmap-ul din iulie 2026.

**Modelul de încredere, pe straturi:** nivelul 1 nu cere încredere în noi (oricine poate re-deriva datele rulând de la genesis software open-source: cardano-node + db-sync + acest repo); nivelul 2 cere încredere în semnatari, nu în platformă (ambele semnături sunt on-chain); stratul AI (F3) citează întotdeauna sursele de nivel 1/2 și nu emite judecăți proprii nefondate.

## 3. Moduri de operare și granițe de capabilitate

- **Vizitator (fără wallet)**: tot explorer-ul, toate API-urile de citire, export. Publicul principal.
- **Utilizator cu wallet (din F1, pe preprod; pe mainnet din F2)**: revendică profil, emite/acceptă atestări, deleghează și votează din copilot (F3) — întotdeauna prin build-pe-server → CBOR nesemnat → semnare CIP-30/CIP-95 în wallet-ul propriu → submit. Niciodată custodie de chei.
- **Admin**: deploy, oracle (pauză, parametri, rotația cheii — obligatorie de data asta), arbitraj dispute v1.
- **Granițe explicite**: sistemul nu votează în numele nimănui, nu recomandă algoritmic un DRep „cel mai bun", nu agregă scoruri opace, nu găzduiește discuții (linkuim spre forum/X), nu face KYC.

## 4. Cerințe funcționale per componentă

### 4.1 Stratul de date de guvernanță: cardano-db-sync + proiecții proprii (decizie revizuită 2026-10-19 — vezi Anexa)

Kupo vede doar UTxO-uri; certificatele, voturile și acțiunile de guvernanță trăiesc în corpul tranzacției și în ledger state. Analiza tehnologică din 2026-10-19 a înlocuit planul inițial („follower Ogmios propriu + snapshot-uri de ledger state la granițe de epocă") cu:

- **(call, revizuit 2026-10-19) Substratul de indexare: cardano-db-sync 13.7.x** cu presetul `only_governance` și ledger-ul ACTIVAT (obligatoriu — `ledger: disable` golește `drep_distr`). Un sync de la genesis populează nativ tot ce cere F1: `drep_distr` (puterea de vot per DRep **per epocă**, pentru toate epocile Conway trecute), `gov_action_proposal` cu epocile de ratificare/enactare/expirare/abandon, `voting_procedure`, `delegation_vote`, `drep_registration`, `committee`, `treasury_withdrawal`. Asta elimină complet schema fragilă a snapshot-urilor la granițe de epocă în timpul sync-ului — aceea ar fi însemnat reimplementarea solo a buclei centrale din db-sync, exact partea în care contributorii și auditorii ar fi avut cea mai puțină încredere. Precedent: GovTool însuși e un API peste db-sync.
- **Serviciul nostru Haskell devine stratul de proiecții + API** peste Postgres-ul db-sync: transformări deterministe, re-rulabile, din tabelele db-sync în tabelele noastre de query (metrice, agregate, pagini compozite). Independența se afirmă precis: „oricine poate re-deriva datele rulând software open-source — cardano-node + db-sync + acest repo — de la genesis".
- **Protocolul de atestări își păstrează pipeline-ul existent**: validatoarele noastre produc UTxO-uri cu token-urile policy-ului propriu, deci chain-sync-ul pe Kupo din Belt System (cursor, raw log idempotent, replay ordonat) se refolosește ca atare pentru proiecțiile de atestări — zero tehnologie nouă pe acest drum.
- **(call) Cross-check**: job periodic de reconciliere cu **Koios** (`drep_voting_power_history`, `proposal_list`, `vote_list`, `drep_epoch_summary`) care alertează la divergențe — Koios rulează tot pe db-sync, deci e o verificare gratuită în CI, nu o sursă.
- **Metadata fetcher**: pentru fiecare ancoră — fetch cu timeout și limită de mărime, verificare blake2b-256, stocare conținut + status (`valid | hash_mismatch | unreachable | oversized`); re-încercări exponențiale; cache permanent re-derivabil.
- **Mini-follower Ogmios: tăiat definitiv** (toate cele 5 piste de design, 2026-10-19 — pe motive de lene: polling-ul pe db-sync acoperă activity feed-ul, iar un al doilea drum de date ar trebui întreținut și reconciliat).
- **Cerințe moștenite (fixuri obligatorii în codul extras)**: timestamps = timp de chain (slot→UTCTime), nu `insertedAt` (problema #5 din analiza din oct 2026); orice proiecție bazată pe datum-uri verifică prezența token-ului numit în datum în valoarea UTxO-ului (problema #3, „projection poisoning") — se aplică proiecțiilor de atestări (F1).

### 4.2 Query API (reuse: scheletul query-api + webapi-lib)

- **(call) Public, fără Basic auth** (spre deosebire de Belt System): e infrastructură publică; protecție prin rate-limit la BFF + cache HTTP. Swagger auto-generat, ca acum.
- **Profil DRep** (`/drep/{id}`): identitate (metadata CIP-119 verificată), status, istoric complet de voturi cu rationale, metrice derivate (vezi mai jos), puterea de vot pe epoci, delegatori (număr, churn), timeline unificat (pattern-ul din Belt System).
- **Metrice v1 — doar fapte reproductibile, cu formula publicată**: rată de participare (acțiuni votate / acțiuni în care era eligibil să voteze), rată de rationale (voturi cu ancoră validă / total voturi), vechime, consecvență delegatori (retenție pe epoci), latență de vot (timp propunere→vot), distribuția voturilor pe tip de acțiune. Explicit FĂRĂ scor compozit.
- **Acțiuni de guvernanță** (`/gov-action/{id}`, `/gov-actions`): detaliu + voturi agregate pe grup (DRep/SPO/CC) cu praguri și progres, metadata CIP-108, rezultat, cine a propus (+ istoric al proposer-ului).
- **Descoperirea delegării** (`/dreps` cu filtre): activ/inactiv, participare, rată de rationale, putere de vot, vechime, limbă/metadata declarate; sortări multiple; paginare limit/offset (convențiile existente).
- **Comitete & SPO ca votanți**: pagini echivalente minimale.
- **Vendori/treasury (din date publice)**: pagina unei retrageri de treasury → beneficiar, sumă, acțiunea-mamă; în F2 se leagă de profilul revendicat și de atestările de milestone.
- **Agregate & pagini compozite**: dashboard de rețea (DRep-i activi, stake delegat vs abstain, acțiuni în zbor, participare pe epocă), activity feed cu `since`, search unificat, `/pages/*` compozite pe ecran (pattern-ul existent — frontend-ul nu face N+1).
- **Export**: CSV/JSON pe orice listă (cerință a cercetătorilor și jurnaliștilor).

### 4.3 Stratul de atestări (on-chain; în F1 pe preprod, pe mainnet în F2; reuse masiv din engine)

- **Profiluri CIP-68** cu user token ca purtător de autoritate (exact ca în Belt System), tipuri: Persoană/DRep, Organizație, Comitet, Comunitate, Vendor, Auditor (vezi §2 — publicul-țintă e ecosistemul Intersect/comitete/comunități/vendori/auditori, nu doar DRep-ii).
- **Claim de identitate on-chain**: regula unică din §2 — „dovedește că poți cheltui ca acel credential". Pentru credentiale de cheie: extra signatory în tx-ul de claim. Pentru credentiale de script (cazul normal la comitete și organizații multisig): tx-ul de claim consumă un UTxO de la adresa acelui script, deci condiția de spend a scriptului a fost satisfăcută. Se aplică identic pentru DRep ID, cheie cold de CC și adresă de stake; legătura identitate ↔ profil devine fapt on-chain. **Asta înlocuiește nota „script-DReps: detaliu la builder"** — nu mai e un detaliu, e cazul majoritar.
- **Atestări în doi pași**: mint pending de către emitent (cu user token-ul emitentului) → acceptare de către subiect (cu user token-ul subiectului) → permanentă, înlănțuită (lineage de livrări: milestone 1 ← milestone 2 …). Subiectul poate fi un profil **sau o atestare** (auditul unei livrări). Reuse direct al pattern-ului Promotion/Achievement, fără ierarhie de „centuri" — autoritatea nu e un rang, ci identitatea verificabilă a emitentului.
- **Ancoră opțională la nivelul 1**: datumul atestării poartă opțional `(gov_action_id, stake_address)` al retragerii de treasury sau al acțiunii vizate (§2). Validatorul nu verifică existența faptului on-chain — e un pointer, confruntat la citire cu `palmares.treasury_withdrawal`, iar API-ul marchează explicit dacă pointerul se rezolvă sau nu. **(call)**: alternativa — verificare on-chain prin reference input — ar cere ca retragerea să fie un UTxO adresabil, ceea ce nu e, deci rămâne verificare la citire.
- **Dispute**: token de contestație cu depozit, referință la atestarea vizată, motiv ancorat; verdictul (v1) de la adminul oracle, afișat lângă atestare fără a o ascunde.
- **Oracle**: pauză, min-UTxO, parametri de depozit + **rotația cheii de admin** (fix al problemei #2) și, dezirabil, admin multisig nativ (M-din-N; decizie la builder după estimarea de ExUnits — respectă principiul „no ExUnits for non-exploitable defense").
- **FĂRĂ fee-uri de protocol ca model de business** (lecția JPG Store); depozitele anti-spam se returnează. Finanțarea = granturi/buget, nu taxare per-tx.
- **Validatoare**: PlutusTx/Plinth, filozofia existentă „onchain minimal, mirror-and-pin" + regulile din `src/lib/onchain-lib/CLAUDE.md`; property-test care pinuiește regula off-chain ≡ on-chain, ca acum. Fixul problemei #1 (selector de fee greșit) și #4 (date nelegate de validity range) se tratează prin design: orice dată purtată în datum e validată contra intervalului de validitate al tranzacției.
- **Desfășurare**: preprod întâi, mainnet după ≥ 1 lună de soak + review extern al validatoarelor.

### 4.4 MCP Server + Copilot (F3; reuse: mcp-server-lib + agent-service)

- **Read tools** peste tot API-ul de query (echivalentele celor 15 existente, plus `get_drep_track_record`, `get_gov_action_summary`, `compare_dreps`, `get_delegation_options`).
- **Write tools** (gated prin env, ca acum): `build_delegate_tx` (delegare de vot către un DRep), `build_vote_tx` (pentru DRep-i, prin CIP-95), `build_claim_profile_tx`, `build_attestation_tx`, `build_accept_attestation_tx` — toate întorc `{cbor_hex}` pentru semnare în wallet; puntea de semnare din chat (checkpoint Redis + `tx_action` SSE) se refolosește ca atare. Verificat 2026-10-19: Atlas 0.14.1 (versiunea pinuită) suportă deja certificatele DRep, delegarea de vot și `VotingProcedures` în `GYTxSkeleton`, deci aceste tool-uri se construiesc cu stack-ul TxBuilding existent, fără muncă nouă de SDK.
- **(decizie amânată la planul F3, de adus la owner atunci)**: expunerea copilotului (anonim vs. gated pe wallet vs. quote per IP — un endpoint LLM public pe un site gratuit e o suprafață reală de cost și abuz) și providerul de inferență (agent-service permite deja schimbarea base-URL-ului OpenAI → vLLM/on-prem; pentru un tool public de guvernanță, alegerea providerului e o decizie de încredere, nu doar de cost).
- **Copilot**: (a) rezumate de propuneri STRICT cu citate din metadata on-chain + linkuri (conținut politic ⇒ fiecare afirmație trasabilă la sursă); (b) „ce s-a schimbat de la ultimul tău vot"; (c) triaj: propunerile nevotate încă de DRep-ul conectat, ordonate după deadline; (d) Q&A pe track-record. Guard-rails moștenite din agent-service (sanitizare anti-injection, reflection, limite) + un **policy gate** pe write tools (inspirat din defi-bot): niciun build de tx de vot fără confirmarea explicită a opțiunii de vot în conversație.
- **Resurse MCP**: ghid CIP-1694 pe scurt, glosarul, formulele metricelor (echivalentul `bjj://rules/*`).

### 4.5 Frontend (TypeScript + React + Mesh; decizie revizuită 2026-10-19 — vezi Anexa)

- **(call, revizuit 2026-10-19)** Frontend nou în **TypeScript + React** (Vite), cu **Mesh 1.9.x** pentru wallet connect și **CIP-95** (necesare acțiunilor de guvernanță; suportat azi de Eternl/Lace/Typhon). Motivare: proiectul e pentru comunitate, iar bazinul de contributori e React/TS — GovTool și toate tool-urile pereche sunt React/TS; Mesh înlocuiește gratuit bibliotecile noastre PureScript de wallet, inclusiv partea CIP-95 pe care altfel am scrie-o noi. Se refolosesc **tiparele**, nu codul PureScript: pagini compozite per ecran, stări RemoteData cu skeleton + retry, filtre/paginare/parametri în URL, puntea chat→semnare, wizard-ul în 4 pași. Bibliotecile PS rămân la Belt System.
- **Explorer-first**, aceleași tipare: Hub, Dashboard, listă DRep-i cu filtre+paginare+URL params, pagină DRep cu timeline unificat, pagini de acțiuni, **graful delegărilor/atestărilor** (cytoscape.js, același motor ca în BJJ), ⌘K search, activity feed, protocol-status.
- **Claim & attest wizard**: pattern-ul Action Composer (Select → Fill → Review → Track), reimplementat în React, cu formulare noi.
- **Verificare**: VerificationChip/Panel parametrizate pe `credentialKind` (pattern existent), link-uri de explorer + tx-proof; widget embed „Verified track record" pentru site-urile DRep-ilor (itemul E12 din backlog-ul BJJ își găsește aici publicul real).
- **Copilot drawer** (F3): pattern-ul Chatbot + aceeași punte de semnare (checkpoint + `tx_action`), reimplementate; în plus față de BJJ: card de semnare cu **sumar decodat al tranzacției** (fix al riscului de blind-signing semnalat în analiza frontendului).
- **Cerințe moștenite (fixuri)**: network configurabil la build (nu hardcodat), fără chei de provider în bundle, BFF cu rate-limit.

### 4.6 Admin & operare

- CLI de admin pe modelul existent: deploy-reference-scripts, write-blueprint (CIP-57), oracle ops (+ `rotate-admin`), arbitraj dispute.
- Deploy: Docker images + k8s/Flux pe minirack (topologia existentă), docker-compose pentru dev/audit; totul self-hostable de terți (criteriu de credibilitate).

## 5. Fluxul de date

```
Nod Cardano (preprod în F1, mainnet din F2) ──▶ cardano-db-sync (only_governance, ledger ON) ──▶ Postgres db-sync
                                                                        │
                                   Koios (cross-check periodic, în CI) ──┤
                                                                        ▼
        Proiecții + metrice (serviciu Haskell; deterministe, re-rulabile) ◀── Metadata Fetcher (ancore CIP-100/108/119)
                                                                        │
Validatoare atestări ──▶ Kupo (pattern-ul Belt System: raw log + replay) ─┤
                                                                        ▼
                                       Query API (public, rate-limited) ──▶ Frontend explorer / Export / terți
                                                                        └──▶ MCP Server ──▶ Copilot (citate obligatorii)
Atestări (F1 preprod → F2 mainnet): Interaction API ──build──▶ CBOR nesemnat ──▶ wallet user (CIP-30/95) ──▶ submit ──▶ chain
```

## 6. Modelul de date (proiecții cheie)

- `drep` (id, tip credential, status, ancoră+verdict, slot reg/retire), `drep_update_log`
- `vote` (actor_type, actor_id, gov_action_id, verdict, ancoră+verdict, tx, slot)
- `gov_action` (id, tip, proposer (reward account), depozit, ancoră, stare, epoci cheie, rezultat)
- `vote_delegation` (stake_addr, target, tx, slot), `drep_epoch_power` (drep, epocă, lovelace, nr. delegatori)
- `committee_member`, `committee_vote_auth`, `treasury_withdrawal`
- `anchor_content` (url, hash, status, conținut, fetched_at)
- Atestări (F1): `profile_projection`, `attestation_projection`, `dispute_projection` (adaptate din cele existente, cu tx_hash/slot/output_index ca acum)
- Bookkeeping (doar pipeline-ul de atestări): `chain_cursor`, `raw_event_log`, `schema_version` — identice ca mecanism cu Belt System; stratul de guvernanță își ia consistența de la db-sync, iar proiecțiile noastre peste el sunt transformări deterministe re-rulabile.

## 7. Cerințe non-funcționale

- **Open-source complet, Apache-2.0 (call)**, repo public de la primul commit; fără secrete în istorie (lecția token-ului Maestro).
- **Reproductibilitate**: oricine poate rula stack-ul și obține aceleași cifre; formulele metricelor sunt documentate în repo și expuse ca resursă MCP.
- **(call) Calea de scriere (build-tx) folosește un provider comercial** (Maestro, exact ca în Belt System azi — incompatibilitatea Atlas↔Ogmios v7 persistă), cu cheia injectată din env, niciodată în repo. „Self-hostable" se promite precis: calea de citire (indexer + API + UI) e 100% autonomă pe nod propriu; calea de scriere cere cheia proprie de provider (free tier suficient pentru volume mici). Restaurarea compatibilității Atlas↔Ogmios v7 se consemnează ca workstream viitor ne-blocant (ADR la F0). De evaluat la plan: providerii **local-node** sau **db-sync** ai Atlas — db-sync oricum rulează (§4.1), deci dependența de Maestro ar putea dispărea complet.
- **Corectitudine la rollback**: identică cu garanțiile actuale (replay din raw log); timpul afișat = timp de chain.
- **Disponibilitate**: explorer public servit prin BFF cu cache; indexer-ul poate rămâne în urmă max. N sloturi înainte ca `/ready` să pice (pattern-ul existent).
- **Fără date personale** dincolo de ce e deja public on-chain/în ancore; cache-urile de metadata sunt re-derivabile și șterse la cerere doar dacă și sursa a dispărut (de detaliat la plan).
- **Haskell/devx Nix** ca acum; CI cu `cabal build all` + `cabal test` + teste de paritate pe fixture-uri.

## 8. Presupuneri și dependențe

- F1 rulează pe **preprod** (decizia ownerului, 2026-10-19): e nevoie de un nod preprod + **cardano-db-sync** (`only_governance`, ledger ON) în minirack — încape pe Pi (≈16 GB Postgres + ≈12 GB node DB, ~3,5 GB RAM db-sync cu backend-ul clasic de ledger); bootstrap de nod prin Mithril (procedură exersată în iulie 2026). **Mainnet (F2) are un prag hardware real**: db-sync mainnet cere o mașină x86_64 cu 48–64 GB RAM și ~1 TB NVMe (backend-ul LSM experimental e documentat ca riscant pe Raspberry Pi) — nu promitem mainnet pe minirack; se bugetează o mașină dedicată la F2.
- **Versiuni pentru repo-ul nou** (fixate după fact-check, 2026-10-19): cardano-node 11.1.2; **cardano-db-sync 13.7.1.0** — imaginea arm64 `ghcr.io/blinklabs-io/cardano-db-sync:13.7.1.0` pinuită prin digest (NU 13.7.2.1: nu există build arm64 pentru ea); Postgres 17; GHC 9.6.6 (pin impus de plutus-tx-plugin); Atlas 0.14.1; Plutus/Plinth 1.7x; FE: React + Vite curente, Mesh 1.9.x. Registry de imagini: **GHCR**, `ghcr.io/<owner>/palmares-<svc>`. Fără bump-uri de pin în timpul extragerii.
- Postgres-ul și pipeline-ul k8s/Flux existente.
- **Librării extrase din Decentralized-Belt-System** (repo nou le consumă ca librării locale/sub-tree; mecanismul exact la plan): `webapi-lib` integral; mecanismul `Storage`/raw-log/replay; scheletul `TxBuilding` (Context, Skeletons, Transactions, partea de oracle/fee/validity din Operations); plumbing-ul MCP; scheletul query-api (backend dual, paginare, pages); din frontend: bibliotecile de wallet (deja repo-uri separate), shell-ul de explorer (RemoteData, filtre, paginare, ⌘K, feed), BFF-ul, agent-service-ul.
- **Fixurile moștenite** se fac în codul extras înainte de prima lansare: #1 selector fee (pattern), #2 rotație admin, #3 token-check la proiecții, #4 date legate de validity range, #5 chain-time.
- **Tooling de dezvoltare**: plugin `cardano-dev-skills` (deja activ) + cele 13 skills vendorate azi din defi-bot în `.claude/skills/` (cardano-cli family, plutus-v3-conway, cardano-protocol-params, koios-agent-wallet dezactivat; proveniență și hardening în `.claude/skills/VENDORED.md`) — se copiază în repo-ul nou la bootstrap.
- Belt System **îngheață** pe preview ca demo/vertical istoric; nu se mai dezvoltă activ (decizia ownerului, 2026-10-19).

## 9. Criterii de acceptanță pe faze

### F0 — Bootstrap (săptămâni, nu luni)
- Repo nou creat (nume ales de owner), Apache-2.0, CI verde, librăriile extrase compilează, fixurile #1–#5 aplicate în codul extras și testate.
- Ieșire: `cabal build all` + teste verzi pe scheletul gol + ADR `0001-governance-data-substrate.md` care fixează sursa de date (cardano-db-sync, preset `only_governance`, ledger ON) cu măsurătorile din Experimentul A, și ADR `0002-write-path-provider.md` pentru calea de scriere.

### F1 — Produs complet pe PREPROD (goal-ul implementării, fixat de owner 2026-10-19; lărgit tot atunci la cererea ownerului: include atestările on-chain și UI-ul complet)

**GOAL**: pe **preprod** rulează produsul întreg, cap-coadă: stratul de date de guvernanță (cardano-db-sync `only_governance` → view-uri `palmares.*` + o singură matview) + protocolul de atestări on-chain (profiluri revendicabile, atestări contrasemnate, dispute) + Query API public + **UI complet** — cu corectitudine demonstrată prin paritate externă, idempotență la re-sync și teste de emulator pe validatoare, și cu fluxul de utilizator demonstrabil de la claim până la verificare publică. Mainnet (F2) nu mai cere apoi schimbări de cod la indexer/API/UI, doar config, resurse și redeploy-ul validatoarelor după review.

Ordinea internă de construcție rămâne: strat de date → protocol on-chain → UI — dar faza e „done" doar cu toate checks-urile trecute. Ordinea exactă a task-urilor și dependențele dure sunt în `docs/superpowers/design/f1-synthesis.md` §3.

**A. Stratul de date de guvernanță** (fiecare pas e „done" doar cu verificarea lui trecută):
1. Nod preprod + cardano-db-sync (`preset: only_governance`, `ledger: enable`, `shelley: enable: true`, `offchain_vote_data: enable`) sincronizate **de la genesis** — iar „de la genesis" e o cerință de **corectitudine**, nu doar de completitudine: db-sync issue #1975 documentează două instanțe identice care diferă cu ~740× pe puterea de vot a unui DRep, iar Known Issue-ul 13.6.0.4 numește explicit preprod („valorile `drep_distribution` de dinaintea upgrade-ului nu sunt bine definite"); remediul documentat e exact recalcularea de la genesis cu fișiere de ledger-state proaspete. Un db-sync restaurat din snapshot ar moșteni valori prost definite exact în coloana pe care se sprijină tot produsul → verify: db-sync la tip, `lag_seconds` din `/status` < 120s susținut pe o fereastră de soak; `drep_distr` populat pentru toate epocile Conway de pe preprod; `stake_deregistration` și `pool_update` nevide (dovada că flag-ul `shelley` a prins — altfel se plătește un re-sync complet).
2. View-urile `palmares.*` acoperă tot: DRep-i (reg/update/retire), delegările de vot, comitete (cu rezolvarea hot→cold), gov actions cu ciclul de viață complet (ratified/enacted/expired/dropped), voturile (toate 3 rolurile), retragerile → verify: count > 0 pe fiecare tip de entitate + eșantioane confruntate manual cu un explorer public de preprod.
3. Determinism (restated 2026-10-19 — stratul de guvernanță e view-uri, nu un serviciu de proiecție, deci determinismul e prin construcție): rularea de două ori a SQL-ului din catalogul de metrice peste același db-sync dă ieșire identică → verify: hash pe ieșire, identic între rulări. **Nu se construiește un harness de determinism pentru un `SELECT`.**
4. Rollback safety (restated): rollback-ul e tratat de db-sync însuși, iar view-urile îl reflectă automat → verify: pentru pipeline-ul de atestări (singurul cu stare proprie), testul de replay existent (pattern-ul chain-sync) adaptat și verde.
5. Puterea de vot pe epocă expusă din `drep_distr` prin `palmares.drep_epoch_power` (cu `target_kind`, incluzând pseudo-DRep-ii always-abstain / always-no-confidence) → verify: suma puterilor ≈ totalul delegat raportat de Koios preprod la aceeași epocă (toleranță documentată).
6. **Paritate externă**: pe ≥ 20 DRep-i și ≥ 10 gov actions de pe preprod, voturile/delegările coincid cu Koios preprod → verify: script de reconciliere cu 0 divergențe neexplicate (divergențele explicate se documentează).
7. Ancore de metadata: **fetcher-ul propriu e tăiat** — db-sync (`offchain_vote_data: enable`) fetch-uiește și verifică hash-ul el însuși; noi doar clasificăm rezultatul → verify: fiecare ancoră are verdict din `palmares.anchor` (`valid | invalid_schema | hash_mismatch | oversized | unreachable | inline | not_fetched`), cu test golden pe șirurile exacte de eroare; verdictul e vizibil în API.

**B. Protocol de atestări on-chain (preprod):**
8. Validatoare PlutusTx (profiluri CIP-68 cu claim de identitate on-chain pentru **ambele** feluri de credential — cheie și script —, atestări bifazice cu subiect profil sau atestare, dispute, oracle cu rotație de admin) → verify: suită de emulator pe pattern-ul `mkTestFor` cu acoperire pe căile negative, inclusiv un claim de script-credential reușit și unul respins (UTxO-ul consumat nu e de la adresa revendicată), property-test on-chain ≡ off-chain, blueprint CIP-57 generat.
9. Fixurile moștenite aplicate în codul extras (#1 selector de fee, #2 rotație admin, #3 token-check la proiecții, #4 date legate de validity range, #5 chain-time) → verify: câte un test dedicat per fix, care pică pe comportamentul vechi.
10. Deploy pe preprod: reference scripts + oracle → verify: comanda de deploy produce configul validatorilor, `/ready` pe interaction-api verde.
11. Fluxuri complete prin interaction-api: claim de profil pe ambele căi de credential (cheie: semnătură suplimentară; script: consum de UTxO de la adresa revendicată) → emitere atestare (pending), cu și fără ancoră de treasury → contrasemnare (accept); dispute deschis → rezolvat → verify: scripturi de smoke cap-coadă pe preprod (pattern-ul populate/test_*.sh), iar proiecțiile rezultate apar corect în query-api, cu tx-proof; atestarea cu ancoră arată în API retragerea de treasury corespunzătoare din `palmares.treasury_withdrawal`, iar una cu ancoră nerezolvabilă e marcată ca atare, nu ascunsă.

**C. Query API + UI complet (preprod):**
12. Query API public (fără Basic auth, rate-limited) cu Swagger: `/dreps` (filtre+paginare), `/drep/{id}` (istoric complet + metrice cu formulele publicate + atestări), `/gov-actions`, `/gov-action/{id}`, profiluri/atestări/dispute, `/activity`, search, agregate dashboard, export CSV/JSON → verify: teste pe handler-e + smoke script pe stack-ul de preprod.
13. Toate paginile explorer: hub, dashboard, listă DRep-i cu filtre/paginare/parametri în URL, pagină DRep completă (timeline unificat + metrice + delegatori + atestări), pagini gov actions, graf de delegări/atestări (Cytoscape), search ⌘K, activity feed, export → verify: Playwright pe fiecare rută + testele unit FE verzi.
14. Fluxurile cu wallet în UI: claim & attest wizard (pattern-ul Action Composer, în React + Mesh/CIP-95), accept/dispute, card de semnare cu **sumar decodat al tranzacției** (fără blind-signing) → verify: e2e Playwright pe preprod cu wallet de test — tranzacție construită, semnată, confirmată și reflectată în UI.
15. Verificare publică: VerificationChip/Panel pe toate entitățile atestate, linkuri tx-proof, widget embed „verified" → verify: widget-ul randează corect într-o pagină externă de test.
16. Deploy reproducibil al întregului stack (BE + FE + BFF): docker-compose pentru dev/audit + overlay k8s preprod pe minirack → verify: Flux ready=True, `/ready` verde pe toate serviciile, totul GitOps-clean.

Interpretare (call, de contestat de owner): „UI complet" = tot ce ține de explorer + atestări (lista de mai sus); copilotul AI și serverul MCP rămân în F3 și nu intră în acest goal.

Non-goals ale fazei F1: fără mainnet, fără MCP/copilot.

### F2 — Promovare pe mainnet
- Nod + db-sync mainnet pe o mașină dedicată x86_64, 48–64 GB RAM, ~1 TB NVMe ((call) dimensionarea exactă la plan; Mithril pentru bootstrap) — explicit NU pe minirack; indexer/API/UI: același cod, config nou.
- Re-rularea checks 2–7 pe mainnet, cu eșantion mărit (≥ 30 DRep-i, ≥ 20 acțiuni) și cross-check suplimentar față de GovTool; spot-check uman: track-record-ul a 3 DRep-i cunoscuți confirmat de cineva care îi urmărește.
- Validatoarele de atestări: redeploy pe mainnet doar după ≥ 1 lună de soak pe preprod + review extern (cerința din §4.3).
- Explorer public live (lista completă din check 13) + API documentat.
- Lansare publică către comunitate: anunț pe forum Cardano + social cu metodologia și API-ul documentate; feedback-ul colectat devine backlog-ul F3 (S3).

### F3 — Copilot
- MCP read tools cu paritate 1:1 față de API (testele de paritate există ca pattern); write tools gated.
- Copilotul răspunde la 20 de întrebări-etalon cu citate corecte (eval set în repo); zero tranzacții construite fără confirmare explicită a intenției; sumarul decodat al tx-ului apare în cardul de semnare.

## 10. Non-goals (explicit în afara sistemului)

- Scoruri compozite/opace de reputație sau clasamente „best DRep" în v1 (doar fapte + formule publice).
- Vot sau delegare automată; custodie de chei; semnare server-side.
- Forum/discuții, moderare de conținut, KYC.
- DAO propriu al platformei; tokenomics; fee-uri de protocol ca venit.
- Midnight, SSI orizontal, registru de agenți AI (respinse în analiza de pivot din 2026-10-19).
- Catalyst ca domeniu indexat în v1 (doar guvernanța on-chain Conway; Catalyst are API-uri proprii și poate veni ca extensie).

## 11. Surse de inspirație (cerute explicit de owner)

**În ecosistemul Cardano** — de studiat înainte de planul F1, în această ordine:
- **GovTool** (gov.tools; mentenanță preluată de Sireto, cod open-source) — referința funcțională minimă; ce să NU repetăm: lipsa istoricului agregat per DRep.
- **Tempo.vote**, **drep.tools**, **1694.io** (Lido Nation), hub-urile de guvernanță **Cexplorer**/**AdaStat** — ce afișează, ce lipsește (reputația atestată, metrice reproductibile, API public).
- **Procesul manual Intersect** de track-record al proposer-ilor (forum, 2026) — lista exactă de întrebări pe care și le pun e backlog-ul nostru de metrice.
- **Koios** (endpoints de guvernanță) și **cardano-db-sync** (schema tabelelor de guvernanță) — ca referință de completitudine a datelor; **CIP-1694, CIP-95, CIP-100/108/119/136** — contractele de date.
- **Andamio** — singurul protocol viu de credențiale pe Cardano; de studiat modelul lor de access-token.

**În afara ecosistemului:**
- **Karma (karmahq.xyz)** — reputație de delegați pentru DAO-uri EVM; cel mai apropiat produs de ce construim; de studiat ce metrice afișează și cum tratează gaming-ul.
- **Tally** și **Agora** (Optimism) — paginile de delegați + delegation discovery UX; **Boardroom** — agregare multi-DAO; **DeepDAO** — analytics; **L2BEAT** — etalonul de onestitate în prezentarea datelor (badge-uri de risc, metodologie publică); **Snapshot** — ancorarea rationale-urilor.
- **ERC-8004 post-mortems** (arXiv 2026) — de ce registrele de reputație fără contrasemnătură și fără skin-in-the-game eșuează (Sybil); justifică designul nostru bifazic.

**Din repo-urile proprii:**
- `Decentralized-Belt-System` — consimțământul bifazic, lineage-ul, eligibility engine, mirror-and-pin, replay-ul idempotent.
- `defi-bot` — formatul acestei specificații, disciplina ADR-urilor, pattern-ul de **policy gate** pe acțiuni cu bani, hardening-ul de skills vendorate.
- `bjj-frontend` — shell-ul de explorer, puntea chat→semnare, Action Composer.

## 12. Glosar minimal

DRep = reprezentant delegat de vot (CIP-1694); CC = Constitutional Committee; gov action = propunere on-chain; ancoră = (URL, hash) spre metadata off-chain; atestare = afirmație on-chain contrasemnată de subiect; claim = legarea unui credential de guvernanță de un profil CIP-68; Chang = hardfork-ul care a activat Conway/guvernanța.

## 13. Anexă: analiza tehnologică (2026-10-19) — verdict pe straturi

Cercetare la zi făcută la cererea ownerului („alte tehnologii mai potrivite? upgrade la librării?"), cu verdictul aplicat în secțiunile de mai sus:

| Strat | Verdict | Alegerea | De ce (o linie) | Cost migrare |
|---|---|---|---|---|
| Indexare guvernanță | **Înlocuit** | cardano-db-sync **13.7.1.0** (arm64), preset `only_governance`, ledger ON. **Yaci-Store: respins ca substrat** (reevaluat 2026-10-19, `design/f1-yaci-store-evaluation.md`) — `drep_dist`-ul lui depinde de store-ul UTxO, deci e mai *greu*, nu mai ușor; fără ENACTED/DROPPED în lifecycle; fără fetch/verificare de ancore; partiționarea lui ne-ar sparge view-urile. Rămâne doar ca eventual cross-check (a doua derivare independentă). | `drep_distr` per epocă + ciclul de viață al gov actions vin nativ, de la genesis; follower-ul propriu ar fi reimplementat solo bucla centrală a db-sync | M |
| Limbaj on-chain | **Păstrat** | PlutusTx/Plinth (Plutus 1.72; echipa mutată la Midgard Labs, care întreține și Aiken/Plutarch) | 3k linii testate + property-pinning on≡off + harness CLB bat o rescriere în Aiken; blueprint-urile CIP-57 servesc contributorii non-Haskell | S |
| Off-chain Haskell | **Păstrat** | Atlas 0.14.1 (deja ultimul release; gov tx-building există din 0.8.0) | Nicio alternativă Haskell mai bine întreținută; Ogmios v7 rămâne incompatibil — provider Maestro sau (de evaluat) local-node/db-sync | S |
| Frontend | **Înlocuit** | TypeScript + React (Vite) + Mesh 1.9 (CIP-95) | Proiect comunitar ⇒ bazinul de contributori React/TS (GovTool și toate tool-urile pereche sunt React/TS); Mesh dă CIP-95 gratis | L |
| Nod/infra | **Upgrade** | node 11.1.2, db-sync 13.7.2.1, Mithril, Postgres 17 | Pin-uri de rutină; preprod încape pe minirack, mainnet nu (vezi riscuri) | S |
| Agent (F3) | **Păstrat** | Python + LangChain 1.x, LangGraph la F3 | Faza cea mai puțin construită; model-agnostic rămâne scopul | S |

**Riscuri consemnate onest:**
- **Pragul hardware mainnet (F2)**: db-sync mainnet = 48–64 GB RAM x86_64 + ~1 TB NVMe (node DB singur ≈ 203 GB); backend-ul LSM care ar reduce RAM-ul e documentat ca instabil pe Raspberry Pi. Nu promitem mainnet pe minirack.
- **Atlas e dependență single-vendor** (GeniusYield; 5 luni fără release, ~45 issues deschise): avem ieșirea de siguranță „construim cele câteva tipuri de tranzacții direct pe cardano-api" dacă stagnează.
- **Mentenanța componentelor core Cardano s-a mutat în 2026** (Plutus→Midgard Labs, Mithril→Teragone, ledger→alte echipe, prin Intersect): continuitatea e promisă, nu dovedită — încă un motiv pentru doctrina existentă „validatoare simple, logica off-chain".
- **De evitat (moarte sau nepotrivite)**: Carp (fără activitate în 2026), cardano-graphql (legacy), Kupo pentru guvernanță (nu vede certificate/voturi — rămâne doar pe contractele de atestări), Dolos (promițător, dar servește stare curentă, nu depozit istoric SQL; de urmărit).
- **Lecția GovTool**: finanțarea comunitară a tooling-ului de guvernanță e precară — proiectăm astfel încât o singură persoană cu o singură mașină db-sync să poată rula credibil întregul registru (presetul `only_governance` face asta realist).

## Ce rămâne la builder (pentru superpowers:writing-plans)

Mecanismul exact de extragere a librăriilor (sub-tree vs copiere vs cabal local packages); schema exactă a proiecțiilor peste tabelele db-sync (+ decizia asupra mini-follower-ului opțional de tip); schema Postgres finală; claim-ul pentru script-DReps; parametrii dispute (depozit, fereastră); dimensionarea nodului mainnet; alegerea numelui (owner). Rezolvat 2026-10-19 (owner): nu există fereastră de finanțare cu dată fixă — proiectul se face pentru comunitate; o eventuală propunere de finanțare rămâne opțională și ulterioară lansării.
