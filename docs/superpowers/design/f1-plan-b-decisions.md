# Plan B — the decisions, settled before the tasks

Drafted from: spec §4.3, `f1-synthesis.md` §3 Phase 4 (steps 21–24), `src/lib/onchain-lib/CLAUDE.md`,
and two code surveys of `palmares@cca13eb`. Everything here is a builder decision with its reasoning,
per the autonomy mandate. What is genuinely the owner's is marked **OWNER**.

## 0. What the surveys found, because it changes the plan's shape

The attestation protocol **cannot run at all today**, and not because of a stub:

1. **No profile can be created.** `MintingRedeemer` has exactly one constructor, `NewAchievement`
   (`Onchain/Validators/MintingPolicy.hs:70-78`). There is no `MintProfile` on-chain and no
   `CreateProfileAction` off-chain (`DomainTypes/Core/Actions.hs:49-64`). Every one of the three user
   operations takes a `ProfileRefAC` as *input*, so all three are unreachable: issuing an attestation
   requires the issuer's profile user NFT (`MintingPolicy.hs:160`), and nothing mints one.
   Dead code confirms it was removed rather than never written: `fcProfileCreationFee`
   (`Protocol/Types.hs:47`), `MintingContext.ctxProfilesAddress` (set at `:108`, never read),
   `ProtocolParams.profilesValidatorScriptHash` (used only to compute that dead field), and the
   uncalled CIP-68 mint/burn pair `txMustMintCIP68UserAndRef` / `txMustBurnCIP68UserAndRef`
   (`TxBuilding/Skeletons.hs:161-166`).
2. **No identity claim exists, for either credential kind.** `grep -rni "credential|claim"` over
   `src/lib/onchain-lib` returns zero hits. `txInfoSignatories` appears **once** in the whole layer,
   for the oracle admin (`OracleValidator.hs:74`). `Utils.hasTxInAtAddressWithNFT` (`Utils.hs:92-93`)
   is the only helper shaped for a claimed-address check and is never called.
3. **Disputes do not exist and never did** — `git log --all -- '*DisputesValidator*'` is empty. What
   exists is an unreachable HTTP error code `DisputeNotFound` (`WebAPI/Envelope.hs:138`) and a design
   sentence in `onchain-lib/CLAUDE.md:48`.
4. **The blueprint publishes two false claims.** `Blueprint.hs:206` — the oracle can be "re-keyed
   without redeploying a validator" — and `Protocol/Types.hs:59` — "Admin public key hash (can rotate
   itself)". Rotation is not merely unimplemented: `OracleValidator.hs:75` requires the output datum to
   equal `applyOracleAdminAction action currentParams`, and that function (`Types.hs:88-92`) preserves
   `opAdminPkh` by construction. **Rotation is forbidden by the validator.**
   Third blueprint defect: the Attestations datum schema is `definitionRef @()` (`Blueprint.hs:189`)
   rather than `CIP68Datum OnchainAchievement`, and that type is absent from `contractDefinitions`.
5. **The off-chain write path cannot build a script-issuer transaction**, although the blueprint
   advertises exactly that (`Blueprint.hs:60-62`: "lets an organisation or a committee acting through a
   script be a first-class issuer"). `txMustSpendFromAddress` hardcodes
   `gyTxInWitness = GYTxInWitnessKey` (`TxBuilding/Skeletons.hs:117-125`), and all three user
   operations route through it (`Operations.hs:77`, `:148`, `:303`). A user NFT held at a script address
   cannot be spent by this code at any price.

So Plan B is not "wire up the attestation layer". It is: **build the profile half, build the claim,
build disputes, and make the off-chain path able to witness a script.** The attestation half
(mint pending → accept) is the only part that exists, and it has two correctness gaps of its own (§4, §5).

## 1. The attestation domain, renamed — and how far the rename goes

The on-chain layer carries **no BJJ structure at all** — no rank, belt, membership-interval, promotion
or dojo type exists in `onchain-lib`. What survives is vocabulary: `Achievement`/`achievement*` (~70
occurrences), the `Practitioner` constructor of `OnchainProfileType` (`Protocol/Types.hs:110-113`), and
`practitioner*` locals.

**Decision: rename on-chain types in Plan B's first on-chain task, not later, and not never.**
Nothing is deployed (`MintingPolicy.hs:76-77` states the stable-index rule binds from first deployment
and that has not happened), so this is the one moment the rename is free. After `deploy-reference-scripts`
every renamed constructor is a validator-hash change and a coordinated redeploy.

| now | becomes | why |
|---|---|---|
| `OnchainAchievement` | `OnchainAttestation` | the spec's word |
| `achievementAwardedBy` / `achievementAwardedTo` | `attestationIssuer` / `attestationSubject` | "awarded" presumes a rank; the spec says issuer/subject |
| `achievementIsAccepted` | `attestationAccepted` | redundant prefix, per repo style |
| `OnchainProfileType = Practitioner \| Organization` | see §2 | the type is also wrong, not just its names |
| `AchievementsRedeemer` | `AttestationsRedeemer` | the module is already `AttestationsValidator` |

Off-chain the same rename reaches `Achievement`, `AwardAchievementAction`, `AcceptAchievementAction`,
`achievementsValidator*`, `scriptHashesAchievementsValidator` and the JSON field names
`awarded_to_profile_id` / `awarded_by_profile_id` (`DomainTypes/Core/Types.hs:102-125`,
`Core/Actions.hs:55-64`, `TxBuilding/Validators.hs:44-54`, `Transfer/Types.hs:82`). **The JSON rename is
breaking**, and that is acceptable precisely once, now, before any client exists.

**Also deleted in that task** (they are this rename's orphans, not unrelated cleanup): the 16
unreachable `TxBuildingException` constructors and the whole `AddMembershipIntervalReason` type with
their `displayException` arms and HTTP mappings (`TxBuilding/Exceptions.hs:15-20`, `:33-62`, `:77-99`,
`:129-152`), the orphaned membership haddock block at `Operations.hs:174-182`, and the three dead
lookups (`hasUserNFTAt`, `getOrganizationInformation`, `getProfileRefACsAtAddress`). Keeping an error
message that reads "Invalid belt number (must be 0–14)" (`Exceptions.hs:95`) in a governance registry is
not a style question.

## 2. The profile type, and what a profile actually is

The spec §4.3 names six kinds: Persoană/DRep, Organizație, Comitet, Comunitate, Vendor, Auditor. The
on-chain type has two (`Practitioner | Organization`).

**Decision: `OnchainProfileKind` with six constructors at stable indices 0–5, in the spec's order.**

Rejected alternative — keep two constructors and put the kind in CIP-68 metadata: the kind decides
nothing on-chain (no validator branches on it), so metadata would be cheaper. But the kind is what the
*read* layer filters and groups by, and metadata is a `Map BuiltinByteString BuiltinByteString` that
attest-sync would have to parse and that a profile update could silently change
(`CIP68.updateCIP68DatumMetadata` rewrites description and image only — but a future redeemer need not).
A constructor is one byte in the datum and cannot drift. Six constructors, and the enum is closed:
a seventh kind is a redeploy, which is the right cost for changing what the registry recognises.

**OWNER, one line:** is `Auditor` a profile *kind* or a role an `Organization` holds? The spec lists it
as a kind, so that is what is built; the question is only whether the owner wants an auditor that is
also an organisation to need two profiles. Not blocking — the plan builds the spec's six.

## 3. The identity claim — the piece synthesis §3 step 24 calls "the one genuinely unspecified piece
of on-chain design left"

The rule, from spec §2 and §4.3, is one sentence: **prove you can spend as that credential.**

The claimed thing is a `Credential` — which is exactly what CIP-129 encodes for a DRep id, a CC cold
credential and a stake credential, and which is already decoded by the read layer
(`DomainTypes/Core/Ids.hs:57-87`). So the datum carries `Credential`, not a bare hash, and the header
byte that distinguishes key from script is not thrown away.

**Decision: `MintProfile` requires, for the claimed credential `c`:**

- `PubKeyCredential pkh` → `pkh` ∈ `txInfoSignatories`. One list membership. This is the
  "extra signatory" the spec names.
- `ScriptCredential sh` → **some input of the transaction has `sh` as its payment credential or as its
  staking credential.** If the ledger is spending that input, the script's own spending condition has
  already been satisfied — by the ledger, before our validator runs. We do not re-check it and could
  not: a native (timelock/multisig) script has no redeemer we could inspect, and committees and
  multisig organisations are the majority case the spec calls out.

Both arms are a fold over `txInfoInputs` reading two fields of each input's address. No `txInfoRedeemers`
inspection, no reference inputs, no signature lists.

**Why the staking credential too, and not just payment:** a stake credential that is a script is claimed
by the same rule, and a script registered as a stake credential normally appears in the *staking* half
of an address, not the payment half. Reading both fields costs one more field access per input and
removes an entire class of "I cannot claim my own stake script" support question. This is also
security rule 13 in `onchain-lib/CLAUDE.md` ("reason about full addresses, payment + staking") applied
in the useful direction.

**The documented limitation, stated here so it is not discovered by a user:** a Plutus script that holds
no UTxO at its own address — a script that only ever votes or withdraws — cannot claim in one
transaction. Its workaround is one transaction: send yourself a UTxO at your own script address, then
claim. We do **not** add a `txInfoRedeemers`-purpose check to cover it. That check would cost ExUnits on
every claim to serve a case whose workaround is free, and this repo has a standing rule against exactly
that trade (`feedback_no_exunits_for_nonexploitable_defense`). It goes in the runbook and in the
validator's haddock, not in the validator.

**What the claim does NOT prove, and must be said in the API:** that the credential's *holder* is who
they say they are. It proves control of the credential, which is the only thing a chain can prove. The
`VerificationChip` wording (Plan C) must say "controls this credential", never "is".

## 4. Inherited fix #1 — the fee selector

`onchain-lib/CLAUDE.md` already states the target shape: `feeFor :: MintAction -> FeeConfig -> Integer`
in `Protocol/Lookup.hs` is "the only place a `FeeConfig` field is ever named, on or off chain", and
`checkFee :: OracleParams -> MintAction -> [TxOut] -> Bool` "takes the action, never a selector".

On-chain is already half-right: `MintingPolicy.hs:115` passes `fcAttestationFee` directly rather than
through a selector-taking helper — but it passes the *field*, so the naming site is still at the call
site. **Off-chain is wrong in exactly the Belt System's shape**: `getFeeSkeleton :: ... -> (FeeConfig ->
Integer) -> m (GYTxSkeleton 'PlutusV3)` (`TxBuilding/Operations.hs:42-53`) takes a selector, and
`getOracleFeeAndValiditySkeleton` (`:58-67`) passes one through. That is the mechanism that charged the
wrong field in the Belt System.

**Decision: introduce `MintAction` on-chain (one constructor per minting redeemer) and make both
`feeFor` and the off-chain skeleton take it.** The off-chain `getFeeSkeleton` loses its function
argument. The test that proves it: a property that for every `MintAction`, the off-chain fee equals
`feeFor` of the same action — which is the on-chain ≡ off-chain pinning this repo uses everywhere else.

**Note for §9:** spec §4.3 says *no protocol fees as a business model* and that anti-spam deposits are
returned. `FeeConfig` therefore stays as machinery but ships with all fees at zero on preprod, and
`fcProfileCreationFee` — currently dead on-chain — becomes live only because `MintProfile` exists.
The dispute **deposit** is not a fee and needs its own field (§6).

## 5. Inherited fix #2 — oracle admin rotation, and the lie in the blueprint

`OracleAdminAction` gains `OracleSetAdmin PubKeyHash`. Index discipline: `onchain-lib/CLAUDE.md` says
"**`OracleAdminAction` must include `SetAdmin`**" and the synthesis says "at index 4", so index 4 it is —
appended after the existing 0–3 (`Protocol/Types.hs:83`), never inserted.

`applyOracleAdminAction` (`Types.hs:88-92`) gains the arm that writes `opAdminPkh`. The validator needs
no change: `O1` already requires the *current* admin's signature and `O2` already requires the output to
equal `applyOracleAdminAction`, so rotation becomes possible by making that function able to express it.
That is the cheapest possible fix and it is cheap because the validator was written correctly.

**The two false claims are corrected in the same commit**, because a published blueprint asserting a
capability the validator forbids is worse than the missing capability: `Blueprint.hs:206`,
`Blueprint.hs:63-64`, `Protocol/Types.hs:59`. And the third blueprint defect with them — the Attestations
datum schema (`Blueprint.hs:189`) and the missing `contractDefinitions` entry.

Off-chain: `AdminActionType` gains `RotateAdminAction PubKeyHash`, `admin rotate-admin` appears in the
CLI (spec §4.6 names it), and — unlike `SetFeesAction` — it stays **out** of the HTTP `FromJSON`
(`Core/Actions.hs:116` already rejects admin actions it does not want over the wire).

## 6. Disputes, from nothing

Built to `onchain-lib/CLAUDE.md:48`: "opening a dispute requires the disputer's User NFT plus the
deposit." So:

- `DisputesValidator.hs`, unparameterised, address from the datum's `protocolParams` (the repo's rule —
  validator hashes are never validator parameters).
- `ProtocolParams` gains `disputesValidatorScriptHash`. **This changes the minting policy's parameter
  and therefore its hash**, which is why disputes land in the *same* deployment as the profile work and
  not in a later one.
- `MintingRedeemer` gains `OpenDispute`; `DisputesRedeemer` = `ResolveDispute Verdict Integer | Cleanup`.
- Verdict in v1 is the oracle admin's (spec §4.3), so `ResolveDispute` requires `opAdminPkh`'s signature,
  read from the oracle **reference input** exactly as the minting policy already does.
- The deposit is returned on resolution, both verdicts. That is the spec's rule and it is also what makes
  the dispute non-punitive: a dispute that costs money to lose is a dispute nobody opens.
- A dispute **never hides the attestation** (spec §4.3). It is a separate UTxO pointing at one; the read
  layer shows both. Nothing in `AttestationsValidator` learns about disputes, which keeps the attestation
  path's ExUnits unchanged.
- `OracleParams.opFeeConfig` gains `fcDisputeDeposit`, or a separate `opDisputeDeposit` — **decided in
  the task** by which keeps `FeeConfig`'s "fees are zero on preprod" statement true. A deposit that is
  returned is not a fee, so: `opDisputeDeposit :: Integer`, outside `FeeConfig`.

## 7. The two correctness gaps in the half that exists

**Double-accept has no on-chain guard.** `AttestationsValidator.hs:71-96` never checks that
`attestationAccepted` was `False`, so a second accept rewrites an identical datum and passes. The
off-chain code already refuses (`AchievementAlreadyAccepted`, `Operations.hs:286-341`) and says so:
`Operations.hs:305-310` — "The on-chain half of this (fix #4) is Plan B's." Added as one
`traceIfFalse`. Cost: one boolean read of a datum already parsed. This is **not** the "no ExUnits for
non-exploitable defense" case, because the off-chain guard is bypassable by anyone building their own
transaction, and the result is a permanent record whose acceptance timestamp is whatever the last
accepter chose.

**The attestation date is unvalidated client input, in two senses.** It arrives as `aa_achievement_date
:: GYTime` from the HTTP body (`ServiceRequests.hs:69` → `ServiceHandlers.hs:41` →
`Interactions.hs:92`) and flows into both the redeemer (`Operations.hs:241`) and the stored datum
(`Operations.hs:256`) **with no comparison against `slotOfCurrentBlock` and no bound**. On-chain there
*is* one check — `achievementDate` before `txInfoValidRange` (`MintingPolicy.hs:158`) — which blocks
future-dating (`before` is `False` for a `NegInf` lower bound, so a finite lower bound is forced) but
leaves **back-dating unbounded**: an attestation can claim 2015.

Fix #4 proper, per `onchain-lib/CLAUDE.md` ("any date carried in a datum is validated against the
transaction's validity interval"): the date must be **inside** the validity interval, not merely before
its end. Since `Skeletons.hs:47-56` already builds a ~7h window from `slotOfCurrentBlock`
(`safeEraTime = 25920`), "inside the interval" means "within the last 7 hours of chain time", which is
the honest meaning of an attestation's date: when it was *issued*. A claim about an earlier event belongs
in metadata, where it is self-reported and labelled as such — and the API must label it so.

**OWNER, one line, and it is a product question not a technical one:** should an attestation be able to
record an event date earlier than its issuance (a milestone delivered last month)? If yes, the datum
needs a second, explicitly self-reported field and the API must never present it as chain-verified. The
plan builds the strict version — `attestationDate` is issuance time, bounded by the validity interval —
and notes the extension.

## 8. The script-issuer gap off-chain

The blueprint's claim that a committee acting through a script is a first-class issuer is true on-chain
(authority is NFT possession, not a signature list) and **false in our own off-chain code**, which
hardcodes `GYTxInWitnessKey` (`Skeletons.hs:124`).

**Decision: `txMustSpendFromAddress` takes a witness.** For a key-held NFT, `GYTxInWitnessKey` as now.
For a script-held one, the caller supplies the script and redeemer — which means the *request* must
carry them, so `ServiceRequests.hs` grows an optional witness block and `interaction-api` passes it
through. This is the minimum that makes the advertised capability real.

Scope limit, stated: we support a **native script** (`GYTxInWitnessSimpleScript`) and a Plutus script
with a caller-supplied redeemer. We do not build a multisig *signing* flow — collecting N signatures is
the wallet's job and Plan C's wizard shows the partial-signature state.

## 9. What stays out of Plan B

- **Protocol fees as revenue** — spec §4.3 is explicit. `FeeConfig` ships at zero on preprod.
- **Admin multisig (M-of-N)** — spec §4.3 marks it "dezirabil … decizie la builder după estimarea de
  ExUnits". Decision: **not in F1.** `OracleSetAdmin` makes key compromise recoverable, which is the
  actual risk; M-of-N costs ExUnits on every admin action to reduce a risk that rotation already bounds.
  Revisit before mainnet (F2), where the admin key matters more and a soak month exists to test it.
- **Attestation revocation / expiry** — not in the spec. An attestation is a permanent record; a dispute
  is how it is contested. Noted because the survey shows nothing can be burned (`Mj`,
  `MintingPolicy.hs:165`), so this is a conscious property, not an oversight.
- **The oracle validator's missing NFT check.** `OracleValidator` is unparameterised and does not verify
  the oracle NFT, so any UTxO at that address with a parseable `OracleParams` datum is spendable by
  whatever pkh *its own datum* names. Not exploitable: every reader authenticates by NFT
  (`readOracleParams`), so an attacker can only plant and spend their own UTxO. Per the standing rule,
  **no check added**; documented in the validator's haddock so the next reader does not re-derive it.
- **Subject-existence check at mint.** `MintingPolicy.hs:161` validates only that the subject asset class
  carries the protocol currency symbol, so an attestation can name a profile that does not exist — and
  since nothing burns, it sits forever as an unacceptable pending record. Checking existence needs the
  subject's profile UTxO as a reference input on every issuance. Decision: **no on-chain check**; the
  off-chain path already looks the subject up to build the transaction, and the read layer marks an
  attestation whose subject does not resolve. Same trade as the anchor pointer in spec §4.3, and the
  same reasoning.
