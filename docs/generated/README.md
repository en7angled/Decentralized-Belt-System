# Generated artifacts

Files in this directory are **written by code** — do not hand-edit them. Edits here will be overwritten on the next regeneration.

| File | Regenerate with | Source |
| ---- | --------------- | ------ |
| `bjj-belt-system-blueprint.json` | `cabal run admin -- write-blueprint` | `src/lib/onchain-lib/Onchain/Blueprint.hs` (version in `myPreamble`); type schemas via `HasBlueprintDefinition` |
| `swagger/query-swagger-api.json` | Start the `query-api` executable (writes on startup) | `src/exe/query-api/RestAPI.hs` + `ToSchema` instances |
| `swagger/interaction-swagger-api.json` | Start the `interaction-api` executable (writes on startup) | `src/exe/interaction-api/RestAPI.hs` + `ToSchema` instances |

For API and blueprint versioning policy, see [`.cursor/rules/workflow/api-versioning.mdc`](../../.cursor/rules/workflow/api-versioning.mdc).
