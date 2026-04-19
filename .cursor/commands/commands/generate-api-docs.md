# Generate API Documentation

## Overview

Generate and review API documentation for the Decentralized Belt System. Swagger/OpenAPI specs are **auto-generated on API startup** — do not write them by hand. This command helps verify, extend, and supplement the auto-generated docs.

## Steps

1. **Regenerate Swagger Specs**
    - Start the interaction-api or query-api — Swagger JSON is written to `docs/generated/swagger/` on startup
    - Alternatively, review the Servant API type in `RestAPI.hs` to understand endpoints without running the server
    - Do **not** manually edit files in `docs/generated/swagger/` — they are overwritten on each startup

2. **Review Endpoint Coverage**
    - Cross-reference `RestAPI.hs` endpoint definitions with the generated Swagger
    - Ensure all endpoints have Swagger `Summary` and `Description` annotations
    - Verify request/response types have `ToSchema` instances for Swagger generation
    - Check that `deriving-aeson` instances produce the expected snake_case field names

3. **Supplement with Prose Documentation**
    - For complex workflows (e.g., promotion flow, profile updates), write narrative docs in `docs/`
    - Document authentication requirements (wallet signature, admin key)
    - Document error response shapes and `TxBuildingException` codes
    - Add curl examples for common operations

4. **Version Alignment**
    - Confirm the API version in `RestAPI.hs` (Swagger info block) matches the current state
    - Bump the API version when endpoints are added, removed, or have breaking changes
    - API version is independent from app version — bump separately

## API Documentation Checklist

- [ ] Swagger specs regenerated (start API or review `RestAPI.hs`)
- [ ] All endpoints have `Summary` and `Description` annotations
- [ ] Request/response types have `ToSchema` instances
- [ ] JSON field names match `deriving-aeson` conventions (snake_case)
- [ ] Complex workflows documented in `docs/`
- [ ] Error responses documented
- [ ] API version in `RestAPI.hs` is current
