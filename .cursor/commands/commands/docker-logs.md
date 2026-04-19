# Service Logs

## Overview

Check logs from the Decentralized Belt System services to diagnose issues, monitor behavior, and debug transaction failures.

## Services

| Service              | Default Port | Purpose                          |
| -------------------- | ------------ | -------------------------------- |
| `interaction-api`    | 8082         | Transaction building & submission |
| `query-api`          | 8083         | Read-only queries & projections   |
| `chainsync-service`  | 8084         | Chain indexing & event projection  |
| `admin` (CLI)        | —            | Administrative operations          |

## Steps

1. **Check Running Services**
    - List running processes: `ps aux | grep -E '(interaction-api|query-api|chainsync-service)'`
    - Check if ports are in use: `lsof -i :8082` / `:8083` / `:8084`

2. **Read Service Logs**
    - Services log to stdout/stderr by default
    - If running in foreground, check the terminal output
    - If running via systemd: `journalctl -u <service-name> -f`
    - If running via Docker Compose: `docker compose logs -f <service-name>`

3. **Common Log Patterns to Check**
    - **Transaction failures**: Look for `TxBuildingException` messages — they include the error constructor and context
    - **Chain-sync issues**: Check for `projectChainEvent` errors, missed blocks, or DB write failures
    - **Config errors**: `decodeConfigEnvOrFile` failures indicate missing config files or env vars
    - **Provider errors**: Atlas/GeniusYield provider timeout or connection refused

4. **Debug Transaction Issues**
    - Check the interaction-api logs for the full `TxBuildingException` error
    - Cross-reference trace codes with `docs/onchain-trace-codes.md`
    - Use `admin` CLI to inspect on-chain state: current UTxOs, datum contents
    - Check the Cardano node sync status if queries return stale data

5. **Filter & Search**
    - Filter for errors: `grep -i 'error\|exception\|fail'`
    - Filter by practitioner: grep for the practitioner ID or wallet address
    - Filter by interaction type: grep for the action name (e.g., `PromoteAction`, `UpdateProfile`)

## Service Logs Checklist

- [ ] Identified which service is relevant to the issue
- [ ] Located and read the service logs
- [ ] Found relevant error messages or exceptions
- [ ] Cross-referenced trace codes with `docs/onchain-trace-codes.md`
- [ ] Diagnosed root cause
