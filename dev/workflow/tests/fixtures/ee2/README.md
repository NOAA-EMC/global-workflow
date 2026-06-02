# EE2_Baseline_Recording fixtures

This directory holds the committed **EE2_Baseline_Recording** artifacts
(Requirement 10.3) produced by the authoritative agentcore MCP RAG EE2 tooling
in the development environment. They capture the per-file / per-category EE2
verdict so that EE2 authority is **reproducible offline** by CI and by unit
tests — without ever calling the live RAG server (Req 10.6).

## Authority and the offline constraint

- The agentcore MCP RAG server (EE2 v11, Phase 2 SME-corrected patterns) is the
  authoritative EE2 judge. It is reachable **only inside the development
  environment**, never in CI.
- A developer runs the RAG scan once at authoring time and commits the result
  here. CI and tests then read these JSON recordings and confirm the reconciled
  in-repo `deployment/ee2_scanner.py` reproduces the same verdict via
  `deployment.rag_ee2_adapter.check_against_baseline()`.
- Per the Phase 2 SME correction, `err_chk` / `err_exit` / `cpreq` / `cpfs` are
  the correct error-handling patterns and `set -e` / `set -eu` are **not**
  required. The recordings reflect that corrected judgment.

## Recordings

### `forecast_postdet_baseline.json`

Authoritative RAG verdict for the modified `ush/forecast_postdet.sh` (the
deploy-time `cpreq` config-staging blocks for FV3 / WW3 / MOM6 / CICE / GOCART
that replace the legacy `parsing_namelists_*.sh` runtime atparse generation).

- `scan_repository_compliance` over the five scan categories
  (`error_handling`, `environment_variables`, `file_naming`,
  `shebang_compliance`, `production_utilities`) → **0 files with issues**.
- `extract_code_for_analysis` over `output_file_naming`, `shebang_compliance`,
  `env_var_validation` → **no unresolved findings**. The staging blocks copy
  pre-rendered configs into the `${DATA}` working directory (not COM product
  files), so output-file-naming does not apply; the shebang is valid on line 1;
  and the pre-flight existence check that emits `FATAL ERROR:` and aborts before
  each `cpreq` is the EE2-compliant essential-file / env-validation pattern.

Net verdict: **PASS** (`passed: true`).

## Schema

Each recording is deterministic (sorted keys, no volatile timestamp), so the
fixture is byte-stable and diff-friendly:

| field                 | meaning                                                        |
| --------------------- | -------------------------------------------------------------- |
| `schema_version`      | recording schema version                                       |
| `authority`           | the authoritative source + the SME-corrected pattern note      |
| `scan_categories`     | the five categories passed to `scan_repository_compliance`     |
| `extract_categories`  | the three categories passed to `extract_code_for_analysis`     |
| `scanner_categories`  | the categories the offline `ee2_scanner.py` actually evaluates  |
| `files_with_issues`   | scan count of flagged files                                    |
| `passed`              | overall pass (`files_with_issues == 0` and no extract finding) |
| `issues_by_category`  | raw RAG scan issues, grouped by category                       |
| `extract_findings`    | raw RAG extract findings, grouped by category                  |
| `files`               | per-file `{scan, extract}` clean/issue verdict map             |

## Reproducing a recording (development environment only)

1. Use the venv interpreter: `dev/workflow/.venv/bin/python`.
2. Derive the changed-file set and run the live RAG scan through the adapter:

   ```python
   from pathlib import Path
   from deployment.rag_ee2_adapter import (
       derive_changed_files, run_rag_ee2_scan, record_baseline,
   )

   # `client` wraps the live agentcore MCP RAG EE2 tools and is available
   # ONLY in the development environment.
   repo_root = Path(".").resolve().parents[2]   # global-workflow root
   changed = derive_changed_files(repo_root)     # *.sh, J-Jobs, ex*, ush/
   result = run_rag_ee2_scan(client, changed, repo_root=repo_root)
   record_baseline(result, Path("dev/workflow/tests/fixtures/ee2"),
                   name="forecast_postdet_baseline.json")
   ```

3. Commit the regenerated JSON. CI never performs this step.
