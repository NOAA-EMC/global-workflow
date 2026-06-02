# Immutable DAG Workflow Transformation

## Executive Summary

This document describes the architectural transformation of the NOAA EMC global-workflow from a legacy Rocoto-orchestrated, runtime-templated system into an **immutable, ecFlow-only DAG** deployment model. The new system produces self-contained, versioned experiment directories (EXPDIRs) where all configuration is resolved at deployment time — eliminating runtime template resolution, ensuring bit-for-bit reproducibility, and satisfying NCO Environmental Equivalence (EE2) standards.

For questions about the legacy system's structure, dependencies, and execution flow, consult the **agentcore MCP RAG** knowledge base which indexes the upstream global-workflow documentation, J-Job dependencies, and EE2 compliance standards.

---

## Legacy System (Before)

### Architecture

The legacy global-workflow uses a multi-step, interactive setup process with runtime configuration generation:

```
Developer → setup_expt.py → setup_workflow.py → Rocoto XML → rocotorun → J-Jobs → atparse templates
```

### Setup Flow (Legacy)

1. **`setup_expt.py`** — Interactive experiment generator. Takes `NET` (gfs/gefs/sfs/gcafs), `MODE` (forecast-only/cycled), resolution, date range. Produces an EXPDIR with config files containing `@[VAR]` atparse tokens and shell variables.

2. **`setup_workflow.py`** — Generates a Rocoto XML workflow document from the config files. The XML defines the task DAG, dependencies, and resource requirements.

3. **`rocotorun` / `rocotostat`** — Rocoto iterates the workflow: submits jobs, tracks dependencies, manages retries via a SQLite database. Requires periodic cron execution (`rocotorun -w workflow.xml -d workflow.db`).

4. **Runtime execution** — Each J-Job sources `jjob_header.sh`, which calls runtime parsing scripts (`parsing_namelists_FV3.sh`, `parsing_namelists_MOM6.sh`, `parsing_namelists_CICE.sh`, `parsing_namelists_WW3.sh`) that resolve `@[VAR]` tokens into final model configurations at job execution time.

### Legacy Problems

| Problem | Impact |
|---------|--------|
| **Runtime template resolution** | Model configs generated at execution time; failures happen during forecast, not at setup |
| **Mutable EXPDIR** | Config files writable; accidental edits between cycles cause silent divergence |
| **No versioning** | No manifest or SHA-256 integrity check; impossible to reproduce a past deployment |
| **Rocoto dependency** | Single-threaded workflow manager; cron-based; no native HPC scheduler integration |
| **`@[VAR]` atparse engine** | Custom token resolution separate from Jinja2; two templating engines coexist |
| **Shared J-Job naming** | `JGLOBAL_FORECAST` serves GFS, GDAS, GCAFS, GEFS — NCO production requires application-specific names |
| **Full-copy deployment** | All 92 J-Jobs, 43 ex-scripts, 229 config files staged regardless of which are needed |
| **Platform coupling** | Platform-specific logic scattered across config files via shell conditionals evaluated at runtime |
| **No compliance gate** | EE2 naming/structure violations discovered at NCO submission, not at development time |

### Legacy File Flow

```
setup_expt.py creates:
  EXPDIR/
  ├── config.base          ← shell variables + @[VAR] tokens
  ├── config.fcst          ← case/if blocks for RUN (resolved at runtime)
  ├── config.resources     ← all platforms in one file (runtime conditional)
  └── ...

setup_workflow.py creates:
  EXPDIR/
  ├── workflow.xml         ← Rocoto DAG definition
  └── workflow.db          ← Rocoto state (created on first run)

Runtime resolves:
  J-Job sources config → calls parsing_namelists_*.sh → atparse resolves @[VAR]
  → writes input.nml, MOM_input, ice_in, ww3_shel.nml to $DATA/
```

---

## New System (After)

### Architecture

The new system uses a single deterministic deployment command that produces an immutable, sealed EXPDIR:

```
Developer → deploy_workflow (8-stage pipeline) → Sealed EXPDIR → ecflow_server loads .def
```

### Deployment Flow (New)

1. **Single command** — `deploy_workflow --config gcafs.yaml --platform HERA --expdir /path --version v1.0.0 --dag-filter`

2. **8-stage pipeline** — Validates, renders all templates at deploy time, stages only DAG-reachable artifacts, generates ecFlow DAG, runs EE2 compliance, computes manifest, seals permissions.

3. **ecFlow orchestration** — `ecflow_server` loads the `.def` file; tasks are `.ecf` scripts that call a Universal_Wrapper → J-Job → ex-script chain. No cron; server manages state natively.

4. **Runtime execution** — J-Jobs use `cpreq` to copy pre-rendered configs from the sealed EXPDIR to `$DATA/`. No runtime template resolution. No `atparse`. No `parsing_namelists_*.sh`.

### New File Flow

```
deploy_workflow creates:
  EXPDIR/ (mode 0444/0555 — immutable)
  ├── manifest.yaml            ← SHA-256 of every file
  ├── workflow/provenance.yaml ← git commit, deployer, timestamp
  ├── ecf/defs/gcafs_v1.def   ← ecFlow suite definition
  ├── ecf/scripts/*.ecf        ← per-task ecFlow scripts
  ├── jobs/JGCAFS_FORECAST     ← application-named J-Jobs (renamed from JGLOBAL_)
  ├── parm/config/gcafs/       ← conditioned configs (dead branches eliminated)
  ├── parm/ufs/ocean/MOM_input ← pre-rendered (no @[VAR], no runtime generation)
  ├── parm/ufs/ice/ice_in      ← pre-rendered
  ├── parm/ufs/wave/ww3_shel.nml ← pre-rendered
  └── ush/python/pygfs/        ← runtime Python library

Runtime uses:
  ecFlow → .ecf → universal_wrapper.sh → J-Job → ex-script
  ex-script: cpreq "${EXPDIR}/parm/ufs/ocean/MOM_input" "${DATA}/INPUT/MOM_input"
  (no parsing, no atparse, no template resolution)
```

---

## Side-by-Side Comparison

| Aspect | Legacy | New |
|--------|--------|-----|
| **Orchestrator** | Rocoto (XML + cron + SQLite) | ecFlow (`.def` + server + events/meters) |
| **Setup** | `setup_expt.py` + `setup_workflow.py` (interactive, multi-step) | `deploy_workflow` (single deterministic command) |
| **Templating** | `@[VAR]` atparse (runtime) + partial Jinja2 | Jinja2 only (deploy-time); `@[VAR]` eliminated |
| **Config resolution** | Runtime: `parsing_namelists_*.sh` called during forecast | Deploy-time: all configs pre-rendered and sealed |
| **EXPDIR mutability** | Writable (accidents possible) | Immutable (mode 0444/0555 after seal) |
| **Versioning** | None | SHA-256 manifest + Snapshot_ID + provenance |
| **J-Job naming** | Shared (`JGLOBAL_FORECAST` for all apps) | Application-specific (`JGCAFS_FORECAST`, `JGCDAS_FORECAST`) |
| **Staging** | Full copy (all files) | DAG-filtered (only reachable artifacts) |
| **Platform handling** | Runtime shell conditionals | Deploy-time conditioning (dead branches eliminated) |
| **EE2 compliance** | Checked at NCO submission (late) | Pipeline Stage 6 (early, automated) |
| **Reproducibility** | Not guaranteed | Property 1: same commit + config → identical manifest |
| **Self-containment** | EXPDIR references source tree at runtime | Property 4: EXPDIR runs with `dev/` removed |
| **Model inputs** | Generated at runtime from `.IN` files | Pre-rendered at deploy time; `cpreq` to `$DATA/` |
| **Monitoring** | `rocotostat` / `rocoto_viewer.py` | `ecflow_client --get_state` + structured JSON logging |

---

## Key Transformations

### 1. Rocoto → ecFlow

**Legacy:** Rocoto XML defines tasks with `<dependency>` elements. A cron job runs `rocotorun` periodically to advance the workflow. State tracked in a flat SQLite database.

**New:** A declarative YAML (e.g., `gcafs.yaml`) defines the DAG using `families`, `tasks`, `triggers`, `events`, and `meters`. The pipeline generates an ecFlow `.def` file and per-task `.ecf` scripts. `ecflow_server` manages state natively with real-time event propagation — no polling.

### 2. atparse → Jinja2 (Deploy-Time)

**Legacy:** Model configuration files use `@[VAR]` tokens resolved by `ush/atparse.bash` at runtime. The `parsing_namelists_*.sh` scripts read config files, compute values, and write final model inputs into `$DATA/`.

**New:** All model configs are Jinja2 templates (`.j2`) rendered during Stage 3 of the pipeline. The deployed EXPDIR contains final configs with zero unresolved tokens. Forecast scripts use `cpreq` (EE2-compliant copy) to stage from EXPDIR to `$DATA/`.

### 3. Shared Names → Application Naming

**Legacy:** A single `JGLOBAL_FORECAST` file in `jobs/` serves GFS, GDAS, GCAFS, and all other applications. NCO production requires application-specific packaging.

**New:** The Workflow_YAML references application-named J-Jobs (`JGCAFS_FORECAST`). The Name_Resolver maps these back to shared source files (`JGLOBAL_FORECAST`) via a configurable Prefix_Registry. The File_Stager performs rename-on-copy: source content preserved, destination carries the application name.

### 4. Full Copy → DAG-Filtered Staging

**Legacy:** All 92 J-Jobs, 43 ex-scripts, 71 ush scripts, and 229 config files are copied into the EXPDIR regardless of what the workflow actually uses.

**New:** The DAG_Filter traces reachability from the Workflow_YAML through 4 layers (J-Jobs → ex-scripts → ush scripts → config files). Only transitively reachable artifacts are staged. Typical result: 21/92 J-Jobs, 13/43 ex-scripts, 5/71 ush scripts, 20/229 configs.

### 5. Runtime Conditionals → Deploy-Time Conditioning

**Legacy:** Config files contain `case ${RUN}` and `if [[ ${MACHINE} == ... ]]` blocks evaluated at every job execution. All branches remain in the deployed file.

**New:** The Config_Conditioner evaluates deploy-time-known variables (RUN, NET, MACHINE, APP, etc.) and eliminates dead branches. Only the matching code path remains. Runtime variables (`${PDY}`, `${cyc}`, `${FHOUR}`) are preserved unchanged.

---

## Formal Correctness Guarantees

The new system is validated against **14 correctness properties** — machine-verifiable invariants proven by a test suite of 1191+ tests including property-based testing (Hypothesis):

1. **Determinism** — same inputs → identical outputs (SHA-256 verified)
2. **Manifest integrity** — on-disk hashes match recorded hashes
3. **Immutability** — sealed files reject writes
4. **Self-containment** — runs without the source tree
5. **Atomicity** — partial failures leave COMOUT unchanged
6. **Idempotence** — re-runs produce identical results
7. **Statelessness** — no leftover state from prior runs
8. **Platform isolation** — two-platform EXPDIRs differ only in platform files
9-10. **Parser/printer round-trip** — lossless YAML → DAG → YAML
11. **ecFlow round-trip** — `.def` serialization preserves structure
12. **DAG acyclicity** — no dependency cycles
13. **Definition fidelity** — emitted `.def` matches source DAG exactly
14. **No unresolved tokens** — zero `{{`, `{%`, `{#`, `@[...]` in deployed files

---

## MCP RAG Reference

The **agentcore MCP RAG** knowledge base indexes the legacy global-workflow documentation and provides authoritative answers about:

- **Legacy execution flow** — `get_job_details`, `trace_execution_path`, `find_callers_callees`
- **EE2 compliance standards** — `search_ee2_standards`, `analyze_ee2_compliance`, `scan_repository_compliance`
- **Dependency tracing** — `find_dependencies`, `analyze_code_structure`, `trace_data_flow`
- **Workflow structure** — `get_workflow_structure`, `explain_workflow_component`, `list_job_scripts`
- **Architecture overview** — `search_architecture`, `get_code_context`

Example queries for understanding the legacy system:

```
# What does the legacy forecast job do?
→ get_job_details(job_name="JGLOBAL_FORECAST")

# How did runtime config generation work?
→ trace_execution_path(function_name="MOM6_namelists")

# What are EE2 requirements for error handling?
→ search_ee2_standards(query="error handling err_chk cpreq")

# What scripts depend on atparse?
→ find_dependencies(target="ush/atparse.bash")
```

---

## Migration Path

For teams currently using the legacy workflow:

1. **No code changes to J-Jobs or ex-scripts** — The existing `dev/jobs/` and `dev/scripts/` files are the source of truth. The pipeline copies them (with optional rename) into the EXPDIR.

2. **Config files converted to Jinja2** — Legacy `config.*` files under `dev/parm/config/` are now `.j2` templates. The `@[VAR]` syntax has been replaced with `{{ var }}`.

3. **Rocoto XML no longer generated** — `setup_workflow.py` has been decommissioned for workflow generation. The `deploy_workflow` CLI replaces both `setup_expt.py` and `setup_workflow.py`.

4. **Workflow definition is YAML** — Task DAGs are defined in `dev/parm/workflow/*.yaml` (e.g., `gcafs.yaml`, `gfs_cycled.yaml`). These are human-readable and version-controlled.

5. **ecFlow replaces Rocoto** — The server loads the `.def` file directly. Monitoring uses `ecflow_client` commands or the ecFlow GUI. No cron jobs needed.

---

## File Structure

```
dev/
├── workflow/
│   ├── deploy.py                          # CLI entry point
│   ├── deployment/
│   │   ├── pipeline.py                    # 8-stage pipeline orchestration
│   │   ├── template_renderer.py           # Jinja2 rendering (wxflow)
│   │   ├── dag_generator.py               # ecFlow .def emission
│   │   ├── workflow_config.py             # YAML parser + DAG model
│   │   ├── dag_filter.py                  # DAG reachability analysis
│   │   ├── file_stager.py                 # File staging with rename-on-copy
│   │   ├── name_resolver.py              # Application name → source resolution
│   │   ├── prefix_registry.yaml          # Registry: app prefix → search list
│   │   ├── config_conditioner.py          # Deploy-time conditional evaluation
│   │   ├── model_config_renderer.py       # UFS model input rendering
│   │   ├── completeness_verifier.py       # Cross-reference integrity check
│   │   ├── ee2_scanner.py                 # EE2 compliance validation
│   │   ├── manifest.py                    # SHA-256 manifest generation
│   │   └── seal.py                        # Permission sealing (immutability)
│   └── tests/                             # 1191+ tests (unit, integration, property-based)
├── parm/
│   ├── workflow/                           # Workflow_Configuration YAMLs
│   │   ├── gcafs.yaml
│   │   ├── gfs_cycled.yaml
│   │   └── ...
│   ├── config/<app>/                      # App-specific config templates (.j2)
│   └── ufs/                               # UFS model input templates (.j2)
│       ├── fv3/input.nml.j2
│       ├── ocean/MOM_input.j2
│       ├── ice/ice_in.j2
│       └── wave/ww3_shel.nml.j2
├── jobs/                                   # J-Jobs (shared source names)
├── scripts/                                # Ex-scripts
└── ush/                                    # Utility scripts + python/pygfs/
```
