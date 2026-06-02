# Deployment Pipeline — User Guide

## Overview

The global-workflow deployment pipeline transforms the `dev/` source tree into an immutable, versioned **EXPDIR** (experiment directory) that matches the NCO production layout. The pipeline renders templates, generates an ecFlow DAG, runs EE2 compliance checks, and seals the deployment artifact so it is self-contained and reproducible.

This system was built incrementally through six specs, each adding a layer of capability:

| Spec | Purpose |
|------|---------|
| **immutable-dag-workflow-modernization** | Core 8-stage pipeline architecture, ecFlow-only orchestration, Rocoto decommission |
| **templated-model-configs** | FV3 model configs (field_table, input.nml, model_configure, ufs.configure) as Jinja2 templates |
| **coupled-model-configs** | MOM6, CICE6, WW3, GOCART configs as Jinja2 templates; eliminates runtime parsing scripts |
| **minimal-sealed-expdir** | DAG-filtered staging (only reachable artifacts deployed), config conditioning, completeness verification |
| **application-jjob-naming** | Application-specific J-Job naming (JGCAFS_, JGCDAS_, etc.) with rename-on-copy staging |
| **immutable-dag-goal-realization** | Verification gate proving all 14 correctness properties hold; token scans, EE2 baseline recordings |

---

## Quick Start

### Prerequisites

```bash
cd dev/workflow
source .venv/bin/activate
pip install -r requirements.txt   # wxflow, uwtools, hypothesis, pytest, pyyaml
```

### Deploy a GCAFS Workflow

```bash
.venv/bin/python deploy.py \
    --config ../../dev/parm/workflow/gcafs.yaml \
    --platform HERA \
    --expdir /path/to/EXPDIR/gcafs_v1 \
    --version v1.0.0 \
    --dag-filter \
    --submodule-policy skip
```

### Dry-Run (Validate Without Writing)

```bash
.venv/bin/python deploy.py \
    --config ../../dev/parm/workflow/gcafs.yaml \
    --platform HERA \
    --expdir /path/to/EXPDIR/gcafs_v1 \
    --version v1.0.0 \
    --dag-filter \
    --dry-run
```

The dry-run mode validates inputs and prints a **name resolution table** showing how application names map to shared source files.

---

## CLI Reference

```
deploy_workflow --config <YAML> --platform <PLATFORM> --expdir <PATH> --version <SEMVER>
               [--dag-filter] [--dry-run] [--allowlist <PATHS>]
               [--submodule-policy require|fixture|skip]
```

| Flag | Default | Description |
|------|---------|-------------|
| `--config` | *required* | Path to Workflow_Configuration YAML (e.g., `dev/parm/workflow/gcafs.yaml`) |
| `--platform` | *required* | Target HPC: HERA, HERCULES, ORION, WCOSS2, GAEAC6, DERECHO, URSA, AWSPW, AZUREPW, GOOGLEPW, CONTAINER |
| `--expdir` | *required* | Destination EXPDIR path |
| `--version` | *required* | Semantic version string for the Snapshot_ID (e.g., `v1.0.0`) |
| `--dag-filter` | disabled | Enable DAG-filtered staging with application naming |
| `--dry-run` | disabled | Validate inputs without writing files |
| `--allowlist` | none | Comma-separated dev/ paths to include (e.g., `dev/ctests/`) |
| `--submodule-policy` | `require` | How to handle missing submodules: `require` (abort), `fixture` (use test fixtures), `skip` (skip with warning) |

---

## Pipeline Stages

The pipeline executes 8 stages in order. A failure at any stage halts the pipeline before subsequent stages run.

| Stage | Name | Description |
|-------|------|-------------|
| 1 | **Validate** | Config file exists, platform supported, EXPDIR not already sealed, prefix_registry.yaml exists, wxflow/uwtools versions match |
| 2 | **Build Context** | Assemble Jinja2 context from YAML + platform + version + git metadata; load PrefixRegistry and NameResolver |
| 3 | **Render Templates** | Render `.j2` files (config, model inputs, ecFlow includes) via wxflow/Jinja2 |
| 4 | **Stage Files** | Copy non-template files from dev/ to EXPDIR (DAG-filtered or full); rename J-Jobs with application naming; stage unconditional artifacts; copy submodule files; condition config files; verify completeness |
| 5 | **Generate DAG** | Emit ecFlow `.def` file and per-task `.ecf` scripts from Workflow_YAML |
| 6 | **EE2 Scan** | Validate all J-Jobs, ex-scripts, and ush scripts for NCO EE2 compliance |
| 7 | **Manifest** | Compute SHA-256 of every EXPDIR file; write `manifest.yaml` |
| 8 | **Seal** | Set file permissions to 0444 / directory to 0555 (immutable) |

---

## Application-Specific J-Job Naming

When `--dag-filter` is enabled, the pipeline uses **application-specific naming**:

- The Workflow_YAML references application-named J-Jobs (e.g., `JGCAFS_FORECAST`, `JGCDAS_FORECAST`)
- The **Name_Resolver** maps these back to shared source files in `dev/jobs/` (e.g., `JGLOBAL_FORECAST`)
- The **File_Stager** copies the source file into EXPDIR using the application name as the destination filename

### Resolution Algorithm

1. **Direct check** — if the name exists directly in `dev/jobs/`, pass through unchanged
2. **Prefix lookup** — identify the application prefix (e.g., `JGCAFS_`) from the registry
3. **Ordered search** — search shared prefixes in registry order (e.g., `JGLOBAL_` first)
4. **First match wins** — return the first source file found
5. **FATAL** — if no source exists, emit error listing all paths searched

### Prefix Registry

The mapping is configured in `dev/workflow/deployment/prefix_registry.yaml`:

```yaml
registry:
  JGCAFS_:  [JGLOBAL_]
  JGCDAS_:  [JGLOBAL_, JGDAS_]
  JGFS_:    [JGLOBAL_, JGFS_]
  JGDAS_:   [JGLOBAL_, JGDAS_]
  JGEFS_:   [JGLOBAL_, JGEFS_]
  JSFS_:    [JGLOBAL_, JSFS_]
```

New applications can be added by editing this file — no pipeline code changes needed.

### Backward Compatibility

- Shared names (e.g., `JGLOBAL_FORECAST`) in `jjob:` fields still work — they pass through without renaming
- Mixed-mode YAMLs containing both application and shared names are handled correctly

---

## DAG-Filtered Staging

When `--dag-filter` is enabled, only artifacts **transitively reachable** from the Workflow_YAML task DAG are staged:

```
Workflow_YAML → J-Jobs → Ex-Scripts → Ush Scripts → Config Files
```

This produces a minimal EXPDIR — typically 20-30% of the full source tree:

```
DAG Filter Results:
  J-Jobs:      21/92 staged
  Ex-Scripts:  13/43 staged
  Ush Scripts: 5/71 staged
  Configs:     20/229 staged
```

**Unconditional artifacts** are always staged regardless of DAG filtering:
- `sorc/link_workflow.sh`
- `sorc/ufs_utils.fd/fix/link_fixdirs.sh`
- `ush/python/` (Python runtime library)

---

## Expected Output

A successful deployment produces:

```
<EXPDIR>/
├── ecf/
│   ├── defs/gcafs_v1.def        # ecFlow suite definition
│   ├── scripts/*.ecf            # Per-task ecFlow scripts
│   └── include/                 # ecFlow include headers
├── env/HERA.env                 # Platform environment
├── jobs/                        # Application-named J-Jobs (JGCAFS_*, JGCDAS_*)
├── manifest.yaml                # SHA-256 hashes of all files
├── modulefiles/HERA/            # Platform module files
├── parm/
│   ├── config/gcafs/            # Conditioned config files (dead branches eliminated)
│   ├── ufs/                     # Pre-rendered model inputs (FV3, ocean, ice, wave)
│   └── workflow/                # Rendered workflow config
├── scripts/                     # Reachable ex-scripts
├── sorc/                        # link_workflow.sh, link_fixdirs.sh
├── ush/                         # Reachable ush scripts + python/pygfs/
├── versions/                    # Version files
└── workflow/
    ├── provenance.yaml          # Git commit, deployer, timestamp
    └── state.db                 # Empty SQLite for runtime event logging
```

---

## Running Tests

```bash
cd dev/workflow
.venv/bin/python -m pytest tests/ -q
```

The test suite includes:
- **8 property-based tests** (Hypothesis) covering application naming correctness properties
- **Unit tests** for all pipeline components (name resolver, DAG filter, file stager, EE2 scanner, etc.)
- **Integration tests** exercising end-to-end deployment with application naming
- **Goal realization gate** verifying all 14 parent correctness properties

Current status: **1191 passed, 4 skipped** (92s).

---

## Correctness Properties

The system is validated against 14 formal correctness properties:

| # | Property | What It Guarantees |
|---|----------|-------------------|
| 1 | Deployment Determinism | Same commit + config + platform → identical manifest hashes |
| 2 | Manifest Integrity | On-disk SHA-256 equals recorded hash for every file |
| 3 | Immutability | No regular file writable after sealing |
| 4 | Self-Containment | EXPDIR executes without reading `dev/` |
| 5 | Atomicity | Partial staging failure leaves COMOUT unchanged |
| 6 | Idempotence | Re-running a task with identical inputs produces identical outputs |
| 7 | Statelessness | Task succeeds with empty DATAROOT |
| 8 | Platform Isolation | Two-platform EXPDIRs differ only in platform-conditioned files |
| 9 | Parser Round-Trip | `pretty_print(parse(f))` structurally equals `parse(f)` |
| 10 | Printer Round-Trip | `parse(pretty_print(d))` structurally equals `d` |
| 11 | ecFlow Round-Trip | `save_as_defs` → `read_from_path` → structural equality |
| 12 | DAG Acyclicity | No cycles in the dependency graph |
| 13 | Definition Fidelity | emitted `Defs` matches source DAG task set |
| 14 | No Unresolved Tokens | No `{{`, `{%`, `{#`, or `@[...]` in rendered EXPDIR |

---

## Workflow YAML Configuration

Workflow configurations live under `dev/parm/workflow/`. Each YAML defines:

- **Suite metadata** (name, ecf paths)
- **Cycles** (date ranges, time slots)
- **Families and tasks** (hierarchical DAG structure with triggers, events, meters)
- **Inter-cycle dependencies** (cross-cycle trigger relationships)

Available workflow configs:
- `gcafs.yaml` — GCAFS coupled aerosol system (GCDAS + GCAFS cycles)
- `gfs_cycled.yaml` — GFS full cycled (GDAS + GFS)
- `gfs_forecast_only.yaml` — GFS forecast-only mode
- `gefs.yaml` — GEFS ensemble
- `sfs.yaml` — Seasonal Forecast System

---

## Troubleshooting

| Error | Cause | Fix |
|-------|-------|-----|
| `Configuration file not found` | Relative path resolved from wrong CWD | Use absolute path or correct relative path |
| `Unsupported platform` | Platform string not in supported set | Check supported list in `--help` |
| `Prefix registry not found` | Missing `prefix_registry.yaml` | Verify `dev/workflow/deployment/prefix_registry.yaml` exists |
| `Cannot resolve 'JGCAFS_XYZ'` | No matching source file in `dev/jobs/` | Check prefix_registry.yaml and verify source file exists |
| `Submodule source not found` | Submodules not checked out | Run `git submodule update --init` or use `--submodule-policy skip` |
| `EE2 violation [shebang_compliance]` | Non-script file scanned | Python library modules under `ush/python/` are excluded automatically |
| `EXPDIR already published` | Re-deploying to sealed EXPDIR | Delete the existing EXPDIR or use a new path |
