# Coupled-Model Config CI Cases (ecFlow)

These CI cases validate the Jinja2-based coupled-model configuration rendering
pipeline introduced by the `coupled-model-configs` and `immutable-dag-workflow-modernization`
specs. They exercise the deployment-time template rendering for MOM6 ocean,
CICE6 ice, WW3 wave, FV3 nested grid, and UPP post-processing configurations.

All cases use the new ecFlow CI case format (matching `C48_ATM_gfs_fcst_only.yaml`
and `C48_S2SW_gfs_cycled.yaml`) with `case`, `workflow_config`, `deployment`,
`model_context`, `validation`, `steps`, and `cleanup` sections.

## Directory Structure

```
coupled_configs/
├── *.yaml                # Core property/feature cases
├── gcafsv1/              # GCAFS aerosol cases
├── gfsv17/               # GFS v17 production + retro stream cases
│   └── overrides/        # Reusable workflow override fragments
├── hires/                # High-resolution forecast-only smoke tests
├── sfsv1/                # SFS seasonal forecast cases
└── weekly/               # Weekly long-running cases (atmosphere-only)
```

## Core Property Cases (top-level)

| Case | Ocean Res | Wave | Ice Start | Post | Key Validation |
|------|-----------|------|-----------|------|----------------|
| `C48mx500_S2SW_ocean_res500` | 500 | CPL/CPL | cold | gfs | Coarse resolution branch |
| `C96mx050_S2SW_ocean_res050` | 050 | YES/YES | cold | gfs | Half-degree, no river runoff |
| `C96mx100_S2S_ocean_res100` | 100 | YES/YES | warm | sfs | 1-degree, warm start |
| `C384mx025_S2SW_ocean_res025` | 025 | CPL/CPL | warm | gfs | Quarter-degree, ODA, SPPT |
| `C48mx500_S2SW_gcafs_post` | 500 | CPL/YES | cold | gcafs | GCAFS post system |
| `C48mx500_S2SW_gefs_post` | 500 | CPL/CPL | cold | gefs | GEFS post system |
| `C48_S2SW_nested_grid` | 500 | CPL/CPL | cold | gfs | FV3 nested grid |
| `C48mx500_S2SW_submodule_copy` | 500 | CPL/CPL | cold | gfs | Stage 4 verbatim copy |
| `C48mx500_S2SW_schema_validation` | — | — | — | — | Negative tests |
| `C48mx500_S2SW_resolution_defaults` | 500 | CPL/CPL | cold | gfs | Defaults override logic |
| `C48mx500_S2SW_cycled_DA` | 500 | CPL/CPL | warm | gfs | Full cycled DAG |

## gcafsv1/ — GCAFS Aerosol Cases

| Case | Notes |
|------|-------|
| `C96_gcafs_cycled` | C96 GCAFS with aerosol DA (gcdas), NEXUS submodule copy |
| `C96_gcafs_cycled_noDA` | C96 GCAFS forecast-only (USE_AERO_ANL=NO) |
| `C96_gcafs_cycled_noDA_dev` | C96 dev mode (single cycle, FHMAX=24) |
| `C384_gcafs_cycled` | C384 high-res GCAFS with aerosol DA |
| `C384_gcafs_cycled_noDA` | C384 high-res GCAFS forecast-only |
| `C384_gcafs_cycled_noDA_dev` | C384 dev mode |

## gfsv17/ — GFS v17 Production & Retro Streams

| Case | Notes |
|------|-------|
| `C1152mx025_S2SW` | Production: C1152 atm, mx025 ocean, 80 ens, 384h fcst (WCOSS2) |
| `C1152mx025_S2SW_rdhpcs` | RDHPCS variant on Gaea |
| `C384mx025_3DVarAOWCDA` | 3DVar coupled DA (no hybrid) |
| `C384mx025_hybAOWCDA` | Hybrid coupled DA with 30-member ensemble |
| `gfs.v17_lowres_extended` | Lowres C96/mx500 cycled smoke |
| `retrov17_realtime` | Realtime cycling (extended date range) |
| `retrov17_stream1a` | Aug-Oct 2022 retro on Gaea |
| `retrov17_stream1b` | Feb-May 2024 retro (12h interval) |
| `retrov17_stream2` | May-Nov 2024 retro on WCOSS2 |
| `retrov17_stream3` | Nov 2024 - May 2025 retro (cross-year) |
| `retrov17_stream4` | May-Sep 2025 retro on Gaea |
| `overrides/s2sw_realtime.yaml` | Reusable realtime overrides |
| `overrides/s2sw_rdhpcs.yaml` | Reusable RDHPCS overrides |

## hires/ — High-Resolution Forecast-Only

| Case | Notes |
|------|-------|
| `C768_S2SW` | C768 forecast-only S2SW with mx025 ocean |
| `C1152_S2SW` | C1152 forecast-only S2SW with mx025 ocean |

## sfsv1/ — SFS Seasonal Forecast

| Case | Notes |
|------|-------|
| `C96mx025_S2S` | SFS C96/mx025 with 2 ensemble members |
| `C96mx100_S2S` | SFS C96/mx100 with 10 ensemble members |

## weekly/ — Atmosphere-Only Weekly Long-Running

| Case | Notes |
|------|-------|
| `C384_atm3DVar` | C384 atmosphere-only 3DVar (validates no coupled configs rendered) |
| `C384C192_hybatmda` | C384/C192 hybrid atmosphere DA with 2 ensemble members |

## Coverage Summary

These cases collectively validate:

- **All 4 ocean resolutions** (025, 050, 100, 500)
- **Both wave forcing modes** (CPL→'C', YES→'T') plus mixed
- **Warm and cold start** ice configurations
- **All 4 post systems** (gfs, gcafs, gefs, sfs)
- **FV3 nested grid** parameters (NEST_IMO/NEST_JMO)
- **Submodule copy integrity** (NEXUS, UPP)
- **Schema validation** (missing keys, invalid enums)
- **Resolution defaults merge** (explicit override logic)
- **Shell variable preservation** (`${TOPOEDITS}`, `${SYEAR}`, `${FHMAX}`, etc.)
- **No legacy `@[VAR]` tokens** in rendered output
- **No symlinks** in EXPDIR
- **Atmosphere-only mode** (selective rendering — no coupled configs)
- **Multi-platform** (CONTAINER, WCOSS2, GAEAC6)
- **Forecast-only and cycled modes**
- **Hybrid and 3DVar DA variants**
- **Inter-cycle dependencies** in cycled cases
- **Production C1152mx025 + 80-member ensemble**
- **All retro streams** (1a, 1b, 2, 3, 4)

## Running

```bash
# Single case
deploy_workflow --config <workflow_config> --platform CONTAINER \
  --expdir /path/to/EXPDIR/<case_name> --version ci-test

# All cases (via CI pipeline)
# Cases are picked up by dev/ci/gitlab-ci-hosts.yml
```

## Traces

- coupled-model-configs Requirements 1–14 (templates, schema, copies, manifest)
- immutable-dag-workflow-modernization Requirements 1–14 (ecFlow, EXPDIR, EE2)
- templated-model-configs Requirements (parent infrastructure)
