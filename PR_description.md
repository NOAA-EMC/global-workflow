# Phase 25: CTest Framework Expansion - C48_S2SW Wave Component Tests

## Summary

This PR adds comprehensive CTest coverage for wave component jobs in the C48_S2SW workflow configuration. Five new test cases validate critical wave post-processing tasks, expanding the automated testing framework for the S2SW (Subseasonal-to-Seasonal with Waves) system.

## Changes

### New Test Cases

| Test | Job | Description |
|------|-----|-------------|
| `C48_S2SW-gfs_waveinit` | `gfs_waveinit` | Wave initialization - generates mod_def files for 8 wave grids |
| `C48_S2SW-gfs_wavepostpnt` | `gfs_wavepostpnt` | Wave point post-processing - creates spectral and bulletin tar archives |
| `C48_S2SW-gfs_wavepostsbs_f000-f002` | `gfs_wavepostsbs_f000-f002` | Wave gridded post-processing - generates GRIB2 output for first forecast segment |
| `C48_S2SW-gfs_wavepostbndpnt` | `gfs_wavepostbndpnt` | Wave boundary point post-processing - creates interpolated boundary point spectra |
| `C48_S2SW-gfs_wavepostbndpntbll` | `gfs_wavepostbndpntbll` | Wave boundary point bulletin generation - creates WMO bulletins for boundary points |

### Files Added
- `dev/ctests/cases/C48_S2SW-gfs_waveinit.yaml`
- `dev/ctests/cases/C48_S2SW-gfs_wavepostpnt.yaml`
- `dev/ctests/cases/C48_S2SW-gfs_wavepostsbs_f000-f002.yaml`
- `dev/ctests/cases/C48_S2SW-gfs_wavepostbndpnt.yaml`
- `dev/ctests/cases/C48_S2SW-gfs_wavepostbndpntbll.yaml`

### Files Modified
- `dev/ctests/CMakeLists.txt` - Added 5 new `AddJJOBTest()` entries

## Test Details

### gfs_waveinit
- **Purpose**: Validates wave model initialization and mod_def file generation
- **Inputs**: None (generation job)
- **Outputs**: 8 mod_def binary files for wave grids:
  - `uglo_100km` (native unstructured global)
  - `glo_30m` (global 30-minute)
  - `aoc_9km` (Arctic Ocean 9km)
  - `at_10m`, `ep_10m`, `wc_10m` (regional 10-minute grids)
  - `gnh_10m`, `gsh_15m` (hemispheric grids)

### gfs_wavepostpnt
- **Purpose**: Validates wave point output post-processing
- **Inputs**: 121 point files (f000-f120), 8 mod_def files
- **Outputs**: `spec.tar.gz` (spectral data), `bull.tar` (bulletins)

### gfs_wavepostsbs_f000-f002
- **Purpose**: Validates wave gridded side-by-side post-processing (first segment of metatask)
- **Inputs**: 3 uglo_100km history files (f000-f002), 8 mod_def files
- **Outputs**: GRIB2 files for `global.0p16` and `global.0p50` grids

### gfs_wavepostbndpnt
- **Purpose**: Validates interpolated boundary point spectral output
- **Inputs**: 121 point .nc and .log files (f000-f120), 8 mod_def files
- **Outputs**: `ibp.tar` (interpolated boundary point spectra)

### gfs_wavepostbndpntbll
- **Purpose**: Validates WMO bulletin generation for boundary points
- **Inputs**: 121 point .nc and .log files (f000-f120), 8 mod_def files
- **Outputs**: `ibpbull.tar` (boundary point bulletins), `ibpcbull.tar` (combined bulletins)

## Testing

All 20 test phases (5 tests × 4 phases) validated successfully on Hera HPC:

```
✅ gfs_waveinit:             setup → stage → execute → validate
✅ gfs_wavepostpnt:          setup → stage → execute → validate
✅ gfs_wavepostsbs_f000-f002: setup → stage → execute → validate
✅ gfs_wavepostbndpnt:       setup → stage → execute → validate
✅ gfs_wavepostbndpntbll:    setup → stage → execute → validate
```

### Test Execution Commands
```bash
cd dev/ctests/build
cmake ../../..
ctest -R test_C48_S2SW-gfs_wave -V
```

## Technical Notes

1. **Wave History File Pattern**: The `uglo_100km` history files follow a non-uniform pattern:
   - f000-f048: hourly (49 files)
   - f051-f120: every 3 hours (24 files)
   - Total: 73 files

2. **Wave Point File Pattern**: Point output files exist for all 121 hours (f000-f120)

3. **Metatask Handling**: The `wavepostsbs` task is a Rocoto metatask requiring specific subtask names (e.g., `gfs_wavepostsbs_f000-f002`) for `rocotoboot`

4. **Task Output Separation**: 
   - `wavepostbndpnt` produces only `ibp.tar`
   - `wavepostbndpntbll` (separate task) produces `ibpbull.tar` and `ibpcbull.tar`

## Baseline Reference

- **Experiment**: `C48_S2SW_b40eab88-7222`
- **Location**: `/scratch3/NCEPDEV/global/role.glopara/GFS_CI_CD/HERA/BUILDS/GITLAB/stable/RUNTESTS`

## Dependencies

- Requires Rocoto dryrun build at `GFS_CI_ROCOTO_PATH`
- Uses `fv3-cpu` allocation on Hera
- Baseline data must be present at `STAGED_CTESTS`

## Checklist

- [x] YAML test cases follow existing patterns
- [x] CMakeLists.txt entries use correct `AddJJOBTest()` format
- [x] All 4 test phases pass for each test case (5 tests total)
- [x] Input/output file paths match baseline structure
- [x] Jinja2 templating with wxflow filters works correctly
