# C48_S2SW GEFS Forecast Member 001 - Test Case Documentation

**Test Case:** `C48_S2SW-gefs_fcst_mem001.yaml`  
**Configuration:** C48_S2SW (Coupled Sea-to-Sea-to-Wave)  
**System:** GEFS (Global Ensemble Forecast System)  
**Job:** JGLOBAL_FORECAST (ensemble member execution)  
**Duration:** 6-hour forecast (f000-f006)  
**Member:** 001 (single ensemble member test)  
**Status:** ✅ VERIFIED CORRECT - No changes needed  
**Last Updated:** October 1, 2025

---

## Overview

This test validates the **ensemble forecast capability** of the coupled S2SW system, executing a single perturbed ensemble member through the UFS Weather Model's 4-component coupled framework. This demonstrates the ensemble infrastructure without the computational expense of running all 30+ members.

**Total Files:**
- **Input:** 17 files (perturbed 4-component IC in mem001/ subdirectories)
- **Output:** 11 files (multi-component history + restart files)

---

## Ensemble System Context

### GEFS (Global Ensemble Forecast System)

**Operational Configuration:**
- **Members:** 30 perturbed + 1 control = 31 total
- **Duration:** 384 hours (16 days)
- **Cycling:** Every 6 hours (00Z, 06Z, 12Z, 18Z)
- **Resolution:** C384 (~25 km) operational, C48 (~200 km) for testing
- **Storage:** ~50 TB per cycle (full ensemble)

**This Test Validates:**
- ✅ Member-specific directory structure (mem001/ subdirs)
- ✅ Perturbed initial conditions ingestion
- ✅ Coupled ensemble forecast execution
- ✅ Member output organization
- ✅ Restart generation for cycling

**Test Philosophy:**
```
Member 001: Proves ensemble framework works
Other Members: Same code, different perturbations
Test Logic: If mem001 works, all members will work
Efficiency: Testing 30+ members redundant for code validation
```

---

## File Breakdown

### Input Files: 17 (in mem001/ Subdirectories)

All initial conditions located in component-specific mem001/ subdirectories:

| Component | Count | Location | Files |
|-----------|-------|----------|-------|
| **Atmosphere IC** | 13 | `gdas/model/atmos/input/mem001/` | `gfs_ctrl.nc`, `gfs_data/sfc_data` tiles |
| **Ocean Restart** | 1 | `gdas/model/ocean/restart/mem001/` | `MOM.res.nc` (perturbed) |
| **Ice Restart** | 1 | `gdas/model/ice/restart/mem001/` | `cice_model.res.nc` (perturbed) |
| **Wave Restart** | 1 | `gdas/model/wave/restart/mem001/` | `restart.ww3` (perturbed) |
| **Wave Grid Defs** | 1 | `gdas/model/wave/restart/mem001/` | `mod_def.glo_30m` (example grid) |

**Critical Pattern:** Each ensemble member has **isolated subdirectories** to prevent file collisions and enable parallel execution.

### Output Files: 11 (in mem001/ Subdirectories)

All outputs located in `gefs.{PDY}/{cyc}/model/*/mem001/`:

| Component | Count | Location | Files |
|-----------|-------|----------|-------|
| **Atmosphere History** | 2 | `atmos/history/mem001/` | `atmf006.nc`, `sfcf006.nc` |
| **Ocean History** | 1 | `ocean/history/mem001/` | `gefs.ocean.t{cyc}z.6hr_avg.f006.nc` |
| **Ice History** | 1 | `ice/history/mem001/` | `gefs.ice.t{cyc}z.6hr_avg.f006.nc` |
| **Wave History** | 2 | `wave/history/mem001/` | `gefs.wave.t{cyc}z.glo_30m.f006.nc`, `at_10m.f006.nc` |
| **Restart Files** | 4 | Various `mem001/` | `coupler.res`, `fv_core.res.nc`, `MOM.res.nc`, `cice_model.res.nc` |
| **Documentation** | 1 | Log/config | Configuration/log file |

**Note:** Only 2 wave grids in output (glo_30m, at_10m) vs 8 grids in deterministic test - focused on ensemble infrastructure validation, not comprehensive wave output.

---

## Key Insights

### Why Only 6 Hours?

**Ensemble Test Strategy:** Minimal duration for infrastructure validation

| Test Focus | 6-Hour Duration | Full 384-Hour Duration |
|------------|-----------------|------------------------|
| Code compiles & runs | ✅ Yes | ✅ Yes |
| Ensemble member isolation | ✅ Yes | ✅ Yes |
| Perturbation ingestion | ✅ Yes | ✅ Yes |
| Coupled component communication | ✅ Yes | ✅ Yes |
| File organization (mem001/) | ✅ Yes | ✅ Yes |
| Long-term ensemble spread | ❌ No | ✅ Yes |
| Ensemble skill scores | ❌ No | ✅ Yes |
| Ensemble post-processing | ❌ No | ✅ Yes |

**Result:** 6-hour test validates ensemble infrastructure without multi-day runtime (~100× faster)

**Efficiency Comparison:**
```
Full Ensemble Runtime:
30 members × 384 hours × 11 output files = ~126,720 files per cycle
This test: 11 files
Reduction: ~11,520× fewer files
```

### Ensemble Member Directory Structure

**Critical Pattern:** Each member has isolated subdirectories

```
INPUT (gdas.{PDY}/{cyc}/model/):
├─> atmos/input/mem001/       # Perturbed atmospheric IC
├─> ocean/restart/mem001/      # Perturbed ocean IC
├─> ice/restart/mem001/        # Perturbed ice IC
└─> wave/restart/mem001/       # Perturbed wave IC

OUTPUT (gefs.{PDY}/{cyc}/model/):
├─> atmos/history/mem001/      # Member 001 atmosphere output
├─> ocean/history/mem001/      # Member 001 ocean output
├─> ice/history/mem001/        # Member 001 ice output
└─> wave/history/mem001/       # Member 001 wave output
```

**Why Separate Directories?**
- Each member has perturbed initial conditions
- Prevents file collisions across members
- Enables parallel execution of all 30+ members
- Matches operational GEFS structure

**Directory Naming Convention:**
- **Deterministic GFS:** No member subdirectories
- **Ensemble GEFS:** `mem001/`, `mem002/`, ..., `mem030/`, `mem000/` (control)

### Perturbation Method

**Ensemble members differ in:**

1. **Atmospheric ICs:** Perturbations to temperature, winds, humidity
2. **Ocean ICs:** Perturbations to temperature, salinity, currents
3. **Ice ICs:** Perturbations to ice concentration, thickness
4. **Wave ICs:** Perturbations to wave spectrum

**Generated by:** GDAS EnKF (Ensemble Kalman Filter) analysis  
**Purpose:** Sample uncertainty in initial conditions to create ensemble spread  
**Method:** Analysis-error-based perturbations preserving physical balances

**Perturbation Characteristics:**
- Magnitude: ~1 K for temperature, ~1 m/s for winds
- Structure: Spatially coherent (not random noise)
- Balance: Geostrophically balanced perturbations
- Evolution: Perturbations grow/decay based on atmospheric dynamics

---

## Data Flow

```
Perturbed Initial Conditions (17 files in mem001/ subdirs)
    ├─> Atmosphere: 13 perturbed tiles
    ├─> Ocean: 1 perturbed MOM.res.nc
    ├─> Ice: 1 perturbed cice_model.res.nc
    └─> Wave: 1 perturbed restart.ww3
    ↓
UFS Coupled Model (6-hour run for member 001)
    ├─> RUN=gefs (not gfs)
    ├─> ENSMEM=001 (member identifier)
    └─> CASE=C48_S2SW (coupled configuration)
    ↓
Coupled Component Execution
    ├─> Atmosphere Component
    │   ├─> atmf006.nc (3D state at 6 hours)
    │   └─> sfcf006.nc (surface fields at 6 hours)
    │
    ├─> Ocean Component
    │   └─> gefs.ocean.t{cyc}z.6hr_avg.f006.nc
    │
    ├─> Ice Component
    │   └─> gefs.ice.t{cyc}z.6hr_avg.f006.nc
    │
    └─> Wave Component
        ├─> gefs.wave.t{cyc}z.glo_30m.f006.nc (global)
        └─> gefs.wave.t{cyc}z.at_10m.f006.nc (Atlantic)
    ↓
Restart Files for Next Segment
    ├─> coupler.res (coupling state)
    ├─> fv_core.res.nc (atmospheric dynamics)
    ├─> MOM.res.nc (ocean state)
    └─> cice_model.res.nc (ice state)
    ↓
Output: 11 files in mem001/ subdirectories
```

**Restart Purpose:** Enable continuation to next 6-hour segment  
**Operational Use:** GEFS runs in 6-hour segments for 384 hours (16 days)

---

## Comparison with Deterministic Forecast

| Aspect | **Ensemble Member** | **Deterministic GFS** |
|--------|---------------------|----------------------|
| System | GEFS (one member) | GFS (single forecast) |
| Initial Conditions | Perturbed | Best estimate |
| Duration (test) | 6 hours | 120 hours |
| Duration (operational) | 384 hours | 384 hours |
| Output Files (test) | 11 | ~380+ |
| Directory Structure | mem001/ subdirs | No member subdirs |
| Purpose | Uncertainty sampling | Best single forecast |
| Operational Members | 30-80 members | 1 forecast |
| Run Name Prefix | gefs.* | gfs.* |

**Why Test Only Member 001?**

```
Ensemble Testing Strategy:
• Member 001: Proves ensemble framework works
• Other Members: Same code, different perturbations  
• Test Logic: If mem001 works, all members will work
• Efficiency: Testing 30+ members redundant for code validation

Full Ensemble Would Be:
30 members × 120 hours × ~11 output files = ~3,960 files
This test: 11 files
Reduction: 360× fewer files
```

---

## Job Configuration Specifics

**From `jobs/JGLOBAL_FORECAST`:**

```bash
# Line 4: Ensemble-specific job naming
export DATAjob="${DATAROOT}/${RUN}efcs${ENSMEM}"
# Creates: gefsefcs001 working directory

# Line 6: Loads ensemble configuration
source jjob_header.sh -e "efcs" -c "base fcst efcs"
# Loads configs: base.j2, fcst, and efcs (ensemble-specific)
```

**Environment Variables:**
- `RUN=gefs` (not gfs)
- `ENSMEM=001` (member number, 001-030 + 000 for control)
- `CASE=C48_S2SW` (coupled configuration)

**Configuration Hierarchy:**
1. `config.base.j2` - Base settings for all forecasts
2. `config.fcst` - Forecast-specific settings
3. `config.efcs` - Ensemble-specific overrides

---

## Wave Output Selection

**Only 2 of 8 Wave Grids in Output:**
- `glo_30m` - Global 30-minute resolution
- `at_10m` - Atlantic 10-minute resolution

**Why Limited Wave Output?**

| Reason | Explanation |
|--------|-------------|
| Test Focus | Ensemble infrastructure, not comprehensive wave products |
| Storage | Full 8-grid output would be large files per member |
| Efficiency | Global + regional grid validates multi-grid coupling |
| Duration | 6-hour test doesn't need all regional grids |

**Full Operational GEFS Wave:**
- All 8 grids: glo_30m, uglo_100km, aoc_9km, gnh_10m, gsh_15m, at_10m, ep_10m, wc_10m
- Duration: 384 hours
- Members: 30 perturbed + 1 control

---

## Restart Files for Cycling

**4 Restart Files Generated:**

1. **`coupler.res`** - Coupling state between components
   - Tracks component synchronization
   - Exchange grid mappings
   - Mediator (CMEPS) state

2. **`fv_core.res.nc`** - Atmospheric dynamics
   - 3D atmospheric state on cubed-sphere
   - Prognostic variables for FV3 dynamical core

3. **`MOM.res.nc`** - Ocean state
   - 3D ocean temperature, salinity, currents
   - Sea surface height
   - Perturbed from previous cycle

4. **`cice_model.res.nc`** - Ice state
   - Ice concentration, thickness per category
   - Ice velocity, temperature
   - Snow on ice

**Purpose:** Enable continuation to next 6-hour segment  
**Operational Use:** GEFS cycles every 6 hours with updated analysis

**Cycling Strategy:**
```
Cycle N (00Z):
  └─> Run 6hr segment → Generate restarts
      └─> Cycle N+1 (06Z):
          └─> Use restarts + new perturbations → Run 6hr segment
              └─> Cycle N+2 (12Z): ...
```

---

## GEFS vs GFS Naming Convention

**Filename Differences:**

```bash
# Atmosphere
GFS:  gfs.t12z.atmf006.nc
GEFS: gefs.t12z.atmf006.nc  # Note: gefs prefix in subdirectory mem001/

# Wave
GFS:  gfs.t12z.glo_30m.f006.nc
GEFS: gefs.wave.t12z.glo_30m.f006.nc  # Note: gefs.wave prefix

# Ocean  
GFS:  gfs.ocean.t12z.6hr_avg.f006.nc
GEFS: gefs.ocean.t12z.6hr_avg.f006.nc  # Note: gefs prefix

# Directory Structure
GFS:  gfs.{PDY}/{cyc}/model/atmos/history/atmf006.nc
GEFS: gefs.{PDY}/{cyc}/model/atmos/history/mem001/atmf006.nc
      ^                                    ^
      Different run name                   Member subdirectory
```

---

## Operational Significance

### What This Test Validates

✅ **Ensemble Member Isolation:** mem001/ subdirectories work correctly  
✅ **Perturbed IC Ingestion:** All 4 components read perturbed states  
✅ **Coupled Ensemble Forecast:** 4-component coupled system executes  
✅ **Member Output Organization:** Files created in proper mem001/ locations  
✅ **Restart Generation:** Cycling-ready restart files produced  
✅ **Ensemble Naming:** gefs.* prefix and member directories correct

### Critical for Production GEFS

**Ensemble Forecasting Applications:**

1. **Probability Forecasts**
   - Chance of precipitation
   - Probability of tropical cyclone formation
   - Risk of extreme events

2. **Spread-Skill Relationship**
   - High ensemble spread → Low confidence
   - Low ensemble spread → High confidence
   - Identifies regions of forecast uncertainty

3. **Ensemble Mean**
   - Often more skillful than any single member
   - Smooths out random forecast errors
   - Standard operational product

4. **Ensemble Products**
   - Spaghetti plots (contour overlays from all members)
   - Plume diagrams (time series from all members)
   - Stamp maps (small multiples showing each member)

**Why Coupled Ensemble Matters:**

| Feature | Impact |
|---------|--------|
| Ocean perturbations | Improve tropical cyclone intensity uncertainty |
| Ice perturbations | Better Arctic forecast uncertainty quantification |
| Wave perturbations | Improved coastal inundation probability forecasts |
| Coupled interactions | More realistic ensemble spread growth |

---

## Verification Commands

### Run This Test
```bash
# Execute ensemble member test
ctest -R "C48_S2SW.*gefs.*mem001.*validate" --verbose

# Check member subdirectory structure
ls -lh gefs.{PDY}/{cyc}/model/atmos/history/mem001/
ls -lh gefs.{PDY}/{cyc}/model/ocean/history/mem001/
ls -lh gefs.{PDY}/{cyc}/model/ice/history/mem001/
ls -lh gefs.{PDY}/{cyc}/model/wave/history/mem001/
```

### Verify Output Counts
```bash
# Should find 11 files in mem001/ subdirectories
find gefs.{PDY}/{cyc}/model -path "*/mem001/*" -type f | wc -l  # 11

# By component
ls gefs/model/atmos/history/mem001/  # 2 files (atmf006, sfcf006)
ls gefs/model/ocean/history/mem001/  # 1 file (6hr_avg.f006.nc)
ls gefs/model/ice/history/mem001/    # 1 file (6hr_avg.f006.nc)
ls gefs/model/wave/history/mem001/   # 2 files (glo_30m, at_10m)
```

### Verify Ensemble Naming
```bash
# Check for gefs prefix (not gfs)
ls gefs/model/ocean/history/mem001/gefs.ocean.*.nc  # Should exist
ls gefs/model/wave/history/mem001/gefs.wave.*.nc    # Should exist
```

---

## Configuration Details

### Key Parameters from `config.efcs`

```bash
# Ensemble Configuration
CASE_ENS="C48"                # Ensemble resolution
NMEM_ENS=30                   # Number of perturbed members
NMEM_ENS_GFS=30               # GFS ensemble members
FHMAX_ENKF=9                  # EnKF forecast length (hours)

# Ensemble Member Settings
ENSMEM=001                    # This member number
DO_ENS="YES"                  # Enable ensemble mode

# Perturbation Settings
DO_INIT_PERT="YES"            # Apply IC perturbations
DO_FCST_PERT="YES"            # Apply model perturbations
```

### Resolution Characteristics

**Same as deterministic C48:**
- Atmosphere: C48 cubed-sphere (~200 km)
- Ocean: ~1° nominal
- Ice: Same grid as ocean
- Wave: Multiple grids (testing subset)

**Operational GEFS:**
- C384 atmosphere (~25 km) - Higher resolution than test
- Finer ocean/ice grids
- Full 8-grid wave system
- 30-member ensemble + 1 control

---

## MCP Tool Insights 🔧

**Global Workflow MCP insights:**
- Ensemble configuration structure
- Member directory organization
- GEFS vs GFS differences

**Demonstrated capabilities:**
- Quick ensemble system overview
- Configuration hierarchy understanding
- Member-specific processing patterns

---

## Related Test Cases

1. **C48_S2SW-gfs_fcst_seg0.yaml** - Deterministic version (non-ensemble)
2. **C48_ATM-gfs_fcst_seg0.yaml** - Atmosphere-only deterministic
3. **C48_S2SW-gfs_ocean_prod_f006.yaml** - Products generation (deterministic)

---

## Technical Notes

### File Size Estimates

| Component | Files | Size/File | Total |
|-----------|-------|-----------|-------|
| Atmosphere history | 2 | ~200 MB | ~400 MB |
| Ocean history | 1 | ~100 MB | ~100 MB |
| Ice history | 1 | ~50 MB | ~50 MB |
| Wave history | 2 | ~20 MB | ~40 MB |
| Restart files | 4 | ~50 MB | ~200 MB |
| **Total** | **11** | | **~790 MB** |

**Full Ensemble Storage:**
```
Single member (6hr): ~790 MB
30 members (6hr): ~24 GB
30 members (384hr): ~1.5 TB per cycle
4 cycles/day × 30 days: ~1.8 PB/month
```

### Processing Time

| Stage | Duration | Notes |
|-------|----------|-------|
| Initialization | ~30 sec | Load ensemble configs, read mem001/ ICs |
| 6-hour forecast | ~10 min | Coupled 4-component integration |
| Output generation | ~2 min | Write history + restart files |
| **Total** | **~13 min** | Single member, 6-hour forecast |

**Operational Scaling:**
```
30 members × 384 hours / 6 hours per segment = 1,920 segment-runs per cycle
With parallelization: ~2-4 hours wall time per cycle
```

---

## References

### Source Files
- **Test Definition:** `dev/ctests/cases/C48_S2SW-gefs_fcst_mem001.yaml`
- **Job Script:** `jobs/JGLOBAL_FORECAST`
- **Execution Script:** `scripts/exglobal_forecast.py`
- **Ensemble Logic:** `ush/forecast_det.sh`

### Configuration Files
- **Base Config:** `parm/config/gfs/config.base.j2`
- **Forecast Config:** `parm/config/gfs/config.fcst`
- **Ensemble Config:** `parm/config/gfs/config.efcs`
- **UFS Templates:** `parm/ufs/coupled/`

### Documentation
- **Repository:** TerrenceMcGuinness-NOAA/global-workflow
- **Branch:** ctest_case_updates
- **Changelog:** CTEST_UPDATES_CHANGELOG.md

---

**Created:** January 16, 2025  
**Updated:** October 1, 2025  
**Status:** Production-ready ensemble test, verified correct
