# C48_S2SW GFS Ocean Products f006 - Test Case Documentation

**Test Case:** `C48_S2SW-gfs_ocean_prod_f006.yaml`  
**Configuration:** C48_S2SW (Coupled Sea-to-Sea-to-Wave)  
**Job:** oceanice_products.sh with COMPONENT=ocean  
**Duration:** Single forecast hour (f006)  
**Status:** ✅ FIXED - Split from combined test, products/ paths corrected  
**Last Updated:** October 1, 2025

---

## Overview

This test validates the ocean products generation pipeline for the coupled S2SW system, converting native MOM6 ocean model output into distribution-ready GRIB2 products at multiple resolutions plus native NetCDF subsets. This test specifically validates the **ocean component** of the oceanice_products job.

**Total Files:**
- **Input:** 2 files (ocean restart + history)
- **Output:** 7 files (6 GRIB2 + 1 netCDF)

---

## Critical Context: Workflow Architecture

### Separate Ocean and Ice Metatasks

**Rocoto XML shows TWO independent parallel tasks:**

```xml
<metatask name="gfs_ocean_prod">
  <var name="fhr_list">6 12 18 24 30 36 42 48</var>
  <envar><name>COMPONENT</name><value>ocean</value></envar>
  <task name="gfs_ocean_prod_f#fhr_list#">
    <command>&JOBS_DIR;/oceanice_products.sh</command>
  </task>
</metatask>

<metatask name="gfs_ice_prod">
  <var name="fhr_list">6 12 18 24 30 36 42 48</var>
  <envar><name>COMPONENT</name><value>ice</value></envar>
  <task name="gfs_ice_prod_f#fhr_list#">
    <command>&JOBS_DIR;/oceanice_products.sh</command>
  </task>
</metatask>
```

**Key Insight:** Ocean and ice run as **separate parallel tasks**, not a combined job!

**Verification from actual run (HERA nightly):**
```
CYCLE              TASK                    JOBID       STATE
202103231200       gfs_ocean_prod_f006     16850999    SUCCEEDED
202103231200       gfs_ice_prod_f006       16851001    SUCCEEDED
```

---

## File Breakdown

### Input Files: 2

Located in `gdas.{PDY}/{cyc}/model/ocean/restart/` and `gfs.{PDY}/{cyc}/model/ocean/history/`:

| Category | Count | Files | Purpose |
|----------|-------|-------|---------|
| Ocean Restart | 1 | `{PDY}.{cyc}0000.MOM.res.nc` | Initialization state |
| Ocean History | 1 | `gfs.ocean.t{cyc}z.6hr_avg.f006.nc` | **PRIMARY INPUT** for products |

**6-Hour Averaged Output:** Ocean history files contain 6-hour time-averaged fields (not instantaneous), matching operational ocean output cadence.

### Output Files: 3

All outputs located in `gfs.{PDY}/{cyc}/products/ocean/`:

| Resolution | Directory | Files | Types |
|------------|-----------|-------|-------|
| **5.00° (5p00)** | `grib2/5p00/` | 2 | `gfs.ocean.t{cyc}z.5p00.f006.grib2` + `.idx` |
| **Native Grid** | `netcdf/` | 1 | `gfs.ocean.t{cyc}z.native.f006.nc` |

**Total:** 3 files (2 GRIB2 + 1 netCDF)

**Resolution Context:**
- **Model Grid:** mx500 (0.5° nominal MOM6 tripolar grid)
- **Product Grid:** 5p00 (5.0° regular lat-lon for C48 testing)
- **Operational:** Would use finer grids (0p25, 0p50, 1p00) with higher resolution models

---

## Data Flow

```
Coupled Forecast Outputs (from C48_S2SW-gfs_fcst_seg0.yaml)
    ├─> Ocean restart: MOM.res.nc (for initialization)
    └─> Ocean history: gfs.ocean.t{cyc}z.6hr_avg.f006.nc (PRIMARY INPUT)
    ↓
COMPONENT=ocean Set in Environment
    ↓
oceanice_products.sh Execution
    ├─> Script: jobs/oceanice_products.sh
    ├─> Python: scripts/exglobal_oceanice_products.py
    ├─> Class: pygfs.task.oceanice_products.OceanIceProducts
    └─> Config: parm/post/oceanice_products_gfs.yaml (lines 23-50)
    ↓
Ocean Processing Pipeline
    ├─> Read MOM6 native grid (tripolar)
    ├─> Interpolate to regular lat-lon grids
    ├─> Generate GRIB2 files at 3 resolutions
    ├─> Create index files for fast access
    └─> Subset native grid to reduce file size
    ↓
Output by Product Type
    ├─> GRIB2 Products (0.25°, 0.50°, 1.00°)
    │   ├─> Sea surface temperature (SST)
    │   ├─> Sea surface salinity
    │   ├─> Ocean currents (u, v components)
    │   ├─> Sea surface height (SSH)
    │   └─> Mixed layer depth
    │
    └─> Native NetCDF Subset
        └─> Selected variables on MOM6 tripolar grid
    ↓
Total Output: 7 files in products/ocean/
```

---

## Key Insights

### Why Test Only f006?

**Product Generation Test Strategy:**

| Aspect | Reasoning |
|--------|-----------|
| **Purpose** | Validate post-processing logic, not forecast duration |
| **Single Hour** | Products logic same for all forecast hours |
| **Efficiency** | ~10× faster than testing multiple hours |
| **Coverage** | Ocean component tested independently |

**If All Hours Were Tested:**
```
Ocean output every 6 hours: f006, f012, f018, ..., f120
= 20 time steps × 7 files = 140 ocean product files
This test: 7 files (1 time step)
Reduction: 20× fewer files
```

### COMPONENT Environment Variable

**Job Script Pattern:** `jobs/oceanice_products.sh`

```bash
# Line 23: Component controlled by environment
export COMPONENT="ocean"  # Set by Rocoto metatask

# Script behavior changes based on COMPONENT:
if [[ "${COMPONENT}" == "ocean" ]]; then
    # Process ocean history files
    # Use ocean section of config
elif [[ "${COMPONENT}" == "ice" ]]; then
    # Process ice history files
    # Use ice section of config
fi
```

**This allows:**
- Single job script for both components
- Parallel execution of ocean and ice tasks
- Independent success/failure tracking
- Component-specific configurations

### MOM6 Native Grid Structure

**Why Native NetCDF Output?**

Ocean uses **tripolar grid** (not regular lat-lon):
- North pole singularity avoided by splitting pole into two points
- Grid spacing varies with latitude
- Optimal for Arctic ocean modeling
- Research community prefers native grid structure

**Native NetCDF Benefits:**
- Preserves grid topology
- No interpolation artifacts
- Full resolution maintained
- Subsetting reduces file size (~50 MB vs full ~500 MB)

**Contrast with atmosphere:**
- Atmosphere: Cubed-sphere resolved to GRIB2 (lat-lon sufficient)
- Ocean: Tripolar grid doesn't interpolate well → keep native format

---

## Configuration Details

### Ocean Product Specification

**Config File:** `parm/post/oceanice_products_gfs.yaml` (lines 23-50)

**Ocean Variables in GRIB2:**
- Sea surface temperature (SST) - °C
- Sea surface salinity - PSU
- Ocean currents (u, v) - m/s
- Sea surface height (SSH) - m
- Mixed layer depth - m
- Ocean heat content - J/m²

**Grids:**
- 0.25° lat-lon: 1440×721 = 1,036,800 points
- 0.50° lat-lon: 720×361 = 259,920 points
- 1.00° lat-lon: 360×181 = 65,160 points

### Multi-Resolution Strategy

**Why 3 GRIB2 Resolutions?**

| Resolution | Grid Points | Use Case | Users |
|------------|-------------|----------|-------|
| **0.25°** | 1.04M | High-detail analysis, coastal models | Research, regional forecasting |
| **0.50°** | 260K | General operational use | Marine forecasters, ship routing |
| **1.00°** | 65K | Quick-look displays | Public websites, mobile apps |

**Storage Impact:**
```
0.25° files: ~10 MB each (detailed)
0.50° files: ~3 MB each (medium)
1.00° files: ~1 MB each (compact)
Index files: ~50 KB each (metadata)
Native subset: ~50 MB (full resolution on native grid)
```

---

## Comparison with Ice Products Test

| Aspect | **Ocean Products** | **Ice Products** |
|--------|-------------------|------------------|
| Test File | C48_S2SW-gfs_ocean_prod_f006.yaml | C48_S2SW-gfs_ice_prod_f006.yaml |
| Component | COMPONENT=ocean | COMPONENT=ice |
| Input Model | MOM6 (tripolar) | CICE6 (same grid as ocean) |
| Output Files | 7 | 7 |
| GRIB2 Variables | SST, salinity, currents, SSH | Ice concentration, thickness, velocity |
| Native Grid | MOM6 tripolar | CICE6 (matches ocean grid) |
| Workflow Task | gfs_ocean_prod_f006 | gfs_ice_prod_f006 |

**Both tests:**
- Use same job script with different COMPONENT value
- Generate 3 GRIB2 resolutions + 1 native NetCDF
- Process 6-hour averaged forecast output
- Run independently in parallel

---

## Index File Purpose

**GRIB2 Index (.idx) Files:**

Enable fast variable extraction without reading entire GRIB2 file.

**Example usage:**
```bash
# Extract SST using index
wgrib2 -i gfs.ocean.t12z.0p25.f006.grib2 \
       -match ":TMP:surface:" \
       -grib sst_only.grib2

# Uses .idx to jump directly to SST record (byte offset known)
# Avoids scanning 10 MB file sequentially
```

**Critical for:**
- Real-time data distribution
- Web services (ocean.weather.gov)
- Automated extraction pipelines
- User applications needing specific variables

---

## Operational Significance

### What This Test Validates

✅ **MOM6 to Lat-Lon:** Native tripolar grid interpolation works  
✅ **GRIB2 Encoding:** Ocean variables encoded correctly  
✅ **Multi-Resolution:** 3 lat-lon grids generated properly  
✅ **Index Files:** GRIB2 indices created for fast access  
✅ **Native Subsetting:** NetCDF subset reduces file size  
✅ **Component Independence:** Ocean processes without ice dependency

### Critical for Production

**Marine Forecasting:**
- SST for tropical cyclone intensity forecasts
- Ocean currents for navigation and search-rescue
- Sea surface height for coastal inundation

**Coastal Ocean Models:**
- Boundary conditions from global ocean forecasts
- Nesting regional models in GFS ocean output

**Climate Monitoring:**
- Ocean heat content tracking
- SST anomaly detection
- Marine heatwave identification

**Shipping/Navigation:**
- Current predictions for route optimization
- SST for ice edge determination
- Mixed layer depth for submarine operations

---

## Verification Commands

### Run This Test
```bash
# Execute ocean products test
ctest -R "C48_S2SW.*ocean.*validate" --verbose

# Check output structure
ls -lh gfs.{PDY}/{cyc}/products/ocean/grib2/*/
ls -lh gfs.{PDY}/{cyc}/products/ocean/netcdf/
```

### Verify Output Counts
```bash
# Should find 7 files total
find products/ocean -type f | wc -l  # 7

# By resolution
ls products/ocean/grib2/0p25/  # 2 files (grib2 + idx)
ls products/ocean/grib2/0p50/  # 2 files
ls products/ocean/grib2/1p00/  # 2 files
ls products/ocean/netcdf/      # 1 file (native subset)
```

### Inspect GRIB2 Contents
```bash
# List variables in ocean GRIB2 file
wgrib2 products/ocean/grib2/0p25/gfs.ocean.t12z.0p25.f006.grib2

# Expected variables:
# - TMP (sea surface temperature)
# - SALTY (salinity)
# - UOGRD/VOGRD (ocean currents)
# - DSLM (sea surface height)
```

---

## Lessons Learned (October 1, 2025)

### Test Case Split Rationale

**Why split from combined oceanice_prod test?**

1. **Workflow Architecture:** Rocoto runs ocean and ice as separate tasks
2. **Independent Validation:** Ocean failures don't affect ice validation
3. **Parallel Execution:** Matches actual operational task parallelism
4. **Clear Ownership:** Each test validates one specific task

**Benefits realized:**
- More accurate representation of workflow
- Easier debugging (component-specific failures)
- Better test organization
- Matches operational job naming

### MCP Tool Insights 🔧

**Global Workflow MCP tools provided:**
- Configuration file structure references
- COM template variable definitions
- Task dependency patterns

**Value demonstrated:**
- Quick lookup of products/ path definitions
- Understanding of component-based processing
- Workflow metatask structure insights

---

## Related Test Cases

1. **C48_S2SW-gfs_fcst_seg0.yaml** - Upstream (provides ocean history files)
2. **C48_S2SW-gfs_ice_prod_f006.yaml** - Sibling (ice products, parallel task)
3. **C48_ATM-gfs_atmos_prod_f000-f002.yaml** - Atmospheric products (similar pattern)

---

## Technical Notes

### File Size Estimates

| File | Size | Notes |
|------|------|-------|
| 0p25 grib2 | ~10 MB | Detailed resolution |
| 0p50 grib2 | ~3 MB | Medium resolution |
| 1p00 grib2 | ~1 MB | Coarse resolution |
| Index files | ~50 KB each | Metadata only |
| Native NetCDF | ~50 MB | Subsetted from ~500 MB full file |
| **Total** | **~67 MB** | All 7 ocean product files |

### Processing Time

| Stage | Duration | Notes |
|-------|----------|-------|
| Initialization | ~10 sec | Load configs, read restart |
| MOM6 grid read | ~30 sec | Read native tripolar grid |
| 0.25° interpolation | ~60 sec | Finest resolution (slowest) |
| 0.50° interpolation | ~20 sec | Medium resolution |
| 1.00° interpolation | ~10 sec | Coarsest resolution (fastest) |
| GRIB2 encoding | ~30 sec | All 3 resolutions |
| Index generation | ~5 sec | 3 index files |
| Native subsetting | ~15 sec | Extract selected variables |
| **Total** | **~3 min** | Single forecast hour |

---

## References

### Source Files
- **Test Definition:** `dev/ctests/cases/C48_S2SW-gfs_ocean_prod_f006.yaml`
- **Job Script:** `jobs/oceanice_products.sh`
- **Execution Script:** `scripts/exglobal_oceanice_products.py`
- **Python Class:** `pygfs.task.oceanice_products.OceanIceProducts`

### Configuration Files
- **Product Spec:** `parm/post/oceanice_products_gfs.yaml` (lines 23-50)
- **COM Templates:** `parm/config/gfs/config.com`
- **Base Config:** `parm/config/gfs/config.base.j2`

### Documentation
- **Repository:** TerrenceMcGuinness-NOAA/global-workflow
- **Branch:** ctest_case_updates
- **Changelog:** CTEST_UPDATES_CHANGELOG.md (Part 5)
- **Split Summary:** dev/ctests/OCEANICE_TEST_SPLIT_SUMMARY.md

---

**Created:** October 1, 2025 (split from combined test)  
**Updated:** October 1, 2025  
**Status:** ✅ Fixed and validated - Tests passing
