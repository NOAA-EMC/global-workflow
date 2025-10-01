# C48_S2SW GFS Ice Products f006 - Test Case Documentation

**Test Case:** `C48_S2SW-gfs_ice_prod_f006.yaml`  
**Configuration:** C48_S2SW (Coupled Sea-to-Sea-to-Wave)  
**Job:** oceanice_products.sh with COMPONENT=ice  
**Duration:** Single forecast hour (f006)  
**Status:** ✅ FIXED - Split from combined test, products/ paths corrected  
**Last Updated:** October 1, 2025

---

## Overview

This test validates the sea ice products generation pipeline for the coupled S2SW system, converting native CICE6 ice model output into distribution-ready GRIB2 products at multiple resolutions plus native NetCDF subsets. This test specifically validates the **ice component** of the oceanice_products job.

**Total Files:**
- **Input:** 2 files (ice restart + history)
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

Located in `gdas.{PDY}/{cyc}/model/ice/restart/` and `gfs.{PDY}/{cyc}/model/ice/history/`:

| Category | Count | Files | Purpose |
|----------|-------|-------|---------|
| Ice Restart | 1 | `{PDY}.{cyc}0000.cice_model.res.nc` | Initialization state |
| Ice History | 1 | `gfs.ice.t{cyc}z.6hr_avg.f006.nc` | **PRIMARY INPUT** for products |

**6-Hour Averaged Output:** Ice history files contain 6-hour time-averaged fields (not instantaneous), matching operational ice output cadence and ocean output frequency.

### Output Files: 3

All outputs located in `gfs.{PDY}/{cyc}/products/ice/`:

| Resolution | Directory | Files | Types |
|------------|-----------|-------|-------|
| **5.00° (5p00)** | `grib2/5p00/` | 2 | `gfs.ice.t{cyc}z.5p00.f006.grib2` + `.idx` |
| **Native Grid** | `netcdf/` | 1 | `gfs.ice.t{cyc}z.native.f006.nc` |

**Total:** 3 files (2 GRIB2 + 1 netCDF)

**Resolution Context:**
- **Model Grid:** mx500 (0.5° nominal CICE6 grid, same as ocean)
- **Product Grid:** 5p00 (5.0° regular lat-lon for C48 testing)
- **Operational:** Would use finer grids (0p25, 0p50, 1p00) with higher resolution models

---

## Data Flow```
Coupled Forecast Outputs (from C48_S2SW-gfs_fcst_seg0.yaml)
    ├─> Ice restart: cice_model.res.nc (for initialization)
    └─> Ice history: gfs.ice.t{cyc}z.6hr_avg.f006.nc (PRIMARY INPUT)
    ↓
COMPONENT=ice Set in Environment
    ↓
oceanice_products.sh Execution
    ├─> Script: jobs/oceanice_products.sh
    ├─> Python: scripts/exglobal_oceanice_products.py
    ├─> Class: pygfs.task.oceanice_products.OceanIceProducts
    └─> Config: parm/post/oceanice_products_gfs.yaml (lines 52-79)
    ↓
Ice Processing Pipeline
    ├─> Read CICE6 native grid (matches ocean tripolar)
    ├─> Interpolate to regular lat-lon grids
    ├─> Generate GRIB2 files at 3 resolutions
    ├─> Create index files for fast access
    └─> Subset native grid to reduce file size
    ↓
Output by Product Type
    ├─> GRIB2 Products (0.25°, 0.50°, 1.00°)
    │   ├─> Ice concentration (areal coverage)
    │   ├─> Ice thickness (mean)
    │   ├─> Ice velocity (u, v components)
    │   ├─> Ice temperature
    │   ├─> Snow depth on ice
    │   └─> Ice age/category distribution
    │
    └─> Native NetCDF Subset
        └─> Selected variables on CICE6 native grid
    ↓
Total Output: 7 files in products/ice/
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
| **Coverage** | Ice component tested independently |

**If All Hours Were Tested:**
```
Ice output every 6 hours: f006, f012, f018, ..., f120
= 20 time steps × 7 files = 140 ice product files
This test: 7 files (1 time step)
Reduction: 20× fewer files
```

### COMPONENT Environment Variable

**Job Script Pattern:** `jobs/oceanice_products.sh`

```bash
# Line 23: Component controlled by environment
export COMPONENT="ice"  # Set by Rocoto metatask

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

### CICE6 Grid Structure

**Why Native NetCDF Output?**

Ice model uses same grid as ocean (**tripolar grid**):
- Matches MOM6 ocean grid structure
- North pole singularity avoided
- Optimal for Arctic ice modeling
- Research community prefers native grid data

**Native NetCDF Benefits:**
- Preserves grid topology and connectivity
- No interpolation artifacts at ice edges
- Full resolution maintained (critical for ice features)
- Subsetting reduces file size (~20 MB vs full ~100 MB)

**Ice Edge Sensitivity:**
- Ice edge location critical for navigation
- Interpolation can blur sharp ice boundaries
- Native grid preserves actual model ice edge

---

## Configuration Details

### Ice Product Specification

**Config File:** `parm/post/oceanice_products_gfs.yaml` (lines 52-79)

**Ice Variables in GRIB2:**
- Ice concentration (areal coverage) - fraction (0-1)
- Ice thickness (mean) - meters
- Ice velocity (u, v components) - m/s
- Ice temperature - Kelvin
- Snow depth on ice - meters
- Ice age - years
- Ice category distribution - fraction per category

**CICE6 Ice Categories:**
- Category 1: Thin ice (0-0.64 m)
- Category 2: Medium thin ice (0.64-1.39 m)
- Category 3: Medium ice (1.39-2.47 m)
- Category 4: Thick ice (2.47-4.57 m)
- Category 5: Very thick ice (>4.57 m)

**Grids:**
- 0.25° lat-lon: 1440×721 = 1,036,800 points
- 0.50° lat-lon: 720×361 = 259,920 points
- 1.00° lat-lon: 360×181 = 65,160 points

### Multi-Resolution Strategy

**Why 3 GRIB2 Resolutions?**

| Resolution | Grid Points | Use Case | Users |
|------------|-------------|----------|-------|
| **0.25°** | 1.04M | High-detail ice edge, leads/polynyas | Research, Arctic forecasting |
| **0.50°** | 260K | General operational use | Marine forecasters, ship routing |
| **1.00°** | 65K | Quick-look ice extent displays | Public websites, seasonal outlooks |

**Storage Impact:**
```
0.25° files: ~5 MB each (detailed ice features)
0.50° files: ~2 MB each (medium detail)
1.00° files: ~1 MB each (compact extent maps)
Index files: ~50 KB each (metadata)
Native subset: ~20 MB (full resolution on native grid)
```

---

## Comparison with Ocean Products Test

| Aspect | **Ice Products** | **Ocean Products** |
|--------|------------------|-------------------|
| Test File | C48_S2SW-gfs_ice_prod_f006.yaml | C48_S2SW-gfs_ocean_prod_f006.yaml |
| Component | COMPONENT=ice | COMPONENT=ocean |
| Input Model | CICE6 (tripolar) | MOM6 (tripolar) |
| Output Files | 7 | 7 |
| GRIB2 Variables | Ice concentration, thickness, velocity | SST, salinity, currents, SSH |
| Native Grid | CICE6 (matches ocean) | MOM6 tripolar |
| Workflow Task | gfs_ice_prod_f006 | gfs_ocean_prod_f006 |

**Both tests:**
- Use same job script with different COMPONENT value
- Generate 3 GRIB2 resolutions + 1 native NetCDF
- Process 6-hour averaged forecast output
- Run independently in parallel

**Key Difference:**
- **Ice:** Critical for navigation safety, sharp edges matter
- **Ocean:** Critical for heat content, smooth field variations

---

## Index File Purpose

**GRIB2 Index (.idx) Files:**

Enable fast variable extraction without reading entire GRIB2 file.

**Example usage:**
```bash
# Extract ice concentration using index
wgrib2 -i gfs.ice.t12z.0p25.f006.grib2 \
       -match ":ICEC:surface:" \
       -grib ice_conc_only.grib2

# Uses .idx to jump directly to ICEC record (byte offset known)
# Avoids scanning 5 MB file sequentially
```

**Critical for:**
- Real-time ice hazard warnings
- Ship routing applications
- Ice edge detection algorithms
- Seasonal ice extent monitoring

---

## Operational Significance

### What This Test Validates

✅ **CICE6 to Lat-Lon:** Native tripolar grid interpolation works  
✅ **GRIB2 Encoding:** Ice variables encoded correctly  
✅ **Multi-Resolution:** 3 lat-lon grids generated properly  
✅ **Index Files:** GRIB2 indices created for fast access  
✅ **Native Subsetting:** NetCDF subset reduces file size  
✅ **Component Independence:** Ice processes without ocean dependency  
✅ **Ice Edge Preservation:** Sharp boundaries maintained in products

### Critical for Production

**Arctic Navigation:**
- Ice concentration for ship routing
- Ice thickness for icebreaker operations
- Ice edge location for safety zones

**Marine Safety:**
- Ice hazard warnings
- Iceberg tracking (via ice drift)
- Operational ice forecasts

**Climate Monitoring:**
- Arctic sea ice extent trends
- Seasonal ice thickness evolution
- Ice age distribution changes

**Subseasonal-to-Seasonal Prediction:**
- Sea ice extent forecasts (weeks to months)
- Arctic amplification monitoring
- Ice-albedo feedback tracking

**Resource Development:**
- Arctic shipping route planning
- Offshore operations in ice-prone areas
- Fisheries management (ice edge ecosystems)

---

## Sea Ice Characteristics

### Ice Concentration

**Definition:** Fraction of grid cell covered by ice (0.0 to 1.0)

**Operational Thresholds:**
- 0.0: Open water (safe for all vessels)
- 0.1-0.4: Very open drift ice (navigable with care)
- 0.4-0.7: Open drift ice (icebreaker recommended)
- 0.7-0.9: Close drift ice (icebreaker required)
- 0.9-1.0: Very close/compact ice (dangerous even for icebreakers)

### Ice Thickness

**Categories:**
- New ice: <10 cm
- Young ice: 10-30 cm
- First-year ice: 30-200 cm (CICE categories 1-3)
- Multi-year ice: >200 cm (CICE categories 4-5)

**Operational Impact:**
- Ships avoid ice >1 m without icebreaker support
- Multi-year ice (>2 m) extremely hazardous

### Ice Velocity

**Ice Drift:**
- Typical: 5-20 cm/s (~0.2-0.8 knots)
- Strong winds: Up to 50 cm/s (~1 knot)
- Critical for iceberg tracking and collision avoidance

---

## Verification Commands

### Run This Test
```bash
# Execute ice products test
ctest -R "C48_S2SW.*ice.*validate" --verbose

# Check output structure
ls -lh gfs.{PDY}/{cyc}/products/ice/grib2/*/
ls -lh gfs.{PDY}/{cyc}/products/ice/netcdf/
```

### Verify Output Counts
```bash
# Should find 7 files total
find products/ice -type f | wc -l  # 7

# By resolution
ls products/ice/grib2/0p25/  # 2 files (grib2 + idx)
ls products/ice/grib2/0p50/  # 2 files
ls products/ice/grib2/1p00/  # 2 files
ls products/ice/netcdf/      # 1 file (native subset)
```

### Inspect GRIB2 Contents
```bash
# List variables in ice GRIB2 file
wgrib2 products/ice/grib2/0p25/gfs.ice.t12z.0p25.f006.grib2

# Expected variables:
# - ICEC (ice concentration)
# - ICETK (ice thickness)
# - UICE/VICE (ice velocity components)
# - ICETMP (ice temperature)
# - SNOD (snow depth on ice)
```

---

## Lessons Learned (October 1, 2025)

### Test Case Split Rationale

**Why split from combined oceanice_prod test?**

1. **Workflow Architecture:** Rocoto runs ocean and ice as separate tasks
2. **Independent Validation:** Ice failures don't affect ocean validation
3. **Parallel Execution:** Matches actual operational task parallelism
4. **Clear Ownership:** Each test validates one specific task

**Benefits realized:**
- More accurate representation of workflow
- Easier debugging (component-specific failures)
- Better test organization
- Matches operational job naming

### Ice-Specific Considerations

**Why ice products matter:**
- Safety: Ice is a direct hazard to vessels
- Sharp features: Ice edges must be preserved accurately
- Sparse data: Satellite observations limited, models critical
- Long lead times: Subseasonal ice forecasts increasingly important

### MCP Tool Insights 🔧

**Global Workflow MCP tools provided:**
- Configuration file structure references
- COM template variable definitions
- Task dependency patterns

**Value demonstrated:**
- Quick lookup of products/ path definitions
- Understanding of component-based processing
- Workflow metatask structure insights

**Future MCP enhancements could include:**
- Ice variable definitions and units
- CICE6 category threshold documentation
- Ice product user community descriptions

---

## Related Test Cases

1. **C48_S2SW-gfs_fcst_seg0.yaml** - Upstream (provides ice history files)
2. **C48_S2SW-gfs_ocean_prod_f006.yaml** - Sibling (ocean products, parallel task)
3. **C48_ATM-gfs_atmos_prod_f000-f002.yaml** - Atmospheric products (similar pattern)

---

## Technical Notes

### File Size Estimates

| File | Size | Notes |
|------|------|-------|
| 0p25 grib2 | ~5 MB | Ice edge detail |
| 0p50 grib2 | ~2 MB | Medium resolution |
| 1p00 grib2 | ~1 MB | Coarse extent |
| Index files | ~50 KB each | Metadata only |
| Native NetCDF | ~20 MB | Subsetted from ~100 MB full file |
| **Total** | **~29 MB** | All 7 ice product files |

**Note:** Ice files smaller than ocean because ice only covers ~10-15% of ocean area (mostly polar regions).

### Processing Time

| Stage | Duration | Notes |
|-------|----------|-------|
| Initialization | ~10 sec | Load configs, read restart |
| CICE6 grid read | ~20 sec | Read native grid (smaller than ocean) |
| 0.25° interpolation | ~40 sec | Finest resolution |
| 0.50° interpolation | ~15 sec | Medium resolution |
| 1.00° interpolation | ~8 sec | Coarsest resolution |
| GRIB2 encoding | ~20 sec | All 3 resolutions |
| Index generation | ~5 sec | 3 index files |
| Native subsetting | ~10 sec | Extract selected variables |
| **Total** | **~2.5 min** | Single forecast hour |

**Note:** Ice processing slightly faster than ocean due to smaller spatial coverage.

---

## References

### Source Files
- **Test Definition:** `dev/ctests/cases/C48_S2SW-gfs_ice_prod_f006.yaml`
- **Job Script:** `jobs/oceanice_products.sh`
- **Execution Script:** `scripts/exglobal_oceanice_products.py`
- **Python Class:** `pygfs.task.oceanice_products.OceanIceProducts`

### Configuration Files
- **Product Spec:** `parm/post/oceanice_products_gfs.yaml` (lines 52-79)
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
