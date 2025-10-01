# Ocean/Ice Products Test Case Split Summary

## Overview
The ocean and ice products test case has been split from a single combined test into two separate component-specific tests to accurately reflect the actual Rocoto workflow structure.

## Workflow Architecture Analysis

### Rocoto XML Structure
The workflow defines TWO separate parallel metatasks for ocean and ice products:

```xml
<metatask name="gfs_ocean_prod">
  <var name="fhr_list">6 12 18 24 30 36 42 48</var>
  <envar><name>COMPONENT</name><value>ocean</value></envar>
  <task name="gfs_ocean_prod_f#fhr_list#" maxtries="&MAXTRIES;">
    <command>&JOBS_DIR;/oceanice_products.sh</command>
  </task>
</metatask>

<metatask name="gfs_ice_prod">
  <var name="fhr_list">6 12 18 24 30 36 42 48</var>
  <envar><name>COMPONENT</name><value>ice</value></envar>
  <task name="gfs_ice_prod_f#fhr_list#" maxtries="&MAXTRIES;">
    <command>&JOBS_DIR;/oceanice_products.sh</command>
  </task>
</metatask>
```

**Key Observations:**
1. Both metatasks use the **same job script** (`oceanice_products.sh`)
2. Each sets a different `COMPONENT` environment variable
3. Both create **separate named tasks** (e.g., `gfs_ocean_prod_f006` vs `gfs_ice_prod_f006`)
4. Tasks run **independently in parallel**

### Verification from Actual Run
```
CYCLE              TASK                    JOBID       STATE   EXIT STATUS   TRIES   DURATION
202103231200       gfs_ocean_prod_f006     16850999    SUCCEEDED    0          1       0:03:22
202103231200       gfs_ice_prod_f006       16851001    SUCCEEDED    0          1       0:03:20
```

## Test Case Organization

### Before (INCORRECT)
**File:** `C48_S2SW-gfs_oceanice_prod.yaml`
- Single test case combining both ocean and ice
- 15 total output files expected
- Did not reflect actual workflow task structure
- Combined both COMPONENT types in one test

### After (CORRECT)
**Two Separate Files:**

#### 1. `C48_S2SW-gfs_ocean_prod_f006.yaml`
**Purpose:** Validates ocean products generation at f006

**Simulates:** `gfs_ocean_prod_f006` task execution
- Job: `jobs/oceanice_products.sh`
- Environment: `COMPONENT=ocean`
- Script: `scripts/exglobal_oceanice_products.py`

**Input Files:**
- Ocean restart: `gdas.20210323/06/model/ocean/restart/20210323.120000.MOM.res.nc`
- Ocean history: `gfs.20210323/12/model/ocean/history/gfs.ocean.t12z.6hr_avg.f006.nc`

**Expected Outputs (7 files):**
```
products/ocean/grib2/0p25/gfs.ocean.t12z.0p25.f006.grib2
products/ocean/grib2/0p25/gfs.ocean.t12z.0p25.f006.grib2.idx
products/ocean/grib2/0p50/gfs.ocean.t12z.0p50.f006.grib2
products/ocean/grib2/0p50/gfs.ocean.t12z.0p50.f006.grib2.idx
products/ocean/grib2/1p00/gfs.ocean.t12z.1p00.f006.grib2
products/ocean/grib2/1p00/gfs.ocean.t12z.1p00.f006.grib2.idx
products/ocean/netcdf/gfs.ocean.t12z.native.f006.nc
```

**Directory Structure Created:**
- `gfs.20210323/12/products/ocean/grib2/{0p25,0p50,1p00}/`
- `gfs.20210323/12/products/ocean/netcdf/`

---

#### 2. `C48_S2SW-gfs_ice_prod_f006.yaml`
**Purpose:** Validates ice products generation at f006

**Simulates:** `gfs_ice_prod_f006` task execution
- Job: `jobs/oceanice_products.sh`
- Environment: `COMPONENT=ice`
- Script: `scripts/exglobal_oceanice_products.py`

**Input Files:**
- Ice restart: `gdas.20210323/06/model/ice/restart/20210323.120000.cice_model.res.nc`
- Ice history: `gfs.20210323/12/model/ice/history/gfs.ice.t12z.6hr_avg.f006.nc`

**Expected Outputs (7 files):**
```
products/ice/grib2/0p25/gfs.ice.t12z.0p25.f006.grib2
products/ice/grib2/0p25/gfs.ice.t12z.0p25.f006.grib2.idx
products/ice/grib2/0p50/gfs.ice.t12z.0p50.f006.grib2
products/ice/grib2/0p50/gfs.ice.t12z.0p50.f006.grib2.idx
products/ice/grib2/1p00/gfs.ice.t12z.1p00.f006.grib2
products/ice/grib2/1p00/gfs.ice.t12z.1p00.f006.grib2.idx
products/ice/netcdf/gfs.ice.t12z.native.f006.nc
```

**Directory Structure Created:**
- `gfs.20210323/12/products/ice/grib2/{0p25,0p50,1p00}/`
- `gfs.20210323/12/products/ice/netcdf/`

## Technical Implementation

### Job Script Integration
**File:** `jobs/oceanice_products.sh`
- Checks `COMPONENT` environment variable
- Sets component-specific configuration paths
- Calls execution script with component context

### Execution Script
**File:** `scripts/exglobal_oceanice_products.py`
- Uses `pygfs.task.oceanice_products.OceanIceProducts` class
- Processes based on `COMPONENT` value
- Generates GRIB2 + netCDF products

### Product Configuration
**File:** `parm/post/oceanice_products_gfs.yaml`
- Lines 23-50: Ocean product specifications
- Lines 52-79: Ice product specifications
- Defines grids, variables, and output formats

## Benefits of Split Structure

### 1. Accurate Workflow Representation
- Test cases now match actual Rocoto metatask structure
- Each test validates one specific workflow task
- Reflects production job naming (gfs_ocean_prod_f006 vs gfs_ice_prod_f006)

### 2. Independent Component Testing
- Ocean failures don't affect ice test results
- Ice failures don't affect ocean test results
- Clearer diagnosis when component-specific issues occur

### 3. Parallel Execution Simulation
- Tests can run independently (like actual workflow)
- Better reflects real-world task parallelism
- Each test has minimal required inputs

### 4. Maintainability
- Component-specific changes only affect one test
- Easier to add new ocean or ice products
- Clear separation of concerns

## Validation Testing

### Test Execution Commands
```bash
# Test ocean products only
ctest -R "C48_S2SW.*ocean.*validate" --verbose

# Test ice products only
ctest -R "C48_S2SW.*ice.*validate" --verbose

# Test both (independent execution)
ctest -R "C48_S2SW.*prod.*validate" --verbose
```

### Expected Results
- Each test should independently pass validation
- Ocean test validates 7 ocean product files
- Ice test validates 7 ice product files
- Total: 14 files validated across 2 separate tests

## Files Modified
- ✅ Created: `dev/ctests/cases/C48_S2SW-gfs_ocean_prod_f006.yaml`
- ✅ Created: `dev/ctests/cases/C48_S2SW-gfs_ice_prod_f006.yaml`
- ✅ Removed: `dev/ctests/cases/C48_S2SW-gfs_oceanice_prod.yaml`
- ✅ Updated: `CTEST_UPDATES_CHANGELOG.md` (Part 5)

## Git History
```bash
commit 8dda765a1
Author: Terrence McGuinness <terrence.mcguinness@noaa.gov>
Date:   [timestamp]

    Split ocean/ice products test into separate component tests
    
    Split C48_S2SW-gfs_oceanice_prod.yaml into two separate test cases:
    - C48_S2SW-gfs_ocean_prod_f006.yaml (ocean component, 7 files)
    - C48_S2SW-gfs_ice_prod_f006.yaml (ice component, 7 files)
```

## Related Documentation
- `CTEST_UPDATES_CHANGELOG.md` - Complete changelog with all 5 parts
- `CTEST_FIXES_COMPLETE_SUMMARY.md` - Overview of all fixes
- `FHOUT_PGBS_FIX_SUMMARY.md` - Forecast hour behavior
- `PATH_FIX_SUMMARY.md` - Directory structure fixes
- `HERA_PATH_VERIFICATION.md` - Manual verification results
- `verify_paths_on_hera.sh` - Automated verification script

## Next Steps
1. ✅ Test cases created and committed
2. ✅ Documentation updated
3. 📋 Run validation tests: `ctest -R "C48_S2SW.*prod.*validate" --verbose`
4. 📋 Verify both ocean and ice tests pass independently
5. 📋 Confirm file counts match expectations (7 files each)

## Conclusion
The ocean/ice test split aligns the CTest framework with the actual Rocoto workflow architecture, providing more accurate component-level validation and better reflecting production job execution patterns.
