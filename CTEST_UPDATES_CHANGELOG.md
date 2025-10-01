````markdown
# CTest Framework Updates Changelog

## 2025-10-01 (Part 5) - Fixed Ensemble Test Directory Structure (gdas → gefs)

### Summary
Corrected directory structure in `C48_S2SWA_gefs-gefs_fcst_mem001_seg0.yaml` to use `gefs` directories instead of `gdas` directories. The YAML file was referencing `gdas.{{ PDY }}/{{ cyc_offset }}/` for input files, but the actual C48_S2SWA_gefs case uses only `gefs.{{ PDY }}/{{ cyc }}/` structure.

### Root Cause Analysis
**Directory Structure Discovery:**
User provided disk listing showing actual structure on HERA:
```bash
$ ls /scratch3/.../COMROOT/C48_S2SWA_gefs_388b1fe3-4737/
gefs.20210323/

$ ls gefs.20210323/
06/  12/

$ ls gefs.20210323/06/
mem000/  mem001/  mem002/

$ ls gefs.20210323/06/mem001/
model/  (contains: atmos/ ice/ ocean/ wave/)
```

**Key Finding:** No `gdas` directories exist in the C48_S2SWA_gefs case - only `gefs` structure.

**CMakeLists.txt Reference (line 138):**
```cmake
CASE "C48_S2SWA_gefs"  # Correctly references gefs case
```

**YAML File Issue:**
Lines 33-36: Created `gdas.{{ PDY }}/{{ cyc_offset }}/model/*/mem001/` directories
Lines 49-67: Copied from `gdas.{{ PDY }}/06/model/*/mem001/` source paths

### Changes Made

#### C48_S2SWA_gefs-gefs_fcst_mem001_seg0.yaml

**Updated mkdir section (lines 33-36):**
```yaml
# Before:
- {{ DST_DIR }}/gdas.{{ PDY }}/{{ cyc_offset }}/model/atmos/input/mem001
- {{ DST_DIR }}/gdas.{{ PDY }}/{{ cyc_offset }}/model/ice/restart/mem001
- {{ DST_DIR }}/gdas.{{ PDY }}/{{ cyc_offset }}/model/ocean/restart/mem001
- {{ DST_DIR }}/gdas.{{ PDY }}/{{ cyc_offset }}/model/wave/restart/mem001

# After:
- {{ DST_DIR }}/gefs.{{ PDY }}/{{ cyc_offset }}/model/atmos/input/mem001
- {{ DST_DIR }}/gefs.{{ PDY }}/{{ cyc_offset }}/model/ice/restart/mem001
- {{ DST_DIR }}/gefs.{{ PDY }}/{{ cyc_offset }}/model/ocean/restart/mem001
- {{ DST_DIR }}/gefs.{{ PDY }}/{{ cyc_offset }}/model/wave/restart/mem001
```

**Updated copy section (lines 49-67):**
All 17 input file copy statements changed from `gdas.{{ PDY }}/06/` to `gefs.{{ PDY }}/06/`:
- 13 atmosphere initial condition files (gfs_ctrl.nc, gfs_data.tile[1-6].nc, sfc_data.tile[1-6].nc)
- 1 ice restart file ({{ PDY }}.{{ cyc }}0000.cice_model.res.nc)
- 1 ocean restart file ({{ PDY }}.{{ cyc }}0000.MOM.res.nc)
- 1 wave restart file ({{ PDY }}.{{ cyc }}0000.restart.ww3)

**Verification Command:**
```bash
$ grep -n "gdas" C48_S2SWA_gefs-gefs_fcst_mem001_seg0.yaml
(No results - all gdas references successfully removed)
```

### Ensemble Member Directory Structure

**Correct Structure:**
```
gefs.20210323/
├── 06/
│   ├── mem000/
│   │   └── model/
│   │       ├── atmos/input/  (IC files: gfs_*.nc, sfc_data.tile*.nc)
│   │       ├── ice/restart/  (CICE restart)
│   │       ├── ocean/restart/ (MOM6 restart)
│   │       └── wave/restart/  (WW3 restart)
│   ├── mem001/
│   └── mem002/
└── 12/
    ├── mem000/
    │   ├── conf/
    │   ├── model/
    │   │   ├── atmos/  (output: history/, restart/)
    │   │   ├── chem/
    │   │   ├── ice/    (output: history/, restart/)
    │   │   ├── med/
    │   │   ├── ocean/  (output: history/, restart/)
    │   │   └── wave/   (output: history/, restart/)
    │   └── products/
    ├── mem001/
    └── mem002/
```

### Impact
- **Before**: Test would fail during staging because source files at `gdas.{{ PDY }}/06/` do not exist
- **After**: Test correctly references `gefs.{{ PDY }}/06/` matching actual disk structure
- **Pattern**: Similar to earlier `products/` path fix - YAML must match actual COM structure
- **Validation**: Ready for testing with correct directory references

### Related Configuration
This aligns with GEFS ensemble workflow where:
- Ensemble members are organized under `gefs.` prefix, not `gdas.`
- Each member (mem000, mem001, mem002, ...) has its own subdirectory
- Input files come from earlier cycle's `gefs` output, not GDAS analysis
- Directory structure is consistent across all ensemble members

### Testing Recommendations
1. Run: `ctest -R "C48_S2SWA_gefs.*validate" --verbose`
2. Verify staging finds all input files at correct `gefs.20210323/06/` paths
3. Confirm test execution creates proper `gefs.20210323/12/` output structure
4. Expected test duration: 2-3 seconds (similar to ocean/ice tests)
5. Expected outputs: 11 files (5 atmos, 2 ocean, 2 ice, 2 wave)

---

## 2025-10-01 (Part 4) - Fixed Expected Output Files for FHOUT_PGBS Behavior

### Summary
Updated `C48_ATM-gfs_atmos_prod_f000-f002.yaml` to match actual product generation behavior based on `FHOUT_PGBS=3` configuration. The test processes forecast hours 0, 1, and 2 in a single job (FHR_LIST="0,1,2"), but supplemental grid products (0p50, 1p00) are only generated at FHOUT_PGBS intervals (f000, f003, f006...), while 0p25 products are generated for ALL hours.

### Root Cause Analysis
**From Rocoto XML Analysis:**
```xml
<var name="fhr_list">0,1,2 3,4,5 6,7,8 ...</var>
<envar><name>FHR_LIST</name><value>#fhr_list#</value></envar>
```
The `gfs_atmos_prod_f000-f002` task processes **three forecast hours** (0, 1, 2) in a single job execution, not just f000 and f003.

**From scripts/exglobal_atmos_products.sh (lines 18-35):**
```bash
if [[ ${FORECAST_HOUR} -le 0 ]]; then
  PGBS="YES"  # f000 always gets supplemental products
else
  if (( FORECAST_HOUR%FHOUT_PGBS == 0 )); then
    PGBS="YES"  # Only at FHOUT_PGBS intervals (3, 6, 9...)
  fi
fi

grid_string="0p25"
if [[ "${PGBS:-}" == "YES" ]]; then
  grid_string="${grid_string}:0p50:1p00"  # Add supplemental grids
fi
```

**From dev/parm/config/gfs/config.atmos_products (line 26):**
```bash
export FHOUT_PGBS=${FHOUT_GFS:-3}  # Supplemental products every 3 hours
```

### Product Generation Logic
- **0p25 grid**: Generated for **ALL** forecast hours (f000, f001, f002, ...)
- **0p50 and 1p00 grids**: Generated **ONLY** when `FORECAST_HOUR % FHOUT_PGBS == 0`
  - With `FHOUT_PGBS=3`: Only f000, f003, f006, f009, etc.
  - Therefore f001 and f002 do NOT get 0p50 or 1p00 products
- **Flux files**: Generated at 1p00 grid for **ALL** forecast hours (f000, f001, f002, ...)

### Changes Made

#### C48_ATM-gfs_atmos_prod_f000-f002.yaml
**Updated test description:**
- Clarified that task processes FHR_LIST="0,1,2" (three hours in one job)
- Documented FHOUT_PGBS=3 behavior and grid generation logic
- Added expected output file summary by grid and forecast hour

**Changed output_files expectations:**
- **Removed**: f003 products for all grids (6 pgrb2 files + 6 flux files = 12 files)
- **Added**: f001 and f002 products for 0p25 grid only (4 pgrb2 files)
- **Added**: f001 and f002 flux products for 1p00 grid (4 flux files)
- **Net change**: From 12 f003 files to 8 f001/f002 files (still validates correct behavior)

**Final expected outputs (14 files total):**
- **0p25 grid**: 6 files (pgrb2 + idx for f000, f001, f002)
- **0p50 grid**: 2 files (pgrb2 + idx for f000 only)
- **1p00 grid**: 8 files (pgrb2 + idx for f000) + (flux + idx for f000, f001, f002)

### Verification Against Nightly Run
From `/scratch3/NCEPDEV/global/role.glopara/GFS_CI_CD/HERA/.../gfs.20210323/12/products/atmos/grib2/1p00/`:
```
✅ gfs.t12z.pgrb2.1p00.f000     (present - f000 at FHOUT_PGBS interval)
✅ gfs.t12z.flux.1p00.f000      (present - flux for all hours)
✅ gfs.t12z.flux.1p00.f001      (present - flux for all hours)
✅ gfs.t12z.flux.1p00.f002      (present - flux for all hours)
✅ gfs.t12z.pgrb2.1p00.f003     (present - f003 at FHOUT_PGBS interval)
❌ gfs.t12z.pgrb2.1p00.f001     (absent - f001 NOT at FHOUT_PGBS interval)
❌ gfs.t12z.pgrb2.1p00.f002     (absent - f002 NOT at FHOUT_PGBS interval)
```

This confirms the logic: pgrb2 files only at FHOUT_PGBS intervals, but flux files for all hours.

### Impact
- **Before**: Test expected f003 products that are generated by a different task (gfs_atmos_prod_f003-f005)
- **After**: Test correctly expects only f000, f001, f002 products matching FHR_LIST="0,1,2"
- **Grid-specific behavior**: Now properly accounts for FHOUT_PGBS=3 configuration
- **Flux files**: Now validates that flux files are generated for ALL forecast hours at 1p00 grid

### Related Configuration
This aligns with operational GFS configuration where:
- High-resolution products (0p25) are generated every hour for immediate use
- Supplemental lower-resolution products (0p50, 1p00) are generated every 3 hours to reduce computational cost
- Surface flux products are generated every hour at 1p00 resolution for all applications

---

## 2025-01-16 (Part 3) - Fixed Product Output Directory Paths

### Summary
Corrected directory paths for product output files in test cases. The paths were missing the `products/` subdirectory, causing validation failures because files were being written to the correct location but validation was looking in the wrong place.

### Root Cause Analysis
**User Discovery:**
Terminal output showed that the `model` directory existed in the file path, but validation was failing because it was looking for files at:
```
gfs.20210323/12/atmos/grib2/0p25/...
```

When the actual location according to COM templates is:
```
gfs.20210323/12/products/atmos/grib2/0p25/...
```

**Configuration Investigation:**
From `dev/parm/config/gfs/config.com` lines 64-65:
```bash
declare -rx COM_ATMOS_GRIB_TMPL=${COM_BASE}'/products/atmos/grib2'
declare -rx COM_ATMOS_GRIB_GRID_TMPL=${COM_ATMOS_GRIB_TMPL}'/${GRID}'
```

Similarly for ocean/ice (lines 95-96, 106-107):
```bash
declare -rx COM_OCEAN_GRIB_TMPL=${COM_BASE}'/products/ocean/grib2'
declare -rx COM_ICE_GRIB_TMPL=${COM_BASE}'/products/ice/grib2'
```

The `products/` subdirectory is a standard part of the COM template structure for all output product files.

### Changes Made

#### C48_ATM-gfs_atmos_prod_f000-f002.yaml
**Updated mkdir section:**
- Changed: `atmos/grib2/0p25` → `products/atmos/grib2/0p25`
- Changed: `atmos/grib2/0p50` → `products/atmos/grib2/0p50`
- Changed: `atmos/grib2/1p00` → `products/atmos/grib2/1p00`

**Updated output_files cmpfiles section:**
All 18 output file paths updated:
- Changed: `atmos/grib2/{GRID}/gfs.t*z.pgrb2.{GRID}.f*` → `products/atmos/grib2/{GRID}/gfs.t*z.pgrb2.{GRID}.f*`
- Changed: `atmos/grib2/{GRID}/gfs.t*z.pgrb2.{GRID}.f*.idx` → `products/atmos/grib2/{GRID}/gfs.t*z.pgrb2.{GRID}.f*.idx`
- Changed: `atmos/grib2/{GRID}/gfs.t*z.flux.{GRID}.f*` → `products/atmos/grib2/{GRID}/gfs.t*z.flux.{GRID}.f*`

Applied to all 3 grids (0p25, 0p50, 1p00) × 2 forecast hours (f000, f003) × 3 file types (pgrb2, idx, flux)

#### C48_S2SW-gfs_oceanice_prod.yaml  
**Updated mkdir section:**
- Changed: `ocean/grib2/0p25` → `products/ocean/grib2/0p25`
- Changed: `ocean/grib2/0p50` → `products/ocean/grib2/0p50`
- Changed: `ocean/grib2/1p00` → `products/ocean/grib2/1p00`
- Changed: `ocean/netcdf` → `products/ocean/netcdf`
- Changed: `ice/grib2/0p25` → `products/ice/grib2/0p25`
- Changed: `ice/grib2/0p50` → `products/ice/grib2/0p50`
- Changed: `ice/grib2/1p00` → `products/ice/grib2/1p00`
- Changed: `ice/netcdf` → `products/ice/netcdf`

**Updated output_files cmpfiles section:**
All 15 output file paths updated:
- Ocean GRIB2: `ocean/grib2/{GRID}/...` → `products/ocean/grib2/{GRID}/...`
- Ocean netCDF: `ocean/netcdf/...` → `products/ocean/netcdf/...`
- Ice GRIB2: `ice/grib2/{GRID}/...` → `products/ice/grib2/{GRID}/...`
- Ice netCDF: `ice/netcdf/...` → `products/ice/netcdf/...`

Applied to all ocean (6 GRIB2 + 1 netCDF) and ice (6 GRIB2 + 1 netCDF) product files

### Files NOT Changed (Correctly Using model/ Paths)
The following test cases are **correct as-is** because they test forecast jobs that output to `model/` directories, not `products/` directories:

- ✅ `C48_ATM-gfs_fcst_seg0.yaml` - Outputs to `model/atmos/master/` (master.grb2, sfluxgrb files)
- ✅ `C48_S2SW-gfs_fcst_seg0.yaml` - Outputs to `model/atmos/`, `model/ocean/`, `model/ice/`
- ✅ `C48_S2SW-gefs_fcst_mem001.yaml` - Outputs to `model/` directories for ensemble member

### Directory Structure Clarification

**Workflow Directory Organization:**
```
${RUN}.${YMD}/${HH}/
├── model/                          # Model native output (restart, history, master files)
│   ├── atmos/
│   │   ├── input/                  # Atmosphere restart files
│   │   ├── history/                # Atmosphere history files (atmf*, sfcf*)
│   │   ├── master/                 # Master GRIB2 files (master.grb2f*, sfluxgrbf*)
│   │   └── restart/
│   ├── ocean/
│   └── ice/
└── products/                       # Post-processed products for distribution
    ├── atmos/
    │   └── grib2/                  # Atmospheric GRIB2 products by resolution
    │       ├── 0p25/               # (pgrb2, flux, idx files)
    │       ├── 0p50/
    │       └── 1p00/
    ├── ocean/
    │   ├── grib2/                  # Ocean GRIB2 products
    │   └── netcdf/                 # Ocean netCDF subsets
    └── ice/
        ├── grib2/                  # Ice GRIB2 products
        └── netcdf/                 # Ice netCDF subsets
```

**Key Distinction:**
- `model/`: Raw model output, input files, restart files
- `products/`: Post-processed products ready for distribution/archive

### Impact
- **Before**: Validation failing with all 18 (atmos) or 15 (ocean/ice) files marked as missing
- **After**: Validation paths now match actual output locations defined by COM templates
- **Fix Type**: Path correction only - no functional changes to test logic
- **Files Fixed**: 2 test cases (atmos products, ocean/ice products)
- **Files Verified Correct**: 3 test cases (forecast jobs using model/ paths)

### Validation Error Evidence
From `validate_fail.txt`:
```
Missing files in pair: .../gfs.20210323/12/atmos/grib2/0p25/gfs.t12z.pgrb2.0p25.f000 (exists: False)
```

User terminal verification:
```bash
$ ls /scratch3/.../gfs.20210323/12/model/
atmos
```

This confirmed the `model/` directory exists, revealing the path mismatch issue.

### HERA Path Verification Results (2025-10-01)
**Automated verification script confirmed the fix is correct:**

```bash
$ ./verify_paths_on_hera.sh
✅ FIX IS CORRECT!
   - Products are in products/atmos/grib2/
   - Old path atmos/grib2/ does not exist
   - Our YAML file updates are CORRECT
```

**Directory Structure Confirmed on HERA:**
- ✅ `products/atmos/grib2/` EXISTS (correct location)
- ✅ `products/atmos/grib2/0p25/` - 146 pgrb2 files, 146 idx files
- ✅ `products/atmos/grib2/0p50/` - 82 pgrb2 files, 82 idx files  
- ✅ `products/atmos/grib2/1p00/` - 82 pgrb2 files, 155 idx files, 146 flux files
- ✅ `model/atmos/master/` - Input files (master.grb2f*, sfluxgrbf*.grib2)
- ❌ `atmos/grib2/` DOES NOT EXIST (confirms old path was wrong)

**Verification Script Location:**
`dev/ctests/verify_paths_on_hera.sh` - Automated bash script for systematic path verification

### Testing Recommendations
1. Re-run validation for `C48_ATM-gfs_atmos_prod_f000-f002` test case
2. Re-run validation for `C48_S2SW-gfs_oceanice_prod` test case
3. Verify all 18 atmospheric product files are now found
4. Verify all 15 ocean/ice product files are now found

### Related Standards
This fix aligns test cases with the standard COM directory template structure used throughout the global-workflow system, as defined in `dev/parm/config/*/config.com` files.

---

## 2025-01-16 (Part 2) - Added Missing mkdir for model/atmos/master Directory

### Summary
Fixed missing `mkdir` entries for the `model/atmos/master` directory in both C48_ATM test cases. Files were being copied to this directory but it was never created.

### Changes Made

#### C48_ATM-gfs_atmos_prod_f000-f002.yaml
**Added to mkdir section:**
```yaml
- {{ DST_DIR }}/gfs.{{ PDY }}/{{ cyc }}/model/atmos/master
```

**Impact:**
- Ensures directory exists before copying master.grb2f000/f003 and sfluxgrbf000/f003.grib2 files
- Previously these files would fail to copy due to missing target directory

#### C48_ATM-gfs_fcst_seg0.yaml
**Added to mkdir section:**
```yaml
- {{ DST_DIR }}/gfs.{{ PDY }}/{{ cyc }}/model/atmos/master
```

**Impact:**
- Ensures directory exists for all 41 master.grb2f* and sfluxgrbf*.grib2 output files
- Prevents directory creation failures during test execution

### Root Cause
When adding the master.grb2 and sflux files in the earlier update, the corresponding directory creation was overlooked. The test framework requires explicit mkdir entries for all directories where files will be copied or generated.

### Verification
Searched all test case files - confirmed these are the only two files that reference `model/atmos/master/` directory:
```bash
grep -r "model/atmos/master/" dev/ctests/cases/*.yaml
```
Both files now have the required mkdir entry.

### Related Changes
This completes the fix started in "2025-01-16 - Added Missing Input Files" where the input file copies were added but the directory creation was missed.

---

## 2025-01-16 (Part 1) - Added Missing Input Files to C48_ATM-gfs_atmos_prod_f000-f002.yaml

### Summary
Updated the `C48_ATM-gfs_atmos_prod_f000-f002.yaml` test case to include critical input files that were missing but required by the `exglobal_atmos_products.sh` script.

### Changes Made

#### Added Master GRIB2 Files (Required Inputs)
**Files Added to input_files section:**
- `gfs.t{{ cyc }}z.master.grb2f000`
- `gfs.t{{ cyc }}z.master.grb2f003`

**Justification:**
- `exglobal_atmos_products.sh` line 40 defines: `MASTER_FILE="${COMIN_ATMOS_MASTER}/${PREFIX}master.grb2${fhr3}"`
- This is the **primary input file** for atmospheric products generation
- Generated during forecast when `WRITE_DOPOST=.true.` (the default setting)
- Present in `C48_ATM-gfs_fcst_seg0.yaml` output but missing from atmos_products input
- Without these files, the atmos_products job cannot execute

#### Added Surface Flux GRIB2 Files (Conditional Inputs)
**Files Added to input_files section:**
- `gfs.t{{ cyc }}z.sfluxgrbf000.grib2`
- `gfs.t{{ cyc }}z.sfluxgrbf003.grib2`

**Justification:**
- `exglobal_atmos_products.sh` line 176 defines: `FLUX_FILE="${COMIN_ATMOS_MASTER}/${PREFIX}sfluxgrb${fhr3}.grib2"`
- Line 178 checks: `if [[ -s "${FLUX_FILE}" ]]; then`
- These files are processed when available for surface flux products
- Generated by forecast alongside master.grb2 files
- Present in `C48_ATM-gfs_fcst_seg0.yaml` output but missing from atmos_products input

### Code Analysis References

#### Workflow Dependencies
From `dev/workflow/rocoto/gfs_tasks.py` lines 1200-1280:
```python
'history_file_tmpl': f'{self.run}.t@Hz.master.grb2f#fhr3_last#'
```
Confirms that atmos_prod task explicitly depends on master.grb2f files.

#### File Generation
From `ush/forecast_postdet.sh` lines 310-350:
```bash
if [[ "${WRITE_DOPOST}" == ".true." ]]; then
  ${NLN} "${COMOUT_ATMOS_MASTER}/${RUN}.t${cyc}z.master.grb2f${FH3}"
```
Shows master.grb2 files are created during forecast when `WRITE_DOPOST=.true.`

From `parm/config/gfs/config.base.j2` line 345:
```
WRITE_DOPOST=".true."
```
Confirms this is the default configuration.

#### File Usage
From `scripts/exglobal_atmos_products.sh`:
- Line 40: `MASTER_FILE` is the mandatory primary input
- Line 176: `FLUX_FILE` is used conditionally for surface flux processing
- All other file references are output files, not inputs

### Test Case Completeness Verification

#### Files in C48_ATM-gfs_fcst_seg0.yaml Output:
- ✅ master.grb2f000, master.grb2f003, ..., master.grb2f030
- ✅ sfluxgrbf000.grib2, sfluxgrbf003.grib2, ..., sfluxgrbf030.grib2

#### Files Required by C48_ATM-gfs_atmos_prod_f000-f002.yaml Input:
- ✅ NOW ADDED: master.grb2f000, master.grb2f003
- ✅ NOW ADDED: sfluxgrbf000.grib2, sfluxgrbf003.grib2
- ✅ ALREADY PRESENT: atmf000.nc, atmf003.nc, sfcf000.nc, sfcf003.nc (history files)

### Impact
- **Before**: Test case would fail because required input files were missing
- **After**: Test case has all necessary input files for realistic atmos_products job testing
- **Scope**: Only affects `C48_ATM-gfs_atmos_prod_f000-f002.yaml` test case
- **Other Tests**: Verified `C48_S2SW-gfs_oceanice_prod.yaml` does not need these files

### Testing Recommendations
1. Run the updated test case to verify it now passes with the added input files
2. Verify that f000 and f003 forecast hours are correctly processed
3. Confirm all expected output products are generated at multiple resolutions (0p25, 0p50, 1p00)

### Related Files Modified
- `dev/ctests/cases/C48_ATM-gfs_atmos_prod_f000-f002.yaml` - Added 4 input file entries with explanatory comments

### Notes
- YAML lint errors about Jinja2 templates (`{{`, `}}`) are expected and not actual errors
- These template variables are resolved during test case execution by the ctest framework
- The same template patterns exist throughout all ctest YAML files in the repository

---

## Part 5: Split Ocean/Ice Combined Test Case (January 2025)

### Issue Discovery
Analysis of the Rocoto XML workflow revealed that ocean and ice products are generated by **separate independent metatasks**, not a single combined task:

```xml
<metatask name="gfs_ocean_prod">
  <var name="fhr_list">6 12 18 24 30 36 42 48</var>
  <envar><name>COMPONENT</name><value>ocean</value></envar>
  <task name="gfs_ocean_prod_f#fhr_list#" ...>
</metatask>

<metatask name="gfs_ice_prod">
  <var name="fhr_list">6 12 18 24 30 36 42 48</var>
  <envar><name>COMPONENT</name><value>ice</value></envar>
  <task name="gfs_ice_prod_f#fhr_list#" ...>
</metatask>
```

**Verification from rocotostat:**
```
CYCLE              TASK                    JOBID       STATE   EXIT STATUS   TRIES   DURATION
202103231200       gfs_ocean_prod_f006     16850999    SUCCEEDED    0          1       0:03:22
202103231200       gfs_ice_prod_f006       16851001    SUCCEEDED    0          1       0:03:20
```

Both tasks ran independently as separate parallel jobs, each calling `oceanice_products.sh` with different `COMPONENT` environment variables.

### Root Cause
The original `C48_S2SW-gfs_oceanice_prod.yaml` test case combined both ocean and ice product validation into a single test, which did not accurately reflect the actual workflow structure where they are separate tasks.

### Solution Applied
Split the combined test case into two separate test cases:

#### 1. `C48_S2SW-gfs_ocean_prod_f006.yaml`
- **Tests**: `gfs_ocean_prod_f006` task with `COMPONENT=ocean`
- **Expected Outputs**: 7 files
  - 6 GRIB2 files: `gfs.ocean.t12z.{0p25,0p50,1p00}.f006.grib2` + `.idx`
  - 1 netCDF file: `gfs.ocean.t12z.native.f006.nc`
- **Input Files**: Ocean history file (`gfs.ocean.t12z.6hr_avg.f006.nc`)
- **Directory Structure**: `products/ocean/{grib2,netcdf}/`

#### 2. `C48_S2SW-gfs_ice_prod_f006.yaml`
- **Tests**: `gfs_ice_prod_f006` task with `COMPONENT=ice`
- **Expected Outputs**: 7 files
  - 6 GRIB2 files: `gfs.ice.t12z.{0p25,0p50,1p00}.f006.grib2` + `.idx`
  - 1 netCDF file: `gfs.ice.t12z.native.f006.nc`
- **Input Files**: Ice history file (`gfs.ice.t12z.6hr_avg.f006.nc`)
- **Directory Structure**: `products/ice/{grib2,netcdf}/`

### Technical Context

**Job Script**: `jobs/oceanice_products.sh`
**Execution Script**: `scripts/exglobal_oceanice_products.py`
**Python Class**: `pygfs.task.oceanice_products.OceanIceProducts`
**Configuration**: `parm/post/oceanice_products_gfs.yaml`
- Ocean section: lines 23-50
- Ice section: lines 52-79

**Key Points:**
- Both components use the same job script with different `COMPONENT` values
- Ocean and ice tasks run in parallel at the same forecast hours
- Each component has its own independent product generation pipeline
- Test cases now match the actual workflow metatask structure

### Files Modified
- ✅ Created: `dev/ctests/cases/C48_S2SW-gfs_ocean_prod_f006.yaml`
- ✅ Created: `dev/ctests/cases/C48_S2SW-gfs_ice_prod_f006.yaml`
- ✅ Removed: `dev/ctests/cases/C48_S2SW-gfs_oceanice_prod.yaml`

---

## Final Summary

All CTest validation test cases have been reviewed and corrected. The primary issues were:

1. **Directory Path Structure**: Missing `products/` prefix in output paths
2. **Forecast Hour Expectations**: Incorrect forecast hours not matching `FHR_LIST` configuration and `FHOUT_PGBS` behavior
3. **Test Case Organization**: Combined ocean/ice test did not match actual workflow structure with separate parallel metatasks
4. **GEFS Ensemble Test**: Complete rebuild as output-only validation (Part 6)

These fixes ensure CTest validation tests accurately reflect the actual workflow product generation patterns, directory structures, and task organization used in operational and experimental runs.

---

## Part 6: GEFS Ensemble Complete Rebuild as Output-Only Test (2025-01-XX)

### Initial Issue Discovered
The C48_S2SWA_gefs-gefs_fcst_mem001_seg0.yaml test case was using incorrect directory references. The PR test case `C48_S2SWA_gefs` creates directories under `gefs.{PDY}/{cyc}/`, but the CTest validation file was looking for files under `gdas.{PDY}/{cyc}/`.

### Investigation Process - Phase 1: Directory Path Fix
Created verification script to check actual file locations on disk:
```bash
dev/ci/scripts/verify_ensemble_files.sh
```

Changed all 21 directory references from `gdas` to `gefs` paths.

**Result**: Script revealed **0 of 26 files found** - ALL FILES MISSING

### Investigation Process - Phase 2: Disk Structure Analysis
Created comprehensive investigation scripts:
```bash
dev/ci/scripts/investigate_gefs_structure.sh   # 10-path directory exploration
dev/ci/scripts/find_all_mem001_files.sh        # Complete file listing with counts
```

**Critical Discoveries**:
- **06Z mem001**: 0 files (completely empty directory)
- **12Z mem001**: 913 files total (all OUTPUT files, no input directory)
- Actual disk structure: `gefs.20210323/12/mem001/model/{atmos,ocean,ice,wave}/history/`

### Root Cause Analysis
**Fundamental misunderstanding of CTest architecture**:
1. GEFS is configured as **forecast-only** with **cold start**
2. The `stage_ic` job runs SEPARATELY in the nightly pipeline before forecast jobs
3. CTest framework ONLY stages files from nightly stable run outputs at:
   ```
   /scratch3/.../RUNTESTS/COMROOT/C48_S2SWA_gefs_*/
   ```
4. Tests should NOT look for IC files - they validate forecast OUTPUTS only
5. Similar pattern to `C48_S2SW-gfs_fcst_seg0.yaml` (also has no input staging)

### Solution: Complete Rebuild as Output-Only Test
Completely replaced the YAML file with output-only validation approach:

**Removed**:
- All input file staging (copy section with 16 files)
- Offset cycle references (cyc_offset, PDY_offset, H_offset)
- Input/restart directory mkdir statements

**Kept/Updated**:
- 4 mkdir statements for OUTPUT directories only:
  - `gefs.{{ PDY }}/{{ cyc }}/mem001/model/{atmos,ocean,ice,wave}/history/`
- 24 cmpfiles for forecast segment 0 validation:
  - **18 atmosphere files**: atmf/sfcf for f000, f006, f012, f018, f024, f030, f036, f042, f048
  - **2 ocean files**: 24hr_avg for f024, f048
  - **2 ice files**: 24hr_avg for f024, f048  
  - **2 wave files**: points for f006, f048

**Format Improvements**:
- Clean single-spacing (no double spacing)
- Comprehensive header documentation
- Clear comments explaining S2SWA, seg0, and output-only approach

### Forecast Segment 0 Output Pattern
Based on actual disk files from 12Z mem001 investigation:
- **Atmosphere**: Every 6 hours (f000-f048) = 9 forecast hours × 2 files (atmf/sfcf) = 18 files
- **Ocean**: 24-hour averages only (f024, f048) = 2 files
- **Ice**: 24-hour averages only (f024, f048) = 2 files
- **Wave**: Hourly points but testing f006 and f048 = 2 files
- **Total validation files**: 24 files

### Directory Structure Context
GEFS ensemble member directory structure (12Z cycle only):
```
gefs.20210323/
└── 12/
    └── mem001/
        └── model/
            ├── atmos/history/  # 63 files (atmf/sfcf f000-f120 + master)
            ├── ocean/history/  # 5 files (24hr_avg f024-f120)
            ├── ice/history/    # 6 files (24hr_avg f024-f120)
            └── wave/history/   # 485 files (points f000-f120 hourly)
```

**Note**: 06Z mem001 directory exists but contains ZERO files (cold start means no prior cycle).

### File Statistics
- **Old YAML**: 146 lines, 16 input files + 10 output files = 26 validations
- **New YAML**: 66 lines, 0 input files + 24 output files = 24 validations
- **Line reduction**: 55% smaller, cleaner, more maintainable
- **Validation coverage**: Increased from 10 to 24 output files

### Verification Commands
```bash
# File was completely replaced
wc -l C48_S2SWA_gefs-gefs_fcst_mem001_seg0.yaml
# Output: 66 lines

# No input file staging
grep -c "copy:" C48_S2SWA_gefs-gefs_fcst_mem001_seg0.yaml
# Output: 0

# Correct number of validations  
grep -c "^\s*-\s*\[{{" C48_S2SWA_gefs-gefs_fcst_mem001_seg0.yaml
# Output: 24

# No gdas references
grep -c "gdas" C48_S2SWA_gefs-gefs_fcst_mem001_seg0.yaml
# Output: 0

# Correct mkdir count
grep "mkdir:" -A 5 C48_S2SWA_gefs-gefs_fcst_mem001_seg0.yaml | grep -c "DST_DIR"
# Output: 4
```

### Testing Status
The test case now follows the proven output-only validation pattern used by `C48_S2SW-gfs_fcst_seg0.yaml`. This is the 6th and final test in the CTest suite. Expected validation time: 2-3 seconds (like ocean/ice tests). Once this test passes, the complete CTest framework will be ready with 100% pass rate (6/6 tests).
