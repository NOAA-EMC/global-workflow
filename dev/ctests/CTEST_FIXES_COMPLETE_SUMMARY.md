# Complete Summary of CTest Case Fixes

## Overview
Fixed critical path and forecast hour issues in CTest validation cases for C48_ATM and C48_S2SW configurations.

## Test Cases Status

### ✅ C48_ATM-gfs_fcst_seg0.yaml
**Status**: CORRECT - No changes needed  
**Purpose**: Tests forecast job (JGLOBAL_FORECAST)  
**Paths Used**: `model/atmos/master/`, `model/atmos/history/`  
**Why Correct**: Forecast jobs output to model/ directories

### ✅ C48_ATM-gfs_atmos_prod_f000-f002.yaml  
**Status**: FIXED (2 issues)  
**Purpose**: Tests atmospheric products generation for FHR_LIST="0,1,2"  

**Issue 1 - Missing products/ prefix (Part 3)**:
- Changed: `atmos/grib2/` → `products/atmos/grib2/`
- Affects: All output file paths (mkdir and cmpfiles sections)

**Issue 2 - Wrong forecast hours (Part 4)**:
- Was expecting: f000 and f003 (wrong - f003 is in different task)
- Now expects: f000, f001, f002 (correct - matches FHR_LIST)
- Understanding: FHOUT_PGBS=3 means supplemental grids only at 3-hour intervals
  - 0p25 grid: f000, f001, f002 (all hours)
  - 0p50 grid: f000 only (FHOUT_PGBS interval)
  - 1p00 grid: pgrb2 f000 only, flux f000/f001/f002 (flux always generated)

**Final Expected Files (14 total)**:
- 0p25: 6 files (pgrb2 + idx for f000, f001, f002)
- 0p50: 2 files (pgrb2 + idx for f000 only)
- 1p00: 6 files (pgrb2 + idx for f000, flux + idx for f000/f001/f002)

### ✅ C48_S2SW-gfs_fcst_seg0.yaml
**Status**: CORRECT - No changes needed  
**Purpose**: Tests S2SW forecast job  
**Paths Used**: `model/atmos/`, `model/ocean/`, `model/ice/`  
**Why Correct**: Forecast jobs output to model/ directories

### ✅ C48_S2SW-gfs_oceanice_prod.yaml
**Status**: FIXED (1 issue)  
**Purpose**: Tests ocean/ice products generation for f006  

**Issue - Missing products/ prefix (Part 3)**:
- Changed ocean: `ocean/grib2/` → `products/ocean/grib2/`
- Changed ocean: `ocean/netcdf` → `products/ocean/netcdf`
- Changed ice: `ice/grib2/` → `products/ice/grib2/`
- Changed ice: `ice/netcdf` → `products/ice/netcdf`
- Affects: 8 mkdir entries + 15 output file paths

**Expected Files (15 total)**:
- Ocean: 6 GRIB2 files (0p25, 0p50, 1p00) + 1 netCDF
- Ice: 6 GRIB2 files (0p25, 0p50, 1p00) + 1 netCDF
- Ocean/ice products are generated at 6-hour intervals (f006, f012, ...)

### ✅ C48_S2SW-gefs_fcst_mem001.yaml
**Status**: CORRECT - No changes needed  
**Purpose**: Tests GEFS ensemble forecast member  
**Paths Used**: `model/` directories  
**Why Correct**: Forecast jobs output to model/ directories

## Key Technical Insights

### 1. Directory Structure
```
${RUN}.${YMD}/${HH}/
├── model/              # Raw forecast output (history, master, restart)
│   ├── atmos/
│   │   ├── history/   # Native model output (atmf*, sfcf*)
│   │   ├── master/    # Post-processed GRIB2 (master.grb2f*, sfluxgrbf*)
│   │   └── restart/
│   ├── ocean/
│   └── ice/
└── products/          # Distribution-ready products
    ├── atmos/grib2/   # Atmospheric GRIB2 by resolution
    ├── ocean/grib2/   # Ocean GRIB2 products
    ├── ocean/netcdf/  # Ocean netCDF subsets
    ├── ice/grib2/     # Ice GRIB2 products
    └── ice/netcdf/    # Ice netCDF subsets
```

### 2. FHR_LIST in Rocoto XML
Tasks process **multiple forecast hours per job**:
```xml
<var name="fhr_list">0,1,2 3,4,5 6,7,8 ...</var>
<envar><name>FHR_LIST</name><value>#fhr_list#</value></envar>
```
- `gfs_atmos_prod_f000-f002`: Processes hours 0, 1, AND 2
- `gfs_atmos_prod_f003-f005`: Processes hours 3, 4, AND 5
- Test names reflect the range, not individual hours

### 3. FHOUT_PGBS Configuration
From `dev/parm/config/gfs/config.atmos_products`:
```bash
export FHOUT_PGBS=${FHOUT_GFS:-3}  # Supplemental products every 3 hours
```

Product generation logic in `scripts/exglobal_atmos_products.sh`:
```bash
# f000 always gets supplemental products
if [[ ${FORECAST_HOUR} -le 0 ]]; then
  PGBS="YES"
# Other hours only at FHOUT_PGBS intervals
else
  if (( FORECAST_HOUR%FHOUT_PGBS == 0 )); then
    PGBS="YES"
  fi
fi

# Determine grids
grid_string="0p25"  # Always
if [[ "${PGBS:-}" == "YES" ]]; then
  grid_string="${grid_string}:0p50:1p00"  # Add supplemental
fi
```

**Result**:
- 0p25: ALL hours (0, 1, 2, 3, 4, ...)
- 0p50/1p00: Only at intervals (0, 3, 6, 9, ...)
- Flux at 1p00: ALL hours (special case)

### 4. COM Templates
From `dev/parm/config/gfs/config.com`:
```bash
COM_ATMOS_GRIB_TMPL=${COM_BASE}'/products/atmos/grib2'
COM_OCEAN_GRIB_TMPL=${COM_BASE}'/products/ocean/grib2'
COM_ICE_GRIB_TMPL=${COM_BASE}'/products/ice/grib2'
```

All product files MUST have `products/` prefix.

## Files Modified

### Test Cases (2 files)
1. `dev/ctests/cases/C48_ATM-gfs_atmos_prod_f000-f002.yaml`
   - Fixed products/ path prefix (3 mkdir + 18→14 output paths)
   - Fixed forecast hours (f000,f003 → f000,f001,f002)
   - Added FHOUT_PGBS logic explanation

2. `dev/ctests/cases/C48_S2SW-gfs_oceanice_prod.yaml`
   - Fixed products/ path prefix (8 mkdir + 15 output paths)

### Documentation (4 files)
1. `CTEST_UPDATES_CHANGELOG.md` - Complete changelog (Parts 1-4)
2. `dev/ctests/PATH_FIX_SUMMARY.md` - Directory structure analysis
3. `dev/ctests/HERA_PATH_VERIFICATION.md` - Manual verification steps
4. `dev/ctests/FHOUT_PGBS_FIX_SUMMARY.md` - FHOUT_PGBS behavior explanation

### Verification Tools (1 file)
1. `dev/ctests/verify_paths_on_hera.sh` - Automated path verification script

## Verification Results

### HERA Path Verification (verified 2025-10-01)
```bash
✅ products/atmos/grib2/0p25/ - EXISTS with f000, f001, f002 files
✅ products/atmos/grib2/0p50/ - EXISTS with f000 files only
✅ products/atmos/grib2/1p00/ - EXISTS with f000 pgrb2 + f000/f001/f002 flux
✅ model/atmos/master/ - EXISTS with master.grb2f* and sfluxgrbf*
❌ atmos/grib2/ - DOES NOT EXIST (confirms old path was wrong)
```

**Conclusion**: All fixes are correct and match actual nightly run output.

## Testing Instructions

### Run Individual Tests
```bash
cd /home/tmcguinness/NOAA/global-workflow_forked/build

# Test atmospheric products (should now pass with 14 files)
ctest -R C48_ATM-gfs_atmos_prod_f000-f002_validate --verbose

# Test ocean/ice products (should now pass with 15 files)
ctest -R C48_S2SW-gfs_oceanice_prod_validate --verbose
```

### Run All C48 Tests
```bash
ctest -R "C48_(ATM|S2SW)" --verbose
```

## Lessons Learned

1. **Always check COM templates** - Directory structure is defined in config.com files
2. **Understand FHR_LIST** - Tasks process multiple hours per job, not single hours
3. **FHOUT_PGBS matters** - Supplemental products only at configured intervals
4. **Verify against actual output** - Check nightly run directories to confirm behavior
5. **Product type differences** - pgrb2 vs flux have different generation rules
6. **Use automation** - Created verification script to systematically check paths

## Next Steps

1. ✅ Commit all changes with comprehensive documentation
2. ✅ Push to branch `ctest_case_updates`
3. ⏭️ Run ctest validation on HERA
4. ⏭️ Verify all tests pass
5. ⏭️ Create merge request to develop branch
