# CTest Product Path Fix Summary

## Issue Discovery

During validation testing, all product output files were reported as missing despite successful test execution. Investigation revealed that the validation was looking in the wrong directory paths.

**Error Pattern:**
```
Missing files in pair: .../gfs.20210323/12/atmos/grib2/0p25/gfs.t12z.pgrb2.0p25.f000 (exists: False)
```

**User Verification:**
```bash
Terry.McGuinness (hfe07) build $ ls /scratch3/.../gfs.20210323/12/model/
atmos
```

This showed that files were being created in `model/` subdirectory structure, but validation paths were missing the `products/` prefix.

## Root Cause

Test case YAML files used simplified paths like `atmos/grib2/0p25/` instead of the standard COM template paths `products/atmos/grib2/0p25/`.

The global-workflow COM templates (from `dev/parm/config/gfs/config.com`) define:

```bash
# Atmospheric products
COM_ATMOS_GRIB_TMPL=${COM_BASE}'/products/atmos/grib2'
COM_ATMOS_GRIB_GRID_TMPL=${COM_ATMOS_GRIB_TMPL}'/${GRID}'

# Ocean products  
COM_OCEAN_GRIB_TMPL=${COM_BASE}'/products/ocean/grib2'
COM_OCEAN_GRIB_GRID_TMPL=${COM_OCEAN_GRIB_TMPL}'/${GRID}'

# Ice products
COM_ICE_GRIB_TMPL=${COM_BASE}'/products/ice/grib2'
COM_ICE_GRIB_GRID_TMPL=${COM_ICE_GRIB_TMPL}'/${GRID}'
```

All product output paths require the `products/` subdirectory prefix.

## Solution Applied

### Files Modified

1. **C48_ATM-gfs_atmos_prod_f000-f002.yaml**
   - Updated 3 mkdir entries (0p25, 0p50, 1p00 grids)
   - Updated 18 output file paths (pgrb2, idx, flux × 3 grids × 2 times)
   - Pattern: `atmos/grib2/{GRID}/` → `products/atmos/grib2/{GRID}/`

2. **C48_S2SW-gfs_oceanice_prod.yaml**
   - Updated 8 mkdir entries (ocean + ice, grib2 + netcdf)
   - Updated 15 output file paths (ocean: 7 files, ice: 8 files)
   - Pattern: `{ocean|ice}/{grib2|netcdf}/` → `products/{ocean|ice}/{grib2|netcdf}/`

### Files Verified Correct (No Changes Needed)

The following test cases correctly use `model/` paths because they test **forecast jobs**, not **product generation jobs**:

- ✅ `C48_ATM-gfs_fcst_seg0.yaml` - Forecast outputs to `model/atmos/master/`
- ✅ `C48_S2SW-gfs_fcst_seg0.yaml` - Coupled forecast outputs to `model/atmos/`, `model/ocean/`, `model/ice/`
- ✅ `C48_S2SW-gefs_fcst_mem001.yaml` - Ensemble forecast outputs to `model/` directories

## Directory Structure Clarification

```
${RUN}.${YMD}/${HH}/
├── model/                          # Raw model output (FORECAST jobs)
│   ├── atmos/
│   │   ├── input/                  # Restart files
│   │   ├── history/                # History files (atmf*, sfcf*)
│   │   ├── master/                 # Master GRIB2 (master.grb2f*, sfluxgrbf*)
│   │   └── restart/
│   ├── ocean/
│   └── ice/
│
└── products/                       # Post-processed products (PRODUCTS jobs)
    ├── atmos/
    │   └── grib2/
    │       ├── 0p25/               # pgrb2, flux, idx files
    │       ├── 0p50/
    │       └── 1p00/
    ├── ocean/
    │   ├── grib2/
    │   └── netcdf/
    └── ice/
        ├── grib2/
        └── netcdf/
```

**Key Distinction:**
- **`model/` paths**: Used by forecast jobs (JGLOBAL_FORECAST, JGLOBAL_ATMENS_FORECAST, etc.)
  - Raw model output
  - History files for post-processing input
  - Master GRIB2 files (intermediate format)
  
- **`products/` paths**: Used by product generation jobs (JGLOBAL_ATMOS_PRODUCTS, JGLOBAL_OCEANICE_PRODUCTS)
  - Final GRIB2 products at multiple resolutions
  - netCDF subset products
  - Files ready for distribution/archive

## Workflow Job Pattern

```
FORECAST JOB → model/atmos/history/atmf*.nc
            → model/atmos/master/master.grb2f*
                          ↓
                     (input to)
                          ↓
PRODUCTS JOB ← model/atmos/master/master.grb2f*
            → products/atmos/grib2/{GRID}/pgrb2*
            → products/atmos/grib2/{GRID}/flux*
            → products/atmos/grib2/{GRID}/*.idx
```

## Impact Assessment

### Before Fix
- ❌ Validation failing with all product files marked as missing
- ❌ `C48_ATM-gfs_atmos_prod_f000-f002`: 18 files not found
- ❌ `C48_S2SW-gfs_oceanice_prod`: 15 files not found
- ✅ Test execution succeeded (files were created)
- ⚠️ Path mismatch prevented validation

### After Fix
- ✅ Paths now match actual COM template structure
- ✅ mkdir creates correct `products/` subdirectories
- ✅ Validation looks in correct `products/` locations
- ✅ All 5 test cases use correct path patterns
- ✅ Aligns with global-workflow standards

## Testing Verification

### Commands to Verify Fix
```bash
# On HERA, after running tests:

# Check atmospheric products (should now find 18 files)
ls /scratch3/.../gfs.20210323/12/products/atmos/grib2/0p25/
ls /scratch3/.../gfs.20210323/12/products/atmos/grib2/0p50/
ls /scratch3/.../gfs.20210323/12/products/atmos/grib2/1p00/

# Check ocean/ice products (should now find 15 files)
ls /scratch3/.../gfs.20210323/12/products/ocean/grib2/0p25/
ls /scratch3/.../gfs.20210323/12/products/ice/grib2/0p25/
ls /scratch3/.../gfs.20210323/12/products/ocean/netcdf/
ls /scratch3/.../gfs.20210323/12/products/ice/netcdf/
```

### Re-run Validation
```bash
cd /scratch3/NCEPDEV/global/Terry.McGuinness/global-workflow_forked/dev/ctests/build
ctest -R C48_ATM-gfs_atmos_prod_f000-f002_validate
ctest -R C48_S2SW-gfs_oceanice_prod_validate
```

Expected result: All file pairs present, validation passes.

## Analysis Methodology

1. **MCP Tools Used**: 
   - `mcp_global-workfl_search_documentation` - Searched for COM template patterns
   - Semantic search for GRIB2 directory structures
   
2. **Code Investigation**:
   - Examined `dev/parm/config/gfs/config.com` for COM template definitions
   - Reviewed `jobs/JGLOBAL_ATMOS_PRODUCTS` for directory construction
   - Analyzed existing test cases for correct patterns
   
3. **Pattern Recognition**:
   - Identified `products/` prefix requirement in all COM GRIB2 templates
   - Distinguished between `model/` (forecast output) and `products/` (processed output)
   - Verified ocean/ice use same `products/` pattern as atmosphere

## Lessons Learned

### Test Case Development Guidelines

1. **Always reference COM templates** when defining output paths:
   ```bash
   grep "COM_.*_GRIB.*TMPL" dev/parm/config/*/config.com
   ```

2. **Understand job types**:
   - **Forecast jobs** → `model/` paths
   - **Product generation jobs** → `products/` paths
   - **Analysis jobs** → `analysis/` paths

3. **Verify mkdir entries match output paths**:
   - Every directory in output_files must have corresponding mkdir
   - Path structure must match COM templates exactly

4. **Use existing test cases as templates**:
   - Forecast tests: Reference `C48_ATM-gfs_fcst_seg0.yaml`
   - Product tests: Reference fixed `C48_ATM-gfs_atmos_prod_f000-f002.yaml`

## Related Documentation

- **COM Templates**: `dev/parm/config/gfs/config.com` (lines 54-107)
- **Job Scripts**: `jobs/JGLOBAL_ATMOS_PRODUCTS`, `jobs/JGLOBAL_OCEANICE_PRODUCTS`
- **Execution Scripts**: `scripts/exglobal_atmos_products.sh`, `scripts/exglobal_oceanice_products.py`
- **Archive Templates**: `parm/archive/*.yaml.j2` (show product path usage)

## Change Log Reference

See `CTEST_UPDATES_CHANGELOG.md` - **Part 3: Fixed Product Output Directory Paths** for detailed change documentation.
