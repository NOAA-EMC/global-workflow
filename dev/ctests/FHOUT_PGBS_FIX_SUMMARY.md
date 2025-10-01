# CTest Fix: FHOUT_PGBS Product Generation Behavior

## Issue Discovery
When examining test output on disk, we found:
- `0p25/` directory: Had f000, f001, f002 files ✅
- `1p00/` directory: Had ONLY f000 files ❌

But the YAML test was expecting f000 and f003 for ALL grids.

## Root Cause: FHOUT_PGBS Configuration

### Key Configuration
From `dev/parm/config/gfs/config.atmos_products`:
```bash
export FHOUT_PGBS=${FHOUT_GFS:-3}  # Supplemental products every 3 hours
```

### Product Generation Logic
From `scripts/exglobal_atmos_products.sh` (lines 18-50):

```bash
# For f000, PGBS is always YES
if [[ ${FORECAST_HOUR} -le 0 ]]; then
  PGBS="YES"
# For other hours, only if divisible by FHOUT_PGBS
else
  if (( FORECAST_HOUR%FHOUT_PGBS == 0 )); then
    PGBS="YES"
  fi
fi

# Determine which grids to generate
grid_string="0p25"  # Always generate 0p25
if [[ "${PGBS:-}" == "YES" ]]; then
  grid_string="${grid_string}:0p50:1p00"  # Add supplemental grids
fi
```

**Result:**
- **0p25 grid**: Generated for ALL forecast hours (0, 1, 2, 3, 4, ...)
- **0p50 and 1p00 grids**: Generated ONLY at FHOUT_PGBS intervals (0, 3, 6, 9, ...)

### Flux Files Exception
From `scripts/exglobal_atmos_products.sh` (lines 176-203):
```bash
FLUX_FILE="${COMIN_ATMOS_MASTER}/${PREFIX}sfluxgrb${fhr3}.grib2"
if [[ -s "${FLUX_FILE}" ]]; then
  # Process flux file at 1p00 grid for ALL forecast hours
fi
```

**Result:** Flux files are generated at 1p00 grid for ALL forecast hours, regardless of FHOUT_PGBS.

## Workflow Context: FHR_LIST Variable

From Rocoto XML metatask:
```xml
<var name="fhr_list">0,1,2 3,4,5 6,7,8 9,10,11 ...</var>
<envar><name>FHR_LIST</name><value>#fhr_list#</value></envar>
```

**Critical Understanding:**
- The task `gfs_atmos_prod_f000-f002` processes **THREE forecast hours** (0, 1, 2) in a single job
- The script loops through FHR_LIST and processes each hour individually
- Each hour follows the FHOUT_PGBS logic independently

## Expected Output by Grid and Forecast Hour

| Forecast Hour | 0p25 pgrb2 | 0p50 pgrb2 | 1p00 pgrb2 | 1p00 flux | Reason |
|---------------|------------|------------|------------|-----------|---------|
| f000 | ✅ | ✅ | ✅ | ✅ | f000 always gets PGBS="YES" |
| f001 | ✅ | ❌ | ❌ | ✅ | 1 % 3 ≠ 0, no supplemental pgrb2 |
| f002 | ✅ | ❌ | ❌ | ✅ | 2 % 3 ≠ 0, no supplemental pgrb2 |
| f003 | ✅ | ✅ | ✅ | ✅ | 3 % 3 = 0, PGBS="YES" |

## Test Case Fix Summary

### C48_ATM-gfs_atmos_prod_f000-f002.yaml

**Removed (incorrect expectations):**
- `products/atmos/grib2/0p25/gfs.t12z.pgrb2.0p25.f003` and `.idx`
- `products/atmos/grib2/0p50/gfs.t12z.pgrb2.0p50.f003` and `.idx`
- `products/atmos/grib2/1p00/gfs.t12z.pgrb2.1p00.f003` and `.idx`
- `products/atmos/grib2/0p25/gfs.t12z.flux.0p25.f000` and `f003`
- `products/atmos/grib2/0p50/gfs.t12z.flux.0p50.f000` and `f003`

**Added (correct expectations):**
- `products/atmos/grib2/0p25/gfs.t12z.pgrb2.0p25.f001` and `.idx`
- `products/atmos/grib2/0p25/gfs.t12z.pgrb2.0p25.f002` and `.idx`
- `products/atmos/grib2/1p00/gfs.t12z.flux.1p00.f000/f001/f002` and `.idx` (6 files)

**Total expected output files: 14**
- 0p25 grid: 6 files (pgrb2 + idx for f000, f001, f002)
- 0p50 grid: 2 files (pgrb2 + idx for f000 only)
- 1p00 grid: 6 files (pgrb2 + idx for f000, flux + idx for f000/f001/f002)

## Verification Against Nightly Run

From source directory:
```bash
$ ls /scratch3/NCEPDEV/global/role.glopara/.../gfs.20210323/12/products/atmos/grib2/1p00/ | grep -E '\.f00'

# pgrb2 files (only at FHOUT_PGBS intervals)
gfs.t12z.pgrb2.1p00.f000      ✅
gfs.t12z.pgrb2.1p00.f003      ✅
gfs.t12z.pgrb2.1p00.f006      ✅
(no f001, f002, f004, f005...)

# flux files (ALL forecast hours)
gfs.t12z.flux.1p00.f000       ✅
gfs.t12z.flux.1p00.f001       ✅
gfs.t12z.flux.1p00.f002       ✅
gfs.t12z.flux.1p00.f003       ✅
gfs.t12z.flux.1p00.f004       ✅
...
```

**Confirms:** pgrb2 at intervals, flux for all hours.

## Operational Rationale

This behavior optimizes computational resources:
1. **High-resolution 0p25 products**: Generated every hour for immediate operational use
2. **Lower-resolution supplemental products**: Generated every 3 hours (sufficient for most applications)
3. **Surface flux products**: Generated every hour at 1p00 resolution (needed for various applications)

## Lessons Learned

1. **Test names can be misleading**: "f000-f002" suggests a range, but actually means "f000, f001, AND f002"
2. **FHR_LIST is critical**: Tasks process multiple forecast hours per job execution
3. **Configuration drives behavior**: FHOUT_PGBS determines supplemental product frequency
4. **Product type matters**: Different products (pgrb2 vs flux) have different generation rules
5. **Always verify against actual output**: Check nightly run directories to confirm expectations

## Related Files

- Test case: `dev/ctests/cases/C48_ATM-gfs_atmos_prod_f000-f002.yaml`
- Script: `scripts/exglobal_atmos_products.sh`
- Configuration: `dev/parm/config/gfs/config.atmos_products`
- Workflow XML: Shows FHR_LIST variable definition
- Changelog: `CTEST_UPDATES_CHANGELOG.md` (Part 4)
