# HERA Path Verification for CTest Fixes

## Commands to Run on HERA

Please run these commands on HERA and paste the results back:

### 1. Check the top-level structure
```bash
ls -la /scratch3/NCEPDEV/global/role.glopara/GFS_CI_CD/HERA/BUILDS/GITLAB/stable/RUNTESTS/COMROOT/C48_ATM_388b1fe3-4737/gfs.20210323/12/
```

### 2. Check if products/ directory exists
```bash
ls -la /scratch3/NCEPDEV/global/role.glopara/GFS_CI_CD/HERA/BUILDS/GITLAB/stable/RUNTESTS/COMROOT/C48_ATM_388b1fe3-4737/gfs.20210323/12/ | grep -E "products|model"
```

### 3. Check atmospheric products location (our fix expects products/atmos/grib2/)
```bash
# Check if products/atmos/grib2 exists
ls -la /scratch3/NCEPDEV/global/role.glopara/GFS_CI_CD/HERA/BUILDS/GITLAB/stable/RUNTESTS/COMROOT/C48_ATM_388b1fe3-4737/gfs.20210323/12/products/atmos/grib2/ 2>&1

# Check if atmos/grib2 exists (old incorrect path)
ls -la /scratch3/NCEPDEV/global/role.glopara/GFS_CI_CD/HERA/BUILDS/GITLAB/stable/RUNTESTS/COMROOT/C48_ATM_388b1fe3-4737/gfs.20210323/12/atmos/grib2/ 2>&1
```

### 4. Check model/atmos/master location (this should exist for input files)
```bash
ls -la /scratch3/NCEPDEV/global/role.glopara/GFS_CI_CD/HERA/BUILDS/GITLAB/stable/RUNTESTS/COMROOT/C48_ATM_388b1fe3-4737/gfs.20210323/12/model/atmos/master/ | head -20
```

### 5. Check for any pgrb2 files to see where they actually are
```bash
find /scratch3/NCEPDEV/global/role.glopara/GFS_CI_CD/HERA/BUILDS/GITLAB/stable/RUNTESTS/COMROOT/C48_ATM_388b1fe3-4737/gfs.20210323/12/ -name "*pgrb2*" -type f 2>/dev/null | head -10
```

### 6. Check the full tree structure
```bash
tree -L 4 /scratch3/NCEPDEV/global/role.glopara/GFS_CI_CD/HERA/BUILDS/GITLAB/stable/RUNTESTS/COMROOT/C48_ATM_388b1fe3-4737/gfs.20210323/12/ 2>&1 | head -50
```

---

## Expected Results Based on Our Fixes

### If our fix is CORRECT:
- `products/atmos/grib2/0p25/` should exist and contain pgrb2 files
- `products/atmos/grib2/0p50/` should exist and contain pgrb2 files  
- `products/atmos/grib2/1p00/` should exist and contain pgrb2 files
- `model/atmos/master/` should exist and contain master.grb2f* files

### If our fix is WRONG:
- Files might be in `atmos/grib2/` (without products/ prefix)
- Or in some other location we need to discover

---

## Analysis Template

Once you provide the output, I'll analyze:
1. ✅ or ❌ Are files in `products/atmos/grib2/`?
2. ✅ or ❌ Do master files exist in `model/atmos/master/`?
3. 🔍 What is the actual directory structure?
4. 📝 Do we need to adjust our fix?
