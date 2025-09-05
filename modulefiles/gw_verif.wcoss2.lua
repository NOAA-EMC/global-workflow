help([[
Load environment to run verification on WCOSS2
]])

load(pathJoin("PrgEnv-intel", (os.getenv("PrgEnv_intel_ver") or "None")))
load(pathJoin("craype", (os.getenv("craype_ver") or "None")))
load(pathJoin("intel", (os.getenv("intel_ver") or "None")))
load(pathJoin("cray-mpich", (os.getenv("cray_mpich_ver") or "None")))
load(pathJoin("cray-pals", (os.getenv("cray_pals_ver") or "None")))
load(pathJoin("cfp", (os.getenv("cfp_ver") or "None")))
setenv("USE_CFP","YES")

-- Use Python 3.8.6 for verification
load(pathJoin("python", 3.8.6)

load(pathJoin("hdf5-D", (os.getenv("hdf5_ver") or "None")))
load(pathJoin("pnetcdf-D", (os.getenv("pnetcdf_ver") or "None")))
load(pathJoin("netcdf-D", (os.getenv("netcdf_ver") or "None")))

-- The cray library path for C MPI libraries (needed by C-only programs using netCDF)
local cray_lib_path=os.getenv("CRAY_LD_LIBRARY_PATH") or ""
prepend_path("LD_LIBRARY_PATH", cray_lib_path)

load(pathJoin("prod_util", (os.getenv("prod_util_ver") or "None")))
load(pathJoin("grib_util", (os.getenv("grib_util_ver") or "None")))
load(pathJoin("wgrib2", (os.getenv("wgrib2_ver") or "None")))

prepend_path("MODULEPATH", "/apps/ops/para/libs/modulefiles/compiler/intel/19.1.3.304")
setenv("HPC_OPT", "/apps/ops/para/libs")
load(pathJoin("met", (os.getenv("met_ver") or "None")))
load(pathJoin("metplus", (os.getenv("metplus_ver") or "None")))

whatis("Description: Verification environment")
