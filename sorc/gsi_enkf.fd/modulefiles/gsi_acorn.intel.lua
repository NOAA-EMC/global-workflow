help([[
]])

local PrgEnv_intel_ver=os.getenv("PrgEnv_intel_ver") or "8.1.0"
local intel_ver=os.getenv("intel_ver") or "19.1.3.304"
local craype_ver=os.getenv("craype_ver") or "2.7.8"
local cray_mpich_ver=os.getenv("cray_mpich_ver") or "8.1.7"
local cmake_ver= os.getenv("cmake_ver") or "3.20.2"
local python_ver=os.getenv("python_ver") or "3.8.6"
local prod_util_ver=os.getenv("prod_util_ver") or "2.0.10"

local netcdf_ver=os.getenv("netcdf_ver") or "4.7.4"
local bufr_ver=os.getenv("bufr_ver") or "11.7.0"
local bacio_ver=os.getenv("bacio_ver") or "2.4.1"
local w3emc_ver=os.getenv("w3emc_ver") or "2.9.2"
local sp_ver=os.getenv("sp_ver") or "2.3.3"
local ip_ver=os.getenv("ip_ver") or "3.3.3"
local sigio_ver=os.getenv("sigio_ver") or "2.3.2"
local sfcio_ver=os.getenv("sfcio_ver") or "1.4.1"
local nemsio_ver=os.getenv("nemsio_ver") or "2.5.4"
local wrf_io_ver=os.getenv("wrf_io_ver") or "1.2.0"
local ncio_ver=os.getenv("ncio_ver") or "1.1.2"
local crtm_ver=os.getenv("crtm_ver") or "2.4.0.1"
local ncdiag_ver=os.getenv("ncdiag_ver") or "1.1.1"

load(pathJoin("PrgEnv-intel", PrgEnv_intel_ver))
load(pathJoin("intel", intel_ver))
load(pathJoin("craype", craype_ver))
load(pathJoin("cray-mpich", cray_mpich_ver))
load(pathJoin("cmake", cmake_ver))
load(pathJoin("python", python_ver))

load(pathJoin("prod_util", prod_util_ver))

load(pathJoin("netcdf", netcdf_ver))
load(pathJoin("bufr", bufr_ver))
load(pathJoin("bacio", bacio_ver))
load(pathJoin("w3emc", w3emc_ver))
load(pathJoin("sp", sp_ver))
load(pathJoin("ip", ip_ver))
load(pathJoin("sigio", sigio_ver))
load(pathJoin("sfcio", sfcio_ver))
load(pathJoin("nemsio", nemsio_ver))
load(pathJoin("wrf_io", wrf_io_ver))
load(pathJoin("ncio", ncio_ver))
load(pathJoin("ncdiag",ncdiag_ver))

-- Lastly, load CRTM from the EMC location
append_path("MODULEPATH", "/lfs/h1/emc/nceplibs/noscrub/hpc-stack/libs/hpc-stack/modulefiles/compiler/intel/19.1.3.304")
load(pathJoin("crtm", crtm_ver))

pushenv("GSI_BINARY_SOURCE_DIR", "/lfs/h2/emc/global/noscrub/emc.global/FIX/fix/gsi/20241022")

whatis("Description: GSI environment on WCOSS2 Acorn")
