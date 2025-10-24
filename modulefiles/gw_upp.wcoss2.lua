help([[
Load environment to run the UPP on WCOSS2
]])

local homegfs=os.getenv("HOMEgfs") or ""
prepend_path("MODULEPATH", pathJoin(homegfs,"/sorc/ufs_model.fd/UFSATM/upp/modulefiles"))
-- Load UPP modules
load("wcoss2_intel")

load(pathJoin("cray-pals", "1.0.17"))

-- Load workflow modules
load(pathJoin("prod_util", "2.0.9"))
load(pathJoin("python", "3.12.0"))
load(pathJoin("libjpeg", "9c"))
load(pathJoin("wgrib2", "2.0.8"))
load(pathJoin("grib_util","1.2.3"))
setenv("WGRIB2","wgrib2")

-- Load the GW Python environment
prepend_path("MODULEPATH", "/apps/dev/modulefiles")
load(pathJoin("ve","gw", "1.0"))

whatis("Description: UPP run environment")
