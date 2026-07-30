help([[
Load environment to run the UPP on WCOSS2
]])

local homeglobal=os.getenv("HOMEglobal") or ""
prepend_path("MODULEPATH", pathJoin(homeglobal,"/modulefiles/upp"))
-- Load UPP modules
load("wcoss2_intel")

load(pathJoin("cray-pals", os.getenv("cray_pals_ver")))

-- Load workflow modules
-- Do not load prod_util when running ecflow
local is_ecf = os.getenv("ECF_JOB") ~= nil
if not is_ecf then
    load(pathJoin("prod_util", "2.0.9"))
end
load(pathJoin("python", os.getenv("python_ver")))
load(pathJoin("libjpeg", os.getenv("libjpeg_ver")))
load(pathJoin("wgrib2", os.getenv("wgrib2_ver")))
load(pathJoin("grib_util",os.getenv("grib_util_ver")))
setenv("WGRIB2","wgrib2")

-- Load the GFS Python environment
load(pathJoin("ve","gfs", os.getenv("ve_gfs_ver")))

whatis("Description: UPP run environment")
