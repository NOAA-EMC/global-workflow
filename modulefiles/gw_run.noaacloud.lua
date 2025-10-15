help([[
Load environment to run GFS on NOAA cloud
]])

-- Test that HOMEgfs is set.
-- If not, load_modules.sh was not sourced to load this module.
local homegfssdir=os.getenv("HOMEgfs") or "None"
if (homegfssdir == "None") then
    LmodError("FATAL ERROR HOMEgfs variable is unset.\n" ..
              "Please \"source dev/ush/load_modules.sh\" rather than loading this module directly.\n")
end

load(pathJoin("perl", (os.getenv("perl_ver") or "None")))
load(pathJoin("mkl", (os.getenv("mkl_ver") or "None")))

load("gw_run.common")
load(pathJoin("wgrib2", (os.getenv("wgrib2_ver") or "None"))) -- TODO: Remove once wgrib2 is loaded in gw_run.common

prepend_path("MODULEPATH", pathJoin("/contrib/git/prepobs/v" .. (os.getenv("prepobs_run_ver") or "None"), "modulefiles"))
load(pathJoin("prepobs", (os.getenv("prepobs_run_ver") or "None")))

prepend_path("MODULEPATH", pathJoin("/contrib/git/Fit2Obs/v" .. (os.getenv("fit2obs_ver") or "None"), "modulefiles"))
load(pathJoin("fit2obs", (os.getenv("fit2obs_ver") or "None")))

load(pathJoin("imagemagick", (os.getenv("imagemagick_ver") or "None")))

setenv("CRTM_FIX", "/contrib/global-workflow-shared-data/fix/crtm/v2.4.0.2")

whatis("Description: GFS run environment")
