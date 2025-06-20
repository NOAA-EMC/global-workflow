help([[
Load environment to run GFS on Hera
]])

-- Test that HOMEgfs is set.
-- If not, load_gw_run_modules.sh was not sourced to load this module.
local homegfssdir=os.getenv("HOMEgfs") or "None"
if (homegfssdir == "None") then
    LmodError("FATAL ERROR HOMEgfs variable is unset.\n" ..
              "Please \"source ush/load_gw_run_modules.sh\" rather than loading this module directly.\n")
end

load("gw_run.common")

load(pathJoin("hpss", (os.getenv("hpss_ver") or "None")))
load(pathJoin("gempak", (os.getenv("gempak_ver") or "None")))
load(pathJoin("perl", (os.getenv("perl_ver") or "None")))

prepend_path("MODULEPATH", pathJoin("/scratch1/NCEPDEV/global/glopara/git/prepobs/v" .. (os.getenv("prepobs_run_ver") or "None"), "modulefiles"))
load(pathJoin("prepobs", (os.getenv("prepobs_run_ver") or "None")))

prepend_path("MODULEPATH", pathJoin("/scratch1/NCEPDEV/global/glopara/git/Fit2Obs/v" .. (os.getenv("fit2obs_ver") or "None"), "modulefiles"))
load(pathJoin("fit2obs", (os.getenv("fit2obs_ver") or "None")))

whatis("Description: GFS run environment")

load(pathJoin("imagemagick", (os.getenv("imagemagick_ver") or "None")))
