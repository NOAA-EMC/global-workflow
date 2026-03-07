help([[
Load environment to run GFS on Derecho
]])

-- Test that HOMEglobal is set.
-- If not, load_modules.sh was not sourced to load this module.
local homegfssdir=os.getenv("HOMEglobal") or "None"
if (homegfssdir == "None") then
    LmodError("FATAL ERROR HOMEglobal variable is unset.\n" ..
              "Please \"source dev/ush/load_modules.sh\" rather than loading this module directly.\n")
end

setenv("LMOD_TMOD_FIND_FIRST","yes")

load("gw_run.common")
load(pathJoin("wgrib2", (os.getenv("wgrib2_ver") or "None")))

prepend_path("MODULEPATH", pathJoin("/glade/work/kolczynski/global_externals/prepobs/v" .. (os.getenv("prepobs_run_ver") or "None"), "modulefiles"))
load(pathJoin("prepobs", (os.getenv("prepobs_run_ver") or "None")))

prepend_path("MODULEPATH", pathJoin("/glade/work/kolczynski/global_externals/Fit2Obs/v" .. (os.getenv("fit2obs_ver") or "None"), "modulefiles"))
load(pathJoin("fit2obs", (os.getenv("fit2obs_ver") or "None")))

setenv("CRTM_FIX","/gpfs/csfs1/work/huangwei/GW-fix-data/crtm/v2.4.0.2")

whatis("Description: GFS run environment")
