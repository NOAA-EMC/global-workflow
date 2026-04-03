help([[
Load environment to run GFS on Derecho
]])

-- Test that HOMEglobal is set.
-- If not, load_modules.sh was not sourced to load this module.
local homegfssdir=os.getenv("HOMEglobal") or "None"
if (homegfssdir == "None" and mode() == "load") then
    LmodError("FATAL ERROR HOMEglobal variable is unset.\n" ..
              "Please \"source dev/ush/load_modules.sh\" rather than loading this module directly.\n")
end

setenv("LMOD_TMOD_FIND_FIRST","yes")

load("gw_run.common")
load(pathJoin("wgrib2", (os.getenv("wgrib2_ver") or "None")))

prepend_path("MODULEPATH", pathJoin("/lustre/desc1/p/nral0032/global/global_externals/prepobs/v" .. (os.getenv("prepobs_run_ver") or "None"), "modulefiles"))
load(pathJoin("prepobs", (os.getenv("prepobs_run_ver") or "None")))

prepend_path("MODULEPATH", pathJoin("/lustre/desc1/p/nral0032/global/global_externals/Fit2Obs/v" .. (os.getenv("fit2obs_ver") or "None"), "modulefiles"))
load(pathJoin("fit2obs", (os.getenv("fit2obs_ver") or "None")))

-- Derecho's installation or perl doesn't have a needed module
-- So, it is installed separately and added to the path
-- Can likely be removed after vminmon is retired
prepend_path("PERL5LIB", "/lustre/desc1/p/nral0032/global/perl_modules/lib/perl5")

setenv("CRTM_FIX", "/lustre/desc1/p/nral0032/global/data/fix/crtm/v2.4.0.2")

whatis("Description: GFS run environment")
