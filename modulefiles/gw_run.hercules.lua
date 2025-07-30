help([[
Load environment to run GFS on Hercules
]])

-- Test that HOMEgfs is set.
-- If not, load_gw_run_modules.sh was not sourced to load this module.
local homegfssdir=os.getenv("HOMEgfs") or "None"
if (homegfssdir == "None") then
    LmodError("FATAL ERROR HOMEgfs variable is unset.\n" ..
              "Please \"source ush/load_gw_run_modules.sh\" rather than loading this module directly.\n")
end

load("gw_run.common")
load(pathJoin("intel-oneapi-mkl", (os.getenv("intel_mkl_ver") or "None")))
load(pathJoin("wgrib2", (os.getenv("wgrib2_ver") or "None")))

-- Set the path for the Sven executables
append_path("PATH", pathJoin((os.getenv("sven_root_path") or "None"), "bin"))
-- Load globus-cli for Globus-HPSS archiving
load("globus-cli")

prepend_path("MODULEPATH", pathJoin("/work2/noaa/global/role-global/git/prepobs/v" .. (os.getenv("prepobs_run_ver") or "None"), "modulefiles"))
load(pathJoin("prepobs", (os.getenv("prepobs_run_ver") or "None")))

prepend_path("MODULEPATH", pathJoin("/work2/noaa/global/role-global/git/Fit2Obs/v" .. (os.getenv("fit2obs_ver") or "None"), "modulefiles"))
load(pathJoin("fit2obs", (os.getenv("fit2obs_ver") or "None")))

whatis("Description: GFS run environment")
