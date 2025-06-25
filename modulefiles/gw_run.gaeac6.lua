help([[
Load environment to run GFS on Gaea C6
]])

-- Test that HOMEgfs is set.
-- If not, load_gw_run_modules.sh was not sourced to load this module.
local homegfssdir=os.getenv("HOMEgfs") or "None"
if (homegfssdir == "None") then
    LmodError("FATAL ERROR HOMEgfs variable is unset.\n" ..
              "Please \"source ush/load_gw_run_modules.sh\" rather than loading this module directly.\n")
end

load("gw_run.common")

load(pathJoin("perlbrew", (os.getenv("perl_ver") or "None")))

prepend_path("MODULEPATH", pathJoin("/gpfs/f6/drsa-precip3/world-shared/role.glopara/git/prepobs/v" .. (os.getenv("prepobs_run_ver") or "None"), "modulefiles"))
load(pathJoin("prepobs", (os.getenv("prepobs_run_ver") or "None")))

prepend_path("MODULEPATH", pathJoin("/gpfs/f6/drsa-precip3/world-shared/role.glopara/git/Fit2Obs/v" .. (os.getenv("fit2obs_ver") or "None"), "modulefiles"))
load(pathJoin("fit2obs", (os.getenv("fit2obs_ver") or "None")))

local hsi_mod_path=(os.getenv("hsi_mod_path") or "None")
append_path("MODULEPATH", hsi_mod_path)
load(pathJoin("hsi", (os.getenv("hsi_ver") or "None")))

-- Point to the wgrib2 executable with ipolates
setenv("WGRIB2", "/autofs/ncrc-svm1_proj/epic/c6/spack-stack/spack-stack-1.9.1/envs/gw-intel-2023.2.0/install/intel/2023.2.0/wgrib2-3.6.0-fjguk3g/bin/wgrib2")
prepend_path("PATH", "/autofs/ncrc-svm1_proj/epic/c6/spack-stack/spack-stack-1.9.1/envs/gw-intel-2023.2.0/install/intel/2023.2.0/wgrib2-3.6.0-fjguk3g/bin")

unload("cray-libsci")

whatis("Description: GFS run setup environment")
