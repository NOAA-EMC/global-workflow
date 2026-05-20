help([[
Load environment to run GFS on AWS EC2
]])

-- Test that HOMEglobal is set.
-- If not, load_modules.sh was not sourced to load this module.
local homegfssdir=os.getenv("HOMEglobal") or "None"
if (homegfssdir == "None" and mode() == "load") then
    LmodError("FATAL ERROR HOMEglobal variable is unset.\n" ..
              "Please \"source dev/ush/load_modules.sh\" rather than loading this module directly.\n")
end

-- The spack-stack location on AWS is non-standard, so load them here
prepend_path("MODULEPATH", "/opt/modulefiles")
prepend_path("MODULEPATH", "/opt/spack-stack/envs/ue-oneapi-2024.2.1/install/modulefiles/Core")

load("gw_run.common")
load(pathJoin("wgrib2", (os.getenv("wgrib2_ver") or "None")))

setenv("UTILROOT",(os.getenv("prod_util_ROOT") or "None"))
setenv("I_MPI_PMI_LIBRARY", "/opt/slurm/lib/libpmi2.so")
setenv("I_MPI_HYDRA_BOOTSTRAP", "slurm")
setenv("FI_PROVIDER", "tcp")
setenv("I_MPI_OFI_PROVIDER", "tcp")

prepend_path("MODULEPATH", pathJoin("/lustre", "global", "external_programs", "prepobs", "v" .. (os.getenv("prepobs_run_ver") or "None"), "modulefiles"))
load(pathJoin("prepobs", (os.getenv("prepobs_run_ver") or "None")))

prepend_path("MODULEPATH", pathJoin("/lustre", "global", "external_programs", "Fit2Obs", "v" .. (os.getenv("fit2obs_ver") or "None"), "modulefiles"))
load(pathJoin("fit2obs", (os.getenv("fit2obs_ver") or "None")))

setenv("CRTM_FIX", "/lustre/global/data/fix/crtm/v2.4.0.2")

prepend_path("MODULEPATH", "/opt/amazon/modules/modulefiles")
load(pathJoin("libfabric-aws","2.1.0amzn2.0"))
load(pathJoin("openmpi","4.1.7"))

whatis("Description: GFS run environment")
