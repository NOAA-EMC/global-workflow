help([[
Load environment to run GFS on AWS EC2
]])

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

prepend_path("MODULEPATH", pathJoin("/lustre/global_externals/prepobs/v" .. (os.getenv("prepobs_run_ver") or "None"), "modulefiles"))
load(pathJoin("prepobs", (os.getenv("prepobs_run_ver") or "None")))

prepend_path("MODULEPATH", pathJoin("/lustre/global_externals/Fit2Obs/v" .. (os.getenv("fit2obs_ver") or "None"), "modulefiles"))
load(pathJoin("fit2obs", (os.getenv("fit2obs_ver") or "None")))

-- load(pathJoin("imagemagick", (os.getenv("imagemagick_ver") or "None")))

setenv("CRTM_FIX", "/lustre/sharedGWdata/fix/crtm/v2.4.0.2")

prepend_path("LD_LIBRARY_PATH", "/opt/amazon/efa/lib")
prepend_path("LD_LIBRARY_PATH", "/opt/amazon/openmpi/lib")

whatis("Description: GFS run environment")
