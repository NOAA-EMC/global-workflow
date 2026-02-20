help([[
Load environment to run GFS on Derecho
]])

setenv("LMOD_TMOD_FIND_FIRST","yes")
prepend_path("MODULEPATH", "/lustre/desc1/scratch/epicufsrt/contrib/modulefiles_extra")
prepend_path("MODULEPATH", "/glade/work/epicufsrt/contrib/spack-stack/derecho/spack-stack-1.9.2/envs/ue-oneapi-2024.2.1/install/modulefiles/Core")

load("gw_run.common")
load(pathJoin("wgrib2", (os.getenv("wgrib2_ver") or "None")))

-- prepend_path("MODULEPATH", pathJoin("/scratch3/NCEPDEV/global/role.glopara/git/prepobs/v" .. (os.getenv("prepobs_run_ver") or "None"), "modulefiles"))
-- load(pathJoin("prepobs", (os.getenv("prepobs_run_ver") or "None")))

-- prepend_path("MODULEPATH", pathJoin("/scratch3/NCEPDEV/global/role.glopara/git/Fit2Obs/v" .. (os.getenv("fit2obs_ver") or "None"), "modulefiles"))
-- load(pathJoin("fit2obs", (os.getenv("fit2obs_ver") or "None")))

whatis("Description: GFS run environment")

-- load(pathJoin("imagemagick", (os.getenv("imagemagick_ver") or "None")))
