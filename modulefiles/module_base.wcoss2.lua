help([[
Load environment to run GFS on WCOSS2
]])

load(pathJoin("PrgEnv-intel", (os.getenv("PrgEnv_intel_ver") or "None")))
load(pathJoin("craype", (os.getenv("craype_ver") or "None")))
load(pathJoin("intel", (os.getenv("intel_ver") or "None")))
load(pathJoin("cray-mpich", (os.getenv("cray_mpich_ver") or "None")))
load(pathJoin("cray-pals", (os.getenv("cray_pals_ver") or "None")))
load(pathJoin("cfp", (os.getenv("cfp_ver") or "None")))
setenv("USE_CFP","YES")

load(pathJoin("python", (os.getenv("python_ver") or "None")))
load(pathJoin("gempak", (os.getenv("gempak_ver") or "None")))
load(pathJoin("perl", (os.getenv("perl_ver") or "None")))
load(pathJoin("libjpeg", (os.getenv("libjpeg_ver") or "None")))
load(pathJoin("udunits", (os.getenv("udunits_ver") or "None")))
load(pathJoin("gsl", (os.getenv("gsl_ver") or "None")))
load(pathJoin("cdo", (os.getenv("cdo_ver") or "None")))
load(pathJoin("imagemagick", (os.getenv("imagemagick_ver") or "None")))

load(pathJoin("hdf5", (os.getenv("hdf5_ver") or "None")))
load(pathJoin("netcdf", (os.getenv("netcdf_ver") or "None")))

load(pathJoin("nco", (os.getenv("nco_ver") or "None")))
load(pathJoin("prod_util", (os.getenv("prod_util_ver") or "None")))
load(pathJoin("grib_util", (os.getenv("grib_util_ver") or "None")))
load(pathJoin("bufr_dump", (os.getenv("bufr_dump_ver") or "None")))
load(pathJoin("util_shared", (os.getenv("util_shared_ver") or "None")))
load(pathJoin("g2tmpl", (os.getenv("g2tmpl_ver") or "None")))
load(pathJoin("ncdiag", (os.getenv("ncdiag_ver") or "None")))
load(pathJoin("crtm", (os.getenv("crtm_ver") or "None")))
load(pathJoin("wgrib2", (os.getenv("wgrib2_ver") or "None")))

prepend_path("MODULEPATH", "/apps/ops/para/libs/modulefiles/compiler/intel/19.1.3.304")
setenv("HPC_OPT", "/apps/ops/para/libs")
load(pathJoin("met", (os.getenv("met_ver") or "None")))
load(pathJoin("metplus", (os.getenv("metplus_ver") or "None")))

prepend_path("MODULEPATH", pathJoin("/lfs/h2/emc/global/save/emc.global/git/prepobs/gfsv17_v" .. (os.getenv("prepobs_run_ver") or "None"), "modulefiles"))
load(pathJoin("prepobs", (os.getenv("prepobs_run_ver") or "None")))

prepend_path("MODULEPATH", pathJoin("/lfs/h2/emc/global/save/emc.global/git/Fit2Obs/v" .. (os.getenv("fit2obs_ver") or "None"), "modulefiles"))
load(pathJoin("fit2obs", (os.getenv("fit2obs_ver") or "None")))

append_path("MODULEPATH", pathJoin("/apps/ops/prod/nco/models/modulefiles"))
load(pathJoin("mos_shared", (os.getenv("mos_shared_ver") or "None")))

whatis("Description: GFS run environment")
