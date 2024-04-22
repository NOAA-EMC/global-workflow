help([[
Load environment to run GFS workflow setup scripts on Gaea C5 
]])

load(pathJoin("rocoto"))

prepend_path("MODULEPATH", "/ncrc/proj/epic/spack-stack/spack-stack-1.6.0/envs/unified-env/install/modulefiles/Core")
prepend_path("MODULEPATH", "/ncrc/proj/epic/spack-stack/modulefiles")
prepend_path("MODULEPATH", "/ncrc/proj/epic/rocoto/modulefiles")
load(pathJoin("rocoto","1.3.6"))

local stack_intel_ver=os.getenv("stack_intel_ver") or "2023.1.0"
local python_ver=os.getenv("python_ver") or "3.9.12"

load(pathJoin("stack-intel", stack_intel_ver))
load("py-jinja2")
load("py-pyyaml")
load("py-numpy")

-- load(pathJoin("hpss", os.getenv("hpss_ver")))
-- load(pathJoin("gempak", os.getenv("gempak_ver")))
load(pathJoin("ncl", os.getenv("ncl_ver")))
load(pathJoin("jasper", os.getenv("jasper_ver")))
load(pathJoin("libpng", os.getenv("libpng_ver")))
load(pathJoin("cdo", os.getenv("cdo_ver")))
load(pathJoin("R", os.getenv("R_ver")))
load(pathJoin("hdf5", os.getenv("hdf5_ver")))
load(pathJoin("netcdf-c", os.getenv("netcdf_c_ver")))
load(pathJoin("netcdf-fortran", os.getenv("netcdf_fortran_ver")))
load(pathJoin("nco", os.getenv("nco_ver")))
load(pathJoin("prod_util", os.getenv("prod_util_ver")))
load(pathJoin("grib-util", os.getenv("grib_util_ver")))
load(pathJoin("g2tmpl", os.getenv("g2tmpl_ver")))
load(pathJoin("gsi-ncdiag", os.getenv("gsi_ncdiag_ver")))
load(pathJoin("crtm", os.getenv("crtm_ver")))
load(pathJoin("bufr", os.getenv("bufr_ver")))
load(pathJoin("wgrib2", os.getenv("wgrib2_ver")))
load(pathJoin("py-netcdf4", os.getenv("py_netcdf4_ver")))
load(pathJoin("py-pyyaml", os.getenv("py_pyyaml_ver")))
load(pathJoin("py-jinja2", os.getenv("py_jinja2_ver")))
load(pathJoin("met", os.getenv("met_ver")))
load(pathJoin("metplus", os.getenv("metplus_ver")))
setenv("WGRIB2","wgrib2")
setenv("UTILROOT",(os.getenv("prod_util_ROOT") or "None"))
--prepend_path("MODULEPATH", pathJoin("/gpfs/f5/epic/proj-shared/global/glopara/data/git/prepobs/v" .. (os.getenv("prepobs_run_ver") or "None"), "modulefiles"))
prepend_path("MODULEPATH", pathJoin("/gpfs/f5/epic/proj-shared/global/glopara/data/git/prepobs/feature-GFSv17_com_reorg_log_update/modulefiles"))
load(pathJoin("prepobs", os.getenv("prepobs_run_ver")))
prepend_path("MODULEPATH", pathJoin("/gpfs/f5/epic/proj-shared/global/glopara/data/git/Fit2Obs/v" .. (os.getenv("fit2obs_ver") or "None"), "modulefiles"))
load(pathJoin("fit2obs", os.getenv("fit2obs_ver")))

whatis("Description: GFS run setup environment")
