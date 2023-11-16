help([[
Load environment to run GFS on Hera
]])

spack_stack_ver=(os.getenv("spack_stack_ver") or "None")
spack_env=(os.getenv("spack_env") or "None")
prepend_path("MODULEPATH", "/scratch1/NCEPDEV/nems/role.epic/spack-stack/spack-stack-" .. spack_stack_ver .. "/envs/" .. spack_env .. "/install/modulefiles/Core")

load(pathJoin("stack-intel", os.getenv("stack_intel_ver")))
load(pathJoin("stack-intel-oneapi-mpi", os.getenv("stack_impi_ver")))
load(pathJoin("python", os.getenv("python_ver")))

load(pathJoin("hpss", os.getenv("hpss_ver")))
load(pathJoin("gempak", os.getenv("gempak_ver")))
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
setenv("UTILROOT",os.getenv("prod_util_ROOT"))

--prepend_path("MODULEPATH", pathJoin("/scratch1/NCEPDEV/global/glopara/git/prepobs/v" .. os.getenv("prepobs_run_ver"), "modulefiles"))
prepend_path("MODULEPATH", pathJoin("/scratch1/NCEPDEV/global/glopara/git/prepobs/feature-GFSv17_com_reorg_log_update/modulefiles"))
load(pathJoin("prepobs", os.getenv("prepobs_run_ver")))

prepend_path("MODULEPATH", pathJoin("/scratch1/NCEPDEV/global/glopara/git/Fit2Obs/v" .. (os.getenv("fit2obs_ver") or "None"), "modulefiles"))
load(pathJoin("fit2obs", os.getenv("fit2obs_ver")))

whatis("Description: GFS run environment")
