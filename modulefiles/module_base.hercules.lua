help([[
Load environment to run GFS on Hercules
]])

spack_stack_ver=(os.getenv("spack_stack_ver") or "None")
spack_env=(os.getenv("spack_env") or "None")
prepend_path("MODULEPATH", "/work/noaa/epic/role-epic/spack-stack/hercules/spack-stack-" .. spack_stack_ver .. "/envs/" .. spack_env .. "/install/modulefiles/Core")

load(pathJoin("stack-intel", (os.getenv("stack_intel_ver") or "None")))
load(pathJoin("stack-intel-oneapi-mpi", (os.getenv("stack_impi_ver") or "None")))
load(pathJoin("intel-oneapi-mkl", (os.getenv("intel_mkl_ver") or "None")))
load(pathJoin("python", (os.getenv("python_ver") or "None")))
load(pathJoin("perl", (os.getenv("perl_ver") or "None")))

-- TODO load NCL once the SAs remove the 'depends_on' statements within it
--      NCL is a static installation and does not depend on any libraries
--      but as is will load, among others, the system netcdf-c/4.9.0 module
--load(pathJoin("ncl", (os.getenv("ncl_ver") or "None")))
load(pathJoin("jasper", (os.getenv("jasper_ver") or "None")))
load(pathJoin("libpng", (os.getenv("libpng_ver") or "None")))
load(pathJoin("cdo", (os.getenv("cdo_ver") or "None")))

load(pathJoin("hdf5", (os.getenv("hdf5_ver") or "None")))
load(pathJoin("netcdf-c", (os.getenv("netcdf_c_ver") or "None")))
load(pathJoin("netcdf-fortran", (os.getenv("netcdf_fortran_ver") or "None")))

load(pathJoin("nco", (os.getenv("nco_ver") or "None")))
load(pathJoin("prod_util", (os.getenv("prod_util_ver") or "None")))
load(pathJoin("grib-util", (os.getenv("grib_util_ver") or "None")))
load(pathJoin("g2tmpl", (os.getenv("g2tmpl_ver") or "None")))
load(pathJoin("gsi-ncdiag", (os.getenv("gsi_ncdiag_ver") or "None")))
load(pathJoin("crtm", (os.getenv("crtm_ver") or "None")))
load(pathJoin("bufr", (os.getenv("bufr_ver") or "None")))
load(pathJoin("wgrib2", (os.getenv("wgrib2_ver") or "None")))
load(pathJoin("py-netcdf4", (os.getenv("py_netcdf4_ver") or "None")))
load(pathJoin("py-pyyaml", (os.getenv("py_pyyaml_ver") or "None")))
load(pathJoin("py-jinja2", (os.getenv("py_jinja2_ver") or "None")))

setenv("WGRIB2","wgrib2")
setenv("UTILROOT",(os.getenv("prod_util_ROOT") or "None"))

prepend_path("MODULEPATH", pathJoin("/work/noaa/global/glopara/git/prepobs/feature-GFSv17_com_reorg_log_update/modulefiles"))
load(pathJoin("prepobs", (os.getenv("prepobs_run_ver") or "None")))

prepend_path("MODULEPATH", pathJoin("/work/noaa/global/glopara/git/Fit2Obs/v" .. (os.getenv("fit2obs_ver") or "None"), "modulefiles"))
load(pathJoin("fit2obs", (os.getenv("fit2obs_ver") or "None")))

whatis("Description: GFS run environment")
