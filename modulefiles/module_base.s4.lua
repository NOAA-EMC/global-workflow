help([[
Load environment to run GFS on S4
]])

local spack_mod_path=(os.getenv("spack_mod_path") or "None")
prepend_path("MODULEPATH", spack_mod_path)

load(pathJoin("stack-intel", (os.getenv("stack_intel_ver") or "None")))
load(pathJoin("stack-intel-oneapi-mpi", (os.getenv("stack_impi_ver") or "None")))
load(pathJoin("python", (os.getenv("python_ver") or "None")))

load(pathJoin("ncl", (os.getenv("ncl_ver") or "None")))
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
load(pathJoin("py-f90nml", (os.getenv("py_f90nml_ver") or "None")))
load(pathJoin("py-netcdf4", (os.getenv("py_netcdf4_ver") or "None")))
load(pathJoin("py-pyyaml", (os.getenv("py_pyyaml_ver") or "None")))
load(pathJoin("py-jinja2", (os.getenv("py_jinja2_ver") or "None")))
load(pathJoin("py-pandas", (os.getenv("py_pandas_ver") or "None")))
load(pathJoin("py-python-dateutil", (os.getenv("py_python_dateutil_ver") or "None")))
load(pathJoin("met", (os.getenv("met_ver") or "None")))
load(pathJoin("metplus", (os.getenv("metplus_ver") or "None")))
load(pathJoin("py-xarray", (os.getenv("py_xarray_ver") or "None")))

setenv("WGRIB2","wgrib2")

-- Stop gap fix for wgrib with spack-stack 1.6.0
-- TODO Remove this when spack-stack issue #1097 is resolved
setenv("WGRIB","wgrib")
setenv("UTILROOT",(os.getenv("prod_util_ROOT") or "None"))

--prepend_path("MODULEPATH", pathJoin("/data/prod/glopara/git/prepobs/v" .. (os.getenv("prepobs_run_ver") or "None"), "modulefiles"))
prepend_path("MODULEPATH", pathJoin("/data/prod/glopara/git/prepobs/feature-GFSv17_com_reorg_log_update/modulefiles"))
load(pathJoin("prepobs", (os.getenv("prepobs_run_ver") or "None")))

prepend_path("MODULEPATH", pathJoin("/data/prod/glopara/git/Fit2Obs/v" .. (os.getenv("fit2obs_ver") or "None"), "modulefiles"))
load(pathJoin("fit2obs", (os.getenv("fit2obs_ver") or "None")))

whatis("Description: GFS run environment")
