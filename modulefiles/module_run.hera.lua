help([[
Load environment to run GFS on Hera
]])

prepend_path("MODULEPATH", "/scratch1/NCEPDEV/nems/role.epic/spack-stack/spack-stack-1.6.0/envs/gsi-addon-dev-fms-2024.01/install/modulefiles/Core")

-- load(pathJoin("hpss", (os.getenv("hpss_ver") or "None")))

load(pathJoin("stack-intel", (os.getenv("stack_intel_ver") or "2021.5.0")))
load(pathJoin("stack-intel-oneapi-mpi", (os.getenv("stack_impi_ver") or "2021.5.1")))
load(pathJoin("python", (os.getenv("python_ver") or "3.11.6")))
load(pathJoin("prod_util", (os.getenv("prod_util_ver") or "2.1.1")))
load(pathJoin("py-f90nml", (os.getenv("py_f90nml_ver") or "1.4.3")))
load(pathJoin("py-netcdf4", (os.getenv("py_netcdf4_ver") or "1.5.8")))
load(pathJoin("py-pyyaml", (os.getenv("py_pyyaml_ver") or "6.0")))
load(pathJoin("py-jinja2", (os.getenv("py_jinja2_ver") or "3.1.2")))
load(pathJoin("py-pandas", (os.getenv("py_pandas_ver") or "1.5.3")))
load(pathJoin("py-numpy", (os.getenv("py_numpy_ver") or "1.23.4")))
load(pathJoin("py-xarray", (os.getenv("py_xarray_ver") or "2023.7.0")))
load(pathJoin("py-python-dateutil", (os.getenv("py_python_dateutil_ver") or "2.8.2")))

setenv("WGRIB2","wgrib2")
setenv("WGRIB","wgrib")
-- setenv("UTILROOT",(os.getenv("prod_util_ROOT") or "None"))

whatis("Description: GFS run host environment")

