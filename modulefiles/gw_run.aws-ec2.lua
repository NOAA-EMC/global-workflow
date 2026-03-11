help([[
Load environment to run GFS on AWS EC2
]])

prepend_path("MODULEPATH", "/opt/spack-stack/envs/ue-oneapi-2024.2.1/install/modulefiles/Core")
-- prepend_path("MODULEPATH", "/opt/spack-stack/envs/ue-oneapi-2024.2.1/install/modulefiles/oneapi/2024.2.1")
prepend_path("MODULEPATH", "/opt/modulefiles")

local stack_oneapi_ver=os.getenv("stack_oneapi_ver") or "2024.2.1"
local stack_impi_ver=os.getenv("stack_impi_ver") or "2021.13"
local cmake_ver=os.getenv("cmake_ver") or "3.27.9"

load(pathJoin("stack-oneapi", stack_oneapi_ver))
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))
load(pathJoin("cmake", cmake_ver))

local gw_run_modules = {
  {["stack-python"]    = "3.11.7" },
  {["jasper"]          = "2.0.32" },
  {["libpng"]          = "1.6.37" },
  {["cdo"]             = "2.4.4" },
  {["hdf5"]            = "1.14.3" },
  {["netcdf-c"]        = "4.9.2" },
  {["netcdf-fortran"]  = "4.6.1" },
  {["esmf"]            = "8.8.0" },
  {["w3nco"]           = "2.4.1" },
  {["prod_util"]       = "2.1.1" },
  {["grib-util"]       = "1.4.0" },
  {["g2tmpl"]          = "1.13.0" },
  {["gsi-ncdiag"]      = "1.1.2" },
  {["crtm"]            = "2.4.0.1" },
  {["bufr"]            = "12.1.0" },
  {["wgrib2"]          = "3.6.0" },
  {["py-f90nml"]       = "1.4.3" },
  {["py-netcdf4"]      = "1.7.1.post2" },
  {["py-pyyaml"]       = "6.0.2" },
  {["py-jinja2"]       = "3.1.4" },
  {["py-pandas"]       = "2.2.3" },
  {["py-python-dateutil"]  = "2.8.2" },
  {["py-xarray"]       = "2024.7.0" },
  {["libfabric-aws"]   = "2.1.0amzn2.0" },
  -- TODO: Reenable when MET/METplus and verif-global are at compatible versions
  -- "met",
  -- "metplus",
}

for i = 1, #gw_run_modules do
  for name, version in pairs(gw_run_modules[i]) do
    load(pathJoin(name, version))
  end
end

setenv("UTILROOT",(os.getenv("prod_util_ROOT") or "None"))
setenv("I_MPI_PMI_LIBRARY", "/opt/slurm/lib/libpmi2.so")

-- prepend_path("MODULEPATH", pathJoin("/contrib/git/prepobs/v" .. (os.getenv("prepobs_run_ver") or "None"), "modulefiles"))
-- load(pathJoin("prepobs", (os.getenv("prepobs_run_ver") or "None")))

-- prepend_path("MODULEPATH", pathJoin("/contrib/git/Fit2Obs/v" .. (os.getenv("fit2obs_ver") or "None"), "modulefiles"))
-- load(pathJoin("fit2obs", (os.getenv("fit2obs_ver") or "None")))

-- load(pathJoin("imagemagick", (os.getenv("imagemagick_ver") or "None")))

-- setenv("CRTM_FIX", "/lustre/fix/crtm/v2.4.0.2")

whatis("Description: GFS run environment")
