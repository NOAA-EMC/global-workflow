help([[
  This module loads libraries required for running atmos_post job
  on the NOAA RDHPC machine GaeaC6 using ue-oneapi-2025.2.1.
]])
prepend_path("MODULEPATH", "/ncrc/proj/epic/spack-stack/c6/spack-stack-2.1.0/envs/ue-oneapi-2025.2.1/modules/Core")

load("stack-intel-oneapi-compilers/2025.2.1")
load("stack-cray-mpich/8.1.32")

load("prod_util/2.1.2")

load("wgrib2/3.8.0")
load("grib-util/1.4.0")

setenv("WGRIB2","wgrib2")
setenv("GMERGE","gmerge")

whatis("Description: atmos_post run environment on GaeaC6")
