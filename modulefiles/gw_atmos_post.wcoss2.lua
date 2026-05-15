help([[
  This module loads libraries required for running atmos_post job
  on Wcoss2 (NOAA Acorn) using ue-oneapi-2024.2.1.
]])
prepend_path("MODULEPATH", "/lfs/h1/emc/nceplibs/noscrub/spack-stack/spack-stack-2.1.0/envs/ue-oneapi-2024.2.1/modules/Core")

load("stack-intel-oneapi-compilers/2024.2.1")
load("stack-cray-mpich/8.1.29")

load("wgrib2/3.8.0")
load("grib-util/1.4.0")

setenv("WGRIB2","wgrib2")
setenv("GMERGE","gmerge")

whatis("Description: atmos_post run environment on Wcoss2 (NOAA Acorn)")
