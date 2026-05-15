help([[
  This module loads libraries required for running atmos_post job
  on the NOAA Cloud (AWS/Azure) using ue-oneapi-2025.3.0.
]])
setenv("MODULES_AUTO_HANDLING","1")

prepend_path("MODULEPATH", "/pw/apps/modules/intel/2025.3.0")
prepend_path("MODULEPATH", "/contrib/spack-stack-rocky9/spack-stack-2.1.0/envs/ue-oneapi-2025.3.0/modules/Core")

load("stack-intel-oneapi-compilers/2025.3.0")
load("stack-intel-oneapi-mpi/2021.17")

load("wgrib2/3.8.0")
load("grib-util/1.4.0")

setenv("WGRIB2","wgrib2")
setenv("GMERGE","gmerge")

whatis("Description: atmos_post run environment on noaa cloud")
