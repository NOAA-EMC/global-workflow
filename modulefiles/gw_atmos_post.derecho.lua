help([[
  This module loads libraries required for running atmos_post job
  on NCAR Derecho using ue-oneapi-2025.3.1.
]])
prepend_path("MODULEPATH", "/opt/cray/pe/modulefiles")
load("crayenv/25.03")

prepend_path("MODULEPATH", "/glade/work/epicufsrt/contrib/spack-stack/derecho/installs/oneapi-2025.3.1/modulefiles")
prepend_path("MODULEPATH", "/glade/work/epicufsrt/contrib/spack-stack/derecho/spack-stack-2.1.0/envs/ue-oneapi-2025.3.1/modules/Core")

load("stack-intel-oneapi-compilers/2025.3.1")
load("stack-cray-mpich/8.1.32")

unload("cray-libsci")

load("wgrib2/3.8.0")
load("grib-util/1.4.0")

setenv("WGRIB2","wgrib2")
setenv("GMERGE","gmerge")

whatis("Description: atmos_post run environment on NCAR Derecho)")
