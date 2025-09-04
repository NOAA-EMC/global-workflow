help([[
Load environment to run EMC_verif-global on Hera
]])

prepend_path("MODULEPATH", "/contrib/spack-stack/spack-stack-1.6.0/envs/gsi-addon-dev-fms-2024.01/install/modulefiles/Core")
load("stack-intel")
load("stack-python")
load("stack-intel-oneapi-mpi")
load("met/9.1.3")
load("metplus/3.1.1")
load("grib-util")
load("prod_util")
load("wgrib2")

whatis("Description: EMC_verif-global run environment")
