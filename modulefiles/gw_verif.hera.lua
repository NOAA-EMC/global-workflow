help([[
Load environment to run GFS on Hera
]])

-- Test that HOMEgfs is set.
-- If not, load_gw_run_modules.sh was not sourced to load this module.
prepend_path("MODULEPATH", "/contrib/spack-stack/spack-stack-1.6.0/envs/gsi-addon-dev-fms-2024.01/install/modulefiles/Core")
load("stack-intel")
load("stack-python")
load("stack-intel-oneapi-mpi")
load("met/9.1.3")
load("metplus/3.1.1")
load("grib-util")
load("wgrib2")

whatis("Description: GFS run environment")

load(pathJoin("imagemagick", (os.getenv("imagemagick_ver") or "None")))
