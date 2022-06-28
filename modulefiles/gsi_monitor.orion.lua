help([[
Build environment for GSI monitor on Orion
]])

prepend_path("MODULEPATH", "/apps/contrib/NCEP/libs/hpc-stack/modulefiles/stack")

load(pathJoin("hpc", "1.1.0"))
load(pathJoin("hpc-intel", "2018.4"))
load(pathJoin("hpc-impi", "2018.4"))

load(pathJoin("cmake", "3.22.1"))

load(pathJoin("hdf5", "1.10.6"))
load(pathJoin("netcdf", "4.7.4"))

load(pathJoin("w3emc", "2.9.1"))
