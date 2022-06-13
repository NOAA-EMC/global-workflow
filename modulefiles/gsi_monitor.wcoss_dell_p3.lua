help([[
Build environment for GSI monitor on WCOSS Dell
]])

prepend_path("MODULEPATH", "/usrx/local/nceplibs/dev/hpc-stack/libs/hpc-stack/modulefiles/stack")

load(pathJoin("hpc", "1.1.0"))
load(pathJoin("hpc-ips", "18.0.1.163"))
load(pathJoin("hpc-impi", "18.0.1"))

load(pathJoin("cmake", "3.20.1"))

load(pathJoin("hdf5", "1.10.6"))
load(pathJoin("netcdf", "4.7.4"))

load(pathJoin("w3emc", "2.9.1"))
