help([[
Build environment for WW3 on S4
]])

load("license_intel")
prepend_path("MODULEPATH", "/data/prod/hpc-stack/modulefiles/stack")
load(pathJoin("hpc", "1.1.0"))
load(pathJoin("hpc-intel", "18.0.4"))
load(pathJoin("hpc-impi", "18.0.4"))

load(pathJoin("jasper", "2.0.25"))
load(pathJoin("zlib", "1.2.11"))
load(pathJoin("png", "1.6.35"))

load(pathJoin("bacio", "2.4.1"))
load(pathJoin("g2", "3.4.1"))

load(pathJoin("hdf5", "1.10.6"))
load(pathJoin("netcdf", "4.7.4"))

load(pathJoin("w3emc", "2.9.2"))
