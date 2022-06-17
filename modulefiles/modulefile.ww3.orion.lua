help([[
Build environment for WW3 on Orion 
]])

prepend_path("MODULEPATH", "/apps/contrib/NCEP/libs/hpc-stack/modulefiles/stack")

load(pathJoin("hpc", "1.1.0"))
load(pathJoin("hpc-intel", "2022.1.2"))
load(pathJoin("hpc-impi", "2022.1.2"))

load("contrib")
load("noaatools")
load(pathJoin("cmake", "3.22.1"))

load(pathJoin("jasper", "2.0.25"))
load(pathJoin("zlib", "1.2.11"))
load(pathJoin("libpng", "1.6.37"))

load(pathJoin("bacio", "2.4.1"))
load(pathJoin("g2", "3.4.5"))

load(pathJoin("hdf5", "1.10.6"))
load(pathJoin("netcdf", "4.7.4"))

load(pathJoin("w3nco", "2.4.1"))
