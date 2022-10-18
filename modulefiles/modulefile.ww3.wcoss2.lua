help([[
Build environment for WW3 on WCOSS2 
]])

load(pathJoin("PrgEnv-intel", "8.1.0"))
load(pathJoin("craype", "2.7.10"))
load(pathJoin("intel", "19.1.3.304"))
load(pathJoin("cray-mpich", "8.1.9"))

load(pathJoin("cmake", "3.20.2"))

load(pathJoin("jasper", "2.0.25"))
load(pathJoin("zlib", "1.2.11"))
load(pathJoin("libpng", "1.6.37"))

load(pathJoin("bacio", "2.4.1"))
load(pathJoin("g2", "3.4.5"))

load(pathJoin("hdf5", "1.10.6"))
load(pathJoin("netcdf", "4.7.4"))

load(pathJoin("w3emc", "2.9.2"))
