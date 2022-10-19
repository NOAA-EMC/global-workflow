help([[
Build environment for workflow utilities on WCOSS2
]])

load(pathJoin("PrgEnv-intel", "8.1.0"))
load(pathJoin("craype", "2.7.10"))
load(pathJoin("intel", "19.1.3.304"))
load(pathJoin("cray-mpich", "8.1.9"))

load(pathJoin("cmake", "3.20.2"))

load(pathJoin("jasper", "2.0.25"))
load(pathJoin("zlib", "1.2.11"))
load(pathJoin("libpng", "1.6.37"))

load(pathJoin("hdf5", "1.10.6"))
load(pathJoin("netcdf", "4.7.4"))

load(pathJoin("bacio", "2.4.1"))
load(pathJoin("g2", "3.4.5"))
load(pathJoin("ip", "3.3.3"))
load(pathJoin("nemsio", "2.5.2"))
load(pathJoin("sp", "2.3.3"))
load(pathJoin("w3emc", "2.9.2"))
load(pathJoin("w3nco", "2.4.1"))
load(pathJoin("nemsiogfs", "2.5.3"))
load(pathJoin("ncio", "1.0.0"))
load(pathJoin("landsfcutil", "2.4.1"))
load(pathJoin("sigio", "2.3.2"))
load(pathJoin("bufr", "11.5.0"))

load(pathJoin("wgrib2", "2.0.8"))
