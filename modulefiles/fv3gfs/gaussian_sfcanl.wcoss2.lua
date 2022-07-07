help([[
Load environment for building gaussian_sfcanl on WCOSS2
]])

setenv("ver","v16.2.0")

load(pathJoin("PrgEnv-intel", os.getenv("PrgEnv_intel_ver")))
load(pathJoin("craype", os.getenv("craype_ver")))
load(pathJoin("intel", os.getenv("intel_ver")))
load(pathJoin("cray-mpich", os.getenv("cray_mpich_ver")))

load(pathJoin("hdf5", os.getenv("hdf5_ver")))
load(pathJoin("netcdf", os.getenv("netcdf_ver")))

load(pathJoin("bacio", os.getenv("bacio_ver")))
load(pathJoin("w3nco", os.getenv("w3nco_ver")))
load(pathJoin("nemsio", os.getenv("nemsio_ver")))
load(pathJoin("sp", os.getenv("sp_ver")))

setenv("FCOMP","ftn")
