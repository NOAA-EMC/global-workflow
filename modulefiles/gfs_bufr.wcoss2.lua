help([[
Load environment to build gfs_bufr on WCOSS2
]])

load(pathJoin("PrgEnv-intel", os.getenv("PrgEnv_intel_ver")))
load(pathJoin("craype", os.getenv("craype_ver")))
load(pathJoin("intel", os.getenv("intel_ver")))
load(pathJoin("cray-mpich", os.getenv("cray_mpich_ver")))

load(pathJoin("gempak", os.getenv("gempak_ver")))

load(pathJoin("hdf5", os.getenv("hdf5_ver")))
load(pathJoin("netcdf", os.getenv("netcdf_ver")))

load(pathJoin("bacio", os.getenv("bacio_ver")))
load(pathJoin("w3nco", os.getenv("w3nco_ver")))
load(pathJoin("nemsio", os.getenv("nemsio_ver")))
load(pathJoin("sigio", os.getenv("sigio_ver")))
load(pathJoin("w3emc", os.getenv("w3emc_ver")))
load(pathJoin("bufr", os.getenv("bufr_ver")))

setenv("myFC","ftn")
setenv("myFCFLAGS","-O3 -convert big_endian -traceback -g -fp-model source")
setenv("myCPP","/lib/cpp")
setenv("myCPPFLAGS","-P")
