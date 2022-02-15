help([[
Load environment to build gfs_bufr on Hera
]])

prepend_path("MODULEPATH", "/scratch2/NCEPDEV/nwprod/hpc-stack/libs/hpc-stack/modulefiles/stack")

load(pathJoin("hpc", os.getenv("hpc_ver")))
load(pathJoin("hpc-intel", os.getenv("hpc_intel_ver")))
load(pathJoin("hpc-impi", os.getenv("hpc_impi_ver")))

load(pathJoin("gempak", os.getenv("gempak_ver")))

load(pathJoin("hdf5", os.getenv("hdf5_ver")))
load(pathJoin("netcdf", os.getenv("netcdf_ver")))

load(pathJoin("bacio", os.getenv("bacio_ver")))
load(pathJoin("w3nco", os.getenv("w3nco_ver")))
load(pathJoin("nemsio", os.getenv("nemsio_ver")))
load(pathJoin("sigio", os.getenv("sigio_ver")))
load(pathJoin("w3emc", os.getenv("w3emc_ver")))
load(pathJoin("bufr", os.getenv("bufr_ver")))

setenv("myFC","mpiifort")
setenv("myFCFLAGS","-O3 -convert big_endian -traceback -g -fp-model source -qopenmp")
setenv("myCPP","/lib/cpp")
setenv("myCPPFLAGS","-P")
