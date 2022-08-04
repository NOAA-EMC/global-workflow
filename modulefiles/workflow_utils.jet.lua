help([[
Build environment for workflow utilities on Jet
]])

prepend_path("MODULEPATH", "/lfs4/HFIP/hfv3gfs/nwprod/hpc-stack/libs/modulefiles/stack")

load(pathJoin("hpc", os.getenv("hpc_ver")))
load(pathJoin("hpc-intel", os.getenv("hpc_intel_ver")))
load(pathJoin("hpc-impi", os.getenv("hpc_impi_ver")))

load(pathJoin("cmake", os.getenv("cmake_ver")))

load(pathJoin("jasper", os.getenv("jasper_ver")))
load(pathJoin("zlib", os.getenv("zlib_ver")))
load(pathJoin("libpng", os.getenv("libpng_ver")))

load(pathJoin("hdf5", os.getenv("hdf5_ver")))
load(pathJoin("netcdf", os.getenv("netcdf_ver")))

load(pathJoin("bacio", os.getenv("bacio_ver")))
load(pathJoin("g2", os.getenv("g2_ver")))
load(pathJoin("ip", os.getenv("ip_ver")))
load(pathJoin("nemsio", os.getenv("nemsio_ver")))
load(pathJoin("sp", os.getenv("sp_ver")))
load(pathJoin("w3emc", os.getenv("w3emc_ver")))
load(pathJoin("nemsiogfs", os.getenv("nemsiogfs_ver")))
load(pathJoin("ncio", os.getenv("ncio_ver")))
load(pathJoin("landsfcutil", os.getenv("landsfcutil_ver")))
load(pathJoin("sigio", os.getenv("sigio_ver")))
load(pathJoin("bufr", os.getenv("bufr_ver")))

load(pathJoin("wgrib2", os.getenv("wgrib2_ver")))
