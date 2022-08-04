help([[
Build environment for workflow utilities on WCOSS2
]])

load(pathJoin("PrgEnv-intel", os.getenv("PrgEnv_intel_ver")))
load(pathJoin("craype", os.getenv("craype_ver")))
load(pathJoin("intel", os.getenv("intel_ver")))
load(pathJoin("cray-mpich", os.getenv("cray_mpich_ver")))

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
load(pathJoin("w3nco", os.getenv("w3nco_ver")))
load(pathJoin("nemsiogfs", os.getenv("nemsiogfs_ver")))
load(pathJoin("ncio", os.getenv("ncio_ver")))
load(pathJoin("landsfcutil", os.getenv("landsfcutil_ver")))
load(pathJoin("sigio", os.getenv("sigio_ver")))
load(pathJoin("bufr", os.getenv("bufr_ver")))

load(pathJoin("wgrib2", os.getenv("wgrib2_ver")))
