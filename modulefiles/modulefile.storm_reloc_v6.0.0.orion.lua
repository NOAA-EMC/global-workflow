help([[
Load environment to build storm_reloc on Orion
]])

prepend_path("MODULEPATH", "/apps/contrib/NCEP/libs/hpc-stack-gfsv16/modulefiles/stack")

load(pathJoin("hpc", os.getenv("hpc_ver")))
load(pathJoin("hpc-intel", os.getenv("hpc_intel_ver")))
load(pathJoin("hpc-impi", os.getenv("hpc_impi_ver")))

load(pathJoin("jasper", os.getenv("jasper_ver")))
load(pathJoin("libpng", os.getenv("libpng_ver")))
load(pathJoin("zlib", os.getenv("zlib_ver")))

load(pathJoin("bacio", os.getenv("bacio_ver")))
load(pathJoin("w3nco", os.getenv("w3nco_ver")))
load(pathJoin("nemsio", os.getenv("nemsio_ver")))
load(pathJoin("nemsiogfs", os.getenv("nemsiogfs_ver")))
load(pathJoin("sigio", os.getenv("sigio_ver")))
load(pathJoin("w3emc", os.getenv("w3emc_ver")))
load(pathJoin("sp", os.getenv("sp_ver")))
load(pathJoin("g2", os.getenv("g2_ver")))

setenv("myFC","mpiifort")
