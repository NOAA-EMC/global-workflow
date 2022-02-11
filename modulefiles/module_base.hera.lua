help([[
Load environment to run GFS on Hera
]])

prepend_path("MODULEPATH", "/scratch2/NCEPDEV/nwprod/hpc-stack/libs/hpc-stack/modulefiles/stack")

load(pathJoin("hpc", os.getenv("hpc_ver")))
load(pathJoin("hpc-intel", os.getenv("hpc_intel_ver")))
load(pathJoin("hpc-impi", os.getenv("hpc_impi_ver")))

load(pathJoin("esmf", os.getenv("esmf_ver")))

load(pathJoin("python", os.getenv("python_ver")))
load(pathJoin("gempak", os.getenv("gempak_ver")))
load(pathJoin("perl", os.getenv("perl_ver")))
load(pathJoin("libjpeg", os.getenv("libjpeg_ver")))
load(pathJoin("hpss", os.getenv("hpss_ver")))

load(pathJoin("cdo", os.getenv("cdo_ver")))

load(pathJoin("hdf5", os.getenv("hdf5_ver")))
load(pathJoin("netcdf", os.getenv("netcdf_ver")))

load(pathJoin("udunits", os.getenv("udunits_ver")))
load(pathJoin("nco", os.getenv("nco_ver")))
load(pathJoin("prod_util", os.getenv("prod_util_ver")))
load(pathJoin("grib_util", os.getenv("grib_util_ver")))
load(pathJoin("bufr_dump", os.getenv("bufr_dump_ver")))
load(pathJoin("util_shared", os.getenv("util_shared_ver")))
load(pathJoin("crtm", os.getenv("crtm_ver")))
load(pathJoin("g2tmpl", os.getenv("g2tmpl_ver")))
load(pathJoin("wgrib2", os.getenv("wgrib2_ver")))

setenv("USE_CFP","YES")

whatis("Description: GFS run environment")
