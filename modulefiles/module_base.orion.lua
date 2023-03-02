help([[
Load environment to run GFS on Orion
]])

prepend_path("MODULEPATH", "/apps/contrib/NCEP/hpc-stack/libs/hpc-stack/modulefiles/stack")

load(pathJoin("hpc", os.getenv("hpc_ver")))
load(pathJoin("hpc-intel", os.getenv("hpc_intel_ver")))
load(pathJoin("hpc-impi", os.getenv("hpc_impi_ver")))

load(pathJoin("gempak", os.getenv("gempak_ver")))
load(pathJoin("ncl", os.getenv("ncl_ver")))
load(pathJoin("jasper", os.getenv("jasper_ver")))
load(pathJoin("zlib", os.getenv("zlib_ver")))
load(pathJoin("png", os.getenv("png_ver")))
load(pathJoin("cdo", os.getenv("cdo_ver")))

load(pathJoin("hdf5", os.getenv("hdf5_ver")))
load(pathJoin("netcdf", os.getenv("netcdf_ver")))

load(pathJoin("nco", os.getenv("nco_ver")))
load(pathJoin("prod_util", os.getenv("prod_util_ver")))
load(pathJoin("grib_util", os.getenv("grib_util_ver")))
load(pathJoin("g2tmpl", os.getenv("g2tmpl_ver")))
load(pathJoin("ncdiag", os.getenv("ncdiag_ver")))
load(pathJoin("crtm", os.getenv("crtm_ver")))
load(pathJoin("wgrib2", os.getenv("wgrib2_ver")))
setenv("WGRIB2","wgrib2")

prepend_path("MODULEPATH", pathJoin("/work/noaa/global/glopara/git/prepobs/v" .. os.getenv("prepobs_run_ver"), "modulefiles"))
load(pathJoin("prepobs", os.getenv("prepobs_run_ver")))

-- Temporary until official hpc-stack is updated
prepend_path("MODULEPATH", "/work2/noaa/global/wkolczyn/save/hpc-stack/modulefiles/stack")
load(pathJoin("hpc", "1.2.0"))
load(pathJoin("hpc-intel", "2018.4"))
load(pathJoin("hpc-miniconda3", "4.6.14"))
load(pathJoin("ufswm", "1.0.0"))
load(pathJoin("met", "9.1"))
load(pathJoin("metplus", "3.1"))
--

whatis("Description: GFS run environment")
