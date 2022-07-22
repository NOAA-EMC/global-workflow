help([[
Load environment to run GFS on WCOSS2
]])

load(pathJoin("PrgEnv-intel", os.getenv("PrgEnv_intel_ver")))
load(pathJoin("craype", os.getenv("craype_ver")))
load(pathJoin("intel", os.getenv("intel_ver")))
load(pathJoin("cray-mpich", os.getenv("cray_mpich_ver")))
load(pathJoin("cray-pals", os.getenv("cray_pals_ver")))
load(pathJoin("esmf", os.getenv("esmf_ver")))
load(pathJoin("cfp", os.getenv("cfp_ver")))
setenv("USE_CFP","YES")

load(pathJoin("python", os.getenv("python_ver")))
load(pathJoin("prod_envir", os.getenv("prod_envir_ver")))
load(pathJoin("gempak", os.getenv("gempak_ver")))
load(pathJoin("perl", os.getenv("perl_ver")))
load(pathJoin("libjpeg", os.getenv("libjpeg_ver")))

load(pathJoin("cdo", os.getenv("cdo_ver")))

load(pathJoin("hdf5", os.getenv("hdf5_ver")))
load(pathJoin("netcdf", os.getenv("netcdf_ver")))

load(pathJoin("udunits", os.getenv("udunits_ver")))
load(pathJoin("gsl", os.getenv("gsl_ver")))
load(pathJoin("nco", os.getenv("nco_ver")))
load(pathJoin("prod_util", os.getenv("prod_util_ver")))
load(pathJoin("grib_util", os.getenv("grib_util_ver")))
load(pathJoin("bufr_dump", os.getenv("bufr_dump_ver")))
load(pathJoin("util_shared", os.getenv("util_shared_ver")))
load(pathJoin("crtm", os.getenv("crtm_ver")))
load(pathJoin("g2tmpl", os.getenv("g2tmpl_ver")))
load(pathJoin("wgrib2", os.getenv("wgrib2_ver")))

whatis("Description: GFS run environment")
