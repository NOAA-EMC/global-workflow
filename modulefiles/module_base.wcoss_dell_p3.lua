help([[
Load environment to run GFS on WCOSS-Dell
]])

prepend_path("MODULEPATH", "/usrx/local/nceplibs/dev/hpc-stack/libs/hpc-stack/modulefiles/stack")

load(pathJoin("hpc", "1.1.0"))
load(pathJoin("hpc-ips", "18.0.1.163"))
load(pathJoin("hpc-impi", "18.0.1"))

load(pathJoin("lsf", "10.1"))
load(pathJoin("EnvVars", "1.0.3"))
load(pathJoin("HPSS", "5.0.2.5"))
load(pathJoin("NCL", "6.4.0"))

load(pathJoin("prod_util", "1.2.2"))
load(pathJoin("prod_envir", "1.1.0"))
load(pathJoin("grib_util", "1.2.2"))
load(pathJoin("util_shared", "1.3.0"))

load(pathJoin("crtm", "2.3.0"))
setenv("CRTM_FIX","/gpfs/dell1/nco/ops/nwprod/lib/crtm/v2.3.0/fix")

load(pathJoin("NCO", "4.7.0"))
load(pathJoin("CFP", "2.0.2"))
setenv("USE_CFP","YES")
load("pm5")

load(pathJoin("jasper", "2.0.25"))
load(pathJoin("zlib", "1.2.11"))
load(pathJoin("png", "1.6.35"))

load(pathJoin("hdf5", "1.10.6"))
load(pathJoin("netcdf", "4.7.4"))
load(pathJoin("pio", "2.5.2"))
load(pathJoin("esmf", "8.2.1b04"))
load(pathJoin("fms", "2021.03"))

load(pathJoin("bacio", "2.4.1"))
load(pathJoin("g2", "3.4.2"))
load(pathJoin("g2tmpl", "1.10.0"))
load(pathJoin("ip", "3.3.3"))
load(pathJoin("nemsio", "2.5.2"))
load(pathJoin("sp", "2.3.3"))
load(pathJoin("w3emc", "2.7.3"))
load(pathJoin("w3nco", "2.4.1"))

load(pathJoin("wgrib2", "2.0.8"))
setenv("WGRIB2","wgrib2")

append_path("MODULEPATH", "/gpfs/dell1/nco/ops/nwprod/modulefiles/")
load(pathJoin("gempak", "7.3.3"))

load(pathJoin("bufr_dumplist", "2.0.0"))
load(pathJoin("dumpjb", "5.1.0"))

load(pathJoin("python", "3.6.3"))

load(pathJoin("cdo", "1.9.8"))

whatis("Description: GFS run environment")
