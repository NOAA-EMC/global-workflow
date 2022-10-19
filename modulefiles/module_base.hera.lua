help([[
Load environment to run GFS on Hera
]])

prepend_path("MODULEPATH", "/scratch2/NCEPDEV/nwprod/hpc-stack/libs/hpc-stack/modulefiles/stack")

load(pathJoin("hpc", "1.1.0"))
load(pathJoin("hpc-intel", "18.0.5.274"))
load(pathJoin("hpc-impi", "2018.0.4"))

load(pathJoin("cmake", "3.20.1"))

load(pathJoin("hpss", "hpss"))
load(pathJoin("nco", "4.9.1"))
load(pathJoin("gempak", "7.4.2"))
load(pathJoin("ncl", "6.6.2"))

load(pathJoin("prod_util", "1.2.2"))
load(pathJoin("grib_util", "1.2.2"))

load(pathJoin("crtm", "2.3.0"))
setenv("CRTM_FIX","/scratch2/NCEPDEV/nwprod/NCEPLIBS/fix/crtm_v2.3.0")

load(pathJoin("jasper", "2.0.25"))
load(pathJoin("zlib", "1.2.11"))
load(pathJoin("png", "1.6.35"))

load(pathJoin("hdf5", "1.10.6"))
load(pathJoin("netcdf", "4.7.4"))
load(pathJoin("pio", "2.5.7"))
load(pathJoin("esmf", "8.3.0b09"))
load(pathJoin("fms", "2021.03"))

load(pathJoin("bacio", "2.4.1"))
load(pathJoin("g2", "3.4.2"))
load(pathJoin("g2tmpl", "1.10.0"))
load(pathJoin("ip", "3.3.3"))
load(pathJoin("nemsio", "2.5.2"))
load(pathJoin("sp", "2.3.3"))
load(pathJoin("w3emc", "2.7.3"))
load(pathJoin("w3nco", "2.4.1"))
load(pathJoin("ncdiag", "1.0.0"))

load(pathJoin("wgrib2", "2.0.8"))
setenv("WGRIB2","wgrib2")

load(pathJoin("cdo", "1.9.5"))

load(pathJoin("R", "3.5.0"))

prepend_path("MODULEPATH", pathJoin("/scratch1/NCEPDEV/global/glopara/git/prepobs/v1.0.1/modulefiles"))
load(pathJoin("prepobs", "1.0.1"))

-- Temporary until official hpc-stack is updated
prepend_path("MODULEPATH", "/scratch2/NCEPDEV/ensemble/save/Walter.Kolczynski/hpc-stack/modulefiles/stack")
load(pathJoin("hpc", "1.2.0"))
load(pathJoin("hpc-intel", "18.0.5.274"))
load(pathJoin("hpc-miniconda3", "4.6.14"))
load(pathJoin("ufswm", "1.0.0"))
load(pathJoin("met", "9.1"))
load(pathJoin("metplus", "3.1"))

whatis("Description: GFS run environment")
