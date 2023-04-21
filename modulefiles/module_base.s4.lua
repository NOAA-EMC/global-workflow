help([[
Load environment to run GFS on S4
]])

load("license_intel")
prepend_path("MODULEPATH", "/data/prod/hpc-stack/modulefiles/stack")

load(pathJoin("hpc", "1.1.0"))
load(pathJoin("hpc-intel", "18.0.4"))
load(pathJoin("hpc-impi", "18.0.4"))

load(pathJoin("miniconda", "3.8-s4"))
load(pathJoin("ncl", "6.4.0-precompiled"))
load(pathJoin("cdo", "1.9.8"))
load(pathJoin("jasper", "2.0.25"))
load(pathJoin("zlib", "1.2.11"))
load(pathJoin("png", "1.6.35"))

load(pathJoin("hdf5", "1.10.6"))
load(pathJoin("netcdf", "4.7.4"))

load(pathJoin("nco", "4.9.3"))
load(pathJoin("prod_util", "1.2.2"))
load(pathJoin("grib_util", "1.2.2"))
load(pathJoin("g2tmpl", "1.10.0"))
load(pathJoin("ncdiag", "1.0.0"))
load(pathJoin("crtm", "2.4.0"))
load(pathJoin("wgrib2", "2.0.8"))
setenv("WGRIB2","wgrib2")

prepend_path("MODULEPATH", pathJoin("/data/prod/glopara/git/prepobs/v1.0.1/modulefiles"))
load(pathJoin("prepobs", "1.0.1"))

prepend_path("MODULEPATH", pathJoin("/data/prod/glopara/git/Fit2Obs/v1.0.0/modulefiles"))
load(pathJoin("fit2obs", "1.0.0"))

whatis("Description: GFS run environment")
