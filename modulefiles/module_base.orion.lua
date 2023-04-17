help([[
Load environment to run GFS on Orion
]])

prepend_path("MODULEPATH", "/apps/contrib/NCEP/hpc-stack/libs/hpc-stack/modulefiles/stack")

load(pathJoin("hpc", "1.1.0"))
load(pathJoin("hpc-intel", "2018.4"))
load(pathJoin("hpc-impi", "2018.4"))

load(pathJoin("gempak", "7.5.1"))
load(pathJoin("ncl", "6.6.2"))
load(pathJoin("jasper", "2.0.25"))
load(pathJoin("zlib", "1.2.11"))
load(pathJoin("png", "1.6.35"))
load(pathJoin("cdo", "1.9.5"))

load(pathJoin("hdf5", "1.10.6"))
load(pathJoin("netcdf", "4.7.4"))

load(pathJoin("nco", "4.8.1"))
load(pathJoin("prod_util", "1.2.2"))
load(pathJoin("grib_util", "1.2.2"))
load(pathJoin("g2tmpl", "1.10.0"))
load(pathJoin("ncdiag", "1.0.0"))
load(pathJoin("crtm", "2.4.0"))
load(pathJoin("wgrib2", "2.0.8"))
setenv("WGRIB2","wgrib2")

prepend_path("MODULEPATH", pathJoin("/work/noaa/global/glopara/git/prepobs/feature-GFSv17_com_reorg/modulefiles"))
load(pathJoin("prepobs", "1.0.1"))

-- Temporary until official hpc-stack is updated
prepend_path("MODULEPATH", "/work2/noaa/global/wkolczyn/save/hpc-stack/modulefiles/stack")
load(pathJoin("hpc", "1.2.0"))
load(pathJoin("hpc-intel", "2018.4"))
load(pathJoin("hpc-miniconda3", "4.6.14"))
load(pathJoin("ufswm", "1.0.0"))
load(pathJoin("met", "9.1"))
load(pathJoin("metplus", "3.1"))

whatis("Description: GFS run environment")
