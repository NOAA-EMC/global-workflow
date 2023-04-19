help([[
Load environment to run GFS workflow ci scripts on Orion
]])

prepend_path("MODULEPATH", "/apps/contrib/NCEP/hpc-stack/libs/hpc-stack/modulefiles/stack")

load(pathJoin("hpc", "1.1.0"))
load(pathJoin("hpc-intel", "2018.4"))
load(pathJoin("hpc-impi", "2018.4"))
load(pathJoin("netcdf","4.7.4"))
load(pathJoin("nccmp","1.8.5"))
load(pathJoin("contrib","0.1"))
load(pathJoin("wgrib2","3.0.2"))

prepend_path("MODULEPATH", "/work2/noaa/global/wkolczyn/save/hpc-stack/modulefiles/stack")
load(pathJoin("hpc", "1.2.0"))
load(pathJoin("hpc-intel", "2018.4"))
load(pathJoin("hpc-miniconda3", "4.6.14"))
load(pathJoin("gfs_workflow", "1.0.0"))

whatis("Description: GFS run ci top-level sripts environment")
