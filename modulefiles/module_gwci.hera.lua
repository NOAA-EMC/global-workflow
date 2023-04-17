help([[
Load environment to run GFS workflow setup scripts on Hera
]])

prepend_path("MODULEPATH", "/scratch2/NCEPDEV/nwprod/hpc-stack/libs/hpc-stack/modulefiles/stack")

load(pathJoin("hpc", "1.1.0"))
load(pathJoin("hpc-intel", "18.0.5.274"))
load(pathJoin("hpc-impi", "2018.0.4"))

load(pathJoin("netcdf","4.7.0"))
load(pathJoin("nccmp","1.8.5"))
load(pathJoin("wgrib2", "2.0.8"))

whatis("Description: GFS run setup CI environment")
