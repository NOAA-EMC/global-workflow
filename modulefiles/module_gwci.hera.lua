help([[
Load environment to run GFS workflow setup scripts on Hera
]])

load(pathJoin("nccmp","1.9.0.1"))

prepend_path("MODULEPATH", "/scratch2/NCEPDEV/nwprod/hpc-stack/libs/hpc-stack/modulefiles/stack")

load(pathJoin("hpc", "1.1.0"))
load(pathJoin("hpc-intel", "18.0.5.274"))
load(pathJoin("hpc-impi", "2018.0.4"))

load(pathJoin("wgrib2", "2.0.8"))

whatis("Description: GFS run setup CI environment")
