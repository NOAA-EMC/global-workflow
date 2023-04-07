help([[
Load environment to run GFS workflow setup scripts on Hera
]])

-- Temporary until official hpc-stack is updated

load(pathJoin("rocoto"))

prepend_path("MODULEPATH", "/scratch2/NCEPDEV/ensemble/save/Walter.Kolczynski/hpc-stack/modulefiles/stack")
load(pathJoin("hpc", "1.2.0"))
load(pathJoin("hpc-miniconda3", "4.6.14"))
load(pathJoin("gfs_workflow", "1.0.0"))
load(pathJoin("nccmp","1.9.0.1"))

prepend_path("MODULEPATH", "/scratch2/NCEPDEV/nwprod/hpc-stack/libs/hpc-stack/modulefiles/stack")

load(pathJoin("netcdf", "4.7.4"))
load(pathJoin("wgrib2", "2.0.8"))

whatis("Description: GFS run setup environment")
