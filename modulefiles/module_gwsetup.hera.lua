help([[
Load environment to run GFS workflow setup scripts on Hera
]])

load(pathJoin("rocoto"))

-- Temporary until official hpc-stack is updated
prepend_path("MODULEPATH", "/scratch2/NCEPDEV/ensemble/save/Walter.Kolczynski/hpc-stack/modulefiles/stack")
load(pathJoin("hpc", "1.2.0"))
load(pathJoin("hpc-miniconda3", "4.6.14"))
load(pathJoin("gfs_workflow", "1.0.0"))

whatis("Description: GFS run setup environment")
