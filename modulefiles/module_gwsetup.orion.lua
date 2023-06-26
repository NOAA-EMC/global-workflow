help([[
Load environment to run GFS workflow ci scripts on Orion
]])

prepend_path("MODULEPATH", "/apps/modulefiles/core")
load(pathJoin("contrib","0.1"))
load(pathJoin("rocoto","1.3.3"))
load(pathJoin("git","2.28.0"))

-- Temporary until official hpc-stack is updated
prepend_path("MODULEPATH", "/work2/noaa/global/wkolczyn/save/hpc-stack/modulefiles/stack")
load(pathJoin("hpc", "1.2.0"))
load(pathJoin("hpc-miniconda3", "4.6.14"))
load(pathJoin("gfs_workflow", "1.0.0"))

whatis("Description: GFS run ci top-level sripts environment")
