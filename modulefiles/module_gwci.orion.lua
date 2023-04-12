help([[
Load environment to run GFS workflow ci scripts on Orion
]])

-- Temporary until official hpc-stack is updated

prepend_path("MODULEPATH", "/apps/modulefiles/core")
load(pathJoin("contrib","0.1"))
load(pathJoin("nccmp","1.8.7"))
load(pathJoin("wgrib2","3.0.2"))

load(pathJoin("hpc", "1.2.0"))
load(pathJoin("hpc-miniconda3", "4.6.14"))
load(pathJoin("gfs_workflow", "1.0.0"))

whatis("Description: GFS run ci top-level sripts environment")
