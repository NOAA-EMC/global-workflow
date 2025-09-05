help([[
Load environment to run GFS workflow ci scripts on WCOSS2
]])

prepend_path("MODULEPATH", "/apps/ops/test/nco/modulefiles/core")
load(pathJoin("rocoto","1.3.5"))
load(pathJoin("PrgEnv-intel"))
load(pathJoin("intel","19.1.3.304"))
load(pathJoin("python", "3.8.6"))

whatis("Description: GFS run setup environment")
