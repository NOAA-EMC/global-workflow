help([[
Load environment to run GFS workflow ci scripts on WCOSS2
]])

load(pathJoin("git","2.29.0"))

prepend_path("MODULEPATH", "/apps/ops/test/nco/modulefiles/core")
load(pathJoin("rocoto","1.3.5"))

whatis("Description: GFS run setup environment")
