help([[
Load environment to run GFS workflow ci scripts on WCOSS2
]])

prepend_path("MODULEPATH", "/apps/ops/para/nco/modulefiles/core")
load(pathJoin("gh","2.28.0"))

whatis("Description: GFS run setup ci environment")
