help([[
Load environment for run atmos_post on WCOSS2
]])

load(pathJoin("PrgEnv-intel", "8.3.3"))
load(pathJoin("craype", "2.7.13"))
load(pathJoin("intel", "19.1.3.304"))
load(pathJoin("craype", "2.7.17"))
load(pathJoin("cray-mpich", "8.1.19"))
load(pathJoin("cray-pals", "1.0.17"))
load(pathJoin("cfp", "2.0.4"))
setenv("USE_CFP","YES")

prepend_path("MODULEPATH", "/apps/ops/para/libs/modulefiles/compiler/intel/19.1.3.304")

load(pathJoin("prod_util", "2.0.9"))

load(pathJoin("wgrib2", "3.8.0"))

load(pathJoin("g2c", "2.3.0"))
load(pathJoin("libjpeg-turbo", "2.1.0"))
load(pathJoin("libaec", "1.1.3"))

load(pathJoin("netcdf-D", "4.9.2"))

setenv("WGRIB2","wgrib2")
setenv("GMERGE","gmerge")
whatis("Description: atmos_post run environment on WCOSS2")
