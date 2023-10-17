help([[
Load environment to run GFS workflow setup scripts on Hera
]])

prepend_path("MODULEPATH", "/scratch1/NCEPDEV/nems/Alexander.Richert/spack-stack-1.4.1-gw/envs/gw/install/modulefiles/Core")
prepend_path("MODULEPATH", "/scratch1/NCEPDEV/jcsda/jedipara/spack-stack/modulefiles")

load(pathJoin("stack-intel", os.getenv("2021.5.0")))
load(pathJoin("stack-intel-oneapi-mpi", os.getenv("2021.5.1")))

load(pathJoin("netcdf-c", os.getenv("4.9.2")))
load(pathJoin("netcdf-fortran", os.getenv("4.6.0")))
load(pathJoin("nccmp","1.9.0.1"))
load(pathJoin("wgrib2", "2.0.8"))

whatis("Description: GFS run setup CI environment")
