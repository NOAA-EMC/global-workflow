help([[
Load environment to run GFS workflow ci scripts on Orion
]])

prepend_path("MODULEPATH", "/work/noaa/epic/role-epic/spack-stack/orion/spack-stack-1.6.0/envs/gsi-addon-env/install/modulefiles/Core")

load(pathJoin("stack-intel", os.getenv("2022.0.2")))
load(pathJoin("stack-intel-oneapi-mpi", os.getenv("2021.5.1")))

load(pathJoin("netcdf-c", os.getenv("4.9.2")))
load(pathJoin("netcdf-fortran", os.getenv("4.6.1")))
load(pathJoin("nccmp","1.9.0.1"))
load(pathJoin("wgrib2", "2.0.8"))

whatis("Description: GFS run ci top-level sripts environment")
