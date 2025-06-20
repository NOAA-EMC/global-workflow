help([[
Load environment to run GFS workflow ci scripts on Hercules
]])

prepend_path("MODULEPATH", "/work/noaa/epic/role-epic/spack-stack/hercules/spack-stack-1.6.0/envs/gsi-addon-env/install/modulefiles/Core")

load(pathJoin("stack-intel", os.getenv("2021.9.0")))
load(pathJoin("stack-intel-oneapi-mpi", os.getenv("2021.9.0")))

load(pathJoin("netcdf-c", os.getenv("4.9.2")))
load(pathJoin("netcdf-fortran", os.getenv("4.6.0")))
load(pathJoin("nccmp","1.9.0.1"))
load(pathJoin("wgrib2", "3.1.1"))

whatis("Description: GFS run ci top-level sripts environment")
