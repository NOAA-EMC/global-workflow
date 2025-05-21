help([[
Load environment to run GFS on Ursa
]])

prepend_path("MODULEPATH", "/scratch3/NCEPDEV/nems/role.epic/spack-stack/spack-stack-1.6.0/envs/gsi-addon-dev-fms-2024.01/install/modulefiles/Core")

-- load(pathJoin("hpss", (os.getenv("hpss_ver") or "None")))

load(pathJoin("stack-intel", (os.getenv("stack_intel_ver") or "2021.5.0")))
load(pathJoin("stack-intel-oneapi-mpi", (os.getenv("stack_impi_ver") or "2021.5.1")))
load(pathJoin("python", (os.getenv("python_ver") or "3.11.6")))
load(pathJoin("prod_util", (os.getenv("prod_util_ver") or "2.1.1")))

setenv("WGRIB2","wgrib2")
setenv("WGRIB","wgrib")
-- setenv("UTILROOT",(os.getenv("prod_util_ROOT") or "None"))

whatis("Description: GFS run host environment")

