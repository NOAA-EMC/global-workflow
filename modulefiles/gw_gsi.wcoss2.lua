help([[
Load environment to run the GSI on WCOSS2
]])

load(pathJoin("PrgEnv-intel", (os.getenv("PrgEnv_intel_ver") or "None")))
load(pathJoin("craype", (os.getenv("craype_ver") or "None")))
load(pathJoin("intel", (os.getenv("intel_ver") or "None")))
load(pathJoin("cray-mpich", (os.getenv("cray_mpich_ver") or "None")))
load(pathJoin("cray-pals", (os.getenv("cray_pals_ver") or "None")))
load(pathJoin("cfp", (os.getenv("cfp_ver") or "None")))
setenv("USE_CFP","YES")

load(pathJoin("python", (os.getenv("python_ver") or "None")))
prepend_path("MODULEPATH", "/apps/dev/modulefiles")
load(pathJoin("ve","gw", (os.getenv("gw_ve_ver") or "None")))

load(pathJoin("hdf5-D", (os.getenv("hdf5_ver") or "None")))
load(pathJoin("pnetcdf-D", (os.getenv("pnetcdf_ver") or "None")))
load(pathJoin("netcdf-D", (os.getenv("netcdf_ver") or "None")))

load(pathJoin("prod_util", (os.getenv("prod_util_ver") or "None")))
load(pathJoin("ncdiag-A", (os.getenv("ncdiag_ver") or "None")))
load(pathJoin("crtm", (os.getenv("crtm_ver") or "None")))

-- Perl is needed by the GSI monitor
load(pathJoin("perl", (os.getenv("perl_ver") or "None")))

whatis("Description: GFS run environment")
