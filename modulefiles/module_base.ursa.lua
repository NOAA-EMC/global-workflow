help([[
Load environment to run GFS on Ursa
]])

local spack_mod_path=(os.getenv("spack_mod_path") or "None")
prepend_path("MODULEPATH", spack_mod_path)

-- load(pathJoin("stack-intel", (os.getenv("stack_intel_ver") or "None")))
-- load(pathJoin("stack-intel-oneapi-mpi", (os.getenv("stack_impi_ver") or "None")))
-- load(pathJoin("python", (os.getenv("python_ver") or "None")))

load(pathJoin("hpss", (os.getenv("hpss_ver") or "None")))
load(pathJoin("gempak", (os.getenv("gempak_ver") or "None")))
load(pathJoin("prod_util", (os.getenv("prod_util_ver") or "None")))

setenv("WGRIB2","wgrib2")

-- Stop gap fix for wgrib with spack-stack 1.6.0
-- TODO Remove this when spack-stack issue #1097 is resolved
-- setenv("WGRIB","wgrib")
-- setenv("UTILROOT",(os.getenv("prod_util_ROOT") or "None"))

-- prepend_path("MODULEPATH", pathJoin("/scratch1/NCEPDEV/global/glopara/git/prepobs/v" .. (os.getenv("prepobs_run_ver") or "None"), "modulefiles"))
-- load(pathJoin("prepobs", (os.getenv("prepobs_run_ver") or "None")))

-- prepend_path("MODULEPATH", pathJoin("/scratch1/NCEPDEV/global/glopara/git/Fit2Obs/v" .. (os.getenv("fit2obs_ver") or "None"), "modulefiles"))
-- load(pathJoin("fit2obs", (os.getenv("fit2obs_ver") or "None")))

whatis("Description: GFS run environment")

