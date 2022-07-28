help([[
Load environment to run GFS on Hera
]])

prepend_path("MODULEPATH", "/scratch2/NCEPDEV/nwprod/hpc-stack/libs/hpc-stack/modulefiles/stack")

load(pathJoin("hpc", os.getenv("hpc_ver")))
load(pathJoin("hpc-intel", os.getenv("hpc_intel_ver")))
load(pathJoin("hpc-impi", os.getenv("hpc_impi_ver")))

load(pathJoin("hpss", os.getenv("hpss_ver")))
load(pathJoin("nco", os.getenv("nco_ver")))
load(pathJoin("gempak", os.getenv("gempak_ver")))
load(pathJoin("ncl", os.getenv("ncl_ver")))

load(pathJoin("prod_util", os.getenv("prod_util_ver")))
load(pathJoin("grib_util", os.getenv("grib_util_ver")))

load(pathJoin("crtm", os.getenv("crtm_ver")))

load(pathJoin("hdf5", os.getenv("hdf5_ver")))
load(pathJoin("netcdf", os.getenv("netcdf_ver")))
load(pathJoin("pio", os.getenv("pio_ver")))
load(pathJoin("esmf", os.getenv("esmf_ver")))
load(pathJoin("fms", os.getenv("fms_ver")))

load(pathJoin("g2tmpl", os.getenv("g2tmpl_ver")))
load(pathJoin("ncdiag", os.getenv("ncdiag_ver")))
load(pathJoin("wgrib2", os.getenv("wgrib2_ver")))

load(pathJoin("cdo", os.getenv("cdo_ver")))

load(pathJoin("R", os.getenv("R_ver")))

-- Temporary until official hpc-stack is updated
prepend_path("MODULEPATH", "/scratch2/NCEPDEV/ensemble/save/Walter.Kolczynski/hpc-stack/modulefiles/stack")
load(pathJoin("hpc", "1.2.0"))
load(pathJoin("hpc-intel", "18.0.5.274"))
load(pathJoin("hpc-miniconda3", "4.6.14"))
load(pathJoin("ufswm", "1.0.0"))
load(pathJoin("met", "9.1"))
load(pathJoin("metplus", "3.1"))
--

prepend_path("MODULEPATH", pathJoin("/scratch1/NCEPDEV/global/glopara/git/prepobs/v" .. os.getenv("prepobs_run_ver"), "modulefiles"))
load(pathJoin("prepobs", os.getenv("prepobs_run_ver")))

setenv("USE_CFP","YES")

whatis("Description: GFS run environment")
