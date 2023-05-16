help([[
Load environment to run GFS on S4
]])

load("license_intel")
prepend_path("MODULEPATH", "/data/prod/hpc-stack/modulefiles/stack")

load(pathJoin("hpc", os.getenv("hpc_ver")))
load(pathJoin("hpc-intel", os.getenv("hpc_intel_ver")))
load(pathJoin("hpc-impi", os.getenv("hpc_impi_ver")))

load(pathJoin("miniconda", os.getenv("miniconda_ver")))
load(pathJoin("ncl", os.getenv("ncl_ver")))
load(pathJoin("cdo", os.getenv("cdo_ver")))
load(pathJoin("jasper", os.getenv("jasper_ver")))
load(pathJoin("zlib", os.getenv("zlib_ver")))
load(pathJoin("png", os.getenv("libpng_ver")))

load(pathJoin("hdf5", os.getenv("hdf5_ver")))
load(pathJoin("netcdf", os.getenv("netcdf_ver")))

load(pathJoin("nco", os.getenv("nco_ver")))
load(pathJoin("prod_util", os.getenv("prod_util_ver")))
load(pathJoin("grib_util", os.getenv("grib_util_ver")))
load(pathJoin("g2tmpl", os.getenv("g2tmpl_ver")))
load(pathJoin("ncdiag", os.getenv("ncdiag_ver")))
load(pathJoin("crtm", os.getenv("crtm_ver")))
load(pathJoin("wgrib2", os.getenv("wgrib2_ver")))
setenv("WGRIB2","wgrib2")

--prepend_path("MODULEPATH", pathJoin("/data/prod/glopara/git/prepobs/v" .. os.getenv("prepobs_run_ver"), "modulefiles"))
prepend_path("MODULEPATH", pathJoin("/data/prod/glopara/git/prepobs/feature-GFSv17_com_reorg/modulefiles"))
load(pathJoin("prepobs", os.getenv("prepobs_run_ver")))

prepend_path("MODULEPATH", pathJoin("/data/prod/glopara/git/Fit2Obs/v1.0.0/modulefiles"))
load(pathJoin("fit2obs", "1.0.0"))

whatis("Description: GFS run environment")
