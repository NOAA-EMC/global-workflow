help([[
Load environment to run GFS on Jet
]])

prepend_path("MODULEPATH", "/lfs4/HFIP/hfv3gfs/role.epic/hpc-stack/libs/intel-18.0.5.274/modulefiles/stack")

load(pathJoin("hpc", "1.2.0"))
load(pathJoin("hpc-intel", "18.0.5.274"))
load(pathJoin("hpc-impi", "2018.4.274"))
load(pathJoin("cmake", "3.20.1"))

load("hpss")
load(pathJoin("gempak", "7.4.2"))
load(pathJoin("ncl", "6.6.2"))
load(pathJoin("jasper", "2.0.25"))
load(pathJoin("libpng", "1.6.35"))
load(pathJoin("cdo", "1.9.5"))
load(pathJoin("R", "4.0.2"))

load(pathJoin("hdf5", "1.10.6"))
load(pathJoin("netcdf", "4.7.4"))

load(pathJoin("nco", "4.9.1"))
load(pathJoin("prod_util", "1.2.2"))
load(pathJoin("grib_util", "1.2.2"))
load(pathJoin("g2tmpl", "1.10.0"))
load(pathJoin("ncdiag", "1.0.0"))
load(pathJoin("crtm", "2.4.0"))
load(pathJoin("wgrib2", "2.0.8"))
setenv("WGRIB2","wgrib2")

prepend_path("MODULEPATH", pathJoin("/lfs4/HFIP/hfv3gfs/glopara/git/prepobs/v1.0.1/modulefiles"))
load(pathJoin("prepobs", "1.0.1"))

prepend_path("MODULEPATH", "/contrib/anaconda/modulefiles")
load(pathJoin("anaconda", "5.3.1"))

prepend_path("MODULEPATH", pathJoin("/lfs4/HFIP/hfv3gfs/glopara/git/prepobs/feature-GFSv17_com_reorg_log_update/modulefiles"))
load(pathJoin("prepobs", "1.0.1"))
prepend_path("MODULEPATH", pathJoin("/lfs4/HFIP/hfv3gfs/glopara/git/Fit2Obs/v1.0.0/modulefiles"))
load(pathJoin("fit2obs", "1.0.0"))

whatis("Description: GFS run environment")
