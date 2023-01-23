help([[
Load environment to run GFS on Jet
]])

prepend_path("MODULEPATH", "/lfs4/HFIP/hfv3gfs/role.epic/hpc-stack/libs/intel-18.0.5.274/modulefiles/stack")

load(pathJoin("hpc", "1.2.0"))
load(pathJoin("hpc-intel", "18.0.5.274"))
load(pathJoin("hpc-impi", "2018.4.274"))
load(pathJoin("cmake", "3.20.1"))

load(pathJoin("hpss"))
load(pathJoin("nco", "4.9.1"))
load(pathJoin("gempak", "7.4.2"))
load(pathJoin("ncl", "6.6.2"))

load(pathJoin("prod_util", "1.2.2"))
load(pathJoin("grib_util", "1.2.2"))

load(pathJoin("crtm", "2.3.0"))
setenv("CRTM_FIX","/lfs4/HFIP/hfv3gfs/nwprod/NCEPLIBS/fix/crtm_v2.3.0")

load(pathJoin("jasper", "2.0.25"))
load(pathJoin("zlib", "1.2.11"))
load(pathJoin("libpng", "1.6.35"))

load(pathJoin("hdf5", "1.10.6"))
load(pathJoin("netcdf", "4.7.4"))
load(pathJoin("pio", "2.5.7"))
load(pathJoin("esmf", "8.3.0b09"))
load(pathJoin("fms", "2021.03"))

load(pathJoin("bacio", "2.4.1"))
load(pathJoin("g2", "3.4.2"))
load(pathJoin("g2tmpl", "1.10.0"))
load(pathJoin("ip", "3.3.3"))
load(pathJoin("nemsio", "2.5.2"))
load(pathJoin("sp", "2.3.3"))
load(pathJoin("w3emc", "2.7.3"))
load(pathJoin("w3nco", "2.4.1"))
load(pathJoin("ncdiag", "1.0.0"))

load(pathJoin("wgrib2", "2.0.8"))
setenv("WGRIB2","wgrib2")

load(pathJoin("cdo", "1.9.5"))

load(pathJoin("R", "4.0.2"))

--TODO need to port/build prepobs
prepend_path("MODULEPATH", pathJoin("/lfs1/NESDIS/nesdis-rdo2/David.Huber/prepobs/v1.0.1/modulefiles"))
load(pathJoin("prepobs", "1.0.1"))

prepend_path("MODULEPATH", "/contrib/anaconda/modulefiles")
load(pathJoin("anaconda", "5.3.1"))

whatis("Description: GFS run environment")
