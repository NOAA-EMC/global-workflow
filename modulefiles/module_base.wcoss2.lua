help([[
Load environment to run GFS on WCOSS2
]])

PrgEnv_intel_ver=os.getenv("PrgEnv_intel_ver")
craype_ver=os.getenv("craype_ver")
intel_ver=os.getenv("intel_ver")
cray_mpich_ver=os.getenv("cray_mpich_ver")
cray_pals_ver=os.getenv("cray_pals_ver")
esmf_ver=os.getenv("esmf_ver")
cfp_ver=os.getenv("cfp_ver")

python_ver=os.getenv("python_ver")
prod_envir_ver=os.getenv("prod_envir_ver")
gempak_ver=os.getenv("gempak_ver")
perl_ver=os.getenv("perl_ver")
libjpeg_ver=os.getenv("libjpeg_ver")

cdo_ver=os.getenv("cdo_ver")

hdf5_ver=os.getenv("hdf5_ver")
netcdf_ver=os.getenv("netcdf_ver")

nco_ver=os.getenv("nco_ver")
prod_util_ver=os.getenv("prod_util_ver")
grib_util_ver=os.getenv("grib_util_ver")
bufr_dump_ver=os.getenv("bufr_dump_ver")
util_shared_ver=os.getenv("util_shared_ver")
crtm_ver=os.getenv("crtm_ver")
g2tmpl_ver=os.getenv("g2tmpl_ver")
wgrib2_ver=os.getenv("wgrib2_ver")

load(pathJoin("PrgEnv-intel", PrgEnv_intel_ver))
load(pathJoin("craype", craype_ver))
load(pathJoin("intel", intel_ver))
load(pathJoin("cray-mpich", cray_mpich_ver))
load(pathJoin("cray-pals", cray_pals_ver))
load(pathJoin("esmf", esmf_ver))
load(pathJoin("cfp", cfp_ver))
setenv("USE_CFP","YES")

load(pathJoin("python", python_ver))
load(pathJoin("prod_envir", prod_envir_ver))
load(pathJoin("gempak", gempak_ver))
load(pathJoin("perl", perl_ver))
load(pathJoin("libjpeg", libjpeg_ver))

load(pathJoin("cdo", cdo_ver))

load(pathJoin("hdf5", hdf5_ver))
load(pathJoin("netcdf", netcdf_ver))

load(pathJoin("nco", nco_ver))
load(pathJoin("prod_util", prod_util_ver))
load(pathJoin("grib_util", grib_util_ver))
load(pathJoin("bufr_dump", bufr_dump_ver))
load(pathJoin("util_shared", util_shared_ver))
load(pathJoin("crtm", crtm_ver))
load(pathJoin("g2tmpl", g2tmpl_ver))
load(pathJoin("wgrib2", wgrib2_ver))

whatis("Description: GFS run environment")
