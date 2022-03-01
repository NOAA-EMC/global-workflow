#%Module######################################################################
##
##      FV3GFS prerequisites
##

module use /apps/contrib/NCEP/libs/hpc-stack/modulefiles/stack
module load hpc/1.1.0
module load hpc-intel/2018.4
module load hpc-impi/2018.4

#module load hpss/hpss
module load nco/4.8.1
module load gempak/7.5.1
module load ncl/6.6.2

module load grib_util/1.2.2
module load prod_util/1.2.2

module load crtm/2.3.0
setenv CRTM_FIX /apps/contrib/NCEPLIBS/orion/fix/crtm_v2.3.0

module load jasper/2.0.25
module load zlib/1.2.11
module load png/1.6.35

module load hdf5/1.10.6
module load netcdf/4.7.4
module load pio/2.5.2
module load esmf/8_2_0_beta_snapshot_14
module load fms/2021.03

module load bacio/2.4.1
module load g2/3.4.1
module load g2tmpl/1.9.1
module load ip/3.3.3
module load nemsio/2.5.2
module load sp/2.3.3
module load w3emc/2.7.3
module load w3nco/2.4.1
module load upp/10.0.8

module load wgrib/2.0.8
setenv WGRIB2 wgrib2

module load contrib
module load rocoto/1.3.3
module load slurm/19.05.3-2

# Python
module load python/3.7.5

# waveprep
module load cdo/1.9.5
