#%Module######################################################################
##
##      FV3GFS prerequisites
##

module load intel/2018.4
module load impi/2018.4
module load wgrib/2.0.8
#module load hpss/hpss
module load nco/4.8.1
module load cdo/1.9.5

module load gempak/7.5.1

#Load from official NCEPLIBS
module use /apps/contrib/NCEPLIBS/orion/modulefiles
module load grib_util/1.2.0
module load prod_util/1.2.0
module load g2tmpl/1.6.0
module load crtm/2.3.0
#module load netcdf_parallel/4.7.4
module load hdf5_parallel/1.10.6

## load ESMF library for above compiler / MPI combination
### use pre-compiled EMSF library for above compiler / MPI combination
###
module use /apps/contrib/NCEPLIBS/lib/modulefiles
module load netcdfp/4.7.4 
#module load hdf5_parallel/1.10.6
module load esmflocal/8.0.1.08bs
module load post-intel-sandybridge/8.0.5

module load contrib
module load rocoto/1.3.2
module load slurm/19.05.3-2

# Python
module load python/3.7.5

