#%Module#####################################################
## gaussian_sfcanl build module for Orion
#############################################################

module load intel/2018.4
module load impi/2018.4

module use -a /apps/contrib/NCEPLIBS/orion/modulefiles
module load w3nco/2.0.7
module load bacio/2.0.3
module load nemsio/2.2.4
module load sp/2.0.3
module load hdf5_parallel/1.10.6
module load netcdf_parallel/4.7.4

export NETCDF_INCLUDE="-I${NETCDF}/include"
export NETCDF_LDFLAGS_F="-L${NETCDF}/lib -lnetcdf -lnetcdff -L${HDF5_LIBRARY_DIRS}/lib -lhdf5 -lhdf5_fortran"

#export FCOMP=$FCOMP
