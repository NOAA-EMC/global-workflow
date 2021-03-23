#%Module#####################################################
## gaussian_sfcanl build module for Hera
#############################################################

module use -a /scratch2/NCEPDEV/nwprod/NCEPLIBS/modulefiles
module load w3nco/2.0.6
module load bacio/2.0.2
module load nemsio/2.2.3
module load sp/2.0.2
module load hdf5_parallel/1.10.6
module load netcdf_parallel/4.7.4


export NETCDF_INCLUDE="-I${NETCDF}/include"
export NETCDF_LDFLAGS_F="-L${NETCDF}/lib -lnetcdf -lnetcdff -L${HDF5}/lib -lhdf5 -lhdf5_fortran"

#export FCOMP=$FCOMP
