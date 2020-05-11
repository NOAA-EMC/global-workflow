#%Module#####################################################
## Module file for regrid_nemsio
#############################################################

module load intel/2018.4
module load impi/2018.4

module use -a /apps/contrib/NCEPLIBS/orion/modulefiles
module load hdf5_parallel/1.10.6
module load netcdf_parallel/4.7.4
module load bacio/2.0.3
module load w3nco/2.0.7
module load nemsio/2.2.4
module load sp/2.0.3

export FCMP="$FCOMP" 
export NETCDF_LDFLAGS_F="-L${NETCDF}/lib -lnetcdf -lnetcdff -lnetcdf  -L${HDF5_ROOT}/lib -lhdf5 -lhdf5_hl -lhdf5 -lz"
export NETCDF_LDFLAGS="-L${NETCDF}/lib -lnetcdf -lnetcdff -lnetcdf  -L${HDF5_ROOT}/lib -lhdf5 -lhdf5_hl -lhdf5 -lz"
export NETCDF_INCLUDE=-I$NETCDF/include
#export FCMP="mpif90 -f90=ifort"
