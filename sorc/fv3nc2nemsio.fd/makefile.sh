#!/bin/ksh
set -x

. $MODULESHOME/init/sh

module load NetCDF-intel-haswell/4.2
module load HDF5-serial-intel-haswell/1.8.9
module load bacio-intel/2.0.1
module load nemsio-intel/2.2.2
module load w3nco-intel/2.0.6

FFLAGS='-free -O3 -xHOST'

ftn $FFLAGS -c kinds.f90
ftn $FFLAGS -c constants.f90
ftn $FFLAGS $NETCDF_INCLUDE -I $NEMSIO_INC -c fv3_module.f90
ftn $FFLAGS $NETCDF_INCLUDE -I $NEMSIO_INC -I. -o fv3nc2nemsio.x fv3_main.f90 fv3_module.o $NETCDF_LDFLAGS_F $NETCDF_LDFLAGS_C $HDF5_LDFLAGS_C -lz $NEMSIO_LIB $BACIO_LIB4 $W3NCO_LIBd 

mv fv3nc2nemsio.x ../../exec/.
rm -f *.o *.mod


