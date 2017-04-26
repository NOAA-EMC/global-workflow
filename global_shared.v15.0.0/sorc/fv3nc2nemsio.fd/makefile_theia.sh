#!/bin/ksh
set -x

. $MODULESHOME/init/sh

module load netcdf/4.3.0
module load hdf5/1.8.14
module load bacio/v2.0.1
module load nemsio/v2.2.0
module load w3nco/v2.0.6

FFLAGS='-g -O2 -traceback'
FCMP='ifort'

NETCDF_INC=$NETCDF/include
NETCDF_LDFLAGS_F="-L $NETCDF/lib -lnetcdff -lnetcdf"

$FCMP $FFLAGS -c kinds.f90
$FCMP $FFLAGS -c constants.f90
$FCMP $FFLAGS -I $NETCDF_INC -I $NEMSIO_INC -c fv3_module.f90
$FCMP $FFLAGS -I $NETCDF_INC -I $NEMSIO_INC -I. -o fv3nc2nemsio.x fv3_main.f90 fv3_module.o $NETCDF_LDFLAGS_F -lz $NEMSIO_LIB $BACIO_LIB4 $W3NCO_LIBd 

mv fv3nc2nemsio.x ../../exec/.
rm -f *.o *.mod


