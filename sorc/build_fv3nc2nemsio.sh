#!/bin/sh
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

source ../modulefiles/modulefile.fv3nc2nemsio.$target             > /dev/null 2>&1

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

cd ./fv3nc2nemsio.fd

LIBnetcdf=`$NETCDF/bin/nf-config --flibs`
INCnetcdf=`$NETCDF/bin/nf-config --fflags`
export NETCDF_LDFLAGS=$LIBnetcdf
export NETCDF_INCLUDE=$INCnetcdf

$FCMP $FFLAGS -c kinds.f90
$FCMP $FFLAGS -c constants.f90
$FCMP $FFLAGS $NETCDF_INCLUDE -I $NEMSIO_INC -c fv3_module.f90
$FCMP $FFLAGS $NETCDF_INCLUDE -I $NEMSIO_INC -I. -o fv3nc2nemsio.x fv3_main.f90 fv3_module.o $NETCDF_LDFLAGS $NEMSIO_LIB $BACIO_LIB4 $W3NCO_LIBd -L$HDF5/lib -lhdf5_hl -lhdf5 -lz

rm -f *.o *.mod

cd $cwd
mv fv3nc2nemsio.fd/fv3nc2nemsio.x ../exec/.

exit
