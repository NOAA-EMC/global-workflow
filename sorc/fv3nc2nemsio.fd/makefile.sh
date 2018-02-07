#!/bin/ksh
set -x

machine=${1:-"cray"}

if [ $machine = "cray" ]; then
:
elif [ $machine = "theia" ]; then
:
else

    echo "machine $machine is unsupported, ABORT!"
    exit 1

fi

source ../../modulefiles/module-setup.sh.inc
module use ../../modulefiles
module load modulefile.fv3nc2nemsio.$machine

LIBnetcdf=`$NETCDF/bin/nf-config --flibs`
INCnetcdf=`$NETCDF/bin/nf-config --fflags`
export NETCDF_LDFLAGS=$LIBnetcdf
export NETCDF_INCLUDE=$INCnetcdf


$FCMP $FFLAGS -c kinds.f90
$FCMP $FFLAGS -c constants.f90
$FCMP $FFLAGS $NETCDF_INCLUDE -I $NEMSIO_INC -c fv3_module.f90
$FCMP $FFLAGS $NETCDF_INCLUDE -I $NEMSIO_INC -I. -o fv3nc2nemsio.x fv3_main.f90 fv3_module.o $NETCDF_LDFLAGS $NEMSIO_LIB $BACIO_LIB4 $W3NCO_LIBd

mv fv3nc2nemsio.x ../../exec/.
rm -f *.o *.mod

exit 0
