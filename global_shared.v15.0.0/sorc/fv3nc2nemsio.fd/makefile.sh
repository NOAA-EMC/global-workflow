#!/bin/ksh
set -x

machine=${1:-"cray"}

if [ $machine = "cray" ]; then

    . $MODULESHOME/init/sh
    module unload NetCDF-intel-haswell/3.6.3
    module load NetCDF-intel-haswell/4.2
    module load HDF5-serial-intel-haswell/1.8.9
    module load bacio-intel/2.0.1
    module load nemsio-intel/2.2.2
    module load w3nco-intel/2.0.6

    FCMP="ftn"
    FFLAGS='-free -O3 -xHOST'

elif [ $machine = "theia" ]; then

    . $MODULESHOME/init/sh
    module load intel/14.0.2
    module load netcdf/4.3.0
    module load hdf5/1.8.14
    module use -a /scratch3/NCEPDEV/nwprod/lib/modulefiles
    module load bacio/v2.0.1
    module load nemsio/v2.2.0
    module load w3nco/v2.0.6

    FCMP="ifort"
    FFLAGS='-g -O2 -traceback'

else

    echo "machine $machine is unsupported, ABORT!"
    exit 1

fi

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
