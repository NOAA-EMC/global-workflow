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
    module load w3emc-intel/2.2.0
    module load w3nco-intel/2.0.6

    export FCMP="ftn"

elif [ $machine = "theia" ]; then

    . $MODULESHOME/init/sh
    module load intel/14.0.2
    module load netcdf/4.3.0
    module load hdf5/1.8.14
    module use -a /scratch3/NCEPDEV/nwprod/lib/modulefiles
    module load bacio/v2.0.1
    module load nemsio/v2.2.2
    module load w3emc/v2.2.0
    module load w3nco/v2.0.6

    export FCMP="ifort"

else

    echo "machine $machine is unsupported, ABORT!"
    exit 1

fi

export F90=${FCMP}
export LD=${FCMP}
export F77=${FCMP}

export FCFFLAGS="" # "-convert native -assume byterecl -heap-arrays -mcmodel=large -shared-intel"
export LDFLAGS="${FCFFLAGS}"
export OPTIMIZATION="-O3 -xHOST" #-axCORE-AVX2,AVX -xSSE4.2 -O3
export DEBUG="-traceback -g" #-O0 #-C #-fp-stack-check #-check all -fp-stack-check
#export DEBUG="-g -O0 -check all -ftrapuv -fp-stack-check -fstack-protector -heap-arrays -recursive -traceback"


LIBnetcdf=`$NETCDF/bin/nf-config --flibs`
INCnetcdf=`$NETCDF/bin/nf-config --fflags`
export NETCDF_LDFLAGS=$LIBnetcdf
export NETCDF_INCLUDE=$INCnetcdf

make -f Makefile clean
make -f Makefile
make -f Makefile install
make -f Makefile clean

exit 0
