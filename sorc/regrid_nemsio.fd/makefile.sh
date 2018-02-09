#!/bin/ksh --login
set -xue

machine=${1:-cray}

source ../../modulefiles/module-setup.sh.inc
module use ../../modulefiles
module load modulefile.regrid_nemsio.$machine

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
