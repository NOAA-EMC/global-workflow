#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

source ../modulefiles/modulefile.regrid_nemsio.$target             > /dev/null 2>&1

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

cd ./regrid_nemsio.fd

export F90=${FCMP}
export LD=${FCMP}
export F77=${FCMP}

export FCFFLAGS="" # "-convert native -assume byterecl -heap-arrays -mcmodel=large -shared-intel"
export LDFLAGS="${FCFFLAGS}"
export OPTIMIZATION="-O3 -xHOST" #-axCORE-AVX2,AVX -xSSE4.2 -O3
export DEBUG="-traceback -g" #-O0 #-C #-fp-stack-check #-check all -fp-stack-check

if [ $target != hera ]; then
  LIBnetcdf=`$NETCDF/bin/nf-config --flibs`
  INCnetcdf=`$NETCDF/bin/nf-config --fflags`
  export NETCDF_LDFLAGS=$LIBnetcdf
  export NETCDF_INCLUDE=$INCnetcdf
fi

make -f Makefile clean
make -f Makefile
make -f Makefile install
make -f Makefile clean

exit
