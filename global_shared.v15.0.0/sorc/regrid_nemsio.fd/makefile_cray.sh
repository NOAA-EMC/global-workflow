#!/bin/ksh
set -x

. $MODULESHOME/init/sh
module load NetCDF-intel-haswell/4.2
module load HDF5-serial-intel-haswell/1.8.9
module load bacio-intel/2.0.1
module load nemsio-intel/2.2.2
module load w3emc-intel/2.2.0 
module load w3nco-intel/2.0.6 
module load sp-intel/2.0.2   

export F90=ftn
export LD=ftn  
export F77=ftn

#expirt FCFFLAGS="-convert native -assume byterecl -heap-arrays -mcmodel=large -shared-intel"
export FCFFLAGS=
export LDFLAGS="${FCFFLAGS}"
#export OPTIMIZATION="-O3 -xHOST"
export OPTIMIZATION="-O3"
#export DEBUG="-g -O0 -check all -ftrapuv -fp-stack-check -fstack-protector -heap-arrays -recursive -traceback"


export INCnemsio="-I$NEMSIO_INC" 
export LIBnetcdf="$NETCDF/lib/libnetcdf.a"
export INCnetcdf="$NETCDF_INCLUDE"
export INSTALL_DIR="`pwd`/../../exec"

make -f Makefile_cray clean
make -f Makefile_cray
make -f Makefile_cray install
make -f Makefile_cray clean

