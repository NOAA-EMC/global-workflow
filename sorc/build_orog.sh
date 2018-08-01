#!/bin/sh
#####################################################################################
# orog using module compile standard
# 10/10/2016 Fanglin.Yang@noaa.gov:    Create module load version
#####################################################################################
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

USE_PREINST_LIBS=${USE_PREINST_LIBS:-"true"}
if [ $USE_PREINST_LIBS = true ]; then
  export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
  source ../modulefiles/fv3gfs/orog.$target             > /dev/null 2>&1
else
  export MOD_PATH=${cwd}/lib/modulefiles
  if [ $target = wcoss_cray ]; then
    source ../modulefiles/fv3gfs/orog.${target}_userlib > /dev/null 2>&1
  else
    source ../modulefiles/fv3gfs/orog.$target           > /dev/null 2>&1
  fi
fi

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

cd ./orog.fd

if [ $target = wcoss_cray ]; then
 export LIBSM="${BACIO_LIB4} ${IP_LIBd} ${W3NCO_LIBd} ${SP_LIBd}"
 export FFLAGSM="-O3 -g -traceback -r8  -convert big_endian -fp-model precise  -assume byterecl"
elif [ $target = wcoss_dell_p3 ]; then
 INCS="${NETCDF_INCLUDE}"
 export LIBSM="${BACIO_LIB4} ${W3NCO_LIBd} ${IP_LIBd} ${SP_LIBd} ${NETCDF_LDFLAGS}"
 export FFLAGSM="-O3 -g -traceback -r8  -convert big_endian -fp-model precise  -assume byterecl ${INCS}"
elif [ $target = wcoss ]; then
 INCS="${NETCDF_INCLUDE}"
 export LIBSM="${BACIO_LIB4} ${W3NCO_LIBd} ${IP_LIBd} ${SP_LIBd} ${NETCDF_LDFLAGS}"
 export FFLAGSM="-O3 -g -traceback -r8  -convert big_endian -fp-model precise  -assume byterecl ${INCS}"
elif [ $target = theia ]; then
 INCS="-I${NETCDF}/include"
 export LIBSM="${BACIO_LIB4} ${W3NCO_LIBd} ${IP_LIBd} ${SP_LIBd} -L${NETCDF}/lib -lnetcdff -lnetcdf"
 export FFLAGSM="-O3 -g -traceback -r8  -convert big_endian -fp-model precise  -assume byterecl ${INCS}"
else
 echo machine $target not found
 exit 1
fi

export FCMP=${FCMP:-ifort}
export FCMP95=$FCMP

export LDFLAGSM="-qopenmp -auto"
export OMPFLAGM="-qopenmp -auto"

make -f Makefile clobber
make -f Makefile
make -f Makefile install
make -f Makefile clobber

exit
