#! /usr/bin/env bash
set -eux

source ./machine-setup.sh > /dev/null 2>&1
cwd=`pwd`

USE_PREINST_LIBS=${USE_PREINST_LIBS:-"true"}
if [ $USE_PREINST_LIBS = true ]; then
  export MOD_PATH=/scratch3/NCEPDEV/nwprod/lib/modulefiles
  source ../modulefiles/fv3gfs/global_chgres.$target             > /dev/null 2>&1
else
  export MOD_PATH=${cwd}/lib/modulefiles
  if [ $target = wcoss_cray ]; then
    source ../modulefiles/fv3gfs/global_chgres.${target}_userlib > /dev/null 2>&1
  else
    source ../modulefiles/fv3gfs/global_chgres.$target           > /dev/null 2>&1
  fi
fi

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

#
# --- Chgres part
#
cd global_chgres.fd

export FCMP=${FCMP:-ifort}
export FCMP95=$FCMP

export FFLAGSM="-i4 -O3 -r8  -convert big_endian -fp-model precise"
export RECURS=
export LDFLAGSM=${LDFLAGSM:-"-openmp -auto"}
export OMPFLAGM=${OMPFLAGM:-"-openmp -auto"}

export INCS="-I${SIGIO_INC4} -I${SFCIO_INC4} -I${LANDSFCUTIL_INCd} \
             -I${NEMSIO_INC} -I${NEMSIOGFS_INC} -I${GFSIO_INC4} -I${IP_INCd} ${NETCDF_INCLUDE}"

export LIBSM="${GFSIO_LIB4} \
              ${NEMSIOGFS_LIB} \
              ${NEMSIO_LIB} \
              ${SIGIO_LIB4} \
              ${SFCIO_LIB4} \
              ${LANDSFCUTIL_LIBd} \
              ${IP_LIBd} \
              ${SP_LIBd} \
              ${W3EMC_LIBd} \
              ${W3NCO_LIBd} \
              ${BACIO_LIB4} \
              ${NETCDF_LDFLAGS_F}"

make -f Makefile clobber
make -f Makefile
make -f Makefile install
make -f Makefile clobber

exit
