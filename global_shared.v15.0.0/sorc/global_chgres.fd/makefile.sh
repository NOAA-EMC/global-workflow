#!/bin/ksh
set -x

#-----------------------------------------------------
#-use standard modules.
#-----------------------------------------------------

export FCMP=${FCMP:-ifort}
export FCMP95=$FCMP

export FFLAGSM="-i4 -O3 -r8  -convert big_endian -fp-model precise"
export RECURS=
export LDFLAGSM="-openmp -auto"
export OMPFLAGM="-openmp -auto"

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
make -f Makefile clean
