#!/bin/ksh
set -x

#-----------------------------------------------------
#-use standard modules.
#-----------------------------------------------------

export FCMP=${FCMP:-ifort}
export FCMP95=$FCMP

export FFLAGSM="-i4 -O3 -r8  -convert big_endian -fp-model precise"
export RECURS=
#export LDFLAGSM="-openmp -auto"
#export OMPFLAGM="-openmp -auto"

export INCS="-I${NEMSIO_INC}"

export LIBSM="${NEMSIO_LIB} \
              ${W3EMC_LIBd} \
              ${W3NCO_LIBd} \
              ${BACIO_LIB4} "


make -f Makefile clobber
make -f Makefile
make -f Makefile install
make -f Makefile clean
