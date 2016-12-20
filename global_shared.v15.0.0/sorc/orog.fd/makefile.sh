#!/bin/ksh
set -x

#-----------------------------------------------------
#-use standard modules. 
#-----------------------------------------------------

export FCMP=${FCMP:-ifort}
export FCMP95=$FCMP

export FFOPTS="-auto -O0  -msse2 -g -traceback -r8 -openmp -convert big_endian -assume byterecl -fp-model precise -g"

export FFLAGSM="-O0 -r8  -convert big_endian -fp-model precise  -assume byterecl"
export RECURS=
export LDFLAGSM="-qopenmp -auto"
export OMPFLAGM="-qopenmp -auto"

export INCS=""

export LIBSM="${BACIO_LIB8} \
              ${W3NCO_LIB8} \
              ${W3EMC_LIB8} \
              ${IP_LIB8} \
              ${SP_LIB8} "


make -f Makefile clobber
make -f Makefile
make -f Makefile install
make -f Makefile clean
