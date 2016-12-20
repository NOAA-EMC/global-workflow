#!/bin/ksh
set -x

#-----------------------------------------------------
#-use standard module.
#-----------------------------------------------------

export FCMP=${FCMP:-ifort}

##export DEBUG='-ftrapuv -check all -check nooutput_conversion -fp-stack-check -fstack-protector -traceback -g'
export INCS="-I$SFCIO_INC4 -I$W3EMC_INCd -I$NEMSIO_INC -I$NEMSIOGFS_INC"
export FFLAGS="$INCS -O3 -r8 -convert big_endian -traceback -g"
export OMPFLAG=-openmp
export LDFLG=-openmp

export LIBSM="${NEMSIOGFS_LIB} \
              ${NEMSIO_LIB} \
              ${W3EMC_LIBd} \
              ${SFCIO_LIB4} \
              ${W3NCO_LIBd} \
              ${BACIO_LIB4} \
              ${SP_LIBd} "

make -f Makefile clean
make -f Makefile
make -f Makefile install
make -f Makefile clean
