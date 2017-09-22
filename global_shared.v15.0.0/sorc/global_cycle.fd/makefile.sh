#!/bin/ksh
set -x

#-----------------------------------------------------
#-use standard module.
#-----------------------------------------------------

export FCMP=${FCMP:-ifort}

##export DEBUG='-ftrapuv -check all -check nooutput_conversion -fp-stack-check -fstack-protector -traceback -g'
export INCS="-I$SFCIO_INC4 -I$W3EMC_INCd -I$IP_INCd ${NETCDF_INCLUDE}"
export FFLAGS="$INCS -O3 -r8 -convert big_endian -traceback -g"
export OMPFLAG=-openmp
export LDFLG=-openmp

export LIBSM="${W3EMC_LIBd} \
              ${SFCIO_LIB4} \
              ${W3NCO_LIBd} \
              ${BACIO_LIB4} \
              ${IP_LIBd} \
              ${SP_LIBd} ${NETCDF_LDFLAGS_F}"

make -f Makefile clean
make -f Makefile
make -f Makefile install
make -f Makefile clean
