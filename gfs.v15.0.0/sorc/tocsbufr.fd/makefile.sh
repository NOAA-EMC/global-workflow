#!/bin/ksh
set -x
export FCMP=${1:-ifort}
export FCMP95=${2:-$FCMP}
export LIBDIR=${3:-${LIBDIR:-/nwprod/lib}}
mac=$(hostname | cut -c1-1)

if [ $mac = f ] ; then  #For Zeus
  export LIBDIR=/contrib/nceplibs/nwprod/lib
  export LIBSMOD="-L$LIBDIR -lw3nco_4 -lbacio_4 -lbufr_4_64"
else
#  export INCMOD="-I -I${SIGIO_INC4} -I ${W3EMC_INC4}"
  export INCMOD="-I ${SIGIO_INC4} ${W3EMC_INC4}"
  export LIBSMOD="${W3EMC_LIB4} ${W3NCO_LIB4} ${BUFR_LIB4} ${BACIO_LIB4} ${SP_LIB4} ${SIGIO_LIB4}" 
fi

export FFLAGSM="-O2"
make -f Makefile
