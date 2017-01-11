#!/bin/ksh
set -x
export FCMP=${1:-mpiifort}
export FCMP95=${2:-$FCMP}
export LIBDIR=${3:-${LIBDIR:-/nwprod/lib}}
mac=$(hostname | cut -c1-1)

if [ $mac = f ] ; then  #For Zeus
  export LIBDIR=/contrib/nceplibs/nwprod/lib
  export INCMOD="-I $LIBDIR/incmod/sigio_4 -I $LIBDIR/incmod/w3emc_v2.0.3_4"
  export LIBSMOD="-L$LIBDIR -lw3emc_v2.0.3_4 -lw3nco_4 -lbacio_4 -lsp_4 -lbufr_4_64 -lsigio_4"
  export FCMP="ifort -lmpi"
else
  SIGIO_VER=v2.0.1
  W3NCO_VER=v2.0.6
  W3EMC_VER=v2.0.3
  BUFR_VER=v10.2.5
  BACIO_VER=v2.0.1
  SP_VER=v2.0.2
  NEMSIO_VER=v2.2.1
  export INCMOD="-I $LIBDIR/incmod/nemsio_${NEMSIO_VER} -I $LIBDIR/incmod/sigio_${SIGIO_VER}_4 -I $LIBDIR/incmod/w3emc_${W3EMC_VER}_4"
  export LIBSMOD="-L$LIBDIR -lnemsio_${NEMSIO_VER} -lsigio_${SIGIO_VER}_4 -lw3emc_${W3EMC_VER}_4 -lw3nco_${W3NCO_VER}_4 -lbufr_${BUFR_VER}_4_64 -lbacio_${BACIO_VER}_4 -lsp_${SP_VER}_4"
fi

export FFLAGSM="-O2 -g -traceback -openmp -convert big_endian -auto"
make -f Makefile
