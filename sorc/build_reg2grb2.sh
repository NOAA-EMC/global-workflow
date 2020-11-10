#! /usr/bin/env bash
set -x

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

source ./machine-setup.sh > /dev/null 2>&1

source ../modulefiles/modulefile.reg2grb2.$target

export FCMP=${FCMP:-ifort}
export FCMP95=$FCMP
export CPP=fpp

export INCS="-I${LANDSFCUTIL_INCd} \
             -I${IP_INCd} \
             -I${NETCDF}/include"

export FFLAGSM="-O3 -free -convert big_endian -traceback -qopenmp -fp-model precise  -assume byterecl ${INCS}"
export FFLAGSM2="-O3 -free -r8"

export LIBSM="${LANDSFCUTIL_LIBd} \
              ${IP_LIBd} \
              ${SP_LIBd} \
              ${W3NCO_LIB4} \
              ${BACIO_LIB4} \
              -L${NETCDF}/lib -lnetcdff -lnetcdf"

WGRIB2_LIB=${WGRIB2_LIB:-"$WGRIB2_ROOT/lib/wgrib2"}
WGRIB2_LIBAPI=${WGRIB2_LIBAPI:-"$WGRIB2_ROOT/lib/wgrib2_api"}
WGRIB2_INC=${WGRIB2_INC:-$WGRIB2_ROOT/lib}
export LIB2="${WGRIB2_LIB} ${WGRIB2_LIBAPI}"
export MOD2="-I${WGRIB2_INC}"

cd reg2grb2.fd

pwd

make -f Makefile clean
make -f Makefile
make -f Makefile install
