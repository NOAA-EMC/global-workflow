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
             -I${NETCDF_INCLUDES}"

export FFLAGSM="-O3 -free -convert big_endian -traceback -qopenmp -fp-model precise -assume byterecl ${INCS}"
export FFLAGSM2="-O3 -free -r8"

export LIBSM="${LANDSFCUTIL_LIBd} \
              ${IP_LIBd} \
              ${SP_LIBd} \
              ${W3NCO_LIB4} \
              ${BACIO_LIB4} \
              -L${NETCDF_LIBRARIES} -lnetcdff -lnetcdf -L${HDF5_LIBRARIES} -lhdf5_hl -lhdf5 -L${ZLIB_LIBRARIES} -lz -ldl -lm"

WGRIB2_ROOT=${WGRIB2_ROOT:-$wgrib2_ROOT}
WGRIB2_LIB=${WGRIB2_LIB:-${WGRIB_LIB:-"$WGRIB2_ROOT/lib/libwgrib2.a"}}
WGRIB2_LIBAPI=${WGRIB2_LIBAPI:-"$WGRIB2_ROOT/lib/libwgrib2_api.a"}
WGRIB2_INC=${WGRIB2_INC:-"$WGRIB2_ROOT/include"}
JASPER_LIB=${JASPER_LIB:-"$JASPER_LIBRARIES/libjasper.a"}
export LIB2="${WGRIB2_LIBAPI} ${WGRIB2_LIB} ${JASPER_LIB}"
export MOD2="-I${WGRIB2_INC}"

cd reg2grb2.fd

pwd

make -f Makefile clean
make -f Makefile
make -f Makefile install
