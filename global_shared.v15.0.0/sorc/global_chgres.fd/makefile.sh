#!/bin/ksh
set -x


export FCMP=${1:-ftn}
export FCMP95=$FCMP

export W3NCO_VER=v2.0.6
export W3NCO_DIR=../../NCEPlibs/w3nco_$W3NCO_VER
export W3NCO_LIBd=w3nco_d

export W3EMC_VER=v2.0.5
export W3EMC_DIR=../../NCEPlibs/w3emc_$W3EMC_VER
export W3EMC_LIBd=w3emc_d

export SP_VER=v2.0.2
export SP_DIR=../../NCEPlibs/sp_$SP_VER
export SP_LIBd=sp_${SP_VER}_d

export IP_VER=v2.0.0
export IP_DIR=../../NCEPlibs/ip_$IP_VER
export IP_LIBd=ip_d

export SFCIO_VER=v1.0.0
export SFCIO_DIR=../../NCEPlibs/sfcio_$SFCIO_VER
export SFCIO_LIB4=sfcio_${SFCIO_VER}_4
export SFCIO_INC4=${SFCIO_DIR}/incmod

export SIGIO_VER=v2.0.1
export SIGIO_DIR=../../NCEPlibs/sigio_$SIGIO_VER/sigio_$SIGIO_VER/lib
export SIGIO_LIB4=sigio
export SIGIO_INC4=../../NCEPlibs/sigio_$SIGIO_VER/sigio_$SIGIO_VER/include

export GFSIO_VER=v1.1.0
export GFSIO_DIR=../../NCEPlibs/gfsio_$GFSIO_VER
export GFSIO_LIB4=gfsio_${GFSIO_VER}_4
export GFSIO_INC4=${GFSIO_DIR}/incmod

export NEMSIO_VER=v2.2.1
export NEMSIO_DIR=../../NCEPlibs/nemsio_$NEMSIO_VER
export NEMSIO_LIB=nemsio
export NEMSIO_INC=${NEMSIO_DIR}/incmod

export LANDSFCUTIL_VER=v2.0.0
export LANDSFCUTIL_DIR=../../NCEPlibs/landsfcutil_$LANDSFCUTIL_VER
export LANDSFCUTIL_LIBd=landsfcutil_d
export LANDSFCUTIL_INCd=${LANDSFCUTIL_DIR}/incmod

export BACIO_VER=v2.0.1
export BACIO_DIR=../../NCEPlibs/bacio_$BACIO_VER
export BACIO_LIB4=bacio_4
 export FFLAGSM="-i4 -O3 -g -r8  -convert big_endian  -fp-model precise"
 export FFLAGS2M="-i4 -O3 -g -r8 -convert big_endian -fp-model precise -FR"
 export RECURS=
 export LDFLAGSM="-openmp -auto"
 export OMPFLAGM="-openmp -auto"

make -f Makefile clean
make -f Makefile
make -f Makefile install
make -f Makefile clean
