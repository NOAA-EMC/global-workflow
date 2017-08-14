#!/bin/ksh
set -x

machine=${machine:-WCOSS}

if [ $machine = WCOSS ] ; then
 CF=ifort
 export MP_CORE_FILE_FORMAT=lite
 #FFOPTS="-g -O0 -i4 -r8 -check all -ftrapuv -convert big_endian -fp-stack-check -fstack-protector -heap-arrays -recursiv -traceback -openmp"
 FFOPTS="-i4 -O3 -r8  -convert big_endian -fp-model precise -openmp"
 LDIR=/nwprod/lib
 LIBS="-L/$LDIR -lw3emc_d -lw3nco_d -lbacio_4 -lsp_v2.0.1_d"
 LDOPTS="-openmp -mkl"
elif [ $machine = WCOSS_C ] ; then
 CF=ftn
 FFOPTS="-i4 -O3 -r8  -convert big_endian -fp-model precise -openmp"
 LIBS="${W3EMC_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${SP_LIBd}"
 LDOPTS="-openmp -mkl"
elif [ $machine = THEIA ] ; then
 CF=ftn
 FFOPTS="-i4 -O3 -r8  -convert big_endian -fp-model precise -openmp"
 LIBS="${W3EMC_LIBd} ${W3NCO_LIBd} ${BACIO_LIB4} ${SP_LIBd}"
fi

f=mtnlm7_slm30g.f 
x=../../exec/terrain.x
$CF $FFOPTS $f $LIBS $LDOPTS -o $x

f=mtnlm7_slm30g_oclsm.f
x=../../exec/terrain_oclsm.x
$CF $FFOPTS $f $LIBS $LDOPTS -o $x







