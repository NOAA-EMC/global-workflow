#!/bin/sh
set -x

# Check final exec folder exists
if [ ! -d "../exec" ]; then
  mkdir ../exec
fi

finalexecdir=$( pwd -P )/../exec

set +x
source ./machine-setup.sh > /dev/null 2>&1

source ../modulefiles/modulefile.ww3.$target
set -x 


cd fv3_coupled.fd/WW3
export WW3_DIR=$( pwd -P )/model
export WW3_BINDIR="${WW3_DIR}/bin"
export WW3_TMPDIR=${WW3_DIR}/tmp
export WW3_EXEDIR=${WW3_DIR}/exe
export WW3_COMP=$target 
export WW3_CC=gcc
export WW3_F90=gfortran
export SWITCHFILE="${WW3_DIR}/esmf/switch"

export WWATCH3_ENV=${WW3_BINDIR}/wwatch3.env
export PNG_LIB=$PNG_ROOT/lib64/libpng.a
export Z_LIB=$ZLIB_ROOT/lib/libz.a
export JASPER_LIB=$JASPER_ROOT/lib64/libjasper.a
export WWATCH3_NETCDF=NC4
export NETCDF_CONFIG=$NETCDF_ROOT/bin/nc-config

rm  $WWATCH3_ENV
echo '#'                                              > $WWATCH3_ENV
echo '# ---------------------------------------'      >> $WWATCH3_ENV
echo '# Environment variables for wavewatch III'      >> $WWATCH3_ENV
echo '# ---------------------------------------'      >> $WWATCH3_ENV
echo '#'                                              >> $WWATCH3_ENV
echo "WWATCH3_LPR      $PRINTER"                      >> $WWATCH3_ENV
echo "WWATCH3_F90      $WW3_F90"                      >> $WWATCH3_ENV
echo "WWATCH3_CC       $WW3_CC"                       >> $WWATCH3_ENV
echo "WWATCH3_DIR      $WW3_DIR"                      >> $WWATCH3_ENV
echo "WWATCH3_TMP      $WW3_TMPDIR"                   >> $WWATCH3_ENV
echo 'WWATCH3_SOURCE   yes'                           >> $WWATCH3_ENV
echo 'WWATCH3_LIST     yes'                           >> $WWATCH3_ENV
echo ''                                               >> $WWATCH3_ENV

${WW3_BINDIR}/w3_clean -m 
${WW3_BINDIR}/w3_setup -q -c $WW3_COMP $WW3_DIR

echo $(cat ${SWITCHFILE}) > ${WW3_BINDIR}/tempswitch

sed -e "s/DIST/SHRD/g"\
    -e "s/OMPG/ /g"\
    -e "s/OMPH/ /g"\
    -e "s/MPIT/ /g"\
    -e "s/MPI/ /g"\
    -e "s/PDLIB/ /g"\
       ${WW3_BINDIR}/tempswitch > ${WW3_BINDIR}/switch

#Build exes for prep jobs: 
${WW3_BINDIR}/w3_make ww3_grid 

#Build exes for post jobs (except grib)"
${WW3_BINDIR}/w3_make ww3_outp 

exit
#Update switch for grib: 
echo $(cat ${SWITCHFILE}) > ${WW3_BINDIR}/tempswitch

sed -e "s/DIST/SHRD/g"\
    -e "s/OMPG/ /g"\
    -e "s/OMPH/ /g"\
    -e "s/MPIT/ /g"\
    -e "s/MPI/ /g"\
    -e "s/PDLIB/ /g"\
    -e "s/NOGRB/NCEP2 NCO/g"\
       ${WW3_BINDIR}/tempswitch > ${WW3_BINDIR}/switch
#Build exe for grib
${WW3_BINDIR}/w3_make ww3_grib

cp $WW3_EXEDIR/ww3_* $finalexecdir/
${WW3_BINDIR}/w3_clean -c
