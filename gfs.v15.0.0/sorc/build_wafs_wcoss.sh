#! /bin/sh
set -x

##################################################################
# wafs using module compile standard
# 02/01/2016 yali.ma@noaa.gov:    Create module load version
##################################################################

mod=$( cd ../../global_shared.v15.0.0/modulefiles/ ; pwd -P )
source "$mod/module-setup.sh.inc"
module use ../modulefiles
module load wafs_v13.0.0
module list

 curdir=`pwd`
 export INC="${G2_INC4}"
 export FC=ifort

 export FFLAGSawc="-FR -I ${G2_INC4} -g -O2 -convert big_endian -assume noold_ldout_format"
 export FFLAGSblnd="-O -I ${G2_INC4}"
 export FFLAGST="-O -FR -I ${G2_INC4}"
 export FFLAGSgcip="-FR -I ${G2_INC4} -g -O2"

 export FFLAGScnv="-O3 -g -I ${G2_INC4}"
 export FFLAGSmkwfs="-O3 -g -r8 -i8"

for dir in awc_wafavn.fd gcip.fd wafs_blending.fd makewafs.fd cnvgrib2_wafs.fd; do
 export LIBS="${G2_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4} ${IP_LIB4} ${SP_LIB4} ${JASPER_LIB} ${PNG_LIB} ${Z_LIB}  ${BUFR_LIB4}"
 cd ${curdir}/$dir
 make clean
 make
 make install
 make clean
done


