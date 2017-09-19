#! /bin/sh
set -x

##################################################################
# wafs using module compile standard
# 02/01/2016 yali.ma@noaa.gov:    Create module load version
##################################################################

mod=$( cd ../../global_shared.v15.0.0/modulefiles/ ; pwd -P )
source "$mod/module-setup.sh.inc"
module use ../modulefiles
module load modulefile.gfs_smartinit.wcoss

 curdir=`pwd`

for dir in smartinit.fd smartprecip.fd; do
 cd ${curdir}/$dir
 make clean
 make
 make clean
done


