#! /usr/bin/env bash
set -eux

#####################################################################################
# using module compile standard
# 01/26/2016 Fanglin.Yang@noaa.gov:    Create module load version
# 07/10/2016 Fanglin.Yang@noaa.gov:    update for building on wcoss, theia and cray
#####################################################################################

if [ $# -ne 1 ]; then
 echo "Usage: $0 wcoss or cray or theia"
 exit
fi

target=$1

EXECdir=../exec
[ -d $EXECdir ] || mkdir $EXECdir

set +x
if [ $target = wcoss ]; then
. /usrx/local/Modules/3.2.10/init/sh
elif [ $target = cray ]; then
. $MODULESHOME/init/sh
elif [ $target = theia ]; then
. /apps/lmod/lmod/init/sh
else
 exit
fi

module purge
if [ $target = wcoss -o $target = cray ]; then
 module load ../modulefiles/fv3gfs/global_chgres.$target
else
 source ../modulefiles/fv3gfs/global_chgres.$target
fi
module list
set -x

curdir=`pwd`

cd ${curdir}/global_chgres.fd
./makefile.sh

if [ -d ${curdir}/nst_mask_namchg.fd ]; then
 cd ${curdir}/nst_mask_namchg.fd
 ./makefile.sh
fi

