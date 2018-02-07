#!/bin/sh
set -eux

#####################################################################################
# using module compile standard
# 12/20/2017 Rahul.Mahajan@noaa.gov:    update for building on wcoss, theia and cray
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
 module load ../modulefiles/fv3gfs/enkf_chgres_recenter.$target
else
 source ../modulefiles/fv3gfs/enkf_chgres_recenter.$target
fi
module list
set -x

curdir=`pwd`

cd ${curdir}/enkf_chgres_recenter.fd

export FFLAGS="-O0 -r8 -i4 -qopenmp -traceback"

make clean
make
make install
make clean
