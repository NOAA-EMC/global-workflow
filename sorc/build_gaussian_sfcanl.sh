#! /usr/bin/env bash
set -eux

#####################################################################################
# 01/30/2018 George.Gayno@noaa.gov:    Initial version
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
 exit 2
fi

module purge
if [ $target = wcoss -o $target = cray ]; then
 module load ../modulefiles/fv3gfs/gaussian_sfcanl.$target
else
 source ../modulefiles/fv3gfs/gaussian_sfcanl.$target
fi
module list
set -x

curdir=`pwd`

cd ${curdir}/gaussian_sfcanl.fd
makefile.sh
