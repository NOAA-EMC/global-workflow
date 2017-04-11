#! /usr/bin/env bash
set -eux

#####################################################################################
# orog using module compile standard
# 10/10/2016 Fanglin.Yang@noaa.gov:    Create module load version
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
 module load ../modulefiles/fv3gfs/orog.$target
else
 source ../modulefiles/fv3gfs/orog.$target
fi

module list
set +x

curdir=$(pwd)

cd ${curdir}/orog.fd
./makefile.sh_$target

