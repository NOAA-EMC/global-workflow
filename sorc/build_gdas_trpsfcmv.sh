#!/bin/sh
set -x -e

target=$1
if [ $# -ne 1 ]; then
 echo "Usage: $0 wcoss or cray or theia"
 exit
fi

EXECdir=../exec
[ -d $EXECdir ] || mkdir $EXECdir

mod=$( cd ../modulefiles/ ; pwd -P )
source "$mod/module-setup.sh.inc"

if [ $target = wcoss -o $target = cray ]; then
 module load ../modulefiles/Module_gdas_trpsfcmv.$target
else
 source ../modulefiles/Module_gdas_trpsfcmv.$target
fi
module list

cd gdas_trpsfcmv.fd
   make -f makefile
   cp gdas_trpsfcmv ../$EXECdir/.
   cd ../
