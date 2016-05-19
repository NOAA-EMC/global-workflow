#!/bin/sh
target=${1:-wcoss}
set -x -e
EXECdir=../exec
[ -d $EXECdir ] || mkdir $EXECdir

. /usrx/local/Modules/3.2.10/init/sh
module purge
module load ../modulefiles/modulefile.global_gsi.$target
module list

dlist="gsi.fd"
for dir in $dlist; do
 cd $dir
 ./configure clean
 ./configure $target
 make -f Makefile clean
 make -f Makefile -j 8
 cp -p global_gsi ../$EXECdir
 make -f Makefile clean
 ./configure clean
 cd ..
done
