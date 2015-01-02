#!/bin/sh
target=${1:-wcoss}
set -x -e
EXECdir=../exec
[ -d $EXECdir ] || mkdir $EXECdir
for dir in *.fd; do
 cd $dir
 ./configure clean
 ./configure $target
 make -f Makefile clean
 make -f Makefile -j 8
 cp -p global_gsi ../$EXECdir
 cd ..
done
