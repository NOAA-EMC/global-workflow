#!/bin/sh
target=${1:-wcoss}
set -x -e
EXECdir=../exec
[ -d $EXECdir ] || mkdir $EXECdir
for dir in *.fd; do
 if [ $dir = enkf_update.fd ]; then
   cd $dir
   ./configure clean
   ./configure $target
   make -f Makefile clean
   make -f Makefile -j 8
   cd enkf
   ./configure clean
   ./configure $target
   make -f Makefile clean
   make -f Makefile
   cp -p global_enkf ../../$EXECdir/
   cd ../../
 else
   cd $dir
   ./configure clean
   ./configure $target
   make -f Makefile clean
   make -f Makefile
   cp -p *.x ../$EXECdir/
   rm -f ../$EXECdir/log*.x 
   cd ../
 fi
done
