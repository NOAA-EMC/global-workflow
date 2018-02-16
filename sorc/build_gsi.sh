#!/bin/sh

target=$1
if [ $# -ne 1 ]; then
 echo "Usage: $0 wcoss or cray or theia"
 exit
fi

cd gsi.fd/ush/

# Workarounds for bugs in gsi build scripts:
export PATH=$PATH:.

if [[ "$target" == theia ]] ; then
    module use -a /scratch3/NCEPDEV/nwprod/lib/modulefiles
fi

./build_all.sh "$target"
