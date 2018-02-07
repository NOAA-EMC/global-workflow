#!/bin/sh     
set -x

target=${1:-cray}    ;#wcoss, cray or theia

pwd=`pwd`
for script in build_chgres.sh build_orog.sh; do
 cd $pwd
 sh $script $target
done

cd $pwd/fre-nctools.fd
./BUILD_TOOLS.csh ${target}

exit
