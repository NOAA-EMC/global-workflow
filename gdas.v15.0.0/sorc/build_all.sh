#!/bin/sh
set -x

target=${1:-cray}  ;#wcoss, cray or theia

for script in build_enkf.sh build_gdas_gridbull.sh build_gdas_navybull.sh build_gdas_trpsfcmv.sh; do
 chmod u+x $script
 $script $target
done

exit
