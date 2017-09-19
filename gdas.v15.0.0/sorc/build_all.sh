#!/bin/sh
set -xue

target=$1  ;#wcoss, cray or theia

mod=$( cd ../../global_shared.v15.0.0/modulefiles/ ; pwd -P )

mkdir -p ../exec/

# Initialize environment for module command and purge modules:
setup=$mod/module-setup.sh.inc
test -s $setup
source $setup

# Add our modulefiles:
module use $mod
module load module_base.$target

export PATH=$PATH:.

for script in build_gdas_gridbull.sh build_gdas_navybull.sh build_gdas_trpsfcmv.sh; do
#for script in build_enkf.sh build_gdas_gridbull.sh build_gdas_navybull.sh build_gdas_trpsfcmv.sh; do
 chmod u+x $script
 $script $target
done

exit
