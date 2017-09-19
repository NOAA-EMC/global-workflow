#! /bin/sh
set -xue

target=$1  ;#wcoss, cray or theia

mod=$( cd ../../global_shared.v15.0.0/modulefiles/ ; pwd -P )

# Initialize environment for module command and purge modules:
setup=$mod/module-setup.sh.inc
test -s $setup
source $setup

# Add our modulefiles:
module use $mod
module load module_base.$target

if [ ! -d ../exec ]; then
  mkdir ../exec
fi

export PATH=$PATH:.

if [ $target = theia ]; then
 for script in build_smartinit_theia.sh build_tocsbufr_bufr_flux_theia.sh build_wafs_theia.sh build_gfs_cnvgrib21_gfs.sh_theia; do
  sh $script
 done
 for script in build_gfs_fbwndgfs.sh build_gfs_overpdtg2.sh build_gfs_wintemv.sh; do
  sh $script $target
 done
elif [ $target = cray ]; then
 for script in build_gfs_fbwndgfs.sh build_gfs_overpdtg2.sh build_gfs_wintemv.sh build_gfs_cnvgrib21_gfs.sh; do
  sh $script $target
 done
 for script in build_gfs_bufrsnd.sh build_wafs_cray.sh build_smartinit_cray.sh; do
  sh $script
 done
elif [ $target = wcoss ]; then
 for script in build_gfs_fbwndgfs.sh build_gfs_overpdtg2.sh build_gfs_wintemv.sh build_smartinit_wcoss.sh build_tocsbufr_bufr_flux_wcoss.sh build_wafs_wcoss.sh; do
  sh $script
 done
else
 echo "Please indicate build target...  "
 exit
fi
