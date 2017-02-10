SHELL=/bin/sh
set -x

target=${1:-cray}  ;#wcoss, cray or theia

if [ ! -d ../exec ]; then
  mkdir ../exec
fi

if [ $target = theia ]; then
 for script in build_gfs_fbwndgfs.sh_theia build_gfs_overpdtg2.sh_theia build_gfs_wintemv.sh_theia build_smartinit_theia.sh build_tocsbufr_bufr_flux_theia.sh build_wafs_theia.sh; do
  sh $script
 done
elif [ $target = cray ]; then
 for script in build_gfs_fbwndgfs.sh build_gfs_overpdtg2.sh build_gfs_wintemv.sh build_smartinit_wcoss.sh; do
  sh $script $target
 done
 for script in build_gfs_bufrsnd.sh build_wafs_cray.sh; do
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
