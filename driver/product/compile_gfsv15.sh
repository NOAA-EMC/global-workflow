set -x

#
# Compile all FV3GFS utilities
#

cd /gpfs/dell2/emc/modeling/noscrub/Boi.Vuong/git/gfs.v15.0.0/sorc

build_gdas.sh
build_gfs_fbwndgfs.sh  
build_gfs_overpdtg2.sh 
build_gfs_wintemv.sh  
build_gfs_util.sh  

