

set -x

export version=v16.0.0

cd /gpfs/dell2/emc/modeling/noscrub/Boi.Vuong/git/gfs.$version/sorc

build_gfs_fbwndgfs.sh  
build_gfs_util.sh  

cp /gpfs/dell2/emc/modeling/noscrub/Boi.Vuong/git/trim_rh.sh /gpfs/dell2/emc/modeling/noscrub/Boi.Vuong/git/gfs.$version/ush

cd /gpfs/dell2/emc/modeling/noscrub/Boi.Vuong/git/gfs.$version/util/sorc
compile_gfs_util_wcoss.sh


