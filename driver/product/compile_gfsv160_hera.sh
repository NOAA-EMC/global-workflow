

set -x

export version=v16.0.0

cd /scratch2/NCEPDEV/stmp3/Boi.Vuong/gfs.v16.0.0/sorc

./build_gfs_fbwndgfs.sh  
./build_gfs_util.sh  

cp /scratch2/NCEPDEV/stmp3/Boi.Vuong/trim_rh.sh /scratch2/NCEPDEV/stmp3/Boi.Vuong/gfs.$version/ush

cd /scratch2/NCEPDEV/stmp3/Boi.Vuong/gfs.$version/util/sorc
sh compile_gfs_util_wcoss.sh
