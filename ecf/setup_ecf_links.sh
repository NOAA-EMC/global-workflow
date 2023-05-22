#!/bin/bash

set -eu

ECF_DIR=$(pwd)

# Function that loops over forecast hours and
# creates link between the master and target
function link_master_to_fhr(){
  tmpl=$1  # Name of the master template
  fhrs=$2  # Array of forecast hours
  for fhr in ${fhrs[@]}; do
    fhrchar=$(printf %03d $fhr)
    master=${tmpl}_master.ecf
    target=${tmpl}_f${fhrchar}.ecf
    rm -f $target
    ln -sf $master $target
  done
}

# EnKF GDAS post files
cd $ECF_DIR/scripts/enkfgdas/post
echo "Linking enkfgdas/post ..."
fhrs=($(seq 3 9))
link_master_to_fhr "jenkfgdas_post" "$fhrs"

# GDAS post files
cd $ECF_DIR/scripts/gdas/atmos/post
echo "Linking gdas/atmos/post ..."
rm -f jgdas_atmos_post_anl.ecf
ln -sf jgdas_atmos_post_master.ecf jgdas_atmos_post_anl.ecf
fhrs=($(seq 0 9))
link_master_to_fhr "jgdas_atmos_post" "$fhrs"

# GFS post files
cd $ECF_DIR/scripts/gfs/atmos/post
echo "Linking gfs/atmos/post ..."
rm -f jgfs_atmos_post_anl.ecf
ln -sf jgfs_atmos_post_master.ecf jgfs_atmos_post_anl.ecf
fhrs=($(seq 0 1 120) $(seq 123 3 384))
link_master_to_fhr "jgfs_atmos_post" "$fhrs"

# GFS awips 20km 1p0 files
cd $ECF_DIR/scripts/gfs/atmos/post_processing/awips_20km_1p0
echo "Linking gfs/atmos/post_processing/awips_20km_1p0 ..."
fhrs=($(seq 0 3 84) $(seq 90 6 240))
link_master_to_fhr "jgfs_atmos_awips" "$fhrs"

# GFS awips g2 files
cd $ECF_DIR/scripts/gfs/atmos/post_processing/awips_g2
echo "Linking gfs/atmos/post_processing/awips_g2 ..."
fhrs=($(seq 0 3 84) $(seq 90 6 240))
link_master_to_fhr "jgfs_atmos_awips_g2" "$fhrs"

