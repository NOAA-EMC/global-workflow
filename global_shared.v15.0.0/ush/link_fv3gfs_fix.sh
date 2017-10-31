#!/bin/sh
################################################################################
####  UNIX Script Documentation Block
# Script name:         link_fv3gfs_fix.sh
# Script description:  Link FV3GFS fix directories for your machine
#
# Author:        Rahul Mahajan      Org: NCEP/EMC     Date: 2017-03-11
#
# Abstract: Tired of loosing your FV3GFS links to the fix directory
#           after every merge from the trunk?
#           Execute this.
#
# $Id: link_fv3gfs_fix.sh 98517 2017-10-20 15:52:58Z fanglin.yang@noaa.gov $
#
# Attributes:
#   Language: POSIX shell
#   Machine: WCOSS/WCOSS-Cray/Theia
#
####
################################################################################

set -x

machine=${1:-${machine:-"WCOSS_C"}}
target_dir=${2:-$(pwd)}

machine=$(echo $machine | tr '[a-z]' '[A-Z]')

if [ $machine = "WCOSS" ]; then
    FIX_DIR="/global/noscrub/emc.glopara/svn/fv3gfs/fix"
elif [ $machine = "WCOSS_C" -o $machine == "CRAY" ]; then
    FIX_DIR="/gpfs/hps3/emc/global/noscrub/emc.glopara/svn/fv3gfs/fix"
elif [ $machine = "THEIA" ]; then
    FIX_DIR="/scratch4/NCEPDEV/global/save/glopara/svn/fv3gfs/fix"
else
    echo "machine $machine is unsupported, ABORT!"
    exit 1
fi

cd $target_dir

rm -rf fix
ln -sf $FIX_DIR .
ls -l  fix

exit 0
