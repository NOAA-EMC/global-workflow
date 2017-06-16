#!/bin/sh
# This script submits forecast jobs.
if [ $# -ne 4 ]; then
    echo " Usage: $0 config cdate cdump step  "
    exit 8
fi
#
set -x
CONFIG=$1
CDATE=$2
CDUMP=$3
CSTEP=$4
#
mac=`hostname | cut -c1`

base_d=/gpfs/${mac}d1/emc/global
#
psub=/gpfs/${mac}d1/emc/global/save/emc.glopara/svn/gfs/trunk/para/bin/psub_wcoss

$psub $CONFIG $CDATE $CDUMP $CSTEP
#$psub para_config_WCOSS 2013082500 gdas fcst1
#$psub para_config_WCOSS 2013082500 gdas efmn 
