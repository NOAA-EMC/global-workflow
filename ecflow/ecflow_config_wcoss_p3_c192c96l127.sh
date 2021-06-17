#!/bin/bash
###################################################################################################################
# This script is for developer to configure ecflow on WCOSS DELL p3 running with lowres cycled C192C96L127 parallel
# Author Lin Gan
# On-Line Google documentation can be found in:
#   https://docs.google.com/presentation/d/1ImLJpFaw9WJj-ghpzqcs727xdXHeQMhMkeK7TSqwocA/edit?usp=sharing
# Target to run with global-workflow develop
###################################################################################################################   

set +x

# Replase the following with your ecflow location

# ECFLOW_ROOT is the main location in which your ecflow files located
ECFLOW_ROOT=/gpfs/dell2/emc/modeling/noscrub/$USER/ecflow-dell

# PTMP will be used to put COM and log files
PTMP=/gpfs/dell3/ptmp/$USER/ecfr

# DATAROOT is the stmp location for DATA
DATAROOT=/gpfs/dell3/stmp/$USER/ecfr/tmpnwprd

# ECF_HOST is the machine which ecflow will submit the job
ECF_HOST=mdecflow2

# ECF_PORT is developer individual assigned port number
#   If running in group account, please get port assigned first
ECF_PORT=`grep $USER /usrx/local/sys/ecflow/assigned_ports.txt|rev|cut -c1-5|rev`

ECF_HOME=${ECFLOW_ROOT}/submit
ECF_ROOT=${ECFLOW_ROOT}/ecflow
HOMEgfs=${ECFLOW_ROOT}/ecf_gfsv16_low
ECF_OUT=${PTMP}/com/ecflow_logs
ECF_LOG=${PTMP}/com/ecflow_logs/ecf.log
decfcom=$PTMP/com/gfs/prod
decflog=$PTMP/com/output/prod/today
jlogfile=$PTMP/com/gfs/prod/jlogfile

# Create directory and new file
mkdir -p $ECF_HOME $ECF_ROOT $decfcom $decflog $jlogfile $ECF_OUT $DATAROOT
touch $jlogfile/jlogfile $ECF_LOG

cd ${HOMEgfs}/ecflow/ecf
s_string=PTMP_ECFR
t_string=$PTMP
grep -RiIl $s_string|xargs sed -i "s|$s_string|$t_string|g"

s_string=ECF_ROOT_ECFR
t_string=$ECFLOW_ROOT
grep -RiIl $s_string|xargs sed -i "s|$s_string|$t_string|g"

s_string=DATAROOT_ECFR
t_string=$DATAROOT
grep -RiIl $s_string|xargs sed -i "s|$s_string|$t_string|g"

echo "Please exam directory ${HOMEgfs}/parm/config to ensure config.base and other config files are ready"

exit 0















