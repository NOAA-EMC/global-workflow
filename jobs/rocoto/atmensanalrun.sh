#!/bin/bash -x

###############################################################
# Source FV3GFS workflow modules
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# set JJOB variables
export configs="base atmensanal atmensanalrun"
export EXSCRIPT=${GDASRUNSH:-$HOMEgfs/sorc/gdas.cd/scripts/exgdas_global_atmos_ensanal_run.sh}
# Execute the JJOB
$HOMEgfs/jobs/JGDAS_GLOBAL_ATMOS_ENSANAL
status=$?
exit $status
