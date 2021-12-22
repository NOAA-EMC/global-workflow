#!/bin/ksh -x

###############################################################
# Source FV3GFS workflow modules
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
# set JJOB variables
export configs="base atmanal atmanalrun"
export EXSCRIPT=${UFSDARUNSH:-$SCRgfs/exufsda_global_atmos_analysis_run.sh
# Execute the JJOB
$HOMEgfs/jobs/JUFSDA_GLOBAL_ATMOS_ANALYSIS
status=$?
exit $status
