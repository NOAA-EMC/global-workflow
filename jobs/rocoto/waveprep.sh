#! /usr/bin/env bash

source "$HOMEgfs/ush/preamble.sh"

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
. $HOMEgfs/ush/load_fv3gfs_modules.sh
status=$?
[[ $status -ne 0 ]] && exit $status

###############################################################
echo
echo "=============== START TO RUN WAVE PREP ==============="
# Execute the JJOB
$HOMEgfs/jobs/JGLOBAL_WAVE_PREP
status=$?
[[ $status -ne 0 ]] && exit $status

exit 0
