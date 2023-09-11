#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
. "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
[[ ${status} -ne 0 ]] && exit ${status}

export job="gempak"
export jobid="${job}.$$"

# Execute the JJOB
${HOMEgfs}/jobs/JGFS_ATMOS_GEMPAK

status=$?
exit ${status}
