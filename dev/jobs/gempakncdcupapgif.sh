#! /usr/bin/env bash

set -x

###############################################################
source "${HOMEgfs}/dev/ush/load_fv3gfs_modules.sh"
status=$?
if (( status != 0 )); then exit "${status}"; fi

export job="gempakncdcupapgif"
export jobid="${job}.$$"

# Execute the JJOB
"${HOMEgfs}/jobs/JGFS_ATMOS_GEMPAK_NCDC_UPAPGIF"

status=$?
exit "${status}"
