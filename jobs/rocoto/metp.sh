#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
source "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
if (( status != 0 )); then exit "${status}"; fi

export job="metp${METPCASE}"
export jobid="${job}.$$"

"${HOMEgfs}/jobs/JGFS_ATMOS_VERIFICATION"

exit $?
