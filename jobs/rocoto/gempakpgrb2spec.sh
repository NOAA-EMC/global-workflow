#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
. "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
if (( status != 0 )); then exit "${status}"; fi

export job="gempakpgrb2spec"
export jobid="${job}.$$"

# Execute the JJOB
"${HOMEgfs}/jobs/JGFS_ATMOS_GEMPAK_PGRB2_SPEC"

status=$?
exit "${status}"
