#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"
source "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
if (( status != 0 )); then exit "${status}"; fi

export job="gempak"
export jobid="${job}.$$"


# Execute the JJOB
"${HOMEgfs}/jobs/J${RUN^^}_ATMOS_GEMPAK"

status=$?
exit "${status}"
