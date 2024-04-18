#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

# Source FV3GFS workflow modules
. "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
#status=$?
status=0
[[ "${status}" -ne 0 ]] && exit "${status}"

export job="stage_ic"
#export jobid="${job}.$$"
export jobid="${job}.5"

# Execute the JJOB
# "${HOMEgfs}/jobs/JGLOBAL_STAGE_IC"
status=0

exit "${status}"
