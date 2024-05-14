#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

# Source FV3GFS workflow modules
. "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
[[ "${status}" -ne 0 ]] && exit "${status}"

###############################################################
# setup python path for workflow utilities and tasks
PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${HOMEgfs}/ush/python"
export PYTHONPATH

export job="stage_ic"
export jobid="${job}.$$"

# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_STAGE_IC"
status=$?


exit "${status}"
