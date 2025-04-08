#! /usr/bin/env bash

set -x

###############################################################
# Source FV3GFS workflow modules
. "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
if [[ ${status} -ne 0 ]]; then exit "${status}"; fi

###############################################################
# setup python path for workflow utilities and tasks
PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${HOMEgfs}/ush/python"
export PYTHONPATH

export job="globus_earc"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_ENS_GLOBUS_ARCH"
status=$?

exit "${status}"
