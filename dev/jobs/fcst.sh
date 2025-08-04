#! /usr/bin/env bash

set -x

###############################################################
# Source FV3GFS workflow modules
# TODO clean this up once ncdiag/1.1.2 is installed on WCOSS2
source "${HOMEgfs}/ush/detect_machine.sh"
if [[ "${MACHINE_ID}" == "wcoss2" ]]; then
  source "${HOMEgfs}/dev/ush/load_ufswm_modules.sh"
else
  source "${HOMEgfs}/dev/ush/load_fv3gfs_modules.sh"
fi
status=$?
if [[ ${status} -ne 0 ]]; then
  exit "${status}"
fi

export job="fcst"
export jobid="${job}.$$"

# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_FORECAST"
status=$?

exit "${status}"
