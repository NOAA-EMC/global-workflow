#! /usr/bin/env bash

set -x

###############################################################
# Source FV3GFS workflow modules
source "${HOMEgfs}/dev/ush/load_modules.sh" run
status=$?
[[ ${status} -ne 0 ]] && exit "${status}"

export job="globus_arch"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}/dev/jobs/JGLOBAL_GLOBUS_ARCH"
status=$?

exit "${status}"
