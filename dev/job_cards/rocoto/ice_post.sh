#! /usr/bin/env bash

set -x

###############################################################
## ice post (monthly mean) driver script when run with segs
###############################################################

source "${HOMEglobal}/ush/detect_machine.sh"
source "${HOMEglobal}/dev/ush/load_modules.sh" run
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="ice_post"

# shellcheck disable=SC2153
export jobid
jobid="${job}.$$"
###############################################################
# Execute the JJOB
###############################################################
"${HOMEglobal}/dev/jobs/JGLOBAL_ICE_POST"
status=$?
[[ ${status} -ne 0 ]] && exit "${status}"

exit 0
