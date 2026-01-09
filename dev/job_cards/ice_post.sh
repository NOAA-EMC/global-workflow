#! /usr/bin/env bash

set -x

###############################################################
## ice post (monthly mean) driver script when run with segs
###############################################################

# Source FV3GFS workflow modules
#source "${HOMEgfs}/dev/ush/load_modules.sh" run
#status=$?
#if ((status != 0)); then exit "${status}"; fi

source "${HOMEgfs}/ush/detect_machine.sh"
source "${HOMEgfs}/dev/ush/load_modules.sh" run
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}";
fi

export job="ice_post"

# shellcheck disable=SC2153
export jobid
jobid="${job}.$$"
###############################################################
# Execute the JJOB
###############################################################
"${HOMEgfs}/jobs/JGLOBAL_ICE_POST"
status=$?
[[ ${status} -ne 0 ]] && exit "${status}"

exit 0
