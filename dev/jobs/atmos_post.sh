#! /usr/bin/env bash
set -x

###############################################################
## atmosphere post (daily and monthly mean) driver script
###############################################################

# Source FV3GFS workflow modules
source "${HOMEgfs}/dev/ush/load_modules.sh" run
status=$?
if ((status != 0)); then exit "${status}"; fi

export job="atmos_post"

# shellcheck disable=SC2153
export jobid
jobid="${job}.$$"
###############################################################
# Execute the JJOB
###############################################################
"${HOMEgfs}/jobs/JGLOBAL_ATMOS_POST"
status=$?
[[ ${status} -ne 0 ]] && exit "${status}"

exit 0
