#! /usr/bin/env bash
set -x

###############################################################
## atmosphere post (daily and monthly mean) driver script
###############################################################

# Source FV3GFS workflow modules
source "${HOMEglobal}/ush/detect_machine.sh" || exit 1
source "${HOMEglobal}/dev/ush/load_modules.sh" atmos_post
status=$?
export job="atmos_post"
# shellcheck disable=SC2153
export jobid
jobid="${job}.$$"
###############################################################
# Execute the JJOB
###############################################################
"${HOMEglobal}/dev/jobs/JGLOBAL_ATMOS_POST"
status=$?
[[ ${status} -ne 0 ]] && exit "${status}"

exit 0
