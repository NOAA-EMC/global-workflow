#! /usr/bin/env bash
set -x
set -e

###############################################################
## atmosphere post (daily and monthly mean) driver script
###############################################################

# Source FV3GFS workflow modules
#if ((status != 0)); then exit "${status}"; fi
source "${HOMEglobal}/ush/detect_machine.sh" || exit 1
source "${HOMEglobal}/dev/ush/load_modules.sh" atmos_post
status=$?
export job="atmos_post"
export jobid
jobid="${job}.$$"
###############################################################
# Execute the JJOB
###############################################################
"${HOMEglobal}/dev/jobs/JGLOBAL_ATMOS_POST"
status=$?
[[ ${status} -ne 0 ]] && exit "${status}"

exit 0
