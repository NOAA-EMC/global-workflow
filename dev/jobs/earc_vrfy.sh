#! /usr/bin/env bash

set -x

###############################################################
# Source FV3GFS workflow modules
source "${HOMEgfs}/dev/ush/load_modules.sh" run
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="earc_vrfy"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_ENS_ARCHIVE_VRFY"
status=$?

exit "${status}"
