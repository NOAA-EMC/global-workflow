#! /usr/bin/env bash

set -x

###############################################################
source "${HOMEgfs}/dev/ush/load_modules.sh" run
status=$?
if ((status != 0)); then exit "${status}"; fi

export job="gempakmetancdc"
export jobid="${job}.$$"

# Execute the JJOB
"${HOMEgfs}/dev/jobs/JGDAS_ATMOS_GEMPAK_META_NCDC"

status=$?
exit "${status}"
