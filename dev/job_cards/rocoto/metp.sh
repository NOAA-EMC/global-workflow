#! /usr/bin/env bash

set -x

###############################################################
source "${HOMEglobal}/dev/ush/load_modules.sh" verif
status=$?
if ((status != 0)); then exit "${status}"; fi

export job="metp${METPCASE}"
export jobid="${job}.$$"

"${HOMEglobal}/dev/jobs/JGFS_ATMOS_VERIFICATION"

exit $?
