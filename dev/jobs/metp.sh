#! /usr/bin/env bash

set -x

###############################################################
source "${HOMEgfs}/dev/ush/load_gw_verif_modules.sh"
status=$?
if (( status != 0 )); then exit "${status}"; fi

export job="metp${METPCASE}"
export jobid="${job}.$$"

"${HOMEgfs}/jobs/JGFS_ATMOS_VERIFICATION"

exit $?
