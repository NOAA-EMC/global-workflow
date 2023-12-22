#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
. "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
if (( status != 0 )); then exit "${status}"; fi

export job="gempakmetancdc"
export jobid="${job}.$$"

# Execute the JJOB
"${HOMEgfs}/jobs/JGDAS_ATMOS_GEMPAK_META_NCDC"

status=$?
exit "${status}"
