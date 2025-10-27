#! /usr/bin/env bash

set -x
source "${HOMEgfs}/dev/ush/load_modules.sh" run
status=$?
if ((status != 0)); then exit "${status}"; fi

export job="gempakpgrb2spec"
export jobid="${job}.$$"

# Execute the JJOB
"${HOMEgfs}/jobs/JGFS_ATMOS_GEMPAK_PGRB2_SPEC"

status=$?
exit "${status}"
