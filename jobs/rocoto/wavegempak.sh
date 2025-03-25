#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
source "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
err=$?
if [[ "${err}" -ne 0 ]]; then exit "${err}"; fi

export job="wavegempak"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_WAVE_GEMPAK"
if [[ "${err}" -ne 0 ]]; then exit "${err}"; fi

exit 0
