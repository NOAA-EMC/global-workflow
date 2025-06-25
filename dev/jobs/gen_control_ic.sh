#! /usr/bin/env bash
source "${HOMEgfs}/ush/load_fv3gfs_modules.sh"
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="gen_control_ic"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
# "${HOMEgfs}/jobs/GEN_CONTROL"
# status=$?

exit "${status}"
