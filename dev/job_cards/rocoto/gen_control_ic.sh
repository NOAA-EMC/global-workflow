#! /usr/bin/env bash
source "${HOMEglobal}/dev/ush/load_modules.sh" run
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="gen_control_ic"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEglobal}/dev/jobs/JGLOBAL_ATMOS_CHGRES_GEN_CONTROL"
status=$?

exit "${status}"
