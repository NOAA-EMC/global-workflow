#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Execute the JJOB
if [[ "${RUN_WITH_CONTAINER}" == "YES" ]]; then
"${HOMEgfs}/exec/JGLOBAL_WAVE_INIT"
else
"${HOMEgfs}/jobs/JGLOBAL_WAVE_INIT"
fi
status=$?

exit "${status}"
