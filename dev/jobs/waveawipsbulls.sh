#! /usr/bin/env bash

set -x

###############################################################
# Source FV3GFS workflow modules
source "${HOMEgfs}/dev/ush/load_modules.sh" run
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="waveawipsbulls"
export jobid="${job}.$$"

###############################################################
# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_WAVE_PRDGEN_BULLS"
status=$?

exit "${status}"
