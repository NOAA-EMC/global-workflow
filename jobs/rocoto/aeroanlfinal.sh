#! /usr/bin/env bash
export STRICT="NO"
source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source UFSDA workflow modules
. "${HOMEgfs}/ush/load_ufsda_modules.sh"
status=$?
[[ ${status} -ne 0 ]] && exit "${status}"

export STRICT="YES"
export job="aeroanlfinal"
export jobid="${job}.$$"

###############################################################
# setup python path for workflow utilities and tasks
export PYTHONPATH="$PYTHONPATH:${HOMEgfs}/ush/python:${HOMEgfs}/ush/python/pygw/src"
###############################################################
# Execute the JJOB
"${HOMEgfs}/jobs/JGDAS_GLOBAL_AERO_ANALYSIS_FINALIZE"
status=$?
exit "${status}"
