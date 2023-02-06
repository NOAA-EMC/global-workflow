#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source UFSDA workflow modules
. "${HOMEgfs}/ush/load_ufsda_modules.sh"
status=$?
[[ ${status} -ne 0 ]] && exit "${status}"

export job="aeroanlinit"
export jobid="${job}.$$"

###############################################################
# setup python path for workflow utilities and tasks
export PYTHONPATH="${PYTHONPATH}:${HOMEgfs}/ush/python:${HOMEgfs}/ush/python/pygw/src"
###############################################################
# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_AERO_ANALYSIS_INITIALIZE"
status=$?
exit "${status}"
