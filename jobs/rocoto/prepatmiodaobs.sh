#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source UFSDA workflow modules
. "${HOMEgfs}/ush/load_ufsda_modules.sh"
status=$?
[[ ${status} -ne 0 ]] && exit "${status}"

export job="prepatmobs"
export jobid="${job}.$$"

###############################################################
# setup python path for workflow and ioda utilities
wxflowPATH="${HOMEgfs}/ush/python:${HOMEgfs}/ush/python/wxflow/src"
PYIODALIB="${HOMEgfs}/sorc/gdas.cd/build/lib/python3.7"
PYTHONPATH="${PYIODALIB}:${wxflowPATH}:${PYTHONPATH}"
export PYTHONPATH

###############################################################
# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_ATM_PREP_IODA_OBS"
status=$?
exit "${status}"
