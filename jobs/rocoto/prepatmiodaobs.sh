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
py_ver=$(regex="[0-9]+\.[0-9]+"; if [[ $(python --version) =~ ${regex} ]]; then echo "${BASH_REMATCH[0]}"; fi)
PYIODALIB="${HOMEgfs}/sorc/gdas.cd/build/lib/python${py_ver}"
PYTHONPATH="${PYIODALIB}:${wxflowPATH}:${PYTHONPATH}"
export PYTHONPATH

###############################################################
# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_ATM_PREP_IODA_OBS"
status=$?
exit "${status}"
