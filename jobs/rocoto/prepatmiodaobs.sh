#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source UFSDA workflow modules
. "${HOMEgfs}/ush/load_ufsda_modules.sh"
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="prepatmobs"
export jobid="${job}.$$"

###############################################################
# setup python path for ioda utilities
# shellcheck disable=SC2311
pyiodaPATH="${HOMEgfs}/sorc/gdas.cd/build/lib/python$(detect_py_ver)/"
PYTHONPATH="${pyiodaPATH}:${PYTHONPATH}"
export PYTHONPATH

###############################################################
# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_ATM_PREP_IODA_OBS"
status=$?
exit "${status}"
