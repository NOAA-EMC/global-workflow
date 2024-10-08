#! /usr/bin/env bash

export STRICT="NO"
source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source UFSDA workflow modules
. "${HOMEgfs}/ush/load_ufsda_modules.sh"
status=$?
[[ ${status} -ne 0 ]] && exit "${status}"

export job="prepoceanobs"
export jobid="${job}.$$"

###############################################################
# setup python path for class defs and utils
# shellcheck disable=SC2311
pyiodaPATH="${HOMEgfs}/sorc/gdas.cd/build/lib/python$(detect_py_ver)/"
PYTHONPATH="${pyiodaPATH}:${PYTHONPATH}"
export PYTHONPATH

###############################################################
# Execute the JJOB
"${HOMEgfs}"/jobs/JGLOBAL_PREP_OCEAN_OBS
status=$?
exit "${status}"
