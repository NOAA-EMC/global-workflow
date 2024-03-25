#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source UFSDA workflow modules
. "${HOMEgfs}/ush/load_ufsda_modules.sh"
status=$?
[[ ${status} -ne 0 ]] && exit "${status}"

export job="prepsnowobs"
export jobid="${job}.$$"

###############################################################
# setup python path for workflow utilities and tasks
wxflowPATH="${HOMEgfs}/ush/python:${HOMEgfs}/ush/python/wxflow/src"
py_ver=$(regex="[0-9]+\.[0-9]+"; if [[ $(python --version) =~ ${regex} ]]; then echo "${BASH_REMATCH[0]}"; fi)
gdasappPATH="${HOMEgfs}/sorc/gdas.cd/sorc/iodaconv/src:${HOMEgfs}/sorc/gdas.cd/build/lib/python${py_ver}"
PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${wxflowPATH}:${gdasappPATH}"
export PYTHONPATH

###############################################################
# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_PREP_SNOW_OBS"
status=$?
exit "${status}"
