#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source UFSDA workflow modules
. "${HOMEgfs}/ush/load_ufsda_modules.sh"
status=$?
[[ ${status} -ne 0 ]] && exit "${status}"

export job="landensanl"
export jobid="${job}.$$"

###############################################################
# setup python path for workflow utilities and tasks
wxflowPATH="${HOMEgfs}/ush/python:${HOMEgfs}/ush/python/wxflow/src"
gdasappPATH="${HOMEgfs}/sorc/gdas.cd/iodaconv/src:${HOMEgfs}/sorc/gdas.cd/build/lib/python3.7"
PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${wxflowPATH}:${gdasappPATH}"
export PYTHONPATH

###############################################################
# Execute the JJOB
"${HOMEgfs}/jobs/JGLOBAL_LANDENS_ANALYSIS"
status=$?
exit "${status}"
