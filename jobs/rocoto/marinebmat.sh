#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source UFSDA workflow modules
. "${HOMEgfs}/ush/load_ufsda_modules.sh"
status=$?
[[ "${status}" -ne 0 ]] && exit "${status}"

export job="marinebmat"
export jobid="${job}.$$"

###############################################################
# setup python path for workflow utilities and tasks
wxflowPATH="${HOMEgfs}/ush/python:${HOMEgfs}/ush/python/wxflow/src"
socaToolsPATH="${HOMEgfs}/sorc/gdas.cd/sorc/soca/tools"
PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${wxflowPATH}"
export PYTHONPATH

###############################################################
# Execute the JJOB
"${HOMEgfs}"/jobs/JGLOBAL_MARINE_BMAT
echo "BMAT gets run here"
status=$?
exit "${status}"
