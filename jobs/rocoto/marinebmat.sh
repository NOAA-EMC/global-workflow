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
# TODO(G): ad to the linking script instead?
#          Temporary, should be using JCSDA module when ready
socaToolsPATH="${HOMEgfs}/sorc/gdas.cd/ush/soca"
PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${socaToolsPATH}"
export PYTHONPATH

###############################################################
# Execute the JJOB
"${HOMEgfs}"/jobs/JGLOBAL_MARINE_BMAT
status=$?
exit "${status}"
