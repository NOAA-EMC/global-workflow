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

PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${HOMEgfs}/ush"
test="/home/Guillaume.Vernieres/scratch1/runs/cp2/global-workflow/sorc/gdas.cd/build/lib/python3.10"
PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${test}"
###############################################################
# Execute the JJOB
"${HOMEgfs}"/jobs/JGLOBAL_PREP_OCEAN_OBS
status=$?
exit "${status}"
