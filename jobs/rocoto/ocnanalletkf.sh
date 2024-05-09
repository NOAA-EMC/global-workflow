#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source UFSDA workflow modules
. "${HOMEgfs}/ush/load_ufsda_modules.sh"
status=$?
[[ ${status} -ne 0 ]] && exit "${status}"

export job="ocnanalletkf"
export jobid="${job}.$$"

###############################################################
# Setup Python path for GDASApp ush
PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${HOMEgfs}/ush/python"
export PYTHONPATH

###############################################################
# Execute the JJOB
"${HOMEgfs}"/jobs/JGDAS_GLOBAL_OCEAN_ANALYSIS_LETKF
status=$?
exit "${status}"
