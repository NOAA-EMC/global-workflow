#! /usr/bin/env bash

set -x

###############################################################
# Source UFSDA workflow modules
source "${HOMEglobal}/dev/ush/load_modules.sh" ufsda
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="marineanlecen"
export jobid="${job}.$$"

###############################################################
# Setup Python path for GDASApp ush
PYTHONPATH="${PYTHONPATH:+${PYTHONPATH}:}${HOMEglobal}/sorc/gdas.cd/ush"
export PYTHONPATH

###############################################################
# Execute the JJOB
"${HOMEglobal}"/dev/jobs/JGLOBAL_MARINE_ANALYSIS_ECEN
status=$?
exit "${status}"
