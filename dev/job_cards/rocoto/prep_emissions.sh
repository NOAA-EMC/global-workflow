#! /usr/bin/env bash

set -x

###############################################################
# Source UFSDA workflow modules
source "${HOMEgfs}/dev/ush/load_modules.sh" run
status=$?
((status != 0)) && exit "${status}"

export job="prep_emissions"
export jobid="${job}.$$"

###############################################################
# Source relevant configs
configs="base aero prep_emissions"
for config in ${configs}; do
    source "${EXPDIR}/config.${config}"
    status=$?
    if [[ ${status} -ne 0 ]]; then
        exit "${status}"
    fi
done

###############################################################
# Execute the JJOB
"${HOMEgfs}/dev/jobs/JGLOBAL_PREP_EMISSIONS"
status=$?
exit "${status}"
