#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source FV3GFS workflow modules
. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

###############################################################
# Loop over groups to Execute the JJOB
fhrlst=$(echo ${FHRLST} | sed -e 's/_/ /g; s/f/ /g; s/,/ /g')
for fhr in ${fhrlst}; do

    export FHMIN_ECEN=${fhr}
    export FHMAX_ECEN=${fhr}
    export FHOUT_ECEN=${fhr}
    export job=ecen
    export jobid="${job}.$$"

    ${HOMEgfs}/jobs/JGDAS_ENKF_ECEN
    status=$?
    if [[ ${status} -ne 0 ]]; then
        exit "${status}"
    fi

done

###############################################################
# Exit out cleanly


exit 0
