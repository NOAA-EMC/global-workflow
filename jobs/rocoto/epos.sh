#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
# Source FV3GFS workflow modules
. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
status=$?
if [[ ${status} -ne 0 ]]; then
    exit "${status}"
fi

export job="epos"
export jobid="${job}.$$"
    
###############################################################
# Loop over groups to Execute the JJOB
fhrlst=$(echo ${FHRLST} | sed -e 's/_/ /g; s/f/ /g; s/,/ /g')

for fhr in ${fhrlst}; do

    export FHMIN_EPOS=${fhr}
    export FHMAX_EPOS=${fhr}
    export FHOUT_EPOS=${fhr}
    ${HOMEgfs}/jobs/JGDAS_ENKF_POST
    status=$?
    if [[ ${status} -ne 0 ]]; then
        exit "${status}"
    fi

done

###############################################################
# Exit out cleanly


exit 0
