#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

###############################################################
echo
echo "=============== START TO SOURCE FV3GFS WORKFLOW MODULES ==============="
#. ${HOMEgfs}/ush/load_fv3gfs_modules.sh
. ${HOMEgfs}/ush/load_ufswm_modules.sh

status=$?
[[ ${status} -ne 0 ]] && exit ${status}

export job="wavestat"
export jobid="${job}.$$"

###############################################################
echo
echo "=============== START TO RUN WAVE PREP ==============="
# Execute the JJOB
IFS=', ' read -r -a fhr_list <<< "${FHR_LIST}"

export FHR3 jobid
for FORECAST_HOUR in "${fhr_list[@]}"; do
        FHR3=$(printf '%03d' "${FORECAST_HOUR}")
        jobid="${job}_f${FHR3}.$$"
        # Execute the JJOB
        "${HOMEgfs}/jobs/JGLOBAL_WAVE_STAT"
        status=$?
        [[ ${status} -ne 0 ]] && exit "${status}"
done

exit 0

