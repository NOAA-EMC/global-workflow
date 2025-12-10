#! /usr/bin/env bash

###############################################################
source "${HOMEgfs}/dev/ush/load_modules.sh" ufswm
err=$?
if [[ "${err}" -ne 0 ]]; then
    exit "${err}"
fi

export job="wave_stat"
export jobid="${job}.$$"

###############################################################
echo
echo "=============== START TO RUN WAVE STAT ==============="
# Execute the JJOB
# shellcheck disable=SC2153
IFS=', ' read -r -a fhr_list <<< "${FHR_LIST}"

export FHR3 jobid
for FORECAST_HOUR in "${fhr_list[@]}"; do
    FHR3=$(printf '%03d' "${FORECAST_HOUR}")
    jobid="${job}_f${FHR3}.$$"
    # Execute the JJOB
    "${HOMEgfs}/dev/jobs/JGEFS_WAVE_STAT"
    err=$?
    if [[ "${err}" -ne 0 ]]; then
        exit "${err}"
    fi
done

exit 0
