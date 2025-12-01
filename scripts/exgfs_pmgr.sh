#! /usr/bin/env bash

#
# Script name:         exgfs_pmgr.sh.sms
#
#  This script monitors the progress of the gfs_fcst job
#

hour=0
TEND=384

if [[ -e posthours ]]; then
    rm -f posthours
fi

declare -a posthours
while [[ "${hour}" -le "${TEND}" ]]; do
    posthours+=("${hour}")
    if [[ ${hour} -lt 240 ]]; then
        if [[ ${hour} -lt 120 ]]; then
            hour=$((hour + 1))
        else
            hour=$((hour + 3))
        fi
    else
        hour=$((hour + 12))
    fi
done

#
# Wait for all fcst hours to finish
#
sleep_interval=10
max_tries=1000
for fhr in "${posthours[@]}"; do
    fhr3=$(sprintf "%03d" "${fhr}")
    log_file="${COMIN_ATMOS_HISTORY}/${RUN}.${cycle}.atm.logf${fhr3}.txt"
    if ! wait_for_file "${log_file}" "${sleep_interval}" "${max_tries}"; then
        msg="FATAL ERROR: After 2 hours of waiting for GFS FCST hour ${fhr3}."
        err_exit "${msg}"
    fi
    if [[ ${fhr} -eq 0 ]]; then
        ecflow_client --event release_postanl
    fi
    ecflow_client --event "release_post${fhr3}"
done

exit
