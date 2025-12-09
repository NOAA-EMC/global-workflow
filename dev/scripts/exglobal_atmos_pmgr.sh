#! /usr/bin/env bash

#
# Script name:         exgfs_pmgr.sh.sms
#
#  This script monitors the progress of the gfs_fcst job
#

hour=0

case "${RUN}" in
    gfs) TEND=384 ;;
    gdas) TEND=9 ;;
    *)
        err_exit "Run ${RUN} not supported at this time"
        ;;
esac

declare -a posthours
while [[ "${hour}" -le "${TEND}" ]]; do
    posthours+=("${hour}")
    if [[ ${hour} -lt 120 ]]; then
        hour=$((hour + 1))
    else
        hour=$((hour + 3))
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
        export err=1
        err_exit "After 2 hours of waiting for GFS FCST hour ${fhr3}."
    fi
    if [[ ${fhr} -eq 0 ]]; then
        ecflow_client --event release_postanl
    fi
    ecflow_client --event "release_post${fhr3}"
done

exit
