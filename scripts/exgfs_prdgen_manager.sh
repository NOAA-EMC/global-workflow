#! /usr/bin/env bash

#
# Script name:         exgfs_pmgr.sh.sms
#
#  This script monitors the progress of the gfs_fcst job
#

hour=0
TEND=384

if [[ -e pgrb2_hours ]]; then
    rm -f pgrb2_hours
fi

declare -a pgrb2_hours
while [[ "${hour}" -le "${TEND}" ]]; do
    pgrb2_hours+=("${hour}")
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
for fhr in "${pgrb2_hours[@]}"; do
    fhr3=$(sprintf "%03d" "${fhr}")
    master_file="${COMIN_ATMOS_MASTER}/gfs.${cycle}.master.grb2f${fhr3}"
    if ! wait_for_file "${master_file}" "${sleep_interval}" "${max_tries}"; then
        export err=1
        err_exit "After 2 hours of waiting for GFS POST hour ${fhr3}."
    fi
    ecflow_client --event "release_pgrb2_${fhr3}"
done

exit
