#! /usr/bin/env bash

fhr3=$(printf "%03d" "${FORECAST_HOUR}")

if [[ -e mpmd_script ]]; then
    rm -f mpmd_script
fi

{
    for grid in '0p25' '0p50' '1p00'; do
        echo "${USHgfs}/atmos_ensstat.sh ${grid} ${fhr3}"
        # echo "${USHgfs}/atmos_ensstat.sh ${grid} ${fhr3} b"
    done
} > mpmd_script

"${USHgfs}/run_mpmd.sh" mpmd_script
export err=$?
if [[ ${err} -ne 0 ]]; then
    err_exit "One or more MPMD jobs failed to calculate ensemble statistics!"
fi
