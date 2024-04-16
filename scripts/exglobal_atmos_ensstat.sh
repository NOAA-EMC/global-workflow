#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

fhr3=$(printf "%03d" "${FORECAST_HOUR}")

if [[ -a mpmd_script ]]; then rm -Rf mpmd_script; fi

{
    for grid in '0p25' '0p50' '1p00'; do
        echo "${USHgfs}/atmos_ensstat.sh ${grid} ${fhr3}"
        # echo "${USHgfs}/atmos_ensstat.sh ${grid} ${fhr3} b"
    done
} > mpmd_script

"${USHgfs}/run_mpmd.sh" mpmd_script
err=$?

exit "${err}"
