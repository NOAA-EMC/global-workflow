#! /usr/bin/env bash
##############################################################
# Add the NCDC GIF processing to the end of the gempak_gif job
# There is no timing issue with the NCDC GIF, so it is
# okay to just add it here. If timing becomes a problem
# in the future, we should move it above somewhere else.
##############################################################

source "${HOMEgfs}/ush/preamble.sh"

cd "${DATA}" || exit 2

export NTS="${HOMEgfs}/gempak/ush/restore"

if [[ ${MODEL} == GDAS ]]; then
    fcsthrs="000"

    sleep_interval=20
    max_tries=180
    export fhr3
    for fhr3 in ${fcsthrs}; do
        gempak_file="${COMIN_ATMOS_GEMPAK_1p00}/${RUN}_1p00_${PDY}${cyc}f${fhr3}"
        if ! wait_for_file "${gempak_file}" "${sleep_interval}" "${max_tries}" ; then
            echo "FATAL ERROR: ${gempak_file} not found after ${max_tries} iterations"
            exit 10
        fi

        cp "${gempak_file}" "gem_grids${fhr3}.gem"
        export err=$?
        if (( err != 0 )) ; then
            echo "FATAL: Could not copy ${gempak_file}"
            exit "${err}"
        fi

        "${HOMEgfs}/gempak/ush/gempak_${RUN}_f${fhr3}_gif.sh"
    done
fi

exit
