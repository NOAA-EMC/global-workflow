#! /usr/bin/env bash
##############################################################
# Add the NCDC GIF processing to the end of the gempak_gif job
# There is no timing issue with the NCDC GIF, so it is
# okay to just add it here. If timing becomes a problem
# in the future, we should move it above somewhere else.
##############################################################

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
            export err=10
            if [[ ${err} -ne 0 ]]; then
              err_exit "${gempak_file} not found after ${max_tries} iterations"
            fi
        fi

        if [[ ! -f "${gempak_file}" ]]; then
            export err=1
            if [[ ${err} -ne 0 ]]; then
              err_exit "Could not copy ${gempak_file}"
            fi
        fi

        cpreq "${gempak_file}" "gem_grids${fhr3}.gem"

        "${HOMEgfs}/gempak/ush/gempak_${RUN}_f${fhr3}_gif.sh" && true
        export err=$?
        if [[ ${err} -ne 0 ]]; then
          err_exit
        fi
    done
fi

exit
