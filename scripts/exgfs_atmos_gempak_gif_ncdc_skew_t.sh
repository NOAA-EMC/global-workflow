#! /usr/bin/env bash

##############################################################
# Add the NCDC GIF processing to the end of the gempak_gif job
# There is no timing issue with the NCDC GIF, so it is
# okay to just add it here. If timing becomes a problem
# in the future, we should move it above somewhere else.
##############################################################

cd "${DATA}" || exit 1

export NTS="${HOMEgfs}/gempak/ush/restore"

if [[ "${MODEL}" == GDAS ]] || [[ "${MODEL}" == GFS ]]; then
    case "${MODEL}" in
        GDAS) fcsthrs="0";;
        GFS)  fcsthrs="0 12 24 36 48";;
        *)
            echo "FATAL ERROR: Unrecognized model type ${MODEL}"
            exit 5
            ;;
    esac

    sleep_interval=20
    max_tries=180
    for fhr in ${fcsthrs}; do
        fhr3=$(printf %03d "${fhr}")
        export GRIBFILE=${COMIN_ATMOS_GEMPAK_1p00}/${RUN}_1p00_${PDY}${cyc}f${fhr3}
        if ! wait_for_file "${GRIBFILE}" "${sleep_interval}" "${max_tries}" ; then
            echo "FATAL ERROR: ${GRIBFILE} not found after ${max_tries} iterations"
            exit 10
        fi

        cpreq "${GRIBFILE}" "gem_grids${fhr3}.gem"
        export fhr3
        if [[ ${fhr} -eq 0 ]]; then
            "${HOMEgfs}/gempak/ush/gempak_${RUN}_f000_gif.sh"
        else
            "${HOMEgfs}/gempak/ush/gempak_${RUN}_fhhh_gif.sh"
        fi
    done
fi

cd "${DATA}" || exit 1

export RSHPDY="${PDY:4:}${PDY:2:2}"

cpreq "${HOMEgfs}/gempak/dictionaries/sonde.land.tbl" sonde.land.tbl
cpreq "${HOMEgfs}/gempak/dictionaries/metar.tbl" metar.tbl
sort -k 2n,2 metar.tbl > metar_stnm.tbl
cpreq "${COMIN_OBS}/${model}.${cycle}.adpupa.tm00.bufr_d" fort.40

"${HOMEgfs}/exec/rdbfmsua.x" >> "${pgmout}" 2> errfile
export err=$?
if [[ ${err} -ne 0 ]]; then
   err_exit "Failed to run rdbfmsua!"
fi

# shellcheck disable=SC2012,SC2155
export filesize=$( ls -l rdbfmsua.out | awk '{print $5}' )

################################################################
#   only run script if rdbfmsua.out contained upper air data.
################################################################

if [[ ${filesize} -gt 40 ]]; then
    cpfs rdbfmsua.out "${COMOUT_ATMOS_GEMPAK_UPPER_AIR}/${RUN}.${cycle}.msupperair"
    cpfs sonde.idsms.tbl "${COMOUT_ATMOS_GEMPAK_UPPER_AIR}/${RUN}.${cycle}.msupperairtble"
    if [[ ${SENDDBN} = "YES" ]]; then
        "${DBNROOT}/bin/dbn_alert" DATA MSUPPER_AIR "${job}" "${COMOUT_ATMOS_GEMPAK_UPPER_AIR}/${RUN}.${cycle}.msupperair"
        "${DBNROOT}/bin/dbn_alert" DATA MSUPPER_AIRTBL "${job}" "${COMOUT_ATMOS_GEMPAK_UPPER_AIR}/${RUN}.${cycle}.msupperairtble"
    fi
fi

############################################################

if [[ -e "${pgmout}" ]] ; then
   cat "${pgmout}"
fi


exit
