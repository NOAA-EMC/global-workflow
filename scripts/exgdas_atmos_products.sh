#! /usr/bin/env bash

# Copied from exgdas_atmos_post.sh
# There might be differences with exgfs_atmos_post.sh that need to be included here

source "${HOMEgfs}/ush/preamble.sh"

cd "${DATA}" || exit 1

GFSDOWNSH=${GFSDOWNSH:-${USHgfs}/fv3gfs_downstream_nems.sh}
export GFSDWNSH=${GFSDWNSH:-${USHgfs}/fv3gfs_dwn_nems.sh}

stime="$(echo "${post_times}" | cut -c1-3)"
if [[ "${stime}" = "anl" ]]; then

    #Proces pgb files
    export FH=-1
    export downset=${downset:-1}
    ${GFSDOWNSH}
    export err=$?; err_chk

    if [[ "${SENDDBN}" = 'YES' ]]; then
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_MSC_sfcanl" "${job}" "${COM_ATMOS_ANALYSIS}/${PREFIX}sfcanl.nc"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SA" "${job}" "${COM_ATMOS_ANALYSIS}/${PREFIX}atmanl.nc"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGA_GB2" "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.anl"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGA_GB2_WIDX" "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.anl.idx"
    fi

else   ## not_anl if_stimes

  ############################################################
  # Loop Through the Post Forecast Files
  ############################################################

  for fhr3 in ${post_times}; do
    fhr=$(( 10#${fhr3} ))

    #Process pgb files
    export FH=$(( fhr ))
    export downset=${downset:-1}
    ${GFSDOWNSH}
    export err=$?; err_chk

    if [[ "${SENDDBN}" = "YES" ]]; then
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_0P25" "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2.0p25.f${fhr3}"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_0P25_WIDX ""${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2.0p25.f${fhr3}.idx"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB_GB2" "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.f${fhr3}"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB_GB2_WIDX" "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.f${fhr3}.idx"
    fi

    if [[ "${SENDDBN}" = 'YES' ]] && [[ "${RUN}" = 'gdas' ]] && (( fhr % 3 == 0 )); then
        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SF" "${job}" "${COM_ATMOS_HISTORY}/${PREFIX}atmf${fhr3}.nc"
        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_BF" "${job}" "${COM_ATMOS_HISTORY}/${PREFIX}sfcf${fhr3}.nc"
        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SGB_GB2" "${job}" "${COM_ATMOS_MASTER}/${PREFIX}sfluxgrbf${fhr3}.grib2"
        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SGB_GB2_WIDX ""${job}" "${COM_ATMOS_MASTER}/${PREFIX}sfluxgrbf${fhr3}.grib2.idx"
    fi

  done
fi   ## end_if_times

exit 0

################## END OF SCRIPT #######################
