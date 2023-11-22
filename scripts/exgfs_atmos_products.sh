#! /usr/bin/env bash

# Copied from exgfs_atmos_post.sh
# There might be differences with exgdas_atmos_post.sh that need to be included here

source "${HOMEgfs}/ush/preamble.sh"

cd "${DATA}" || exit 1

GFSDOWNSH=${GFSDOWNSH:-${USHgfs}/fv3gfs_downstream_nems.sh}
export GFSDWNSH=${GFSDWNSH:-${USHgfs}/fv3gfs_dwn_nems.sh}
GFSDOWNSHF=${GFSDOWNSHF:-${USHgfs}/inter_flux.sh}
FLXGF=${FLXGF:-"YES"}

# Process analysis when post_times is 00
stime="$(echo "${post_times}" | cut -c1-3)"
if [[ "${stime}" = "anl" ]]; then

    #  Process pgb files
    if [[ "${PGBF}" = 'YES' ]]; then
      export FH=-1
      export downset=${downset:-2}
      ${GFSDOWNSH}
      export err=$?; err_chk
    fi

    if [[ "${SENDDBN}" = 'YES' ]]; then
        "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_MSC_sfcanl "${job}" "${COM_ATMOS_ANALYSIS}/${PREFIX}sfcanl.nc"
        "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_SA "${job}" "${COM_ATMOS_ANALYSIS}/${PREFIX}atmanl.nc"
        if [[ "${PGBF}" = 'YES' ]]; then
            "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_PGB2_0P25 "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2.0p25.anl"
            "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_PGB2_0P25_WIDX "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2.0p25.anl.idx"
            "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_PGB2B_0P25 "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2b.0p25.anl"
            "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_PGB2B_0P25_WIDX "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2b.0p25.anl.idx"

            "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_PGB2_0P5 "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2.0p50.anl"
            "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_PGB2_0P5_WIDX "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2.0p50.anl.idx"
            "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_PGB2B_0P5 "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2b.0p50.anl"
            "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_PGB2B_0P5_WIDX "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2b.0p50.anl.idx"

            "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_PGB2_1P0 "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.anl"
            "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_PGB2_1P0_WIDX "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.anl.idx"
            "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_PGB2B_1P0 "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2b.1p00.anl"
            "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_PGB2B_1P0_WIDX "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2b.1p00.anl.idx"
        fi
    fi

else   ## not_anl if_stime

  ############################################################
  # Loop Through the Post Forecast Files
  ############################################################

  for fhr3 in ${post_times}; do
    echo "Start processing fhr=${fhr3}"
    fhr=$(( 10#${fhr3} ))

    #  Process pgb files
    if [[ "${PGBF}" = 'YES' ]]; then
      export FH=$(( fhr ))
      export downset=${downset:-2}
      ${GFSDOWNSH}
      export err=$?; err_chk
    fi

    if [[ "${SENDDBN}" = 'YES' ]]; then
      if [[ "${PGBF}" = 'YES' ]]; then
        "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_PGB2_0P25 "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2.0p25.f${fhr3}"
        "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_PGB2_0P25_WIDX "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2.0p25.f${fhr3}.idx"
        "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_PGB2B_0P25 "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2b.0p25.f${fhr3}"
        "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_PGB2B_0P25_WIDX "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2b.0p25.f${fhr3}.idx"

        if [[ -s "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2.0p50.f${fhr3}" ]]; then
          "${DBNROOT}/bin/dbn_alert"  MODEL ${RUN^^}_PGB2_0P5 "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2.0p50.f${fhr3}"
          "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_PGB2_0P5_WIDX "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2.0p50.f${fhr3}.idx"
          "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_PGB2B_0P5 "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2b.0p50.f${fhr3}"
          "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_PGB2B_0P5_WIDX "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2b.0p50.f${fhr3}.idx"
        fi

        if [[ -s "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.f${fhr3}" ]]; then
          "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_PGB2_1P0 "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.f${fhr3}"
          "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_PGB2_1P0_WIDX "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.f${fhr3}.idx"
          "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_PGB2B_1P0 "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2b.1p00.f${fhr3}"
          "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_PGB2B_1P0_WIDX "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2b.1p00.f${fhr3}.idx"
        fi
      fi

      if [[ -s "${COM_ATMOS_MASTER}/${PREFIX}sfluxgrbf${fhr3}.grib2" ]]; then
      "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_SGB_GB2 "${job}" "${COM_ATMOS_MASTER}/${PREFIX}sfluxgrbf${fhr3}.grib2"
      "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_SGB_GB2_WIDX "${job}" "${COM_ATMOS_MASTER}/${PREFIX}sfluxgrbf${fhr3}.grib2.idx"
      fi

      "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_SF "${job}" "${COM_ATMOS_HISTORY}/${PREFIX}atmf${fhr3}.nc"
      if (( fhr > 0 && fhr <= 84 )) || (( fhr == 120 )); then
        "${DBNROOT}/bin/dbn_alert" MODEL ${RUN^^}_BF "${job}" "${COM_ATMOS_HISTORY}/${PREFIX}sfcf${fhr3}.nc"
      fi
    fi

    # Add extra flux.1p00 file for coupled runs
    if [[ "${FLXGF}" = 'YES' ]]; then
        ${GFSDOWNSHF} "${COM_ATMOS_MASTER}/${FLUXFL}" "${COM_ATMOS_GRIB_1p00}/${FLUXFL}" "1p00"
        export err=$?; err_chk
    fi

  done

  #----------------------------------
fi   ## end_if_stime

exit 0

################## END OF SCRIPT #######################
