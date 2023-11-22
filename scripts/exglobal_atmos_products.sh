#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

cd "${DATA}" || exit 1

GFSDOWNSH=${GFSDOWNSH:-${USHgfs}/fv3gfs_downstream_nems.sh}
export GFSDWNSH=${GFSDWNSH:-${USHgfs}/fv3gfs_dwn_nems.sh}
GFSDOWNSHF=${GFSDOWNSHF:-${USHgfs}/inter_flux.sh}

if [[ "${RUN}" = "gdas" ]]; then
  export downset=${downset:-1}
  FLXGF=${FLXGF:-"NO"}
elif [[ "${RUN}" = "gfs" ]]; then
  export downset=${downset:-2}
  FLXGF=${FLXGF:-"YES"}
fi

# post_times is either "anl" or "000", "003", "006", etc (without the 'f').
if [[ "${post_times}" == "anl" ]]; then
  export FH=-1
else
  export FH=$(( 10#${post_times} ))
  fhr3=$(printf "f%03d" "${FH}")
fi

# Call the downstream grib2 interpolated products script
"${GFSDOWNSH}"
export err=$?; err_chk

# Create sflux.1p00 files when desired
if [[ "${FLXGF:-}" = 'YES' ]]; then
  "${GFSDOWNSHF}" "${COM_ATMOS_MASTER}/${FLUXFL}" "${COM_ATMOS_GRIB_1p00}/${FLUXFL}" "1p00"  # TODO: FLUXFL is not defined; FIXME.
  export err=$?; err_chk
fi

# If SENDDBN is YES, then send the files to DBN; otherwise nothing more to do in this script. Tata!
if [[ "${SENDDBN}" != "YES" ]]; then
  exit 0
fi

# Start sending DBN alerts
# Everything below this line is for sending files to DBN (SENDDBN=YES)
if [[ "${post_times}" == "anl" ]]; then

  "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_MSC_sfcanl" "${job}" "${COM_ATMOS_ANALYSIS}/${PREFIX}sfcanl.nc"
  "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SA"         "${job}" "${COM_ATMOS_ANALYSIS}/${PREFIX}atmanl.nc"

  "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGA_GB2"      "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.anl"
  "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGA_GB2_WIDX" "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.anl.idx"

  if [[ "${RUN}" == "gfs" ]]; then
    if [[ "${PGBF}" == "YES" ]]; then
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_0P25"       "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2.0p25.anl"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_0P25_WIDX"  "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2.0p25.anl.idx"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_0P25"      "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2b.0p25.anl"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_0P25_WIDX" "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2b.0p25.anl.idx"

      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_0P5"       "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2.0p50.anl"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_0P5_WIDX"  "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2.0p50.anl.idx"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_0P5"      "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2b.0p50.anl"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_0P5_WIDX" "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2b.0p50.anl.idx"

      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_1P0"      "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2b.1p00.anl"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_1P0_WIDX" "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2b.1p00.anl.idx"
    fi
  fi

else  # forecast hours 000, 003, 006, etc.

  if [[ "${RUN}" == "gdas" ]]; then
    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_0P25"      "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2.0p25.f${fhr3}"
    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_0P25_WIDX" "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2.0p25.f${fhr3}.idx"
    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB_GB2"        "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.f${fhr3}"
    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB_GB2_WIDX"   "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.f${fhr3}.idx"
    if (( FH % 3 == 0 )); then
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SF"           "${job}" "${COM_ATMOS_HISTORY}/${PREFIX}atmf${fhr3}.nc"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_BF"           "${job}" "${COM_ATMOS_HISTORY}/${PREFIX}sfcf${fhr3}.nc"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SGB_GB2"      "${job}" "${COM_ATMOS_MASTER}/${PREFIX}sfluxgrbf${fhr3}.grib2"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SGB_GB2_WIDX" "${job}" "${COM_ATMOS_MASTER}/${PREFIX}sfluxgrbf${fhr3}.grib2.idx"
    fi
  fi  # end if RUN=gdas

  if [[ "${RUN}" == "gfs" ]]; then
    if [[ "${PGBF}" == "YES" ]]; then
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_0P25"       "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2.0p25.f${fhr3}"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_0P25_WIDX"  "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2.0p25.f${fhr3}.idx"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_0P25"      "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2b.0p25.f${fhr3}"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_0P25_WIDX" "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2b.0p25.f${fhr3}.idx"

      if [[ -s "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2.0p50.f${fhr3}" ]]; then
        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_0P5"       "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2.0p50.f${fhr3}"
        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_0P5_WIDX"  "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2.0p50.f${fhr3}.idx"
        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_0P5"      "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2b.0p50.f${fhr3}"
        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_0P5_WIDX" "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2b.0p50.f${fhr3}.idx"
      fi

      if [[ -s "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.f${fhr3}" ]]; then
        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_1P0"       "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.f${fhr3}"
        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_1P0_WIDX"  "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.f${fhr3}.idx"
        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_1P0"      "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2b.1p00.f${fhr3}"
        "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_1P0_WIDX" "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2b.1p00.f${fhr3}.idx"
      fi
    fi

    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SF" "${job}" "${COM_ATMOS_HISTORY}/${PREFIX}atmf${fhr3}.nc"
    if (( fhr > 0 && fhr <= 84 )) || (( fhr == 120 )); then
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_BF" "${job}" "${COM_ATMOS_HISTORY}/${PREFIX}sfcf${fhr3}.nc"
    fi

    if [[ -s "${COM_ATMOS_MASTER}/${PREFIX}sfluxgrbf${fhr3}.grib2" ]]; then
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SGB_GB2"      "${job}" "${COM_ATMOS_MASTER}/${PREFIX}sfluxgrbf${fhr3}.grib2"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SGB_GB2_WIDX" "${job}" "${COM_ATMOS_MASTER}/${PREFIX}sfluxgrbf${fhr3}.grib2.idx"
    fi
  fi  # end if RUN=gfs

fi  # end if post_times=anl

exit 0