#! /usr/bin/env bash

source "${HOMEgfs}/ush/preamble.sh"

cd "${DATA}" || exit 1


# Set paramlist files based on FORECAST_HOUR (-1, 0, 3, 6, etc.)
# Determine if supplemental products (PGBS) (1-degree and 1/2-degree) should be generated
if (( FORECAST_HOUR <= 0 )); then
  if (( FORECAST_HOUR < 0 )); then
    export fhr3="anl"
    export paramlista="paramlista_anl"
  else
    export fhr3=$(printf "f%03d" "${FORECAST_HOUR}")
    export paramlista="paramlista_f000"
  fi
  export PGBS="YES"
else
  export fhr3=$(printf "f%03d" "${FORECAST_HOUR}")
  if (( FORECAST_HOUR%FHOUT_PGBS == 0 )); then
    export PGBS="YES"
  fi
fi

# Variables needed by ${GFSDOWNSH}
export MASTER_FILE="${COM_ATMOS_MASTER}/${PREFIX}master.grb2${fhr3}"

# Call the downstream grib2 interpolated products script
"${GFSDOWNSH}"
export err=$?; err_chk

# Create sflux.1p00 files when desired
if [[ "${FLXGF:-}" == "YES" ]]; then
  "${GFSDOWNSHF}" "${COM_ATMOS_MASTER}/${FLUXFL}" "${COM_ATMOS_GRIB_1p00}/${FLUXFL}" "1p00"  # TODO: FLUXFL is not defined; FIXME.
  export err=$?; err_chk
fi

#---------------------------------------------------------------
# If SENDDBN is YES, then send the files to DBN; otherwise nothing more to do in this script. Tata!
if [[ "${SENDDBN:-}" == "YES" ]]; then
  echo "SENDDBN='YES', so sending alerts to DBN"
else
  exit 0
fi

#---------------------------------------------------------------
# Start sending DBN alerts
# Everything below this line is for sending files to DBN (SENDDBN=YES)
"${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_0P25"       "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2.0p25.${fhr3}"
"${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_0P25_WIDX"  "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2.0p25.${fhr3}.idx"
if [[ "${RUN}" == "gfs" ]]; then
  "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_0P25"      "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2b.0p25.${fhr3}"
  "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_0P25_WIDX" "${job}" "${COM_ATMOS_GRIB_0p25}/${PREFIX}pgrb2b.0p25.${fhr3}.idx"
  if [[ -s "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2.0p50.f${fhr3}" ]]; then
    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_0P5"       "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2.0p50.${fhr3}"
    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_0P5_WIDX"  "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2.0p50.${fhr3}.idx"
    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_0P5"      "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2b.0p50.${fhr3}"
    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_0P5_WIDX" "${job}" "${COM_ATMOS_GRIB_0p50}/${PREFIX}pgrb2b.0p50.${fhr3}.idx"
  fi
  if [[ -s "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.f${fhr3}" ]]; then
    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_1P0"       "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.${fhr3}"
    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2_1P0_WIDX"  "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.${fhr3}.idx"
    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_1P0"      "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2b.1p00.${fhr3}"
    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB2B_1P0_WIDX" "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2b.1p00.${fhr3}.idx"
  fi
fi

if [[ "${fhr3}" == "anl" ]]; then

  "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_MSC_sfcanl" "${job}" "${COM_ATMOS_ANALYSIS}/${PREFIX}sfc${fhr3}.nc"
  "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SA"         "${job}" "${COM_ATMOS_ANALYSIS}/${PREFIX}atm${fhr3}.nc"

  "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGA_GB2"      "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.${fhr3}"
  "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGA_GB2_WIDX" "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.${fhr3}.idx"

else  # forecast hours f000, f003, f006, etc.

  if [[ "${RUN}" == "gdas" ]]; then
    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB_GB2"        "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.${fhr3}"
    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_PGB_GB2_WIDX"   "${job}" "${COM_ATMOS_GRIB_1p00}/${PREFIX}pgrb2.1p00.${fhr3}.idx"
    if (( FORECAST_HOUR % 3 == 0 )); then
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SF"           "${job}" "${COM_ATMOS_HISTORY}/${PREFIX}atm${fhr3}.nc"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_BF"           "${job}" "${COM_ATMOS_HISTORY}/${PREFIX}sfc${fhr3}.nc"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SGB_GB2"      "${job}" "${COM_ATMOS_MASTER}/${PREFIX}sfluxgrb${fhr3}.grib2"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SGB_GB2_WIDX" "${job}" "${COM_ATMOS_MASTER}/${PREFIX}sfluxgrb${fhr3}.grib2.idx"
    fi
  elif [[ "${RUN}" == "gfs" ]]; then

    "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SF" "${job}" "${COM_ATMOS_HISTORY}/${PREFIX}atm${fhr3}.nc"
    if (( fhr > 0 && fhr <= 84 )) || (( fhr == 120 )); then
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_BF" "${job}" "${COM_ATMOS_HISTORY}/${PREFIX}sfc${fhr3}.nc"
    fi

    if [[ -s "${COM_ATMOS_MASTER}/${PREFIX}sfluxgrbf${fhr3}.grib2" ]]; then
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SGB_GB2"      "${job}" "${COM_ATMOS_MASTER}/${PREFIX}sfluxgrb${fhr3}.grib2"
      "${DBNROOT}/bin/dbn_alert" MODEL "${RUN^^}_SGB_GB2_WIDX" "${job}" "${COM_ATMOS_MASTER}/${PREFIX}sfluxgrb${fhr3}.grib2.idx"
    fi
  fi  # end if RUN=gfs

fi  # end if fhr3=anl

exit 0