#! /usr/bin/env bash

#####
## "forecast_det.sh"
## This script sets value of all variables
##
## This is the child script of ex-global forecast,
## This script is a definition of functions.
#####

# For all non-evironment variables
# Cycling and forecast hour specific parameters

FV3_GFS_det(){
  #-------------------------------------------------------
  # warm start?
  warm_start=${EXP_WARM_START:-".false."}
  read_increment=${read_increment:-".false."}
  res_latlon_dynamics="''"

  # Determine if this is a warm start or cold start
  if [[ -f "${COM_ATMOS_RESTART_PREV}/${sPDY}.${scyc}0000.coupler.res" ]]; then
    export warm_start=".true."
  fi

  # turn IAU off for cold start
  DOIAU_coldstart=${DOIAU_coldstart:-"NO"}
  if [ ${DOIAU} = "YES" -a ${warm_start} = ".false." ] || [ ${DOIAU_coldstart} = "YES" -a ${warm_start} = ".true." ]; then
    export DOIAU="NO"
    echo "turning off IAU since warm_start = ${warm_start}"
    DOIAU_coldstart="YES"
    IAU_OFFSET=0
    sCDATE=${CDATE}
    sPDY=${PDY}
    scyc=${cyc}
    tPDY=${sPDY}
    tcyc=${cyc}
  fi

  #-------------------------------------------------------
  # determine if restart IC exists to continue from a previous forecast run attempt

  RERUN=${RERUN:-"NO"}
  # Get a list of all YYYYMMDD.HH0000.coupler.res files from the atmos restart directory
  file_array=( $(find "${COM_ATMOS_RESTART:-/dev/null}" -name "????????.??0000.coupler.res" -print) )
  if [[ ( "${RUN}" = "gfs" || "${RUN}" = "gefs" ) \
    && "${restart_interval}" -gt 0 \
    && "${restart_interval}" -lt "${FHMAX}" \
    && "${#file_array[@]}" -gt 0 ]]; then

    # Look in reverse order of file_array to determine available restart times
    for ((ii=${#file_array[@]}-1; ii>=0; ii--)); do

      filepath="${file_array[ii]}"
      filename=$(basename ${filepath})  # Strip path from YYYYMMDD.HH0000.coupler.res
      PDYS=${filename:0:8}  # match YYYYMMDD of YYYYMMDD.HH0000.coupler.res
      cycs=${filename:9:2}  # match HH of YYYYMMDD.HH0000.coupler.res

      # Assume all is well; all restarts are available
      fv3_rst_ok="YES"
      mom6_rst_ok="YES"
      cice6_rst_ok="YES"
      cmeps_rst_ok="YES"
      ww3_rst_ok="YES"

      # Check for availability of FV3 restarts
      if [[ -f "${COM_ATMOS_RESTART}/${PDYS}.${cycs}0000.coupler.res" ]]; then
        mv "${COM_ATMOS_RESTART}/${PDYS}.${cycs}.coupler.res" "${COM_ATMOS_RESTART}/${PDYS}.${cycs}.coupler.res.old"
      else
        fv3_rst_ok="NO"
      fi

      # Check for availability of MOM6 restarts  # TODO
      # Check for availability of CICE6 restarts  # TODO
      # Check for availability of CMEPS restarts  # TODO

      # Check for availability of WW3 restarts
      if [[ "${cplwav}" = ".true." ]]; then
        for ww3_grid in ${waveGRD} ; do
          if [[ ! -f "${COM_WAVE_RESTART}/${PDYS}.${cycs}0000.restart.${ww3_grid}" ]]; then
            ww3_rst_ok="NO"
          fi
        done
      fi

      # Collective check
      if [[ "${fv3_rst_ok}" = "YES" ]] \
        && [[ "${mom6_rst_ok}" = "YES" ]] \
        && [[ "${cice6_rst_ok}" = "YES" ]] \
        && [[ "${cmeps_rst_ok}" = "YES" ]] \
        && [[ "${ww3_rst_ok}" = "YES" ]]; then

        if [[ -f "${COM_ATMOS_RESTART}/coupler.res" ]]; then
          mv "${COM_ATMOS_RESTART}/coupler.res" "${COM_ATMOS_RESTART}/coupler.res.old"
        fi

        SDATE="${PDYS}${cycs}"
        CDATE_RST="${SDATE}"
        RERUN="YES"
        echo "Restarts have been found for CDATE_RST=${CDATE_RST}, returning with 'RERUN=YES'"
        break
      fi

    done
  fi
  #-------------------------------------------------------
}

WW3_det(){
  echo "SUB ${FUNCNAME[0]}: Run type determination for WW3"
}

CICE_det(){
  echo "SUB ${FUNCNAME[0]}: Run type determination for CICE"
}

MOM6_det(){
  echo "SUB ${FUNCNAME[0]}: Run type determination for MOM6"
}
