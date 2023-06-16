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
  # determine if restart IC exists to continue from a previous forecast
  RERUN=${RERUN:-"NO"}
  filecount=$(find "${COM_ATMOS_RESTART:-/dev/null}" -type f | wc -l)
  if [[ ( ${CDUMP} = "gfs" || ( ${RUN} = "gefs" && ${CDATE_RST} = "" )) && ${restart_interval} -gt 0 && ${FHMAX} -gt ${restart_interval} && ${filecount} -gt 10 ]]; then
    filelist=$(ls -1 ${COM_ATMOS_RESTART}/????????.??0000.coupler.res)
    filelist=$(ls -1 ${COM_ATMOS_RESTART}/????????.??0000.coupler.res)
    flag2=${COM_ATMOS_RESTART}/coupler.res
    reverse=$(echo "${restart_interval[@]} " | tac -s ' ')
    for xfh in ${reverse} ; do
      yfh=$((xfh-(IAU_OFFSET/2)))
      SDATE=$(${NDATE} ${yfh} "${CDATE}")
      PDYS=$(echo "${SDATE}" | cut -c1-8)
      cycs=$(echo "${SDATE}" | cut -c9-10)
      flag1=${COM_ATMOS_RESTART}/${PDYS}.${cycs}0000.coupler.res
      flag2=${COM_ATMOS_RESTART}/coupler.res

      #make sure that the wave restart files also exist if cplwav=true
      waverstok=".true."
      if [[ "${cplwav}" = ".true." ]]; then
        for wavGRD in ${waveGRD} ; do
          if [[ ! -f "${COM_WAVE_RESTART}/${PDYS}.${cycs}0000.restart.${wavGRD}" ]]; then
            waverstok=".false."
          fi
        done
      fi

      if [[ -s "${flag1}" ]] && [[ ${waverstok} = ".true." ]]; then
        CDATE_RST=${SDATE}
        [[ ${RERUN} = "YES" ]] && break
        mv "${flag1}" "${flag1}.old"
        if [[ -s "${flag2}" ]]; then mv "${flag2}" "${flag2}.old" ;fi
        RERUN="YES"
        [[ ${xfh} = ${restart_interval} ]] && RERUN="NO"
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
