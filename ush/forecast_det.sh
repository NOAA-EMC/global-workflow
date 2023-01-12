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
  if [[ -f "${gmemdir}/RESTART/${sPDY}.${scyc}0000.coupler.res" ]]; then
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
  RERUN="NO"
  filecount=$(find "${RSTDIR_ATM:-/dev/null}" -type f | wc -l)
  if [ ${CDUMP} = "gfs" -a ${rst_invt1} -gt 0 -a ${FHMAX} -gt ${rst_invt1} -a ${filecount} -gt 10 ]; then
    reverse=$(echo "${restart_interval[@]} " | tac -s ' ')
    for xfh in ${reverse} ; do
      yfh=$((xfh-(IAU_OFFSET/2)))
      SDATE=$(${NDATE} +${yfh} "${CDATE}")
      PDYS=$(echo "${SDATE}" | cut -c1-8)
      cycs=$(echo "${SDATE}" | cut -c9-10)
      flag1=${RSTDIR_ATM}/${PDYS}.${cycs}0000.coupler.res
      flag2=${RSTDIR_ATM}/coupler.res

      #make sure that the wave restart files also exist if cplwav=true
      waverstok=".true."
      if [[ "${cplwav}" = ".true." ]]; then
        for wavGRD in ${waveGRD} ; do
          if [[ ! -f "${RSTDIR_WAVE}/${PDYS}.${cycs}0000.restart.${wavGRD}" ]]; then
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
        [[ ${xfh} = ${rst_invt1} ]] && RERUN="NO"
      fi
    done
  fi
  #-------------------------------------------------------
}

FV3_GEFS_det(){
  echo "SUB ${FUNCNAME[0]}: Defining variables for FV3GEFS"
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
